(ns eval-apply)

;; from https://github.com/transducer/clojure-experiments/blob/master/meta-circular-experiments/mc-evaluator.clj

(require '[second-third-fourth :as stf])
(require '[environ :as env])
(require '[error :as err])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; have to declare all defined stuff below on top (except eval and
;; apply.
(declare self-evaluating? variable? lookup-variable-value quoted?
         assignment? definition? if? lambda? make-procedure begin?
         cond? application? text-of-quotation eval-assignment
         eval-definition eval-if eval-sequence lambda-parameters
         lambda-body val-sequence cond->if list-of-values
         operator operands begin-actions no-operands? first-operand
         rest-operands primitive-procedure?
         apply-primitive-procedure compound-procedure?
         procedure-body  procedure-parameters
         procedure-environment last-exp? first-exp rest-exps
         set-variable-value! assignment-variable assignment-value
         define-variable! definition-variable definition-value
         if-predicate if-consequent if-alternative cond-predicate
         make-lambda make-begin expand-clauses
         prompt-for-input
         announce-output user-print
          boolean? tagged-list?       cond-clauses
         cond-else-clause? primitive-implementation
           sequence->exp
         cond-actions make-if
         true? false? apply-from-underlying-lisp
         the-global-environment driver-loop)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; store apply from Clojure, we need it to apply primitive
;; procedures. And then declare it as well, otherwise Clojure
;; mistakes apply defined in eval for the Clojure one.
(defn apply-from-underlying-lisp [f args] (apply f args))
(declare apply)                                             ;; our 'apply' used in 'eval'

;; @TODO: split eval-apply.clj into eval.clj and apply.clj
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eval
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn eval [exp env]
  (cond (self-evaluating? exp) exp
        (variable? exp) (lookup-variable-value exp env)
        (quoted? exp) (text-of-quotation exp)
        (assignment? exp) (eval-assignment exp env)
        (definition? exp) (eval-definition exp env)
        (if? exp) (eval-if exp env)
        (lambda? exp) (make-procedure (lambda-parameters exp)
                                      (lambda-body exp)
                                      env)
        (begin? exp) (eval-sequence (begin-actions exp) env)
        (cond? exp) (eval (cond->if exp) env)
        (application? exp) (apply (eval (operator exp) env)
                                  (list-of-values (operands exp) env))
        :else (err/error "unknown expression type -- eval" exp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn self-evaluating? [exp]
  (cond (number? exp) true
        (string? exp) true
        (boolean? exp) true
        :else false))

;; routines to detect expressions
(defn variable? [exp] (symbol? exp))
(defn quoted? [exp] (tagged-list? exp 'quote))
(defn assignment? [exp] (tagged-list? exp 'set!))
(defn definition? [exp] (tagged-list? exp 'defn))
(defn if? [exp] (tagged-list? exp 'if))
(defn lambda? [exp] (tagged-list? exp 'fn))
(defn begin? [exp] (tagged-list? exp 'begin))
(defn cond? [exp] (tagged-list? exp 'cond))
(defn application? [exp] (list? exp))

(defn boolean? [exp] (or (= 'true exp) (= 'false exp)))
(defn true? [x] (not (= x false)))
(defn false? [x] (= x false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn tagged-list? [exp tag]
  (and (list? exp) (= (first exp) tag)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn lookup-variable-value [variable env]
  (letfn [(env-loop [env]
            (letfn [(scan [frame]
                      (if (contains? frame variable)
                        (let [value (get frame variable)]
                          value)
                        (env-loop (env/enclosing-environment env))))]
              (if (= env env/the-empty-environment)
                (err/error "unbound variable -- lookup-variable-value" variable)
                (let [frame (env/first-frame env)]
                  (scan @frame)))))]
    (env-loop env)))

(defn text-of-quotation [exp] (stf/second exp))

(defn eval-assignment [exp env]
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) exp)
                       env))

(defn eval-definition [exp env]
  (let [definition-name (definition-variable exp)]
    (do
      (define-variable! definition-name
                        (eval (definition-value exp) env)
                        env)
      (str definition-name " defined"))))

(defn eval-if [exp env]
  (if (eval (if-predicate exp) env)
    (eval (if-consequent exp) env)
    (eval (if-alternative exp) env)))

(defn make-procedure [parameters body env]
  (list 'procedure parameters body env))

(defn eval-sequence [exps env]
  (if (last-exp? exps)
    (eval (first-exp exps) env)
    (do
      (eval (first-exp exps) env)
      (eval-sequence (rest-exps exps) env))))

(defn cond->if [exp]
  (expand-clauses (cond-clauses exp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn lambda-parameters [exp] (stf/second exp))
(defn lambda-body [exp] (stf/rest-after-second exp))

(defn begin-actions [exp] (rest exp))

(defn list-of-values [exps env]
  (if (no-operands? exps)
    '()
    (cons (eval (first-operand exps) env)
          (list-of-values (rest-operands exps) env))))

(defn set-variable-value! [variable value env]
  (letfn [(env-loop [env]
            (letfn [(scan [frame]
                      (if (contains? @frame variable)
                        (swap! frame assoc variable value)
                        (env-loop (env/enclosing-environment env))))]
              (if (= env env/the-empty-environment)
                (err/error "unbound variable -- set-variable-value!" variable)
                (scan (env/first-frame env)))))]
    (env-loop env)))

(defn assignment-variable [exp] (stf/second exp))
(defn assignment-value [exp] (stf/third exp))

(defn define-variable! [variable value env]
  (swap! (env/first-frame env) assoc variable value))

(defn definition-variable [exp]
  (if (symbol? (stf/second exp))
    (stf/second exp)
    (stf/flat-third exp)))

(defn definition-value [exp]
  (if (symbol? (stf/second exp))
    (nth exp 2)
    (make-lambda ((comp rest stf/second) exp)
                 (drop 2 exp))))

(defn make-lambda [parameters body]
  (list 'fn parameters body))

(defn if-predicate [exp] (stf/second exp))
(defn if-consequent [exp] (stf/third exp))

(defn if-alternative [exp]
  (if (not (empty? (stf/rest-after-third exp)))
    (stf/fourth exp)
    'false))

(defn last-exp? [seq] (empty? (rest seq)))
(defn first-exp [seq] (first seq))
(defn rest-exps [seq] (rest seq))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn expand-clauses [clauses]
  (if (empty? clauses)
    'false                                                  ;; no else clause
    (let [first (first clauses)
          rest (rest clauses)]
      (if (cond-else-clause? first)
        (if (empty? rest)
          (sequence->exp (cond-actions first))
          (err/error "'else' clause isn't last -- cond->if expand-clauses" clauses) )
        (make-if (cond-predicate first)
                 (sequence->exp (cond-actions first))
                 (expand-clauses rest))))))

(defn sequence->exp [seq]
  (cond (empty? seq) seq
        (last-exp? seq) (first-exp seq)
        :else (make-begin seq)))

(defn cond-else-clause? [clause]
  (= (cond-predicate clause) 'else))

(defn cond-clauses [exp] (rest exp))
(defn cond-predicate [clause] (first clause))
(defn cond-actions [clause] (rest clause))

(defn make-if [predicate consequent alternative]
  (list 'if predicate consequent alternative))

(defn make-begin [seq] (cons 'begin seq))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; routines to manipulate expressions
(defn operator [exp] (first exp))
(defn operands [exp] (rest exp))
(defn no-operands? [args] (empty? args))
(defn first-operand [args] (first args))
(defn rest-operands [args] (rest args))

;; @TODO: split eval-apply.clj into eval.clj and apply.clj
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; apply
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn apply [procedure arguments]
  (cond (primitive-procedure? procedure)
        (apply-primitive-procedure procedure arguments)
        (compound-procedure? procedure)
        (eval-sequence
          (procedure-body procedure)
          (env/extend-environment (procedure-parameters procedure)
                              arguments
                              (procedure-environment procedure)))
        :else (err/error "unknown procedure type -- apply" procedure)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn primitive-procedure? [proc] (tagged-list? proc 'primitive))

(defn apply-primitive-procedure [proc args]
  (apply-from-underlying-lisp                               ;; note: lisp
    (primitive-implementation proc) args))

(defn compound-procedure? [p] (tagged-list? p 'procedure))
(defn procedure-body [p] (stf/third p))
(defn primitive-implementation [proc] (stf/second proc))
(defn procedure-environment [p] (stf/fourth p))
(defn procedure-parameters [p] (stf/second p))

;; @TODO: move to evaluator.clj namespace
;; from here-->>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; code to interact with the evaluator:
(def input-prompt ";;; eval input:")
(def output-prompt ";;; eval value:")

(defn driver-loop []
  (prompt-for-input input-prompt)
  (let [input (read)]
    (user-print input)
    (let [output (eval input the-global-environment)]
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(defn prompt-for-input [string]
  (newline) (println string) (newline))

(defn announce-output [string]
  (newline) (println string) (newline))

(defn user-print [object]
  (if (compound-procedure? object)
    (println (list 'compound-procedure
                   (procedure-parameters object)
                   (procedure-body object)
                   '<procedure-env>))
    (println object)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def the-global-environment (env/setup-environment))
;; <<--to here