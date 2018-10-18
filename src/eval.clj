(ns eval)

;; from https://github.com/transducer/clojure-experiments/blob/master/meta-circular-experiments/mc-evaluator.clj

(require '[parser :as p])
(require '[apply :as a])
(require '[error :as err])

(declare my-eval)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; my-eval
(defn my-eval [exp env]
  (cond (p/self-evaluating? exp) exp
        (p/variable? exp) (p/lookup-variable-value exp env)
        (p/quoted? exp) (p/text-of-quotation exp)
        (p/assignment? exp) (p/eval-assignment exp env)
        (p/definition? exp) (p/eval-definition exp env)
        (p/if? exp) (p/eval-if exp env)
        (p/lambda? exp) (p/make-procedure (p/lambda-parameters exp)
                                      (p/lambda-body exp)
                                      env)
        (p/begin? exp) (p/eval-sequence (p/begin-actions exp) env)
        (p/cond? exp) (my-eval (p/cond->if exp) env)
        (p/application? exp) (a/my-apply (my-eval (p/operator exp) env)
                                    (p/list-of-values (p/operands exp) env))
        :else (err/error "unknown expression type -- my-eval" exp)))
