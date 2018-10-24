(ns environ)

(require '[second-third-fourth :as stf])
(require '[error :as err])

(declare the-empty-environment setup-environment extend-environment primitive-procedure-names primitive-procedure-objects
         enclosing-environment copy-environment environments-equal?
         make-frame)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; environment structure
(def the-empty-environment '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setup environment
(defn setup-environment []
  (let [initial-env
        (extend-environment primitive-procedure-names
                            primitive-procedure-objects
                            the-empty-environment)]
    initial-env))

(defn extend-environment [vars vals base-env]
  (if (= (count vars) (count vals))
    (cons (make-frame vars vals) base-env)
    (if (< (count vars) (count vals))
      (err/error "too many arguments supplied -- extend-environment" vars)
      (err/error "too few arguments supplied -- extend-environment" vars))))

(def primitive-procedures
  (list (list 'first first)
        (list 'rest rest)
        (list 'cons cons)
        (list 'nil? nil?)
        (list 'list list)
        (list 'empty? empty?)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        ;; more primitives
        ))

(def primitive-procedure-names
  (map first primitive-procedures))

(def primitive-procedure-objects
  (map (fn [proc] (list 'primitive (stf/second proc)))
       primitive-procedures))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn enclosing-environment [env] (rest env))
(defn copy-environment [e] (doall (map #(atom @%) e)))
(defn environments-equal? [x y] (reduce #(and %1 %2) true (map #(= @%1 @%2) x y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; frame stuff
(defn make-frame [variables values] (atom (zipmap variables values)))
