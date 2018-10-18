(ns apply)

(require '[environ :as env])
(require '[parser :as p])
(require '[error :as err])

(declare my-apply)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; my-apply
(defn my-apply [procedure arguments]
  (cond (p/primitive-procedure? procedure)
        (p/apply-primitive-procedure procedure arguments)
        (p/compound-procedure? procedure)
        (p/eval-sequence
          (p/procedure-body procedure)
          (env/extend-environment (p/procedure-parameters procedure)
                                  arguments
                                  (p/procedure-environment procedure)))
        :else (err/error "unknown procedure type -- my-apply" procedure)))

