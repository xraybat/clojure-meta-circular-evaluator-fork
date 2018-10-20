(ns driver)

(require '[environ :as env])
;;(require '[parser :as p])
(require '[eval :as e])

(declare driver-loop the-global-environment prompt-for-input input-prompt output-prompt announce-output user-print)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; code to interact with the evaluator:
(def input-prompt "@input")
(def output-prompt "@value")

(defn driver-loop []
  (prompt-for-input input-prompt)
  (let [input (read)]
    (user-print input)
    (let [output (e/my-eval input the-global-environment)]
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(defn prompt-for-input [string]
  (newline) (println string) (newline))

(defn announce-output [string]
  (newline) (println string) (newline))

(defn user-print [object]
  (if (e/compound-procedure? object)
    (println (list 'compound-procedure
                   (e/procedure-parameters object)
                   (e/procedure-body object)
                   '<procedure-env>))
    (println object)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def the-global-environment (env/setup-environment))

;; test; see main.clj for real call to driver loop
(comment
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; meta-circular-evaluator loaded
  'METACIRCULAR-EVALUATOR-LOADED
  (driver-loop)
)