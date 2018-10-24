(ns driver)
;; this is just a test of driver /environment / frame logic separate to eval / apply
;; real drver-loop lives in eval.clj

(require '[environ :as env])

(declare my-driver-loop the-global-environment prompt-for-input input-prompt output-prompt announce-output user-print)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; code to interact with the evaluator:
(def input-prompt "@input")
(def output-prompt "@value")

(defn my-driver-loop []
  (prompt-for-input input-prompt)
  (let [input (read)]
    (user-print input)
    (let [output (println "eval: " input the-global-environment)]
      (announce-output output-prompt)
      (user-print output)))
  (my-driver-loop))

(defn prompt-for-input [string]
  (newline) (println string) (newline))                     ;;??extra newline??

(defn announce-output [string]
  (newline) (println string) (newline))                     ;;??extra newline??

(defn user-print [object]
    (println "user-print: " object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def the-global-environment (env/setup-environment))

;; test; see main.clj for real call to driver loop
;;(comment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; meta-circular-evaluator loaded
'METACIRCULAR-EVALUATOR-LOADED
(my-driver-loop)
;;)