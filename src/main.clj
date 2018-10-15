(ns main)

(require '[environ :as env])
(require '[eval-apply :as e-a])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; meta-circular-evaluator loaded
'METACIRCULAR-EVALUATOR-LOADED
(e-a/driver-loop)