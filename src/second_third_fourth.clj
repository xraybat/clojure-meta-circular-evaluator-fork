(ns second-third-fourth)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; store apply from Clojure, we need it to apply primitive
;; procedures. And then declare it as well, otherwise Clojure
;; mistakes apply defined in eval for the Clojure one.
(defn second-from-underlying-lisp [exp] (second exp))

(declare second rest-of-second rest-after-second third rest-after-third flat-third fourth second-from-underlying-lisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; routines to get information out of expressions
(defn second [exp] (first (rest exp)))                      ;; cadr (already exists)
(defn rest-of-second [exp] (rest (first (rest exp))))       ;; cdadr
(defn rest-after-second [exp] (rest (rest exp)))            ;; cddr
(defn third [exp] (first (rest (rest exp))))                ;; caddr
(defn rest-after-third [exp] (rest (rest (rest exp))))      ;; cdddr
(defn flat-third [exp] (first (first (rest exp))))          ;; caadr
(defn fourth [exp] (first (rest (rest (rest exp)))))        ;; cadddr
