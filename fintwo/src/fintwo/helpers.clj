(ns fintwo.helpers
  (:require [clojure.string :as str]))

(defn process-string [inc-str]                              ;; NOTE I really shouldn't have to do this but whatever.
  (into {}
        (for [[k v]
              (for [ns (str/split inc-str #"&")]
                (str/split ns #"="))]
          [(keyword k) v])))