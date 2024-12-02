(ns xscreenbane.utils.siunits
  (:require [clojure.string :as string]
            [clojure.edn :as edn]))


(defn parse-bytes-str 
  "Expects string like '123 kB' and returns bytes as double"
  [s]
  (let [[number unit] (string/split s #" ")]
    (when-let [number (edn/read-string number)]
      (case unit
        "kB" (* 1024 number)
        "mB" (* 1048576 number)
        "gB" (* 1073741824 number)
        ))))

