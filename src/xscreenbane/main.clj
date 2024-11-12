(ns xscreenbane.main
  (:require [xscreenbane.core :as core])
  (:gen-class))


(defn -main [& args]
  (when-not (System/getenv "DISPLAY")
    (println "Please set the $DISPLAY")
    (System/exit 1))

  (when (< (count args) 1)  
    (println "Please name a hack")
    (System/exit 2))

  (core/run args))
