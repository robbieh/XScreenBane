(ns xscreenbane.utils.xscreensaver-config
  (:require [clojure.java.io :as io]
            [clojure.string :as s])
  )


(def raw 
  (-> "HOME" System/getenv (io/file ".xscreensaver") slurp))

;(-> raw (s/split #"\n"))
