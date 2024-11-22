(ns xscreenbane.color
  (:use com.rpl.specter)
  (:require [clojure.set]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure2d.color :as c2color])
  (:import [java.io PushbackReader]))


;(edn/read-string {:readers {'xsc/color c2color/color }} 
;"{:line #xsc/color \"#00f000\"}")

;(edn/read-string {:readers {'color identity}} "#color 'test'")
;(Color/decode "000000")


(def palettes 
  "Map of palettes merged from internal resources and local config files"
  (let [config-files [
                     (io/file (System/getenv "HOME") ".xscreenbane-palettes.edn")
                     (io/file (System/getenv "HOME") ".config" "xscreenbane" "palettes.edn")
                     (io/file (System/getenv "XDG_CONFIG_HOME") "xscreenbane" "palettes.edn")
                   ]
        resource-config (-> "palettes.edn" io/resource io/reader PushbackReader. edn/read)
        ]
    (println "color config" config-files)
    (println "color config exists" (->> config-files (filter #(.exists %))))
  (->> config-files 
     (filter #(.exists %))
     (mapv io/reader)
     (mapv #(PushbackReader. %))
     (mapv edn/read)
     ;add a spec step here, once the spec is known
     (apply merge resource-config)
     (transform [(recursive-path [] nxt (if-path map? [MAP-VALS nxt] [STAY])) (if-path vector? [ALL] [STAY])] c2color/color)
     )))
