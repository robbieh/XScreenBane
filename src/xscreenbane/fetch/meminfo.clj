(ns xscreenbane.fetch.meminfo
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            ))

;TODO: ensure both /proc and OSHI methods for getting data return the same structure

(def meminfo (atom {}))

"MemTotal:       65767304 kB"

(defn parse-meminfo-line [line]
  (let [[k v] (-> line (s/replace #"[()]+" "_") (s/replace #"[ ]+" " ") (s/trim ) (s/split #": " ))]
    [(keyword k) v]))

;Can't use io/reader here. It doesn't understand these kernel files.
(defn linux-read-meminfo []
  (->> (for [line (rest (line-seq (io/reader (java.io.FileReader. "/proc/meminfo"))))]
         (parse-meminfo-line line)) 
      (into {})))

;(def ips (-> (oshi.SystemInfo.) (.getOperatingSystem) (.getInternetProtocolStats)))

;(defn oshi-get-meminfo []
;  (-> ips
;      (.getConnections)
;      (->> (map bean))))
(defn oshi-get-meminfo []
  (println "oshi meminfo not implemented yet")
  (System/exit 1))

(defn get-meminfo-by-os []
  (case (System/getProperty "os.name")
    "Linux" (linux-read-meminfo)
    (oshi-get-meminfo)))


(defn get-meminfo []
  (if-let [meminfo' (get-meminfo-by-os)]
    (reset! meminfo meminfo')
    (println "Failed to get memory info")))


