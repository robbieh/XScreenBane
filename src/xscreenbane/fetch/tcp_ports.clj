(ns xscreenbane.fetch.tcp-ports
  (:use com.rpl.specter)
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            ))

;TODO: ensure both /proc and OSHI methods for getting data return the same structure

(def open-ports (atom {}))

(def state-map {1 :TCP_ESTABLISHED
                2 :TCP_SYN_SENT
                3 :TCP_SYN_RECV
                4 :TCP_FIN_WAIT1
                5 :TCP_FIN_WAIT2
                6 :TCP_TIME_WAIT
                7 :TCP_CLOSE
                8 :TCP_CLOSE_WAIT
                9 :TCP_LAST_ACK
                10 :TCP_LISTEN
                11 :TCP_CLOSING 
                12 :TCP_NEW_SYN_RECV})

;  sl  local_address rem_address   st tx_queue rx_queue tr tm->when retrnsmt   uid  timeout inode                                                     
;   0: 00000000:D9ED 00000000:0000 0A 00000000:00000000 00:00000000 00000000     0        0 24722 1 0000000000000000 100 0 0 10 0                     
;   1: 0100007F:0CEA 00000000:0000 0A 00000000:00000000 00:00000000 00000000   130        0 23601 1 0000000000000000 100 0 0 10 0                     
(defn parse-proc-tcp-line [line]
  (-> line
        (s/replace #"[ ]+" " ")
        (s/trim )
        (s/split #"[ :]+" )
        (->> (interleave [:index :laddr :lport :raddr :rport :state :txq :rxq :timer 
                          :timerexp :retransmit :uid :timeout :inode :sockrefs :memloc 
                          :retranstimeout :softclock :quickack :congestion :slowstart]))
        (->> (apply assoc { }))
        (->> (transform [:lport] (fn [x] (Integer/parseInt x 16)) ,))
        ))

;Can't use io/reader here. It doesn't understand these kernel files.
(defn linux-proc-get-tcp []
  (for [line (rest (line-seq (io/reader (java.io.FileReader. "/proc/net/tcp"))))]
    (parse-proc-tcp-line line)))

(def ips (-> (oshi.SystemInfo.)
      (.getOperatingSystem)
      (.getInternetProtocolStats)))

(defn oshi-get-tcp []
  (-> ips
      (.getConnections)
      (->> (map bean))))

(defn get-tcp-by-os []
  (case (System/getProperty "os.name")
    "Linux" (linux-proc-get-tcp)
    (oshi-get-tcp)))


(defn get-tcp []
  (if-let [portinfo (get-tcp-by-os)]
    (reset! open-ports portinfo)
    (println "Failed to get TCP port info")))


