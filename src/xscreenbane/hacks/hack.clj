(ns xscreenbane.hacks.hack
  (:import [java.awt.image BufferedImage]))

(defn set-up-state [^BufferedImage bi args])
(defn draw [^BufferedImage bi]
  (println "you shouldn't see this")
  (System/exit 3))
