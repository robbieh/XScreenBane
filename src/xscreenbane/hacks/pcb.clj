(ns xscreenbane.hacks.pcb
  (:require 
    [xscreenbane.color])
  (:import
    [java.awt.image BufferedImage]
    [java.awt Graphics Graphics2D]
    [java.awt BasicStroke Color RenderingHints]
    [java.awt.geom Arc2D Arc2D$Float])
  )

(def state (atom {}))



(defn set-up-state [^BufferedImage canvas]
  (let [width (.getWidth canvas)
        height (.getHeight canvas)
       ]
    (swap! state assoc :width width)
    (swap! state assoc :height height)
  ))

(defn draw [^BufferedImage canvas]
  (let [g (.getGraphics canvas)])
  )
