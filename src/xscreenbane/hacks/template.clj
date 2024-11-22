(ns xscreenbane.hacks.template
  "Template for new hacks. When XScreenBane launches, it will call
  (set-up-state) in the hack just once. After that, it will call (draw) in a
  loop."
  (:require 
    [xscreenbane.color])
  (:import
    [java.awt.image BufferedImage]
    [java.awt Graphics Graphics2D]
    [java.awt BasicStroke Color RenderingHints]
    [java.awt.geom Arc2D Arc2D$Float])
  )

(def hackstate "State atom for this hack" (atom {}))

(defn set-up-state 
  "Stores state info in @hackstate atom and sets palette if passed from command line."
  [^BufferedImage canvas args]
  (let [width (.getWidth canvas)
        height (.getHeight canvas)
       ]
    (swap! hackstate assoc :width width)
    (swap! hackstate assoc :height height)))

(defn draw 
  "Draws next frame and updates state if needed."
  [^BufferedImage canvas]
  (let [g (.getGraphics canvas)]))
