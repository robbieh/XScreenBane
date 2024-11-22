(ns xscreenbane.hacks.template-c2d
  "Template for new hacks. This one includes Clojure2D. When XScreenBane
  launches, it will call (set-up-state) in the hack just once. After that, it
  will call (draw) in a loop."
  (:require 
    [xscreenbane.utils.cli :as cli]
    [xscreenbane.color :as xsb-color]
    [xscreenbane.clojure2d-helper :as c2dhelper]
    [clojure2d.core :as c2d])
  (:import [java.awt.image BufferedImage]))

(def hackstate "State atom for this hack" (atom {}))
(def palette "Default palette, to be overridden later if needed" (:greenpunk xsb-color/palettes))

(defn set-up-state 
  "Stores state info in @hackstate atom and sets palette if passed from command line."
  [^BufferedImage canvas args]
  (let [width (.getWidth canvas)
        height (.getHeight canvas)
        canvas (c2dhelper/canvas-from-bufferedimage canvas)]
    (when-let  [palette-from-cli (keyword (cli/pair-get args :palette))]
      (when-let [palette-from-config (get xsb-color/palettes palette-from-cli)]
        (alter-var-root #'palette (fn [_] palette-from-config))
        ))
    (c2d/set-font canvas "Hack")
    (c2d/set-font-attributes canvas 30)
    (c2d/set-stroke canvas 2)
    (reset! hackstate {:width width 
                       :height height
                       :canvas canvas})))

(defn draw 
  "Draws next frame and updates state if needed."
  [^BufferedImage _]
  (let [canvas (:canvas @hackstate)
        width (:width @hackstate)
        height (:height @hackstate) ]
    (c2d/set-color canvas (:background palette))
    (c2d/rect canvas 0 0 width height)))
