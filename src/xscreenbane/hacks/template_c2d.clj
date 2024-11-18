(ns xscreenbane.hacks.template-c2d
  (:require 
    [xscreenbane.utils.cli :as cli]
    [xscreenbane.color :as xsb-color]
    [xscreenbane.clojure2d-helper :as c2dhelper]
    [clojure2d.core :as c2d]
    )
  (:import [java.awt.image BufferedImage]))

(def hackstate (atom {}))
(def palette (:greenpunk xsb-color/palettes))

(defn set-up-state [^BufferedImage canvas args]
  (let [width (.getWidth canvas)
        height (.getHeight canvas)
        canvas (c2dhelper/canvas-from-bufferedimage canvas)
       ]
    (when-let  [palette-from-cli (keyword (cli/pair-get args :palette))]
      (when-let [palette-from-config (get xsb-color/palettes palette-from-cli)]
        (alter-var-root #'palette (fn [_] palette-from-config))
        ))
    (c2d/set-font canvas "Hack")
    (c2d/set-font-attributes canvas 30)
    (c2d/set-stroke canvas 2)
    (reset! hackstate {:width width 
                   :height height})
  ))

(defn draw [^BufferedImage _]
  (let [canvas (:canvas @hackstate)
        width (:width @hackstate)
        height (:height @hackstate) ]
    (c2d/set-color canvas (:background palette))
    (c2d/rect canvas 0 0 width height)
    )
  )
