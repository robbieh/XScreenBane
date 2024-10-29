(ns xscreenbane.color
  (:require [clojure.set]
            [xscreenbane.config :refer [config]]))


(def gradients "map containing gradients in form ..." ;TODO
               {
                :grayscale [[0 0 0] [255 255 255]]
                :greens    [[127 255 0] [0 255 127]]
                :greenpunk []

                })


(def palettes  "map containing palettes in form ..." ;TODO
               {
                :greens    []
                :greenpunk []

               })
(def color-sets "List of keywords representing entires in both `gradients` and `palettes`"
  (let [gs (keys gradients)
        ps (keys palettes)]
    (clojure.set/intersection (set gs) (set ps))))


