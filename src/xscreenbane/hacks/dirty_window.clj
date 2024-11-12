(ns xscreenbane.hacks.dirty-window
  (:require
    [xscreenbane.color :as c]
    [xscreenbane.clojure2d-helper :as c2dhelper]
    [clojure2d.color :as color]
    [clojure2d.core :as c2d]
    )
  (:import
    [java.awt.image BufferedImage]
    )
  )

(def state (atom {}))

(defn make-boxes [boxsize scrw scrh gap colors]
  (let [gap 0
        xcount (Math/floor (/ scrw (+ boxsize gap)))
        ycount (Math/floor (/ scrh (+ boxsize gap)))
        direction [:NE :NW :SE :SW]
        ]
    (println (map #(double (* boxsize %)) (range #_(* 0.5 gap) xcount)))
    (println (map #(double (* boxsize %)) (range #_(* 0.5 gap) ycount)))
    (for [x (map #(double (* boxsize %)) (range #_(* 0.5 gap) xcount))
          y (map #(double (* boxsize %)) (range #_(* 0.5 gap) ycount))]
      {:x x :y y :start (rand-nth direction) :color (rand-nth colors) })))

(defn set-up-state [^BufferedImage canvas args]
  (let [width    (.getWidth canvas)
        height   (.getHeight canvas)
        boxsize  (/ (min width height) 20)
        gap      2
        ;gradient (mapv #(color/set-alpha % 200) (:greens c/gradients))
        bgcolor  (get-in c/palettes [:greenpunk :background])
        gradient (get-in c/palettes [:greenpunk :gradients :standard] )
        colors   (color/palette (color/gradient gradient) 10)
        boxes    (make-boxes boxsize width height gap colors)
       ]
    (swap! state merge
      {:width   width
       :height  height
       :boxsize boxsize
       :colors  colors
       :boxes   boxes
       :bgcolor bgcolor
       :canvas  (c2dhelper/canvas-from-bufferedimage canvas)})
  ))

; (c2d/with-canvas [c (:canvas @state)]
;   (println (.graphics c))
;   (.setComposite ^Graphics2D (.graphics c) java.awt.AlphaComposite/Src))

(defn gradient-calc [x1 y1 size direction]
  (let [x2 (+ x1 size)
        y2 (+ y1 size)
        ]
  (case direction
    :NE [x2 y1 x1 y2]
    :NW [x1 y1 x2 y2]
    :SE [x2 y2 x1 y1]
    :SW [x1 y2 x2 y1]
    )))

(defn draw [^BufferedImage canvas]
  (let [c (:canvas @state)
        w (:width @state)
        h (:height @state)
        ]
      (c2d/set-background c (:bgcolor @state)  255)
      (c2d/set-color c :black)
      (c2d/set-stroke c 3)
      ;(c2d/set-color c color 255)
      (doseq [{:keys [x y start color]} (:boxes @state)]
        (let [boxsize (:boxsize @state)
              [x1 y1 x2 y2] (gradient-calc x y boxsize start)
              ]
        (c2d/gradient-mode c x1 y1 color x2 y2 :black)
        (c2d/rect c x y boxsize boxsize)
        (c2d/paint-mode c)
        (c2d/set-color c :black)
        (c2d/rect c x y boxsize boxsize true)
        ))))

