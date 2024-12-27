(ns xscreenbane.hacks.mandala
  (:require 
    [xscreenbane.utils.cli :as cli]
    [xscreenbane.color :as xsb-color]
    [xscreenbane.fetch.netstat :as netstat]
    [xscreenbane.utils.geom :as geom]
    [xscreenbane.clojure2d-helper :as c2dhelper]
    [amalloy.ring-buffer :refer [ring-buffer]]
    [clojure2d.core :as c2d]
    [clojure2d.color :as c2color]
    )
  (:import [java.awt.image BufferedImage]))

(def hackstate (atom {}))
(def palette (:greenpunk xsb-color/palettes))

(def inoct (atom {:rbuf (ring-buffer 60) :last (->>  @netstat/netstat :IpExt :InOctets read-string)}))
(def outoct (atom {:rbuf (ring-buffer 60) :last (->> @netstat/netstat :IpExt :OutOctets read-string)}))
(def inbcast (atom {:rbuf (ring-buffer 60) :last (->> @netstat/netstat :IpExt :InBcastPkts )}))
(def outbcast (atom {:rbuf (ring-buffer 60) :last (->> @netstat/netstat :IpExt :OutBcastPkts )}))
(def inmcast (atom {:rbuf (ring-buffer 60) :last (->> @netstat/netstat :IpExt :InMcastPkts )}))
(def outmcast (atom {:rbuf (ring-buffer 60) :last (->> @netstat/netstat :IpExt :OutMcastPkts )}))

(defn set-up-state 
  [^BufferedImage canvas args]
  (let [width (.getWidth canvas)
        height (.getHeight canvas)
        canvas (c2dhelper/canvas-from-bufferedimage canvas)
        sides  8]
    (when-let  [palette-from-cli (keyword (cli/pair-get args :palette))]
      (when-let [palette-from-config (get xsb-color/palettes palette-from-cli)] (alter-var-root #'palette (fn [_] palette-from-config))
        ))
    (c2d/set-font canvas "Hack")
    (c2d/set-font-attributes canvas 30)
    (c2d/set-stroke canvas 2)
    (reset! hackstate {:width width 
                       :height height
                       :cx (* 0.5 width)
                       :cy (* 0.5 height)
                       :canvas canvas
                       :sides sides
                       :rings {150 (geom/polygon sides 150)
                               300 (geom/polygon sides 300)
                               450 (geom/polygon sides 450)
                               600 (geom/polygon sides 600)
                               750 (geom/polygon sides 750)
                               900 (geom/polygon sides 900)
                               }
                       })))

(defn ring-shape [pointlist curvature]
  (c2d/path-def->shape (vec (concat [[:move (first pointlist)] ]
   (for [[[x1 y1] [x2 y2]] (partition 2 1 pointlist pointlist)]
    (let [[midx midy]        (geom/line-midpoint x1 y1 x2 y2)
          theta              (geom/x-y-to-theta midx midy)
          from-origin-length (geom/line-length 0 0 midx midy)
          [curvex curvey]    (geom/radius-theta-to-x-y (+ from-origin-length curvature) theta)
            ]
      [:cubic [x1 y1 curvex curvey x2 y2]]
      )
    ))))
  )


(defn radial-gradient-mode [canvas x1 y1 radius fractions-vec color-vec]
  (let [fractions-array (float-array fractions-vec)
        color-array     (into-array java.awt.Color color-vec)
        rgp             (java.awt.RadialGradientPaint. 
                          (float x1) (float y1) (float radius) fractions-array color-array)
        ^java.awt.Graphics2D g (.graphics canvas)]
    (.setPaint g rgp)
    canvas))

(comment
(type (into-array java.awt.Color [(clojure2d.color/awt-color :yellow)])) ; java.awt.Color/1

(type (float-array [(float 0.1)  (float 1.0) (float 3.0)]))
(radial-gradient-mode (:canvas @hackstate) 
                      (float 0.0) (float 0.0) (float 100.0) [(float 0.0) (float 1.0)] 
                      [(clojure2d.color/awt-color :black) (clojure2d.color/awt-color :white)])

(clojure2d.color/awt-color :yellow)

)

(defn draw-ring [pointlist curvature gradcolor]
  (let [{:keys [canvas cx cy]} @hackstate]
    (c2d/reset-matrix canvas)
    (c2d/translate canvas cx cy)
      (let [shape   (ring-shape pointlist curvature)
            gradrad (-> pointlist first first)
            ;[g1 g2] (mapv clojure2d.color/awt-color (-> palette :gradients :standard))
            g2 (clojure2d.color/awt-color gradcolor)
            g1 (clojure2d.color/awt-color [0.0 0.0 0.0])
            ]
      ;(println x1 y1)
     ; (c2d/line canvas x1 y1 x2 y2)
     ; (c2d/line canvas 0 0 midx midy)
      ;(c2d/gradient-mode canvas 0 0 g1 0 100 g2)
      (radial-gradient-mode canvas 0.0 0.0 gradrad [0.0 1.0] [g1 g2])
      (c2d/shape canvas shape false)
      (c2d/paint-mode canvas)
      (c2d/set-color canvas (get-in palette [:lines :standard :line]))
      (c2d/set-color canvas [0.0 0.0 0.0])
      (c2d/shape canvas shape true)
      ))
  )

;(->> (netstat/get-netstat) :IpExt :OutOctets read-string)

; ([:move [0.0 0.0] :non-zero]
;  [:line [10.0 0.0] :non-zero]
;  [:line [10.0 10.0] :non-zero]
;  [:line [0.0 10.0] :non-zero]
;  [:line [0.0 0.0] :non-zero]
;  [:close nil :non-zero])
;(netstat/get-netstat)
;(->> netstat/netstat :TcpExt :OutOctets read-string)

(defn update-rbuf [value bufmap]
    ;(println (:last @bufmap))
    (let [diff (- value (-> bufmap deref :last))
          diff (max diff 1)]
      ;(println "diff:" diff)
      (swap! bufmap merge {:last value
                           :rbuf (conj (:rbuf @bufmap) diff)
                           })
      ;[
      ;(apply max (:rbuf @inoct))
      ;(double (/ (apply + (:rbuf @inoct))   (apply max (:rbuf @inoct)) ))
      (double (/ (apply max (:rbuf @bufmap)) diff  )) 
      ;(double (/ diff                      (apply max (:rbuf @inoct))))
      ;]
      ))

(defn draw 
  [^BufferedImage _]
  (Thread/sleep 250)
  (netstat/get-netstat)
  (let [canvas (:canvas @hackstate)
        width (:width @hackstate)
        height (:height @hackstate) 
        inoctsize  (update-rbuf (->> @netstat/netstat :IpExt :InOctets read-string) inoct)
        outoctsize (update-rbuf (->> @netstat/netstat :IpExt :OutOctets read-string) outoct)
        inbcastsize (update-rbuf (->> @netstat/netstat :IpExt :InBcastPkts ) inbcast)
        outbcastsize (update-rbuf (->> @netstat/netstat :IpExt :OutBcastPkts ) outbcast)
        inmcastsize (update-rbuf (->> @netstat/netstat :IpExt :InBcastPkts ) inmcast)
        outmcastsize (update-rbuf (->> @netstat/netstat :IpExt :OutBcastPkts ) outmcast)
        ring-c (count (:rings @hackstate))
        ring-pal (c2color/palette (-> palette :gradients :standard) ring-c)
        sizes   [inoctsize outoctsize inbcastsize outbcastsize inmcastsize outmcastsize]
        ]
    (c2d/reset-matrix canvas)
    (c2d/set-color canvas (:background palette))
    (c2d/rect canvas 0 0 width height)
    (c2d/set-color canvas (get-in palette [:lines :standard :line]))
    (doseq [[i ring] (map-indexed vector (-> @hackstate :rings vals reverse))]
      (draw-ring ring (+ 0 (nth sizes i) ) (nth ring-pal i)))
    ))


(comment
  
  (repeatedly 10 
     #(do
      (Thread/sleep 1000)
      (update-rbuf (->> (netstat/get-netstat) :IpExt :InOctets read-string) inoct)
      )
    )
  
  (apply max (:rbuf @inoct))

  (def buf (atom (ring-buffer 60)))
  (def lst (atom 0))
  (conj @buf 1) 
  (deref buf)
  (swap! buf conj 1)
  
  (time (reduce max @buf)) ; 316644963575
  (time (apply max @buf))  ; 316644963575
  (->> (netstat/get-netstat) :IpExt :InOctets read-string (swap! buf conj))
  
  
  )
