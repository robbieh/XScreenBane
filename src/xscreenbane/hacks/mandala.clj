(ns xscreenbane.hacks.mandala
  (:use com.rpl.specter)
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

; (def inoct (atom {:rbuf (ring-buffer 60) :last (->>  @netstat/netstat :IpExt :InOctets read-string)}))
; (def outoct (atom {:rbuf (ring-buffer 60) :last (->> @netstat/netstat :IpExt :OutOctets read-string)}))
; (def inbcast (atom {:rbuf (ring-buffer 60) :last (->> @netstat/netstat :IpExt :InBcastPkts )}))
; (def outbcast (atom {:rbuf (ring-buffer 60) :last (->> @netstat/netstat :IpExt :OutBcastPkts )}))
; (def inmcast (atom {:rbuf (ring-buffer 60) :last (->> @netstat/netstat :IpExt :InMcastPkts )}))
; (def outmcast (atom {:rbuf (ring-buffer 60) :last (->> @netstat/netstat :IpExt :OutMcastPkts )}))

(def statlist [[:IpExt :InOctets read-string]
               [:IpExt :OutOctets read-string] 
               [:IpExt :InBcastPkts identity]
               [:IpExt :OutBcastPkts identity]
               [:IpExt :InMcastPkts identity]
               [:IpExt :OutMcastPkts identity]
               ])

(defn mk-stat-data [statlist]
  (netstat/get-netstat)
  (vec (for [[statgroup stat postproc] statlist]
    {:rbuf (ring-buffer 60) 
     :last-val (->> @netstat/netstat statgroup stat postproc)
     :path [statgroup stat postproc]
     })))

(comment
  (reverse (:netstat @hackstate))
  (select [ATOM :netstat ALL] hackstate)
  (conj (butlast (first (select [ATOM :netstat ALL :rbuf ] hackstate))) 1)
  (first (select [ATOM :netstat ALL :rbuf ] hackstate))
  (transform [ATOM :netstat ALL] update-rbuf hackstate)
  )

(defn update-rbuf [{:keys [rbuf path last-val]}]
    (let [[statgroup stat postproc] path
          new-value  (-> @netstat/netstat statgroup stat postproc)
          rest-value (conj rbuf 1)
          diff       (- new-value last-val)
          diff       (max diff 1)
          calculated (double (/ (apply max rest-value) diff)) 
          ]
      {:rbuf (conj rbuf diff)
       :last-val new-value
       :path path
       :calculated calculated}
  ))

;(update-rbuf {:rbuf (-> (ring-buffer 10) (conj 1)) :path [:IpExt :InOctets read-string]})
(defn update-rbuf-old [value bufmap]
    ;(println (:last @bufmap))
    (let [diff (- value (-> bufmap deref :last))
          diff (max diff 1)]
      ;(println "diff:" diff)
      (swap! bufmap merge 
             {:last value
                           :rbuf (conj (:rbuf @bufmap) diff)
                           })
      ;[
      ;(apply max (:rbuf @inoct))
      ;(double (/ (apply + (:rbuf @inoct))   (apply max (:rbuf @inoct)) ))
      (double (/ (apply max (:rbuf @bufmap)) diff  )) 
      ;(double (/ diff                      (apply max (:rbuf @inoct))))
      ;]
      ))

;(defn update-stat-data [statdata]
;  (for [{:keys [rbuf lastval path]} statdata]
;    (let [[statgroup stat postproc] path 
;          value (->> @netstat/netstat statgroup stat postproc)
;          diff  (- value lastval)
;          diff  (max diff 1)
;          ]
;      {:last value
;       :rbuf (conj rbuf diff)})
;      ;(apply max (:rbuf @inoct))
;      ;(double (/ (apply + (:rbuf @inoct))   (apply max (:rbuf @inoct)) ))
;      (double (/ (apply max (:rbuf @bufmap)) diff  )) 
;      ;(double (/ diff                      (apply max (:rbuf @inoct))))
;      ;]
;      )
;    )

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
                       :netstat (mk-stat-data statlist)
                       :next-update-time (+ 1000 (System/currentTimeMillis))
                       :curvatures [0 0 0 0 0 0]
                       :rotations [0 0 0 0 0 0]
                       :rings {150 (geom/polygon sides 150)
                               300 (geom/polygon sides 300)
                               450 (geom/polygon sides 450)
                               600 (geom/polygon sides 600)
                               750 (geom/polygon sides 750)
                               900 (geom/polygon sides 900)
                               }
                       })
     (transform [ATOM :netstat ALL] update-rbuf hackstate)
    ))

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

(defn draw-ring [pointlist curvature rotation gradcolor]
  (let [{:keys [canvas cx cy]} @hackstate]
    (c2d/reset-matrix canvas)
    (c2d/translate canvas cx cy)
    (c2d/rotate canvas rotation)
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

(def decay 0.3)
(defn update-curvature [curvature calculated]
  (if (> curvature 0)
    (min 600
      (+ curvature (- (* 3 decay)) (-> calculated Math/log10 (- 1 ) (* 10))))
    (max -150
      (+ curvature (- decay) (-> calculated Math/log10 (- 1 ) (* 10))))
    )
  )

(:netstat @hackstate)
(:rotations @hackstate)
(:curvatures @hackstate)
(mapv update-curvature 
(:curvatures @hackstate)(select [ATOM :netstat ALL :calculated] hackstate))
(mapv Math/log10 [0 10 100 1000 10000])

(defn draw 
  [^BufferedImage _]
  (when (> (System/currentTimeMillis) (:next-update-time @hackstate))
    (netstat/get-netstat)
    (transform [ATOM :netstat ALL] update-rbuf hackstate)
    (swap! hackstate merge {:next-update-time (+ 250 (System/currentTimeMillis))}))
  (let [{:keys [canvas width height curvatures rotations]} @hackstate
        ; inoctsize  (update-rbuf (->> @netstat/netstat :IpExt :InOctets read-string) inoct)
        ; outoctsize (update-rbuf (->> @netstat/netstat :IpExt :OutOctets read-string) outoct)
        ; inbcastsize (update-rbuf (->> @netstat/netstat :IpExt :InBcastPkts ) inbcast)
        ; outbcastsize (update-rbuf (->> @netstat/netstat :IpExt :OutBcastPkts ) outbcast)
        ; inmcastsize (update-rbuf (->> @netstat/netstat :IpExt :InBcastPkts ) inmcast)
        ; outmcastsize (update-rbuf (->> @netstat/netstat :IpExt :OutBcastPkts ) outmcast)
        sizes  (select [ATOM :netstat ALL :calculated] hackstate)
        updated-curvatures (mapv update-curvature curvatures sizes)
        ring-c (count (:rings @hackstate))
        ring-pal (c2color/palette (-> palette :gradients :standard) ring-c)
        ; sizes   [inoctsize outoctsize inbcastsize outbcastsize inmcastsize outmcastsize]
        ]
    (swap! hackstate assoc :curvatures updated-curvatures)
    ;(swap! hackstate assoc :rotations (mapv + rotations [0.001 0.002 0.003 0.004 0.005 0.006]))
    (swap! hackstate assoc :rotations (mapv + rotations [0.001 -0.001 0.002 -0.002 0.003 -0.003]))
    (c2d/reset-matrix canvas)
    (c2d/set-color canvas (:background palette))
    (c2d/rect canvas 0 0 width height)
    (c2d/set-color canvas (get-in palette [:lines :standard :line]))
    ; (doseq [[i ring] (map-indexed vector (-> @hackstate :rings vals reverse))]
    ;   (draw-ring ring (+ 0 (nth sizes i) ) (nth ring-pal i)))
    (doseq [[i ring] (map-indexed vector (-> @hackstate :rings vals reverse))]
      (draw-ring ring (+ 0 (nth updated-curvatures i) )(nth rotations i) (nth ring-pal i)))
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
