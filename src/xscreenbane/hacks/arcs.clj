(ns xscreenbane.hacks.arcs
  (:use com.rpl.specter)
  (:require 
    [xscreenbane.color]
    [xscreenbane.clojure2d-helper :as c2dhelper]
    [xscreenbane.fetch.tcp-ports :as tcpp]
    [clojure2d.color :as color]
    [clojure2d.core :as c2d]
    )
  (:import
    [java.awt.image BufferedImage]
    [java.awt Graphics Graphics2D]
    [java.awt BasicStroke Color RenderingHints]
    [java.awt.geom Arc2D Arc2D$Float]
    )
  )

;arc: {:radius :length :color :cap} ... possibly direction, speed
;
; established |----| main
; syn_sent    <----  main
; listening   --O--  highlight
; close_wait  ----<  alert
; time_wait   -|--|- alert
; closed      ------ inactive

(def hackstate (atom {}))
(def cx 0)
(def cy 0)

(comment 
  (first @tcpp/open-ports))

;need to incorporate new ideas about color schemes, but simple mapping will help with getting this going
(def state-color-map
  {
   "01" Color/GREEN
   "02" Color/CYAN
   "03" Color/CYAN
   "04" Color/YELLOW
   "05" Color/YELLOW
   "06" Color/MAGENTA
   "07" Color/GRAY
   "08" Color/ORANGE
   "09" Color/RED
   "0A" "#00FFDD"
   "0B" Color/ORANGE
   "0C" Color/BLUE
   })

;take a hashmap from tcp-ports module and translate to a visual arc
;have to track arc's position in state atom and update that

(defn update-arc [{:keys [lport state sockrefs] :as portmap}]
  ;(println portmap)
  (if (select-one [ATOM :arcs lport] hackstate)
    ;update it in place
    (let [speed (select-one [ATOM :arcs lport :speed] hackstate)]
      (transform [ATOM :arcs lport :theta] (partial + speed) hackstate)
      )
    ;else add it
    (transform [ATOM :arcs lport]
               (fn [x] (merge {:radius (* (:arcwidth @hackstate) lport)
                :length (* 0.1 (inc (Integer/parseInt sockrefs)))
                :color  (get state-color-map state)
                :cap nil
                :speed (+ 0.001 (* 0.02 (rand)))
                :theta 0 }))
               hackstate)))



(let [open-ports    (set (map :lport @tcpp/open-ports))
      tracked-ports (set (keys (:arcs @hackstate)))
      diff          (clojure.set/difference tracked-ports open-ports)
      closed-ports  (select-keys (:arcs @hackstate) diff)
      now           (System/currentTimeMillis)
      old           (- (System/currentTimeMillis) (* 1000 60))
      closed-ports  (transform [MAP-VALS] #(assoc % :date now) closed-ports)
      ]
    ;(transform [ATOM :arcs MAP-KEYS diff] NONE hackstate)
    ;closed-ports
    ;(transform [ATOM :closed] (fn [m] (merge closed-ports)) hackstate)
    ;(select [ATOM :closed]  hackstate)
    ;(transform [ATOM :closed MAP-VALS :date VAL #(< % old) ] NONE  hackstate)
    ;(select [ATOM :closed MAP-VALS (fn [k] (filterer (< (:date k) old)) ) ] hackstate)
    ;(select [ATOM :closed (fn [k] (filterer (< (:date k) old)) ) ] hackstate)
    ;(transform [ATOM :closed MAP-VALS (fn [k] (filterer (< (:date k) old)) ) ] NONE hackstate) <- this leaves behind the key
    ;(transform [ATOM :closed  (fn [k] (filterer (< (:date (val k)) old)) ) ] NONE hackstate)
    ;(setval [ATOM :closed (selected? MAP-VALS :date #(< % old))] NONE hackstate)
    ;(setval [ATOM :closed (selected? MAP-VALS )] NONE hackstate)
    ;(select [ATOM :closed (selected? MAP-VALS :date #(< % old))] hackstate)
    (setval [ATOM :closed (selected? MAP-VALS :date #(< % old))] NONE hackstate)
    )

(comment count (keys (:arcs @hackstate)))
(comment count (keys (:closed @hackstate)))
(comment (swap! hackstate assoc :closed {}))
(comment (update-all-arcs))

(defn update-all-arcs []
  (tcpp/get-tcp)
  (doseq [arc @tcpp/open-ports ]
    (update-arc arc))
  ;now handle closed ports - they'll be missing from tcpp/open-ports
  ;add them to :closed in @hackstate
  (let [open-ports    (set (map :lport @tcpp/open-ports))
        tracked-ports (set (keys (:arcs @hackstate)))
        diff          (clojure.set/difference tracked-ports open-ports)
        closed-ports  (select-keys (:arcs @hackstate) diff)
        now           (System/currentTimeMillis)
        old           (- (System/currentTimeMillis) (* 1000 60))
        closed-ports  (transform [MAP-VALS] #(assoc % :date now) closed-ports)
        ]
    ;(transform [ATOM :closed ALL ] (fn [m] (merge closed-ports)) hackstate)
    (transform [ATOM :closed] (fn [m] (merge closed-ports)) hackstate) ;add closed-ports to [@hackstate :closed]
    (transform [ATOM :arcs MAP-KEYS diff] NONE hackstate) ;remove the closed ports
    (setval [ATOM :closed (selected? MAP-VALS :date #(< % old))] NONE hackstate) ;remove expired closed ports
    ))

;(deref hackstate)
;(get-in  {:arcs {1 "Abc"}} [:arcs 1])
(defn set-up-state [^BufferedImage canvas]
  (tcpp/get-tcp)
  (let [width (.getWidth canvas)
        height (.getHeight canvas)
   ;     arcs (update-arcs)
       ]
    (alter-var-root #'cx (fn [_] (* 0.5 width)))
    (alter-var-root #'cy (fn [_] (* 0.5 height)))
    (swap! hackstate assoc :width width)
    (swap! hackstate assoc :height height)
    (swap! hackstate merge {:width width
                        :height height
                        :maxradius (* 0.5 height)
                        :arcwidth (/ (* 1 height) 65536)  
                        :arcs {}
                        :closed {}
                        :canvas  (c2dhelper/canvas-from-bufferedimage canvas)
                        })
  ))
;(update-arc (first @tcpp/open-ports))
;(comment apply max (keys (:arcs @hackstate)))
;    (for [{:keys [radius length color theta] :as arc} (:arcs @hackstate)]
;      [radius theta]
;      )

(defn draw [^BufferedImage canvas]
  (update-all-arcs)
  (let [g (.getGraphics canvas)
        canvas (:canvas @hackstate)
        width (:width @hackstate)
        height (:height @hackstate) ]
    (.setStroke g (new BasicStroke 1))
    (.clearRect g 0 0 width height)
    ;(println (count (:arcs @hackstate)))
    (doseq [[_portnum {:keys [radius length color theta]}] (:arcs @hackstate)]
    ;(doseq [port (map :lport @tcpp/open-ports)] 
      ;(let [{:keys [radius length color theta]} (get-in @hackstate [:arcs port])]
      ;(println radius length color theta)
      ;(.setColor g color)
      (c2d/set-color canvas color)
      ;(let [arc (new Arc2D$Float cx cy radius radius theta length Arc2D/OPEN)] (.draw g arc))
      (c2d/arc canvas cx cy radius radius theta length )
      )
    (doseq [[_portnum {:keys [radius length color theta]}] (:closed @hackstate)]
      (c2d/set-color canvas Color/GRAY)
      (c2d/arc canvas cx cy radius radius theta length )
      )
    )
    ;(c2d/arc g cx cy 10 10 0 100 )

  )

;(set (select [ATOM ALL :slowstart] tcpp/open-ports))
;(set (select [ATOM ALL :sockrefs] tcpp/open-ports))
