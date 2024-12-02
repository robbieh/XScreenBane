(ns xscreenbane.hacks.arcs
  (:use com.rpl.specter)
  (:require 
    [xscreenbane.utils.cli :as cli]
    [xscreenbane.color :as xsb-color]
    [xscreenbane.clojure2d-helper :as c2dhelper]
    [xscreenbane.fetch.tcp-ports :as tcpp]
    [clojure2d.core :as c2d]
    )
  (:import
    [java.awt.image BufferedImage]
  ;  [java.awt Graphics Graphics2D]
  ;  [java.awt BasicStroke Color RenderingHints]
  ;  [java.awt.geom Arc2D Arc2D$Float]
    )
  )

;arc: {:radius :length :color :cap} ... possibly direction, speed

(def palette (:greenpunk xsb-color/palettes))
(def hackstate (atom {}))
(def cx 0)
(def cy 0)

(comment 
  (first @tcpp/open-ports))


(def state-color-map
  {
   "01" (get-in palette [:lines :standard :line] )
   "02" (get-in palette [:lines :alert :line] )
   "03" (get-in palette [:lines :alert :line] )
   "04" (get-in palette [:lines :alternate :line] )
   "05" (get-in palette [:lines :alternate :line] )
   "06" (get-in palette [:lines :alternate :line] )
   "07" (get-in palette [:gradients :new-old 1] )
   "08" (get-in palette [:lines :highlight :line] )
   "09" (get-in palette [:lines :alternate :line] )
   "0A" (get-in palette [:lines :near :line] )
   "0B" (get-in palette [:lines :highlight :line] )
   "0C" (get-in palette [:lines :warning :line] )
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
               (fn [x] (merge 
                         {:radius (* (:arcwidth @hackstate) lport)
                          :length (* 0.1 (inc (Integer/parseInt sockrefs)))
                          :color  (get state-color-map state)
                          :lport lport
                          :portstate state
                          :speed (if (= "0A" state) 
                                   (- (+ 0.001 (* 0.01 (rand))))
                                   (+ 0.001 (* 0.01 (rand)))
                                   )
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
        old           (- now (* 10000 60 ))
        closed-ports  (transform [MAP-VALS] #(assoc % :date now) closed-ports)
        ]
    ;(transform [ATOM :closed ALL ] (fn [m] (merge closed-ports)) hackstate)
    (transform [ATOM :closed] (fn [m] (merge closed-ports)) hackstate) ;add closed-ports to [@hackstate :closed]
    (transform [ATOM :arcs MAP-KEYS diff] NONE hackstate) ;remove the closed ports
    (setval [ATOM :closed (selected? MAP-VALS :date #(< % old))] NONE hackstate) ;remove expired closed ports
    ))

;(deref hackstate)
;(get-in  {:arcs {1 "Abc"}} [:arcs 1])
(defn set-up-state [^BufferedImage canvas args]
  (tcpp/get-tcp)
  (let [width (.getWidth canvas)
        height (.getHeight canvas)
        canvas (c2dhelper/canvas-from-bufferedimage canvas)
       ]
    ;(println "Args"  args)
    (when-let  [palette-from-cli (keyword (cli/pair-get args :palette))]
      ;(println "Arg pal" palette-from-cli)
      (when-let [palette-from-config (get xsb-color/palettes palette-from-cli)]
        ;(println "found pal" palette-from-config)
        (alter-var-root #'palette (fn [_] palette-from-config))
        (alter-var-root #'state-color-map (fn [_] {
                                             "01" (get-in palette [:lines :standard :line] )   ;
                                             "02" (get-in palette [:lines :alert :line] )      ;
                                             "03" (get-in palette [:lines :alert :line] )
                                             "04" (get-in palette [:lines :alternate :line] )
                                             "05" (get-in palette [:lines :alternate :line] )
                                             "06" (get-in palette [:lines :alternate :line] )
                                             "07" (get-in palette [:gradients :new-old 1] )
                                             "08" (get-in palette [:lines :highlight :line] )
                                             "09" (get-in palette [:lines :alternate :line] )
                                             "0A" (get-in palette [:lines :near :line] )
                                             "0B" (get-in palette [:lines :highlight :line] )
                                             "0C" (get-in palette [:lines :warning :line] )
                                             } ))))
    
    ;(println "selected palette" palette)
    (c2d/set-font canvas "Hack")
    (c2d/set-font-attributes canvas 30)
    (c2d/set-stroke canvas 2)
    (alter-var-root #'cx (fn [_] (* 0.5 width)))
    (alter-var-root #'cy (fn [_] (* 0.5 height)))
    (swap! hackstate assoc :width width)
    (swap! hackstate assoc :height height)
    (swap! hackstate merge {:width width
                        :height height
                        :maxradius (* 0.5 height)
                        :arcwidth (double (/ (* 1 height) 65536)  )
                        :arcs {}
                        :closed {}
                        :canvas  canvas
                        :legend-endtime (+ (System/currentTimeMillis) (* 1000 60))
                        })
  ))
;(update-arc (first @tcpp/open-ports))
;(comment apply max (keys (:arcs @hackstate)))
;    (for [{:keys [radius length color theta] :as arc} (:arcs @hackstate)]
;      [radius theta]
;      )

(defn draw-label [^clojure2d.core.Canvas canvas color text x y texth]
  (c2d/set-color canvas (get-in palette [:lines :near :line]))
  ;(c2d/rect canvas x y texth texth true)
  (c2d/text canvas text (+ x texth) (+ y texth))
  (c2d/set-color canvas (get state-color-map color))
  (c2d/rect canvas (+ x 2 ) (+ y 2) (- texth 2) (- texth 2 ))
  )

(defn draw-legend [^clojure2d.core.Canvas canvas]
  (let [height (:height @hackstate)
        x      10
        texth  (nth (c2d/text-bounding-box canvas "Xy") 3)
        ytop   (- height (* 9 texth))
        ys     (range ytop height texth)
        labels ["Established" "SYN sent/received" "FIN wait 1/2" "Time wait" "Closed"
   "Close wait / closing" "Last ACK" "Listening" "New SYN received"]
        colors ["01" "02" "04" "06" "07" "08" "09" "0A" "0C"]

        ]
    (doall(map draw-label
        (repeat canvas) colors labels (repeat x) ys (repeat texth)
      ))
    )
  )

(defn draw [^BufferedImage _]
  (update-all-arcs)
  (let [canvas (:canvas @hackstate)
        width (:width @hackstate)
        height (:height @hackstate) ]
    ;(.setStroke g (new BasicStroke 1))
    ;(.clearRect g 0 0 width height)
    (c2d/set-color canvas (:background palette))
    (c2d/rect canvas 0 0 width height)
    ;(println (count (:arcs @hackstate)))
    (doseq [[_portnum {:keys [radius length color theta lport portstate]}] (:arcs @hackstate)
            :let [x (+ cx (* 0.5 radius (Math/cos theta)))
                  y (+ cy (* -0.5 radius (Math/sin theta)))
                  ]]
    ;(doseq [port (map :lport @tcpp/open-ports)] 
      ;(let [{:keys [radius length color theta]} (get-in @hackstate [:arcs port])]
      ;(println radius length color theta)
      ;(.setColor g color)
      (c2d/set-color canvas color)
      ;(let [arc (new Arc2D$Float cx cy radius radius theta length Arc2D/OPEN)] (.draw g arc))
      (c2d/arc canvas cx cy radius radius theta length )
      ;(println lport (double radius) theta x y)
      ;
      ; 1 established |----| main
      ; 2 syn_sent    <----  main
      ; 10 listening   --O--  highlight
      ; 8 close_wait  ----<  alert
      ; 6 time_wait   -|--|- alert
      ; closed      ------ inactive
      (case portstate
        "02" (c2d/ellipse canvas x y 6 6 )
        "03" (c2d/ellipse canvas x y 6 6 )
        "09" (let [
                   x2 (+ cx (* 0.5 radius (Math/cos (+ 0.01 theta))))
                   y2 (+ cy (* -0.5 radius (Math/sin (+ 0.01 theta))))
                   x3 (+ cx (* 0.5 radius (Math/cos (+ 0.02 theta))))
                   y3 (+ cy (* -0.5 radius (Math/sin (+ 0.02 theta))))
                   ]
               (c2d/ellipse canvas x y 6 6 )
               (c2d/ellipse canvas x2 y2 6 6 )
               (c2d/ellipse canvas x3 y3 6 6 )
               )
        nil
        )
      )
    (doseq [[_portnum {:keys [radius length color theta]}] (:closed @hackstate)
            ]
      (c2d/set-color canvas (get-in palette [:gradients :new-old 1]))
      (c2d/arc canvas cx cy radius radius theta length )
      )
    (when (> (:legend-endtime @hackstate) (System/currentTimeMillis))
      (draw-legend canvas))
    )
    ;(c2d/arc g cx cy 10 10 0 100 )
  )

;(type (:canvas @hackstate))
;(set (select [ATOM ALL :slowstart] tcpp/open-ports))
;(set (select [ATOM ALL :sockrefs] tcpp/open-ports))
