(ns xscreenbane.hacks.arp-sigils
  (:require [xscreenbane.utils.arp-sigils :as s]
            [xscreenbane.utils.arp-glyphs :as g]
            [xscreenbane.clojure2d-helper :as c2dhelper]
            [xscreenbane.utils.cli :as cli]
            [xscreenbane.fetch.arp :as arp]
            [xscreenbane.color :as xsb-color]
            [clojure2d.core :as c2d]
            [clojure2d.color :as color])
  (:import
    [java.awt.image BufferedImage]))


;modes
;one-sigil
;passing-sigils
;mandala
(def hackstate (atom {
                  :mode :new
                  :exception nil
                  :exception-timeout 0
                  }))
(def sigils (atom {}))
(def sigilmeta (atom {}))
(def errstate (atom {}))
(def modes [:one-sigil :passing-sigils])
(def stroke 3)
(def palette (:greenpunk xsb-color/palettes))
(def color-list [:green :lime :forestgreen :springgreen])

(def hackstate (atom {}))

;(color/palette (color/gradient [:darkblue [0 50 0] :dark-green]) 100)
;(nth pal (nth (:temp @fetch/forecast) x))

(defn reset-state []
  (reset! hackstate {:mode :new
                :exception nil
                :exception-timeout 0}))

(defn now [] (System/currentTimeMillis))

;(def testsigil (mac->sigil (rand-nth @arp/macs)))
(defn mac->sigil 
  "Translates a MAC address string into a sigil"
  [macstr]
  (let [pairs (clojure.string/split macstr #":")
        firsts (map (comp str first) pairs)
        seconds (map (comp str second) pairs)
        fg (mapv g/char-glyph-map firsts)
        sg (mapv g/char-glyph-map seconds)
        ]
    (s/size-sigil (-> (s/append-glyph [] :join-line )
      (s/append-glyph-at-line-end , 0 (nth fg 0))
      (s/append-glyph-at-line-end , 0 :join-line)
      (s/append-glyph-at-line-end , 0 (nth fg 1))
      (s/append-glyph-at-line-end , 0 :join-line)
      (s/append-glyph-at-line-end , 0 (nth fg 2))
      (s/append-glyph-at-line-end , 0 :join-line)
      (s/append-glyph-at-line-end , 0 (nth fg 3))
      (s/append-glyph-at-line-end , 0 :join-line)
      (s/append-glyph-at-line-end , 0 (nth fg 4))
      (s/append-glyph-at-line-end , 0 :join-line)
      (s/append-glyph-at-line-end , 0 (nth fg 5))
      (s/attach-child , 1 (nth sg 0))
      (s/attach-child , 3 (nth sg 1))
      (s/attach-child , 5 (nth sg 2))
      (s/attach-child , 7 (nth sg 3))
      (s/attach-child , 9 (nth sg 4))
      (s/attach-child , 11 (nth sg 5)))
                    0)))

(defn mac->mandala
  "Translates a MAC to a mandala-style sigil"
  [macstr]
  (let [pairs (clojure.string/split macstr #":")
        firsts (map (comp str first) pairs)
        seconds (map (comp str second) pairs)
        fg (mapv g/char-glyph-map firsts)
        sg (mapv g/char-glyph-map seconds)
        ]
  ))

(defn update-sigil-map []
  (if (= 0 (count @arp/macs))  ;put in a single all-zeroes mac if the list is empty
    (let [mac "00:00:00:00:00:00"
          fullwidth (s/calc-length (get @sigils mac) 0)]
      (swap! sigils assoc mac (mac->sigil mac))
      (swap! sigilmeta assoc mac {:fullwidth fullwidth}))
    ;else
    (doseq [mac @arp/macs
          :let [fullwidth (s/calc-length (get @sigils mac) 0)]]
      (swap! sigils assoc mac (mac->sigil mac))
      (swap! sigilmeta assoc mac {:fullwidth fullwidth}))))

(defn negpoint "return negative of x,y in a point" [[x y]]
  [(- x) (- y)])

(defn line-pct 
  "Draw a percentage of a line"
  ([canref pct p1 p2]
   (let [[x1 y1] p1
         [x2 y2] p2]
     (line-pct canref pct x1 y1 x2 y2)))
  ([canref pct x1 y1 x2 y2]
   (let [pct   (* 0.1 pct)
         xdiff (* 0.1 (- x2 x1))
         ydiff (* 0.1 (- y2 y1))
         xnew  (+ x1 (* pct xdiff))
         ynew  (+ y1 (* pct ydiff))
         ]
     (c2d/line canref x1 y1 xnew ynew))))

(defn arc-pct 
  "Draw a percentage of an arc"
  [canref pct x y w h start extent]
  (let [pct    (* 0.01 pct)
        ;diff   (- end start)
        ;newend (+ 0 (* pct diff))
        newextent (* pct extent)
        ]
    (c2d/arc canref x y w h start newextent)))

(defn draw-glyph-pct 
  "Incrementally draw a glyph, returning new percentage done"
  [canref glyph]
  (let [{:keys [parts width in out bbox]} glyph
        pct (get glyph :pct 0)
        [inx iny] in
        [outx outy] out ]
    (c2d/push-matrix canref)
    (c2d/set-stroke canref stroke)
    (doseq [part parts
            :let [kind (first part)
                  params (rest part)]]
      (case kind
        :line  (apply (partial line-pct canref pct) params)
        :point (do (c2d/set-stroke canref (* 3 stroke)) (apply (partial c2d/point canref) params) (c2d/set-stroke canref  stroke))
        :arc   (apply (partial arc-pct canref pct) params)
        ))
    (c2d/pop-matrix canref)
    (inc pct)))

(defn draw-glyph 
  "Draw a glyph"
  [canref glyph]
  (let [{:keys [parts width in out bbox]} glyph
        [inx iny] in
        [outx outy] out ]
    (c2d/push-matrix canref)
    (c2d/set-stroke canref stroke)
    (doseq [part parts
            :let [kind (first part)
                  params (rest part)]]
      (case kind
        :line  (apply (partial c2d/line canref) params)
        :point (apply (partial c2d/point canref) params)
        :arc   (apply (partial c2d/arc canref) params)
        ))
    (c2d/pop-matrix canref)
    ))


(defn draw-sigil-pct 
  "Incrementally draw a sigil, updating :data :pct as it goes"
  [canref sigilkey node]
  (let [sigil  (get @sigils sigilkey)
        me     (get sigil node)
        {:keys [:next :children :data ]} me
        out    (:out data [20, 0])
        width  (:width data 20)  
        pct    (min (:pct data 0) 100)
        n      (get sigil next)
        in     (negpoint (get-in n [:data :in] [0 0]))
        ]
    (c2d/set-stroke canref stroke)
    (draw-glyph-pct canref data )
    (when (>= pct 100 ) 
      (if-not (empty? children)
        (doseq [[child tmatrix] (map vector children (:attach data))
                :let [[x y theta] tmatrix
                      chin (get-in sigil [child :data :in] )]]
          (c2d/push-matrix canref)
          (c2d/translate canref x y)
          (c2d/rotate canref theta)
          (c2d/translate canref (negpoint chin))
          (draw-sigil-pct canref sigilkey child)
          (c2d/pop-matrix canref)
          )))
    (c2d/translate canref out)
    (c2d/translate canref in)
    ;(c2d/translate canref (* 0.5 width) 0)
    (when (and (>= pct 100) next)
        (draw-sigil-pct canref sigilkey next))
    (swap! sigils assoc-in [sigilkey node :data :pct] (inc pct))
    ))

(defn draw-sigil 
  "Draw a full sigil"
  [canref sigilkey node]
  (let [sigil  (get @sigils sigilkey)
        me     (get sigil node)
        {:keys [:next :children :data ]} me
        out    (:out data [20, 0])
        width  (:width data 20)  
        n      (get sigil next)
        in     (negpoint (get-in n [:data :in] [0 0]))
        ]
    (draw-glyph canref data)
    (if-not (empty? children)
      (doseq [[child tmatrix] (map vector children (:attach data))
              :let [[x y theta] tmatrix
                    chin (get-in sigil [child :data :in] )]]
        (c2d/push-matrix canref)
        (c2d/translate canref x y)
        (c2d/rotate canref theta)
        (c2d/translate canref (negpoint chin) )
        (draw-sigil canref sigilkey child)
        (c2d/pop-matrix canref)))
    (c2d/translate canref out)
    (c2d/translate canref in)
    (when next
        (draw-sigil canref sigilkey next))
    ))


(defn draw-error [canvas]
  (let [textsz  15
        exc     (:exception @hackstate)
        message (.getMessage exc)
        traces  (.getStackTrace exc)
        lines   (filter #(re-matches #".*arp_sigils.*" %) (concat (map str traces)))
        lines   (concat lines (map str @hackstate))
        lines   (concat lines (map str @sigils))
        tmout   (:exception-timeout @hackstate)
        diff    (max 0 (- tmout (System/currentTimeMillis)))  
        ]
    (cond 
      (= 0 tmout)
      (do
        (swap! hackstate assoc :exception-timeout (+ (* 1000 10) (System/currentTimeMillis)))
        )
      (>= (System/currentTimeMillis) tmout)
      (do
        ;(c2d/with-canvas-> canvas (c2d/line 0 0 1000 1000))
        (reset! errstate @hackstate)
        (reset-state)
        ) 
      )
    (c2d/with-canvas [canvas canvas]
                     (c2d/set-font canvas "Hack")
                     (c2d/set-font-attributes canvas textsz :bold)
                     ;(c2d/set-background canvas 45 51 45)
                     (c2d/set-color canvas :lime)
                     (c2d/text canvas message 10 textsz)
                     (c2d/text canvas (str diff) (- (:w canvas) 200) (- (:h canvas) textsz))
                     (doall (map-indexed #(c2d/text canvas %2 10 (+ 50 (* textsz %1))) lines))
                     (c2d/text canvas (:mode @hackstate) (- (:w canvas) 200) (- (:h canvas) (* 2 textsz)))
                     )
    ))

(defn draw-one-sigil [canvas]
  (let [hw  (* 1/2 (:w canvas))
        hh  (* 1/2 (:h canvas))
        cs  (:current-sigil @hackstate)
        fw  (get-in @sigilmeta [cs :fullwidth])
        hfw (* 1/2 fw) ] 
    (c2d/with-canvas [canref canvas]
                     (c2d/set-color canref (-> palette :lines :standard :line))
                     ;(c2d/text canref fw 10 10)
                     (c2d/translate canref (- hw hfw) hh)
                     (draw-sigil-pct canref cs 0)
                     )))


(defn mode-one-sigil []
  (when-not (:current-sigil @hackstate)
    (let [sigilkey (rand-nth (keys @sigils))]
      (swap! hackstate assoc :current-sigil sigilkey)
      (swap! sigils assoc sigilkey (mac->sigil sigilkey))
      ))
  (when-not (:timeout @hackstate)
    (swap! hackstate assoc :timeout (+ (now) (* 1000 30))))
  (when (> (now) (:timeout @hackstate))
    (swap! hackstate assoc :mode :new))
  )

(defn draw-passing-sigils [canvas]
  (doseq [sigilkey (:sigil-set @hackstate)]
    (let [sigilm   (get @sigilmeta sigilkey)
          {:keys [x y stroke color speed]} sigilm]
      (c2d/with-canvas [canref canvas]
                       (c2d/translate canref x y)
                       (c2d/push-matrix canref)
                       (c2d/set-color canref color)
                       (with-redefs [stroke stroke]
                         (draw-sigil canref sigilkey 0))
                       (c2d/pop-matrix canref)
                       )
      (swap! sigilmeta assoc-in [sigilkey :x] (- x speed))
      ))
  )

(defn mode-passing-sigils [width height]
  (when-not (:sigil-set @hackstate)
    (let [
          ;setcount  (count @sigils)   
          ;howmany   (-> setcount (* 0.5) int dec rand-int inc)
          ;sigil-set (take howmany (keys @sigils))  
          sigil-set (keys @sigils)
          sh        width
          sw        height
          ]
      (swap! hackstate assoc :sigil-set sigil-set)
      (doseq [sigilkey sigil-set]
        (let [
              st (+ 2 (rand-int 7))
              ;ms (-> (rand-int 6) inc (* 10))
              ms (* st 10)
              y (rand-int sh)
              len (get-in @sigilmeta [sigilkey :fullwidth])
              x (+ sw (rand-int (* st 0.5 len )))
              color (rand-nth color-list)
              ]
          (swap! sigilmeta 
                 (partial merge-with merge) {sigilkey {
                                                       :color color
                                                       :stroke st
                                                       :y y
                                                       :x x
                                                       ;:speed ( * 1/3 (- 7 st))
                                                       :speed (* 0.3 st)
                                                       }})
          (swap! sigils assoc sigilkey (with-redefs [g/MS ms g/-MS (- ms)]
                                                    (mac->sigil sigilkey)))
          ))
      ))
  (when (empty? (remove true? (for [k (:sigil-set @hackstate)] 
                              (let [{:keys [x fullwidth]} (get-in @sigilmeta [k])
                                    rpos (+ x fullwidth)
                                    out (< rpos 0)]
                                out))))
    (swap! hackstate assoc :mode :new)))

(defn mode-new 
  "Cleanup state and select new mode randomly"
  []
  (arp/get-macs)
  (update-sigil-map)
  (swap! hackstate dissoc :current-sigil :timeout :sigil-set)
  (let [newmode (rand-nth modes)
        ]
    (swap! hackstate assoc :mode newmode))
  )



(defn draw-testbed [canvas]
  )

(defn set-up-state [^BufferedImage canvas args]
  (arp/get-macs)
  (update-sigil-map)
  (let [width (.getWidth canvas)
        height (.getHeight canvas)
        
       ]
    (def bgcanvas (c2d/canvas width height))
    (when-let  [palette-from-cli (keyword (cli/pair-get args :palette))]
      ;(println "Arg pal" palette-from-cli)
      (when-let [palette-from-config (get xsb-color/palettes palette-from-cli)]
        ;(println "found pal" palette-from-config)
        (alter-var-root #'palette (fn [_] palette-from-config))
        ))
    
    ;(println "selected palette" palette)
    ;(alter-var-root #'cx (fn [_] (* 0.5 width)))
    ;(alter-var-root #'cy (fn [_] (* 0.5 height)))
    (alter-var-root #'color-list 
                    (fn [_]  
                      (concat 
                        (color/palette (-> palette :gradients :standard) 10)
                        [
                         (-> palette :lines :near :line)
                         (-> palette :lines :alternate :line)
                         (-> palette :lines :highlight :line)
                         ]
                        )
                      ))
    (swap! hackstate assoc :width width)
    (swap! hackstate assoc :height height)
    (swap! hackstate merge {:width width
                        :height height
                        :maxradius (* 0.5 height)
                        :mode :new
                        :exception nil
                        :exception-timeout 0
                        :canvas (c2dhelper/canvas-from-bufferedimage canvas)
                        })
  ))



(defn draw [^BufferedImage _]
  (let
    [canvas (:canvas @hackstate)
     width (:width @hackstate)
     height (:height @hackstate) ]

    (try
      (c2d/set-color canvas (:background palette))
      (c2d/rect canvas 0 0 width height)

      (case (:mode @hackstate)
        :error (draw-error canvas)
        :new (mode-new)
        :one-sigil (do (mode-one-sigil) (draw-one-sigil canvas))
        :passing-sigils (do (mode-passing-sigils width height) (draw-passing-sigils canvas))
        nil)
      (catch Exception e
        (do
          (swap! hackstate assoc :exception e)
          (swap! hackstate assoc :mode :error)
          )))))



