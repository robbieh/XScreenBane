(ns xscreenbane.hacks.arp-sigils
  (:require [arp-sigils.sigils :as s]
            [arp-sigils.glyphs :as g]
            [arp-sigils.arp :as arp]
            [clojure2d.core :as c2d]
            [clojure2d.color :as color]
            [clojure2d.pixels :as pix]
            [clojure2d.extra.signal :as sig]
            ;[clojure2d.extra.glitch :as glch]
            [clojure2d.extra.overlays :as ov]
            [fastmath.core :as fm]
            ))

(def window)

;modes
;one-sigil
;passing-sigils
;mandala
(def state (atom {:mode :new
                  :exception nil
                  :exception-timeout 0
                  }))
(def sigils (atom {}))
(def sigilmeta (atom {}))
(def errstate (atom {}))
(def modes [:one-sigil :passing-sigils])
(def stroke 3)
(def palette )
(def color-list [:green :lime :forestgreen :springgreen])
(declare bgcanvas)
(declare glchcanvas)
(declare canvas)

;(color/palette (color/gradient [:darkblue [0 50 0] :dark-green]) 100)
;(nth pal (nth (:temp @fetch/forecast) x))

(defn reset-state []
  (reset! state {:mode :new
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
  (doseq [mac @arp/macs
          :let [fullwidth (s/calc-length (get @sigils mac) 0)]]
    (swap! sigils assoc mac (mac->sigil mac))
    (swap! sigilmeta assoc mac {:fullwidth fullwidth})))

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
        exc     (:exception @state)
        message (.getMessage exc)
        traces  (.getStackTrace exc)
        lines   (filter #(re-matches #".*arp_sigils.*" %) (concat (map str traces)))
        tmout   (:exception-timeout @state)
        diff    (max 0 (- tmout (System/currentTimeMillis)))  
        ]
    (cond 
      (= 0 tmout)
      (do
        (swap! state assoc :exception-timeout (+ (* 1000 10) (System/currentTimeMillis)))
        )
      (>= (System/currentTimeMillis) tmout)
      (do
        ;(c2d/with-canvas-> canvas (c2d/line 0 0 1000 1000))
        (reset! errstate @state)
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
                     (c2d/text canvas (:mode @state) (- (:w canvas) 200) (- (:h canvas) (* 2 textsz)))
                     )
    ))

(defn draw-one-sigil [canvas]
  (let [hw  (* 1/2 (:w canvas))
        hh  (* 1/2 (:h canvas))
        cs  (:current-sigil @state)
        fw  (get-in @sigilmeta [cs :fullwidth])
        hfw (* 1/2 fw) ] 
    (c2d/with-canvas [canref canvas]
                     (c2d/set-color canref :lime)
                     ;(c2d/text canref fw 10 10)
                     (c2d/translate canref (- hw hfw) hh)
                     (draw-sigil-pct canref cs 0)
                     )))

(defn mode-mandala []
  (when-not (:current-sigil @state)
    (let [sigilkey (rand-nth (keys @sigils))]
      (swap! state assoc :current-sigil sigilkey)
      (swap! sigils assoc sigilkey (mac->sigil sigilkey))
      ))
  (when-not (:timeout @state)
    (swap! state assoc :timeout (+ (now) (* 1000 60))))
  (when (> (now) (:timeout @state))
    (swap! state assoc :mode :new))
  )

(defn mode-one-sigil []
  (when-not (:current-sigil @state)
    (let [sigilkey (rand-nth (keys @sigils))]
      (swap! state assoc :current-sigil sigilkey)
      (swap! sigils assoc sigilkey (mac->sigil sigilkey))
      ))
  (when-not (:timeout @state)
    (swap! state assoc :timeout (+ (now) (* 1000 60))))
  (when (> (now) (:timeout @state))
    (swap! state assoc :mode :new))
  )

(defn draw-passing-sigils [canvas]
  (doseq [sigilkey (:sigil-set @state)]
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

(defn mode-passing-sigils []
  (when-not (:sigil-set @state)
    (let [sigil-set (random-sample 0.3 (keys @sigils)) 
          sh        (:h canvas)
          sw        (:w canvas)
          ]
      (swap! state assoc :sigil-set sigil-set)
      (doseq [sigilkey sigil-set]
        (let [
              st (inc (rand-int 6))
              ;ms (-> (rand-int 6) inc (* 10))
              ms (* st 10)
              y (rand-int sh)
              len (get-in @sigilmeta [sigilkey :fullwidth])
              x (+ sw (rand-int (* 1/2 len)))
              color (rand-nth color-list)
              ]
          (swap! sigilmeta 
                 (partial merge-with merge) {sigilkey {
                                                       :color color
                                                       :stroke st
                                                       :y y
                                                       :x x
                                                       :speed ( * 1/3 (- 7 st))
                                                       }})
          (swap! sigils assoc sigilkey (with-redefs [g/MS ms g/-MS (- ms)]
                                                    (mac->sigil sigilkey)))
          ))
      ))
  (if (empty? (remove true? (for [k (:sigil-set @state)] 
                              (let [{:keys [x fullwidth]} (get-in @sigilmeta [k])
                                    rpos (+ x fullwidth)
                                    out (< rpos 0)]
                                out))))
    (swap! state assoc :mode :new)))

(defn mode-new 
  "Cleanup state and select new mode randomly"
  []
  (arp/get-macs)
  (update-sigil-map)
  (swap! state dissoc :current-sigil :timeout :sigil-set)
  (let [newmode (rand-nth modes)]
    (swap! state assoc :mode newmode))
  )

(defn draw-postprocess [bgcanvas glchcanvas canvas]
  (when-not (:exception @state)
    ;just blur
    (comment let [bi (:buffer canvas)]
             (-> canvas (pix/set-canvas-pixels! (->> (pix/to-pixels bi)
                                                  (pix/filter-channels (pix/box-blur 3))))))
    ;glitch lines
    ;(comment let [p1 (pix/to-pixels (:buffer canvas))
    ;              p2 (pix/filter-channels (glch/pix2line {:nx 3 :ny 3
    ;                                                      :scale 3 :tolerance 55
    ;                                                      :nseed -1 :whole false
    ;                                                      :shiftx -0.46
    ;                                                      :shifty 0.66}) p1)
    ;              ]
    ;         (-> canvas (pix/set-canvas-pixels! p2)))
    ;overlay
    (when (> (rand) 0.99)
      (let []
        (c2d/with-canvas-> glchcanvas 
          (c2d/image (ov/render-crt-scanlines (:buffer canvas))))
        ))

    )
  )

(defn draw-testbed [canvas]
  )


(def composite-SRC (java.awt.AlphaComposite/getInstance java.awt.AlphaComposite/SRC))
(defn draw [bgcanvas _ _ _]
  (try
    (c2d/with-canvas-> bgcanvas (c2d/set-background :black )) 
    ;(c2d/with-canvas-> canvas (c2d/set-background :black )) 
    (c2d/with-canvas-> canvas 
                       (c2d/set-composite composite-SRC)
                       (c2d/set-background :black 0.0)
                       (c2d/set-composite )) 

    (case (:mode @state)
      :error (draw-error canvas)
      :new (mode-new)
      :one-sigil (do (mode-one-sigil) (draw-one-sigil canvas))
      :passing-sigils (do (mode-passing-sigils) (draw-passing-sigils canvas))
      nil)
    ;(draw-postprocess bgcanvas glchcanvas canvas)
    (draw-testbed canvas)
    ;(c2d/with-canvas-> bgcanvas (c2d/image glchcanvas))
    (c2d/with-canvas-> bgcanvas (c2d/image canvas))
    (catch Exception e
      (do
        (swap! state assoc :exception e)
        (swap! state assoc :mode :error)
        ))))

(defn start [fullscreen? & wharg]
  (arp/get-macs)
  (update-sigil-map)
  (if fullscreen?
    (let [w (c2d/screen-width)
          h (c2d/screen-height)]
      (def bgcanvas (c2d/canvas w h))
      (def glchcanvas (c2d/canvas w h))
      (def canvas (c2d/canvas w h))
      (def weather-canvas (c2d/canvas w h))
      (def window 
        (c2d/show-window {:canvas bgcanvas 
                          :window-name "arp-sigils"
                          :draw-fn draw
                          :fps 30
                          :position [0 0]
                          :w w
                          :h h
                          })))
    (let [[w h] wharg]
      (def bgcanvas (c2d/canvas w h))
      (def glchcanvas (c2d/canvas w h))
      (def canvas (c2d/canvas w h))
      (def window 
        (c2d/show-window {:canvas bgcanvas 
                          :window-name "arp-sigils"
                          :draw-fn draw
                          :fps 30
                          :w w
                          :h h
                          }
                         )))
    )
    (c2d/with-canvas-> bgcanvas (c2d/set-background :black))
    (c2d/with-canvas-> glchcanvas (c2d/set-background :black 0.0))
  )

(comment
  (start false 1200 600)
  (identity @state)
  (keys @sigils)
  (first @sigils)
  (identity @sigilmeta)
  (:sigil-set @state)
  (for [k (keys @sigilmeta)]
    (get-in @sigilmeta [k :x]))
  (swap! state assoc :mode :passing-sigils)
  (swap! state dissoc :current-sigil :timeout :sigil-set)
  (get-in @sigilmeta [(key (first @sigils)) :fullwidth])
  (keys (get @sigilmeta (key (first @sigils))))
  (s/calc-length (get @sigils "e4:b9:7a:92:2e:bc") 0)
  (get-in @sigils [(:current-sigil @state) :fullwidth])
  (type @sigils)
  (identity testsigil)
  (swap! sigils assoc-in [:s1 0 :data :pct] 10)
  (swap! sigils assoc :s1 testsigil)
  (with-redefs [g/MS 20 g/-MS -20] 
               (def testsigil 
                 (s/size-sigil (-> (s/append-glyph [] :join-line)
                                 (s/append-glyph-at-line-end , 0 :one)
                                 (s/append-glyph-at-line-end , 0 :join-line)
                                 (s/append-glyph-at-line-end , 0 :fifteen)
                                 (s/append-glyph-at-line-end , 0 :join-line)
                                 (s/append-glyph-at-line-end , 0 :fifteen)
                                 (s/append-glyph-at-line-end , 0 :join-line)
                                 (s/append-glyph-at-line-end , 0 :fifteen)
                                 (s/attach-child , 1 :fourteen)
                                 (s/attach-child , 3 :fourteen)
                                 (s/attach-child , 5 :eleven)
                                 (s/attach-child , 7 :eight)
                                 ) 0))
               )
  (def gset [:zero :one :two :three :four :five :six :seven :eight :nine :ten :eleven :twelve :thirteen :fourteen :fifteen])
  (def testsigil 
    (s/size-sigil (-> (s/append-glyph [] :join-line)
      (s/append-glyph-at-line-end , 0 (rand-nth gset))
      (s/append-glyph-at-line-end , 0 :join-line)
      (s/append-glyph-at-line-end , 0 (rand-nth gset))
      (s/append-glyph-at-line-end , 0 :join-line)
      (s/append-glyph-at-line-end , 0 (rand-nth gset))
      (s/append-glyph-at-line-end , 0 :join-line)
      (s/append-glyph-at-line-end , 0 (rand-nth gset))
      (s/append-glyph-at-line-end , 0 :join-line)
      (s/append-glyph-at-line-end , 0 (rand-nth gset))
      (s/append-glyph-at-line-end , 0 :join-line)
      (s/append-glyph-at-line-end , 0 (rand-nth gset))
      (s/attach-child , 1 (rand-nth gset))
      (s/attach-child , 3 (rand-nth gset))
      (s/attach-child , 5 (rand-nth gset))
      (s/attach-child , 7 (rand-nth gset))
      (s/attach-child , 9 (rand-nth gset))
      (s/attach-child , 11 (rand-nth gset))
    ) 0))
  (swap! sigils assoc :s1 allsigil)
  (def allsigil
    (s/size-sigil (-> (s/append-glyph [] :zero)
                    (s/append-glyph-at-line-end , 0 :join-line)
                    (s/append-glyph-at-line-end , 0 :one)
                    (s/append-glyph-at-line-end , 0 :join-line)
                    (s/append-glyph-at-line-end , 0 :two)
                    (s/append-glyph-at-line-end , 0 :join-line)
                    (s/append-glyph-at-line-end , 0 :three)
                    (s/append-glyph-at-line-end , 0 :join-line)
                    (s/append-glyph-at-line-end , 0 :four)
                    (s/append-glyph-at-line-end , 0 :join-line)
                    (s/append-glyph-at-line-end , 0 :five)
                    (s/append-glyph-at-line-end , 0 :join-line)
                    (s/append-glyph-at-line-end , 0 :six)
                    (s/append-glyph-at-line-end , 0 :join-line)
                    (s/append-glyph-at-line-end , 0 :seven)
                    (s/append-glyph-at-line-end , 0 :join-line)
                    (s/append-glyph-at-line-end , 0 :eight)
                    (s/append-glyph-at-line-end , 0 :join-line)
                    (s/append-glyph-at-line-end , 0 :nine)
                    (s/append-glyph-at-line-end , 0 :join-line)
                    (s/append-glyph-at-line-end , 0 :ten)
                    (s/append-glyph-at-line-end , 0 :join-line)
                    (s/append-glyph-at-line-end , 0 :eleven)
                    (s/append-glyph-at-line-end , 0 :join-line)
                    (s/append-glyph-at-line-end , 0 :twelve)
                    (s/append-glyph-at-line-end , 0 :join-line)
                    (s/append-glyph-at-line-end , 0 :thirteen)
                    (s/append-glyph-at-line-end , 0 :join-line)
                    (s/append-glyph-at-line-end , 0 :fourteen)
                    (s/append-glyph-at-line-end , 0 :join-line)
                    (s/append-glyph-at-line-end , 0 :fifteen)
                    ) 0))

  (c2d/with-canvas [canref canvas]
                   (line-pct canref 50 0 0 1000 1000))
  
  )
