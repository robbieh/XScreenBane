(ns xscreenbane.utils.arp-glyphs
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as fm]))

(defn scale [x]
  (* 10 x))

;MS - minimum size
(def MS 40)
(def -MS (- MS))

(defn empty-glyph [name] {:name name })

(defn attach-glyph [base addition]
  (let [acount (count (:attach base))]
    (assoc base :subglyphs (vec (repeat acount addition)))))

(defn attach-out-glyph [base addition]
  (assoc base :outglyph addition))

;(defn attach-glyphs-by-line [a b]
;  (let [join (join-line)]
;    (attach-out-glyph a (attach-out-glyph join b))))


(defn size-glyph [glyph]
  (let [theta     (:rotation glyph)
        [x y]     (:in glyph)
        bb        (:bbox glyph) 
        subglyphs (:subglyphs glyph)
        sizefn    (:sizefn glyph)]
    (if subglyphs 
      (let [sizedsubs (map size-glyph subglyphs)
            sized     (sizefn glyph sizedsubs)]
        (merge glyph sized {:subglyphs sizedsubs}))
      glyph
      )))

(defn rotate-shape-at [shape x y theta]
    (.createTransformedShape (java.awt.geom.AffineTransform/getRotateInstance  theta x y) 
    shape))
;(get-rotated-bounds 10 10 fm/QUARTER_PI)
;(get-rotated-bounds 10 10 fm/HALF_PI)
(defn get-rotated-bounds [w h theta]
  (let [bounds (.getBounds (rotate-shape-at (c2d/crect-shape 0 0 w h) 0 0 theta))]
    [(.getWidth bounds) (.getHeight bounds)]))

;(attach-rect2d-at (c2d/rect-shape -10 -10 20 20) 0 -10 -10 0 fm/-HALF_PI)
(defn attach-rect2d-at [rect inx iny ax ay theta]
  (let [tform (doto (java.awt.geom.AffineTransform.)  
                    (.translate (- inx) (- iny))
                    (.rotate theta)
                    (.translate ax ay)
                )
        path  (.createTransformedShape tform rect)
        ]
    (.getBounds path)
    )
    )

(defn bbox->wh [bbox]
  (let [[x1 y1 x2 y2] bbox] 
    [(- x2 x1) (- y2 y1)]))

(defn rect2d->corners [rect]
  [(.getX rect) 
   (.getY rect)
   (+ (.getX rect) (.getWidth rect))
   (+ (.getY rect) (.getHeight rect))
   ])

(defn union-bboxes [& bboxes]
  (->> bboxes
    (mapv (partial apply c2d/rect-shape) , )
    (reduce #(.createUnion %1 %2) ,)
    rect2d->corners))

(defn divide-line 
  ([p1 p2 divisions]
   (let [[x1 y1] p1
         [x2 y2] p2]
     (divide-line x1 y1 x2 y2 divisions)))
  ([x1 y1 x2 y2 divisions]
   (let [r     (map #(double ( / % divisions)) (range 1 divisions))
         xdiff (- x2 x1)
         ydiff (- y2 y1)
         xs (map #(- x2 (* % xdiff)) r)
         ys (map #(- y2 (* % ydiff)) r) ]
     (mapv vector xs ys))))


(def child-count-map 
  {:join-line 0
   :zero 1
   :one 1
   :two 2
   :three 3
   :four 4
   :five 1
   :six 2
   :seven 3
   :eight 4
   :nine 1
   :ten 2
   :eleven 3
   :twelve 4
   :thirteen 1
   :fourteen 2
   :fifteen 3
   })

(def char-glyph-map
  {"0" :zero
   "1" :one
   "2" :two
   "3" :three
   "4" :four
   "5" :five
   "6" :six
   "7" :seven
   "8" :eight
   "9" :nine
   "a" :ten
   "b" :eleven
   "c" :twelve
   "d" :thirteen
   "e" :fourteen
   "f" :fifteen})

(defn size-join-line [_]
  {:parts [[:line (* -1/2 MS) 0 (* 1/2 MS) 0]]
   :width (* 1 MS)
   :in [(* -1/2 MS)0]
   :out [(* 1/2 MS)0]
   :bbox (c2d/rect-shape (* -1/2 MS)(* -1/2 MS)(* 1/2 MS)(* 1/2 MS))
   })

(defn size-zero [children]
  (let [childbb (-> children first :bbox)
        w       (if childbb (.getWidth childbb) MS)
        h       (if childbb (.getHeight childbb) MS)
        myw     (max w h)
        hmyw    (* 0.5 myw)]
    {:parts [[:arc 0 0 myw myw (fm/radians 180.0) (fm/radians 360)]]
     :width myw
     :in [(- hmyw) 0]
     :out [hmyw 0]
     :bbox (c2d/crect-shape 0 0  myw myw)
     :attach [[0 hmyw (fm/radians 90)]] }))

(defn size-one [children]
  (let [childbb (-> children first :bbox)
        h       (if childbb (.getWidth childbb) MS)
        w       (if childbb (.getHeight childbb) MS)
        myw     (max MS w)
        myh     (* 2 w)
        totw    (max myw w)
        toth    (+ myh h)
        hmyw    (* 1/2 myw)
        hmyh    (* 1/2 myh)
        tmyh    (* 1/3 myh)
        center  [0 0]
        coreN   [0 (- tmyh)]
        ]
    {:parts [
             [:arc 0 0  myw hmyh 0 fm/PI ]
             [:point 0 0]
             [:line center coreN]
             ]
     :width totw
     :in [(- hmyw) 0]
     :out [hmyw 0]
     :bbox (c2d/crect-shape 0 0  totw toth)
     :attach [[0 (- tmyh) fm/-HALF_PI]] }))

(defn size-two [children]
  (let [childbbs (map :bbox children)
        chs      (mapv #(.getWidth %) childbbs)
        cws      (mapv #(.getHeight %) childbbs)
        myw     (* 1 MS)
        myh     (* 1 MS)
        totw    (apply max (conj cws myw))
        toth    (+ myh (apply max (conj chs myh)))
        hmyw    (* 1/2 myw)
        hmyh    (* 1/2 myh)
        fmyw    (* 1/5 myw)
        fmyh    (* 1/5 myh)
        center  [0 0]
        coreN   [0 (- hmyh)]
        coreS   [0  hmyh]
        ]
    {:parts [[:line (- hmyw) 0 (* -2 fmyw) 0]
             [:line center coreN]
             [:line center coreS]
             ;[:line 0 (- hmyh) 0 hmyh] 
             [:arc (- hmyw) 0 (* 3 fmyw) (* 3 fmyh)
              (fm/radians 90) (fm/radians 180)]
             [:arc hmyw 0 (* 3 fmyw) (* 3 fmyh)
              (fm/radians 90) (- (fm/radians 180))]
             [:point 0 (- hmyh)]
             [:point [fmyw fmyh]] [:point [(- fmyw) (- fmyh)]]
             [:line (* 2 fmyw) 0 hmyw 0 ]
             ]
     :width totw
     :in [(- hmyw) 0]
     :out [hmyw 0]
     :bbox (c2d/crect-shape 0 0  totw toth)
     :attach [[0 (- hmyh) fm/-HALF_PI] [0 hmyh fm/HALF_PI]] }))    

(defn old-size-three [children]
  (let [childbbs (map :bbox children)
        cws      (mapv #(.getWidth %) childbbs)
        chs      (mapv #(.getHeight %) childbbs)
        thetas   [(fm/radians 315) (fm/radians 225) (fm/radians 90)]
        newbb    (mapv #(get-rotated-bounds %1 %2 %3 ) cws chs thetas)
        newws    (mapv first newbb)
        newhs    (mapv second newbb)
        myw     (+ 0 (reduce + (take 2 newws)))
        myh     (+ 0 (reduce + (take-last 2 newhs)))
        ;combyw  (apply max (conj cws myw))
        ;combyh  (apply + (conj chs myh))
        hmyw    (* 0.5 myw)
        hmyh    (* 0.5 myh)
        ]
    {:parts [[:line (- hmyw) 0 0 0]
             [:line 0 0 0 hmyh] [:line 0 0 (- hmyw) (- hmyh)] [:line 0 0 hmyw (- hmyh)]
             [:point (- hmyw) (- hmyh)] [:point hmyw (- hmyh)] [:point 0 hmyh]
             [:line 0 0 hmyw 0]
             ]
     :width myw
     :in [(- hmyw) 0]
     :out [hmyw 0]
     :bbox (c2d/crect-shape 0 0  myw myh)
     :attach [
              [(- hmyw) (- hmyh) (fm/radians 225)]
              [hmyw (- hmyh) (fm/radians 315)]
              [0 hmyh (fm/radians 90)]
              ] }))

(defn size-three [children]
  (let [childbbs (map :bbox children)
        cws     (mapv #(.getWidth %) childbbs)
        chs     (mapv #(.getHeight %) childbbs)
        thetas  [(fm/radians 120) (fm/radians 90) (fm/radians 60)]
        newbb   (mapv #(get-rotated-bounds %1 %2 %3 ) cws chs thetas)
        newws   (mapv first newbb)
        newhs   (mapv second newbb)
        newmaxw (apply max (conj cws 0))
        newmaxh (apply max (conj chs 0))
        totw    (max (reduce + newhs) MS)
        myw     (max (* 2/3 totw) MS)
        myh     myw
        toth    (+ myh  newmaxh)
        hmyw    (* 1/2 myw)
        hmyh    (* 1/2 myh) 
        smyw    (* 1/6 myw)  
        inp     [(- hmyw) 0]
        outp    [hmyw 0]
        center  [0 0]
        r1      (* hmyh (fm/sin (fm/radians 120)))
        r2      (* hmyh (fm/sin (fm/radians 60)))
        ]
    {:parts [[:arc 0 0 myw myh (fm/radians -30) (fm/radians 300)]
             [:arc 0 0 hmyw hmyh (fm/radians -30) (fm/radians 300)]
             [:point 0 0]
             [:line center outp]
             ]
     :width totw
     :in [(- hmyw) 0]
     :out [hmyw 0]
     :bbox (c2d/crect-shape 0 0  totw toth)
     :attach [
              [(* -2 smyw) r1 (fm/radians 120)]
              [0 hmyh (fm/radians 90)]
              [(* 2 smyw) r2 (fm/radians 60)]
              ]
     }))

(defn old-size-four [children]
  (let [childbbs (map :bbox children)
        chs      (mapv #(.getWidth %) childbbs)
        cws      (mapv #(.getHeight %) childbbs)
        thetas   [(fm/radians 270) (fm/radians 270) (fm/radians 90) (fm/radians 90)]
        newbb    (mapv #(get-rotated-bounds %1 %2 %3 ) cws chs thetas)
        newws    (mapv first newbb)
        newhs    (mapv second newbb)
        newmaxw (apply max (conj cws MS))
        newmaxh (apply max (conj chs MS))
        qs      (* 0.25 MS)
        hs      (* 0.5 MS)
        myw     (+ MS (* 0.5 newmaxw))
        myh     (+ MS newmaxh)
        mid     0
        hmyw    (* 0.5 myw)
        hmyh    (* 0.5 myh)
        dpN     [mid (- qs)]
        dpE     [(+ mid qs) 0]
        dpS     [mid qs]
        dpW     [(- mid qs) 0]
        apNW    [(- hmyw) (- hmyh)]
        apNE    [hmyw (- hmyh)]
        apSE    [hmyw  hmyh]
        apSW    [(- hmyw) hmyh]
        ]
    {:parts [[:line [(- hmyw) 0] dpW]
             ;diamond
             [:line dpN dpE] [:line dpE dpS] [:line dpS dpW] [:line dpW dpN]
             ;arrows 
             [:line dpW apNW] [:line dpN apNW]
             [:line dpE apNE] [:line dpN apNE]
             [:line dpW apSW] [:line dpS apSW]
             [:line dpE apSE] [:line dpS apSE]
             ;out
             [:line dpE [hmyw 0]]
             ]
     :width myw
     :in [(- hmyw) 0]
     :out [hmyw 0]
     :bbox (c2d/crect-shape 0 0  myw myh)
     :attach [
              [(- hmyw) (- hmyh) (fm/radians 270)]
              [hmyw (- hmyh) (fm/radians 270)]
              [(- hmyw) hmyh (fm/radians 90)]
              [hmyw hmyh (fm/radians 90)]
              ] }))

(defn size-four [children]
  (let [childbbs (map :bbox children)
        chs     (mapv #(.getWidth %) childbbs)
        cws     (mapv #(.getHeight %) childbbs)
        newmaxw (apply max (conj cws MS))
        newmaxh (apply max (conj chs MS))
        myw     (* 1.5 newmaxw)
        myh     (* 2 newmaxh)
        hmyh    (* 1/2 myh)
        tmyh    (* 1/3 myh)
        qmyh    (* 1/4 myh)
        smyh    (* 1/6 myh) 
        emyh    (* 1/8 myh) 
        hmyw    (* 1/2 myw)
        tmyw    (* 1/3 myw)
        qmyw    (* 1/4 myw)
        smyw    (* 1/6 myw) 
        emyw    (* 1/8 myw) 
        inp     [(- hmyw) 0]
        outp    [hmyw 0]
        N       [0 (- emyh)]
        NW      [(- hmyw) (- tmyh)]
        NE      [(+ hmyw) (- tmyh)]
        [midNW] (divide-line N NW 2)
        [midNE] (divide-line N NE 2)
        S       [0 (+ emyh)]
        SW      [(- hmyw) (+ tmyh)]
        SE      [(+ hmyw) (+ tmyh)]
        cN      [0 (- emyh)]
        cS      [0 (+ emyh)]
        cW      [(- emyh) 0]
        cE      [(+ emyh) 0]
        [midSW] (divide-line S SW 2)
        [midSE] (divide-line S SE 2)
        totw    myw
        toth    myh
        ]
    {:parts [[:line inp cW][:line outp cE]
             [:line N NW][:line N NE][:line S SW][:line S SE]
             [:line cW cN][:line cN cS][:line cS cE]
             ;[:point (- qmyw) 0]
             ;[:point (- hmyw) 0]
             ]
     :width totw
     :in [(- hmyw) 0]
     :out [hmyw 0]
     :bbox (c2d/crect-shape 0 0  totw toth)
     :attach [
              [(first midNW) (second midNW) (fm/radians 290)]
              [(first midNE) (second midNE) (fm/radians 250)]
              [(first midSW) (second midSW) (fm/radians 70)]
              [(first midSE) (second midSE) (fm/radians 110)]
              ]
     }))

(defn size-five [children]
  (let [childbbs (map :bbox children)
        chs     (mapv #(.getWidth %) childbbs)
        cws     (mapv #(.getHeight %) childbbs)
        newmaxw (apply max (conj cws MS))
        newmaxh (apply max (conj chs MS))
        myw     (max MS newmaxw)
        myh     (* 1.5 newmaxh)
        hmyw    (* 0.5 myw)
        tmyh    (* 1/3 myh) 
        smyh    (* 1/6 myh) 
        inp     [(- hmyw) 0]
        outp    [hmyw 0]
        mp      [0 tmyh]
        mpN     [0 0]
        inN     [(- hmyw) (* -2 smyh)]
        outN    [(+ hmyw) (* -2 smyh)]
        ]
    {:parts [[:line inp mp][:line outp mp]
             [:line inN mpN][:line outN mpN]
             [:point 0 (* -2 smyh)]
             ]
     :width myw
     :in [(- hmyw) 0]
     :out [hmyw 0]
     :bbox (c2d/crect-shape 0 0  myw myh)
     :attach [[0 tmyh  (fm/radians 90)]]
     }))

(defn size-six [children]
  (let [childbbs (map :bbox children)
        chs     (mapv #(.getWidth %) childbbs)
        cws     (mapv #(.getHeight %) childbbs)
        newmaxw (apply max (conj cws MS))
        newmaxh (apply max (conj chs MS))
        myw     (* 2 newmaxw)
        myh     (* 1.5 newmaxh)
        hmyw    (* 0.5 myw)
        hhmyw   (* 0.25 myw)
        tmyh    (* 1/3 myh) 
        inp     [(- hmyw) 0]
        outp    [hmyw 0]
        mp      [0 0]
        tip1    [(- hhmyw) (- tmyh)]
        tip2    [hhmyw (- tmyh)]
        ]
    {:parts [[:line inp tip1][:line tip1 mp][:line mp tip2][:line tip2 outp]
             [:point (first tip2) 0]
             ]
     :width myw
     :in [(- hmyw) 0]
     :out [hmyw 0]
     :bbox (c2d/crect-shape 0 0  myw myh)
     :attach [[(- hhmyw) (- tmyh)  (fm/radians 270)][hhmyw (- tmyh) (fm/radians 270)]]
     }))

(defn size-seven [children]
  (let [childbbs (map :bbox children)
        chs     (mapv #(.getWidth %) childbbs)
        cws     (mapv #(.getHeight %) childbbs)
        newmaxw (apply max (conj cws MS))
        newmaxh (apply max (conj chs MS))
        myw     (* 2 newmaxw)
        myh     (* 1.5 newmaxh)
        hmyw    (* 0.5 myw)
        smyw    (* 1/6 myw) 
        tmyh    (* 1/3 myh) 
        inp     [(- hmyw) 0]
        outp    [hmyw 0]
        tip1    [(- (* 2 smyw)) tmyh]
        tip2    [0 (- tmyh)]
        tip3    [(* 2 smyw) tmyh]
        ]
    {:parts [[:line inp tip1][:line tip1 tip2][:line tip2 tip3][:line tip3 outp]]
     :width myw
     :in [(- hmyw) 0]
     :out [hmyw 0]
     :bbox (c2d/crect-shape 0 0  myw myh)
     :attach [
              [(first tip1) tmyh  (fm/radians 90)]
              [0 (- tmyh) (fm/radians 270)]
              [(first tip3) tmyh  (fm/radians 90)]
              ]
     }))

(defn size-eight [children]
  (let [childbbs (map :bbox children)
        chs     (mapv #(.getWidth %) childbbs)
        cws     (mapv #(.getHeight %) childbbs)
        newmaxw (apply max (conj cws MS))
        newmaxh (apply max (conj chs MS))
        myw     (* 2 newmaxw)
        myh     (* 2 MS)
        hmyw    (* 1/2 myw)
        qmyw    (* 1/4 myw)
        hmyh    (* 1/2 myh) 
        qmyh    (* 1/4 myh) 
        emyh    (* 1/8 myh) 
        inp     [(- hmyw) 0]
        outp    [hmyw 0]
        vertN   [0 (- qmyh)]
        vertS   [0 qmyh]
        horzNW  [(- qmyw) (- qmyh)]
        horzNE  [qmyw (- qmyh)]
        horzSW  [(- qmyw) qmyh]
        horzSE  [qmyw qmyh]
        shortN  [(- qmyw) (* -3 emyh)]
        shortS  [qmyw (* 3 emyh)]
        longN   [qmyw (* -5 emyh)]
        longS   [(- qmyw) (* 5 emyh)]
        toth    (+ myh (* 2 newmaxh))
        ]
    {:parts [
             [:line inp outp][:line vertN vertS]
             [:line horzNW horzNE] [:line horzSW horzSE]
             [:line horzNW shortN] [:line horzNE longN]
             [:line horzSW longS] [:line horzSE shortS]
             ]
     :width myw
     :in [(- hmyw) 0]
     :out [hmyw 0]
     :bbox (c2d/crect-shape 0 0  myw toth)
     :attach [[(first horzNW) (second shortN)  (fm/radians 270)]
              [(first horzNE) (second longN)  (fm/radians 270)]
              [(first horzSW) (second longS)  (fm/radians 90)]
              [(first horzSE) (second shortS)  (fm/radians 90)]]
     }))

(defn size-nine [children]
  (let [childbbs (map :bbox children)
        chs     (mapv #(.getWidth %) childbbs)
        cws     (mapv #(.getHeight %) childbbs)
        newmaxw (apply max (conj cws MS))
        newmaxh (apply max (conj chs MS))
        myw     newmaxw
        myh     (* 2 MS)
        hmyw    (* 1/2 myw)
        fmyw    (* 1/5 myw)
        hmyh    (* 1/2 myh) 
        fmyh    (* 1/5 myh) 
        fMS     (* 1/5 MS)
        inp     [(- hmyw) 0]
        outp    [hmyw 0]
        W       [(- fMS) 0]
        E       [fMS 0]
        NW      [(- fMS) (* 1 fmyh)]
        NE      [fMS (* 1 fmyh)]
        SW      [(- fMS) (* -1 fmyh)]
        SE      [fMS (* -1 fmyh)]
        coreN   [0 hmyh]
        coreS   [0 (- hmyh)]
        toth    (+ myh newmaxh)
        ]
    {:parts [[:line inp W]
             [:line NW SW][:line NE SE]
             [:line coreN coreS]
             [:line E outp]
             [:point 0 hmyh]
             ]
     :width myw
     :in [(- hmyw) 0]
     :out [hmyw 0]
     :bbox (c2d/crect-shape 0 0  myw toth)
     :attach [
              [0 (- hmyh) (fm/radians 270)]
              ]
     }))


(defn size-ten [children]
  (let [childbbs (map :bbox children)
        chs     (mapv #(.getWidth %) childbbs)
        cws     (mapv #(.getHeight %) childbbs)
        newmaxw (apply max (conj cws MS))
        newmaxh (apply max (conj chs MS))
        myw     MS
        myh     MS
        hmyw    (* 1/2 myw)
        fmyw    (* 1/5 myw)
        hmyh    (* 1/2 myh) 
        fmyh    (* 1/5 myh) 
        fMS     (* 1/5 MS)
        inp     [(- hmyw) 0]
        outp    [hmyw 0]
        S       [0 fMS]
        N       [0 (- fMS)]
        E       [fMS 0]
        W       [(- fMS) 0]
        SW      [(* -1 fMS) (* 1 fMS)]
        SSW     [(* -1 fMS) (* 3 fMS)]
        SWW     [(* -3 fMS) (* 1 fMS)]
        SE      [(* 1 fMS) (* 1 fMS)]
        SSE     [(* 1 fMS) (* 3 fMS)]
        SEE     [(* 3 fMS) (* 1 fMS)]
        NW      [(* -1 fMS) (* -1 fMS)]
        NNW     [(* -1 fMS) (* -3 fMS)]
        NWW     [(* -3 fMS) (* -1 fMS)]
        NE      [(* 1 fMS) (* -1 fMS)]
        NNE     [(* 1 fMS) (* -3 fMS)]
        NEE     [(* 3 fMS) (* -1 fMS)]
        coreN   [0 (* -6 fMS)]
        coreS   [0 (* 6 fMS )]
        toth    (+ myh newmaxh)
        ]
    {:parts [[:line inp W]
             [:line NW NNW][:line NW NWW]
             [:line NE NNE][:line NE NEE]
             [:line SW SSW][:line SW SWW]
             [:line SE SSE][:line SE SEE]
             [:line N coreN]
             [:line S coreS]
             ;[:line forkSW forkSE]
             [:line outp E]
             ]
     :width myw
     :in [(- hmyw) 0]
     :out [hmyw 0]
     :bbox (c2d/crect-shape 0 0  myw toth)
     :attach [
              [0 (* -6 fMS) (fm/radians 270)]
              [0 (* 6 fMS) (fm/radians 90)]
              ]
     }))

(defn size-eleven [children]
  (let [childbbs (map :bbox children)
        chs     (mapv #(.getWidth %) childbbs)
        cws     (mapv #(.getHeight %) childbbs)
        newmaxw (apply max (conj cws MS))
        newmaxh (apply max (conj chs MS))
        myw     newmaxw
        myh     (* 2 MS)
        hmyw    (* 1/2 myw)
        fmyw    (* 1/5 myw)
        hmyh    (* 1/2 myh) 
        fmyh    (* 1/5 myh) 
        fMS     (* 1/5 MS)
        inp     [(- hmyw) 0]
        outp    [hmyw 0]
        S       [0 fMS]
        N       [0 (- fMS)]
        SW      [(* -2 fmyw) (* 1 fMS)]
        SE      [(* 2 fmyw) (* 1 fMS)]
        NW      [(* -2 fmyw) (* -1 fMS)]
        NE      [(* 2 fmyw) (* -1 fMS)]
        coreS   [0 hmyh]
        coreNW  [(* -2 fmyw) (* -2 fmyh)]
        coreNE  [(* 2 fmyw) (- fmyh)]
        toth    (+ MS newmaxh)
        ]
    {:parts [[:line inp outp]
             [:line NW N][:line NE N]
             [:line SW S][:line SE S]
             [:line S coreS]
             [:line NW coreNW]
             [:line NE coreNE]
             ]
     :width myw
     :in [(- hmyw) 0]
     :out [hmyw 0]
     :bbox (c2d/crect-shape 0 0  myw toth)
     :attach [
              [(* 2 fmyw) (- fmyh) (fm/radians 270)]
              [(* -2 fmyw) (* -2 fmyh) (fm/radians 270)]
              [0 hmyh (fm/radians 90)]
              ]
     }))


(defn size-twelve [children]
  (let [childbbs (map :bbox children)
        chs     (mapv #(.getWidth %) childbbs)
        cws     (mapv #(.getHeight %) childbbs)
        newmaxw (apply max (conj cws (* 2 MS)))
        newmaxh (apply max (conj chs (* 2 MS)))
        myw     (* 1 newmaxw)
        myh     (* 1 newmaxh)
        hmyw    (* 1/2 myw)
        hmyh    (* 1/2 myw)
        qmyw    (* 1/4 myw)
        qmyh    (* 1/4 myw)
        fmyw    (* 1/5 myw)
        inp     [(- hmyw) 0]
        outp    [hmyw 0]
        N       [0 (- qmyh)]
        S       [0 qmyh]
        W       [(- qmyw) 0]
        E       [qmyw 0]
        [n1 n2 n3 n4] (divide-line W N 5)
        [m1 m2 m3 m4] (divide-line W E 5) 
        [s1 s2 s3 s4] (divide-line W S 5)
        [mNW] (divide-line W N 2) 
        [mNE] (divide-line E N 2) 
        [mSW] (divide-line W S 2) 
        [mSE] (divide-line E S 2) 
        ]
    {:parts [[:line inp W] [:line E outp]
             [:line W N][:line W E][:line W S][:line N E][:line S E]
             [:line n1 m1][:line n2 m2][:line n3 m3][:line n4 m4]
             [:line s1 m1][:line s2 m2][:line s3 m3][:line s4 m4]
             ]
     :width myw
     :in [(- hmyw) 0]
     :out [hmyw 0]
     :bbox (c2d/crect-shape 0 0  myw myh)
     :attach [
              [(first mNW) (second mNW) (fm/radians 225)]
              [(first mNE) (second mNE) (fm/radians 315)]
              [(first mSW) (second mSW) (fm/radians 135)]
              [(first mSE) (second mSE) (fm/radians 45)]
              ]
     }))

(defn size-thirteen [children]
  (let [childbbs (map :bbox children)
        chs     (mapv #(.getWidth %) childbbs)
        cws     (mapv #(.getHeight %) childbbs)
        newmaxw (apply max (conj cws MS))
        newmaxh (apply max (conj chs MS))
        myw     (* 1 newmaxw)
        myh     (* 1 newmaxh)
        hmyw    (* 1/2 myw)
        hmyh    (* 1/2 myw)
        qmyw    (* 1/4 myw)
        smyw    (* 1/6 myw)
        inp     [(- hmyw) 0]
        outp    [hmyw 0]
        NW      [(- smyw) (- hmyh)]
        NE      [smyw (- hmyh)]
        SW      [(- qmyw) hmyh]
        SE      [qmyw hmyh]
        [w1 w2 w3 w4] (divide-line NW SW 5)
        [e1 e2 e3 e4] (divide-line NE SE 5)
        [mW]      (divide-line NW SW 2)
        [mE]      (divide-line NE SE 2)
        toth    (+ myh newmaxh)
        ]
    {:parts [[:line inp mW] [:line mE outp]
             [:line NW NE][:line SE SW][:line NW SW][:line NE SE]
             [:line w1 e1][:line w2 e2][:line w3 e3][:line w4 e4]
             ]
     :width myw
     :in [(- hmyw) 0]
     :out [hmyw 0]
     :bbox (c2d/crect-shape 0 0  myw toth)
     :attach [
              [0 (- hmyh) (fm/radians 270)]
              ]
     }))

(defn size-fourteen [children]
  (let [childbbs (map :bbox children)
        chs     (mapv #(.getWidth %) childbbs)
        cws     (mapv #(.getHeight %) childbbs)
        newmaxw (apply max (conj cws MS))
        newmaxh (apply max (conj chs MS))
        myw     (max (* 1/2 newmaxw) (* 2 MS))
        myh     (* 2 MS)
        hmyw    (* 1/2 myw)
        hmyh    (* 1/2 myh)
        smyw    (* 1/6 myw)
        emyw    (* 1/8 myw)
        smyh    (* 1/6 myh)
        emyh    (* 1/8 myh)
        inp     [(- hmyw) 0]
        outp    [hmyw 0]
        incon   [(* -2 smyw) 0]
        outcon  [(* 2 smyw) 0]
        N       [(* -2 smyw) (- hmyh)]
        NW      [(* -3 smyw) (* -3 emyh)]
        NE      [(* -1 smyw) (* -3 emyh)]
        Ns      [(* -2 smyw) (* -3 emyh)]
        NWs     [(* -3 smyw) (* -2 emyh)]
        NEs     [(* -1 smyw) (* -2 emyh)]
        Nss     [(* -2 smyw) (* -2 emyh)]
        NWss    [(* -3 smyw) (* -1 emyh)]
        NEss    [(* -1 smyw) (* -1 emyh)]
        tailW   [(* -2 smyw) (* 2 emyh)]
        S       [(* 2 smyw) hmyh]
        SW      [(* 3 smyw) (* 3 emyh)]
        SE      [(* 1 smyw) (* 3 emyh)]
        tailE   [(* 2 smyw) (* -2 emyh)]
        totw    (+ myw (* 2 newmaxw))
        toth    (+ myh newmaxh)
        ]
    {:parts [[:line inp incon] [:line outcon outp]
             [:line N NW][:line N NE][:line N tailW]
             [:line Ns NWs][:line Ns NEs]
             [:line Nss NWss][:line Nss NEss]
             [:line S SW][:line S SE][:line S tailE]
             [:arc (* -1 smyw) 0 (* 2 smyw) (* 2 smyh) (fm/radians 0) (fm/radians 180)]
             [:arc (* 1 smyw) 0 (* 2 smyw) (* 2 smyh) (fm/radians 180) (fm/radians 180)]
             ]
     :width totw
     :in [(- hmyw) 0]
     :out [hmyw 0]
     :bbox (c2d/crect-shape 0 0  totw toth)
     :attach [
              [(first N) (- hmyh) (fm/radians 270)]
              [(first S) hmyh (fm/radians 90)]
              ]
     }))

(defn size-fifteen [children]
  (let [childbbs (map :bbox children)
        cws     (mapv #(.getWidth %) childbbs)
        chs     (mapv #(.getHeight %) childbbs)
        thetas  [(fm/radians 135) (fm/radians 45) (fm/radians 225)]
        newbb    (mapv #(get-rotated-bounds %1 %2 %3 ) cws chs thetas)
        newws    (mapv first newbb)
        newhs    (mapv second newbb)
        newmaxw (apply max (conj cws MS))
        newmaxh (apply max (conj chs MS))
        myw     (* 2 (max newmaxw newmaxh))
        myh     myw
        hmyw    (* 1/2 myw)
        hmyh    (* 1/2 myh)
        smyw    (* 1/6 myw)
        smyh    (* 1/6 myh)
        inp     [(- hmyw) 0]
        outp    [hmyw 0]
        N       [0 (- hmyh)]
        S       [0 (+ hmyh)]
        W       [(- hmyw) 0]
        E       [(+ hmyw) 0]
        [midNW] (divide-line N W 2)
        [midNE] (divide-line N E 2)
        [midSW] (divide-line S W 2)
        [midSE] (divide-line S E 2)
        [d1 d2] (divide-line [0 0] midSE 3)


        NW      [(- hmyw) (* -1 smyh)]
        NE      [hmyw (* -2 smyh)]
        tip     [(* 0 smyw) (* -3 smyh)]
        SW      [(- hmyw) hmyh]
        SE      [hmyw hmyh]
        toth    (+ myh newmaxh)
        ]
    {:parts [
             [:line N W][:line W S][:line S E][:line E N]
             [:point (first d1) (second d1)]
             [:point (first d2) (second d2)]
             ]
     :width myw
     :in [(- hmyw) 0]
     :out [hmyw 0]
     :bbox (c2d/crect-shape 0 0  myw toth)
     :attach [
              [(first midNW) (second midNW) (fm/radians 45)]
              [(first midNE) (second midNE) (fm/radians 135)]
              [(first midSW) (second midSW) (fm/radians 315)]
              ]
     }))

(def size-function-map
  {:join-line size-join-line
   :zero      size-zero
   :one       size-one
   :two       size-two
   :three     size-three
   :four      size-four
   :five      size-five
   :six       size-six
   :seven     size-seven
   :eight     size-eight
   :nine      size-nine
   :ten       size-ten
   :eleven    size-eleven
   :twelve    size-twelve
   :thirteen  size-thirteen
   :fourteen  size-fourteen
   :fifteen   size-fifteen
   })
