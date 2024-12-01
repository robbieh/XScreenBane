(ns xscreenbane.hacks.dirty-window
  (:require
    [xscreenbane.color :as c]
    [xscreenbane.utils.cli :as cli]
    [xscreenbane.utils.siunits :as si]
    [xscreenbane.clojure2d-helper :as c2dhelper]
    [xscreenbane.fetch.meminfo :as meminfo]
    [clojure2d.color :as color]
    [clojure2d.core :as c2d]
    )
  (:import
    [java.awt.image BufferedImage]
    )
  )

(def hackstate (atom {}))
(def palette (:greenpunk c/palettes))

(defn make-boxes [boxsize scrw scrh gap colors]
  (let [gap 0
        xcount (Math/floor (/ scrw (+ boxsize gap)))
        ycount (Math/floor (/ scrh (+ boxsize gap)))
        direction [:NE :NW :SE :SW]
        ]
    ;(println (map #(double (* boxsize %)) (range #_(* 0.5 gap) xcount)))
    ;(println (map #(double (* boxsize %)) (range #_(* 0.5 gap) ycount)))
    (for [x (map #(double (* boxsize %)) (range #_(* 0.5 gap) xcount))
          y (map #(double (* boxsize %)) (range #_(* 0.5 gap) ycount))]
      {:x x :y y :start (rand-nth direction) :color (rand-nth colors) })))

;[1 2 3 4 5 6 7 8 9]
;(map #(mod  % 3) [0 1 2 3 4 5 6 7 8 ]) ; (0 1 2 0 1 2 0 1 2)
;(map #(Math/ceil (* % 1/3)) [0 1 2 3 4 5 6 7 8 ])
;(map-indexed 
;  (fn [i _] 
;    [
;     (mod i 3)
;     (int (Math/ceil (* i 1/3)))
;     ])
;  [0 1 2 3 4 5 6 7 8 ])
;
;(map #(vector [%1 %2] %3 ) 
;     (flatten (repeat 3 (range 1 4)))
;     _
;     [0 1 2 3 4 5 6 7 8 ])


;(amap (to-array-2d [[123][456][789]]) idx ret (println idx ret ) )
;(vec (for [x (range 1 4)]
;  (vec(for [ y (range 1 4)]
;    0))))

(defn make-background [xcount ycount gradient]
  (let [maxi   (+ xcount ycount)
        colors (vec (color/palette gradient maxi))]
    (vec 
      (for [x (range xcount)]
        (vec
          (for [y (range ycount)]
            (nth colors (+ x y))))))))

(defn make-coords [xcount ycount]
  (vec (shuffle (for [x (range xcount)
               y (range ycount)]
           [x y]) )))

(defn make-corners [xcount ycount ]
    (vec 
      (for [x (range xcount)]
        (vec
          (for [y (range ycount)]
            (rand-nth [:NE :NW :SE :SE] ))))))

;(defn make-dirty [width height]
;    (for [x (range width)]
;      (vec
;        (for [y (range height)]
;          ))))

;(defn draw-box [index width])

(defn draw-bg []
  (let [
        {:keys [canvas width height colorboxes boxsize gap]} @hackstate
        ]
    (c2d/paint-mode canvas)
    (doseq [[x column] (map-indexed vector colorboxes)]
      (doseq [[y color-value] (map-indexed vector column)]
        (c2d/set-color canvas color-value)
        (c2d/rect canvas (* x (+ gap boxsize))  (* y (+ gap boxsize)) boxsize boxsize)
      ))
  ))

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

(defn draw-dirty []
  (print "clean count: " (count (:clean @hackstate)))
  (println " dirty count: " (count (:dirty @hackstate)))
  (doseq [[xidx yidx] (:dirty @hackstate)
          :let [
                canvas (:canvas @hackstate)
                gap (:gap @hackstate)
                boxsize (double (:boxsize @hackstate))
                color (get-in @hackstate [:colorboxes xidx yidx])
                bgcolor (:background palette)
                corner (get-in @hackstate [:corners xidx yidx])
                x (* xidx (+ gap boxsize))
                y (* yidx (+ gap boxsize))
                ;_ (println xidx yidx " - " x y " - " gap boxsize)
                [x1 y1 x2 y2] (gradient-calc x y boxsize corner)
                                ]]
    (c2d/gradient-mode canvas x1 y1 color x2 y2 bgcolor)
    (c2d/rect canvas x y boxsize boxsize)

    ))

;(keys (meminfo/get-meminfo))
; (:NFS_Unstable :SwapTotal :DirectMap1G :Buffers :AnonHugePages :Active_file_
; :Inactive_anon_ :SReclaimable :SwapFree :Mlocked :VmallocUsed :WritebackTmp
; :VmallocTotal :SwapCached :Slab :Percpu :HardwareCorrupted :DirectMap4k
; :Shmem :KernelStack :DirectMap2M :SecPageTables :Hugepagesize :Mapped
; :Hugetlb :Active :PageTables :HugePages_Free :VmallocChunk :FileHugePages
; :CommitLimit :Inactive_file_ :Zswapped :Cached :AnonPages :Active_anon_
; :Inactive :ShmemHugePages :SUnreclaim :Writeback :Bounce :MemFree
; :HugePages_Surp :Dirty :FilePmdMapped :Committed_AS :MemAvailable
; :HugePages_Total :Zswap :ShmemPmdMapped :KReclaimable :HugePages_Rsvd
; :Unevictable :Unaccepted)

(comment (:colorboxes @hackstate))
(comment (get (:colorboxes @hackstate) 0))
(comment (get-in (:colorboxes @hackstate) [1 2]))
(comment (get-in @hackstate [:colorboxes 2]))
(comment (count (:boxes @hackstate)))

(defn get-dirty-bytes []
  (-> (meminfo/get-meminfo) :Dirty si/parse-bytes-str))

(defn set-up-state [^BufferedImage canvas args]
  (when-let  [palette-from-cli (keyword (cli/pair-get args :palette))]
    (when-let [palette-from-config (get c/palettes palette-from-cli)]
      (alter-var-root #'palette (fn [_] palette-from-config))
      ))
  (let [width      (.getWidth canvas)
        height     (.getHeight canvas)
        numboxes   10
        boxsize    (/ (min width height) numboxes)
        xcount     (Math/floor (/ width  boxsize))
        ycount     (Math/floor (/ height boxsize))
        gap        2
        ;gradient   (mapv #(color/set-alpha % 200) (:greens c/gradients))
        bgcolor    (get-in c/palettes [:greenpunk :background])
        gradient   (get-in c/palettes [:greenpunk :gradients :standard])
        colors     (color/palette (color/gradient gradient) 10)
        ;boxes      (make-boxes boxsize width height gap colors)
        colorboxes (make-background xcount ycount gradient)
        cleanboxes (make-coords xcount ycount)
        corners    (make-corners xcount ycount)
       ]
    (reset! hackstate 
      {:width       width
       :height      height
       :boxsize     boxsize
       :colors      colors
       ;:boxes       boxes
       :bgcolor     bgcolor
       :colorboxes  colorboxes
       :corners     corners
       :gap         gap

       :dirty-bytes-high
                    (get-dirty-bytes)
       :dirty       []
       :clean       cleanboxes
       :boxcount    (count cleanboxes)
       :canvas      (c2dhelper/canvas-from-bufferedimage canvas)})
  ))

; (c2d/with-canvas [c (:canvas @state)]
;   (println (.graphics c))
;   (.setComposite ^Graphics2D (.graphics c) java.awt.AlphaComposite/Src))



(defn update-dirty-get-ratio []
  (let [dirtybytes  (max 1 (get-dirty-bytes))
        high        (max 1 (:dirty-bytes-high @hackstate))
        ratio       (float (/ dirtybytes high ) )
        ]
    ;(println "high: " high  "dirtybytes: " dirtybytes "diff: " (- high dirtybytes) ratio)
    (cond
      (>= ratio 1.0)   
        (do 
          ;(println "big")
          (swap! hackstate assoc :dirty-bytes-high dirtybytes)
            100)
      :else
        (do 
          ;(println "else")
          (Math/ceil (* 100 ratio))
            )
      )))

(defn update-dirty-clean-lists [percent] 
  (let [dcount (count (:dirty @hackstate))
        ccount (count (:cleaned @hackstate))
        bcount (:boxcount @hackstate)
        box-percent (Math/ceil (* dcount (/ 100 bcount)))
        diff   (- percent box-percent)
        ]
    (println "udcl: dirty " percent " shown " box-percent  " diff " diff)
    (cond
      (> diff 0) ;more dirty than shown
        (let [taken (take diff (:clean @hackstate))
              left  (drop diff (:clean @hackstate))
              dirty (concat (:dirty @hackstate) taken)
              ]
          (println "removing from clean")
          (swap! hackstate assoc :dirty dirty)
          (swap! hackstate assoc :clean left)
          )
      (< diff 0) ;more shown than dirty
        (let [taken (take (abs diff) (:dirty @hackstate))
              left  (drop (abs diff) (:dirty @hackstate))
              clean (concat (:clean @hackstate) taken)
              ]
          (println "removing from dirty: " taken)
          (println "still dirty: " left)
          (swap! hackstate assoc :clean clean)
          (swap! hackstate assoc :dirty left)
          )
        :else
        nil
      )
    )
  )
[(:clean @hackstate) (:dirty @hackstate)]

;k(repeatedly 10 #(do (Thread/sleep 1000) (update-dirty)) )
(defn draw [^BufferedImage _]
  (let [{:keys [canvas width height]} @hackstate
        ratio (update-dirty-get-ratio)
        ]
    ;(println "udgr: " ratio)
      (update-dirty-clean-lists ratio)
      (c2d/set-background canvas (:bgcolor @hackstate) 255)
      ;(c2d/set-color c :black)
      ;(c2d/set-stroke c 3)
      ;;(c2d/set-color c color 255)
      ;(doseq [{:keys [x y start color]} (:boxes @hackstate)]
      ;  (let [boxsize (:boxsize @hackstate)
      ;        [x1 y1 x2 y2] (gradient-calc x y boxsize start)
      ;        ]
      ;  (c2d/gradient-mode c x1 y1 color x2 y2 :black)
      ;  (c2d/rect c x y boxsize boxsize)
      ;  (c2d/paint-mode c)
      ;  (c2d/set-color c :black)
      ;  (c2d/rect c x y boxsize boxsize true)
      ; ))
      (draw-bg)
      (draw-dirty)
))

