(ns xscreenbane.core
  (:import 
    [com.sun.jna Memory NativeLong]
    [com.sun.jna.platform.unix X11]
    [com.sun.jna.platform.unix X11$XGCValues X11$Window X11$XWindowAttributes]
    [java.awt.image BufferedImage])
  (:require [xscreenbane.hacks.hack :as hack]
            [signal.handler :refer [on-signal]])
  (:gen-class)
  )


(def depth-map {24 BufferedImage/TYPE_INT_RGB
                32 BufferedImage/TYPE_INT_ARGB})

(defn setup [window-id]
  (def x (X11/INSTANCE))
  (def dpy (.XOpenDisplay x (System/getenv "DISPLAY")))
  (def xgcv (new X11$XGCValues))
  (def win (new X11$Window window-id )) 
  (def xgc (.XCreateGC x dpy win (new NativeLong 0) xgcv)) 
  (def xwa (new X11$XWindowAttributes))
  (.XGetWindowAttributes x dpy win xwa)
  (def width (.width xwa))
  (def height (.height xwa))
  (def depth (.depth xwa))

  (def canvas (BufferedImage. width height (get depth-map depth)))
  (def buffer (new Memory (* 4 width height)))
  (def image (.XCreateImage x dpy (.visual xwa) depth 2 0 buffer width height 32 (* 4 width)))
  )

(defn cleanup []
  (try
    (.XFree x (.getPointer image))
    (.XFreeGC x dpy xgc)
    (.XCloseDisplay x dpy)
    (catch java.lang.IllegalArgumentException e (str "Failed to free all resources: " (.getMessage e)))))

(defn xput [^BufferedImage canvas]
  (let [raster (.getData canvas)]
    (.write buffer 0 (.getData (.getDataBuffer raster)) 0 (.getSize (.getDataBuffer raster)))
    (.XPutImage x dpy win xgc image 0 0 0 0 width height)))

(defn require-hack [hack]
  (println "reloading hack" hack)
  (println (ns-publics 'xscreenbane.hacks.hack))
  (println "aliases" (ns-aliases 'xscreenbane.core))
  (let [fqhack (str "xscreenbane.hacks." hack)
        sym    (symbol fqhack)]
    (println "remove-ns" (remove-ns 'xscreenbane.hacks.hack))
  (println "aliases" (ns-aliases 'xscreenbane.core))
    (println "ns-unalias" (ns-unalias 'xscreenbane.core 'hack))
  (println "aliases" (ns-aliases 'xscreenbane.core))
    ;(ns-unalias sym 'hack)
    (println "require reload-all" (require [sym :as 'hack :reload-all true]))
  (println "aliases" (ns-aliases 'xscreenbane.core))
    ))

(defn load-hack [hack]
  (let [
        hackname "arcs"
        fqhack (str "xscreenbane.hacks." hackname)
        fqsym  (symbol fqhack)
        draw (resolve (symbol fqhack "draw"))
        ;set-up-state (resolve fqsym "set-up-state")
        ]
    (draw)
    )
  )

;need to support params to make testing easier
;any extra params pass to hack
;-root
;-window 
;-window-id?
(defn run [args]

  (.addShutdownHook (Runtime/getRuntime) 
                    (Thread. #(cleanup)))

  ;get $XSCREENSAVER_WINDOW from environment
  (println "hack" (first args))
  (let [window-id (System/getenv "XSCREENSAVER_WINDOW")
        window-id-int (read-string window-id)
        ;it's a hex string, so Integer/parse-int won't do
        hackname (first args)
        fqhack (str "xscreenbane.hacks." hackname)
        set-up-state (requiring-resolve (symbol fqhack "set-up-state"))
        draw (requiring-resolve (symbol fqhack "draw"))
        ]
    (println "draw" draw)
    (println "id" window-id)
    (println "id int" window-id-int)
    (setup window-id-int) 
    (set-up-state canvas (rest args))
    (loop []
      (draw canvas)
      (xput canvas)
      (recur))))


(comment 
  (setup 0x3800007)
  (def set-up-state (requiring-resolve (symbol "xscreenbane.hacks.arcs" "set-up-state")))
  (def draw (requiring-resolve (symbol "xscreenbane.hacks.arcs" "draw")))

  (def set-up-state (requiring-resolve (symbol "xscreenbane.hacks.arp-sigils" "set-up-state")))
  (def draw (requiring-resolve (symbol "xscreenbane.hacks.arp-sigils" "draw")))
  (set-up-state canvas [:palette :greenpunk])
  (do (draw canvas) (xput canvas))
  (future 
    (repeatedly 100 #(do 
                     ;(Thread/sleep 10)
                     (draw canvas) 
                     (xput canvas)
                     )))
  )

(defn -main-old [& args]
  (when-not (System/getenv "DISPLAY")
    (println "Please set the $DISPLAY")
    (System/exit 1))

  (when (< (count args) 1)  
    (println "Please name a hack")
    (System/exit 2))

  (.addShutdownHook (Runtime/getRuntime) 
                    (Thread. #(cleanup)))

  ;get $XSCREENSAVER_WINDOW from environment
  (let [window-id (System/getenv "XSCREENSAVER_WINDOW")
        ;it's a hex string, so Integer/parse-int won't do
        window-id-int (read-string window-id)]
    (println "id" window-id)
    (println "id int" window-id-int)
    (setup window-id-int) 
    (println "hack" (first args) )
    ;(require-hack (first args))
    (load-hack (first args))
    (hack/set-up-state canvas args)
    (loop []
      (hack/draw canvas)
      (xput canvas)
      (recur))))

(comment ;testing area
         (ns-aliases 'xscreenbane.hacks.hack)
         (ns-unalias 'xscreenbane.core 'hack)
  (require-hack "dirty-window")
  (require-hack "arcs")
  (require-hack "arp-sigils")
  (setup 0x3800007)
  (hack/set-up-state canvas [:palette :greenpunk])
  (hack/set-up-state canvas [:palette :craftsman])
  (do (hack/draw canvas) (xput canvas))
  (cleanup)
  (repeatedly 100 #(do 
                     (Thread/sleep 10)
                     (hack/draw canvas) 
                     (xput canvas)
                     ))
  (vals (ns-publics 'xscreenbane.hacks.hack))
  (vals (ns-publics 'xscreenbane.hacks.arcs))
  (ns-publics 'xscreenbane.hacks.hack)
  (ns-aliases 'xscreenbane.core)
  (dir 'xscreenbane.hacks.hack)
  )
