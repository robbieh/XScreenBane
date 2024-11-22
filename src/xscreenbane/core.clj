(ns xscreenbane.core
  (:import 
    [com.sun.jna Memory NativeLong]
    [com.sun.jna.platform.unix X11]
    [com.sun.jna.platform.unix X11$XGCValues X11$Window X11$XWindowAttributes]
    [java.awt.image BufferedImage])
  (:require [signal.handler :refer [on-signal]])
  (:gen-class))


(def depth-map 
  "Map X11 window depths to BufferedImage types"
  {24 BufferedImage/TYPE_INT_RGB
  32 BufferedImage/TYPE_INT_ARGB})

(defn setup 
  "Prepare to use the given X11 window ID for drawing"
  [window-id]
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
  (def image (.XCreateImage x dpy (.visual xwa) depth 2 0 buffer width height 32 (* 4 width))))

(defn cleanup 
  "Release X11 resources"
  []
  (try
    (.XFree x (.getPointer image))
    (.XFreeGC x dpy xgc)
    (.XCloseDisplay x dpy)
    (catch java.lang.IllegalArgumentException e (str "Failed to free all resources: " (.getMessage e)))))

(defn xput 
  "Put BufferedImage to target X11 window"
  [^BufferedImage canvas]
  (let [raster (.getData canvas)]
    (.write buffer 0 (.getData (.getDataBuffer raster)) 0 (.getSize (.getDataBuffer raster)))
    (.XPutImage x dpy win xgc image 0 0 0 0 width height)))

;need to support params to make testing easier
;any extra params pass to hack
;-root
;-window 
;-window-id?
(defn run [args]

  (.addShutdownHook (Runtime/getRuntime) 
                    (Thread. #(cleanup)))

  ;get $XSCREENSAVER_WINDOW from environment
  (let [window-id (System/getenv "XSCREENSAVER_WINDOW")
        window-id-int (read-string window-id)
        ;it's a hex string, so Integer/parse-int won't do
        hackname (first args)
        fqhack (str "xscreenbane.hacks." hackname)
        set-up-state (requiring-resolve (symbol fqhack "set-up-state"))
        draw (requiring-resolve (symbol fqhack "draw"))
        ]
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
  (repeatedly 100 #(do 
    ;(Thread/sleep 10)
    (draw canvas) 
    (xput canvas)
    ))
  )




