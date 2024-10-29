(ns xscreenbane.core
  (:import 
    [com.sun.jna Memory NativeLong]
    [com.sun.jna.platform.unix X11]
    [com.sun.jna.platform.unix X11$XGCValues X11$Window X11$XWindowAttributes]
    [java.awt.image BufferedImage])
  (:require [xscreenbane.hacks.hack :as hack])
  (:gen-class))

(when-not (System/getenv "DISPLAY")
  (println "Please set the $DISPLAY")
  (System/exit 1))

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
  (let [fqhack (str "xscreenbane.hacks." hack)
        sym    (symbol fqhack)]
    (remove-ns 'xscreenbane.hacks.hack)
    (ns-unalias 'xscreenbane.core 'hack)
    ;(ns-unalias sym 'hack)
    (require [sym :as 'hack :reload-all true])))

;need to support params to make testing easier
;any extra params pass to hack
;-root
;-window 
;-window-id?

(defn -main [& args]

  (.addShutdownHook (Runtime/getRuntime) 
                    (Thread. #(cleanup)))

  ;get $XSCREENSAVER_WINDOW from environment
  (let [window-id (System/getenv "XSCREENSAVER_WINDOW")
        ;it's a hex string, so Integer/parse-int won't do
        window-id-int (read-string window-id)]
    (println "id" window-id)
    (println "id int" window-id-int)
    (setup window-id-int) 
    (require-hack "dirty-window")
    ;(hack/set-up-state canvas)
    (loop []
    ;  (hack/draw canvas)
      (xput canvas)
      (recur))))

(comment ;testing area
         (ns-aliases 'xscreenbane.hacks.hack)
         (ns-unalias 'xscreenbane.core 'hack)
  (require-hack "dirty-window")
  (require-hack "arcs")
  (setup 0x3800007)
  (hack/set-up-state canvas)
  (do (hack/draw canvas) (xput canvas))
  (cleanup)
  )
