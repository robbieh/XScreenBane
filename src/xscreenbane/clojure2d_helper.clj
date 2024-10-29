(ns xscreenbane.clojure2d-helper
  (:require [clojure2d.core :as c2d])
  (:import [clojure2d.core Canvas]
           [java.awt.geom Ellipse2D$Double Line2D$Double Rectangle2D$Double Arc2D$Double]))

(defn canvas-from-bufferedimage [bi]
  (Canvas. (.getGraphics bi); graphics
           bi
           (Line2D$Double.)
           (Rectangle2D$Double.)
           (Ellipse2D$Double.)
           (Arc2D$Double.)
           (:mid c2d/rendering-hints)
           (.getWidth bi) (.getHeight bi)
           (atom []); transform-stack
           nil; font
           false; retina?
           ))
