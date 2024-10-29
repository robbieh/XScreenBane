(ns xscreenbane.utils.arp-sigils
  (:require [xscreenbane.utils.arp-glyphs :as g]
            [clojure2d.core :as c2d]
            [clojure.walk :as walk]
            [fastmath.core :as fm]))



(reduce #(apply (partial assoc %1) %2) {} [[:1 1] [:2 2]])

(defn size-sigil [sigil node]
  ;(print "sizing at " node)
  (let [me          (get sigil node)
        myname      (:name me)
        nextglyph   (:next me)
        nextdata    (:data (get-in sigil [nextglyph :data]))
        childglyphs (:children me)
        myfunc      (get g/size-function-map myname)
        ;_ (println " with name " myname "next: " nextglyph "children: " childglyphs)
        combined    (vec(remove nil? (flatten [nextglyph childglyphs])))
        sized       (if-not (empty? combined)
                      (reduce #(size-sigil %1 %2) sigil combined)
                      sigil)
        childdata   (mapv #(get-in sized [%1 :data]) childglyphs)
        ;sized       (if-not (nil? nextglyph) 
        ;              (size-sigil sized nextglyph)
        ;              sized)
        sizeresult  (myfunc childdata)
        ]
      (assoc-in sized [node :data] sizeresult)))

(defn calc-length [sigil node]
  (let [me        (get sigil node)
        nextglyph (:next me)
        width     (get-in me [:data :width])
        ]
    (if nextglyph
      (+ width (calc-length sigil nextglyph))
      width)))

(defn find-line-last [sigil node]
  (let [nodeinfo (get sigil node)
        next     (:next nodeinfo)]
    (if (nil? next )
      node
      (find-line-last sigil next)
      )))

(defn next-slot [sigil] (count sigil))

(defn append-glyph [sigil childname]
  (let [child (g/empty-glyph childname)
        slot  (next-slot sigil)]
    (assoc sigil slot child)))

(defn append-glyph-at-line-end [sigil node childname]
  (let [child (g/empty-glyph childname)
        end   (find-line-last sigil node)
        slot  (next-slot sigil)]
    (-> sigil
      (assoc , slot child)
      (assoc-in , [end :next] slot))))

(defn attach-child [sigil parent childname]
  (let [parentname (get-in sigil [parent :name])
        child      (g/empty-glyph childname)
        childcount (get g/child-count-map parentname)
        ;children   (vec (repeat child childcount))
        slot       (next-slot sigil)
        childrange (vec (range slot (+ slot childcount))) ]
    (-> (reduce #(assoc %1 %2 child) sigil childrange)
      (assoc-in , [parent :children] childrange))))


;{:name _
; :next _
; :chlidren _
; :data _}

;(defn sigil [glyphvec attachvec outvec]
;  (let [attachparts (partition 2 attachvec)
;        nextparts   (partition 2 outvec)
;        att-glyph   (reduce #(apply (partial attach-child %1) %2) glyphvec attachparts)
;        next-glyph  (reduce #(apply (partial append-glyph %1) %2) att-glyph nextparts)
;        ]
;;    (vec next-glyph)
;    ))

(comment
  (def ts
    (size-sigil (-> (append-glyph [] :join-line)
      (append-glyph-at-line-end , 0 :zero)
      (append-glyph-at-line-end , 0 :join-line)
      (append-glyph-at-line-end , 0 :one)
      (append-glyph-at-line-end , 0 :join-line)
      (attach-child , 1 :zero)
      (attach-child , 3 :two)
    ) 0))
  )
