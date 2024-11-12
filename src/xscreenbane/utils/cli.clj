(ns xscreenbane.utils.cli
  (:require [clojure.string :as string]))

;(def reserved [:root :window-id :palette])

(defn keywordize-arg [argstr]
  (if (= \- (first argstr))
    (keyword (string/replace-first argstr #"-+" ""))
    argstr))

(defn keywordize-args [argl]
  (mapv keywordize-arg argl))

(defn pair-get 
  "Treat a list like a faux-hashmap and get a key's value. 
  Example: (faux-get [:param1 :param2 \"a value\" \"another value\" :param3] :param2)
   => \"a value\" "
  [paramlist value]
  (try (when-let [idx (.indexOf (keywordize-args paramlist) value)]
      (nth paramlist (inc idx))) 
    (catch IndexOutOfBoundsException e nil)))

