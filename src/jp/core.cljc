(ns jp.core
  (:require [its.log :as log]))

(defn all-paths
  "Find all terminal key paths in a map.

   eg.
      (all-paths {:a {:b 1 :c 2} :d {:e {:f 3}}})
      ;; => [[:a :b] [:a :c] [:d :e :f]]
  "
  [m]
  ;; http://stackoverflow.com/questions/21768802/how-can-i-get-the-nested-keys-of-a-map-in-clojure
  ;; (log/debug ::all-paths :m :redacted)
  (if-not (map? m)
    []
    (vec (mapcat (fn [[k v]]
                  (let [sub (all-paths v)
                        nested (map #(into [k] %) (filter (comp not empty?) sub))]
                    (if (seq nested)
                      nested
                      [[k]])))
                m))))

(defn args->opts
  "Converts an arglist of k/v pairs (eg. `[:a 1 :b 2]`) into a map (`{:a 1 :b 2}`)."
  [args]
  (->> args
       (partition 2)
       (mapv vec)
       (into {})))

(defn call-back
  "Apply a callback function `f` to `args` trapping and logging any exceptions."
  [f & args]
  (try
    (apply f args)
    (catch #?(:clj Exception :cljs :default) ex
      (log/error ::call-back :failed ex)
      (log/error ::call-back :f f)
      (log/error ::call-back :args args)
      #?(:cljs (js/console.log ex.stack)))))

(defn conjv
  "Like `conj` but returns an empty vector instead of an empty list when `coll`
   is `nil`."
  [coll x]
  (if coll
    (conj coll x)
    [x]))

(defn contains-all?
  "Returns `true` if collection `c` contains every key in `ks`."
  [c ks]
  (every? #(contains? c %) ks))

(defn ensure-with
  "Ensures `pred?` is true of `arg` by calling `xform` on `arg` if
   `(pred? arg)` is not already true.

   eg.
      (ensure-with map? #(-> {:val %}) {:val 7})  ;; => {:val 7}
      (ensure-with map? #(-> {:val %}) 5)         ;; => {:val 5}
  "
  [pred? xform arg]
  (if (pred? arg)
    arg
    (xform arg)))

(defn filterm
  "Filters a seq of [k v] pairs (like a map) and returns a map."
  [f xs]
  (->> xs
       seq
       (filter f)
       (into {})))

(declare seq!)

(defn not-implemented!
  "Logs and raises an exception indicating that the execution of the code
  has reached a point that is not implemented yet."
  [& args]
  (apply log/error :not-implemented args)
  (throw (ex-info (if (empty? args)
                    "not-implemented"
                    (str "not-implemented: " (vec args)))
                  {:args args})))

(defn seq!
  "If `x` is sequential, returns it unchanged, otherwise returns a vector
  containing `x`.  Helpful when writing functions that operate on either an
  individual item or a collection of items (ie to turn a korks into a ks)."
  [x]
  (if (sequential? x)
    x
    [x]))

(defn sortv-by
  "Like `sort-by` but returns a vector instead of a list."
  ([keyfn coll]
   (vec (sort-by keyfn coll)))
  ([keyfn comp coll]
   (vec (sort-by keyfn comp coll))))

(defn sort-table-by
  "Sort a `:foo/by-id` style \"table\" by a \"column\".

   eg. (sort-table {1 {:id 1 :name \"x\"} 2 {:id 2 :name \"a\"}})
   ;; => [{:id 2 :name \"a\"} {:id 1 :name \"x\"}]
  "
  [k table]
  (->> table
       vals
       (sortv-by k)))

(defn truncate
  "Truncate string `s` to length `sz` with suffix `suff` defaulting to `...`.
   a non-positive size"
  ([s sz]
   (truncate s sz "..."))
  ([s sz suff]
   (assert (<= (count suff) sz))
   (let [len     (count s)
         sufflen (count suff)]
     (cond
       (not (pos? len)) ""
       (<= len sz)      s
       :else            (let [r    (min (- sz sufflen) len)
                              base (subs s 0 r)]
                          (str base suff))))))
