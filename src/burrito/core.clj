(ns burrito.core
  (:require
   [clojure.string]
   [clojure.reflect]
   [clojure.pprint]))

(def ^:dynamic *trim-return-types* false)
(def ^:dynamic *trim-class-type* false)
(def ^:dynamic *max-var-name-length* nil)
(def ^:dynamic *printable-primitive-types* false)

(defn camel->snake
  [s]
  (-> s
      (clojure.string/replace #"([^_A-Z])([A-Z])" "$1-$2")
      clojure.string/lower-case))

(defn trim-object-name
  [c]
  (->
   c
   str
   (clojure.string/split #"\.")
   last
   str))

(defn gen-param*
  [c]
  (->
   c
   trim-object-name
   (cond-> *max-var-name-length* (subs 0 *max-var-name-length*))
   camel->snake
   (clojure.string/replace #"<>" "-array")))

(defn fix-array-repr
  [o]
  (let [s (str o)]
    (if (clojure.string/ends-with? s "<>")
      (str "[L" (clojure.string/replace s #"<>" "") ";")
      o)))

(comment
  (fix-array-repr "Object<>")
  (def ^"[Lblah;" sym 1)
  (meta #'sym))

(defn gen-param
  [cnt c]
  [(inc cnt) (symbol (str (gen-param* c) cnt))])

(defn gen-hinted-param
  [cnt c]
  [(inc cnt) (with-meta (symbol (str (gen-param* c) cnt)) {:tag (fix-array-repr (with-meta c {}))})])

(defn emit-wrapper-body
  [class {:keys [name parameter-types]}]
  (let [[_this & params]
        (nth
         (reduce
          (fn [[cnt ps] c]
            (let [[cnt p] (gen-param cnt c)]
              [cnt (conj ps p)]))
          [0 ['c]]
          parameter-types) 1)
        method (symbol (str "." name))
        unhinted-params (map #(with-meta % {}) params)]
    `([~(with-meta 'c {:tag (if *trim-class-type*
                              (symbol (trim-object-name class))
                              class)})
       ~@unhinted-params]
      (~method ~_this ~@unhinted-params))))

(def primitives
  '#{int
     ints
     long
     longs
     float
     floats
     double
     doubles
     void
     short
     shorts
     boolean
     booleans
     byte
     bytes
     char
     chars})

(defn add-return-type-meta
  [fn-name return-type]
  (if (= return-type 'java.lang.Object)
    fn-name ;; Ignore object because that's everything by default
    (with-meta fn-name
      {:tag (fix-array-repr
             (if *trim-return-types*
               (symbol (trim-object-name return-type))
               return-type))})))

(defn emit-wrapper
  [class [method decls]]
  (let [fn-name (-> method str camel->snake symbol)
        return-type (vary-meta (:return-type (first decls)) empty)
        bodies (sort-by (comp count first) (map (partial emit-wrapper-body class) decls))
        prim (primitives return-type)
        prim (if (and prim *printable-primitive-types*) (symbol (str "'" prim)) prim)]
    (if prim
      (list* 'defn fn-name {:tag prim} bodies)
      (list* 'defn
             (add-return-type-meta fn-name return-type)
             bodies))))

(defn class->sym
  [c]
  (symbol (.getName c)))

(defn public-members
  [c]
  (filter
   (comp :public :flags)
   (:members (clojure.reflect/reflect c))))

(defn wrapping-candidates
  [c]
  (group-by :name (public-members c)))

(defn smoosh-types
  [ts]
  (with-meta
    (apply mapv
           (fn [& args]
             (symbol (clojure.string/join
                      "-or-"
                      (map gen-param* args))))
           ts)
    {:collapsed true}))

(comment
  (smoosh-types '[[a b] [c d]])
  )

(defn collapse-decls
  [decls]
  (let [ret (first decls)]
    (if (= 1 (count decls))
      ret
      (assoc ret :parameter-types
             (smoosh-types (map :parameter-types decls))))))

(defn collapse-candidates
  [candidates]
  (reduce-kv
   (fn [m k v]
     (assoc m k
            (mapv collapse-decls
                  (vals
                   (group-by (comp count :parameter-types) v)))))
   {}
   candidates))

(defn ->constructor
  [method]
  (binding [*max-var-name-length* nil]
    (symbol (str "->" (gen-param* method)))))

(defn extract-constructor
  [c [method decls]]
  (let [f (if (= (resolve method) c) ->constructor identity)]
    [c [(f method) decls]]))

(defn with-constructor
  [candidates c]
  (let [sym (class->sym c)
        decls (get candidates sym)]
    (cond-> {}
      (find candidates sym)
      (assoc sym (mapv (fn [m] (assoc m :return-type sym)) decls)))))

(defn emit-constructor-body
  [class {:keys [name parameter-types]}]
  (let [params
        (nth
         (reduce
          (fn [[cnt ps] c]
            (let [[cnt p] (gen-param cnt c)]
              [cnt (conj ps p)]))
          [0 []]
          parameter-types) 1)
        unhinted-params (map #(with-meta % {}) params)]
    `([~@unhinted-params]
      (new ~name ~@unhinted-params))))

(defn emit-constructor
  [m]
  (let [[class decls] (first m)
        fn-name (->constructor class)
        return-type (vary-meta (:return-type (first decls)) empty)
        bodies (sort-by (comp count first) (map (partial emit-constructor-body class) decls))]
    (list* 'defn
           (add-return-type-meta fn-name return-type)
           bodies)))

(defn prepare-ingredients
  [c]
  (-> c wrapping-candidates collapse-candidates))

(defn wrap
  ([c]
   (map
    (partial emit-wrapper c)
    (remove
     (comp (partial = c) resolve first)
     (prepare-ingredients c)))))

(defmacro wrap!
  [c]
  (let [c (resolve c)]
    `(do ~@(wrap c))))

(defn ret-types
  [c]
  (set (remove (some-fn #{'java.lang.Object} primitives nil?)
               (map :return-type (public-members c)))))

(comment
  (ret-types java.util.HashMap))

(defn imports*
  [names]
  (mapv
   (fn [[base classes]]
     (vec (list* base (map second classes))))
   (group-by
    first
    (map (fn [c]
           (let [c (clojure.string/split (str c) #"\.")
                 base (butlast c)
                 name (last c)]
             [(symbol (clojure.string/join "." base)) (symbol name)]))
         names))))

(defn imports
  [c]
  (-> c ret-types imports*))

(comment

  (-> java.util.HashMap
      wrapping-candidates
      collapse-candidates
      (with-constructor java.util.HashMap)
      emit-constructor)


  (wrapping-candidates java.util.HashMap)

  (wrap java.util.HashMap)

  (binding [*print-meta* true
            *trim-class-type* true
            *max-var-name-length* 1
            *trim-return-types* true]
    (doseq [e (wrap java.util.HashMap)]
      (clojure.pprint/pprint e)))

  (binding [*print-meta* true]
    (doseq [e (wrap java.util.HashMap)]
      (clojure.pprint/pprint e)))

  (binding [*print-meta* true
            *trim-class-type* true
            *max-var-name-length* 1
            *trim-return-types* true
            *printable-primitive-types* true]
    (doseq [e (wrap java.util.concurrent.ConcurrentLinkedDeque)]
      (clojure.pprint/pprint e)))

  (wrap! java.util.HashMap))
