(ns monad-clj.core
  (:gen-class)
  (:require [clojure.algo.monads :as m]))

;;domonad just made m-bind and m-result implicit
(m/domonad m/identity-m
           [a 1
            b (inc a)]
           (* a b))


;;with monad is like a global m-bind/m-result overrider
(=
 (m/with-monad m/identity-m
   (m/m-bind 1 (fn [a]
                 (m/m-bind (inc a) (fn [b]
                                     (m/m-result (* a b)))))))

 (let [m-bind (fn [v f]
                (f v))
       m-result identity]
   (m-bind 1 (fn [a]
               (m-bind (inc a) (fn [b]
                                 (m-result (* a b)))))))

 (let [a 1
       b (inc a)]
   (* a b)))

;;maybe monad is a nil checker
(=
 (m/with-monad m/maybe-m
   (m/m-bind nil (fn [a]
                   (m/m-bind (inc a) (fn [b]
                                       (m/m-result (* a b)))))))

 (let [m-bind (fn [v f]
                (if (nil? v)
                  nil
                  (f v)))
       m-result identity]
   (m-bind nil (fn [a]
                 (m-bind (inc a) (fn [b]
                                   (m-result (* a b))))))))




;; m-lift is like a contained values extractor for functions
(def nil-respecting-addition
  (m/with-monad m/maybe-m (m/m-lift 2 +)))

(nil-respecting-addition 1 nil)
(nil-respecting-addition 1 3)

;; sequence monad always make things non-deterministic'ish
(m/with-monad m/sequence-m
  (defn mymap [f xs]
    ((m/m-lift 1 f) xs)))

(mymap inc [1 3])

;; m-seq is like a power set
(m/with-monad m/sequence-m
  (m/m-seq [[1 2] [1 2]]))

(m/with-monad m/set-m
  (defn ntuples [n xs]
    (m/m-seq (replicate n xs))))

(ntuples 2 [1 2])

;; m-chain is like a self mapcatting pool
(=
 (m/with-monad m/sequence-m
   ((m/m-chain [parents parents]) (class [])))

 (mapcat parents (parents (class []))))

(m/with-monad m/sequence-m
  ((m/m-chain [range range]) 9))

(m/with-monad m/sequence-m
  ((m/m-chain [range (partial repeat 3)]) 9))

;; review maybe monad
(m/defmonad maybe-m
           [m-zero nil
            m-result (fn [v] v)
            m-bind (fn [mv f]
                     (if (nil? mv)
                       nil
                       (f mv)))
            m-plus (fn [& mvs]
                     (first (filter (complement nil?) mvs)))])

(m/with-monad maybe-m
  (m-bind nil (fn [a] (m-bind (inc a) (fn [b] (m-result b))))))

(m/with-monad maybe-m
  (m-bind 1 (fn [a] (m-bind (inc a) (fn [b] (m-result b))))))

;;with do-notation
(m/with-monad maybe-m
  (m/domonad  [a (m-result nil)
               b (m-result (inc a))]
              b))

;; review maybe monad
(m/defmonad sequence-m
  [m-result (fn [v]
              (vector v))
   m-bind (fn [mv f]
            (vec (mapcat f mv)))
   m-zero (vector)
   m-plus (fn [& mvs]
            (vec (apply concat mvs)))])

;; m-bind for a should be inside a m-result
;(m/with-monad sequence-m
;  (m-bind [1 2 3] (fn [a] (m-bind (inc a) (fn [b] (m-result b))))))

(m/with-monad sequence-m
  (m-bind [1 2 3] (fn [a] (m-bind (m-result (inc a)) (fn [b] (m-result b))))))

(m/with-monad sequence-m
  (m/domonad [a [1 2 3]
              b (m-result (inc a))]
             b))

;;the maybe monad transformer
(defn maybe-t
  [m]
  (m/monad [m-result (m/with-monad m m-result)
            m-bind (m/with-monad m
                     (fn [mv f]
                       (m-bind mv
                               (fn [x]
                                 (if (nil? x)
                                   (m-result nil)
                                   (f x))))))
            m-zero (m/with-monad m m-zero)
            m-plus (m/with-monad m m-plus)
            ]))

(def maybe-in-seq-M (maybe-t sequence-m))

(m/with-monad maybe-in-seq-M
  (m/domonad [a [1 2 3 nil]
              b (m-result (inc a))]
             b))

(m/with-monad maybe-in-seq-M
  (m/domonad [a [0 nil 1 2 3]
              b [1 2 3 4 5]]
             (* a b)))
;;notice that nil is only appear once. Because (* a b) is ommited.  Whether the last m-result is executed depends on the m-bind, in this case, when a is a nil, the b binding function will not be executed - line:140

;; probability distribution monad
(m/defmonad dist-m
  [m-result (fn [v] {v 1})
   m-bind   (fn [mv f]
              (letfn [(add-prob [dist [x p]]
                        (assoc dist x (+ (get dist x 0) p)))]
                (reduce add-prob {}
                        (for [[x p] mv
                              [y q] (f x)]
                          [y (* q p)]))))])

;; monty halls problem
(defn uniform [s]
  (apply array-map
         (interleave s (repeat (/ 1 (count s))))))

(def doors #{:a :b :c})

;; picked a door
(m/domonad dist-m
           [prize (uniform doors)
            choice (uniform doors)]
           (= choice prize))

;; revealed a door of no car
(m/domonad dist-m
           [prize (uniform doors)
            choice (uniform doors)
            reveal (uniform (disj doors prize choice))]
           (= prize reveal))

;; picked another door
(m/domonad dist-m
           [prize (uniform doors)
            choice (uniform doors)
            reveal (uniform (disj doors prize choice))
            switch (uniform (disj doors choice reveal))]
           (= switch prize))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
