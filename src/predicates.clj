(ns predicates)

(defn sum-f [f g x]
  (+ (f x) (g x)))

(defn less-than [n]
  (fn [k] (< k n)))

(defn equal-to [n]
  (fn [k] (== k n)))

(defn set->predicate [a-set]
  (fn [k] (contains? a-set k)))

(defn pred-and [pred1 pred2]
  (fn [k] (and (pred1 k) (pred2 k))))

(defn pred-or [pred1 pred2]
  (fn [k] (or (pred1 k) (pred2 k))))

(defn whitespace? [character]
  (Character/isWhitespace character))

(defn blank? [string]
  (every? (pred-or whitespace? (fn( [c] (or (= nil c) (= "" c))))) string))

(defn has-award? [book award]
  (contains? (book :awards) award))

(defn HAS-ALL-THE-AWARDS? [book awards]
  (every? (fn [x] (has-award? book x))  awards))

(defn my-some [pred a-seq]
  (let [result (filter (fn [x] (pred x)) a-seq)]
    (if (= [1] (first result))
      (get (first result) 0)
      ((complement empty?) result))))

(defn my-every? [pred a-seq]
  (let [result (filter pred a-seq)]
    (= (count result) (count a-seq))))

(defn prime? [n]
  (let [pred (fn [x] (zero? (mod n x)))]
    (not (some pred (range 2 n)))))
;^^
