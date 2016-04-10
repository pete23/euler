(ns euler.core
  (use clojure.math.combinatorics)
  (use clojure.math.numeric-tower))

(def primes
  (concat
   [2 3 5 7]
   (lazy-seq
    (let [primes-from
      (fn primes-from [n [f & r]]
        (if (some #(zero? (rem n %))
              (take-while #(<= (* % %) n) primes))
          (recur (+ n f) r)
          (lazy-seq (cons n (primes-from (+ n f) r)))))
      wheel (cycle [2 4 2 4 6 2 6 4 2 4 6 6 2 6  4  2
            6 4 6 8 4 2 4 2 4 8 6 4 6 2  4  6
            2 6 6 4 2 4 6 2 6 4 2 4 2 10 2 10])]
      (primes-from 11 wheel)))))

(defn strdigits [string]
  (vec (map #(- (int %) (int \0)) string)))

(defn digits
  ([n] (digits n '()))
  ([n a] (if (= n 0)
        a
        (let [digit (rem n 10)
              left (quot n 10)]
          (recur left (conj a (int digit)))))))

(defn number-from-digits
  ([s] (number-from-digits (rest s) (first s)))
  ([s a] (if (empty? s) a
           (recur (rest s) (+ (* 10 a) (first s))))))

(defn ndigits
  ([n] (ndigits n 0))
  ([n a] (if (< n 1)
           a
           (recur (/ n 10) (inc a)))))

(defn roll
  ([d] (roll d 1 ))
  ([d r] (+ r (reduce + (repeatedly #(rand-int d)))))
  ([d r a] (if
             (= r 0) a
             (recur d (dec r) (+ a 1 (rand-int d))))))

(defn factor [n]
  (some #(if (= 0 (mod n %)) (vector % (/ n %))) (take-while #(<= (* % %) n) primes)))

(defn factor-w-prime [n]
  (let [factor (factor n)]
    (if factor factor [n 1])))

(defonce factor-oracle (vec (apply concat (pmap factor-w-prime (range 1 10000000 2)))))

(defn oracle-prime-factors
  ([n] (oracle-prime-factors n '()))
  ([n a] (let [shift (bit-shift-right n 1)
               offset (+ shift shift)]
           (cond (<= n 1) a
                 (odd? n) (recur (nth factor-oracle (inc offset)) (conj a (nth factor-oracle offset)))
                 true (recur shift (conj a 2))))))

(defn oracle-prime? [^long n]
  (cond (= 2 n) 1
        (even? n) 0
        (= 1 (nth factor-oracle n)) 1
        true 0))

(defn prime? [^long n] (if (.isProbablePrime (java.math.BigInteger/valueOf n) 5) 1 0))

(defn spiral [n i]
  (cons [n (+ i n) (+ i i n) (+ i i i n)] (lazy-seq (spiral (+ 2 i i i i n) (+ 2 i)))))

(defn euler-58-accumulator
  ([spiral-prime-counts] (euler-58-accumulator 2 0 spiral-prime-counts))
  ([spirals primes spiral-prime-counts]
    (let [primes (+ primes (first spiral-prime-counts))
          corners (- (* 4 spirals) 3)
          ratio (/ primes corners)]
	      (if (< ratio 1/10)
	        (dec (* 2 spirals))
	        (recur (inc spirals) primes (rest spiral-prime-counts))))))

(defn euler-58 []
  (euler-58-accumulator (map #(reduce + (map prime? %)) (spiral 3 2))))

(def rank { \2 2 \3 3 \4 4 \5 5 \6 6 \7 7 \8 8 \9 9 \T 10 \J 11 \Q 12 \K 13 \A 14 })

(defn rank-hand [h]
  (vec (reverse (sort (map second (group-by identity (map first h)))))))

(defn load-poker-hands []
  (partition 2 (partition 5
      (map #(vector (rank (first %)) (second %))
           (clojure.string/split (slurp "poker.txt") #"[ \n]")))))

(defn flush? [h r]
  (let [suits (map second h)]
    (if (every? #(= (first suits) %) (rest suits)) r)))

(defn straight? [_ r]
  (let [flat (flatten r)]
    (if (every? #(= 1 %) (map #(- %1 %2) flat (rest flat))) r)))

(defn straight-flush? [h r]
  (if (flush? h r) (straight? h r)))

(defn n-of-a-kind? [n _ r]
  (if (= n (count (first r))) r))

(defn m-n-of-a-kind? [m n _ r]
  (if (and (= m (count (first r)))
           (= n (count (second r)))) r))

(defn high-card? [h r] r)

(def hand-hierarchy
  [straight-flush?
   (partial n-of-a-kind? 4)     ; four of a kind
   (partial m-n-of-a-kind? 3 2) ; full house
   flush?
   straight?
   (partial n-of-a-kind? 3)     ; three of a kind
   (partial m-n-of-a-kind? 2 2) ; two pairs
   (partial n-of-a-kind? 2)     ; pair
   high-card?])

(defn compare1 [f hands]
  (apply compare (map #(f % (rank-hand %)) hands)))

(defn compare-poker-hands [hands]
  (some #(if (not= 0 %) %) (map #(compare1 % hands) hand-hierarchy)))

(defn euler-54 []
  (count
    (filter #(= 1 %)
      (map compare-poker-hands (load-poker-hands)))))

(defn eligible-digit [n]
  (if (> n 100000)
    (some #(if (and (= 3 (second %)) (> 3 (first %))) (first %))
          (frequencies (drop-last (digits n))))))

(defn count-prime-subst [n d]
   (let [dig (digits n)
         rng (if (= (first dig) d) ; avoid situations where we 0 first digit
               (range 1 10)        ; hackety hackety hack
               (range 10))]
     (reduce + (map #(prime? (number-from-digits (replace {d %} dig))) rng))))

(defn check-prime-subst [m n d]
  (if (= m (count-prime-subst n d)) n))

(defn euler-51 []
  (some #(if-let [ed (eligible-digit %)]
         (check-prime-subst 8 % ed)) primes))

(defn triangle [n]
  (/ (* n (inc n)) 2))

(defn square [n]
  (* n n))

(defn pentagon [n]
  (/ (* n (dec (* 3 n))) 2))

(defn hexagon [n]
  (* n (dec (* 2 n))))

(defn heptagon [n]
  (/ (* n (- (* 5 n) 3)) 2))

(defn octagon [n]
  (* n (- (* 3 n) 2)))

(defn four-digit-pastrami [f]
  (filter #(> (rem % 100) 9) (take-while #(< % 10000) (drop-while #(< % 1000) (map f (range))))))

(defn div [a b] (int (/ a b)))

(defn two-digit-map [f]
  (map #(vector (div % 100) (rem % 100)) (four-digit-pastrami f)))

(defn all-links [] (map two-digit-map [octagon pentagon triangle heptagon square hexagon]))

(defn each-and-rest
  ([s] (each-and-rest '() '() (first s) (rest s)))
  ([a bef at aft] (if (nil? at) a
                    (recur (conj a (vector at (concat bef aft)))
                           (conj bef at)
                           (first aft)
                           (rest aft)))))

(defn make-links [from to]
  (filter identity
    (for [f from
          t to]
      (if (= (last f) (first t)) (conj f (second t))))))

(defn recursively-link [from [to remaining]]
  (let [chains (make-links from to)]
    (if (empty? remaining)
      chains
      (mapcat (partial recursively-link chains) (each-and-rest remaining)))))

(defn find-chain [components]
  (some #(if (= (first %) (last %)) %)
    (mapcat (partial recursively-link (first components)) (each-and-rest (rest components)))))

(defn euler-61 []
    (* 101 (reduce + (rest (find-chain (all-links))))))

(defn match [digits test]
  (cond (empty? test) true
        (empty? digits) false
        true (let [test (if (= (first digits) (first test)) (rest test) test)]
               (recur (rest digits) test))))

(defn match-all [tests digits]
  (if (every? (partial match digits) tests) digits))

(defn euler-79 []
  (let [codes (map strdigits (clojure.string/split (slurp "keylog.txt") #"\n"))]
    (some (partial match-all codes) (map digits (range 73100000 732000000)))))

(defn totient
  ([n] (totient n (set (oracle-prime-factors n))))
  ([n distinct-factors] (* n (reduce * (map #(- 1 (/ 1 %)) distinct-factors)))))

(defn my-comp [x]
  (fn
     ([a] a)
     ([a b] (if (= x (compare a b)) a b))))

(def my-max (my-comp 1))
(def my-min (my-comp -1))

(defn euler-69 []
  (reduce my-max (map #(vector (/ % (totient %)) %) (range 2 1000001))))

(defn euler-70 []
  (reduce my-min
    (map #(vector (/ % (totient %)) %)
       (filter #(= (frequencies (digits %))
                   (frequencies (digits (totient %)))) (range 2 10000001)))))

(defn farey-next
  ([n ab cd]
    (let [a (numerator ab) b (denominator ab)
          c (numerator cd) d (denominator cd)]
      (farey-next n a b c d)))
  ([n a b c d]
    (let [nbd (int (/ (+ n b) d))
          p (- (* c nbd) a)
          q (- (* d nbd) b)]
      [p q])))

(defn farey-sequence
  ([n] (farey-sequence n 1 n 1 (dec n)))
  ([n a b c d]
    (cons (/ a b)
          (lazy-seq (let [[e f] (farey-next n a b c d)]
                      (farey-sequence n c d e f))))))

(defn euler-72 []
  (count (drop-while #(<= % 1/3) (take-while #(< % 1/2) (farey-sequence 12000)))))

(defn euler-72-hard-recur
  ([] (euler-72-hard-recur [1 12000 1 3 4000 11999]))
  ([[^int acc ^int n ^int a ^int b ^int c ^int d]]
    (if (and (= c 1) (= d 2)) acc
      (let [nbd (int (/ (+ n b) d))
            e (- (* c nbd) a)
            f (- (* d nbd) b)]
        (recur [(inc acc) n c d e f])))))

(defn get-triangles []
 (partition 3 (partition 2 (map read-string (clojure.string/split (slurp "triangles.txt") #"[,\n]")))))

(defn twice-area-triangle [[^int ax ^int ay] [^int bx ^int by] [^int cx ^int cy]]
  (Math/abs (+ (* ax (- by cy)) (* bx (- cy ay)) (* cx (- ay by)))))

(defn contains-origin [[a b c]]
  (let [origin [0 0]]
    (= (twice-area-triangle a b c)
       (+ (twice-area-triangle a b origin)
          (twice-area-triangle b c origin)
          (twice-area-triangle c a origin)))))

(defn euler-102 []
  (count (filter true? (map contains-origin (get-triangles)))))

; given numbers x and y, are xy and yx prime?
(defn concatted-prime? [x y]
  (let [xdig (digits x)
        ydig (digits y)
        xy (number-from-digits (concat xdig ydig))
        yx (number-from-digits (concat ydig xdig))]
    (and (= 1 (prime? xy))
         (= 1 (prime? yx)))))

; if the predicate applies against the test value vs each member of the set, return a set including the test value
(defn refine-set-match-one-value [predicate values to-test]
  (if (every? (partial predicate to-test) values)
    (conj values to-test)))

; return sets where the each of the initial values can be matched with a test value through the predicate
(defn refine-set [predicate values test-values]
  (let [lim (last values)]
    (filter identity
      (for [e test-values :while (< e lim)]
        (refine-set-match-one-value predicate values e)))))

; return a set of sets-of-values, where all of the initial sets-of-values can be matched with additional test values through the predicate
(defn refine-sets [predicate test-values sets-of-values]
  (mapcat #(refine-set predicate % test-values) sets-of-values))

(defn euler-60 []
  (let [primes-in-scope (take-while #(< % 10000) (drop 1 primes)) ; try <10000 to start
        pairs (refine-sets concatted-prime? primes-in-scope (map vector primes-in-scope))
        pair-factors (set (map (fn [[a b]] (* a b)) pairs))
        pair-predicate #(pair-factors (* %1 %2))
        refine-pair-sets (partial refine-sets pair-predicate primes-in-scope)
        triples (refine-pair-sets pairs)
        quads (refine-pair-sets triples)
        quins (refine-pair-sets quads)]
    (first (map #(apply + %) quins))))

(defn sqrt-2-expansion
     ([n] (sqrt-2-expansion n 2))
     ([n a] (if (= n 0)
              (+ 1 (/ 1 a))
              (recur (dec n) (+ 2 (/ 1 a))))))

(defn more-num-digits-than-den? [n]
  (> (ndigits (numerator n)) (ndigits (denominator n))))

(defn euler-57 []
  (count (filter more-num-digits-than-den? (map sqrt-2-expansion (range 1 1000)))))

; given the starting number and a FINITE sequence of continued fraction, evaluates this as a ratio
(defn evaluate-convergent-sequence
  ([start continued-fraction]
    (if (empty? continued-fraction)
      start
      (let [backwards (reverse continued-fraction)] ; as the function needs to be evaluated bottom up
        (evaluate-convergent-sequence start (rest backwards) (first backwards)))))
  ([start continued-fraction accumulator]
    (if (empty? continued-fraction)
      (+ start (/ 1 accumulator))
      (recur start (rest continued-fraction) (+ (first continued-fraction) (/ 1 accumulator))))))

; a sequence of the continued fraction for e - 1 2 1 1 4 1 1 6 1 ...
(defn continued-fraction-for-e
  ([] (continued-fraction-for-e 2))
  ([n] (concat [1 n 1] (lazy-seq (continued-fraction-for-e (+ 2 n))))))

; a sequence of the continued fraction of sqrt 2
(def continued-fraction-for-sqrt-2
  (repeat 2))

; given a start and a (possibily infinite) sequence of continued fraction, return a lazy sequence of the expansion
(defn convergent-sequence [start fraction-sequence]
  (map #(evaluate-convergent-sequence start (take % fraction-sequence)) (range)))

(defn euler-65 []
  ; 100th number in sequence is our 99th due to starting with the 0th:-)
  (apply + (digits (numerator (nth (convergent-sequence 2 (continued-fraction-for-e)) 99)))))

(defn n-rects [^long x ^long y]
  (* 1/4 (* x (inc x)) (* y (inc y))))

(defn quadratic-solution
  ([a b c] [(quadratic-solution -1 a b c) (quadratic-solution 1 a b c)])
  ([sgn a b c] (/ (+ b (* sgn (first (exact-integer-sqrt (- (* b b) (* 4 a c))))))  (* 2 a))))

(defn closest-x-for-y [^long y]
  (let [a (* y (inc y))
        b a
        c (* -4 2000000)]
    (abs (first (quadratic-solution a b c)))))

(defn e-85-find-closest [target]
  (loop [y 1
         best-error target
         best-area 0]
    (let [x (closest-x-for-y y)
          rects (n-rects x y)
          diff (- target rects)
          error (abs diff)
          next-error (min error best-error)
          next-area (if (= error best-error) [x y] best-area)]
      (if (= y 1000) next-area
        (recur (inc y) next-error next-area)))))

(defn euler-85 []
  (e-85-find-closest 2000000))

(defn find-prime-sums [n]
  (count
    (let [f #(< % n)]
      (set
        (filter f
                (for [s (take-while f (map #(* % %) primes))
                      t (take-while f (map #(* % % %) primes))
                      u (take-while f (map #(* % % % %) primes))]
                  (+ s t u)))))))

(defn euler-87 []
  (find-prime-sums 50000000))

; get integer part of root
; multiplicand = 1, additive = -integer part -4
; additive = -additive 4, multiplicand = multiplicand 1 / (n - additive^2) 7
; digit = floor (* multiplicand (root + additive))
(defn step [n additive divisor]
     (let [; flip upside by multiplying by opposite fraction
           multiplicand (/ divisor (- n (* additive additive)))
           additive (- additive)
           ; get digit as the whole number part
           digit (int (* multiplicand (+ (sqrt n) additive)))
           ; work out what's left
           new-additive (/ (- (* multiplicand additive) digit) multiplicand)
           new-divisor (/ 1 multiplicand)]
       [digit [new-additive new-divisor]]))

(defn root-continued-fraction-seq
  ([n] (let [[root remainder] (exact-integer-sqrt n)]
         (if (= remainder 0) []
           (root-continued-fraction-seq n [(- (first (exact-integer-sqrt n))) 1/1] [] #{}))))
  ([n add-div accumulator seen] ; we keep the add/div as a pair so we can check the set for their presence trivially
    (if (seen add-div) accumulator ; if we've previously computed this additive/divisor pair, sequence over
      (let [seen (conj seen add-div)
            [additive divisor] add-div
            next-step (step n additive divisor)
            accumulator (conj accumulator (first next-step))
            next-add-div (second next-step)]
        (recur n next-add-div accumulator seen)))))

(defn euler-64 [n]
  (count (filter odd? (map count (map root-continued-fraction-seq (range (inc n)))))))

(defn pascal-step [n]
     (map + (concat '(1) n) n))

(defn pascal
  ([] (cons [2] (pascal [2])))
  ([n] (let [odd-step (pascal-step n)
             even-step (conj (vec (pascal-step odd-step)) (* 2 (last odd-step)))]
         (cons odd-step (cons even-step (lazy-seq (pascal even-step)))))))

(defn pascal-distinct-in-rows [n]
  (set (cons 1 (flatten (take (- n 2) (pascal))))))

(def prime-squares (map #(* % %) primes))

(defn divisible-by? [n m]
  (if (= 0 (rem n m)) m))

(defn divisible-by-prime-square? [n]
   (some (partial divisible-by? n) (take-while #(<= % n) prime-squares)))

(defn euler-203 []
   (reduce + (filter #(not (divisible-by-prime-square? %)) (pascal-distinct-in-rows 51))))

(defn counter-create [n]
  (long-array n))

(defn counter-carry [^longs a]
  (loop [a a n 0]
    (if (== 0 (aget a n))
      (recur a (inc n))
      (if (= (inc n) (alength a))
        nil
        (do
          (aset a n 0)
          (aset a (inc n) (inc (aget a (inc n))))
          a)))))

(defn counter-inc [^longs a]
  (do (aset a 0 (inc (aget a 0)))) a)

(defn pow
  ([^long x ^long y] (pow 1 x y))
  ([^long a ^long x ^long y]
    (if (zero? y) a
      (recur (* a x) x (dec y)))))

(defn product [^longs x ^longs y]
  (loop [x x y y n 0 a 1]
    (if (>= n (alength x)) a
      (recur x y (inc n) (* a (pow (aget x n) (aget y n)))))))

(defn next-counter [primes counter maximum]
  (if (nil? counter) nil
    (let [next-number (product primes counter)]
      (if (<= next-number maximum) [next-number counter]
        (recur primes (counter-carry counter) maximum)))))

(defn hamming-sequence [primes number counter maximum]
  (let [c (next-counter primes (counter-inc counter) maximum)]
    (if (nil? c) [number]
      (cons number (lazy-seq (hamming-sequence primes (first c) (second c) maximum))))))

(defn hamming-numbers-less-than [type n]
  (let [primes (take-while #(<= % type) primes)
        maximum n]
    (hamming-sequence (long-array primes) 1 (counter-create (count primes)) maximum)))

(defn euler-204 []
  (count (hamming-numbers-less-than 100 1000000000)))



(def monopoly-board)

(defn mono-square [game] (nth monopoly-board (:location game)))

(defn move-player [from spaces]
  (rem (+ from spaces) 40))

(defn mono-move [dice-roll game]
  (update-in game [:location] #(move-player % dice-roll)))

(defn go-to-one-of [game locations]
  (if (locations (mono-square game)) game
    (recur (mono-move 1 game) locations)))

(defn go-to [game & locations]
  (go-to-one-of game (set locations)))

(defn go-to-jail [game] (go-to game :jail))

(defn mono-act [game]
  (let [square (mono-square game)]
    (if (fn? square) (square game) game)))

(defn rotate [r] (conj (subvec r 1) (first r)))

(defn monopoly-card [key game]
  (let [card (first (game key))
        game (update-in game [key] rotate)]
    (cond (keyword? card) (go-to game card)
          (set? card) (go-to-one-of game card)
          (fn? card) (mono-act (card game))
          true game)))

(def chest (partial monopoly-card :chest))
(def chance (partial monopoly-card :chance))

(def monopoly-board [:go :a1 chest :a2 :t1 :r1 :b1 chance :b2 :b3
                     :jail :c1 :u1 :c2 :c3 :r2 :d1 chest :d2 :d3
                     :fp :e1  chance :e2 :e3 :r3 :f1 :f2 :u2 :f3
                     go-to-jail :g1 :g2 chest :g3 :r4 chance :h1 :t2 :h2])

(defn zero [n] 0)

(defn count-conseq-doubles [game dice-roll]
  (update-in game [:doubles] (if (apply == dice-roll) inc zero)))

(defn monopoly-turn [dice-roll game]
  (let [game (count-conseq-doubles game dice-roll)]
    (if (= 3 (:doubles game))
      (go-to-jail (assoc game :doubles 0))
      (mono-act (mono-move (apply + dice-roll) game)))))

(defn new-freqs []
  (apply hash-map (drop-last (interpose 0 (range 41)))))

(def rails (set [:r1 :r2 :r3 :r4]))
(def utils (set [:u1 :u2]))

(defn new-monopoly [] {:location 0
                       :doubles 0
                       :chest (shuffle [:go :jail nil nil nil nil nil nil nil nil nil nil nil nil nil nil])
                       :chance (shuffle [:go :jail :c1 :e3 :h2 :r1
                                         rails rails utils
                                         (partial mono-move -3)
                                         nil nil nil nil nil nil])})

(defn monopoly-frequencies [dice]
  (loop [game (new-monopoly)
         n 1000000
         f (new-freqs) ]
    (if (== n 0) f
      (recur (monopoly-turn (dice) game) (dec n) (update-in f [(:location game)] inc)))))

(defn euler-84 []
  (sort-by second (monopoly-frequencies (fn [] [(inc (rand-int 4)) (inc (rand-int 4))]))))

(defn make-multi [s e]
  (if (nil? e) [s]
    (conj e s)))

(defn build-ana-map-fn [keyfn]
  (fn [m s]
    (update-in m [(keyfn s)] #(make-multi s %))))

(defn index-of [s c]
  (.indexOf s (int c)))

(defn ana-transformer [[s t]]
  (map #(index-of s %) t))

(defn int-sqrt-round-up [n]
  (let [root (exact-integer-sqrt n)]
    (if (== (second root) 0) (first root)
      (inc (first root)))))

(defn squares-of-length [n]
  (let [low (int-sqrt-round-up (pow 10 (dec n)))
        high (int-sqrt-round-up (pow 10 n))]
  (map #(* % %) (range low high))))

(defn build-ana [source-data ana-fn]
  (mapcat #(combinations % 2)
          (filter #(< 1 (count %))
                  (map second (reduce (build-ana-map-fn ana-fn) {} source-data)))))

(defn ana-form [ana number]
  (number-from-digits
    (let [dig (digits number)]
      (map #(nth dig %) ana))))

(defn ana-flarp [haystack a]
  (let [tran (ana-transformer a)]
    (if (or (= (ana-form tran (second haystack)) (first haystack))
            (= (ana-form tran (first haystack)) (second haystack)))
            [a haystack])))

(defn match-ana-groups [groups haystack]
  (filter not-empty (map #(ana-flarp haystack %) groups)))

(defn euler-98-test-group [[size groups]]
  (let [haystack (build-ana (squares-of-length size) #(sort (digits %)))]
    (filter not-empty (map #(match-ana-groups groups %) haystack))))

(defn build-ana-from-words []
   (let [words (clojure.string/split (clojure.string/replace (slurp "words.txt") #"\"" "") #",")]
     (build-ana words sort)))

(defn euler-98 []
  (let [ana (build-ana-from-words)]
    (map euler-98-test-group (group-by #(count (first %)) ana))))

(use 'clojure.tools.trace)

                                        ; f(n) = sum(for each prime p < n f(n - p)) + 1 if n prime


(defn next-ways [ways prime]
  (loop [n prime
         ways ways]
    (if (< n (count ways))
      (recur (inc n) (assoc ways n (+ (ways n) (ways (- n prime)))))
      ways)))

(defn calc-ways [ways]
  (reduce next-ways ways (take 50 primes)))

(defn euler-77 []
  (count
   (take-while
    (partial > 5000)
    (calc-ways (vec (concat '(1) (repeat 100 0)))))))

                                        ; words pairs become transforms i.e. CARE RACE becomes 3214
                                        ; test by applying transform to (digits number nth
(defn pythagorean-triple [^long m ^long n]
  [(- (* m m) (* n n)) (* 2 m n) (+ (* m m) (* n n))])

(defn gcd [^long a ^long b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(def pythagorean-triples (for [^long m (drop 1 (range))
                               ^long n (drop 1 (range)) :while (> m n) :when (and (odd? (+ m n))
                                                                                  (= 1 (gcd m n)))]
                           (pythagorean-triple m n)))


(defn generate-ls [v]

(defn euler-75 []
  (count
   (filter #(= 1 %)
           (generate-ls (vec (repeat 1500001 0))))))
