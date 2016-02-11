(ns thinktopic.aljabr.test-all
  (:require
   [thinktopic.aljabr.core :as nd]
   [clojure.core.matrix :as m]
   [#?@(:clj [clojure.test :refer] :cljs [cljs.test :refer-macros]) [is deftest run-tests run-all-tests]]
   #?(:cljs [thi.ng.typedarrays.core :as a])))

#?(:cljs (enable-console-print!))

(deftest test-3d
  (let [shape [3 3 3]
        raw   (range (apply * shape))
        a     (nd/ndarray :int8 raw shape)]
    (is (satisfies? nd/PNDArray a))
    #?(:cljs (is (a/typed-array? (nd/data a))))
    (is (= shape (m/shape a)))
    (is (= [9 3 1] (nd/stride a)))
    (is (= (apply * shape) (nd/size a)))
    ;(is (= 3 (nd/dimension a)))
    (is (= raw (seq a)))
    (is (= [2 2 2]
           (-> a (nd/truncate-h 2 2 2) m/shape)))
    (is (= [0 1 3 4 9 10 12 13]
           (-> a (nd/truncate-h 2 2 2) seq)))
    (is (= (-> a (nd/truncate-h 2 2 2) seq)
           (-> a (nd/truncate-h -1 -1 -1) seq)))
    (is (= [2 2 2]
           (-> a (nd/truncate-l 1 1 1) m/shape)))
    (is (= [13 14 16 17 22 23 25 26]
           (-> a (nd/truncate-l 1 1 1) seq)))
    (is (= [13]
           (-> a (nd/truncate-h 2 2 2) (nd/truncate-l 1 1 1) seq)))
    (is (= [0 1 2 3 4 5 6 7 8 18 19 20 21 22 23 24 25 26]
           (-> a (nd/step 2 nil nil) seq)))
    (is (= [0 1 2 6 7 8 9 10 11 15 16 17 18 19 20 24 25 26]
           (-> a (nd/step nil 2 nil) seq)))
    (is (= [0 2 3 5 6 8 9 11 12 14 15 17 18 20 21 23 24 26]
           (-> a (nd/step nil nil 2) seq)))
    (is (= [9 11 12 14 15 17 18 20 21 23 24 26]
           (-> a (nd/truncate-l 1 0 0) (nd/step nil nil 2) seq)))
    (is (= [3 5 6 8 12 14 15 17 21 23 24 26]
           (-> a (nd/truncate-l 0 1 0) (nd/step nil nil 2) seq)))
    (is (= [1 4 7 10 13 16 19 22 25]
           (-> a (nd/truncate-l 0 0 1) (nd/step nil nil 2) seq)))
    (is (= [18 19 20 21 22 23 24 25 26 9 10 11 12 13 14 15 16 17 0 1 2 3 4 5 6 7 8]
           (-> a (nd/step -1 nil nil) seq)))
    (is (= [6 7 8 3 4 5 0 1 2 15 16 17 12 13 14 9 10 11 24 25 26 21 22 23 18 19 20]
           (-> a (nd/step nil -1 nil) seq)))
    (is (= [2 1 0 5 4 3 8 7 6 11 10 9 14 13 12 17 16 15 20 19 18 23 22 21 26 25 24]
           (-> a (nd/step nil nil -1) seq)))
    (is (= [0 9 18 3 12 21 6 15 24 1 10 19 4 13 22 7 16 25 2 11 20 5 14 23 8 17 26]
           (-> a (nd/transpose 2 1 0) seq)))
    (is (= [0 3 6 9 12 15 18 21 24 1 4 7 10 13 16 19 22 25 2 5 8 11 14 17 20 23 26]
           (-> a (nd/transpose 2 0 1) seq)))
    (is (= (reduce + (seq a)) (reduce + a)))
    (is (= (reduce + 0 (seq a)) (reduce + 0 a)))
    (is (= (seq (nd/truncate-l a 1 1 1)) (reduce conj [] (nd/truncate-l a 1 1 1))))
    (is (= (seq (nd/transpose a 2 1 0)) (reduce conj [] (nd/transpose a 2 1 0))))
    (is (= (seq (nd/step a 2 nil -1)) (reduce conj [] (nd/step a 2 nil -1))))))

#?(:cljs
   (if (a/typed-arrays-supported?)
     (run-all-tests)
     (prn "Can't test - typed arrays not supported!")))
