(ns gist.t-canon
  (:use midje.sweet
        clojure.test
        gist.canon))

(def trees
     '[(add (add 4 5) (sub 6 7))
       (add (add &x 3) (sub &x 4))
       (add (add $x 3) (sub $x 4))
       (add (add %R 3) (sub %R 4))
       (add (add %R $x) (sub %R $x))
       (add (add %R $x) (sub $x %R))])

(facts "about canonicalization without change."
  (canon '(add (add 4 5) (sub 6 7)))   => '(add (sub 6 7) (add 4 5))
  (canon '(add (add 3 $x) (sub 3 $x))) => '(add 3 (add (sub 3 $x) $x))
  (canon '(add (add 3 %x) (sub 3 %x))) => '(add (add 3 %x) (sub 3 %x))
  (canon '(add (add 3 &x) (sub 3 &x))) => '(add 3 (add (sub 3 &x) &x)))
       
(facts "about canonicalization with simple ordering."
  (canon '(add (add $x 3) (sub $x 3))) => '(add 3 (add (sub 3 $x) $x))
  (canon '(add (add &x 3) (sub &x 3))) => '(add 3 (add (sub 3 &x) &x))
  (canon '(add (add %x 3) (sub %x 3))) => '(add (add 3 %x) (sub 3 %x)))

(facts "about associative operations."
  (canon '(add (add 6 5) (add $x 4)))  => '(add 6 (add 5 (add 4 $x)))
  (canon '(add (add $x 4) (add 6 5)))  => '(add 4 (add 6 (add 5 $x)))
  (canon '(add (add $x 6) (add 4 5)))  => '(add 6 (add 4 (add 5 $x)))
  (canon '(add (add $x 5) (add 6 4)))  => '(add 5 (add 6 (add 4 $x))))  