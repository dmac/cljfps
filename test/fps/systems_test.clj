(ns fps.systems_test
  (:use fps.systems
        midje.sweet))

(facts "nearby-blocks works"
  (let [game {:world [[[0 1 2] [3 4 5] [6 7 8]]
                      [[9 10 11] [12 13 14] [15 16 17]]
                      [[18 19 20] [21 22 23] [24 25 26]]]}]
    (nearby-blocks {:position {:x 1 :y 1 :z 1}} game 1) => (flatten (:world game))
    (nearby-blocks {:position {:x 0 :y 0 :z 0}} game 5) => (flatten (:world game))
    (nearby-blocks {:position {:x 0 :y 0 :z 0}} game 1) => [0 1 3 4 9 10 12 13]
    (nearby-blocks {:position {:x 2 :y 0 :z 1}} game 1) => [9 10 11 12 13 14 18 19 20 21 22 23]
    (nearby-blocks {:position {:x -5 :y -5 :z -5}} game 1) => []))
