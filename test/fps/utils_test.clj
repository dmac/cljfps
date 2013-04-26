(ns fps.utils-test
  (:use fps.utils
        midje.sweet))

(facts "safe-subvec works"
  (safe-subvec [0 1 2 3] -1 2) => [0 1]
  (safe-subvec [0 1 2 3] 2 8) => [2 3]
  (safe-subvec [0 1 2 3] -3 -2) => []
  (safe-subvec [0 1 2 3] 4 5) => []
  (safe-subvec [0 1 2 3] -1 6) => [0 1 2 3])
