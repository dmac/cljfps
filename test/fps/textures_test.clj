(ns fps.textures-test
  (:use midje.sweet
        [midje.util :only [testable-privates]]
        fps.textures))

(testable-privates fps.textures texture-coords-from-atlas-coords)

(facts "texture-coords-from-atlas-coords works"
  (let [buffer 0.001]
    (texture-coords-from-atlas-coords [0 0] 2 2) => [[(+ 0 buffer) (+ 0 buffer)]
                                                     [(- 0.5 buffer) (+ 0 buffer)]
                                                     [(- 0.5 buffer) (- 0.5 buffer)]
                                                     [(+ 0 buffer) (- 0.5 buffer)]]))
