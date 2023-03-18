; Copyright 2022 mrcl dbbrk
; SPDX-License-Identifier: Apache-2.0

(ns rays.core
  (:gen-class)
  (:require [quil.core :as q]))


;*****************************************************************************;
;* vector math *;

(defn translate [v t] (mapv + v t))

(defn scale [v s] (mapv #(* s %) v))

(defn mag [v] (q/sqrt (apply + (map * v v))))

(defn norm [v] (scale v (/ (mag v))))

(defn dot [a b] (apply + (map * a b)))

(defn rotate-x [a [x y z]]
  (let [c (q/cos a), s (q/sin a), -s (- s)]
    [x (dot [y z] [c -s]) (dot [y z] [s c])]))

(defn rotate-y [a [x y z]]
  (let [c (q/cos a), s (q/sin a), -s (- s)]
    [(dot [x z] [c s]) y (dot [x z] [-s c])]))

(defn rotate-z [a [x y z]]
  (let [c (q/cos a), s (q/sin a), -s (- s)]
    [(dot [x y] [c -s]) (dot [x y] [s c]) z]))

(defn rotate-xy [[rx ry] v] (rotate-y ry (rotate-x rx v)))


;*****************************************************************************;
;* signed distance field function *;

(defn sphere-distfn
  ([r p] (- (mag p) r))
  ([p] (sphere-distfn 1.0 p))
  )

(defn cube-distfn
  ([a p] (let [[x y z] (mapv #(- (q/abs %) (* a 0.5)) p)]
           (case (mapv pos? [x y z]); octant
             [false false false] (max x y z)
             [true  false false] x
             [false true  false] y
             [false false true ] z
             [false true  true ] (mag [y z])
             [true  false true ] (mag [x z])
             [true  true  false] (mag [x y])
             [true  true  true ] (mag [x y z]))))

  ([p] (cube-distfn 1.0 p)))

(defn distfn [p]
  (min (cube-distfn (translate p [1.5 0 0]))
       (max (- (sphere-distfn p))
            (+ (cube-distfn 1.5 p)))))


;*****************************************************************************;
;*  *;

(defn march [start heading distfn max-steps max-dist]
  (let [dist (distfn start)]
    (if (and (> max-steps 0) (> max-dist dist 0))
      (recur (translate start (scale heading dist))
             heading
             distfn
             (dec max-steps)
             max-dist)
      {:final-position start,
       :final-heading heading,
       :remaining-distance dist,
       :remaining-steps max-steps}
      )))

(defn rays [w h]
  (vec (for [y (range h) x (range w)]
    (norm [(- x (/ w 2)), (- y (/ h 2)), (/ w 2)]))))


;*****************************************************************************;
;* callbacks *;

(defn setup []
  (q/background 127)
  (q/set-state!
    :idxs (cycle (shuffle (range (* (q/width) (q/height)))))
    :cam-pos [0.7 -1.7 -2.7]
    :cam-rot [-0.6 -0.6]
    :rays (rays (q/width) (q/height))
    ))

(defn draw []
  (let [abort (+ (q/millis) 16)
        px (q/pixels)
        pxlen (alength px)
        rays (q/state :rays)

        drw (fn [idxs]
              (if (< (q/millis) abort)
                (let [idx (first idxs)
                      ray (rotate-xy (q/state :cam-rot) (rays idx))
                      acc (march (q/state :cam-pos) ray distfn 255 10)
                      ]
                  (aset-int px idx
                    (q/color
                      (if (<= (acc :remaining-distance) 0)
                        (acc :remaining-steps)
                        0)
                      ))
                  (recur (rest idxs))
                  )
                idxs))
        ]

    (let [idxs (drw (q/state :idxs))]
      (swap! (q/state-atom) assoc :idxs idxs))
    (q/update-pixels))

  (let [[px py pz] (q/state :cam-pos)
        [rx ry]    (q/state :cam-rot)]
    (q/fill 255 0 0)
    (q/text (str "cam-pos:" \newline
                 "  x: " px \newline
                 "  y: " py \newline
                 "  z: " pz \newline
                 "cam-rot:" \newline
                 "  x: " rx \newline
                 "  y: " ry)
            10 20)
    ))

(defn key-pressed []
  (letfn [(update-rot [i a]
            (q/background 127)
            (swap! (q/state-atom) update-in [:cam-rot i] + a))

          (update-pos [d]
            (q/background 127)
            (swap! (q/state-atom) update :cam-pos translate
              (rotate-xy (q/state :cam-rot) d)))
          ]

    (case (q/key-as-keyword)
      :up    (update-rot 0 (/ q/PI +64))
      :down  (update-rot 0 (/ q/PI -64))
      :left  (update-rot 1 (/ q/PI -64))
      :right (update-rot 1 (/ q/PI +64))
      :w (update-pos [0 0 +0.1])
      :s (update-pos [0 0 -0.1])
      :a (update-pos [-0.1 0 0])
      :d (update-pos [+0.1 0 0])
      ())
    ))


;*****************************************************************************;
;* main *;

(defn -main [& args]
  (q/defsketch fu
    :title "use wasd and arrow keys to move and rotate camera"
    :setup setup
    :draw draw
    :key-pressed key-pressed
    ))

