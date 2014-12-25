(ns camel-up)

(def board-state (vec (repeat 20 [])))

(def camel-colors [:blue :yellow :green :orange :white])

(def die-options [1 2 3])

(defn index-of [coll v]
  (let [i (count (take-while #(not= v %) coll))]
    (when (or (< i (count coll))
              (= v (last coll)))
      i)))

(defn current-location
  [color board]
  (index-of (map #(if (seqable? %1) (some #{color} %1) nil) board) color))

(current-location :red [[:white] [:red]])

(defn get-board-after-drop
  [board location picked-up-camels]
  (let [location-value (nth board location)
        action (if (= -1 location-value) #(into %2 %1) #(into %1 %2))
        final-location (if (integer? location-value) (+ location-value location) location)
        drop-location-camels (action (nth board final-location) picked-up-camels)
        new-board (assoc board final-location drop-location-camels)]
    new-board))

(get-board-after-drop [[] [:orange :green] [:blue :yellow :white] -1 [] [] [] [] [] [] [] []] 3 [:fred])

(defn move
  [board color distance]
  (let [grab-location (current-location color board)
        grab-location-original-camels (nth board grab-location)
        grab-location-index (index-of grab-location-original-camels color)
        picked-up-camels (subvec grab-location-original-camels grab-location-index)
        grab-location-camels (subvec grab-location-original-camels 0 grab-location-index)
        new-board (assoc board grab-location grab-location-camels)
        board-after-drop (get-board-after-drop new-board (+ grab-location distance) picked-up-camels)]
    board-after-drop))

(move [[] [:orange :green] [:blue :yellow :white] -1 [] [] [] [] []] :yellow 1)

(defn loser
  [board]
  (last (get-places board)))

(defn winner
  [board]
  (first (get-places board)))

(defn get-places
  [board]
  (mapcat reverse (filter seq (reverse board))))

(get-places [[] [] [] [:bob :fred]])

(winner [[] [] [] [:bob :fred]])
(get-places [[] [] [] [:a :b] [:awesome-face :bob :fred]])