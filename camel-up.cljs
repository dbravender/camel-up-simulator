(ns camel-up)

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

(defn get-board-after-drop
  [board location picked-up-camels]
  (let [location-value (nth board location)
        action (if (= -1 location-value) #(into %2 %1) #(into %1 %2))
        final-location (if (integer? location-value) (+ location-value location) location)
        drop-location-camels (action (nth board final-location) picked-up-camels)
        new-board (assoc board final-location drop-location-camels)]
    new-board))

(get-board-after-drop [[] [:orange :green] [:blue :yellow :white] -1 [] [] [] [] [] [] [] []] 3 [:fred])

(defn dice-for-one-leg
  [played-colors]
  (let [color-order (shuffle (remove (set played-colors) camel-colors))
        rolled-dice (map #(identity [%1 (rand-nth die-options)]) color-order)]
    rolled-dice))

(dice-for-one-leg [:green :white :orange :yellow])

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

(defn one-leg
  ([board played-colors] (one-leg board played-colors (dice-for-one-leg played-colors)))
  ([board played-colors dice]
   (let [[[color distance] & rest-dice] dice]
     (if (empty? dice) board
       (one-leg (move board color distance) played-colors rest-dice)))))

(defn get-winner-count
  ([board played-colors] (get-winner-count board played-colors 1000 {}))
  ([board played-colors count results]
   (if (= count 0) results
     (let [result (one-leg board played-colors)
           winning-camel (winner result)
           losing-camel (loser result)
           new-results (update-in results [winning-camel] inc)]
       (recur board played-colors (dec count) new-results)))))

(defn get-chances
  [winners]
  (let [total (apply + (vals winners))
        percentages (map #(identity {(first %1) (/ (second %1) total)}) (seq winners))]
    percentages))

(defn get-winner-chances
  [board played-colors]
  (get-chances (get-winner-count board played-colors)))

(one-leg [[:white :green :orange :blue :yellow] -1 [] -1 [] -1 [] [] [] [] [] [] [] [] [] [] [] [] [] [] []] [:green :orange :blue :white])

(get-winner-chances [[:white :green :orange :blue :yellow] [] [] -1 [] -1 [] [] [] [] [] [] [] [] [] [] [] [] [] [] []] [:green :orange :blue :white])

(defn loser
  [board]
  (last (get-places board)))

(defn winner
  [board]
  (first (get-places board)))

(defn get-places
  [board]
  (mapcat reverse (filter seq (reverse (filter #(not (number? %)) board)))))

(get-places [[] [] [] [:bob :fred]])

(winner [[] [] [] [:bob :fred]])
(get-places [[] [] [] [:a :b] [:awesome-face :bob :fred]])
