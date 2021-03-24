(ns grid.sidewinder)

(defn sidewinder-sub-row [columns]
  (if (> 2 columns)
    {:walls {:north 1 :east 0 :south 0 :west 0}}
    (let [start {:walls {:north 0 :east 1 :south 0 :west 0}}
          middle (repeat (- columns 2) {:walls {:north 0 :east 1 :south 0 :west 1}})
          end {:walls {:north 0 :east 0 :south 0 :west 1}}
          all (flatten [start middle end])]
      (-> (vec all)
          (assoc-in [(rand-int (count all)) :walls :north] 1)))))

(defn sidewinder-first-row [columns]
  (if (> 2 columns)
    {:walls {:north 0 :east 0 :south 0 :west 0}}
    (let [start {:walls {:north 0 :east 1 :south 0 :west 0}}
          middle (repeat (- columns 2) {:walls {:north 0 :east 1 :south 0 :west 1}})
          end {:walls {:north 0 :east 0 :south 0 :west 1}}]
      (vec (flatten [start middle end])))))

(defn add-south-path [maze north-indexes]
  (if (empty? north-indexes)
    maze
    (let [row (dec (count maze))
          [index & remaining-indexes] north-indexes]
      (-> maze
          (assoc-in [row index :walls :south] 1)
          (add-south-path remaining-indexes)))))

(defn random-partition [columns]
  (let [rand (->> (range columns)
                  (partition-by (fn[_] (rand-nth [true false])))
                  (map #(count %)))]
    (if (= (first rand) columns)
      ;; try again if the size of the collection is the size of the parition
      (random-partition columns)
      rand)))

(defn get-north-coordinates
  ([new-row] (get-north-coordinates new-row [] 0))
  ([new-row indexes counter]
   (if (empty? new-row)
     indexes
     (if (zero? (get-in (first new-row) [:walls :north]))
       (get-north-coordinates (rest new-row) indexes (inc counter))
       (get-north-coordinates (rest new-row) (conj indexes counter) (inc counter))))))
  
(defn sidewinder-create-row [columns]
  (let [row (->>
             columns
             (random-partition)
             (map #(sidewinder-sub-row %))
             (flatten))]
    [(get-north-coordinates row) row]))

(defn create-maze
  ([rows columns] (create-maze
                   (dec rows)
                   columns
                   [(sidewinder-first-row columns)]))
  ([rows columns maze]
   (if (zero? rows)
     maze
     (let [[indexes new-row] (sidewinder-create-row columns)]
       (create-maze (dec rows)
                           columns
                           (conj (add-south-path maze indexes) (vec new-row)))))))
