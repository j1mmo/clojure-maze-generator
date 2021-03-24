(ns grid.binary_tree)

(defn random-direction []
  ;;for carving
  (rand-nth '(:east :south)))

(defn add-north-path [row south-indexes]
  (if (empty? south-indexes)
    row
    (let [[index & remaining-indexes] south-indexes]
      (-> row
          (assoc-in [index :walls :north] 1)
          (add-north-path remaining-indexes)))))

(defn get-south-coordinates
  ([new-row] (get-south-coordinates new-row [] 0))
  ([new-row indexes counter]
   (if (empty? new-row)
     indexes
     (if (zero? (get-in (first new-row) [:walls :south]))
       (get-south-coordinates (rest new-row) indexes (inc counter))
       (get-south-coordinates (rest new-row) (conj indexes counter) (inc counter))))))

(defn create-cell [direction prev-east]
  (let [cell {:walls {:north 0 :east 0 :south 0 :west 0}}]
    (if (true? prev-east)
      (-> cell
          (assoc-in [:walls direction] 1)
          (assoc-in [:walls :west] 1))
      (assoc-in cell [:walls direction] 1))))

(defn create-final-cell []
  (let [cell {:walls {:north 0 :east 0 :south 1 :west 0}}]
    cell))

(defn create-final-row-helper [columns]
  (->> (repeat columns {:walls {:north 0 :east 1 :south 0 :west 1}})))

(defn create-final-row [columns]
  (let [first {:walls {:north 0 :east 1 :south 0 :west 0}}
        middle (create-final-row-helper (- columns 2))
        end {:walls {:north 1 :east 0 :south 0 :west 1}}
        all [first middle end]]
    (vec (flatten all))))

(defn create-row
  ([columns] (create-row (dec columns) [] false))
  ([columns row east?]
   (if (zero? columns)
     (conj row (create-final-cell))
     (let [direction (random-direction)]
     (create-row (dec columns)
                 (conj row
                       (create-cell direction east?))
                 (= direction :east))))))

(defn create-maze
  ([rows columns] (create-maze (dec rows) columns []))
  ([rows columns maze]
   (let [row (create-row columns)]
     (create-maze (dec rows) columns (conj maze row) (get-south-coordinates row))))
  ([rows columns maze walls]
   (if (zero? rows)
     (conj maze (create-final-row columns))
     (let [north-index (get-south-coordinates (last maze))
           row (add-north-path (create-row columns) north-index)]
       (create-maze (dec rows) columns (conj maze row) (get-south-coordinates row))))))







