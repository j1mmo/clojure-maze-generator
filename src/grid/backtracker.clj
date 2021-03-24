(ns grid.backtracker)

(defn create-row [columns]
  (->> (vec (repeat columns {:visited false :walls {:north 0 :east 0 :south 0 :west 0}}))))

(defn find-adjacent-cell-coords [grid coords]
  (let [[x y] coords]
    (loop [random-directions (shuffle [:north :east :south :west])]
      (if (empty? random-directions)
        nil
        (let [next-direction (peek random-directions)]
          (cond (and (= :east next-direction) (= false (get-in grid [x (inc y) :visited])))
                {:dir :east :coords [x (inc y)] }
                (and (= :west next-direction) (= false (get-in grid [x (dec y) :visited])))
                {:dir :west :coords [x (dec y)] }
                (and (= :south next-direction) (= false (get-in grid [(inc x) y :visited])))
                {:dir :south :coords [(inc x) y]}
                (and (= :north next-direction) (= false (get-in grid [(dec x) y :visited])))
                {:dir :north :coords [(dec x) y]}
                :else (recur (pop random-directions))))))))

(defn opposite-direction [direction-key]
  (cond (= :east direction-key) :west
        (= :north direction-key) :south
        (= :south direction-key) :north
        :else :east))

(defn create-path
  ([grid]
   (-> grid
       (assoc-in [0 0 :visited] true)
       (create-path [[0 0]])))
  ([grid stack]
   (if (empty? stack)
     grid
     (let [[x y] (peek stack)
           next-location (find-adjacent-cell-coords grid [x y])
           cardinal-direction (:dir next-location)
           [next-x next-y] (:coords next-location)]
       (if (nil? next-location)
         (create-path grid (pop stack))
         (-> grid
             (assoc-in [next-x next-y :visited] true)
             (assoc-in [x y :walls cardinal-direction] 1)
             (assoc-in [next-x next-y :walls (opposite-direction cardinal-direction)] 1)
             (create-path (conj stack [next-x next-y]))))))))

(defn create-maze [rows columns]
  (->> (repeat rows (create-row columns))
       (mapv vec)
       (create-path)))
