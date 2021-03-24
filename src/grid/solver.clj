(ns grid.solver
  (:require 
            [grid.backtracker :as bt]))

(defn reconstruct-maze-from-file
  ([row] (reconstruct-maze-from-file row []))
  ([row result]
   (let [[n e s w & remaining :as all] row]
     (if (empty? all)
       (vec result)
       (recur remaining (conj result
                              {:visited false
                               :path false
                               :walls {:north n :east e :south s :west w}}))))))
     
(defn create-maze-from-file-output [filename]
  (->> (read-string (slurp filename))
       (map #(reconstruct-maze-from-file %))
       (mapv vec)))

(defn possible-routes [maze current-dir dir]
  (let [[row col] dir
        [crow ccol] current-dir]
    (true? (get-in maze [(+ row crow) (+ col ccol) :visited]))))

(defn add-coord [c1 c2]
  (let [[row1 col1] c1
        [row2 col2] c2]
    [(+ row1 row2) (+ col1 col2)]))

(defn cardinal-direction-to-vector [dir]
  (cond (= dir :north) [-1  0]
        (= dir :east)  [ 0  1]
        (= dir :south) [ 1  0]
        (= dir :west)  [ 0 -1]
        :else nil))

(defn valid-directions [maze coord]
  (let [[row col] coord]
    (->> (get-in maze [row col :walls])
         (filter #(= (second %) 1))
         (flatten)
         (remove #(= % 1))
         (map #(cardinal-direction-to-vector %))
         (filter #(= false (possible-routes maze coord %)))
         (map #(add-coord % coord)))))

(defn path-finder
  ([maze start-row start-col end-row end-col]
   (-> maze
       (assoc-in [start-row start-col :visited] true)
       (assoc-in [start-row start-col :path] true)
       (path-finder [end-row end-col] [[start-row start-col]])))
  ([maze destination stack]
   (let [top (peek stack)]
     (if (= top destination)
       stack
       (let [[dir & rest-dir :as all] (valid-directions maze top)
             [row col] dir]
         (if (empty? all)
           (recur maze destination (pop stack))
           (-> maze
               (assoc-in [row col :visited] true)
               (recur destination (conj stack dir)))))))))

(defn input-path
  ([maze start end]
   (let [[start-row start-col] start
         [end-row end-col] end
         path (path-finder maze start-row start-col end-row end-col)]
     (input-path maze path)))
  ([maze path]
   (if (empty? path)
     maze
     (let [[row col] (peek path)]
       (-> maze
           (assoc-in [row col :path] true)
           (recur (pop path)))))))

(defn solve-maze [filename]
  (let [filepath (str "resources/" filename)
        maze (create-maze-from-file-output filepath)
        row (dec (count maze)) col (dec (count (first maze)))
        solved-maze (input-path maze [0 0] [row col])]
    solved-maze))
