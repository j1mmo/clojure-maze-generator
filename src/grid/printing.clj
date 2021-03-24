(ns grid.printing
   (:require
    [clojure.string :as string]))

(defn check-border-east [cell]
  (not (zero? (get-in cell [:walls :east]))))

(defn check-border-south [cell]
  (not (zero? (get-in cell [:walls :south]))))

(defn border-east-string-helper [cell]
  (if (check-border-east cell)
    "    "
    "   |"))

(defn border-east-string [cell]
  (let [default (border-east-string-helper cell)]
    (if (true? (get-in cell [:path]))
      (apply str (assoc (vec default) 1 \*))
      default)))
  
(defn border-south-string [cell]
  (if (check-border-south cell)
    "   +"
    "---+"))

(defn row-to-string [row]
  (let [first-line (string/join (map #(border-east-string %) row))
        second-line (string/join (map #(border-south-string %) row))]
    (str "|" first-line "\n+" second-line "\n")))

(defn start-string [column-count]
  (let [start "\n+"
        middle (string/join (repeat column-count "---+"))
        end "\n"]
    (str start middle end)))

(defn grid-to-string [grid]
  (loop [[row & rst] grid
         output (start-string (count row))]
    (if (empty? row)
      output
      (recur rst (str output (row-to-string row))))))
