(ns grid.core
  (:use [seesaw core color border font graphics])
  (:require
   [clojure.java.io :as io]
   [grid.solver :as solver]
   [grid.binary_tree :as binarytree]
   [grid.printing :as printing]
   [grid.sidewinder :as sidewinder]
   [grid.backtracker :as backtracker]))

(defn row-to-output-format [row]
  (->> row
       (map #(vals (get % :walls)))
       (flatten)))

(defn get-data-from-grid [grid rows cols]
  (loop [[row & rst] grid
         grid-info []]
    (if (empty? row)
      (pr-str grid-info)
      (recur rst (conj grid-info (row-to-output-format row))))))

(defn create-write-grid [row col maze-generator]
  (let [dir "resources/"
        filename (str "maze-" (.format
                               (java.text.SimpleDateFormat. "MM-dd-yyyy'-'HH:mm:ss")
                               (new java.util.Date)))
        filepath (str dir filename)
        grid (maze-generator row col)]
    (spit filepath (get-data-from-grid grid row col))
    [grid filename]))

(defn get-mazes []
  (seq (.list (io/file "resources/"))))

(defn string-to-algorithmn [s]
  (cond (= s "binary-tree") binarytree/create-maze
        (= s "backtracker") backtracker/create-maze
        (= s "sidewinder")  sidewinder/create-maze))

(defn row-col-pnl []
    (let [
          x-label (label :text "Columns"
                        :font (font :name :serif :size 10))
          y-label (label :text "Rows"
                        :font (font :name :serif :size 10))
          x-in (slider :id :x-slider :min 2 :max 15 :value 4 :paint-ticks? true :major-tick-spacing 1)
          y-in (slider :id :y-slider :min 2 :max 15 :value 4 :paint-ticks? true :major-tick-spacing 1)
          panel (grid-panel :rows 2 :columns 2 :hgap 2
                           :items [x-label x-in y-label y-in])
         ]
      panel))

(defn select-algorithm []
  (let [
        select (combobox :id :wombo-combo
                          :model ["binary-tree" "backtracker" "sidewinder"])]
    select))

(defn control-panel [maze-panel]
    (let [
          generate-btn (button :text "Generate a maze"
                               :font (font :name :sans-serif :size 16))
          solve-btn (button :text "Solved Maze"
                            :font (font :name :sans-serif :size 16))
          row-col-pnl (row-col-pnl)
          select-maze (combobox :id :maze-list
                                :model (get-mazes))
          select-algorithm (select-algorithm)
          panel (vertical-panel
                    :border 5
                    :items [generate-btn
                            select-algorithm
                            row-col-pnl
                            select-maze
                            solve-btn])]
      
      (listen generate-btn :action
              (fn [e]
                (let [rows (value (select row-col-pnl [:#y-slider]))
                      columns (value (select row-col-pnl [:#x-slider]))
                      string (value (select select-algorithm [:#wombo-combo]))
                      algorithm (string-to-algorithmn string)
                      [maze filename] (create-write-grid rows columns algorithm)]
                  (text! (select maze-panel [:#text-area])(printing/grid-to-string maze))
                  (.addItem select-maze filename)
                  )))
      
      (listen solve-btn :action
              (fn [e]
                (let [file (value (select select-maze [:#maze-list]))
                      maze (solver/solve-maze file)]
                  (text! (select maze-panel [:#text-area])(printing/grid-to-string maze)))
                ))
      panel))

(defn maze-panel []
  (let [panel (scrollable (text :id :text-area
                               :text ""
                               :font (font :name :monospaced :size 12)
                               :columns 70
                               :rows 50
                               :multi-line? true
                               :editable? false))]
    panel))

(defn content []
  (let [maze-panel (maze-panel)
        control-panel (control-panel maze-panel)
        panel (border-panel :west control-panel
                            :east maze-panel
                            :hgap 10)]
    panel))
                            
(defn -main [& args]
    (invoke-later
        (-> (frame :title "Maze Generator",
                   :content (content)
                   :on-close :dispose)
            pack!
            show!)))
           
