(ns nqueens.core
  (:gen-class))

(set! *warn-on-reflection* true)

(def ^:dynamic size 5)

(def solutions (ref []))

(defn store-solution [solution]
  (dosync (alter solutions conj solution)))

(defn conflict? [[#^int row-first #^int col-first] [#^int row-second #^int col-second]]
  (or
    (= row-first row-second)
    (= col-first col-second)
    (= (Math/abs (- row-first row-second));; diagonal conflict   
       (Math/abs (- col-second col-first)))))

(defn position-safe-for-board [board pos]
  (if (empty? board) true
  (not (some  #(conflict? pos %) board))))
  

(defn nqueens [current-sol row]
   (dotimes [col size]
     (let [current-pos (list row col)
           current-pos-safe (position-safe-for-board current-sol current-pos)]
      (if current-pos-safe
        (if (= row  (- size 1))
          (store-solution (conj current-sol current-pos))
          (nqueens (conj current-sol current-pos) (+ row 1)))))))

(defn -main [n]
  (binding [size (Integer/parseInt n)]
    (do
      (time (nqueens #{} 0))
      (println (str (count @solutions))))))




