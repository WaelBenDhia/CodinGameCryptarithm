(ns Solution
  (:gen-class))

(defn letter-map-has
  [letter-map number]
  (contains? (set (vals letter-map)) number))

(defn get-first-non-used-value
  [letter-map number]
  (loop [number number]
    (if (letter-map-has letter-map number)
      (recur (if (= number 9) 0 (inc number)))
      number)))

(defn map-word-to-number
  [word letter-map]
  (reduce (fn
            [final-number letter]
            (+ (* final-number 10) (get letter-map (keyword (str letter)))))
          0
          word))

(defn test-map
  [result-word letter-map & words]
  (and (= (count (set (vals letter-map))) (count letter-map))
       (not (reduce #(or %1 (zero? (get letter-map (keyword (str (get %2 0)))))) false (cons result-word words)))
       (= (map-word-to-number result-word letter-map)
          (reduce (fn [sum word] (+ sum (map-word-to-number word letter-map)))
                  0
                  words))))

(defn inc-map
  [letter-map]
  (let [key (nth (keys letter-map) (dec (count letter-map)))]
    (if (= (get letter-map key) 9)
      (assoc (inc-map (dissoc letter-map key)) key 0)
      (assoc letter-map key (inc (get letter-map key))))))


(defn is-first-letter
  [letter & words]
  (reduce #(or %1 (= letter (.charAt %2 0))) false words))

(defn create-first-map
  [& words]
  (into (sorted-map) (reduce (fn
            [final-map letter]
            (assoc final-map
              (keyword (str letter))
              (get-first-non-used-value final-map
                                        (if (apply is-first-letter (cons letter words)) 1 0))))
          {}
          (set (apply str words)))))

(defn convert-map-to-printable-solution
  [letter-map]
  (.substring (reduce-kv #(str %1 "\n" (name %2) " " %3) "" letter-map) 1))

(defn solve
  [result-word & added-words]
  (loop [letter-map (apply create-first-map (cons result-word added-words))]
    (if (apply test-map (into [result-word letter-map] added-words))
      (convert-map-to-printable-solution letter-map)
      (recur (inc-map letter-map)))))

(defn -main [& args]
  (let [N (read)
        words (loop [i N words []]
                (if (>= i 0)
                  (recur (dec i) (cons (str (read)) words))
                  words))]
    (println (apply solve words))))