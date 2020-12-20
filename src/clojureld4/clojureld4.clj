(ns clojureld4.clojureld4
  (:use [clojure.test]))

(defn mapping [string rails]
  (->> string
       (map (partial vector) rails)
       (sort-by first)
       (map second)
       )
  )

(defn encrypt [string key]
  (mapping string (cycle
              (concat (range 1 (+ key 1))
                      (range (- key 1) 1 -1)))
           )
  )

(defn decrypt [string key]
  (mapping string (encrypt (range (count string)
                                  ) key))
  )

(defn encrypt-call [string key]
  (if (nil? (re-matches #"^[a-zA-Z\_]+$"
                        (clojure.string/replace string #" " "_")))
    nil
    (apply str (encrypt
                 (clojure.string/replace string #" " "_") key))
    )
  )

(defn decrypt-call [string key]
  (if (nil? (re-matches #"^[a-zA-Z\_]+$"
                        (clojure.string/replace string #" " "_")))
    nil
    (apply str (decrypt
                 (clojure.string/replace string #"_" " ") key))
    )
  )

;small testing for result approve
(testing "enc"
  (is (= (encrypt-call "hello world" 3) "horel_ollwd"))
  (is (= (encrypt-call "Nikita Kovalovs" 6) "NaivlkooiKvt_sa")))

(testing "dec"
  (is (= (decrypt-call "horel_ollwd" 3) "hello world"))
  (is (= (decrypt-call "NaivlkooiKvt_sa" 6) "Nikita Kovalovs")))