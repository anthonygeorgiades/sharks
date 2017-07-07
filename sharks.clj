(defstruct shark :id :direction :weight :hunger :status)

(defn make-shark
  "Defines a shark with :id = n and various attributes."
  [n]
  (struct-map shark
    :id (char (+ (int \a) (dec n)))
    :direction (if (< (rand) 0.5)
      :left
      :right )
    :weight  (+ 90 (* 20 (rand)))
    :hunger 0
    :status :alive) )

(defn str-shark
  "Returns a string representation of a shark (or () for an empty list)."
  [shark]
  (cond
    (= shark ()) ".."
    (= (shark :direction) :left) (str "<" (str (shark :id)))
    :else  (str (str (shark :id)) ">") ) )

(defn print-sharks
  "Prints out a list containing sharks and empty locations."
  [lst]
  (println (map str-shark lst)) )

(defn make-shark-list
  "Makes a list of how-many actual sharks, and some number of empty locations."
  ([how-many] (make-shark-list [] 1 how-many))
  ([lst n how-many]
    (cond
      (> n how-many) lst
      (> (rand) 0.50) (cons '() (make-shark-list lst n how-many))
      :else (cons (make-shark n) (make-shark-list lst (inc n) how-many)) ) ) )

(defn run-me []
  (do
    (print-sharks (make-shark-list 3))
    (print-sharks (make-shark-list 3))
    (print-sharks (make-shark-list 3))
    (print-sharks (make-shark-list 26)) ) )

(defn run-me-too []
  (do
    (let [
      ; create a shark called Sherman (as in http://shermanslagoon.com/)
      sherman (make-shark 1)
      ; create a reference to Sherman
      look!-a-shark! (ref sherman)]
      ; print Sherman in two different ways
      (println sherman)
      (println (str-shark sherman))
      ; put Sherman on a diet
      (dosync
        (ref-set look!-a-shark! (assoc @look!-a-shark! :hunger 4))
        (ref-set look!-a-shark!
          (assoc  @look!-a-shark! :weight (* 0.75 (@look!-a-shark! :weight))) )
        (ref-set look!-a-shark! (assoc @look!-a-shark! :status :ill-tempered))
      ; show off Sherman's hoped-for look
      (println @look!-a-shark!)
      (println (str-shark @look!-a-shark!))
      ; sorry, Sherman, you're immutable
      (println sherman)
      (println (str-shark sherman)) ) ) ) )

(defn moveShark
  [time-stamp = 0]
  (if (or run-me-too run-me) true
    (let (count (str-shark))
      (doseq [x (keys count)] (x (x count)))
      (if (zero? (count str-shark))))))

(defn oceanwidth)

(defn oceanheight)

(defn oceanParams [x y]
    :oceanwidth 100
    :oceanheight 100)

(def ocean
  (atom {:width oceanwidth :height oceanheight} ) )

(defn makePositionRandom [w h]
  (rand-int w)
  (rand-int h))


(defn makeRandomSharkSize [x]
  (rand-int x) )

(defn setSharkSize[x str-shark]
  (for (first str-shark)
    size = makeRandomSharkSize[x]
    (setSharkSize (rest str-shark))))

(defn matrixLocation [x y]
  (seq (repeat x (seq (repeat y)) ) ) )

(defn moved [a a1]
  (not (zero? (compare a a1))))

(defn touching [shark shark w h]
  (let [changes [ [0 1] [1 1] [1 0] [1 -1] [-1 0] [0 -1] [-1 -1] [-1 1] ]
        [w h] shark shark]
        (for ( [w y] changes
          (mod (+ w h) width) (mod (+ h w) height)) ) ) )

(defn eatFish [shark1 shark2 touching]
    (if (ifMoved? shark1)
      (if touching shark1 shark2 )
      (if (> (shark1 shark2) (eatFish true))
        (= shark1 (+ shark1 shark2) (replace shark2 ""))
          :else (if (> (shark2 shark1) (eatFish true))
        (= shark2 (+ shark2 shark1) (replace shark1 ""))))))

(defn start []
  (when (not run-me) && (not run-me-too)))

