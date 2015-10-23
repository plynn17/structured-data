(ns structured-data)

(defn do-a-thing [x]
  (let [p (+ x x)]
    (Math/pow p p )))


(defn spiff [v]
  (+ (get v 0) (get v 2)))

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [[x y z]]
  (+ x z))

(defn point [x y]
  [x y])
(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [[[x1 y1] [x2 y2]]]
  (- x2 x1))

(defn height [[[x1 y1] [x2 y2]]]
  (- y2 y1))

(defn square? [rectangle]
  (if (= (width rectangle) (height rectangle))
    true
    false))

(defn area [rectangle]
  (* (height rectangle) (width rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [xp yp] point]
    (if (and (<= x1 xp x2) (<= y1 yp y2))
      true
      false)))

(defn contains-rectangle? [outer inner]
  (let [[bl tr] inner]
    (if (and (contains-point? outer bl) (contains-point? outer tr))
      true
      false)))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book)))

(defn multiple-authors? [book]
  (if (> (author-count book) 1)
    true
    false))

(defn add-author [book new-author]
  (let [old (book :authors)]
    (assoc book :authors (conj old new-author))))

(defn alive? [author]
  (if (= (author :death-year) nil)
    true
    false))

(defn element-lengths [collection]
  (map count collection))

(defn second-elements [collection]
  (let [get-snd (fn [x] (get x 1))]
    (map get-snd collection)))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (if (or (apply <= a-seq) (apply >= a-seq))
    true
    false))

(defn stars [n]
  (apply str (repeat n \*)))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn contains-duplicates? [a-seq]
  (if (> (count a-seq) (count (set a-seq)))
    true
    false))

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
  (if (contains? (:authors book) author)
    true
    false))

(defn authors [books]
  (let [a (fn [x] (:authors x))]
    (set (apply clojure.set/union (map a books)))))

(defn all-author-names [books]
  (let [a (fn [x] (x :name))]
     (set (map a (authors books)))))

(defn author->string [author]
  (let [n (:name author)
        by (:birth-year author)
        dy (:death-year author)]
    (if (= by nil)
      (str n)
      (str n " (" by " - " dy ")"))))

(defn authors->string [authors]
  (apply str(interpose ", " (map author->string authors))))

(defn book->string [book]
  (let [b (:title book)
        a (:authors book)]
    (str b ", written by " (authors->string a))))

(defn books->string [books]
  (let [c (count books)]
    (if (= c 0)
      (str "No books.")
      (if (= c 1)
        (str (str c) " book. " (apply str(interpose ". " (map book->string books))) ".")
        (str (str c) " books. " (apply str(interpose ". " (map book->string books))) ".")))))

(defn books-by-author [author books]
  (let [a? (fn [b] (has-author? b author))]
    (filter a? books)))

(defn author-by-name [name authors]
  (let [n? (fn [a] (= (:name a) name))
        a  (filter n? authors)]
    (first a)))
        

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (let [a (living-authors (:authors book))]
    (if (= a ())
      false
      true)))

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
