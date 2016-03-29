(defprotocol Heap
  (is-empty? [self])
  (insert [self value])
  (merge [self other])
  (rank [self])
  (min [self])
  (delete-min [self]))

(defrecord LeftistHeap [rank value left right])

;; but what about nil case (recursive base case?) clojure allows
;; us to extend a protocol to core types
(extend-protocol Heap
  nil
  (rank [_] 0) ; rank when nil is 0
  (merge [_ other] other) ; merge with nil is target
  (is-empty? [_] true) ; nil Heap is empty alright.

  LeftistHeap
  (is-empty? [self]
    (nil? self))

  (rank [self]
    (:rank self))

  (merge [{self-val :value
           self-left :left
           self-right :right
           :as this}
          {other-val :value
           other-left :left
           other-right :right
           :as other}]
    (cond
      ;; if the target of the merge is empty, resolve to self. nil case.
      (is-empty? other) self
      ;; if our value is less than the other merge our right value into the other
      ;; and then ensure-leftist (enforce constraint) (not fully understood)
      (<= self-val other-val) (ensure-leftist self-left
                                              (merge self-right other) self-val)
      ;; otherwise... wat.
      :else (ensure-leftist other-left
                            (merge self other-right) other-val)))

  ;; implement insert as a merge of self and a shell leftist heap
  ;; with the supplied value
  (insert [self value]
    (merge (->LefistHeap 1 value nil nil) self))

  
  (min [{value :value}] value)

  (delete-min [{left :left right :right}]
    (merge right left)))



;; ensures each heap remains leftist. Isolated since
;; the Heap protocal is generic and could be used for other types
;; (i.e. non-leftist) of heaps.
(defn ensure-leftist [self other value]
  
  ;; accessed mutliple times, cache the outcome
  (let [self-rank (rank self)
        other-rank (rank other)]
    (if (>= self-rank other-rank)
      (->LeftistHeap (inc other-rank) value self other)
      (->LeftistHeap (inc self-rank) value other self))))


(-> (->LeftistHeap 1 3 nil nil)
    (insert 2)
    (insert 7)
    (insert 4)
    (insert 10)
    (insert 1)
    (insert 20))
