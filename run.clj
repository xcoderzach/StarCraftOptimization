(def constructions-map 
  {["supply-depot"]             "barracks"
   ["barracks"]                 "tech-lab"
   ["refinery", "barracks"]     "factory"
   ["factory"]                  "starport"
   ["starport"]                 "banshee"
   ["starport" "tech-lab"]      "cloaking" 
   })

(def num-allowed-constructions 
  {"scv"           16
   "refinery"      2
   "supply-depot"  1
   "barracks"      1
   "factory"       1
   "starport"      1
   "tech-lab"      1
   "cloaking"      1
   "banshee"       1
   })

(def unit-mineral-costs
  {"scv"           50
   "refinery"      75
   "supply-depot"  100
   "barracks"      150
   "factory"       150
   "starport"      150
   "tech-lab"      50
   "cloaking"      200
   "banshee"       150
   }) 

(def unit-gas-costs
  {"scv"           0
   "refinery"      0
   "supply-depot"  0
   "barracks"      0
   "factory"       50
   "starport"      50
   "tech-lab"      25
   "cloaking"      200
   "banshee"       125
   })

(def unit-resource-modifiers 
  { "scv"          {:mineral 50   :gas 0    :mineral-collectors 0 :gas-collectors 0  :num-allowed 5 :requirements []                        :build-time 17  }
    "refinery"     {:mineral 75   :gas 0    :mineral-collectors 1 :gas-collectors 1  :num-allowed 2  :requirements []                       :build-time 32  }
    "supply-depot" {:mineral 100  :gas 0    :mineral-collectors 1 :gas-collectors 0  :num-allowed 1  :requirements []                       :build-time 33  }
    "barracks"     {:mineral 150  :gas 50   :mineral-collectors 1 :gas-collectors 0  :num-allowed 1  :requirements ["supply-depot"]         :build-time 68  }
    "factory"      {:mineral 150  :gas 50   :mineral-collectors 1 :gas-collectors 0  :num-allowed 1  :requirements ["refinery", "barracks"] :build-time 63  }
    "starport"     {:mineral 150  :gas 50   :mineral-collectors 1 :gas-collectors 0  :num-allowed 1  :requirements ["factory"]              :build-time 53  }
    "tech-lab"     {:mineral 50   :gas 25   :mineral-collectors 0 :gas-collectors 0  :num-allowed 1  :requirements ["barracks"]             :build-time 25  }
    "cloaking"     {:mineral 200  :gas 200  :mineral-collectors 0 :gas-collectors 0  :num-allowed 1  :requirements ["starport" "tech-lab"]  :build-time 110 }
    "banshee"      {:mineral 150  :gas 125  :mineral-collectors 0 :gas-collectors 0  :num-allowed 1  :requirements ["starport" "tech-lab"]  :build-time 60  }})

(defn contains-string? [coll string]
  (reduce (fn [prev value] 
    (or (= string value) prev)) false coll)) 

(defn count-occurences [s]
  (reduce (fn [memo unit] (if (not (nil? (get memo unit)))                                                                                                                                                       
                            (assoc memo unit (+ 1 (get memo unit)))
                            (assoc memo unit 1))) {} s))

(defn diff-occurences [source target]
  (reduce (fn [memo unit] 
    (let [unit-name (first unit) unit-count (last unit) minus-unit-count (target (first unit))]
      (if (not (nil? (get target unit-name)))
        (if (> (- unit-count minus-unit-count) 0) (assoc memo unit-name (- unit-count minus-unit-count)))
        (assoc memo unit-name unit-count))))
    {} source))
 
(defn get-available-constructions [owned-buildings]
  (reduce (fn [prev requirement]
    (let [required-building (first requirement) allowed-building (last requirement)]
      (if (reduce (fn [memo req] (and memo (contains-string? owned-buildings req))) true required-building)
        (conj prev allowed-building)
        prev)))
    ["scv", "refinery", "supply-depot"] constructions-map))

(defn get-allowed-constructions [current-buildings-count]
  (let [buildings-count (count-occurences current-buildings-count)]
    (reduce (fn [memo building] 
              (let [unit-name (first building) unit-count (buildings-count (first building)) num-allowed (get (get unit-resource-modifiers (first building)) :num-allowed)]
                (assoc memo unit-name (if (nil? unit-count) num-allowed (- num-allowed unit-count)))))
      {} num-allowed-constructions)))


(defn get-construction-options [completed building]
  (let [completed-count (count-occurences completed) building-count (count-occurences (vals building))]
    (let [available (get-available-constructions (keys (diff-occurences completed-count building-count)))
          available-counts (get-allowed-constructions (seq completed))]

      (filter (fn [x] 
                (and (contains-string? available (first x)) (> (last x) 0))) available-counts))))

(defn time-until-start [unit resources]
  (let [required-minerals (- (get unit-mineral-costs unit) (get resources :mineral))
        required-gas (- (get unit-gas-costs unit) (get resources :gas)) 
        ]
    (max 
      (if (<= required-minerals 0) 
        0
        (/ required-minerals (* 0.7 (get resources :mineral-collectors))))
      (if (<= required-gas 0) 
        0
        (/ required-gas (* 0.6333 (get resources :gas-collectors)))))))

(defn get-build-times [units current-time resources]
  (reduce (fn [memo unit] 
            (assoc memo (+ current-time (time-until-start unit resources)) unit))
    (sorted-map) units))

(defn update-resources-end [resources unit time-range]
  {:mineral (+ (get resources :mineral) (* time-range 0.68 (get resources :mineral-collectors)))
   :gas (+ (get resources :gas) (* time-range 0.6333 (get resources :gas-collectors)))
   :mineral-collectors (+ (if (= unit "scv") 1 0) (- (+ (get resources :mineral-collectors) (get (get unit-resource-modifiers unit) :mineral-collectors)) (get (get unit-resource-modifiers unit) :gas-collectors)))
   :gas-collectors (+ (get resources :gas-collectors) (get (get unit-resource-modifiers unit) :gas-collectors))})

(defn update-resources-start [resources unit time-range]
  {:mineral (- (+ (* time-range 0.68 (get resources :mineral-collectors)) (get resources :mineral)) (get unit-mineral-costs unit))
   :gas (- (+ (get resources :gas) (* time-range 0.6333 (get resources :gas-collectors))) (get unit-gas-costs unit))
   :mineral-collectors (- (get resources :mineral-collectors) (get (get unit-resource-modifiers unit) :mineral-collectors))
   :gas-collectors (get resources :gas-collectors)})

(defn update-resources-refinery-hack [resources unit time-range extra]
  (let [extra-gassers (min (- (get resources :mineral-collectors) 1) extra)]
    {:mineral (+ (get resources :mineral) (* time-range 0.68 (get resources :mineral-collectors)))
     :gas (+ (get resources :gas) (* time-range 0.6333 (get resources :gas-collectors)))
     :mineral-collectors (- (+ (get resources :mineral-collectors) (get (get unit-resource-modifiers unit) :mineral-collectors)) extra-gassers)
     :gas-collectors (+ (get resources :gas-collectors) extra-gassers)}))

(def best-time 100000)
(def best-build-order '())
(def solutions 0)
(def nodes 0)
(defn before-nodes [resource-state production-queue build-order current-time annotated-build-order]
  (comment 20792631)
  (def nodes (+ 1 nodes))
  (if (and (contains-string? build-order "banshee") (contains-string? build-order "cloaking"))
    (let [final-time (reduce (fn [end-time building]
          (if (or (= (last building) "banshee") (= (last building) "cloaking")) 
            (max (first building) end-time)
            end-time)) current-time production-queue)]
      (def solutions (+ 1 solutions))
      (if (= (mod solutions 1000000) 0) (println solutions))
      (if (< final-time best-time)
        (let [] 
          (def best-time final-time)
          (def best-build-order build-order)
          (println "\n=================")
          (println annotated-build-order)
          (println "\n")
          (println build-order final-time)
          (println "\n")
          (println resource-state)
          (println "\n")
          (println production-queue)
          (println "\n\n"))))
    (loop [soonest-end (first production-queue) possibilities (get-build-times (keys (get-construction-options build-order production-queue)) current-time resource-state)]
      (if (not (and (empty? production-queue) (empty? possibilities)))
        (if (or (and (not (empty? production-queue)) (empty? possibilities)) (and (not (nil? soonest-end)) (< (first soonest-end) (first (first possibilities)))))
          (let [] 
            (before-nodes (update-resources-end resource-state (last soonest-end) (- (first soonest-end) current-time))
                          (dissoc production-queue (first soonest-end))
                          build-order
                          (first soonest-end) 
                          (conj annotated-build-order (str (last soonest-end) " ended @ " (first soonest-end) "\n")))
            (if (= "refinery" (last soonest-end))
              (let []
                (before-nodes (update-resources-refinery-hack resource-state (last soonest-end) (- (first soonest-end) current-time) 2)
                              (dissoc production-queue (first soonest-end))
                              build-order
                              (first soonest-end)
                              (conj annotated-build-order (str (last soonest-end) "2 ended @ " (first soonest-end) "\n")))

                (before-nodes (update-resources-refinery-hack resource-state (last soonest-end) (- (first soonest-end) current-time) 3)
                              (dissoc production-queue (first soonest-end))
                              build-order
                              (first soonest-end) 
                              (conj annotated-build-order (str (last soonest-end) "3 ended @ " (first soonest-end) "\n"))))))
          (let [] 
            (before-nodes (update-resources-start resource-state (last (first possibilities)) (- (first (first possibilities)) current-time)) 
                          (conj production-queue {(+ (first (first possibilities)) (get (get unit-resource-modifiers (last (first possibilities))) :build-time )) (last (first possibilities))}) 
                          (conj build-order (last (first possibilities)))
                          (first (first possibilities))
                          (conj annotated-build-order (str (last (first possibilities)) " began @ " (first (first possibilities)) "\n")))
            (recur soonest-end (dissoc possibilities (first (first possibilities))))))))) solutions)

(println (before-nodes {:mineral 50 :gas 0 :mineral-collectors 6 :gas-collectors 0} (sorted-map) [] 5 []))
