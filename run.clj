(def constructions-map 
  {["supply-depot"]             "barracks"
   ["refinery", "barracks"]     "factory"
   ["barracks"]                 "tech-lab"
   ["factory"]                  "starport"
   ["starport"]                 "banshee"
   ["starport" "tech-lab"]      "cloaking" })

(def num-allowed-constructions 
  {"scv"           16
   "refinery"      2
   "supply-depot"  1
   "barracks"      1
   "factory"       1
   "starport"      1
   "tech-lab"      1
   "cloaking"      1
   "banshee"       1})

(def unit-mineral-costs
  {"scv"           50
   "refinery"      75
   "supply-depot"  100
   "barracks"      150
   "factory"       150
   "starport"      150
   "tech-lab"      50
   "cloaking"      200
   "banshee"       150}) 

(def unit-gas-costs
  {"scv"           0
   "refinery"      0
   "supply-depot"  0
   "barracks"      0
   "factory"       50
   "starport"      50
   "tech-lab"      25
   "cloaking"      200
   "banshee"       125})

(def unit-resource-modifiers 
  { "scv"          {:mineral 50   :gas 0    :mineral-collectors 0 :gas-collectors 0  :num-allowed 11 :requirements []                       :build-time 17  }
    "refinery"     {:mineral 75   :gas 0    :mineral-collectors 1 :gas-collectors 1  :num-allowed 2  :requirements []                       :build-time 30  }
    "supply-depot" {:mineral 100  :gas 0    :mineral-collectors 1 :gas-collectors 0  :num-allowed 1  :requirements []                       :build-time 30  }
    "barracks"     {:mineral 150  :gas 50   :mineral-collectors 1 :gas-collectors 0  :num-allowed 1  :requirements ["supply-depot"]         :build-time 65  }
    "factory"      {:mineral 150  :gas 50   :mineral-collectors 1 :gas-collectors 0  :num-allowed 1  :requirements ["refinery", "barracks"] :build-time 60  }
    "starport"     {:mineral 150  :gas 50   :mineral-collectors 1 :gas-collectors 0  :num-allowed 1  :requirements ["factory"]              :build-time 50  }
    "tech-lab"     {:mineral 50   :gas 25   :mineral-collectors 0 :gas-collectors 0  :num-allowed 1  :requirements ["barracks"]             :build-time 25  }
    "cloaking"     {:mineral 200  :gas 200  :mineral-collectors 0 :gas-collectors 0  :num-allowed 1  :requirements ["starport" "tech-lab"]  :build-time 110 }
    "banshee"      {:mineral 150  :gas 125  :mineral-collectors 0 :gas-collectors 0  :num-allowed 1  :requirements ["starport" "tech-lab"]  :build-time 60  }})

(defn contains-string? [coll string]
  (reduce (fn [prev value] 
    (or (= string value) prev)) false coll)) 

(defn get-available-constructions [owned-buildings]
  (reduce (fn [prev requirement]
    (let [required-building (first requirement) allowed-building (last requirement)]
      (if (reduce (fn [memo req] (and memo (contains-string? owned-buildings req))) true required-building)
        (conj prev allowed-building)
        prev)))
    ["scv", "refinery", "supply-depot"] constructions-map)) 

(defn get-allowed-constructions [current-buildings-count]
  (let [buildings-count (reduce (fn [memo unit] 
                          (if (not (nil? (get memo unit))) 
                            (assoc memo unit (+ 1 (get memo unit)))
                            (assoc memo unit 1))) {} current-buildings-count)] 
    (reduce (fn [memo building] 
              (let [unit-name (first building) unit-count (last building) num-allowed (get buildings-count (first building))]
                (assoc memo unit-name (if (nil? num-allowed) unit-count (- unit-count num-allowed)))))
      {} num-allowed-constructions)))

(defn get-construction-options [completed building]
  (let [available (get-available-constructions completed)
        available-counts (get-allowed-constructions (concat (vals building) (seq completed)))]
    (filter (fn [x] 
              (and (contains-string? available (first x)) (not= 0 (last x)))) available-counts)))

(def start-state {"scv" 6})

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

(defn update-resources-end [resources unit]
  {:mineral (get resources :mineral)
   :gas (get resources :gas)
   :mineral-collectors (+ (get resources :mineral-collectors) (get (get unit-resource-modifiers unit) :mineral-collectors))
   :gas-collectors (+ (get resources :gas-collectors) (get (get unit-resource-modifiers unit) :gas-collectors))})

(defn update-resources-start [resources unit]
  {:mineral (- (get resources :mineral) (get unit-mineral-costs unit))
   :gas (- (get resources :gas) (get unit-gas-costs unit))
   :mineral-collectors (- (get resources :mineral-collectors) (get (get unit-resource-modifiers unit) :mineral-collectors))
   :gas-collectors (get resources :gas-collectors)})

(defn before-nodes [resource-state production-queue build-order current-time]
  (if (and (contains-string? build-order "banshee") (contains-string? build-order "cloak") (not (contains-string? build-order "banshee")) (not (contains-string? production-queue "cloak")))
    (let [] 
      (println "\n=================")
      (println build-order current-time)
      (println "\n\n"))
    (loop [soonest-end (first production-queue) possibilities (get-build-times (keys (get-construction-options build-order production-queue)) current-time resource-state)]
      (if (or (and (not (empty? production-queue)) (empty? possibilities)) (and (not (nil? soonest-end)) (< (first soonest-end) (first (first possibilities)))))
        (before-nodes (update-resources-end resource-state (last soonest-end)) (rest production-queue) build-order (first soonest-end))
        (let [] 
          (before-nodes (update-resources-start resource-state (last (first possibilities))) 
                        (conj production-queue [(+ current-time (get (get unit-resource-modifiers (last (first possibilities))) :build-time )) (last (first possibilities))]) 
                        (conj build-order (last (first possibilities)))
                        (first (first possibilities)))
          (recur soonest-end (rest possibilities)))))))

(before-nodes {:mineral 50 :gas 0 :mineral-collectors 6 :gas-collectors 0} (sorted-map) [] 0)
