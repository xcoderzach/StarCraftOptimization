(def constructions-map 
  {["supply-depot"]                  "barracks"
   ["refinery", "barracks"]          "factory"
   ["barracks"]                      "expansion-bay"
   ["factory"]                       "starport"
   ["starport"]                      "banshee"
   ["starport" "expansion-bay"]      "cloaking" })

(def num-allowed-constructions 
  {"scv"           11
   "refinery"      2
   "supply-depot"  1
   "barracks"      1
   "factory"       1
   "startport"     1
   "expansion-bay" 1
   "cloaking"      1
   "banshee"       1})

(def unit-mineral-costs
  {"scv"           50
   "refinery"      75
   "supply-depot"  100
   "barracks"      150
   "factory"       150
   "startport"     150
   "expansion-bay" 50
   "cloaking"      200
   "banshee"       150}) 

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
  (reduce (fn [memo building] 
            (let [name (first building) count (last building) num-allowed (get current-buildings-count name)]
              (assoc memo name (if (nil? num-allowed) count (- count num-allowed)))))
    {} num-allowed-constructions))

(defn get-construction-options [state]
  (let [available (get-available-constructions (keys state))
        available-counts (get-allowed-constructions state)]
    (filter (fn [x] 
              (and (contains-string? available (first x)) (not= 0 (last x)))) available-counts)))

(def start-state {"scv" 6})
(println (get-construction-options start-state))
