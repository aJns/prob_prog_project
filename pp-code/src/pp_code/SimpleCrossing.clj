;; gorilla-repl.fileformat = 1

;; **
;;; # Traffic light optimator
;;; 
;;; 
;; **

;; @@
(ns pp-code.SimpleCrossing
  (:require [gorilla-plot.core :as plot])
  (:use [anglican core emit runtime
         [state :only [get-predicts get-log-weight]]]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; Set up variables. Change downtime means the duration when nobody moves after a change in light. Waiting cost function means that waiting 10 turns sucks more than 10x more than waiting 1 turn.
;; **

;; @@
(def simulation_duration 500)
(def traffic_rate_a 0.5)
(def traffic_rate_b 0.4)

(def change_downtime 2)

(defn waiting_cost [t] (+ (* 10 t) (* 1 t t) ) )

;; @@
;; =>
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-var'>#&#x27;pp-code.SimpleCrossing/simulation_duration</span>","value":"#'pp-code.SimpleCrossing/simulation_duration"},{"type":"html","content":"<span class='clj-var'>#&#x27;pp-code.SimpleCrossing/traffic_rate_a</span>","value":"#'pp-code.SimpleCrossing/traffic_rate_a"}],"value":"[#'pp-code.SimpleCrossing/simulation_duration,#'pp-code.SimpleCrossing/traffic_rate_a]"},{"type":"html","content":"<span class='clj-var'>#&#x27;pp-code.SimpleCrossing/traffic_rate_b</span>","value":"#'pp-code.SimpleCrossing/traffic_rate_b"}],"value":"[[#'pp-code.SimpleCrossing/simulation_duration,#'pp-code.SimpleCrossing/traffic_rate_a],#'pp-code.SimpleCrossing/traffic_rate_b]"},{"type":"html","content":"<span class='clj-var'>#&#x27;pp-code.SimpleCrossing/change_downtime</span>","value":"#'pp-code.SimpleCrossing/change_downtime"}],"value":"[[[#'pp-code.SimpleCrossing/simulation_duration,#'pp-code.SimpleCrossing/traffic_rate_a],#'pp-code.SimpleCrossing/traffic_rate_b],#'pp-code.SimpleCrossing/change_downtime]"},{"type":"html","content":"<span class='clj-var'>#&#x27;pp-code.SimpleCrossing/waiting_cost</span>","value":"#'pp-code.SimpleCrossing/waiting_cost"}],"value":"[[[[#'pp-code.SimpleCrossing/simulation_duration,#'pp-code.SimpleCrossing/traffic_rate_a],#'pp-code.SimpleCrossing/traffic_rate_b],#'pp-code.SimpleCrossing/change_downtime],#'pp-code.SimpleCrossing/waiting_cost]"}
;; <=

;; **
;;; Generate some traffic
;; **

;; @@
(defquery generate_traffic [duration]
  (loop [
          turns_left duration
          traffic_a (list)
          traffic_b (list)
          ]
    (if (< turns_left 1)
      {:traffic_a traffic_a :traffic_b traffic_b}
      (let [
           new_a (sample (flip traffic_rate_a))
           new_queue_a (conj traffic_a new_a)
           
           new_b (sample (flip traffic_rate_b))
           new_queue_b (conj traffic_b new_b)
           ]
      
        (recur (- turns_left 1)
             new_queue_a
             new_queue_b)
        )
      )    	
    )
  )
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;pp-code.SimpleCrossing/generate_traffic</span>","value":"#'pp-code.SimpleCrossing/generate_traffic"}
;; <=

;; @@
(def generate_traffic_query (doquery :importance generate_traffic [simulation_duration]))
(def generate_traffic_sample (first generate_traffic_query))
(def generated_traffic (:result generate_traffic_sample))

;; @@
;; =>
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-var'>#&#x27;pp-code.SimpleCrossing/generate_traffic_query</span>","value":"#'pp-code.SimpleCrossing/generate_traffic_query"},{"type":"html","content":"<span class='clj-var'>#&#x27;pp-code.SimpleCrossing/generate_traffic_sample</span>","value":"#'pp-code.SimpleCrossing/generate_traffic_sample"}],"value":"[#'pp-code.SimpleCrossing/generate_traffic_query,#'pp-code.SimpleCrossing/generate_traffic_sample]"},{"type":"html","content":"<span class='clj-var'>#&#x27;pp-code.SimpleCrossing/generated_traffic</span>","value":"#'pp-code.SimpleCrossing/generated_traffic"}],"value":"[[#'pp-code.SimpleCrossing/generate_traffic_query,#'pp-code.SimpleCrossing/generate_traffic_sample],#'pp-code.SimpleCrossing/generated_traffic]"}
;; <=

;; **
;;; Test different traffic light configurations with generated traffic
;; **

;; @@
(defquery simulate_crossing [duration traffic_a traffic_b]
  (let [
         traffic_cycle_a (sample (uniform-discrete 1 10))
         traffic_cycle_b (sample (uniform-discrete 1 10))
         ]
    (loop [
            turns_left duration
            queue_a (list)
            queue_b (list)
            a_left traffic_cycle_a
            b_left 0
            pass 0
            cost_times (list)
            iteration_log (list)
           ]
      (if (< turns_left 1)
        (let [total_cost_times (concat cost_times queue_a queue_b)]
          {:cycle_a traffic_cycle_a 
           :cycle_b traffic_cycle_b 
           :cost_times total_cost_times
           :iteration_log iteration_log}
          )
        
        (let [
               
               
               turn_situation {
                                :turns_left turns_left
                                :cost_times cost_times
                                :a_left a_left
                                :b_left b_left
                                :pass pass
                                :queue_a queue_a
                                :queue_b queue_b
                                :new_a (nth traffic_a (- turns_left 1))
                                :new_b (nth traffic_a (- turns_left 1))
                                }
               new_iteration_log (conj iteration_log turn_situation)
             
               
               new_a (nth traffic_a (- turns_left 1))
               added_queue_a (if new_a (conj queue_a turns_left) queue_a)
               
               new_b (nth traffic_b (- turns_left 1))
               added_queue_b (if new_b (conj queue_b turns_left) queue_b)
               
               new_queue_a (if
                             (and (> a_left 0) (= pass 0))
                             (drop-last added_queue_a)
                             added_queue_a
                             )

               new_queue_b (if
                             (and (> b_left 0) (= pass 0))
                             (drop-last added_queue_b)
                             added_queue_b
                             )
               
               new_a_left (if 
                            (> pass 0)
                            a_left
                            (if
                              (or (> b_left 1) (> a_left 0))
                              (max (- a_left 1) 0)
                              traffic_cycle_a
                              )
                            )
               
               new_b_left (if
                            (> pass 0)
                            b_left
                            (if
                              (or (> a_left 1) (> b_left 0))
                              (max (- b_left 1) 0)
                              traffic_cycle_b
                              )
                            )
               
               new_pass (if
                          (or (> new_a_left a_left) (> new_b_left b_left))
                          change_downtime
                          (max (- pass 1) 0)
                          )
               
               car_from_queue (if (> pass 0)
                               nil
                               (if (> b_left 0)
                                 (last queue_b)
                                 (last queue_a)
                                 
                                 )
                                )
               
               new_cost_times (if car_from_queue
                                (conj cost_times (- car_from_queue turns_left) )
                                cost_times
                                )
               
               
               ]
          (recur (- turns_left 1)
                 new_queue_a
                 new_queue_b
                 new_a_left
                 new_b_left
                 new_pass
                 new_cost_times
                 new_iteration_log
                 )
          )
          
        )
      )  ))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;pp-code.SimpleCrossing/simulate_crossing</span>","value":"#'pp-code.SimpleCrossing/simulate_crossing"}
;; <=

;; @@

(def query_def (doquery :lmh simulate_crossing [simulation_duration (:traffic_a generated_traffic) (:traffic_b generated_traffic)]))
(def query_sample (take-nth 10 (take 2000 (drop 1000 query_def))))
;; @@
;; =>
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-var'>#&#x27;pp-code.SimpleCrossing/query_def</span>","value":"#'pp-code.SimpleCrossing/query_def"},{"type":"html","content":"<span class='clj-var'>#&#x27;pp-code.SimpleCrossing/query_sample</span>","value":"#'pp-code.SimpleCrossing/query_sample"}],"value":"[#'pp-code.SimpleCrossing/query_def,#'pp-code.SimpleCrossing/query_sample]"}
;; <=

;; @@
(def results (map :result query_sample) )

(def performance (map (fn [x] {
               :cost (reduce + 0 (map waiting_cost (:cost_times x) ))
               :cycle_a (:cycle_a x)
               :cycle_b (:cycle_b x)
               } ) 
     results )
)

(def top_cycles (take 25 (sort-by :cost performance)))


(doseq [c top_cycles]
  (println c)
  )
;; @@
;; ->
;;; {:cost 198910, :cycle_a 9, :cycle_b 7}
;;; {:cost 230476, :cycle_a 9, :cycle_b 8}
;;; {:cost 230476, :cycle_a 9, :cycle_b 8}
;;; {:cost 230476, :cycle_a 9, :cycle_b 8}
;;; {:cost 230476, :cycle_a 9, :cycle_b 8}
;;; {:cost 230476, :cycle_a 9, :cycle_b 8}
;;; {:cost 230476, :cycle_a 9, :cycle_b 8}
;;; {:cost 308451, :cycle_a 8, :cycle_b 6}
;;; {:cost 352653, :cycle_a 7, :cycle_b 6}
;;; {:cost 385172, :cycle_a 9, :cycle_b 9}
;;; {:cost 385172, :cycle_a 9, :cycle_b 9}
;;; {:cost 385172, :cycle_a 9, :cycle_b 9}
;;; {:cost 385172, :cycle_a 9, :cycle_b 9}
;;; {:cost 385172, :cycle_a 9, :cycle_b 9}
;;; {:cost 458764, :cycle_a 8, :cycle_b 8}
;;; {:cost 483609, :cycle_a 9, :cycle_b 6}
;;; {:cost 483609, :cycle_a 9, :cycle_b 6}
;;; {:cost 583268, :cycle_a 7, :cycle_b 5}
;;; {:cost 583268, :cycle_a 7, :cycle_b 5}
;;; {:cost 583268, :cycle_a 7, :cycle_b 5}
;;; {:cost 583347, :cycle_a 7, :cycle_b 7}
;;; {:cost 583347, :cycle_a 7, :cycle_b 7}
;;; {:cost 596241, :cycle_a 6, :cycle_b 5}
;;; {:cost 596241, :cycle_a 6, :cycle_b 5}
;;; {:cost 713912, :cycle_a 8, :cycle_b 9}
;;; 
;; <-
;; =>
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-var'>#&#x27;pp-code.SimpleCrossing/results</span>","value":"#'pp-code.SimpleCrossing/results"},{"type":"html","content":"<span class='clj-var'>#&#x27;pp-code.SimpleCrossing/performance</span>","value":"#'pp-code.SimpleCrossing/performance"}],"value":"[#'pp-code.SimpleCrossing/results,#'pp-code.SimpleCrossing/performance]"},{"type":"html","content":"<span class='clj-var'>#&#x27;pp-code.SimpleCrossing/top_cycles</span>","value":"#'pp-code.SimpleCrossing/top_cycles"}],"value":"[[#'pp-code.SimpleCrossing/results,#'pp-code.SimpleCrossing/performance],#'pp-code.SimpleCrossing/top_cycles]"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[[[#'pp-code.SimpleCrossing/results,#'pp-code.SimpleCrossing/performance],#'pp-code.SimpleCrossing/top_cycles],nil]"}
;; <=

;; @@
(let [get-cycles (fn [cycle-key] (map (fn [c] (vals (select-keys c [cycle-key :cost]))) top_cycles))]
  (plot/compose
    (plot/list-plot (get-cycles :cycle_a) :colour "red" :opacity 0.75) ;; Extract cycle_a and cost, and change the map into a sequence for list-plot
  	(plot/list-plot (get-cycles :cycle_b) :colour "blue" :opacity 0.75)))
;; @@
;; =>
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"5b10f598-a02d-48d6-8f2f-e3542ecc7416","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"5b10f598-a02d-48d6-8f2f-e3542ecc7416","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}],"data":[{"name":"5b10f598-a02d-48d6-8f2f-e3542ecc7416","values":[{"x":9,"y":198910},{"x":9,"y":230476},{"x":9,"y":230476},{"x":9,"y":230476},{"x":9,"y":230476},{"x":9,"y":230476},{"x":9,"y":230476},{"x":8,"y":308451},{"x":7,"y":352653},{"x":9,"y":385172},{"x":9,"y":385172},{"x":9,"y":385172},{"x":9,"y":385172},{"x":9,"y":385172},{"x":8,"y":458764},{"x":9,"y":483609},{"x":9,"y":483609},{"x":7,"y":583268},{"x":7,"y":583268},{"x":7,"y":583268},{"x":7,"y":583347},{"x":7,"y":583347},{"x":6,"y":596241},{"x":6,"y":596241},{"x":8,"y":713912}]},{"name":"39decfb1-28a1-462c-90e9-dc1bb2c871b2","values":[{"x":7,"y":198910},{"x":8,"y":230476},{"x":8,"y":230476},{"x":8,"y":230476},{"x":8,"y":230476},{"x":8,"y":230476},{"x":8,"y":230476},{"x":6,"y":308451},{"x":6,"y":352653},{"x":9,"y":385172},{"x":9,"y":385172},{"x":9,"y":385172},{"x":9,"y":385172},{"x":9,"y":385172},{"x":8,"y":458764},{"x":6,"y":483609},{"x":6,"y":483609},{"x":5,"y":583268},{"x":5,"y":583268},{"x":5,"y":583268},{"x":7,"y":583347},{"x":7,"y":583347},{"x":5,"y":596241},{"x":5,"y":596241},{"x":9,"y":713912}]}],"marks":[{"type":"symbol","from":{"data":"5b10f598-a02d-48d6-8f2f-e3542ecc7416"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"fill":{"value":"red"},"fillOpacity":{"value":0.75}},"update":{"shape":"circle","size":{"value":70},"stroke":{"value":"transparent"}},"hover":{"size":{"value":210},"stroke":{"value":"white"}}}},{"type":"symbol","from":{"data":"39decfb1-28a1-462c-90e9-dc1bb2c871b2"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"fill":{"value":"blue"},"fillOpacity":{"value":0.75}},"update":{"shape":"circle","size":{"value":70},"stroke":{"value":"transparent"}},"hover":{"size":{"value":210},"stroke":{"value":"white"}}}}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"5b10f598-a02d-48d6-8f2f-e3542ecc7416\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"5b10f598-a02d-48d6-8f2f-e3542ecc7416\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :data ({:name \"5b10f598-a02d-48d6-8f2f-e3542ecc7416\", :values ({:x 9, :y 198910} {:x 9, :y 230476} {:x 9, :y 230476} {:x 9, :y 230476} {:x 9, :y 230476} {:x 9, :y 230476} {:x 9, :y 230476} {:x 8, :y 308451} {:x 7, :y 352653} {:x 9, :y 385172} {:x 9, :y 385172} {:x 9, :y 385172} {:x 9, :y 385172} {:x 9, :y 385172} {:x 8, :y 458764} {:x 9, :y 483609} {:x 9, :y 483609} {:x 7, :y 583268} {:x 7, :y 583268} {:x 7, :y 583268} {:x 7, :y 583347} {:x 7, :y 583347} {:x 6, :y 596241} {:x 6, :y 596241} {:x 8, :y 713912})} {:name \"39decfb1-28a1-462c-90e9-dc1bb2c871b2\", :values ({:x 7, :y 198910} {:x 8, :y 230476} {:x 8, :y 230476} {:x 8, :y 230476} {:x 8, :y 230476} {:x 8, :y 230476} {:x 8, :y 230476} {:x 6, :y 308451} {:x 6, :y 352653} {:x 9, :y 385172} {:x 9, :y 385172} {:x 9, :y 385172} {:x 9, :y 385172} {:x 9, :y 385172} {:x 8, :y 458764} {:x 6, :y 483609} {:x 6, :y 483609} {:x 5, :y 583268} {:x 5, :y 583268} {:x 5, :y 583268} {:x 7, :y 583347} {:x 7, :y 583347} {:x 5, :y 596241} {:x 5, :y 596241} {:x 9, :y 713912})}), :marks ({:type \"symbol\", :from {:data \"5b10f598-a02d-48d6-8f2f-e3542ecc7416\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :fill {:value \"red\"}, :fillOpacity {:value 0.75}}, :update {:shape \"circle\", :size {:value 70}, :stroke {:value \"transparent\"}}, :hover {:size {:value 210}, :stroke {:value \"white\"}}}} {:type \"symbol\", :from {:data \"39decfb1-28a1-462c-90e9-dc1bb2c871b2\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :fill {:value \"blue\"}, :fillOpacity {:value 0.75}}, :update {:shape \"circle\", :size {:value 70}, :stroke {:value \"transparent\"}}, :hover {:size {:value 210}, :stroke {:value \"white\"}}}})}}"}
;; <=

;; **
;;; Print log for debugging purposes
;; **

;; @@
(def head_result (:result (first query_sample)))
(println (:cycle_a head_result))
(println (:cycle_b head_result))
(def turns (reverse (:iteration_log head_result)))

(doseq [turn turns]
  (println turn)
  )
;; @@
;; ->
;;; 3
;;; {:queue_a (), :cost_times (), :b_left 0, :new_a true, :new_b true, :queue_b (), :a_left 3, :turns_left 500, :pass 0}
;;; {:queue_a (), :cost_times (), :b_left 0, :new_a true, :new_b true, :queue_b (500), :a_left 2, :turns_left 499, :pass 0}
;;; {:queue_a (), :cost_times (), :b_left 0, :new_a true, :new_b true, :queue_b (499 500), :a_left 1, :turns_left 498, :pass 0}
;;; {:queue_a (), :cost_times (), :b_left 2, :new_a true, :new_b true, :queue_b (498 499 500), :a_left 0, :turns_left 497, :pass 2}
;;; {:queue_a (497), :cost_times (), :b_left 2, :new_a true, :new_b true, :queue_b (498 499 500), :a_left 0, :turns_left 496, :pass 1}
;;; {:queue_a (496 497), :cost_times (), :b_left 2, :new_a true, :new_b true, :queue_b (496 498 499 500), :a_left 0, :turns_left 495, :pass 0}
;;; {:queue_a (495 496 497), :cost_times (5), :b_left 1, :new_a true, :new_b true, :queue_b (496 498 499), :a_left 0, :turns_left 494, :pass 0}
;;; {:queue_a (494 495 496 497), :cost_times (5 5), :b_left 0, :new_a true, :new_b true, :queue_b (496 498), :a_left 3, :turns_left 493, :pass 2}
;;; {:queue_a (493 494 495 496 497), :cost_times (5 5), :b_left 0, :new_a true, :new_b true, :queue_b (496 498), :a_left 3, :turns_left 492, :pass 1}
;;; {:queue_a (492 493 494 495 496 497), :cost_times (5 5), :b_left 0, :new_a true, :new_b true, :queue_b (496 498), :a_left 3, :turns_left 491, :pass 0}
;;; {:queue_a (491 492 493 494 495 496), :cost_times (6 5 5), :b_left 0, :new_a false, :new_b false, :queue_b (496 498), :a_left 2, :turns_left 490, :pass 0}
;;; {:queue_a (491 492 493 494 495), :cost_times (6 6 5 5), :b_left 0, :new_a true, :new_b true, :queue_b (490 496 498), :a_left 1, :turns_left 489, :pass 0}
;;; {:queue_a (489 491 492 493 494), :cost_times (6 6 6 5 5), :b_left 2, :new_a false, :new_b false, :queue_b (489 490 496 498), :a_left 0, :turns_left 488, :pass 2}
;;; {:queue_a (489 491 492 493 494), :cost_times (6 6 6 5 5), :b_left 2, :new_a true, :new_b true, :queue_b (489 490 496 498), :a_left 0, :turns_left 487, :pass 1}
;;; {:queue_a (487 489 491 492 493 494), :cost_times (6 6 6 5 5), :b_left 2, :new_a true, :new_b true, :queue_b (489 490 496 498), :a_left 0, :turns_left 486, :pass 0}
;;; {:queue_a (486 487 489 491 492 493 494), :cost_times (12 6 6 6 5 5), :b_left 1, :new_a true, :new_b true, :queue_b (489 490 496), :a_left 0, :turns_left 485, :pass 0}
;;; {:queue_a (485 486 487 489 491 492 493 494), :cost_times (11 12 6 6 6 5 5), :b_left 0, :new_a true, :new_b true, :queue_b (485 489 490), :a_left 3, :turns_left 484, :pass 2}
;;; {:queue_a (484 485 486 487 489 491 492 493 494), :cost_times (11 12 6 6 6 5 5), :b_left 0, :new_a true, :new_b true, :queue_b (484 485 489 490), :a_left 3, :turns_left 483, :pass 1}
;;; {:queue_a (483 484 485 486 487 489 491 492 493 494), :cost_times (11 12 6 6 6 5 5), :b_left 0, :new_a true, :new_b true, :queue_b (483 484 485 489 490), :a_left 3, :turns_left 482, :pass 0}
;;; {:queue_a (482 483 484 485 486 487 489 491 492 493), :cost_times (12 11 12 6 6 6 5 5), :b_left 0, :new_a true, :new_b true, :queue_b (483 484 485 489 490), :a_left 2, :turns_left 481, :pass 0}
;;; {:queue_a (481 482 483 484 485 486 487 489 491 492), :cost_times (12 12 11 12 6 6 6 5 5), :b_left 0, :new_a true, :new_b true, :queue_b (483 484 485 489 490), :a_left 1, :turns_left 480, :pass 0}
;;; 
;; <-
;; =>
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-var'>#&#x27;pp-code.SimpleCrossing/head_result</span>","value":"#'pp-code.SimpleCrossing/head_result"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[#'pp-code.SimpleCrossing/head_result,nil]"},{"type":"html","content":"<span class='clj-var'>#&#x27;pp-code.SimpleCrossing/turns</span>","value":"#'pp-code.SimpleCrossing/turns"}],"value":"[[#'pp-code.SimpleCrossing/head_result,nil],#'pp-code.SimpleCrossing/turns]"}
;; <=

;; **
;;; 
;; **

;; **
;;; 
;; **
