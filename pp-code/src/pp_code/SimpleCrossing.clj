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
(def sim-params {:turns 500, :traffic-rate-a 0.2, :traffic-rate-b 0.4, :downtime 2})
(def simulation_duration (sim-params :turns))
(def traffic_rate_a (sim-params :traffic-rate-a))
(def traffic_rate_b (sim-params :traffic-rate-b))

(def change_downtime (sim-params :downtime))

(defn waiting_cost [t] (+ (* 10 t) (* 1 t t) ) )

;; @@
;; =>
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-var'>#&#x27;pp-code.SimpleCrossing/sim-params</span>","value":"#'pp-code.SimpleCrossing/sim-params"},{"type":"html","content":"<span class='clj-var'>#&#x27;pp-code.SimpleCrossing/simulation_duration</span>","value":"#'pp-code.SimpleCrossing/simulation_duration"}],"value":"[#'pp-code.SimpleCrossing/sim-params,#'pp-code.SimpleCrossing/simulation_duration]"},{"type":"html","content":"<span class='clj-var'>#&#x27;pp-code.SimpleCrossing/traffic_rate_a</span>","value":"#'pp-code.SimpleCrossing/traffic_rate_a"}],"value":"[[#'pp-code.SimpleCrossing/sim-params,#'pp-code.SimpleCrossing/simulation_duration],#'pp-code.SimpleCrossing/traffic_rate_a]"},{"type":"html","content":"<span class='clj-var'>#&#x27;pp-code.SimpleCrossing/traffic_rate_b</span>","value":"#'pp-code.SimpleCrossing/traffic_rate_b"}],"value":"[[[#'pp-code.SimpleCrossing/sim-params,#'pp-code.SimpleCrossing/simulation_duration],#'pp-code.SimpleCrossing/traffic_rate_a],#'pp-code.SimpleCrossing/traffic_rate_b]"},{"type":"html","content":"<span class='clj-var'>#&#x27;pp-code.SimpleCrossing/change_downtime</span>","value":"#'pp-code.SimpleCrossing/change_downtime"}],"value":"[[[[#'pp-code.SimpleCrossing/sim-params,#'pp-code.SimpleCrossing/simulation_duration],#'pp-code.SimpleCrossing/traffic_rate_a],#'pp-code.SimpleCrossing/traffic_rate_b],#'pp-code.SimpleCrossing/change_downtime]"},{"type":"html","content":"<span class='clj-var'>#&#x27;pp-code.SimpleCrossing/waiting_cost</span>","value":"#'pp-code.SimpleCrossing/waiting_cost"}],"value":"[[[[[#'pp-code.SimpleCrossing/sim-params,#'pp-code.SimpleCrossing/simulation_duration],#'pp-code.SimpleCrossing/traffic_rate_a],#'pp-code.SimpleCrossing/traffic_rate_b],#'pp-code.SimpleCrossing/change_downtime],#'pp-code.SimpleCrossing/waiting_cost]"}
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
(def generate_traffic_query (doquery :importance generate_traffic [(sim-params :turns)]))
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
;;; {:cost 38417, :cycle_a 5, :cycle_b 9}
;;; {:cost 38823, :cycle_a 4, :cycle_b 8}
;;; {:cost 38823, :cycle_a 4, :cycle_b 8}
;;; {:cost 38823, :cycle_a 4, :cycle_b 8}
;;; {:cost 38823, :cycle_a 4, :cycle_b 8}
;;; {:cost 43133, :cycle_a 6, :cycle_b 9}
;;; {:cost 43133, :cycle_a 6, :cycle_b 9}
;;; {:cost 43133, :cycle_a 6, :cycle_b 9}
;;; {:cost 43133, :cycle_a 6, :cycle_b 9}
;;; {:cost 43133, :cycle_a 6, :cycle_b 9}
;;; {:cost 45051, :cycle_a 4, :cycle_b 9}
;;; {:cost 45051, :cycle_a 4, :cycle_b 9}
;;; {:cost 45051, :cycle_a 4, :cycle_b 9}
;;; {:cost 45936, :cycle_a 4, :cycle_b 7}
;;; {:cost 45936, :cycle_a 4, :cycle_b 7}
;;; {:cost 50361, :cycle_a 5, :cycle_b 8}
;;; {:cost 58439, :cycle_a 5, :cycle_b 7}
;;; {:cost 58439, :cycle_a 5, :cycle_b 7}
;;; {:cost 59414, :cycle_a 7, :cycle_b 9}
;;; {:cost 75995, :cycle_a 6, :cycle_b 8}
;;; {:cost 75995, :cycle_a 6, :cycle_b 8}
;;; {:cost 86746, :cycle_a 4, :cycle_b 6}
;;; {:cost 86746, :cycle_a 4, :cycle_b 6}
;;; {:cost 86746, :cycle_a 4, :cycle_b 6}
;;; {:cost 86746, :cycle_a 4, :cycle_b 6}
;;; 
;; <-
;; =>
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-var'>#&#x27;pp-code.SimpleCrossing/results</span>","value":"#'pp-code.SimpleCrossing/results"},{"type":"html","content":"<span class='clj-var'>#&#x27;pp-code.SimpleCrossing/performance</span>","value":"#'pp-code.SimpleCrossing/performance"}],"value":"[#'pp-code.SimpleCrossing/results,#'pp-code.SimpleCrossing/performance]"},{"type":"html","content":"<span class='clj-var'>#&#x27;pp-code.SimpleCrossing/top_cycles</span>","value":"#'pp-code.SimpleCrossing/top_cycles"}],"value":"[[#'pp-code.SimpleCrossing/results,#'pp-code.SimpleCrossing/performance],#'pp-code.SimpleCrossing/top_cycles]"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[[[#'pp-code.SimpleCrossing/results,#'pp-code.SimpleCrossing/performance],#'pp-code.SimpleCrossing/top_cycles],nil]"}
;; <=

;; @@
(let [get-cycles (fn [cycle-key] (map (fn [c] (vals (select-keys c [cycle-key :cost]))) top_cycles))
      plot-size 1200
      opacity 0.25
      x-range [1 10]]
  (println sim-params)
  (doseq [c (take 5 top_cycles)]
    (println c))
  (plot/compose
    (plot/list-plot (get-cycles :cycle_a) :plot-size plot-size :opacity opacity :plot-range [x-range :all] :colour "red")
  	(plot/list-plot (get-cycles :cycle_b) :plot-size plot-size :opacity opacity :plot-range [x-range :all] :colour "blue")))
;; @@
;; ->
;;; {:turns 500, :traffic-rate-a 0.2, :traffic-rate-b 0.4, :downtime 2}
;;; {:cost 38417, :cycle_a 5, :cycle_b 9}
;;; {:cost 38823, :cycle_a 4, :cycle_b 8}
;;; {:cost 38823, :cycle_a 4, :cycle_b 8}
;;; {:cost 38823, :cycle_a 4, :cycle_b 8}
;;; {:cost 38823, :cycle_a 4, :cycle_b 8}
;;; 
;; <-
;; =>
;;; {"type":"vega","content":{"width":1200,"height":741.6563720703125,"padding":{"top":10,"left":55,"bottom":40,"right":10},"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":[1,10]},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"b52b29a3-7814-4e30-a87f-2db0a819bd92","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}],"data":[{"name":"b52b29a3-7814-4e30-a87f-2db0a819bd92","values":[{"x":5,"y":38417},{"x":4,"y":38823},{"x":4,"y":38823},{"x":4,"y":38823},{"x":4,"y":38823},{"x":6,"y":43133},{"x":6,"y":43133},{"x":6,"y":43133},{"x":6,"y":43133},{"x":6,"y":43133},{"x":4,"y":45051},{"x":4,"y":45051},{"x":4,"y":45051},{"x":4,"y":45936},{"x":4,"y":45936},{"x":5,"y":50361},{"x":5,"y":58439},{"x":5,"y":58439},{"x":7,"y":59414},{"x":6,"y":75995},{"x":6,"y":75995},{"x":4,"y":86746},{"x":4,"y":86746},{"x":4,"y":86746},{"x":4,"y":86746}]},{"name":"a75f38dc-ac0e-4eb5-a700-5871b9825b52","values":[{"x":9,"y":38417},{"x":8,"y":38823},{"x":8,"y":38823},{"x":8,"y":38823},{"x":8,"y":38823},{"x":9,"y":43133},{"x":9,"y":43133},{"x":9,"y":43133},{"x":9,"y":43133},{"x":9,"y":43133},{"x":9,"y":45051},{"x":9,"y":45051},{"x":9,"y":45051},{"x":7,"y":45936},{"x":7,"y":45936},{"x":8,"y":50361},{"x":7,"y":58439},{"x":7,"y":58439},{"x":9,"y":59414},{"x":8,"y":75995},{"x":8,"y":75995},{"x":6,"y":86746},{"x":6,"y":86746},{"x":6,"y":86746},{"x":6,"y":86746}]}],"marks":[{"type":"symbol","from":{"data":"b52b29a3-7814-4e30-a87f-2db0a819bd92"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"fill":{"value":"red"},"fillOpacity":{"value":0.25}},"update":{"shape":"circle","size":{"value":70},"stroke":{"value":"transparent"}},"hover":{"size":{"value":210},"stroke":{"value":"white"}}}},{"type":"symbol","from":{"data":"a75f38dc-ac0e-4eb5-a700-5871b9825b52"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"fill":{"value":"blue"},"fillOpacity":{"value":0.25}},"update":{"shape":"circle","size":{"value":70},"stroke":{"value":"transparent"}},"hover":{"size":{"value":210},"stroke":{"value":"white"}}}}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 1200, :height 741.6564, :padding {:top 10, :left 55, :bottom 40, :right 10}, :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain [1 10]} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"b52b29a3-7814-4e30-a87f-2db0a819bd92\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :data ({:name \"b52b29a3-7814-4e30-a87f-2db0a819bd92\", :values ({:x 5, :y 38417} {:x 4, :y 38823} {:x 4, :y 38823} {:x 4, :y 38823} {:x 4, :y 38823} {:x 6, :y 43133} {:x 6, :y 43133} {:x 6, :y 43133} {:x 6, :y 43133} {:x 6, :y 43133} {:x 4, :y 45051} {:x 4, :y 45051} {:x 4, :y 45051} {:x 4, :y 45936} {:x 4, :y 45936} {:x 5, :y 50361} {:x 5, :y 58439} {:x 5, :y 58439} {:x 7, :y 59414} {:x 6, :y 75995} {:x 6, :y 75995} {:x 4, :y 86746} {:x 4, :y 86746} {:x 4, :y 86746} {:x 4, :y 86746})} {:name \"a75f38dc-ac0e-4eb5-a700-5871b9825b52\", :values ({:x 9, :y 38417} {:x 8, :y 38823} {:x 8, :y 38823} {:x 8, :y 38823} {:x 8, :y 38823} {:x 9, :y 43133} {:x 9, :y 43133} {:x 9, :y 43133} {:x 9, :y 43133} {:x 9, :y 43133} {:x 9, :y 45051} {:x 9, :y 45051} {:x 9, :y 45051} {:x 7, :y 45936} {:x 7, :y 45936} {:x 8, :y 50361} {:x 7, :y 58439} {:x 7, :y 58439} {:x 9, :y 59414} {:x 8, :y 75995} {:x 8, :y 75995} {:x 6, :y 86746} {:x 6, :y 86746} {:x 6, :y 86746} {:x 6, :y 86746})}), :marks ({:type \"symbol\", :from {:data \"b52b29a3-7814-4e30-a87f-2db0a819bd92\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :fill {:value \"red\"}, :fillOpacity {:value 0.25}}, :update {:shape \"circle\", :size {:value 70}, :stroke {:value \"transparent\"}}, :hover {:size {:value 210}, :stroke {:value \"white\"}}}} {:type \"symbol\", :from {:data \"a75f38dc-ac0e-4eb5-a700-5871b9825b52\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :fill {:value \"blue\"}, :fillOpacity {:value 0.25}}, :update {:shape \"circle\", :size {:value 70}, :stroke {:value \"transparent\"}}, :hover {:size {:value 210}, :stroke {:value \"white\"}}}})}}"}
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
;;; nil
;;; nil
;;; 
;; <-
;; =>
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-var'>#&#x27;pp-code.SimpleCrossing/head_result</span>","value":"#'pp-code.SimpleCrossing/head_result"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[#'pp-code.SimpleCrossing/head_result,nil]"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[[#'pp-code.SimpleCrossing/head_result,nil],nil]"},{"type":"html","content":"<span class='clj-var'>#&#x27;pp-code.SimpleCrossing/turns</span>","value":"#'pp-code.SimpleCrossing/turns"}],"value":"[[[#'pp-code.SimpleCrossing/head_result,nil],nil],#'pp-code.SimpleCrossing/turns]"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[[[[#'pp-code.SimpleCrossing/head_result,nil],nil],#'pp-code.SimpleCrossing/turns],nil]"}
;; <=
