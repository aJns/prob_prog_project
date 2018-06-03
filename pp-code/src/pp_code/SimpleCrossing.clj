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
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[nil,nil]"}
;; <=

;; **
;;; Set up variables. Change downtime means the duration when nobody moves after a change in light. Waiting cost function means that waiting 10 turns sucks more than 10x more than waiting 1 turn.
;; **

;; @@
(def simulation_duration 500)
(def traffic_rate_a 0.2)
(def traffic_rate_b 0.4)

(def change_downtime 2)

(defn waiting_cost [t] (+ (* 10 t) (* 1 t t) ) )

;; @@
;; =>
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-var'>#&#x27;template/simulation_duration</span>","value":"#'template/simulation_duration"},{"type":"html","content":"<span class='clj-var'>#&#x27;template/traffic_rate_a</span>","value":"#'template/traffic_rate_a"}],"value":"[#'template/simulation_duration,#'template/traffic_rate_a]"},{"type":"html","content":"<span class='clj-var'>#&#x27;template/traffic_rate_b</span>","value":"#'template/traffic_rate_b"}],"value":"[[#'template/simulation_duration,#'template/traffic_rate_a],#'template/traffic_rate_b]"},{"type":"html","content":"<span class='clj-var'>#&#x27;template/change_downtime</span>","value":"#'template/change_downtime"}],"value":"[[[#'template/simulation_duration,#'template/traffic_rate_a],#'template/traffic_rate_b],#'template/change_downtime]"},{"type":"html","content":"<span class='clj-var'>#&#x27;template/waiting_cost</span>","value":"#'template/waiting_cost"}],"value":"[[[[#'template/simulation_duration,#'template/traffic_rate_a],#'template/traffic_rate_b],#'template/change_downtime],#'template/waiting_cost]"}
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
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;template/generate_traffic</span>","value":"#'template/generate_traffic"}
;; <=

;; @@
(def generate_traffic_query (doquery :importance generate_traffic [simulation_duration]))
(def generate_traffic_sample (first generate_traffic_query))
(def generated_traffic (:result generate_traffic_sample))

;; @@
;; =>
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-var'>#&#x27;template/generate_traffic_query</span>","value":"#'template/generate_traffic_query"},{"type":"html","content":"<span class='clj-var'>#&#x27;template/generate_traffic_sample</span>","value":"#'template/generate_traffic_sample"}],"value":"[#'template/generate_traffic_query,#'template/generate_traffic_sample]"},{"type":"html","content":"<span class='clj-var'>#&#x27;template/generated_traffic</span>","value":"#'template/generated_traffic"}],"value":"[[#'template/generate_traffic_query,#'template/generate_traffic_sample],#'template/generated_traffic]"}
;; <=

;; **
;;; 
;; **

;; **
;;; 
;; **

;; **
;;; Test different traffic light configurations with generated traffic
;; **

;; **
;;; 
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
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;template/simulate_crossing</span>","value":"#'template/simulate_crossing"}
;; <=

;; @@

;; @@

;; @@

(def query_def (doquery :lmh simulate_crossing [simulation_duration (:traffic_a generated_traffic) (:traffic_b generated_traffic)]))
(def query_sample (take-nth 10 (take 2000 (drop 1000 query_def))))
;; @@
;; =>
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-var'>#&#x27;template/query_def</span>","value":"#'template/query_def"},{"type":"html","content":"<span class='clj-var'>#&#x27;template/query_sample</span>","value":"#'template/query_sample"}],"value":"[#'template/query_def,#'template/query_sample]"}
;; <=

;; @@


;; @@

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
;;; {:cost 41618, :cycle_a 5, :cycle_b 9}
;;; {:cost 41618, :cycle_a 5, :cycle_b 9}
;;; {:cost 41618, :cycle_a 5, :cycle_b 9}
;;; {:cost 42251, :cycle_a 6, :cycle_b 9}
;;; {:cost 42988, :cycle_a 5, :cycle_b 8}
;;; {:cost 43003, :cycle_a 4, :cycle_b 7}
;;; {:cost 43003, :cycle_a 4, :cycle_b 7}
;;; {:cost 43003, :cycle_a 4, :cycle_b 7}
;;; {:cost 46286, :cycle_a 4, :cycle_b 8}
;;; {:cost 46286, :cycle_a 4, :cycle_b 8}
;;; {:cost 54764, :cycle_a 4, :cycle_b 9}
;;; {:cost 54764, :cycle_a 4, :cycle_b 9}
;;; {:cost 58266, :cycle_a 3, :cycle_b 6}
;;; {:cost 58266, :cycle_a 3, :cycle_b 6}
;;; {:cost 58266, :cycle_a 3, :cycle_b 6}
;;; {:cost 58266, :cycle_a 3, :cycle_b 6}
;;; {:cost 74260, :cycle_a 6, :cycle_b 8}
;;; {:cost 74260, :cycle_a 6, :cycle_b 8}
;;; {:cost 74260, :cycle_a 6, :cycle_b 8}
;;; {:cost 75253, :cycle_a 3, :cycle_b 7}
;;; {:cost 75253, :cycle_a 3, :cycle_b 7}
;;; {:cost 75253, :cycle_a 3, :cycle_b 7}
;;; {:cost 75253, :cycle_a 3, :cycle_b 7}
;;; {:cost 82301, :cycle_a 5, :cycle_b 7}
;;; {:cost 127514, :cycle_a 8, :cycle_b 9}
;;; 
;; <-
;; =>
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-var'>#&#x27;template/results</span>","value":"#'template/results"},{"type":"html","content":"<span class='clj-var'>#&#x27;template/performance</span>","value":"#'template/performance"}],"value":"[#'template/results,#'template/performance]"},{"type":"html","content":"<span class='clj-var'>#&#x27;template/top_cycles</span>","value":"#'template/top_cycles"}],"value":"[[#'template/results,#'template/performance],#'template/top_cycles]"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[[[#'template/results,#'template/performance],#'template/top_cycles],nil]"}
;; <=

;; **
;;; TODO: 
;;; - FIXED Currently, the algorithm strongly favors traffic cycle a. This might be due to importance algorithm or a bug. This needs to be checked! May be also fixed by moving traffic generation outside the query.
;;; 
;;; - Generate traffic once for all traffic light variations. Now the traffic is generated for each variation separately which makes them less comparable.
;; **

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
;;; 1
;;; 9
;;; {:queue_a (), :cost_times (), :b_left 0, :new_a true, :new_b true, :queue_b (), :a_left 1, :turns_left 50, :pass 0}
;;; {:queue_a (), :cost_times (), :b_left 9, :new_a false, :new_b false, :queue_b (50), :a_left 0, :turns_left 49, :pass 2}
;;; {:queue_a (), :cost_times (), :b_left 9, :new_a true, :new_b true, :queue_b (50), :a_left 0, :turns_left 48, :pass 1}
;;; {:queue_a (48), :cost_times (), :b_left 9, :new_a true, :new_b true, :queue_b (50), :a_left 0, :turns_left 47, :pass 0}
;;; {:queue_a (47 48), :cost_times (3), :b_left 8, :new_a false, :new_b false, :queue_b (), :a_left 0, :turns_left 46, :pass 0}
;;; {:queue_a (47 48), :cost_times (3), :b_left 7, :new_a false, :new_b false, :queue_b (), :a_left 0, :turns_left 45, :pass 0}
;;; {:queue_a (47 48), :cost_times (3), :b_left 6, :new_a false, :new_b false, :queue_b (), :a_left 0, :turns_left 44, :pass 0}
;;; {:queue_a (47 48), :cost_times (3), :b_left 5, :new_a false, :new_b false, :queue_b (), :a_left 0, :turns_left 43, :pass 0}
;;; {:queue_a (47 48), :cost_times (3), :b_left 4, :new_a false, :new_b false, :queue_b (), :a_left 0, :turns_left 42, :pass 0}
;;; {:queue_a (47 48), :cost_times (3), :b_left 3, :new_a false, :new_b false, :queue_b (), :a_left 0, :turns_left 41, :pass 0}
;;; {:queue_a (47 48), :cost_times (3), :b_left 2, :new_a false, :new_b false, :queue_b (), :a_left 0, :turns_left 40, :pass 0}
;;; {:queue_a (47 48), :cost_times (3), :b_left 1, :new_a false, :new_b false, :queue_b (), :a_left 0, :turns_left 39, :pass 0}
;;; {:queue_a (47 48), :cost_times (3), :b_left 0, :new_a false, :new_b false, :queue_b (), :a_left 1, :turns_left 38, :pass 2}
;;; {:queue_a (47 48), :cost_times (3), :b_left 0, :new_a false, :new_b false, :queue_b (38), :a_left 1, :turns_left 37, :pass 1}
;;; {:queue_a (47 48), :cost_times (3), :b_left 0, :new_a false, :new_b false, :queue_b (38), :a_left 1, :turns_left 36, :pass 0}
;;; {:queue_a (47), :cost_times (12 3), :b_left 9, :new_a false, :new_b false, :queue_b (38), :a_left 0, :turns_left 35, :pass 2}
;;; {:queue_a (47), :cost_times (12 3), :b_left 9, :new_a false, :new_b false, :queue_b (35 38), :a_left 0, :turns_left 34, :pass 1}
;;; {:queue_a (47), :cost_times (12 3), :b_left 9, :new_a false, :new_b false, :queue_b (35 38), :a_left 0, :turns_left 33, :pass 0}
;;; {:queue_a (47), :cost_times (5 12 3), :b_left 8, :new_a true, :new_b true, :queue_b (35), :a_left 0, :turns_left 32, :pass 0}
;;; {:queue_a (32 47), :cost_times (3 5 12 3), :b_left 7, :new_a false, :new_b false, :queue_b (), :a_left 0, :turns_left 31, :pass 0}
;;; {:queue_a (32 47), :cost_times (3 5 12 3), :b_left 6, :new_a false, :new_b false, :queue_b (), :a_left 0, :turns_left 30, :pass 0}
;;; {:queue_a (32 47), :cost_times (3 5 12 3), :b_left 5, :new_a false, :new_b false, :queue_b (), :a_left 0, :turns_left 29, :pass 0}
;;; {:queue_a (32 47), :cost_times (3 5 12 3), :b_left 4, :new_a false, :new_b false, :queue_b (), :a_left 0, :turns_left 28, :pass 0}
;;; {:queue_a (32 47), :cost_times (3 5 12 3), :b_left 3, :new_a false, :new_b false, :queue_b (), :a_left 0, :turns_left 27, :pass 0}
;;; {:queue_a (32 47), :cost_times (3 5 12 3), :b_left 2, :new_a false, :new_b false, :queue_b (), :a_left 0, :turns_left 26, :pass 0}
;;; {:queue_a (32 47), :cost_times (3 5 12 3), :b_left 1, :new_a false, :new_b false, :queue_b (), :a_left 0, :turns_left 25, :pass 0}
;;; {:queue_a (32 47), :cost_times (3 5 12 3), :b_left 0, :new_a true, :new_b true, :queue_b (), :a_left 1, :turns_left 24, :pass 2}
;;; {:queue_a (24 32 47), :cost_times (3 5 12 3), :b_left 0, :new_a false, :new_b false, :queue_b (), :a_left 1, :turns_left 23, :pass 1}
;;; {:queue_a (24 32 47), :cost_times (3 5 12 3), :b_left 0, :new_a false, :new_b false, :queue_b (23), :a_left 1, :turns_left 22, :pass 0}
;;; {:queue_a (24 32), :cost_times (25 3 5 12 3), :b_left 9, :new_a true, :new_b true, :queue_b (23), :a_left 0, :turns_left 21, :pass 2}
;;; {:queue_a (21 24 32), :cost_times (25 3 5 12 3), :b_left 9, :new_a true, :new_b true, :queue_b (23), :a_left 0, :turns_left 20, :pass 1}
;;; {:queue_a (20 21 24 32), :cost_times (25 3 5 12 3), :b_left 9, :new_a true, :new_b true, :queue_b (23), :a_left 0, :turns_left 19, :pass 0}
;;; {:queue_a (19 20 21 24 32), :cost_times (4 25 3 5 12 3), :b_left 8, :new_a false, :new_b false, :queue_b (), :a_left 0, :turns_left 18, :pass 0}
;;; {:queue_a (19 20 21 24 32), :cost_times (4 25 3 5 12 3), :b_left 7, :new_a false, :new_b false, :queue_b (), :a_left 0, :turns_left 17, :pass 0}
;;; {:queue_a (19 20 21 24 32), :cost_times (4 25 3 5 12 3), :b_left 6, :new_a false, :new_b false, :queue_b (), :a_left 0, :turns_left 16, :pass 0}
;;; {:queue_a (19 20 21 24 32), :cost_times (4 25 3 5 12 3), :b_left 5, :new_a false, :new_b false, :queue_b (), :a_left 0, :turns_left 15, :pass 0}
;;; {:queue_a (19 20 21 24 32), :cost_times (4 25 3 5 12 3), :b_left 4, :new_a false, :new_b false, :queue_b (), :a_left 0, :turns_left 14, :pass 0}
;;; {:queue_a (19 20 21 24 32), :cost_times (4 25 3 5 12 3), :b_left 3, :new_a false, :new_b false, :queue_b (), :a_left 0, :turns_left 13, :pass 0}
;;; {:queue_a (19 20 21 24 32), :cost_times (4 25 3 5 12 3), :b_left 2, :new_a false, :new_b false, :queue_b (), :a_left 0, :turns_left 12, :pass 0}
;;; {:queue_a (19 20 21 24 32), :cost_times (4 25 3 5 12 3), :b_left 1, :new_a false, :new_b false, :queue_b (), :a_left 0, :turns_left 11, :pass 0}
;;; {:queue_a (19 20 21 24 32), :cost_times (4 25 3 5 12 3), :b_left 0, :new_a false, :new_b false, :queue_b (), :a_left 1, :turns_left 10, :pass 2}
;;; {:queue_a (19 20 21 24 32), :cost_times (4 25 3 5 12 3), :b_left 0, :new_a false, :new_b false, :queue_b (), :a_left 1, :turns_left 9, :pass 1}
;;; {:queue_a (19 20 21 24 32), :cost_times (4 25 3 5 12 3), :b_left 0, :new_a false, :new_b false, :queue_b (), :a_left 1, :turns_left 8, :pass 0}
;;; {:queue_a (19 20 21 24), :cost_times (24 4 25 3 5 12 3), :b_left 9, :new_a false, :new_b false, :queue_b (), :a_left 0, :turns_left 7, :pass 2}
;;; {:queue_a (19 20 21 24), :cost_times (24 4 25 3 5 12 3), :b_left 9, :new_a false, :new_b false, :queue_b (), :a_left 0, :turns_left 6, :pass 1}
;;; {:queue_a (19 20 21 24), :cost_times (24 4 25 3 5 12 3), :b_left 9, :new_a false, :new_b false, :queue_b (), :a_left 0, :turns_left 5, :pass 0}
;;; {:queue_a (19 20 21 24), :cost_times (24 4 25 3 5 12 3), :b_left 8, :new_a false, :new_b false, :queue_b (), :a_left 0, :turns_left 4, :pass 0}
;;; {:queue_a (19 20 21 24), :cost_times (24 4 25 3 5 12 3), :b_left 7, :new_a false, :new_b false, :queue_b (), :a_left 0, :turns_left 3, :pass 0}
;;; {:queue_a (19 20 21 24), :cost_times (24 4 25 3 5 12 3), :b_left 6, :new_a true, :new_b true, :queue_b (), :a_left 0, :turns_left 2, :pass 0}
;;; {:queue_a (2 19 20 21 24), :cost_times (24 4 25 3 5 12 3), :b_left 5, :new_a false, :new_b false, :queue_b (), :a_left 0, :turns_left 1, :pass 0}
;;; 
;; <-
;; =>
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-var'>#&#x27;template/head_result</span>","value":"#'template/head_result"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[#'template/head_result,nil]"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[[#'template/head_result,nil],nil]"},{"type":"html","content":"<span class='clj-var'>#&#x27;template/turns</span>","value":"#'template/turns"}],"value":"[[[#'template/head_result,nil],nil],#'template/turns]"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[[[[#'template/head_result,nil],nil],#'template/turns],nil]"}
;; <=

;; **
;;; 
;; **

;; **
;;; 
;; **
