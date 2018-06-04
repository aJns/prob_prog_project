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

;; **
;;; Set up variables. Change downtime means the duration when nobody moves after a change in light. Waiting cost function means that waiting 10 turns sucks more than 10x more than waiting 1 turn.
;; **

;; @@
(let [a 0.5]
  (def sim-params {:runs 1000, :turns 250, :traffic-rate-a a, :traffic-rate-b (- 1 a), :downtime 0}))

(def simulation-runs(sim-params :runs))
(def simulation_duration (sim-params :turns))
(def traffic_rate_a (sim-params :traffic-rate-a))
(def traffic_rate_b (sim-params :traffic-rate-b))

(def change_downtime (sim-params :downtime))

(defn waiting_cost [t] (+ (* 10 t) (* 1 t t) ) )

;; @@

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

;; @@
(def generate_traffic_query (doquery :importance generate_traffic [(sim-params :turns)]))
(def generate_traffic_sample (first generate_traffic_query))
(def generated_traffic (:result generate_traffic_sample))

;; @@

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

;; @@

(def query_def (doquery :lmh simulate_crossing [simulation_duration (:traffic_a generated_traffic) (:traffic_b generated_traffic)]))
(def query_sample (take-nth 10 (take simulation-runs (drop 1000 query_def))))
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

;; @@
(let [get-cycles (fn [cycle-key] (map (fn [c] (vals (select-keys c [cycle-key :cost]))) top_cycles))
      plot-size 1200
      opacity 0.25
      x-range [0 10]]
  (println sim-params)
  (doseq [c (take 5 top_cycles)]
    (println c))
  (plot/compose
    (plot/list-plot (get-cycles :cycle_a) :plot-size plot-size :opacity opacity :plot-range [x-range :all] :colour "red")
  	(plot/list-plot (get-cycles :cycle_b) :plot-size plot-size :opacity opacity :plot-range [x-range :all] :colour "blue")))
;; @@

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
;;; {:queue_a (407 408 411 414 415 416 417), :cost_times (53 56 8 8 8 8 8 8 46 47 3 3 3 3 3 6 38 39 1 3 3 30 30 2 2 2 4 21 22 3 3 3 5 15 17 1 3 9 9 1 1 4 4 5 3), :b_left 0, :new_a false, :new_b false, :queue_b (410 411 412 413 414 415 416 420 421 422 424 425 426 427 428 429 430 432 433 436 437 439 442 445 446 448 450 451 453 455 456 457 458), :a_left 6, :turns_left 406, :pass 2}
;;; {:queue_a (407 408 411 414 415 416 417), :cost_times (53 56 8 8 8 8 8 8 46 47 3 3 3 3 3 6 38 39 1 3 3 30 30 2 2 2 4 21 22 3 3 3 5 15 17 1 3 9 9 1 1 4 4 5 3), :b_left 0, :new_a false, :new_b false, :queue_b (410 411 412 413 414 415 416 420 421 422 424 425 426 427 428 429 430 432 433 436 437 439 442 445 446 448 450 451 453 455 456 457 458), :a_left 6, :turns_left 405, :pass 1}
;;; {:queue_a (407 408 411 414 415 416 417), :cost_times (53 56 8 8 8 8 8 8 46 47 3 3 3 3 3 6 38 39 1 3 3 30 30 2 2 2 4 21 22 3 3 3 5 15 17 1 3 9 9 1 1 4 4 5 3), :b_left 0, :new_a true, :new_b true, :queue_b (410 411 412 413 414 415 416 420 421 422 424 425 426 427 428 429 430 432 433 436 437 439 442 445 446 448 450 451 453 455 456 457 458), :a_left 6, :turns_left 404, :pass 0}
;;; {:queue_a (404 407 408 411 414 415 416), :cost_times (13 53 56 8 8 8 8 8 8 46 47 3 3 3 3 3 6 38 39 1 3 3 30 30 2 2 2 4 21 22 3 3 3 5 15 17 1 3 9 9 1 1 4 4 5 3), :b_left 0, :new_a true, :new_b true, :queue_b (404 410 411 412 413 414 415 416 420 421 422 424 425 426 427 428 429 430 432 433 436 437 439 442 445 446 448 450 451 453 455 456 457 458), :a_left 5, :turns_left 403, :pass 0}
;;; {:queue_a (403 404 407 408 411 414 415), :cost_times (13 13 53 56 8 8 8 8 8 8 46 47 3 3 3 3 3 6 38 39 1 3 3 30 30 2 2 2 4 21 22 3 3 3 5 15 17 1 3 9 9 1 1 4 4 5 3), :b_left 0, :new_a false, :new_b false, :queue_b (403 404 410 411 412 413 414 415 416 420 421 422 424 425 426 427 428 429 430 432 433 436 437 439 442 445 446 448 450 451 453 455 456 457 458), :a_left 4, :turns_left 402, :pass 0}
;;; {:queue_a (403 404 407 408 411 414), :cost_times (13 13 13 53 56 8 8 8 8 8 8 46 47 3 3 3 3 3 6 38 39 1 3 3 30 30 2 2 2 4 21 22 3 3 3 5 15 17 1 3 9 9 1 1 4 4 5 3), :b_left 0, :new_a true, :new_b true, :queue_b (402 403 404 410 411 412 413 414 415 416 420 421 422 424 425 426 427 428 429 430 432 433 436 437 439 442 445 446 448 450 451 453 455 456 457 458), :a_left 3, :turns_left 401, :pass 0}
;;; {:queue_a (401 403 404 407 408 411), :cost_times (13 13 13 13 53 56 8 8 8 8 8 8 46 47 3 3 3 3 3 6 38 39 1 3 3 30 30 2 2 2 4 21 22 3 3 3 5 15 17 1 3 9 9 1 1 4 4 5 3), :b_left 0, :new_a true, :new_b true, :queue_b (401 402 403 404 410 411 412 413 414 415 416 420 421 422 424 425 426 427 428 429 430 432 433 436 437 439 442 445 446 448 450 451 453 455 456 457 458), :a_left 2, :turns_left 400, :pass 0}
;;; {:queue_a (400 401 403 404 407 408), :cost_times (11 13 13 13 13 53 56 8 8 8 8 8 8 46 47 3 3 3 3 3 6 38 39 1 3 3 30 30 2 2 2 4 21 22 3 3 3 5 15 17 1 3 9 9 1 1 4 4 5 3), :b_left 0, :new_a false, :new_b false, :queue_b (400 401 402 403 404 410 411 412 413 414 415 416 420 421 422 424 425 426 427 428 429 430 432 433 436 437 439 442 445 446 448 450 451 453 455 456 457 458), :a_left 1, :turns_left 399, :pass 0}
;;; {:queue_a (400 401 403 404 407), :cost_times (9 11 13 13 13 13 53 56 8 8 8 8 8 8 46 47 3 3 3 3 3 6 38 39 1 3 3 30 30 2 2 2 4 21 22 3 3 3 5 15 17 1 3 9 9 1 1 4 4 5 3), :b_left 2, :new_a true, :new_b true, :queue_b (400 401 402 403 404 410 411 412 413 414 415 416 420 421 422 424 425 426 427 428 429 430 432 433 436 437 439 442 445 446 448 450 451 453 455 456 457 458), :a_left 0, :turns_left 398, :pass 2}
;;; 
;; <-
