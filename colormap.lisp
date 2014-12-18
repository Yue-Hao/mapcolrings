;;Yue Hao
;;CS 580 Fall 2014
;;Dr. Duric
;;Project

;; coloring a map using the following step
;; 1. find a loop cutset S from map
;; 2. color S using forward checking algorithm
;; 3. color the remaining part of the map which is a forrest
;; 4. combie the result to get the colorings of whole the map

;; the algorithm for finding the cutset in included in cutset.lisp
(load "cutset.lisp")
;; the algorithm for coloring the remaining forrest is included in treecoloring.lisp
(load "treecoloring.lisp")

(defun update-domains (states color_pair domains)
  "update the color domains for states after one state's color is assigned
   will remove all inconsistency in the domains caused by the new assignment
   color_pair: one assignment for a state, eg. (A Green)"
  (mapcar (lambda (x)
	    (list (first x)
		        ;; condition 1: set the domain of the state in color_pair
		  (cond ((eq (first color_pair) (first x))
			 (list (second color_pair)))
			;; condition 2: remove inconsistency of adjcent states
			((member (first color_pair) (second (assoc (first x) states)))
			 (remove (second color_pair) (second x)))
			;; default: no inconsistency, keep the orignial domain
			(t (second x)))))
	  domains))

(defun gen-colorings-aux (domains c_states states colors t_sorted)
  "generate all possible color assignments for c_states using forward checking.
  domains: color domains of c_states ((A (R G)) (B (G))...);
  c_states: assoc list of the cutset;
  states: assoc list of the map;
  colors: list of colors to be assigned;
  c_states_cp: a backup copy of the original states;
  t_sorted: topologically sorted remaining trees"
  (let ((s (first c_states)))
    (if (null c_states)
	;;basic condition: no more countries in 'c_states' to be assigned
	(let ((c_coloring (mapcar (lambda (x)
				    (list (first x) (first (second x))))
				  domains)))
	  ;; assign the remaining part of the map based on the coloring of cutset
	  (gen-tree-colorings t_sorted c_coloring states colors))
	(dolist (x (second (assoc (first s) domains)))
	  ;; color the current state s and update domain
	  (let ((new_domains (update-domains
			      states
			      (list (first s) x)
			      domains)))
	    ;; recursive call to color the next state
	    (if (not (some #'null (mapcar #'second new_domains)))
		(gen-colorings-aux
		 new_domains
		 (rest c_states)
		 states
		 colors
		 t_sorted)))))))

(defun gen-colorings (states colors)
  "initial call of 'gen-color-assign'
   states: the assoc list of the map, eg. ((A (B C)) (B (A))...)
   colors: list of possible colors, eg. (R G B)"
       ;;c_vertices: vertices in cutset
  (let ((c_vertices (greedy-algorithm states)))
          ;; c_states: subgraph of the cutset
    (let ((c_states (build-subgraph-V-in-G states c_vertices))
	  ;;t_states: subgraph of the remaining map
	  (t_states (remove-V-from-G states c_vertices)))
      (gen-colorings-aux
       ;; create the initial full domains for all states
       (mapcar (lambda (x) (list (first x) colors)) c_states)
       ;; sort the cutset so we can color the vertex with larger degrees first
       (stable-sort c_states #'> :key #'(lambda (x) (list-length (second x))))
       states
       colors
       ;; sort the remaining graph, which is a forrest
       (topological-sort t_states)))))
