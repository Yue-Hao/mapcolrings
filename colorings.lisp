;;Yue Hao, Yifeng Gao, Xiaosheng Li
;;CS 580 Fall 2014
;;Dr. Duric
;;Project

;; coloring a map using the following step in THREE MODULES
;; where each module is written by one of our team members
;; 1. find a loop cutset S from map (Module 1)
;; 2. color S using forward checking algorithm (Module 3)
;; 3. color the remaining part of the map which is a forrest (Module 2)
;; 4. combie the result to get the colorings of whole the map (Module 3)

;;The following module is mainly written by Yifeng Gao
;; Module 1: cutset
;; this module includes functions for finds the loop cutset
;; using greedy algorithm by A. Becker et. al.

(defun is-tree (map)
  "predicate if a map is a tree (or forrest)"
  (let ((visited ()) (parent nil) (nodes ()))
    (setf nodes (cons (list (first (first map)) nil) nodes))
    (do ((mk t) (node nil)) ((or (null mk) (null nodes)) mk)
      (setf node (first nodes))
      (setf nodes (rest nodes))
      (setf parent (second node))
      (setf nodes (append
		   (mapcar #'(lambda (x) (list x (first node)))
			   (remove parent (second (assoc (first node) map)))) nodes))
      (setf nodes (remove-if #'(lambda (x) (if (null (first x)) t)) nodes))
      (if (find (first node) visited) (setf mk nil))
      (setf visited (cons (first node) visited)))))

(defun remove-V-from-G (G V)
  "remove vertices V from graph G"
  (let ((nG '()))
    (dolist (x G nG)
      (if (not (member (first x) V))
	  (setf nG
		(append nG
			(list (list (first x)
				    (remove nil
					    (mapcar (lambda (y)
						      (if (member y V)
							  nil
							  y))
						    (second x)))))))))))

(defun remove-v-deg01 (G)
  "remove vertices with degree less or equal than 1 form graph G"
  (let ((V nil)
	(G_result G))
    (loop
       (when (null (setf V
			 (remove nil
				 (mapcar (lambda (x)
					   (if (< (list-length (second x)) 2)
					       (first x)
					       nil))
					 G_result))))
	 (return G_result))
       (setf G_result (remove-V-from-G G_result V)))))


(defun get-minimum-v (G)
  "find the vertex with the maximum deg(v)"
  (let ((min_v (first G)))
    (dolist (x G min_v)
      (if (>
	   (list-length (second x))
	   (list-length (second min_v)))
	  (setf min_v x)))))

(defun greedy-algorithm (G)
  "greedy algorithm to find the loop cutset of graph G
will return a list of vertices in the cutset"
  (let ((F '())
	(min_v nil))
    (loop
       (when (null (setf G (remove-v-deg01 G)))
	 (return F))
       ;; find the min_v vertix with minimal 1/deg
       (setf min_v (first (get-minimum-v G)))
       (setf F (append F (list min_v)))
       ;; remove min_v from graph of G
       (setf G (remove-V-from-G G (list min_v))))))

(defun build-subgraph-V-in-G (G V)
  "build a subgraph with subset of vertices from G
V: subset of vertices from G"
  (remove-V-from-G
   G
   (set-difference (mapcar #'first G) V)))

;; End cutset module



;;The following module is mainly written by Xiaosheng Li
;; Module 2: treecoloring
;; this module includes funciton to color a tree(forrest) by firstly do a
;; topological sort of the tree, secondly color the tree very efficently

(defun check-legal-assign (states coloring)
  "check if one color assignment is leagal using recursive call
this function is only for testing"
  (let ((assigned_states (intersection states coloring :key #'first)))
    (dolist (x assigned_states coloring)
      (if (every (lambda (y)
		   (if (not (eq (second (assoc (first x) coloring))
				(second (assoc y coloring))))
		       T
		       nil))
		 (second x))
	  T
	  (return (setf coloring nil))))))

(defun gen-tree-domains (cs_coloring t_sorted states colors)
  "generate the domians of the remaining trees, which essentially remove all
inconsistency introduce by the cutset colorings.
cs_coloring: cutset colorings
t_sorted: topologically sorted trees
states, colors: assoc map and list of colors"
  ;; generate the full domains for the trees
  (let ((full_domains (mapcar (lambda (x)
				(list (first x) colors))
			      t_sorted)))
    (mapcar (lambda (x)
	      ;; bad_colors is the colors of vertices in the cutset that
	      ;; adjcent to x in the trees
	      (let ((bad_colors (mapcar (lambda (y)
					  (second (assoc y cs_coloring)))
					(second (assoc (first x) states)))))
		;; remove bad_colors from the full domain
		(list (first x) (set-difference (second x) bad_colors))))
	    full_domains)))

(defun gen-tree-colorings-aux (t_coloring t_sorted domains c_coloring states)
  "coloring the trees using recursive calls, since the tree is topologically
sorted, we only need to make sure the color of any nodes is different from
its parent
t_coloring: colorings of the trees, eg ((A G) (B R)...)
domains: the remaining domains of the trees
c_coloring: the cutset colorings"
  (let ((n (first t_sorted)))
    (if (null t_sorted)
	(print (append c_coloring t_coloring))
	;; check if the coloring is legal which is not nessary, because the
	;; algorithm always generate valid colorings
	;;(if (not (check-legal-assign states (append c_coloring t_coloring)))
	;;    (print "error!"))
	;; find the parent_color of n, them remove it from the domain of n
	(let ((parent_color (second (assoc (second n) t_coloring))))
	  (dolist (x (remove parent_color (second (assoc (first n) domains))))
	    (gen-tree-colorings-aux (append t_coloring (list (list (first n) x)))
				    (rest t_sorted)
				    domains
				    c_coloring
				    states))))))


(defun gen-tree-colorings (t_sorted c_coloring states colors)
  "T the graph of the remaining tree(forrest), should be topologically sorted!
t_sorted: the sorted trees, eg. ((A A's parent) (B B's parent) ...)
c_coloring: the cutset coloring"
  (let ((domains (gen-tree-domains c_coloring t_sorted states colors)))
    (if (not (some #'null (mapcar #'second domains)))
	(gen-tree-colorings-aux '()
				t_sorted
				domains
				c_coloring
				states))))

(defun del (x map)
  "helper function to delete a element from map"
  (mapcar #'(lambda (z)
	      (cons (first z) (list (mapcan #'(lambda (a)
						(if (eql a x) (setf a nil) (list a))) (second z))))) map))

(defun del-element (el map)
  (del el (remove-if #'(lambda (x) (eql (first x) el)) map)))

(defun topological-sort (tree)
  "order the nodes from root to leaf.Take the first element of the first node in tree
as the root node, set its parent nil, put the combination in to the new order tree as the node.
Delete the node from the original tree.
find all it children, put them into a list, put them in the order tree with the parent and find
their children, putting them in the children list. Repeat the process until not child is found.
If the original is not null,chose another node and repeat the same process until the rree is null"
  (do ((newtree nil) (children (list (list (first (first tree)) nil))) (newchildren nil nil))
      ((null tree) newtree)
    (setf newtree (append newtree children))
    (dolist (x children)
      (setf newchildren
	    (append newchildren
		    (mapcar #'
		     (lambda (y) (list y (first x))) (second (assoc (first x) tree))))))
    (dolist (z children)
      (setf tree (del-element (first z) tree)))
    (if (null newchildren) (setf children (list (list (first (first tree)) nil)))
	(setf children (remove-if #'(lambda (x) (null (first x))) newchildren)))))

;; End treecoloring module


;;The following module is mainly written by Yue Hao
;; Module 3: map colorings
;; This module will color the cutset and then join the results with the
;; tree colorings

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
