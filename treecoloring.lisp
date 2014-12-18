;;Xiaosheng Li
;;CS 580 Fall 2014
;;Dr. Duric
;;Project

;; treecoloring.lisp
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
