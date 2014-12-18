;;Yifeng Gao
;;CS 580 Fall 2014
;;Dr. Duric
;;Project

;; cutset.lisp
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

