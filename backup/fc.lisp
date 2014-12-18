;;Yue Hao
;;CS 580 Fall 2014
;;Dr. Duric
;;Project

(load "cutset.lisp")

(defun update-domains (states color_pair domains)
  (mapcar (lambda (x)
	    (list (first x)
		  (cond ((eq (first color_pair) (first x))
			 (list (second color_pair)))
			((member (first color_pair) (second (assoc (first x) states)))
			  (remove (second color_pair) (second x)))
			(t (second x)))))
	  domains))

(defun gen-colorings-aux (domains states colors states_cp)
  "generate all possible color assignments using forward checking
  domain: one possbile assignment, ((A 1) (B 2)...), where different colors are denoted by 1,2,3...
  states: assoc list of the map
  colors: list of colors to be assigned
  states_cp: a backup copy of the original states"
  (let ((s (first states)))
    (if (null states)
	;;basic condition: no more countries in 'map_lst' to be assigned
	(let ((cutset_coloring (mapcar (lambda (x)
					 (list (first x) (first (second x))))
				       domains)))
	  (print cutset_coloring))
	(dolist (x (second (assoc (first s) domains)))
	  (let ((new_domains (update-domains
			      states_cp
			      (list (first s) x)
			      domains)))
	    (if (not (some #'null (mapcar #'second new_domains)))
		(gen-colorings-aux
		 new_domains
		 (rest states)
		 colors
		 states_cp)))))))

(defun gen-colorings (states colors)
  "initial call of 'gen-color-assign'"
  (let ((full_domains (mapcar (lambda (x)
				(list (first x) colors))
			      states))
	(sorted_states (stable-sort states #'> :key #'(lambda (x)
						 (list-length (second x))))))
    (gen-colorings-aux
     full_domains
     sorted_states
     colors
     sorted_states)))

(defun remove-inconsistent (domain tree)
	(dolist (node (reverse tree) domain)
		(dolist (col-p (second (assoc (second node) domain)))
			(if (equal (list col-p) (second (assoc (first node) domain))) 
				(setf domain (mapcar #'(lambda (x) 
					(if (eql (first x) (second node)) 
					(cons (second node) (list (remove col-p 
						(second (assoc (second node) domain))))) x)) domain))))))
					
;(remove-inconsistent '((B (R G B)) (C (G)) (D (R)) (F (R G B))) '((B NIL) (F B) (C F) (D F)))
;>((B (R G)) (C (G)) (D (R)) (F (B)))

;function to color a ordered tree given a pre-processed domain
(defun color-t (domain tree)
	(let ((colorings nil))
		(dolist (node tree colorings)
			;(print colorings)
			(setf colorings 
				(append colorings (list (list (first node)
					(color-c node colorings domain))))))))

;Helper function to select a color from the domain					
(defun color-c (node colorings domain)
	(let ((color-p (second (assoc (second node) colorings))) (color-l nil))
		(dolist (x (second (assoc (first node) domain)) color-l)
			(if (not (equal x color-p))
				(return x)))))
		
;(color-t '((B (R G B)) (C (G)) (D (R B)) (F (R B))) '((B NIL) (F B) (C F) (D F)))
;>((B R) (F B) (C G) (D R))


(defvar *50-states*)
(load "states.cl")

(print "colormap.lisp")

