(defun gen-colorings (states colors)
  (let ((MAXNUM (- (length colors) 1)) (colorings nil) (coloring nil) (NUMStates (length states)))
    (dotimes (i NUMStates)
      (push 0 coloring))
    (do ((i 0) (len NUMStates) (newl 0))
	((null coloring) colorings)
      (cond ((eql (length coloring) NUMStates)
	     (setf newl (mapcar #'(lambda (x) (nth x colors)) coloring))
	     ;; (setf newl (copy-list coloring))
	     (setf colorings (cons newl colorings)))
	    (t nil))
      (setf i (pop coloring))
      (decf len)
      (cond ((< i MAXNUM) 
	     (push (+ i 1) coloring)
	     (incf len)
	     (dotimes (j (- NUMStates len))
	       (push 0 coloring))
	     (setf len NUMStates))
	    (t nil)))))

(defun alt-colorings (states colors)
  (let ((MAXNUM (length colors))
	(colorings '(nil))
	(NUMStates (length states))
	(coloring nil)
	(tmp nil))
    (dotimes (i NUMStates)
       (setf tmp nil)
       (dolist (elt colorings)
	 (dotimes (j MAXNUM)
	   (setf coloring (copy-list elt))
	   (push (nth j colors) coloring)
	   (push coloring tmp)))
       (setf colorings (copy-list tmp)))
    colorings))

; returns nil if any of the assoc lists intersect
; map is the original assoc list, cmap is colored map
; if T is returned this is a legal coloring
(defun compare-maps (map cmap)
    (let ((newl (mapcar #'(lambda (x y) (intersection (second x) (second y))) map cmap)))
      (every #'null newl)))

(defun legal-colorings (map colorings)
  (let ((states (mapcar #'first map))
	(lcolorings nil)
	(mapc nil))
    (dolist (elt colorings)
      (setf mapc (create-map states elt))
      (if (compare-maps map mapc)
	  (setf lcolorings (cons elt lcolorings))))
    lcolorings))


; create map for given coloring - neighbors colored with the same color
(defun create-map (states coloring)
  (do ((map nil) (stat states (rest stat)) (col coloring (rest col)) (neigh nil nil))
      ((null stat) (reverse map))
    ;(format t "~A ~A ~%" stat map) 
    (do ((rstat (rest stat) (rest rstat)) (rcol (rest col) (rest rcol)))
	((null rstat) neigh)
      (if (eq (first col) (first rcol))
	  (setf neigh (cons (first rstat) neigh))))
    (setf map (cons (list (first stat) neigh) map))))

(print "colorings.lisp")