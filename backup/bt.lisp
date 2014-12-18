(defun check-legal-assign (states coloring)
  (let ((assigned_states (intersection states coloring :key #'first)))
  "check if one color assignment is leagal using recursive call"
  (dolist (x assigned_states coloring)
    (if (every (lambda (y)
		 (if (not (eq (second (assoc (first x) coloring))
			      (second (assoc y coloring))))
		     T
		     nil))
	       (second x))
	T
	(return (setf coloring nil))))))

