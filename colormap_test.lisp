(load "colormap.lisp")
(load "data.lisp")

;;(setf aust_full_domain (mapcar (lambda (x)
;;				 (list (first x) colors))
;;			       australia))
;;(update-domains australia '(SA R) aust_full_domain)

(untrace gen-colorings-aux)
(gen-colorings australia colors)
(print australia)

(gen-colorings map_lst colors)
(print map_lst)
