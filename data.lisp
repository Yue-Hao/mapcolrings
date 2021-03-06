(defvar colors)
(setf colors '(R G B))

(defvar australia)
(setf australia '( (T ()) (SA (WA NT Q NSW V)) (WA (NT SA)) (NT (WA SA Q)) (Q (NT SA NSW)) (NSW (Q V SA)) (V (SA NSW))))

(defvar map_lst)
(setf map_lst '((A (B C E)) (B (A E F)) (C (A E F)) (D (F)) (E (A B C F)) (F (B C D E))))

(defvar *50-states*)
(load "states.cl")

(load "countriesmap.lisp")
