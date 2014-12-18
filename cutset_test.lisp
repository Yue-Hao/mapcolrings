(load "cutset.lisp")
(load "data.lisp")

;;australia
(defvar aus_cut)
(setf aus_cut (greedy-algorithm australia))

(defvar aus_cutgraph)
(setf aus_cutgraph (build-subgraph-V-in-G australia aus_cut))

(defvar aus_tree)
(setf aus_tree (remove-V-from-G australia aus_cut))

;;united states
(defvar us_cut)
(setf us_cut (greedy-algorithm *50-states*))
(list-length us_cut)

(defvar us_cutgraph)
(setf us_cutgraph (build-subgraph-V-in-G *50-states* us_cut))
(print us_cutgraph)

(defvar us_tree)
(setf us_tree (remove-V-from-G *50-states* us_cut))
(print us_tree)

(is-tree us_tree)

;;world
(defvar world_cut)
(setf world_cut (greedy-algorithm countries-map))
(list-length world_cut)

(defvar world_cutgraph)
(setf world_cutgraph (build-subgraph-V-in-G countries-map world_cut))
(print world_cutgraph)

(defvar world_tree)
(setf world_tree (remove-V-from-G countries-map world_cut))
(print world_tree)

(is-tree world_tree)
