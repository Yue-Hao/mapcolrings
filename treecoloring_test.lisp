(load "cutset_test.lisp")
(load "treecoloring.lisp")

(defvar aus_c_coloring)
(setf aus_c_coloring '((SA R)))

(gen-tree-domains aus_cs_coloring aus_tree australia colors)


(defvar aus_tree_sorted)
(setf aus_tree_sorted (topological-sort aus_tree))

(trace gen-tree-colorings-aux)
(untrace gen-tree-colorings-aux)
(gen-tree-colorings aus_tree_sorted
		    aus_c_coloring
		    australia
		    colors)
