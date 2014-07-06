(defpackage :de-raulin-rosario-graph
  (:use :cl)
  (:export :graph
	   :node-number
	   :edges
	   :names
	   :name-map
	   :add-edge
	   :symbolic-graph
	   :graph->dot))

(in-package :de-raulin-rosario-graph)

(defclass graph ()
  ((node-number :initarg :node-number
		:reader node-number)
   (edges :accessor edges)))

(defmethod initialize-instance :after ((graph graph) &rest initargs)
  (declare (ignore initargs))
  (setf (edges graph) (make-array (node-number graph) :initial-element nil)))

(defclass symbolic-graph (graph)
  ((names :initarg :names
	  :reader names)
   (name-map :initform (make-hash-table)
	     :reader name-map))))

(defmethod initialize-instance :after ((graph symbolic-graph) &rest initargs)
  (declare (ignore initargs))
  (let ((number -1))
    (mapc
     (lambda (name) (setf (gethash name (name-map graph)) (incf number)))
     (names graph))))

(defmethod add-edge ((graph graph) from to)
  (push (cons from to) (aref (edges graph) from)))

(defmethod add-edge ((graph symbolic-graph) from to)
  (with-accessors ((edges edges) (name-map name-map)) graph
    (let ((from-node (gethash from name-map))
	  (to-node (gethash to name-map)))
      (push (cons from-node to-node) (aref edges from-node)))))

(defmethod get-edges ((graph graph) of)
  (aref (edges graph) of))

(defmethod get-edges ((graph symbolic-graph) (of symbol))
  (aref (edges graph) (gethash of (name-map graph))))

(defun edge-name->dot (edge-name)
  (substitute-if #\_ (complement #'alphanumericp) (prin1-to-string edge-name)))

(defmethod node-list ((graph graph))
  (loop for i from 0 below (node-number graph)
     collect i))

(defmethod node-list ((graph symbolic-graph))
  (names graph))

(defmethod node->dot ((graph graph) node)
  node)

(defmethod node->dot ((graph symbolic-graph) node)
  (nth node (names graph)))

(defmethod graph->dot ((graph graph))
  (with-output-to-string (*standard-output*)
    (princ "digraph {")
    (mapc (lambda (from-symbol)
	    (mapc (lambda (edge)
		    (let ((to-symbol (node->dot graph (cdr edge))))
		      (fresh-line)
		      (princ (edge-name->dot from-symbol))
		      (princ " -> ")
		      (princ (edge-name->dot to-symbol))
		      (princ ";")))
		  (get-edges graph from-symbol)))
	  (node-list graph))
    (fresh-line)
    (princ "}")))

(defmethod get-cc ((graph graph))
  "makes a dfs to find the connected components of the graph"
  (let ((marked (make-array (node-number graph) :initial-element nil)))
    (labels ((dfs (node cc)
	       (unless (aref marked node)
		 (setf (aref marked node) cc)
		 (mapc (lambda (edge) (dfs (cdr edge) cc)) (get-edges graph node)))))
      (dotimes (node (node-number graph) marked)
	(unless (aref marked node)
	  (dfs node node))))))

(defmethod transpose ((graph graph) &key instance)
  "constructs the transposed graph"
  (let ((transposed
	 (or instance
	     (make-instance 'graph :node-number (node-number graph)))))
    (dotimes (node (node-number graph) transposed)
      (mapc
       (lambda (edge) (add-edge transposed (cdr edge) node))
       (get-edges graph node)))))
