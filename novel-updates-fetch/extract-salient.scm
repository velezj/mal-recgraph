;;;;
;;;; Library definition for extract-salient
(module (novel-updates extract-salient) *
  (import (chicken base)
	  scheme
	  html-parser
	  (srfi 1)
	  (srfi 28))


  ;;;;
  ;;;; Extract HTML paths from HTML "tree"

  ;;
  ;; An node in an html tree
  (define-record-type html-tree-node-t
    (html-tree-node tag attributes)
    html-tree-node?
    (tag html-tree-node-tag)
    (attributes html-tree-node-attributes))
  (set-record-printer!
   html-tree-node-t
   (lambda (x out)
     (display "Node[" out)
     (display (html-tree-node-tag x) out)
     (display "]" out)))
  

  ;;
  ;; an html tree path, which is basically a list of nodes
  (define-record-type html-tree-path-t
    (html-tree-path nodes)
    html-tree-path?
    (nodes html-tree-path-nodes))
  (set-record-printer!
   html-tree-path-t
   (lambda (x out)
     (display "Path{" out)
     (display (html-tree-path-nodes x) out)
     (display "}" out)))

  ;;
  ;; Generates full list of path for an HTML document.
  ;; The HTML is treated as a tree and all paths are returned as
  ;; a list of lists, where a path is just a list of elements down
  ;; the tree.
  (define (all-paths html-document-port)
    (let* ((%start-tag
	    (lambda (tag attrs seed virtual?)
	      (let ((previous-paths (first seed))
		    (current-path (second seed)))
		(list previous-paths
		      (append current-path
			      (list (html-tree-node tag attrs)))))))
	   (%end-tag (lambda (tag attrs parent-seed seed virtual?)
		       (let ((previous-paths (first seed))
			     (current-path (second seed)))
			 (list
			  (append previous-paths
				  (list (html-tree-path current-path)))
			  (if (null? current-path)
			      '()
			      (drop-right current-path 1))))))
	   (parser (make-html-parser
		   start: %start-tag
		   end: %end-tag)))
      (parser (list '() '() ) html-document-port)))


  ;;
  ;; Utility method to convert from HTML string to paths
  (define (html-string->paths html)
    (let ((port (open-input-string html)))
      (all-paths port)))


  )
