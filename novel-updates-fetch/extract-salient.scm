;;;;
;;;; Library definition for extract-salient
(module (novel-updates extract-salient) *
  (import (chicken base)
	  scheme
	  html-parser
	  (srfi 1)
	  (srfi 13)
	  (srfi 28)	  
	  (srfi 128)
	  (srfi 146)
	  (chicken file))


  ;;;;
  ;;;; Extract HTML paths from HTML "tree"

  ;;
  ;; An node in an html tree
  (define-record-type html-tree-node-t
    (html-tree-node tag attributes text)
    html-tree-node?
    (tag html-tree-node-tag)
    (attributes html-tree-node-attributes)
    (text html-tree-node-text set-html-tree-node-text!))
  
  ;; display nicely for humans
  (set-record-printer!
   html-tree-node-t
   (lambda (x out)
     (display "Node[" out)
     (display (html-tree-node-tag x) out)
     (display "]" out)))

  ;; convert node to simple alist
  (define (html-tree-node->alist n)
    (list (cons 'tag (html-tree-node-tag n))
	  (cons 'attributes (html-tree-node-attributes n))
	  (cons 'text (html-tree-node-text n))))

  ;; comparator interface (for using with mappings)
  (define html-tree-node-comparator
    (make-comparator
     html-tree-node?
     (lambda (a b)
       (and (equal? (html-tree-node-tag a)
		    (html-tree-node-tag b))
	    (equal? (html-tree-node-attributes a)
		    (html-tree-node-attributes b))))
     (lambda (a b)
       (< (default-hash (html-tree-node->alist a))
	  (default-hash (html-tree-node->alist b))))
     (lambda (a)
       (default-hash (html-tree-node->alist a)))))
     
  
  ;;
  ;; an html tree path, which is basically a list of nodes
  (define-record-type html-tree-path-t
    (html-tree-path nodes)
    html-tree-path?
    (nodes html-tree-path-nodes))

  (set-record-printer!
   html-tree-path-t
   (lambda (x out)
     ;; (display "Path{" out)
     ;; (display (html-tree-path-nodes x) out)
     ;; (display "}" out)))
     (for-each (lambda (n)
		 (display "/" out)
		 (display (html-tree-node-tag n) out))
	       (html-tree-path-nodes x))))


  ;; comparator interface (for using with mappings)
  (define html-tree-path-comparator
    (let ((c (make-list-comparator
	     html-tree-node-comparator
	     html-tree-path?
	     null?
	     car
	     cdr)))
      (make-comparator
       html-tree-path?
       (lambda (a b)
	 ((comparator-equality-predicate c)
	  (html-tree-path-nodes a)
	  (html-tree-path-nodes b)))
       (lambda (a b)
	 ((comparator-ordering-predicate c)
	  (html-tree-path-nodes a)
	  (html-tree-path-nodes b)))
       (lambda (a)
	 ((comparator-hash-function c)
	  (html-tree-path-nodes a))))))
	  


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
			      (list (html-tree-node tag attrs "")))))))
	   (%text
	    (lambda (text seed)
	      (let ((previous-paths (first seed))
		    (current-path (second seed)))
		(if (null? current-path)
		    seed
		    (begin
		      (set-html-tree-node-text!
		       (last current-path)
		       (string-append
			(html-tree-node-text
			 (last current-path))
			text))
		      (list previous-paths current-path))))))
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
		    text: %text
		    end: %end-tag)))
      (first (parser (list '() '() ) html-document-port))))
  
  
  ;;
  ;; Utility method to convert from HTML string to paths
  (define (html-string->paths html)
    (let ((port (open-input-string html)))
      (all-paths port)))


  ;;;;
  ;;;; Find salient paths

  ;; compute a count of the times each path in html tree is seen
  (define (%count-equal-paths html-tree-paths path-comparator)
    (fold
     (lambda (path seed)
       (let ((count (+ (mapping-ref/default seed path 0) 1)))
	 (mapping-set seed path count)))
     (mapping path-comparator)
     html-tree-paths))

  
  ;; returns is an element is inside a lisst using given comparator
  (define (member/c? elt lst comparator)
    (let ((found #f))
      (for-each
       (lambda (x)
	 (if ( (comparator-equality-predicate comparator)
	       elt
	       x)
	     (set! found #t)))
       lst)
      found))


  ;; grab paragraphs as salient
  (define (compute-salient-paths html-tree-paths path-comparator)
    (let ((salient-paths
	   (mapping-keys
	    (mapping-filter
	     (lambda (k v)
	       (and (> v 2)
		    (equal?
		     (html-tree-node-tag (last (html-tree-path-nodes k)))
		     'p)))
	     (%count-equal-paths html-tree-paths path-comparator)))))
      (filter (lambda (p) (member/c? p salient-paths path-comparator))
	      html-tree-paths)))


  ;; Returns a concatenated text from list of paths using only the
  ;; last node in path
  (define (compute-text-from-paths paths)
    (string-join
     (map (lambda (p)
	    (if (> (length (html-tree-path-nodes p))
		   0)
		(or (html-tree-node-text (last (html-tree-path-nodes p)))
		    "")
		""))
	  paths)
     (format "~%")))
  



  ;;;;
  ;;;; Work with directories and html files within

  (define (get-chapter-directories base-dir)
    (find-files base-dir
		test: ".*chapters_[^/]*"))

  (define (get-all-chapter-files base-dir)
    (filter (lambda (p)
	      (not (string-suffix? ".txt" p)))
	    (find-files base-dir
			test: ".*chapters_.*/.*")))

  (define (extract-text-for-file filename)
    (let ((paths '()))
      (with-input-from-file filename
	(lambda ()
	  (set! paths (all-paths (current-input-port)))))
      (let* ((outfile (string-append filename ".txt"))
	     (text-paths
	      (compute-salient-paths paths html-tree-path-comparator))
	     (text (compute-text-from-paths text-paths)))
	(with-output-to-file outfile
	  (lambda ()
	    (write text))))))

  (define (extract-text-for-all-chapters base-dir)
    (for-each
     (lambda (fn)
       (display (format "Extracting for '~a' ~%" fn))
       (extract-text-for-file fn))
     (get-all-chapter-files base-dir)))
  
  )

