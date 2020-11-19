(import http-client
	html-parser
	json
	srfi-1
	srfi-13
	srfi-9
	srfi-18
	srfi-27
	srfi-28
	mailbox
	simple-timer
	(chicken random)
	(chicken io)
	(chicken process))

(define *a*
  (with-input-from-request
   "https://www.novelupdates.com/series/a-wild-last-boss-appeared/"
   #f
   html->sxml))

(define *parser0*
  (make-html-parser
   'start: (lambda (tag attrs seed virtual?)
	     (let ((target (assoc 'class attrs)))
	       (if (and (pair? target)
			(= (length target) 2)
			(or (string-contains (second target)
					     "chp-release")
			    (string-contains (second target)
					     "next_page")))
		   (append (list attrs) seed)
		   seed )))))



(define *b*
  (with-input-from-request
   "https://www.novelupdates.com/series/a-wild-last-boss-appeared/"
   #f
   (lambda () (*parser0* '() ))))


(define-record-type external-chapter-link-t
  (external-chapter-link name link)
  external-chapter-link?
  (name external-chapter-link-name)
  (link external-chapter-link-link))

(define-record-type index-link-t
  (index-link name link)
  index-link?
  (name index-link-name)
  (link index-link-link))


(define (relative-url-href rel base)
  (string-concatenate
   (list
    base
    (if (string-prefix? "./" rel)
	(string-copy rel 2)
	rel))))

(define (produce-index-link idx-link index-mbox external-mbox)
  (let ((parser
	 (make-html-parser
	  'start:
	  (lambda (tag attrs seed virtual?)
	    (let ((target (assoc 'class attrs)))
	      (if (and (pair? target)
		       (= (length target) 2))
		  (if (string-contains (second target)
				       "chp-release")
		      (append (list (external-chapter-link
				     (assoc 'name attrs)
				     (second (assoc 'href attrs))))
			      seed)
		      (if (string-contains (second target)
					   "next_page")
			  (append (list (index-link
					 (assoc 'name attrs)
					 (relative-url-href
					  (second (assoc 'href attrs))
					  (index-link-link idx-link))))
				  seed)
			  seed ))
		  seed))))))
    (with-input-from-request
     (index-link-link idx-link)
     #f
     (lambda ()
       (let ((links (parser '())))
	 (for-each
	  (lambda (link)
	    (if (index-link? link)
	 	(mailbox-send! index-mbox link)
	 	(if (external-chapter-link? link)
	 	    (mailbox-send! external-mbox link)
	 	    (display "!!"))))
	  links))))))

(define (process-next-link mbox index-mbox external-mbox)
  (let ((link (mailbox-receive! mbox)))
    (if (index-link? link)
	(let ((seconds (+ 10 (random-integer 30))))
	  (display (format "Link: ~a (~a)"
			   (index-link-name link)
			   (index-link-link link)))
	  (newline)
	  (produce-index-link link index-mbox external-mbox)
	  (display (format "Seconds Sleep: ~a" seconds))
	  (newline)
	  (thread-sleep! seconds)
	  (process-next-link mbox index-mbox external-mbox)))))

(define (write-out-external-links mbox filename)
  (let ((link (mailbox-receive! mbox)))
    (if (external-chapter-link? link)
	(begin
	  (with-output-to-file  filename
	    (lambda ()
	      (write (external-chapter-link-link link))
	      (newline))
	    #:append)
	  (write-out-external-links mbox filename)))))


(define *link*
  (index-link
   "seed-link"
   "https://www.novelupdates.com/series/a-wild-last-boss-appeared/" ) )

(define *index-mbox* (make-mailbox))
(define *external-mbox* (make-mailbox))

(define *index-thread*
  (thread-start!
   (lambda ()
     (process-next-link *index-mbox*
			*index-mbox*
			*external-mbox*))))

(define *writer-thread*
  (thread-start!
   (lambda ()
     (write-out-external-links *external-mbox*
			       "external-links.txt"))))

;; (produce-index-link *link* *index-mbox* *external-mbox*)
