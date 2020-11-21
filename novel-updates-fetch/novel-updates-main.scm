(module (novel-updates main) *
  (import (chicken base)
	  scheme
	  (novel-updates uri)
	  (novel-updates process)
	  mailbox
	  (srfi 18)
	  (srfi 28)
	  (srfi 27) )

  ;;;;
  ;;;; mailboxes to use forindex links and extenral links
  (define *index-mbox* (make-mailbox))
  (define *external-mbox* (make-mailbox))


  ;;;;
  ;;;; creates and returns a thread that writes external links to a file
  (define (make-external-link-mbox-file-writer-thread filename mbox)
    (letrec* ((handle-next-link
	       (lambda ()
		 (let ((link (mailbox-receive! mbox)))
		   (with-output-to-file filename
		     (lambda ()
		       (write
			(uri-to-string (external-chapter-link-link link)))
		       (newline))
		     #:append))))
	      (loop
	       (lambda ()
		 (handle-next-link)
		 (loop))))
      (make-thread loop)))

  ;;;;
  ;;;; creates and returns a thread the consumed an index-link from
  ;;;; a mailbox and produces resulting index and external links to
  ;;;; the given mailboxes, sleeping an amount of time between
  ;;;; link consumption
  (define (make-process-index-link-thread source-mbox
					  result-index-mbox
					  result-external-mbox
					  timeout-seconds)
    (letrec* ((process-next-link
	       (lambda ()
		 (let ((link (mailbox-receive! source-mbox)))
		   (display (format "Processing index: ~a"
				    (uri-to-string (index-link-link link))))
		   (newline)
		   (produce-index-link
		    link
		    result-index-mbox
		    result-external-mbox))))
	      (loop
	       (lambda ()
		 (let ((seconds (+ 10 (random-integer 30))))
		   (process-next-link)
		   (thread-sleep! seconds)
		   (loop)))))
      (make-thread loop)))


  ;;;;
  ;;;; builds and starts a "simple" system
  (define (start-simple-system output-filename seconds seed-index-url)
    (let ((index-thread (make-process-index-link-thread *index-mbox*
							*index-mbox*
							*external-mbox*
							seconds))
	  (writer-thread (make-external-link-mbox-file-writer-thread
			  output-filename
			  *external-mbox*)))
      (thread-start! writer-thread)
      (thread-start! index-thread)
      (produce-index-link (index-link "seed" (parse-uri-string seed-index-url))
			  *index-mbox*
			  *external-mbox*)
      (list
       index-thread
       writer-thread
       *index-mbox*
       *external-mbox*)))


  
  )
	  
