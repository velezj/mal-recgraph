(module (novel-updates main) *
  (import (chicken base)
	  scheme
	  r7rs
	  (novel-updates uri)
	  (novel-updates process)
	  mailbox
	  (srfi 13)
	  (srfi 18)
	  (srfi 28)
	  (srfi 27)
	  (srfi 111) )

  ;;;;
  ;;;; mailboxes to use forindex links and extenral links
  (define *index-mbox* (make-mailbox))
  (define *external-mbox* (make-mailbox))


  ;;;;
  ;;;; creates and returns a thread that writes external links to a file
  (define (make-external-link-mbox-file-writer-thread filename
						      mbox
						      timeout-seconds
						      done?)
    (letrec* ((handle-next-link
	       (lambda ()
		 (let ((link (mailbox-receive! mbox timeout-seconds 'timeout)))
		   (if (equal? link 'timeout)
		       (begin (display ".")
			      (flush-output-port)
			      'timeout)
		       (with-output-to-file filename
			 (lambda ()
			   (write
			    (uri-to-string (external-chapter-link-link link)))
			   (newline)
			   'wrote)
			 #:append)))))
	      (loop
	       (lambda ()
		 (let ((h (handle-next-link)))
		   (if (done?)
		       (begin
			 (display "index link processer done")
			 (newline)
			 'done)
		       (loop))))))
      (make-thread loop)))

  ;;;;
  ;;;; creates and returns a thread the consumed an index-link from
  ;;;; a mailbox and produces resulting index and external links to
  ;;;; the given mailboxes, sleeping an amount of time between
  ;;;; link consumption
  (define (make-process-index-link-thread source-mbox
					  result-index-mbox
					  result-external-mbox
					  timeout-seconds
					  mbox-timeout-seconds
					  done?
					  done-flag-box)
    (letrec* ((process-next-link
	       (lambda ()
		 (let ((link (mailbox-receive! source-mbox
					       mbox-timeout-seconds
					       'timeout)))
		   (if (equal? link 'timeout)
		       (begin
			 (display "#")
			 (flush-output-port)
			 'timeout)
		       (begin 
			 (display
			  (format
			   "Processing index: ~a"
			   (uri-to-string (index-link-link link))))
			 (newline)
			 (let ((found-next
				(produce-index-link
				 link
				 result-index-mbox
				 result-external-mbox)))
			   (display (format "found next: ~a" found-next))
			   (newline)
			   (if (not found-next)
			       (set-box! done-flag-box #t)))
			 'processed)))))
	      (loop
	       (lambda ()
		 (let* ((min-seconds (* 0.5 timeout-seconds))
			(max-seconds (* 1.5 timeout-seconds))
			(range (- max-seconds min-seconds))
			(seconds (+ min-seconds (random-integer range))))
		   (if (done?)
		       (begin
			 (set-box! done-flag-box #t)
			 (display "writer done")
			 (newline)
			 'done)
		       (begin
			 (process-next-link)
			 (thread-sleep! seconds)
			 (loop)))))))
      (make-thread loop)))


  ;;;;
  ;;;; builds and starts a "simple" system
  (define (start-simple-system output-filename seconds seed-index-url)
    (let* ((done-flag-box (box #f))
	   (done? (lambda () (unbox done-flag-box)))
	   (index-thread (make-process-index-link-thread *index-mbox*
							*index-mbox*
							*external-mbox*
							seconds
							2
							done?
							done-flag-box))
	  (writer-thread (make-external-link-mbox-file-writer-thread
			  output-filename
			  *external-mbox*
			  2
			  done?)))
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
	  
