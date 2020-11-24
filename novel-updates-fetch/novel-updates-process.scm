(module (novel-updates process) *
  (import (chicken base)
          scheme
          (novel-updates uri)
          http-client
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


    ;;;;
    ;;;; Structures used for processing:
    ;;;;
    ;;;; Two types of "links"
    ;;;; - index links which point to a page with lsits of links for chapters
    ;;;; - external chapter links which point to the website with a chapter
    ;;;;
    ;;;; The 'link' attributes are instances of transformable-uri-t

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


    ;;;;
    ;;;; Index links

    ;; Given an index-link-t and two mailboxes,
    ;; retrieve the page for link and extract resulting links.
    ;; The extracted links are placed into the respective mailbox
    ;; (one for index links, another for external chapter links)
    ;;
    ;; This procedure is extremely integrated with the structure of
    ;; novel-updates.com website :)
    ;;
    ;; returns #t if a next link/page is found otherwise #f
    (define (produce-index-link idx-link index-mbox external-mbox)
      (let* ((parser
              (make-html-parser
               'start:
               (lambda (tag attrs seed virtual?)
                 (let ((target (assoc 'class attrs)))
                   (if (and target
                            (pair? target)
			    (list? target)
			    (= (length target) 2)
                            (string? (second target)))
                       (if (string-contains (second target)
                                            "chp-release")
                           (begin
			     ;; (display (format "found chapter: target=~a ~a"
			     ;; 		      target
			     ;; 		      attrs))
			     ;; (newline)
			     (append
                              (list (external-chapter-link
                                     (assoc 'name attrs)
                                     (uri-ensure-absolute-path
                                      (parse-uri-string
                                       (second (assoc 'href attrs)))
                                      (index-link-link idx-link))))
                              seed))
                           (if (string-contains (second target)
                                                "next_page")
                               (begin
                                 ;; (display (format
                                 ;;           "next-page parsed out: ~a" attrs))
                                 ;; (newline)
                                 (append
                                  (list (index-link
                                         (assoc 'name attrs)
                                         (uri-ensure-absolute-path
                                          (parse-uri-string
                                           (second (assoc 'href attrs)))
                                          (index-link-link idx-link))))
                                  seed))
                               seed ))
		       seed))))))
	     
        (with-input-from-request
         (uri-to-string (index-link-link idx-link))
         #f
         (lambda ()
           (display
            (format "request finished: ~a"
                    (uri-to-string (index-link-link idx-link))))
           (newline)
           (let ((links (parser '()))
                 (found-next #f))
              (display (format "parsed links: #~a"
                              (length links)))
             (newline)
             (for-each
              (lambda (link)
                (if (index-link? link)
                    (begin
                      (mailbox-send! index-mbox link)
                      (set! found-next #t)
                      (display "found next")
                      (newline))
                    (if (external-chapter-link? link)
                        (mailbox-send! external-mbox link)
                        (display " !! "))))
              links)
             found-next)))))
    
    )


