(import srfi-14
	srfi-13
	srfi-1
	(chicken irregex))

;;
;; THE rfc3986 regex
;;
;; ^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\?([^#]*))?(#(.*))?
;;     12            3  4          5       6  7        8 9
;;
;; The numbers in the second line above are only to assist readability;
;; they indicate the reference points for each subexpression (i.e., each
;; paired parenthesis).  We refer to the value matched for subexpression
;; <n> as $<n>.  For example, matching the above expression to
;;
;;    http://www.ics.uci.edu/pub/ietf/uri/#Related
;;
;; results in the following subexpression matches:
;;
;;    $1 = http:
;;    $2 = http
;;    $3 = //www.ics.uci.edu
;;    $4 = www.ics.uci.edu
;;    $5 = /pub/ietf/uri/
;;    $6 = <undefined>
;;    $7 = <undefined>
;;    $8 = #Related
;;    $9 = Related
;;
;; where <undefined> indicates that the component is not present, as is
;; the case for the query component in the above example.  Therefore, we
;; can determine the value of the five components as
;;
;;    scheme    = $2
;;    authority = $4
;;    path      = $5
;;    query     = $7
;;    fragment  = $9



;;;;
;;;; The records needed for a transformable and usable URI
;;;; (so not just a string representation :) )

;; a path is just a lsit of elements without the '/' in the elements.
;; along with some metatdata of relative/absolute
(define-record-type uri-path-t
  (make-uri-path elements relative?)
  uri-path?
  (elements uri-path-elements)
  (relative? uri-path-relative?))

;; The query parameters are an alist so that we can compute on the
;; parameters and values and transform them
(define-record-type uri-query-parameters-t
  (uri-query-parameters param-alist)
  uri-query-parameters?
  (param-alist uri-query-parameters-alist))

;; a tranformable uri is just one which is not a string representation
;; but has some structure parsed out (especially path and query parmeters)
(define-record-type transformable-uri-t
  (uri-t scheme
	 authority
	 path
	 query
	 fragment)
  uri-t?
  (scheme uri-scheme)
  (authority uri-authority)
  (path uri-path)
  (query uri-query)
  (fragment uri-fragment))



;;;;
;;;; Parsing query string into parameter alist
;;;; and into a resuling uri-query-parameters-t

;; charsets for string-tokenize
(define char-set:amperstand
  (char-set-adjoin char-set:empty #\&))
(define char-set:not-amperstand
  (char-set-complement char-set:amperstand))
(define char-set:equal
  (char-set-adjoin char-set:empty #\=))
(define char-set:not-equal
  (char-set-complement char-set:equal))

;; parses out a query string into query parameters structure (alist)
(define (%parse-query-string-to-alist query-string)
  (let ((amperstand_tokens
	 (string-tokenize query-string char-set:not-amperstand)))
    (map
     (lambda (expr)
       (string-tokenize expr char-set:not-equal))
     amperstand_tokens)))

;; creates a uri-query-parameters-t from a query string (without leading ?)
(define (%parse-query-string-to-parameters query-string)
  (uri-query-parameters (%parse-query-string-to-alist query-string)))



;;;;
;;;; Parsing of uri path into uri-path-t

;; charsets for string-tokenize
(define char-set:slash
  (char-set-adjoin char-set:empty #\/))
(define char-set:not-slash
  (char-set-complement char-set:slash))

;; tokenize the path string into a list of elements
(define (%parse-path-string-to-path-list path-string)
  (string-tokenize (string-trim path-string) char-set:not-slash))

;; is this uri path relative or not
(define (%path-string-is-relative? path-string)
  (not (string-prefix? "/" (string-trim path-string))))

;; parse a path string into a uri-path-t
(define (%path-string-to-path path-string)
  (make-uri-path (%parse-path-string-to-path-list path-string)
		 (%path-string-is-relative? path-string)))



;;;;
;;;; Parsing uri string into 5 major string components

;; the regex
(define *rfc3986-posix-regex-string*
  "^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\\?([^#]*))?(#(.*))?")
(define *rfc3986-regex*
  (string->irregex *rfc3986-posix-regex-string*
		   '( backtrack utf8 case-insensitive)))


;; parse from uri string into basic (string) components
(define (%parse-uri-string-to-components uri-string)
  (let ((matches (irregex-search *rfc3986-regex* (string-trim uri-string))))
    (if (null? matches)
	#f
	(list
	 (cons 'scheme (irregex-match-substring matches 2))
	 (cons 'authority (irregex-match-substring matches 4))
	 (cons 'path (irregex-match-substring matches 5))
	 (cons 'query (irregex-match-substring matches 7))
	 (cons 'fragment (irregex-match-substring matches 9))))))

;; utility to get alist value only or #f
(define (assoc-value k alist)
  (if (null? alist)
      #f
      (let ((kv (assoc k alist)))
	(if (null? kv)
	    #f
	    (if (not (pair? kv))
		#f
		(if (null? (cdr kv))
		    #f
		    (cdr kv)))))))
	  

;; ok, actually gor from uri string to transformable-uri-t
(define (parse-uri-string uri-string)
  (let ((components (%parse-uri-string-to-components uri-string)))
    (if (null? components)
	#f
	(uri-t (assoc-value 'scheme components)
	       (assoc-value 'authority components)
	       (%path-string-to-path (assoc-value 'path components))
	       (%parse-query-string-to-parameters
		(assoc-value 'query components))
	       (assoc-value 'fragment components)))))

;; create a uri string from a transformable-uri-t
(define (uri-as-string uri)
  #f)


;; easy representation of a transformable-uri-t
(define (transformable-uri->alist uri)
  (list
   (cons 'scheme (uri-scheme uri))
   (cons 'authority (uri-authority uri))
   (cons 'path
	 (list
	  (cons 'relative? (uri-path-relative? (uri-path uri)))
	  (cons 'elements (uri-path-elements (uri-path uri)))))
   (cons 'query (uri-query-parameters-alist (uri-query uri)))
   (cons 'fragment (uri-fragment uri))))

(define *uri-string* "https://www.novelupdates.com/reading-list/?list=1")
	 




;;;;
;;;; Transformation and Computations of transformable-uri-t


;; sets the given query parameters (as alist)
(define (%query-parmeters-set-all param-alist query)
  (let* ((params-without-keys
	  (fold (lambda (kv alist)
		  (alist-delete (car kv) alist))
		(uri-query-parameters-alist query)
		param-alist))
	 (params-key-set
	  (fold (lambda (kv alist)
		  (alist-cons
		   (car kv) (cdr kv)
		   alist))
		params-without-keys
		param-alist)))
    (uri-query-parameters params-key-set)))
  

;; create a new uri with the given query parameters set
(define (uri-ensure-query-parameters uri param-alist)
  (let ((original-params (uri-query uri)))
    (uri-t
     (uri-scheme uri)
     (uri-authority uri)
     (uri-path uri)
     (%query-parmeters-set-all param-alist (uri-query uri))
     (uri-fragment uri))))
