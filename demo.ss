;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Copyright 2016-2080 evilbinary.
;作者:evilbinary on 12/24/16.
;邮箱:rootdebug@163.com
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "./lib/slib/chez.init")
(import (simple-socket socket))

(require 'http)
(require 'cgi)
(require 'array)

(define port 8080)
(define server (make-socket AF_INET SOCK_STREAM port))

(socket:bind server)
(socket:listen server)

(define serve-proc
  (lambda (request-line query-string header)
    (printf "HTTP=>%a\n" request-line)
    (printf "path=%a\n" (cadr request-line))
    (string-append
		 	(http:content
		  	'(("Content-Type" . "text/html"))
		  	(html:head "hello" "")
		  	"<div>test</div>"))))

(let loop ([client (socket:accept server]))
  (when (>= (car client) 0)
	(let ([iport (make-fd-input-port client)]
		  [oport (make-fd-output-port client)])
		(http:serve-query serve-proc iport oport)
		(close-port iport)
		(close-port oport)
		(socket:shutdown client)
		(socket:close client))
	(loop (socket:accept server))
  )
)

(socket:shutdown server)
(socket:close server)
(socket:cleanup)