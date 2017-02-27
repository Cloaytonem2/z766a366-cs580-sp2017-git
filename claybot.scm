; (load "IRC_bot_test.scm")

(require-extension matchable)
(require-extension regex)	; this has the string-search function
(require-extension irc posix)	; irc library
(require-extension srfi-13)	; this has many string functions, it mainly has downcase
(require-extension srfi-14) ; has char-set standards
#|
; WORDS
(define nouns '())	; list of pairs; first is singular, second is plural
(define adjectives '())
(define verbs '())	; list of lists; first is base word, second is past, third is current
(define adverbs '())
|#
(define (random-from-list ls)
	(list-ref ls (random (length ls))))
	
(define (in-string? chk str)
	(list? (string-search chk str)))

(define (in-list? chk ls)	; used mainly to check a list of strings, but should work for most things
	(if (equal? (car ls) chk)
		#t
		(if (= (length (cdr ls)) 0)
			#f
			(in-list? chk (cdr ls)))))
			
(define (slist->string ls) ; a list of symbols to a string
	(cond
	[(= (length (cdr ls)) 0)
		(symbol->string (car ls))]
	[else
		(string-append (string-append (symbol->string (car ls)) " ") (slist->string (cdr ls)))]))
		
	
(define (fc iport)	;file contents, requires input port
	(let loop ((ls '()))
		(if (eof-object? (peek-char iport))
			ls
			(loop (append ls (cons (read iport) '()))))))
			
#|
(define (load-words)	; THIS COMPLETELY RESETS THE WORD LIST WITHIN THE BOT ITSELF
	(define ifile (open-input-file "words"))
	(define cur_add 'noun)	;what the word gets added to depends on this; this changes if it hits an asterisk
	(let loop ()
		|#


;(define in_conv #f)
(define conv_with "nobody")

(define con (irc:connection server: "irc.sourli.me" nick: "ClayBot" user: "ClayBot"))

#|
(define (set_conv sender)
	(irc:say con (random-from-list '("Yeah?" "That's me!" "What's up?")) "#test")
;	(set! in_conv #t)
	(set! conv_with sender))|#

(irc:connect con)

(irc:add-message-handler!
	con
	(lambda (msg)
		(irc:command con (string-append "PONG :" (car (irc:message-parameters msg)))))
	tag: 'ping command: "PING")

(define (try-to-join mes)
	(cond
		[(string=? (irc:message-command mes) "001") (irc:join con "#test")]
		[(string=? (irc:message-command mes) "PING") 
			(irc:process-message con mes)
			(irc:join con "#test")]
		[else (try-to-join (irc:wait con))]))

;;;;;;;;;;;;;;;;
;;  BIG NOTE  ;;
;;;;;;;;;;;;;;;;
; (list-ref (irc:message-parameters msg) 1) returns the message itself

#|
(define (res code)
	(cond
	[(string=? code "leave") (irc:say con "See ya!" "#test") (irc:quit con "leaving")]
	[(string=? code "done") (irc:say con "Okay, then." "#test") (set! conv_with "nobody")]
	[else (irc:say con "Wait, something went wrong here." "#test")]))
|#
	
(define (res msg)
	(define m (string-delete char-set:punctuation (string-downcase (list-ref (irc:message-parameters msg) 1)))) ; what the person said
	(write m) (newline) ; displays what bot hears for debug
	(define wl (string-split m))	; list of words
	(define sender (irc:message-sender msg)) ; who sent the message
	(define called (in-string? "claybot" m)) ; whether or not the person said the bot's name
	(if called (set! conv_with sender))
	(cond
	[(string=? m "identify") (irc:say con "I'm ClayBot!" "#test")]
	[(string=? sender conv_with) (cond
		[(or (in-list? "hi" wl) (in-list? "hello" wl) (in-list? "hey" wl))
			irc:say con (random-from-list '("Hi." "Hello." "Yo." "Hey.")) "#test")]
		[(in-list? "who" wl) (cond
			[(in-list? "you" wl) (irc:say con "I'm ClayBot." "#test")]
			[(in-list? "i" wl) (irc:say con (string-append (string-append "You're " sender) ".") "#test")]
			)]
		[(in-string? "?" m) ; default question handler
			(irc:say con (random-from-list '("Dunno." "I dunno." )) "#test")]
		[(in-list? "leave" wl)
			(irc:say con (random-from-list '("Goodbye!" "See ya!" "Later!" "Bye!")) "#test")
			(irc:quit con "leaving")]
		[(and (in-list? "done" wl) (in-string? "talk" m))
			(if called 
				(irc:say con (random-from-list '("We never started a conversation, though." "We need to start talking to be done talking." "Okay?")) "#test")
				(irc:say con (random-from-list '("Say my name if you need me again." "I'll be here." "That was a good talk." "Okay, then.")) "#test"))
			(set! conv_with "nobody")]
		[(in-list? "repeat" wl)
			(irc:say con m "#test")]
		[(in-list? "read" wl) (cond
			[(in-string? "test file" m)
				(define ifile (open-input-file "test"))
;				(write (slist->string (fc ifile))) (newline)
				(irc:say con (slist->string (fc ifile)) "#test")
				(close-input-port ifile)]
			)]
		[else (if called (irc:say con (random-from-list '("Yeah?" "That's me!" "What's up?")) "#test"))]
	)])
	)

(irc:add-message-handler!
	con (lambda (msg)
		(res msg))
	command: "PRIVMSG")
	

(try-to-join (irc:wait con))
(define (try-again) (irc:run-message-loop con debug: #t))
(try-again)