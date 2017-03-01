;;======================================================================
;; Copyright 2017, Matthew Welland.
;; 
;;  This program is made available under the GNU GPL version 2.0 or
;;  greater. See the accompanying file COPYING for details.
;; 
;;  This program is distributed WITHOUT ANY WARRANTY; without even the
;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;  PURPOSE.
;;======================================================================

;; a function <pagename>-action is called on POST

(define (index-action action)
  (case (string->symbol action)
    ((filter)
     (let ((target-type   (s:get-input 'target-type))
	   (target-filter (s:get-input 'tfilter))
	   (target        (s:get-input 'target))
	   (row-or-col    (s:get-input 'row-or-col)))
       ;; should not be using session vars for these, session vars are not multi-tab
       ;; resistant (thinking of you Jeff!)
       (s:session-var-set! "row-or-col" (if (list? row-or-col)
					    (string-intersperse row-or-col ",")
					    row-or-col))
       (s:session-var-set! "target-type" target-type)
       (s:set! "tfilter" target-filter)
       (s:session-var-set! "target"  target)
       (s:session-var-set! "target-filter" target-filter)))))

;;======================================================================
;; Below are the raw chunks of html, css and jquery stuff needed to make
;; html kickstart and other useful things work
;;======================================================================

(define index:kickstart-junk
#<<EOF
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0"/>
<meta name="description" content="" />

<link rel="stylesheet" type="text/css" href="/css/kickstart.css" media="all" />                  <!-- KICKSTART -->
<link rel="stylesheet" type="text/css" href="/style.css" media="all" />                          <!-- CUSTOM STYLES -->

<link rel="icon" type="image/x-icon" href="/favicon.ico" />
<style type="text/css">
      .column {
        /* border:1px solid red; */
        padding:0px;
     }

</style>
EOF
)

(define index:jquery
  (if #t 

#<<EOF
<script type="text/javascript" src="https://code.jquery.com/jquery-1.11.3.js"></script>
<script src="http://code.jquery.com/jquery-migrate-1.2.1.js"></script>
EOF

#<<EOF
<script type="text/javascript" src="https://ajax.googleapis.com/ajax/libs/jquery/1.6.4/jquery.min.js"></script>
<!--[if lt IE 9]><script src="http://html5shiv.googlecode.com/svn/trunk/html5.js"></script><![endif]-->
EOF
))

(define index:javascript
#<<EOF
<script type="text/javascript" src="/js/prettify.js"></script>                                   <!-- PRETTIFY -->
<script type="text/javascript" src="/js/kickstart.js"></script>                                  <!-- KICKSTART -->
EOF
)

