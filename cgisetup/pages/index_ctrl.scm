;; Copyright 2007-2008, Matthew Welland. Kiatoa.com All rights reserved.
;; 
;; this gets read for ALL pages. Don't weigh it down excessively!

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

