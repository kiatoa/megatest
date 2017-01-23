(use ducttape-lib)

(let (
      (customers (skim-cmdline-opts-withargs-by-regex "--cust(omer)?"))
      (magicmode (skim-cmdline-opts-noarg-by-regex "--magic"))
      )
  (print "your customers are " customers)
  (if (null? magicmode)
      (print "no unicorns for you")
      (print "magic!")
  )
  )

(idbg "hello")
(idbg "hello2" 2)
(idbg "hello2" 3)
(inote "note")
(iwarn "warn")
(ierr "err")
