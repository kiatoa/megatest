(define *remotehost* "orion")
(define *homehost*   "zeus")
(define *homepath*   "/nfs/phoebe/disk1/home/mfs_matt/data/megatest/minimt/runtest")
(define *numsteps*   100)
(define *numtests*   50)
(define *numruns*    5)
(define *targets*    '("targ1"))
(define *testdelay*  0)
(define *rundelay*   0)
(define *launchdelay* 0)
(define *stepdelay*   0)

(use trace)
(trace-call-sites #t)
(trace
;;  open-create-db
 )
