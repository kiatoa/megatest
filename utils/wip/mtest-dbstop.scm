#!/p/foundry/env/pkgs/chicken/4.10.0_ext/bin/csi -s

(use chicken)
(use data-structures)


(include "/nfs/site/home/bjbarcla/bin2/mtest-repair-lib.scm")
(glib-color-mode 1)

(set! *this-cmd* "/nfs/site/home/bjbarcla/bin2/mtest-dbstop.scm")
(kill-in-db)
  
