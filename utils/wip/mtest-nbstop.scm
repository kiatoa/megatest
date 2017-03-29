#!/p/foundry/env/pkgs/chicken/4.10.0_ext/bin/csi -s

(use chicken)
(use data-structures)


(include "/nfs/site/home/bjbarcla/bin2/mtest-repair-lib.scm")
(glib-color-mode 1)

(set! *this-cmd* "/nfs/site/home/bjbarcla/bin2/mtest-nbstop.scm")

(inote "Killing local mtest/dboard in this run area.")
(kill-mtest-dboard)

;;;; move logs dir aside
(inote "move logs")
(system (conc "mv logs logs-aside-`date +%s`"))
(system "mkdir logs")



(inote "Killing netbatch mtest jobs launched from this run area.")
(let ((jobcount (kill-mtest-jobs-in-netbatch)))
  (when (> jobcount 0)
    (inote "Marking in-flight tests killed in db")
    (when (db-islocked? "megatest.db")
      (iwarn "Unlocking megatest.db")
      (db-unlock "megaetest.db"))
    (kill-in-db)))

(inote "Final reaping of mtest/dboard")
(kill-mtest-dboard)


