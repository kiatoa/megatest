# This is the library of stuff for megatest

def run_and_record(stepname, cmd, checks)
  system "megatest -step #{stepname} :state start :status n/a"
  system cmd
  exitcode=$?
  if exitcode==0
    exitcode='pass'
  else
    exitcode='fail'
  end
  system "megatest -step #{stepname} :state end :status #{exitcode}"
end

def record_step(stepname,state,status)
  system "megatest -step #{stepname} :state #{state} :status #{status}"
end

def test_status(state,status)
  system "megatest -test-status :state #{state} :status #{status}"
end


# WARNING: This example is deprecated. Don't use the -test-status command
#          unless you know for sure what you are doing.
def file_size_checker(stepname,filename,minsize,maxsize)
  fsize=File.size(filename)
  if fsize > maxsize or fsize < minsize
    system "megatest -test-status :state COMPLETED :status fail"
  else
   system "megatest -test-status :state COMPLETED :status pass"
  end
end


def wait_for_step(testname,stepname)
end
