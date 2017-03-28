#!/usr/bin/env ruby

#dir = "."
#if ARGV.length == 1
#  dir = ARGV[0]
#end
#puts dir
#exit 1

allfiles = []
server_logs = `find logs/ -type f -name 'server*.log' 2>/dev/null`.split /\n/
allfiles += server_logs
ARGV.each{|dir|
  nbfiles = `find #{dir} -type f -name '##*' 2>/dev/null`.split /\n/
  fakefiles = `find #{dir} -type f -name 'NBFAKE-*' 2>/dev/null`.split /\n/
  allfiles = allfiles + nbfiles + fakefiles
}

buckets = Hash.new{|h,k| h[k]=[]}
#;buckets['OK'] = []
#;buckets['stackdump'] = []


sig_patterns = [
                'cannot create directory - File exists',
                'in thread: \(finalize!\) bad argument type - not a database or statement: #<unspecified>',
                'cannot delete file - No such file or directory: .*\/.runconfig.',
                'Error: \(hash-table-ref/default\) bad argument type - not a structure of the required type',
                '\(#<thread: Watchdog thread>\): in thread: \(open-output-file\) cannot open file - File exists:',
                'http-transport.scm:442: posix-extras#change-file-times',
                'thread: \(file-exists\?\) system error while trying to access file:',
                'error: database is locked',
                'Finalizing failed, unable to close due to unfinalized statements or unfinished backups',
                'rmt.scm:276: current-milliseconds',
                'http-transport.scm:366: exit',
                'should never happen',
                'FATAL: \*configdat\* was inaccessible! This should never happen.',
                '!!ISOENV PRESENT!!'

           ]

allfiles.each{|logfile|
  bucket = 'OK'
  open(logfile){|fh|
    fh.each{|line|

      if line.match(/Call history/)
        if bucket == 'OK'
          bucket='??'
        end
      end
      sig_patterns.each_with_index{|pat,bucket_name|
        #bucket_name,pat = i
        if line.match(/#{pat}/)
          bucket=bucket_name
        end
      }
        
    }
  }
  buckets[bucket] << logfile
}

puts "count\tsignature\texample file"
buckets.keys.each{|bucket|
  count = buckets[bucket].length

  example = buckets[bucket][0]
  if example
    puts "#{count}\tsignature-#{bucket}\t#{example}"
    if bucket.to_s.match(/^[0-9]+$/)
      puts "                   `- pattern = /#{sig_patterns[bucket]}/"
    end
  else
    puts "#{count}\tsignature-#{bucket}"
  end
}
#puts buckets['stackdump'][0]
