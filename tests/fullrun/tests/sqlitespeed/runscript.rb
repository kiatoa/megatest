#! /usr/bin/env ruby

require "#{ENV['MT_RUN_AREA_HOME']}/../supportfiles/ruby/librunscript.rb"

# run_record(stepname, cmd) - will record in db if exit code of script was zero or not
run_and_record('create db',"sqlite3 testing.db << EOF\ncreate table if not exists blah(id INTEGER PRIMARY KEY,name TEXT);\n.q\nEOF","")

if (! File.exists?("../../runfirst/I_was_here"))
    puts "ERROR: This test was started before the prerequisites ran!"
    system "megatest -test-status :state INCOMPLETE :status FAIL"
    exit 1
end

system "megatest -setlog logcheck.html"

# file_size_checker(stepname, filename, minsize, maxsize) - negative means ignore
# file_size_checker('create db','testing.db',100,-1)

num_records=rand(5) # 0000
record_step("add #{num_records}","start","n/a")
status=false
(0..num_records).each do |i|
  randstring="abc";
  # "a;lskdfja;sdfj;alsdfj;aslfdj;alsfja;lsfdj;alsfja;lsjfd;lasfjl;asdfja;slfj;alsjf;asljf;alsjf;lasdjf;lasjf;lasjf;alsjf;lashflkashflkerhflkdsvnlasldhlfaldf"
  # status=system "sqlite3 testing.db \"insert into blah (name) values ('#{randstring}');\""
  system "megatest -step testing :state wrote_junk :status #{num_records}"
  sleep(5)
  puts "i=#{i}"
end
if status==0
  status='pass'
else
  status='fail'
end

record_step("add #{num_records}","end",status)

