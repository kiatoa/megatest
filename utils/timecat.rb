#!/usr/bin/env ruby

# -*- Mode: Ruby; -*-

# Author: brandon.j.barclay (bjbarcla) 13ww48.1

require 'pathname' 
STDOUT.sync = true
STDERR.sync = true

### main sequence

prev = start = Time.now.to_f

STDIN.each_line{|line|
  now = Time.now.to_f
  total_elapsed = now - start
  delta = now - prev
  prev = now
  puts sprintf("[line_elapsed=%7.2f] [tot_elapsed=%7.2f] |> %s", delta, total_elapsed, line)
}
exit 0
