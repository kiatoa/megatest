#!/usr/bin/env ruby


def get_next_tag(branch)



  abort "Not on a version branch like v1.64 (got: >#{branch}<)" unless branch.match(/^v\d\.\d\d$/)
  
  #puts "this branch: #{branch}"

  tag_pat = /#{branch}(\d\d)/
  remote=`fsl remote`.chomp.sub(/^file:\/\//,'') # get tagset from origin
  cmd="fossil tag -R '#{remote}' list"
  tags = `#{cmd}`.split /\n/
  abort "fossil command failed [#{cmd}]" if $? != 0
  branch_tags = tags.find_all{|x| x.match(tag_pat) }.sort
  if branch_tags.length == 0
    return branch + "01"
  else
    latest_tag = branch_tags.last
    m1 = latest_tag.match(tag_pat)
    minor_digits = m1[1].to_i + 1
    if (minor_digits % 10) == 0
      minor_digits += 1
    end
    new_tag=sprintf("%s%02d", branch, minor_digits)
    return new_tag
  end
end

branch = `fossil branch`.sub(/\A.*\* /m,'').sub(/\n.*\z/m,'')
tag= get_next_tag(branch)

puts "TODO: Write to megatest-version.scm:"
puts ";; Always use two or four digit decimal
;; 1.01, 1.02...1.10,1.11,1.1101 ... 1.99,2.00..

(declare (unit megatest-version))

(define megatest-version #{tag.sub(/^v/,'')})

"

puts "TODO: fossil tag add #{tag} #{branch}"
puts ""
