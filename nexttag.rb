#!/usr/bin/env ruby


def get_next_tag


  this_branch = `fossil branch`.sub(/\A.*\* /m,'').sub(/\n.*\z/m,'')
  abort "Not on a version branch like v1.64 (got: >#{this_branch}<)" unless this_branch.match(/^v\d\.\d\d$/)
  
  #puts "this branch: #{this_branch}"

  tag_pat = /#{this_branch}(\d\d)/
  remote=`fsl remote`.chomp.sub(/^file:\/\//,'') # get tagset from origin
  cmd="fossil tag -R '#{remote}' list"
  tags = `#{cmd}`.split /\n/
  abort "fossil command failed [#{cmd}]" if $? != 0
  branch_tags = tags.find_all{|x| x.match(tag_pat) }.sort
  if branch_tags.length == 0
    return this_branch + "01"
  else
    latest_tag = branch_tags.last
    m1 = latest_tag.match(tag_pat)
    minor_digits = m1[1].to_i + 1
    if (minor_digits % 10) == 0
      minor_digits += 1
    end
    new_tag=sprintf("%s%02d", this_branch, minor_digits)
    return new_tag
  end
end

print get_next_tag
