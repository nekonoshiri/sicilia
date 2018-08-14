function cal
  if test -n "$argv"
    command cal $argv
    return 1
  end
  set day (date +'%e')
  command cal $argv | grep -C6 -E '(\s|^)'$day'(\s|$)'
end
