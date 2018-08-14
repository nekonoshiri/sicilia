function :e
  if type -q nvr
    nvr -s $argv
  else
    echo ':e: Could not find nvr'
  end
end
