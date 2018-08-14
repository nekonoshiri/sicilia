function v
  if type -q nvr
    nvr -s $argv
  else if type -q vim
    vim $argv
  else if type -q vi
    vi $argv
  else
    echo 'v: Could not find vi/vim'
  end
end
