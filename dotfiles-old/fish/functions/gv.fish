function gv
  if type -q gvim
    gvim $argv
  else
    echo 'gv: Could not find gvim'
  end
end
