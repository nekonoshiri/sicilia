# use wrapper function of fish, not builtin
function ca
  cd $argv
  and la
end
