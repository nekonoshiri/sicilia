# use wrapper function of fish, not builtin
function nvcd
  nvr -c "cd "(realpath $argv)
end
