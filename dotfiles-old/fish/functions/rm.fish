function rm
  if test -z "$argv"
    echo "usage: rm file1 file2 ..."
    return 1
  end
  if not set -q TRASH_DIR
    echo "rm: TRASH_DIR is not set"
    return 1
  end
  if not test -d $TRASH_DIR
    echo "rm: The directory $TRASH_DIR (TRASH_DIR) does not exist"
    return 1
  end

  set dest $TRASH_DIR'/trash_'(date +%Y%m%d)
  if not test -d $dest
    mkdir $dest
  end

  for item in $argv
    mv -f $item $dest
  end
end
