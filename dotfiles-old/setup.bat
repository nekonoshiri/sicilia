cd %HOMEPATH%
mkdir .vim\vimbackup

mklink %HOMEPATH%"\.bashrc" %HOMEPATH%"\.dotfiles\bash\bashrc"
mklink %HOMEPATH%"\.bash_profile" %HOMEPATH%"\.dotfiles\bash\bash_profile"
mklink %HOMEPATH%"\.bash_logout" %HOMEPATH%"\.dotfiles\bash\bash_logout"
mklink %HOMEPATH%"\.vimrc" %HOMEPATH%"\.dotfiles\vim\vimrc"
