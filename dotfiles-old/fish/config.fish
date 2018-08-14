# temporaliry lists settings on macos

# rm
set -gx TRASH_DIR ~/tmp/.trash

# Go
set -gx GOPATH ~/gopath
set -gx PATH $GOPATH/bin $PATH

# Openssl / Homebrew
set -gx PATH /usr/local/opt/openssl@1.1/bin $PATH

# sdkman
set -gx JAVA_HOME (/usr/libexec/java_home)
set -gx SDKMAN_DIR /Users/yoshihara/.sdkman

# sdk help > /dev/null

# OPAM configuration
sed -e 's/"//g' ~/.opam/opam-init/variables.fish | source

# tmp: local aliases
alias dk docker
alias dkc docker-compose
alias dkill 'docker stop (docker ps -q); docker rm (docker ps -aq)'
