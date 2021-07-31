# .bash_profile

# Ocaml opam configuration
test -r /home/alex/.opam/opam-init/init.sh &&
    . /home/alex/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true

# Get the aliases and functions
if [ -f ~/.bashrc ]; then
    . ~/.bashrc
fi

# Start i3
if [[ -z $DISPLAY && $XDG_VTNR -eq 2 ]]
then
    exec startx
fi
