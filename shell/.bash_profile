# .bash_profile

# Get the aliases and functions
if [ -f ~/.bashrc ]; then
    . ~/.bashrc
fi

# Start i3
if [[ -z $DISPLAY && $XDG_VTNR -eq 2 ]]
then
    exec startx
fi

eval `ssh-agent -s`
eval `gpg-agent --daemon`
