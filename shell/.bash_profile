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

# start ssh agent if it is not
SSH_AGENT_PID=$(pidof ssh-agent)
if [ -z "$SSH_AGENT_PID" ];
then
    eval "$(ssh-agent -s)"
fi

# start gpg agent if it is not
GPG_AGENT_PID=$(pidof gpg-agent)
if [ -z "$GPG_AGENT_PID" ];
then
    eval `gpg-agent --daemon`
fi

# opam configuration
test -r /home/alex/.opam/opam-init/init.sh && . /home/alex/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true
. "$HOME/.cargo/env"
