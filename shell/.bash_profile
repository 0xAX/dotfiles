# .bash_profile

# Get the aliases and functions
if [ -f ~/.bashrc ]; then
	. ~/.bashrc
fi

export PATH="$HOME/.cargo/bin:$PATH"

# opam configuration
test -r /home/alex/.opam/opam-init/init.sh && . /home/alex/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true
