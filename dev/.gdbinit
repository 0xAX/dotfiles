# Improved gdb configuration for x86_64
#
# INSTALL INSTRUCTIONS: save as ~/.gdbinit
#
# VERSION : 0.0.1 (05-12--2017)

##
#  Default configuration
##

# default architecture
set architecture i386:x86-64

# don't ask me everytime. I know what I'm doing
set confirm off

# be quiet
set verbose off

define stack_top
  display/x *(int *)$rsp
end
