set architecture i386:x86-64

set confirm off

set disassembly-flavor intel
set disassemble-next-line on

set height 0
set width 0

set $USECOLOR = 1
set $COLOREDPROMPT = 1

set $BLACK = 0
set $RED = 1
set $GREEN = 2
set $YELLOW = 3
set $BLUE = 4
set $MAGENTA = 5
set $CYAN = 6
set $WHITE = 7

set $COLOR_REGNAME = $GREEN
set $COLOR_REGVAL = $BLACK
set $COLOR_REGVAL_MODIFIED = $RED
set $COLOR_SEPARATOR = $BLUE
set $COLOR_CPUFLAGS = $RED


# this is ugly but there's no else if available :-(
define color
	if $USECOLOR == 1
		# BLACK
		if $arg0 == 0
			echo \033[30m
		else
		# RED
		if $arg0 == 1
			echo \033[31m
		else
		# GREEN
		if $arg0 == 2
			echo \033[32m
		else
		# YELLOW
		if $arg0 == 3
			echo \033[33m
		else
		# BLUE
		if $arg0 == 4
			echo \033[34m
		else
		# MAGENTA
		if $arg0 == 5
			echo \033[35m
		else
		# CYAN
		if $arg0 == 6
			echo \033[36m
		else
		# WHITE
		if $arg0 == 7
			echo \033[37m
		end
		end
		end
		end
		end
		end
		end
		end
		end
		end
