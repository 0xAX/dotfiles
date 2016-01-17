# INSTALL INSTRUCTIONS: save as ~/.gdbinit
#
# DESCRIPTION: A user-friendly gdb configuration file.
#
# REVISION : 7.4.4 (02/01/2012)
#
# CONTRIBUTORS: mammon_, elaine, pusillus, mong, zhang le, l0kit,
#               truthix the cyberpunk, fG!, gln
#
# FEEDBACK: http://reverse.put.as - reverser@put.as
#			https://www.reverse-engineering.net
#           The Linux Area
#           Topic: "+HCU's .gdbinit Version 7.1 -- humble announce"
#
# NOTES: 'help user' in gdb will list the commands/descriptions in this file
#        'context on' now enables auto-display of context screen
#
# MAC OS X NOTES: If you are using this on Mac OS X, you must either attach gdb to a process
#                 or launch gdb without any options and then load the binary file you want to analyse with "exec-file" option
#                 If you load the binary from the command line, like $gdb binary-name, this will not work as it should
#                 For more information, read it here http://reverse.put.as/2008/11/28/apples-gdb-bug/
#
#  UPDATE: This bug can be fixed in gdb source. Refer to http://reverse.put.as/2009/08/10/fix-for-apples-gdb-bug-or-why-apple-forks-are-bad/
#          and http://reverse.put.as/2009/08/26/gdb-patches/ (if you want the fixed binary for i386)
#
#          An updated version of the patch and binary is available at http://reverse.put.as/2011/02/21/update-to-gdb-patches-fix-a-new-bug/
#
# CHANGELOG: (older changes at the end of the file)
#
#   Version 7.4.4 (02/01/2012)
#     - Added the "skip" command. This will jump to the next instruction after EIP/RIP without executing the current one.
#       Thanks to @bSr43 for the tip to retrieve the current instruction size.
#
#	Version 7.4.3 (04/11/2011)
#	  - Modified "hexdump" command to support a variable number of lines (optional parameter)
#	  - Removed restrictions on type of addresses in the "dd" command - Thanks to Plouj for the warning :-)
#	   I don't know what was the original thinking behind those :-)
#	  - Modified the assemble command to support 64bits - You will need to recompile nasm since the version shipped with OS X doesn't supports 64bits (www.nasm.us).
#	   Assumes that the new binary is installed at /usr/local/bin - modify the variable at the top if you need so. 
#	   It will assemble based on the target arch being debugged. If you want to use gdb for a quick asm just use the 32bits or 64bits commands to set your target.
#      Thanks to snare for the warning and original patch :-)
#	  - Added "asm" command - it's a shortcut to the "assemble" command.
#	  - Added configuration variable for colorized prompt. Plouj reported some issues with Ubuntu's gdb 7.2 if prompt is colorized.
#
#   TODO:
#

# __________________gdb options_________________

# set to 1 to enable 64bits target by default (32bits is the default)
set $64BITS = 0
# set to 0 if you have problems with the colorized prompt - reported by Plouj with Ubuntu gdb 7.2
set $COLOUREDPROMPT = 1
# Colour the first line of the disassembly - default is green, if you want to change it search for
# SETCOLOUR1STLINE and modify it :-)
set $SETCOLOUR1STLINE = 0
# set to 0 to remove display of objectivec messages (default is 1)
set $SHOWOBJECTIVEC = 1
# set to 0 to remove display of cpu registers (default is 1)
set $SHOWCPUREGISTERS = 1
# set to 1 to enable display of stack (default is 0)
set $SHOWSTACK = 0
# set to 1 to enable display of data window (default is 0)
set $SHOWDATAWIN = 0
# set to 0 to disable coloured display of changed registers
set $SHOWREGCHANGES = 1
# set to 1 so skip command to execute the instruction at the new location
# by default it EIP/RIP will be modified and update the new context but not execute the instruction
set $SKIPEXECUTE = 0
# if $SKIPEXECUTE is 1 configure the type of execution
# 1 = use stepo (do not get into calls), 0 = use stepi (step into calls)
set $SKIPSTEP = 1

set confirm off
set verbose off

if $COLOUREDPROMPT == 1
	set prompt \033[31mgdb$ \033[0m
end

set output-radix 0x10
set input-radix 0x10

# These make gdb never pause in its output
set height 0
set width 0

# Display instructions in Intel format - change to "att" if you prefer AT&T format
set disassembly-flavor intel

set $SHOW_CONTEXT = 1
set $SHOW_NEST_INSN = 0

set $CONTEXTSIZE_STACK = 6
set $CONTEXTSIZE_DATA  = 8
set $CONTEXTSIZE_CODE  = 8

# __________________end gdb options_________________

# Initialize these variables else comparisons will fail for colouring
# we must initialize all of them at once, 32 and 64 bits.
set $oldrax = 0
set $oldrbx = 0
set $oldrcx = 0
set $oldrdx = 0
set $oldrsi = 0
set $oldrdi = 0
set $oldrbp = 0
set $oldrsp = 0
set $oldr8  = 0
set $oldr9  = 0
set $oldr10 = 0
set $oldr11 = 0
set $oldr12 = 0
set $oldr13 = 0
set $oldr14 = 0
set $oldr15 = 0
set $oldeax = 0
set $oldebx = 0
set $oldecx = 0
set $oldedx = 0
set $oldesi = 0
set $oldedi = 0
set $oldebp = 0
set $oldesp = 0

# used by ptraceme/rptraceme
set $ptrace_bpnum = 0

# ______________window size control___________
define contextsize-stack
    if $argc != 1
        help contextsize-stack
    else
        set $CONTEXTSIZE_STACK = $arg0
    end
end
document contextsize-stack
Set stack dump window size to NUM lines.
Usage: contextsize-stack NUM
end


define contextsize-data
    if $argc != 1
        help contextsize-data
    else
        set $CONTEXTSIZE_DATA = $arg0
    end
end
document contextsize-data
Set data dump window size to NUM lines.
Usage: contextsize-data NUM
end


define contextsize-code
    if $argc != 1
        help contextsize-code
    else
        set $CONTEXTSIZE_CODE = $arg0
    end
end
document contextsize-code
Set code window size to NUM lines.
Usage: contextsize-code NUM
end




# _____________breakpoint aliases_____________
define bpl
    info breakpoints
end
document bpl
List all breakpoints.
end

define bp
    if $argc != 1
        help bp
    else
        break $arg0
    end
end
document bp
Set breakpoint.
Usage: bp LOCATION
LOCATION may be a line number, function name, or "*" and an address.

To break on a symbol you must enclose symbol name inside "".
Example:
bp "[NSControl stringValue]"
Or else you can use directly the break command (break [NSControl stringValue])
end


define bpc 
    if $argc != 1
        help bpc
    else
        clear $arg0
    end
end
document bpc
Clear breakpoint.
Usage: bpc LOCATION
LOCATION may be a line number, function name, or "*" and an address.
end


define bpe
    if $argc != 1
        help bpe
    else
        enable $arg0
    end
end
document bpe
Enable breakpoint with number NUM.
Usage: bpe NUM
end


define bpd
    if $argc != 1
        help bpd
    else
        disable $arg0
    end
end
document bpd
Disable breakpoint with number NUM.
Usage: bpd NUM
end


define bpt
    if $argc != 1
        help bpt
    else
        tbreak $arg0
    end
end
document bpt
Set a temporary breakpoint.
This breakpoint will be automatically deleted when hit!
Usage: bpt LOCATION
LOCATION may be a line number, function name, or "*" and an address.
end

define bpm
    if $argc != 1
        help bpm
    else
        awatch $arg0
    end
end
document bpm
Set a read/write breakpoint on EXPRESSION, e.g. *address.
Usage: bpm EXPRESSION
end

define bhb
    if $argc != 1
        help bhb
    else
        hb $arg0
    end
end
document bhb
Set hardware assisted breakpoint.
Usage: bhb LOCATION
LOCATION may be a line number, function name, or "*" and an address.
end

define bht
    if $argc != 1
        help bht
    else
        thbreak $arg0
    end
end
document bht
Set a temporary hardware breakpoint.
This breakpoint will be automatically deleted when hit!
Usage: bht LOCATION
LOCATION may be a line number, function name, or "*" and an address.
end


# ______________process information____________
define argv
    show args
end
document argv
Print program arguments.
end


define stack
    if $argc == 0
        info stack
    end
    if $argc == 1
        info stack $arg0
    end
    if $argc > 1
        help stack
    end
end
document stack
Print backtrace of the call stack, or innermost COUNT frames.
Usage: stack <COUNT>
end


define frame
    info frame
    info args
    info locals
end
document frame
Print stack frame.
end


define flags
# OF (overflow) flag
    if (($eflags >> 0xB) & 1)
        printf "O "
        set $_of_flag = 1
    else
        printf "o "
        set $_of_flag = 0
    end
    if (($eflags >> 0xA) & 1)
        printf "D "
    else
        printf "d "
    end
    if (($eflags >> 9) & 1)
        printf "I "
    else
        printf "i "
    end
    if (($eflags >> 8) & 1)
        printf "T "
    else
        printf "t "
    end
# SF (sign) flag
    if (($eflags >> 7) & 1)
        printf "S "
        set $_sf_flag = 1
    else
        printf "s "
        set $_sf_flag = 0
    end
# ZF (zero) flag
    if (($eflags >> 6) & 1)
        printf "Z "
	set $_zf_flag = 1
    else
        printf "z "
	set $_zf_flag = 0
    end
    if (($eflags >> 4) & 1)
        printf "A "
    else
        printf "a "
    end
# PF (parity) flag
    if (($eflags >> 2) & 1)
        printf "P "
	set $_pf_flag = 1
    else
        printf "p "
	set $_pf_flag = 0
    end
# CF (carry) flag
    if ($eflags & 1)
        printf "C "
	set $_cf_flag = 1
    else
        printf "c "
	set $_cf_flag = 0
    end
    printf "\n"
end
document flags
Print flags register.
end


define eflags
    printf "     OF <%d>  DF <%d>  IF <%d>  TF <%d>",\
           (($eflags >> 0xB) & 1), (($eflags >> 0xA) & 1), \
           (($eflags >> 9) & 1), (($eflags >> 8) & 1)
    printf "  SF <%d>  ZF <%d>  AF <%d>  PF <%d>  CF <%d>\n",\
           (($eflags >> 7) & 1), (($eflags >> 6) & 1),\
           (($eflags >> 4) & 1), (($eflags >> 2) & 1), ($eflags & 1)
    printf "     ID <%d>  VIP <%d> VIF <%d> AC <%d>",\
           (($eflags >> 0x15) & 1), (($eflags >> 0x14) & 1), \
           (($eflags >> 0x13) & 1), (($eflags >> 0x12) & 1)
    printf "  VM <%d>  RF <%d>  NT <%d>  IOPL <%d>\n",\
           (($eflags >> 0x11) & 1), (($eflags >> 0x10) & 1),\
           (($eflags >> 0xE) & 1), (($eflags >> 0xC) & 3)
end
document eflags
Print eflags register.
end


define reg
 if ($64BITS == 1)
# 64bits stuff
    printf "  "
    # RAX
    if ($rax != $oldrax && $SHOWREGCHANGES == 1)
	    echo \033[32m
	    printf "RAX:"
	    echo \033[31m
	    printf " 0x%016lX  ", $rax
	else
	    echo \033[32m
	    printf "RAX:"
	    echo \033[0m
	    printf " 0x%016lX  ", $rax
	end
	# RBX
	if ($rbx != $oldrbx && $SHOWREGCHANGES == 1)
	    echo \033[32m
	    printf "RBX:"
	    echo \033[31m
	    printf " 0x%016lX  ", $rbx
	else
		echo \033[32m
	    printf "RBX:"
	    echo \033[0m
	    printf " 0x%016lX  ", $rbx
    end
    # RCX
    if ($rcx != $oldrcx && $SHOWREGCHANGES == 1)
	    echo \033[32m
	    printf "RCX:"
	    echo \033[31m
	    printf " 0x%016lX  ", $rcx
	else
	    echo \033[32m
	    printf "RCX:"
	    echo \033[0m
	    printf " 0x%016lX  ", $rcx
	end
	# RDX
    if ($rdx != $oldrdx && $SHOWREGCHANGES == 1)
	    echo \033[32m
	    printf "RDX:"
	    echo \033[31m
	    printf " 0x%016lX  ", $rdx
	else
	    echo \033[32m
	    printf "RDX:"
	    echo \033[0m
	    printf " 0x%016lX  ", $rdx
	end
	echo \033[1m\033[4m\033[31m
    flags
    echo \033[0m
    printf "  "
    # RSI
    if ($rsi != $oldrsi && $SHOWREGCHANGES == 1)
	    echo \033[32m
	    printf "RSI:"
	    echo \033[31m
	    printf " 0x%016lX  ", $rsi
	else
	    echo \033[32m
	    printf "RSI:"
	    echo \033[0m
	    printf " 0x%016lX  ", $rsi
	end
	# RDI
	if ($rdi != $oldrdi && $SHOWREGCHANGES == 1)
	    echo \033[32m
    	printf "RDI:"
	    echo \033[31m
	    printf " 0x%016lX  ", $rdi
	else
	    echo \033[32m
    	printf "RDI:"
	    echo \033[0m
	    printf " 0x%016lX  ", $rdi	
	end
	# RBP
	if ($rbp != $oldrbp && $SHOWREGCHANGES == 1)
	    echo \033[32m
    	printf "RBP:"
	    echo \033[31m
	    printf " 0x%016lX  ", $rbp
	else
	    echo \033[32m
    	printf "RBP:"
	    echo \033[0m
	    printf " 0x%016lX  ", $rbp
	end
	# RSP
	if ($rsp != $oldrsp && $SHOWREGCHANGES == 1)
	    echo \033[32m
    	printf "RSP:"
	    echo \033[31m
	    printf " 0x%016lX  ", $rsp
	else
	    echo \033[32m
    	printf "RSP:"
	    echo \033[0m
	    printf " 0x%016lX  ", $rsp	
    end
    echo \033[32m
    printf "RIP:"
    echo \033[0m
    printf " 0x%016lX\n  ", $rip
    # R8
	if ($r8 != $oldr8 && $SHOWREGCHANGES == 1)
	    echo \033[32m
    	printf "R8 :"
	    echo \033[31m
	    printf " 0x%016lX  ", $r8
	else
	    echo \033[32m
    	printf "R8 :"
	    echo \033[0m
	    printf " 0x%016lX  ", $r8
    end
    # R9
    if ($r9 != $oldr9 && $SHOWREGCHANGES == 1)
	    echo \033[32m
    	printf "R9 :"
	    echo \033[31m
	    printf " 0x%016lX  ", $r9
	else
	    echo \033[32m
    	printf "R9 :"
	    echo \033[0m
	    printf " 0x%016lX  ", $r9
    end
    # R10
    if ($r10 != $oldr10 && $SHOWREGCHANGES == 1)
	    echo \033[32m
    	printf "R10:"
	    echo \033[31m
	    printf " 0x%016lX  ", $r10
	else
	    echo \033[32m
    	printf "R10:"
	    echo \033[0m
	    printf " 0x%016lX  ", $r10
    end
    # R11
	if ($r11 != $oldr11 && $SHOWREGCHANGES == 1)
    	echo \033[32m
	    printf "R11:"
	    echo \033[31m
	    printf " 0x%016lX  ", $r11
	else
    	echo \033[32m
	    printf "R11:"
	    echo \033[0m
	    printf " 0x%016lX  ", $r11
    end
    # R12
    if ($r12 != $oldr12 && $SHOWREGCHANGES == 1)
	    echo \033[32m
   		printf "R12:"
	    echo \033[31m
	    printf " 0x%016lX\n  ", $r12
	else
	    echo \033[32m
   		printf "R12:"
	    echo \033[0m
	    printf " 0x%016lX\n  ", $r12
    end
    # R13
    if ($r13 != $oldr13 && $SHOWREGCHANGES == 1)
	    echo \033[32m
    	printf "R13:"
	    echo \033[31m
	    printf " 0x%016lX  ", $r13
	else
	    echo \033[32m
    	printf "R13:"
	    echo \033[0m
	    printf " 0x%016lX  ", $r13
    end
    # R14
    if ($r14 != $oldr14 && $SHOWREGCHANGES == 1)
	    echo \033[32m
	    printf "R14:"
	    echo \033[31m
	    printf " 0x%016lX  ", $r14
	else
	    echo \033[32m
	    printf "R14:"
	    echo \033[0m
	    printf " 0x%016lX  ", $r14
    end
    # R15
    if ($r15 != $oldr15 && $SHOWREGCHANGES == 1)
	    echo \033[32m
    	printf "R15:"
	    echo \033[31m
	    printf " 0x%016lX\n  ", $r15
	else
	    echo \033[32m
    	printf "R15:"
	    echo \033[0m
	    printf " 0x%016lX\n  ", $r15
    end
  	echo \033[32m
    printf "CS:"
    echo \033[0m
    printf " %04X  ", $cs
    echo \033[32m
    printf "DS:"
    echo \033[0m
    printf " %04X  ", $ds
    echo \033[32m
    printf "ES:"
    echo \033[0m
    printf " %04X  ", $es
    echo \033[32m
    printf "FS:"
    echo \033[0m
    printf " %04X  ", $fs
    echo \033[32m
    printf "GS:"
    echo \033[0m
    printf " %04X  ", $gs
    echo \033[32m
    printf "SS:"
    echo \033[0m
    printf " %04X", $ss
    echo \033[0m
# 32bits stuff
 else
    printf "  "
    # EAX
    if ($eax != $oldeax && $SHOWREGCHANGES == 1)
	    echo \033[32m
   		printf "EAX:"
   	 	echo \033[31m
   	 	printf " 0x%08X  ", $eax
   	else
	    echo \033[32m
   		printf "EAX:"
   	 	echo \033[0m
   	 	printf " 0x%08X  ", $eax
   	end
   	# EBX
   	if ($ebx != $oldebx && $SHOWREGCHANGES == 1) 
	    echo \033[32m
    	printf "EBX:"
	    echo \033[31m
   		printf " 0x%08X  ", $ebx
   	else
	    echo \033[32m
    	printf "EBX:"
	    echo \033[0m
   		printf " 0x%08X  ", $ebx   	
   	end
   	# ECX
   	if ($ecx != $oldecx && $SHOWREGCHANGES == 1)
	    echo \033[32m
    	printf "ECX:"
	    echo \033[31m
	    printf " 0x%08X  ", $ecx
	else
	    echo \033[32m
    	printf "ECX:"
	    echo \033[0m
	    printf " 0x%08X  ", $ecx
	end
	# EDX
	if ($edx != $oldedx && $SHOWREGCHANGES == 1)
	    echo \033[32m
    	printf "EDX:"
	    echo \033[31m
	    printf " 0x%08X  ", $edx
	else
	    echo \033[32m
    	printf "EDX:"
	    echo \033[0m
	    printf " 0x%08X  ", $edx
	end
    echo \033[1m\033[4m\033[31m
    flags
    echo \033[0m
    printf "  "
    # ESI
    if ($esi != $oldesi && $SHOWREGCHANGES == 1)
	    echo \033[32m
    	printf "ESI:"
	    echo \033[31m
	    printf " 0x%08X  ", $esi
	else
	    echo \033[32m
    	printf "ESI:"
	    echo \033[0m
	    printf " 0x%08X  ", $esi
	end
	# EDI
	if ($edi != $oldedi && $SHOWREGCHANGES == 1)
	    echo \033[32m
    	printf "EDI:"
	    echo \033[31m
	    printf " 0x%08X  ", $edi
	else
	    echo \033[32m
    	printf "EDI:"
	    echo \033[0m
	    printf " 0x%08X  ", $edi
	end
	# EBP
	if ($ebp != $oldebp && $SHOWREGCHANGES == 1)
	    echo \033[32m
	    printf "EBP:"
	    echo \033[31m
	    printf " 0x%08X  ", $ebp
	else
	    echo \033[32m
	    printf "EBP:"
	    echo \033[0m
	    printf " 0x%08X  ", $ebp
	end
	# ESP
	if ($esp != $oldesp && $SHOWREGCHANGES == 1)
	    echo \033[32m
    	printf "ESP:"
	    echo \033[31m
	    printf " 0x%08X  ", $esp
	else
	    echo \033[32m
    	printf "ESP:"
	    echo \033[0m
	    printf " 0x%08X  ", $esp
    end
    echo \033[32m
    printf "EIP:"
    echo \033[0m
    printf " 0x%08X\n  ", $eip
    echo \033[32m
    printf "CS:"
    echo \033[0m
    printf " %04X  ", $cs
    echo \033[32m
    printf "DS:"
    echo \033[0m
    printf " %04X  ", $ds
    echo \033[32m
    printf "ES:"
    echo \033[0m
    printf " %04X  ", $es
    echo \033[32m
    printf "FS:"
    echo \033[0m
    printf " %04X  ", $fs
    echo \033[32m
    printf "GS:"
    echo \033[0m
    printf " %04X  ", $gs
    echo \033[32m
    printf "SS:"
    echo \033[0m
    printf " %04X", $ss
    echo \033[0m
 end
# call smallregisters
	smallregisters
# display conditional jump routine
	if ($64BITS == 1)
	 printf "\t\t\t\t"
	end
    dumpjump
    printf "\n"
    if ($SHOWREGCHANGES == 1)
    	if ($64BITS == 1)
	    	set $oldrax = $rax
			set $oldrbx = $rbx
			set $oldrcx = $rcx
			set $oldrdx = $rdx
			set $oldrsi = $rsi
			set $oldrdi = $rdi
			set $oldrbp = $rbp
			set $oldrsp = $rsp
			set $oldr8 = $r8
			set $oldr9 = $r9
			set $oldr10 = $r10
			set $oldr11 = $r11
			set $oldr12 = $r12
			set $oldr13 = $r13
			set $oldr14 = $r14
			set $oldr15 = $r15
		else
	    	set $oldeax = $eax
			set $oldebx = $ebx
			set $oldecx = $ecx
			set $oldedx = $edx
			set $oldesi = $esi
			set $oldedi = $edi
			set $oldebp = $ebp
			set $oldesp = $esp
		end
	end

end
document reg
Print CPU registers.
end

define smallregisters
 if ($64BITS == 1)
#64bits stuff
	# from rax
	set $eax = $rax & 0xffffffff
	set $ax = $rax & 0xffff
	set $al = $ax & 0xff
	set $ah = $ax >> 8
	# from rbx
	set $bx = $rbx & 0xffff
	set $bl = $bx & 0xff
	set $bh = $bx >> 8
	# from rcx
	set $ecx = $rcx & 0xffffffff
	set $cx = $rcx & 0xffff
	set $cl = $cx & 0xff
	set $ch = $cx >> 8
	# from rdx
	set $edx = $rdx & 0xffffffff
	set $dx = $rdx & 0xffff
	set $dl = $dx & 0xff
	set $dh = $dx >> 8
	# from rsi
	set $esi = $rsi & 0xffffffff
	set $si = $rsi & 0xffff
	# from rdi
	set $edi = $rdi & 0xffffffff
	set $di = $rdi & 0xffff		
#32 bits stuff
 else
	# from eax
	set $ax = $eax & 0xffff
	set $al = $ax & 0xff
	set $ah = $ax >> 8
	# from ebx
	set $bx = $ebx & 0xffff
	set $bl = $bx & 0xff
	set $bh = $bx >> 8
	# from ecx
	set $cx = $ecx & 0xffff
	set $cl = $cx & 0xff
	set $ch = $cx >> 8
	# from edx
	set $dx = $edx & 0xffff
	set $dl = $dx & 0xff
	set $dh = $dx >> 8
	# from esi
	set $si = $esi & 0xffff
	# from edi
	set $di = $edi & 0xffff		
 end
 
end
document smallregisters
Create the 16 and 8 bit cpu registers (gdb doesn't have them by default)
And 32bits if we are dealing with 64bits binaries
end

define func
    if $argc == 0
        info functions
    end
    if $argc == 1
        info functions $arg0
    end
    if $argc > 1
        help func
    end
end
document func
Print all function names in target, or those matching REGEXP.
Usage: func <REGEXP>
end


define var
    if $argc == 0
        info variables
    end
    if $argc == 1
        info variables $arg0
    end
    if $argc > 1
        help var
    end
end
document var
Print all global and static variable names (symbols), or those matching REGEXP.
Usage: var <REGEXP>
end


define lib
    info sharedlibrary
end
document lib
Print shared libraries linked to target.
end


define sig
    if $argc == 0
        info signals
    end
    if $argc == 1
        info signals $arg0
    end
    if $argc > 1
        help sig
    end
end
document sig
Print what debugger does when program gets various signals.
Specify a SIGNAL as argument to print info on that signal only.
Usage: sig <SIGNAL>
end


define threads
    info threads
end
document threads
Print threads in target.
end


define dis
    if $argc == 0
        disassemble
    end
    if $argc == 1
        disassemble $arg0
    end
    if $argc == 2
        disassemble $arg0 $arg1
    end 
    if $argc > 2
        help dis
    end
end
document dis
Disassemble a specified section of memory.
Default is to disassemble the function surrounding the PC (program counter)
of selected frame. 
With one argument, ADDR1, the function surrounding this address is dumped.
Two arguments are taken as a range of memory to dump.
Usage: dis <ADDR1> <ADDR2>
end




# __________hex/ascii dump an address_________
define ascii_char
    if $argc != 1
        help ascii_char
    else
        # thanks elaine :)
        set $_c = *(unsigned char *)($arg0)
        if ($_c < 0x20 || $_c > 0x7E)
            printf "."
        else
            printf "%c", $_c
        end
    end
end
document ascii_char
Print ASCII value of byte at address ADDR.
Print "." if the value is unprintable.
Usage: ascii_char ADDR
end


define hex_quad
    if $argc != 1
        help hex_quad
    else
        printf "%02X %02X %02X %02X %02X %02X %02X %02X", \
               *(unsigned char*)($arg0), *(unsigned char*)($arg0 + 1),     \
               *(unsigned char*)($arg0 + 2), *(unsigned char*)($arg0 + 3), \
               *(unsigned char*)($arg0 + 4), *(unsigned char*)($arg0 + 5), \
               *(unsigned char*)($arg0 + 6), *(unsigned char*)($arg0 + 7)
    end
end
document hex_quad
Print eight hexadecimal bytes starting at address ADDR.
Usage: hex_quad ADDR
end

define hexdump
    if $argc == 1
        hexdump_aux $arg0
	else
		if $argc == 2
			set $_count = 0
			while ($_count < $arg1)
				set $_i = ($_count * 0x10)
				hexdump_aux $data_addr+$_i
				set $_count++
			end
		else
			help hexdump
		end
    end
end
document hexdump
Display a 16-byte hex/ASCII dump of memory starting at address ADDR.
Optional parameter is the number of lines to display if you want more than one. 
Usage: hexdump ADDR [nr lines]
end

define hexdump_aux
    if $argc != 1
        help hexdump_aux
    else
        echo \033[1m
        if ($64BITS == 1)
         printf "0x%016lX : ", $arg0
        else
         printf "0x%08X : ", $arg0
        end
        echo \033[0m
        hex_quad $arg0
        echo \033[1m
        printf " - "
        echo \033[0m
        hex_quad $arg0+8
        printf " "
        echo \033[1m
        ascii_char $arg0+0x0
        ascii_char $arg0+0x1
        ascii_char $arg0+0x2
        ascii_char $arg0+0x3
        ascii_char $arg0+0x4
        ascii_char $arg0+0x5
        ascii_char $arg0+0x6
        ascii_char $arg0+0x7
        ascii_char $arg0+0x8
        ascii_char $arg0+0x9
        ascii_char $arg0+0xA
        ascii_char $arg0+0xB
        ascii_char $arg0+0xC
        ascii_char $arg0+0xD
        ascii_char $arg0+0xE
        ascii_char $arg0+0xF
        echo \033[0m
        printf "\n"
    end
end
document hexdump_aux
Display a 16-byte hex/ASCII dump of memory at address ADDR.
Usage: hexdump_aux ADDR
end


# _______________data window__________________
define ddump
    if $argc != 1
        help ddump
    else
        echo \033[34m
        if ($64BITS == 1)
         printf "[0x%04X:0x%016lX]", $ds, $data_addr
        else
         printf "[0x%04X:0x%08X]", $ds, $data_addr
        end
	echo \033[34m
	printf "------------------------"
    printf "-------------------------------"
    if ($64BITS == 1)
     printf "-------------------------------------"
	end

	echo \033[1;34m
	printf "[data]\n"
        echo \033[0m
        set $_count = 0
        while ($_count < $arg0)
            set $_i = ($_count * 0x10)
            hexdump $data_addr+$_i
            set $_count++
        end
    end
end
document ddump
Display NUM lines of hexdump for address in $data_addr global variable.
Usage: ddump NUM
end


define dd
    if $argc != 1
        help dd
    else
            set $data_addr = $arg0
            ddump 0x10
    end
end
document dd
Display 16 lines of a hex dump of address starting at ADDR.
Usage: dd ADDR
end


define datawin
 if ($64BITS == 1)
    if ((($rsi >> 0x18) == 0x40) || (($rsi >> 0x18) == 0x08) || (($rsi >> 0x18) == 0xBF))
        set $data_addr = $rsi
    else
        if ((($rdi >> 0x18) == 0x40) || (($rdi >> 0x18) == 0x08) || (($rdi >> 0x18) == 0xBF))
            set $data_addr = $rdi
        else
            if ((($rax >> 0x18) == 0x40) || (($rax >> 0x18) == 0x08) || (($rax >> 0x18) == 0xBF))
                set $data_addr = $rax
            else
                set $data_addr = $rsp
            end
        end
    end

 else
    if ((($esi >> 0x18) == 0x40) || (($esi >> 0x18) == 0x08) || (($esi >> 0x18) == 0xBF))
        set $data_addr = $esi
    else
        if ((($edi >> 0x18) == 0x40) || (($edi >> 0x18) == 0x08) || (($edi >> 0x18) == 0xBF))
            set $data_addr = $edi
        else
            if ((($eax >> 0x18) == 0x40) || (($eax >> 0x18) == 0x08) || (($eax >> 0x18) == 0xBF))
                set $data_addr = $eax
            else
                set $data_addr = $esp
            end
        end
    end
 end
    ddump $CONTEXTSIZE_DATA
end
document datawin
Display valid address from one register in data window.
Registers to choose are: esi, edi, eax, or esp.
end

################################
##### ALERT ALERT ALERT ########
################################
# Huge mess going here :) HAHA #
################################
define dumpjump
## grab the first two bytes from the instruction so we can determine the jump instruction
 set $_byte1 = *(unsigned char *)$pc
 set $_byte2 = *(unsigned char *)($pc+1)
## and now check what kind of jump we have (in case it's a jump instruction)
## I changed the flags routine to save the flag into a variable, so we don't need to repeat the process :) (search for "define flags")

## opcode 0x77: JA, JNBE (jump if CF=0 and ZF=0)
## opcode 0x0F87: JNBE, JA
 if ( ($_byte1 == 0x77) || ($_byte1 == 0x0F && $_byte2 == 0x87) )
 	# cf=0 and zf=0
 	if ($_cf_flag == 0 && $_zf_flag == 0)
		echo \033[31m
   		printf "  Jump is taken (c=0 and z=0)"
  	else
	# cf != 0 or zf != 0
   		echo \033[31m
   		printf "  Jump is NOT taken (c!=0 or z!=0)"
  	end 
 end

## opcode 0x73: JAE, JNB, JNC (jump if CF=0)
## opcode 0x0F83: JNC, JNB, JAE (jump if CF=0)
 if ( ($_byte1 == 0x73) || ($_byte1 == 0x0F && $_byte2 == 0x83) )
 	# cf=0
 	if ($_cf_flag == 0)
		echo \033[31m
   		printf "  Jump is taken (c=0)"
  	else
	# cf != 0
   		echo \033[31m
   		printf "  Jump is NOT taken (c!=0)"
  	end 
 end
 	
## opcode 0x72: JB, JC, JNAE (jump if CF=1)
## opcode 0x0F82: JNAE, JB, JC
 if ( ($_byte1 == 0x72) || ($_byte1 == 0x0F && $_byte2 == 0x82) )
 	# cf=1
 	if ($_cf_flag == 1)
		echo \033[31m
   		printf "  Jump is taken (c=1)"
  	else
	# cf != 1
   		echo \033[31m
   		printf "  Jump is NOT taken (c!=1)"
  	end 
 end

## opcode 0x76: JBE, JNA (jump if CF=1 or ZF=1)
## opcode 0x0F86: JBE, JNA
 if ( ($_byte1 == 0x76) || ($_byte1 == 0x0F && $_byte2 == 0x86) )
 	# cf=1 or zf=1
 	if (($_cf_flag == 1) || ($_zf_flag == 1))
		echo \033[31m
   		printf "  Jump is taken (c=1 or z=1)"
  	else
	# cf != 1 or zf != 1
   		echo \033[31m
   		printf "  Jump is NOT taken (c!=1 or z!=1)"
  	end 
 end

## opcode 0xE3: JCXZ, JECXZ, JRCXZ (jump if CX=0 or ECX=0 or RCX=0)
 if ($_byte1 == 0xE3)
 	# cx=0 or ecx=0
 	if (($ecx == 0) || ($cx == 0))
		echo \033[31m
   		printf "  Jump is taken (cx=0 or ecx=0)"
  	else
	#
   		echo \033[31m
   		printf "  Jump is NOT taken (cx!=0 or ecx!=0)"
  	end 
 end

## opcode 0x74: JE, JZ (jump if ZF=1)
## opcode 0x0F84: JZ, JE, JZ (jump if ZF=1)
 if ( ($_byte1 == 0x74) || ($_byte1 == 0x0F && $_byte2 == 0x84) )
 # ZF = 1
  	if ($_zf_flag == 1)
   		echo \033[31m
   		printf "  Jump is taken (z=1)"
  	else
 # ZF = 0
   		echo \033[31m
   		printf "  Jump is NOT taken (z!=1)"
  	end 
 end

## opcode 0x7F: JG, JNLE (jump if ZF=0 and SF=OF)
## opcode 0x0F8F: JNLE, JG (jump if ZF=0 and SF=OF)
 if ( ($_byte1 == 0x7F) || ($_byte1 == 0x0F && $_byte2 == 0x8F) )
 # zf = 0 and sf = of
  	if (($_zf_flag == 0) && ($_sf_flag == $_of_flag))
   		echo \033[31m
   		printf "  Jump is taken (z=0 and s=o)"
  	else
 #
   		echo \033[31m
   		printf "  Jump is NOT taken (z!=0 or s!=o)"
  	end 
 end

## opcode 0x7D: JGE, JNL (jump if SF=OF)
## opcode 0x0F8D: JNL, JGE (jump if SF=OF)
 if ( ($_byte1 == 0x7D) || ($_byte1 == 0x0F && $_byte2 == 0x8D) )
 # sf = of
  	if ($_sf_flag == $_of_flag)
   		echo \033[31m
   		printf "  Jump is taken (s=o)"
  	else
 #
   		echo \033[31m
   		printf "  Jump is NOT taken (s!=o)"
  	end 
 end

## opcode: 0x7C: JL, JNGE (jump if SF != OF)
## opcode: 0x0F8C: JNGE, JL (jump if SF != OF)
 if ( ($_byte1 == 0x7C) || ($_byte1 == 0x0F && $_byte2 == 0x8C) )
 # sf != of
  	if ($_sf_flag != $_of_flag)
   		echo \033[31m
   		printf "  Jump is taken (s!=o)"
  	else
 #
   		echo \033[31m
   		printf "  Jump is NOT taken (s=o)"
  	end 
 end

## opcode 0x7E: JLE, JNG (jump if ZF = 1 or SF != OF)
## opcode 0x0F8E: JNG, JLE (jump if ZF = 1 or SF != OF)
 if ( ($_byte1 == 0x7E) || ($_byte1 == 0x0F && $_byte2 == 0x8E) )
 # zf = 1 or sf != of
  	if (($_zf_flag == 1) || ($_sf_flag != $_of_flag))
   		echo \033[31m
   		printf "  Jump is taken (zf=1 or sf!=of)"
  	else
 #
   		echo \033[31m
   		printf "  Jump is NOT taken (zf!=1 or sf=of)"
  	end 
 end

## opcode 0x75: JNE, JNZ (jump if ZF = 0)
## opcode 0x0F85: JNE, JNZ (jump if ZF = 0)
 if ( ($_byte1 == 0x75) || ($_byte1 == 0x0F && $_byte2 == 0x85) )
 # ZF = 0
  	if ($_zf_flag == 0)
   		echo \033[31m
   		printf "  Jump is taken (z=0)"
  	else
 # ZF = 1
   		echo \033[31m
   		printf "  Jump is NOT taken (z!=0)"
  	end 
 end
 
## opcode 0x71: JNO (OF = 0)
## opcode 0x0F81: JNO (OF = 0)
 if ( ($_byte1 == 0x71) || ($_byte1 == 0x0F && $_byte2 == 0x81) )
 # OF = 0
	if ($_of_flag == 0)
   		echo \033[31m
   		printf "  Jump is taken (o=0)"
	else
 # OF != 0
   		echo \033[31m
   		printf "  Jump is NOT taken (o!=0)"
  	end 
 end

## opcode 0x7B: JNP, JPO (jump if PF = 0)
## opcode 0x0F8B: JPO (jump if PF = 0)
 if ( ($_byte1 == 0x7B) || ($_byte1 == 0x0F && $_byte2 == 0x8B) )
 # PF = 0
  	if ($_pf_flag == 0)
   		echo \033[31m
   		printf "  Jump is NOT taken (p=0)"
  	else
 # PF != 0
   		echo \033[31m
   		printf "  Jump is taken (p!=0)"
  	end 
 end

## opcode 0x79: JNS (jump if SF = 0)
## opcode 0x0F89: JNS (jump if SF = 0)
 if ( ($_byte1 == 0x79) || ($_byte1 == 0x0F && $_byte2 == 0x89) )
 # SF = 0
  	if ($_sf_flag == 0)
   		echo \033[31m
   		printf "  Jump is taken (s=0)"
  	else
 # SF != 0
   		echo \033[31m
   		printf "  Jump is NOT taken (s!=0)"
  	end 
 end

## opcode 0x70: JO (jump if OF=1)
## opcode 0x0F80: JO (jump if OF=1)
 if ( ($_byte1 == 0x70) || ($_byte1 == 0x0F && $_byte2 == 0x80) )
 # OF = 1
	if ($_of_flag == 1)
		echo \033[31m
   		printf "  Jump is taken (o=1)"
  	else
 # OF != 1
   		echo \033[31m
   		printf "  Jump is NOT taken (o!=1)"
  	end 
 end

## opcode 0x7A: JP, JPE (jump if PF=1)
## opcode 0x0F8A: JP, JPE (jump if PF=1)
 if ( ($_byte1 == 0x7A) || ($_byte1 == 0x0F && $_byte2 == 0x8A) )
 # PF = 1
  	if ($_pf_flag == 1)
   		echo \033[31m
   		printf "  Jump is taken (p=1)"
  	else
 # PF = 0
   		echo \033[31m
   		printf "  Jump is NOT taken (p!=1)"
  	end 
 end

## opcode 0x78: JS (jump if SF=1)
## opcode 0x0F88: JS (jump if SF=1)
 if ( ($_byte1 == 0x78) || ($_byte1 == 0x0F && $_byte2 == 0x88) )
 # SF = 1
	if ($_sf_flag == 1)
   		echo \033[31m
   		printf "  Jump is taken (s=1)"
  	else
 # SF != 1
   		echo \033[31m
   		printf "  Jump is NOT taken (s!=1)"
  	end 
 end

# end of dumpjump function
end
document dumpjump
Display if conditional jump will be taken or not
end

# _______________process context______________
# initialize variable
set $displayobjectivec = 0

define context 
    echo \033[34m
    if $SHOWCPUREGISTERS == 1
	    printf "----------------------------------------"
	    printf "----------------------------------"
	    if ($64BITS == 1)
	     printf "---------------------------------------------"
	    end
	    echo \033[34m\033[1m
	    printf "[regs]\n"
	    echo \033[0m
	    reg
	    echo \033[36m
    end
    if $SHOWSTACK == 1
	echo \033[34m
		if ($64BITS == 1)
		 printf "[0x%04X:0x%016lX]", $ss, $rsp
		else
    	 printf "[0x%04X:0x%08X]", $ss, $esp
    	end
        echo \033[34m
		printf "-------------------------"
    	printf "-----------------------------"
	    if ($64BITS == 1)
	     printf "-------------------------------------"
	    end
	echo \033[34m\033[1m
	printf "[stack]\n"
    	echo \033[0m
    	set $context_i = $CONTEXTSIZE_STACK
    	while ($context_i > 0)
       	 set $context_t = $sp + 0x10 * ($context_i - 1)
       	 hexdump $context_t
       	 set $context_i--
    	end
    end
# show the objective C message being passed to msgSend
   if $SHOWOBJECTIVEC == 1
#FIXME64
# What a piece of crap that's going on here :)
# detect if it's the correct opcode we are searching for
    	set $__byte1 = *(unsigned char *)$pc
    	set $__byte = *(int *)$pc
#
    	if ($__byte == 0x4244489)
      		set $objectivec = $eax
      		set $displayobjectivec = 1
    	end
#
    	if ($__byte == 0x4245489)
     		set $objectivec = $edx
     		set $displayobjectivec = 1
    	end
#
    	if ($__byte == 0x4244c89)
     		set $objectivec = $edx
     		set $displayobjectivec = 1
    	end
# and now display it or not (we have no interest in having the info displayed after the call)
    	if $__byte1 == 0xE8
     		if $displayobjectivec == 1
      			echo \033[34m
      			printf "--------------------------------------------------------------------"
  			    if ($64BITS == 1)
			     printf "---------------------------------------------"
	    		end
			echo \033[34m\033[1m
			printf "[ObjectiveC]\n"
      			echo \033[0m\033[30m
      			x/s $objectivec
     		end   
     		set $displayobjectivec = 0     
    	end
    	if $displayobjectivec == 1
      		echo \033[34m
      		printf "--------------------------------------------------------------------"
      		if ($64BITS == 1)
	     	 printf "---------------------------------------------"
	    	end
		echo \033[34m\033[1m
		printf "[ObjectiveC]\n"
      		echo \033[0m\033[30m
      		x/s $objectivec 
    	end   
   end
    echo \033[0m
# and this is the end of this little crap

    if $SHOWDATAWIN == 1
	 datawin
    end

    echo \033[34m
    printf "--------------------------------------------------------------------------"
    if ($64BITS == 1)
	 printf "---------------------------------------------"
	end
    echo \033[34m\033[1m
    printf "[code]\n"
    echo \033[0m
    set $context_i = $CONTEXTSIZE_CODE
    if ($context_i > 0)
    	if ($SETCOLOUR1STLINE == 1)	
	    	echo \033[32m
    	    x /i $pc
	        echo \033[0m
	    else
	    	x /i $pc
	    end
        set $context_i--
    end
    while ($context_i > 0)
        x /i
        set $context_i--
    end
    echo \033[34m
    printf "----------------------------------------"
    printf "----------------------------------------"
    if ($64BITS == 1)
     printf "---------------------------------------------\n"
	else
	 printf "\n"
	end

    echo \033[0m
end
document context
Print context window, i.e. regs, stack, ds:esi and disassemble cs:eip.
end


define context-on
    set $SHOW_CONTEXT = 1
    printf "Displaying of context is now ON\n"
end
document context-on
Enable display of context on every program break.
end


define context-off
    set $SHOW_CONTEXT = 0
    printf "Displaying of context is now OFF\n"
end
document context-off
Disable display of context on every program break.
end


# _______________process control______________
define n
    if $argc == 0
        nexti
    end
    if $argc == 1
        nexti $arg0
    end
    if $argc > 1
        help n
    end
end
document n
Step one instruction, but proceed through subroutine calls.
If NUM is given, then repeat it NUM times or till program stops.
This is alias for nexti.
Usage: n <NUM>
end


define go
    if $argc == 0
        stepi
    end
    if $argc == 1
        stepi $arg0
    end
    if $argc > 1
        help go
    end
end
document go
Step one instruction exactly.
If NUM is given, then repeat it NUM times or till program stops.
This is alias for stepi.
Usage: go <NUM>
end


define pret
    finish
end
document pret
Execute until selected stack frame returns (step out of current call).
Upon return, the value returned is printed and put in the value history.
end


define init
    set $SHOW_NEST_INSN = 0
    tbreak _init
    r
end
document init
Run program and break on _init().
end


define start
    set $SHOW_NEST_INSN = 0
    tbreak _start
    r
end
document start
Run program and break on _start().
end


define sstart
    set $SHOW_NEST_INSN = 0
    tbreak __libc_start_main
    r
end
document sstart
Run program and break on __libc_start_main().
Useful for stripped executables.
end


define main
    set $SHOW_NEST_INSN = 0
    tbreak main
    r
end
document main
Run program and break on main().
end

# FIXME64
#### WARNING ! WARNING !!
#### More more messy stuff starting !!!
#### I was thinking about how to do this and then it ocurred me that it could be as simple as this ! :)
define stepoframework
## we know that an opcode starting by 0xE8 has a fixed length
## for the 0xFF opcodes, we can enumerate what is possible to have
# first we grab the first 3 bytes from the current program counter
 set $_byte1 = *(unsigned char *)$pc
 set $_byte2 = *(unsigned char *)($pc+1)
 set $_byte3 = *(unsigned char *)($pc+2)
# and start the fun
# if it's a 0xE8 opcode, the total instruction size will be 5 bytes
# so we can simply calculate the next address and use a temporary breakpoint ! Voila :)
 set $_nextaddress = 0
 # this one is the must useful for us !!!
 if ($_byte1 == 0xE8)
  set $_nextaddress = $pc + 0x5
 else
   # just other cases we might be interested in... maybe this should be removed since the 0xE8 opcode is the one we will use more
   # this is a big fucking mess and can be improved for sure :) I don't like the way it is ehehehe
   if ($_byte1 == 0xFF)
    # call *%eax (0xFFD0) || call *%edx (0xFFD2) || call *(%ecx) (0xFFD1) || call (%eax) (0xFF10) || call *%esi (0xFFD6) || call *%ebx (0xFFD3) || call   DWORD PTR [edx] (0xFF12)
    if ($_byte2 == 0xD0 || $_byte2 == 0xD1 || $_byte2 == 0xD2 || $_byte2 == 0xD3 || $_byte2 == 0xD6 || $_byte2 == 0x10 || $_byte2 == 0x11 || $_byte2 == 0xD7 || $_byte2 == 0x12)
     set $_nextaddress = $pc + 0x2
    end
    # call *0x??(%ebp) (0xFF55??) || call *0x??(%esi) (0xFF56??) || call *0x??(%edi) (0xFF5F??) || call *0x??(%ebx)
    # call *0x??(%edx) (0xFF52??) || call *0x??(%ecx) (0xFF51??) || call *0x??(%edi) (0xFF57??) || call *0x??(%eax) (0xFF50??)
    if ($_byte2 == 0x55 || $_byte2 == 0x56 || $_byte2 == 0x5F || $_byte2 == 0x53 || $_byte2 == 0x52 || $_byte2 == 0x51 || $_byte2 == 0x57 || $_byte2 == 0x50) 
     set $_nextaddress = $pc + 0x3
    end
    # call *0x????????(%ebx) (0xFF93????????) || 
    if ($_byte2 == 0x93 || $_byte2 == 0x94 || $_byte2 == 0x90 || $_byte2 == 0x92)
     set $_nextaddress = $pc + 6
    end
    # call *0x????????(%ebx,%eax,4) (0xFF94??????????)
    if ($_byte2 == 0x94)
     set $_nextaddress = $pc + 7
    end
   end
 end
# if we have found a call to bypass we set a temporary breakpoint on next instruction and continue 
 if ($_nextaddress != 0)
  if ($arg0 == 1)
   thbreak *$_nextaddress
  else
   tbreak *$_nextaddress
  end
  continue
# else we just single step
 else
  nexti
 end
# end of stepo function
end

define stepo
 stepoframework 0
end
document stepo
Step over calls (interesting to bypass the ones to msgSend)
This function will set a temporary breakpoint on next instruction after the call so the call will be bypassed
You can safely use it instead nexti or n since it will single step code if it's not a call instruction (unless you want to go into the call function)
end

define stepoh
 stepoframework 1 
end
document stepoh
Same as stepo command but uses temporary hardware breakpoints
end

define skip
	x/2i $pc
	set $instruction_size = (int)($_ - $pc)
	set $pc = $pc + $instruction_size
	if ($SKIPEXECUTE == 1)
		if ($SKIPSTEP == 1)
			stepo
		else
			stepi
		end
	else
		context
	end
end
document skip
Skip over the instruction located at EIP/RIP. By default, the instruction will not be executed!
Some configurable options are available on top of gdbinit to override this.
end

# _______________eflags commands______________
define cfc
    if ($eflags & 1)
        set $eflags = $eflags&~0x1
    else
        set $eflags = $eflags|0x1
    end
end
document cfc
Change Carry Flag.
end


define cfp
    if (($eflags >> 2) & 1)
        set $eflags = $eflags&~0x4
    else
        set $eflags = $eflags|0x4
    end
end
document cfp
Change Parity Flag.
end


define cfa
    if (($eflags >> 4) & 1)
        set $eflags = $eflags&~0x10
    else
        set $eflags = $eflags|0x10
    end
end
document cfa
Change Auxiliary Carry Flag.
end


define cfz
    if (($eflags >> 6) & 1)
        set $eflags = $eflags&~0x40
    else
        set $eflags = $eflags|0x40
    end
end
document cfz
Change Zero Flag.
end


define cfs
    if (($eflags >> 7) & 1)
        set $eflags = $eflags&~0x80
    else
        set $eflags = $eflags|0x80
    end
end
document cfs
Change Sign Flag.
end


define cft
    if (($eflags >>8) & 1)
        set $eflags = $eflags&~0x100
    else
        set $eflags = $eflags|0x100
    end
end
document cft
Change Trap Flag.
end


define cfi
    if (($eflags >> 9) & 1)
        set $eflags = $eflags&~0x200
    else
        set $eflags = $eflags|0x200
    end
end
document cfi
Change Interrupt Flag.
Only privileged applications (usually the OS kernel) may modify IF.
This only applies to protected mode (real mode code may always modify IF).
end


define cfd
    if (($eflags >>0xA) & 1)
        set $eflags = $eflags&~0x400
    else
        set $eflags = $eflags|0x400
    end
end
document cfd
Change Direction Flag.
end


define cfo
    if (($eflags >> 0xB) & 1)
        set $eflags = $eflags&~0x800
    else
        set $eflags = $eflags|0x800
    end
end
document cfo
Change Overflow Flag.
end



# ____________________patch___________________
define nop
    if ($argc > 2 || $argc == 0)
        help nop
    end
    
    if ($argc == 1)
    	set *(unsigned char *)$arg0 = 0x90
    else
	set $addr = $arg0
	while ($addr < $arg1)
		set *(unsigned char *)$addr = 0x90
		set $addr = $addr + 1
	end
    end
end
document nop
Usage: nop ADDR1 [ADDR2]
Patch a single byte at address ADDR1, or a series of bytes between ADDR1 and ADDR2 to a NOP (0x90) instruction.
end


define null
    if ( $argc >2 || $argc == 0)
        help null
    end
 
    if ($argc == 1)
	set *(unsigned char *)$arg0 = 0
    else
	set $addr = $arg0
	while ($addr < $arg1)
	        set *(unsigned char *)$addr = 0
		set $addr = $addr +1
	end
    end
end
document null
Usage: null ADDR1 [ADDR2]
Patch a single byte at address ADDR1 to NULL (0x00), or a series of bytes between ADDR1 and ADDR2.

end

define int3
    if $argc != 1
        help int3
    else
# save original bytes and address
        set $ORIGINAL_INT3BYTE = *(unsigned char *)$arg0
# patch
        set $ORIGINAL_INT3ADDRESS = $arg0

        set *(unsigned char *)$arg0 = 0xCC
    end
end
document int3
Patch byte at address ADDR to an INT3 (0xCC) instruction.
Usage: int3 ADDR
end

define rint3
	set *(unsigned char *)$ORIGINAL_INT3ADDRESS = $ORIGINAL_INT3BYTE
	set $eip = $ORIGINAL_INT3ADDRESS
end
document rint3
Restore the original byte previous to 0xCC patch issued with "int3" command
end



# ____________________cflow___________________
define print_insn_type
    if $argc != 1
        help print_insn_type
    else
        if ($arg0 < 0 || $arg0 > 5)
            printf "UNDEFINED/WRONG VALUE"
        end
        if ($arg0 == 0)
            printf "UNKNOWN"
        end
        if ($arg0 == 1)
            printf "JMP"
        end
        if ($arg0 == 2)
            printf "JCC"
        end
        if ($arg0 == 3)
            printf "CALL"
        end
        if ($arg0 == 4)
            printf "RET"
        end
        if ($arg0 == 5)
            printf "INT"
        end
    end
end
document print_insn_type
Print human-readable mnemonic for the instruction type (usually $INSN_TYPE).
Usage: print_insn_type INSN_TYPE_NUMBER
end


define get_insn_type
    if $argc != 1
        help get_insn_type
    else
        set $INSN_TYPE = 0
        set $_byte1 = *(unsigned char *)$arg0
        if ($_byte1 == 0x9A || $_byte1 == 0xE8)
            # "call"
            set $INSN_TYPE = 3
        end
        if ($_byte1 >= 0xE9 && $_byte1 <= 0xEB)
            # "jmp"
            set $INSN_TYPE = 1
        end
        if ($_byte1 >= 0x70 && $_byte1 <= 0x7F)
            # "jcc"
            set $INSN_TYPE = 2
        end
        if ($_byte1 >= 0xE0 && $_byte1 <= 0xE3 )
            # "jcc"
            set $INSN_TYPE = 2
        end
        if ($_byte1 == 0xC2 || $_byte1 == 0xC3 || $_byte1 == 0xCA || \
            $_byte1 == 0xCB || $_byte1 == 0xCF)
            # "ret"
            set $INSN_TYPE = 4
        end
        if ($_byte1 >= 0xCC && $_byte1 <= 0xCE)
            # "int"
            set $INSN_TYPE = 5
        end
        if ($_byte1 == 0x0F )
            # two-byte opcode
            set $_byte2 = *(unsigned char *)($arg0 + 1)
            if ($_byte2 >= 0x80 && $_byte2 <= 0x8F)
                # "jcc"
                set $INSN_TYPE = 2
            end
        end
        if ($_byte1 == 0xFF)        
            # opcode extension
            set $_byte2 = *(unsigned char *)($arg0 + 1)
            set $_opext = ($_byte2 & 0x38)
            if ($_opext == 0x10 || $_opext == 0x18) 
                # "call"
                set $INSN_TYPE = 3
            end
            if ($_opext == 0x20 || $_opext == 0x28)
                # "jmp"
                set $INSN_TYPE = 1
            end
        end
    end
end
document get_insn_type
Recognize instruction type at address ADDR.
Take address ADDR and set the global $INSN_TYPE variable to
0, 1, 2, 3, 4, 5 if the instruction at that address is
unknown, a jump, a conditional jump, a call, a return, or an interrupt.
Usage: get_insn_type ADDR
end


define step_to_call
    set $_saved_ctx = $SHOW_CONTEXT
    set $SHOW_CONTEXT = 0
    set $SHOW_NEST_INSN = 0
 
    set logging file /dev/null
    set logging redirect on
    set logging on
 
    set $_cont = 1
    while ($_cont > 0)
        stepi
        get_insn_type $pc
        if ($INSN_TYPE == 3)
            set $_cont = 0
        end
    end

    set logging off

    if ($_saved_ctx > 0)
        context
    end

    set $SHOW_CONTEXT = $_saved_ctx
    set $SHOW_NEST_INSN = 0
 
    set logging file ~/gdb.txt
    set logging redirect off
    set logging on
 
    printf "step_to_call command stopped at:\n  "
    x/i $pc
    printf "\n"
    set logging off

end
document step_to_call
Single step until a call instruction is found.
Stop before the call is taken.
Log is written into the file ~/gdb.txt.
end


define trace_calls

    printf "Tracing...please wait...\n"

    set $_saved_ctx = $SHOW_CONTEXT
    set $SHOW_CONTEXT = 0
    set $SHOW_NEST_INSN = 0
    set $_nest = 1
    set listsize 0
  
    set logging overwrite on
    set logging file ~/gdb_trace_calls.txt
    set logging on
    set logging off
    set logging overwrite off

    while ($_nest > 0)
        get_insn_type $pc
        # handle nesting
        if ($INSN_TYPE == 3)
            set $_nest = $_nest + 1
        else
            if ($INSN_TYPE == 4)
                set $_nest = $_nest - 1
            end
        end
        # if a call, print it
        if ($INSN_TYPE == 3)
            set logging file ~/gdb_trace_calls.txt
            set logging redirect off
            set logging on

            set $x = $_nest - 2
            while ($x > 0)
                printf "\t"
                set $x = $x - 1
            end
            x/i $pc
        end

        set logging off
        set logging file /dev/null
        set logging redirect on
        set logging on
        stepi
        set logging redirect off
        set logging off
    end

    set $SHOW_CONTEXT = $_saved_ctx
    set $SHOW_NEST_INSN = 0
 
    printf "Done, check ~/gdb_trace_calls.txt\n"
end
document trace_calls
Create a runtime trace of the calls made by target.
Log overwrites(!) the file ~/gdb_trace_calls.txt.
end


define trace_run
 
    printf "Tracing...please wait...\n"

    set $_saved_ctx = $SHOW_CONTEXT
    set $SHOW_CONTEXT = 0
    set $SHOW_NEST_INSN = 1
    set logging overwrite on
    set logging file ~/gdb_trace_run.txt
    set logging redirect on
    set logging on
    set $_nest = 1

    while ( $_nest > 0 )

        get_insn_type $pc
        # jmp, jcc, or cll
        if ($INSN_TYPE == 3)
            set $_nest = $_nest + 1
        else
            # ret
            if ($INSN_TYPE == 4)
                set $_nest = $_nest - 1
            end
        end
        stepi
    end

    printf "\n"

    set $SHOW_CONTEXT = $_saved_ctx
    set $SHOW_NEST_INSN = 0
    set logging redirect off
    set logging off

    # clean up trace file
    shell  grep -v ' at ' ~/gdb_trace_run.txt > ~/gdb_trace_run.1
    shell  grep -v ' in ' ~/gdb_trace_run.1 > ~/gdb_trace_run.txt
    shell  rm -f ~/gdb_trace_run.1
    printf "Done, check ~/gdb_trace_run.txt\n"
end
document trace_run
Create a runtime trace of target.
Log overwrites(!) the file ~/gdb_trace_run.txt.
end

#define ptraceme
#    catch syscall ptrace
#    commands
#        if ($64BITS == 0)
#            if ($ebx == 0)
#	        set $eax = 0
#                continue
#            end
#        else
#            if ($rdi == 0)
#                set $rax = 0
#                continue
#            end
#        end
#    end
#    set $ptrace_bpnum = $bpnum
#end
#document ptraceme
#Hook ptrace to bypass PTRACE_TRACEME anti debugging technique
#end

define rptraceme
    if ($ptrace_bpnum != 0)
        delete $ptrace_bpnum
        set $ptrace_bpnum = 0
    end
end
document rptraceme
Remove ptrace hook
end


# ____________________misc____________________
define hook-stop

    # this makes 'context' be called at every BP/step
    if ($SHOW_CONTEXT > 0)
        context
    end
    if ($SHOW_NEST_INSN > 0)
        set $x = $_nest
        while ($x > 0)
            printf "\t"
            set $x = $x - 1
        end
    end
end
document hook-stop
!!! FOR INTERNAL USE ONLY - DO NOT CALL !!!
end

# original by Tavis Ormandy (http://my.opera.com/taviso/blog/index.dml/tag/gdb) (great fix!)
# modified to work with Mac OS X by fG!
# seems nasm shipping with Mac OS X has problems accepting input from stdin or heredoc
# input is read into a variable and sent to a temporary file which nasm can read
define assemble
 # dont enter routine again if user hits enter
 dont-repeat
 if ($argc)
  if (*$arg0 = *$arg0)
    # check if we have a valid address by dereferencing it,
    # if we havnt, this will cause the routine to exit.
  end
  printf "Instructions will be written to %#x.\n", $arg0
 else
  printf "Instructions will be written to stdout.\n"
 end
 printf "Type instructions, one per line."
 echo \033[1m
 printf " Do not forget to use NASM assembler syntax!\n"
 echo \033[0m
 printf "End with a line saying just \"end\".\n"
 if ($argc)
	if ($64BITS == 1)
		# argument specified, assemble instructions into memory at address specified.
		shell ASMOPCODE="$(while read -ep '>' r && test "$r" != end ; do echo -E "$r"; done)" ; GDBASMFILENAME=$RANDOM; \
		echo -e "BITS 64\n$ASMOPCODE" >/tmp/$GDBASMFILENAME ; /usr/local/bin/nasm -f bin -o /dev/stdout /tmp/$GDBASMFILENAME | /usr/bin/hexdump -ve '1/1 "set *((unsigned char *) $arg0 + %#2_ax) = %#02x\n"' >/tmp/gdbassemble ; /bin/rm -f /tmp/$GDBASMFILENAME
		source /tmp/gdbassemble
		# all done. clean the temporary file
		shell /bin/rm -f /tmp/gdbassemble
	else
		# argument specified, assemble instructions into memory at address specified.
		shell ASMOPCODE="$(while read -ep '>' r && test "$r" != end ; do echo -E "$r"; done)" ; GDBASMFILENAME=$RANDOM; \
		echo -e "BITS 32\n$ASMOPCODE" >/tmp/$GDBASMFILENAME ; /usr/bin/nasm -f bin -o /dev/stdout /tmp/$GDBASMFILENAME | /usr/bin/hexdump -ve '1/1 "set *((unsigned char *) $arg0 + %#2_ax) = %#02x\n"' >/tmp/gdbassemble ; /bin/rm -f /tmp/$GDBASMFILENAME
		source /tmp/gdbassemble
		# all done. clean the temporary file
		shell /bin/rm -f /tmp/gdbassemble
	end
 else
	if ($64BITS == 1)
		# no argument, assemble instructions to stdout
		shell ASMOPCODE="$(while read -ep '>' r && test "$r" != end ; do echo -E "$r"; done)" ; GDBASMFILENAME=$RANDOM; \
		echo -e "BITS 64\n$ASMOPCODE" >/tmp/$GDBASMFILENAME ; /usr/local/bin/nasm -f bin -o /dev/stdout /tmp/$GDBASMFILENAME | /usr/local/bin/ndisasm -i -b64 /dev/stdin ; \
		/bin/rm -f /tmp/$GDBASMFILENAME

	else
		# no argument, assemble instructions to stdout
		shell ASMOPCODE="$(while read -ep '>' r && test "$r" != end ; do echo -E "$r"; done)" ; GDBASMFILENAME=$RANDOM; \
		echo -e "BITS 32\n$ASMOPCODE" >/tmp/$GDBASMFILENAME ; /usr/bin/nasm -f bin -o /dev/stdout /tmp/$GDBASMFILENAME | /usr/bin/ndisasm -i -b32 /dev/stdin ; \
		/bin/rm -f /tmp/$GDBASMFILENAME
	end
 end
end
document assemble
Assemble instructions using nasm.
Type a line containing "end" to indicate the end.
If an address is specified, insert/modify instructions at that address.
If no address is specified, assembled instructions are printed to stdout.
Use the pseudo instruction "org ADDR" to set the base address.
end

define asm
	if $argc == 1
		assemble $arg0
	else
		assemble
	end
end
document asm
Shortcut to the asssemble command
end

define assemble_gas
    printf "\nType code to assemble and hit Ctrl-D when finished.\n"
    printf "You must use GNU assembler (AT&T) syntax.\n"

    shell filename=$(mktemp); \
          binfilename=$(mktemp); \
          echo -e "Writing into: ${filename}\n"; \
          cat > $filename; echo ""; \
          as -o $binfilename < $filename; \
          objdump -d -j .text $binfilename; \
          rm -f $binfilename; \
          rm -f $filename; \
          echo -e "temporaly files deleted.\n"
end
document assemble_gas
Assemble instructions to binary opcodes. Uses GNU as and objdump.
Usage: assemble_gas
end


define dump_hexfile
    dump ihex memory $arg0 $arg1 $arg2
end
document dump_hexfile
Write a range of memory to a file in Intel ihex (hexdump) format.
The range is specified by ADDR1 and ADDR2 addresses.
Usage: dump_hexfile FILENAME ADDR1 ADDR2
end


define dump_binfile
    dump memory $arg0 $arg1 $arg2
end
document dump_binfile
Write a range of memory to a binary file.
The range is specified by ADDR1 and ADDR2 addresses.
Usage: dump_binfile FILENAME ADDR1 ADDR2
end


define cls
    shell clear
end
document cls
Clear screen.
end

define search
 set $start = (char *) $arg0
 set $end = (char *) $arg1
 set $pattern = (short) $arg2
 set $p = $start
 while $p < $end
   if (*(short *) $p) == $pattern
     printf "pattern 0x%hx found at 0x%x\n", $pattern, $p
   end
   set $p++
 end
end
document search
Search for the given pattern beetween $start and $end address
Usage: search <start> <end> <pattern>
end

# _________________user tips_________________
# The 'tips' command is used to provide tutorial-like info to the user
define tips
    printf "Tip Topic Commands:\n"
    printf "\ttip_display : Automatically display values on each break\n"
    printf "\ttip_patch   : Patching binaries\n"
    printf "\ttip_strip   : Dealing with stripped binaries\n"
    printf "\ttip_syntax  : AT&T vs Intel syntax\n"
end
document tips
Provide a list of tips from users on various topics.
end


define tip_patch
    printf "\n"
    printf "                   PATCHING MEMORY\n"
    printf "Any address can be patched using the 'set' command:\n"
    printf "\t`set ADDR = VALUE` \te.g. `set *0x8049D6E = 0x90`\n"
    printf "\n"
    printf "                 PATCHING BINARY FILES\n"
    printf "Use `set write` in order to patch the target executable\n"
    printf "directly, instead of just patching memory\n"
    printf "\t`set write on` \t`set write off`\n"
    printf "Note that this means any patches to the code or data segments\n"
    printf "will be written to the executable file\n"
    printf "When either of these commands has been issued,\n"
    printf "the file must be reloaded.\n"
    printf "\n"
end
document tip_patch
Tips on patching memory and binary files.
end


define tip_strip
    printf "\n"
    printf "             STOPPING BINARIES AT ENTRY POINT\n"
    printf "Stripped binaries have no symbols, and are therefore tough to\n"
    printf "start automatically. To debug a stripped binary, use\n"
    printf "\tinfo file\n"
    printf "to get the entry point of the file\n"
    printf "The first few lines of output will look like this:\n"
    printf "\tSymbols from '/tmp/a.out'\n"
    printf "\tLocal exec file:\n"
    printf "\t        `/tmp/a.out', file type elf32-i386.\n"
    printf "\t        Entry point: 0x80482e0\n"
    printf "Use this entry point to set an entry point:\n"
    printf "\t`tbreak *0x80482e0`\n"
    printf "The breakpoint will delete itself after the program stops as\n"
    printf "the entry point\n"
    printf "\n"
end
document tip_strip
Tips on dealing with stripped binaries.
end


define tip_syntax
    printf "\n"
    printf "\t    INTEL SYNTAX                        AT&T SYNTAX\n"
    printf "\tmnemonic dest, src, imm            mnemonic src, dest, imm\n" 
    printf "\t[base+index*scale+disp]            disp(base, index, scale)\n"
    printf "\tregister:      eax                 register:      %%eax\n"
    printf "\timmediate:     0xFF                immediate:     $0xFF\n"
    printf "\tdereference:   [addr]              dereference:   addr(,1)\n"
    printf "\tabsolute addr: addr                absolute addr: *addr\n"
    printf "\tbyte insn:     mov byte ptr        byte insn:     movb\n"
    printf "\tword insn:     mov word ptr        word insn:     movw\n"
    printf "\tdword insn:    mov dword ptr       dword insn:    movd\n"
    printf "\tfar call:      call far            far call:      lcall\n"
    printf "\tfar jump:      jmp far             far jump:      ljmp\n"
    printf "\n"
    printf "Note that order of operands in reversed, and that AT&T syntax\n"
    printf "requires that all instructions referencing memory operands \n"
    printf "use an operand size suffix (b, w, d, q)\n"
    printf "\n"
end
document tip_syntax
Summary of Intel and AT&T syntax differences.
end


define tip_display
    printf "\n"
    printf "Any expression can be set to automatically be displayed every time\n"
    printf "the target stops. The commands for this are:\n"
    printf "\t`display expr'     : automatically display expression 'expr'\n"
    printf "\t`display'          : show all displayed expressions\n"
    printf "\t`undisplay num'    : turn off autodisplay for expression # 'num'\n"
    printf "Examples:\n"
    printf "\t`display/x *(int *)$esp`      : print top of stack\n"
    printf "\t`display/x *(int *)($ebp+8)`  : print first parameter\n"
    printf "\t`display (char *)$esi`        : print source string\n"
    printf "\t`display (char *)$edi`        : print destination string\n"
    printf "\n"
end
document tip_display
Tips on automatically displaying values when a program stops.
end

# bunch of semi-useless commands

# enable and disable shortcuts for stop-on-solib-events fantastic trick!
define enablesolib
	set stop-on-solib-events 1
	printf "Stop-on-solib-events is enabled!\n"
end
document enablesolib
Shortcut to enable stop-on-solib-events trick!
end

define disablesolib
	set stop-on-solib-events 0
	printf "Stop-on-solib-events is disabled!\n"
end
document disablesolib
Shortcut to disable stop-on-solib-events trick!
end

# enable commands for different displays
define enableobjectivec
	set $SHOWOBJECTIVEC = 1
end
document enableobjectivec
Enable display of objective-c information in the context window
end

define enablecpuregisters
	set $SHOWCPUREGISTERS = 1
end
document enablecpuregisters
Enable display of cpu registers in the context window
end

define enablestack
	set $SHOWSTACK = 1
end
document enablestack
Enable display of stack in the context window
end

define enabledatawin
	set $SHOWDATAWIN = 1
end
document enabledatawin
Enable display of data window in the context window
end

# disable commands for different displays
define disableobjectivec
	set $SHOWOBJECTIVEC = 0
end
document disableobjectivec
Disable display of objective-c information in the context window
end

define disablecpuregisters
	set $SHOWCPUREGISTERS = 0
end
document disablecpuregisters
Disable display of cpu registers in the context window
end

define disablestack
	set $SHOWSTACK = 0
end
document disablestack
Disable display of stack information in the context window
end

define disabledatawin
	set $SHOWDATAWIN = 0
end
document disabledatawin
Disable display of data window in the context window
end

define 32bits
	set $64BITS = 0
end
document 32bits
Set gdb to work with 32bits binaries
end

define 64bits
	set $64BITS = 1
end
document 64bits
Set gdb to work with 64bits binaries
end

define enablelib
	set stop-on-solib-events 1
end
document enablelib
Activate stop-on-solib-events
end

define disablelib
	set stop-on-solib-events 0
end
document disablelib
Deactivate stop-on-solib-events
end
