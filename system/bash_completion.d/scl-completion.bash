# main function bound to scl command
_scl()
{
    local cur actions cur_action collections
    COMPREPLY=()

    cur="${COMP_WORDS[COMP_CWORD]}"
    actions="enable run load unload list-collections list-packages man register deregister --help"

    collections=`scl list-collections`

    # Complete action names
    if ((COMP_CWORD == 1)); then
        COMPREPLY=( $(compgen -W "${actions}" -- ${cur}) )
        return 0;
    fi

    # If there is command or separator in arguments then stop completition
    if ((COMP_CWORD > 3)); then
        for word in "${COMP_WORDS[@]}"; do
            if [[ ${word} == \'* || ${word} == \"* || ${word} == "--" ]] ; then
                return 0
            fi
        done
    fi

    # Complete one or none action argument
    if ((COMP_CWORD >= 2)); then
        cur_action="${COMP_WORDS[1]}"

        case "$cur_action" in
            # No argument
            list-collections|--help)
                return 0
            ;;

            # Argument is collection name
            list-packages|man)
                if ((COMP_CWORD == 2)); then
                    COMPREPLY=( $(compgen -W  "$collections" -- ${cur}) )
                fi
                return 0
            ;;

            # Argument is collection name or "-f" or "--force"
            deregister)
                if ((COMP_CWORD == 2)); then
                    COMPREPLY=( $(compgen -W  "$collections --force -f" -- ${cur}))
                fi
                if [ "$COMP_CWORD" -eq 3 -a  "(" "${COMP_WORDS[2]}" == "--force" -o "${COMP_WORDS[2]}" == "-f" ")" ]; then
                    COMPREPLY=( $(compgen -W  "$collections" -- ${cur}))
                fi
                return 0
            ;;

            # Argument is directory
            register)
                compopt -o plusdirs
                if ((COMP_CWORD == 2)); then
                    COMPREPLY=( $(compgen -A  directory -- ${cur}) )
                fi
                return 0
            ;;

            # Arguments are collections or "-x" or "--exec"
            run|enable)
                if ((COMP_CWORD == 2)); then
                    COMPREPLY=( $(compgen -W  "$collections -x --exec" -- ${cur}) )
                else
                    COMPREPLY=( $(compgen -W  "$collections" -- ${cur}) )
                fi
                return 0
            ;;

            # Arguments are collections 
            load|unload)
                COMPREPLY=( $(compgen -W  "$collections" -- ${cur}) )
                return 0
            ;;
            *)
            ;;
        esac
    fi

}

# bind the scl command to the _scl function for completion
complete -F _scl scl
