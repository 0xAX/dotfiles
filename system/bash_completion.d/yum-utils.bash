# need functions from yum.bash
type -t _yum >/dev/null || . $(pkg-config --variable=completionsdir bash-completion)/yum

# bash completion for yum-utils

_yu_init_completion()
{
    if declare -F _get_comp_words_by_ref &>/dev/null ; then
        _get_comp_words_by_ref -n = cur prev words
    else
        cur=$1 prev=$2 words=("${COMP_WORDS[@]}")
    fi
    declare -F _split_longopt &>/dev/null && _split_longopt && split=true
}

# repomanage
_yu_repomanage()
{
    local cur prev words=() split=false
    _yu_init_completion "$2" "$3"

    case $prev in
        -h|--help)
            return 0
            ;;
        -k|--keep)
            COMPREPLY=( $( compgen -W '1 2 3 4 5 6 7 8 9' -- "$cur" ) )
            return 0
            ;;
    esac

    $split && return 0

    if [[ $cur == -* ]] ; then
        COMPREPLY=( $( compgen -W '--old --new --space --keep --nocheck
            --help' -- "$cur" ) )
        return 0
    fi

    local IFS=$'\n'
    COMPREPLY=( $( compgen -d -- "$cur" ) )
} &&
complete -F _yu_repomanage -o filenames repomanage repomanage.py

# package-cleanup
_yu_package_cleanup()
{
    local cur prev words=() split=false
    _yu_init_completion "$2" "$3"

    _yum_complete_baseopts "$cur" "$prev" 2>/dev/null && return 0

    case $prev in
        --leaf-regex|--qf|--queryformat)
            return 0
            ;;
        --count)
            COMPREPLY=( $( compgen -W '1 2 3 4 5 6 7 8 9' -- "$cur" ) )
            return 0
            ;;
    esac

    $split && return 0

    COMPREPLY=( $( compgen -W '$( _yum_baseopts 2>/dev/null ) --problems
        --queryformat --orphans --dupes --cleandupes --noscripts --leaves --all
        --leaf-regex --exclude-devel --exclude-bin --oldkernels --count
        --keepdevel' -- "$cur" ) )
} &&
complete -F _yu_package_cleanup -o filenames package-cleanup package-cleanup.py

# verifytree
_yu_verifytree()
{
    local cur prev words=() split=false
    _yu_init_completion "$2" "$3"

    case $prev in
        -h|--help|-t|--testopia)
            return 0
            ;;
    esac

    $split && return 0

    if [[ $cur == -* ]] ; then
        COMPREPLY=( $( compgen -W '--help --checkall --testopia --treeinfo' \
            -- "$cur" ) )
        return 0
    fi

    local IFS=$'\n'
    COMPREPLY=( $( compgen -d -- "$cur" ) )
} &&
complete -F _yu_verifytree -o filenames verifytree verifytree.py

# repo-graph
_yu_repo_graph()
{
    local cur prev words=() split=false
    _yu_init_completion "$2" "$3"

    case $prev in
        -h|--help)
            return 0
            ;;
        --repoid)
            _yum_helper repolist all "$cur" 2>/dev/null
            return 0
            ;;
        -c)
            local IFS=$'\n'
            COMPREPLY=( $( compgen -f -o plusdirs -X '!*.conf' -- "$cur" ) )
            return 0
            ;;
    esac

    $split && return 0

    COMPREPLY=( $( compgen -W '--help --repoid -c' -- "$cur" ) )
} &&
complete -F _yu_repo_graph -o filenames repo-graph repo-graph.py

# repo-rss
_yu_repo_rss()
{
    COMPREPLY=()

    case $3 in
        -h|--help|-l|-t|-d|-r|-a)
            return 0
            ;;
        -f)
            local IFS=$'\n'
            COMPREPLY=( $( compgen -f -o plusdirs -X '!*.xml' -- "$cur" ) )
            return 0
            ;;
        -c)
            local IFS=$'\n'
            COMPREPLY=( $( compgen -f -o plusdirs -X '!*.conf' -- "$cur" ) )
            return 0
            ;;
    esac

    COMPREPLY=( $( compgen -W '--help -f -l -t -d -r --tempcache -g -a -c' \
        -- "$2" ) )
    [[ $2 == -* ]] || _yum_helper repolist all "$2" 2>/dev/null || return 0
} &&
complete -F _yu_repo_rss -o filenames repo-rss repo-rss.py

# repoclosure
_yu_repoclosure()
{
    local cur prev words=() split=false
    _yu_init_completion "$2" "$3"

    case $prev in
        -h|--help|-a|--arch|--basearch|--repofrompath)
            return 0
            ;;
        -c|--config)
            local IFS=$'\n'
            COMPREPLY=( $( compgen -f -o plusdirs -X '!*.conf' -- "$cur" ) )
            return 0
            ;;
        -l|--lookaside|-r|--repoid)
            _yum_helper repolist all "$cur" 2>/dev/null
            return 0
            ;;
        -p|--pkg)
            _yum_list all "$cur" 2>/dev/null
            return 0
            ;;
        -g|--group)
            _yum_helper groups list all "$cur" 2>/dev/null
            return 0
            ;;
    esac

    $split && return 0

    COMPREPLY=( $( compgen -W '--help --config --arch --basearch --builddeps
        --lookaside --repoid --tempcache --quiet --newest --repofrompath --pkg
        --group' -- "$cur" ) )
} &&
complete -F _yu_repoclosure -o filenames repoclosure repoclosure.py

# repoquery
_yu_repoquery()
{
    local cur prev words=() split=false
    _yu_init_completion "$2" "$3"

    local word groupmode=false
    for word in "${words[@]}" ; do
        case $word in -g|--group) groupmode=true ; break ;; esac
    done

    case $prev in
        -h|--help|--version|--qf|--queryformat|--archlist|--repofrompath|\
        --setopt)
            return 0
            ;;
        -f|--file)
            local IFS=$'\n'
            COMPREPLY=( $( compgen -f -o plusdirs -- "$cur" ) )
            return 0
            ;;
        -l|--list|-i|--info|-R|--requires)
            if $groupmode ; then
                _yum_helper groups list all "$cur" 2>/dev/null
            else
                declare -F _yum_atgroups &>/dev/null && \
                    _yum_atgroups "$cur" || _yum_list all "$cur" 2>/dev/null
            fi
            return 0
            ;;
        --grouppkgs)
            COMPREPLY=( $( compgen -W 'all default optional mandatory' \
                -- "$cur" ) )
            return 0
            ;;
        --pkgnarrow)
            COMPREPLY=( $( compgen -W 'all available updates installed extras
                obsoletes recent repos' -- "$cur" ) )
            return 0
            ;;
        --repoid)
            _yum_helper repolist all "$cur" 2>/dev/null
            return 0
            ;;
        --enablerepo)
            _yum_helper repolist disabled "$cur" 2>/dev/null
            return 0
            ;;
        --disablerepo)
            _yum_helper repolist enabled "$cur" 2>/dev/null
            return 0
            ;;
        -c|--config)
            local IFS=$'\n'
            COMPREPLY=( $( compgen -f -o plusdirs -X '!*.conf' -- "$cur" ) )
            return 0
            ;;
        --level)
            COMPREPLY=( $( compgen -W '{1..9} all' -- "$cur" ) )
            return 0
            ;;
        --output)
            COMPREPLY=( $( compgen -W 'text ascii-tree dot-tree' -- "$cur" ) )
            return 0
            ;;
        --search-fields)
            COMPREPLY=( $( compgen -W 'name summary description' -- "$cur" ) )
            return 0
            ;;
        --installroot)
            local IFS=$'\n'
            COMPREPLY=( $( compgen -d -- "$cur" ) )
            return 0
            ;;
    esac

    $split && return 0

    if [[ $cur == -* ]] ; then
        COMPREPLY=( $( compgen -W '--version --help --list --info --file
            --queryformat --groupmember --all --requires --provides --obsoletes
            --conflicts --changelog --location --nevra --envra --nvr --source
            --srpm --resolve --exactdeps --recursive --whatprovides
            --whatrequires --whatobsoletes --whatconflicts --group --grouppkgs
            --archlist --pkgnarrow --installed --show-duplicates --repoid
            --enablerepo --disablerepo --repofrompath --plugins --quiet
            --verbose --cache --tempcache --querytags --config --level --output
            --search --search-fields --setopt --installroot' -- "$cur" ) )
        return 0
    fi

    declare -F _yum_atgroups &>/dev/null && \
        _yum_atgroups "$cur" || _yum_list all "$cur" 2>/dev/null
} &&
complete -F _yu_repoquery -o filenames repoquery repoquery.py

# yumdb
_yu_yumdb()
{
    local cur prev words=() split=false
    _yu_init_completion "$2" "$3"

    case $prev in
        -h|--help|-version)
            return 0
            ;;
        -c|--config)
            local IFS=$'\n'
            COMPREPLY=( $( compgen -f -o plusdirs -X '!*.conf' -- "$cur" ) )
            return 0
            ;;
        shell)
            local IFS=$'\n'
            COMPREPLY=( $( compgen -f -o plusdirs -- "$cur" ) )
            return 0
            ;;
    esac

    $split && return 0

    if [ $COMP_CWORD -le 1 ] ; then
        COMPREPLY=( $( compgen -W 'get set del rename rename-force copy search
            exist unset info sync undeleted shell --version --help --noplugins
            --config' -- "$cur" ) )
    fi
} &&
complete -F _yu_yumdb -o filenames yumdb yumdb.py

# repodiff
_yu_repodiff()
{
    local cur prev words=() split=false
    _yu_init_completion "$2" "$3"

    case $prev in
        -h|--help|--version|-n|--new|-o|--old|-a|--archlist)
            return 0
            ;;
    esac

    $split && return 0

    COMPREPLY=( $( compgen -W '--version --help --new --old --quiet --archlist
        --compare-arch --size --downgrade --simple' -- "$cur" ) )
} &&
complete -F _yu_repodiff repodiff repodiff.py

# yum-builddep
_yu_builddep()
{
    local cur prev words=() split=false
    _yu_init_completion "$2" "$3"

    _yum_complete_baseopts "$cur" "$prev" && return 0

    case $prev in
        --target)
            declare -F _rpm_buildarchs &>/dev/null && _rpm_buildarchs
            return 0
            ;;
    esac

    $split && return 0

    if [[ $cur == -* ]] ; then
        COMPREPLY=( $( compgen -W '$( _yum_baseopts 2>/dev/null )' -- "$cur" ) )
        return 0
    fi

    local IFS=$'\n'
    COMPREPLY=( $( compgen -f -o plusdirs -X "!*.sp@(ec|m)" -- "$cur" ) )
    COMPREPLY+=( $( compgen -f -o plusdirs -X "!*.?(no)src.rpm" -- "$cur" ) )
    [[ $cur != */* && $cur != ~* ]] && _yum_list all "$cur" 2>/dev/null
} &&
complete -F _yu_builddep -o filenames yum-builddep yum-builddep.py

# debuginfo-install
_yu_debuginfo_install()
{
    local cur prev words=() split=false
    _yu_init_completion "$2" "$3"

    _yum_complete_baseopts "$cur" "$prev" && return 0

    $split && return 0

    if [[ $cur == -* ]] ; then
        COMPREPLY=( $( compgen -W '$( _yum_baseopts 2>/dev/null )
            --no-debuginfo-plugin' -- "$cur" ) )
        return 0
    fi

    _yum_list all "$cur"
} &&
complete -F _yu_debuginfo_install debuginfo-install debuginfo-install.py

# yum-debug-dump
_yu_debug_dump()
{
    COMPREPLY=()

    case $3 in
        -h|--help)
            return 0
            ;;
    esac

    if [[ $2 == -* ]] ; then
        COMPREPLY=( $( compgen -W '--help --norepos' -- "$2" ) )
        return 0
    fi

    local IFS=$'\n'
    COMPREPLY=( $( compgen -f -o plusdirs -- "$cur" ) )
} &&
complete -F _yu_debug_dump -o filenames yum-debug-dump yum-debug-dump.py

# yumdownloader
_yu_yumdownloader()
{
    local cur prev words=() split=false
    _yu_init_completion "$2" "$3"

    _yum_complete_baseopts "$cur" "$prev" 2>/dev/null && return 0

    case $prev in
        --destdir)
            local IFS=$'\n'
            COMPREPLY=( $( compgen -d -- "$cur" ) )
            return 0
            ;;
        --archlist)
            return 0
            ;;
    esac

    $split && return 0

    if [[ $cur == -* ]] ; then
        COMPREPLY=( $( compgen -W '$( _yum_baseopts 2>/dev/null ) --destdir
            --urls --resolve --source --archlist' -- "$cur" ) )
        return 0
    fi

    _yum_list all "$cur"
} &&
complete -F _yu_yumdownloader -o filenames yumdownloader yumdownloader.py

# Local variables:
# mode: shell-script
# sh-basic-offset: 4
# sh-indent-comment: t
# indent-tabs-mode: nil
# End:
# ex: ts=4 sw=4 et filetype=sh
