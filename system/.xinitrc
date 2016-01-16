DEFAULT_SESSION="gnome-session"

case $1 in
    gnome)
        exec $DEFAULT_SESSION
        ;;
    i3)
        exec i3
        ;;
    *)
        exec $DEFAULT_SESSION
        ;;
esac
setxkbmap -layout us,ru -option grp:lwin_toggle
