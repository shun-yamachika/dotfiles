set -l commands install download uninstall freeze list show check search wheel hash completion help

complete -c pip3 -n "not __fish_seen_subcommand_from $commands" -x -a 'install download uninstall freeze list show check search wheel hash completion help'
