function fd
    if not type -q fdfind
        echo "fdfind: not found. Install package 'fd-find' (Debian) or provide 'fd'."
        return 1
    end
    fdfind $argv
end
