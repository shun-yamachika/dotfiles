function fish_title
end

function fish_prompt
	if not set -q __fish_prompt_hostname
                set -g __fish_prompt_hostname (hostname|cut -d . -f 1)
        end

	set -l id
	set id (set_color -o cyan) "$USER" '@' "$__fish_prompt_hostname"

	if set -q WINDOW
		set win (set_color green) '[' $WINDOW ']'
	end

	set -l pwd_color
	set -l prompt
	if test "$USER" = root
		set pwd_color (set_color normal) (set_color -u magenta)
		set prompt '#'
	else
		set pwd_color (set_color normal) (set_color -u green)
		set prompt '>'
	end

        echo -n -s $id $win ':' $pwd_color (prompt_pwd) $prompt (set_color normal) ' '
end

function attach_screen
	if test "$TERM" != rxvt-unicode; and test "$TERM" != rxvt-unicode-256color
		return
	end
	if not which screen >/dev/null; or test -n "$STY"
		return
	end
	set -l pts
	set pts (screen -list | grep -m1 Detached | awk '{ print $1 }')
	if test -z "$pts"
		exec screen
	else
		exec screen -r $pts
	end
end

ulimit -c 0

set -x PATH /usr/local/bin /usr/bin /bin /usr/local/sbin /usr/sbin /sbin
if test -d ~/bin
	set PATH ~/bin $PATH
end

set -x BIBINPUTS .:~/lib/texmf/bib:~/lib/texmf/bib/fserv:~/lib/texmf/bib/weisshorn
set -x BSTINPUTS .:~/lib/texmf//:/usr/share/texmf//:/usr/share/texlive//:/etc/texmf//
set -x MAKEFILES ~/.Makefile
set -x PYTHONPATH ~/lib/python
set -x QT_FONT_DPI 90
set -x QT_IM_MODULE uim
set -x TEXINPUTS .:~/lib/texmf//:/usr/share/texmf//:/usr/share/texlive//:/etc/texmf//

set fish_greeting

alias d docker
alias f from
alias h head
alias l lg
alias la "ls -aF $LS_OPTION"
alias lg "ls -lgF $LS_OPTION"
alias ll "ls -lF $LS_OPTION"
alias m 'lv -c'
alias p pyhelp
alias t tail
alias x mupdf
alias bl-keyboard 'bluetoothctl connect EF:1B:C1:B8:6E:08'
alias bl-headphone 'bluetoothctl connect 80:99:E7:FF:AC:63'
alias clamshell-on 'xrandr --output eDP-1 --off --output HDMI-1 --auto'
alias clamshell-off 'xrandr --output eDP-1 --auto --output HDMI-1 --auto --same-as eDP-1'

if status --is-interactive
	if status --is-login
		attach_screen
	end
end

# pyenv initialization
set -gx PYENV_ROOT $HOME/.pyenv
if test -d $PYENV_ROOT/bin
    set -gx PATH $PYENV_ROOT/bin $PATH
end
if command -v pyenv >/dev/null
    pyenv init - fish | source
end

alias netsquid-env="source ~/LinkSelFiE/netsquid-env/bin/activate.fish"
set -gx PATH $HOME/.npm-global/bin $PATH

set -gx PATH /home/shun/.local/bin $PATH

# nb-env virtual environment shortcut
function nb-env
    source /home/shun/nb-env/bin/activate.fish
end

# Screen recording function with auto-incrementing filenames
function rec
    # Create Videos directory if it doesn't exist
    mkdir -p ~/Videos

    # Find the next available number
    set -l num 1
    while test -e ~/Videos/output$num.mp4
        set num (math $num + 1)
    end

    # Start recording
    echo "録画開始: ~/Videos/output$num.mp4"
    echo "停止するには Ctrl+C を押してください"
    ffmpeg -video_size 1920x1080 -framerate 60 -f x11grab -i :0.0 -f alsa -i default -c:v libx264 -preset ultrafast -c:a aac ~/Videos/output$num.mp4
end

# Audio output switching functions
function audio-hdmi
    pactl set-default-sink alsa_output.pci-0000_00_1f.3-platform-skl_hda_dsp_generic.HiFi__hw_sofhdadsp_3__sink
    echo "HDMI出力に切り替えました"
end

function audio-speaker
    pactl set-default-sink alsa_output.pci-0000_00_1f.3-platform-skl_hda_dsp_generic.HiFi__hw_sofhdadsp__sink
    echo "本体スピーカーに切り替えました"
end
