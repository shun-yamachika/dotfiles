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

alias netsquid-env="source ~/quantum/LinkSelFiE/netsquid-env/bin/activate.fish"
set -gx PATH $HOME/.npm-global/bin $PATH

set -gx PATH /home/shun/.local/bin $PATH
