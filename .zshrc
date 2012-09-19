# Variables: History
HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000

# Options: ZLE
setopt complete_in_word

unsetopt auto_menu

# Optons: Prompt
setopt transient_rprompt prompt_subst

# Options: History
setopt hist_ignore_dups extended_history append_history
setopt hist_no_store hist_lex_words

unsetopt beep


# The following lines were added by compinstall
zstyle :compinstall filename '/home/foo/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

COMPLETIONFONT=243
# Completion Customization
zstyle ':completion:*:warnings' format "%F{$COMPLETIONFONT}%BNo matches for %d%b%f"
zstyle ':completion:*:descriptions' format "%F{$COMPLETIONFONT}%B%d%b%f"
zstyle ':completion:*' group-name ''
zstyle ':completion:*:*:-command-:*:(commands|builtins|reserved-words|aliases)' group-name commands
zstyle ':completion:*:manuals' separate-sections true

zmodload zsh/complist
zstyle ':completion:*:default' list-prompt '%S%M matches %s'
zstyle ':completion:*:default' menu 'select=0'
bindkey -M listscroll '\C-m' send-break

# bindkey -M menuselect '\C-o' accept-and-menu-complete


zstyle ':completion:*:windows' menu 'on=0' # for xkill -id
#zstyle ':completion:*:(^approximate):*' matcher-list 'm:{a-z}={A-Z}' # lower-case for upper-case but not vice-versa
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

# prefix: when completion considering the suffix fails, do completion without the suffix
zstyle ':completion::*:::' completer _complete _prefix _ignored  #  _approximate
# zstyle -e ':completion:*:approximate:*' max-errors 'reply=( $(( ($#PREFIX+$#SUFFIX)/3 )))'
# zstyle ':completion:*:corrections' format "%F{$COMPLETIONFONT}%B%d (errors: %e)%b%f"
#zstyle ':completion:*:expand:*' tag-order all-expansions

# Add a space after completion ignoring suffix
zstyle ':completion:*:prefix:*' add-space true

# ignored files
zstyle ':completion:*:*files' ignored-patterns '*?.o' '*?~' '*?.pyc'

# Ignore expanding to the same file twice
zstyle ':completion::*:(less|rm|vi):*' ignore-line true

# After ../, ignore the corrent directory
zstyle ':completion:*' ignore-parents parent pwd

autoload -U select-word-style
select-word-style shell



# Prompt
#PS1="%F{131}%20<..<%2c%<<%f%F{35}%#%f "
case $TERM in 
    *rxvt* | *screen*) 
	if [[ -z $SSH_CLIENT ]]; then
	    PS1=$'%{\e]2;%n@%M:%l\a\e]1;urxvt\a%}%F{131}%20<..<%2c%<<%f%F{35}%#%f ' # '%{\a%} '
	else
	    PS1=$'%{\e]2;%n@$SSH_CONNECTION[(w)3]:%l\a%}%F{131}%n@%M:%20<..<%2c%<<%f%F{35}%#%f '
	fi
	RPS1=$'%(?..(%?%) )'


	;;
    *)
	PS1=$'%F{131}%20<..<%2c%<<%f%F{35}%#%f '
	RPS1=$'%(?..(%?%) )' # '%n@%M:%l %@'
esac


PS1="
$PS1"
PS2="%F{blue}%_%f%F{cyan}>%f "


# Plugins
zmodload -i zsh/complist
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

autoload -U zargs zmv

# Aliases
alias ls="ls --color=auto"
alias pu='pushd'



# Keybindings
bindkey -e
bindkey '\ep' history-beginning-search-backward
bindkey '\en' history-beginning-search-forward
bindkey '\eq' push-input
#bindkey '\t' expand-or-complete-prefix
bindkey '\e\t' menu-complete
bindkey '\e;' vi-repeat-find
bindkey '\e,' vi-rev-repeat-find

zle -N rev-delete-to-char
bindkey '\ez' rev-delete-to-char



zle -N convert-num-to-range
bindkey '\C-xn' convert-num-to-range

zle -N add-an-option
bindkey '\eo' add-an-option


zle -N zle-example
bindkey '\em' zle-example



#precmd_functions=(time_after_exec)  
#precmd_functions=(xterm_title_reset)
#preexec_functions=(xterm_title) # time_before_exec)  should be the last function in the array

REPORTTIME=1
TIMEFMT="Time %*E; CPU %P; Max Mem %MKB"

# Watch for people logging in
watch=(all)
LOGCHECK=30

header () {
    setopt local_options
    unsetopt prompt_subst
    case $TERM in
	(xterm*|rxvt*)
	    print -nP "\e]2;$*\a"
	    ;;
	(*) ;;
    esac
}

iconname () {
    setopt local_options
    unsetopt prompt_subst
    case $TERM in
	(xterm*|rxvt*)
	    print -nP "\e]1;$*\a"
	    ;;
	(*) ;;
    esac
}


preexec() {
    iconname busy
}

# Zle functions

autoload ~/.zfunc/zle/*(:t)


# Functions 

# adapted from emacs-fu
function sem() {
    filename=$1
    without_beg_slash="${1##/}"
    if [[ $without_beg_slash == $1 ]];then
	filename="${PWD%//}/$1"
    fi
	emacsclient -c -a emacs "/sudo:root@localhost:$filename"
}

function unr() {
    filename="$@[-1]"		# filename would be the last parameter
    

    if [[ $filename == *.rar ]];then # check if it is a rar file
	tmpdir="$(mktemp -d unr.XXXXXXXX)"

	if unrar x -x'*.url' -x'*.URL'  -x'*.txt' -x'*.TXT' "$@" "$tmpdir/";then # -x'*.htm' -x'*.HTM' -x'*.html' -x'*.HTML' -x'*.txt' -x'*.TXT'
	    if [[ $filename == *.part0#1.rar ]];then
		new_dir=${filename%.part0#1.rar}
		rm -- "${filename%%0#1.rar}"*.rar
	    else
		new_dir=${filename:r}
		rm -- $filename
	    fi
	    
	    no_files="$(ls $tmpdir/ | wc -l)"
	    
	    # if there are only < 5 files, we don't need the directory - copy the files and remove the dir
	    if (( no_files < 4 )); then
		mv "$tmpdir"/* .; rmdir "$tmpdir"
	    elif		# rename the directory to something meaningful if possible
		[[ ! -e "$new_dir" ]];then
		mv "$tmpdir" "$new_dir"
	    fi
	
	else			# unrar unsuccessful.  Remove the temp dir
	    rm -rf "$tmpdir"
	fi
    elif [[ $filename == *.zip ]];then # check if it is a zip file
	tmpdir="$(mktemp -d unr.XXXXXXXX)"
	if unzip  "$@" -x '*.url' '*.URL' '*.htm' '*.HTM' '*.html' '*.HTML' '*.txt' '*.TXT' -d "$tmpdir/";then
	    new_dir=${filename:r}
	    rm $filename
	    
	    no_files="$(ls $tmpdir/ | wc -l)"
	    
	    # if there are only < 5 files, we don't need the directory - copy the files and remove the dir
	    if (( no_files < 5 )); then
		mv "$tmpdir"/* .; rmdir "$tmpdir"
	    elif		# rename the directory to something meaningful if possible
		[[ ! -e "$new_dir" ]];then
		mv "$tmpdir" "$new_dir"
	    fi
	    
	else			# unrar unsuccessful.  Remove the temp dir
	    rm -rf "$tmpdir"
	fi



    fi
}


function alex() {
    for i in *.AlexandriZ.rar *.Alexandriz.rar -x'*.nfo' ;do
	unr $i
    done

    for i in *AlexandriZ *.Alexandriz;do
	cd $i
	mv *epub ..
	cd ..
	rm -r $i
    done
}
    


# Aliases


alias evmv='ebook-view-rename'
alias ev='ebook-viewer'
alias se='sudoedit'
alias lci='locate -i'
alias mp='mplayer'
