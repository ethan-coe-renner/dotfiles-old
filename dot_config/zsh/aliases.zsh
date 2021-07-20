# aliases
ex() {
        if [ -f $1 ]; then
                case $1 in
                *.tar.bz2) tar xjf $1 ;;
                *.tar.gz) tar xzf $1 ;;
                *.bz2) bunzip2 $1 ;;
                *.rar) unrar x $1 ;;
                *.gz) gunzip $1 ;;
                *.tar) tar xf $1 ;;
                *.tbz2) tar xjf $1 ;;
                *.tgz) tar xzf $1 ;;
                *.zip) unzip $1 ;;
                *.Z) uncompress $1 ;;
                *.7z) 7z x $1 ;;
                *.deb) ar x $1 ;;
                *.tar.xz) tar xf $1 ;;
                *.tar.zst) unzstd $1 ;;
                *) echo "'$1' cannot be extracted via ex()" ;;
                esac
        else
                echo "'$1' is not a valid file"
        fi
}

#=Tool configuration
alias ls='exa --group-directories-first'
alias ll='exa --long --grid --git --group-directories-first'
alias la='exa -a --group-directories-first'
alias nnn='nnn -c'
alias lal='exa -a --long --grid --git --group-directories-first'
#alias rm='echo "use rip instead to remove"'
alias cat='bat --theme=Nord'
alias grep='rg'
alias yt='youtube-dl -f best -i'
alias yta="youtube-dl -f bestaudio "
alias cp="cp -i"
alias mv='mv -i'
alias :q='exit'
alias rip='rip --graveyard ~/.local/share/Trash'

#=program shortcuts
alias cd='z'
alias v='nvim'
alias sn='shutdown now'
alias rb='reboot'
alias sx='startx'
alias chwall='betterlockscreen -u'
alias backup='restic backup ~/code ~/media ~/school ~/readlater'
alias syncbackup='rclone sync .bak raspi:backup/bak'
alias syncmedia='rclone sync media raspi:media'