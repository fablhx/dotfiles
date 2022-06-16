[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return

plugins=(
    compleat
    zsh-syntax-highlighting
    history-substring-search
)

source "${HOME}"/.oh-my-zsh/oh-my-zsh.sh

REPORTTIME=10
HISTSIZE=50000
SAVEHIST=50000

setopt append_history
setopt complete_in_word
setopt hist_expire_dups_first
setopt hist_find_no_dups
setopt hist_ignore_all_dups
setopt hist_ignore_dups
setopt hist_save_no_dups
setopt inc_append_history
setopt notify
setopt prompt_subst
setopt share_history

zstyle ':completion:*' menu select
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' verbose yes
zstyle ':completion:*' group-name ''
zstyle ':completion:*:descriptions' format "$fg[yellow]%B--- %d%b"
zstyle ':completion:*:messages' format '%d'
zstyle ':completion:*:warnings' format "$fg[red]No matches$reset_color"
zstyle ':completion:*:corrections' format '%B%d (errors: %e)%b'

# TODO: Fix this! Do I really need zsh-syntax-highlighting
#
# This speeds up pasting w/ autosuggest
# https://github.com/zsh-users/zsh-autosuggestions/issues/238
pasteinit() {
  OLD_SELF_INSERT=${${(s.:.)widgets[self-insert]}[2,3]}
  zle -N self-insert url-quote-magic # I wonder if you'd need `.url-quote-magic`?
}

pastefinish() {
  zle -N self-insert $OLD_SELF_INSERT
}
zstyle :bracketed-paste-magic paste-init pasteinit
zstyle :bracketed-paste-magic paste-finish pastefinish

typeset -A ZSH_HIGHLIGHT_STYLES
ZSH_HIGHLIGHT_STYLES[path]=''

export ALTERNATE_EDITOR=""
export EDITOR='emacs';
export LESS="-RFX"

alias ls='ls --color=auto -v'
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

alias tree='tree -C'

alias xclip='xclip -selection clipboard'

alias l='ls -lh'
alias ll='ls -lh'
alias lll='ls -lha'
alias la='ls -lhd .?*'
alias lt='ls -lht'

alias rm='rm -i'

alias xopen='xdg-open'

alias ocaml='rlwrap -pMagenta ocaml'

alias pretty_py='autopep8 --in-place --aggressive --aggressive'

function __emacs_client() {
    split_file_line=()
    for p in "$@"; do
        if [[ "$p" =~ "(.*):([0-9]+)$" ]]; then
            split_file_line+="+${match[2]}"
            split_file_line+="${match[1]}"
        else
            split_file_line+="$p"
        fi
    done
    # This check if I have already run an emacsclient --create-frame,
    # if yes, use the current frame, if no create a new frame.
    if emacsclient -n -e "(if (> (length (frame-list)) 1) 't)" | grep -q t;
    then
        emacsclient -n "${split_file_line[@]}"
    else
        emacsclient -c -n "${split_file_line[@]}"
    fi
}

function __emacs() {
    split_file_line=()
    for p in "$@"; do
        if [[ "$p" =~ "(.*):([0-9]+)$" ]]; then
            split_file_line+="+${match[2]}"
            split_file_line+="${match[1]}"
        else
            split_file_line+="$p"
        fi
    done
    emacs "${split_file_line[@]}"
}

alias cedit='emacsclient -c -nw'
alias wedit='__emacs_client'
alias emacs='__emacs'
alias start_emacs='emacs --daemon'
alias stop_emacs='emacsclient -e "(kill-emacs)"'

__git_prompt() {
    local git_status fast_status
    fast_status=("${(f)$(LANG=en_US git -C "$PWD" symbolic-ref --short -q HEAD 2>/dev/null)}")
    git_status=("${(f)$(LANG=en_US timeout 0.2s git -C "$PWD" status --branch --porcelain 2>/dev/null)}")

    if [[ -z "${fast_status[1]}" && -z "${git_status[1]}" ]]
    then
        return
    fi

    if [[ -n "${fast_status[1]}" && -z "${git_status[1]}" ]]
    then
        # Just display the branch name
        echo " %{\033[38;5;3m%}⎇  ${fast_status[1]}%{\033[0m%}"
        return
    fi

    local branch detached behind ahead

    local sub_str tmp
    sub_str="## "
    git_status[1]="${git_status[1]#$sub_str}"

    if [[ "${git_status[1]}" == "No commits yet on master" ]]
    then
        branch="master"
        detached="no"
        behind=0
        ahead=0
    fi

    sub_str="Initial commit on "
    tmp="${git_status[1]#$sub_str}"
    if [[ "${git_status[1]}" != "$tmp" ]]
    then
        branch="$tmp"
        detached="no"
        behind=0
        ahead=0
    fi

    sub_str=" (no branch)"
    tmp="${git_status[1]%$sub_str}"
    if [[ "${git_status[1]}" != "$tmp" ]]
    then
        branch="$tmp"
        detached="yes"
        behind=0
        ahead=0
    fi

    tmp="${git_status[1]#*...}"
    if [[ "${git_status[1]}" != "$tmp" ]]
    then
        branch="${git_status[1]%$tmp}"
        branch="${branch%...}"
        detached="no"
        behind=0
        ahead=0
        sub_str_ahead="ahead "
        sub_str_behind="behind "
        tmp_ahead="${tmp#*$sub_str_ahead}"
        tmp_behind="${tmp#*$sub_str_behind}"
        if [[ "$tmp_ahead" != "$tmp" && "$tmp_behind" != "$tmp" ]]
        then
            ahead="${tmp_ahead%%,*}"
            behind="${tmp_behind%%]*}"
        else
            if [[ "$tmp_ahead" != "$tmp" ]]
            then
                ahead="${tmp_ahead%%]*}"
            fi
            if [[ "$tmp_behind" != "$tmp" ]]
            then
                behind="${tmp_behind%%]*}"
            fi
        fi
    fi

    if [[ -z "$branch" ]]
    then
        branch="${git_status[1]}"
        detached="no"
        behind=0
        ahead=0
    fi

    if [[ "$branch" == "HEAD" ]]
    then
        branch="$(LANG=en_US git -C "$PWD" describe --exact-match --tags HEAD 2>/dev/null)"
        if [[ -z "$branch" ]]
        then
            branch="$(LANG=en_US git -C "$PWD" rev-parse --short HEAD 2>/dev/null)"
        fi
    fi

    local staged unmerged changed untracked stashed

    staged=0
    unmerged=0
    changed=0
    untracked=0
    for line in "${git_status[@]:1}"
    do
        if [[ ("${line:0:1}" == "M" || "${line:0:1}" == "R" || "${line:0:1}" == "C") ||
              ("${line:0:1}" == "D" && "${line:1:1}" != "D") ||
              ("${line:0:1}" == "A" && "${line:1:1}" != "A") ]]
        then
            staged=$((staged+1))
        fi

        if [[ "${line:0:1}" == "U" || "${line:1:1}" == "U" ||
              "${line:0:2}" == "AA" || "${line:0:2}" == "DD" ]]
        then
            unmerged=$((unmerged+1))
        fi

        if [[ "${line:1:1}" == "M" ||
              ("${line:1:1}" == "D" && "${line:0:1}" != "D") ]]
        then
            changed=$((changed+1))
        fi

        if [[ "${line:0:1}" == "?" ]]
        then
            untracked=$((untracked+1))
        fi
    done

    stashed=("${(f)$(LANG=en_US git -C "$PWD" stash list --no-decorate 2>/dev/null)}")
    if [[ -z "${stashed[1]}" ]]
    then
        stashed=0
    else
        stashed="${#stashed}"
    fi

    local color_detached="%{\033[38;5;200m%}"
    local color_dirty="%{\033[38;5;240m%}"
    local color_clean="%{\033[38;5;22m%}"
    local color_branch="$color_clean"
    if [[ "$detached" == "yes" ]]
    then
        color_branch="$color_detached"
    fi
    if [[ "$staged" -ne 0 || "$unmerged" -ne 0 ||
          "$changed" -ne 0 || "$untracked" -ne 0 ]];
    then
        color_branch="$color_dirty"
    fi
    local color_behind="%{\033[38;5;250m%}"
    local color_ahead="%{\033[38;5;250m%}"
    local color_staged="%{\033[38;5;28m%}"
    local color_unmerged="%{\033[38;5;196m%}"
    local color_changed="%{\033[38;5;166m%}"
    local color_untracked="%{\033[38;5;172m%}"
    local color_stashed="%{\033[38;5;26m%}"
    local reset_color="%{\033[0m%}"

    # symbols:
    # ⎇ branch
    # ↓ ↑ number of commit behind, ahead
    # • staged
    # + changed
    # … untracked
    # ⚑ stashed

    git_prompt=" $color_branch⎇  $branch${reset_color}"
    if [[ "$behind" -ne 0 ]]
    then
        git_prompt="$git_prompt $color_behind↓ $behind${reset_color}"
    fi
    if [[ "$ahead" -ne 0 ]]
    then
        git_prompt="$git_prompt $color_ahead↑$ahead${reset_color}"
    fi
    if [[ "$staged" -ne 0 ]]
    then
        git_prompt="$git_prompt $color_staged•$staged${reset_color}"
    fi
    if [[ "$unmerged" -ne 0 ]]
    then
        git_prompt="$git_prompt $color_unmerged$unmerged${reset_color}"
    fi
    if [[ "$changed" -ne 0 ]]
    then
        git_prompt="$git_prompt $color_changed+$changed${reset_color}"
    fi
    if [[ "$untracked" -ne 0 ]]
    then
        git_prompt="$git_prompt $color_untracked…$untracked${reset_color}"
    fi
    if [[ "$stashed" -ne 0 ]]
    then
        git_prompt="$git_prompt $color_stashed⚑ $stashed${reset_color}"
    fi

    echo "$git_prompt"
}

__build_prompt() {
    PROMPT=$'%{\033[38;5;135m%}%n@%m%{\033[0m%} %4(c:…/:)%3c'
    test -z "$TIS_CHOOSE" || PROMPT+=$' %{\033[00;36m%}{${TIS_CHOOSE##*/}}%{\033[0m%}'
    PROMPT+="$(__git_prompt)"
    PROMPT+=$'%(0?.. %{\033[38;5;196m%}[%?]%{\033[0m%}) %# '
}

precmd() { __build_prompt; }
