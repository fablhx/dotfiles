#!/usr/bin/env bash
# Claude Code status line. Two blocks separated by a divider:
#   LLM:  model, reasoning effort, context-usage bar
#   repo: working directory, git branch

input=$(cat)

# A malformed payload must not put jq's parse error where the status line goes.
mapfile -t fields < <(
    jq -r '
        (.model.display_name // "?"),
        (.context_window.used_percentage // 0 | floor),
        (.workspace.current_dir // .cwd // ".")
    ' <<<"$input" 2>/dev/null
)
model=${fields[0]:-?}
pct=${fields[1]:-0}
cwd=${fields[2]:-.}

[[ "$pct" =~ ^[0-9]+$ ]] || pct=0
(( pct > 100 )) && pct=100

# The reasoning tier isn't part of the status-line stdin payload; it's read from
# settings.json (or the CLAUDE_CODE_EFFORT_LEVEL override). Haiku has no effort
# tier, so no badge is shown for it.
effort=""
if [[ "$model" != *Haiku* ]]; then
    settings="${CLAUDE_CONFIG_DIR:-$HOME/.claude}/settings.json"
    [[ -r "$settings" ]] && effort=$(jq -r '.effortLevel // empty' "$settings" 2>/dev/null)
    [[ -z "$effort" ]] && effort=${CLAUDE_CODE_EFFORT_LEVEL:-}
    case "$effort" in
        low|medium|high|xhigh|max) ;;
        *) effort="" ;;
    esac
fi

RESET=$'\033[0m'
MODEL_C=$'\033[1;36m'
EFFORT_C=$'\033[1;35m'
DIR_C=$'\033[1;34m'
BRANCH_C=$'\033[38;5;35m'
SEP_C=$'\033[38;5;240m'

# U+2502 BOX DRAWINGS LIGHT VERTICAL, built from bytes to keep the source
# ASCII-only, dividing the LLM block from the repo block.
SEP=$(printf '\xe2\x94\x82')

if   (( pct >= 90 )); then CTX_C=$'\033[38;5;196m'
elif (( pct >= 70 )); then CTX_C=$'\033[38;5;220m'
else                       CTX_C=$'\033[38;5;35m'
fi

# U+2588 FULL BLOCK / U+2591 LIGHT SHADE, built from codepoints to keep
# the source ASCII-only.
FULL=$(printf '\xe2\x96\x88')
LIGHT=$(printf '\xe2\x96\x91')
width=10
filled=$(( pct * width / 100 ))
(( filled > width )) && filled=width
empty=$(( width - filled ))
printf -v f '%*s' "$filled" ''
printf -v e '%*s' "$empty" ''
bar="${f// /$FULL}${e// /$LIGHT}"

branch=""
if timeout 0.3s git -C "$cwd" --no-optional-locks rev-parse --is-inside-work-tree >/dev/null 2>&1; then
    branch=$(timeout 0.3s git -C "$cwd" --no-optional-locks branch --show-current 2>/dev/null)
    if [[ -z "$branch" ]]; then
        sha=$(timeout 0.3s git -C "$cwd" --no-optional-locks rev-parse --short HEAD 2>/dev/null)
        [[ -n "$sha" ]] && branch="@$sha"
    fi
fi

dir=${cwd/#"$HOME"/\~}

out="${MODEL_C}${model}${RESET}"
[[ -n "$effort" ]] && out+="  ${EFFORT_C}${effort}${RESET}"
out+="  ${CTX_C}${bar} ${pct}%${RESET}"
out+="  ${SEP_C}${SEP}${RESET}  ${DIR_C}${dir}${RESET}"
[[ -n "$branch" ]] && out+="  ${BRANCH_C}${branch}${RESET}"

printf '%s\n' "$out"
