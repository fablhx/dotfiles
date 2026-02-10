# =============================================================================
# Legacy git prompt (compatible with git < 2.11)
# Uses separate git commands for branch, status, and ahead/behind.
# =============================================================================

__git_prompt_compute_legacy() {
    __gp_active=false
    __gp_branch=""
    __gp_detached=no
    __gp_behind=0
    __gp_ahead=0
    __gp_staged=0
    __gp_unmerged=0
    __gp_changed=0
    __gp_untracked=0
    __gp_stashed=0

    [[ "$PWD" == *"/.git"* ]] && return

    # Resolve git_dir — avoid double fork by reusing .git directory check
    local git_dir
    if [[ -d ".git" ]]; then
        git_dir=".git"
    else
        git_dir=$(git rev-parse --git-dir 2>/dev/null) || return
    fi

    __gp_active=true

    local base_timeout=0.1
    local fast_timeout=0.05
    local slow_timeout=0.2
    local large_repo=false

    if __is_large_repo "${git_dir}"; then
        base_timeout=0.05
        fast_timeout=0.03
        slow_timeout=0.1
        large_repo=true
    fi

    # Branch name
    __gp_branch=$(timeout "${fast_timeout}s" git symbolic-ref --short -q HEAD 2>/dev/null)
    if [[ -z "${__gp_branch}" ]]; then
        __gp_detached="yes"
        __gp_branch=$(timeout "${base_timeout}s" git describe --exact-match --tags HEAD 2>/dev/null \
                      || timeout "${base_timeout}s" git rev-parse --short HEAD 2>/dev/null)
    fi

    # Status — single call, include untracked for small repos
    local git_status="" status_ok=true
    if [[ "${large_repo}" == "true" ]]; then
        if ! git_status=$(timeout "${fast_timeout}s" git status --porcelain --untracked-files=no 2>/dev/null); then
            status_ok=false
        fi
    else
        if ! git_status=$(timeout "${slow_timeout}s" git status --porcelain --untracked-files=normal 2>/dev/null); then
            status_ok=false
        fi
    fi

    # Fallback if git status timed out
    if [[ "${status_ok}" == "false" ]]; then
        local staged_files changed_files
        staged_files=$(timeout "${fast_timeout}s" git diff --cached --name-only 2>/dev/null)
        changed_files=$(timeout "${fast_timeout}s" git diff --name-only 2>/dev/null)
        git_status=""
        [[ -n "${staged_files}" ]] && while IFS= read -r f; do
            git_status+="M  ${f}"$'\n'
        done <<< "${staged_files}"
        [[ -n "${changed_files}" ]] && while IFS= read -r f; do
            git_status+=" M ${f}"$'\n'
        done <<< "${changed_files}"
    fi

    # Ahead/behind
    if [[ "${__gp_detached}" != "yes" ]]; then
        local upstream
        upstream=$(timeout "${fast_timeout}s" git rev-parse --abbrev-ref '@{upstream}' 2>/dev/null)
        if [[ -n "${upstream}" ]]; then
            local ahead_behind
            if ahead_behind=$(timeout "${fast_timeout}s" git rev-list --left-right --count HEAD..."${upstream}" 2>/dev/null); then
                [[ -n "${ahead_behind}" ]] && read -r __gp_ahead __gp_behind <<< "${ahead_behind}"
            else
                local merge_base head_commit upstream_commit
                merge_base=$(timeout "${fast_timeout}s" git merge-base HEAD "${upstream}" 2>/dev/null)
                if [[ -n "${merge_base}" ]]; then
                    head_commit=$(timeout "${fast_timeout}s" git rev-parse HEAD 2>/dev/null)
                    upstream_commit=$(timeout "${fast_timeout}s" git rev-parse "${upstream}" 2>/dev/null)
                    if [[ -n "${head_commit}" && -n "${upstream_commit}" ]]; then
                        [[ "${merge_base}" != "${head_commit}" ]] && __gp_ahead="?"
                        [[ "${merge_base}" != "${upstream_commit}" ]] && __gp_behind="?"
                    fi
                fi
            fi
        fi
    fi

    # Parse status lines
    if [[ -n "${git_status}" ]]; then
        while IFS= read -r line; do
            [[ -z "${line}" ]] && continue
            local idx="${line:0:1}" wt="${line:1:1}"
            if [[ "${idx}" == "?" ]]; then
                ((__gp_untracked++)); continue
            fi
            case "${idx}" in [MADRC]) ((__gp_staged++)) ;; U) ((__gp_unmerged++)) ;; esac
            case "${wt}" in [MD]) ((__gp_changed++)) ;; U) ((__gp_unmerged++)) ;; esac
        done <<< "${git_status}"
    fi

    # Stash count
    if [[ -f "${git_dir}/refs/stash" ]]; then
        __gp_stashed=$(timeout "${base_timeout}s" git rev-list --walk-reflogs --count refs/stash 2>/dev/null || echo 0)
    fi
}
