#!/usr/bin/env python
# -*- coding: utf-8 -*-

from subprocess import PIPE, Popen
import os, re, string, sys

def execute(command):
    proc = Popen(command, stdout=PIPE, stderr=PIPE)
    out, err = proc.communicate()

    return (out.decode('utf-8').splitlines(), err.decode('utf-8').splitlines())

def parse_branch(line):
    if not line:
        return ('', False, 0, 0)

    if line.startswith('## '):
        line = line[3:]

    match = re.search('^No commits yet on master$', line)
    if match is not None:
        return (match.group(1), False, 0, 0)

    match = re.search('^Initial commit on (.+)$', line)
    if match is not None:
        return (match.group(1), False, 0, 0)

    match = re.search('^(.+) \(no branch\)$', line)
    if match is not None:
        return (match.group(1), True, 0, 0)

    match = re.search('^(.+?)\.\.\.', line)
    if match is not None:
        branch = match.group(1)

        match = re.search('\[ahead (\d+), behind (\d+)\]$', line)
        if match is not None:
            return (branch, False, int(match.group(2)), int(match.group(1)))
        match = re.search('\[ahead (\d+)\]$', line)
        if match is not None:
            return (branch, False, 0, int(match.group(1)))
        match = re.search('\[behind (\d+)\]$', line)
        if match is not None:
            return (branch, False, int(match.group(1)), 0)

        return (branch, False, 0, 0)

    return (line, False, 0, 0)

def parse_status(lines):
    staged    = len([True for l in lines if l[0] in 'MRC' or (l[0] == 'D' and l[1] != 'D') or (l[0] == 'A' and l[1] != 'A')])
    unmerged  = len([True for l in lines if l[0] == 'U' or l[1] == 'U' or (l[0] == 'A' and l[1] == 'A') or (l[0] == 'D' and l[1] == 'D')])
    changed   = len([True for l in lines if l[1] == 'M' or (l[1] == 'D' and l[0] != 'D')])
    untracked = len([True for l in lines if l[0] == '?'])

    return (staged, unmerged, changed, untracked)


def git_prompt():
    cwd = os.getcwd()

    if not cwd:
        return

    base = ['git', '-C', cwd]

    status, err = execute(base + ['status', '--branch', '--porcelain'])

    if err and ('error' in err[0] or 'fatal' in err[0]):
        return

    branch, detached, behind, ahead = parse_branch(status.pop(0))

    if not branch:
        return

    if branch == 'HEAD':
        branch = execute(base + ['rev-parse', '--short', 'HEAD'])[0][0]

    staged, unmerged, changed, untracked = parse_status(status)

    stashed = len(execute(base + ['stash', 'list', '--no-decorate'])[0])

    if detached:
        branch_group = 'detached'
    elif staged or unmerged or changed or untracked:
        branch_group = 'dirty'
    else:
        branch_group = 'clean'

    prompt = [branch, branch_group, str(behind), str(ahead), str(staged),
              str(unmerged), str(changed), str(untracked), str(stashed)]

    print ' '.join(prompt)

git_prompt()
