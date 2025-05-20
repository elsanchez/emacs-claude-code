<!-- ---
!-- Timestamp: 2025-05-21 00:32:33
!-- Author: ywatanabe
!-- File: /home/ywatanabe/.claude/to_claude/guidelines/guidelines-command-rm.md
!-- --- -->

# `rm` Command
- ALWAYS KEEP REPOSITORY CLEAN
- For this, use `./docs/to_claude/bin/safe_rm.sh` below to hide old/unrelated files with timestamp.
- `rm` command is not allowed. 

## Usage
`safe_rm.sh [-h|--help] file_or_directory [file_or_directory...]`

## Examples:
`$(basename $0) file1.txt dir1`
`$(basename $0) *.txt`

## Your Understanding Check
Did you understand the guideline? If yes, please say:
`CLAUDE UNDERSTOOD: <THIS FILE PATH HERE>`

<!-- EOF -->