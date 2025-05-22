<!-- ---
!-- Timestamp: 2025-05-22 14:37:24
!-- Author: ywatanabe
!-- File: /home/ywatanabe/.claude/to_claude/guidelines/IMPORTANT-guidelines-command-tree.md
!-- --- -->

# Tree Command Guidelines

To understand project structure, use `tree` wisely. It is installed from source so that various options are available.

``` bash
TREE_IGNORE="__pycache__"
TREE_IGNORE+="-I .env"
...

# Incrementally run tree to avoid too much information 
tree -a -L 1
tree -a -L 2
tree -a -L 3 -I $TREE_IGNORE
```

## Common Usage Patterns

| Option | Description | Example |
|--------|-------------|---------|
| `-a` | Show all files, including hidden files | `tree -a` |
| `-d` | Show only directories | `tree -d` |
| `-L n` | Limit depth to n levels | `tree -L 2` |
| `-I pattern` | Exclude files/directories matching pattern | `tree -I "node_modules\|.git"` |
| `--gitignore` | Honor gitignore patterns | `tree -a --gitignore` |
| `-h` | Print sizes in human-readable format | `tree -h` |
| `-C` | Enable colorization | `tree -C` |

## Examples

| Use Case | Command | Description |
|----------|---------|-------------|
| Project Overview | `tree -L 2` | View top-level directories and immediate subdirectories |
| Code Navigation | `tree -L 3 src` | View source code structure to 3 levels deep |
| Full Directory Analysis | `tree -a -h --du` | Show all files with human-readable sizes and calculate directory sizes |
| Exclude Build Artifacts | `tree -I "node_modules\|build\|dist\|__pycache__"` | Show only relevant source files |
| Find Large Directories | `tree -d -h --du \| grep -B 3 -A 1 "[GM]"` | Identify directories with large content |
| JSON Output | `tree -J > structure.json` | Output structure in JSON format for parsing |

For more details, see `tree --help`

<!-- EOF -->