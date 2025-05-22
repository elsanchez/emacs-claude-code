#!/bin/bash

# Default location, can be overridden by providing an argument
WORKTREE_DIR="${1:-/tmp/scitex-worktree}"

# Verify worktree exists
if [ ! -d "$WORKTREE_DIR" ]; then
  echo "Error: Worktree directory $WORKTREE_DIR does not exist."
  echo "Usage: $0 [worktree_directory]"
  exit 1
fi

# Save the current branch information
CURRENT_BRANCH=$(cd "$WORKTREE_DIR" && git branch --show-current)
echo "Using Git worktree at $WORKTREE_DIR on branch $CURRENT_BRANCH"

# Create a CLAUDE.md that points to this worktree
cat > "$WORKTREE_DIR/CLAUDE.md" << EOF
---
claude_dir: $WORKTREE_DIR
---

# Claude Worktree Configuration

This file sets persistent configuration for Claude to use this Git worktree.
EOF

# Export environment variable for Claude
export CLAUDE_DIR="$WORKTREE_DIR"
echo "CLAUDE_DIR set to $WORKTREE_DIR"

# Change to the worktree directory
cd "$WORKTREE_DIR"
echo "Current directory: $(pwd)"

# Start a shell in the worktree
echo "Starting shell in worktree. Type 'exit' to return to the main repo."
exec bash