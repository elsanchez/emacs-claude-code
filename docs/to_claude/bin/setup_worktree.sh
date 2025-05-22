#!/bin/bash

# Configuration - change these as needed
TGT_DIR=/tmp/scitex-worktree
BRANCH_NAME=claude-worktree
BASE_BRANCH=develop

# Clean up any existing worktree
echo "Cleaning up any existing worktree..."
rm -rf $TGT_DIR >/dev/null
git worktree prune

# Create a new branch from develop if it doesn't exist
if ! git show-ref --quiet refs/heads/$BRANCH_NAME; then
  echo "Creating new branch $BRANCH_NAME from $BASE_BRANCH..."
  git branch $BRANCH_NAME $BASE_BRANCH
else
  echo "Branch $BRANCH_NAME already exists, reusing it."
fi

# Create a new worktree with that branch
echo "Creating worktree at $TGT_DIR..."
mkdir -p $TGT_DIR
git worktree add $TGT_DIR $BRANCH_NAME

# Launch Claude in the worktree
echo "Starting Claude worktree environment..."
./claude_worktree.sh $TGT_DIR