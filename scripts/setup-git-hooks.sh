#!/usr/bin/env bash
#
# Setup script for git hooks
# Installs git hooks from scripts/git-hooks/ to .git/hooks/

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
GIT_HOOKS_SOURCE="$SCRIPT_DIR/git-hooks"
GIT_HOOKS_TARGET="$(git rev-parse --git-common-dir)/hooks"

if [ ! -d "$GIT_HOOKS_TARGET" ]; then
  echo "Error: .git/hooks directory not found. Are you in a git repository?"
  exit 1
fi

echo "Installing git hooks..."

# Install pre-commit hook
if [ -f "$GIT_HOOKS_SOURCE/pre-commit" ]; then
  cp "$GIT_HOOKS_SOURCE/pre-commit" "$GIT_HOOKS_TARGET/pre-commit"
  chmod +x "$GIT_HOOKS_TARGET/pre-commit"
  echo "Installed pre-commit hook"
else
  echo "Warning: pre-commit hook not found in $GIT_HOOKS_SOURCE"
fi

echo "Git hooks installed successfully!"
