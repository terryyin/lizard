#!/usr/bin/env bash
#
# Smart command runner that automatically handles nix environment:
# - If nix is installed and not in a nix shell: prefixes with "nix develop -c"
# - If already in a nix shell or nix is not installed: runs command directly
#
# Usage: ./scripts/run.sh <command> [args...]

set -e

# Check if nix is installed
if command -v nix >/dev/null 2>&1; then
    # Check if we're currently in a nix shell
    if [ -z "${IN_NIX_SHELL:-}" ]; then
        # Nix is installed but not in a nix shell, prefix with nix develop
        exec nix develop -c "$@"
    else
        # Already in a nix shell, run command directly
        exec "$@"
    fi
else
    # Nix is not installed, run command directly
    exec "$@"
fi
