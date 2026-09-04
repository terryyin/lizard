#!/usr/bin/env bash

set -euo pipefail

MODE="${1:?Usage: quality_changed.sh format|lint}"
case "$MODE" in
  format|lint) ;;
  *)
    echo "Unknown quality mode: $MODE" >&2
    exit 1
    ;;
esac

REPO_ROOT="$(git rev-parse --show-toplevel)"
cd "$REPO_ROOT"

selected_components=""

select_component() {
  if [[ " $selected_components " != *" $1 "* ]]; then
    selected_components+=" $1"
  fi
}

select_python_components() {
  select_component lizard
  select_component lizard_ext
  select_component lizard_languages
}

select_components_for_file() {
  case "$1" in
    lizard.py)
      select_component lizard
      ;;
    lizard_ext/*)
      select_component lizard_ext
      ;;
    lizard_languages/*)
      select_component lizard_languages
      ;;
    setup.cfg)
      select_python_components
      ;;
  esac
}

if [[ "$MODE" == format ]]; then
  changed_files="$({
    git diff --name-only
    git diff --cached --name-only
    git ls-files --others --exclude-standard
  } | sort -u)"
else
  changed_files="$(git diff --cached --name-only)"
fi

while IFS= read -r file; do
  [[ -n "$file" ]] && select_components_for_file "$file"
done <<< "$changed_files"

run_quality_for_component() {
  case "$1" in
    lizard)
      pycodestyle lizard.py
      ;;
    lizard_ext)
      pycodestyle lizard_ext
      ;;
    lizard_languages)
      pycodestyle lizard_languages
      ;;
  esac
}

for component in lizard lizard_ext lizard_languages; do
  if [[ " $selected_components " == *" $component "* ]]; then
    run_quality_for_component "$component"
  fi
done
