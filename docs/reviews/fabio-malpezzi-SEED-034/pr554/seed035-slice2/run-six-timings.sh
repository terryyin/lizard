#!/usr/bin/env bash
# Sequential quiet-load paired NS timings for SEED-035 slice 2.
set -euo pipefail

HOST="$(hostname -s)"
ROOT="/Users/terryyin/git/lizard/.worktrees/land-nested-structure-member-name-skip-under-cum"
EVIDENCE="$ROOT/docs/reviews/fabio-malpezzi-SEED-034/pr554/seed035-slice2"
CACHE="/Users/terryyin/.cache/lizard-pr-reviews-SEED-034/pr554"
CORPUS_ROOT="/Users/terryyin/.cache/lizard-pr-reviews-SEED-034/pr552/proof/corpus"
BENCH="$ROOT/.agents/skills/lizard-process-pr/scripts/paired_benchmark.py"
LOAD_LOG="$EVIDENCE/load-gate-timings.log"
DECISION_LOG="$EVIDENCE/expensive-proof-decisions.log"

BASELINE_INC="$CACHE/baseline-inc"
CANDIDATE="$CACHE/candidate"
BASELINE_CUM="$CACHE/baseline-cum"

load1() {
  uptime | sed -E 's/.*load averages: ([0-9.]+).*/\1/'
}

wait_quiet() {
  local label="$1"
  echo "$(date '+%Y-%m-%dT%H:%M:%S%z') WAIT_QUIET start label=$label" | tee -a "$DECISION_LOG"
  local streak=0
  local sample=0
  while true; do
    sample=$((sample + 1))
    local l
    l="$(load1)"
    echo "$(date '+%Y-%m-%dT%H:%M:%S%z') label=$label sample=$sample load1=$l streak=$streak" | tee -a "$LOAD_LOG"
    # awk compare as float
    if awk -v x="$l" 'BEGIN{exit !(x < 2.5)}'; then
      streak=$((streak + 1))
    else
      streak=0
    fi
    if [[ "$streak" -ge 3 ]]; then
      echo "$(date '+%Y-%m-%dT%H:%M:%S%z') QUIET_GATE_MET label=$label load1=$l" | tee -a "$DECISION_LOG" "$LOAD_LOG"
      return 0
    fi
    sleep 60
  done
}

run_one() {
  local name="$1" mode="$2" workload="$3" corpus="$4" seconds="${5:-30}"
  local baseline candidate out
  if [[ "$mode" == "inc" ]]; then
    baseline="$BASELINE_INC"
    candidate="$CANDIDATE"
  else
    baseline="$BASELINE_CUM"
    candidate="$CANDIDATE"
  fi
  out="$EVIDENCE/${name}.json"

  wait_quiet "$name"

  echo "$(date '+%Y-%m-%dT%H:%M:%S%z') LAUNCH name=$name mode=$mode workload=$workload seconds=$seconds host=$HOST" | tee -a "$DECISION_LOG"
  echo "  baseline=$(git -C "$baseline" rev-parse HEAD) candidate=$(git -C "$candidate" rev-parse HEAD)" | tee -a "$DECISION_LOG"

  (
    cd "$ROOT"
    nix develop -c python "$BENCH" \
      --baseline "$baseline" \
      --candidate "$candidate" \
      --corpus "$CORPUS_ROOT/$corpus" \
      --workload "$workload" \
      --extension ns \
      --seconds "$seconds" \
      --pairs 20 \
      --limit-pct 3 \
      --out "$out"
  ) 2>&1 | tee "$EVIDENCE/${name}.log"

  local passes ci change
  passes="$(python3 -c "import json; d=json.load(open('$out')); print(d.get('passes'))")"
  ci="$(python3 -c "import json; d=json.load(open('$out')); print(d.get('paired_95pct_ci'))")"
  change="$(python3 -c "import json; d=json.load(open('$out')); print(d.get('paired_change_pct'))")"
  echo "$(date '+%Y-%m-%dT%H:%M:%S%z') RESULT name=$name passes=$passes change=$change ci=$ci load_after=$(load1)" | tee -a "$DECISION_LOG"
}

mkdir -p "$EVIDENCE"
: > "$LOAD_LOG"
echo "$(date '+%Y-%m-%dT%H:%M:%S%z') START host=$HOST master_candidate=$(git -C "$CANDIDATE" rev-parse HEAD) baseline_cum=$(git -C "$BASELINE_CUM" rev-parse HEAD) baseline_inc=$(git -C "$BASELINE_INC" rev-parse HEAD)" | tee "$DECISION_LOG"

# Prefer 30s; cli may later retry at 18–30 if needed.
run_one incremental-api           inc api            affected-expanded 30
run_one incremental-api-parameters inc api-parameters  consumers         30
run_one incremental-cli           inc cli            affected-expanded 30
run_one cumulative-api            cum api            affected-expanded 30
run_one cumulative-api-parameters cum api-parameters  consumers         30
run_one cumulative-cli            cum cli            affected-expanded 30

echo "$(date '+%Y-%m-%dT%H:%M:%S%z') ALL_SIX_DONE" | tee -a "$DECISION_LOG"
