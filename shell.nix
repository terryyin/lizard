{ pkgs ? import <nixpkgs> { }, nodejs ? pkgs.nodejs }:
with pkgs;
let
  apple_sdk = darwin.apple_sdk.frameworks;
in mkShell {
  name = "lizard";
  buildInputs = [
    python3Full
    python39Packages.pip
    python39Packages.setuptools
    python39Packages.virtualenv
    nodejs
    vim
    git less
  ];
  shellHook = ''
    # Prepare when missing or when dependency inputs change; otherwise reuse.
    _lizard_req=dev_requirements.txt
    _lizard_stamp=.venv/.dev_requirements.sha256
    _lizard_hash="$(python -c 'import hashlib, pathlib, sys; print(hashlib.sha256(pathlib.Path(sys.argv[1]).read_bytes()).hexdigest())' "$_lizard_req")"
    if [ ! -x .venv/bin/python ] || [ ! -f "$_lizard_stamp" ] || [ "$(cat "$_lizard_stamp" 2>/dev/null)" != "$_lizard_hash" ]; then
      if [ ! -x .venv/bin/python ]; then
        python -m venv .venv || exit $?
      fi
      .venv/bin/pip install -r "$_lizard_req" || exit $?
      printf '%s\n' "$_lizard_hash" > "$_lizard_stamp" || exit $?
    fi
    source .venv/bin/activate

    # Add local bin to PATH
    export PATH=$PATH:$PWD/.venv/bin
  '';
}
