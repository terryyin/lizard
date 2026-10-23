{ pkgs ? import <nixpkgs> { } }:
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
    vim
    git less
  ];
  shellHook = ''
    # Create and activate virtual environment
    python -m venv .venv
    source .venv/bin/activate
    
    # Install dependencies
    pip install -r dev_requirements.txt
    
    # Add local bin to PATH
    export PATH=$PATH:$PWD/.venv/bin
  '';
}
