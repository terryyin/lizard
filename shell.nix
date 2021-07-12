{ pkgs ? import <nixpkgs> { } }:
with pkgs;
let
  apple_sdk = darwin.apple_sdk.frameworks;
in mkShell {
  name = "lizard";
  MYSQL_HOME = builtins.getEnv "MYSQL_HOME";
  MYSQL_DATADIR = builtins.getEnv "MYSQL_DATADIR";
  buildInputs = [
    python3Full
    python39Packages.pip
    python39Packages.setuptools
    vim
    git less
  ];
  shellHook = ''
    export PYTHONUSERBASE=$PWD/.local
    export USER_SITE=`python -c "import site; print(site.USER_SITE)"`

    # bug? it will print 3.8 somehow
    export USER_SITE=${"\$\{USER_SITE//3.8/3.9}"}

    export PYTHONPATH=$PYTHONPATH:$USER_SITE
    export PATH=$PATH:$PYTHONUSERBASE/bin
    make deps
  '';

}
