with import <nixpkgs> {};

stdenv.mkDerivation {
    name = "node";
    buildInputs = [
        nodejs-16_x
        (yarn.override { nodejs = nodejs-16_x; })
        yarn-bash-completion
        bash
        elmPackages.elm
        elmPackages.elm-live
        elmPackages.elm-analyse
        elmPackages.elm-coverage
        elmPackages.elm-format
        elmPackages.elm-test
    ];
    shellHook = ''
        # export PATH="$PWD/node_modules/.bin/:$PATH"
        export NODE_FLAGS="--experimental-repl-await --async-stack-traces"
        export PLAYWRIGHT_SKIP_BROWSER_DOWNLOAD=1
        . ~/.bashrc
    '';
}