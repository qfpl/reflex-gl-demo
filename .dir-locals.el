(("src" . ((nil . ((dante-target . "lib:reflex-gl-demo")))))
 ("app" . ((nil . ((dante-target . "exe:reflex-gl-demo")))))
 ("doctest" . ((nil . ((dante-target . "doctest")))))
 ;("test" . ((nil . ((dante-target . "codeworld-raycaster-test")))))
 (nil . ((dante-repl-command-line
          . ("nix-shell" "--run"
(concat "cabal new-repl " dante-target " --builddir=dist/dante"))))))
