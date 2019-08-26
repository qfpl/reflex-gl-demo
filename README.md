# Reflex GL Demo

Demonstration program wiring together Reflex and OpenGL. Presented at
Compose Melbourne on 2019-09-02.

# Building

## Cabal

`cabal v2-build` should fetch and build all necessary dependencies
(including a couple of not-yet-on-hackage ones).

`cabal v2-run reflex-gl-demo` will start the program. WASD moves the
camera, and arrow keys control where it points.

## Nix

If you use [nix](https://nixos.org/nix/), you can enter a shell with
all the development tools installed by running `nix-shell` and waiting
a while.

If you are using nix, and you are not using
[NixOS](https://nixos.org/nixos), you will need to use
[nixGL](https://github.com/guibou/nixGL) to ensure that the program is
run against the right OpenGL libraries.

Run `cabal v2-build --project-file=cabal.project.no-sources` otherwise
cabal will try to pull in sources specified in the `cabal.project`
file. Similarly, launch the program with `cabal v2-run
--project-file=cabal.project.no-sources reflex-gl-demo`.
