
hex2nix
=======

hex2nix is a command that generates correct Nix Expressions from the
Hex.pm repository. It only generates those expressions for packages
that have a buildable Build System. Right now that is `rebar`,
`rebar3`, or `make`. It builds all of these with `rebar3`. That works
for the vast majority of packages but not all.

## Running `hex2nix`

The `hex2nix` command takes one argument. The directory where it will
output the `hex-packages.nix` file.

    $ hex2nix pkgs/development/erlang-modules

If that directory doesn't exist, then it will be created.

## Building `hex2nix'

Assuming you are using Nix, you can simply run `make` and it will
build the command for you.
