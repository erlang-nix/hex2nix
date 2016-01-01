
hex2nix
=======

hex2nix is a command that generates correct Nix Expressions from the
Hex.pm repository. It only generates those expressions for packages
that have a buildable Build System. Right now that is `rebar`,
`rebar3`, or `make`. It builds all of these with `rebar3`. That works
for the vast majority of packages but not all.

## Running `hex2nix`

```
Usage: hex2nix [-h [<output_path>]] [-r [<registry_url>]] [-c [<cache>]]
               [-h [<help>]]

  -o, --output-path   Output Path for the `hex-packages.nix` file
                      [default: ./]
  -r, --registry-url  The url of the registry to base generation on
                      [default:
                      https://s3.amazonaws.com/s3.hex.pm/registry.ets.gz]
  -c, --cache-result  Cache the result of package download so it can be
                      reused. This is primarily useful for testing.
                      [default: false]
  -h, --help          Print the help message for this command [default:
                      false]

```

You can run `hex2nix` with no arguments and it will dump a
`hex-packages.nix` file in the current directory. Optionally, you can
give it an `output-path` and point that to the erlang-modules
directory of your nixpkgs repo. That is usually
`pkgs/development/erlang-modules`. By default it pulls from the
current canonical Hex.pm registry, howeever you can give it a
different url if you so desire. Finally, you can tell it to cache the
intermediate results. This is mostly useful during development of
`hex2nix` so it doesn't have to redownload the entire package
repository for every run.

## Building `hex2nix`

Assuming you are using Nix, you can simply run `make` and it will
build the command for you.
