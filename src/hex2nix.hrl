-record(indexed_deps, {roots :: sets:set(hex2nix:app()),
                       index :: dict:dict(hex2nix:app_name(), [hex2nix:app_version()]),
                       detail :: dict:dict(hex2nix:app(),
                                           hex2nix:app_detail())}).

-record(dep_desc, {app :: hex2nix:app(),
                   description :: h2n_fetcher:description(),
                   position :: h2n_fetcher:position(),
                   licenses :: [h2n_fetcher:license()],
                   homepage :: h2n_fetcher:link(),
                   sha :: h2n_fetcher:sha(),
                   has_native_code :: boolean(),
                   deps :: [hex2nix:app()]}).
