{ ... }:

let
  fixOverride = f: overrides: let x = f (x // overrides); in x // overrides // {
    override = fixOverride f;
  };

  rice = self: {
    ## Overridden by the caller
    lib = {};
    dirs = {};
    override = fixOverride rice;
  };

in fixOverride rice {}
