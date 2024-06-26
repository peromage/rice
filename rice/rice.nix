{ ... }:

let
  fixOverride = f: overrides: let x = f (x // overrides); in x // overrides // {
    override = fixOverride f;
  };

  rice = self: {
    /* Attributes are added by the caller on demand */
    override = fixOverride rice;
  };

in fixOverride rice {}
