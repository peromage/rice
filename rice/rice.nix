let
  /* Fix point and override pattern: http://r6.ca/blog/20140422T142911Z.html
  */
  fixOverride = f: let x = f x; in x // {
    override = overrides: fixOverride (self: f self // (
      if builtins.isFunction overrides then
        overrides self
      else
        overrides
    ));
  };

  rice = self: {
    /* Attributes are added by the caller on demand */
  };

in fixOverride rice
