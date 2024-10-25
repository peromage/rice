{ self, nixpkgs, ... }:

let
  lib = nixpkgs.lib;

in with self; {
  /* Join a list of strings/paths with separaters.

     Type:
       joinStrs :: String -> [Any] -> String
  */
  join = sep: list: with lib; foldl' (a: i: a + "${sep}${i}") (head list) (tail list);

  /* Apply a list of arguments to the function.

     Type:
       apply :: (Any -> Any) -> [Any] -> Any
  */
  apply = lib.foldl' (f: x: f x);

  /* Filter the return value of the original function.

     Note that n (the number of arguments) must be greater than 0 since a
     function should at least have one argument.  This is required because for
     curried functions the number of arguments can not be known beforehand.  The
     caller must tell this function where to end.

     Type:
       wrapReturn :: Number -> (Any -> Any) -> (Any -> ... -> Any) -> Any
  */
  wrapReturn = n: wf: f:
    let wrap = f: n: a: if n == 1 then wf (f a) else wrap (f a) (n - 1);
    in assert n > 0; wrap f n;

  /* Filter the arguments of the original function.

     Note that n (the number of arguments) must be greater than 0 since a
     function should at least have one argument.  This is required because for
     curried functions the number of arguments can not be known beforehand.  The
     caller must tell the wrapper function where to end.

     The wrapper function must have the same signature of the original function
     and return a list of altered arguments.

     Type:
       wrapArgs :: Number -> (Any -> ... -> [Any]) -> (Any -> ... -> Any) -> Any
  */
  wrapArgs = n: wf: f:
    let wrap = wf: n: a: if n == 1 then apply f (wf a) else wrap (wf a) (n - 1);
    in assert n > 0; wrap wf n;

  /* Fix point and override pattern.
     See: http://r6.ca/blog/20140422T142911Z.html
  */
  fixOverride = f: let x = f x; in x // {
    override = overrides: fixOverride (self: f self // (
      if builtins.isFunction overrides then
        overrides self
      else
        overrides
    ));
  };
}
