{ self, nixpkgs, ... }:

let
  lib = nixpkgs.lib;

in with self; {
  ## Concatenations
  strConcat = a: b: a + b;

  listConcat = a: b: a ++ b;

  attrConcat = a: b: a // b;

  ## Prepend a prefix to a list of strings
  withPrefix = prefix: list: map (strConcat prefix) list;

  ## Merge a list of attribute sets
  attrsFoldl = listOfAttrs: builtins.foldl' attrConcat {} listOfAttrs;

  ## Like attrFoldl but merge attribute sets based on each'es predicate.
  ## Each element in the list is an attribute set as follow:
  ##   { cond = expr; as = attrSet; }
  attrsCondFoldl = list: attrsFoldl (map (attr: lib.optionalAttrs attr.cond attr.as) list);
}
