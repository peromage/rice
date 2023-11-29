{ self, nixpkgs, ... }:

let
  lib = nixpkgs.lib;

in with self; {
  /* Concatenate strings.

     Type:
       strConcat :: String -> String -> String
  */
  strConcat = x: y: x + y;

  /* Concatenate lists.

     Type:
       listConcat :: [a] -> [a] -> [a]
  */
  listConcat = x: y: x ++ y;

  /* Concatenate (merge) attribute sets.

     Type:
       attrConcat :: AttrSet -> AttrSet -> AttrSet
  */
  attrConcat = x: y: x // y;

  /* Prepend a prefix to a list of strings.

     Type:
       withPrefix :: String -> [String] -> [String]
  */
  withPrefix = prefix: list: map (strConcat prefix) list;

  /* Merge a list of attribute sets.

     Type:
       attrsFoldl :: [AttrSet] -> AttrSet
  */
  attrsFoldl = listOfAttrs: builtins.foldl' attrConcat {} listOfAttrs;

  /* Like `attrFoldl' but merge attribute sets based on each'es predicate.
     Each element in the list is an attribute set as follow:
       { cond = expr; as = attrset; }

     Type:
       attrsCondFoldl :: [AttrSet] -> AttrSet
  */
  attrsCondFoldl = listOfConds: attrsFoldl (map (attr: lib.optionalAttrs attr.cond attr.as) listOfConds);
}
