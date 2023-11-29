{ self, nixpkgs, ... }:

let
  lib = nixpkgs.lib;

in with self; {
  /* Concatenate strings.

     Type:
       concatStr :: String -> String -> String
  */
  concatStr = x: y: x + y;

  /* Concatenate lists.

     Type:
       concatList :: [a] -> [a] -> [a]
  */
  concatList = x: y: x ++ y;

  /* Concatenate (merge) attribute sets.

     Type:
       concatAttr :: AttrSet -> AttrSet -> AttrSet
  */
  concatAttr = x: y: x // y;

  /* Prepend a prefix to a list of strings.

     Type:
       prefixWith :: String -> [String] -> [String]
  */
  prefixWith = prefix: list: map (concatStr prefix) list;

  /* Merge a list of attribute sets.

     Type:
       mergeAttrs :: [AttrSet] -> AttrSet
  */
  mergeAttrs = listOfAttrs: builtins.foldl' concatAttr {} listOfAttrs;

  /* Like `mergeAttrs' but merge attribute sets based on each'es predicate.
     Each element in the list is an attribute set as follow:
       { cond = expr; as = attrset; }

     Type:
       mergeAttrCond :: [AttrSet] -> AttrSet
  */
  mergeAttrsCond = listOfConds: mergeAttrs (map (attr: lib.optionalAttrs attr.cond attr.as) listOfConds);
}
