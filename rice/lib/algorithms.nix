{ self, nixpkgs, ... }:

let
  inherit (nixpkgs.lib) foldAttrs optionalAttrs listToAttrs  mapAttrsToList nameValuePair any all id foldl';

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
       concatAttrs :: AttrSet -> AttrSet -> AttrSet
  */
  concatAttrs = x: y: x // y;

  /* Prepend a prefix to a list of strings.

     Type:
       prefixWith :: String -> [String] -> [String]
  */
  prefixWith = prefix: list: map (concatStr prefix) list;

  /* Merge a list of attribute sets.

     Type:
       mergeAttrs :: [AttrSet] -> AttrSet
  */
  mergeAttrs = listOfAttrs: foldl' concatAttrs {} listOfAttrs;

  /* Like `mergeAttrs' but merge the first level instead of top level.

     Type:
       mergeAttrsFirstLevel :: [AttrSet] -> AttrSet
  */
  mergeAttrsFirstLevel = listOfAttrs: foldAttrs concatAttrs {} listOfAttrs;

  /* Apply optionalAttrs on each attribute set from the list.
     Each element in the list is of the form as follow:
       { cond = expr; as = attrset; }

     Type:
       optionalAttrsList :: [AttrSet] -> [AttrSet]
  */
  optionalAttrsList = listOfConds: (map (a: optionalAttrs a.cond a.as) listOfConds);

  /* Like `mergeAttrs' but merge attribute sets based on each'es predicate.

     Type:
       mergeAttrsCond :: [AttrSet] -> AttrSet
  */
  mergeAttrsCond = listOfConds: mergeAttrs (optionalAttrsList listOfConds);

  /* Return the first non-null value between a and b.
     If both are null the result is null.

     Type:
       either :: a -> a -> a
  */
  either = a: b: if null != a then a else if null != b then b else null;

  /* Like `either' but a default return value can be specified.

     Type:
       either' :: a -> a -> a -> a
  */
  either' = a: b: r: let x = either a b; in if null != x then x else r;

  /* Return the first argument passed to this function.

     Type:
       pairName :: a -> a -> a
  */
  pairName = n: v: n;

  /* Return the second argument passed to this function.

     Type:
       pairName :: a -> a -> a
  */
  pairValue = n: v: v;

  /* Map a list to an attrs.

     `fn' and `fv' are used to map each element from the list to the name and
     value respectively.

     Type:
       mapListToAttrs :: (a -> String) -> (a -> a) -> [a] -> AttrSet
  */
  mapListToAttrs = fn: fv: list:
    listToAttrs (map (i: nameValuePair (fn i) (fv i)) list);

  /* Return true if the function pred returns true for at least one element of
     attrs, and false otherwise.

     Type:
       anyAttrs :: (String -> a -> Bool) -> AttrSet -> Bool

  */
  anyAttrs = pred: attrs: any id (mapAttrsToList pred attrs);

  /* Return true if the function pred returns true for all elements of attrs,
     and false otherwise.

     Type:
       anyAttrs :: (String -> a -> Bool) -> AttrSet -> Bool
  */
  allAttrs = pred: attrs: all id (mapAttrsToList pred attrs);
}
