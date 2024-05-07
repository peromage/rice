{ self, nixpkgs, ... }:

let
  inherit (nixpkgs) foldl' head tail;

in with self; {
  /* Join a list of strings/paths with separaters.

     Type:
       join :: String -> [Any] -> String
  */
  join = sep: list: foldl' (a: i: a + "${sep}${i}") (head list) (tail list);
}
