/* `aspell' is a successor of `ispell'.

   When this is installed Emacs will be able to switch to `aspell' automatically.
*/

{ pkgs, ... }:

let
  useAspellWith = with pkgs; aspellWithDicts;
  aspellDicts = apkgs: with apkgs; [
    en
  ];

in useAspellWith aspellDicts
