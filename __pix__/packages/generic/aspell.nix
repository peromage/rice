/* `aspell' is a successor of `ispell'.

   When this is installed Emacs will be able to switch to `aspell' automatically.
*/

{ pkgs, ... }:

pkgs.aspellWithDicts (apkgs: with apkgs; [
  en
])
