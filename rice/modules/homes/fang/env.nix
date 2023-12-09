{ ... }:

{
  /* Alternatively source in a manual way:
     ~/.nix-profile/etc/profile.d/hm-session-vars.sh
     or
     /etc/profiles/per-user/fang/etc/profile.d/hm-session-vars.sh
  */
  home.sessionVariables = {
    EDITOR = "vim";
  };

  home.sessionPath = [
    "\${HOME}/bin"
    "\${HOME}/.local/bin"
  ];
}
