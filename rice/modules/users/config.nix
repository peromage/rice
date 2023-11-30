### Option handling

{ config, ... }:

let
  cfg = config.rice.users;

in {
  config = let
    ## Handle users.disableRoot
    rootConfig = if cfg.root.disable
                 then {
                   root = {
                     ## TODO: Remove this plain password
                     hashedPassword = "**DISABLED**";
                   };
                 } else {};

    ## Handle users.users
    userList = with builtins; mapAttrs
      (n: v: {
        isNormalUser = true;
        isSystemUser = false;
        uid = v.id;
        group = n;
        extraGroups = v.groups;
      })
      cfg.users;

    ## Handle users.users
    groupList = with builtins;
      mapAttrs
        (n: v: {
          gid = v.id;
        })
        cfg.users;

    ## Handle users.immutable
    mutableUsers = !cfg.immutable;

  in {
    users.mutableUsers = mutableUsers;
    users.users = userList // rootConfig;
    users.groups = groupList;
  };
}
