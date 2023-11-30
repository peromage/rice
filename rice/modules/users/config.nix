### Option handling

{ config, ... }:

let
  cfg = config.rice.users;

in {
  config = let
    ## Handle user.disableRoot
    rootConfig = if ! cfg.disableRoot
                 then {}
                 else {
                   root = {
                     ## TODO: Remove this plain password
                     hashedPassword = "**DISABLED**";
                   };
                 };

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

    ## Handle user.immutable
    mutableUsers = !cfg.immutable;

  in {
    users.mutableUsers = mutableUsers;
    users.users = userList // rootConfig;
    users.groups = groupList;
  };
}
