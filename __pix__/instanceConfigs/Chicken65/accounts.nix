{ ... }:

let
  admin = {
    name = "chick";
    id = 1000;
  };

in {
  users.mutableUsers = false;

  users.users.${admin.name} = with admin; {
    uid = id;
    group = name;
    isNormalUser = true;
    extraGroups = [
      "networkmanager"
      "wheel"
    ];
    initialPassword = "${name}${name}";
  };

  users.groups.${admin.name} = with admin; {
    gid = id;
  };

  users.users.root = {
    ## FIXME: Remove this plain password
    hashedPassword = "**DISABLED**";
  };
}
