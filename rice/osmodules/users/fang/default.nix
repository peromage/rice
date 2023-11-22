{ rice, ... }:

let
  name = "fang";
  uid = 1001;

in
rice.lib.createSudoUser name uid
