{ system, pix, ... }:

/* Fake derivation */
pix.homeConfigurations.${system} // { type = "derivation"; name = "homeConfigurations"; }
