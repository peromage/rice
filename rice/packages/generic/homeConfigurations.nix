{ system, homeConfigurations, ... }:

/* Fake derivation */
homeConfigurations.${system} // { type = "derivation"; name = "homeConfigurations"; }
