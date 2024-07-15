{ system, rice, ... }:

/* Fake derivation */
rice.homeConfigurations.${system} // { type = "derivation"; name = "homeConfigurations"; }
