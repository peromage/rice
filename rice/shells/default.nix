pkgs:

{
  ## NIXPKGS_ALLOW_INSECURE must be explicitly passed when invoking the shell
  etcher = pkgs.mkShell {
    packages = with pkgs; [
      etcher
    ];
  };
}
