{
  /* Example */
  hello_x86_64_linux = { pkgs, ... } : pkgs.mkShell {
    packages = with pkgs.unrestrictedPkgs; [
      hello
    ];
  };
}
