let
  packages = import ./.;
  inherit (packages) pkgs cardano-dex-backend;
  inherit (cardano-dex-backend) haskell;

in
  haskell.project.shellFor {
    withHoogle = false;

    shellHook = "
              export LC_CTYPE=C.UTF-8
              export LC_ALL=C.UTF-8
              export LANG=C.UTF-8
              ";

    nativeBuildInputs = with cardano-dex-backend; [
      hlint
      cabal-install
      haskell-language-server
      stylish-haskell
      pkgs.niv
      cardano-repo-tool
      pkgs.ghcid
      # HACK: This shouldn't need to be here.
      pkgs.lzma.dev
    ];

    buildInputs = with cardano-dex-backend; [
      pkgs.rdkafka
      pkgs.leveldb
    ];
  }