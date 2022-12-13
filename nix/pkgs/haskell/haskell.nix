############################################################################
# Builds Haskell packages with Haskell.nix
############################################################################
{ haskell-nix
, gitignore-nix
, compiler-nix-name
, lib
, libsodium-vrf
}:

let
  project = haskell-nix.project {
    # 'cleanGit' cleans a source directory based on the files known by git
    src = haskell-nix.haskellLib.cleanGit {
      name = "cardano-dex-backend";
      src = ../../../.;
    };

    inherit compiler-nix-name;

    sha256map = {
      "https://github.com/input-output-hk/plutus-apps"."19e1e6cf0e567c0222d723b57438e9a8efa878fb" = "CIuI/Nz7O67ljOHDg7UBbXgWuIE7VPRdPX4VK0/DI3A=";
      "https://github.com/Quid2/flat"."ee59880f47ab835dbd73bea0847dab7869fc20d8" = "1lrzknw765pz2j97nvv9ip3l1mcpf2zr4n56hwlz0rk7wq7ls4cm";
      "https://github.com/input-output-hk/purescript-bridge"."47a1f11825a0f9445e0f98792f79172efef66c00" = "/SbnmXrB9Y2rrPd6E79Iu5RDaKAKozIl685HQ4XdQTU=";
      "https://github.com/input-output-hk/servant-purescript"."44e7cacf109f84984cd99cd3faf185d161826963" = "DH9ISydu5gxvN4xBuoXVv1OhYCaqGOtzWlACdJ0H64I=";
      "https://github.com/input-output-hk/cardano-crypto"."f73079303f663e028288f9f4a9e08bcca39a923e" = "2Fipex/WjIRMrvx6F3hjJoAeMtFd2wGnZECT0kuIB9k=";
      "https://github.com/input-output-hk/cardano-base"."0f3a867493059e650cda69e20a5cbf1ace289a57" = "4b0keLjRaVSdEwfBXB1iT3QPlsutdxSltGfBufT4Clw=";
      "https://github.com/input-output-hk/cardano-prelude"."bb4ed71ba8e587f672d06edf9d2e376f4b055555" = "kgX3DKyfjBb8/XcDEd+/adlETsFlp5sCSurHWgsFAQI=";
      "https://github.com/input-output-hk/typed-protocols"."181601bc3d9e9d21a671ce01e0b481348b3ca104" = "5Wof5yTKb12EPY6B8LfapX18xNZZpF+rvhnQ88U6KdM=";
      "https://github.com/input-output-hk/io-sim"."57e888b1894829056cb00b7b5785fdf6a74c3271" = "TviSvCBEYtlKEo9qJmE8pCE25nMjDi8HeIAFniunaM8=";
      "https://github.com/vshabanov/ekg-json"."00ebe7211c981686e65730b7144fbf5350462608" = "VT8Ur585TCn03P2TVi6t92v2Z6tl8vKijICjse6ocv8=";
      "https://github.com/input-output-hk/cardano-addresses"."b7273a5d3c21f1a003595ebf1e1f79c28cd72513" = "91F9+ckA3lBCE4dAVLDnMSpwRLa7zRUEEBYEHv0sOYk=";
      "https://github.com/input-output-hk/cardano-wallet"."18a931648550246695c790578d4a55ee2f10463e" = "3Rnj/g3KLzOW5YSieqsUa9IF1Td22Eskk5KuVsOFgEQ=";
      "https://github.com/input-output-hk/ouroboros-network"."cb9eba406ceb2df338d8384b35c8addfe2067201" = "3ElbHM1B5u1QD0aes1KbaX2FxKJzU05H0OzJ36em1Bg=";
      "https://github.com/input-output-hk/iohk-monitoring-framework"."066f7002aac5a0efc20e49643fea45454f226caa" = "0ia5UflYEmBYepj2gkJy9msknklI0UPtUavMEGwk3Wg=";
      "https://github.com/input-output-hk/quickcheck-dynamic"."c272906361471d684440f76c297e29ab760f6a1e" = "TioJQASNrQX6B3n2Cv43X2olyT67//CFQqcpvNW7N60=";
      "https://github.com/input-output-hk/cardano-node"."1.35.3-rc1" = "4IrheFeoWfvkZQndEk4fGUkOiOjcVhcyXZ6IqmvkDgg=";
      "https://github.com/input-output-hk/ekg-forward"."297cd9db5074339a2fb2e5ae7d0780debb670c63" = "jwj/gh/A/PXhO6yVESV27k4yx9I8Id8fTa3m4ofPnP0=";
      "https://github.com/input-output-hk/optparse-applicative"."7497a29cb998721a9068d5725d49461f2bba0e7a" = "1gvsrg925vynwgqwplgjmp53vj953qyh3wbdf34pw21c8r47w35r";
      "https://github.com/input-output-hk/Win32-network"."3825d3abf75f83f406c1f7161883c438dac7277d" = "19wahfv726fa3mqajpqdqhnl9ica3xmf68i254q45iyjcpj1psqx";
      "https://github.com/input-output-hk/goblins"."cde90a2b27f79187ca8310b6549331e59595e7ba" = "17c88rbva3iw82yg9srlxjv2ia5wjb9cyqw44hik565f5v9svnyg";
      "https://github.com/input-output-hk/plutus"."a56c96598b4b25c9e28215214d25189331087244" = "coD/Kpl7tutwXb6ukQCH5XojBjquYkW7ob0BWZtdpok=";
      "https://github.com/input-output-hk/cardano-ledger"."c7c63dabdb215ebdaed8b63274965966f2bf408f" = "zTQbMOGPD1Oodv6VUsfF6NUiXkbN8SWI98W3Atv4wbI=";
      "https://github.com/input-output-hk/plutus-apps"."593ffafa59dd30ad28cfaf144c526c66328595d2" = "CIuI/Nz7O67ljOHDg7UBbXgWuIE7VPRdPX4VK0/DI3A=";
      "https://github.com/input-output-hk/hedgehog-extras"."714ee03a5a786a05fc57ac5d2f1c2edce4660d85" = "6KQFEzb9g2a0soVvwLKESEbA+a8ygpROcMr6bkatROE=";
      "https://github.com/ergolabs/cardano-dex-contracts"."2fb44f444897d84e313ceb4d3d467441385802dd" = "Kih0IS6Ty3EnXlgqAyF04nWIWJAnHOEVfraebh5RsNI=";
      "https://github.com/ergolabs/hlog"."231ff5f0a12841174cf6d92a932f45fea4552bae" = "vjqo/cd8eGQ03PsISJrkAizMYouGMc4vZsMl+ozSQ5Y=";
      "https://github.com/ergolabs/cardano-dex-sdk-haskell"."030ea8ce0c7a1a6e903e4db51bcbc60d911383e0" = "tAN2gc2cxTggTUKuXGauKko8m+5HfubywcZEKF7A7xI=";
      "https://github.com/daleiz/rocksdb-haskell"."109af08f95b40f458d4933e3725ecb3e59337c39" = "1i1ya491fapa0g96527krarv0w0iybizqcz518741iw06hhpikiy";
    };

    modules = [
      {
        enableLibraryProfiling = true;
        packages = {
          spectrum-prelude.package.buildable = false;
          amm-executor.package.buildable = false;
          ledger-sync.package.buildable = false;

          # Broken due to haddock errors. Refer to https://github.com/input-output-hk/plutus/blob/master/nix/pkgs/haskell/haskell.nix
          plutus-ledger.doHaddock = false;
          plutus-use-cases.doHaddock = false;

          # See https://github.com/input-output-hk/iohk-nix/pull/488
          cardano-crypto-praos.components.library.pkgconfig = lib.mkForce [ [ libsodium-vrf ] ];
          cardano-crypto-class.components.library.pkgconfig = lib.mkForce [ [ libsodium-vrf ] ];
        };
      }
    ];
  };
in
  project
