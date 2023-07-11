let
  pinnedNixHash = "02336c5c5f719cd6bd4cfc5a091a1ccee6f06b1d";


  pinnedNix =
    builtins.fetchGit {
      name = "nixpkgs-pinned";
      url = "https://github.com/NixOS/nixpkgs.git";
      rev = "${pinnedNixHash}";
    };

  erlangReleases =
    builtins.fetchGit {
      name = "nixpkgs-nixerl";
      url = "https://github.com/id3as/nixpkgs-nixerl.git";
      rev = "32e4ad0c60b218038bbf8582dc0cbcebecc81302";
    };

  purerlReleases =
    builtins.fetchGit {
      url = "https://github.com/purerl/nixpkgs-purerl.git";
      ref = "master";
      rev = "c6df9171735273d5a734438bd5d235fe9c42a2e4";
    };

  easy-ps = import
    (nixpkgs.pkgs.fetchFromGitHub {
      ## Temporarily on Fabrizio's fork to get spago-next
      owner = "f-f";
      repo = "easy-purescript-nix";
      rev = "2e62b746859e396e541bdd63dbd10b2f231027d4";
      sha256 = "sha256-qQpWKE40wKkxb4y2+z0n4lF/OFrCsEU3Gwiugl3H+xc=";
    }) { pkgs = nixpkgs; };

  nixpkgs =
    import pinnedNix {
      overlays = [
        (import erlangReleases)
        (import purerlReleases)
      ];
    };

  erlang = nixpkgs.nixerl.erlang-26-0;

in

with nixpkgs;

let
    inherit (stdenv.lib) optionals;
in

mkShell {
  buildInputs = with pkgs; [

    erlang.erlang
    erlang.rebar3.rebar3
    erlang.erlang-ls

    # Purescript
    easy-ps.purs-0_15_9-2
    easy-ps.spago-next
    easy-ps.purescript-language-server
    easy-ps.purs-tidy
    purerl.purerl-0-0-20

  ];
}
