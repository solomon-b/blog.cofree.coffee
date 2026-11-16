{
  description = "blog.cofree.coffee";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";

    # The posts import these libraries. They are not in nixpkgs.
    # The head of feature/todo-behavior. "Chat Bots Revisited" describes this revision.
    cofree-bot = {
      url = "github:cofree-coffee/cofree-bot/ab88844b2f8e8278d095ff6aa7e68e9984432987";
      flake = false;
    };
    # That revision pins this revision in its cabal.project.
    monoidal-functors = {
      url = "github:solomon-b/monoidal-functors/e951a2be496d57d7f4e5b582ad175e8e97a9ab7b";
      flake = false;
    };
    # The revision that cofree-bot main pins. The older revision that the branch
    # pins does not build with base64-1.0.
    matrix-client = {
      url = "github:softwarefactory-project/matrix-client-haskell/57f13c2579fbcd9088fbb6b25046460e6cdbfdb0";
      flake = false;
    };
    # The head of main on 2026-11-13. "Servant Auth Roles" ends with this library.
    servant-auth-roles = {
      url = "github:solomon-b/servant-auth-roles/2f7728174307a707c757fba5dde28f0fea2155bf";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, cofree-bot, monoidal-functors, matrix-client, servant-auth-roles }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f (import nixpkgs {
        inherit system;
        overlays = [ self.overlays.default ];
      }));
    in
    {
      overlays.default = final: prev: {
        haskellPackages = prev.haskellPackages.override {
          overrides = hfinal: hprev:
            let
              inherit (final.haskell.lib.compose) doJailbreak dontCheck;
              fromCofreeBot = name: dontCheck (hfinal.callCabal2nix name "${cofree-bot}/${name}" { });
            in
            {
              chat-bots = fromCofreeBot "chat-bots";
              chat-bots-contrib = fromCofreeBot "chat-bots-contrib";
              cofree-bot = fromCofreeBot "cofree-bot";
              list-t = fromCofreeBot "list-t";
              machines-coalgebras = fromCofreeBot "machines-coalgebras";
              servant-auth-roles = dontCheck (hfinal.callCabal2nix "servant-auth-roles" servant-auth-roles { });
              monoidal-functors = hfinal.callCabal2nix "monoidal-functors" monoidal-functors { };
              # The pinned revision has stale upper bounds on hashable and time.
              matrix-client = doJailbreak (hfinal.callCabal2nix "matrix-client" matrix-client { });
            };
        };
      };

      devShells = forAllSystems (pkgs: {
        default = pkgs.mkShell {
          packages = [
            pkgs.gnumake
            pkgs.pandoc
            # Emacs tangles the Haskell blocks out of the org files.
            pkgs.emacs-nox
            # Happy and Alex generate Haskell from the grammar blocks.
            pkgs.haskellPackages.alex
            pkgs.haskellPackages.happy
            # "Bounded Space Automata" uses Idris 1. "Finally Modular Arithmetic" uses Idris 2.
            pkgs.idris
            pkgs.idris2
            # GHC typechecks the tangled files. Add the libraries that the posts import.
            (pkgs.haskellPackages.ghcWithPackages (hpkgs: [
              hpkgs.chat-bots
              hpkgs.chat-bots-contrib
              hpkgs.cofree-bot
              hpkgs.containers
              hpkgs.data-fix
              hpkgs.lens
              hpkgs.list-t
              hpkgs.machines
              hpkgs.machines-coalgebras
              hpkgs.megaparsec
              hpkgs.monoidal-functors
              hpkgs.mtl
              hpkgs.profunctors
              hpkgs.random
              hpkgs.semialign
              hpkgs.servant-auth-roles
              hpkgs.servant-server
              hpkgs.text
              hpkgs.these
              hpkgs.transformers
              hpkgs.typed-process
              hpkgs.unordered-containers
              hpkgs.wai
            ]))
          ];
        };
      });
    };
}
