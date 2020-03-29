{ nur ? import ~/personal/nur-packages {}
}:
let

  ghcVersion = "ghc882";

  advent-of-code-overlay = self: super:
    let

      dontCheck = super.haskell.lib.dontCheck;

      hsPkgs = super.haskell.packages.${ghcVersion}.extend (selfH: superH: {

        # 0.3.2
        ghc-tcplugins-extra =
          (selfH.callCabal2nix
            "ghc-tcplugins-extra"
            (builtins.fetchGit {
              url = "https://github.com/clash-lang/ghc-tcplugins-extra.git";
              rev = "eda51dccd47522cd26c5cef7c5bf56a52976864b";
            })
            {});

        monad-dijkstra = dontCheck superH.monad-dijkstra;

        polysemy =
          (selfH.callCabal2nix
            "polysemy"
            (builtins.fetchGit {
              url = "https://github.com/polysemy-research/polysemy.git";
              rev = "016c16fbb1b57a0d728e57e2cf8e36453e8edd8d";
            })
            {});

      });

    in
      {
        haskell = super.haskell // {
          inherit ghcVersion;
          packages = super.haskell.packages // {
            "${ghcVersion}" = super.haskell.packages.${ghcVersion} // hsPkgs;
          };
        };
      };

in

nur.lib.haskellDevShell {

  nixpkgsRev = "c2dcdea8c68631fc15ec837d0df7def2b66d0676";

  nixpkgsArgs = {
    overlays = [
      (nur.overlays.haskell-dev { inherit ghcVersion; })
      advent-of-code-overlay
    ];
  };

  pkg = {
    name = "advent-of-code";
    path = ./.;
    args = {};
  };

}
