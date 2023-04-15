# symbols

Manipulate type-level strings.

Available on [Hackage](https://hackage.haskell.org/package/symbols)

The implementation is described in [this blog post](https://kcsongor.github.io/symbol-parsing-haskell/).

## Contribute

### Nix

You can use a [Nix flake](https://nixos.wiki/wiki/Flakes) from this repo to get several development tools.

1. [Enable flakes](https://nixos.wiki/wiki/Flakes#Enable_flakes).

2. Run `nix develop`. This command will make available `cabal`, `ghc`, and `haskell-language-server`.

3. Run `cabal build` to build the project.
