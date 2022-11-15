Apropos is a project whose goal is intended to allow effective testing of Plutus contracts. This spans running Plutus dApps as full transactions in test suites, as well as generating appropriate test data.

The Apropos approach to testing is ‘description tests’ - property testing, but reusing tests for input data meeting different properties.

## Project scope

The Apropos project (Apropos-the-project) enompasses at core two repos:

- [`apropos`](https://github.com/mlabs-haskell/apropos) (apropos-the-repo)
Description-type based testing. apropos-the-repo is *Plutus-free* - see below.
- [`hedgehog-plutus-simple`](https://github.com/mlabs-haskell/hedgehog-plutus-simple)
Harnesses to test contracts using `hedgehog` and [`plutus-simple-model`](https://github.com/mlabs-haskell/plutus-simple-model).

Together, these items intend to provide usable and credible end-to-end testing of Plutus contract flows - not just individual scripts.

In addition, the following repo is a dependency of [`hedgehog-plutus-simple`](https://github.com/mlabs-haskell/hedgehog-plutus-simple). Since its developer and hitherto maintainer is not currently at the company, its maintenance has been subsumed into Apropos-the-project:

- [`plutus-simple-model`](https://github.com/mlabs-haskell/plutus-simple-model)

## Plutus-free

The Apropos development philosophy divides projects between *Plutus-specific* - functionality relevant only to Plutus development - and *Plutus-free* - functionality that is also useful for pure Haskell development, and as such is kept free of Plutus dependency. As such, Apropos-the project supplies a way of specifying Plutus contract flows as Hedgehog tests ([`hedgehog-plutus-simple`](https://github.com/mlabs-haskell/hedgehog-plutus-simple)), as well as description-based testing ([`apropos`](https://github.com/mlabs-haskell/apropos)-the-repo), which combined, constitute an end-to-end testing solution for Plutus contracts.

## Project board

The project board for Apropos is here: <https://github.com/orgs/mlabs-haskell/projects/33>