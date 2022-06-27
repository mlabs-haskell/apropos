The objective of the `apropos` project is to provide usable testing for Plutus dApps. This includes the neccesary test harnesses and runners, as well as the generation of appropriate test data.

This result will be composed of three components -

* `apropos` (this repo) - a pure Haskell testing framework for generating test data, appropriate for (but not exclusive to) testing Plutus dApps.  
  **STATUS:** Functionally complete, documentation and examples need completing and coding style needs improving.

* [`plutus-simple-model`](https://github.com/mlabs-haskell/plutus-simple-model) - A Haskell library for running Plutus transactions with their scripts.  
  **STATUS:** Lacks GHC 9 support, needed for Plutarch in current projects under development.

* `hedgehog-plutus-simple` - Test drivers for the Hedgehog testing framework and `plutus-simple-model`.  
  **STATUS:** Not started, awaiting GHC 9 support on `plutus-simple-model`.

The following tasks remain to be completed for MVP:

* Complete Haddocks for `apropos`.

* More examples for `apropos`.

* GHC 9 support on `plutus-simple-model`.

* `hedgehog-plutus-simple` implementation - not expected to be too involved.

Following this, communication with teams using the `apropos` project will establish
priorities for further functionality.
