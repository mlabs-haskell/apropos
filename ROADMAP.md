- Split the repo into `apropos` and `apropos-plutus`
  - move `Apropos.Script` and `Apropos.Tx` to `apropos-plutus`
- Implement `Apropos.Tx`
  - This could depend on `plutus-simple-model` or be influenced by its design
- Consider design of `Apropos.Gen`
  - possibly create a Free Monad abstraction layer to hide implementation details
- Consider HasParameterisedEnumerator for types that are easy to exhaustively enumerate
  - if the type has <100 inhabitants it may be preferable to enumerate rather than randomly generate them
  - this may be aided by a better Gen abstraction that allows us to compose the enuerated tests into
  a single property that runs once.
- Create road plan for `apropos-plutus` standard library models
  - having a shared library of specifications could make the library more usable
  - this shared library might be best developed by encouraging upstreaming from library users

