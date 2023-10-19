# Apheleia

## Compilation stages:
1. Bookwyrm: Source text -> Token stream
2. Mandrake: Token stream -> Concrete Syntax Tree
3. Narcissus: CST roots -> HIR
   - Perform name resolution on the CST and lower complex constructs (such as while) to simple ones
   (such as loops)
4. Hydra: Type checking + Inferencing the HIR
   - Perform type inference, checking, and generally finish resolving type info

## Other crates:
- Bestiary: Types, unspecific to any one representation
- Prism: Generalized helpers related to IR representation and the Database backing the compiler
- Macro: Macros, mostly related to Prism