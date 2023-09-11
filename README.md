# Apheleia

## Compilation stages:
1. Bookwyrm: Source text -> Token stream
2. Mandrake: Token stream -> Concrete Syntax Tree
3. Hydra: Concrete Syntax Tree -> Abstract Syntax Tree
4. Narcissus: AST roots -> Module Tree
   - Merge all file ASTs together into one module tree, walking the AST to collect all declarations
   and performing name resolution

## Other crates:
- Prism: Generalized helpers related to IR representation and the Database backing the compiler
- Macro: Macros, mostly related to Prism