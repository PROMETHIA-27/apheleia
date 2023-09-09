# Apheleia

## Compilation stages:
1. Bookwyrm: Source text -> Token stream
2. Mandrake: Token stream -> Concrete Syntax Tree
3. Hydra: Concrete Syntax Tree -> Abstract Syntax Tree
4. Narcissus: Vec<AST> -> Module Tree
   - Merge all file ASTs together into one module tree, walking the AST to collect all declarations
   and performing name resolution