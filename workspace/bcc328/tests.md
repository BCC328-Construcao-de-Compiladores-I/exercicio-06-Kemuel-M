cabal buil [l1,l2,l3]

#L1:
- Análise léxica
cabal run l1 -- --lexer-only test/examples/l1/test1.l1
- Analise sintatica descendente recursiva
cabal run l1 -- --recursive test/examples/l1/test1.l1
- Analise sintatica lalr
cabal run l1 -- --lalr test/examples/l1/test1.l1

# L2:
- Análise léxica
cabal run l2 -- --lexer-only test/examples/l2/test1.l2
- Análise sintática recursiva MegaParsec
cabal run l2 -- --parse-only test/examples/l2/test1.l2
- Interpretação completa
cabal run l2 -- --interpret test/examples/l2/test1.l2
- C Code Generation
cabal run l2 -- --c test/examples/l2/test1.l2


