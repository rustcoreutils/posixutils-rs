# yacc - POSIX Parser Generator

## Pipeline

```
.y file → lexer.rs → parser.rs → grammar.rs → first_follow.rs → lr0.rs → lalr.rs → codegen.rs → y.tab.c
          (tokens)   (AST)       (symbols)    (FIRST/FOLLOW)    (LR(0))   (LALR(1))  (C code)
```

## Algorithms

### FIRST/FOLLOW Sets (first_follow.rs)
**Fixed-point iteration** - compute parse-guiding sets.

- FIRST(X) = terminals that can begin strings derived from X
- FOLLOW(A) = terminals that can appear immediately after nonterminal A
- Nullable = symbols that can derive ε

For each production A → X₁X₂...Xₙ:
1. Add FIRST(X₁) to FIRST(A); if X₁ nullable, add FIRST(X₂), etc.
2. For B in RHS at position i with suffix β: add FIRST(β) to FOLLOW(B)
3. If β nullable, add FOLLOW(A) to FOLLOW(B)

### LR(0) Automaton (lr0.rs)
**Worklist algorithm** - builds DFA of item sets.

Item = production with dot marking parse progress: A → α•β

1. Initial state = closure({S' → •S$})
2. For each state and grammar symbol X:
   - GOTO(state, X) = closure({A → αX•β | A → α•Xβ ∈ state})
3. Add new states to worklist until fixed point

**Closure**: For A → α•Bβ where B is nonterminal, add B → •γ for all B-productions.

### LALR(1) Lookaheads (lalr.rs)
**Propagation algorithm** - computes lookahead sets for reduce actions.

Uses marker symbol to distinguish:
- **Spontaneous**: lookahead from FIRST(β) where item is A → α•Bβ
- **Propagation**: lookahead inherited from predecessor item

1. For each kernel item, compute closure with marker lookahead
2. Record propagation edges and spontaneous lookaheads
3. Fixed-point propagate until stable
4. Extend kernel lookaheads to closure items

### Conflict Resolution
**Precedence/associativity** - resolves shift/reduce conflicts.

- Compare rule precedence vs lookahead token precedence
- Higher precedence wins
- Equal: %left → reduce, %right → shift, %nonassoc → error
- Reduce/reduce: earlier rule wins (no precedence resolution)

### Table Packing (codegen.rs)
**Dense indexing** with default actions.

- ACTION[state × num_terminals + term_idx]: >0=shift, <0=reduce, 0=default, MIN=error
- GOTO[state × num_nonterminals + nt_idx]: ≥0=target, -1=use defgoto
- defact[state]: most common reduce action (compression)
- consistent[state]: POSIX optimization - skip yylex() when only one reduce

### Formal Verification (verify.rs)
**Exhaustive table check** - runs every invocation.

Decodes packed tables and compares against canonical LALR(1) tables.
Accepts default-action optimization (Error → Reduce via defact).
Panics on mismatch (internal bug detection).

## Modules

| Module | Purpose |
|--------|---------|
| `main.rs` | CLI (-b, -d, -l, -p, -t, -v), orchestration |
| `lexer.rs` | Tokenize .y files (directives, literals, actions) |
| `parser.rs` | Parse declarations, rules, programs sections |
| `grammar.rs` | Symbol table, productions, precedence, dense indexing |
| `first_follow.rs` | FIRST/FOLLOW/nullable computation |
| `lr0.rs` | LR(0) automaton construction |
| `lalr.rs` | LALR(1) lookahead computation, ACTION/GOTO tables |
| `codegen.rs` | C parser emission, table packing, semantic actions |
| `verify.rs` | Formal verification of packed vs canonical tables |
| `diag.rs` | GCC-style error/warning output |
| `error.rs` | Error types (lexical, syntax, grammar, I/O) |

## Key Data Structures

```
Grammar:
  symbols: Vec<SymbolInfo>           # name, terminal?, tag, precedence, assoc
  productions: Vec<Production>       # lhs, rhs, action, precedence
  term_idx: Map<SymbolId, usize>     # dense terminal indexing
  nt_idx: Map<SymbolId, usize>       # dense nonterminal indexing

Item:
  production: ProductionId           # which rule
  dot: usize                         # position in RHS (0..len)

LR0Automaton:
  states: Vec<State>                 # kernel items, full closure, transitions
  kernel_to_state: Map<Set<Item>, StateId>

LALRAutomaton:
  action_table: Vec<Map<SymbolId, Action>>   # state → terminal → action
  goto_table: Vec<Map<SymbolId, StateId>>    # state → nonterminal → state
  conflicts: Map<(StateId, SymbolId), Vec<Action>>

PackedTables:
  action: Vec<i16>                   # dense[state * num_terms + term_idx]
  goto: Vec<i16>                     # dense[state * num_nts + nt_idx]
  defact: Vec<u16>                   # default reduction per state
  defgoto: Vec<i16>                  # default goto per nonterminal
  consistent: Vec<bool>              # POSIX skip-lookahead states
```

## Generated Parser

**Shift-reduce parser** with:
- Hybrid stack allocation (C stack initially, heap on overflow via realloc)
- POSIX consistent-state optimization (skip yylex in single-reduce states)
- Three-phase error recovery per POSIX errflag protocol (0→3→discard→pop)

Runtime: O(n) for unambiguous grammars, input length n.

## POSIX Compliance

- LALR(1) parsing with S' → S$ augmentation
- %token, %left, %right, %nonassoc, %type, %start, %prec
- %union for YYSTYPE, semantic values via $$ and $n
- %expect, %expect-rr for conflict suppression
- Error recovery with `error` token, yyerrok, yyclearin
- YYERROR, YYACCEPT, YYABORT, YYRECOVERING()
- #line directives (-l to omit)
- y.tab.c, y.tab.h (-d), y.output (-v)

## Extensions

- %expect-rr for reduce/reduce conflict count
- --strict mode to disable consistent-state optimization
- Negative $ references ($0, $-1) for left-context access
- $<tag>$ and $<tag>n for explicit type casts

## Output Files

| File | Content |
|------|---------|
| y.tab.c | Parser tables + yyparse() + semantic actions |
| y.tab.h | Token defines + YYSTYPE (-d flag) |
| y.output | Grammar summary, states, conflicts (-v flag) |
