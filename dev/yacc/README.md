# yacc - POSIX LALR(1) Parser Generator

## Pipeline

```
.y → lexer → parser → grammar → first_follow → lr0 → lalr → codegen → y.tab.c
     tokens   AST      symbols  FIRST/FOLLOW   DFA   tables  C code
```

## Algorithms

### FIRST/FOLLOW (first_follow.rs)
Fixed-point iteration. For each production A → X₁...Xₙ:
- FIRST(A) ← FIRST(X₁); if X₁ nullable, add FIRST(X₂), ...
- FOLLOW(B) for B in RHS: add FIRST(suffix); if suffix nullable, add FOLLOW(A)

### LR(0) Automaton (lr0.rs)
Worklist on item sets. Item = production with dot: A → α•β.
- Initial = closure({S' → •S$})
- GOTO(I, X) = closure({A → αX•β | A → α•Xβ ∈ I})
- Iterate until fixed point

### LALR(1) Lookaheads (lalr.rs)
DeRemer & Pennello propagation:
1. For each kernel item, compute closure with marker symbol
2. Marker in GOTO target → propagation edge; real symbol → spontaneous lookahead
3. Fixed-point propagate
4. Extend kernel lookaheads to closure via FIRST

### Conflict Resolution (lalr.rs)
Shift/reduce: compare rule precedence vs token precedence.
- Higher wins
- Equal: %left→reduce, %right→shift, %nonassoc→error
- Unresolved: shift wins (default)

Reduce/reduce: earlier production wins (no precedence resolution).

### Table Packing (codegen.rs)
Dense 2D arrays with default compression:
- `action[state × num_terms + term_idx]`: >0=shift, <0=reduce, 0=default, MIN=error
- `goto[state × num_nts + nt_idx]`: ≥0=target, -1=defgoto
- `defact[state]`: most common reduce (prod+1, or 0)
- `defgoto[nt]`: most common goto target
- `consistent[state]`: POSIX optimization—skip yylex() when single reduce

Type optimization: selects smallest C99 type (int8_t/uint8_t through int32_t) per table based on value range.

Bounds checking: rejects grammars exceeding i16 encoding limits (32767 states or productions).

### Formal Verification (verify.rs)
Every invocation decodes packed tables and compares against canonical LALR(1) tables.
Accepts default-action compression (Error→Reduce via defact). Panics on mismatch.

## Generated Parser

Shift-reduce with:
- Hybrid stack: starts on C stack (YYINITDEPTH), malloc+memcpy to heap on first overflow, realloc thereafter
- POSIX consistent-state: skips yylex() in single-reduce states
- Three-phase error recovery: detect→pop to error-shifting state→discard tokens until 3 shifts

Runtime: O(n) for unambiguous grammars.

## POSIX Compliance

Declarations: %token, %left, %right, %nonassoc, %type, %start, %union, %prec
Semantic: $$, $n, $<tag>$, $<tag>n (n can be ≤0 for left-context)
Error handling: error token, yyerrok, yyclearin, YYERROR, YYACCEPT, YYABORT, YYRECOVERING()
Outputs: y.tab.c, y.tab.h (-d), y.output (-v)
#line directives: present by default, -l to omit

## Extensions

| Feature | Description |
|---------|-------------|
| %expect N | Suppress S/R warning if exactly N conflicts |
| %expect-rr N | Suppress R/R warning if exactly N conflicts |
| --strict | Disable consistent-state optimization (preserves yylex timing) |
| Negative $n | $0, $-1 etc. access stack before current rule |
| $<tag>$ | Explicit type cast on LHS |
| $<tag>n | Explicit type cast on RHS reference |

## Data Structures

```
Grammar:
  symbols: Vec<SymbolInfo>      # name, terminal?, tag, prec, assoc
  productions: Vec<Production>  # lhs, rhs, action, prec, line
  term_idx: SymbolId → usize    # dense terminal numbering
  nt_idx: SymbolId → usize      # dense nonterminal numbering

Item: (production_id, dot_position)

LR0Automaton:
  states: Vec<State>            # kernel, full items, transitions
  kernel_to_state: Map<Set<Item>, StateId>

LALRAutomaton:
  action_table: Vec<Map<SymbolId, Action>>
  goto_table: Vec<Map<SymbolId, StateId>>
  conflicts: Map<(StateId, SymbolId), Vec<Action>>

PackedTables:
  action: Vec<i16>              # dense[state * num_terms + term_idx]
  goto: Vec<i16>                # dense[state * num_nts + nt_idx]
  defact: Vec<u16>              # default reduce per state
  defgoto: Vec<i16>             # default goto per nonterminal
  consistent: Vec<bool>         # skip-lookahead states
```
