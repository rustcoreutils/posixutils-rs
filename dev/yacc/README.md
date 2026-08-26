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

Reduce/reduce: earlier production wins (no precedence resolution). An N-way
reduce/reduce counts as N-1 conflicts, matching bison, so `%expect-rr` is
satisfiable above 2.

A rule's precedence comes from its last terminal, not its last terminal *with* a
precedence — taking the latter silently resolved conflicts POSIX says to report.

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

### Grammar Validation (grammar.rs)

Each non-terminal is checked for **productivity** — does it derive some string of
terminals? One that does not gets a warning naming the line of its first rule; it
is an error only when the *start symbol* derives nothing, which is what bison
does. This also guards the lookahead fixed point, which previously spun forever
on a grammar like `x : y ; y : x ;`.

**Known gap:** there is no *reachability* check. A non-terminal that is productive
but unreachable from the start symbol is accepted silently, where bison reports
"nonterminal useless in grammar". Harmless to the generated parser, but a dead
rule can still contribute to the reported conflict counts, since useless rules
are not deleted before the tables are built.

Character literals are range-checked to `1..=255` on both paths that reach the
symbol table — rule bodies, and `%token`/`%left`/`%right`/`%nonassoc` declarations
— since a byte-oriented `yylex` can never return a larger value.

## Generated Parser

Shift-reduce with:
- Hybrid stack: starts on C stack (YYINITDEPTH), malloc+memcpy to heap on first overflow, realloc thereafter
- POSIX consistent-state: skips yylex() in single-reduce states. A state holding
  one reduce plus a `%nonassoc` error entry is *not* consistent — treating it as
  such reduced without reading the lookahead
- Three-phase error recovery: detect→pop to error-shifting state→discard tokens
  until 3 shifts. The error-token shift stores a value, so `stmt : error ';' { $1 }`
  reads initialized memory
- `yytranslate` maps unassigned and out-of-range token numbers to a reserved
  `$undefined` terminal that appears in no rule, so undeclared input is a syntax
  error rather than being shifted as `error`
- `yytname` is emitted in internal symbol order — terminals by dense index, then
  non-terminals — which is the numbering `yytranslate` yields and `yyr1` stores,
  so `yytname[yyr1[n]]` names a rule's LHS

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

## Known Limitations

- No reachability check on non-terminals (see Grammar Validation).
- `$` substitution in actions runs over the raw text, so `$1` inside a C string
  literal or comment is rewritten too: `printf("costs $1")` becomes
  `printf("costs (yyvsp[0])")`. The lexer tracks string/comment state in
  `read_action`; `transform_action` does not.

## Differential testing

Conflict counts and diagnostics are checked against bison 3.8.2. On
`dev/tests/fixtures/python39.y` both report 2 shift/reduce and 4 reduce/reduce,
in default and `--strict` modes. `--strict` zeroes the consistent-state table, so
every grammar is exercised both with and without that optimization.
