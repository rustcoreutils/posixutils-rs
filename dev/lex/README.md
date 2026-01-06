# lex - POSIX Lexical Analyzer Generator

## Pipeline

```
.l file → lexfile.rs → regex_syntax → nfa.rs → dfa.rs → codegen.rs → lex.yy.c
          (parse)      (HIR)          (NFA)    (DFA)    (C code)
```

## Algorithms

### NFA Construction (nfa.rs)
**Thompson's Construction** - O(n) states for pattern of length n.

- Each regex operator maps to NFA fragment with single start/end state
- Concatenation: chain fragments via ε-transitions
- Alternation: new start with ε to each branch, all ends ε to new end
- Kleene star: ε-loops with skip path
- Trailing context: main pattern end state tracked in `main_pattern_end` map

### DFA Construction (dfa.rs)
**Subset Construction** (powerset) - converts NFA to DFA.

1. Initial state = ε-closure({NFA start})
2. For each DFA state and input symbol:
   - Compute move(state, symbol) → target NFA states
   - Compute ε-closure(target) → new DFA state
3. Accept state = contains any NFA accept state; priority = min(rule indices)

**Character Equivalence Classes** - reduces alphabet size.
- Characters with identical transition signatures → same class
- Typically reduces 256 chars to 10-40 classes

### DFA Minimization (dfa.rs)
**Hopcroft's Algorithm** - partition refinement.

1. Initial partition: group by (accepting_rules, main_pattern_end_rules)
2. Refine: split partitions where states differ on target partition for any symbol
3. Loop until fixed point
4. Merge states within same partition

Preserves semantic distinctions for REJECT and trailing context.

### Code Generation (codegen.rs)
**Direct-coded with span compression**

- Each DFA state → C label (`yy_state_N:`)
- Transitions via span-compressed if-chains:
  - Consecutive equivalence classes with same target → single range check
  - `if (yych <= K) goto target;` instead of individual cases
- Buffer management with dynamic growth
- YYCURSOR/YYLIMIT/YYMARKER pointer model

## Modules

| Module | Purpose |
|--------|---------|
| `main.rs` | CLI, orchestration, fixed-length pattern analysis |
| `lexfile.rs` | Parse .l files (definitions, rules, user code) |
| `pattern_escape.rs` | POSIX escape sequences, bracket constructs |
| `pattern_validate.rs` | Anchors (^/$), trailing context (/) parsing |
| `nfa.rs` | Thompson's construction, ε-closure, move |
| `dfa.rs` | Subset construction, Hopcroft minimization, char classes |
| `codegen.rs` | C code emission, span compression, runtime support |
| `diag.rs` | Error/warning diagnostics with source locations |

## Key Data Structures

```
NFA:
  states: Vec<NfaState>        # state → [(Transition, target)]
  main_pattern_end: Map<state, Vec<rule>>  # trailing context tracking

DFA:
  states: Vec<DfaState>        # state → {transitions, accepting, accepting_rules}
  char_classes: [u8; 256]      # char → equivalence class

Span:                          # codegen optimization
  lower, upper: usize          # equivalence class range
  target: usize                # target state
```

## POSIX Compliance

- Longest match semantics
- First-listed rule priority for equal-length matches
- BOL anchor (^), EOL anchor ($), trailing context (r/s)
- Start conditions (%s inclusive, %x exclusive)
- REJECT, yymore(), yyless(), unput(), input()
- Substitution definitions with {name} expansion

## Extensions

- `%option noinput/nounput` - suppress unused function warnings
- Interval expressions: `{n}`, `{n,}`, `{n,m}`
- POSIX character classes: `[:alpha:]`, `[:digit:]`, etc.

## Inspiration

- Rust regex crates (regex, regex-syntax)
- Traditional lex
- re2c

