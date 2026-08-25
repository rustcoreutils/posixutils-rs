# lex - POSIX Lexical Analyzer Generator

## Pipeline

```
.l file → lexfile.rs → regex_syntax → nfa.rs → dfa.rs → codegen.rs → lex.yy.c
          (parse)      (HIR)          (NFA)    (DFA)    (C code)
```

## Algorithms

### Character Equivalence Classes (dfa.rs)

Computed from the **NFA, before determinization** — this ordering is what keeps
the rest of the pipeline cheap, and is the same one flex uses.

- Start with all 256 bytes in one class; split by each transition's character
  set. Two bytes share a class exactly when no transition distinguishes them.
- Everything downstream — subset construction, minimization, DFA transitions —
  is keyed by `ClassId`, not by character. Measured: 4 classes for a two-rule
  spec, 71 for the 136-rule `python39.l` fixture.
- A subsidiary automaton can be built over a given class set
  (`Dfa::from_nfa_with_classes`) so it shares the scanner's `yy_ec` table.

### NFA Construction (nfa.rs)

**Thompson's Construction** — O(n) states for pattern of length n.

- Each regex operator maps to an NFA fragment with a single start/end state
- Concatenation: chain fragments via ε-transitions
- Alternation: new start with ε to each branch, all ends ε to new end
- Kleene star: ε-loops with skip path
- Trailing context: main pattern end state tracked in `main_pattern_end` map

**Two roots per start condition.** `Nfa::starts[cond]` holds a `plain` and a
`bol` root. A rule is wired only to the roots of the conditions it is active in,
and a `^`-anchored rule only to `bol`. Start conditions and `^` are therefore
properties of the automaton's *shape*: a rule that cannot apply here is
unreachable, not filtered after the fact.

### DFA Construction (dfa.rs)

**Subset Construction** (powerset) — converts NFA to DFA.

1. Seed one DFA state per NFA root — `starts[cond].plain` and `.bol`. Roots with
   the same ε-closure collapse, so an unanchored spec pays nothing.
2. For each DFA state and equivalence class: compute move on a representative
   byte of the class, then ε-closure → target DFA state
3. Accept state = contains any NFA accept state; priority = min(rule indices)

### DFA Minimization (dfa.rs)

**Moore's partition refinement** — refine until fixed point. (Not Hopcroft's:
there is no worklist and no smaller-half rule, so this is O(n²·|Σ|), with |Σ|
the class count rather than 256.)

1. Initial partition: group by (accepting_rules, main_pattern_end_rules)
2. Refine: split partitions where states differ on target partition for any class
3. Loop until fixed point
4. Merge states within same partition

Preserves semantic distinctions for REJECT and trailing context. Equivalence
classes are unaffected by merging, so they carry over unchanged.

### Code Generation (codegen.rs)

**Direct-coded with span compression**

- Each DFA state → C label (`yy_state_N:`), plus a `yy_resume_N:` label after
  the accept record so a buffer refill resumes without recording it twice
- Transitions via span-compressed if-chains:
  - Consecutive equivalence classes with same target → single range check
  - `if (yych <= K) goto target;` instead of individual cases
- Entry dispatch selects the automaton for (start condition, at-BOL)
- `YYCURSOR`/`YYLIMIT`/`YYTOKEN`/`YYMARKER` pointer model

**Buffer management.** Three routines are the only code that moves the input
buffer, and each leaves every saved position on the byte it named:
`yy_buffer_grow()`, `yy_buffer_compact()`, `yy_buffer_fill()`. Compaction
preserves distances from `YYTOKEN`, which is why the stacks below can store
offsets from it and need no fixups.

**REJECT.** A growable history stack records every accepting position of the
current token as a (offset-from-`YYTOKEN`, DFA state) pair. On `REJECT` the
scanner first resumes the current position's accept list past the rule just
taken, then walks back to shorter matches, discarding the exhausted top entry.

**Trailing context.** A fixed-length main pattern uses a compile-time length.
A variable-length one records *every* position the main pattern could have ended
and, at match time, takes the longest whose remainder the trailing context
accepts exactly — tested by a standalone per-rule DFA emitted alongside the
scanner and indexed by the shared `yy_ec`. A main pattern that can match the
empty string admits a zero-length split; lex warns at generation time and the
scanner steps over the position rather than rescanning it forever.

**yytext.** `%pointer` grows a heap buffer; `%array` is a fixed `char[YYLMAX]`
and a token exceeding it is a fatal "token too large, exceeds YYLMAX".

## Modules

| Module | Purpose |
|--------|---------|
| `main.rs` | CLI, orchestration, fixed-length pattern analysis, trailing-context DFAs |
| `lexfile.rs` | Parse .l files (definitions, rules, user code), condition membership |
| `pattern_escape.rs` | POSIX escape sequences, bracket constructs |
| `pattern_validate.rs` | Anchors (^/$), trailing context (/), shared bracket/quote scanner |
| `nfa.rs` | Thompson's construction, ε-closure, move, per-condition roots |
| `dfa.rs` | Equivalence classes, subset construction, Moore minimization |
| `codegen.rs` | C code emission, span compression, runtime support |

Diagnostics come from `plib::diag` (shared with `yacc`), not a local module.

## Key Data Structures

```
NfaRule:                       # one rule as presented to NFA construction
  main, trailing: Hir
  index: usize
  bol_anchor: bool
  active_conditions: Vec<usize>

NFA:
  states: Vec<NfaState>        # state → [(Transition, target)]
  starts: Vec<StartRoots>      # per condition: { plain, bol }
  main_pattern_end: Map<state, Vec<rule>>  # trailing context tracking

DFA:
  states: Vec<DfaState>        # state → {transitions, accepting, accepting_rules,
                               #          main_pattern_end_rules}
  transitions: Map<ClassId, usize>         # keyed by class, not character
  starts: Vec<StartStates>     # per condition: { plain, bol }
  char_classes: CharClasses    # { char_to_class: [u8; 256], num_classes, reps }

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
- `%array` / `%pointer` yytext representation
- Substitution definitions with {name} expansion
- Bracket expressions, including a leading `]` as an ordinary member

Matching semantics are checked against flex 2.6.4 differentially — anchoring,
start conditions, REJECT, and both kinds of trailing context.

## Extensions

- `%option noinput/nounput` - suppress unused function warnings
- Interval expressions: `{n}`, `{n,}`, `{n,m}`
- POSIX character classes: `[:alpha:]`, `[:digit:]`, etc.
- `-o <file>` output selection (hidden from `--help`; POSIX has only `-t`)

## Known Limitations

- The alphabet is bytes. A pattern containing a character above U+00FF compiles
  but cannot match, since the scanner reads bytes and classes cover 0..=255.

## Inspiration

- Rust regex crates (regex, regex-syntax)
- Traditional lex; flex, for matching semantics
- re2c, for the direct-coded scanner shape
