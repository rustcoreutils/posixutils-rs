# POSIX.1-2024 Conformance Audit — `awk`

**Implementation:** `awk/` crate (~13.7 kloc): `main.rs` (99), `program.rs` (335),
`compiler.rs` (4482), `grammar.pest` (406), `regex.rs` (171, wraps `plib::regex`
with `RegexFlags::ere()`), `interpreter/{mod.rs 1046, format.rs 2696,
builtins.rs 412, io.rs 618, record.rs 235, value.rs 251, stack.rs 387,
string.rs 142, array.rs 338}`.
**Tests:** `awk/interpreter/tests.rs` (1711) + `awk/tests/integration.rs` (832).
**Spec:** POSIX.1-2024 (IEEE Std 1003.1-2024), Vol. 3 §3, pp. 2653–2691.
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/awk.md` (lines 85158–86827).
**Date:** 2026-06-05
**Method:** spec read in full; implementation read paired with spec; every
Critical/Major finding confirmed by building `target/release/awk` and running it
(behavioral evidence inline). No code was modified.

## TL;DR

The language core is broad and largely correct: expressions, numeric-string
comparison, special variables, FS/RS modes (including paragraph mode and
regex/multi-char extensions), field/`$0` rebuild with OFS, NF truncate/extend,
print/printf redirection and pipes, getline forms, OFMT/CONVFMT separation, the
arithmetic and most string builtins, gsub/sub `&` handling, srand-returns-prior-seed,
`exit`, `next`/`nextfile`, user functions/recursion, arrays, ENVIRON/ARGV/ARGC,
and locale/gettext setup are all present and behave per spec. The conformance
gaps are concentrated and concrete: **`close()` crashes the interpreter when used
as an expression** (Critical); **`-F sepstring` skips the escape processing that
`-v FS=` performs** so `-F '\t'` does not mean tab (Major); **`length`/`index`/
`match`/RSTART/RLENGTH count bytes, not characters** (Major); **printf `*`
dynamic width/precision is unimplemented** (Major); **`-f -` does not read the
program from stdin** (Major); **uninitialized/empty/nonexistent fields do not
compare numerically equal to 0** (Major); and **a hard 1024-field cap silently
drops fields** (Major).

## Priority issues

### Critical

- [x] **#1 — `close()` panics ("empty stack") when used as an expression and never returns a status.** `awk/interpreter/mod.rs:554-562` pops the filename but pushes **no** return value; `BuiltinFunction::Close` is otherwise treated as a value-producing call, so any expression use underflows the operand stack. Verified: `awk 'BEGIN{print "x">"/tmp/f"; print close("/tmp/f")}'` → `panicked at awk/interpreter/stack.rs:205:36: empty stack`; `r=close("/tmp/f")` → panic at `stack.rs:237`. Bare-statement use (`close(f)` alone) works. POSIX 85911-85915: close "If the close was successful, the function shall return …" a status (0/non-zero). Fix: push the close result (0 on success, non-zero on error) like `fflush` does at `mod.rs:580`. **✓ Fixed:** the four `close_*` I/O helpers now return `Option<i32>` and the `Close` opcode pushes 0 (success) / non-zero (e.g. not open); regression test `awk/tests/awk/close_returns_status.{awk,out}` + `test_awk_close_returns_status`.

### Major

- [x] **#2 — `-F sepstring` does not apply escape-sequence processing.** `awk/interpreter/mod.rs:843-847` assigns the `-F` argument to FS verbatim (`AwkString::from(separator)`), bypassing the `escape_string_contents()` used for `-v` assignments (`mod.rs:799`) and assignment operands (`mod.rs:903`). POSIX 85182-85187: `-F sepstring` "shall be equivalent to `-v FS=sepstring`". Verified: `awk -F '\t' 'BEGIN{print length(FS)}'` → `2` (literal `\t`), but `awk -v 'FS=\t' 'BEGIN{print length(FS)}'` → `1` (a real tab); `printf 'a\tb\n' | awk -F '\t' '{print $2}'` prints nothing. The ubiquitous `-F'\t'` idiom is broken. Fix: run `separator` through `escape_string_contents` before assigning FS. **✓ Fixed:** `interpret()` now wraps `-F` in `escape_string_contents`; test `test_awk_dash_f_escape_processing`.

- [x] **#3 — `length`, `index`, `match` (and RSTART/RLENGTH) count bytes, not characters.** `length` uses `value_str.len()` (`builtins.rs:288`), `index` uses `str::find` byte offset (`builtins.rs:273-278`), and `match` derives RSTART/RLENGTH from `plib::regex` byte offsets (`builtins.rs:140-152`). `substr` and `split("")` correctly use `chars()` (`builtins.rs:336`, `record.rs:64`), so the implementation is internally inconsistent. POSIX 85859-85868 and APPLICATION USAGE 86394-86396: "the awk versions deal with characters, while the ISO C standard deals with bytes." Verified on a 3-character / 4-byte record `AÉB`: `length`→`4` (want 3), `index($0,"B")`→`4` (want 3), `match($0,/B/)` RSTART→`4` (want 3). Fix: count characters (`chars().count()`, char indices) in these functions and translate regex byte offsets to char offsets. **✓ Fixed:** `length` uses `chars().count()`; `index`/`match` convert byte offsets via `byte_offset_to_char_count`; test `test_awk_multibyte_char_counts`.

- [x] **#4 — printf/sprintf `*` dynamic field width/precision is unimplemented.** `parse_conversion_specifier_args` (`awk/interpreter/format.rs`) only consumes from the format-string iterator; `sprintf` (`builtins.rs:38`) has no path to fetch a width/precision argument from the value list. POSIX EXTENDED DESCRIPTION item 4 (85798-85800): "A field width or precision can be specified as the `*` character … the next argument … shall be fetched and its numeric value taken as the field width or precision." Verified: `awk 'BEGIN{printf "%*d\n",5,42}'` → `runtime error: unsupported format specifier '*'` (same for `%.*f`, `%-*d`). Fix: when `*` is seen in the width/precision position, pull the next expression argument as an integer. **✓ Fixed:** `FormatArgs` records `width_star`/`precision_star`; `sprintf` fetches width then precision then value (negative width → left-justify, negative precision → omitted); test `test_awk_printf_star_width`.

- [x] **#5 — `-f -` does not read the program from standard input.** `awk/main.rs:64-77` opens every `-f` argument with `std::fs::File::open`, which fails on `-`. POSIX 85188-85192: "A pathname of `-` shall denote the standard input." Verified: `echo 'BEGIN{print 1}' | awk -f -` → `Error: "could not open file '-'"`. Fix: special-case `-` to read `stdin` (and concatenate in order with other `-f` files). **✓ Fixed:** the `-f` loop reads `stdin` when the arg is `-`; test `test_awk_program_file_from_stdin`.

- [x] **#6 — Uninitialized / empty / nonexistent fields do not compare numerically equal to 0.** Two causes: (a) the compare macro at `awk/interpreter/stack.rs:362-371` explicitly excludes field refs (`!lhs.ref_type.is_field() && !rhs.ref_type.is_field()`) from the "Number vs UninitializedScalar → numeric" rule, dropping uninitialized fields to the string-compare fallback (`stack.rs:378-380`); (b) empty fields are stored as empty *strings* (`record.rs:119,198`, via `maybe_numeric_string("")`) rather than the uninitialized value. POSIX 85481 (numeric comparison "if one is numeric and the other has the uninitialized value"), 85506 (nonexistent fields "shall evaluate to the uninitialized value"), 85511 (empty fields from `$0`/FS "shall have the uninitialized value … considered a numeric string"). Verified: `echo 'a b' | awk '{print ($5==0)}'` → `0` (want 1); `echo 'a::b' | awk -F: '{print ($2==0)}'` → `0` (want 1); scalar/array uninitialized compares (`x==0`, `a["k"]==0`) correctly give 1, confirming the divergence is field-specific. Fix: treat field-ref uninitialized values like scalar uninitialized values in comparison, and create empty fields as the uninitialized value. **✓ Fixed:** removed the `is_field()` guard in `compare_op!` (and the now-unused `AwkRefType::is_field`); `record.rs` `make_field` stores empty fields as the uninitialized value; test `test_awk_uninitialized_field_comparison`.

- [x] **#7 — Hard 1024-field cap silently truncates records and errors on high field assignment.** `awk/interpreter/record.rs:103` (`MAX_FIELDS = 1024`); split silently drops fields past 1024 (`record.rs:115-117,194-196`) and `is_valid_record_index` (`record.rs:229-235`) rejects any `$n` with `n>1024`. POSIX places no such low fixed limit and the spec model is dynamic. Verified: a 1100-field record → `NF` reports `1024` (76 fields lost with no diagnostic); `echo x | awk '{$2000="z"}'` → `runtime error: invalid field index`. Fix: grow the fields vector dynamically (or raise the cap well beyond {LINE_MAX}-implied field counts and never silently drop). **✓ Fixed:** field storage is now `RefCell<Vec<Box<…>>>` grown on demand to a `u16::MAX` ceiling (boxed cells keep stack-held field pointers valid across growth; surplus fields are cleared, not dropped, for soundness). Tests `test_awk_record_with_many_fields`, `test_awk_high_field_assignment`.

### Minor

- [x] **#8 — `substr(s,m,n)` with `m<1` keeps `n` characters from position 1 instead of from `m`.** `builtins.rs:332` clamps `m` to `1.0` then `builtins.rs:336` takes `n` chars, so leading positions below 1 are not counted against `n`. POSIX 85895-85898 defines the result as "the at most n-character substring … that begins at position m". Verified: `substr("hello",-1,3)`→`hel` and `substr("hello",0,2)`→`he`; nawk/gawk give `h` and `h`. The code comment acknowledges the `<1` case is "not specified", so this is a divergence-from-common-behavior rather than a hard `shall`; remaining substr edges (`m`/`n` fractional truncation, negative `n`→empty, over-long `n` clamp) are correct. **✓ Fixed:** substr now computes the character window `[max(m,1), m+n)` clamped to the string, so out-of-range leading positions consume `n`; test `test_awk_substr_edges`.

- [ ] **#9 — Division / modulo by zero yields `inf`/`-nan` with no diagnostic and exit 0.** `awk 'BEGIN{print 1/0}'` → `inf` (exit 0); `1%0` → `-nan`. POSIX 85426-85428 makes the result undefined (ISO C error case), so this is conforming-but-surprising; most awks emit a fatal "division by zero" diagnostic. Consider emitting a diagnostic and non-zero exit. **Left as-is (intentional):** current behavior is POSIX-conforming (undefined → `inf`/`-nan`); not changed to avoid breaking programs that rely on the IEEE result.

- [x] **#10 — `tolower`/`toupper` use Unicode default case mapping, not the `LC_CTYPE` mapping.** `builtins.rs:339-349` call Rust `to_lowercase()`/`to_uppercase()`. POSIX 85899-85906 ties the mapping to "the LC_CTYPE category of the current locale". Practical impact is small (ASCII identical); flagged for strict-conformance completeness, consistent with the `plib::locale` direction taken in `dev/audit.md`. **✓ Fixed:** added `plib::locale::to_lower`/`to_upper` (libc `tolower`/`towlower`); awk maps each character through them. Tests `plib locale::tests::to_lower_upper_ascii`, `test_awk_case_mapping`.

- [x] **#11 — `cmd | getline` does not set NR.** `awk/interpreter/mod.rs:598-622` sets neither NR nor FNR for the file and pipe getline forms. The POSIX 2024 text for `expression | getline [var]` (85922-85933) is silent on NR, so this conforms to the letter, but historical awk and gawk increment NR for the pipe form. Verified: `awk 'BEGIN{"echo hi"|getline x; print NR}'` → `0`. Document or align with historical behavior. **✓ Fixed:** the `GetLineFromPipe` branch now advances NR (not FNR) on a successful read, matching gawk; `getline < file` still touches neither. Test `test_awk_getline_pipe_advances_nr`.

- [x] **#12 — `split(s,a,"")` performs a per-character split.** `record.rs:76-77` maps an empty FS to `Null` → one element per character. POSIX 85876 calls a null `fs` "unspecified", so this is conforming; worth a one-line doc note since it is an observable choice (`split("abc",a,"")` → 3 elements). Likewise FS="" for record field splitting is the same unspecified-but-defined char split. **✓ Documented:** comment added at the `FieldSeparator::Null` construction noting the gawk-compatible per-character behavior.

- [ ] **#13 — clap exposes `--help`/`-h`/`--version`/`-V`.** `awk/main.rs:24-45`. These are non-POSIX but standard and harmless. `--` end-of-options and unknown-option rejection are handled by clap (both verified working). No action required beyond noting the extension surface.

## Detailed conformance matrix

### SYNOPSIS / argv parsing
- [x] Both synopsis forms supported — program-as-first-operand and `-f progfile` — `main.rs:64-94`.
- [x] `-F`, `-f` (repeatable, `ArgAction::Append`), `-v` (repeatable) parsed — `main.rs:27-42`.
- [x] `-f` concatenation in order forms one program — `main.rs:65-77`.
- [x] `--` end-of-options handled (clap) — verified `awk -- 'BEGIN{print 1}'` → `1`.
- [x] **`-f -` routed to stdin** (#5 ✓ fixed) — `main.rs:64-77`.

### OPTIONS
| Opt | Status | Notes (file:line) |
|---|---|---|
| `-F sepstring` | CONFORMS | (#2 ✓ fixed) escapes processed via `escape_string_contents`; `-F '\t'` is a tab. |
| `-f progfile` | CONFORMS | (#5 ✓ fixed) Multiple/concatenation OK; `-` reads stdin. |
| `-v assignment` | CONFORMS | `main.rs:37-42`; applied before BEGIN (`mod.rs:836-841`), escapes processed (`mod.rs:799`), numeric-string tagged. Verified `-v x=5` and `-v 'x=a\tb'`. |

### OPERANDS / STDIN / INPUT FILES
- [x] `program` is the first operand when no `-f` — `main.rs:84-93`.
- [x] `var=value` assignment operand detection (`name` = `[A-Za-z_][A-Za-z0-9_]*` then `=`) — `mod.rs` `parse_assignment`. Verified `awk '{print v,$0}' v=hi file`.
- [x] Assignment operand escapes processed and numeric-string tagged — `mod.rs:903`. Verified `v=10` then `(v>5)` → `1`.
- [x] Assignment processed "just prior to" the following file; before END if after last file — argv loop `mod.rs:868+`.
- [x] `-` file operand → stdin at that position — argv loop.
- [x] STDIN used only when no file operands or `-`; empty program (no rules/END) exits 0 without reading — `mod.rs:864-866`. Verified `echo data | awk ''` → exit 0.
- [x] `{LINE_MAX}`-and-beyond records supported (records read fully, no line cap) — `io.rs` record readers.

### ENVIRONMENT VARIABLES
| Var | Status | Notes |
|---|---|---|
| `LANG`/`LC_ALL`/`LC_COLLATE`/`LC_CTYPE`/`LC_MESSAGES` | CONFORMS (setup) | `setlocale(LC_ALL,"")` at `main.rs:58`; regex/collation via libc-backed `plib::regex`. `tolower`/`toupper` are the one LC_CTYPE gap (#10). |
| `LC_NUMERIC` | CONFORMS | Program literals always use `.` (`lexical` C format); numeric output radix via `format.rs` locale `decimal_point`, matching 85271-85276. |
| `NLSPATH` (XSI) | CONFORMS (via gettext) | `textdomain`/`bind_textdomain_codeset` — `main.rs:59-60`. |
| `PATH` | CONFORMS | `system()` uses libc `system(3)` (`builtins.rs:354-369`), inheriting PATH. |
| `ENVIRON` array | CONFORMS | Populated from `std::env::vars()`, numeric-string tagged — `mod.rs:823-825`. Verified `ENVIRON["FOO"]`. |

### ASYNCHRONOUS EVENTS
- [x] Default — spec says "Default" (85282); no special handling required.

### STDOUT / STDERR
- [x] print/printf write stdout by default — `builtins.rs:371-376`.
- [x] Diagnostics to stderr, gettext-wrapped — `main.rs:51,68-71,95`; pipe-close warnings `io.rs`.

### OUTPUT FILES
- [x] Nature depends on program (redirections/pipes) — `mod.rs:520-552`, `io.rs`.

### EXTENDED DESCRIPTION

#### Expressions / operators
- [x] Full precedence ladder: grouping, `$`, pre/post `++`/`--`, `^` (right assoc, `pow`), unary `!`/`+`/`-`, `* / %` (`fmod`), `+ -`, concatenation, relational, `~`/`!~`, `in`, `&&`, `||`, `?:`, all assignment ops. Verified `2^10`→1024, `7.5%2`→1.5, `1?"y":"n"`→y, `"a" "b" 1+2`→`ab3`.
- [x] String→number conversion per 85356-85374 (`strtod`-style, leading-numeric) — `mod.rs:59-66`.
- [x] Number→string: integers via `%d`, others via CONVFMT (85375-85381) — `value.rs:88-106`. Verified integer bypass and CONVFMT concat.
- [x] Numeric-string sources (fields, getline, FILENAME, ARGV, ENVIRON, split, cmdline assign) tagged — `mod.rs:78-82,814-825`, `record.rs:43`.
- [x] Comparison rule (numeric if both numeric / numeric+numeric-string / both numeric-strings / numeric+uninitialized) — `stack.rs:345-381`. Verified `"10"==10`→y, `" 10 "==10`→n, field `10>9`→1.
- [x] **Field uninitialized comparison** (#6 ✓ fixed) — `stack.rs` `compare_op!`, `record.rs` `make_field`.
- [x] Boolean context (zero/`""` false) — verified via `if`, `?:`, patterns.

#### Arrays
- [x] Associative, auto-vivify on reference, `in` does not create — verified `("z" in a)`→0.
- [x] Multi-dim subscript via SUBSEP; `(i,j) in a`; subscript uses CONVFMT — verified `a[1,2]` + `split(k,p,SUBSEP)`; `CONVFMT` in subscript → `0.12`.
- [x] `delete a[i]` / `delete a` (whole array) — verified `length(a)`→0 after `delete a`.
- [x] `for (k in a)` iterates indices — verified count 2.
- [x] `length(array)` → element count — `builtins.rs:283-285`. Verified →3.

#### Special variables
| Var | Status | Notes |
|---|---|---|
| `ARGC`/`ARGV` | CONFORMS | `ARGV[0]="awk"`, `ARGC`=count; modifiable (verified injecting a file via `ARGV`/`ARGC`). `mod.rs:814-821`. |
| `CONVFMT` | CONFORMS | default `%.6g`; used for number→string (`value.rs`). |
| `ENVIRON` | CONFORMS | see ENV table. |
| `FILENAME` | CONFORMS | set per file; `-` for stdin. |
| `FNR`/`NR` | CONFORMS | reset per file / cumulative; getline plain & `getline var` bump both (`mod.rs:589-592`). |
| `FS` | CONFORMS (value) | default space; see FS modes. (`-F` escape gap is #2.) |
| `NF` | CONFORMS | recomputed on split; assignment truncates/extends (`mod.rs`/`record.rs:135-183`). Verified `NF=2` truncate, `$4=` grow. Capped at 1024 (#7). |
| `OFMT` | CONFORMS | print of non-integers; integers bypass. Verified `OFMT="%.2f"`→`3.14`, `print 100`→`100`. |
| `OFS`/`ORS` | CONFORMS | print separators; OFS on `$0` rebuild. Verified `OFS="-"; $1=$1`. |
| `RLENGTH`/`RSTART` | CONFORMS | set by `match`; character-based (#3 ✓ fixed). No-match → 0/-1. |
| `RS` | CONFORMS+ext | first-char separator; `""` paragraph mode; multi-char/regex accepted (spec leaves multi-char unspecified — conforming extension). Verified all three. |
| `SUBSEP` | CONFORMS | multi-dim subscript join. |

#### Records and fields
- [x] FS modes: space-default (strip leading/trailing blanks+newlines, split on runs), single-char, regex/multi-char — `record.rs:44-86`. Verified all, incl. `FS="\t"`, `FS="[0-9]"`.
- [x] RS modes incl. paragraph (`RS=""`, newline always a field sep) — `io.rs` + `mod.rs` effective-FS. Verified.
- [x] Assigning `$n` rebuilds `$0` with OFS; assigning `$0` re-splits; `$(NF+k)` grows NF with intervening uninitialized fields — `record.rs:135-205`. Verified.
- [x] **Empty fields carry the uninitialized value** (#6 ✓ fixed) — `record.rs` `make_field`.
- [x] **Dynamic field count** (#7 ✓ fixed) — `record.rs` grow-on-demand boxed cells.

#### Regular expressions (ERE)
- [x] ERE via `plib::regex` `RegexFlags::ere()` — libc-backed POSIX ERE (`regex.rs:84`). Verified anchors `^…$`, alternation, intervals `{2}`, bracket class `[[:alpha:]]`, `~`/`!~`, dynamic regex from a string variable.
- [x] C-style escapes in STRING/ERE (`\\ \a \b \f \n \r \t \v`, `\ddd` octal) — `compiler.rs`. Verified octal `\101`→`A`.
- [x] `\x` hex escapes correctly NOT supported (spec RATIONALE 86527; only octal) — rejected at lex.
- [x] Bare ERE in expression context ≡ `$0 ~ /ere/` — grammar/compiler.
- [x] Record-separator-not-matchable-in-`$0` semantics — record reader strips terminator before fielding.

#### Patterns and actions
- [x] `pattern { action }`, missing pattern = always, missing action = `{ print }` — `program.rs`/compiler.
- [x] BEGIN/END (multiple, ordered; END-before-BEGIN allowed; BEGIN-only exits without reading; END forces input read) — `mod.rs:849-866` + end loop. Verified empty-program and exit paths.
- [x] Expression patterns (Boolean) and range patterns `e1,e2` — compiler + `range_pattern_started` (`mod.rs:833`).
- [x] `if/else`, `while`, `do…while`, `for`, `for (k in a)`, `break`, `continue` — verified do-while, recursion.
- [x] `next` (verified skips record), `nextfile` (verified abandons file), `delete`, `exit [expr]`.
- [x] `exit` runs END once then terminates; exit code from expr — verified `exit 2` + END → rc 2; `exit 3` → rc 3.

#### Output statements
- [x] `print` list joined by OFS, terminated by ORS, numbers via OFMT; empty list = `$0` — `builtins.rs:390-411`. Verified bare `print`.
- [x] `printf`/`sprintf`: `%d %i %o %x %X %u %e %E %f %F %g %G %a %A %c %s %%`; flags/width/precision — `builtins.rs:26-118`, `format.rs`. Verified `%i`,`%x`,`%o`,`%e`,`%8.2f`,`%d` large.
- [x] `%c`: numeric→char-by-value, string→first char (`builtins.rs:85-101`). Verified `65`→`A`, `"hello"`→`h`.
- [x] Redirection `>` (truncate-once-then-append), `>>` (append), `| cmd` (popen `w`) — `mod.rs:520-552`, `io.rs`. Verified truncate-then-append, cross-run `>>`, `print|"sort"`.
- [x] **`*` dynamic width/precision** (#4 ✓ fixed) — `format.rs` `FormatArgs`, `builtins.rs` `sprintf`.
- [x] Insufficient args → error (spec "undefined"; erroring is acceptable) — `builtins.rs:45-46`. Extra args ignored (awk printf does not cycle) — verified.

#### Built-in arithmetic functions
- [x] `atan2 cos sin exp log sqrt int rand srand` — `builtins.rs:237-264`, `mod.rs:623-639`. `int` truncates toward 0; `rand`∈[0,1); **`srand` returns previous seed** (verified `srand(1); print srand(2)`→`1`), reseeds deterministically.

#### Built-in string functions
| Func | Status | Notes |
|---|---|---|
| `gsub`/`sub` | CONFORMS | count returned; `&`/`\&`/`\\` replacement handled (`builtins.rs:154-228`). Verified `gsub(/b/,"[&]")`→`a[b]c`, `\\&`→`a[&]c`, count `gsub(/a/,…)`→3. |
| `index` | CONFORMS | (#3 ✓ fixed) character position via `byte_offset_to_char_count`. |
| `length` | CONFORMS | (#3 ✓ fixed) `chars().count()`. Bare `length` = `length($0)`; `length(array)` works. |
| `match` | CONFORMS | (#3 ✓ fixed) RSTART/RLENGTH character-based. No-match 0/-1 correct. |
| `split` | CONFORMS | clears array; default FS / regex / single-char; numeric-string tagging (verified `10>9`→1); empty-fs char split (#12). |
| `sprintf` | CONFORMS | (#4 ✓ fixed) `*` width/precision supported. |
| `substr` | CONFORMS | (#8 ✓ fixed) `m<1` window math; char-based, fractional truncation, negative-`n`→empty, over-long-`n` clamp. |
| `tolower`/`toupper` | CONFORMS | (#10 ✓ fixed) `plib::locale` LC_CTYPE mapping. |

#### Input/Output & general functions
| Func | Status | Notes |
|---|---|---|
| `close` | CONFORMS | (#1 ✓ fixed) returns 0/non-zero; `mod.rs:554-579`, `io.rs` close helpers. |
| `fflush` | CONFORMS | returns 0/-1; no-arg flushes all (`mod.rs:563-581`). |
| `system` | CONFORMS | libc `system(3)`; returns exit status / 128+sig (`builtins.rs:354-369`). |
| `getline` | CONFORMS | sets `$0`,NF,NR,FNR (verified NF=3/NR=2/FNR=2). |
| `getline var` | CONFORMS | sets var,NR,FNR; not `$0`/NF (verified). |
| `getline < file` | CONFORMS | sets `$0`/NF (or var); not NR/FNR (verified NR=0). |
| `cmd \| getline [var]` | CONFORMS | (#11 ✓ fixed) advances NR (not FNR), like historical awk. |
| getline returns 1/0/-1 | CONFORMS | verified missing-file→-1, EOF→0, success→1 (`mod.rs:608-621`). |

#### Grammar / lexical conventions
- [x] Keyword and BUILTIN_FUNC_NAME token sets, NAME vs FUNC_NAME (`(` adjacency), two-char tokens, `/`-vs-ERE ambiguity — `grammar.pest` + `compiler.rs`.
- [x] String/ERE constant escape handling, comments, newline-continuation, optional newlines after `,`/`{`/`&&`/`||`/`do`/`else`/`)` — compiler.
- [x] Hex numeric constants optional (85333 "may"); not provided — conforming N/A. Verified `0x1A`→`0`.
- [x] User functions: forward reference, by-value scalars / by-reference arrays, excess params as locals, recursion — verified `f(5)`→120.

### EXIT STATUS / CONSEQUENCES OF ERRORS
- [x] 0 on success, >0 on error; `exit [expr]` overrides — `main.rs:47-54,98`, `mod.rs` exit propagation. Verified.
- [x] Compile/runtime errors → diagnostic to stderr + exit 1 — `main.rs:47-55`.
- [ ] **Division/modulo by zero: no diagnostic, exit 0** (#9) — arithmetic ops.
- [x] Inaccessible file operand → diagnostic + terminate — file readers in `io.rs`/`mod.rs`.

### Cross-cutting
- [x] i18n: `setlocale(LC_ALL,"")` + gettext domain wired (`main.rs:58-60`); diagnostics gettext-wrapped; `tolower`/`toupper` honor LC_CTYPE (#10 ✓ fixed).
- [x] Regex flavor: ERE (correct for awk) via libc-backed `plib::regex`.
- [x] Signals: Default (non-interactive) — no handlers required.
- [x] Character vs byte: `length`/`index`/`match` now character-based, consistent with `substr`/`split` (#3 ✓ fixed).

## Test coverage signal

Existing tests (`interpreter/tests.rs` 1711, `tests/integration.rs` 832) cover the
golden language paths well (operators, builtins, getline, printf, arrays, regex,
srand-prior-seed). Gaps that map to findings — add tests that:
- [x] assert `print close(f)` / `r=close(f)` return a status without panicking (#1). ✓ `test_awk_close_returns_status`
- [x] assert `-F '\t'` yields a single-char tab FS (`length(FS)==1`) (#2). ✓ `test_awk_dash_f_escape_processing`
- [x] assert `length`/`index`/`match`/RSTART/RLENGTH on a multibyte record return character counts (#3). ✓ `test_awk_multibyte_char_counts`
- [x] exercise `printf "%*d"`, `"%.*f"`, `"%-*d"` (#4). ✓ `test_awk_printf_star_width`
- [x] exercise `awk -f -` reading the program from stdin (#5). ✓ `test_awk_program_file_from_stdin`
- [x] assert `$5==0` and empty `$2==0` are true (#6). ✓ `test_awk_uninitialized_field_comparison`
- [x] assert records with >1024 fields keep all fields and `$2000=…` works (#7). ✓ `test_awk_record_with_many_fields`, `test_awk_high_field_assignment`
- [x] pin `substr("hello",-1,3)` behavior (#8). ✓ `test_awk_substr_edges`

## Suggested PR groupings

- **PR A — "close() returns a status (no crash)"**: #1. Push close result; add expression-context tests. Smallest fix that removes a Critical crash.
- **PR B — "argv/option conformance"**: #2 (`-F` escape processing), #5 (`-f -` stdin). Both in `main.rs`/`interpret()` option wiring.
- **PR C — "characters, not bytes"**: #3. Convert `length`/`index`/`match`/RSTART/RLENGTH to character semantics; align with `substr`/`split`.
- **PR D — "printf `*` dynamic width/precision"**: #4. `format.rs` + `sprintf` arg fetch.
- **PR E — "field uninitialized value semantics"**: #6. `stack.rs` field-ref comparison + `record.rs` empty-field creation.
- **PR F — "dynamic field count"**: #7. Remove/raise `MAX_FIELDS`; never silently drop.
- **PR G — "edge-case polish"**: #8 (substr `m<1`), #9 (divide-by-zero diagnostic), #10 (LC_CTYPE case mapping), #11 (`cmd|getline` NR), #12 (document null-FS split).
