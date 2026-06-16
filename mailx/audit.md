# POSIX.1-2024 Conformance Audit — `mailx`

**Implementation:** `mailx/` — `main.rs` (368), `args.rs` (147), `commands.rs` (1562), `escapes.rs` (775), `mailbox.rs` (397), `message.rs` (248), `msglist.rs` (271), `send.rs` (444), `variables.rs` (163) — ~4.4 kloc
**Tests:** `mailx/tests/` — `cli/mod.rs` (866, 27 `#[test]`), `receive/mod.rs` (2401, 66), `variables/mod.rs` (1269, 35), `send/mod.rs` (518, 13) — 141 tests
**Spec:** POSIX.1-2024 (IEEE Std 1003.1-2024), Vol. 3 §3, pp. 3150–3177
**Reference slice:** `~/tmp/posix.2024/sliced/xcu-shell-and-utilities/3-utilities/mailx.md`
**Date:** 2026-06-15

## TL;DR

The Receive-Mode command interpreter is broad and largely well-shaped: nearly all of the POSIX command set and tilde-escape set are present, message-list parsing (`n`, `+`, `-`, `.`, `^`, `$`, `*`, `n-m`, `/string`, `:c`, address) is implemented, and the mbox read/write/quit state machine is mostly faithful. But there are three hard defects: the **`-E` option — one of only two required on all systems — is unimplemented and makes `mailx -E …` error out**; there is **no signal handling whatsoever**, so the entire ASYNCHRONOUS EVENTS section is absent and `SIGINT` simply kills mailx; and **header-summary display can panic** on a multibyte address/subject (byte-offset string slicing). Below those, a cluster of Major divergences: every `sh -c` invocation omits the mandated `--` argument (Austin Group Defect 1528); the `-f file` operand is mis-parsed into Send Mode unless it is glued to `-f`; `-n` wrongly suppresses the user `MAILRC`; there is no `setlocale`/`gettext` and diagnostics are hardcoded English; loaded messages are never given the `new` state; and the `~:` command-level escape is a stub.

## Priority issues

### Critical

- [x] **#1 — `-E` option unimplemented; `mailx -E …` exits non-zero with "illegal option".** *(FIXED: `'E'` arm in `args.rs` → `Args.discard_empty`; `send_mode` drops an empty-body message before delivery.)* `args.rs:59-118` has no `'E'` arm, so `-E` falls through to `_ => Err("illegal option -- {}")`. `-E` (discard messages with an empty body) is one of only two options *required on all systems* (the other is `-s`), added by Austin Group Defect 1367 (spec 104270). No empty-body-discard logic exists in `send.rs` either. Fix: parse `-E`, and in `send_message`/`send_mode` drop the message (success, no delivery) when the body is empty.
- [x] **#2 — No asynchronous-event / signal handling at all.** *(FIXED: `signals.rs` installs a `SIGINT` handler via `sigaction` (no `SA_RESTART`); command mode aborts the command and re-prompts, input mode honors `ignore` (`@`) and the two-interrupt abort with dead-letter save. Interactive reads go through `read_line_interruptible`, which surfaces `EINTR`.)* `grep -nE 'SIGINT|SIGCONT|signal_hook|libc::signal' *.rs` → zero matches. The whole ASYNCHRONOUS EVENTS section (spec 104388-104411) is unimplemented: in command mode `SIGINT` should abort the current command and re-prompt; in input mode it should (per `ignore`) print `@` and drop the line, or require a second interrupt to abort and write the partial message to `DEAD`. Rust default-terminates on `SIGINT`, so mailx just dies. The `-i`/`ignore` branch in `send.rs:176-193` keys off `io::ErrorKind::Interrupted` from `read_line`, which never occurs for a delivered signal — so `-i`/`ignore` is inert and the two-interrupt-to-abort flow is dead code. Fix: install a `SIGINT` handler (signal-hook) with command-mode vs input-mode state; honor `ignore`, `save`/`DEAD`.
- [x] **#3 — Header-summary display panics on multibyte text.** *(FIXED: single `truncate_display` helper truncates by `chars()`, not byte offsets; used by `message.rs` and `mailbox.rs`.)* `message.rs:226` (`&s[..max_len - 3]`) and `mailbox.rs:395` (`&s[..max_len.saturating_sub(3)]`) slice a `String` at a byte offset; a `From:`/`Subject:` value whose UTF-8 boundary does not fall on byte 15/22 panics (`byte index N is not a char boundary`) while printing headers — i.e. an incoming message can crash mailx at startup. LC_CTYPE governs multibyte interpretation (spec 104333). Fix: truncate on `char_indices()` (or grapheme/`floor_char_boundary`).

### Major

- [x] **#4 — `sh -c` invocations omit the mandated `--` argument (Austin Group Defect 1528).** *(FIXED: `.arg("--")` inserted at every `sh -c` site in `commands.rs` and `escapes.rs`.)* Spec requires three arguments `"-c"`, `"--"`, *command* for `pipe` (104870), `!` (105021), `~!` (105043), `~|` (105099) and `~r !command` (105087). All sites pass only `-c` + command: `commands.rs:833` (pipe), `commands.rs:1146` (`!`), `escapes.rs:334` (`~!`/`~: !`), `escapes.rs:680` (`~|`), `escapes.rs:371` (`~r !`). A command beginning with `-` is then mis-parsed by the shell. Fix: insert `.arg("--")` before the command at each site.
- [x] **#5 — `-f file` operand mis-parsed into Send Mode.** *(FIXED: `file` is now a trailing operand consumed in Receive Mode; `-f` always selects Receive; `--` end-of-options terminator added.)* `args.rs:64-73` only captures the file when it is the last char of the `-f` cluster *and* the next token does not start with `-`; otherwise the file token hits the `else` at `args.rs:121-124` and is pushed onto `addresses`, and `args.rs:134-135` then selects `Mode::Send`. So `mailx -f -N file` and the RATIONALE's explicitly-required `mailx -fin mymail.box` (spec 105205-105206) send mail to "file"/"mymail.box" instead of reading it. Fix: treat `file` as a normal trailing operand (per XBD 12.2; `file` is an operand, not an option-argument), consumed in Receive Mode when `-f` is set.
- [x] **#6 — `-n` also suppresses the user `MAILRC` start-up file.** *(FIXED: `load_startup_files` gates only the system file on `-n`; the user MAILRC is always processed.)* Spec Start-Up steps 4–5 (104553-104555) attach "unless the −n option is given" only to the *system* start-up file; the user file named by `MAILRC` is *always* processed. `main.rs:53/78/105` gate the entire `load_startup_files` (both system and user) on `!args.no_init`, so `mailx -n` skips `~/.mailrc` too. Fix: split the two; `-n` skips only the system file.
- [ ] **#7 — No locale handling; diagnostics hardcoded English.** `grep -nE 'setlocale|gettext|LC_' *.rs` → zero. Spec lists `LANG`/`LC_ALL`/`LC_CTYPE`/`LC_MESSAGES`/`NLSPATH` (104328-104359) as affecting execution; `LC_MESSAGES` shall govern diagnostic and informative text. `setlocale(LC_ALL,"")` is never called and every `eprintln!`/`println!` string is literal English. Fix: `setlocale` near `main`, route diagnostics through `gettext` per project convention.
- [x] **#8 — `new` message state never produced; `N` shown as `U`, `:n` selector dead.** *(FIXED: loaded messages default to `New`; `Status:` with `R`→`Read`, `O`-only→`Unread`; absence stays `New`. `:n` and the `N` char now work; current = first new/unread per spec 104481-104482.)* `mailbox.rs:69` sets every loaded message to `MessageState::Unread` (`// Assume unread for now`); only a `Status:` header containing `R`/`O` promotes to `Read` (`mailbox.rs:100-102`). `MessageState::New` is therefore unreachable from a real mailbox, so genuinely new mail displays state char `U` instead of `N` (spec 104491-104496), and `:n` (`msglist.rs:192`) matches nothing. Fix: treat absence of `Status:`/`O` as `New`, presence of `O` (without `R`) as `Unread`.
- [ ] **#9 — `~:` / `~_` command-level escape is a stub, and `~:set` does not set.** `escapes.rs:732-775` (`execute_input_mode_command`) handles only `set`, `echo`, `!`; it ignores `_msg`/`_mb`, and its `set` arm merely `println!`s each argument (`escapes.rs:755-759`) instead of mutating `vars`. Spec 105048-105049: "`~: mailx-command` … Perform the command-level request." Fix: dispatch through the real command interpreter (the command-mode subset that is valid here), and actually apply `set`.
- [ ] **#10 — `~w file` truncates instead of appending.** `escapes.rs:293` uses `fs::write` (truncate). Spec 105094-105097: "The file shall be created or the message shall be appended to it if the file already exists." Fix: open with create+append.
- [x] **#11 — reply-all ignores `Reply-To`.** *(FIXED: reply-all seeds the sender portion from `Reply-To` when present, else `From`.)* `compose_reply` for the lowercase (reply-all) form (`send.rs:392-432`) unconditionally seeds recipients from `From` + `To` + `Cc` and never consults `Reply-To`. Spec 104911-104916: in the lowercase form, only *when there is no* `Reply-To` are `From`,`To`,`Cc` used; when `Reply-To` is present the From-derived path does not apply (implementation-defined Reply-To/To/Cc). Fix: branch on `Reply-To` presence.

### Minor

- [ ] **#12 — `set escape=` (null) does not disable escaping.** `variables.rs:99-103` returns `'~'` via `unwrap_or('~')` when the value is empty. Spec 104610-104612: "if it is set to null, command escaping shall be disabled." Fix: distinguish unset (`~`) from set-empty (disabled).
- [x] **#13 — `mbox`/`touch`/`hold` not restricted to the system mailbox; `mbox` cannot override a set `hold` variable.** *(FIXED: the three commands are gated on `is_system_mailbox`; a new `Message.force_mbox` flag set by `mbox`/`touch` forces the message to the secondary mbox at quit, overriding the `hold` variable.)* `commands.rs` `cmd_mbox`/`cmd_touch`/`cmd_hold` perform no "system mailbox only" check (spec 104831, 104853, 104986). `cmd_mbox` sets `Read`; at quit a `Read` message with the `hold` *variable* set is kept in place (`mailbox.rs:291-296`), so `mbox` does not force the message to the secondary mailbox as required (spec 104853-104855). Fix: add a distinct "force-to-mbox" state and gate the three commands on `is_system_mailbox`.
- [ ] **#14 — Start-up files: invalid commands silently ignored; several legal commands not executed.** `execute_startup_command` (`commands.rs:259-273`) whitelists only alias/alternates/discard/retain/set/unset/source/if and maps everything else to `Ok(())`. The spec's *invalid-in-startup* list (104557-104559) should produce a diagnostic (and "any errors … shall … terminate … or … continue after writing a diagnostic"); conversely legal start-up commands such as `cd`, `echo`, `folders` are dropped. Fix: diagnose the invalid set, execute the rest.
- [ ] **#15 — `crt` pagination ignores whether stdout is a terminal.** `commands.rs:891` / `escapes.rs:233` paginate whenever the line count exceeds `crt`. Spec 104362-104367 gates pagination on stdout being a terminal device. Fix: only auto-page when `io::stdout().is_terminal()`.
- [ ] **#16 — `if s|r` is a no-op in command mode.** `commands.rs:137-139` returns `Continue` for `if`/`else`/`endif`, so an interactive/`source`d conditional block always executes its body regardless of mode (spec 104836-104844). Only the start-up loader (`main.rs:264-331`) honors conditionals. Fix: track conditional state in `execute_command`.
- [ ] **#17 — `alias` backslash recursion-prevention unimplemented.** `variables.rs:125-141` (`expand_alias`) always recurses; spec 104720-104721 lets a leading unquoted `\` on a group member prevent expansion.
- [ ] **#18 — `#` (previous file) folder substitution unimplemented.** `commands.rs:1490-1493` returns the literal `#` (spec 104794).
- [ ] **#19 — `ignoreeof` not honored during Receive-Mode composition.** `compose_message` (`commands.rs:1512-1538`) breaks on EOF unconditionally and only checks `dot`; send-mode (`send.rs:167-221`) handles `ignoreeof` but the receive-mode reply/mail composer does not (spec 104630-104632, Austin Group Defect 1034 for `~.`).
- [ ] **#20 — Interactive prompts don't all gate on a terminal.** `prompt_headers` (`escapes.rs:545`, `~h`) prompts unconditionally; spec 105065 says `~h` prompts "If standard input is a terminal".
- [ ] **#21 — `-u user` hardcodes `/var/mail/user`.** `args.rs:141-143` ignores the other spool locations checked by `get_system_mailbox` and performs no privilege check (spec 104287-104289 requires appropriate privileges).
- [ ] **#22 — Some informative output goes to stderr.** e.g. `save_dead_letter` writes "Message saved to …" to stderr (`send.rs:366`); spec STDOUT (104412-104414) routes messages to stdout, reserving stderr for diagnostics. Minor.

## Detailed conformance matrix

### Options (`args.rs`)

| Opt | Status | Notes |
|---|---|---|
| `-E` | CONFORMS | (#1 FIXED) `'E'` arm; empty-body messages discarded in `send_mode`. |
| `-e` | CONFORMS | `args.rs:61-63`, `main.rs:64-71` writes nothing, exit 0 if mail else 1. |
| `-f [file]` | CONFORMS | (#5 FIXED) `file` is a trailing operand read in Receive Mode; `-f` always selects Receive. |
| `-F` | PARTIAL | `args.rs:75-77`; `send.rs:311-315` records to first-recipient login. Overrides `record` (spec 104278) — OK. |
| `-H` | CONFORMS | `args.rs:78-80`, `main.rs:73-98` header summary then exit. |
| `-i` | CONFORMS | (#2 FIXED) Sets `ignore`; the `SIGINT` handler now honors it (`@`, line discarded). |
| `-n` | CONFORMS | (#6 FIXED) Skips only the system start-up file; user `MAILRC` always read. |
| `-N` | CONFORMS | `args.rs:87-89`; suppresses initial summary (`main.rs:137`). |
| `-s subject` | CONFORMS | `args.rs:90-102`; `send.rs:112-113`. All chars preserved. |
| `-u user` | PARTIAL | (#21) Hardcodes `/var/mail/user`; no privilege check. |
| `--` end-of-options | CONFORMS | (#5 FIXED) `--` terminator stops option parsing (XBD 12.2). |
| `+`-prefix exception | N/A | Spec does not invoke the XBD 12.2 `+` exception for mailx. |

### Operands / STDIN / INPUT FILES

- [x] `address...` operands → Send Mode recipients — `args.rs:121-124`, `send.rs:99-109`.
- [x] **`file` operand** read in Receive Mode as a trailing operand (#5 FIXED).
- [x] Send Mode: stdin is the message body — `send.rs:157-233`.
- [x] Receive Mode: commands read from stdin — `main.rs:144-188`.
- [x] Non-interactive send reads stdin literally (no tilde processing) — `send.rs:226-233` (spec leaves `~` "unspecified" off-terminal).

### Environment variables

| Var | Status | Notes |
|---|---|---|
| `DEAD` | CONFORMS | imported `main.rs:201-203`; used `send.rs:359`, `escapes.rs:152`. |
| `EDITOR` | CONFORMS | imported `main.rs:204`; used `commands.rs:527`, `escapes.rs:167`. |
| `HOME` | CONFORMS | used throughout for defaults. |
| `LISTER` | CONFORMS | imported `main.rs:210`; used `commands.rs:569`. |
| `MAILRC` | PARTIAL | used `main.rs:247`; but skipped under `-n` (#6). |
| `MBOX` | CONFORMS | imported `main.rs:207`; used in save/quit. |
| `PAGER` | CONFORMS | imported `main.rs:213`; used `commands.rs:880`, `escapes.rs:240`. |
| `SHELL` | CONFORMS | imported `main.rs:216`; used for all `sh -c`. |
| `VISUAL` | CONFORMS | imported `main.rs:219`; used `commands.rs:1357`, `escapes.rs:287`. |
| `TERM` | MISSING | Not read; `screen` defaults to hardcoded `20` (spec 104374, "unspecified default" — acceptable but TERM ignored). |
| `TZ` | PARTIAL | Not read explicitly; `chrono::Local` uses the process tz. Spec "may affect" (104380) — acceptable. |
| `LANG`/`LC_ALL`/`LC_CTYPE`/`LC_MESSAGES`/`LC_TIME`/`NLSPATH` | **MISSING** | (#7) No `setlocale`, never consulted. |

### Asynchronous events

- [x] **Implemented** (#2 FIXED). `SIGINT` handler installed; command-abort/re-prompt, input-mode `@`/two-interrupt-abort, and `DEAD` save-on-interrupt all present (`signals.rs`, `main.rs`, `send.rs`, `commands.rs`). `SIGQUIT`/`SIGHUP` left at defaults (acceptable; spec focuses on `SIGINT`).

### STDOUT / STDERR

- [x] Prompt, header summaries, message bodies → stdout — `main.rs:150-152`, `mailbox.rs:252`, `commands.rs:910`.
- [x] Command/parse errors → stderr — `main.rs:185`.
- [ ] **Some informative notices → stderr** (#22).

### Output files (mbox format)

- [x] `From ` separator line + headers + blank + body — `mailbox.rs:348-382`.
- [x] Body `From ` lines escaped with `>` on write; `>From ` unescaped on read — `mailbox.rs:117-122, 369-374`.
- [x] `keep` empty-mailbox truncation vs default removal — `mailbox.rs:316-328`.
- [x] quit state machine: deleted dropped, read→mbox (system, `!hold`), saved handling per `keepsave`, preserved/new/unread retained — `mailbox.rs:280-305`.

### Commands (`commands.rs`)

| Command | Status | Notes |
|---|---|---|
| `alias`/`group` | PARTIAL | list/define OK; (#17) no `\` recursion guard. |
| `alternates` | CONFORMS | `commands.rs:326-337`. |
| `cd` | CONFORMS | `commands.rs:348-357`. |
| `copy`/`Copy` | CONFORMS | `commands.rs:359-437`. |
| `delete` | CONFORMS | current-message update + `autoprint` — `commands.rs:439-475`. |
| `discard`/`ignore` | CONFORMS | `commands.rs:477-490`; retain overrides — `message.rs:116-122`. |
| `dp`/`dt` | CONFORMS | `commands.rs:501-518`. |
| `echo` | CONFORMS | `commands.rs:96-99`. |
| `edit` | CONFORMS | `EDITOR`, temp file — `commands.rs:520-544`. |
| `exit` | CONFORMS | no message moving — `commands.rs:105-106`. |
| `file`/`folder` | PARTIAL | quit-then-load OK; (#18) `#` unimplemented. |
| `folders` | PARTIAL | `LISTER` via `sh -c` (no `--`, minor) — `commands.rs:566-581`. |
| `followup`/`Followup` | CONFORMS | ignores `record`, records by author — `commands.rs:583-614`. |
| `from` | CONFORMS | `commands.rs:616-625`. |
| `headers` | CONFORMS | screenful pagination — `commands.rs:627-644`. |
| `help`/`?` | CONFORMS | `commands.rs:646-689`. |
| `hold`/`preserve` | CONFORMS | (#13 FIXED) gated on the system mailbox. |
| `if`/`else`/`endif` | **DIVERGES** | (#16) no-op in command mode. |
| `list` | CONFORMS | `commands.rs:707-717`. |
| `mail` | CONFORMS | alias expand, asksub — `commands.rs:719-750`. |
| `mbox` | CONFORMS | (#13 FIXED) gated on the system mailbox; `force_mbox` overrides the `hold` variable. |
| `next` | CONFORMS | `displayed`-flag logic per RATIONALE — `commands.rs:768-787`. |
| `pipe` | CONFORMS | (#4 FIXED) `--` inserted; `cmd`/`page` handled — `commands.rs:789-867`. |
| `Print`/`Type` | CONFORMS | overrides suppression — `commands.rs:867-930`. |
| `print`/`type` | PARTIAL | (#15) crt pagination ignores tty. |
| `quit` | CONFORMS | `main.rs:178-179`, `mailbox.rs:261-331`. |
| `reply`/`Reply` (`flipr`) | CONFORMS | (#11 FIXED) reply-all uses `Reply-To` when present; r/R + flipr mapping correct. |
| `retain` | CONFORMS | overrides discard/ignore — `commands.rs:965-978`. |
| `save`/`Save` | CONFORMS | msglist-or-file disambiguation — `commands.rs:989-1089`. |
| `set` | CONFORMS | `no`-prefix unset, `=` values, `print_all` — `commands.rs:1091-1115`. |
| `shell` | CONFORMS | interactive `SHELL` — `commands.rs:1187-1191`. |
| `size` | CONFORMS | `commands.rs:1193-1207`. |
| `source` | CONFORMS | `commands.rs:1209-1223`. |
| `top` | CONFORMS | `toplines` — `commands.rs:1238-1276`. |
| `touch` | CONFORMS | (#13 FIXED) gated on the system mailbox; forces message to mbox. |
| `unalias` | CONFORMS | `commands.rs:1296-1301`. |
| `undelete` | CONFORMS | default-selection rules — `commands.rs:1303-1334`. |
| `unset` | CONFORMS | `commands.rs:1336-1341`. |
| `visual` | CONFORMS | `VISUAL`, default `vi` — `commands.rs:1350-1374`. |
| `write` | CONFORMS | body only, append — `commands.rs:1376-1424`. |
| `z[+|-]` | CONFORMS | `commands.rs:1426-1453`. |
| `!command` | CONFORMS | (#4 FIXED) `--` inserted; `bang` expansion OK — `commands.rs`. |
| `#` comment | CONFORMS | `commands.rs:45-47`. |
| `=` | CONFORMS | `commands.rs:55-58`. |

### Command escapes (`escapes.rs`)

| Escape | Status | Notes |
|---|---|---|
| `~!command` | CONFORMS | (#4 FIXED) `--` inserted. |
| `~.` | CONFORMS | finish input — `escapes.rs:82-85`. |
| `~:`/`~_` | **PARTIAL** | (#9) stub; `set` doesn't set. |
| `~?` | CONFORMS | `escapes.rs:103-107`. |
| `~A`/`~a` | CONFORMS | `Sign`/`sign` with `\t`/`\n` — `escapes.rs:118-135`. |
| `~b`/`~c`/`~t` | CONFORMS | Bcc/Cc/To — `escapes.rs:136-149, 278-284`. |
| `~d` | CONFORMS | read `DEAD` — `escapes.rs:150-164`. |
| `~e`/`~v` | CONFORMS | EDITOR/VISUAL — `escapes.rs:165-169, 285-289`. |
| `~f`/`~F` | CONFORMS | forward — `escapes.rs:170-183`. |
| `~h` | PARTIAL | (#20) no tty gate. |
| `~i var` | CONFORMS | insert variable — `escapes.rs:189-198`. |
| `~m`/`~M` | CONFORMS | indented insert — `escapes.rs:199-212`. |
| `~p` | PARTIAL | (#15) crt/tty. |
| `~q`/`~x` | CONFORMS | abort w/ and w/o dead-letter — `escapes.rs:247-302`. |
| `~r`/`~<` (`!command`) | CONFORMS | (#4 FIXED) `--` inserted on the command path — `escapes.rs`. |
| `~s` | CONFORMS | `escapes.rs:273-277`. |
| `~w file` | **DIVERGES** | (#10) truncates — `escapes.rs:290-298`. |
| `~|command` | CONFORMS | (#4 FIXED) `--` inserted — `escapes.rs`. |
| `~~` | CONFORMS | literal `~` — `escapes.rs:317-323`. |

### Internal variables (`variables.rs`)

- [x] Defaults set: `asksub`, `header`, `save`, `prompt="? "`, `indentprefix="\t"`, `toplines=5` — `variables.rs:45-54`.
- [x] `ask`/`asksub` synonym — `variables.rs:60-62`.
- [x] `onehop` rejected (permanent `noonehop`) — `commands.rs:1102-1105` (documented divergence; spec default is `noonehop`, acceptable).
- [ ] **`escape=` (null) doesn't disable escaping** (#12).
- Boolean/string handling, `no`-prefix unset, alias/alternates/ignored/retained tracking — CONFORMS.

### Exit status / consequences of errors

- [x] `-e`: 0 if mail, >0 otherwise — `main.rs:64-71`.
- [x] Send Mode: 0 success, 1 on error — `main.rs:48-61`.
- [x] Receive Mode command errors print + continue (CONSEQUENCES = Default) — `main.rs:184-186`.
- [ ] Input/Send Mode tilde-error policy (diagnostic but don't block send vs. other errors block) not modeled (spec 105114-105119) — escape errors currently abort the compose loop (`commands.rs:1522-1528`, `send.rs:202`). Minor.

## Test coverage signal

141 tests exercise CLI options, receive-mode commands, msglist parsing, and variables. Not covered (mostly because unimplemented):

- [x] `-E` empty-body discard (#1) — `opt_e_uppercase_discards_empty_body`, `opt_e_uppercase_keeps_nonempty_body`.
- [x] Signal / interrupt behavior (#2) — `signals::tests::sigint_sets_and_clears_flag` unit test; manual TTY/FIFO verification (mailx survives SIGINT in command mode, exits 0 on quit).
- [x] Multibyte header/subject truncation panic (#3) — `header_summary_multibyte_no_panic`.
- [x] `sh -c` receiving the `--` argument (#4) — `pipe_command_leading_dash_reaches_program`.
- [x] `-f -N file` / `mailx -fin file` operand parsing (#5) — `opt_f_operand_after_other_options`, `opt_f_clustered_fin_operand`.
- [x] `mailx -n` still reading `~/.mailrc` (#6) — `opt_n_still_reads_user_mailrc`.
- [ ] `LC_*`/`setlocale` effect on diagnostics (#7).
- [x] `new` vs `unread` state characters / `:n` selector (#8) — `new_state_and_n_selector`.
- [ ] `~:set` actually setting a variable (#9); `~w` append (#10); reply-all `Reply-To` (#11); `escape=` disabling escapes (#12).

## Suggested PR groupings

- **PR A — "Required options & operands"**: #1 (`-E` + empty-body discard), #5 (`-f` operand → Receive Mode), #6 (`-n` keeps user MAILRC). Argv/start-up layer.
- **PR B — "Asynchronous events"**: #2. Install `SIGINT` handling; wire `ignore`/`save`/`DEAD`; remove the dead `ErrorKind::Interrupted` path.
- **PR C — "Crash & shell-exec correctness"**: #3 (char-boundary truncation), #4 (`--` argument across all `sh -c` sites).
- **PR D — "Message state & reply fidelity"**: #8 (`new` state), #11 (`Reply-To`), #13 (`mbox`/`touch`/`hold` semantics + system-mailbox gate).
- **PR E — "Input-mode escapes"**: #9 (`~:` dispatch + real `set`), #10 (`~w` append), #12 (`escape=` null), #19 (`ignoreeof` in compose), #20 (`~h` tty gate).
- **PR F — "Locale + i18n plumbing"**: #7. `setlocale` + `gettext`, honor `LC_MESSAGES`/`LC_CTYPE`.
- **PR G — "Start-up & command-mode niceties"**: #14 (start-up diagnostics/legal cmds), #15 (crt tty gate), #16 (`if` in command mode), #17 (alias `\`), #18 (`#` folder), #21 (`-u` spool paths), #22 (stderr routing).
