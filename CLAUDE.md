# CLAUDE.md

Read CONTRIBUTING.md before building, testing or changing code: it holds the
build and test commands, source layout, code style, dependency policy and
test conventions, shared with human contributors.  README.md has the goals
and platforms.  This file holds only rules for the agent.

## Git

Ask before any `git add` or `git commit`. Never amend a commit; make a new one.

Before committing, re-read `git diff HEAD` in full, and get
`cargo clippy --all-targets` to zero warnings, fixing pre-existing ones too.

## Debugging

Keep one wrapper script at `/tmp/<name>.sh`. Change it with the Write/Edit
tools, not `cat` heredocs, and run it as `/tmp/<name>.sh`, not
`bash /tmp/<name>.sh`, so that each run does not need a new permission prompt.
