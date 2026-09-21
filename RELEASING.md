# Releasing posixutils-rs

The project ships one version number for the whole workspace: every crate in
`*/Cargo.toml` carries the same `version`, and the release is named by a git
tag `vX.Y.Z` plus a GitHub release.

Versioning is a simple `0.MINOR.PATCH` cadence. A release that only fixes
defects in the previously tagged tree is a patch bump; anything else — new
utilities, new options, behavioural corrections, dependency churn — is a minor
bump. There is no 1.0 commitment yet.

## Full-gate validation

Several steps below call for *full-gate validation*. That means the same four
checks CI runs, in this order, all from the repository root and all passing
with a zero exit status:

```sh
cargo fmt --all -- --check
cargo clippy --all-targets -- -D warnings
cargo build --release
cargo test --release
```

Never use `-p` here: the gate is the whole workspace. The test run takes
15+ minutes. Zero warnings is the standard — a warning that predates the
release is still a warning that blocks it.

## Steps

### 1. Refresh dependencies

```sh
cargo update
```

Then run full-gate validation. Fix whatever the new dependency versions break
before going further; those fixes belong in their own commits, not in the
release commits. Commit the `Cargo.lock` change on its own.

### 2. Bump the version

First check that nothing outside the member manifests pins the old version:

```sh
grep -rn 'OLD\.VER\.SION' --include=Cargo.toml --include='*.md' --include='*.rs' . | grep -v '^./target'
```

The `[package] version` is the first `version =` in each member manifest, so:

```sh
NEW=X.Y.Z
for f in */Cargo.toml; do
    perl -0777 -i -pe 's/^version = ".*"/version = "'"$NEW"'"/m' "$f"
done
grep -l "^version = \"$NEW\"" */Cargo.toml | wc -l   # must equal the member count
```

`perl -i` rather than `sed -i` because the bump has to run on macOS too: BSD
sed requires a backup suffix after `-i`, and its addresses do not include the
GNU `0,/re/` form that would stop at the first match. The `-0777` slurp plus
`/m` gives perl the same first-match-only behaviour without it.

Verify the count rather than trusting the loop — a substitution that matches
nothing fails silently.

Building refreshes the path-dependency versions recorded in `Cargo.lock`, so
that file changes too; include it in the same commit. Then run full-gate
validation a second time.

### 3. Write the release notes

Review every commit since the previous tag and distil it into a bulleted
summary of the notable changes. Useful starting points:

```sh
git log vPREV..HEAD --no-merges --format='%s' | sort -u        # what changed
git log vPREV..HEAD --merges --format='%s%n%b'                 # PR-level shape
git diff --shortstat vPREV..HEAD                               # scale
git shortlog -sn vPREV..HEAD --no-merges                       # contributors
```

Group the result by area — the commit subject prefix (`cc:`, `sh:`, `make:`,
`fix(pax):` …) is the grouping key — and lead with a short Highlights section
naming the handful of changes a user would notice first. Prefer describing
what a utility can now do over listing the commits that got it there.

Write this to a scratch file **outside the repository**. The notes are pasted
into the GitHub release form; they are not checked in, and there is no
`CHANGELOG.md`.

### 4. Tag and publish

```sh
git tag -a vX.Y.Z -m 'Version X.Y.Z'
git push origin vX.Y.Z
```

Then create the GitHub release against that tag and paste in the notes from
step 3.

Nothing is published to crates.io.

### 5. Confirm the container image

Pushing the tag starts `.github/workflows/container.yml`, which builds
`linux/amd64` and `linux/arm64` natively, runs `scripts/docker-smoke` against
each, and pushes a manifest list to the GitHub container registry. Nothing else
publishes the image, and nothing about it is part of the local gate — so check
it once the workflow is green:

```sh
docker buildx imagetools inspect ghcr.io/rustcoreutils/posixutils-rs:X.Y.Z
```

Both platforms must be present. The tag also moves `X.Y` and `X`, so a user
pinned to a minor series picks the release up.
