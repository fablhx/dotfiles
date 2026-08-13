# Coding & review conventions

## Verify against CI before declaring a task done

- A task is not "done" until the code builds and passes checks. "Looks
  finished" is not finished. This applies to any change CI would check; skip
  it for pure docs / non-code edits.
- When you believe you're finished, find the project's CI before stopping:
  read `.github/workflows/*.yml` (and any scripts they call) and identify every
  check step — build, tests, lint, format, typecheck, etc.
- Replicate those checks locally by running the same commands the workflow
  runs, in the same order and with the same flags/env where feasible. You are
  not running CI itself; you are running the commands it would run.
- Run only what is reproducible locally. Skip steps needing secrets,
  deployment, external services, or a specific runner OS — list what you
  skipped rather than failing on it.
- If a check fails, fix the ROOT CAUSE and re-run the full set. Repeat until
  everything runnable locally is green.
- Never fake green: do NOT disable, ignore, weaken, or delete tests, add
  suppressions (`#[allow]`, `// eslint-disable`, `# noqa`), loosen config, or
  use `--no-verify` to force a pass. Fix the actual problem.
- Bound the effort: if a check still fails after ~3–4 genuine fix attempts,
  stop and report what fails, the exact error, and what you tried — don't
  thrash or stack speculative changes.
- If there's no CI workflow, fall back to the project's own build / test / lint
  commands (and my personal `run_format` / `run_build` if present).

## Test new behavior

- Any new feature or behavior change ships with tests that exercise it. A
  feature isn't done when the code exists — it's done when tests prove it
  behaves as intended. (Skip for pure refactors with no behavior change and
  for trivial/non-code edits.)
- Test the intended behavior, not the implementation. Assert on observable
  outcomes and the public contract so the test survives a refactor. Do NOT
  write tests that merely re-encode what the code currently happens to do.
- The test must be able to FAIL. If it would still pass against a broken or
  reverted implementation, it proves nothing. Sanity check: invert the core
  logic in your head — the test should go red. If it wouldn't, rewrite it.
- Cover more than the happy path: edge cases, boundaries, empty/zero/overflow
  inputs, and the error paths the feature introduces — not just success.
- Pick the right level (unit vs integration) for what's being verified. If a
  feature genuinely can't be tested at a reasonable level, say so and explain
  why — don't write a hollow test just to satisfy this rule.
- Every test asserts something meaningful: no assertion-free tests, no
  `#[ignore]`/skips to make a suite pass, no keeping tests green by weakening
  what they check.
- Placement follows my Rust test-organization rules; style follows the
  project's existing tests.
- "Test" means verification appropriate to what changed — not always a
  test-framework test:
  - Application code / logic → automated tests, per the rules above.
  - Build targets, CI steps, scripts, Dockerfiles, config (e.g. a new Makefile
    target) → run it and confirm it actually does what it claims, then ensure
    CI exercises it so it can't silently break later. Don't author a unit test
    for a thin wrapper like `build: cargo build` — running it once is enough.
  - If it contains real logic (codegen, multi-step release, conditionals),
    verify the meaningful outcomes, not just that it exits 0.

## Naming

- Never use time-relative words like "legacy", "old", "new", "current", "modern"
  in identifiers, comments, or docs. They're true today and wrong tomorrow. Name
  things by what they are/do, not when they existed.

## Comments explain WHY, not WHAT.

- Comments explaining WHAT the code does (well-named identifiers already do
  that), narrating the change, or referencing the task/caller — delete; keep
  only non-obvious WHY (hidden constraints, subtle invariants, workarounds).
- Well-named identifiers already say what the code does, so a comment restating
  it is noise — delete it. Write a comment only when the reason isn't visible in
  the code itself.
- Delete: comments that restate the code (`// call reset` above
  `reset()`), narrate the change ("now using X", "refactored to…"),
  reference the task/caller/spec, or mark structure ("// helpers below").
- Keep: the non-obvious WHY — hidden constraints, subtle invariants, why an
  unusual or seemingly-wrong approach is correct, workarounds (with the reason,
  ideally a link/issue), safety/ordering requirements, units/edge cases a
  caller can't infer.
- Test: if the comment is derivable from the code it sits on, it's noise. If
  removing it would lose knowledge not recoverable from reading the code, keep
  it.
- Don't write comments to satisfy a perceived quota. Zero comments on
  self-explanatory code is correct. Never add a comment just to have one.
- Default to fixing the code over commenting it: if a comment exists to
  explain an unclear name or a confusing block, rename or refactor so the
  comment becomes unnecessary, then drop it.
- Doc comments (`///`, docstrings, JSDoc) on public API are exempt — this rule
  targets explanatory inline comments, not API documentation.

## Working documents (specs, plans, design notes)

- I often hand you an uncommitted working document as the source for an
  implementation. The name varies — SPEC.md, PLAN.md, etc. Treat any such file
  as a transient working document.
- Identify them by property, not by name: if a file is not tracked by git
  (untracked or gitignored), treat it as a working document.
- You may implement from these files, but never reference, cite, link, or
  mention them in code, comments, doc comments, or commit messages. Anything
  committed must stand on its own without pointing to an uncommitted file.

## Don't encode the build process into the artifact

- Implementation is usually split into steps / phases / iterations. That
  sequencing describes how we built the code, not what it is.
- Never mention it in committed artifacts (code, comments, doc comments,
  commit messages): no "Phase 1/2", "Step 3", "in this iteration",
  "for now", "as a first pass", "later we'll", "temporary until…", etc.
- Write every comment and message as if describing the finished code in its
  current, standalone state.

## Committing across multi-step work

- When work is split into steps / phases, commit the completed phase before
  moving to the next one — do it yourself, without being asked or reminded.
- Commit a phase only when it's in a coherent, working state; each commit
  should stand on its own.
- Before each commit, if a formatter or format check is available, run it and
  include any resulting changes in the same commit:
  - If a pre-commit hook enforces formatting, let it run; if it fails, fix and
    retry rather than bypassing it.
  - Otherwise run the project's formatter (e.g. rustfmt/`cargo fmt`,
    clang-format, prettier, gofmt, black) or its `fmt`/`format` build task.
  - If no formatter is configured, skip this step silently.
- Commit messages describe what changed, never the workflow — no "Phase 1/2",
  "step N", "iteration", etc.

## Pre-commit checks

- Before every commit, if my personal tooling is present, run both of these
  and only commit once both pass clean:
  - `~/work/git/tools/bin/run_format` — formatting
  - `~/work/git/tools/bin/run_build` — compilation
- Detect availability first (e.g. the files exist and are executable). If they
  aren't present on this machine, skip these checks silently and fall back to
  the project's own formatter / build — do not error out or ask about them.
- Order: run `run_format` first, restage any files it changed, then
  `run_build`. Include formatting changes in the same commit.
- If either script fails, do NOT commit. Fix the cause and re-run until both
  pass; never bypass with `--no-verify`.
- These scripts are the source of truth for "format and compilation are clean"
  — prefer them over guessing project-specific commands when they're available.

## ASCII-only code string literals

- String and character literals must contain ASCII only, in ANY language
  (C, Rust, Python, JS, …). This subsumes smart punctuation (em/en dashes,
  curly quotes, ellipsis, NBSP), arrows (→ ← ⇒), and every other non-ASCII
  glyph. Comments, doc comments, and prose are exempt.
- Use ASCII equivalents: `-`/`--` dashes, `'` `"` quotes, `...` ellipsis,
  `->` `<-` `=>` arrows, normal space for NBSP.
- Exception: when a literal genuinely must carry a Unicode character (real
  user-facing UTF-8 text), encode it with an explicit escape so the intent is
  visible — `"\u{2192}"` (Rust), `"\u2192"` (Python/JS) — never a pasted glyph.

## Rust error handling

- We use `error_stack = "0.5"` (replace with your actual version) — keep all
  method names matching it.
- Use `error_stack` for all fallible functions. Return
  `error_stack::Result<T, MyError>` (the alias for `Result<T, Report<MyError>>`)
  — not bare `Result` or `Box<dyn Error>`. Bring `error_stack::ResultExt` into
  scope; it provides `change_context` / `attach_printable`.
- Error *context* types are lightweight: an enum/struct with STATIC messages,
  deriving `Display` + `Error` (thiserror is fine). Never store dynamic strings
  in the error type — no `MyError(String)`.
- Enrich errors as they propagate:
  - `.change_context(MyError::Variant)` when crossing a module / abstraction
    boundary (adds a layer + source location).
  - `.attach_printable(x)` / `.attach_printable_lazy(|| …)` for dynamic context
    (ids, paths, values). Prefer the lazy variant — it runs only on the error
    path. Use the *printable* variants; plain `.attach(...)` attachments aren't
    shown by default.
- Dynamic detail goes in an attached printable, never as a field on the error.

Canonical pattern:

```rust
use error_stack::{Result, ResultExt};

#[derive(Debug, thiserror::Error)]
enum ConfigError {
    #[error("failed to read config file")]
    Read,
    #[error("config is not valid TOML")]
    Parse,
}

fn load_config(path: &Path) -> Result<Config, ConfigError> {
    let raw = std::fs::read_to_string(path)
        .change_context(ConfigError::Read)
        .attach_printable_lazy(|| format!("path: {}", path.display()))?;

    let config = toml::from_str::<Config>(&raw)
        .change_context(ConfigError::Parse)?;

    Ok(config)
}
```

## Rust test organization

- Unit tests always live in a dedicated sibling file, never inline. In the
  source file, declare the module with `#[cfg(test)] mod tests;` (a bare
  declaration, no inline `{ … }` block).
- Put the tests in the module's directory: `foo/tests.rs` for `foo.rs`, or
  `src/tests.rs` for the crate root (`lib.rs` / `main.rs`). Start the file with
  `use super::*;` so tests keep access to the parent module's private items.
- Creating `foo/tests.rs` does NOT require renaming `foo.rs` to `foo/mod.rs` —
  the submodule file coexists with `foo.rs`.
- Integration tests are unaffected: they stay in the top-level `tests/`
  directory and exercise only the public API.

# Capturing follow-up work (~/work/git/TODO.md)

- Maintain a running list of follow-up work in `~/work/git/TODO.md`. Create it if it
  doesn't exist; always append — never rewrite or drop existing entries.
- While implementing something, if you discover a bug, a blocker, or a
  side-issue that belongs to separate work, do NOT fix it inline. Record it in
  `~/work/git/TODO.md` and carry on with the current task, so each piece of work stays
  focused and self-contained.
- Exception: if the discovery actually blocks finishing the current task, log
  it AND tell me — don't silently continue on a broken path.
- Each entry must carry enough context to act on later in a fresh session that
  has no memory of this one:
  - where it is (file / function / area),
  - what you observed (symptom, plus how to reproduce it if known),
  - one line on why it was deferred (what you were doing when you hit it).
- How the task is framed matters:
  - For a bug, do NOT write "there's a bug, fix it." Write it as an
    investigation: "Investigate the root cause; evaluate options including
    refactoring and broader improvements; then implement the best solution."
  - For a non-bug item, state the concrete outcome you want instead.
