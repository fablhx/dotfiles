# Coding & review conventions

These are global conventions and apply to every project. A project's own
`CLAUDE.md` wins where the two disagree.

---

# Workflow

## Definition of done

A task is done when all of the following hold. "Looks finished" is not
finished.

1. The new behavior is covered by tests (see *Test new behavior*).
2. Formatting and compilation are clean (see *Committing*).
3. Everything CI would check passes locally (see *Verify against CI*).
4. Side-issues found on the way are logged, not silently left behind (see
   *Capturing follow-up work*).

Skip 1-3 for pure docs / non-code edits.

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
- "Test" means verification appropriate to what changed — not always a
  test-framework test:
  - Application code / logic → automated tests, per the rules above.
  - Build targets, CI steps, scripts, Dockerfiles, config (e.g. a new Makefile
    target) → run it and confirm it actually does what it claims, then ensure
    CI exercises it so it can't silently break later. Don't author a unit test
    for a thin wrapper like `build: cargo build` — running it once is enough.
  - If it contains real logic (codegen, multi-step release, conditionals),
    verify the meaningful outcomes, not just that it exits 0.
- Style follows the project's existing tests; placement follows the
  language-specific rules below (for Rust, see *Rust > Test organization*).
- Prefer a few discriminating tests over many shallow ones. Coverage (a line
  ran) is not quality (a wrong line would be caught). When a unit has a clear
  invariant that must hold for all inputs, reach for a property-based test
  instead of a handful of hand-picked cases (for Rust, see *Rust > Property
  tests* and *Rust > Mutation testing*).

## Committing

Work is usually split into steps / phases. Commit each completed phase before
moving to the next — do it yourself, without being asked or reminded.

- Commit a phase only when it's in a coherent, working state; each commit
  should stand on its own.
- Before every commit, if my personal tooling is present, run both of these
  and only commit once both pass clean:
  - `~/work/git/tools/bin/run_format` — formatting
  - `~/work/git/tools/bin/run_build` — compilation
- Detect availability first (the files exist and are executable). If they
  aren't present on this machine, skip them silently and fall back to the
  project's own formatter / build (rustfmt/`cargo fmt`, clang-format,
  prettier, gofmt, black, or its `fmt`/`format` task). If no formatter is
  configured, skip that step silently.
- Order: format first, restage any files it changed, then build. Include
  formatting changes in the same commit.
- If a pre-commit hook enforces formatting, let it run; if it fails, fix the
  cause and retry.
- If any of these fails, do NOT commit. Fix the cause and re-run until clean;
  never bypass with `--no-verify` (see *Never fake green*).
- When present, these scripts are the source of truth for "format and
  compilation are clean" — prefer them over guessing project-specific
  commands.
- Commit messages describe what changed, never the workflow (see *Don't encode
  the build process into the artifact*).

## Verify against CI

When you believe you're finished, verify against the project's CI before
stopping.

- Read `.github/workflows/*.yml` (and any scripts they call) and identify
  every check step — build, tests, lint, format, typecheck, etc.
- Replicate those checks locally by running the same commands the workflow
  runs, in the same order and with the same flags/env where feasible. You are
  not running CI itself; you are running the commands it would run.
- Run only what is reproducible locally. Skip steps needing secrets,
  deployment, external services, or a specific runner OS — list what you
  skipped rather than failing on it.
- If a check fails, fix the ROOT CAUSE and re-run the full set. Repeat until
  everything runnable locally is green.
- **Never fake green**: do NOT disable, ignore, weaken, or delete tests, add
  suppressions (`#[allow]`, `// eslint-disable`, `# noqa`), loosen config, or
  use `--no-verify` to force a pass. Fix the actual problem.
- Bound the effort: if a check still fails after ~3-4 genuine fix attempts,
  stop and report what fails, the exact error, and what you tried — don't
  thrash or stack speculative changes.
- If there's no CI workflow, fall back to the project's own build / test /
  lint commands and the pre-commit tooling above.

## Capturing follow-up work

- Maintain a running list of follow-up work in `~/work/git/TODO.md`. Create it
  if it doesn't exist; always append — never rewrite or drop existing entries.
- While implementing something, if you discover a bug, a blocker, or a
  side-issue that belongs to separate work, do NOT fix it inline. Record it
  and carry on with the current task, so each piece of work stays focused and
  self-contained.
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

---

# Code conventions (all languages)

## Naming

- Never use time-relative words like "legacy", "old", "new", "current",
  "modern" in identifiers, comments, or docs. They're true today and wrong
  tomorrow. Name things by what they are/do, not when they existed.

## Comments explain WHY, not WHAT

Well-named identifiers already say what the code does, so a comment restating
it is noise. Write a comment only when the reason isn't visible in the code.

- Delete: comments that restate the code (`// call reset` above `reset()`),
  narrate the change ("now using X", "refactored to..."), reference the
  task/caller/spec, or mark structure ("// helpers below").
- Keep: the non-obvious WHY — hidden constraints, subtle invariants, why an
  unusual or seemingly-wrong approach is correct, workarounds (with the
  reason, ideally a link/issue), safety/ordering requirements, units/edge
  cases a caller can't infer.
- Rule of thumb: if the comment is derivable from the code it sits on, it's
  noise. If removing it would lose knowledge not recoverable from reading the
  code, keep it.
- Don't write comments to satisfy a perceived quota. Zero comments on
  self-explanatory code is correct. Never add a comment just to have one.
- Default to fixing the code over commenting it: if a comment exists to
  explain an unclear name or a confusing block, rename or refactor so the
  comment becomes unnecessary, then drop it.
- Doc comments (`///`, docstrings, JSDoc) on public API are exempt — this rule
  targets explanatory inline comments, not API documentation.

## String literals: ASCII only

- String and character literals must contain ASCII only, in ANY language.
  This subsumes smart punctuation (em/en dashes, curly quotes, ellipsis,
  non-breaking space), arrows, and every other non-ASCII glyph. Comments, doc
  comments, and prose are exempt.
- Use ASCII equivalents: `-` / `--` dashes, `'` and `"` quotes, `...`
  ellipsis, `->` `<-` `=>` arrows, a normal space for NBSP.
- Exception: when a literal genuinely must carry a Unicode character (real
  user-facing UTF-8 text), encode it with an explicit escape so the intent is
  visible — `"\u{2192}"` (Rust), `"\u2192"` (Python/JS) — never a pasted
  glyph.

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
  commit messages): no "Phase 1/2", "Step 3", "in this iteration", "for now",
  "as a first pass", "later we'll", "temporary until...", etc.
- Write every comment and commit message as if describing the finished code in
  its current, standalone state.

## Types are the primary verification mechanism

Prefer a guarantee the compiler enforces over a runtime check, and a runtime
check over a comment or a convention. If a class of bug can be made
unrepresentable in the types, do that instead of defending against it.

- **Make illegal states unrepresentable.** Model the domain with sum types
  (Rust enums) so invalid combinations don't type-check. Do NOT use a struct of
  independent `bool`s / `Option`s whose valid combinations are a subset of the
  possible ones — use one enum with data in each variant instead.
- **Parse, don't validate.** Validate at the boundary and return a type that
  carries the invariant, so downstream code cannot re-check or forget to check.
  Once a value has the type, the invariant holds by construction.
- **No primitive obsession.** Don't pass bare `String` / integers for domain
  values (ids, paths, keys, byte counts, durations, units). Wrap them in
  newtypes so mixing them up is a compile error, not a runtime surprise.
- **No boolean parameters.** `f(true)` is unreadable and easy to invert; use a
  two-variant enum whose name says what it means.
- **Total functions.** A function's signature must tell the whole truth: if it
  can fail, return `Result`; if a value may be absent, return `Option`. No
  sentinel values, no panicking on inputs the type permits.
- **Exhaustive matching.** Match on all variants explicitly; avoid catch-all
  `_ =>` arms on our own enums, so adding a variant produces compile errors at
  every site that must be updated. That failure is the feature.
- **Don't defeat the type system.** No `as` numeric casts (use `TryFrom` and
  handle the error), no stringly-typed APIs, no `unwrap`/`expect` to paper over
  an invariant that could have been encoded, no shortcut through `Any` or
  untyped maps.
- **Encode state machines in types** where the mistake is plausible: a phantom
  type parameter or a consuming state transition (`fn open(self) -> Open<T>`)
  makes use-in-the-wrong-state a compile error rather than a runtime check.

## Reading and verifying code

- **Read signatures first.** When exploring or reviewing, work from the types
  before the bodies; a good signature already narrows what the function can
  legally do. If a signature doesn't constrain the behavior meaningfully, that
  is itself the finding — say so and propose a tighter type.
- **When you finish a change, state where the guarantees come from**: which
  invariants the types enforce, which are enforced at runtime, and which rest
  only on convention. The last group is the list of things to fix or test.
- **Prefer strengthening a type over adding a check.** If you catch yourself
  writing a defensive `assert!`, an early-return guard, or a comment saying
  "caller must ensure X", first ask whether a type could make X impossible.
  Note the choice if you decide not to.
- **Don't test what the types forbid.** A test asserting a state the type
  system makes unrepresentable can't even be written — that's a guarantee, not
  a coverage gap. Spend tests on behavior the types can't express.

---

# Rust

## Error handling

- We use `error_stack = "0.5"` — keep all method names matching that version.
- Use `error_stack` for all fallible functions. Return
  `error_stack::Result<T, MyError>` (the alias for `Result<T, Report<MyError>>`)
  — not bare `Result` or `Box<dyn Error>`. Bring `error_stack::ResultExt` into
  scope; it provides `change_context` / `attach_printable`.
- Error *context* types are lightweight: an enum/struct with STATIC messages,
  deriving `Display` + `Error` (thiserror is fine). Never store dynamic
  strings in the error type — no `MyError(String)`.
- Enrich errors as they propagate:
  - `.change_context(MyError::Variant)` when crossing a module / abstraction
    boundary (adds a layer + source location).
  - `.attach_printable(x)` / `.attach_printable_lazy(|| ...)` for dynamic
    context (ids, paths, values). Prefer the lazy variant — it runs only on
    the error path. Use the *printable* variants; plain `.attach(...)`
    attachments aren't shown by default.
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

## Keep side effects out of assertion / error macros

- Control-flow and error macros — `ensure!`, `bail!`, `report!`, `assert!`,
  `debug_assert!`, and similar — must receive only side-effect-free
  expressions. Do not put a function call inside the macro invocation.
- Evaluate the call into a `let` binding on the line before, then pass the
  binding. The macro's job is to test an already-computed value
  (`ensure!(rc == 0, ...)`), never to run the work.
- This covers every argument — the condition, the error/context, and any
  format arguments. In particular the error/context of `ensure!`/`report!` is
  evaluated only on the failure path, so a call hidden there silently does NOT
  run on success — and burying a call in a macro hides evaluation order and
  makes the side effect invisible at the call site. (Some macros also evaluate
  arguments more than once.)
- Quick test: if moving the call out of the macro would change whether or when
  a side effect happens, it belonged outside the macro.

Bad:

```rust
ensure!(reset_handle() == 0, CryptoError::Reset);
bail!("cleanup failed: {}", run_cleanup());
```

Good:

```rust
let rc = reset_handle();
ensure!(rc == 0, CryptoError::Reset);

let status = run_cleanup();
bail!("cleanup failed: {status}");
```

## Test organization

- Unit tests always live in a dedicated sibling file, never inline. In the
  source file, declare the module with `#[cfg(test)] mod tests;` (a bare
  declaration, no inline `{ ... }` block).
- Put the tests in the module's directory: `foo/tests.rs` for `foo.rs`, or
  `src/tests.rs` for the crate root (`lib.rs` / `main.rs`). Start the file
  with `use super::*;` so tests keep access to the parent module's private
  items.
- Creating `foo/tests.rs` does NOT require renaming `foo.rs` to `foo/mod.rs` —
  the submodule file coexists with `foo.rs`.
- Integration tests are unaffected: they stay in the top-level `tests/`
  directory and exercise only the public API.

## Assertion hygiene

- Prefer `assert_eq!` / `assert_ne!` over bare `assert!(a == b)` — the former
  print both operands on failure, the latter prints nothing useful.
- Avoid `#[should_panic]`. It asserts only THAT a panic happened, not which one
  or from where, so an unrelated panic makes it pass green. This follows from
  *Total functions*: a fallible operation returns `Result`, so test it by
  asserting on the `Err` variant, not by catching a panic.
- Snapshot tests (e.g. `insta`) are a last resort, not a default. They are
  trivial to "fix" by blessing whatever the code currently emits, which quietly
  locks in bugs — the opposite of a test that can fail for a real reason. Use
  them only for genuinely stable serialized output, and review a changed
  snapshot as carefully as changed code, never rubber-stamp it.

## Property tests

Use `proptest` for behavior that must hold across a whole input space, rather
than enumerating cases by hand. This is the tool for the "ensure invariant"
half of testing; hand-picked cases are for specific known-tricky inputs.

- Good targets: round-trips (`decode(encode(x)) == x`), idempotence
  (`f(f(x)) == f(x)`), ordering/monotonicity, conservation (counts/sums
  preserved), and agreement with a simple reference implementation.
- State the property as a comment (the WHY), then let proptest search. On
  failure it shrinks to a minimal counterexample — add that counterexample as
  its own named regression test so the specific bug stays pinned even if the
  generator changes. proptest also persists failures under a
  `.proptest-regressions` file; keep it in version control.
- Don't reach for a property test where there is no real invariant — a
  generator that just re-implements the function under test proves nothing.
  When in doubt, one property plus a couple of concrete edge cases beats a
  large opaque strategy.

## Mutation testing

Coverage confirms a line executed; it does not confirm a test would notice if
that line were wrong. `cargo-mutants` is the honest gate: it introduces small
faults (flipped comparisons, swapped match arms, replaced return values) and
reports which ones the suite failed to catch. A surviving mutant is a real gap
that quantity of tests cannot hide.

- Run `cargo mutants` on code whose correctness matters; treat each surviving
  (uncaught) mutant as a missing or too-weak assertion and strengthen the test
  until it kills the mutant. Do NOT kill a mutant by narrowing the mutated code
  to satisfy the tool — fix the test, not the target.
- Scope it: `cargo mutants -f <file>` or `--in-diff` to check only what a change
  touched, so runs stay fast enough to be worth doing.
- Use it as a diagnostic to find weak spots, not as a coverage-style percentage
  to chase to 100%. Some mutants are equivalent (no observable difference) and
  legitimately unkillable — note those rather than contorting tests around
  them.

## Undefined behavior (unsafe)

- Default to no `unsafe`. If a safe formulation exists, use it; the type-system
  guarantees above only hold on safe code.
- If `unsafe` is genuinely unavoidable, every block carries a `// SAFETY:`
  comment stating the invariant that makes it sound (this is a WHY comment, so
  it is exempt from the comment-noise rule), and that invariant is upheld by
  construction wherever possible.
- Run the affected tests under Miri, which interprets the code and detects
  undefined behavior ordinary tests pass over — out-of-bounds access,
  use-after-free, invalid aliasing, unaligned reads:
  `cargo +nightly miri test`. Fix any UB Miri reports at the root; never
  silence it. (Miri is not needed for fully safe crates.)
