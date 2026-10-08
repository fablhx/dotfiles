# Coding & review conventions

These are global conventions and apply to every project. Machine-local
instructions, such as team context, live in `~/.claude/CLAUDE.private.md`,
imported at the end of this file, and shared team rules live under
`~/.claude/rules/`. Where they disagree, the repository's own `CLAUDE.md` wins,
then the project files the private file points to, then the private file and
the team rules, then this file.

---

# Workflow

## Definition of done

A task is done when all of the following hold. "Looks finished" is not
finished.

1. The new behavior is covered by tests (see *Test new behavior*). A Rust
   change to crypto, parsing or serialization code has also been through
   mutation testing (see *Rust > Mutation testing*).
2. Formatting and compilation are clean, and the work is committed (see
   *Committing*).
3. Everything CI would check passes locally (see *Verify against CI*).
4. Side-issues found on the way are logged, not silently left behind (see
   *Capturing follow-up work*).
5. The final message reports, where they apply:
   - the CI steps skipped as not reproducible locally, and any check still
     failing after the attempt bound, with its exact error and what you tried;
   - for a non-trivial change in a typed language, where its guarantees come
     from (see *Reading and verifying code*);
   - every lint suppression added for a false positive;
   - a new build target or script that CI doesn't exercise;
   - equivalent mutants left alive, and any tool that was missing or couldn't
     run the code (`cargo mutants`, Miri);
   - the ids of follow-up entries logged, and of entries moved with their
     resolving commit sha.

For docs and other non-code edits, skip items 1 and 3 and the format / build
gate of item 2.

## Never fake green

Never make a check pass by weakening it: don't disable, skip, ignore, weaken or
delete tests, loosen config, or bypass hooks with `--no-verify`. Fix the actual
problem.

The one exception is a lint suppression (`#[allow]`, `// eslint-disable`,
`# noqa`) for a false positive you can demonstrate: write the reason next to it
(in Rust, `#[allow(lint, reason = "...")]`) and list it in the final report.

## Test new behavior

- Any new feature or behavior change ships with tests that exercise it. A
  feature isn't done when the code exists; it's done when tests prove it
  behaves as intended. A pure refactor with no behavior change needs no new
  tests, but the existing ones must still pass.
- Test the intended behavior, not the implementation. Assert on observable
  outcomes and the public contract so the test survives a refactor. Don't
  write tests that merely re-encode what the code happens to do.
- The test must be able to fail. If it would still pass against a broken or
  reverted implementation, it proves nothing. Sanity check: invert the core
  logic in your head; the test should go red. If it wouldn't, rewrite it.
- Cover more than the happy path: edge cases, boundaries, empty/zero/overflow
  inputs, and the error paths the feature introduces.
- Pick the right level (unit vs integration) for what's being verified. If a
  feature genuinely can't be tested at a reasonable level, say so and explain
  why; don't write a hollow test just to satisfy this rule.
- Every test asserts something meaningful: no assertion-free tests.
- "Test" means verification appropriate to what changed, not always a
  test-framework test:
  - Application code / logic: automated tests, per the rules above.
  - Build targets, CI steps, scripts, Dockerfiles, config (e.g. a new Makefile
    target): run it and confirm it actually does what it claims. Don't author
    a unit test for a thin wrapper like `build: cargo build`; running it once
    is enough. If CI doesn't exercise it, say so; don't edit CI unasked.
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

Commit each completed phase before moving to the next, yourself, without being
asked or reminded. A phase is one item of the plan when there is one;
otherwise the whole task is one phase, committed once at the end.

- Commit a phase only when it's in a coherent, working state; each commit
  should stand on its own.
- Commit on the branch that is checked out, master included. Never create a
  branch unless asked, and never push.
- Stage by path only the files you changed; never `git add -A`, `git add .` or
  `git commit -a`. The tree may hold my own uncommitted edits, which stay out
  of your commits: if a file you changed already had some, leave it unstaged
  and tell me.
- Before every commit, format then compile, and commit only once both pass
  clean. Restage any files the formatter changed and include them in the same
  commit.
- This gate stays incremental and narrow: format, compile, and run the tests
  this phase wrote or touched. No `make clean`, no full test suite, no replay
  of the CI jobs or their build matrices; those run once, under *Verify
  against CI*, when the task is done.
- **Format**: run the format command the repository's `CLAUDE.md` or the
  private file names, if any. Otherwise, if the project has a `make fmt`
  target, run it; it is the single entry point for all of the project's
  formatters. Otherwise use the project's own formatter (rustfmt/`cargo fmt`,
  clang-format, prettier, gofmt, black, or its `format` task). If no formatter
  is configured, skip this step silently.
- **Build**: run the build command the repository's `CLAUDE.md` or the private
  file names, if any; otherwise the project's normal incremental build of the
  default target (`cargo build`, `make`, its `build` task), in the environment
  the project prescribes (e.g. its builder container).
- If a pre-commit hook runs, let it. If the hook or any step above fails,
  don't commit: fix the cause and re-run until clean (see *Never fake green*).
- Commit messages describe what changed, never the workflow (see *Committed
  artifacts stand on their own*).

## Verify against CI

When you believe you're finished, verify against the project's CI before
stopping.

- Read `.github/workflows/*.yml` (and any scripts they call) and identify
  every check step: build, tests, lint, format, typecheck, etc.
- Cover every configuration CI builds, not just the default target. Code
  behind a feature flag, a target `#ifdef`, a 32-bit target or a different
  compiler can build on one configuration and break on another. That means
  every entry of the build matrices, every configuration a job derives from
  them (e.g. `<config>/tests`, a production config), every flavor switched on
  through env or flags (e.g. `CONFIG_BUILD_USE_CLANG=y`) and every extra
  target a job builds (e.g. `make stage1`), each with the lints CI runs
  against it.
- Replicate those checks locally by running the same commands the workflow
  runs, in the same order and with the same flags/env where feasible. You are
  not running CI itself; you are running the commands it would run.
- Never copy a CI step's `make clean`: a CI job starts from an empty
  container, but locally it wipes every configuration's build tree and the
  cache. Use a separate build directory where a fresh tree matters.
- Run only what is reproducible locally. Skip steps needing secrets,
  deployment, external services, or a specific runner OS, and list what you
  skipped rather than failing on it.
- If a check fails, fix the root cause and re-run the full set. Repeat until
  everything runnable locally is green.
- Bound the effort: if a check still fails after 3-4 genuine fix attempts,
  stop and report what fails, the exact error, and what you tried. Don't
  thrash or stack speculative changes.
- If there's no CI workflow, fall back to the project's own build / test /
  lint commands and the format / build steps above.

## Capturing follow-up work

- Keep follow-up work in `~/work/git/TODO.md`. Create it if it doesn't exist.
- Resolved entries move to `~/work/git/TODO-resolved.md`. Neither file is
  versioned, so a deletion is unrecoverable and unauditable: nothing ever
  leaves this pair of files.
- The files are shared across sessions and projects. Any entry you did not
  write in this session belongs to someone else: read it, never reword,
  reorder, merge, summarize or delete it.
- Exactly four edits are legal:
  - append a new entry to the end of `TODO.md`,
  - amend an entry you appended in this session,
  - add a note to an entry this session's work partly resolved (see
    *Resolving an entry*),
  - move one entry, the one this session's work resolved, to
    `TODO-resolved.md`.

  Anything else (rewriting either file, tidying, batch pruning, sorting) is a
  defect, even if the result looks better. `TODO-resolved.md` is append-only
  with no exceptions.
- Append by appending (`cat >> file`), never by rewriting the file, so a
  concurrent session's entries cannot be clobbered. Re-read `TODO.md`
  immediately before removing anything from it.

### While implementing

- If you discover a bug, a blocker, or a side-issue that belongs to separate
  work, don't fix it inline. Record it and carry on, so each piece of work
  stays focused and self-contained.
- Exception: if the discovery blocks finishing the current task, log it, stop
  and tell me. I decide whether it gets fixed inline; don't carry on along a
  broken path.

### Entry format

Each entry carries enough context to act on in a fresh session with no memory
of this one, in this shape:

```markdown
## [<id>] <Investigate|Improve|Implement>: <title>

- Where: <repository>, <file / function / area>
- Observed: <symptom, plus how to reproduce it if known>
- Do: <the task, framed as below>
- Why deferred: <what you were doing when you hit it>
```

- `<id>` is 4 random hex digits (`openssl rand -hex 2`) found in neither file;
  grep both before using it. It lets a later session refer to the entry
  unambiguously.
- `<repository>` is the name from `git remote get-url origin`. The files span
  every project, so a path alone is ambiguous.
- For a bug, use `Investigate`, and don't write "there's a bug, fix it." Frame
  `Do` as an investigation: "Investigate the root cause; evaluate options
  including refactoring and broader improvements; then implement the best
  solution."
- For a non-bug item, `Do` states the concrete outcome you want.

### Resolving an entry

Move an entry only when you resolved it yourself, in the same turn as the work
that resolved it, with the build green and the change committed. Then, in this
order:

1. Append to `TODO-resolved.md`: the entry's original text, unchanged and
   verbatim, followed by one line: the date, the resolving commit sha, and one
   sentence on what was done.
2. Re-read the tail of `TODO-resolved.md` and confirm the entry is there.
3. Only then remove that one entry from `TODO.md`.

Appending before deleting means an interruption leaves a duplicate, which is
recoverable, rather than a hole, which is not.

- Report the move: the id and the resolving commit sha.
- "Appears fixed" or "no longer reproduces" is not resolution. If an entry
  looks obsolete or already handled and you did not resolve it yourself, say
  so and leave it in `TODO.md`. I decide.
- Never move an entry as a side effect of doing something else.
- If you resolved only part of an entry, leave it in place and add a
  `- Done:` and a `- Still open:` line to it, naming what this session did and
  what remains.

---

# Code conventions (all languages)

## Committed artifacts stand on their own

Code, comments, doc comments, docs and commit messages describe the code as it
is, for a reader who never saw how it was built.

- No words relative to the code's history: "legacy", "old", "new", "modern",
  "improved", "v2". They're true today and wrong tomorrow; name things by what
  they are or do. A `new()` constructor and names for runtime state
  (`current_slot`) are fine: they say nothing about the code's history.
- No trace of the implementation sequence: no "Phase 1/2", "Step 3", "in this
  iteration", "for now", "as a first pass", "later we'll", "temporary until
  <our next step>". A workaround tied to an outside condition is different:
  say so, with the link or version that ends it ("until upstream issue #123
  ships").
- No narration of the change in comments: "now using X", "refactored to...",
  references to the task or the caller that prompted it.
- No reference to working documents. A working document is a spec, plan or
  design note (SPEC.md, PLAN.md, ...) that I hand you as the source for an
  implementation and that git does not track. Implement from it, but never
  cite, link or mention it in anything committed. Files you create, and build
  outputs, are not working documents even before they're committed.

## Comments explain why, not what

Well-named identifiers already say what the code does, so write a comment only
when it carries knowledge the code can't: hidden constraints, subtle
invariants, why an unusual or seemingly-wrong approach is correct, workarounds
(with the reason, ideally a link or issue), safety or ordering requirements,
units and edge cases a caller can't infer.

- Don't restate the code (`// call reset` above `reset()`) or mark structure
  ("// helpers below"). Zero comments on self-explanatory code is correct.
- If a comment exists to explain an unclear name or a confusing block, rename
  or refactor instead, then drop the comment.
- This applies to the comments you write and the lines you change; leave
  existing comments elsewhere alone.
- Doc comments (`///`, docstrings, JSDoc) on public API are exempt: this rule
  targets explanatory inline comments, not API documentation.

## String literals: ASCII only

- String and character literals contain ASCII only, in any language. This
  covers smart punctuation (em/en dashes, curly quotes, ellipsis, non-breaking
  space), arrows, and every other non-ASCII glyph. Comments, doc comments, and
  prose are exempt from this rule.
- Use ASCII equivalents: `-` / `--` dashes, `'` and `"` quotes, `...`
  ellipsis, `->` `<-` `=>` arrows, a normal space for NBSP.
- Exception: when a literal genuinely must carry a Unicode character (real
  user-facing UTF-8 text), encode it with an explicit escape so the intent is
  visible, `"\u{2192}"` (Rust) or `"\u2192"` (Python/JS), never a pasted
  glyph.

## Types are the primary verification mechanism

Prefer a guarantee the compiler enforces over a runtime check, and a runtime
check over a comment or a convention. If a class of bug can be made
unrepresentable in the types, do that instead of defending against it.

- **Make illegal states unrepresentable.** Model the domain with sum types
  (Rust enums) so invalid combinations don't type-check. Don't use a struct of
  independent `bool`s / `Option`s whose valid combinations are a subset of the
  possible ones; use one enum with data in each variant instead.
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
  is itself the finding: say so and propose a tighter type.
- **State where the guarantees come from.** When you finish a non-trivial
  change in a typed language, say which invariants the types enforce, which
  are enforced at runtime, and which rest only on convention. The last group
  is the list of things to fix or test.
- **Prefer strengthening a type over adding a check.** If you catch yourself
  writing a defensive `assert!`, an early-return guard, or a comment saying
  "caller must ensure X", first ask whether a type could make X impossible.
  Note the choice if you decide not to.

---

# Rust

## Keep side effects out of assertion / error macros

- Arguments to control-flow and error macros (`ensure!`, `bail!`, `report!`,
  `assert!`, `debug_assert!` and similar) must not have side effects. The
  test: if moving a call out of the macro would change whether or when a side
  effect happens, it belongs outside. Pure calls stay inside, as in
  `ensure!(buf.len() == 32, ...)` or, in a test,
  `assert_eq!(decode(encode(x)), x)`.
- Evaluate a side-effecting call into a `let` binding on the line before, then
  pass the binding. The macro's job is to test an already-computed value
  (`ensure!(rc == 0, ...)`), never to run the work.
- This covers every argument: the condition, the error/context, and any format
  arguments. `debug_assert!` and its variants are compiled out of release
  builds, so a side effect inside them disappears in production. The
  error/context of `ensure!` / `report!` is evaluated only on the failure
  path, so a call there silently doesn't run on success. Some macros evaluate
  an argument more than once, and a call buried in a macro hides evaluation
  order from the call site.

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

- When a module gets its first unit tests, they go in a dedicated sibling
  file, not inline. In the source file, declare the module with
  `#[cfg(test)] mod tests;` (a bare declaration, no inline `{ ... }` block).
- A module that already has inline tests (`#[cfg(test)] mod tests { ... }`)
  keeps them inline: add further tests to that block, and never move them out
  to a sibling file.
- Put the sibling file in the module's directory: `foo/tests.rs` for
  `foo.rs`, or `src/tests.rs` for the crate root (`lib.rs` / `main.rs`).
  Start the file with `use super::*;` so tests keep access to the parent
  module's private items.
- Creating `foo/tests.rs` doesn't require renaming `foo.rs` to `foo/mod.rs`:
  the submodule file coexists with `foo.rs`.

## Assertion hygiene

- Prefer `assert_eq!` / `assert_ne!` over bare `assert!(a == b)`: the former
  print both operands on failure, the latter prints nothing useful.
- Avoid `#[should_panic]`. It asserts only that a panic happened, not which one
  or from where, so an unrelated panic makes it pass green. This follows from
  *Total functions*: a fallible operation returns `Result`, so test it by
  asserting on the `Err` variant, not by catching a panic.
- Snapshot tests (e.g. `insta`) are a last resort, not a default. They are
  trivial to "fix" by blessing whatever the code emits, which quietly locks in
  bugs: the opposite of a test that can fail for a real reason. Use them only
  for genuinely stable serialized output, and review a changed snapshot as
  carefully as changed code, never rubber-stamp it.

## Property tests

Use `proptest` for behavior that must hold across a whole input space, rather
than enumerating cases by hand. This is the tool for the "ensure invariant"
half of testing; hand-picked cases are for specific known-tricky inputs.

- Good targets: round-trips (`decode(encode(x)) == x`), idempotence
  (`f(f(x)) == f(x)`), ordering/monotonicity, conservation (counts/sums
  preserved), and agreement with a simple reference implementation.
- State the property as a comment (the why), then let proptest search. On
  failure it shrinks to a minimal counterexample; add that counterexample as
  its own named regression test so the specific bug stays pinned even if the
  generator changes. proptest also persists failures under a
  `.proptest-regressions` file; keep it in version control.
- Don't reach for a property test where there is no real invariant: a
  generator that just re-implements the function under test proves nothing.
  When in doubt, one property plus a couple of concrete edge cases beats a
  large opaque strategy.

## Mutation testing

`cargo-mutants` introduces small faults (flipped comparisons, swapped match
arms, replaced return values) and reports which ones the suite failed to
catch. A surviving mutant is a real gap that quantity of tests cannot hide.

- Run it before declaring done on any change to crypto, parsing or
  serialization code, scoped to the change: write the change's diff to a file
  (`git diff <base>`) and run `cargo mutants --in-diff <file>`. If
  `cargo mutants` is not installed, say so in the final report rather than
  skipping it silently. On other code, run it when I ask.
- Treat each surviving mutant as a missing or too-weak assertion and
  strengthen the test until it kills the mutant. Don't kill a mutant by
  narrowing the mutated code to satisfy the tool: fix the test, not the
  target.
- Use it as a diagnostic to find weak spots, not as a coverage-style
  percentage to chase to 100%. Some mutants are equivalent (no observable
  difference) and legitimately unkillable: list those in the final report
  rather than contorting tests around them.

## Undefined behavior (unsafe)

- Default to no `unsafe`. If a safe formulation exists, use it; the type-system
  guarantees above only hold on safe code.
- If `unsafe` is genuinely unavoidable, every block carries a `// SAFETY:`
  comment stating the invariant that makes it sound (this is a why comment, so
  the comment rules allow it), and that invariant is upheld by construction
  wherever possible.
- Run the affected tests under Miri, which interprets the code and detects
  undefined behavior ordinary tests pass over (out-of-bounds access,
  use-after-free, invalid aliasing, unaligned reads):
  `cargo +nightly miri test`. Fix any UB Miri reports at the root; never
  silence it. Miri isn't needed for fully safe crates. If it can't run the
  code (FFI, inline asm, target-only code), say so in the final report.

---

@~/.claude/CLAUDE.private.md
