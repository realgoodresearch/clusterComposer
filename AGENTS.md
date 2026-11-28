# Repository workflow

This file is the working agreement for people and agents contributing to
`clusterComposer`. Keep it current when the team adopts or changes a material
workflow, engineering, testing, or release rule.

## Repository identity and purpose

- The canonical GitHub repository for this working tree is
  `realgoodresearch/clusterComposer`, represented by the `origin` remote.
- `cadooley/clusterComposer` is the upstream repository. Do not create or
  modify its issues, comments, pull requests, releases, or other GitHub state
  unless the user explicitly requests work there.
- Work in this fork is intended for contribution to the upstream package.
  Prioritize backward compatibility so changes remain suitable for upstream
  review and existing users.
- Before a GitHub write, verify the destination against `origin` and pass an
  explicit repository argument such as `--repo realgoodresearch/clusterComposer`.
- This R package generates bespoke spatial sampling frames from building
  footprints or buffered building locations. It groups buildings into spatial
  sampling units for survey design and census-frame updates.

## Communication and authorization

- Keep explanations and status reports concise unless the user requests detail.
- Treat a user message ending in a question mark as a request for an answer,
  not authorization to inspect files, run commands, edit code, or start a run.
  Follow an action only when the question also contains an explicit command.
- Do not expand a review or diagnostic request into an implementation without
  authorization.
- State assumptions, evidence, tradeoffs, and blockers plainly. Ask before a
  choice that materially changes public behavior, compatibility, data
  interpretation, outputs, dependencies, or runtime characteristics.
- Before an expensive spatial run, state the inputs, parameters, output path,
  overwrite behavior, core count, expected duration, and stopping behavior,
  then wait for explicit approval.

## Issue-driven Git workflow

1. Start substantive work with an issue that states the problem, desired
   outcome, scope, acceptance criteria, and important non-goals.
2. Create an issue-scoped branch from `main`, named
   `issue-<number>-<short-description>`.
3. Keep the issue current with very brief comments at important planning and
   verified implementation milestones. Record enough decisions, validation,
   and next steps that another contributor can resume without extra prompting.
   Keep comments scoped to development work in this repository. Include
   downstream project evidence only when it is directly relevant diagnostic
   information; omit that project's operational plans and next steps. Format
   comments with actual Markdown line breaks, never literal escape sequences
   such as `\n`, and verify that comments are readable after writing them.
4. Keep changes focused on one coherent outcome. Do not mix behavior changes
   with unrelated cleanup, refactoring, formatting, or generated artifacts.
5. Commit at meaningful, verified milestones with concise imperative subjects.
   Before each commit, summarize the proposed commit, current diff, and
   validation, then wait for user review and explicit approval unless the user
   has already explicitly requested that commit.
6. After a user-approved commit, push the issue branch to its configured
   `origin` upstream so collaborators can see it. Confirm first if the branch
   has no upstream or the destination is ambiguous.
7. When ready, open a pull request into `main`, link the issue, summarize the
   behavior and validation, and request review. Do not merge, close issues,
   delete branches, force-push, or rewrite shared history without explicit
   authorization.

Never disclose credentials, tokens, personal data, sensitive humanitarian
data, or private source records in issues, commits, pull requests, logs, or
chat summaries.

## Working-tree and dependency safety

- Check `git status --short --branch` before edits, commits, branch switches,
  and other operations that could affect existing work.
- Preserve unrelated user changes. Never alter, discard, stage, or commit them
  without explicit authorization.
- Do not silently install or upgrade R packages or system libraries. Explain
  the requirement and obtain approval before changing the environment.
- Do not commit user-specific paths, credentials, large run outputs, temporary
  section files, logs, or unapproved geospatial data.
- Use `tempdir()` or isolated test fixtures for generated test artifacts. Do
  not overwrite the tracked GeoPackages under `inst/extdata/`.

## Package structure and generated files

- `R/clusterComposer.R` contains the exported implementation and roxygen
  documentation.
- `man/clusterComposer.Rd` and `NAMESPACE` are generated from roxygen comments.
  Update them with the approved documentation tool rather than hand-editing
  generated content.
- `README.Rmd` is the source for `README.md`. Keep both synchronized when the
  user-facing interface or examples change.
- `vignettes/` contains simple and full workflow documentation. The full
  workflow is expensive; do not execute it merely to validate a narrow change.
- `inst/extdata/` contains small tracked example inputs and reference outputs.
  Treat reference-output changes as behavior changes and review them explicitly.
- `DESCRIPTION` declares package dependencies and version metadata. Add a
  dependency only when justified and approved.

## R and spatial-programming conventions

- Follow the existing package style unless a focused refactor is part of the
  approved scope. Prefer clear names and small helpers for new behavior.
- Preserve backward-compatible APIs, defaults, output schemas, file naming, and
  behavior. A breaking change requires an explicitly agreed version upgrade,
  migration guidance, and appropriate compatibility tests.
- Validate new arguments early and fail with actionable messages.
- Keep CRS transformations explicit and use a suitable projected CRS for
  distance and area operations. Validate geometries after operations that can
  create empty, invalid, collection, or multipart results.
- Validate required layers, columns, geometry types, population values, and
  paths before expensive processing begins.
- Size parallel workloads conservatively. Ensure core counts are finite
  positive integers, never assume all detected cores are available, and always
  stop worker clusters on exit.
- Make per-section work deterministic where practical. Preserve section-level
  errors and completed outputs rather than silently discarding them.
- Use portable paths with `file.path()` and never build paths by concatenating
  separators manually.

## Validation

- Run the narrowest meaningful check while editing, then broaden validation in
  proportion to risk.
- Syntax-parse edited R files and exercise new helpers with small deterministic
  fixtures.
- Add automated tests for new behavior and regressions. This repository does
  not yet have an established test suite, so introduce testing infrastructure
  deliberately and keep fixtures small.
- When exported arguments or roxygen comments change, regenerate documentation
  and verify `NAMESPACE`, the Rd file, README examples, and vignettes remain
  consistent.
- Before requesting package-level review, run the relevant automated tests and
  an R package check when dependencies and runtime permit. Report the exact
  commands, results, warnings, and checks not run.
- Do not claim success because a long spatial process merely completed. Check
  coverage, feature counts, population conservation, geometry validity,
  section failures, and output paths as appropriate.

## Long-running and operational work

- Prefer small representative fixtures before full spatial runs.
- Design long-running work to produce durable status and intermediate outputs
  that can be inspected, resumed, and safely stopped.
- When a process is running, report that state and stop polling. Ask the user to
  notify the agent when it finishes. If the user explicitly requests
  monitoring, poll at a low and progressively decreasing frequency and report
  only completion, failure, or a condition requiring attention.
- Do not stop a process merely to inspect it. Verify its actual state before
  reporting that it is active or complete.
- Keep temporary outputs by default during development and failure diagnosis.
  Delete them only when the user requests cleanup or the run configuration
  explicitly calls for it.

## Maintaining this file

Agents are responsible for updating this file when user feedback establishes a
new workflow preference or repository rule. Do not wait for a separate request
to preserve an agreed practice.
