# Contributing to ARTIS

Thank you for contributing to the ARTIS model package. This document describes the git an GitHub software development workflow and conventions for the `artis` R package repository. 


## Table of Contents 

- [🧱 GitHub Workflow Components](#github-workflow-components)
  - [Definitions](#definitions)
  - [Details: Issues](#details-issues)
    - [Theme/Epic Lifecycle](#themeepic-lifecycle)
    - [Sub-issue Lifecycle](#sub-issue-lifecycle)
  - [Details: Issue/PR Status Categories](#details-issuepr-status-categories)
  - [Details: Branches](#details-branches)
    - [Branch Naming](#branch-naming)
    - [Branch Workflow Diagram](#branch-workflow-diagram)
- [🍉 The Workflow](#the-workflow)
- [📋 Prepare for Merging Work into `develop`](#prepare-for-merging-work-into-develop)
- [Rebasing Before Merge](#rebasing-before-merge)
- [Pull Requests](#pull-requests)
  - [PR Scope](#pr-scope)
  - [Merging](#merging)
  - [Stable Releases](#stable-releases)
- [🤖 Automated Workflows](#automated-workflows)
- [💃 Code Style](#code-style)

## GitHub Workflow Components 🧱

### Definitions

There are a few key components to our git and GitHub software development workflow for ARTIS. They are defined below as they relate to ARTIS development:

- **Issues** — GitHub's built-in tool for tracking bugs, feature requests, tasks, and discussions within a repository. Issues can be assigned, labeled, and linked to PRs and project boards. In ARTIS, issues are structured in two tiers: 
    - `Theme/Epic` parent issues for large bodies of work, and 
    - "sub-issues" for discrete tasks within a theme/epic.

- **Branches** — An isolated copy of the codebase where changes can be made without affecting other branches. Branches allow multiple lines of work to proceed in parallel and are merged back together when complete. In ARTIS, a Gitflow-style model is used with:
    - The `main` branch for stable releases,
    - the `develop` branch as the integration branch, and 
    - short-lived "feature branches" for each theme/epic.

- **Pull Requests (PRs)** — A GitHub mechanism for proposing that changes on a feature branch be merged into another branch. PRs provide a dedicated space for code review, discussion, checklist tracking, and linking related issues before changes are integrated. In ARTIS, each PR corresponds to one theme/epic issue and targets `develop`.

- **Projects** — A GitHub project board for organizing and tracking work across issues and PRs using a customizable status-based workflow. In ARTIS, all issues move through statuses (Backlog → Ready → In Progress → Needs Review → Done) on the project board, with some transitions automated via GitHub Actions.

- **Milestones** — A GitHub feature for grouping issues and PRs into a named target, typically tied to a version or release goal. Milestones display a completion percentage as issues are closed. In ARTIS, milestones correspond to versioned releases (e.g., `3.0`).

- **Releases** — A GitHub feature for publishing a named, versioned snapshot of a repository at a specific commit, typically accompanied by release notes. In ARTIS, releases are tagged on `main` using simplified semantic versioning (e.g., `v3.0`) and represent stable, production-ready states of the codebase.

- **GitHub Actions** — A CI/CD and automation platform built into GitHub that runs workflows in response to repository events such as pushing code or updating a PR. In ARTIS, Actions automate issue closure when work is marked Done and update theme/epic statuses when draft PRs are marked ready for review.

### Details: Issues

ARTIS uses two tiers of issue types to organize work:

- **Theme/Epic issues** represent a cohesive body of work (e.g., a major refactor, new feature, or data pipeline update). Each theme/epic maps to a feature branch and a single pull request.

- **Sub-issues** represent discrete tasks within an epic. They are created as child issues under the relevant theme/epic and tracked individually on the project board.

#### Theme/Epic Lifecycle

Theme/Epic issues follow this automated lifecycle:

1. Open a draft PR linked to the theme/epic with `Closes #<issue-number>` in the PR body — set theme/epic status to **In Progress** manually
2. When the draft PR is marked ready for review, the theme/epic status automatically updates to **Needs Review**
3. When the PR is merged into `develop`, the theme/epic issue automatically closes via the `Closes #` reference

#### Sub-issue Lifecycle

Sub-issues progress through statuses as work advances on the parent theme/epic branch. When a sub-issue is finished, set its status to **Done** — a GitHub Actions workflow will automatically close the issue. This drives the sub-issue progress bar on the parent theme/epic.

Do not close sub-issues manually unless correcting an error.

### Details: Issue/PR Status Categories

All issues/PRs are tracked in the GitHub Project board using the following statuses:

| Status | Meaning |
|---|---|
| Backlog | Captured but not yet prioritized or scheduled |
| Ready | Prioritized and queued for active development |
| In Progress | Actively being worked on |
| Needs Review | Work is complete and under review (theme/epics: PR is open for review) |
| Stalled | Work has paused; uncertain if or when it will resume |
| Done | Work is complete — triggers automatic issue closure |

### Details: Branches

ARTIS uses a Gitflow-style branching model with `develop` as the default branch.

| Branch | Purpose |
|---|---|
| `main` | Stable releases only — merged from `develop` at versioned release points |
| `develop` | Integration branch — all feature work targets this branch |
| `develop-<theme-epic-issue-number>-<feature/bug-name>` | Theme/Epic feature branches — branched from and merged back into `develop` |

#### Branch Naming

Feature branches should be named `develop-<theme-epic-issue-number>-<feature/bug-name>` to correspond with the Theme/Epic issue they address, e.g. `develop-34-hs-code-refactor`.

#### Branch Workflow Diagram

```mermaid
gitGraph
   commit tag: "v1.1.0"
   branch develop
   checkout develop
   commit id: " "
   branch develop-34-hs-code-refactor
   commit id: "restructure hs codes"
   commit id: "fix lookup table"
   checkout develop
   branch develop-41-data-pipeline
   commit id: "clean raw data"
   commit id: "update pipeline steps"
   checkout develop-34-hs-code-refactor
   commit id: "add documentation"
   checkout develop
   merge develop-34-hs-code-refactor id: "merge rebased #34"
   checkout develop-41-data-pipeline
   commit id: "update docs"
   merge develop id: "rebase onto develop"
   commit id: "finalize pipeline"
   checkout develop
   merge develop-41-data-pipeline id: "merge rebased #41"
   branch develop-55-trade-weight-bug
   commit id: "fix trade weight bug"
   checkout develop
   merge develop-55-trade-weight-bug id: "merge rebased #55"
   checkout main
   merge develop id: "Release v2.0.0" tag: "v2.0.0" type: NORMAL
   checkout develop
   commit id: "continue development"
```


## The Workflow 🍉

> [!IMPORTANT] 
> Theme/Epic parent issues, feature branches, and PRs should correspond directly with each other. 

1) Create a new feature branch based on the Theme/Epic issue

    ```bash
    git checkout develop
    git pull
    git checkout -b develop-<theme-epic-issue-number>-<feature/bug-name>
    ```

2) Open a **draft PR**: `base: develop | compare: develop-<num>-<feature> `. This signals active work and starts the theme/epic's lifecycle on the project board. 
    a) Link the Theme/Epic issue in the PR metadata right-hand-side column under Development.
    b) Include a closing reference in the PR body to link the PR to the theme/epic issue and enable auto-close on merge:

    ```
    Closes #<issue-number>
    ```

    > [!NOTE] 
    > You will need to make a change on the feature branch and push to origin in order to open a draft PR. Try adding a new section to the `CHANGELOG.md` about the work. 

3) Get to work on sub-issues on the feature branch `develop-<theme-epic-issue-number>-<feature/bug-name>`. 

4) When a sub-issue is complete, reference the issue number in the final commit message. 
    - For a full list of linking keywords see the related [GitHub Docs](https://docs.github.com/en/issues/tracking-your-work-with-issues/using-issues/linking-a-pull-request-to-an-issue#linking-a-pull-request-to-an-issue-using-a-keyword)


    ```
    Resolves #43 cleaned up function internal object naming and references. updated Roxygen2 header. 
    ```

5) Manually change the status of the complete sub-issue on the GitHub UI to `Done` in the right-hand-side metadata column. 

    > [!NOTE]
    > This status change event triggers the `close-issue-on-done.yml` GitHub Action 


## Prepare for Merging Work into `develop` 📋

1) Change Pull Request (PR) state from "Draft" to "Open" on GitHub UI at the bottom of the PR body.

2) Change PR status from "In Progress" to "Needs Review" on GitHub UI on the right-hand-side metadata column.

3) Work through checklists in the PR body (populated by a template) that include describing the work done, testing, documentation checks, and `git rebase` instructions to prep the branch for merging (also described below). 

4) Assign a reviewer (if applicable) and notify them directly outside of GitHub about the pending code review. 

## Rebasing Before Merge

1) Before merging, rebase the feature branch onto `develop` to maintain a clean linear history:

    ```bash
    git checkout develop
    git pull
    git checkout develop-<feature-name>
    git rebase develop
    ```

2) Resolve any conflicts

3) Re-run `devtools::check()` and address any errors and warnings

4) Push to origin

    If there were no merge conflicts: 

    ```bash
    git push origin develop-<feature-name> 
    ```

    Or if there were merge conflicts: 

    ```bash
    git push origin develop-<feature-name> --force-with-lease
    ```

## Pull Requests

### PR Scope

Each PR corresponds to one theme/epic issue and one feature branch. Sub-issue work accumulates on the theme/epic branch — sub-issues do not have their own PRs.


### Merging

PRs are "merged with commit" into `develop` after:

- All sub-issues are marked Done
- `devtools::check()` passes with 0 ERRORS and 0 WARNINGS
- At least one reviewer has approved

### Stable Releases

Stable releases are cut by merging `develop` into `main` and tagging a version. This is a periodic, intentional step separate from day-to-day PR merges.

## Automated Workflows 🤖 

Two GitHub Actions workflows support this development process:

**Close issue on Done status** (`.github/workflows/close-issue-on-done.yml`)
Triggered when a project item's Status field changes to "Done". Automatically closes the linked issue with state reason `completed`. Requires a `PROJECT_TOKEN` secret with Projects and Issues write permissions.

**Update theme/epic status on PR ready** (`.github/workflows/update-epic-status-on-review.yml`)
Triggered when a draft PR is marked ready for review. Automatically sets the linked theme/epic issue's project status to "Needs Review". Requires the same `PROJECT_TOKEN` secret.

## Code Style 💃

This project follows Tidyverse style conventions. See the project `AGENTS.md` for full coding and syntax guidelines including pipe operator usage, file I/O conventions, and CLI messaging patterns.
