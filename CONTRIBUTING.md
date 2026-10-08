# Contributing to ARTIS

Thank you for contributing to the ARTIS model package. This document describes the development workflow, issue conventions, and branch/PR practices used in this project.

## Development Workflow Components

### Definitions

There are a few key components to our git and GitHub software development workflow for ARTIS. They are defined below as they relate to ARTIS development: 

- Issues
- Pull Requests
- Branches
- Projects
- Milestones
- GitHub Actions
- Releases

### Issue Types

ARTIS uses two tiers of issues types to organize work:

- **Theme/Epic issues** represent a cohesive body of work (e.g., a major refactor, new feature, or data pipeline update). Each epic maps to a feature branch and a single pull request.

- **Sub-issues** represent discrete tasks within an epic. They are created as child issues under the relevant epic and tracked individually on the project board.

### Issue Status Categories

All issues are tracked in the GitHub Project board using the following statuses:

| Status | Meaning |
|---|---|
| Backlog | Captured but not yet prioritized or scheduled |
| Ready | Prioritized and queued for active development |
| In Progress | Actively being worked on |
| Needs Review | Work is complete and under review (epics: PR is open for review) |
| Stalled | Work has paused; uncertain if or when it will resume |
| Done | Work is complete — triggers automatic issue closure |


### Theme/Epic Lifecycle

Epic issues follow this automated lifecycle:

1. Open a draft PR linked to the epic with `Closes #<issue-number>` in the PR body — set epic status to **In Progress** manually
2. When the draft PR is marked ready for review, the epic status automatically updates to **Needs Review**
3. When the PR is merged into `develop`, the epic issue automatically closes via the `Closes #` reference

### Sub-issue Lifecycle

Sub-issues progress through statuses as work advances on the parent epic branch. When a sub-issue is finished, set its status to **Done** — a GitHub Actions workflow will automatically close the issue. This drives the sub-issue progress bar on the parent epic.

Do not close sub-issues manually unless correcting an error.


## Branch Workflow

ARTIS uses a Gitflow-style branching model with `develop` as the default branch.

| Branch | Purpose |
|---|---|
| `main` | Stable releases only — merged from `develop` at versioned release points |
| `develop` | Integration branch — all feature work targets this branch |
| `develop-<epic-issue-number>-<feature/bug-name>` | Epic/feature branches — branched from and merged back into `develop` |

### Branch Naming

Feature branches should be named `develop-<epic-issue-number>-<feature/bug-name>` to correspond with the Theme/epic issue they address e.g. 
`develop-34-hs-code-refactor`.

### Branch Workflow 

```mermaid
gitGraph
   commit tag: "v1.1.0"
   branch develop
   checkout develop
   commit id: " "
   branch develop-feature-a
   commit id: "added x"
   commit id: "fixed y"
   checkout develop
   branch develop-feature-b
   commit id: "cleaned z"
   commit id: "updated w"
   checkout develop-feature-a
   commit id: "documentation"
   checkout develop
   merge develop-feature-a id: "merge reabsed feature-a"
   checkout develop-feature-b
   commit id: "document"
   merge develop id: "bring in develop updates"
   commit id: "added v"
   checkout develop
   merge develop-feature-b id: "merge reabsed feature-b"
   checkout develop
   branch develop-bug-fix
   commit id: "bug-fix"
   checkout develop
   merge develop-bug-fix id: "merge reabsed bug fix"
   checkout main
   merge develop id: "Merge to release v2.0" tag: "v2.0" type: NORMAL 
   checkout develop
   merge main id: "long-lived develop branch"
```


## The Workflow

> [!IMPORTANT] 
> Theme/Epic parent issues, feature branches, and PRs should correspond directly with eachother. 

1) Create a new feature branch based on the Theme/Epic issue

    ```bash
    git checkout develop
    git pull
    git checkout -b develop-<epic-issue-number>-<feature/bug-name>
    ```

2) Open a **draft PR** targeting `develop`. This signals active work and starts the epic's lifecycle on the project board. 
    - Link the Theme/Epic issue in the PR metadata right-hand-side column under Development.
    - Include a closing reference in the PR body to link the PR to the epic issue and enable auto-close on merge:

    ```
    Closes #<issue-number>
    ```

    > [!NOTE] 
    > You will need to make a change on the feature branch and push to origin in order to open a draft PR. Try adding a new section to the `CHANGELOG.md` about the work. 

3) Get to work on sub-issues on the feature branch `develop-<epic-issue-number>-<feature/bug-name>`. 

4) When a sub-issue is complete, reference the issue number in the final commit message. 
    - For a full list of linking keywords see: https://docs.github.com/en/issues/tracking-your-work-with-issues/using-issues/linking-a-pull-request-to-an-issue#linking-a-pull-request-to-an-issue-using-a-keyword


    ```
    Resolves #43 cleaned up function internal object naming and references. updated Roxygen2 header. 
    ```

5) Manaually change the status of the complete sub-issue on the GitHub UI to `Done` in the right-hand-side metadata column. 

    > [!Note]
    > This status change event triggers the `close-issue-on-done.yml` GitHub Action 

## Prepare for Merging Work into `develop`

1) Change Pull Request (PR) state from "Draft" to "Open" on GitHub UI at the bottom of the PR body.

2) Change PR status from "in Progress" to "Needs Review" on GitHub UI on the right-hand-side metadata column.

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

    If there were not merge conflicts: 

    ```bash
    git push origin develop-<feature-name> 
    ```

    Or if there were merge conflicts: 

    ```bash
    git push origin develop-<feature-name> --force-with-lease
    ```

## Pull Requests

### PR Scope

Each PR corresponds to one epic issue and one feature branch. Sub-issue work accumulates on the epic branch — sub-issues do not have their own PRs.


### Merging

PRs are "merged with commit" into `develop` after:

- All sub-issues are marked Done
- `devtools::check()` passes with 0 ERRORS and 0 WARNINGS
- At least one reviewer has approved

### Stable Releases

Stable releases are cut by merging `develop` into `main` and tagging a version. This is a periodic, intentional step separate from day-to-day PR merges.

## Automated Workflows

Two GitHub Actions workflows support this development process:

**Close issue on Done status** (`.github/workflows/close-issue-on-done.yml`)
Triggered when a project item's Status field changes to "Done". Automatically closes the linked issue with state reason `completed`. Requires a `PROJECT_TOKEN` secret with Projects and Issues write permissions.

**Update epic status on PR ready** (`.github/workflows/update-epic-status-on-review.yml`)
Triggered when a draft PR is marked ready for review. Automatically sets the linked epic issue's project status to "Needs Review". Requires the same `PROJECT_TOKEN` secret.

## Code Style

This project follows Tidyverse style conventions. See the project `AGENTS.md` for full coding and syntax guidelines including pipe operator usage, file I/O conventions, and CLI messaging patterns.
