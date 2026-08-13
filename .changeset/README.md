# Changesets

This project uses [Changesets](https://github.com/changesets/changesets) to collect release notes and bump the app version. Docker / AWS deploy on `dev` and `main` is unchanged — Changesets only updates `package.json`, `DESCRIPTION`, and `NEWS.md`.

## Versioning: CalVer continuous releases

Format: **`YYYY.MM.N`** (not semver).

| Piece | Meaning |
|-------|---------|
| `YYYY.MM` | **Series** — calendar month of a dated release line |
| `N` | **Build** — counter within that series (`0`, `1`, `2`, …) |

This is closer to [CalVer](https://calver.org/) + a continuous-delivery counter than to semantic versioning. We ship often; the version tells you *when the series started* and *which build*, not API compatibility.

| Intent | Changeset bump | Version effect |
|--------|----------------|----------------|
| Normal ship (fix or feature) | `patch` *(preferred)* or `minor` | `YYYY.MM.N` → `YYYY.MM.(N+1)` |
| New dated series (milestone / notable drop) | `major` | `{Auckland release month}.0` |

Examples (from `2026.05.3`):

- all `patch` / `minor` → `2026.05.4`
- any `major` in August 2026 → `2026.08.0`
- `major` while already on `2026.08.*` → `2026.08.(N+1)` (never downgrade)

Release month uses **Pacific/Auckland**. If several changesets are pending, the highest bump wins (`major` > `patch`/`minor`).

Prefer **`patch`** for day-to-day work. Use **`major`** when you want the public version to move to a new month (e.g. after a cluster of infra/user-facing work you’re calling a dated release). `minor` is accepted as an alias of `patch` because Changesets requires one of the three names.

## Add a changeset (on a feature branch / PR)

```bash
npm run changeset
```

Or create a file under `.changeset/` manually:

```md
---
"inzight-lite": patch
---

## Fixes

Short user-facing description of the change.
```

### Section headings

Start the body with a `## Section` heading so notes land in the right `NEWS.md` group. Common sections:

- `## Fixes`
- `## Infrastructure`
- `## New features`
- `## UI Changes`
- `## Changes`

If you omit the heading, the note goes under `## Changes`.

## Apply changesets (version PR / local dry-run)

```bash
npm run version
```

This reads pending changesets, bumps the version, prepends `NEWS.md`, syncs `DESCRIPTION`, and deletes the consumed changeset files.
