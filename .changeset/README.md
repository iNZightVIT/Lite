# Changesets

This project uses [Changesets](https://github.com/changesets/changesets) to collect release notes and bump the app version. Docker / AWS deploy on `dev` and `main` is unchanged — Changesets only updates `package.json`, `DESCRIPTION`, and `NEWS.md`.

## Versioning: CalVer continuous releases

Format: **`YYYY.MM`** for a new dated series, then **`YYYY.MM.NN`** for builds in that series (not semver).

| Form | Meaning |
|------|---------|
| `YYYY.MM` | First release of a dated series (major) |
| `YYYY.MM.NN` | Patches / follow-ups (`01`, `02`, …) |

This is closer to [CalVer](https://calver.org/) + a continuous-delivery counter than to semantic versioning. We ship often; the version tells you *when the series started* and *which build*, not API compatibility.

| Intent | Changeset bump | Version effect |
|--------|----------------|----------------|
| New dated series (milestone / notable drop) | `major` | `{Auckland release month}` e.g. `2026.08` |
| Normal ship (fix or feature) | `patch` *(preferred)* or `minor` | next build: `2026.08` → `2026.08.01` → `2026.08.02` |

Examples:

- `major` in August 2026 from `2026.05.3` → `2026.08`
- `patch` on `2026.08` → `2026.08.01`
- `patch` on `2026.08.01` → `2026.08.02`
- `patch` on `2026.05.3` → `2026.05.04`
- `major` while already on `2026.08` / `2026.08.*` → next `.NN` (never downgrade)

Release month uses **Pacific/Auckland**. If several changesets are pending, the highest bump wins (`major` > `patch`/`minor`).

Prefer **`patch`** for day-to-day work. Use **`major`** when you want the public version to move to a new month. `minor` is accepted as an alias of `patch` because Changesets requires one of the three names.

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

On every push to **`dev`**, [`.github/workflows/changesets.yaml`](../.github/workflows/changesets.yaml) runs `changesets/action` and opens or updates a **Version packages** PR into `dev`. Merging that PR bumps `package.json` / `DESCRIPTION`, rewrites `NEWS.md`, and removes the consumed changeset files. Your existing AWS workflow then deploys the versioned build to preprod.

Locally (or to preview):

```bash
npm run version
```

This reads pending changesets, bumps the version, prepends `NEWS.md`, syncs `DESCRIPTION`, and deletes the consumed changeset files.

**Repo setting:** under *Settings → Actions → General*, enable *Allow GitHub Actions to create and approve pull requests* so the version PR can be opened.

### Suggested flow

1. Feature branch / PR — add a changeset with the change
2. Merge into `dev` — AWS deploy injects `# Unreleased` into `NEWS.md` from pending changesets (build-time only, not committed) so About → Change Log on preprod shows staged work; Changesets opens/updates the version PR
3. Merge **Version packages** → `dev` — version + NEWS land; AWS deploys preprod for testing
4. Promote `dev` → `main` when ready — AWS deploys production (no Unreleased injection, no second version bump)

Preview Unreleased locally:

```bash
npm run unreleased
```
