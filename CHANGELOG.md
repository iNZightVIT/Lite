# inzight-lite

## 2026.05.04

### Fixes

- Fix numeric filter R code quoting the threshold as a string (e.g. `Daily_Steps > "1"`) by coercing the text input to numeric before calling `filter_num`.

### Infrastructure

- Harden the Docker image against ECR vulnerability findings: pin R 4.2.3, apply apt security upgrades, upgrade Traefik from v3.0.0 to v3.7.10, remove unused `linux-libc-dev`, and bump `path-to-regexp` to 0.1.13 in the status collector (CVE-2026-4867).
- Drop unused ImageMagick from the app image: ignore the `magick` Suggest and purge `libmagick*` after install (still pulled as a `webshot` sysreq). Ubuntu only ships those CVE fixes via Pro/ESM.
- Build the app image from Ubuntu 24.04 and compile R 4.2.3 from source, instead of `rocker/r-ver:4.2.3` (Ubuntu 22.04), so OS packages can receive current security updates.
