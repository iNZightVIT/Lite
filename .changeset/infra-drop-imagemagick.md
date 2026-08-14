---
"inzight-lite": patch
---

## Infrastructure

Drop unused ImageMagick from the app image: ignore the `magick` Suggest and purge `libmagick*` after install (still pulled as a `webshot` sysreq). Ubuntu only ships those CVE fixes via Pro/ESM.
