---
"inzight-lite": patch
---

## Fixes

Fix numeric filter R code quoting the threshold as a string (e.g. `Daily_Steps > "1"`) by coercing the text input to numeric before calling `filter_num`.
