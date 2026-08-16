---
"inzight-lite": patch
---

## Infrastructure

Rebuild Traefik v3.7.10 with golang.org/x/mod v0.40.0 so the image no longer ships CVE-2026-56864 / CVE-2026-56865 (ECR critical).
