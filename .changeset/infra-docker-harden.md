---
"inzight-lite": patch
---

## Infrastructure

Harden the Docker image against ECR vulnerability findings: pin R 4.2.3, apply apt security upgrades, upgrade Traefik from v3.0.0 to v3.7.10, remove unused `linux-libc-dev`, and bump `path-to-regexp` to 0.1.13 in the status collector (CVE-2026-4867).
