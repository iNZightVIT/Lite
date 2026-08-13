#!/usr/bin/env node
/**
 * Prepend pending Changesets to NEWS.md under "# Unreleased" without consuming
 * them or bumping the version. Used at AWS Docker build time on the `dev`
 * branch so preprod About → Change Log shows staged work.
 *
 * Does not modify DESCRIPTION, package.json, or .changeset/*.md.
 */

import fs from "node:fs";
import {
  UNRELEASED_TITLE,
  formatNewsBlock,
  newsPath,
  readPendingEntries,
  stripUnreleased,
} from "./changeset-lib.mjs";

function main() {
  const entries = readPendingEntries();
  const existingNews = stripUnreleased(
    fs.existsSync(newsPath) ? fs.readFileSync(newsPath, "utf8") : ""
  );

  if (entries.length === 0) {
    // Ensure a stale Unreleased block is not left behind if changesets were cleared
    if (fs.existsSync(newsPath)) {
      fs.writeFileSync(newsPath, existingNews, "utf8");
    }
    console.log("No pending changesets; NEWS.md has no Unreleased section.");
    process.exit(0);
  }

  const unreleased = formatNewsBlock(UNRELEASED_TITLE, entries);
  const previous = existingNews.startsWith("#")
    ? existingNews
    : existingNews.trimStart();
  fs.writeFileSync(
    newsPath,
    previous ? `${unreleased}\n${previous}` : unreleased,
    "utf8"
  );

  console.log(
    `Injected # ${UNRELEASED_TITLE} (${entries.length} changeset${entries.length === 1 ? "" : "s"}) into NEWS.md`
  );
  for (const entry of entries) {
    console.log(
      `  [${entry.section}] ${entry.note.slice(0, 80)}${entry.note.length > 80 ? "…" : ""}`
    );
  }
}

main();
