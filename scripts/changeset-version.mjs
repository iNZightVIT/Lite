#!/usr/bin/env node
/**
 * Apply pending Changesets for iNZight Lite (CalVer continuous releases):
 * 1. Read .changeset/*.md (bump type, section, notes)
 * 2. Bump package.json version:
 *      major       → {releaseDate:YYYY.MM}     (no .0; fallback to patch if same month)
 *      patch/minor → YYYY.MM.01, .02, …        (two-digit build)
 * 3. Prepend NEWS.md in Lite style; keep CHANGELOG.md for changesets/action
 * 4. Sync DESCRIPTION Version
 * 5. Delete consumed changeset files
 *
 * Does not publish to npm.
 */

import fs from "node:fs";
import {
  changelogPath,
  descriptionPath,
  formatChangelogBlock,
  formatNewsBlock,
  highestBump,
  newsPath,
  nextVersion,
  packageJsonPath,
  readPendingEntries,
  stripUnreleased,
} from "./changeset-lib.mjs";

function syncDescription(version) {
  const desc = fs.readFileSync(descriptionPath, "utf8");
  if (!/^Version:\s*.+$/m.test(desc)) {
    throw new Error("DESCRIPTION has no Version: field");
  }
  fs.writeFileSync(
    descriptionPath,
    desc.replace(/^Version:\s*.+$/m, `Version: ${version}`),
    "utf8"
  );
}

function main() {
  const entries = readPendingEntries();
  if (entries.length === 0) {
    console.log("No pending changesets.");
    process.exit(0);
  }

  const bump = highestBump(entries);
  const pkg = JSON.parse(fs.readFileSync(packageJsonPath, "utf8"));
  const current = pkg.version;
  const next = nextVersion(current, bump);

  const newsBlock = formatNewsBlock(next, entries);
  const existingNews = stripUnreleased(
    fs.existsSync(newsPath) ? fs.readFileSync(newsPath, "utf8") : ""
  );

  const versionHeader = new RegExp(
    `^#\\s*${next.replace(/\./g, "\\.")}\\s*\\n`
  );
  let rest = existingNews;
  if (versionHeader.test(existingNews)) {
    const nextHeader = existingNews.search(/\n#\s+\d/);
    rest = nextHeader === -1 ? "" : existingNews.slice(nextHeader + 1);
  }

  const previous = rest.startsWith("#") ? rest : rest.trimStart();
  const newsContent = previous ? `${newsBlock}\n${previous}` : newsBlock;
  fs.writeFileSync(newsPath, newsContent, "utf8");

  // changesets/action always reads CHANGELOG.md after versioning (ENOENT otherwise).
  const existingChangelog = fs.existsSync(changelogPath)
    ? fs.readFileSync(changelogPath, "utf8")
    : "";
  const changelogHeader = new RegExp(
    `^##\\s*${next.replace(/\./g, "\\.")}\\s*\\n`
  );
  let changelogRest = existingChangelog;
  if (changelogHeader.test(existingChangelog)) {
    const nextHeader = existingChangelog.search(/\n##\s+/);
    changelogRest =
      nextHeader === -1 ? "" : existingChangelog.slice(nextHeader + 1);
  } else if (existingChangelog.startsWith("# ")) {
    const afterTitle = existingChangelog.indexOf("\n");
    changelogRest =
      afterTitle === -1 ? "" : existingChangelog.slice(afterTitle + 1).trimStart();
  }
  const changelogBlock = formatChangelogBlock(next, entries);
  const changelogBody = changelogRest.startsWith("##")
    ? changelogRest
    : changelogRest.trimStart();
  fs.writeFileSync(
    changelogPath,
    `# inzight-lite\n\n${changelogBlock}${changelogBody ? `\n${changelogBody}` : ""}`,
    "utf8"
  );

  pkg.version = next;
  fs.writeFileSync(
    packageJsonPath,
    `${JSON.stringify(pkg, null, 2)}\n`,
    "utf8"
  );
  syncDescription(next);

  for (const entry of entries) {
    fs.unlinkSync(entry.filePath);
  }

  console.log(
    `Version ${current} → ${next} (${bump}; ${entries.length} changeset${entries.length === 1 ? "" : "s"})`
  );
  for (const entry of entries) {
    console.log(
      `  [${entry.bump}/${entry.section}] ${entry.note.slice(0, 80)}${entry.note.length > 80 ? "…" : ""}`
    );
  }
}

main();
