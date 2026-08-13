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
import path from "node:path";
import { fileURLToPath } from "node:url";

const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..");
const changesetDir = path.join(root, ".changeset");
const packageJsonPath = path.join(root, "package.json");
const descriptionPath = path.join(root, "DESCRIPTION");
const newsPath = path.join(root, "NEWS.md");

const SECTION_ORDER = [
  "New features",
  "UI Changes",
  "Changes",
  "Fixes",
  "Infrastructure",
];

const BUMP_RANK = { patch: 1, minor: 1, major: 2 };

function listChangesetFiles() {
  return fs
    .readdirSync(changesetDir)
    .filter(
      (name) =>
        name.endsWith(".md") && name.toLowerCase() !== "readme.md"
    )
    .map((name) => path.join(changesetDir, name));
}

function parseBump(frontmatter, filePath) {
  const line = frontmatter
    .split(/\r?\n/)
    .map((l) => l.trim())
    .find((l) => l.includes("inzight-lite"));
  if (!line) {
    throw new Error(
      `Changeset must include "inzight-lite": ${path.basename(filePath)}`
    );
  }
  const bumpMatch = line.match(/:\s*(patch|minor|major)\s*$/i);
  if (!bumpMatch) {
    throw new Error(
      `Changeset bump must be patch, minor, or major: ${path.basename(filePath)}`
    );
  }
  return bumpMatch[1].toLowerCase();
}

function parseChangeset(filePath) {
  const raw = fs.readFileSync(filePath, "utf8");
  const match = raw.match(/^---\r?\n([\s\S]*?)\r?\n---\r?\n([\s\S]*)$/);
  if (!match) {
    throw new Error(`Invalid changeset (missing frontmatter): ${filePath}`);
  }

  const frontmatter = match[1];
  let body = match[2].trim();
  const bump = parseBump(frontmatter, filePath);

  let section = "Changes";
  const heading = body.match(/^##\s+(.+?)\s*$/m);
  if (heading && body.startsWith("##")) {
    section = heading[1].trim();
    body = body.slice(heading[0].length).trim();
  } else {
    const tag = body.match(/^\[([^\]]+)\]\s*/);
    if (tag) {
      section = tag[1].trim();
      body = body.slice(tag[0].length).trim();
    }
  }

  const note = body
    .split(/\r?\n/)
    .map((line) => line.replace(/^\s*[-*]\s+/, "").trim())
    .filter(Boolean)
    .join(" ");

  if (!note) {
    throw new Error(`Empty changeset body: ${path.basename(filePath)}`);
  }

  return { filePath, bump, section, note };
}

function formatSeries(year, month) {
  return `${year}.${String(month).padStart(2, "0")}`;
}

/** Build counter: 01, 02, … (two digits). Bare YYYY.MM is the first major of a series. */
function formatBuild(year, month, n) {
  return `${formatSeries(year, month)}.${String(n).padStart(2, "0")}`;
}

function parseVersion(version) {
  const match = String(version).trim().match(/^(\d{4})\.(\d{1,2})(?:\.(\d+))?$/);
  if (!match) {
    throw new Error(
      `Expected YYYY.MM or YYYY.MM.NN version, got "${version}". Set package.json Version first.`
    );
  }
  const year = Number(match[1]);
  const month = Number(match[2]);
  // Bare YYYY.MM ⇒ build 0 (series opener); patches start at 01
  const n = match[3] === undefined ? 0 : Number(match[3]);
  return { year, month, n, series: formatSeries(year, month) };
}

/** Release calendar month in Pacific/Auckland (Lite's home timezone). */
function releaseSeries(date = new Date()) {
  const fmt = new Intl.DateTimeFormat("en-NZ", {
    timeZone: "Pacific/Auckland",
    year: "numeric",
    month: "2-digit",
  });
  const parts = Object.fromEntries(
    fmt.formatToParts(date).map((p) => [p.type, p.value])
  );
  const year = Number(parts.year);
  const month = Number(parts.month);
  return { year, month, series: formatSeries(year, month) };
}

function highestBump(entries) {
  return entries.reduce(
    (best, e) => (BUMP_RANK[e.bump] > BUMP_RANK[best] ? e.bump : best),
    "patch"
  );
}

/**
 * CalVer continuous releases:
 * - major: open a dated series → {Auckland YYYY.MM} (no .0)
 * - patch/minor: next build → .01, .02, … (from bare series or existing NN)
 *   If major would not advance the calendar series, fall back to patch bump.
 */
function nextVersion(current, bump) {
  const cur = parseVersion(current);
  if (bump === "major") {
    const dated = releaseSeries();
    const datedKey = dated.year * 100 + dated.month;
    const curKey = cur.year * 100 + cur.month;
    if (datedKey > curKey) {
      return formatSeries(dated.year, dated.month);
    }
  }
  return formatBuild(cur.year, cur.month, cur.n + 1);
}

function formatNewsBlock(version, entries) {
  const bySection = new Map();
  for (const entry of entries) {
    if (!bySection.has(entry.section)) bySection.set(entry.section, []);
    bySection.get(entry.section).push(entry.note);
  }

  const ordered = [
    ...SECTION_ORDER.filter((s) => bySection.has(s)),
    ...[...bySection.keys()]
      .filter((s) => !SECTION_ORDER.includes(s))
      .sort((a, b) => a.localeCompare(b)),
  ];

  const lines = [`# ${version}`, ""];
  for (const section of ordered) {
    lines.push(`## ${section}`, "");
    for (const note of bySection.get(section)) {
      lines.push(`- ${note}`);
    }
    lines.push("");
  }
  return lines.join("\n");
}

/** Changesets action reads CHANGELOG.md (## version); NEWS.md stays Lite-canonical (# version). */
function formatChangelogBlock(version, entries) {
  const bySection = new Map();
  for (const entry of entries) {
    if (!bySection.has(entry.section)) bySection.set(entry.section, []);
    bySection.get(entry.section).push(entry.note);
  }

  const ordered = [
    ...SECTION_ORDER.filter((s) => bySection.has(s)),
    ...[...bySection.keys()]
      .filter((s) => !SECTION_ORDER.includes(s))
      .sort((a, b) => a.localeCompare(b)),
  ];

  const lines = [`## ${version}`, ""];
  for (const section of ordered) {
    lines.push(`### ${section}`, "");
    for (const note of bySection.get(section)) {
      lines.push(`- ${note}`);
    }
    lines.push("");
  }
  return lines.join("\n");
}

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
  const files = listChangesetFiles();
  if (files.length === 0) {
    console.log("No pending changesets.");
    process.exit(0);
  }

  const entries = files.map(parseChangeset);
  const bump = highestBump(entries);
  const pkg = JSON.parse(fs.readFileSync(packageJsonPath, "utf8"));
  const current = pkg.version;
  const next = nextVersion(current, bump);

  const newsBlock = formatNewsBlock(next, entries);
  const existingNews = fs.existsSync(newsPath)
    ? fs.readFileSync(newsPath, "utf8")
    : "";

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
  const changelogPath = path.join(root, "CHANGELOG.md");
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
    // Drop a title-only first line so ## entries stay at top for getChangelogEntry
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
