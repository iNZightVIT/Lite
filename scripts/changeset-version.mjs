#!/usr/bin/env node
/**
 * Apply pending Changesets for iNZight Lite (CalVer continuous releases):
 * 1. Read .changeset/*.md (bump type, section, notes)
 * 2. Bump package.json version:
 *      patch/minor → YYYY.MM.(N+1)
 *      major       → {releaseDate:YYYY.MM}.0  (fallback to N+1 if same month)
 * 3. Prepend NEWS.md in Lite style
 * 4. Sync DESCRIPTION Version
 * 5. Delete consumed changeset files
 *
 * Does not create CHANGELOG.md or publish to npm.
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

function parseVersion(version) {
  const parts = version.split(".");
  if (parts.length !== 3 || parts.some((p) => !/^\d+$/.test(p))) {
    throw new Error(
      `Expected YYYY.MM.N version, got "${version}". Set package.json Version first.`
    );
  }
  return {
    year: Number(parts[0]),
    month: Number(parts[1]),
    n: Number(parts[2]),
    series: `${parts[0]}.${parts[1]}`,
  };
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
  return {
    year: Number(parts.year),
    month: Number(parts.month),
    series: `${parts.year}.${parts.month}`,
  };
}

function highestBump(entries) {
  return entries.reduce(
    (best, e) => (BUMP_RANK[e.bump] > BUMP_RANK[best] ? e.bump : best),
    "patch"
  );
}

/**
 * CalVer continuous releases (YYYY.MM.N):
 * - patch/minor: next build in the current series → N+1
 * - major: open a dated series → {Auckland YYYY.MM}.0
 *   If that series is already current, fall back to N+1 (no version downgrade).
 */
function nextVersion(current, bump) {
  const cur = parseVersion(current);
  if (bump === "major") {
    const dated = releaseSeries();
    const datedKey = dated.year * 100 + dated.month;
    const curKey = cur.year * 100 + cur.month;
    if (datedKey > curKey) {
      return `${dated.year}.${String(dated.month).padStart(2, "0")}.0`;
    }
    // Same or earlier calendar month than current series → just increment
  }
  return `${cur.year}.${String(cur.month).padStart(2, "0")}.${cur.n + 1}`;
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
  fs.writeFileSync(
    newsPath,
    previous ? `${newsBlock}\n${previous}` : newsBlock,
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

  const changelogPath = path.join(root, "CHANGELOG.md");
  if (fs.existsSync(changelogPath)) {
    fs.unlinkSync(changelogPath);
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
