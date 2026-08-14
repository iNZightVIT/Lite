/**
 * Shared helpers for Changesets CalVer tooling (version + unreleased NEWS).
 */

import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

export const root = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..");
export const changesetDir = path.join(root, ".changeset");
export const packageJsonPath = path.join(root, "package.json");
export const descriptionPath = path.join(root, "DESCRIPTION");
export const newsPath = path.join(root, "NEWS.md");
export const changelogPath = path.join(root, "CHANGELOG.md");

export const SECTION_ORDER = [
  "New features",
  "UI Changes",
  "Changes",
  "Fixes",
  "Infrastructure",
];

export const BUMP_RANK = { patch: 1, minor: 1, major: 2 };

export const UNRELEASED_TITLE = "Unreleased";

export function listChangesetFiles() {
  if (!fs.existsSync(changesetDir)) return [];
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

export function parseChangeset(filePath) {
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

export function formatSeries(year, month) {
  return `${year}.${String(month).padStart(2, "0")}`;
}

export function formatBuild(year, month, n) {
  return `${formatSeries(year, month)}.${String(n).padStart(2, "0")}`;
}

export function parseVersion(version) {
  const match = String(version).trim().match(/^(\d{4})\.(\d{1,2})(?:\.(\d+))?$/);
  if (!match) {
    throw new Error(
      `Expected YYYY.MM or YYYY.MM.NN version, got "${version}". Set package.json Version first.`
    );
  }
  const year = Number(match[1]);
  const month = Number(match[2]);
  const n = match[3] === undefined ? 0 : Number(match[3]);
  return { year, month, n, series: formatSeries(year, month) };
}

export function releaseSeries(date = new Date()) {
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

export function highestBump(entries) {
  return entries.reduce(
    (best, e) => (BUMP_RANK[e.bump] > BUMP_RANK[best] ? e.bump : best),
    "patch"
  );
}

export function nextVersion(current, bump) {
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

function groupSections(entries) {
  const bySection = new Map();
  for (const entry of entries) {
    if (!bySection.has(entry.section)) bySection.set(entry.section, []);
    bySection.get(entry.section).push(entry.note);
  }
  return [
    ...SECTION_ORDER.filter((s) => bySection.has(s)),
    ...[...bySection.keys()]
      .filter((s) => !SECTION_ORDER.includes(s))
      .sort((a, b) => a.localeCompare(b)),
  ].map((section) => ({ section, notes: bySection.get(section) }));
}

/** Lite NEWS.md uses `# Title` then `## Section`. */
export function formatNewsBlock(title, entries) {
  const lines = [`# ${title}`, ""];
  for (const { section, notes } of groupSections(entries)) {
    lines.push(`## ${section}`, "");
    for (const note of notes) {
      lines.push(`- ${note}`);
    }
    lines.push("");
  }
  return lines.join("\n");
}

/** changesets/action expects `## version` headings in CHANGELOG.md. */
export function formatChangelogBlock(version, entries) {
  const lines = [`## ${version}`, ""];
  for (const { section, notes } of groupSections(entries)) {
    lines.push(`### ${section}`, "");
    for (const note of notes) {
      lines.push(`- ${note}`);
    }
    lines.push("");
  }
  return lines.join("\n");
}

/** Drop a leading `# Unreleased` block (build-time / stale). */
export function stripUnreleased(news) {
  const trimmed = news.replace(/^\uFEFF/, "");
  if (!/^#\s*Unreleased\b/i.test(trimmed)) return trimmed;
  const nextHeader = trimmed.search(/\n#\s+/);
  if (nextHeader === -1) return "";
  return trimmed.slice(nextHeader + 1);
}

export function readPendingEntries() {
  return listChangesetFiles().map(parseChangeset);
}
