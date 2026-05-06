# URL query parameters (iNZight Lite)

iNZight Lite reads the browser query string (`?name=value&...`) on load. Parameters are parsed with Shiny’s `parseQueryString`, except where noted for Google Sheets links and a few panels that re-extract `url=` from the raw string (so parameter order and encoding matter for long URLs).

Unless stated otherwise, all parameters are optional and can be combined when it makes sense (for example load data from `url` or `example`, then set `land` and panel-specific variables).

---

## Instance configuration

### `lite_config`

Selects a named entry from the server-side JSON in the `LITE_CONFIG` environment variable. The query value is uppercased and must match a key in that JSON (for example `lite_config=cas` with a `CAS` key in the config).

**Effects:**

- Sets `session$userData$LITE_VERSION` and, when the key exists, `session$userData$LITE_CONFIG` (used for restricted builds such as CAS).
- Only applied when `lite_config` is present and `LITE_VERSION` has not already been set for the session.
- Which tabs and panels exist depends on this version (see `server.R` `generate_tabs` and conditional `source()` calls).

Operators deploy the JSON; end users only pass the query name that matches their environment.

---

## Data loading (About panel)

Handled in `panels/A1_About/2_about-panel-server.R` when the About UI renders.

### `url`

HTTP(S) URL of a dataset file to download and open (via `get.data.from.URL`). Must be non-empty. Use standard URL-encoding for special characters.

Works together with optional `land`, `x`, `y`, `time`, and model-fitting parameters described below.

### `example`

Name of a built-in example dataset under the package `data` directory (via `load.data("data", ...)`). Non-empty string.

### `land`

After a successful load from `url` or `example`, switches the main tab strip (`selector`) to a module:

| Value        | Tab / module                          |
| ------------ | -------------------------------------- |
| `visualize`  | Visualize (value `visualize`)        |
| `timeSeries` | Time Series area                      |
| `regression` | Model Fitting (tab value `regression`) |

Ignored if data did not load or the value is not exactly one of these strings.

### CAS-only: `filename` and `iv`

When `session$userData$LITE_VERSION == "CAS"` (normally after `lite_config` + server config), the app can load data without a plain `url`:

- **`filename`**: Base64 payload decrypted with AES-CBC using the server config key and `iv`.
- **`iv`**: Hex-encoded IV for that decryption.

Decrypted content is the file path/name appended to `session$userData$LITE_CONFIG$URL` and then fetched like a normal URL import. The same `land` rules as above apply after load.

---

## Google Sheets / `docs.google.com` URLs

If the **entire** query string contains `docs.google.com`, `functions.R`’s `parseQueryString` does **not** use Shiny’s default parser. It returns only:

- **`url`**: extracted with a regex (expects `url=` … `&land` in the string for some code paths).
- **`land`**: extracted after `&land=`.

The Import panel (`panels/B1_ImportDataset/2_import.data.set.panel-server.R`) also special-cases this case by substring parsing between `url=` and `&land=` and pre-filling the import UI.

**Practical guidance:** For Google export links, structure the query so the spreadsheet URL appears as the `url` parameter and, if you use `land`, include `&land=...` after it, consistent with the visualize panel’s `url` extraction (`.*?url=(.*?)&land`).

---

## Visualize (`panels/C1_Visualize/2_visualize-panel-server.R`, `infoWindow.R`)

Requires data already specified via **`url`** or **`example`** (same conditions as elsewhere: those keys present and non-empty where needed).

### `x` / `y`

Column names for variable 1 and variable 2 when the corresponding selectors are first built (`vari1` / `vari2`).

### `debug`

If present and case-insensitively equal to `true`, plotting and inference code paths prefer `tryCatch` and print errors to the **server** console instead of quieter `try` / `suppressWarnings` behavior. Intended for development and diagnosing visualization or inference issues.

---

## Time Series

### Current module (`panels/F2_TimeSeries/2_timeseries-panel-server.R`)

With **`url`** or **`example`** as above:

### `time`

Column name to select as the time variable in the Time Series UI (`tsui_select_timevars`).

### Legacy module (`panels/F2_TimeSeriesLegacy/2_timeseries-panel-server.R`)

Same **`time`** behavior on the legacy time variable selector.

### `seriesVars` (legacy only)

Comma-separated list of numeric column names pre-selected in the legacy “Series variables” control (`select_variables`).

---

## Model fitting / regression (`panels/F3_ModelFitting/2_modelfitting-panel-server.R`)

With **`url`** or **`example`**:

### `Y`

Outcome variable name (`select_Y`).

### `predict`

Comma-separated list of predictor variable names (`independent_variables`).

### `confound`

Comma-separated list of confounding variable names (`confounding_variables`).

These are only applied on the first panel render while an internal `updatePanel$first` flag is true.

---

## Support / diagnostics

### `showLogs`

If this parameter is present (any value), the About panel renders a log file picker UI backed by files in R’s `tempdir()` for the session (intended for support / debugging with shinylogs-style JSON logs).

---

## Client-side helpers (not Shiny `parseQueryString`)

### `_reconnect`

`www/js/reconnect.js` sets `_reconnect=<unix_ms>` on the query string when the user triggers a forced reconnect (clears cookies and storage, then reloads). You normally do not set this by hand; it exists to bust cached client state.

### `logId`

`www/js/download-logs.js` redirects to `/logs/lite_logs_<logId>.json` when `logId` is present. That script is **commented out** in `ui.R` by default; if enabled in a custom build, `?logId=...` is a direct log download helper.

---

## Encoding and parsing quirks

1. **Long `url` values** must be URL-encoded (`encodeURIComponent` in JavaScript, `URLencode` in R where applicable).
2. Several panels **re-parse** `session$clientData$url_search` with `sub(".*?url=(.*?)&...", ...)` variants (`&land`, `&.*`, etc.). If `url` is the last parameter with no trailing `&`, some regexes may still work depending on the pattern; prefer putting **`url` early** and adding further parameters after it.
3. **`parseQueryString(...)[1]`** in the Import panel passes the first list element by position into `load.data.panel`; rely on stable ordering only if you understand R’s named-list ordering, or use the Google-docs special path when relevant.

---

## Quick reference

| Parameter     | Purpose |
| ------------- | ------- |
| `lite_config` | Named server config / app variant (from `LITE_CONFIG` JSON). |
| `url`         | Load dataset from URL. |
| `example`     | Load built-in example by filename. |
| `land`        | Jump to `visualize`, `timeSeries`, or `regression` after load. |
| `filename`    | CAS: encrypted file identifier (with `iv`). |
| `iv`          | CAS: decryption IV (hex). |
| `x`, `y`      | Visualize variable 1 / 2. |
| `time`        | Time Series time column. |
| `seriesVars`  | Legacy Time Series: comma-separated series columns. |
| `Y`           | Model fitting response. |
| `predict`     | Model fitting predictors (comma-separated). |
| `confound`    | Model fitting confounders (comma-separated). |
| `debug`       | `true`: noisier server-side errors in Visualize / inference. |
| `showLogs`    | Show session log browser on About. |
| `_reconnect`  | Client reconnect helper (timestamp). |
| `logId`       | Optional redirect to log JSON (if `download-logs.js` enabled). |
