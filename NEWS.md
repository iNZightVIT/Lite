# Pre-release, 2023.11.9000

- fix bug in Model Fitting module (panel had a duplicate ID, resulting in some weird behaviour)
- remove gpairs dependency
- fix bug where variables swap after saving residuals

- major update: integrate instance-configurability to allow various instances to run on the same deployment (e.g., the Census at School build)

---

# June 2023

## New Features

- Displays a disconnected message when connection is lost or errors are encountered, listing some possible solutions to common problems.

- Adds preview mode for importing data sets, like in iNZight Desktop. This allows users to see the first few rows of their data before importing it, and modify some import settings as needed.

- Ticker added to alert users of new updates, upcoming maintenance, and other important information.

## Changes

- URL datasets are no longer saved on the disk, and instead are saved in temporary files associated with the user's session. These files are deleted once the user disconnects.

- The 'remove dataset' option in the File menu has been removed, as it is no longer needed.

- Use `iNZightTools::smart_read` when importing from URL

  - Allow users to specify comment symbol when importing text files

- Slight redesign of the logo using the same font family and weigt for 'lite', and fix alignment in header

## Bug Fixes

- Disables redirect when clicking on header logo

- Provides a temporary fix for an issue caused by passing invalid(?) values of 'varnames' to gg\_\* plottypes - there are other instances (e.g., colby, sizeby, etc), and further testing/investigation needed.

- Clears the `par$zoombars` argument when switching V1/V2 to resolve a bug that could cause duplicated bars.

- Addresses an issue in survey calibration specification.

- Import via Paste: remove special characters/spaces from variable names and convert strings to factors (via iNZightTools)

---

_Switch from semantic to date versioning system_

# Release 1.0.4

- Fixes a bug in survey imports with functions moved from iNZightTools to surveyspec

# Release 1.0.3

- Fixes a bug in time series forecasting plot

# Release 1.0.2

- Replace spaces with underscores (\_) and all other special characters with periods (.) in user-provided variable name (in 'Create New Variable')

- Allow users to specify confidence interval levels at inferences and plots.

- Allow users to specify sheet/data name when importing data.

- Added data import related information to `values`:

  - `data.type`
  - `data.available.dnames`
  - `data.current.dname`

- Show data set name in Visualize tab

- Redirect to [VIT](https://vit.inzight.nz/) at `Advanced > VIT`

# Release 1.0.1

- Rolls back dependency versions due to minor version API changes resulting in a few unintended bugs

# Release 1.0.0

This is the first formal release using a new deployment process, using a more conventional versioning scheme.

Earlier changes are not logged.
