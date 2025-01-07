# January 2025

## UI Changes

- _'Store residuals'_ and _'Store fitted values'_ no longer produce a pop-up, and instead create the variable immediately (without asking user to provide a name).
- **Time Series module** has been refactored to connect with our updated `iNZightTS` 2.0, which has new and improved graphics and funcionality. The old version remains available via _'Time Series - Legacy'_.
- **Experimental design module** : help text added to improve usability.
- **Bar plots** have a new option (under Add to Plot) to toggle whether or not the bars are sized by the group size (the historical default is for this to be on).
- **Summary** and **Inference** add options to control significant figures, percentage rounding, and p-value minimums.

## Bug fixes

- Spaces in user supplied variable names are handled appropriately (replaced with underscores), which should prevent disconnect issues.
- Survey objected updated when data changes, so e.g., newly created variables will show up in the outputs.
- Example dataset redirects to Visualize after clicking 'Select set'.
- Various other UI fixes and minor improvements.

## Other

- Refactor data wrangling methods for new `iNZightTools` 2.0.0 API.
- Maps module has been removed following major changes/restructuring to dependent packages that we are unable to resolve at this time. Please get in touch if you used this module or would like to help us fix it.

## Patch 2024.01.01

- clarify wording in 'round %'
- fix bug where double-clicking sig fig up/down would cause an infinite loop
- fix bug where xlab/ylab passed as NULL would hide the axis labels (instead of using the default)

---

# December 2023

## Changes

- Add additional advanced options to the 'Import Data' dialog (delimiter, decimal, etc.).
- Integrates instance-configurability to allow various instances to run on the same deployment (e.g., the Census at School build). This will eventually replace the 'lite2' instance used by CaS.
- Removes 'gpairs' dependency.
- Temporary data files are now deleted after import, instead of remaining on the filesystem until the temporary directory is cleared.

## Fixes

- Fixes bug in Model Fitting module (panel had a duplicate ID, resulting in some weird behaviour).
- Fixes bug where variables swap after saving residuals, and fitted line is removed.
- Installs iNZightMultivariate package so the module can be used.
- Fix bug where help panels were not being displayed

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
