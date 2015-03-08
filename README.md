iNZight *Lite* version 0.9.7
============================

Last Updated: March 8, 2015.

*Lite* is the new online version of iNZight (http://docker.stat.auckland.ac.nz), with four new modules:

- About
- Visualize
- Time Series
- Help

Underway:
---------
- Help documentation for the "Time Series" and "Visualize" modules.
- Error handling in the "Time Series" and "Visualize" modules.
- Update documentation for all modules.

Recent Updates:
---------------
- DataTables error message disabled - permanent fix to be explored (Christoph).
- Fixed question mark error (Christoph).
- 404 Error fixed in "About" module (Christoph).
- Footer appears at the bottom of the screen but sits below all of the content if window size is < 767px (Tom).
- New help text for variable selection in the "Time Series" module.
- New side-panel layout for the "Visualize" module.
- New "Help" module.
- Restructured code - new panels have their own directory.

To do/Known Issues:
-------------------
- [Data] Further extraction / manipulation tools to be added. Discuss with Tom.
- [Time Series] Change the way plots are drawn to how it was done in "Visualize".
- [Time Series] "Provide Time Information" needs to be linked to server.R
- [Time Series] Check if it can handle datasets with the "DATE" columns in column 2+.
- [Time Series] Make the "TS" panel conditional on the type of dataset loaded.
- [Quick Explore] Merge into 1 tab.
- [Visualize] Add inferential markups.
- [Advanced] Nest QE and TS under "Advanced".

Directories:
------------
- data:
This directory holds all the data. The data must be placed in sub-directories, which will come up as data categories within iNZight-Lite.

- gui-elements:
This directory contains the gui functions for all the old modules (from iNZight revamp). 

- panels:
This directory contains the gui, server, help, and other functions for each module. It currently contains the "About", "Time Series", "Visualize", and "Help" modules. This makes it easier to modify code.

- www:
This directory is used for web-related files such as .css and .gif/.jpeg/.png. Images are stored in a sub-directory called "images".

Main files:
-----------
- README:
Documentation for iNZight *Lite*. Update as required.

- global.R:
Defines functions and variables that are used globally. For now, all non-GUI functions are stored here.

- server.R:
Script for shiny server.

- ui.R:
Defines the main GUI.

Note:
-----
This repository will gradually (throughout 2015) be changed into the format described below:

- data:
This directory holds all the data. The data must be placed in sub-directories, which will come up as data categories within iNZight *Lite*. 

- www:
This directory is used for web-related files such as .css and .gif/.jpeg/.png. Images are stored in a sub-directory called "images".

- panels:
This directory contains the code and files that set up each module. 

Each panel will have its own directory within the 'panels' directory, and each sub-panel will also have its own directory within each parent panel, as follows:

- N_panel-name
- M_sub-panel-name

The file names contained within each (sub-) panel directory will be of the following format:

- 1_panelname-panel-gui.R
- 2_panelname-panel-server.R
- 3_panelname-panel-help.R
- 4_panelname-panel-other.R

