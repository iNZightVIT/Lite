iNZight *Lite* v.0.9.5 (More to come)
====================================
This is the online version of iNZight, with two new modules:

- Visualize
- Time Series


To do:
------
- [General] Plots in the "Visualize" module seem to have an ugly border around them for certain browsers - fix.
- [General] Fix the "Datatables" bug
- [General] Fix the footer/panel overlap bug.
- [Time Series] Change the way plots are drawn to how it was done in "Visualize".
- [Time Series] "Provide Time Information" needs to be linked to server.R
- [Time Series] Check if it can handle datasets with the "DATE" columns in column 2+.
- [Time Series] Make the "TS" panel conditional on the type of dataset loaded.
- [Quick Explore] Merge into 1 tab.
- [Visualize] Add inferential markups.
- [Visualize] Re-design the "Customize Plot" panel.
- [Advanced] Nest QE and TS under "Advanced".
- [Help] Add a "Help" tab for reporting bugs and documentation


Directories:
------------

- data:
This directory holds all the data. The data must be placed in sub-directories, which will come up as data categories within iNZight-Lite.

- gui-elemenets:
This directory contains the gui functions for all the modules.

- id-documentation:
This directory contains the documentation for the input and output ID's for the time series module.

- server-elements: 
This directory contains the server functions for the two new modules.

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

This repository will gradually be changed into the following format throughout 2015:

- data:
This directory holds all the data. The data must be placed in sub-directories, which will come up as data categories within iNZight *Lite*. 

- panels:
This directory contains the code and files that set up user selections. There is one sub-directory that contains all the .html and .md files.
Each panel has its own directory within 'panels', and each sub-panel has its own directory within each parent panel.

The file names adhere to the following guidelines:

- N_panel-name
- M_sub-panel-name
- 1_panel-server.R
- 2_panel-gui.R
- 3_panel-help.R
- 4_panel-other.R

- www:
This directory is used for web-related files such as .css and .gif/.jpeg/.png. Images are stored in a sub-directory called "images".
