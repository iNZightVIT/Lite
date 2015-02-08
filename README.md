iNZight-Lite v.0.9.5 (More to come)
====================================
This is the online version of iNZight, with two new modules:

- Visualize
- Time Series


To do:
------
- "Provide Time Information" needs to be linked to server.R
- Add inferential markups to "Visualize"
- Rearrange "Data" tabs.


Directories:
------------

- data:
This directory holds all the data. The data must be placed in sub-directories, which will come up as data categories within iNZight-Lite.

- panels:
This directory contains the code and files that set up user selections. 
Each panel has its own directory within 'panels', and each sub-panel has its own directory within each parent panel.

The file names adhere to the following guidelines:

- N_panel-name
- M_sub-panel-name
- 1_panel-server.R
- 2_panel-gui.R
- 3_panel-help.R
- 4_panel-other.R

where N, M are numbers.

- www:
This directory is used for web-related files such as .css and .gif/.jpeg/.png. Images are stored in a sub-directory called "images".

Main files:
-----------

- README:
Documentation for *iO*. Update as required.

- global.R:
Defines functions and variables that are used globally. For now, all non-GUI functions are stored here.

- server.R:
Script for shiny server.

- ui.R:
Defines the main GUI.

