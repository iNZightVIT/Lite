iNZight *Lite* version 1.0
==============================


*Last Updated: 24/05/17*

This is the online version of iNZight (http://lite.docker.stat.auckland.ac.nz), with four new modules:

- Visualize
- Time Series
- Model Fitting
- Maps

Recent Updates (Most to least recent):
----------------------------------------
- [visualize] A panel for displaying interactive plots available
- [Import dataset ] Now is able to read excel, sas, spss and stata files 
- [visualize] Add time delay between plots options
- [visualize] Add "switch" option among selected variables
- [visualize] It is able to add fitted values and residuals to original data
- [visualize] Add a "revert to old version" button to let users go back to the old visualize panel
- [Visualize] Implement a new version of advanced plot, the "hide" option is repalced by a "Add To Plot" button on the top-left
- [Maps] Add Maps module
- [Visualize] Add Save Plot buttons for both main plot and mini plot
- [Manipulate variables] Fixed the "change to factor" function under "Manipulate variables"
- [Filter data] Fixed a problem from "levels of categorical variables" under "Filter data"
- [Alphabetise variables] Added a new "Alphabetise variables" category
- [Visualize] Change the default backgorund color for plots to "grey93" 
- [General] Fixed some problems occured from the implementation of iNZightPlots 2.50
- [General] Implement iNZightPlots 2.50
- [General] Re-order the logos at the bottom of the starting page
- [General] Change the contact email to inzightlite_support@stat.auckland.ac.nz
- [Visualize] Fixed an visualization problem happened to rds files where the first column name cannot be displayed
- [Visualize] Change the displaying order of "missing" values and "observed" values
- [Missing to categorical] Made the "missing to categorical" function to be consistent with iNZightVIT
- [Visualize] Fixed the problem of "when g2_level == 0, doesn't show plots"
- [Visualize] Get rid of "none" option for the selectinput box of variable 1. Now when users go to "visualize" page, it directly shows the plot of the first column 
- [Visualize] The smallest pch size for dots plot is reset to be 0.1 (was 0.05) 
- [Visualize] Fixed the problem of the "none" option for the 1st subset variable 
- [Filter Dataset] Go back to "visualize" page after filtering the dataset, all the variable slections are cleared and users can select variables to be plotted again based on filtered dataset
- [Visualize] Fixed the "RESET ALL" function
- [Visualize] Fixed the problem of displaying summary and inference for a numeric varibale
- [Visualize] Fixed the problem of considering NAs as a single category 
- [Import dataset] When users switch a page from "visualize" or other analysis modules back to "import dataset", old dataset preview table doesn't disappear
- [Import dataset] Users are able to upload new dataset without removing old dataset or
refreshing page
- [Import dataset] importing local file doesn't need to click "import file" anymore
- [Import dataset] fixed importing dataset from url
- [Visualize] "none" option added for selecting first variable.
- [Global] Implemented GET variables to add to iNZight-lite URL.
- [Visualize] All "Advanced options" are now up to date with iNZight.
- [Model Fitting] Implement the model fitting module
- [Time Series] Check if it can handle datasets with the "DATE" columns in column 2+.
- [Time Series] Fix "Provide Time Information".
- [Help] This module was removed. Even though it is in iNZight links to the new webpage already exist in the "About" start page and help to every module is available for every module inside the modules. 
- [Visualize]  Add inferential markups once Tom finishes updating iNZightPlots().
- [Advanced] Nest QE and TS under "Advanced".
- [Quick Explore] Merge into 1 tab.
- [General] Changed all global variables to reactive values (CK)
- [Row Operations] Row operations added (CK).
- [General] A Markdown 'troubleshoot' document appears when no data set is loaded (CP).
- [Data Import] URL support added (CK).
- [Quick Explore] Single column plot update - player feature (CK).
- [Time Series] Added shiny validation messages and modified the input panel so that only "Provide Time Information" is shown if the data set has no time variable defined in a format recognized by iNZightTS() (CP).
- [Visualize] Fixed the "Reset All" button (CP).
- [General] Updated help.md files for the "Time Series" and "Visualize" modules and created a template for use in other modules in */panels/0_Template/3_panelname-panel-help.md* (CP).
- [General] Fixed Datatables bug (CK).
- [General] Fixed question mark error (CK).
- [General] 404 Error fixed in "About" module (CK).
- [General] Footer appears at the bottom but sits below the content if window $<$ 767px (TE).
- [Help] Added a new "Help" module (CP).
- [General] Restructured code - new panels now have their own directory (CP).

To do:
------
- [General] Documentation.

Package dependencies
--------------------
This are all the packages which are loaded by iNZight lite.
Please install all dependencies.

- iNZightPlots
- iNZightTS
- iNZightMR
- markdown
- GGally
- gpairs
- iNZightRegression
- RJSONIO
- survey

Directories:
------------
- data:
This directory holds all the data. The data must be placed in sub-directories, which will come up as data categories within iNZight-Lite.

- gui-elements: (removed)
This directory contains the gui functions for all the old modules (from iNZight revamp). 

- www:
This directory is used for graphics and/or web-related files such as .css and .gif/.jpeg/.png javascript files. Images are stored in a sub-directory called "images". Javascript code is stored in a subdirectory "js".

- panels:
This directory contains the gui, server, help, and other functions for each module. 

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

- functions.R
Defines al functions used in iNZight-lite, some of them are also in iNZightTools.

Note:
-----
This repository will gradually be changed into the following format throughout 2015:

- data:
This directory holds all the data. The data must be placed in sub-directories, which will come up as data categories within iNZight *Lite*. 

- www:
This directory is used for graphics and/or web-related files such as .css and .gif/.jpeg/.png. Images are stored in a sub-directory called "images".

- panels:
This directory contains the gui, server, help, and other functions for each module. The directory/file names will be of the form:
  + [Directory] N_(sub-)panel-name
  + [File] 1_panelname-panel-gui.R
  + [File] 2_panelname-panel-server.R
  + [File] 3_panelname-panel-help.R
  + [File] 4_panelname-panel-null.R
  + [File] 5_panelname-panel-other.R

iNZight-lite URL feature
------------------------

Some GET parameters can be added to the URL to access iNZight-lite. In the moment 
the following are implemented.

- url:
An URL to a dataset sitting somewhere in the internet.
- example:
A name of an data set inside the data folder.
- land:
The place where iNZight-lite starts. If not specified it is "About". Three 
possibilities (visualize, timeSeries, regression).
- x:
The x variable in the visualize module
- y:
The y variable in the visualize module
- time:
The variable in time series where the dates are stored.
-seriesVars
A comma seperated list of column names. This names are set in time series to be 
plotted
- Y:
The dependend variable in regression.
- predict:
Comma seperated list with predictor variables.
- confound:
Comma seperated list for confounding variables. 


