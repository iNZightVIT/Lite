iNZight *Lite* 
==============================


*Last Updated: 04/12/19*

This is the online version of iNZight (http://lite.docker.stat.auckland.ac.nz)


## To run locally:

iNZight Lite is a shiny app, so you can run it locally if you have R installed. If you use RStudio, see instructions further down.

1. [Download and unzip](https://github.com/iNZightVIT/Lite/archive/master.zip) or clone this repository

    _Note: if you use Git to clone the repository, you can easily keep Lite updated via `git pull`_

2. Open R and set the working directory to the Lite folder
```{r}
setwd("/path/to/Lite")
```

3. Install the `devtools` R package (if you don't already have it):
```{r}
install.packages("devtools")
```
4. Install package dependencies (NOTE: we host iNZight packages on our own server, see below)
```{r}
devtools::install_deps(repos = c("http://r.docker.stat.auckland.ac.nz/R", 
                                 "https://cloud.r-project.org/"))
```
5. Run the iNZight Lite app!
```{r}
shiny::runApp()
```

### RStudio users:

1. __If you have git installed__: in RStudio > File > New Project > Version Control > Git 

    - For the URL, enter: `https://github.com/iNZightVIT/Lite.git`
    - And choose a location to save the app
    - Then Create Project
 
    __If you don't have git__: 
    
    - [Download and unzip](https://github.com/iNZightVIT/Lite/archive/master.zip) the app
    - In the folder, double click `Lite.rproj` to open the project in RStudio

2. To install the dependencies, enter the following into the Console:
```{r}
install.packages("devtools")  ## skip if you know you already have devtools
devtools::install_deps(repos = c("http://r.docker.stat.auckland.ac.nz/R", 
                                 "https://cloud.r-project.org/"))
```
    
3. To run iNZight Lite, open the `ui.R` file and click the __Run App__ button at the top.


Send bug reports/errors to inzightlite_support@stat.auckland.ac.nz.



Recent Updates (Most to least recent):
---------------------------------------- 
- [visualize] Add various new plot types
- [R code history] now, we have code history for ggplot, Import dataset, Example dataset, and all the panels under manipulate variables

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
- shinyWidgets
- iNZightTools
- shinycssloaders

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


