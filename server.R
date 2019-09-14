###-------------------------------------###
### Server Functions for iNZight Lite   ###
###-------------------------------------###
###
### Date Created : January 10, 2015
### Last Modified : May 14, 2017
###
### Please consult the comments before editing any code.
### This file sources the ui files for each panel separately.

###  We load the packages we require. This is done only ONCE per instance.

library(iNZightPlots)
library(iNZightTS)
library(iNZightMR)
library(markdown)
library(GGally)
library(iNZightRegression)
library(RJSONIO)
library(survey)
library(iNZightMaps)
library(colorspace)
library(readxl)
#library(xlsx)
library(sas7bdat)
library(foreign)
#library(shinydashboard)
library(shinyjs)
#library(iNZightTools)
library(dplyr)
library(plotly)


# read in possible command line arguments such as 'vars.path'

args=(commandArgs(TRUE))

## args is now a list of character vectors
## First check to see if arguments are passed.
## Then cycle through each element of the list and evaluate the expressions.
if(length(args) == 0){
  message("No arguments supplied.")
}else{
  for(i in 1:length(args)){
    eval(parse(text=args[[i]]))
  }
}

## read in all the functions used in iNZight Lite
source("functions.R")

### We write the server function.
shinyServer(function(input, output, session) {
  
  ##Specify all the reactive values
  
  values = reactiveValues()
  values$data.name = NULL
  values$data.dir.global = "data"
  values$data.dir.imported = "data"
  values$data.set = NULL
  values$data.restore = NULL
  values$lite.version = "iNZight Lite Version 0.9.7.5"
  values$lite.update = "Last Updated: 30/09/16"
  values$button = F
  values$transform.text = ""
  values$create.variables.expression.text = ""
  
  # dummy variable to update whole panels when the data 
  # set is switched but not updated when data is changed.
  updatePanel =  reactiveValues()
  updatePanel$doit = 0
  updatePanel$datachanged = 0
  updatePanel$first = T
  
  if(!"vars.path"%in%ls()){
    vars.path<<-NULL
  }
  
  vars = get.vars(vars.path)
  if(!is.null(vars)){
    if("data.dir.global"%in%names(vars)&&
         file.exists(vars$data.dir.global)){
      values$data.dir.global = vars[["data.dir.global"]]
    }else if(!file.writable(vars$data.dir.global)){
      warning(paste("The directory : ",vars$data.dir.global,
                    " : is not writable."))
    }
    if("data.dir.imported"%in%names(vars)&&
         dir.create.logical(paste0(vars$data.dir.imported,"/Imported"),
                            recursive=T)){
      if(file.writable(vars$data.dir.imported)){
        values$data.dir.imported = vars[["data.dir.imported"]]
      }
    }else {
      values$data.dir.imported = values$data.dir.global
      if(!file.exists(paste0(values$data.dir.imported,"/Imported"))){
        dir.create(paste0(values$data.dir.imported,"/Imported"),
                   recursive = T)
      }
    }
    if("lite.version"%in%names(vars)){
      values$lite.version = vars$lite.version
    }
    if("lite.update"%in%names(vars)){
      values$lite.update = vars$lite.update
    }
  }
  
  get.data.name = reactive({
    values$data.name
  })
  
  get.data.dir.global = reactive({
    values$data.dir.global
  })
  
  get.data.dir.imported = reactive({
    values$data.dir.imported
  })

  get.data.set = reactive({
    values$data.set
  })
  
  get.data.restore = reactive({
    values$data.restore
  })
  
  get.lite.version = reactive({
    values$lite.version
  })
  
  get.lite.update = reactive({
    values$lite.update
  })
  
  get.button = reactive({
    values$button
  })
  
  get.transform.text = reactive({
    values$transform.text
  })

  get.create.variables.expression.text = reactive({
    values$create.variables.expression.text
  })
  
  #################################
  
  ##  Turn errors and warnings off
  ## options(warn = -1, show.error.messages = FALSE)
  
  ##  Delete all imported files that are older than 1 day.
  ## Load all panels into memory.
  filepaths <- list.files(pattern = "[.]R$",
                          path = "gui-elements/",
                          full.names = TRUE)
  sapply(filepaths, source)

  ##----------------------##
  ##  A1. "About" Module  ##
  ##----------------------##
  source("panels/A1_About/1_about-panel-ui.R",local=T)
  source("panels/A1_About/2_about-panel-server.R",local=T)
  
  ##---------------------------------------##
  ##  B1. "File -> Import Dataset" Module  ##
  ##---------------------------------------##
  source("panels/B1_ImportDataset/1_import.data.set.panel-ui.R", local = TRUE)
  source("panels/B1_ImportDataset/2_import.data.set.panel-server.R", local = TRUE)

  ##---------------------------------------##
  ##  B2. "File -> Import Dataset" Module  ##
  ##---------------------------------------##
  source("panels/B2_ExportDataset/1_export.dataset.panel-ui.R", local = TRUE)
  source("panels/B2_ExportDataset/2_export.dataset.panel-server.R", local = TRUE)

  ##---------------------------------------##
  ##  B3. "File -> Export Dataset" Module  ##
  ##---------------------------------------##
  source("panels/B3_DisplayDataset/1_display.data.set.panel-ui.R", local = TRUE)
  source("panels/B3_DisplayDataset/2_display.data.set.panel-server.R", local = TRUE)
  
  ##---------------------------------------##
  ##  B4. "File -> Remove Dataset" Module  ##
  ##---------------------------------------##
  source("panels/B4_RemoveDataset/1_remove.data.set.panel-ui.R", local = TRUE)
  source("panels/B4_RemoveDataset/2_remove.data.set.panel-server.R", local = TRUE)
  
  ##-----------------------------------------##
  ##  B5. "File -> Dataset Examples" Module  ##
  ##-----------------------------------------##
  source("panels/B5_DatasetExamples/1_data.set.examples-ui.R", local = TRUE)
  source("panels/B5_DatasetExamples/2_data.set.examples-server.R", local = TRUE)

  ##------------------------##
  ##  C1. Visualize Module  ##
  ##------------------------##
  source("panels/C1_Visualize/1_visualize-panel-ui.R", local = TRUE)
  source("panels/C1_Visualize/2_visualize-panel-server.R", local = TRUE)
  
  ##-----------------------------##
  ##  D1. Filter Dataset Module  ##
  ##-----------------------------##
  source("panels/D1_FilterDataset/1_filter.dataset-ui.R", local = TRUE)
  source("panels/D1_FilterDataset/2_filter.dataset-server.R", local = TRUE)
  
  ##-----------------------------##
  ##  D2. Sort data by variables ##
  ##-----------------------------##
  source("panels/D2_SortDataByVariables/1_sort.variables.ui.R", local = TRUE)
  source("panels/D2_SortDataByVariables/2_sort.variables.server.R", local = TRUE)

  ##-----------------------------##
  ##  D3. Aggregate data         ##
  ##-----------------------------##
  source("panels/D3_AggregateData/1_aggregate.data.ui.R", local = TRUE)
  source("panels/D3_AggregateData/2_aggregate.data.server.R", local = TRUE)
  
  ##-----------------------------##
  ##  D4. Stack variables        ##
  ##-----------------------------##
  source("panels/D4_StackVariables/1_stack.variables.ui.R", local = TRUE)
  source("panels/D4_StackVariables/2_stack.variables.server.R", local = TRUE)
  
  ##-----------------------------##
  ##  D8. Reshape data           ##
  ##-----------------------------##
  source("panels/D8_ReshapeData/1_reshape.dataset.ui.R", local = TRUE)
  source("panels/D8_ReshapeData/2_reshape.dataset.server.R", local = TRUE)
  
  ##-----------------------------##
  ##  D9.  Separate columns      ##
  ##-----------------------------##
  source("panels/D9_SeparateColumns/1_separate.columns.ui.R", local = TRUE)
  source("panels/D9_SeparateColumns/2_separate.columns.server.R", local = TRUE)
  
  ##-----------------------------##
  ##  D10. Unite columns         ##
  ##-----------------------------##
  source("panels/D10_UniteColumns/1_unite.columns.ui.R", local = TRUE)
  source("panels/D10_UniteColumns/2_unite.columns.server.R", local = TRUE)
  
  ##-----------------------------##
  ##  D11. Merge/Join Datasets   ##
  ##-----------------------------##
  source("panels/D11_MergeJoinDatasets/1_mergejoin.datasets.ui.R", local = TRUE)
  source("panels/D11_MergeJoinDatasets/2_mergejoin.datasets.server.R", local = TRUE)

  ##-----------------------------##
  ##  D5. Restore data           ##
  ##-----------------------------##
  source("panels/D5_RestoreData/1_restore.data.ui.R", local = TRUE)
  source("panels/D5_RestoreData/2_restore.data.server.R", local = TRUE)
  
  ##-----------------------------##
  ##  D6. Create design (beta)   ##
  ##-----------------------------##
  source("panels/D6_SurveyDesign/1_survey.design.ui.R", local = TRUE)
  source("panels/D6_SurveyDesign/2_survey.design.server.R", local = TRUE)
  
  ##-----------------------------##
  ##  D7. Alphabetise Variables  ##
  ##-----------------------------##
  source("panels/D7_AlphabetiseVariables/1_alphabetise.variables-ui.R", local = TRUE)
  source("panels/D7_AlphabetiseVariables/2_alphabetise.variables-server.R", local = TRUE)
  
  ##-----------------------------##
  ##  E0. Convert to Categorical ##
  ##-----------------------------##
  source("panels/E0_ConvertToCategorical/1_convert.to.categorical.ui.R", local = TRUE)
  source("panels/E0_ConvertToCategorical/2_convert.to.categorical.server.R", local = TRUE)
  
  ##-----------------------------##
  ##  E1. Categorical Variables  ##
  ##-----------------------------##
  source("panels/E1_CategoricalVariables/1_categorical.variables.ui.R", local = TRUE)
  source("panels/E1_CategoricalVariables/2_categorical.variables.server.R", local = TRUE)
  
  ##-----------------------------##
  ##  E2. Numeric Variables      ##
  ##-----------------------------##
  source("panels/E2_NumericVariables/1_numeric.variables.ui.R", local = TRUE)
  source("panels/E2_NumericVariables/2_numeric.variables.server.R", local = TRUE)
  
  ##-----------------------------##
  ##  E3. Rename Variables       ##
  ##-----------------------------##
  source("panels/E3_RenameVariables/1_rename.variables.panel.ui.R", local = TRUE)
  source("panels/E3_RenameVariables/2_rename.variables.server.R", local = TRUE)
  
  ##-----------------------------##
  ##  E4. Create Variables       ##
  ##-----------------------------##
  source("panels/E4_CreateVariables/1_create.variables.panel.ui.R", local = TRUE)
  source("panels/E4_CreateVariables/2_create.variables.panel.server.R", local = TRUE)
  
  ##-----------------------------##
  ##  E5. Missing to categorical ##
  ##-----------------------------##
  source("panels/E5_MissingToCategorical/1_missing.categorical.panel.ui.R", local = TRUE)
  source("panels/E5_MissingToCategorical/2_missing.categorical.panel.server.R", local = TRUE)
  
  ##-----------------------------##
  ##  E6. Add Columns            ##
  ##-----------------------------##
#  source("panels/E6_AddColumns/1_add.columns.panel.ui.R", local = TRUE)
#  source("panels/E6_AddColumns/2_add.columns.panel.server.R", local = TRUE)

  ##-----------------------------##
  ##  E7. Reshape dataset        ##
  ##-----------------------------##
#  source("panels/E7_ReshapeDataset/1_reshape.data.panel.ui.R", local = TRUE)
#  source("panels/E7_ReshapeDataset/2_reshape.data.panel.server.R", local = TRUE)

  ##-----------------------------##
  ##  E8. Remove columns         ##
  ##-----------------------------##
  source("panels/E8_RemoveColumns/1_remove.columns.panel.ui.R", local = TRUE)
  source("panels/E8_RemoveColumns/2_remove.columns.panel.server.R", local = TRUE)
  
  ##-----------------------------##
  ##  E9. Dates and Times        ##
  ##-----------------------------##
  source("panels/E9_DatesTimes/1_datestimes.ui.R", local = TRUE)
  source("panels/E9_DatesTimes/2_datestimes.server.R", local = TRUE)

  ##-----------------------------##
  ##  F1. Qick explore           ##
  ##-----------------------------##
  source("panels/F1_QuickExplore/1_quick.explore.ui.R", local = TRUE)
  source("panels/F1_QuickExplore/2_quick.explore.server.R", local = TRUE)

#   Advanced --> Time Series

  ##----------------------##
  ##  Time Series Module  ##
  ##----------------------##
  source("panels/F2_TimeSeries/1_timeseries-panel-ui.R", local = TRUE)
  source("panels/F2_TimeSeries/2_timeseries-panel-server.R", local = TRUE)

  #   Advanced --> Model Fitting
  
  ##------------------------##
  ##  Model Fitting Module  ##
  ##------------------------##
  source("panels/F3_ModelFitting//1_modelFitting.panel.ui.R", local = TRUE)
  source("panels/F3_ModelFitting//2_modelfitting-panel-server.R", local = TRUE)
  
  #   Advanced --> Maps
  
  ##---------------##
  ##  Maps Module  ##
  ##---------------##
  source("panels/F4_Maps//1_maps.panel-ui.R", local = TRUE)
  source("panels/F4_Maps//2_maps.panel-server.R", local = TRUE)

#   Help

#     ##---------------##
#     ##  Help Module  ##
#     ##---------------##
#     source("panels/7_Help/1_help-panel-ui.R", local = TRUE)
#     output$help.panel <- renderUI({
#         help.panel.ui(get.lite.version(),get.lite.update())
#     })
})
