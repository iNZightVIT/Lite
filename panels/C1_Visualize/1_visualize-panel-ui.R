###  Last Modified : March 12, 2017.
###---------------------------------------------###
###  User Interface for the "Visualize" Module  ###
###---------------------------------------------###
###
###  Date Created  : January 25, 2015.
###  Last Modified : Feb 15, 2019.
###
###  The UI is divided into two panels:
###
###     1.  Sidebar Panel : contains all the user inputs.
###     2.  Main Panel    : contains all the outputs.
###
###  Please consult the comments before editing any code.
###
###  * Note: Thie file is to be sourced locally within "server.R" *


###-----------------###
###  Sidebar Panel  ###
###-----------------###
###
###  First, we set up the "help" functionality for this module.
visualize.help = function() {
  help.display("Visualize Module",
               "Visualize",
               "panels/C1_Visualize/3_visualize-panel-help.md")
}


###  Next, we set up the sidebar panel with "vis.sidebarPanel()".
vis.sidebarPanel = function() {
  #  conditionalPanel(condition = "output.showsidebar == 1",
  sidebarPanelUI = list(
    tabsetPanel(
      id = "visualize_sidebar_tabs",
      type = "pills",
      
      tabPanel(
        title = "Select Variables",
        
        fixedRow(column(9, hr()),
                 column(3, align = "left",
                        conditionalPanel(condition = "output.showsidebar == 1",
                                         actionButton("hideSidebar", 
                                                      icon("arrow-circle-left", "fa-2x"),
                                                      style = "color: #337ab7; 
                                                                                background-color: #ffffff;  
                                                                                border-color: #ffffff;
                                                                                padding:4px; 
                                                                                font-size:100%")))),
        
        h5(strong("Variable selection")),
        
        #br(),
        
        #fixedRow(column(10,h5(strong("Variable selection"))),
        #         column(2,checkboxInput("change_var_selection",
        #                                value=F,
        #                                label=""))),
        
        ##  Select the first variable.
        h5("Select first variable:"),
        fixedRow(column(8, uiOutput("vari1_panel")),
                 column(2, actionButton("switch1", "", 
                                        icon = icon("arrow-down", "fa-2x"),
                                        style="color: #337ab7; 
                                                                background-color: #ffffff;  
                                                                border-color: #ffffff;
                                                                padding:4px; 
                                                                font-size:60%"))),
        
        #uiOutput("vari1_panel"),
        
        ##  Select the second variable.
        h5("Select second variable:"),
        fixedRow(column(8, uiOutput("vari2_panel")),
                 column(2, actionButton("switch2", "", 
                                        icon = icon("arrow-down", "fa-2x"),
                                        style="color: #337ab7; 
                                                                background-color: #ffffff;  
                                                                border-color: #ffffff;
                                                                padding:4px; 
                                                                font-size:60%"))),
        
        #uiOutput("vari2_panel"),
        
        fixedRow(column(10, hr())),
        
        
        ## Select desired subset for the first variable.
        h5("Subset by:"),
        fixedRow(column(8, uiOutput("subs1_panel")),
                 column(2, actionButton("switch3", "", 
                                        icon = icon("arrow-down", "fa-2x"),
                                        style="color: #337ab7; 
                                                                background-color: #ffffff;  
                                                                border-color: #ffffff;
                                                                padding:4px; 
                                                                font-size:60%"))),
        
        #uiOutput("subs1_panel"),
        
        ##  Select desired subset for the second variable.
        h5("Subset by:"),
        fixedRow(column(8, uiOutput("subs2_panel"))),
        #uiOutput("subs2_panel"),
        
        fixedRow(column(10, hr())),
        
        #actionButton(inputId = "go.to.old",
        #             label = "REVERT To Old Version",
        #             icon("paper-plane"), 
        #             style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
        
        #br(),
        #br(),
        
        actionButton(inputId = "reset.graphics",
                     label = "Reset To Default",
                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
        
        br(),
        br(),
        
        visualize.help()
      ),
      
      tabPanel(
        title = "Add To Plot",
        id = "add.to.plot",
        fixedRow(column(9, hr()),
                 column(3, conditionalPanel(condition = "output.showsidebar == 1",
                                            actionButton("hideSidebar2", 
                                                         icon("arrow-circle-left", "fa-2x"),
                                                         style = "color: #337ab7; 
                                                                          background-color: #ffffff;  
                                                                          border-color: #ffffff;
                                                                          padding:4px; 
                                                                          font-size:100%")))),
        
        
        #        fixedRow(column(8,h4("Inference")),
        #                 column(2,
        #                        checkboxInput("toggle_inference",
        #                                      label="",
        #                                      value=input$toggle_inference))),
        #        fixedRow(column(width = 8,
        #                        uiOutput("add_inference"))),
        
        #        hr(),
        
        #        h4("Select Additions"),
        
        uiOutput("select_additions_panel"),
        
        conditionalPanel(
          condition = "input.select_additions=='Customise Plot Appearance'",
          uiOutput("plot.appearance.panel.title"),
          uiOutput("plot.appearance.panel"),
          uiOutput("code.variables.panel")
        ),
        
        conditionalPanel(
          condition = "input.select_additions=='Trend Lines and Curves'",
          
          uiOutput("trend.curve.panel"),
          uiOutput("join.points.panel"),
          uiOutput("xy.line.panel")
        ),
        
        conditionalPanel(
          condition = "input.select_additions=='Axes and Labels'",
          
          uiOutput("customize.labels.panel"),
          uiOutput("add.jitter.panel"),
          uiOutput("add.rugs.panel"),
          uiOutput("adjust.axis.panel"),
          uiOutput("adjust.number.bars.panel")
        ),
        
        conditionalPanel(
          condition = "input.select_additions=='Identify Points'",
          
          uiOutput("points.identify.panel")
        ),
        
        conditionalPanel(
          condition = "input.select_additions=='Add Inference Information'",
          
          #          fixedRow(column(8,h4("Inference")),
          #                   column(2,
          #                          checkboxInput("toggle_inference",
          #                                        label="",
          #                                        value=TRUE))),
          fixedRow(column(width = 8,
                          uiOutput("add_inference")))
        )
      )
    )
  )
  #                   )
}



###  We set up the main panel.
vis.mainPanel = function() {
  toggle_advanced_options = T
  if(!is.null(input$toggle_advanced_options)) {
    toggle_advanced_options = input$toggle_advanced_options
  }
  panel = list(
    ## action button: hide/show sidebar menu
    #    conditionalPanel(condition = "output.showsidebar == 1",
    #                     actionButton("hideSidebar", 
    #                                  icon("arrow-circle-left", "fa-2x"),
    #                                  style = "color: #337ab7; 
    #                                           background-color: #ffffff;  
    #                                           border-color: #ffffff;
    #                                           padding:4px; 
    #                                           font-size:100%")),
    
    #    conditionalPanel(condition = "output.showsidebar == 0",
    #                     actionButton("showSidebar", 
    #                                  icon("arrow-circle-right", "fa-2x"),
    #                                  style = "color: #337ab7; 
    #                                           background-color: #ffffff; 
    #                                           border-color: #ffffff;
    #                                           padding:4px; 
    #                                           font-size:100%")),
    
    #    br(),
    #    br()
    tabsetPanel(
      id = "plot_selector",
      type = "pills",
      ##  Plot Panel
      tabPanel(
        title = "Plot",
        #br(),
        fixedRow(
          column(width = 12,
                 
                 
                 fixedRow(column(2, fixedRow(column(5, 
                                                    conditionalPanel(condition = "output.showsidebar == 0",
                                                                     actionButton("showSidebar", 
                                                                                  icon("arrow-circle-right", "fa-2x"),
                                                                                  style = "color: #337ab7; 
                                                                                     background-color: #ffffff; 
                                                                                     border-color: #ffffff;
                                                                                     padding:4px; 
                                                                                     font-size:100%"))),
                                             column(7, 
                                                    actionButton("refreshplot", 
                                                                 icon("refresh", "fa-2x"),
                                                                 style = "color: #337ab7; 
                                                                          background-color: #ffffff; 
                                                                          border-color: #ffffff;
                                                                          padding:4px; 
                                                                          font-size:90%")))
                 ),
                 column(10, hr())
                 ),
                 
                 helpText("Plots for visualizing data."),
                 plotOutput("visualize.plot"),
                 
                 #                 fixedRow(column(width = 2, offset = 1,
                 #                                 downloadButton(outputId = "saveplot", label = "Save Plot")),
                 #                          column(width = 6,
                 #                                 radioButtons(inputId = "saveplottype", 
                 #                                              label = "Select the file type", 
                 #                                              choices = list("jpg", "png", "pdf", "interactive html"), inline = TRUE)),
                 #                          column(width = 2,
                 #                                 actionButton(inputId = "gotointeractivehtml", 
                 #                                              label = "Interactive HTML"))),
                 
                 br(),
                 
                 fixedRow(column(width = 4, 
                                 # offset = 1,
                                 downloadButton(outputId = "saveplot", label = "Download Plot")),
                          column(width = 5,
                                 radioButtons(inputId = "saveplottype", 
                                              label = strong("Select the file type"), 
                                              choices = list("jpg", "png", "pdf", "svg"), inline = TRUE)),
                          #column(width = 4,
                          #       conditionalPanel(
                          #         condition = "input.saveplottype == 'interactive html'",
                          #         uiOutput("extra.vars.html")
                          #       )),
                          column(width = 3,
                                 uiOutput("add.fitted.residuals.panel"))),
                 
                 
                 #                 downloadButton(outputId = "saveplot", label = "Save Plot"),
                 #                 radioButtons(inputId = "saveplottype", 
                 #                              label = "Select the file type", 
                 #                              choices = list("jpg", "png", "pdf"), inline = TRUE),
                 
                 fixedRow(
                   column(
                     width = 5, offset = 1,
                     conditionalPanel(
                       condition = "input.subs1 != 'none'",
                       ##  Slider input GUI for the first variable.
                       br(),
                       uiOutput("subs1_conditional"),
                       uiOutput("speed_value1")
                     )
                   ),
                   column(
                     width = 5, offset = 1,
                     ##  Slider input GUI for the second variable.
                     conditionalPanel(
                       condition = "input.subs2 != 'none'",
                       br(),
                       uiOutput("subs2_conditional"),
                       uiOutput("speed_value2")
                     )
                   )
                 ),
                 
                 br(),
                 
                 includeMarkdown("panels/C1_Visualize/5_visualize-panel-note.md")
          )
        )
      ),
      ##  Summary Panel
      tabPanel(
        title = "Summary",
        
        fixedRow(column(2, conditionalPanel(condition = "output.showsidebar == 0",
                                            actionButton("showSidebar2", 
                                                         icon("arrow-circle-right", "fa-2x"),
                                                         style = "color: #337ab7; 
                                                         background-color: #ffffff; 
                                                         border-color: #ffffff;
                                                         padding:4px; 
                                                         font-size:100%"))
        ),
        column(10, hr())
        ),
        
        
        #br(),
        helpText("Statistical Summary for the data."),
        verbatimTextOutput("visualize.summary")
      ),
      ##  Inference Panel
      tabPanel(
        title = "Inference",
        
        fixedRow(column(2, conditionalPanel(condition = "output.showsidebar == 0",
                                            actionButton("showSidebar3", 
                                                         icon("arrow-circle-right", "fa-2x"),
                                                         style = "color: #337ab7; 
                                                         background-color: #ffffff; 
                                                         border-color: #ffffff;
                                                         padding:4px; 
                                                         font-size:100%"))
        ),
        column(10, hr())
        ),
        
        #br(),
        
        fixedRow(column(3, uiOutput("inference_type")),
                 column(3, uiOutput("inference_test")),
                 column(6, uiOutput("inference_out"))),
        #fixedRow(column(3, actionButton("confirm_inf_button","Confirm",
        #                                style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"), offset = 3)),
        br(),
        helpText("Statistical Inference for the data."),
        verbatimTextOutput("visualize.inference")
      ),
      tabPanel(value = 1,
               title = "Interactive Plot",
               
               fixedRow(column(2, conditionalPanel(condition = "output.showsidebar == 0",
                                                   actionButton("showSidebar4", 
                                                                icon("arrow-circle-right", "fa-2x"),
                                                                style = "color: #337ab7; 
                                                         background-color: #ffffff; 
                                                         border-color: #ffffff;
                                                         padding:4px; 
                                                         font-size:100%"))
               ),
               column(10, hr())
               ),
               
               #br(),
               uiOutput("interactive.plot.select"),
               # helpText("Interactive Plot"),
               br(),
               htmlOutput("interactive.plot")
               #        br(),
               #        actionButton("popup_html",
               #                     "POP UP",
               #                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
      ),
      tabPanel(value = 2,
               title = "Interactive Plot (via plotly)",
               
               # actionButton("interactiveplotly",
               #              "Refresh",
               #              style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
               uiOutput("plotly_nw"), 
               plotlyOutput("plotly_inter", height = "500px") %>% withSpinner()
      )
    )
  )
  panel
}



###----------------------###
###  Visualize Panel UI  ###
###----------------------###
###
###  We combine the vis.sidebarPanel() and vis.mainPanel() functions to
###  complete the UI for the Time Series module. If no data set has been
###  selected, we display a helpful warning message.

visualize.panel.ui = function(data.set) {
  fluidPage(
    
    useShinyjs(),
    
    if (is.null(data.set)) {
      fixedRow(
        includeMarkdown(
          "panels/C1_Visualize/4_visualize-panel-null.md")
      )
    } 
    else {
      
      fluidRow(
        column(width = 4, id = "Sidebar", vis.sidebarPanel()),
        column(width = 8, id = "Main", vis.mainPanel())
      )
      
      
      
      #      tabPanel(NULL,
      #               div( id ="Sidebar", sidebarPanel(vis.sidebarPanel())),
      #               
      #               
      #               mainPanel(vis.mainPanel())
      #      )
      
    }
  )
}


hidesidebar.visualize.panel.ui = function(data.set) {
  fluidPage(  
    if (is.null(data.set)) {
      fixedRow(
        includeMarkdown(
          "panels/C1_Visualize/4_visualize-panel-null.md")
      )
    } else {
      fluidRow(
        column(12, vis.mainPanel())
      )
    }
  )
}



########## the old version ##########

old.vis.sidebarPanel = function() {
  ##  Note the user of "hr()" (= horizontal rule) to separate
  ##  sections.
  sidebarPanelUI = list(
    hr(),
    fixedRow(column(10,h4("Variable selection")),
             column(2,checkboxInput("change_var_selection",
                                    value=F,
                                    label=""))),
    ##  Select the first variable.
    h5("Select first variable:"),
    uiOutput("vari1_panel"),
    
    ##  Select the second variable.
    h5("Select second variable:"),
    uiOutput("vari2_panel"),
    hr(),
    ## Select desired subset for the first variable.
    h5("Subset by:"),
    uiOutput("subs1_panel"),
    
    ##  Select desired subset for the second variable.
    h5("Subset by:"),
    uiOutput("subs2_panel"),
    hr(),
    ##  Reset graphical parameters.
    radioButtons(inputId = "customize_plot",
                 label =  "Advanced Options: ",
                 choices =
                   c("Hide" = 1,
                     "Show" = 2),
                 selected = 1),
    hr(),
    
    actionButton(inputId = "go.to.new",
                 label = "revert To New Version",
                 icon("paper-plane"), 
                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
    
    br(),
    br(),
    
    actionButton(inputId = "reset.graphics",
                 label = "Reset To Default"),
    
    br(),
    br(),
    
    
    visualize.help()
  )
}

old.vis.mainPanel = function() {
  toggle_advanced_options = T
  if(!is.null(input$toggle_advanced_options)){
    toggle_advanced_options = input$toggle_advanced_options
  }
  panel = list(
    br(),
    tabsetPanel(
      id = "plot_selector",
      type = "pills",
      ##  Plot Panel
      tabPanel(
        title = "Plot",
        br(),
        fixedRow(
          column(width = 12,
                 conditionalPanel(condition = "input.customize_plot == 1",
                                  helpText("Plots for visualizing data."),
                                  plotOutput("visualize.plot"),
                                  
                                  fixedRow(
                                    column(
                                      width = 5, offset = 1,
                                      conditionalPanel(
                                        condition = "input.subs1 != 'none'",
                                        ##  Slider input GUI for the first variable.
                                        br(),
                                        uiOutput("subs1_conditional")
                                      )
                                    ),
                                    column(
                                      width = 5, offset = 1,
                                      ##  Slider input GUI for the second variable.
                                      conditionalPanel(
                                        condition = "input.subs2 != 'none'",
                                        br(),
                                        uiOutput("subs2_conditional")
                                      )
                                    ),
                                    
                                    fixedRow(column(width = 2, offset = 1,
                                                    downloadButton(outputId = "saveplot", label = "Save Plot")),
                                             column(width = 6,
                                                    radioButtons(inputId = "saveplottype", 
                                                                 label = "Select the file type", 
                                                                 choices = list("jpg", "png", "pdf"), inline = TRUE)))
                                    
                                    
                                  )
                 ),
                 
                 conditionalPanel(condition = "input.customize_plot == 2",
                                  fixedRow(
                                    column(
                                      width = 8,
                                      helpText("Plots for visualizing the data."),
                                      plotOutput("mini.plot"),
                                      
                                      
                                      
                                      fixedRow(column(width = 6,
                                                      conditionalPanel(
                                                        condition = "input.subs1 != 'none'",
                                                        uiOutput("subs1_conditional_mini")
                                                      )
                                      ),
                                      column(width = 6,
                                             conditionalPanel(condition = "input.subs2 != 'none'",
                                                              uiOutput("subs2_conditional_mini")
                                             )
                                      )
                                      ),
                                      fixedRow(column(width = 2, offset = 1,
                                                      downloadButton(outputId = "saveplot2", label = "Save Plot")),
                                               column(width = 6,
                                                      radioButtons(inputId = "saveplottype2", 
                                                                   label = "Select the file type", 
                                                                   choices = list("jpg", "png", "pdf"), inline = TRUE)))
                                      
                                    ),
                                    column(
                                      width = 4,
                                      fixedRow(column(8,h4("Inference")),
                                               column(2,
                                                      checkboxInput("toggle_inference",
                                                                    label="",
                                                                    value=input$toggle_inference))),
                                      fixedRow(column(width = 8,
                                                      uiOutput("old_add_inference"))),
                                      fixedRow(column(8,h4("Advanced options")),column(2,
                                                                                       checkboxInput("toggle_advanced_options",
                                                                                                     label="",
                                                                                                     value=toggle_advanced_options))),
                                      conditionalPanel("input.toggle_advanced_options",
                                                       fixedRow(column(width = 12,
                                                                       uiOutput("old_advanced_options_panel"))),
                                                       fixedRow(column(width=12,
                                                                       conditionalPanel("input.advanced_options=='Code more variables'",
                                                                                        uiOutput("code.variables.panel")),
                                                                       conditionalPanel("input.advanced_options=='Add trend curves'",
                                                                                        uiOutput("trend.curve.panel")),
                                                                       conditionalPanel("input.advanced_options=='Add x=y line'",
                                                                                        uiOutput("xy.line.panel")),
                                                                       conditionalPanel("input.advanced_options=='Add a jitter'",
                                                                                        uiOutput("add.jitter.panel")),
                                                                       conditionalPanel("input.advanced_options=='Add rugs'",
                                                                                        uiOutput("add.rugs.panel")),
                                                                       conditionalPanel("input.advanced_options=='Join points by line'",
                                                                                        uiOutput("join.points.panel")),
                                                                       conditionalPanel("input.advanced_options=='Change plot appearance'",
                                                                                        uiOutput("plot.appearance.panel")),
                                                                       conditionalPanel("input.advanced_options=='Identify points'",
                                                                                        uiOutput("points.identify.panel")),
                                                                       conditionalPanel("input.advanced_options=='Customize labels'",
                                                                                        uiOutput("customize.labels.panel")),
                                                                       conditionalPanel("input.advanced_options=='Adjust axis limits'",
                                                                                        uiOutput("adjust.axis.panel")),
                                                                       conditionalPanel("input.advanced_options=='Adjust number of Bars'",
                                                                                        uiOutput("adjust.number.bars.panel"))))))
                                  )
                 )
          )
        )
      ),
      ##  Summary Panel
      tabPanel(
        title = "Summary",
        br(),
        helpText("Statistical Summary for the data."),
        verbatimTextOutput("visualize.summary")
      ),
      ##  Inference Panel
      tabPanel(
        title = "Inference",
        br(),
        selectInput("type.inference.select",
                    choices = c("normal",
                                "bootstrap"),
                    label = "Select type of inference"),
        br(),
        helpText("Statistical Inference for the data."),
        verbatimTextOutput("visualize.inference")
      )
    )
  )
  panel
}


old.visualize.panel.ui = function(data.set) {
  fluidPage(  
    if (is.null(data.set)) {
      fixedRow(
        includeMarkdown(
          "panels/C1_Visualize/4_visualize-panel-null.md")
      )
    } else {
      fixedRow(
        column(4, old.vis.sidebarPanel()),
        column(8, old.vis.mainPanel())
      )
    }
  )
}



























