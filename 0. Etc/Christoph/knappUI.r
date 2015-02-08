shinyUI(
  fluidPage(
    theme = "iNZight.css",
    div(class="image-header",img(src="images/inzight.logo.png")),
    navbarPage("iNZight-Online", id="selector",
               inverse=T,
               collapsable=F,
               fluid=T,
               responsive=T,
               tabPanel("About",uiOutput('about.panel')),
               # gui elements of the data import panel
               navbarMenu("Data",id="data",
                          tabPanel("Current Data",uiOutput('current.data')), # presents the current loaded data set
                          tabPanel("Switch Data",uiOutput('switch.data.panel')), # presents all loaded data sets and enables to switch between them. See gui-elements/switch.data.panel.R
                          tabPanel("Load Data",uiOutput('load.data.panel')), # enables to load a user data set
                          tabPanel("Remove Data",uiOutput("remove.data.panel")) # remove previously uploaded data sets
                          ),
               navbarMenu("Modify data",
                          tabPanel("Transform columns",uiOutput('transform.columns')), # perform transormation operation on columns
                          tabPanel("Reorder Levels",uiOutput('reorder.levels')), # reorder the levels of a factor variable
                          tabPanel("Compare dates",uiOutput("compare.dates")), # get the difference in days for two date variables
                          tabPanel("Add columns",uiOutput("add.columns")), # add a column to a data frame
                          tabPanel("Remove columns",uiOutput("remove.columns")) # remove columns from the selected data
                          ),
               navbarMenu("Quick Explore",
                          tabPanel("Data Summary",uiOutput("quick.summary")), # provides a summary of the selected data
                          tabPanel("Single column plot",uiOutput("single.column.plot")), # plots every column in the selected data
                          tabPanel("Column Pair plot",uiOutput("column.pair.plot")), # plots every pair of column in the selected data
                          tabPanel("Compare pairs",uiOutput("matrix.plot")) # all selected columns are plotted in a matrix like format
                          ),
               navbarMenu("iNZight plots",
                          tabPanel("Graphics",uiOutput("advanced.graphics")), # 
                          tabPanel("Plot Summary") # 
               )
             ),
    div(class="page-footer",img(src="images/inzight.logo.footer.png"))
    )
)