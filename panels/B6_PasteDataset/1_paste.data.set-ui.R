# puts together a list of shiny widgets to fill the sidebar


paste.data.main <- function(data.set) {
  if (is.null(data.set)) {
    list(
      div(class = "page-divider"),
      uiOutput("paste.data.info"),
      br(),
      uiOutput("paste.view.title"),
      DTOutput("paste.table.preview"),
      div(class = "page-divider")
    )
  }
}

# switch.data.panel creates reactive panel for input files
paste.data.panel <- function(data.set) {
  sidebarLayout(
    sidebarPanel(
      useShinyjs(),
      textAreaInput("paste.data.area",
        label = "Copy data from Excel/Google Sheets/etc and paste below:",
        value = "",
        height = "250px", placeholder = "Paste data here"
      ),
      radioButtons("paste.delimiter",
        label = "Choose delimiter (or type one) :",
        choices = list(
          "tab (    )" = "\t", "comma (,)" = ",",
          "semicolon (;)" = ";", "type delimiter"
        ),
        selected = "\t"
      ),
      uiOutput("paste.type.delimiter"),
      hr(),
      br(),
      fixedRow(
        column(3, actionButton("paste.load", "Load")),
        column(3, actionButton("paste.reset", "Reset"))
      ),
      br(),
      br(),
      br(),
      HTML("&nbsp;"), br()
    ),
    mainPanel(paste.data.main(data.set))
  )
}
