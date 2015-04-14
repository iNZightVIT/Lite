get.form.class.interval.side = function(data.set){
  list(helpText("Please select a column and specify the number of intervals and the 
                method of forming class intervals. The output can be poduced in two 
                different formats. See help for more information."),
       selectInput(inputId="form.class.interval.column.select",
                   label="Form Class interval",
                   choices=get.numeric.column.names(data.set),
                   selected=1),
       textInput(inputId="form_class_interval_number",
                 label="Number of intervals",
                 value = 2),
       checkboxInput("form.class.interval.format",
                     label="Uncheck to change the format from (open left, closed right]
                     to [closed left, open right)",
                     value =T),
       selectInput(inputId="form_class_interval_method_select",
                   label="Select method to form Class intervals",
                   choices=c(c("equal.width",
                               "equal.count",
                               "specified")),
                   selected=1),
       conditionalPanel("input.form_class_interval_method_select=='specified'",
                        uiOutput("specified.range")),
       checkboxInput("form_class_interval_labels_provide",label="Provide custom labels for the intervals."),
       conditionalPanel("input.form_class_interval_labels_provide==true",
                        uiOutput("labels.provide")),
       actionButton(inputId="form.class.interval.submit",
                    label="Form Class interval"),br(),br(),
       help.display('Form Class interval','form_class_interval',
                    "gui-elements/notes/form.class.interval.md")
       )
}

get.form.class.interval.main = function(){
  dataTableOutput("form.class.interval.table")
}