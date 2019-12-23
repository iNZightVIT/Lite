###-------------------------------------------------------###
###  Server Functions for the "Multiple Response" Module  ###
###-------------------------------------------------------###
###
###
###  Please consult the comments before editing any code.
###
###
###  * Note: This is to be sourced within "server.R" *



## initialize gui
output$multiple.response <- renderUI({
  MultipleResponse.panel.ui(get.data.set())
})


isBinary = function(x) {
  ## NAs are ignored as they are handled by MR
  tab = table(x, useNA = "no")[table(x)!=0]
  n   = length(names(tab))
  ## if not binary, return FALSE
  if (n != 2) { return(FALSE) }
  ## regular expressions for "yes, no, 0, 1, true, false"
  re1 = "([Yy][Ee][Ss])|([Nn][Oo])|([Yy])|([Nn])"
  re2 = "(0)|(1)"
  re3 = "([Tt][Rr][Uu][Ee])|([Ff][Aa][Ll][Ss][Ee])|([Tt])|([Ff])"
  re  = paste(re1, re2, re3, sep = "|")
  ## do those patterns match?
  l = grepl(re, names(tab))
  ## do BOTH binary values match the patterns?
  return(all(l))
}

getVars = function(data) {
  which(apply(data, 2, function(x) isBinary(x)))
}


output$mr.var <- renderUI({
  get.data.set()
  isolate({
    binaryVar <- getVars(get.data.set())
    vars <- names(get.data.set())
    if (length(binaryVar) == 0) {
      shinyalert(
        title = "No Binary Variables",
        text = "Unable to find any binary variables. Code any variables as: ['yes', 'no'] or [0,1] to use this module.",
        type = "error")
    } else {
      selectInput("mr.select.var",
                  label = "Select related variables: ",
                  choices = vars[binaryVar],
                  multiple = T,
                  selectize = F,
                  size = 18)
    }
  })
})

output$mr.sub1 <- renderUI({
  input$mr.select.var
  isolate({
    if(length(req(input$mr.select.var)) > 1) {
      selectInput("mr.select.sub.var1", label = "Select subset variable 1:", 
                  choices = c(" ", names(get.data.set())),
                  selectize = F)
    }
  })
})


output$mr.sub2 <- renderUI({
  input$mr.select.var
  input$mr.select.sub.var1
  isolate({
    if(length(req(input$mr.select.var)) > 1 && req(input$mr.select.sub.var1) != " ") {
      selectInput("mr.select.sub.var2", label = "Select subset variable 2:", 
                  choices = c(" ", names(get.data.set())),
                  selectize = F)
    }
  })
})

output$mr.box <- renderUI({
  input$mr.select.var
  input$mr.select.sub.var1
  input$mr.select.sub.var2
  isolate({
    if(length(req(input$mr.select.var)) > 1 && req(input$mr.select.sub.var1) != " " && 
       req(input$mr.select.sub.var2) != " ") {
      checkboxInput("mr.box.side", label = "Display subset variable 1 Side-by-side", value = FALSE)
    }
  })
})