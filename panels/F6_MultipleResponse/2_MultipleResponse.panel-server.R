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

