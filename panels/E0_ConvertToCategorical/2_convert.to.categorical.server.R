

observe({
  input$select.to.convert
    if(req(input$select.to.convert != " ")) {
      updateTextInput(session, "convert_to_name", 
                      value = paste0(input$select.to.convert, ".cat"))
    }
})





observe({
  input$convert_to_categorical_button
  isolate({
    if(!is.null(input$select.to.convert) && input$select.to.convert != ""
       && !is.null(input$convert_to_name) && !grepl("^\\s*$", input$convert_to_name)) {
      orgVar = input$select.to.convert
      name = gsub('\\n+', "", input$convert_to_name, perl = TRUE)
      temp = iNZightTools::convertToCat(get.data.set(), orgVar, name)
      updatePanel$datachanged = updatePanel$datachanged+1
      values$data.set = temp
      ## code history
      code = tidy_assign_pipe(gsub("get.data.set\\()", code.save$name, iNZightTools::code(values$data.set)))
      code.save$variable = c(code.save$variable, list(c("\n", code, "\n")))
    }
  })
})



output$convert.to.categorical.table = renderDT({
  input$convert_to_categorical_button
  values$data.set
},options = list(lengthMenu = c(5, 30, 50), pageLength = 5, columns.defaultContent = "NA",scrollX = T))



output$convert.to.categorical = renderUI({
  convert.to.categorical.panel()  
})

