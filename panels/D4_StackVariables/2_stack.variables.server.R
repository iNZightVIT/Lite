##  Row operations (Perform row operations) --> Stack variables

observe({
  input$stack_vars
  isolate({
    if (!is.null(input$stack_vars) && input$stack_vars > 0 &&
        !is.null(input$stack_vars_column)) {
      temp = iNZightTools::stackVars(get.data.set(), input$stack_vars_column)
      if (!is.null(temp)) {
        ## save code
        code.save$dataname = paste(code.save$name, "stacked", sep = ".")
        code = tidy_assign_pipe(gsub(
          "get.data.set\\()",
          code.save$name,
          iNZightTools::code(temp)
        ))
        code = do.call(paste, c(as.list(code), sep = ""))
        code = do.call(c, lapply(code, function(x) {
          y <- try({
            formatR::tidy_source(
              text = x,
              width.cutoff = 80,
              output = F,
              indent = 4
            )$text.tidy
          }, silent = TRUE)
          if (inherits(y, "try-error"))
            x
          else
            c(y, "\n")
        }))
        code = c(paste0(code.save$dataname, " <- \n"), code)
        code.save$variable = c(code.save$variable, list(c("\n", code)))
        ## save data
        updatePanel$datachanged = updatePanel$datachanged + 1
        values$data.set = temp
        code.save$name = code.save$dataname
        values$data.name = code.save$dataname
        #       updateSelectInput(session,"stack_vars_which",selected=0)
        updateSelectInput(
          session,
          "stack_vars_column",
          choices = get.numeric.column.names(get.data.set()),
          selected = 0
        )
      }
    }
  })
})

#observe({
#  input$stack_vars_which
#  isolate({
#    if(!is.null(input$stack_vars_which)&&!""%in%input$stack_vars_which){
#      if("categorical"%in%input$stack_vars_which){
#        updateSelectInput(session,inputId="stack_vars_column",
#                          choices=get.categorical.column.names(get.data.set()),
#                          selected=1)
#      }else{
#        updateSelectInput(session,inputId="stack_vars_column",
#                          choices=get.numeric.column.names(get.data.set()),
#                          selected=1)
#      }
#    }
#  })
#})

output$stack.table = renderDataTable({
  get.data.set()
}, options = list(
  lengthMenu = c(5, 30, 50),
  pageLength = 5,
  columns.defaultContent = "NA",
  scrollX = T
))

output$stack.variables = renderUI({
  stack.variables.panel()
})
