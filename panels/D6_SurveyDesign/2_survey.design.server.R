
output$survey.design = renderUI({
  create.design.panel(get.data.set())
})


setDesign = function(strata = NULL, clus1 = NULL, clus2 = NULL,
                     wt = NULL, nest = NULL, fpc = NULL,
                     repweights = NULL, reptype = NULL,
                     scale = NULL, rscales = NULL,
                     poststrat = NULL, name,
                     type = c("survey", "replicate")) {
  if (is.null(strata) & is.null(clus1) & is.null(clus2) &
      is.null(wt) & is.null(nest) & is.null(fpc) &
      is.null(repweights) & is.null(poststrat)) {
    list(dataDesign = NULL,
         dataDesignName = name)
  } else {
    dataDesign = 
      switch(type,
             "survey" = list(
               strata = strata,
               clus1  = clus1,
               clus2  = clus2,
               wt     = wt,
               fpc    = fpc,
               nest   = nest,
               poststrat = poststrat,
               type = type
             ),
             "replicate" = list(
               wt = wt,
               repweights = repweights,
               reptype = reptype,
               scale = scale,
               rscales = rscales,
               poststrat = poststrat,
               type = type
             )
      )
    dataDesignName =
      sprintf("%s.%s",
              name,
              switch(type, "survey" = "svy", "replicate" = "repsvy")
      )
    list(dataDesign = dataDesign,
         dataDesignName = dataDesignName)
  }
}

createSurveyObject = function(design) {
  des <- design$dataDesign
  
  weights <- if (is.null(des$wt)) "NULL" else paste("~", des$wt)
  if (des$type == "survey") {
    id <- if (is.null(des$clus1) & is.null(des$clus2)) {
      "~ 1"
    } else if (is.null(des$clus1)) {
      paste("~", des$clus2)
    } else if (is.null(des$clus2)) {
      paste("~", des$clus1)
    } else {
      paste("~", des$clus1, "+", des$clus2)
    }
    
    strata <- if (is.null(des$strata)) "NULL" else paste("~", des$strata)
    fpcs <- if (is.null(des$fpc)) "NULL" else paste("~", des$fpc)
    obj <-
      parse(text =
              paste0(
                "survey::svydesign(",
                "id = ", id, ", ",
                if (!is.null(des$strata)) sprintf("strata = %s, ", strata),
                if (!is.null(des$wt) || !is.null(des$freq))
                  sprintf("weights = %s, ", weights),
                if (!is.null(des$fpc)) sprintf("fpc = %s, ", fpcs),
                if (!is.null(des$nest) && des$nest) "nest = TRUE, ",
                "data = get.data.set())"
              )
      )
  } else {
    ## replicate weights specified
    repweights <- if(is.null(des$repweights)) "NULL"
    else paste("~", paste(des$repweights, collapse = " + "))
    type <- des$reptype
    rscales <- if (is.null(des$rscales)) "NULL"
    else sprintf("c(%s)", paste(des$rscales, collapse = ", "))
    obj <-
      parse(text =
              paste0("survey::svrepdesign(",
                     if (!is.null(des$wt))
                       sprintf("weights = %s, ", weights),
                     sprintf("repweights = %s, ", repweights),
                     sprintf("type = '%s', ", type),
                     if (!is.null(des$scale))
                       sprintf("scale = %s, ", des$scale),
                     if (!is.null(des$rscales))
                       sprintf("rscales = %s, ", rscales),
                     "data = dataSet)"
              )
      )
  }
  
  if (!is.null(des$poststrat)) {
    design_obj <- eval(obj)
    ## Note: if allowing continuous variables in future,
    ##       this needs a better name:
    pop.totals <- structure(
      do.call(c,
              c(
                list(sum(des$poststrat[[1]]$Freq)),
                lapply(des$poststrat, function(df) df$Freq[-1])
              )
      ),
      .Names = do.call(c,
                       c(
                         list("(Intercept)"),
                         lapply(des$poststrat, function(df)
                           paste0(names(df)[1], as.character(df[-1,1]))
                         )
                       )
      )
    )
    obj <- parse(
      text = sprintf(
        "survey::calibrate(design_obj, ~%s, pop.totals)",
        paste(names(des$poststrat), collapse = " + ")
      )
    )
  }
  
  eval(obj)
}


svalue_or_null <- function(x) {
  if (x == " ") return(NULL)
  x
}


design_param = reactive({
  input$create.design
  isolate({
    if(req(input$svytype) == "survey" && req(input$create.design) > 0) {
      strat <- svalue_or_null(input$stratVar)
      clus1 <- svalue_or_null(input$clus1Var)
      clus2 <- svalue_or_null(input$clus2Var)
      wts <- svalue_or_null(input$wtVar)
      fpc <- svalue_or_null(input$fpcVar)
      nest <- as.logical(input$nestChk)
      name <- values$data.name
      clear <- is.null(input$strat) && is.null(input$clus1) &&
        is.null(input$clus2) && is.null(input$wts) && is.null(input$fpc)
      setDesign(
        strata = strat,
        clus1 = clus1,
        clus2 = clus2,
        wt = wts,
        nest = nest,
        fpc = fpc,
        type = "survey",
        name = name
      )
    }
  })
})


create.Survey.Object = reactive({
  req(design_param())
  createSurveyObject(design_param())
})

