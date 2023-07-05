## ----------------------------------------###
###  UI Functions for the "About" Module  ###
### ---------------------------------------###
###
###  Date Created  : January 25, 2015.
###  Last Modified : February 25, 2015.
###
###  Please consult the comments before editing any code.
###
###  * Note: This file is to be sourced locally within "server.R" *

about.panel.ui <- function(lite.version, lite.update) {
  ##  We manually set the page width to be 10 columns so that it looks
  ##  (rougly) centred.
  fixedPage(
    column(
      style = "font-size: 20px; line-height: 1.4; max-width: 40em; margin-bottom: 6em;",
      width = 12, offset = 0,
      img(src = "/inzight_lite_logo_web.svg", height = 80),
      br(), br(),
      ##  We add version details at the bottom right hand corner
      ##  of the page. "lite.version" is defined in the file
      ##  "global.R" - this is because the version number is also
      ##  included in the "help" page, and it would take far too
      ##  much effort to update the version number more than once.
      ##  Fortune favours the lazy. The same logic applies to
      ##  "lite.update".
      if (lite.version != "" || lite.update != "") {
        p_args <- list()
        if (lite.version != "") {
          p_args <- c(p_args, list("iNZight Lite Version: ", em(lite.version), br()))
        }
        if (lite.update != "") {
          p_args <- c(p_args, list("Last updated: ", em(lite.update)))
        }

        change_log <- list(br(), actionLink(inputId = "change_log_link", label = "CHANGELOG"))
        p_args <- c(
          p_args,
          change_log,
          list(style = "color: gray")
        )
        do.call(p, p_args)
      },
      ##  We include the markdown document that contains the
      ##  text for the "About" module.
      includeMarkdown("panels/A1_About/4_about-panel-text.md")
      # actionButton("disconnect", "Disconnect the app")
    ),

    ##  Next, we add a helpful footer.
    column(
      width = 12,
      HTML(
        glue::glue(
          '<div id = "wrapper">
		        <div id = "footer">
		          <div class = "footer-info-group">
                <div class = "footer-links">
                  <!-- Direct users to the iNZight website -->
                  <a href = "https://inzight.nz">
                    iNZight Project
                  </a>
                  <span>|</span>
                  <!--  Direct users to the source code  -->
                  <a href = "https://github.com/iNZightVIT/Lite/">
                    R Source Code
                  </a>
                  <span>|</span>
                  <a href="https://inzight.nz/about/sponsors">Sponsors</a>
                  <span>|</span>
                  <!--  Contact link. Excuse the line overlap. -->
                  <a href = "mailto:inzightlite_support@stat.auckland.ac.nz?Subject=iNZight-Lite%20Feedback" target = "_top">
                    Contact Us
                  </a>
                </div>
                <div>
                  <!-- Copyright Statement -->
                  Copyright 2015-{as.integer(format(Sys.Date(), "%Y"))} iNZight | All Rights Reserved
                </div>
		          </div>

              <div class="footer-sponsor-group">
                <div class="footer-sponsor-label">Active development supported by</div>
                <!-- Links to sponsors -->
                <div class="footer-sponsors">
                  <a href = "https://stat.auckland.ac.nz">
                    <img src = "uoa_logo.png">
                  </a>
                  <a href = "https://terourou.org">
                    <img src = "https://terourou.org/img/logo.png">
                  </a>
                  <a href = "https://inzight.co.nz">
                    <img src = "https://inzight.co.nz/inzight-light.png"/>
                  </a>
                </div>
            </div>
          </div>
	      </div>'
        )
      )
    )
  )
}

logs.panel.ui <- function(logs) {
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "log_file",
        "Select a log file",
        choices = c("", logs),
        selected = ""
      )
    ),
    mainPanel(
      h3("Session"),
      tableOutput("log_session"),
      h3("Inputs"),
      tableOutput("log_inputs"),
      h3("Errors"),
      tableOutput("log_errors"),
      h3("Outputs"),
      tableOutput("log_outputs")
    )
  )
}
