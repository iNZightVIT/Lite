##----------------------------------------###
###  UI Functions for the "About" Module  ###
###---------------------------------------###
###
###  Date Created  : January 25, 2015.
###  Last Modified : February 25, 2015.
###
###  Please consult the comments before editing any code.
###
###  * Note: This file is to be sourced locally within "server.R" *

about.panel.ui = function(lite.version,lite.update) {
    ##  We manually set the page width to be 10 columns so that it looks
    ##  (rougly) centred.
    fixedPage(
        column(
            width = 10, offset = 1,
            ##  We include the markdown document that contains the
            ##  text for the "About" module. 
            includeMarkdown("panels/1_About/4_about-panel-text.md"),
            ##  We include some line breaks to space things out.
            br(),
            br(),
            br(),
            br(),
            ##  We add version details at the bottom right hand corner
            ##  of the page. "lite.version" is defined in the file
            ##  "global.R" - this is because the version number is also
            ##  included in the "help" page, and it would take far too
            ##  much effort to update the version number more than once.
            ##  Fortune favours the lazy. The same logic applies to
            ##  "lite.update".
            p(em(lite.version),
              br(),
              em(lite.update),
              style = 'color:gray',
              align = 'right')
        ),
       
        ##  Next, we add a helpful footer.
        column(
            width = 12,
            HTML(
                '<div id = "wrapper">
		<div id = "footer">
		  <span style = "float:left;">
                  <br> &nbsp;
                    <!-- Direct users to the iNZight website -->
		    <a href = "https://www.stat.auckland.ac.nz/~wild/iNZight/">
		      iNZight Project
		    </a> |
                    <!--  Direct users to the source code  -->
                    <a href = "https://github.com/iNZightVIT/Lite/">
                      R Source Code
                    </a> |
                    <!--  Contact link. Excuse the line overlap. -->
		    <a href = "mailto:cpar137@aucklanduni.ac.nz?Subject=iNZight-Lite%20Feedback" target = "_top">
		      Contact Us
		    </a>
                    <br>
                    <br>
                    <!-- Copyright Statement -->
                      &nbsp; Copyright 2015 iNZight | All Rights Reserved
		  </span>
		  <span style = "float:right;">
                    <!-- Links to sponsors -->
                    <a href = "http://new.censusatschool.org.nz/">
		      <img src = "census_logo.png"/, height = 75>
		    </a> &nbsp; &nbsp;
                    <a href = "http://www.stats.govt.nz/">
		      <img src = "stats_nz.png"/, height = 75>
		    </a> &nbsp; &nbsp;
                    <a href = "http://www.minedu.govt.nz/">
		      <img src = "minedu_logo.png"/, height = 60, width = 170>
		    </a> &nbsp; &nbsp;
		    <a href = "http://stat.auckland.ac.nz">
                      <img src = "uoa_logo.png", height = 65>
                    </a> &nbsp;
		  </span>
		</div>
	      </div>'
            )
        )
    )
}

