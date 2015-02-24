about.panel = function() {
    fixedPage(
        column(width = 10, offset = 1,
               div(class = "page-spacer",
                   includeMarkdown('gui-elements/notes/about.intro.md'),
                   br(),
                   br(),
                   br(),
                   br(),
                   p(em('Last Updated: 24/02/15'),
                     style = 'color:gray',
                     align = 'right')
                   )
               ),
        column(width = 12,
               HTML(
            '<div id = "wrapper">
		<div id = "footer">
		  <span style = "float:left;">
                  <br> &nbsp;
		    <a href = "https://www.stat.auckland.ac.nz/~wild/iNZight/">
		      iNZight Project
		    </a> |
                    <a href = "https://github.com/iNZightVIT/Lite/">
                      R Source Code
                    </a> |
		    <a href = "mailto:cpar137@aucklanduni.ac.nz?Subject=iNZight-Lite%20Feedback"
		       target = "_top">
		      Contact Us
		    </a>
                    <br><br>
                      &nbsp; Copyright 2015 iNZight | All Rights Reserved
		  </span>
		  <span style = "float:right;">
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
               ))
    )
}

