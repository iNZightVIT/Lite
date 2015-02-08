about.panel = function() {
    fixedPage(
        column(width = 10, offset = 1,
               div(class="page-spacer",
                   includeMarkdown('gui-elements/notes/about.intro.md'),
                   br(),
                   br(),
                   br(),
                   br(),
                   p(em('Last Updated: 03/02/15'),
                     style = 'color:gray',
                     align = 'right')
                   )
               )
    )
}

