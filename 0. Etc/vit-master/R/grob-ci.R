## Functions and methods for a visual representation of a confidence
## intervals with arrows and labels providing the numeric
## representation.

#' Draw a ci grob
grid.confint <- function(...)
    grid.draw(confintGrob(...))

#' Construct a ci grob
#'
#' datatextGrob constructs text of a vector of data.
#' datatextGrobs inherit the class "confint".
#'
#' @param ci the confidence interval to be plotted, given by a vector of length 2.
#' @param at a unit object giving the y-axis position to plot the confidence interval.
#' @param to a unit object giving the y-axis positions of where the arrows are to point.
#' @param col the colour to plot the confidence interval.
#' @param bar a logical value determining whether or not a bar is plotted.

confintGrob <- function(ci, at = unit(0.15, "npc"), height = unit(0.01, "npc"),
                        to = unit(-2, "lines"), col = "red", bar = TRUE, cilabs = ci,
                        fix.label.pos = c(FALSE, FALSE), name = NULL, gp = NULL, vp = NULL){
    cigt <- gTree(ci = ci, at = at, height = height, to = to, col = col, bar = bar,
                  cilabs = cilabs, fix.label.pos = fix.label.pos, name = name, gp = gp, vp = vp, cl = "confint")
    cigt
}
## Utility for updating a ci grob
setConfintGrob <- function(cigt){
    ci <- cigt$ci
    cilabs <- cigt$cilabs
    at <- cigt$at
    height <- cigt$height
    to <- cigt$to
    col <- cigt$col
    bar <- cigt$bar
    fixes <- cigt$fix.label.pos
    ci.rect <- NULL
    if (bar){
        ci.rect <- rectGrob(x = unit(ci[1], "native"), y = at,
                            height = height, width = unit(diff(ci), "native"),
                            just = c("left", "centre"),
                            gp = gpar(col = col, fill = col), name = "ci.rect")
    }
    arrows <- segmentsGrob(x0 = unit(ci, "native"), x1 = unit(ci, "native"),
                           y0 = at, y1 = to, gp = gpar(col = col),
                           arrow = arrow(length = unit(0.1, "inches")),
                           name = "arrows")
    text1 <- textGrob(label = format(cilabs[1], nsmall = 1), x = unit(ci[1], "native"),
                      y = to, gp = gpar(fontface = 2, col = col),
                      just = c(if (fixes[1]) "left" else "right", "top"), name = "text1")
    text1bg <- rectGrob(x = unit(ci[1], "native") + unit(if (fixes[1]) -1 else 1, "mm"),
                        y = to + unit(1, "mm"),
                        height = unit(1, "lines") + unit(2, "mm"),
                        width = stringWidth(format(cilabs[1], nsmall = 1)) + unit(2, "mm"),
                        just = c(if (fixes[1]) "left" else "right", "top"),
                        gp = gpar(col = "white", fill = "white", alpha = 0.75),
                        name = "text1bg")
    text2 <- textGrob(label = format(cilabs[2], nsmall = 1), x = unit(ci[2], "native"),
                      y = to, gp = gpar(fontface = 2, col = col),
                      just = c(if (fixes[2]) "right" else "left", "top"), name = "text2")
    text2bg <- rectGrob(x = unit(ci[2], "native") + unit(if (fixes[2]) 1 else -1, "mm"),
                        y = to + unit(1, "mm"),
                        height = unit(1, "lines") + unit(2, "mm"),
                        width = stringWidth(format(cilabs[2], nsmall = 1)) + unit(2, "mm"),
                        just = c(if (fixes[2]) "right" else "left", "top"),
                        gp = gpar(col = "white", fill = "white", alpha = 0.75),
                        name = "text2bg")
    cigt <- setChildren(cigt, gList(ci.rect, text1bg, text2bg, arrows, text1, text2))
    cigt
}

drawDetails.confint <- function(x, recording){
    x <- setConfintGrob(x)
    for (i in childNames(x)) grid.draw(getGrob(x, i))
}

editDetails.confint <- function(x, specs){
    x <- confintGrob(ci = x$ci, at = x$at, height = x$height, to = x$to, col = x$col,
                     bar = x$bar, cilabs = x$cilabs, fix.label.pos = x$fix.label.pos,
                     name = x$name, gp = x$gp, vp = x$vp)
    x
}

validDetails.confint <- function(x) x
