#' GFS Flextable Themes
#'
#' Possible themes with various defaults to help with formatting tables.
#'
#' @param xtb a flextable object
#' @param pg.width width of page to adjust column widths to.
#' @returns a formatted flextable
#' @examples {
#'   ## TODO
#' }
#' @description
#' TODO
#' @rdname gfs_ft_themes
#' @export
format_flex_table <- function(xtb, pg.width = 6.5) {
  tb.temp <- xtb %>%
    theme_apa() %>%
    font(part = "all", fontname = "Open Sans") %>%
    fontsize(part = "header", size = 10) %>%
    fontsize(part = "body", size = 9) %>%
    line_spacing(space = 0.95, part = "all") %>%
    padding(padding = 0, part = "all") %>%
    align(align = "right", part = "all") %>%
    align(j = 1, align = "left", part = "all") %>%
    valign(valign = "bottom", part = "all") %>%
    border_remove() %>%
    #hline_top(part = "header") %>%
    hline_bottom(part = "header") %>%
    hline_bottom(part = "body")
  tb.temp <-
    width(tb.temp,
          width = dim(tb.temp)$widths * pg.width / (flextable_dim(tb.temp)$widths)
    )
  tb.temp
}

#' @rdname gfs_ft_themes
#' @export
theme_meta_outcome_wide <- function(xtb, pg.width = 9, .ncol = 12) {
  tb.temp <- xtb %>%
    theme_apa() %>%
    font(part = "all", fontname = "Open Sans") %>%
    fontsize(part = "header", size = 9) %>%
    fontsize(part = "body", size = 9) %>%
    line_spacing(space = 0.95, part = "all") %>%
    padding(padding = 0, part = "all") %>%
    align(align = "right", part = "all") %>%
    align(j = 1, align = "left", part = "all") %>%
    valign(valign = "bottom", part = "all")  %>%
    width(j=1,width=2.25)%>%
    width(j=c(2:3,5,8:9,11),width=0.4)%>%
    width(j=c(4,10),width=0.8)%>%
    width(j=c(6,12),width=0.9)%>%
    width(j=7,width=0.10)

  tb.temp <-
    width(tb.temp,
          width = dim(tb.temp)$widths * pg.width / (flextable_dim(tb.temp)$widths)
    )

  tb.temp <- tb.temp %>%
    align(i = 2, j = NULL, align = "center", part = "header") %>%
    align(part = "footer", align = "left", j = 1:.ncol) %>%
    border_remove()  %>%
    hline_bottom(part = "body") %>%
    #hline_top(part = "header") %>%
    hline_bottom(part = "header") %>%
    hline(i=2,j=c(2:6,8:12), part="header") %>%
    hline(i=1, part="header")

  tb.temp
}

#' @rdname gfs_ft_themes
#' @export
theme_meta_evalues <- function(xtb, pg.width = 6.5, .ncol = 6) {
  tb.temp <- xtb %>%
    theme_apa() %>%
    font(part = "all", fontname = "Open Sans") %>%
    fontsize(part = "header", size = 10) %>%
    fontsize(part = "body", size = 9) %>%
    line_spacing(space = 0.95, part = "all") %>%
    padding(padding = 0, part = "all") %>%
    align(align = "right", part = "all") %>%
    align(j = 1, align = "left", part = "all") %>%
    valign(valign = "bottom", part = "all")  %>%
    width(j=1,width=2.60)%>%
    width(j=c(2,5),width=0.60)%>%
    width(j=c(3,6),width=0.90)%>%
    width(j=c(4),width=0.10)%>%
    align(i = 2, j = NULL, align = "center", part = "header") %>%
    align(part = "footer", align = "left", j = 1:.ncol) %>%
    border_remove()  %>%
    hline_bottom(part = "body") %>%
    #hline_top(part = "header") %>%
    hline_bottom(part = "header") %>%
    hline(i=2,j=c(2:3,5:6), part="header") %>%
    hline(i=1, part="header")


  tb.temp
}


#' @rdname gfs_ft_themes
#' @export
theme_meta_predictor_wide <- function(xtb, pg.width = 9, .ncol = 10) {
  tb.temp <- xtb %>%
    theme_apa() %>%
    font(part = "all", fontname = "Open Sans") %>%
    fontsize(part = "header", size = 9) %>%
    fontsize(part = "body", size = 9) %>%
    line_spacing(space = 0.95, part = "all") %>%
    padding(padding = 0, part = "all") %>%
    align(align = "right", part = "all") %>%
    align(j = 1, align = "left", part = "all") %>%
    valign(valign = "bottom", part = "all")  %>%
    width(j=1,width=2.25)%>%
    width(j=c(2,4,7,9),width=0.4)%>%
    width(j=c(3,8),width=0.8)%>%
    width(j=c(5,10),width=0.9)%>%
    width(j=6,width=0.10)

  tb.temp <-
    width(tb.temp,
          width = dim(tb.temp)$widths * pg.width / (flextable_dim(tb.temp)$widths)
    )

  tb.temp <- tb.temp %>%
    align(i = 2, j = NULL, align = "center", part = "header") %>%
    align(part = "footer", align = "left", j = 1:.ncol) %>%
    border_remove()  %>%
    hline_bottom(part = "body") %>%
    #hline_top(part = "header") %>%
    hline_bottom(part = "header") %>%
    hline(i=2,j=c(2:5,7:10), part="header") %>%
    hline(i=1, part="header")

  tb.temp
}
