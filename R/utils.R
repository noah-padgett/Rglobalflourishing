# Utility functions for Rglobalflourishing package

#' Standardized with Complex Survey Adjusted Mean and Variance
#'
#' Standardizes a variable x based on the weights and design. A better replacement for scale(.).
#'
#' @param x (character) a variable name in data
#' @param wgt (character) variable name of weights
#' @param psu (character) variable name of PSUs
#' @param strata (character) variable name of stratum ids
#' @param design (survey.design or survey.design2 object)
#' @returns a vector of standardized scores
#' @examples {
#'   library(survey)
#'   # TODO
#' }
#' @rdname utils
#' @export
svy_scale <- function(x, wgt = NULL, psu = NULL, strata = NULL, design = NULL) {
  if (is.null(design)) {
    tmp.dat <- data.frame(cbind(x, wgt, psu, strata))
    tmp.design <- survey::svydesign(data = tmp.dat, ids = ~psu, strata = ~strata, weights = ~wgt)
  }
  tmp.mean <- survey::svymean(~x, design = tmp.design)
  tmp.mean <- as.numeric(tmp.mean)
  tmp.var <- survey::svyvar(~x, design = tmp.design)
  tmp.sd <- sqrt(as.numeric(tmp.var))
  (x - tmp.mean) / tmp.sd
}

#' Check Variable Variance
#'
#' checks the variances of the predictor variable, identifies whether the variable "x" have any
#' variance (works w/ any variable type, continuous, binary, nominal,...).
#'
#' @param x (character) a variable name in data
#' @param data (data.frame)
#' @returns a logical (TRUE/FALSE)
#' @examples {
#'   df <- data.frame(A = c(1, 1, 0, 0), B = c("B", "B", "B", "B"))
#'   check_variance("A", df)
#'   check_variance("B", df)
#' }
#' @rdname utils
#' @export
check_variance <- function(x, data) {
  ifelse(length(na.omit(unique(data[[x]]))) > 1, TRUE, FALSE)
}

#' Create Keep Variable Vector
#'
#' Creates a logical vector to keep or delete variables for use in a regression model.
#'
#' @param x (character vector) of a vector of variable names in data
#' @param data (data.frame)
#' @returns a logical vector of length equal to the length of 'x'
#' @examples {
#'   df <- data.frame(A = c(1, 1, 0, 0), B = c("B", "B", "B", "B"))
#'   keep_variable(c("A", "B"), df)
#' }
#' @description
#' keep_variable(.), a wrapper around the function 'check_variance(.)' to create a logical
#' vector of length(x) of the results of the check_variance function. The result can then be used to
#' to subset the vector 'x', e.g., 'x[keep_variable(x, data)]', to only include predictors 'x' that
#' have any variance. The inverse selection, 'x[!(keep_variable(x, data))]', can be used to identify
#' which variables were excluded.
#'
#' @rdname utils
#' @export
keep_variable <- function(x, data, reason = "variance") {

  r.var = unlist(lapply(x, function(y) {
    check_variance(y, data)
  }))
  r.miss = unlist(lapply(x, function(y) {
     !anyNA(data[[y]])
  }))
  if(reason == "variance"){
  	out = r.var
  }
  if(reason == "miss"){
  	out = r.miss
  }
  if(reason == "any"){
  	out = as.logical(r.var * r.miss)
  }
  out
}

#' Create Predictor Matrix (modified)
#'
#' Function copied and modified from mice pacakge (https://github.com/amices/mice/blob/master/R/quickpred.R)
#'
#' @param data Matrix or data frame with incomplete data.
#' @param mincor A scalar, numeric vector (of size \code{ncol(data))} or numeric
#' matrix (square, of size \code{ncol(data)} specifying the minimum
#' threshold(s) against which the absolute correlation in the data is compared.
#' @param minpuc A scalar, vector (of size \code{ncol(data))} or matrix (square,
#' of size \code{ncol(data)} specifying the minimum threshold(s) for the
#' proportion of usable cases.
#' @param include A string or a vector of strings containing one or more
#' variable names from \code{names(data)}. Variables specified are always
#' included as a predictor.
#' @param exclude A string or a vector of strings containing one or more
#' variable names from \code{names(data)}. Variables specified are always
#' excluded as a predictor.
#' @param method A string specifying the type of correlation. Use
#' \code{'pearson'} (default), \code{'kendall'} or \code{'spearman'}. Can be
#' abbreviated.
#' @param maxcor A scalar, numeric vector (of size \code{ncol(data))} or numeric
#' matrix (square, of size \code{ncol(data)} specifying the MAXIMUM
#' threshold(s) against which the absolute correlation in the data is compared.
#' this is used to help troubleshoot and resolve non-invertible predictor matrices
#' @return a predictor matrix for use in mice
#' @rdname utils
#' @export
quickpred2 <- function(
    data, mincor = 0.1, minpuc = 0, include = "", exclude = "",
    method = "pearson", maxcor = 0.71) {
  # ` functions copied from mice package to ensure that the above quickpred works... (https://github.com/amices/mice/blob/master/R/check.R)
  check.dataform <- function(data) {
    if (!(is.matrix(data) || is.data.frame(data))) {
      stop("Data should be a matrix or data frame", call. = FALSE)
    }
    if (ncol(data) < 2) {
      stop("Data should contain at least two columns", call. = FALSE)
    }
    data <- as.data.frame(data)
    mat <- sapply(data, is.matrix)
    df <- sapply(data, is.data.frame)
    if (any(mat)) {
      stop(
        "Cannot handle columns with class matrix: ",
        colnames(data)[mat]
      )
    }
    if (any(df)) {
      stop(
        "Cannot handle columns with class data.frame: ",
        colnames(data)[df]
      )
    }

    dup <- duplicated(colnames(data))
    if (any(dup)) {
      stop(
        "Duplicate names found: ",
        paste(colnames(data)[dup], collapse = ", ")
      )
    }
    data
  }
  data <- check.dataform(data)
  nvar <- ncol(data)
  predictorMatrix <- matrix(0, nrow = nvar, ncol = nvar, dimnames = list(
    names(data),
    names(data)
  ))
  x <- data.matrix(data)
  r <- !is.na(x)
  suppressWarnings(v <- abs(cor(x,
    use = "pairwise.complete.obs",
    method = method
  )))
  v[is.na(v)] <- 0
  suppressWarnings(u <- abs(cor(
    y = x, x = r, use = "pairwise.complete.obs",
    method = method
  )))
  u[is.na(u)] <- 0
  maxc <- pmax(v, u)
  predictorMatrix[maxc > mincor] <- 1
  predictorMatrix[maxc > maxcor] <- 0
  p <- mice::md.pairs(data)
  puc <- p$mr / (p$mr + p$mm)
  predictorMatrix[puc < minpuc] <- 0
  yz <- pmatch(exclude, names(data))
  predictorMatrix[, yz] <- 0
  yz <- pmatch(include, names(data))
  predictorMatrix[, yz] <- 1
  diag(predictorMatrix) <- 0
  predictorMatrix[colSums(!r) == 0, ] <- 0
  predictorMatrix
}

#' @rdname utils
#' @export
format_flex_table <- function(xtb, pg.width = 6.5) {
  tb.temp <- xtb %>%
    theme_apa() %>%
    font(part = "all", fontname = "Times New Roman") %>%
    fontsize(part = "header", size = 11) %>%
    fontsize(part = "body", size = 10) %>%
    line_spacing(space = 1, part = "all") %>%
    padding(padding = 2, part = "all") %>%
    align(align = "right", part = "all") %>%
    align(j = 1, align = "left", part = "all") %>%
    valign(valign = "bottom", part = "all") %>%
    border_remove() %>%
    hline_top(part = "header") %>%
    hline_bottom(part = "header") %>%
    hline_bottom(part = "body")
  tb.temp <-
    width(tb.temp,
      width = dim(tb.temp)$widths * pg.width / (flextable_dim(tb.temp)$widths)
    )
  tb.temp
}

# A better rounding function for printing/concatenating results
#' @rdname utils
#' @export
.round <- function(x, digits = 2) {
  sprintf(paste0("%.", digits, "f"), x)
}

#' @rdname utils
#' @export
.round_p <- function(x, .sci=TRUE, .digits=3, .eps = 1e-16){
  format.pval(x, scientific=.sci, digits=.digits, eps = .eps)
}


# functions needed for forest plots
#' @rdname utils
#' @export
.geom_stripes <- function(mapping = NULL,
                          data = NULL,
                          stat = "identity",
                          position = "identity",
                          ...,
                          show.legend = NA,
                          inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = .GeomStripes,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(...)
  )
}
#' @rdname utils
#' @export
.GeomStripes <- ggplot2::ggproto(
  "GeomStripes", ggplot2::Geom,
  required_aes = c("y"),
  default_aes = ggplot2::aes(
    xmin = -Inf, xmax = Inf,
    odd = "#00000000", even = "#22222222",
    # Change 'size' below from 0 to NA.
    # When not NA then when *printing in pdf device* borders are there despite
    # requested 0th size. Seems to be some ggplot2 bug caused by grid overriding
    # an lwd parameter somewhere, unless the size is set to NA. Found solution here
    # https://stackoverflow.com/questions/43417514/getting-rid-of-border-in-pdf-output-for-geom-label-for-ggplot2-in-r
    alpha = NA, colour = "black", linetype = "solid", linewidth = NA
  ),

  # draw_key = ggplot2::draw_key_blank,
  draw_key = ggplot2::draw_key_rect,
  draw_panel = function(data, panel_params, coord) {
    ggplot2::GeomRect$draw_panel(
      data %>%
        dplyr::mutate(
          y = round(y),
          ymin = y - 0.5,
          ymax = y + 0.5
        ) %>%
        dplyr::select(
          xmin, xmax,
          ymin, ymax,
          odd, even,
          alpha, colour, linetype, linewidth
        ) %>%
        unique() %>%
        dplyr::arrange(ymin) %>%
        dplyr::mutate(
          .n = dplyr::row_number(),
          fill = dplyr::if_else(
            .n %% 2L == 1L,
            true = odd,
            false = even
          )
        ) %>%
        dplyr::select(-.n, -odd, -even),
      panel_params,
      coord
    )
  }
)

#' @rdname utils
#' @export
theme_Publication <- function(base_size=14) {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size)
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(),
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.key.size= unit(0.2, "cm"),
            legend.spacing = unit(0, "cm"),
            legend.title = element_text(face="italic"),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text(face="bold")
    ))

}



#' @export
load_meta_result <- function(file, predictor=NULL, outcome=NULL, what = NULL) {
  # Use local() to limit the scope of the loaded object
  local({
    # Load the RDS file
    data <- readRDS(file)
    
    if(is.null(what)){
    		what <- colnames(data)
    }
    if(is.null(predictor)){
    	  predictor = unique(data$FOCAL_PREDICTOR0)
    }
    if(is.null(outcome)){
    	  outcome = unique(data$OUTCOME0)
    }
    
    data <- data %>% 
    		ungroup() %>%
    		dplyr::filter(OUTCOME0 %in% outcome) %>%
    		dplyr::filter(FOCAL_PREDICTOR0 %in% predictor) %>%
    		dplyr::select(any_of(what))
    		
    # Return the processed data
    return(data)
  })
}

#' @export
get_country_specific_regression_results <- function(res.dir, country, predictor, outcome) {
  local({
  	tmp.list <- NULL
      load(here::here(res.dir, paste0(predictor, "_regressed_on_", outcome, "_saved_results.RData")))
      # create new columns in output to help with constructing tables
      output <- output  %>%
        dplyr::filter(Variable == "FOCAL_PREDICTOR") %>%
        dplyr::filter(str_detect(COUNTRY, country))
      tmp.list <- output
  return(tmp.list)
  })
  
}

