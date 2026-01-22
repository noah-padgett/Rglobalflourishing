
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
    method = "pearson", maxcor = 0.99) {
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
            panel.background = element_rect(colour = NA, fill = "white"),
            plot.background = element_rect(colour = NA, fill = 'white'),
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
    	  predictor = unique(data$FOCAL_PREDICTOR)
    }
    if(is.null(outcome)){
    	  outcome = unique(data$OUTCOME0)
    }

    data <- data %>%
    		ungroup() %>%
    		dplyr::filter(OUTCOME0 %in% outcome) %>%
    		dplyr::filter(FOCAL_PREDICTOR %in% predictor) %>%
    		dplyr::select(any_of(what))

    # Return the processed data
    return(data)
  })
}

#' @export
get_country_specific_regression_results <- function(res.dir, country, predictor, outcome, appnd.txt="", replace.cntry.file.start = NULL) {
  local({
  	tmp.list <- NULL
  	file.start <- predictor
  	if(!is.null(replace.cntry.file.start)){
  	  file.start = replace.cntry.file.start
  	}
      load(here::here(res.dir, paste0(file.start, "_regressed_on_", outcome, "_saved_results",appnd.txt,".RData")))
      # create new columns in output to help with constructing tables
      output <- output  %>%
        dplyr::filter(term == predictor) %>%
        dplyr::filter(str_detect(COUNTRY, country))
      tmp.list <- output
  return(tmp.list)
  })

}

#' @export
construct_meta_input_from_saved_results <- function(res.dir, outcomes, predictors, appnd.txt="") {
  local({
    res.dir <- here::here(res.dir)
    tmp.list <- list()
    for (your.outcome in outcomes) {
      for (your.pred in predictors) {
        try({
          load(here::here(res.dir, paste0(your.pred, "_regressed_on_", your.outcome, "_saved_results",appnd.txt,".RData")))
          tmp.list[[paste0(your.outcome, "_", your.pred)]] <- metainput
        })
      }
    }
    return(tmp.list)
  })
}

#' @export
get_country_specific_output <- function(res.dir, outcomes, predictors, appnd.txt="",replace.cntry.file.start=NULL, keep.terms = NULL) {
  local({
    tmp.list <- list()
    for (your.outcome in outcomes) {
      for (your.pred in predictors) {
        file.start = your.pred
        if(!is.null(replace.cntry.file.start)){
          file.start = replace.cntry.file.start
        }
        load(here::here(res.dir, paste0(file.start, "_regressed_on_", your.outcome, "_saved_results",appnd.txt,".RData")))
        tmp.list[[paste0(your.outcome, "_", your.pred)]] <- output
      }
    }
    tmp.list <- tmp.list |>
      bind_rows()
    if(!is.null(keep.terms)){
      tmp.list <- tmp.list |>
        filter(term %in% keep.terms)
    }

    return(tmp.list)
  })
}


#' @export
get_country_specific_pca_summary <- function(res.dir, outcomes, predictors, appnd.txt="", replace.cntry.file.start=NULL) {
  local({
    tmp.list <- list()
    for (your.outcome in outcomes) {
      for (your.pred in predictors) {
        try({
          file.start = your.pred
          if(!is.null(replace.cntry.file.start)){
            file.start = replace.cntry.file.start
          }
          load(here::here(res.dir, paste0(file.start, "_regressed_on_", your.outcome, "_saved_results",appnd.txt,".RData")))
          tmp.list[[paste0(your.outcome, "_", your.pred)]] <- fit.pca.summary
        })
      }
    }
    return(tmp.list)
  })
}

#' @export
get_country_pca_summary <- function(res.dir, country, outcome, predictor, appnd.txt="", replace.cntry.file.start=NULL) {
  local({
    tmp.list <- NULL
    try({
      file.start = predictor
      if(!is.null(replace.cntry.file.start)){
        file.start = replace.cntry.file.start
      }
      load(here::here(res.dir, paste0(file.start, "_regressed_on_", outcome, "_saved_results",appnd.txt,".RData")))
      tmp.list <- fit.pca.summary %>%
        ungroup() %>%
        filter(str_detect(COUNTRY, country))
    })
    return(tmp.list)
  })
}


#' @export
get_fitted_attrition_models <- function(res.dir) {
  local({
    tmp.list <- list()
    tmp.attr.files <- list.files(res.dir)
    i = 1
    for ( i in 1:length(tmp.attr.files)){
      load(here::here(res.dir, tmp.attr.files[i]))
      tmp.list[[str_split_i(tmp.attr.files[i], " fitted", 1)]] <- df.attr$fit.attr[[1]]
    }
    return(tmp.list)
  })

}

#' @export
get_fitted_attrition_model <- function(res.dir, country) {
  local({
    tmp.attr.files <- list.files(here::here(res.dir))
    tmp.attr.files <- tmp.attr.files[str_detect(tmp.attr.files, country)]
    load(here::here(res.dir, tmp.attr.files))
    return(df.attr$fit.attr[[1]])
  })
}



#' @rdname utils
#' @param doc officer object to be printed out
#' @param file name of file to print out internal officer object
#' @param dir name of directory to print out file to
#' @export
gfs_print_docx <- function(doc, file, dir){
  tmp.file <- here::here(dir, "tmp_doc.docx")
  print(doc, target = tmp.file)
  rdoc <- read_docx(file)
  rdoc <- rdoc |> body_add_docx(tmp.file)
  print(rdoc, target = file)
}

#' @rdname utils
#' @param dir name of directory to print out file to
#' @param cur.doc name of pdf file needed to be file appended to
#' @param add name of file in directory to appended to cur.doc
#' @export
gfs_append_pdf <- function(dir, cur.doc, add){
  tmp.dir <- tempdir()
  tmp.dir.file <- here::here(tmp.dir, "tmp_pdf_0.pdf")
  dir.file <- here::here(dir, "tmp_pdf_0.pdf")
  file.copy(here::here(dir, cur.doc), tmp.dir, overwrite=TRUE)
  file.rename(here::here(tmp.dir, cur.doc), tmp.dir.file)
  file.copy(tmp.dir.file, dir, overwrite=TRUE)
  qpdf::pdf_combine(
    input = c(dir.file, add),
    output = here::here(dir, cur.doc)
  )
}


#' @rdname utils
#' @param file full file name and path, e.g., here(path,filename)
#' @param ft flextable object to be added to excel file
#' @param tb table caption
#' @import flexlsx openxlsx2
#' @export
gfs_append_to_xlsx <- function(file, ft, tb){
  # 1. check is file exists
  if(!file.exists(file)){
    wb <- openxlsx2::wb_workbook()
  } else {
    wb <- openxlsx2::wb_load(file)
  }
  # 2. create new sheet
  tb <- strsplit(tb, "\\.")[[1]][1] # extract only the "Table SXX" part of caption
  # 3. a little backup in case the table already got printed
  sheet.names <- openxlsx2::wb_get_sheet_names(wb)
  if( tb %in% sheet.names ){
    wb <- openxlsx2::wb_clean_sheet(wb, sheet = tb)
  } else {
    wb <- openxlsx2::wb_add_worksheet(wb, sheet = tb)
  }
  wb <- flexlsx::wb_add_flextable(wb, sheet = tb, ft = ft)
  try({
    openxlsx2::wb_save(wb, file = file)
  })
}



#' @keywords internal
style_percent0 <- function (x,
                           digits = 0,
                           big.mark = ifelse(decimal.mark == ",", " ", ","),
                           decimal.mark = getOption("OutDec"),
                           prefix = "",
                           suffix = "",
                           symbol,
                           ...)
{
  gtsummary:::set_cli_abort_call()
  if (!missing(symbol)) {
    lifecycle::deprecate_soft(
      when = "2.0.3",
      what = "gtsummary::style_percent(symbol)",
      with = I("style_percent(suffix='%')")
    )
    if (isTRUE(symbol))
      suffix = "%"
  }
  y <- dplyr::case_when(
    x * 100 >= 10 ~ style_number( x * 100, digits = digits , big.mark = big.mark, decimal.mark = decimal.mark, prefix = prefix, suffix = suffix, ... ),
    x * 100 >= 10^(-(digits + 1)) ~ style_number( x * 100, digits = digits , big.mark = big.mark, decimal.mark = decimal.mark, prefix = prefix, suffix = suffix, ... ),
    x > 0 ~ paste0("<", style_number( x = 10^(-(digits + 1)), digits = digits ,  big.mark = big.mark, decimal.mark = decimal.mark,  prefix = prefix, suffix = suffix,...) ),
    x == 0 ~ paste0(prefix, "0", suffix)
  )
  attributes(y) <- attributes(unclass(x))
  return(y)
}

#' @keywords internal
label_style_percent0 <- function (prefix = "", suffix = "", digits = 0, big.mark = ifelse(decimal.mark ==
                                                                    ",", " ", ","), decimal.mark = getOption("OutDec"), ...)
{
  function(x) style_percent0(x, prefix = prefix, suffix = suffix,
                            digits = digits, big.mark = big.mark, decimal.mark = decimal.mark,
                            ...)
}


#' @keywords internal
mixedorder <- function (x, decreasing = FALSE, na.last = TRUE, blank.last = FALSE,  numeric.type = c("decimal", "roman"), roman.case = c("upper", "lower", "both"), scientific = TRUE){
  numeric.type <- match.arg(numeric.type)
  roman.case <- match.arg(roman.case)
  if (length(x) < 1) {
    return(NULL)
  }
  else if (length(x) == 1) {
    return(1)
  }
  if (!is.character(x)) {
    return(order(x, decreasing = decreasing, na.last = na.last))
  }
  delim <- "\\$\\@\\$"
  if (numeric.type == "decimal") {
    if (scientific) {
      regex <- "((?:(?i)(?:[-+]?)(?:(?=[.]?[0123456789])(?:[0123456789]*)(?:(?:[.])(?:[0123456789]{0,}))?)(?:(?:[eE])(?:(?:[-+]?)(?:[0123456789]+))|)))"
    }
    else {
      regex <- "((?:(?i)(?:[-+]?)(?:(?=[.]?[0123456789])(?:[0123456789]*)(?:(?:[.])(?:[0123456789]{0,}))?)))"
    }
    numeric <- function(x) as.numeric(x)
  }
  else if (numeric.type == "roman") {
    regex <- switch(roman.case, both = "([IVXCLDMivxcldm]+)",
                    upper = "([IVXCLDM]+)", lower = "([ivxcldm]+)")
    numeric <- function(x) roman2int(x)
  }
  else {
    stop("Unknown value for numeric.type: ", numeric.type)
  }
  nonnumeric <- function(x) {
    ifelse(is.na(numeric(x)), toupper(x), NA)
  }
  x <- as.character(x)
  which.nas <- which(is.na(x))
  which.blanks <- which(x == "")
  delimited <- gsub(regex, paste(delim, "\\1", delim, sep = ""),
                    x, perl = TRUE)
  step1 <- strsplit(delimited, delim)
  step1 <- lapply(step1, function(x) x[x > ""])
  suppressWarnings(step1.numeric <- lapply(step1, numeric))
  suppressWarnings(step1.character <- lapply(step1, nonnumeric))
  maxelem <- max(sapply(step1, length))
  step1.numeric.t <- lapply(1:maxelem, function(i) {
    sapply(step1.numeric, function(x) x[i])
  })
  step1.character.t <- lapply(1:maxelem, function(i) {
    sapply(step1.character, function(x) x[i])
  })
  rank.numeric <- sapply(step1.numeric.t, rank)
  rank.character <- sapply(step1.character.t, function(x) as.numeric(factor(x)))
  rank.numeric[!is.na(rank.character)] <- 0
  rank.character <- t(t(rank.character) + apply(matrix(rank.numeric),
                                                2, max, na.rm = TRUE))
  rank.overall <- ifelse(is.na(rank.character), rank.numeric,
                         rank.character)
  order.frame <- as.data.frame(rank.overall)
  if (length(which.nas) > 0) {
    if (is.na(na.last)) {
      order.frame[which.nas, ] <- NA
    }
    else if (na.last) {
      order.frame[which.nas, ] <- Inf
    }
    else {
      order.frame[which.nas, ] <- -Inf
    }
  }
  if (length(which.blanks) > 0) {
    if (is.na(blank.last)) {
      order.frame[which.blanks, ] <- NA
    }
    else if (blank.last) {
      order.frame[which.blanks, ] <- 1e+99
    }
    else {
      order.frame[which.blanks, ] <- -1e+99
    }
  }
  order.frame <- as.list(order.frame)
  order.frame$decreasing <- decreasing
  order.frame$na.last <- NA
  retval <- do.call("order", order.frame)
  return(retval)
}

#' @keywords internal
mixedsort <- function (x, decreasing = FALSE, na.last = TRUE, blank.last = FALSE,  numeric.type = c("decimal", "roman"), roman.case = c("upper", "lower", "both"), scientific = TRUE){
  ord <- mixedorder(x, decreasing = decreasing, na.last = na.last,
                    blank.last = blank.last, numeric.type = numeric.type,
                    roman.case = roman.case, scientific = scientific)
  x[ord]
}
