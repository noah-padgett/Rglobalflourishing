#' GFS Formatted Forest Plots
#'
#' Forest plots constructed from GFS meta analysis results for use in manuscripts.
#'
#' @param fit fitted results object from the metafor package (rma)
#' @param better.name a manually supplied name for the focal predictor
#' @param p.title a string passed to title of the forest plot
#' @param p.subtitle a string passed to subtitle of the forest plot.
#' @param ... additional arguments as needed
#' @return a ggplot object
#' @examples
#' # TO-DO
#'
#' @export
gfs_meta_forest_plot <- function(fit, better.name = NULL, p.subtitle = "GFS Forest Plot", p.title = NULL, ...) {
  ALL.COUNTRIES <- c(
    "Australia",
    "Hong Kong",
    "India",
    "Indonesia",
    "Japan",
    "Philippines",
    "Egypt",
    "Germany",
    "Israel",
    "Kenya",
    "Nigeria",
    "Poland",
    "South Africa",
    "Spain",
    "Sweden",
    "Tanzania",
    "Turkey",
    "United Kingdom",
    "United States",
    "Argentina",
    "Brazil",
    "Mexico",
    "China"
  )
  tmp.fit <- fit
  tmp.dat <- fit$data

  if("FOCAL_PREDICTOR" %in% colnames(tmp.dat)){

  	focal.pred <- tmp.dat$FOCAL_PREDICTOR[1]
  	if (!is.null(better.name)) {
   	 focal.pred <- better.name[focal.pred]
  	} else {
  	  focal.pred <- get_outcome_better_name(focal.pred, include.name = FALSE)
  	}
  	tmp.outcome.scale <- get_outcome_scale(tmp.dat$OUTCOME[1])
  	tmp.outcome <- tmp.dat$OUTCOME[1]
  	tmp.outcome <- get_outcome_better_name(tmp.outcome, include.name = FALSE, include.wave = TRUE)

  	focal.pred <- str_to_sentence(focal.pred)
  	tmp.outcome <- str_to_sentence(tmp.outcome)

  }

  if(is.null(p.title)){
  	p.title = paste0("`", focal.pred, "` predicts `", tmp.outcome, "`")
  }
  #for(i in 1:length(tmp.dat$Country)){
  #	tmp.dat$Country[i] <- stringr::str_split_fixed(tmp.dat$Country[i], "\\. ", 2)[,2]
  #}
  # identify countries omitted from meta-analysis
  tmp.included.countries = ""
  if("Country" %in% colnames(tmp.dat)){
    tmp.included.countries <- tmp.dat$Country
    tmp.included.countries <- str_replace(tmp.included.countries, "_", " ")
    tmp.included.countries <- str_trim(tmp.included.countries, "both")
    tmp.excluded.countries <- ALL.COUNTRIES[!(ALL.COUNTRIES %in% tmp.included.countries)]
    tmp.excluded.countries <- ifelse(
      !is_empty(tmp.excluded.countries),
      paste0("Excluded countries: ", paste0(tmp.excluded.countries, collapse = ", ")),
      ""
    )
  }

  xLab <- case_when(
    tmp.outcome.scale == "cont" ~ "Effect Size",
    tmp.outcome.scale != "cont" ~ "log(Risk-Ratio)"
  )

  # construct heterogeneity statement...
  myci <- confint(tmp.fit, type = "PL")

  tmp.het <- paste0(
    "\u03c4 =", .round(sqrt(tmp.fit$tau2), 3),
    "; Q-profile 95% CI [", .round(myci$random[2, 2], 3), ", ", .round(myci$random[2, 3], 3), "]",
    "; Q(df=", tmp.fit$k - tmp.fit$QMdf[1], ")=",
    .round(tmp.fit$QE), ", p=", format.pval(tmp.fit$QEp, digits=3, scientific=TRUE),
    "; I^2=", .round(tmp.fit$I2),
    ";\n",
    tmp.excluded.countries
  )


  xlims <- max(c(abs(tmp.dat$ci.lb.i), abs(tmp.dat$ci.ub.i))) + 0.05

  tmp.dat <- tmp.dat |>
    mutate(
      est_lab = paste0(.round(yi), " (", .round(ci.lb.i), ", ", .round(ci.ub.i), ")")
      # update country label for "turkey" to have umlat? or not?
      # Türkiye
      #Country = case_when(
      #  (str_detect(Country, "Turkey") |
      #    str_detect(Country, "Turkiye") |
      #    str_detect(Country, "rkiye") |
      #    str_detect(Country, "rkey")) ~ "Turkey",
      #  .default = Country
      #)
    )

  dat.below <- data.frame(
    label = "Overall",
    est = as.numeric(fit$b),
    ci.lb = as.numeric(fit$ci.lb),
    ci.ub = as.numeric(fit$ci.ub)
  ) |>
    mutate(
      ci = paste0("(", .round(ci.lb), ",", .round(ci.ub), ")"),
      CI = paste0(.round(est), " ", ci)
    )
  # below are the actual plots being constructed...
  p_mid <-
    tmp.dat |>
    ggplot(aes(y = reorder(Country, yi))) +
    geom_point(aes(x = yi), shape = 15, size = 3) +
    geom_linerange(aes(xmin = ci.lb.i, xmax = ci.ub.i)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    .geom_stripes() +
    labs(x = xLab) +
    lims(x = c(-xlims, xlims)) +
    theme_classic() +
    theme(
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      axis.text.y = element_text(face = "bold"),
      axis.line.y = element_blank(),
      axis.text.x = element_blank()
    )

  # right side of plot - estimates
  p_right <-
    tmp.dat |>
    ggplot(aes(y = reorder(Country, yi))) +
    geom_text(aes(x = 0, label = est_lab), hjust = 0.45) +
    .geom_stripes() +
    theme_void()

  p_below <- dat.below %>%
    ggplot(aes(x = est, y = label)) +
    geom_point(shape = 18, size = 5) +
    geom_linerange(aes(xmin = ci.lb, xmax = ci.ub)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    labs(x = xLab, y = NULL) +
    lims(x = c(-xlims, xlims)) +
    theme_classic() +
    theme(
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.y = element_blank(),
      axis.text = element_text(face = "bold")
    )

  p_below_right <-
    dat.below |>
    ggplot(aes(y = label)) +
    geom_text(aes(x = 0, label = CI), hjust = 0.45) +
    theme_void()


  # final plot arrangement
  p <- (p_mid + plot_spacer() + p_right +
    plot_spacer() + plot_spacer() + plot_spacer() +
    p_below + plot_spacer() + p_below_right) +
    plot_layout(
      byrow = TRUE,
      widths = c(2, -0.175, 1),
      heights = c(10, -0.75, 1)
    ) +
    plot_annotation(
      # title=str_wrap(paste0("Figure S",k+k.shift,f.tag," Forest plot for `", tmp.var,"`-`", tmp.cat, "` effect"), 75),
      title = str_wrap(p.title, 80),
      subtitle = p.subtitle,
      caption = tmp.het
    )

  p
}
