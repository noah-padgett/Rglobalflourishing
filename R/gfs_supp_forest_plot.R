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
gfs_supp_forest_plot <- function(params, plot.type = "combined", ...) {

  set_flextable_defaults(font.family = "Open Sans",font.size = 10)

  OUTCOME.VEC = params$OUTCOME.VEC
  MYLABEL = params$MYLABEL
  focal.predictor = params$focal.predictor
  focal.better.name = params$focal.better.name
  outcome = params$outcome
  dir = params$dir
  file.a = params$file.a
  file.b = params$file.b
  fig.num0 = params$fig.num0
  fig.cap = params$fig.cap
  n.print = params$n.print
  res.dir = params$res.dir
  cache.file = params$cache.file
  start.time = params$start.time
  ignore.cache = params$ignore.cache

  if(plot.type == "panelled"){
    # panelled is the default "raw" plot for each individual outcome/analyss type
    p1 <- load_meta_result(
      file = here::here(dir, file.a),
      predictor = focal.predictor,
      outcome = outcome,
      what = "forest.plot"
    )
    p1 <- p1[[1]][[1]]
    ggsave(
      filename = here::here(res.dir, "fig",paste0("figure_S",fig.num,"A_WOPC_", outcome,"_regressed_on_", focal.predictor,".png")),
      plot=p1, units="in", width=6, height=5, dpi = 1000
    )
    ggsave(
      filename = here::here(res.dir, "fig",paste0("figure_S",fig.num,"A_WOPC_", outcome,"_regressed_on_", focal.predictor,".pdf")),
      plot=p1, units="in", width=6, height=5
    )

    p2 <- load_meta_result(
      file = here::here(dir, file.b),
      predictor = focal.predictor,
      outcome = outcome,
      what = "forest.plot"
    )
    p2 <- p2[[1]][[1]]
    ggsave(
      filename = here::here(res.dir, "fig",paste0("figure_S",fig.num,"B_WPC_", outcome,"_regressed_on_", focal.predictor,".png")),
      plot=p2, units="in", width=6, height=5, dpi = 1000
    )
    ggsave(
      filename = here::here(res.dir, "fig",paste0("figure_S",fig.num,"B_WPC_", outcome,"_regressed_on_", focal.predictor,".pdf")),
      plot=p2, units="in", width=6, height=5
    )


  }

  if(plot.type == "combined"){

  ALL.COUNTRIES <- c("Australia", "Hong Kong", "India", "Indonesia", "Japan", "Philippines", "Egypt", "Germany", "Israel", "Kenya", "Nigeria", "Poland", "South Africa", "Spain", "Sweden", "Tanzania", "Turkey", "United Kingdom", "United States", "Argentina", "Brazil",    "Mexico",  "China"  )


  meta_res_wo <- meta_res_wo <- load_meta_result(
    file = here::here(dir, file.a),
    predictor = focal.predictor,
    outcome = outcome
  )
  meta_res_w <- load_meta_result(
    file = here::here(dir, file.b),
    predictor = focal.predictor,
    outcome = outcome
  )
  # meta fit objects
  fit_wo <- meta_res_wo$meta.rma[[1]]
  fit_w <- meta_res_w$meta.rma[[1]]
  # plot data
  meta_res_w$data[[1]]$model_type <- "Included"
  meta_res_wo$data[[1]]$model_type <- "Excluded"
  plot_df <- bind_rows(meta_res_w$data[[1]], meta_res_wo$data[[1]])

  # boring stuff for variable names...
  if("FOCAL_PREDICTOR" %in% colnames(plot_df)){

    focal.pred <- plot_df$FOCAL_PREDICTOR[1]
    if (!is.null(focal.better.name)) {
      focal.pred <- focal.better.name
    } else {
      focal.pred <- get_outcome_better_name(focal.pred, include.name = FALSE)
    }
    tmp.outcome.scale <- get_outcome_scale( plot_df$OUTCOME[1])
    tmp.outcome <-  plot_df$OUTCOME[1]
    tmp.outcome <- get_outcome_better_name(tmp.outcome, include.name = FALSE, include.wave = TRUE)

    focal.pred <- str_to_sentence(focal.pred)
    tmp.outcome <- str_to_sentence(tmp.outcome)

  }
  p.title = paste0("`", focal.pred, "` predicts `", tmp.outcome, "`")
  # identify countries omitted from meta-analysis
  tmp.included.countries = ""
  if("Country" %in% colnames(plot_df)){
    tmp.included.countries <- plot_df$Country
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

  ## construct heterogeneity statements
  build_het_statement <- function(fit, txt){
    myci <- confint(fit, type = "PL")
    paste0(txt,
           "  \u03c4 (tau)=", .round(sqrt(fit$tau2), 3),
           "; Q-profile 95% CI [", .round(myci$random[2, 2], 3), ", ", .round(myci$random[2, 3], 3), "]",
           "; Q(df=", fit$k - fit$QMdf[1], ")=",
           .round(fit$QE), ", p=", format.pval(fit$QEp, digits=3, scientific=TRUE),
           "; I^2=", .round(fit$I2),
           ifelse(tmp.excluded.countries == "","", ";\n"), tmp.excluded.countries
    )
  }
  tmp.het.wo <- build_het_statement(fit_wo, "PCs Excluded;")
  tmp.het.w <- build_het_statement(fit_w, "PCs Included;")

  # make sure to use the (*)i variables in data so that the correct estimates are being plotted.\
  # Noah: I switch the ordering to be by the overly conservative estimates with PC control
  plot_df <- plot_df |>
    mutate(
      Country = factor(Country, levels = Country[order(meta_res_w$data[[1]]$yi, decreasing = FALSE)], ordered=TRUE),
      est_lab = paste0(.round(yi), " (", .round(ci.lb.i), ", ", .round(ci.ub.i), ")")
    )
  # make sure bounds also contains 0
  xlims <- c(min(plot_df$ci.lb.i) - .05,max(plot_df$ci.ub.i) + .05)
  xlims[1] <- ifelse(xlims[1] > -0.05, -0.05, xlims[1])
  xlims[2] <- ifelse(xlims[2] < 0.05, 0.05, xlims[2])

  # DATA FOR PLOT
  dat.below <- data.frame(
    Country = c("Overall", "Overall"),
    yi = c(as.numeric(fit_wo$b), as.numeric(fit_w$b)),
    ci.lb.i = c(as.numeric(fit_wo$ci.lb), as.numeric(fit_w$ci.lb)),
    ci.ub.i = c(as.numeric(fit_wo$ci.ub), as.numeric(fit_w$ci.ub)),
    model_type = c("Excluded","Included")
  ) |>
    mutate(
      ci = paste0("(", .round(ci.lb.i), ",", .round(ci.ub.i), ")"),
      CI = paste0(.round(yi), " ", ci)
    )

  p_mid <- plot_df |>
    ggplot(aes(y = Country)) +
    Rglobalflourishing:::.geom_stripes() +
    geom_vline(xintercept = 0, linetype = "dashed", alpha = .5) +
    geom_point(aes(x = yi, color = model_type, shape = model_type),
               position=ggstance::position_dodgev(height=0.5),
               size = 2) +
    geom_linerange(aes(xmin = ci.lb.i, xmax = ci.ub.i, color = model_type),
                   position=ggstance::position_dodgev(height=0.5)) +
    scale_color_manual(values = c("#a6cee3", "#1f78b4")) +
    scale_shape_manual(values = c(17,16)) +
    theme_classic() +
    theme(
      legend.position = "none",
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      axis.text.y = element_text(),
      axis.line.y = element_blank(),
      axis.text.x = element_blank()
    ) +
    xlim(xlims)

  p_right_w <- plot_df |> filter(model_type == "Included") |>
    ggplot(aes(y = Country)) +
    geom_text(aes(x = 0, label = est_lab), hjust = 0.45) +
    Rglobalflourishing:::.geom_stripes() +
    theme_void()

  p_right_wo <- plot_df |> filter(model_type == "Excluded") |>
    ggplot(aes(y = Country)) +
    geom_text(aes(x = 0, label = est_lab), hjust = 0.45) +
    Rglobalflourishing:::.geom_stripes() +
    theme_void()

  p_below <- dat.below %>%
    ggplot(aes(x = yi, y = Country)) +
    geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
    geom_point(aes(x = yi, color = model_type, shape = model_type, size = model_type),
               position = ggstance::position_dodgev(height = 0.5)
    ) +
    geom_linerange(aes(xmin = ci.lb.i, xmax = ci.ub.i, color = model_type),
                   position = ggstance::position_dodgev(height = 0.5)) +
    scale_color_manual(values = c("#fcb360", "#ed6f0e")) +
    scale_shape_manual(values = c(18,15)) +
    scale_size_manual(values = c(4,3)) +
    theme_classic() +
    theme(
      legend.position = "none",
      axis.ticks.y = element_blank(),
      axis.text.y = element_text(),
      axis.line.y = element_blank(),
    ) +
    xlim(xlims) +
    labs(x = xLab, y = NULL)

  p_below_right_w <- dat.below |> filter(model_type == "Included") |>
    ggplot(aes(y = Country)) +
    geom_text(aes(x = 0, label = CI), hjust = 0.45) +
    theme_void()

  p_below_right_wo <- dat.below |> filter(model_type == "Excluded") |>
    ggplot(aes(y = Country)) +
    geom_text(aes(x = 0, label = CI), hjust = 0.45) +
    theme_void()

  x_dist <- xlims[2] - xlims[1]
  legend_x1 <- xlims[1] + (x_dist/20)
  legend_x2 <- xlims[1] + 11*(x_dist/20)
  ci_len <- x_dist/40
  text_x1 <- legend_x1 + x_dist/4.5
  text_x2 <- legend_x2 + x_dist/4

  legend_df <- data.frame(legend_shape = c("circ", "square", "tri", "diam"),
                          legend_color = c("darkbl", "darkor", "lightbl", "lightor"),
                          point_x = rep(c(legend_x1, legend_x2), each = 2),
                          point_y = rep(c(2, 1), times = 2)) %>%
    mutate(ci_upper = point_x + ci_len,
           ci_lower = point_x - ci_len)

  p_legend <- ggplot(legend_df, aes(x = point_x, y = point_y)) +
    geom_point(aes(shape = legend_color, color = legend_color, size = legend_color)) +
    geom_linerange(aes(xmin = ci_lower, xmax = ci_upper, color = legend_color)) +
    scale_color_manual(values = c("#1f78b4", "#ed6f0e", "#a6cee3","#fcb360")) +
    scale_shape_manual(values = c(16, 15, 17, 18)) +
    scale_size_manual(values = c(2.5, 3, 2.5, 4)) +
    annotate("text",
             label = "With PC Controls",
             x = text_x1, y = 1.55,
             size = 10/.pt) +
    annotate("text",
             label = "Without PC Controls",
             x = text_x2, y = 1.55,
             size = 10/.pt) +
    theme_bw() +
    theme(legend.position = "none",
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          axis.text.y = element_blank(),
          axis.line.y = element_blank(),
          axis.text.x = element_blank(),
          panel.grid = element_blank()) +
    xlim(xlims) +
    ylim(c(.5, 2.5))

  p_legend_w <- ggplot() +
    theme_void() +
    annotate("text", label = "With PC Controls", x = 0, y = 0)
  p_legend_wo <- ggplot() +
    theme_void() +
    annotate("text", label = "Without PC Controls", x = 0, y = 0)


  p <- (p_mid + plot_spacer() + p_right_w + plot_spacer() + p_right_wo +
          plot_spacer() + plot_spacer() + plot_spacer() + plot_spacer() + plot_spacer() +
          p_below + plot_spacer() + p_below_right_w + plot_spacer() + p_below_right_wo +
          p_legend + plot_spacer() + p_legend_w + plot_spacer() + p_legend_wo) +
    plot_layout(
      byrow = TRUE,
      widths = c(2, -0.1, 1, -0.1, 1),
      heights = c(10, -0.75, 1, -0.75, 1)
    )  +
    plot_annotation(
      # title=str_wrap(paste0("Figure S",k+k.shift,f.tag," Forest plot for `", tmp.var,"`-`", tmp.cat, "` effect"), 75),
      title = str_wrap(p.title, 80),
      #subtitle = p.subtitle,
      caption = paste0(c(tmp.het.wo, tmp.het.w), collapse = "\n")
    )

  ggsave(
    filename = here::here(res.dir, "fig",paste0("figure_S",fig.num0,"_", outcome,"_regressed_on_", focal.predictor,".pdf")),
    plot = p, height = 6, width = 10, units = "in"
  )
  ggsave(
    filename = here::here(res.dir, "fig",paste0("figure_S",fig.num0,"_", outcome,"_regressed_on_", focal.predictor,".png")),
    plot = p, height = 6, width = 10, units = "in", dpi = 1000
  )

  }


}
