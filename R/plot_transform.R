utils::globalVariables(c("var","transformation","var_se","var_skew","var_kurtosis","var_zmin","var_zmax"))

#' PLOT TRANSFORM
#'
#' This function helps users to visualize their purchase task (index-level) variables to determine whether a transformation of
#' log10 or square-root would be best to use.
#'
#' @param pt A data frame consisting of the purchase variable `pt_var` to visualize.
#' @param pt_var The name of the purchase task index-level variable to visualize, as identified in the data frame.
#' @return A ggplot2 graphical object
#' @export

plot_transform <- function(pt, pt_var) {

  se <- function(x){sqrt(var(x[!is.na(x)])/length(x[!is.na(x)]))}

 # CREATE TRANSFORMATIONS

  names(pt)[names(pt) == pt_var] <- "var_orig"

  pt$id <- rownames(pt)

  # ADD small constant for log transformation if zero-values exist in data
  if(0 %in% pt$var_orig){

    pt$var_orig0 <- pt$var_orig+0.01

    pt$var_log10 <- log10(pt$var_orig0)
    pt$var_sqrt <- sqrt(pt$var_orig)
  } else{

    pt$var_log10 <- log10(pt$var_orig)
    pt$var_sqrt <- sqrt(pt$var_orig)
  }

  # SUMMARY STATISTICS: Original
  trfmed_orig <- data.frame(var_se = se(pt$var_orig),
                            var_skew = psych::skew(pt$var_orig, na.rm = T),
                            var_kurtosis = psych::kurtosi(pt$var_orig, na.rm = T),
                            var_zmin = min(scale(pt$var_orig),na.rm = T),
                            var_zmax = max(scale(pt$var_orig), na.rm = T),
                            transformation = "Original")
  # SUMMARY STATISTICS: Log10
  trfmed_log10 <- data.frame(var_se = se(pt$var_log10),
                             var_skew = psych::skew(pt$var_log10, na.rm = T),
                             var_kurtosis = psych::kurtosi(pt$var_log10, na.rm = T),
                             var_zmin = min(scale(pt$var_log10),na.rm = T),
                             var_zmax = max(scale(pt$var_log10), na.rm = T),
                             transformation = "Log")

  # SUMMARY STATISTICS: Square Root
  trfmed_sqrt <- data.frame(var_se = se(pt$var_sqrt),
                            var_skew = psych::skew(pt$var_sqrt, na.rm = T),
                            var_kurtosis = psych::kurtosi(pt$var_sqrt, na.rm = T),
                            var_zmin = min(scale(pt$var_sqrt),na.rm = T),
                            var_zmax = max(scale(pt$var_sqrt), na.rm = T),
                            transformation = "Square Root")

  # COMBINE statistics
  trfmed_summary <- rbind(trfmed_orig,trfmed_log10,trfmed_sqrt)

  trfmed_summary[c(1:5)] <- round(trfmed_summary[c(1:5)], digits = 3)

  pt_long <- stats::reshape(as.data.frame(pt), idvar = "id",
                     varying = c("var_orig","var_log10","var_sqrt"),
                     timevar = c("transformation"), sep = "_", direction = "long")


  pt_long$transformation[pt_long$transformation=="orig"] <- "Original"
  pt_long$transformation[pt_long$transformation=="log10"] <- "Log10"
  pt_long$transformation[pt_long$transformation=="sqrt"] <- "Square Root"

  pt_viz <- ggplot2::ggplot(pt_long, ggplot2::aes (x = var, fill = transformation)) +
    ggplot2::geom_histogram(show.legend = F) +
    ggplot2::ylab("Count") + ggplot2::xlab(pt_var) + ggplot2::theme_classic() +
    ggplot2::theme(strip.text = ggplot2::element_text(size = 17, face = "bold"),
          strip.background = ggplot2::element_blank(),
          axis.line = ggplot2::element_line(linewidth = 1),
          axis.ticks.length = ggplot2::unit(2.5,"mm"),
          axis.ticks = ggplot2::element_line(linewidth = 1),
          axis.title.x = ggplot2::element_text(size = 20, face = "bold"),
          axis.title.y = ggplot2::element_text(size = 15, face = "bold"),
          axis.text = ggplot2::element_text(size = 13)) +
    ggplot2::scale_fill_manual(values = c("#9da6ad","#aeaeb0","#b4a9a9")) +
    #ggplot2::scale_fill_manual(values = c("#b9bfc4","#c8c8c9","#ccc4c4")) +
    ggplot2::facet_wrap(~transformation, scales = "free") +
    ggplot2::geom_text(trfmed_summary, mapping =
                         ggplot2::aes(label = paste0("SE: ", var_se, "\n Skew: ",
                                   var_skew, "\n Kurtosis: ",
                                   var_kurtosis, "\n Z-Score Min: ",
                                   var_zmin, "\n Z-Score Max: ",
                                   var_zmax),
                    group = transformation), x = Inf, y = Inf, hjust = 1, vjust = 1, size = 5)

  pt_viz

}
