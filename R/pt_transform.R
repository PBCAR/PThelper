utils::globalVariables(c("var","transformation","var_se","var_skew","var_kurtosis","var_zmin","var_zmax"))

#' Plotting of variable transformations
#'
#' This function helps users to determine whether a transformation of log or square-root would be appropriate for a specific purchase task indicator.
#' Specifically, it provides values which measure skewness and kurtosis, as well as the minimum and maximum values of z-scores.
#'
#' @param pt A data frame consisting of the purchase variable `pt_var` to visualize.
#' @param pt_var The name of the purchase task variable to visualize, as identified in the data frame.
#' @param zero_val The small constant value to add to zero-value responses to allow for transformation. The default is 0.001.
#' @param distribution A logical argument on whether a plot should be produced. The default is TRUE.
#' @examples
#' ### --- Example Data
#'
#' pt <- data.frame("ID" = c(1:36),
#' "Breakpoint" = c(1,2,5,0,10,3,0.5,0.2,0.3,3,4,5,7.5,0.5,2,0,0.1,
#'                  0.5,0.5,0,3,2,2,1,2,3,4,1,0,2,0,5,5,7.5,2,3))
#'
#' ### --- Function Example
#'
#' pt_transform(pt, pt_var = "Breakpoint")
#'
#' @return A data frame with transformation diagnostics for the 'pt_var'. If
#' distribution is set to TRUE, then a ggplot2 graphical object is also returned.
#' @export

pt_transform <- function(pt, pt_var, zero_val = 0.001, distribution = TRUE) {

  if(!is.data.frame(pt)) stop(rlang::format_error_bullets(c( x = c("'pt' must be a data frame."))), call. = FALSE)
  if(!pt_var %in% names(pt)) stop(rlang::format_error_bullets(c( x = c("The indicator variable ('pt_var') does not exist within 'pt'."))), call. = FALSE)

  ##### ----- CREATE TRANSFORMATIONS

  names(pt)[names(pt) == pt_var] <- "var_orig"

  pt$id <- rownames(pt)

  ### --- ADD small constant for log transformation if zero-values exist in data
  if(0 %in% pt$var_orig){

    pt$var_orig0 <- pt$var_orig + zero_val

    pt$var_log10 <- log10(pt$var_orig0)
    pt$var_sqrt <- sqrt(pt$var_orig)
  } else{

    pt$var_log10 <- log10(pt$var_orig)
    pt$var_sqrt <- sqrt(pt$var_orig)
  }

  ### --- SUMMARY STATISTICS: Original
  trfmed_orig <- data.frame(var_skew = skewness(pt$var_orig),
                            var_kurtosis = kurtosis(pt$var_orig),
                            var_zmin = min(scale(pt$var_orig),na.rm = TRUE),
                            var_zmax = max(scale(pt$var_orig), na.rm = TRUE),
                            transformation = "Original")

  ### SUMMARY STATISTICS: Log10
  trfmed_log10 <- data.frame(var_skew = skewness(pt$var_log10),
                             var_kurtosis = kurtosis(pt$var_log10),
                             var_zmin = min(scale(pt$var_log10),na.rm = TRUE),
                             var_zmax = max(scale(pt$var_log10), na.rm = TRUE),
                             transformation = "Log")

  ### SUMMARY STATISTICS: Square Root
  trfmed_sqrt <- data.frame(var_skew = skewness(pt$var_sqrt),
                            var_kurtosis = kurtosis(pt$var_sqrt),
                            var_zmin = min(scale(pt$var_sqrt),na.rm = TRUE),
                            var_zmax = max(scale(pt$var_sqrt), na.rm = TRUE),
                            transformation = "Square Root")

  ### COMBINE statistics
  trfmed_summary <- rbind(trfmed_orig,trfmed_log10,trfmed_sqrt)

  pt_long <- stats::reshape(as.data.frame(pt), idvar = "id",
                            varying = c("var_orig","var_log10","var_sqrt"),
                            timevar = c("transformation"), sep = "_", direction = "long")

  pt_long$transformation[pt_long$transformation=="orig"] <- "Original"
  pt_long$transformation[pt_long$transformation=="log10"] <- "Log"
  pt_long$transformation[pt_long$transformation=="sqrt"] <- "Square Root"

  pt_viz <- ggplot2::ggplot(pt_long[!is.na(pt_long$var),], ggplot2::aes (x = var, fill = transformation)) +
    ggplot2::geom_density(show.legend = FALSE, linewidth = 1, lineend = "round") +
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
    ggplot2::facet_wrap(~transformation, scales = "free")

  suppressMessages({
    print(pt_viz)
  })

  trfmed_summary <- trfmed_summary[c("transformation","var_skew","var_kurtosis","var_zmin","var_zmax")]
  names(trfmed_summary) <- c("Transformation","Skew","Kurtosis","ZScoreMin","ZScoreMax")

  return(trfmed_summary)

}
