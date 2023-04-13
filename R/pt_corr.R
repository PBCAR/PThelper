utils::globalVariables(c("Var1","Var2","coefficient","pval_sign"))

#' PT CORR
#'
#' This function will provide coefficients and p-values for a pairwise pearson correlation, and can also produce a heatmap
#' @param pt A data frame which includes only variables to be included in the correlation testing.
#' @param heatmap A logical argument on whether a heatmap should be produced. The default is TRUE.
#' @param alpha The alpha value to determine significance, in which significant p-values will have the corresponding correlation
#' coefficient in the heatmap in bold. The default alpha value is 0.05.
#' @param coef_size The text size of the coefficients in the heatmap, when the heatmap argument is set to TRUE. Default is 7.
#' @return A data frame with correlation coefficients and p-values for each variable included in the correlation testing. If
#' heatmap is set to TRUE, then a ggplot2 graphical object is also returned.
#' @export

pt_corr <- function(pt, heatmap = TRUE, alpha = 0.05, coef_size = 7){

  ### complete or pairwise

  pt <- psych::corr.test(pt, use = "pairwise", method = "pearson")

  ### Isolate Upper Triangle Function
  upper_tri <- function(x){
    x[lower.tri(x)] <- NA
    return(x)
  }

  ### Create upper triangle of coefficients and p-values
  upper_corr <- upper_tri(pt[["r"]])
  upper_p <- upper_tri(pt[["p"]])

  ### LONG format of matrix
  corr_dat <- reshape2::melt(upper_corr, na.rm = TRUE)
  pval_dat <- reshape2::melt(upper_p, na.rm = T)

  pval_dat$pval_sign <- ifelse(pval_dat$value<alpha,paste0("p < ",alpha),"N.S.")
  pval_dat$pval_sign[pval_dat$Var1==pval_dat$Var2] <- NA

  corr_dat$coefficient <- corr_dat$value

  ### Keep raw p-values for output

  ptcorr_final <- merge(corr_dat[c("Var1","Var2","coefficient")],
                       pval_dat[c("Var1","Var2","value")], by = c("Var1","Var2"), all = T)
  colnames(ptcorr_final) <- c("Variable1","Variable2","Coefficient","pvalue")
  ptcorr_final$pvalue[ptcorr_final$Variable1==ptcorr_final$Variable2] <- 1

  ptcorr_final$pvalue <- format(ptcorr_final$pvalue, scientific = FALSE)

  if(heatmap == TRUE){
  ### MERGE p-values with the correlation coefficients
  corr_dat <- merge(corr_dat[c("Var1","Var2","coefficient")],
                    pval_dat[c("Var1","Var2","pval_sign")], by = c("Var1","Var2"), all = T)

  hm_plot <- ggplot2::ggplot(data = corr_dat, ggplot2::aes(Var2, Var1, fill = coefficient)) + ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient2(low = "#3e668e", high = "#8e3e3e", mid = "#c8c8c9",
                         midpoint = 0, limit = c(-1,1), space = "Lab",
                         name = "Pearson Correlation") +
    ggplot2::scale_y_discrete(position = "right") + ggplot2::theme_minimal() + ggplot2::coord_fixed() +
    ggplot2::geom_text(corr_dat[(corr_dat$pval_sign=="N.S." & !is.na(corr_dat$pval_sign)),],
              mapping = ggplot2::aes(Var2, Var1, label = round(coefficient,2), colour = pval_sign),
              size = coef_size, colour = "#333333", show.legend = F) +
    ggplot2::geom_text(corr_dat[(corr_dat$pval_sign!="N.S." & !is.na(corr_dat$pval_sign)),],
              mapping = ggplot2::aes(Var2, Var1, label = round(coefficient,2), colour = pval_sign),
              size = coef_size, fontface = "bold", colour = "#000000", show.legend = F) +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(),
          axis.title.y = ggplot2::element_blank(),
          panel.grid.major = ggplot2::element_blank(),
          panel.border = ggplot2::element_blank(),
          legend.text = ggplot2::element_text(size = 15, face = "bold", hjust = "0.5"),
          plot.title = ggplot2::element_text(size = 30, face = "bold", hjust = 0.5),
          legend.title = ggplot2::element_text(face = "bold",size = 15, hjust = 0.5),
          legend.direction = "horizontal",
          legend.position = c(0.2,0.8),
          axis.text = ggplot2::element_text(face = "bold", size = 15),
          axis.line = ggplot2::element_line(linewidth = 1.5),
          axis.ticks.length = ggplot2::unit(2.5,"mm"),
          axis.ticks = ggplot2::element_line(linewidth = 1)) +
    ggplot2::guides(fill = ggplot2::guide_colorbar(barwidth = 15, barheight = 1, title.position = "top", ticks.linewidth = 0))

  print(hm_plot)

  }

  return(ptcorr_final)

}


