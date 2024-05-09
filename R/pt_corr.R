utils::globalVariables(c("Var1","Var2","coefficient","pval_sign"))

#' Pairwise Pearson correlations
#'
#' This function provides coefficients and p-values for a pairwise Pearson correlation, and can also produce a heatmap.
#'
#' @param pt A data frame which includes only variables to be included in the correlation testing.
#' @param heatmap A logical argument on whether a heatmap should be produced. The default is TRUE.
#' @param alpha The alpha value to determine significance, in which significant p-values will have the corresponding correlation
#' coefficient in the heatmap denoted in bold text. The default alpha value is 0.05.
#' @param coef_size The text size of the coefficients in the heatmap, when the heatmap argument is set to TRUE. Default is 7.
#' @examples
#' ### --- Example Data
#'
#' pt <- data.frame("ID" = c(1:36),
#' "Intensity" = c(10,12,15,0,99,11,7,6,12,7,8,10,5,6,10,0,3,
#'                 7,5,0,2,3,5,6,10,15,12,7,0,9,0,6,7,8,4,5),
#' "Breakpoint" = c(1,2,5,0,10,3,0.5,0.2,0.3,3,4,5,7.5,0.5,2,0,0.1,
#'                  0.5,0.5,0,3,2,2,1,2,3,4,1,0,2,0,5,5,7.5,2,3),
#' "Eta" = c(rnorm(36,0.16,0.19)))
#'
#'### --- Function Example
#'
#' pt2 <- pt_corr(pt[c(2:4)])
#'
#' @return A data frame with correlation coefficients and p-values for each variable included in the correlation testing. If
#' heatmap is set to TRUE, then a ggplot2 graphical object is also returned.
#' @export

pt_corr <- function(pt, heatmap = TRUE, alpha = 0.05, coef_size = 7){

  if(!is.data.frame(pt)) stop(rlang::format_error_bullets(c( x = c("'pt' must be a data frame."))), call. = FALSE)

  ##### ----- Pairwise Correlations

  ptdat <- {}

  for(var in names(pt)){
    ptdat[[var]] <- apply(pt, 2, stats::cor.test, pt[[var]], method = "pearson", use = "pairwise.complete.obs")
  }

  corr <- {}
  pval <- {}

  for(i in 1:length(ptdat)){
    corr[[i]] <- sapply(ptdat[[i]][1:length(ptdat)], "[[","estimate")
    pval[[i]] <- sapply(ptdat[[i]][1:length(ptdat)], "[[", "p.value")

  }

  corr <- data.frame("corr" = do.call(rbind.data.frame,corr))
  colnames(corr) <- names(ptdat)
  rownames(corr) <- names(ptdat)

  pval <- data.frame("pval" = do.call(rbind.data.frame,pval))
  colnames(pval) <- names(ptdat)
  rownames(pval) <- names(ptdat)

  ### Isolate Upper Triangle Function
  upper_tri <- function(x){
    x[lower.tri(x)] <- NA
    return(x)
  }

  ### Create upper triangle of coefficients and p-values
  upper_corr <- upper_tri(corr)
  upper_p <- upper_tri(pval)

  ### LONG format of matrix
  corr_dat <- base_melt(upper_corr)
  pval_dat <- base_melt(upper_p)

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
    ggplot2::scale_y_discrete(position = "right") + ggplot2::theme_minimal() + ggplot2::coord_fixed(clip = "off") +
    ggplot2::geom_text(corr_dat[(corr_dat$pval_sign=="N.S." & !is.na(corr_dat$pval_sign)),],
              mapping = ggplot2::aes(Var2, Var1, label = round(coefficient,2), colour = pval_sign),
              size = coef_size, colour = "#333333", show.legend = FALSE) +
    ggplot2::geom_text(corr_dat[(corr_dat$pval_sign!="N.S." & !is.na(corr_dat$pval_sign)),],
              mapping = ggplot2::aes(Var2, Var1, label = round(coefficient,2), colour = pval_sign),
              size = coef_size, fontface = "bold", colour = "#000000", show.legend = F) +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(),
          axis.title = ggplot2::element_blank(),
          panel.grid.major = ggplot2::element_blank(),
          panel.border = ggplot2::element_blank(),
          legend.text = ggplot2::element_text(size = 15, face = "bold", hjust = "0.5"),
          plot.title = ggplot2::element_text(size = 30, face = "bold", hjust = 0.5),
          legend.title = ggplot2::element_text(face = "bold",size = 15, hjust = 0.5),
          legend.direction = "horizontal",
          legend.position = "inside",
          legend.position.inside = c(0.3,0.9),
          axis.text.y = ggplot2::element_text(face = "bold", size = 15),
          axis.text.x = ggplot2::element_text(face = "bold", size = 15, angle = 30, vjust = 1, hjust = 1),
          axis.line = ggplot2::element_line(linewidth = 1.5),
          axis.ticks.length = ggplot2::unit(2.5,"mm"),
          axis.ticks = ggplot2::element_line(linewidth = 1)) +
    ggplot2::guides(fill = ggplot2::guide_colorbar(barwidth = 15, barheight = 1, title.position = "top", ticks.linewidth = 0))

  print(hm_plot)

  }

  return(ptcorr_final)

}


