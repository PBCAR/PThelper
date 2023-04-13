utils::globalVariables(c("id","pred","alpha","q0"))

#' PT ELASTICITY
#'
#' This function derives and visualizes elasticity using the Koffarnus, Franck, Stein, and Bickel (2015) equation. Elasticity can be
#' derived for the entire sample or for individuals. This function will also visualize the demand curve(s).
#'
#' @param pt A data frame consisting of the `id_var` and purchase task variables.
#' @param id_var The name of the unique identifier as identified in the data frame.
#' @param type The type of elasticity curve to derive from the data, one of c("overall","individual"). The default is "overall" which
#' will calculate overall elasticity for the entire data frame.
#' @param k The k-value to use for curve fitting. The default is NULL, in which the k-value is calculated from the entire sample, with
#' k representing the span of consumption in log10 units. Otherwise, a single numerical value can be given as the k-value, allowing for
#' comparisons across studies, since k influences the calculation of elasticity.
#' @param y_type The way the y-axis (prices) are to be displayed. One of c("log10","original"), with "log10" as the default, or
#' "original" for the existing price values.
#' @return A ggplot2 graphical object; For `type` "individual", the original pt data frame plus the derived values for each individual
#' is returned.
#' @export

pt_elasticity <- function(pt, id_var, type, k = NULL, y_type = "log10") {

  pt_names <- names(pt)
  prices <- pt_names[pt_names!=id_var & pt_names!="Intensity" & pt_names!="Breakpoint" & pt_names!="Omax" & pt_names!="Pmax"]
  names(pt)[names(pt) == id_var] <- "id"

  pt$q0 <- as.numeric(pt[,prices[1]])

  pt_long <- stats::reshape(as.data.frame(pt), idvar = "id",
                            varying = prices,
                            v.names = c("q"), timevar = c("c"), sep = "", direction = "long")

  pt_long <- pt_long[order(pt_long$id),]
  pt_long$c <- as.numeric(prices)

  ### grab mean values of y for every price (x); then calculate max value minus min value

  pt_mean <- stats::aggregate(pt_long[c("q")], by = list(c = pt_long[,"c"]), function(x) mean(x, na.rm = T))
  pt_mean$log_q <- log10(pt_mean$q)
  k_range <- (max(pt_mean$log_q)-min(pt_mean$log_q))
  q0_start <- max(pt_mean$q)

  if(is.null(k)){
    kval <- round(k_range,1)
  } else if(!is.null(k)){
    kval <- k
  }

  equation <- "q ~ q0 * 10^(k * (exp(-alpha * q0 * c)-1))"

  ### --- OVERALL ELASTICITY

  if(type=="overall"){

    ### From mean

    pt_mean$k <- kval
    pt_mean$q0 <- q0_start

    pt_mod_mean <- stats::nls(equation, data = pt_mean, start = list(q0 = q0_start, alpha = 0.001))

    coef_mean <- as.character(stats::coef(pt_mod_mean))
    coef_mean_dat <- data.frame(q0 = coef_mean[1], alpha = coef_mean[2])


    ### Calculate pseudo-R^2

    rsquared <- 1 - sum(stats::resid(pt_mod_mean)^2)/ sum((pt_long$q - mean(pt_long$q))^2)

    ### PREP for visualization
    pt_mean$pred <- stats::predict(pt_mod_mean)
    pt_mean$c[pt_mean$c==0] <- 0.001

    pt_plot <- ggplot2::ggplot(pt_mean, ggplot2::aes(x = c, y = q)) +
      ggplot2::geom_line(ggplot2::aes(y = pred), linewidth = 2, colour = "#999999") +
      ggplot2::geom_point(size = 2.5) +
      ggplot2::scale_x_log10(breaks=c(0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000, 100000),
                             labels=c(paste0(min(prices)), 0.01, 0.1, 1, 10, 100, 1000, 10000, 100000)) +
      ggplot2::theme_classic() + ggplot2::xlab("\n Price (Log)") + ggplot2::ylab("Consumption \n") +
      ggplot2::ggtitle("Mean Demand Curve") +
      ggplot2::geom_text(data = coef_mean_dat,
                         ggplot2::aes(x = Inf, y = Inf,label = paste0(
                           "\n \u03b1: ", round(as.numeric(alpha), digits = 4),"     ",
                           "\n Q0: ", round(as.numeric(q0), digits = 5),"     ",
                           "\n R\u00b2: ", round(as.numeric(rsquared), digits = 4),"     ")), hjust = 1, vjust = 1,
                         size = 7, fontface = "bold", show.legend = F) +
      ggplot2::theme(plot.title = ggplot2::element_text(size = 25, face = "bold", hjust = 0.5),
                     axis.title = ggplot2::element_text(size = 22, face = "bold"),
                     axis.text = ggplot2::element_text(size = 19),
                     panel.border = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 2))


  }

  ### --- INDIVIDUAL ELASTICITY

  if(type == "individual"){

    pt_long$k <- kval
    pt_elast <- data.frame(id = NULL, q0 = NULL, alpha = NULL)
    pt_fit <- data.frame(id = NULL, pred = NULL, res = NULL)

    for(id_num in pt$id){

      pt_dat <- pt_long[(pt_long$id == id_num),]
      q0_start_i <- max(pt_dat$q)

      if(pt_dat$q[pt_dat$c==prices[1]]==0 | pt_dat$q[pt_dat$c==prices[2]]==0){
        coef_i <- c(NA,NA)
        res_i <- rep(NA,length(prices))
        pred_i <- rep(NA,length(prices))
      }

      if(pt_dat$q[pt_dat$c==prices[1]]!=0 & pt_dat$q[pt_dat$c==prices[2]]!=0){

        pt_mod_i <- stats::nls(equation, data = pt_dat, start = list(q0 = q0_start_i, alpha = 0.001))

        pred_i <- stats::predict(pt_mod_i)
        res_i <- stats::resid(pt_mod_i)
        coef_i <- as.character(stats::coef(pt_mod_i))
      }

      id_dat <- data.frame(id = id_num, q0 = coef_i[1], alpha = coef_i[2])
      fit_dat <- data.frame(id = id_num, pred = c(pred_i), res = c(res_i), c = as.numeric(prices))

      pt_elast <- rbind(pt_elast,id_dat)
      pt_fit <- rbind(pt_fit,fit_dat)
    }

    pt_plot_dat <- pt_fit
    pt_plot_dat$c[pt_plot_dat$c==0] <- 0.001

    pt_plot <- ggplot2::ggplot(pt_plot_dat, ggplot2::aes(x = c, y = pred, group = id)) +
      ggplot2::geom_line(linewidth = 1, colour = "#999999", alpha  = 0.6) +
      ggplot2::scale_x_log10(breaks=c(0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000, 100000),
                             labels=c(paste0(min(prices)), 0.01, 0.1, 1, 10, 100, 1000, 10000, 100000)) +
      ggplot2::theme_classic() + ggplot2::xlab("\n Price (Log)") + ggplot2::ylab("Consumption (Log) \n") +
      ggplot2::ggtitle("Individual Demand Curves") +
      ggplot2::theme(plot.title = ggplot2::element_text(size = 25, face = "bold", hjust = 0.5),
                     axis.title = ggplot2::element_text(size = 22, face = "bold"),
                     axis.text = ggplot2::element_text(size = 19),
                     panel.border = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 2))


    ### PREPARE data for export

    pt_elast$q0 <- round(as.numeric(pt_elast$q0),2)
    pt_elast$alpha <- round(as.numeric(pt_elast$alpha),4)
    colnames(pt_elast) <- c("id","Q0","Elasticity")

    }

  if(y_type=="log10"){
    pt_plot <- pt_plot + ggplot2::scale_y_log10(breaks=c(0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000, 100000),
                                          labels=c(0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000, 100000)) +
      ggplot2::ylab("Consumption (Log) \n")
  }

  print(pt_plot)

  if(is.null(k)){
    cat(" Calculated k-value: ", kval, "\n")
  }

  if(type=="overall"){
  cat(" R-squared value:", round(rsquared,5),"\n")
  }

  if(type=="individual"){

    pt_final <- merge(pt[c("id",pt_names[pt_names!=id_var])], pt_elast, by = "id", all.x = T)
    names(pt_final)[names(pt_final) == "id"] <- id_var
    return(pt_final)
  }

}
