#' Linear (ln-ln) demand curve fitting
#'
#' This function fits a linear (ln-ln) demand curve in order to calculate eta. See details for additional information
#' on the linear demand equation.
#'
#' The linear demand equation allows for elasticity (eta) to be represented by a constant across the span of the prices.
#' Eta is estimated from the slope of the linear demand equation, and represents the percent change in consumption associated
#' with a one percent increase in price by taking the natural log of both consumption and price. See Gilroy et al. (2020) for further
#' discussion on eta.
#'
#' Gilroy, S. P., Kaplan, B. A., & Reed, D. D. (2020). Interpretation (s) of elasticity in operant demand. Journal of the
#' Experimental Analysis of Behavior, 114(1), 106-115.
#'
#' @param pt A data frame consisting of the `id_var` and purchase task variables.
#' @param id_var The name of the unique identifier as identified in the data frame.
#' @param type The level for fitting the demand curves, one of c("overall","group","individual"). The default is "overall" which will calculate
#' overall demand for the entire data frame. For `type` "group", demand for each of the groups as identified by the `group_var` are visualized.
#' When `type` equals "individual", the equation-derived demand indicator eta is calculated for each individual as identified by `id_var`.
#' @param zero_val The value to substitute zero values in the price and consumption data (default is 0.001).
#' @param group_var The name of the grouping variable when `type` equals "group".
#' @examples
#' ### --- Load Data
#' data("cpt_data")
#'
#' ### --- Prep Data
#' pt <- price_prep(cpt_data, id_var = "ID", vars = c(paste0("cpt",1:15)),
#' prices = c("0","0.05","0.10","0.20","0.30","0.40","0.50", "0.75","1","2","3","4","5","7.5","10"))
#'
#' pt2 <- pt_prep(pt, id_var = "ID", type = "partial", remove0 = TRUE, max_val = 99)
#' pt3 <- pt_qc(pt2, id_var = "ID", type = "partial")
#'
#' ### --- Function Example
#' pt4 <- pt_linear(pt3$data, id_var = "ID", type = "individual")
#'
#' @return A ggplot2 graphical object; For `type` "individual", the original pt data frame plus the derived values for each individual is returned.
#' @export

pt_linear <- function(pt, id_var, type = NULL, zero_val = 0.001, group_var = NULL){

  if(is.null(type)) stop(rlang::format_error_bullets(c( "!" = c("Type required. Please select one of c('overall','group','individual') using the 'type' argument."))), call. = FALSE)
  if(!is.data.frame(pt)) stop(rlang::format_error_bullets(c( x = c("'pt' must be a data frame."))), call. = FALSE)

  pt_names <- names(pt)
  var_exclude <- c("Intensity","Breakpoint","Omax","Pmax","Alpha","K","Q0","UnitElasticity","R2","AUC")

  if(type == "overall" | type == "individual"){

    prices <- pt_names[pt_names!=id_var & !pt_names %in% var_exclude]

    suppressWarnings({
      if(length(prices[is.na(as.numeric(prices))])==length(prices)) stop(rlang::format_error_bullets(c( x = c("Names of purchase task variables must be numeric. Use `price_prep()` to rename variables."))), call. = FALSE)
      if(length(prices[is.na(as.numeric(prices))])>0) stop(rlang::format_error_bullets(c( x = c("Variables other than 'id_var' and the purchase task items are detected. Please include only the variables required."))), call. = FALSE)
      })

  } else if(type == "group"){

    if(is.null(group_var)) stop(rlang::format_error_bullets(c( "!" = c("The grouping variable ('group_var') is missing."))), call. = FALSE)

    prices <- pt_names[pt_names!=id_var & pt_names!=group_var & !pt_names %in% var_exclude]
    names(pt)[names(pt) == group_var] <- "group"

    suppressWarnings({
      if(length(prices[is.na(as.numeric(prices))])==length(prices)) stop(rlang::format_error_bullets(c( x = c("Names of purchase task variables must be numeric. Use `price_prep()` to rename variables."))), call. = FALSE)
      if(length(prices[is.na(as.numeric(prices))])>0) stop(rlang::format_error_bullets(c( x = c("Variables other than 'id_var', 'group_var', and the purchase task items are detected. Please include only the variables required."))), call. = FALSE)
    })


  }

  names(pt)[names(pt) == id_var] <- "id"

  ### Calculate log10 label breaks
  zero_conv <- (as.numeric(prices[2])-as.numeric(prices[1]))/10
  log_labels <- 10^(log10(10^ceiling(log10(zero_conv))):100 * 1)


  pt_long <- stats::reshape(as.data.frame(pt), idvar = "id",
                            varying = prices,
                            v.names = c("q"), timevar = c("c"), sep = "", direction = "long")

  pt_long <- pt_long[order(pt_long$id),]
  pt_long$c <- as.numeric(prices)

  message(rlang::format_error_bullets(c(i = "NOTE: \u03B7 represents the % change in consumption associated with a 1% increase in price")))

  ##### ----- OVERALL ETA

  if(type=="overall"){

  ### Replace 0s with non-zero value
  pt_long$c[pt_long$c==0] <- zero_val
  pt_long$q[pt_long$q==0] <- zero_val

  ### grab mean values of q for every price (c); then calculate max value minus min value
  pt_mean <- stats::aggregate(pt_long[c("q")], by = list(c = pt_long[,"c"]), function(x) mean(x, na.rm = T))

  pt_mod <- stats::lm(log(q) ~ log(c), data = pt_mean)

  eta <- as.numeric(stats::coef(pt_mod)[2])
  rsquared <- summary(pt_mod)$r.squared

  ### Predict using more values of c, rather than set prices (creates smoother line)
  pt_pred <- data.frame(c = seq(zero_val,as.numeric(prices[length(prices)]), zero_conv))
  suppressWarnings({
    pt_pred$pred <- stats::predict(pt_mod, pt_pred)
  })

  pt_pred$pred[pt_pred$pred<0] <- NA

  ### Transform consumption and if mean data point is < the min predicted values; don't show
  pt_mean$q <- log(pt_mean$q)
  pt_mean$q[pt_mean$q<min(pt_pred$pred, na.rm = TRUE)] <- NA

  pt_anno <- paste0("\u03B7: ",signif(eta),"     ","R\u00b2: ",signif(rsquared))

  pt_plot <- ggplot2::ggplot(pt_mean, ggplot2::aes(x = c, y = q)) +
    ggplot2::geom_line(pt_pred, mapping = ggplot2::aes(y = pred), lineend = "round", linewidth = 2, colour = "#999999") +
    ggplot2::geom_point(size = 2.5) +
    ggplot2::scale_x_log10(breaks = c(zero_val,log_labels), labels = c(zero_val,log_labels)) +
    ggplot2::scale_y_log10(breaks = c(zero_val,log_labels), labels = c(zero_val,log_labels)) +
    ggplot2::theme_classic() + ggplot2::xlab("\n Price (Log)") + ggplot2::ylab("Consumption (Log) \n") +
    ggplot2::ggtitle("Mean Demand Curve") +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 25, face = "bold", hjust = 0.5),
                   axis.title = ggplot2::element_text(size = 22, face = "bold"),
                   axis.text = ggplot2::element_text(size = 19),
                   axis.ticks.length = ggplot2::unit(2,"mm"),
                   strip.background = ggplot2::element_blank(),
                   strip.text = ggplot2::element_blank(),
                   axis.line = ggplot2::element_line(linewidth = 1.5))

  message(rlang::format_error_bullets(c(i = pt_anno)))

  ##### ----- GROUP ETA

  } else if(type == "group"){

    ### Replace 0s with non-zero value
    pt_long$c[pt_long$c==0] <- zero_val
    pt_long$q[pt_long$q==0] <- zero_val

    ### grab mean values of y for every price (x) by group; then calculate max value minus min value
    pt_group_mean <- stats::aggregate(pt_long[c("q")], list(c = pt_long[,"c"], group = pt_long[,"group"]), function(x) mean(x, na.rm = T))

    pt_group_mean$group <- as.factor(pt_group_mean$group)

    ### Create loop for each group
    group_uniq <- unique(pt_group_mean$group)

    c_est <- c(seq(zero_val,as.numeric(prices[length(prices)]), zero_conv))
    pt_group_fit <- data.frame(group = NULL, c = NULL, pred = NULL)
    pt_group_elast <- data.frame(group = NULL, eta = NULL, r2 = NULL)

    for(group_u in group_uniq){

      pt_g <- pt_group_mean[(pt_group_mean$group == group_u),]

      pt_mod_g <- stats::lm(log(q) ~ log(c), data = pt_g)

      pt_elast <- data.frame(group = group_u, eta = as.numeric(stats::coef(pt_mod_g)[2]), r2 = summary(pt_mod_g)$r.squared)
      pt_group_pred <- data.frame(c = c_est)

      suppressWarnings({
        pt_group_pred$pred <- stats::predict(pt_mod_g, pt_group_pred)
      })

      pt_group_pred$group <- group_u

      pt_group_fit <- rbind(pt_group_fit,pt_group_pred)
      pt_group_elast <- rbind(pt_group_elast,pt_elast)

    }

    pt_group_fit$pred[pt_group_fit$pred<0] <- NA

    ### Transform consumption and if mean data point is < the min predicted values; don't show
    pt_group_mean$q <- log(pt_group_mean$q)
    pt_group_mean$q[pt_group_mean$q<min(pt_group_pred$pred, na.rm = TRUE)] <- NA

    pt_group_elast$label <- paste0(pt_group_elast$group,": ",
                             "\u03B7: ", signif(as.numeric(pt_group_elast$eta)), "     ",
                             "R\u00b2: ", signif(as.numeric(pt_group_elast$r2)),"     ")

    pt_plot <- ggplot2::ggplot(pt_group_mean, ggplot2::aes(x = c, y = q, group = group, colour = group)) +
      ggplot2::geom_line(pt_group_fit, mapping = ggplot2::aes(y = pred), lineend = "round", alpha = 1, linewidth = 2) +
      ggplot2::geom_point(alpha = 0.8, size = 3.5) +
      ggplot2::scale_x_log10(breaks = c(zero_val,log_labels), labels = c(zero_val,log_labels)) +
      ggplot2::scale_y_log10(breaks = c(zero_val,log_labels), labels = c(zero_val,log_labels)) +
      ggplot2::theme_classic() + ggplot2::xlab("\n Price (Log)") + ggplot2::ylab("Consumption (Log) \n") +
      ggplot2::ggtitle("Mean Demand Curve") +
      ggplot2::theme(plot.title = ggplot2::element_text(size = 25, face = "bold", hjust = 0.5),
                     axis.title = ggplot2::element_text(size = 22, face = "bold"),
                     axis.text = ggplot2::element_text(size = 19),
                     axis.ticks.length = ggplot2::unit(2,"mm"),
                     strip.background = ggplot2::element_blank(),
                     strip.text = ggplot2::element_blank(),
                     axis.line = ggplot2::element_line(linewidth = 1.5),
                     legend.position = "top",
                     legend.title = ggplot2::element_text(size = 17, face = "bold", vjust = 0.5),
                     legend.text = ggplot2::element_text(size = 15, vjust = 0.5)) +
      ggplot2::guides(colour = ggplot2::guide_legend(title = group_var, override.aes = list(alpha = 1, linewidth = 5)))

    if(length(group_uniq)<=3){
      pt_plot <- pt_plot + ggplot2::scale_colour_manual(values = c("#3E668E","#8E3E3E","#66526E"))
    }

    for (out in pt_group_elast$label) {
      message(rlang::format_error_bullets(c(i = out)))
    }

    ##### ----- INDIVIDUAL ETA

    } else if(type == "individual"){

    pt_elast <- data.frame(id = NULL, Eta = NULL, R2 = NULL)

    for(id_num in pt$id){

      pt_i <- pt_long[(pt_long$id == id_num),]

      if(pt_i$q[pt_i$c==prices[1]]==0 | pt_i$q[pt_i$c==prices[2]]==0){

        eta_i <- NA
        r2_i <- NA

      } else if(pt_i$q[pt_i$c==prices[1]]!=0 & pt_i$q[pt_i$c==prices[2]]!=0){

        pt_i$c[pt_i$c==0] <- zero_val
        pt_i$q[pt_i$q==0] <- zero_val

        pt_mod <- stats::lm(log(q) ~ log(c), data = pt_i)

        eta_i <- as.numeric(stats::coef(pt_mod)[2])
        r2_i <- summary(pt_mod)$r.squared
      }

      id_dat <- data.frame(id = id_num, Eta = eta_i, R2 = r2_i)

      pt_elast <- rbind(pt_elast,id_dat)

    }

  }

  if(type == "overall" | type == "group"){
    ### Suppress warning about self$trans$transform(x) and missing values
    suppressWarnings({
      print(pt_plot)
    })
  } else if(type == "individual"){

    pt_final <- merge(pt[c("id",pt_names[pt_names!=id_var])], pt_elast, by = "id", all.x = T)
    names(pt_final)[names(pt_final) == "id"] <- id_var
    return(pt_final)
  }

}
