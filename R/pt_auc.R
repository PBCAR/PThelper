#' Area-under-the-curve demand calculation
#'
#' This function calculates the area-under-the-curve (AUC). See details for additional information.
#'
#' The use of AUC as a measure of demand was proposed by Amlung et al. (2015), and is derived by calculating the area of several
#' trapezoids under the empirical consumption curve. A putative advantage of AUC is that it is an aggregated empirical indicator
#' that does not rely on model assumptions inherent in non-linear regression approaches. Of note, although conceptually reflecting
#' the overall demand curve, in practice it has been highly correlated with individual demand curve indicator of Omax (Amlung et al., 2015).
#'
#' The area of each trapezoid is calculated by the formula (P2 − P1)*[(Q2 + Q1)/ 2]. Here, P2 and P1 are price P and price P - 1, whilst
#' Q2 and Q1 are consumption Q at price P and price P - 1. The AUC is expressed as a proportion of the total area (range from 0 to 1),
#' allowing for comparison across different purchase tasks or samples.
#'
#' Amlung, M., Yurasek, A., McCarty, K. N., MacKillop, J., & Murphy, J. G. (2015). Area Under the Curve as a Novel Metric of Behavioral
#' Economic Demand for Alcohol. Experimental and Clinical Psychopharmacology, 23(3), 168.
#'
#' @param pt A data frame consisting of the `id_var` and purchase task variables.
#' @param id_var The name of the unique identifier as identified in the data frame.
#' @param type The level for fitting the demand curves, one of c("overall","group","individual"). The default is "overall" which will calculate
#' overall demand for the entire data frame. For `type` "group", equation-derived demand indicators for each of the groups as identified by the `group_var`
#' are visualized. When `type` equals "individual", equation-derived demand indicators are calculated for each individual as identified by `id_var`.
#' @param qmax The maximum consumption amount possible on the purchase task. This must be provided in order to calculate AUC proportionately.
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
#' pt4 <- pt_auc(pt3$data, id_var = "ID", type = "individual", qmax = 99)
#'
#' @return A ggplot2 graphical object; For `type` "individual", the original pt data frame plus the derived values for each individual is returned.
#' @export

pt_auc <- function(pt, id_var, type, qmax = NULL, group_var = NULL){

  if(is.null(type)) stop(rlang::format_error_bullets(c( "!" = c("Type required. Please select one of c('overall','group','individual') using the 'type' argument."))), call. = FALSE)
  if(is.null(qmax)) stop(rlang::format_error_bullets(c( "!" = c("A maximum consumption amount is required. Please define using the 'qmax' argument."))), call. = FALSE)
  if(!is.data.frame(pt)) stop(rlang::format_error_bullets(c( x = c("'pt' must be a data frame."))), call. = FALSE)

  pt_names <- names(pt)
  var_exclude <- c("Intensity","Breakpoint","Omax","Pmax","Eta","Alpha","K","Q0","UnitElasticity","R2","Brute")

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

  ### Calculate acceptable non-zero value to replace 0 price points for plotting
  zero_conv <- (as.numeric(prices[2])-as.numeric(prices[1]))/10

  ### Calculate log10 label breaks
  log_labels <- 10^(log10(10^ceiling(log10(zero_conv))):100 * 1)

  pt_long <- stats::reshape(as.data.frame(pt), idvar = "id",
                            varying = prices,
                            v.names = c("q"), timevar = c("c"), sep = "", direction = "long")

  pt_long <- pt_long[order(pt_long$id),]
  pt_long$c <- as.numeric(prices)

  ### represent q (consumption) as proportion of maximum price allowed (thus needs to be inputted manually)
  pt_long$q <- pt_long$q/as.numeric(qmax)

  ### need to represent as proportion of largest price point
  prices <- as.numeric(prices)
  price_prop <- prices/max(prices)

  ##### ----- OVERALL AUC

  if(type == "overall"){

    pt_mean <- stats::aggregate(pt_long[c("q")], by = list(c = pt_long[,"c"]), function(x) mean(x, na.rm = T))

    auc <- 0

    for (i in 2:(length(price_prop))) {
      auc <- auc + ((price_prop[i] - price_prop[i-1]) * (pt_mean$q[pt_mean$c==prices[i]] + pt_mean$q[pt_mean$c==prices[i-1]])/2)
    }

    pt_mean$c[pt_mean$c==0] <- 10^ceiling(log10(zero_conv))/10

    message(rlang::format_error_bullets(c(i = c(paste0("AUC: ",signif(as.numeric(auc)))))))

    pt_plot <- ggplot2::ggplot(pt_mean, ggplot2::aes(x = c, y = q)) +
      ggplot2::geom_line(lineend = "round", linewidth = 2) + ggplot2::geom_area(alpha = 0.5) +
      ggplot2::theme_classic() + ggplot2::xlab("\n Price (Log)") + ggplot2::ylab("Proportion of Consumption \n") +
      ggplot2::scale_x_log10(breaks = c(10^ceiling(log10(zero_conv))/10,log_labels), labels = c(10^ceiling(log10(zero_conv))/10,log_labels)) +
      ggplot2::ggtitle("AUC of Mean Consumption") +
      ggplot2::theme(plot.title = ggplot2::element_text(size = 25, face = "bold", hjust = 0.5),
                     axis.title = ggplot2::element_text(size = 22, face = "bold"),
                     axis.text = ggplot2::element_text(size = 19),
                     axis.ticks.length = ggplot2::unit(2,"mm"),
                     strip.background = ggplot2::element_blank(),
                     strip.text = ggplot2::element_blank(),
                     axis.line = ggplot2::element_line(linewidth = 1.5))

    ##### ----- GROUP AUC

  } else if(type == "group"){

    pt_group_mean <- stats::aggregate(pt_long[c("q")], list(c = pt_long[,"c"], group = pt_long[,"group"]), function(x) mean(x, na.rm = T))

    pt_group_mean$group <- as.factor(pt_group_mean$group)

    ### Create loop for each group

    group_uniq <- unique(pt_group_mean$group)

    pt_auc_group <- data.frame(group = NULL, auc = NULL)

    for(group_u in group_uniq){

      pt_dat_g <- pt_group_mean[(pt_group_mean$group == group_u),]

      auc <- 0 ## reset for each group

      for (i in 2:(length(price_prop))) {
        auc <- auc + ((price_prop[i] - price_prop[i-1]) * (pt_dat_g$q[pt_dat_g$c==prices[i]] + pt_dat_g$q[pt_dat_g$c==prices[i-1]])/2)
      }

      dat_group <- data.frame(group = group_u, auc = auc)

      pt_auc_group <- rbind(pt_auc_group,dat_group)

    }

    pt_auc_group$label <- paste0(pt_auc_group$group,": AUC: ",signif(as.numeric(pt_auc_group$auc)))

    for(out in pt_auc_group$label){
      message(rlang::format_error_bullets(c(i = out)))
    }

    pt_group_mean$c[pt_group_mean$c==0] <- 10^ceiling(log10(zero_conv))/10

    pt_plot <- ggplot2::ggplot(pt_group_mean, ggplot2::aes(x = c, y = q, group = group, colour = group)) +
      ggplot2::geom_line(lineend = "round", linewidth = 2) + ggplot2::geom_area(position = "identity", alpha = 0.25, show.legend = FALSE) +
      ggplot2::theme_classic() + ggplot2::xlab("\n Price (Log)") + ggplot2::ylab("Proportion of Consumption \n") +
      ggplot2::scale_x_log10(breaks = c(10^ceiling(log10(zero_conv))/10,log_labels), labels = c(10^ceiling(log10(zero_conv))/10,log_labels)) +
      ggplot2::ggtitle("AUC of Mean Consumption by Group") +
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

    ##### ----- INDIVIDUAL AUC

  } else if(type == "individual"){

    pt_auc_i <- data.frame(id = NULL, auc = NULL)

    for(id_num in pt$id){

    pt_dat_i <- pt_long[(pt_long$id==id_num),]

    auc <- 0 ## reset for each i

      for (i in 2:(length(price_prop))) {
      auc <- auc + ((price_prop[i] - price_prop[i-1]) * (pt_dat_i$q[pt_dat_i$c==prices[i]] + pt_dat_i$q[pt_dat_i$c==prices[i-1]])/2)
      }

    dat_i <- data.frame(id = id_num, AUC = auc)

    pt_auc_i <- rbind(pt_auc_i,dat_i)

  }
  }

  if(type == "overall" | type == "group"){

    print(pt_plot)

  } else if(type == "individual"){

    pt_final <- merge(pt[c("id",pt_names[pt_names!=id_var])], pt_auc_i, by = "id", all.x = T)
    names(pt_final)[names(pt_final) == "id"] <- id_var
    return(pt_final)
  }

}
