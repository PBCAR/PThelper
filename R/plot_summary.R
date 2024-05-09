utils::globalVariables(c("id","price","measure","group","label","value"))

#' Plotting of consumption and expenditure
#'
#' This function helps users to inspect the price-level purchase task data by visualizing the mean and standard error of both consumption and expenditure
#' of either the overall sample or by a grouping variable. For visualization of consumption for each individual, please refer to the `plot_diagnostics()` function.
#'
#' @param pt A data frame consisting of the purchase variables for each `id_var` (and the grouping variable if `type` equals "group").
#' @param id_var The name of the unique identifier (ID) as found in the data frame.
#' @param type The type of plot to produce. One of c("overall","group"). The "overall" type provides a summary of all individuals in the data frame. The
#' "group" type provides a summary of consumption and expenditure by grouping variable.
#' @param group_var The grouping variable if using type "group".
#' @examples
#' ### --- Load Data
#'
#' data("cpt_data")
#'
#' ### --- Prep Data
#'
#' pt <- price_prep(cpt_data, id_var = "ID", vars = c(paste0("cpt",1:15)),
#' prices = c("0","0.05","0.10","0.20","0.30","0.40","0.50", "0.75","1","2","3","4","5","7.5","10"))
#'
#' pt2 <- pt_prep(pt, id_var = "ID", type = "partial", remove0 = TRUE, max_val = 99)
#' pt3 <- pt_qc(pt2, id_var = "ID", type = "partial")
#'
#' ### --- Function Example
#'
#' plot_summary(pt3$data, id_var = "ID", type = "overall")
#'
#' @return A ggplot2 graphical object
#' @export


plot_summary <- function(pt, id_var, type = NULL, group_var = NULL) {

  if(is.null(type)) stop(rlang::format_error_bullets(c( "!" = c("Type required. Please select one either 'overall' or 'group' using the 'type' argument."))), call. = FALSE)
  if(!is.data.frame(pt)) stop(rlang::format_error_bullets(c( x = c("'pt' must be a data frame."))), call. = FALSE)

  pt_names <- names(pt)

  if(type=="overall"){

    prices <- pt_names[pt_names!=id_var]

    suppressWarnings({
      if(length(prices[is.na(as.numeric(prices))])==length(prices)) stop(rlang::format_error_bullets(c( x = c("Names of purchase task variables must be numeric. Use `price_prep()` to rename variables."))), call. = FALSE)
      if(length(prices[is.na(as.numeric(prices))])>0) stop(rlang::format_error_bullets(c( x = c("Variables other than 'id_var' and the purchase task items are detected. Please include only the variables required."))), call. = FALSE)
    })


  } else if(type=="group"){

    if(is.null(group_var)) stop(rlang::format_error_bullets(c( "!" = c("The grouping variable ('group_var') is missing."))), call. = FALSE)

    prices <- pt_names[pt_names!=id_var & pt_names!=group_var]
    names(pt)[names(pt) == group_var] <- "group"
    pt$group <- factor(pt$group)

    suppressWarnings({
      if(length(prices[is.na(as.numeric(prices))])==length(prices)) stop(rlang::format_error_bullets(c( x = c("Names of purchase task variables must be numeric. Use `price_prep()` to rename variables."))), call. = FALSE)
      if(length(prices[is.na(as.numeric(prices))])>0) stop(rlang::format_error_bullets(c( x = c("Variables other than 'id_var', 'group_var', and the purchase task items are detected. Please include only the variables required."))), call. = FALSE)
    })

  }

  names(pt)[names(pt) == id_var] <- "id"

  pt_long <- stats::reshape(as.data.frame(pt), idvar = "id", varying = prices,
                            v.names = "consume", timevar = "price", sep = "", direction = "long")

  pt_long <- pt_long[order(pt_long$id),]
  pt_long$price <- prices

  ### Calculate acceptable non-zero value to replace 0 price points for plotting
  zero_conv <- (as.numeric(prices[2])-as.numeric(prices[1]))/10

  ### Calculate log10 label breaks
  log_labels <- 10^(log10(10^ceiling(log10(zero_conv))):100 * 1)

  ##### ----- OVERALL SUMMARY STATISTICS

  if(type=="overall"){

    pt_consume_mean <- stats::aggregate(pt_long[c("consume")], by = list(price = pt_long[,"price"]), FUN = mean)
    pt_consume_se <- stats::aggregate(pt_long[c("consume")], by = list(price = pt_long[,"price"]), FUN = se)

    names(pt_consume_mean)[names(pt_consume_mean)=="consume"] <- "mean"
    names(pt_consume_se)[names(pt_consume_se)=="consume"] <- "se"

    pt_consume <- merge(pt_consume_mean, pt_consume_se , by = "price")
    pt_consume$price <- as.numeric(pt_consume$price)
    pt_consume$measure <- "Consumption"

    ### EXPENDITURE calculation:
    pt_long$spend <- as.numeric(pt_long$price)*as.numeric(pt_long$consume)

    pt_spend_mean <- stats::aggregate(pt_long[c("spend")], by = list(price = pt_long[,"price"]), FUN = mean)
    pt_spend_se <- stats::aggregate(pt_long[c("spend")], by = list(price = pt_long[,"price"]), FUN = se)

    names(pt_spend_mean)[names(pt_spend_mean)=="spend"] <- "mean"
    names(pt_spend_se)[names(pt_spend_se)=="spend"] <- "se"

    pt_spend <- merge(pt_spend_mean, pt_spend_se, by = "price")
    pt_spend$price <- as.numeric(pt_spend$price)
    pt_spend$measure <- "Expenditure"

    pt_summ  <- rbind(pt_consume, pt_spend)

    pt_anno <- data.frame(
      label = c(paste0("Max Consumption: ", round(max(pt_summ$mean[pt_summ$measure=="Consumption"]), digits = 1)),
                paste0("Max Expenditure: ", round(max(pt_summ$mean[pt_summ$measure=="Expenditure"]), digits = 1))),
      measure = c("Mean \u00B1 Std Error of Consumption","Mean \u00B1 Std Error of Expenditure"),
      price = c(min(pt_summ$price[pt_summ$measure=="Consumption" & pt_summ$mean==max(pt_summ$mean[pt_summ$measure=="Consumption"])]),
                max(pt_summ$price[pt_summ$measure=="Expenditure" & pt_summ$mean==max(pt_summ$mean[pt_summ$measure=="Expenditure"])])),
      mean = c(max(pt_summ$mean[pt_summ$measure=="Consumption" & pt_summ$price==min(pt_summ$price)]),
               max(pt_summ$mean[pt_summ$measure=="Expenditure"])))

    pt_summ$measure <- paste0("Mean \u00B1 Std Error of ",pt_summ$measure)

    pt_summ$price[pt_summ$price==0] <- 10^ceiling(log10(zero_conv))/10
    pt_anno$price[pt_anno$price==0] <- 10^ceiling(log10(zero_conv))/10

    pt_plot <- ggplot2::ggplot(pt_summ, ggplot2::aes(x = price, y = mean, group = measure)) +
      ggplot2::geom_line(linewidth = 1.5, lineend = "round", alpha = 0.7) +
      ggplot2::geom_pointrange(ggplot2::aes(ymin = mean - se, ymax = mean + se), colour = "#000000", linewidth = 1.25, size = 1.5, fatten = 3, show.legend = F) +
      ggplot2::geom_segment(pt_anno[(pt_anno$measure=="Mean \u00B1 Std Error of Consumption"),],
                            mapping = ggplot2::aes(x = price, xend = price, y = -Inf, yend = mean), colour = "#999999", linewidth = 0.5, linetype = "dashed") +
      ggplot2::geom_segment(pt_anno[(pt_anno$measure=="Mean \u00B1 Std Error of Consumption"),],
                            mapping = ggplot2::aes(x = 10^ceiling(log10(zero_conv))/10, xend = price, y = mean, yend = mean), colour = "#999999", linewidth = 0.5, linetype = "dashed") +
      ggplot2::geom_segment(pt_anno[(pt_anno$measure=="Mean \u00B1 Std Error of Expenditure"),],
                            mapping = ggplot2::aes(x = price, xend = price, y = -Inf, yend = mean), colour = "#999999", linewidth = 0.5, linetype = "dashed") +
      ggplot2::geom_segment(pt_anno[(pt_anno$measure=="Mean \u00B1 Std Error of Expenditure"),],
                            mapping = ggplot2::aes(x = 10^ceiling(log10(zero_conv))/10, xend = price, y = mean, yend = mean), colour = "#999999", linewidth = 0.5, linetype = "dashed") +
      ggplot2::theme_classic() + ggplot2::xlab("\n Price (Log)") +
      ggplot2::scale_x_log10(breaks = c(10^ceiling(log10(zero_conv))/10,log_labels), labels = c(10^ceiling(log10(zero_conv))/10,log_labels)) +
      ggplot2::facet_wrap(~measure, scales = "free_y", ncol = 1) +
      ggplot2::theme(strip.text = ggplot2::element_text(size = 25,face = "bold", hjust = 0.5),
                     strip.background = ggplot2::element_blank(),
                     plot.title = ggplot2::element_blank(),
                     axis.title.x = ggplot2::element_text(size = 22, face = "bold"),
                     axis.title.y = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_text(size = 19, angle = 30, vjust = 0.5),
                     axis.text.y = ggplot2::element_text(size = 19),
                     axis.line = ggplot2::element_line(linewidth = 1.5),
                     axis.ticks.length = ggplot2::unit(2.5,"mm"),
                     axis.ticks = ggplot2::element_line(linewidth = 1)) +
      ggplot2::geom_point(pt_anno, mapping = ggplot2::aes(x = price, y = mean),
                          size = 3.5, show.legend = F, colour = "#999999") + ggplot2::coord_cartesian(clip = "off")

    for(out in pt_anno$label){
      message(rlang::format_error_bullets(c(i = c(out))))
    }

    ##### --- GROUP SUMMARY STATISTICS

  } else if(type=="group"){

    pt_consume_mean <- stats::aggregate(pt_long[c("consume")], by = list(price = pt_long[,"price"], group = pt_long[,"group",]), FUN = mean)
    pt_consume_se <- stats::aggregate(pt_long[c("consume")], by = list(price = pt_long[,"price"], group = pt_long[,"group"]), FUN = se)

    names(pt_consume_mean)[names(pt_consume_mean)=="consume"] <- "mean"
    names(pt_consume_se)[names(pt_consume_se)=="consume"] <- "se"

    pt_consume <- merge(pt_consume_mean, pt_consume_se , by = c("price","group"))
    pt_consume$price <- as.numeric(pt_consume$price)
    pt_consume$measure <- "Mean \u00B1 Std Error of Consumption"

    ### EXPENDITURE calculation:
    pt_long$spend <- as.numeric(pt_long$price)*pt_long$consume

    pt_spend_mean <- stats::aggregate(pt_long[c("spend")], by = list(price = pt_long[,"price"], group = pt_long[,"group"]), FUN = mean)
    pt_spend_se <- stats::aggregate(pt_long[c("spend")], by = list(price = pt_long[,"price"], group = pt_long[,"group"]), FUN = se)

    names(pt_spend_mean)[names(pt_spend_mean)=="spend"] <- "mean"
    names(pt_spend_se)[names(pt_spend_se)=="spend"] <- "se"

    pt_spend <- merge(pt_spend_mean, pt_spend_se, by = c("price","group"))
    pt_spend$price <- as.numeric(pt_spend$price)
    pt_spend$measure <- "Mean \u00B1 Std Error of Expenditure"

    pt_summ  <- rbind(pt_consume, pt_spend)

    pt_mean_g <- stats::aggregate(pt_summ$mean, by = list(group = pt_summ[,"group"], measure = pt_summ[,"measure"]), FUN = max)
    colnames(pt_mean_g) <- c("group","measure","mean")
    pt_mean_g$label <- paste0(pt_mean_g$group,": ",round(pt_mean_g$mean, digits = 1))

    pt_cons <- data.frame(measure = c("Mean \u00B1 Std Error of Consumption"), group = c("group"),
                          label = paste(pt_mean_g$label[pt_mean_g$measure=="Mean \u00B1 Std Error of Consumption"],collapse = "     "))

    pt_cons$label <- paste0("Max Consumption: ",pt_cons$label)

    pt_exp <- data.frame(measure = c("Mean \u00B1 Std Error of Expenditure"), group = c("group"),
                         label = paste(pt_mean_g$label[pt_mean_g$measure=="Mean \u00B1 Std Error of Expenditure"],collapse = "     "))

    pt_exp$label <- paste0("Max Expenditure: ",pt_exp$label)

    pt_anno <- data.frame("label" = c(pt_cons$label,pt_exp$label))

    ### For position of dashed segmented lines
    cons_exp_price <- function(data){
      data_fin <- {}
      for(g in unique(data$group)){
        data_g <- data[(data$group==g),]
        cons_price <- min(data_g$price[data_g$measure=="Mean \u00B1 Std Error of Consumption" & data_g$mean==max(data_g$mean[data_g$measure=="Mean \u00B1 Std Error of Consumption"])])
        exp_price <- max(data_g$price[data_g$measure=="Mean \u00B1 Std Error of Expenditure" & data_g$mean==max(data_g$mean[data_g$measure=="Mean \u00B1 Std Error of Expenditure"])])
        data_out <- data.frame("group" = c(g,g),
                               "measure" = c("Mean \u00B1 Std Error of Consumption","Mean \u00B1 Std Error of Expenditure"),
                               "price" = c(cons_price,exp_price))
        data_fin <- rbind(data_fin,data_out)
      }
      return(data_fin)
    }

    pt_anno_g <- cons_exp_price(pt_summ)
    pt_anno_g <- merge(pt_anno_g,pt_mean_g, by = c("group","measure"))

    pt_summ$price[pt_summ$price==0] <- 10^ceiling(log10(zero_conv))/10
    pt_anno_g$price[pt_anno_g$price==0] <- 10^ceiling(log10(zero_conv))/10

    pt_plot <- ggplot2::ggplot(pt_summ, ggplot2::aes(x = price, y = mean, group = group, colour = group)) +
      ggplot2::geom_line(linewidth = 1.5, alpha = 0.7, lineend = "round", show.legend = TRUE) +
      ggplot2::geom_pointrange(ggplot2::aes(ymin = mean - se, ymax = mean + se), linewidth = 1.25, size = 1.5, fatten = 3, show.legend = FALSE) +
      ggplot2::geom_segment(pt_anno_g[(pt_anno_g$measure=="Mean \u00B1 Std Error of Consumption"),],
                            mapping = ggplot2::aes(xend = price, y = -Inf, yend = mean), colour = "#999999", linewidth = 0.5, linetype = "dashed") +
      ggplot2::geom_segment(pt_anno_g[(pt_anno_g$measure=="Mean \u00B1 Std Error of Consumption"),],
                            mapping = ggplot2::aes(x = 10^ceiling(log10(zero_conv))/10, xend = price, yend = mean), colour = "#999999", linewidth = 0.5, linetype = "dashed") +
      ggplot2::geom_segment(pt_anno_g[(pt_anno_g$measure=="Mean \u00B1 Std Error of Expenditure"),],
                            mapping = ggplot2::aes(xend = price, y = -Inf, yend = mean), colour = "#999999", linewidth = 0.5, linetype = "dashed") +
      ggplot2::geom_segment(pt_anno_g[(pt_anno_g$measure=="Mean \u00B1 Std Error of Expenditure"),],
                            mapping = ggplot2::aes(x = 10^ceiling(log10(zero_conv))/10, xend = price, yend = mean), colour = "#999999", linewidth = 0.5, linetype = "dashed") +
      ggplot2::theme_classic() + ggplot2::xlab("\n Price (Log)") +
      ggplot2::scale_x_log10(breaks = c(10^ceiling(log10(zero_conv))/10,log_labels), labels = c(10^ceiling(log10(zero_conv))/10,log_labels)) +
      ggplot2::facet_wrap(~measure, scales = "free_y", ncol = 1) +
      ggplot2::theme(strip.text = ggplot2::element_text(size = 25,face = "bold", hjust = 0.5),
                     strip.background = ggplot2::element_blank(),
                     plot.title = ggplot2::element_blank(),
                     axis.title.x = ggplot2::element_text(size = 22, face = "bold"),
                     axis.title.y = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_text(size = 19, angle = 30, vjust = 0.5),
                     axis.text.y = ggplot2::element_text(size = 19),
                     axis.line = ggplot2::element_line(linewidth = 1.5),
                     axis.ticks.length = ggplot2::unit(2.5,"mm"),
                     axis.ticks = ggplot2::element_line(linewidth = 1),
                     legend.position = "bottom",
                     legend.title = ggplot2::element_text(size = 17, face = "bold", vjust = 0.5),
                     legend.text = ggplot2::element_text(size = 15, vjust = 0.5)) +
      ggplot2::guides(colour = ggplot2::guide_legend(title=group_var, override.aes = list(alpha = 1, linewidth = 5))) +
      ggplot2::geom_point(pt_anno_g, mapping = ggplot2::aes(x = price, y = mean),
                          size = 3.5, show.legend = F, colour = "#999999") + ggplot2::coord_cartesian(clip = "off")


    if(length(unique(pt$group))<=5){
      pt_plot <- pt_plot + ggplot2::scale_colour_manual(values = c("#3E668E","#8E3E3E","#66526E"))
    }

    for(out in pt_anno$label){
      message(rlang::format_error_bullets(c(i = c(out))))
    }

  }

  print(pt_plot)

}




#
#
# plot_summary <- function(pt, id_var, type = NULL, group_var = NULL) {
#
#   if(is.null(type)) stop(rlang::format_error_bullets(c( "!" = c("Type required. Please select one either 'overall' or 'group' using the 'type' argument."))), call. = FALSE)
#   if(!is.data.frame(pt)) stop(rlang::format_error_bullets(c( x = c("'pt' must be a data frame."))), call. = FALSE)
#
#   pt_names <- names(pt)
#
#   if(type=="overall"){
#
#     prices <- pt_names[pt_names!=id_var]
#
#     suppressWarnings({
#       if(length(prices[is.na(as.numeric(prices))])==length(prices)) stop(rlang::format_error_bullets(c( x = c("Names of purchase task variables must be numeric. Use `price_prep()` to rename variables."))), call. = FALSE)
#       if(length(prices[is.na(as.numeric(prices))])>0) stop(rlang::format_error_bullets(c( x = c("Variables other than 'id_var' and the purchase task items are detected. Please include only the variables required."))), call. = FALSE)
#     })
#
#
#   } else if(type=="group"){
#
#     if(is.null(group_var)) stop(rlang::format_error_bullets(c( "!" = c("The grouping variable ('group_var') is missing."))), call. = FALSE)
#
#     prices <- pt_names[pt_names!=id_var & pt_names!=group_var]
#     names(pt)[names(pt) == group_var] <- "group"
#     pt$group <- factor(pt$group)
#
#     suppressWarnings({
#       if(length(prices[is.na(as.numeric(prices))])==length(prices)) stop(rlang::format_error_bullets(c( x = c("Names of purchase task variables must be numeric. Use `price_prep()` to rename variables."))), call. = FALSE)
#       if(length(prices[is.na(as.numeric(prices))])>0) stop(rlang::format_error_bullets(c( x = c("Variables other than 'id_var', 'group_var', and the purchase task items are detected. Please include only the variables required."))), call. = FALSE)
#     })
#
#   }
#
#   names(pt)[names(pt) == id_var] <- "id"
#
#   pt_long <- stats::reshape(as.data.frame(pt), idvar = "id", varying = prices,
#                             v.names = "consume", timevar = "price", sep = "", direction = "long")
#
#   pt_long <- pt_long[order(pt_long$id),]
#   pt_long$price <- prices
#
#   ### Calculate acceptable non-zero value to replace 0 price points for plotting
#   zero_conv <- (as.numeric(prices[2])-as.numeric(prices[1]))/10
#
#   ### Calculate log10 label breaks
#   log_labels <- 10^(log10(10^ceiling(log10(zero_conv))):100 * 1)
#
#   ##### ----- OVERALL SUMMARY STATISTICS
#
#   if(type=="overall"){
#
#     pt_consume_mean <- stats::aggregate(pt_long[c("consume")], by = list(price = pt_long[,"price"]), FUN = mean)
#     pt_consume_se <- stats::aggregate(pt_long[c("consume")], by = list(price = pt_long[,"price"]), FUN = se)
#
#     names(pt_consume_mean)[names(pt_consume_mean)=="consume"] <- "mean"
#     names(pt_consume_se)[names(pt_consume_se)=="consume"] <- "se"
#
#     pt_consume <- merge(pt_consume_mean, pt_consume_se , by = "price")
#     pt_consume$price <- as.numeric(pt_consume$price)
#     pt_consume$measure <- "Consumption"
#
#     ### EXPENDITURE calculation:
#     pt_long$spend <- as.numeric(pt_long$price)*as.numeric(pt_long$consume)
#
#     pt_spend_mean <- stats::aggregate(pt_long[c("spend")], by = list(price = pt_long[,"price"]), FUN = mean)
#     pt_spend_se <- stats::aggregate(pt_long[c("spend")], by = list(price = pt_long[,"price"]), FUN = se)
#
#     names(pt_spend_mean)[names(pt_spend_mean)=="spend"] <- "mean"
#     names(pt_spend_se)[names(pt_spend_se)=="spend"] <- "se"
#
#     pt_spend <- merge(pt_spend_mean, pt_spend_se, by = "price")
#     pt_spend$price <- as.numeric(pt_spend$price)
#     pt_spend$measure <- "Expenditure"
#
#     pt_summ  <- rbind(pt_consume, pt_spend)
#
#     pt_anno <- data.frame(
#       label = c(paste0("Max Consumption: ", round(max(pt_summ$mean[pt_summ$measure=="Consumption"]), digits = 1)),
#                 paste0("Max Expenditure: ", round(max(pt_summ$mean[pt_summ$measure=="Expenditure"]), digits = 1))),
#       measure = c("Mean \u00B1 Std Error of Consumption","Mean \u00B1 Std Error of Expenditure"),
#       price = c(pt_summ$price[pt_summ$measure=="Consumption" & pt_summ$mean==max(pt_summ$mean[pt_summ$measure=="Consumption"])],
#                 pt_summ$price[pt_summ$measure=="Expenditure" & pt_summ$mean==max(pt_summ$mean[pt_summ$measure=="Expenditure"])]),
#       mean = c(max(pt_summ$mean[pt_summ$measure=="Consumption" & pt_summ$price==min(pt_summ$price)]),
#                max(pt_summ$mean[pt_summ$measure=="Expenditure"])))
#
#     pt_summ$measure <- paste0("Mean \u00B1 Std Error of ",pt_summ$measure)
#
#     pt_summ$price[pt_summ$price==0] <- 10^ceiling(log10(zero_conv))/10
#     pt_anno$price[pt_anno$price==0] <- 10^ceiling(log10(zero_conv))/10
#
#     pt_plot <- ggplot2::ggplot(pt_summ, ggplot2::aes(x = price, y = mean, group = measure)) +
#       ggplot2::geom_line(linewidth = 1.5, lineend = "round", alpha = 0.7) +
#       ggplot2::geom_pointrange(ggplot2::aes(ymin = mean - se, ymax = mean + se), colour = "#000000", linewidth = 1.25, size = 1.5, fatten = 3, show.legend = F) +
#       ggplot2::geom_segment(pt_anno[(pt_anno$measure=="Mean \u00B1 Std Error of Consumption"),],
#                             mapping = ggplot2::aes(x = price, xend = price, y = -Inf, yend = mean), colour = "#999999", linewidth = 0.5, linetype = "dashed") +
#       ggplot2::geom_segment(pt_anno[(pt_anno$measure=="Mean \u00B1 Std Error of Consumption"),],
#                             mapping = ggplot2::aes(x = 10^ceiling(log10(zero_conv))/10, xend = price, y = mean, yend = mean), colour = "#999999", linewidth = 0.5, linetype = "dashed") +
#       ggplot2::geom_segment(pt_anno[(pt_anno$measure=="Mean \u00B1 Std Error of Expenditure"),],
#                             mapping = ggplot2::aes(x = price, xend = price, y = -Inf, yend = mean), colour = "#999999", linewidth = 0.5, linetype = "dashed") +
#       ggplot2::geom_segment(pt_anno[(pt_anno$measure=="Mean \u00B1 Std Error of Expenditure"),],
#                             mapping = ggplot2::aes(x = 10^ceiling(log10(zero_conv))/10, xend = price, y = mean, yend = mean), colour = "#999999", linewidth = 0.5, linetype = "dashed") +
#       ggplot2::theme_classic() + ggplot2::xlab("\n Price (Log)") +
#       ggplot2::scale_x_log10(breaks = c(10^ceiling(log10(zero_conv))/10,log_labels), labels = c(10^ceiling(log10(zero_conv))/10,log_labels)) +
#       ggplot2::facet_wrap(~measure, scales = "free_y", ncol = 1) +
#       ggplot2::theme(strip.text = ggplot2::element_text(size = 25,face = "bold", hjust = 0.5),
#                      strip.background = ggplot2::element_blank(),
#                      plot.title = ggplot2::element_blank(),
#                      axis.title.x = ggplot2::element_text(size = 22, face = "bold"),
#                      axis.title.y = ggplot2::element_blank(),
#                      axis.text.x = ggplot2::element_text(size = 19, angle = 30, vjust = 0.5),
#                      axis.text.y = ggplot2::element_text(size = 19),
#                      axis.line = ggplot2::element_line(linewidth = 1.5),
#                      axis.ticks.length = ggplot2::unit(2.5,"mm"),
#                      axis.ticks = ggplot2::element_line(linewidth = 1)) +
#       ggplot2::geom_point(pt_anno, mapping = ggplot2::aes(x = price, y = mean),
#                           size = 3.5, show.legend = F, colour = "#999999") + ggplot2::coord_cartesian(clip = "off")
#
#     for(out in pt_anno$label){
#       message(rlang::format_error_bullets(c(i = c(out))))
#     }
#
#     ##### --- GROUP SUMMARY STATISTICS
#
#   } else if(type=="group"){
#
#     pt_consume_mean <- stats::aggregate(pt_long[c("consume")], by = list(price = pt_long[,"price"], group = pt_long[,"group",]), FUN = mean)
#     pt_consume_se <- stats::aggregate(pt_long[c("consume")], by = list(price = pt_long[,"price"], group = pt_long[,"group"]), FUN = se)
#
#     names(pt_consume_mean)[names(pt_consume_mean)=="consume"] <- "mean"
#     names(pt_consume_se)[names(pt_consume_se)=="consume"] <- "se"
#
#     pt_consume <- merge(pt_consume_mean, pt_consume_se , by = c("price","group"))
#     pt_consume$price <- as.numeric(pt_consume$price)
#     pt_consume$measure <- "Mean \u00B1 Std Error of Consumption"
#
#     ### EXPENDITURE calculation:
#     pt_long$spend <- as.numeric(pt_long$price)*pt_long$consume
#
#     pt_spend_mean <- stats::aggregate(pt_long[c("spend")], by = list(price = pt_long[,"price"], group = pt_long[,"group"]), FUN = mean)
#     pt_spend_se <- stats::aggregate(pt_long[c("spend")], by = list(price = pt_long[,"price"], group = pt_long[,"group"]), FUN = se)
#
#     names(pt_spend_mean)[names(pt_spend_mean)=="spend"] <- "mean"
#     names(pt_spend_se)[names(pt_spend_se)=="spend"] <- "se"
#
#     pt_spend <- merge(pt_spend_mean, pt_spend_se, by = c("price","group"))
#     pt_spend$price <- as.numeric(pt_spend$price)
#     pt_spend$measure <- "Mean \u00B1 Std Error of Expenditure"
#
#     pt_summ  <- rbind(pt_consume, pt_spend)
#
#     pt_mean_g <- stats::aggregate(pt_summ$mean, by = list(group = pt_summ[,"group"], measure = pt_summ[,"measure"]), FUN = max)
#     pt_mean_g$label <- paste0(pt_mean_g$group,": ",round(pt_mean_g$x, digits = 1))
#
#     pt_cons <- data.frame(measure = c("Mean \u00B1 Std Error of Consumption"), group = c("group"),
#                           label = paste(pt_mean_g$label[pt_mean_g$measure=="Mean \u00B1 Std Error of Consumption"],collapse = "     "))
#
#     pt_cons$label <- paste0("Max Consumption: ",pt_cons$label)
#
#     pt_exp <- data.frame(measure = c("Mean \u00B1 Std Error of Expenditure"), group = c("group"),
#                          label = paste(pt_mean_g$label[pt_mean_g$measure=="Mean \u00B1 Std Error of Expenditure"],collapse = "     "))
#
#     pt_exp$label <- paste0("Max Expenditure: ",pt_exp$label)
#
#     pt_anno <- data.frame("label" = c(pt_cons$label,pt_exp$label))
#
#     ### For position of dashed segmented lines
#     pt_max_summ <- pt_summ[(pt_summ$mean %in% pt_mean_g$x),]
#
#     pt_summ$price[pt_summ$price==0] <- 10^ceiling(log10(zero_conv))/10
#     pt_max_summ$price[pt_max_summ$price==0] <- 10^ceiling(log10(zero_conv))/10
#
#     pt_plot <- ggplot2::ggplot(pt_summ, ggplot2::aes(x = price, y = mean, group = group, colour = group)) +
#       ggplot2::geom_line(linewidth = 1.5, alpha = 0.7, lineend = "round", show.legend = TRUE) +
#       ggplot2::geom_pointrange(ggplot2::aes(ymin = mean - se, ymax = mean + se), linewidth = 1.25, size = 1.5, fatten = 3, show.legend = FALSE) +
#       ggplot2::geom_segment(pt_max_summ[(pt_max_summ$measure=="Mean \u00B1 Std Error of Consumption"),],
#                             mapping = ggplot2::aes(xend = price, y = -Inf, yend = mean), colour = "#999999", linewidth = 0.5, linetype = "dashed") +
#       ggplot2::geom_segment(pt_max_summ[(pt_max_summ$measure=="Mean \u00B1 Std Error of Consumption"),],
#                             mapping = ggplot2::aes(x = 10^ceiling(log10(zero_conv))/10, xend = price, yend = mean), colour = "#999999", linewidth = 0.5, linetype = "dashed") +
#       ggplot2::geom_segment(pt_max_summ[(pt_max_summ$measure=="Mean \u00B1 Std Error of Expenditure"),],
#                             mapping = ggplot2::aes(xend = price, y = -Inf, yend = mean), colour = "#999999", linewidth = 0.5, linetype = "dashed") +
#       ggplot2::geom_segment(pt_max_summ[(pt_max_summ$measure=="Mean \u00B1 Std Error of Expenditure"),],
#                             mapping = ggplot2::aes(x = 10^ceiling(log10(zero_conv))/10, xend = price, yend = mean), colour = "#999999", linewidth = 0.5, linetype = "dashed") +
#       ggplot2::theme_classic() + ggplot2::xlab("\n Price (Log)") +
#       ggplot2::scale_x_log10(breaks = c(10^ceiling(log10(zero_conv))/10,log_labels), labels = c(10^ceiling(log10(zero_conv))/10,log_labels)) +
#       ggplot2::facet_wrap(~measure, scales = "free_y", ncol = 1) +
#       ggplot2::theme(strip.text = ggplot2::element_text(size = 25,face = "bold", hjust = 0.5),
#                      strip.background = ggplot2::element_blank(),
#                      plot.title = ggplot2::element_blank(),
#                      axis.title.x = ggplot2::element_text(size = 22, face = "bold"),
#                      axis.title.y = ggplot2::element_blank(),
#                      axis.text.x = ggplot2::element_text(size = 19, angle = 30, vjust = 0.5),
#                      axis.text.y = ggplot2::element_text(size = 19),
#                      axis.line = ggplot2::element_line(linewidth = 1.5),
#                      axis.ticks.length = ggplot2::unit(2.5,"mm"),
#                      axis.ticks = ggplot2::element_line(linewidth = 1),
#                      legend.position = "bottom",
#                      legend.title = ggplot2::element_text(size = 17, face = "bold", vjust = 0.5),
#                      legend.text = ggplot2::element_text(size = 15, vjust = 0.5)) +
#       ggplot2::guides(colour = ggplot2::guide_legend(title=group_var, override.aes = list(alpha = 1, linewidth = 5))) +
#       ggplot2::geom_point(pt_max_summ, mapping = ggplot2::aes(x = price, y = mean),
#                           size = 3.5, show.legend = F, colour = "#999999") + ggplot2::coord_cartesian(clip = "off")
#
#
#     if(length(unique(pt$group))<=5){
#       pt_plot <- pt_plot + ggplot2::scale_colour_manual(values = c("#3E668E","#8E3E3E","#66526E"))
#     }
#
#     for(out in pt_anno$label){
#       message(rlang::format_error_bullets(c(i = c(out))))
#     }
#
#   }
#
#   print(pt_plot)
#
# }
