utils::globalVariables(c("id","price","measure","group","label","value"))

#' PLOT SUMMARY
#'
#' This function helps users to inspect the price-level purchase task data by
#' visualizing the mean and standard error of both consumption and expenditure.
#'
#' @param pt A data frame consisting of the purchase variables for each `id_var` (and the grouping variable if `type` equals "group")
#' @param id_var The name of the unique identifier (ID) as found in the data frame.
#' @param type The type of plot to produce. One of c("overall","group","individual"). The default is "overall" and provides a summary
#' of all individuals in the data frame. The "group" type provides a summary of consumption and expenditure by grouping variable. If
#' wanting to produce individual plots by each ID, then the "individual" type of plot should be selected, as this will visualize
#' all participants' consumption and expenditure in a spaghetti plot.
#' @param group_var The grouping variable if using type "group".
#' @return A ggplot2 graphical object
#' @export

plot_summary <- function(pt, id_var, type = "default", group_var = NULL) {

  pt_names <- names(pt)
  prices <- pt_names[pt_names!=id_var]

  if(type=="group"){
  prices <- pt_names[pt_names!=id_var & pt_names!=group_var]
  names(pt)[names(pt) == group_var] <- "group"
  pt$group <- factor(pt$group)
  }

  names(pt)[names(pt) == id_var] <- "id"


  pt_long <- stats::reshape(as.data.frame(pt), idvar = "id", varying = prices,
                     v.names = "consume", timevar = "price", sep = "", direction = "long")

  pt_long <- pt_long[order(pt_long$id),]
  pt_long$price <- prices

  ### --- OVERALL SUMMARY STATISTICS

  if(type=="overall"){

  pt_consume_mean <- stats::aggregate(pt_long$consume, by = list(pt_long$price), FUN = mean)
  pt_consume_se <- stats::aggregate(pt_long$consume, by = list(pt_long$price), FUN = se)

  names(pt_consume_mean)[names(pt_consume_mean)=="x"] <- "mean"
  names(pt_consume_mean)[names(pt_consume_mean)=="Group.1"] <- "price"

  names(pt_consume_se)[names(pt_consume_se)=="x"] <- "se"
  names(pt_consume_se)[names(pt_consume_se)=="Group.1"] <- "price"

  pt_consume <- merge(pt_consume_mean, pt_consume_se , by = "price")
  pt_consume$price <- as.numeric(pt_consume$price)
  pt_consume$measure <- "Consumption"

  ### EXPENDITURE calculation:
  pt_long$spend <- as.numeric(pt_long$price)*as.numeric(pt_long$consume)

  pt_spend_mean <- stats::aggregate(pt_long$spend, by = list(pt_long$price), FUN = mean)
  pt_spend_se <- stats::aggregate(pt_long$spend, by = list(pt_long$price), FUN = se)

  names(pt_spend_mean)[names(pt_spend_mean)=="x"] <- "mean"
  names(pt_spend_mean)[names(pt_spend_mean)=="Group.1"] <- "price"

  names(pt_spend_se)[names(pt_spend_se)=="x"] <- "se"
  names(pt_spend_se)[names(pt_spend_se)=="Group.1"] <- "price"

  pt_spend <- merge(pt_spend_mean, pt_spend_se, by = "price")
  pt_spend$price <- as.numeric(pt_spend$price)
  pt_spend$measure <- "Expenditure"

  pt_summ  <- rbind(pt_consume, pt_spend)

  pt_anno <- data.frame(
    label = c(paste0("Max Consumption: ", round(max(pt_summ$mean[pt_summ$measure=="Consumption"]), digits = 1)),
              paste0("Max Expenditure: ", round(max(pt_summ$mean[pt_summ$measure=="Expenditure"]), digits = 1))),
    measure = c("Mean \u00B1 Std Error of Consumption","Mean \u00B1 Std Error of Expenditure"),
    price = c(pt_summ$price[pt_summ$measure=="Consumption" & pt_summ$mean==max(pt_summ$mean[pt_summ$measure=="Consumption"])],
              pt_summ$price[pt_summ$measure=="Expenditure" & pt_summ$mean==max(pt_summ$mean[pt_summ$measure=="Expenditure"])]),
    mean = c(max(pt_summ$mean[pt_summ$measure=="Consumption" & pt_summ$price==min(pt_summ$price)]),
             max(pt_summ$mean[pt_summ$measure=="Expenditure"])))

  pt_summ$measure <- paste0("Mean \u00B1 Std Error of ",pt_summ$measure)

 pt_plot <- ggplot2::ggplot(pt_summ, ggplot2::aes(x = factor(price), y = mean, group = measure)) +
   ggplot2::geom_line(linewidth = 1.5, alpha = 0.7) +
   ggplot2::geom_pointrange(ggplot2::aes(ymin = mean - se, ymax = mean + se), colour = "#000000", linewidth = 1.25, size = 1.5, fatten = 3, show.legend = F) +
   ggplot2::geom_segment(pt_anno[(pt_anno$measure=="Mean \u00B1 Std Error of Consumption"),],
                         mapping = ggplot2::aes(x = factor(price), xend = factor(price), y = -Inf, yend = mean), colour = "#999999", linewidth = 0.5, linetype = "dashed") +
   ggplot2::geom_segment(pt_anno[(pt_anno$measure=="Mean \u00B1 Std Error of Consumption"),],
                         mapping = ggplot2::aes(x = -Inf, xend = factor(price), y = mean, yend = mean), colour = "#999999", linewidth = 0.5, linetype = "dashed") +
   ggplot2::geom_segment(pt_anno[(pt_anno$measure=="Mean \u00B1 Std Error of Expenditure"),],
                         mapping = ggplot2::aes(x = factor(price), xend = factor(price), y = -Inf, yend = mean), colour = "#999999", linewidth = 0.5, linetype = "dashed") +
   ggplot2::geom_segment(pt_anno[(pt_anno$measure=="Mean \u00B1 Std Error of Expenditure"),],
                         mapping = ggplot2::aes(x = -Inf, xend = factor(price), y = mean, yend = mean), colour = "#999999", linewidth = 0.5, linetype = "dashed") +
   ggplot2::theme_classic() + ggplot2::xlab("\n Price") +
   ggplot2::scale_x_discrete(breaks = factor(pt_spend$price),
                             expand = c(0.025,0.025,0.025,0.025)) +
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
   ggplot2::geom_point(pt_anno, mapping = ggplot2::aes(x = factor(price), y = mean),
                       size = 3.5, show.legend = F, colour = "#3e668e") + ggplot2::coord_cartesian(clip = "off") +
   ggplot2::geom_text(pt_anno, mapping = ggplot2::aes(label = label),
                      colour = "#3e668e", size = 7, x = Inf, y = Inf, hjust = 1, vjust = 1, show.legend = F)

  }

  ### --- GROUP SUMMARY STATISTICS

  if(type=="group"){

    pt_consume_mean <- stats::aggregate(pt_long$consume, by = list(pt_long$price, pt_long$group), FUN = mean)
    pt_consume_se <- stats::aggregate(pt_long$consume, by = list(pt_long$price, pt_long$group), FUN = se)

    names(pt_consume_mean)[names(pt_consume_mean)=="x"] <- "mean"
    names(pt_consume_mean)[names(pt_consume_mean)=="Group.1"] <- "price"
    names(pt_consume_mean)[names(pt_consume_mean)=="Group.2"] <- "group"

    names(pt_consume_se)[names(pt_consume_se)=="x"] <- "se"
    names(pt_consume_se)[names(pt_consume_se)=="Group.1"] <- "price"
    names(pt_consume_se)[names(pt_consume_se)=="Group.2"] <- "group"

    pt_consume <- merge(pt_consume_mean, pt_consume_se , by = c("price","group"))
    pt_consume$price <- as.numeric(pt_consume$price)
    pt_consume$measure <- "Mean \u00B1 Std Error of Consumption"

    ### EXPENDITURE calculation:
    pt_long$spend <- as.numeric(pt_long$price)*pt_long$consume

    pt_spend_mean <- stats::aggregate(pt_long$spend, by = list(pt_long$price, pt_long$group), FUN = mean)
    pt_spend_se <- stats::aggregate(pt_long$spend, by = list(pt_long$price, pt_long$group), FUN = se)

    names(pt_spend_mean)[names(pt_spend_mean)=="x"] <- "mean"
    names(pt_spend_mean)[names(pt_spend_mean)=="Group.1"] <- "price"
    names(pt_spend_mean)[names(pt_spend_mean)=="Group.2"] <- "group"

    names(pt_spend_se)[names(pt_spend_se)=="x"] <- "se"
    names(pt_spend_se)[names(pt_spend_se)=="Group.1"] <- "price"
    names(pt_spend_se)[names(pt_spend_se)=="Group.2"] <- "group"

    pt_spend <- merge(pt_spend_mean, pt_spend_se, by = c("price","group"))
    pt_spend$price <- as.numeric(pt_spend$price)
    pt_spend$measure <- "Mean \u00B1 Std Error of Expenditure"

    pt_summ  <- rbind(pt_consume, pt_spend)

    pt_anno <- stats::aggregate(pt_summ$mean, by = list(group = pt_summ$group, measure = pt_summ$measure), FUN = max)
    pt_anno$label <- paste0(pt_anno$group,": ",round(pt_anno$x, digits = 1))

    pt_cons <- data.frame(measure = c("Mean \u00B1 Std Error of Consumption"), group = c("group"),
                          label = paste(pt_anno$label[pt_anno$measure=="Mean \u00B1 Std Error of Consumption"],collapse = " \n"))

    pt_cons$label <- paste0("Max Consumption \n",pt_cons$label)

    pt_exp <- data.frame(measure = c("Mean \u00B1 Std Error of Expenditure"), group = c("group"),
                         label = paste(pt_anno$label[pt_anno$measure=="Mean \u00B1 Std Error of Expenditure"],collapse = " \n"))

    pt_exp$label <- paste0("Max Expenditure \n",pt_exp$label)

    pt_plot <- ggplot2::ggplot(pt_summ, ggplot2::aes(x = factor(price), y = mean, group = group, colour = group)) +
      ggplot2::geom_line(linewidth = 1.5, alpha = 0.7, show.legend = TRUE) +
      ggplot2::geom_pointrange(ggplot2::aes(ymin = mean - se, ymax = mean + se), linewidth = 1.25, size = 1.5, fatten = 3, show.legend = FALSE) +
      ggplot2::theme_classic() + ggplot2::xlab("\n Price") +
      ggplot2::scale_x_discrete(breaks = factor(pt_spend$price),
                                expand = c(0.025,0.025,0.025,0.025)) +
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
      ggplot2::geom_text(pt_cons, mapping = ggplot2::aes(label = label), colour = "#333333",
                         size = 5, x = Inf, y = Inf, hjust = 1, vjust = 1, show.legend = F) +
      ggplot2::geom_text(pt_exp, mapping = ggplot2::aes(label = label), colour = "#333333",
                         size = 5, x = Inf, y = Inf, hjust = 1, vjust = 1, show.legend = F) +
      ggplot2::guides(colour = ggplot2::guide_legend(title=group_var, override.aes = list(alpha = 1, linewidth = 5)))

  }

  ### --- INDIVIDUAL PLOT
  if(type=="individual"){

    pt_long$price <- as.numeric(pt_long$price)
    ### EXPENDITURE calculation:
    pt_long$spend <- as.numeric(pt_long$price)*as.numeric(pt_long$consume)

    pt_consume <- pt_long[c("id","price","consume")]
    names(pt_consume) <- c("id","price","value")
    pt_consume$measure <- "Consumption"

    pt_spend <- pt_long[c("id","price","spend")]
    names(pt_spend) <- c("id","price","value")
    pt_spend$measure <- "Expenditure"

    pt_summ  <- rbind(pt_consume, pt_spend)

    pt_plot <- ggplot2::ggplot(pt_summ, ggplot2::aes(x = factor(price), y = value, group = id)) +
      ggplot2::geom_line(linewidth = 1, alpha = 0.7,colour = "#999999",show.legend = FALSE) +
      ggplot2::theme_classic() + ggplot2::xlab("\n Price") +
      ggplot2::scale_x_discrete(breaks = factor(pt_spend$price),
                                expand = c(0.025,0.025,0.025,0.025)) +
      ggplot2::facet_wrap(~measure, scales = "free_y", ncol = 1) +
      ggplot2::theme(strip.text = ggplot2::element_text(size = 25,face = "bold", hjust = 0.5),
                     strip.background = ggplot2::element_blank(),
                     plot.title = ggplot2::element_blank(),
                     axis.title.x = ggplot2::element_text(size = 22, face = "bold"),
                     axis.title.y = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_text(size = 19, angle = 30, vjust = 0.5),
                     axis.ticks = ggplot2::element_line(linewidth = 1),
                     axis.text.y = ggplot2::element_text(size = 19),
                     axis.line = ggplot2::element_line(linewidth = 1.5),
                     axis.ticks.length = ggplot2::unit(2.5,"mm"))

  }

  pt_plot

  }
