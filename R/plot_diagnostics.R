#' Individual demand diagnostics
#'
#' This function diagnoses individual demand, by visualizing empirical consumption or fitted demand curves (linear or exponentiated). This function can be used to assess
#' fitted demand curves of those with poor R^2 values (default is < 0.5) or those that are greater than a set number of standard deviations away from the mean (default is |3|).
#' Alternatively, this function can be used to visualize all empirical demand or individual demand curves. See details for further information.
#'
#' To diagnose empirical consumption before or after quality control has been implemented, this function provides users the ability to plot individual responses on the purchase task,
#' similar to the consumption visualization produced using the `plot_summary()` function. To diagnose fitted linear or exponentiated demand curves, this function will produce
#' individual demand curves similar to those produced for overall demand in the `pt_linear` or `pt_curve()` function, respectively.
#'
#' @param pt A data frame consisting of the `id_var`, purchase task variables, alpha or eta (depending on the type of demand curve to diagnose), Q0 and k-value if `type` is "curve",
#' as well as and R^2 values is `diagnostics` argument is "r2".
#' @param id_var The name of the unique identifier as identified in the data frame.
#' @param type The type of demand to be visualized. One of c("empirical","linear","curve").
#' @param diagnostics The diagnostic used to identify potentially poor-quality demand curves when `type` is "linear" or "curve". One of c("r2","zscore","all"). The default is "all".
#' @param threshold The threshold of R^2 or z-score, depending on the `diagnostics` selected. For `diagnostics` "r2", the default is 0.5, thus plotting those with R^2 scores lower than 0.5.
#' For `diagnostics` "zscore", the default is 3, plotting those with a demand value (alpha or eta) value that is greater than 3 standard deviations away from the mean (z-score > |3|).
#' @param zero_val For `type` "linear", the value to substitute zero values in the price and consumption data (default is 0.001).
#' @param n_rc The number of rows and columns used to display individual curves. The default is c(4,5).
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
#' pt4 <- pt_curve(pt3$data, id_var = "ID", type = "individual")
#'
#' ### --- Function Example
#' plot_diagnostics(pt4, id_var = "ID", type = "curve", diagnostics = "r2", threshold = 0.7)
#'
#' @return A ggplot2 graphical object
#' @export


plot_diagnostics <- function(pt, id_var, type = NULL, diagnostics = "all", threshold = NULL, zero_val = 0.001, n_rc = c(4,5)){

  if(!is.data.frame(pt)) stop(rlang::format_error_bullets(c( x = c("'pt' must be a data frame."))), call. = FALSE)
  if(is.null(type)) stop(rlang::format_error_bullets(c( "!" = c("Type required. Please select one of c('empirical','linear','curve') using the 'type' argument."))), call. = FALSE)
  if(type!="empirical" & is.null(diagnostics)) stop(rlang::format_error_bullets(c( "!" = c("Diagnostic type required. Please select one of c('r2','zscore','all') using the 'diagnostics' argument."))), call. = FALSE)
  if(type=="empirical" & (diagnostics=="r2" | diagnostics=="zscore")) stop(rlang::format_error_bullets(c( "!" = c("Diagnostic type not available for type 'empirical'."))), call. = FALSE)
  if(diagnostics=="all" & !is.null(threshold)) stop(rlang::format_error_bullets(c( "!" = c("Threshold not applicable when diagnostic type equals 'all'."))), call. = FALSE)
  if(type == "curve" & !"Alpha" %in% names(pt)) stop(rlang::format_error_bullets(c( x = c("The indicator variable ('Alpha') does not exist within 'pt'."))), call. = FALSE)
  if(type == "curve" & !"Q0" %in% names(pt)) stop(rlang::format_error_bullets(c( x = c("The indicator variable ('Q0') does not exist within 'pt'."))), call. = FALSE)
  if(type == "curve" & !"K" %in% names(pt)) stop(rlang::format_error_bullets(c( x = c("The indicator variable ('K') does not exist within 'pt'."))), call. = FALSE)
  if(type == "linear" & !"Eta" %in% names(pt)) stop(rlang::format_error_bullets(c( x = c("The indicator variable ('Eta') does not exist within 'pt'."))), call. = FALSE)

  if(is.null(threshold)){
    threshold[diagnostics=="r2"] <- 0.5
    threshold[diagnostics=="zscore"] <- 3
    threshold[diagnostics=="all"] <- NA
  }

  pt_names <- names(pt)
  var_exclude <- c("Intensity","Breakpoint","Omax","Pmax","Q0","Alpha","K","Eta","UnitElasticity","AUC","R2","Brute")

  names(pt)[names(pt) == id_var] <- "id"

  prices <- pt_names[pt_names!=id_var & !pt_names %in% var_exclude]

  ### Calculate acceptable non-zero value to replace 0 price points for plotting
  zero_conv <- (as.numeric(prices[2])-as.numeric(prices[1]))/10

  ### Calculate log10 label breaks
  log_labels <- 10^(log10(10^ceiling(log10(zero_conv))):100 * 1)

  if(diagnostics == "all" & type == "linear"){
    pt <- pt[!is.na(pt$Eta),]
  } else if(diagnostics == "all" & type == "curve"){
    pt <- pt[!is.na(pt$Alpha),]
  }

  if(diagnostics == "r2"){
    if(!"R2" %in% names(pt)) stop(rlang::format_error_bullets(c( x = c("The diagnostic variable ('R2') does not exist within 'pt'."))), call. = FALSE)
    pt <- pt[(!is.na(pt$R2) & pt$R2<threshold),]
  }

  if(diagnostics == "zscore" & type == "linear"){
    pt <- pt[!is.na(pt$Eta),]
    pt$z_eta <- abs(scale(pt$Eta))
    pt <- pt[(pt$z_eta>threshold),]
  } else if(diagnostics == "zscore" & type == "curve"){
    pt <- pt[!is.na(pt$Alpha),]
    pt$z_alpha <- abs(scale(pt$Alpha))
    pt <- pt[(pt$z_alpha>threshold),]
  }

  length_pt <- length(pt$id)

  if(length_pt==0) stop(rlang::format_error_bullets(c( "!" = c("No individual demand to visualize using the current threshold."))), call. = FALSE)

  unique_id <- pt$id

  pt_long <- stats::reshape(as.data.frame(pt[c("id",prices)]), idvar = "id",
                            varying = prices,
                            v.names = c("q"), timevar = c("c"), sep = "", direction = "long")

  pt_long <- pt_long[order(pt_long$id),]
  pt_long$c <- as.numeric(prices)

  pt_pred <- data.frame(c = seq(zero_conv,as.numeric(prices[length(prices)]), by = (length(prices)/length(prices)^3)))

  pt_fit <- data.frame(id = NULL, c = NULL, pred = NULL)

  if(type == "empirical"){

    pt_long$c[pt_long$c==0] <- 10^ceiling(log10(zero_conv))/10

  } else if(type == "linear"){

    pt_long$c[pt_long$c==0] <- zero_val
    pt_long$q[pt_long$q==0] <- zero_val

    for(id_num in pt$id){

      pt_i <- pt_long[(pt_long$id == id_num),]

      pt_mod_i <- stats::lm(log(q) ~ log(c), data = pt_i)

      suppressWarnings({
        pred_i <- stats::predict(pt_mod_i, pt_pred)
      })

      pt_fit_i <- data.frame(id = id_num, c = pt_pred$c, pred = pred_i)

      pt_fit <- rbind(pt_fit,pt_fit_i)

    }

    pt_fit$pred[pt_fit$pred<0] <- NA

    ### Transform consumption and if mean data point is < the min predicted values; don't show
    pt_long$q <- log(pt_long$q)
    pt_long$q[pt_long$q<min(pt_fit$pred, na.rm = TRUE)] <- NA

    pt_fit <- pt_fit[!is.na(pt_fit$pred),]
    pt_long <- pt_long[!is.na(pt_long$q),]

  } else if(type == "curve"){

    kval <- unique(pt$K[!is.na(pt$K)])
    pt_long$k <- kval
    equation <- "q ~ q0 * 10^(k * (exp(-alpha * q0 * c)-1))"

    for(id_num in pt$id){

      pt_i <- pt_long[(pt_long$id == id_num),]

      q0_i <- pt$Q0[pt$id==id_num]
      alpha_i <- pt$Alpha[pt$id==id_num]

      ### Use nls2 brute-force since alpha and q0 were already found for these participants
      pt_mod_i <- nls2::nls2(equation, data = pt_i, start = list(q0 = q0_i, alpha = alpha_i), control = stats::nls.control(maxiter = 500), algorithm = "brute-force")

      suppressWarnings({
        pred_i <- stats::predict(pt_mod_i, pt_pred)
      })

      pt_fit_i <- data.frame(id = id_num, c = pt_pred$c, pred = pred_i)

      pt_fit <- rbind(pt_fit,pt_fit_i)

    }

    pt_long$c[pt_long$c==0] <- zero_conv/2
    pt_fit$c[pt_fit$c==0] <- zero_conv/2

  }

  split_id_start <- seq(1, length_pt, n_rc[1]*n_rc[2])

  split_id <- lapply(split_id_start,function(x){x:(x+(n_rc[1]*n_rc[2])-1)})


  for(i in 1:length(split_id)){

    if(type == "empirical"){

      pt_long_fin <- pt_long[(pt_long$id %in% unique_id[split_id[[i]]]),]

      pt_plot <- ggplot2::ggplot(pt_long_fin, ggplot2::aes(x = c, y = q)) +
        ggplot2::geom_line(linewidth = 1.5, lineend = "round", alpha = 0.7) +
        ggplot2::geom_point(colour = "#000000", size = 1.5, show.legend = FALSE) +
        ggplot2::theme_classic() + ggplot2::xlab("\n Price (Log)") +
        ggplot2::scale_x_log10(breaks = c(10^ceiling(log10(zero_conv))/10,log_labels), labels = c(10^ceiling(log10(zero_conv))/10,log_labels)) +
        ggplot2::facet_wrap(~id, scales = "free", nrow = n_rc[1], ncol = n_rc[2]) +
        ggplot2::ggtitle("Individual Empirical Consumption") +
        ggplot2::theme(strip.text = ggplot2::element_text(size = 25,face = "bold", hjust = 0.5),
                       strip.background = ggplot2::element_blank(),
                       plot.title = ggplot2::element_blank(),
                       axis.title.x = ggplot2::element_text(size = 22, face = "bold"),
                       axis.title.y = ggplot2::element_blank(),
                       axis.text.x = ggplot2::element_text(size = 19, angle = 30, vjust = 0.5),
                       axis.text.y = ggplot2::element_text(size = 19),
                       axis.line = ggplot2::element_line(linewidth = 1.5),
                       axis.ticks.length = ggplot2::unit(2.5,"mm"),
                       axis.ticks = ggplot2::element_line(linewidth = 1))

    } else if(type == "linear" | type == "curve"){

      pt_fit_fin <- pt_fit[(pt_fit$id %in% unique_id[split_id[[i]]]),]
      pt_long_fin <- pt_long[(pt_long$id %in% unique_id[split_id[[i]]]),]

      ### Will not visualize split in x-axis as facet_wrap is utilized for the individual plots
      pt_plot <- ggplot2::ggplot(pt_fit_fin, ggplot2::aes(x = c, y = pred)) +
        ggplot2::geom_line(lineend = "round", linewidth = 2, colour = "#999999", show.legend = FALSE) +
        ggplot2::geom_point(pt_long_fin, mapping = ggplot2::aes(x = c, y = q), size = 2.5, show.legend = FALSE) +
        ggplot2::facet_wrap(~id, scales = "free", nrow = n_rc[1], ncol = n_rc[2]) +
        ggplot2::scale_x_log10(breaks = c(log_labels), labels = c(log_labels)) +
        ggplot2::theme_classic() + ggplot2::xlab("\n Price (Log)") + ggplot2::ylab("Consumption") +
        ggplot2::ggtitle("Individual Demand Curves") +
        ggplot2::theme(plot.title = ggplot2::element_text(size = 25, face = "bold", hjust = 0.5),
                       axis.title = ggplot2::element_text(size = 22, face = "bold"),
                       axis.text = ggplot2::element_text(size = 19),
                       axis.ticks.length = ggplot2::unit(2,"mm"),
                       strip.background = ggplot2::element_blank(),
                       strip.text = ggplot2::element_text(size = 20),
                       axis.line = ggplot2::element_line(linewidth = 1.5))

      if(type == "linear"){
        pt_plot <- pt_plot + ggplot2::scale_y_log10(breaks = c(log_labels), labels = c(log_labels)) + ggplot2::ylab("Consumption (Log)")
      }
    }


    graphics::par(ask=TRUE)
    suppressWarnings({
      print(pt_plot)
    })

  }

  graphics::par(ask=FALSE)

}

