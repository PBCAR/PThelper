utils::globalVariables(c("id","kval","pred","alpha","q0","pmax","rsquared","pred_dat","unit_elast"))

#' Exponentiated demand curve fitting
#'
#' This function derives demand functions using the Koffarnus et al. (2015) exponentiated equation, which is an exponentiated version of the exponential
#' derivation of demand elasticity (Hursh & Silberberg, 2008). The derived demand indicators from these models can be calculated for the entire sample,
#' for sub-groups (using a grouping variable), or for individuals. Visualizations are produced of the mean aggregate demand curves when the `type` is
#' either "overall" or "group". See details for additional information on the exponentiated demand equation.
#'
#' The exponentiated demand curve function put forth by Koffarnus et al. (2015) is used to derive alpha, which is defined as the rate of change in elasticity (Gilroy et al. 2020).
#' It is also used to derive Q0 (intensity), and unit elasticity (Pmax). This equation accommodates values of zero consumption, meaning that these variables
#' do not need to have a small constant applied to them prior to being fit. Elasticity (epsilon or eta) cannot be derived from this or other non-linear equations directly,
#' as elasticity changes depending on the changes in price (Gilroy et al., 2020).
#'
#' Unit elasticity is the point at which a one log-unit increase in price is associated with a >1 log-unit decrease in consumption. Prior to this point, consumption
#' persists or declines shallowly (<1 log-unit decrease) across increases in price (inelastic demand, sub-proportionate decreases in consumption relative to price), whilst
#' after the price associated with unit elasticity, consumption continues to decrease rapidly alongside increases in price (elastic demand, supra-proportionate
#' decreases in consumption relative to price) (Gilroy et al., 2020).
#'
#' Gilroy, S. P., Kaplan, B. A., & Reed, D. D. (2020). Interpretation (s) of elasticity in operant demand. Journal of the Experimental Analysis of Behavior, 114(1), 106-115.
#'
#' Hursh, S. R., & Silberberg, A. (2008). Economic demand and essential value. Psychological review, 115(1), 186.
#'
#' Koffarnus, M. N., Franck, C. T., Stein, J., & Bickel, W. K. (2015). A modified exponential behavioral economic demand model to better describe consumption
#' data. Experimental and Clinical Psychopharmacology, 23(6), 504–512. https://doi.org/10.1161/CIRCRESAHA.116.303790
#'
#' @param pt A data frame consisting of the `id_var` and purchase task variables.
#' @param id_var The name of the unique identifier as identified in the data frame.
#' @param type The level for fitting the demand curves, one of c("overall","group","individual"). The default is "overall" which will calculate
#' overall demand for the entire data frame. For `type` "group", equation-derived demand indicators for each of the groups as identified by the `group_var`
#' are visualized. When `type` equals "individual", equation-derived demand indicators are calculated for each individual as identified by `id_var`.
#' @param k The k-value to use in the exponentiated demand equation. The default is NULL, in which the k-value is calculated from the entire sample,
#' with k representing the span of consumption in log units. Otherwise, a single numerical value can be given as the k-value,
#' allowing for comparisons across studies, since k influences the calculation of alpha. When k is calculated from the entire sample,
#' it is calculated using the mean average consumption at the lowest and highest price points and in instances when the mean average consumption
#' at the highest price point is 0, the lowest non-zero mean consumption is used.
#' @param group_var The name of the grouping variable when `type` equals "group".
#' @param n_starts The number of starting values used to calculate additional starting values of Q0 and alpha to use for fitting the demand curve when `type` equals "individual". The default is 10.
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
#' pt4 <- pt_curve(pt3$data, id_var = "ID", type = "individual")
#'
#' @return A ggplot2 graphical object; For `type` "individual", the original pt data frame plus the derived values for each individual is returned.
#' @export

pt_curve <- function(pt, id_var, type = NULL, k = NULL, group_var = NULL, n_starts = 10) {

  if(is.null(type)) stop(rlang::format_error_bullets(c( "!" = c("Type required. Please select one of c('overall','group','individual') using the 'type' argument."))), call. = FALSE)
  if(!is.data.frame(pt)) stop(rlang::format_error_bullets(c( x = c("'pt' must be a data frame."))), call. = FALSE)
  if(n_starts<2) stop(rlang::format_error_bullets(c( x = c("Number of starts must be greater than 1"))), call. = FALSE)

  pt_names <- names(pt)
  var_exclude <- c("Intensity","Breakpoint","Omax","Pmax","Eta","R2","AUC")

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

  pt$q0 <- as.numeric(pt[,prices[1]])

  pt_long <- stats::reshape(as.data.frame(pt), idvar = "id",
                            varying = prices,
                            v.names = c("q"), timevar = c("c"), sep = "", direction = "long")

  pt_long <- pt_long[order(pt_long$id),]
  pt_long$c <- as.numeric(prices)

  ### grab mean values of q for every price (c); then calculate max value minus min value

  pt_mean <- stats::aggregate(pt_long[c("q")], by = list(c = pt_long[,"c"]), function(x) mean(x, na.rm = T))

  ## if the mean consumption is 0 for the highest price point, then use the smallest non-0 mean consumption value
  ## otherwise, the k-value range is the mean consumption at the highest and lowest price points
  k_range <- ifelse(pt_mean$q[pt_mean$c==max(pt_mean$c)]==0,
                    log10(pt_mean$q[pt_mean$c==min(pt_mean$c)])-log10(pt_mean$q[pt_mean$c==max(pt_mean$c[pt_mean$q!=0])]),
                    log10(pt_mean$q[pt_mean$c==min(pt_mean$c)])-log10(pt_mean$q[pt_mean$c==max(pt_mean$c)]))

  message(rlang::format_error_bullets(c(i = c("NOTE: \u03b1 is defined as the rate of change in elasticity (\u03B7)"))))

  if(is.null(k)){
    kval <- round(k_range,1)
    message(rlang::format_error_bullets(c(i = paste0("Calculated k-value: ", kval))))
  } else if(!is.null(k)){
    kval <- k
  }

  if(kval<(exp(1)/log(10))){
    message(rlang::format_error_bullets(c(i = "When k is < `exp(1)/log(10)`, the price associated with maximum consumption does not reach unit elasticity")))
  }

  equation <- "q ~ q0 * 10^(k * (exp(-alpha * q0 * c)-1))"

  ### Grab the range of data for start values

  pt_dat <- pt_long
  pt_dat$expenditure <- pt_dat$c*pt_dat$q
  pt_summ_dat <- stats::aggregate(pt_dat[c("expenditure")], by = list(id = pt_dat[,"id"]), function(x) max(x))
  colnames(pt_summ_dat) <- c("id","omax")

  pt_dat <- merge(pt_dat, pt_summ_dat, by = "id", all.x = TRUE)

  ##### ----- OVERALL ELASTICITY

  if(type=="overall"){

    ### From mean

    pt_mean$k <- kval
    pt_mean$expenditure <- pt_mean$c*pt_mean$q
    pt_mean$omax <- max(pt_mean$expenditure)

    pmax_emp <- pt_mean$c[pt_mean$expenditure==pt_mean$omax]

    q0_start <- pt_mean$q[pt_mean$c==min(pt_mean$c)]
    lambert_val <- -(1/log(10^kval))
    lambert_val[lambert_val<(-1/exp(1))] <- -1/exp(1) ## In case of a low k-value, the Lambert W function will not be defined, thus set as lowest defined value
    alpha_start <- -(pracma::lambertWp(lambert_val))/(q0_start*pmax_emp)

    pt_mod_mean <- stats::nls(equation, data = pt_mean, start = list(q0 = q0_start, alpha = alpha_start), control = stats::nls.control(maxiter = 500))

    coef_mean <- as.character(stats::coef(pt_mod_mean))
    coef_mean_dat <- data.frame(q0 = coef_mean[1], alpha = coef_mean[2])

    coef_mean_dat$pmax <- -(pracma::lambertWp(lambert_val))/(as.numeric(coef_mean_dat$alpha) * as.numeric(coef_mean_dat$q0))
    coef_mean_dat$rsquared <- 1 - sum(stats::resid(pt_mod_mean)^2)/ sum((pt_mean$q - mean(pt_mean$q))^2)

    coef_mean_dat$facet <- 1

    ### PREP for visualization

    ### Predict using more values of c, rather than set prices (creates smoother line)
    pt_pred <- data.frame(c = seq(zero_conv,as.numeric(prices[length(prices)]), by = (length(prices)/length(prices)^3)))

    suppressWarnings({
      pt_pred$pred <- stats::predict(pt_mod_mean, pt_pred)
    })

    pt_mean$c[pt_mean$c==0] <- (zero_conv/2)

    ### Should only facet (i.e. create a break if prices[1]==0)
    ### Thus use two different plot codes one for facet, one without.
    pt_mean$facet <- ifelse(pt_mean$c==zero_conv/2,0,1)
    pt_pred$facet <- 1

    pt_anno <- paste0("\u03b1: ", signif(as.numeric(coef_mean_dat$alpha)), "     ",
                      "Q0: ", signif(as.numeric(coef_mean_dat$q0)),"     ",
                      "Pmax: ", signif(as.numeric(coef_mean_dat$pmax)),"     ",
                      "R\u00b2: ", signif(as.numeric(coef_mean_dat$rsquared)),"     ")

    pt_plot <- ggplot2::ggplot(pt_mean, ggplot2::aes(x = c, y = q)) +
      ### Vertical line separating inelasticity and elasticity (i.e. when unit elasticity is reached = -1)
      ggplot2::geom_vline(pt_mean[(pt_mean$facet==1),], mapping = ggplot2::aes(xintercept = coef_mean_dat$pmax), linewidth = 1, linetype = "dashed", colour = "#BEBEBE") +
      ggplot2::geom_line(pt_pred, mapping = ggplot2::aes(y = pred), lineend = "round", linewidth = 2, colour = "#999999") +
      ggplot2::geom_point(size = 2.5) +
      ggplot2::facet_grid(~facet, scales = "free_x", space = "free_x") +
      ggplot2::scale_x_log10(breaks = c(zero_conv/2,log_labels), labels = c(prices[1],log_labels)) +
      ggplot2::theme_classic() + ggplot2::xlab("\n Price (Log)") + ggplot2::ylab("Consumption \n") +
      ggplot2::ggtitle("Mean Demand Curve") +
      ggplot2::theme(plot.title = ggplot2::element_text(size = 25, face = "bold", hjust = 0.5),
                     axis.title = ggplot2::element_text(size = 22, face = "bold"),
                     axis.text = ggplot2::element_text(size = 19),
                     axis.ticks.length = ggplot2::unit(2,"mm"),
                     strip.background = ggplot2::element_blank(),
                     strip.text = ggplot2::element_blank(),
                     axis.line = ggplot2::element_line(linewidth = 1.5))

    message(rlang::format_error_bullets(c(i = pt_anno)))

    ##### ----- GROUP ELASTICITY

  } else if(type == "group"){

  ### grab mean values of y for every price (x) by group; then calculate max value minus min value

  pt_group_mean <- stats::aggregate(pt_long[c("q")], list(c = pt_long[,"c"], group = pt_long[,"group"]), function(x) mean(x, na.rm = T))

  pt_group_mean$group <- as.factor(pt_group_mean$group)

  ### Create loop for each group

  group_uniq <- unique(pt_group_mean$group)

  pt_group_mean$k <- kval
  pt_elast <- data.frame(group = NULL, q0 = NULL, alpha = NULL, unit_elast = NULL, r2 = NULL)
  pt_plot_dat <- data.frame(group = NULL, pred = NULL)

  for(group_u in group_uniq){

    pt_g <- pt_group_mean[(pt_group_mean$group == group_u),]

    pt_g$expenditure <- pt_g$c*pt_g$q
    pt_g$omax <- max(pt_g$expenditure)

    pmax_emp_g <- max(pt_g$c[pt_g$expenditure==pt_g$omax])

    q0_start_g <- pt_g$q[pt_g$c==min(pt_g$c)]
    lambert_val_g <- -(1/log(10^kval))
    lambert_val_g[lambert_val_g<(-1/exp(1))] <- -1/exp(1) ## In case of a low k-value, the Lambert W function will not be defined, thus set as lowest defined value
    alpha_start_g <- -(pracma::lambertWp(lambert_val_g))/(q0_start_g*pmax_emp_g)

    pt_mod_g <- stats::nls(equation, data = pt_g, start = list(q0 = q0_start_g,alpha = alpha_start_g), control = stats::nls.control(maxiter = 500))

      pt_group_pred <- data.frame(c = seq(zero_conv,as.numeric(prices[length(prices)]), zero_conv))

      suppressWarnings({
        pt_group_pred$pred <- stats::predict(pt_mod_g, pt_group_pred)
      })

      coef_g <- as.character(stats::coef(pt_mod_g))

      ### Calculate R^2 for individual curves

      r2_g <- 1 - sum(stats::resid(pt_mod_g)^2)/ sum((pt_g$q - mean(pt_g$q))^2)

      pmax_g <- -(pracma::lambertWp(lambert_val_g))/(as.numeric(coef_g[2]) * as.numeric(coef_g[1]))


    group_dat <- data.frame(group = group_u, q0 = coef_g[1], alpha = coef_g[2], unit_elast = pmax_g, r2 = r2_g)

    pt_group_pred$group <- group_u

    pt_elast <- rbind(pt_elast,group_dat)
    pt_plot_dat <- rbind(pt_plot_dat,pt_group_pred)
  }

    pt_elast$facet <- 1

    pt_plot_dat$c[pt_plot_dat$c==0] <- zero_conv
    pt_plot_dat$group <- as.factor(pt_plot_dat$group)
    pt_plot_dat$facet <- 1

    pt_group_mean$facet <- ifelse(pt_group_mean$c==prices[1],0,1)
    pt_group_mean$c[pt_group_mean$c==0] <- zero_conv/2

    pt_group_pred$facet <- 1

    pt_elast$label <- paste0(pt_elast$group,": ",
                            "\u03b1: ", signif(as.numeric(pt_elast$alpha)), "     ",
                            "Q0: ", signif(as.numeric(pt_elast$q0)),"     ",
                            "Pmax: ", signif(as.numeric(pt_elast$unit_elast)),"     ",
                            "R\u00b2: ", signif(as.numeric(pt_elast$r2)),"     ")

    pt_plot <- ggplot2::ggplot(pt_group_mean, ggplot2::aes(x = c, y = q, group = group, colour = group)) +
      ### Vertical line separating inelasticity and elasticity (i.e. when unit elasticity is reached = -1)
      ggplot2::geom_vline(pt_elast, mapping = ggplot2::aes(xintercept = unit_elast, colour = group), linewidth = 1, linetype = "dashed", show.legend = FALSE) +
      ggplot2::geom_line(pt_plot_dat, mapping = ggplot2::aes(y = pred), lineend = "round", linewidth = 2, alpha = 1) +
      ggplot2::geom_point(alpha = 0.8, size = 3.5, show.legend = FALSE) +
      ggplot2::facet_grid(~facet, scales = "free_x", space = "free_x") +
      ggplot2::scale_x_log10(breaks=c(zero_conv/2, log_labels),
                             labels=c(paste0(min(prices)), log_labels)) +
      ggplot2::theme_classic() + ggplot2::xlab("\n Price (Log)") + ggplot2::ylab("Consumption \n") +
      ggplot2::ggtitle("Demand Curves by Group") +
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

    for(out in pt_elast$label){
      message(rlang::format_error_bullets(c(i = out)))
    }

    ##### ----- INDIVIDUAL ELASTICITY

  } else if(type == "individual"){

    pt_long$k <- kval

    pt_elast <- data.frame(id = NULL, q0 = NULL, alpha = NULL, unit_elast = NULL, r2 = NULL)

    for(id_num in pt$id){

      pt_i <- pt_long[(pt_long$id == id_num),]

      pt_i$expenditure <- pt_i$c*pt_i$q
      pt_i$omax <- max(pt_i$expenditure)

      pmax_emp_i <- max(pt_i$c[pt_i$expenditure==pt_i$omax]) ## don't need to adjust for consumption that only occurs at $0 (as curve will not be fit)

      q0_start_i <- pt_i$q[pt_i$c==min(pt_i$c)]
      lambert_val_i <- -(1/log(10^kval))
      lambert_val_i[lambert_val_i<(-1/exp(1))] <- -1/exp(1) ## In case of a low k-value, the Lambert W function will not be defined, thus set as lowest defined value
      alpha_start_i <- -(pracma::lambertWp(lambert_val_i))/(q0_start_i*pmax_emp_i)

      ## Convert any zero values in the first two price points to NA as they will not be adequately fit
      ### could modify and calculate, then user can decide if they want to remove these estimates
      ### but would then need to use a non-zero pmax (zero_conv) to calculate alpha starting value
      if(pt_i$q[pt_i$c==as.numeric(prices[1])]==0 | pt_i$q[pt_i$c==as.numeric(prices[2])]==0){

        coef_i <- c(NA,NA)
        pmax_i <- NA
        r2_i <- NA
        brute_i <- NA

      } else if(pt_i$q[pt_i$c==as.numeric(prices[1])]!=0 & pt_i$q[pt_i$c==as.numeric(prices[2])]!=0){

        ### Need to feed coefficients that fit for the nls2 to the nls to get actual useful estimates
        ### because grid-search will use the exact start value fed to the algorithm (identical to brute-force).
        ### Thus, will estimate various alpha start values based on varying estimates of empirical Q0,
        ### with Q0 as much as 75% lower or 75% higher than empirical Q0.

        n_start_below <- (n_starts/2)-1
        n_start_above <- (n_starts-1)-n_start_below

        q0_start_above_i <- seq(q0_start_i,q0_start_i*1.75, by = (((q0_start_i*1.75)-q0_start_i)/n_start_above))
        q0_start_below_i <- seq(q0_start_i*0.25,q0_start_i, by = ((q0_start_i-(q0_start_i*0.25))/n_start_below))

        q0_start_range_i <- c(q0_start_below_i,q0_start_above_i)
        q0_start_range_i <- q0_start_range_i[!duplicated(q0_start_range_i)]

        start_val_i <- data.frame("q0" = c(q0_start_range_i))

        start_val_i$alpha <- -(pracma::lambertWp(lambert_val_i))/(start_val_i$q0*pmax_emp_i)

        start_val_i <- expand.grid(start_val_i)

        pt_mod_start_i <- NULL

        ### suppress errors when running models
        mess_nls2a <- utils::capture.output(type = "message", ## see if model will converge on its own (coefficients will be used as final starting values)
          pt_mod_start_i <- try(nls2::nls2(equation, data = pt_i, start = c("q0" = q0_start_i,"alpha" = alpha_start_i)), silent = TRUE))

        if(inherits(pt_mod_start_i,"try-error")){
          mess_nls2b <- utils::capture.output(type = "message", ## if the above model does not converge, then force it to (coefficients will be used as final starting values)
            pt_mod_start_i <- try(nls2::nls2(equation, data = pt_i, start = start_val_i, algorithm = "grid-search"), silent = TRUE))

        }

        while(TRUE){

          pt_mod_i <- NULL
          brute_i <- FALSE

          try(pt_mod_i <- stats::nls(equation, data = pt_i, start = stats::coef(pt_mod_start_i), control = stats::nls.control(maxiter = 500)), silent = TRUE); # does not stop in the case of error

          if(is.null(pt_mod_i)){
            try(pt_mod_i <- nls2::nls2(equation, data = pt_i, start = stats::coef(pt_mod_start_i), algorithm = "grid-search"), silent = TRUE); ### Use Brute Force & Label
            brute_i <- TRUE
          }

          if(!is.null(pt_mod_i))break; ### Quit from loop if NLS works

        }

        coef_i <- as.character(stats::coef(pt_mod_i))

        ### Calculate R^2 for individual curves

        r2_i <- 1 - sum(stats::resid(pt_mod_i)^2)/ sum((pt_i$q - mean(pt_i$q))^2)

        pmax_i <- -(pracma::lambertWp(lambert_val_i))/(as.numeric(coef_i[2]) * as.numeric(coef_i[1]))

        id_dat <- data.frame(id = id_num, q0 = coef_i[1], alpha = coef_i[2], unit_elast = pmax_i, r2 = r2_i, brute = brute_i)

        pt_elast <- rbind(pt_elast,id_dat)

      }

    }

    ### PREPARE data for export

    pt_elast$q0 <- as.numeric(pt_elast$q0)
    pt_elast$alpha <- as.numeric(pt_elast$alpha)
    pt_elast$K <- kval

    pt_elast <- pt_elast[c("id","K","q0","alpha","unit_elast","r2","brute")]
    colnames(pt_elast) <- c("id","K","Q0","Alpha", "UnitElasticity","R2","Brute")

    brute.id <- pt_elast$id[pt_elast$Brute==TRUE]

    if(length(brute.id)==0) (brute.id <- "NULL")

    message(rlang::format_error_bullets(c(i = c("IDs with demand indicators derived via brute-force fitting methods:"),
                                          " " = c(paste(brute.id, collapse = ",")))))

  }

  if(type == "overall" | type == "group"){
  ### Suppress warning about only 1 observation in the facet_grid
  suppressMessages({
    print(pt_plot)
  })
  } else if(type == "individual"){

    pt_final <- merge(pt[c("id",pt_names[pt_names!=id_var])], pt_elast, by = "id", all.x = T)
    names(pt_final)[names(pt_final) == "id"] <- id_var
    return(pt_final)
  }

}
