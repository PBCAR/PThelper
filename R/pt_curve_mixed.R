utils::globalVariables(c("id","kval","pred","alpha","q0","pmax"))

#' Mixed exponentiated demand curve fitting
#'
#' This function derives overall demand indicators using a mixed effects exponentiated equation (Kaplan et al., 2021) for the entire sample or by a
#' grouping variable. The overall demand curve will be visualized alongside each individual demand curve. See details for further information on
#' reaching convergence using mixed effects exponentiated equations.
#'
#' To reach convergence using a mixed effects approach, starting values for each individual (the random effects) are obtained by fitting an
#' exponentiated demand equation to each individual's data. The starting values for the fixed effects are obtained by fitting an exponentiated
#' demand equation to each group's mean data. To also assist with convergence, two mixed effects models are run. The first relaxes the tolerance of
#' the convergence criteria to 1e-3 (from the default 1e-6). The coefficients from this initial model provide the starting values for a second fitted
#' mixed model with a more stringent tolerance level (1e-6), as recommended by Kaplan et al. (2021). If the second model with a more strict tolerance
#' does not converge, then the initial model is returned alongside a message in the console. If the model does not reach convergence despite the
#' relaxed tolerance values, then the non-converged results using the regular default tolerance level are returned alongside a message in the console.
#' These results should not be relied upon and instead it is recommended that fixed effect demand curves are fit instead by using the `pt_curves()` function.
#'
#' Kaplan, B. A., Franck, C. T., McKee, K., Gilroy, S. P., & Koffarnus, M. N. (2021). Applying Mixed-Effects Modeling to Behavioral
#' Economic Demand: An Introduction. Perspectives on behavior science, 44(2-3), 333–358. https://doi.org/10.1007/s40614-021-00299-7
#'
#' @param pt A data frame consisting of the `id_var` and purchase task variables.
#' @param id_var The name of the unique identifier as identified in the data frame.
#' @param type The level for fitting the demand curves, one of c("overall","group"). The default is "overall" which will calculate
#' the overall demand for the entire data frame. For `type` "group", equation-derived demand indicators for each of the groups as identified by the `group_var`.
#' @param k The k-value to use in the exponentiated demand equation. The default is NULL, in which the k-value is calculated from the entire sample,
#' with k representing the span of consumption in log units. Otherwise, a single numerical value can be given as the k-value,
#' allowing for comparisons across studies, since k influences the calculation of alpha. When k is calculated from the entire sample,
#' it is calculated using the mean average consumption at the lowest and highest price points and in instances when the mean average consumption
#' at the highest price point is 0, the lowest non-zero mean consumption is used.
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
#' pt_curve_mixed(pt3$data, id_var = "ID", type = "overall")
#'
#' @return A ggplot2 graphical object
#' @export

pt_curve_mixed <- function(pt, id_var, type = NULL, k = NULL, group_var = NULL) {

  if(!is.data.frame(pt)) stop(rlang::format_error_bullets(c( x = c("'pt' must be a data frame."))), call. = FALSE)

  pt_names <- names(pt)
  var_exclude <- c("Intensity","Breakpoint","Omax","Pmax","Eta","R2","Alpha","K","Q0","UnitElasticity","Brute")

  if(type == "overall"){

    prices <- pt_names[pt_names!=id_var & !pt_names %in% var_exclude]

    suppressWarnings({
      if(length(prices[is.na(as.numeric(prices))])==length(prices)) stop(rlang::format_error_bullets(c( x = c("Names of purchase task variables must be numeric. Use `price_prep()` to rename variables."))), call. = FALSE)
      if(length(prices[is.na(as.numeric(prices))])>0) stop(rlang::format_error_bullets(c( x = c("Variables other than 'id_var' and the purchase task items are detected. Please include only the variables required."))), call. = FALSE)
    })

  }

  if(type == "group"){

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

  ### Grab the range of data for start values

  pt_dat <- pt_long
  pt_dat$expenditure <- pt_dat$c*pt_dat$q
  pt_summ_dat <- stats::aggregate(pt_dat[c("expenditure")], by = list(id = pt_dat[,"id"]), function(x) max(x))
  colnames(pt_summ_dat) <- c("id","omax")

  pt_dat <- merge(pt_dat, pt_summ_dat, by = "id", all.x = TRUE)

  ### In order to successfully provide start values for the random portion of the model,
  ### alpha and Q0 need to first be derived from fit equations

  ### Can only utilize those with non-zero consumption in first two price points for the equation

  pt_id <- pt$id[pt[prices[1]]!=0 & pt[prices[2]]!=0]

  pt_long <- pt_long[(pt_long$id %in% pt_id),]

  pt_long$k <- kval

  pt_starts <- data.frame(id = NULL, q0 = NULL, alpha = NULL)

  equation <- "q ~ q0 * 10^(k * (exp(-alpha * q0 * c)-1))"

  for(id_num in pt_id){

    pt_i <- pt_long[(pt_long$id == id_num),]

    pt_i$expenditure <- pt_i$c*pt_i$q
    pt_i$omax <- max(pt_i$expenditure)

    pmax_emp_i <- max(pt_i$c[pt_i$expenditure==pt_i$omax])

    q0_start_i <- pt_i$q[pt_i$c==min(pt_i$c)]
    lambert_val_i <- -(1/log(10^kval))
    lambert_val_i[lambert_val_i<(-1/exp(1))] <- -1/exp(1) ## In case of a low k-value, the Lambert W function will not be defined, thus set as lowest defined value
    alpha_start_i <- -(pracma::lambertWp(lambert_val_i))/(q0_start_i*pmax_emp_i)

    n_starts <- 10 ## here, just use a fixed number of starts since we are less concerned about using brute force (starts will always be based on empirical data regardless)

    n_start_above <- (n_starts/2)-1
    n_start_below <- (n_starts-1)-n_start_above
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

      try(pt_mod_i <- stats::nls(equation, data = pt_i, start = stats::coef(pt_mod_start_i), control = stats::nls.control(maxiter = 500)), silent = TRUE); # does not stop in the case of error

      if(is.null(pt_mod_i)){
        try(pt_mod_i <- nls2::nls2(equation, data = pt_i, start = stats::coef(pt_mod_start_i), algorithm = "grid-search"), silent = TRUE); ### Use Brute Force
      }

      if(!is.null(pt_mod_i))break; ### Quit from loop if NLS works

    }

    coef_i <- as.character(stats::coef(pt_mod_i))

    id_dat <- data.frame(id = id_num, q0 = coef_i[1], alpha = coef_i[2])

    pt_starts <- rbind(pt_starts,id_dat)

  }

  pt_starts$q0 <- as.numeric(pt_starts$q0)
  pt_starts$alpha <- as.numeric(pt_starts$alpha)
  rownames(pt_starts) <- pt_starts$id

  ##### ----- OVERALL ELASTICITY

  if(type == "overall"){

    pt_mean$expenditure <- pt_mean$c*pt_mean$q
    pt_mean$omax <- max(pt_mean$expenditure)
    pt_mean$k <- kval

    pmax_emp <- max(pt_mean$c[pt_mean$expenditure==pt_mean$omax])

    q0_start <- pt_mean$q[pt_mean$c==min(pt_mean$c)]
    lambert_val <- -(1/log(10^kval))
    lambert_val[lambert_val<(-1/exp(1))] <- -1/exp(1) ## In case of a low k-value, the Lambert W function will not be defined, thus set as lowest defined value
    alpha_start <- -(pracma::lambertWp(lambert_val))/(q0_start*pmax_emp)

    pt_start_i <- as.matrix(pt_starts[c("q0","alpha")])

    ### Mixed effects models suffer more easily from non-convergence. To minimize this issue, empirical starting values are first fed to a fixed effects model
    ### The parameter estimates from the fixed effects model are then used as improved starting values for the mixed effects
    pt_fix_mod <- stats::nls(equation, data = pt_mean, start = list(q0 = q0_start, alpha = alpha_start), control = stats::nls.control(maxiter = 500))

    suppressWarnings({ ## warnings about singularity still make their way through to the console despite silent = TRUE
      mess_mix1 <- utils::capture.output(type = "message", ## grab model fit with less stringent tolerance (as per Kaplan et al., 2021); Iterations increased ten-fold
                                         pt_mix_mod_init <- try(nlme::nlme(q ~ q0 * 10^(k * (exp(-alpha * q0 * c)-1)),
                                                                           data = pt_long,
                                                                           fixed = list(q0 ~ 1, alpha ~ 1),
                                                                           random = list(nlme::pdSymm(q0 + alpha ~ 1)),
                                                                           start = list(fixed = c(stats::coef(pt_fix_mod)),
                                                                                        random = pt_start_i),
                                                                           groups = ~ id,
                                                                           control = list(maxIter = 500, msMaxIter = 500, niterEM = 250, pnlsMaxIter = 70,
                                                                                          tolerance = 0.001, msTol = 0.001, msWarnNoConv = FALSE, opt = "optim")),
                                                                silent = TRUE))
    })

    if(inherits(pt_mix_mod_init,"try-error")){

      suppressWarnings({
        mess_mixf <- utils::capture.output(type = "message", ## grab model fit with less stringent tolerance (as per Kaplan et al., 2021); Iterations increased ten-fold
                                           pt_mix_mod_force <- try(nlme::nlme(q ~ q0 * 10^(k * (exp(-alpha * q0 * c)-1)),
                                                                              data = pt_long,
                                                                              fixed = list(q0 ~ 1, alpha ~ 1),
                                                                              random = list(nlme::pdSymm(q0 + alpha ~ 1)),
                                                                              start = list(fixed = c(stats::coef(pt_fix_mod)),
                                                                                           random = pt_start_i),
                                                                              groups = ~ id,
                                                                              control = list(maxIter = 500, msMaxIter = 500, niterEM = 250, pnlsMaxIter = 70, msWarnNoConv = FALSE, returnObject = TRUE)),
                                                                   silent = TRUE))
      })

      pt_mix_mod <- pt_mix_mod_force

      message(rlang::format_error_bullets(c("!" = c("Model did not converge. Results should be interpreted with extreme caution. It is recommended that fixed effects curves using the `pt_curve()` function are used instead."))))

    }


    if(!inherits(pt_mix_mod_init,"try-error")){

      suppressWarnings({
        mess_mix2 <- utils::capture.output(type = "message", ## try to obtain a converged model with a lower tolerance threshold (more stringent)
                                           pt_mix_mod <- try(nlme::nlme(q ~ q0 * 10^(k * (exp(-alpha * q0 * c)-1)),
                                                                        data = pt_long,
                                                                        fixed = list(q0 ~ 1, alpha ~ 1),
                                                                        random = list(nlme::pdSymm(q0 + alpha ~ 1)),
                                                                        start = list(fixed = c(pt_mix_mod_init$coefficients$fixed),
                                                                                     random = pt_mix_mod_init$coefficients$random),
                                                                        groups = ~ id,
                                                                        control = list(maxIter = 500, msMaxIter = 500, niterEM = 250, pnlsMaxIter = 70, msWarnNoConv = FALSE, opt = "optim")),
                                                             silent = TRUE))
      })
    }

    ### Preference is for mod2. However, in the instance of an error, then mod1 can be leveraged and a message about convergence can appear.
    if(inherits(pt_mix_mod,"try-error") & !inherits(pt_mix_mod_init,"try-error")){
      pt_mix_mod <- pt_mix_mod_init
      message(rlang::format_error_bullets(c(i = c("NOTE: Model results were achieved using a relaxed tolerance level of 1e-3."))))
    }

    coef_mean_dat <- data.frame(q0 = as.character(pt_mix_mod$coefficients$fixed[1]), alpha = as.character(pt_mix_mod$coefficients$fixed[2]))

    coef_mean_dat$pmax <- -(pracma::lambertWp(-(1/log(10^kval))))/(as.numeric(coef_mean_dat$alpha) * as.numeric(coef_mean_dat$q0))

    ### Predict using more values of c, rather than set prices (creates smoother line)
    pt_pred <- data.frame(c = seq(zero_conv,as.numeric(prices[length(prices)]), by = (length(prices)/length(prices)^3)), k = kval)
    pt_pred_i <- data.frame(c = rep(seq(zero_conv,as.numeric(prices[length(prices)]), by = (length(prices)/length(prices)^3)), by = length(pt_id)),
                            id = rep(pt_id, each = length(seq(zero_conv,as.numeric(prices[length(prices)]), by = (length(prices)/length(prices)^3)))), k = kval)

    suppressWarnings({
      pt_pred$pred <- stats::predict(pt_mix_mod, pt_pred, level = 0)
      pt_pred_i$pred <- stats::predict(pt_mix_mod, pt_pred_i, level = 1)
    })

    pt_anno <- paste0("\u03b1: ", signif(as.numeric(coef_mean_dat$alpha)), "     ",
                      "Q0: ", signif(as.numeric(coef_mean_dat$q0)),"     ",
                      "Pmax: ", signif(as.numeric(coef_mean_dat$pmax)),"     ")

    pt_plot <- ggplot2::ggplot(pt_pred_i[!is.na(pt_pred_i$pred),], ggplot2::aes(x = c, y = pred)) +
      ggplot2::geom_line(mapping = ggplot2::aes(group = id), lineend = "round", linewidth = 0.75, colour = "#BEBEBE") +
      ### Vertical line separating inelasticity and elasticity (i.e. when unit elasticity is reached = -1)
      ggplot2::geom_vline(mapping = ggplot2::aes(xintercept = coef_mean_dat$pmax), linewidth = 1, linetype = "dashed", colour = "#999999") +
      ggplot2::geom_line(pt_pred, mapping = ggplot2::aes(x = c, y = pred), lineend = "round", linewidth = 2, colour = "#000000") +
      ggplot2::scale_x_log10(breaks = c(zero_conv/2,log_labels), labels = c(prices[1],log_labels)) +
      ggplot2::theme_classic() + ggplot2::xlab("\n Price (Log)") + ggplot2::ylab("Consumption \n") +
      ggplot2::ggtitle("Overall Demand Curve") +
      ggplot2::theme(plot.title = ggplot2::element_text(size = 25, face = "bold", hjust = 0.5),
                     axis.title = ggplot2::element_text(size = 22, face = "bold"),
                     axis.text = ggplot2::element_text(size = 19),
                     axis.ticks.length = ggplot2::unit(2,"mm"),
                     strip.background = ggplot2::element_blank(),
                     strip.text = ggplot2::element_blank(),
                     axis.line = ggplot2::element_line(linewidth = 1.5))

    message(rlang::format_error_bullets(c(i = pt_anno)))

  } else if(type == "group"){
    ### grab mean values of y for every price (x) by group; then calculate max value minus min value

    pt_group_mean <- stats::aggregate(pt_long[c("q")], list(c = pt_long[,"c"], group = pt_long[,"group"]), function(x) mean(x, na.rm = T))

    pt_group_mean$group <- as.factor(pt_group_mean$group)

    pt_group_mean$k <- kval

    ### Create loop for each group

    group_uniq <- unique(pt_group_mean$group)

    pt_elast <- data.frame(group = NULL, q0 = NULL, alpha = NULL, unit_elast = NULL)
    pt_pred <- data.frame(group = NULL, pred = NULL, k = NULL)
    pt_pred_i <- data.frame(id = NULL, group = NULL, pred = NULL, k = NULL)

    for(group_u in group_uniq){

      ### Add grouping to derived starting values for random effects

      pt_group_id <- pt$id[pt$group== group_u]

      pt_g <- pt_group_mean[(pt_group_mean$group == group_u),]

      pt_g$expenditure <- pt_g$c*pt_g$q
      pt_g$omax <- max(pt_g$expenditure)
      pt_g$k <- kval

      pmax_emp_g <- max(pt_g$c[pt_g$expenditure==pt_g$omax])

      q0_start_g <- pt_g$q[pt_g$c==min(pt_g$c)]
      lambert_val_g <- -(1/log(10^kval))
      lambert_val_g[lambert_val_g<(-1/exp(1))] <- -1/exp(1) ## In case of a low k-value, the Lambert W function will not be defined, thus set as lowest defined value
      alpha_start_g <- -(pracma::lambertWp(lambert_val_g))/(q0_start_g*pmax_emp_g)

      pt_start_ig <- as.matrix(pt_starts[(pt_starts$id %in% pt_group_id),][c("q0","alpha")])

      ### Mixed effects models suffer more easily from non-convergence. To minimize this issue, empirical starting values are first fed to a fixed effects model
      ### The parameter estimates from the fixed effects model are then used as improved starting values for the mixed effects
      pt_fix_mod <- stats::nls(equation, data = pt_g, start = list(q0 = q0_start_g, alpha = alpha_start_g), control = stats::nls.control(maxiter = 500))

      suppressWarnings({ ## warnings about singularity still make their way through to the console despite silent = TRUE
        mess_mix1 <- utils::capture.output(type = "message", ## grab model fit with less stringent tolerance (as per Kaplan et al., 2021); Iterations increased ten-fold
                                           pt_mix_mod_init <- try(nlme::nlme(q ~ q0 * 10^(k * (exp(-alpha * q0 * c)-1)),
                                                                             data = pt_long[(pt_long$group == group_u),],
                                                                             fixed = list(q0 ~ 1, alpha ~ 1),
                                                                             random = list(nlme::pdSymm(q0 + alpha ~ 1)),
                                                                             start = list(fixed = c(stats::coef(pt_fix_mod)),
                                                                                          random = pt_start_ig),
                                                                             groups = ~ id,
                                                                             control = list(maxIter = 500, msMaxIter = 500, niterEM = 250, pnlsMaxIter = 70,
                                                                                            tolerance = 0.001, msTol = 0.001, msWarnNoConv = FALSE)),
                                                                  silent = TRUE))
      })

      if(inherits(pt_mix_mod_init,"try-error")){

        suppressWarnings({
          mess_mixf <- utils::capture.output(type = "message", ## grab model fit with less stringent tolerance (as per Kaplan et al., 2021); Iterations increased ten-fold
                                             pt_mix_mod_force <- try(nlme::nlme(q ~ q0 * 10^(k * (exp(-alpha * q0 * c)-1)),
                                                                                data = pt_long[(pt_long$group == group_u),],
                                                                                fixed = list(q0 ~ 1, alpha ~ 1),
                                                                                random = list(nlme::pdSymm(q0 + alpha ~ 1)),
                                                                                start = list(fixed = c(stats::coef(pt_fix_mod)),
                                                                                             random = pt_start_ig),
                                                                                groups = ~ id,
                                                                                control = list(maxIter = 500, msMaxIter = 500, niterEM = 250, pnlsMaxIter = 70, msWarnNoConv = FALSE, returnObject = TRUE)),
                                                                     silent = TRUE))
        })

        pt_mix_mod <- pt_mix_mod_force

        message(rlang::format_error_bullets(c("!" = c(paste0("For group ",group_u," model did not converge. Results should be interpreted with extreme caution. It is recommended that fixed effects curves using the `pt_curve()` function are used instead.")))))

      }


      if(!inherits(pt_mix_mod_init,"try-error")){ ## If convergence isn't reached with relaxed tolerance above, return results
        suppressWarnings({
          mess_mix2 <- utils::capture.output(type = "message", ## try to obtain a converged model with a lower tolerance threshold (more stringent)
                                             pt_mix_mod <- try(nlme::nlme(q ~ q0 * 10^(k * (exp(-alpha * q0 * c)-1)),
                                                                          data = pt_long[(pt_long$group == group_u),],
                                                                          fixed = list(q0 ~ 1, alpha ~ 1),
                                                                          random = list(nlme::pdSymm(q0 + alpha ~ 1)),
                                                                          start = list(fixed = c(pt_mix_mod_init$coefficients$fixed),
                                                                                       random = pt_mix_mod_init$coefficients$random),
                                                                          groups = ~ id,
                                                                          control = list(maxIter = 500, msMaxIter = 500, niterEM = 250, pnlsMaxIter = 70, mmsWarnNoConv = FALSE, opt = "optim")),
                                                               silent = TRUE))
        })
      }
      ### Preference is for mod2. However, in the instance of an error, then mod1 can be leveraged and a message about convergence can appear.
      if(inherits(pt_mix_mod,"try-error") & !inherits(pt_mix_mod_init,"try-error")){
        pt_mix_mod <- pt_mix_mod_init
        message(rlang::format_error_bullets(c(i = c(paste0("NOTE: For Group ",group_u," Model results were achieved using a relaxed tolerance level of 1e-3.")))))
      }

      coef_mean_dat <- data.frame(group = group_u, q0 = as.character(pt_mix_mod$coefficients$fixed[1]), alpha = as.character(pt_mix_mod$coefficients$fixed[2]))

      coef_mean_dat$unit_elast <- -(pracma::lambertWp(-(1/log(10^kval))))/(as.numeric(coef_mean_dat$alpha) * as.numeric(coef_mean_dat$q0))

      ### Predict using more values of c, rather than set prices (creates smoother line)
      pt_group_pred <- data.frame(c = seq(zero_conv,as.numeric(prices[length(prices)]), by = (length(prices)/length(prices)^3)), k = kval)
      pt_group_pred_i <- data.frame(c = rep(seq(zero_conv,as.numeric(prices[length(prices)]), by = (length(prices)/length(prices)^3)), by = length(pt_id)),
                                    id = rep(pt_id, each = length(seq(zero_conv,as.numeric(prices[length(prices)]), by = (length(prices)/length(prices)^3)))), k = kval)

      pt_group_pred$group <- group_u
      pt_group_pred_i$group <- group_u

      suppressWarnings({
        pt_group_pred$pred <- stats::predict(pt_mix_mod, pt_group_pred, level = 0)
        pt_group_pred_i$pred <- stats::predict(pt_mix_mod, pt_group_pred_i, level = 1)
      })

      pt_elast <- rbind(pt_elast,coef_mean_dat)
      pt_pred <- rbind(pt_pred,pt_group_pred)
      pt_pred_i <- rbind(pt_pred_i,pt_group_pred_i)
    }

    pt_elast$label <- paste0(pt_elast$group,": ",
                             "\u03b1: ", signif(as.numeric(pt_elast$alpha)), "     ",
                             "Q0: ", signif(as.numeric(pt_elast$q0)),"     ",
                             "Pmax: ", signif(as.numeric(pt_elast$unit_elast)),"     ")

    pt_plot <- ggplot2::ggplot(pt_pred_i[!is.na(pt_pred_i$pred),], ggplot2::aes(x = c, y = pred, colour = group)) +
      ggplot2::geom_line(mapping = ggplot2::aes(group = id), lineend = "round", linewidth = 0.75, colour = "#BEBEBE") +
      ### Vertical line separating inelasticity and elasticity (i.e. when unit elasticity is reached = -1)
      ggplot2::geom_vline(pt_elast, mapping = ggplot2::aes(xintercept = unit_elast), linewidth = 1, linetype = "dashed", colour = "#999999") +
      ggplot2::geom_line(pt_pred, mapping = ggplot2::aes(x = c, y = pred, group = group), lineend = "round", linewidth = 2) +
      ggplot2::facet_grid(~group, scales = "free_x", space = "free_x") +
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
  }

  suppressMessages({
    print(pt_plot)
  })

}
