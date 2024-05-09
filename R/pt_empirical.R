#' Calculating empirical demand indicators
#'
#' This function extracts the empirical demand indicators (Intensity, Breakpoint, and Omax) from the purchase task. The demand indicator Pmax is not calculated
#' by default, as it is increasingly considered of dubious conceptual interest and to be epiphenomenonological in relation to health outcomes (Martínez-Loredo et al. 2021).
#'
#' Martínez‐Loredo, V., González‐Roz, A., Secades‐Villa, R., Fernández‐Hermida, J. R., & MacKillop, J. (2021). Concurrent validity of the Alcohol Purchase Task for measuring
#' the reinforcing efficacy of alcohol: An updated systematic review and meta‐analysis. Addiction, 116(10), 2635-2650.
#'
#' @param pt A data frame consisting of the `id_var` and purchase task variables.
#' @param id_var The name of the unique identifier as identified in the data frame.
#' @param max_bp The value of the assigned breakpoint in the absence of zero consumption by the final price point. The default is calculated empirically
#' from the data directly, using the formula Pn + (Pn - Pn-1), where Pn is the final price point, and Pn-1 is the price point immediately before the
#' final price point.
#' @param pmax A logical argument whether Pmax should be calculated. The default is FALSE.
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
#' pt4 <- pt_empirical(pt3$data, id_var = "ID")
#'
#' @return A data frame
#' @export

pt_empirical <- function(pt, id_var, max_bp = NULL, pmax = FALSE){

  if(!is.data.frame(pt)) stop(rlang::format_error_bullets(c( x = c("'pt' must be a data frame."))), call. = FALSE)

  pt_orig <- pt
  pt_names <- names(pt)
  var_exclude <- c("Alpha","K","Q0","UnitElasticity","R2","Brute","Eta","AUC")
  prices <- pt_names[pt_names!=id_var & !pt_names %in% var_exclude]
  names(pt)[names(pt) == id_var] <- "id"

  pt <- pt[c("id",prices)]

  if(is.null(max_bp)){
    max_bp <- as.numeric(prices[length(prices)]) + (as.numeric(prices[length(prices)]) -  as.numeric(prices[length(prices)-1]))
  }

  suppressWarnings({
    if(length(prices[is.na(as.numeric(prices))])==length(prices)) stop(rlang::format_error_bullets(c( x = c("Names of purchase task variables must be numeric. Use `price_prep()` to rename variables."))), call. = FALSE)
    if(length(prices[is.na(as.numeric(prices))])>0) stop(rlang::format_error_bullets(c( x = c("Variables other than 'id_var' and the purchase task items are detected. Please include only the variables required."))), call. = FALSE)
  })

  ### --- Calculate Empirical Q0 (Intensity) and BP

  pt$q0 <- as.numeric(pt[,prices[1]])
  pt$max_q <- as.numeric(apply(pt[ ,prices], 1, function(x) {names(x)[maxval(which(x > 0))] }))

  for(pt_id in pt$id){
    bp_prices <- as.numeric(prices)[as.numeric(prices)>pt$max_q[pt$id==pt_id]]
    pt$bp[pt$id==pt_id] <- ifelse(length(bp_prices)==0,max_bp,min(bp_prices))
  }

  ### RESHAPE to long to calculate Empirical Omax & Pmax

  pt_long <- stats::reshape(as.data.frame(pt), idvar = "id",
                            varying = prices,
                            v.names = c("q"), timevar = c("c"), sep = "", direction = "long")

  pt_long <- pt_long[order(pt_long$id),]
  pt_long$c <- as.numeric(prices)
  pt_long$expenditure <- pt_long$c*pt_long$q

  ### --- Calculate Empirical OMAX and PMAX

  pt_all <- data.frame(id = NULL, omax = NULL, pmax = NULL)

  if(pmax==TRUE){

    for(id_num in pt$id){

      pt_dat_i <- pt_long[(pt_long$id == id_num),]

      omax_i <- max(pt_dat_i$expenditure)

      pmax_i <- max(pt_dat_i$c[pt_dat_i$expenditure==omax_i])

      pmax_i[omax_i==0] <- 0 ## when consumption only occurs at $0 (free)

      dat_i <- data.frame(id = id_num, omax = omax_i, pmax = pmax_i)

      pt_all <- rbind(pt_all,dat_i)
    }

    ### MERGE INDICATORS in final data set
    pt_final <- merge(pt[c("id","q0","bp")],pt_all, by = "id", all.x = T)
    colnames(pt_final) <- c(id_var,"Intensity","Breakpoint","Omax","Pmax")

  }

  if(pmax==FALSE){

    for(id_num in pt$id){

      pt_dat_i <- pt_long[(pt_long$id == id_num),]

      omax_i <- max(pt_dat_i$expenditure)

      dat_i <- data.frame(id = id_num, omax = omax_i)

      pt_all <- rbind(pt_all,dat_i)
    }

    ### MERGE INDICATORS in final data set
    pt_final <- merge(pt[c("id","q0","bp")],pt_all, by = "id", all.x = T)
    colnames(pt_final) <- c(id_var,"Intensity","Breakpoint","Omax")

  }

  pt_final <- merge(pt_orig, pt_final, by = id_var, all.x = T)

  return(pt_final)

}
