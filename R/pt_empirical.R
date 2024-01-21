#' `pt_empirical()`
#'
#' This function extracts the empirical demand indices (Intensity, Breakpoint, Omax, and Pmax) from the purchase task.
#'
#' @param pt A data frame consisting of the `id_var` and purchase task variables.
#' @param id_var The name of the unique identifier as identified in the data frame.
#' @examples
#'
#' ### --- Load Data
#' data("cpt_data")
#'
#' ### --- Prep Data
#' pt <- price_prep(cpt_data, id_var = "ID", vars = c(paste0("cpt",1:15)),
#' prices = c("0","0.05","0.10","0.20","0.30","0.40","0.50", "0.75","1","2","3","4","5","7.5","10"))
#'
#' pt2 <- pt_prep(pt, id_var = "ID", remove0 = TRUE, max_val = 99)
#' pt3 <- pt_qc(pt2, id_var = "ID", type = "partial")
#'
#' ### --- Function Example
#' pt4 <- pt_empirical(pt3$data,id_var = "ID")
#'
#' @return A data frame
#' @export

pt_empirical <- function(pt, id_var){

  if(!is.data.frame(pt)) stop(rlang::format_error_bullets(c( x = c("'pt' must be a data frame."))), call. = FALSE)

  pt_orig <- pt
  pt_names <- names(pt)
  var_exclude <- c("Alpha","Q0","UnitElasticity","R2","Eta","AUC")
  prices <- pt_names[pt_names!=id_var & !pt_names %in% var_exclude]
  names(pt)[names(pt) == id_var] <- "id"

  pt <- pt[c("id",prices)]

  suppressWarnings({
    if(length(prices[is.na(as.numeric(prices))])==length(prices)) stop(rlang::format_error_bullets(c( x = c("Names of purchase task variables must be numeric. Use `price_prep()` to rename variables."))), call. = FALSE)
    if(length(prices[is.na(as.numeric(prices))])>0) stop(rlang::format_error_bullets(c( x = c("Variables other than 'id_var' and the purchase task items are detected. Please include only the variables required."))), call. = FALSE)
  })

  ### --- Calculate Empirical Q0 (Intensity) and BP

  pt$q0 <- as.numeric(pt[,prices[1]])
  pt$bp <- apply(pt[ ,prices], 1, function(x) {names(x)[minval(which(x == 0))] })
  pt$bp <- ifelse(pt[,prices[length(prices)]]!=0,as.numeric(prices[length(prices)])+1,as.numeric(pt$bp))

  ### RESHAPE to long to calculate Empirical Omax & Pmax

  pt_long <- stats::reshape(as.data.frame(pt), idvar = "id",
                            varying = prices,
                            v.names = c("q"), timevar = c("c"), sep = "", direction = "long")

  pt_long <- pt_long[order(pt_long$id),]
  pt_long$c <- as.numeric(prices)
  pt_long$expenditure <- pt_long$c*pt_long$q

  ### --- Calculate Empirical OMAX and PMAX

  pt_all <- data.frame(id = NULL, omax = NULL, pmax = NULL)

  for(id_num in pt$id){

    pt_dat_i <- pt_long[(pt_long$id == id_num),]

    omax_i <- max(pt_dat_i$expenditure)

    pmax_i <- max(pt_dat_i$c[pt_dat_i$expenditure==omax_i])

    pmax_i[omax_i==0] <- 0 ## previous uses of min price above meant this line of code wasn't needed.

    dat_i <- data.frame(id = id_num, omax = omax_i, pmax = pmax_i)

    pt_all <- rbind(pt_all,dat_i)
  }

  ### MERGE INDICATORS in final data set
  pt_final <- merge(pt[c("id","q0","bp")],pt_all, by = "id", all.x = T)
  colnames(pt_final) <- c(id_var,"Intensity","Breakpoint","Omax","Pmax")

  pt_final <- merge(pt_orig, pt_final, by = id_var, all.x = T)

  return(pt_final)

}
