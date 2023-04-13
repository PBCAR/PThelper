#' PT EMPIRICAL
#'
#' This function extracts the empirical demand indices (Intensity, Breakpoint, Omax, and Pmax) from the purchase task.
#'
#' @param pt A data frame consisting of the `id_var` and purchase task variables.
#' @param id_var The name of the unique identifier as identified in the data frame.
#' @return A data frame
#' @export

pt_empirical <- function(pt, id_var){

  pt_orig <- pt
  pt_names <- names(pt)
  prices <- pt_names[pt_names!=id_var & pt_names!="Q0" & pt_names!="Elasticity"]
  names(pt)[names(pt) == id_var] <- "id"

  pt <- pt[c("id",prices)]

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

  pt_omax <- stats::aggregate(pt_long[c("expenditure")], by = list(id = pt_long[,"id"]), function(x) max(x, na.rm = T))
  pt_omax$omax <- pt_omax$expenditure

  pt_long <- merge(pt_long,pt_omax[c("id","omax")], by = "id", all.x = T)
  pt_long$max_price <- ifelse(pt_long$omax==pt_long$expenditure,pt_long$c,NA)
  pt_pmax <- stats::aggregate(pt_long[c("max_price")], by = list(id = pt_long[,"id"]), function(x) max(x, na.rm = T))
  pt_pmax$pmax <- pt_pmax$max_price
  pt_max <- merge(pt_omax[c("id","omax")],pt_pmax[c("id","pmax")], by = "id", all.x = T)

  ### MERGE INDICATORS in final data set
  pt_final <- merge(pt[c("id","q0","bp")],pt_max, by = "id", all.x = T)
  colnames(pt_final) <- c(id_var,"Intensity","Breakpoint","Omax","Pmax")

  pt_final <- merge(pt_orig, pt_final, by = id_var, all.x = T)

  return(pt_final)

}
