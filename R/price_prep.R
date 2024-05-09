#' Rename variables as prices
#'
#' This function helps users efficiently rename their variables as prices. At the same time, it will return only the
#' variables required for purchase task processing: the unique identifier and the purchase task variables themselves.
#'
#' @param pt A data frame which includes the `id_var` and purchase task variables.
#' @param id_var The name of the unique identifier (ID) as identified in the data frame.
#' @param vars The variable names of the purchase task items.
#' @param prices The prices associated with each purchase task item in the same order as the `vars`.
#' @examples
#' ### --- Load Data
#' data("cpt_data")
#'
#' ### --- Function Example
#' pt <- price_prep(cpt_data, id_var = "ID", vars = c(paste0("cpt",1:15)),
#' prices = c("0","0.05","0.10","0.20","0.30","0.40","0.50","0.75","1","2","3","4","5","7.5","10"))
#'
#' @return A data frame consisting of variables `vars` renamed as `prices`, as well as `id_var`.
#' @export

price_prep <- function(pt, id_var, vars, prices) {

  if(length(vars)!=length(prices)) stop(rlang::format_error_bullets( c(x = "Number of variables and prices do not match.")), call. = FALSE)
  if(!is.data.frame(pt)) stop(rlang::format_error_bullets(c( x = c("'pt' must be a data frame."))), call. = FALSE)

    pt <- pt[c(id_var,vars)]
    colnames(pt) <- c(id_var,prices)

  return(pt)
}

