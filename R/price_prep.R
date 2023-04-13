#' PRICE PREP
#'
#' This function helps users efficiently rename their variables as prices, which is essential to processing purchase task data.
#' At the same time, it will return only the variables required for purchase task processing -- the unique identifier and the
#' purchase task variables themselves.
#'
#' @param pt A data frame which includes the `id_var` and purchase task variables.
#' @param id_var The name of the unique identifier (ID) as identified in the data frame.
#' @param vars The names of variables which make up the purchase task.
#' @param prices The prices associated with each variable name in the same order as the `vars`.
#' @examples
#' pt <- price_prep(cpt_data, id_var = "ID", vars = c(paste0("cpt",1:15)),
#' prices = c("0","0.05","0.10","0.20","0.30","0.40","0.50", "0.75","1","2","3","4","5","7.5","10"))
#' @return A data frame with `vars` variables renamed, as well as `id_var`
#' @export

price_prep <- function(pt, id_var, vars, prices) {

  if(length(vars)!=length(prices)) stop("Number of variables and prices do not match!")

    pt <- pt[c(id_var,vars)]
    colnames(pt) <- c(id_var,prices)

  return(pt)
}

