#' `pt_prep()`
#'
#' This function helps users prepare their full or partial purchase task data. See details for full functionality.
#'
#' This function will both identify and remove participants:
#'
#' i) with missing responses across all items;
#'
#' ii) (optional) with zero consumption across all items; and
#''
#' iii) (for purchase tasks which are not administered in full) without zero consumption at the final price point (except in instances of the final price point being reached).
#'
#' In addition, this function can re-assign the maximum value allowed at any price point.
#'
#' @param pt A data frame consisting of the `id_var` and purchase task variables.
#' @param id_var The name of the unique identifier (ID) as found in the data frame.
#' @param remove0 Whether those with zero consumption across all items should be removed. The default is `TRUE`.
#' @param max_val Optional identification of a maximum allowed response for any given price point.
#' @examples
#'
#' ### --- Load Data
#' data("cpt_data")
#'
#' ### --- Prep Data
#' pt <- price_prep(cpt_data, id_var = "ID", vars = c(paste0("cpt",1:15)),
#' prices = c("0","0.05","0.10","0.20","0.30","0.40","0.50", "0.75","1","2","3","4","5","7.5","10"))
#'
#' ### --- Function Example
#' pt2 <- pt_prep(pt, id_var = "ID", remove0 = FALSE, max_val = 99)
#'
#' @return A data frame with the length of participants not identified as removed.
#' @export

pt_prep <- function(pt, id_var, remove0 = TRUE, max_val = NULL) {

  if(!is.data.frame(pt)) stop(rlang::format_error_bullets(c( x = c("'pt' must be a data frame."))), call. = FALSE)

  prices <- names(pt)[names(pt)!=id_var]
  names(pt)[names(pt) == id_var] <- "id"

  suppressWarnings({
    if(length(prices[is.na(as.numeric(prices))])==length(prices)) stop(rlang::format_error_bullets(c( x = c("Names of purchase task variables must be numeric. Use `price_prep()` to rename variables."))), call. = FALSE)
    if(length(prices[is.na(as.numeric(prices))])>0) stop(rlang::format_error_bullets(c( x = c("Variables other than 'id_var' and the purchase task items are detected. Please include only the variables required."))), call. = FALSE)
  })

  ### WARNING: Duplicate IDs are not allowed

  dupe_id <- unique(pt$id)

  if(length(dupe_id)!=length(pt$id)) stop(rlang::format_error_bullets(c( x = "Duplicate IDs detected.")), call. = FALSE)

  ##### ----- MAX VALUE

  if(!is.null(max_val)){

    ### RE-CODE values > the max value as == to the max value
    pt[,c(prices)][pt[,c(prices)] > max_val] <- max_val

  }

  ##### ----- IDENTIFY & REMOVE IDs with missing values on all price points

    remove.id.missing <- {}
    for (id_num in pt$id){
      if (sum(is.na(pt[pt[,"id"]==id_num,])) == length(prices)){
        remove.id.missing <- append(remove.id.missing, id_num)
      }
    }

    pt <- pt[!(pt$id %in% remove.id.missing),]

      if(remove0==TRUE){

        ##### ----- OPTIONAL: IDENTIFY & REMOVE IDs with zero consumption on all price points

        remove.id.zero <- {}
        for (id_num in pt$id){
          prices_i <- names(pt[(pt$id==id_num),][prices])[!is.na(pt[(pt$id==id_num),][prices])]
          if (sum(!is.na(pt[pt[,"id"]==id_num,][prices]) & pt[pt[,"id"]==id_num,][prices]==0)==length(prices_i)){
            remove.id.zero <- append(remove.id.zero, id_num)
          }
        }

      pt <- pt[!(pt$id %in% remove.id.zero),]

      } else if(remove0==FALSE){
      remove.id.zero <- NULL
    }

  ##### ----- IDENTIFY & REMOVE IDs with final non-zero consumption (except in the instance of the maximum price point)

  remove.id.nonzero <- {}

  pt$max_price <- apply(pt[ ,prices], 1, function(x) {names(x)[maxval(which(!is.na(x)))] })
  pt$max_cons <- match(pt$max_price,prices)
  pt$max_cons_val <- sapply(seq_along(pt[,1]), function(x) {pt[,prices][x, pt$max_cons[x]]})

  pt$zero_consumption <- ifelse(as.numeric(pt$max_cons_val)==0,TRUE,
                                ifelse(as.numeric(pt$max_cons_val)!=0 & pt$max_price==prices[length(prices)],TRUE,FALSE))

  remove.id.nonzero <- pt$id[pt$zero_consumption==FALSE]

  pt <- pt[!(pt$id %in% remove.id.nonzero),]

  if(length(remove.id.missing)==0) (remove.id.missing <- "NULL")
  if(length(remove.id.zero)==0) (remove.id.zero <- "NULL")
  if(length(remove.id.nonzero)==0) (remove.id.nonzero <- "NULL")

  if(remove0==TRUE){
  message(rlang::format_error_bullets(c( i = c("IDs with Missing Values:"),
                                         " " = c(paste(remove.id.missing, collapse = ",")),
                                         i = c("IDs with zero consumption:"),
                                         " " = c(paste(remove.id.zero, collapse = ",")),
                                         i = c("IDs not reaching zero consumption (does not include IDs who reach end of purchase task):"),
                                         " " = c(paste(remove.id.nonzero, collapse = ",")))))

  } else if(remove0==FALSE){
    message(rlang::format_error_bullets(c( i = c("IDs with Missing Values:"),
                                           " " = c(paste(remove.id.missing, collapse = ",")),
                                           i = c("IDs not reaching zero consumption (does not include IDs who reach end of purchase task):"),
                                           " " = c(paste(remove.id.nonzero, collapse = ",")))))

  }

  pt <- pt[c("id",prices)]
  names(pt)[names(pt) == "id"] <- id_var
  return(pt)

}
