#' PT PREP
#'
#' This function helps users prep their data by: i) identifying and removing participants with missing responses across all items;
#' ii) (optional) re-assigning the maximum value allowed at any price point; and for purchase tasks which are not administered in
#' full, iii) identifying and removing participants who do not have zero consumption at the final price point (except in instances
#' of the final price point being reached).
#'
#' @param pt A data frame consisting of the `id_var` and purchase task variables.
#' @param id_var The name of the unique identifier (ID) as found in the data frame.
#' @param max_val Optional identification of a maximum allowed response for any given price point.
#' @param partial Optional logical statement whether the purchase task administered was not administered in full. Default is
#' FALSE. If partial is set to TRUE, then individuals whose last non-missing price does not equate to zero are identified (and removed).
#' @examples
#' pt <- pt_prep(cpt_data, id_var = "ID", max_val = 99, partial = TRUE)
#' @return A data frame with the length of participants who reach zero consumption (except at final price point administered)
#' @export


pt_prep <- function(pt, id_var, max_val = NULL, partial = FALSE) {

  pt_names <- names(pt)
  prices <- pt_names[pt_names!=id_var]
  names(pt)[names(pt) == id_var] <- "id"

  ### WARNING: Duplicate IDs are not allowed

  dupe_id <- unique(pt$id)

  if(length(dupe_id)!=length(pt$id)) stop("Duplicate IDs detected!")

  ##### ----- MAX VALUE

  if(!is.null(max_val)){

    # RE-CODE values > the max value as == to the max value
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

  ##### ----- IDENTIFY & REMOVE IDs with final non-zero consumption (except in the instance of the maximum price point)

  remove.id.nonzero <- {}

  maxval <- function(x) {if (length(x)>0) max(x) else Inf}

  pt$max_price <- apply(pt[ ,prices], 1, function(x) {names(x)[maxval(which(!is.na(x)))] })
  pt$max_cons <- match(pt$max_price,prices)
  pt$max_cons_val <- sapply(seq_along(pt[,1]), function(x) {pt[,prices][x, pt$max_cons[x]]})

  pt$zero_consumption <- ifelse(as.numeric(pt$max_cons_val)==0,TRUE,
                                ifelse(as.numeric(pt$max_cons_val)!=0 & pt$max_price==prices[length(prices)],TRUE,FALSE))

  remove.id.nonzero <- pt$id[pt$zero_consumption==FALSE]

  pt <- pt[!(pt$id %in% remove.id.nonzero),]

  if(length(remove.id.missing)==0) (remove.id.missing <- "NULL")
  if(length(remove.id.nonzero)==0) (remove.id.nonzero <- "NULL")

  cat(" IDs with Missing Values: ", remove.id.missing,
      "\n","IDs not reaching zero consumption (does not include IDs who reach max item): ",remove.id.nonzero, "\n")

  pt <- pt[c("id",prices)]
  names(pt)[names(pt) == "id"] <- id_var
  return(pt)

}
