#' `pt_winsor()`
#'
#' This function helps users to manage outliers at the price level, or at the indicator level by using winsorization techniques. See details for the different options.
#'
#' There are 3 winsorization options:
#'
#' i) Option 1 replaces outliers with the maximum non-outlying value rounded up;
#' ii) Option 2 replaces outliers with a value 1 higher than highest (or 1 lower than the lowest) non-outlying value; or
#' iii) outlying values are replaced with 1 value above the next highest non-outlying value to maintain order.
#'
#' @param pt A data frame consisting of the `id_var` and relevant purchase task variables (prices or indicators).
#' @param id_var The name of the unique identifier (ID) as identified in the data frame.
#' @param level The level at which outlier management is occurring, one of c("price","indicator"). The default is "price", which will
#' winsorize all prices of the purchase task. The "indicator" option is for winsorization at the indicator level, in which the indicator
#' to be winsorized needs to be identified by the `index_var` argument.
#' @param index_var The name of the index variable to winsorize.
#' @param z_val The absolute z-value to define outliers. The default is |3.99|.
#' @param option The winsorization option, one of c(1,2,3). The default outlier management technique is option 3.
#' @param delta Used to retain winsorization order when using winsorization type 3 for level "indicator". At the price level, a
#' the 1 unit increase in consumption means that the delta value is by default 1. For winsorization at the "indicator" level, the delta
#' must be defined by the user. For elasticity, a small value of 0.001 is recommended.
#' @examples
#'
#' ### --- Example Data
#' pt <- data.frame("ID" = c(1:36),
#' "Intensity" = c(10,12,15,0,99,11,7,6,12,7,8,10,5,6,10,0,3,
#'                 7,5,0,2,3,5,6,10,15,12,7,0,9,0,6,7,8,4,5),
#' "Breakpoint" = c(1,2,5,0,10,3,0.5,0.2,0.3,3,4,5,7.5,0.5,2,0,0.1,
#'                  0.5,0.5,0,3,2,2,1,2,3,4,1,0,2,0,5,5,7.5,2,3))
#'
#' ### --- Function Example
#' pt2 <- pt_winsor(pt, id_var = "ID", level = "indicator", index_var = c("Intensity"), delta = 1)
#'
#' @return A list consisting of two data frames: "data" which consists of the `id_var` and `pt` including the winsorized value(s); and
#' "wins_table" which provides details on which value(s) by `id_var` were winsorized (values before and after provided).
#' @export

pt_winsor <- function(pt, id_var, level = "price", z_val = 3.99, option = 3, index_var = NULL, delta = NULL) {

  if(is.null(delta) & option==3 & level == "indicator") stop(rlang::format_error_bullets(c( "!" = c("Delta value required for this winsorization option."))), .call = FALSE)
  if(is.null(index_var) & level == "indicator") stop(rlang::format_error_bullets(c( "!" = c("Indicator variable ('index_var') must be defined to use indicator-level winsorization."))), .call = FALSE)
  if(!is.data.frame(pt)) stop(rlang::format_error_bullets(c( x = c("'pt' must be a data frame."))), call. = FALSE)

  pt_names <- names(pt)

  ##### ----- PRICE LEVEL

  if(level=="price"){

    prices <- pt_names[pt_names!=id_var]
    names(pt)[names(pt) == id_var] <- "id"
    pt2 <- pt

    ### TRANSFORM into z-scores
    z_pt <- pt
    z_pt[c(prices)] <- scale(pt[c(prices)], center = TRUE, scale = TRUE)

    ##### MODIFIED Price List (if final price array not reached)
    price.count <- colSums(pt[prices])
    price_df <- data.frame(price.count,prices)
    mod.prices <- price_df$prices[price_df$price.count!=0]

    ### OPTION 1 Winsorization
    if(option == 1) {

      for (price in mod.prices){
        pt2[z_pt[,price]> z_val,price] <- ceiling(z_val*stats::sd(pt2[,price])+
                                                    mean(pt2[,price]))
        pt2[z_pt[,price]< -z_val,price] <- floor(-z_val*stats::sd(pt2[,price])+
                                                   mean(pt2[,price]))
      }

      ### OPTION 2 Winsorization
    } else if(option == 2) {
      for (price in mod.prices){
        pt2[z_pt[,price]> z_val,price] <- max(pt2[z_pt[,price]< z_val,price]) + 1
        pt2[z_pt[,price]< -z_val,price] <- min(pt2[z_pt[,price]> -z_val,price]) - 1
      }

      ### OPTION 3 Winsorization
    } else if(option == 3) {

      for (price in mod.prices){
        above.zval <- unique(z_pt[z_pt[,price]> z_val,price])
        below.neg.zval <- unique(z_pt[z_pt[,price]< -z_val,price])
        if (length(above.zval)>0){
          for (q in seq(1, length(above.zval), by=1)){
            if (length(above.zval)>1){
              quantity.zs <- above.zval[order(above.zval)][q]
            } else if (length(above.zval)==1) {
              quantity.zs <- above.zval[q]
            }
            pt2[z_pt[,price]==quantity.zs,price] <- max(pt2[z_pt[,price]< z_val,price]) + q
          }
        }
        if (length(below.neg.zval)>0){
          for (q in seq(1, length(below.neg.zval), by=1)){
            if (length(below.neg.zval)>1){
              quantity.zs <- below.neg.zval[rev(order(below.neg.zval))][q]
            } else if (length(below.neg.zval)==1) {
              quantity.zs <- below.neg.zval[q]
            }
            pt2[z_pt[,price]==quantity.zs,price] <- min(pt2[z_pt[,price]> -z_val,price]) - q
          }
        }
      }

    }

    ### IDENTIFY IDs with winsorization changes
    pt_winsor <- data.frame(ID=character(),
                            Price=numeric(),
                            Original_Value=integer(),
                            Winsorized_Value=integer())

    i = 1
    for (id_num in pt$id){
      for (price in prices){
        orig = pt[pt$id == id_num,price]
        new = pt2[pt2$id == id_num,price]
        if (orig != new){
          pt_winsor[i,1] <- id_num
          pt_winsor[i,2] <- price
          pt_winsor[i,3] <- orig
          pt_winsor[i,4] <- new
          i = i + 1
        }
      }
    }

    names(pt2)[names(pt2) == "id"] <- id_var

    pt_final <- list(data = as.data.frame(pt2), wins_table = as.data.frame(pt_winsor))


    ##### ----- INDICATOR LEVEL

  } else if(level=="indicator"){

    if(!index_var %in% names(pt)) stop(rlang::format_error_bullets(c( x = c("The indicator variable ('index_var') does not exist within 'pt'."))), call. = FALSE)

    names(pt)[names(pt) == id_var] <- "id"
    pt2 <- pt[!is.na(pt[,c(index_var)]),]
    z_pt <- pt2[c("id",index_var)]
    z_pt[c(index_var)] <- scale(z_pt[c(index_var)], center = TRUE, scale = TRUE)

    ### OPTION 1 Winsorization
    if(option == 1) {

      for (ind in index_var){
        pt2[z_pt[,ind]> z_val,ind] <- ceiling(z_val*stats::sd(pt2[,ind])+
                                                    mean(pt2[,ind]))
        pt2[z_pt[,ind]< -z_val,ind] <- floor(-z_val*stats::sd(pt2[,ind])+
                                                   mean(pt2[,ind]))
      }

      ### OPTION 2 Winsorization
    } else if(option == 2) {
      for (ind in index_var){
        pt2[z_pt[,ind]> z_val,ind] <- max(pt2[z_pt[,ind]< z_val,ind]) + 1
        pt2[z_pt[,ind]< -z_val,ind] <- min(pt2[z_pt[,ind]> -z_val,ind]) - 1
      }

      ### OPTION 3 Winsorization
    } else if(option == 3) {

      for (ind in index_var){
        above.zval <- unique(z_pt[z_pt[,ind]> z_val,ind])
        below.neg.zval <- unique(z_pt[z_pt[,ind]< -z_val,ind])
        if (length(above.zval)>0){
          for (q in seq(1, length(above.zval), by=1)){
            if (length(above.zval)>1){
              quantity.zs <- above.zval[order(above.zval)][q]
            } else if (length(above.zval)==1) {
              quantity.zs <- above.zval[q]
            }
            pt2[z_pt[,ind]==quantity.zs,ind] <- max(pt2[z_pt[,ind]< z_val,ind]) + q*delta
          }
        }
        if (length(below.neg.zval)>0){
          for (q in seq(1, length(below.neg.zval), by=1)){
            if (length(below.neg.zval)>1){
              quantity.zs <- below.neg.zval[rev(order(below.neg.zval))][q]
            } else if (length(below.neg.zval)==1) {
              quantity.zs <- below.neg.zval[q]
            }
            pt2[z_pt[,ind]==quantity.zs,ind] <- min(pt2[z_pt[,ind]> -z_val,ind]) - q*delta
          }
        }
      }

    }

    ### IDENTIFY IDs with winsorization changes
    pt_winsor <- merge(pt[c("id",index_var)],pt2[c("id",index_var)], by = "id", all.x = TRUE)
    colnames(pt_winsor) <- c(id_var,"Old","New")

    pt_winsor <- pt_winsor[!is.na(pt_winsor$Old),]
    pt_winsor <- pt_winsor[(pt_winsor$Old != pt_winsor$New),]

    colnames(pt_winsor) <- c(id_var,paste0("Old_",index_var),paste0("New_", index_var))

    pt_out <- pt2[,c(index_var)]
    pt[,c(index_var)] <- replace(pt[,c(index_var)], !is.na(pt[,c(index_var)]), pt_out)

    names(pt)[names(pt) == "id"] <- id_var

    pt_final <- list(data = as.data.frame(pt), wins_table = as.data.frame(pt_winsor))

  }

  return(pt_final)

}
