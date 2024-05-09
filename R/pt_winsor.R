#' Winsorization of outliers
#'
#' This function helps users to manage outliers at the indicator level by using winsorization techniques. See details for the different options.
#'
#' There are 3 winsorization options:
#'
#' i) Option 1 replaces all outliers with the theoretical value associated at the defined z-score threshold;
#'
#' ii) Option 2 replaces all outliers with the observed minimum/ maximum non-outlying value plus (or minus) a small constant (delta);
#'
#' iii) Option 3 replaces each outlier with the observed minimum/ maximum non-outlying value plus a small constant (delta) to retain order.
#'
#' @param pt A data frame consisting of the `id_var` and relevant purchase task variables.
#' @param id_var The name of the unique identifier (ID) as identified in the data frame.
#' @param w_var The name of the variable to winsorize.
#' @param z_val The absolute z-score value to define outliers. The default is |3.99|, which will remove those exceeding a z-score of |3.99|.
#' @param option The winsorization option, one of c(1,2,3). The default outlier management technique is option 3.
#' @param delta The constant used in winsorization options 2 and 3. The delta must be defined by the user, as
#' the optimal value will vary depending on the indicator. For elasticity, a small value of 0.001 is recommended.
#' @examples
#' ### --- Example Data
#' pt <- data.frame("ID" = c(1:36),
#' "Intensity" = c(10,12,15,0,99,11,7,6,12,7,8,10,5,6,10,0,3,
#'                 7,5,0,2,3,5,6,10,15,12,7,0,9,0,6,7,8,4,5))
#'
#' ### --- Function Example
#' pt2 <- pt_winsor(pt, id_var = "ID", w_var = "Intensity", delta = 1)
#'
#' @return A list consisting of two data frames: "data" which consists of the `id_var` and `pt` including the winsorized value(s); and
#' "wins_table" which provides details on which value(s) by `id_var` were winsorized (values before and after provided).
#' @export

pt_winsor <- function(pt, id_var, z_val = 3.99, option = 3, w_var = NULL, delta = NULL) {

  if(is.null(delta) & (option==2 | option==3)) stop(rlang::format_error_bullets(c( "!" = c("Delta value required for this winsorization option."))), call. = FALSE)
  if(!is.null(delta) & (option==1)) stop(rlang::format_error_bullets(c( "!" = c("Delta value not used in this winsorization option."))), call. = FALSE)
  if(is.null(w_var)) stop(rlang::format_error_bullets(c( "!" = c("Indicator variable ('w_var') must be defined."))), call. = FALSE)
  if(!is.data.frame(pt)) stop(rlang::format_error_bullets(c( x = c("'pt' must be a data frame."))), call. = FALSE)

  if(!w_var %in% names(pt)) stop(rlang::format_error_bullets(c( x = c("The indicator variable ('w_var') does not exist within 'pt'."))), call. = FALSE)

  pt_names <- names(pt)

    names(pt)[names(pt) == id_var] <- "id"
    pt2 <- pt[!is.na(pt[,c(w_var)]),]
    z_pt <- pt2[c("id",w_var)]
    z_pt[c(w_var)] <- scale(z_pt[c(w_var)], center = TRUE, scale = TRUE)

    ### OPTION 1 Winsorization
    if(option == 1) {

        pt2[z_pt[,w_var]> z_val,w_var] <- ceiling(z_val*stats::sd(pt2[,w_var])+
                                                    mean(pt2[,w_var]))
        pt2[z_pt[,w_var]< -z_val,w_var] <- floor(-z_val*stats::sd(pt2[,w_var])+
                                                   mean(pt2[,w_var]))

      ### OPTION 2 Winsorization
    } else if(option == 2) {
        pt2[z_pt[,w_var]> z_val,w_var] <- max(pt2[z_pt[,w_var]< z_val,w_var]) + delta
        pt2[z_pt[,w_var]< -z_val,w_var] <- min(pt2[z_pt[,w_var]> -z_val,w_var]) - delta

      ### OPTION 3 Winsorization
    } else if(option == 3) {

        above.zval <- unique(z_pt[z_pt[,w_var]> z_val,w_var])
        below.neg.zval <- unique(z_pt[z_pt[,w_var]< -z_val,w_var])
        if (length(above.zval)>0){
          for (q in seq(1, length(above.zval), by=1)){
            if (length(above.zval)>1){
              quantity.zs <- above.zval[order(above.zval)][q]
            } else if (length(above.zval)==1) {
              quantity.zs <- above.zval[q]
            }
            pt2[z_pt[,w_var]==quantity.zs,w_var] <- max(pt2[z_pt[,w_var]< z_val,w_var]) + q*delta
          }
        }
        if (length(below.neg.zval)>0){
          for (q in seq(1, length(below.neg.zval), by=1)){
            if (length(below.neg.zval)>1){
              quantity.zs <- below.neg.zval[rev(order(below.neg.zval))][q]
            } else if (length(below.neg.zval)==1) {
              quantity.zs <- below.neg.zval[q]
            }
            pt2[z_pt[,w_var]==quantity.zs,w_var] <- min(pt2[z_pt[,w_var]> -z_val,w_var]) - q*delta
          }
        }

    }

    ### IDENTIFY IDs with winsorization changes
    pt_winsor <- merge(pt[c("id",w_var)],pt2[c("id",w_var)], by = "id", all.x = TRUE)
    colnames(pt_winsor) <- c(id_var,"Old","New")

    pt_winsor <- pt_winsor[!is.na(pt_winsor$Old),]
    pt_winsor <- pt_winsor[(pt_winsor$Old != pt_winsor$New),]

    colnames(pt_winsor) <- c(id_var,paste0("Old_",w_var),paste0("New_", w_var))

    pt_out <- pt2[,c(w_var)]
    pt[,c(w_var)] <- replace(pt[,c(w_var)], !is.na(pt[,c(w_var)]), pt_out)

    names(pt)[names(pt) == "id"] <- id_var

    pt_final <- list(data = as.data.frame(pt), wins_table = as.data.frame(pt_winsor))

  return(pt_final)

}
