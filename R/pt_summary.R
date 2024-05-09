#' Descriptive statistics
#'
#' This function provides basic summary descriptives of the purchase task indicators of the entire sample or by a grouping variable.
#' See details for full list of summary statistics provided.
#'
#' This function will return the following descriptives: The number of individuals with a non-missing value ('Ntot'), as well as
#' the number of individuals with a missing value ('Nmiss'); The mean average ('Mean') and mean standard error ('SE'); as well as
#' the minimum ('Min') and maximum ('Max') values of the variable(s).
#'
#' @param pt A data frame consisting of the purchase variables `pt_vars` to summarize.
#' @param pt_vars The purchase task variables to summarize.
#' @param group_var An optional grouping variable to summarize the purchase task variables by.
#' @examples
#' ### --- Example Data
#' pt <- data.frame("ID" = c(1:36),
#' "Intensity" = c(10,12,15,0,99,11,7,6,12,7,8,10,5,6,10,0,3,
#'                 7,5,0,2,3,5,6,10,15,12,7,0,9,0,6,7,8,4,5),
#' "Breakpoint" = c(1,2,5,0,10,3,0.5,0.2,0.3,3,4,5,7.5,0.5,2,0,0.1,
#'                  0.5,0.5,0,3,2,2,1,2,3,4,1,0,2,0,5,5,7.5,2,3))
#'
#' ### --- Function Example
#' pt_summary(pt, pt_vars = c("Intensity","Breakpoint"))
#'
#' @return An object of type data frame
#' @export

pt_summary <- function(pt, pt_vars, group_var = NULL){

  if(!is.data.frame(pt)) stop(rlang::format_error_bullets(c( x = c("'pt' must be a data frame."))), call. = FALSE)

  if(is.null(group_var)){

  dat <- data.frame(Variables = c(pt_vars),
                    Ntot = c(as.numeric(apply(pt[c(pt_vars)], 2, function(x) length(x[!is.na(x)])))),
                    Nmiss = c(as.numeric(apply(pt[c(pt_vars)], 2, function(x) length(x[is.na(x)])))),
                    Mean = c(as.numeric(apply(pt[c(pt_vars)], 2, function(x) mean(x, na.rm = TRUE)))),
                    SE = c(as.numeric(apply(pt[c(pt_vars)], 2, function(x) se(x)))),
                    Min = c(as.numeric(apply(pt[c(pt_vars)], 2, function(x) min(x, na.rm = TRUE)))),
                    Max = c(as.numeric(apply(pt[c(pt_vars)], 2, function(x) max(x, na.rm = TRUE)))))
}

if(!is.null(group_var)){

  names(pt)[names(pt) == group_var] <- "group"

  dat_ntot <- stats::aggregate(pt[c(pt_vars)], by = list(group = pt[,"group"]), function(x) length(x[!is.na(x)]))
  dat_nmiss <- stats::aggregate(pt[c(pt_vars)], by = list(group = pt[,"group"]), function(x) length(x[is.na(x)]))
  dat_mean <- stats::aggregate(pt[c(pt_vars)], by = list(group = pt[,"group"]), function(x) mean(x, na.rm = TRUE))
  dat_se <- stats::aggregate(pt[c(pt_vars)], by = list(group = pt[,"group"]), se)
  dat_min <- stats::aggregate(pt[c(pt_vars)], by = list(group = pt[,"group"]), function(x) min(x, na.rm = TRUE))
  dat_max <- stats::aggregate(pt[c(pt_vars)], by = list(group = pt[,"group"]), function(x) max(x, na.rm = TRUE))

  dat_ntot$group <- paste0("Ntot_",dat_mean$group)
  dat_nmiss$group <- paste0("Nmiss_",dat_mean$group)
  dat_mean$group <- paste0("Mean_",dat_mean$group)
  dat_se$group <- paste0("SE_",dat_se$group)
  dat_min$group <- paste0("Min_",dat_min$group)
  dat_max$group <- paste0("Max_",dat_max$group)

  dat2 <- as.data.frame(t(rbind(dat_ntot,dat_nmiss,dat_mean,dat_se,dat_min,dat_max)))
  colnames(dat2) <- c(dat2["group",])
  dat2$Variables <- rownames(dat2)

  group_names <- colnames(dat2)[(colnames(dat2)!="Variables")]

  dat <- stats::reshape(as.data.frame(dat2[(dat2$Variables!="group"),]),
                  idvar = "Variables", timevar = "Group", varying = c(group_names), sep = "_", direction = "long")

  dat <- dat[order(dat$Variables),]
  dat[, c("Ntot","Nmiss","Mean","SE","Min","Max")] <- sapply(dat[, c("Ntot","Nmiss","Mean","SE","Min","Max")], as.numeric)

  }

  return(dat)
}
