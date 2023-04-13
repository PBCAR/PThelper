#' PT SUMMARY
#'
#' This function provides basic summary descriptives of the variables. This function can also provide summary by a grouping variable.
#'
#' @param pt A data frame consisting of the purchase variables `pt_vars` to summarize.
#' @param pt_vars The purchase task variables to summarize.
#' @param group An optional grouping variable to summarize the purchase task variables by.
#' @return An object of type data frame
#' @examples
#' ### ---------- Data Prep:
#' cpt_data$AB <- ifelse(grepl("A",cpt_data$ID),"A","B")
#'
#' ### --------- Descriptive Stats by Group
#' data_summ <- pt_summary(cpt_data, pt_vars = c("cpt1","cpt2","cpt3"), group = "AB")
#' @export

pt_summary <- function(pt, pt_vars, group = NULL){

  if(is.null(group)){

  dat <- data.frame(Name = c(pt_vars),
                    Mean = c(as.numeric(apply(pt[c(pt_vars)], 2, function(x) mean(x, na.rm = T)))),
                    SE = c(as.numeric(apply(pt[c(pt_vars)], 2, function(x) se(x)))),
                    Min = c(as.numeric(apply(pt[c(pt_vars)], 2, function(x) min(x, na.rm = T)))),
                    Max = c(as.numeric(apply(pt[c(pt_vars)], 2, function(x) max(x, na.rm = T)))))
}

if(!is.null(group)){

  dat_mean <- stats::aggregate(pt[c(pt_vars)], by = list(Group = pt[,group]), function(x) mean(x, na.rm = T))
  dat_se <- stats::aggregate(pt[c(pt_vars)], by = list(Group = pt[,group]), se)
  dat_min <- stats::aggregate(pt[c(pt_vars)], by = list(Group = pt[,group]), function(x) min(x, na.rm = T))
  dat_max <- stats::aggregate(pt[c(pt_vars)], by = list(Group = pt[,group]), function(x) max(x, na.rm = T))

  dat_mean$Group <- paste0("Mean_",dat_mean$Group)
  dat_se$Group <- paste0("SE_",dat_se$Group)
  dat_min$Group <- paste0("Min_",dat_min$Group)
  dat_max$Group <- paste0("Max_",dat_max$Group)

  dat2 <- as.data.frame(t(rbind(dat_mean,dat_se,dat_min,dat_max)))
  colnames(dat2) <- c(dat2["Group",])
  dat2$vars <- rownames(dat2)

  group_names <- colnames(dat2)[(colnames(dat2)!="vars")]

  dat <- stats::reshape(as.data.frame(dat2[(dat2$vars!="Group"),]),
                  idvar = "vars", varying = c(group_names), sep = "_", direction = "long")

  dat <- dat[order(dat$vars),]

  }

  return(dat)
}
