#' 4-Criterion QC
#'
#' This function helps users to conduct quality control on purchase task data to remove non-systematic data. See details for specific QC criteria.
#'
#' Quality control options follow the proposed 4-criterion method which is based on the Stein et al. (2015) 3-criterion method, and removes IDs with:
#'
#' i) trend violations;
#'
#' ii) excessive global bounce ratios;
#'
#' iii) excessive local bounce ratios; and
#'
#' iv) excessive reversals in responses.
#'
#' However, these criteria can also be customized, accommodating differences of purchase tasks or sub-populations. Additionally, quality
#' control can be applied to partially-administered purchase tasks, such as in the case that administration of the purchase task ceased after
#' breakpoint, or after zero consumption was reached within a price array.
#'
#' Trend violations are identified by delta-Q values lower than the set threshold. Delta-Q is defined as the log-unit reduction in consumption from
#' the first to last price. However, if it is decided (in the `pt_prep()` step) that those with zero consumption across the purchase task are
#' acceptable, then these participants would have a delta-Q of NA.
#'
#' In general, bounces in consumption refer to the proportion of responses in which estimated consumption at a higher price exceeds the estimated
#' consumption at a lower price. However, there are two methods of identifying excessive bounce ratios, each of which serve a different purpose in
#' identifying systematic data: a) global increases in consumption at any price compared to consumption at the first price (intensity); and b) local
#' increases in consumption at price P relative to the consumption at the price point preceding price P (i.e. P - 1). As consumption values exceeding
#' the global bounce threshold impact both empirical and derived demand indicators, consumption exceeding the set global bounce value (defined by the
#' `jump` argument) are re-assigned to this maximum allowed value.
#'
#' Excessive reversals in consumption are defined by whether 0 or 1 reversals in consumption are allowed, and whether a reversal
#' consists of 1 or 2 consecutive responses of 0 consumption. Outside of this definition, any additional zero values followed by
#' non-zero consumption are considered non-systematic. For example, in instances of 2 consecutive zero responses defined as a
#' reversal, an additional single zero value elsewhere would not pass the reversals portion of quality control.
#'
#' Stein, J. S., Koffarnus, M. N., Snider, S. E., Quisenberry, A. J., & Bickel, W. K. (2015).
#' Identification and management of nonsystematic purchase task data: Toward best practice.
#' Experimental and clinical psychopharmacology, 23(5), 377.
#'
#' @param pt A data frame consisting of the `id_var` and purchase task variables.
#' @param id_var The name of the unique identifier (ID) as identified in the data frame.
#' @param type The type of quality control (QC) to apply, one of c("full","partial"). The "full" type of QC processing is used to process purchase task data which
#' administers all price points, regardless of consumption responses. The "partial" type of QC processing is used to process purchase task data which uses
#' consumption responses to determine when to end administration, either at the first instance when consumption reaches zero, or at the end of a price array when
#' zero consumption is reached. Additionally, for type "partial", zero responses will be added to items not administered.
#' @param delta_q The log reduction in consumption from the first to last price. The default is set to 0.025 (suggested by Stein
#' et al., 2015), with values lower than 0.025 deemed non-systematic. If there is zero consumption across all price points, participants
#' are not excluded, and instead will be given a delta-Q value of NA.
#' @param bounce_g Expressed as a proportion, the global bounce value is used as a threshold to identify excessive inconsistencies in consumption at all
#' subsequent prices following the first price point (P1). The default global bounce value is 0.05, and IDs exceeding this global bounce value are deemed non-systematic.
#' @param bounce_l Expressed as a proportion, the local bounce value is used as a threshold to identify excessive inconsistencies in consumption at price P relative to
#' price P - 1. A local bounce threshold is defined as instances where consumption at price P is > consumption at price P - 1.  The default local bounce value is 0.1,
#' and IDs exceeding this local bounce value are deemed non-systematic.
#' @param jump Expressed as a proportion, the jump value is the percent increase in consumption used to define a global bounce. The default is 0.25 (25\% as suggested by Stein et al., 2015), with
#' consumption 25\% higher than consumption at first price considered an excessive jump in consumption.
#' @param rev_n The number of acceptable reversals from zero, one of c(0,1). The default number is 0, meaning any reversals from zero are deemed non-systematic.
#' @param cons_0 The number of consecutive zeroes to signify a reversal from zero, one of c(1,2). The default is 1.
#' @examples
#' ### --- Load Data
#' data("cpt_data")
#'
#' ### --- Prep Data
#' pt <- price_prep(cpt_data, id_var = "ID", vars = c(paste0("cpt",1:15)),
#' prices = c("0","0.05","0.10","0.20","0.30","0.40","0.50", "0.75","1","2","3","4","5","7.5","10"))
#'
#' pt2 <- pt_prep(pt, id_var = "ID", type = "partial", remove0 = TRUE, max_val = 99)
#'
#' ### --- Function Example
#' pt3 <- pt_qc(pt2, id_var = "ID", type = "partial")
#'
#' @return A list consisting of two data frames: "data" which consists of the `id_var` and purchase task variables, and
#' "qc_data" which provides details on the results of quality control for all IDs.
#' @export

pt_qc <- function(pt, id_var, type = NULL, delta_q = 0.025, bounce_g = 0.05, bounce_l = 0.1, jump = 0.25, rev_n = 0, cons_0 = 1){

  if(is.null(type)) stop(rlang::format_error_bullets(c( "!" = c("Type required. Please select either 'partial' or 'full' using the 'type' argument."))), call. = FALSE)
  if(!is.data.frame(pt)) stop(rlang::format_error_bullets(c( x = c("'pt' must be a data frame."))), call. = FALSE)
  if(!rev_n %in% c(0,1)) stop(rlang::format_error_bullets(c( x = c("The number of acceptable reversals (`rev_n`) must be either 0 or 1."))), call. = FALSE)
  if(!cons_0 %in% c(1,2)) stop(rlang::format_error_bullets(c( x = c("The number of consecutive zeroes (`cons_0`) must be either 1 or 2."))), call. = FALSE)

  prices <- names(pt)[names(pt)!=id_var]
  names(pt)[names(pt) == id_var] <- "id"

  remove.id.trend <- {}
  remove.id.bounce.global <- {}
  remove.id.bounce.local <- {}
  remove.id.reversal <- {}

  ##### ----- "FULL" QC (Trend & Bounce Ratio)

  if(type=="full"){

    ### WARNING: NA values should have been changed to 0 as outlined in the `pt_prep()` function
    if(any(is.na(pt))) stop(rlang::format_error_bullets(c("!" = c("IDs with missing values"))), call. = FALSE)

    ### --- IDENTIFY & REMOVE IDs with a trend violation

    pt$price_0 <- rowSums(pt[c(prices)]==0)
    pt$dq_1 <- as.numeric(pt[,prices[1]])+0.001 ### quantity at price 1
    pt$dq_n <- as.numeric(pt[,prices[length(prices)]])+0.001 ### quantity at price n
    pt$dp_1 <- as.numeric(prices[1])+0.001 ### price 1 (intensity)
    pt$dp_n <- as.numeric(prices[length(prices)])+0.001 ## price n

    ### FORMULA: FORMULA: deltaQ = (log10(quantity at price 1) - log10(quantity at price n)) / (log10(price n) - log10(price 1))
    ### allow those with zero consumption at price 1 to pass (by making delta_q the value considered systematic)

    pt$end_price <- apply(pt[,prices], 1, function(x) {names(x)[maxval(which(!is.na(x)))]})
    pt$end_cons <- match(pt$end_price,prices)

    pt$delta_q <- (log10(pt$dq_1)-log10(pt$dq_n))/(log10(pt$dp_n)-log10(pt$dp_1))
    pt$delta_q <- ifelse(pt$price_0==pt$end_cons,delta_q,pt$delta_q)

    remove.id.trend <- pt$id[pt$delta_q<delta_q]

    pt$delta_q <- ifelse(pt$price_0==pt$end_cons,NA,pt$delta_q) ### Set delta Q to NA for those with zero consumption at intensity

    pt$first_cons <- pt[,prices[1]]*(1+jump)
    pt$jumps_global <-  rowSums(pt[c(prices)] > pt$first_cons)

    pt$bounce_global_val <- pt$jumps_global/(length(prices)-1)

    remove.id.bounce.global <- pt$id[pt$bounce_global_val> bounce_g]

    for (id_num in pt$id){
      num.jumps <- 0
      for (j in seq(1,length(prices)-1,1)){
        if (pt[pt$id == id_num,prices[j+1]] > pt[pt$id == id_num,prices[j]]){
          num.jumps <- num.jumps + 1
        }
      }
      pt$jumps_local[pt$id == id_num] <- num.jumps
    }

    pt$bounce_local_val <- pt$jumps_local/(pt$end_cons-1)
    remove.id.bounce.local <- pt$id[pt$bounce_local_val> bounce_l]

    ##### ----- "PARTIAL" QC (Trend & Bounce Ratio)

  } else if(type=="partial"){

    ### --- IDENTIFY & REMOVE IDs with a trend violation

    pt$price_0 <- rowSums(pt[c(prices)]==0, na.rm = T)
    pt$end_price <- apply(pt[,prices], 1, function(x) {names(x)[maxval(which(!is.na(x)))]})
    pt$end_cons <- match(pt$end_price,prices)
    pt$end_cons_val <- sapply(seq_along(pt[,"id"]), function(x) {pt[,prices][x, pt$end_cons[x]]})

    ### USE BASED ON END VALUES
    pt$dq_1 <- as.numeric(pt[,prices[1]])+0.001 ### quantity at price 1
    pt$dq_n <- as.numeric(pt$end_cons_val)+0.001 ### quantity at price n
    pt$dp_1 <- as.numeric(prices[1])+0.001 ### price 1 (intensity)
    pt$dp_n <- as.numeric(pt$end_price)+0.001 ## price n

    ### allow those with zero consumption at price 1 to pass

    pt$delta_q <- (log10(pt$dq_1)-log10(pt$dq_n))/(log10(pt$dp_n)-log10(pt$dp_1))
    pt$delta_q <- ifelse(pt$price_0==pt$end_cons,delta_q,pt$delta_q)

    remove.id.trend <- pt$id[pt$delta_q<delta_q]

    pt$delta_q <- ifelse(pt$price_0==pt$end_cons,NA,pt$delta_q) ### Set delta Q to NA for those with zero consumption at intensity

    pt$first_cons <- pt[,prices[1]]*(1+jump)
    pt$jumps_global <-  rowSums(!is.na(pt[c(prices)]) & pt[c(prices)] > pt$first_cons)

    pt$bounce_global_val <- pt$jumps_global/(pt$end_cons-1)

    remove.id.bounce.global <- pt$id[pt$bounce_global_val> bounce_g]

    ### --- ASSIGN zero values
    ### For bounce violations ("local"), denominator will not be effected by presence of imputed zeroes (variable `end_cons` captures denominator above)
    ### For reversals, imputation of zeroes after first breakpoint will not affect formula to detect reversals

    for (id_num in pt$id){
      if (pt[prices][pt[,"id"]==id_num,][max(which(!is.na(pt[c(prices)][pt[,"id"]==id_num,])))] == 0){
        pt[prices][pt[,"id"]==id_num,][is.na(pt[c(prices)][pt[,"id"]==id_num,])] <- 0
      }
    }

    for (id_num in pt$id){
      num.jumps <- 0
      for (j in seq(1,length(prices)-1,1)){
        if (pt[pt$id == id_num,prices[j+1]] > pt[pt$id == id_num,prices[j]]){
          num.jumps <- num.jumps + 1
        }
      }
      pt$jumps_local[pt$id == id_num] <- num.jumps
    }

    pt$bounce_local_val <- pt$jumps_local/(pt$end_cons-1)
    remove.id.bounce.local <- pt$id[pt$bounce_local_val> bounce_l]

  }

  ### --- REASSIGN GLOBAL BOUNCE VALUES

  for (id_num in pt$id){
    pt[c(prices)][pt[,"id"]==id_num,][pt[c(prices)][pt[,"id"]==id_num,] > pt$first_cons[pt$id==id_num]] <- pt$first_cons[pt$id==id_num]
  }

  ### --- REMOVE IDS WITH REVERSALS

  ### calculate number of zero-consumption responses present for each participant
  pt$zero_count <- rowSums(pt[c(prices)]==0, na.rm = T)

  ### get breakpoint price
  pt$bp <- apply(pt[ ,prices], 1, function(x) {names(x)[minval(which(x == 0))] })

  ### get number of price items at and after bp
  ### and determine if non-zero values exist between breakpoint and end price

  pt$bp_items <- sapply(pt$bp, function(x) {sum(as.numeric(prices)>=as.numeric(x), na.rm = T)})
  pt$postbp_val <- sapply(seq_along(pt[,"id"]), function(x) {pt[,prices][x, prices[(match(pt$bp,prices)+1)][x]]})

  pt$postbp_val[as.numeric(pt$bp)==max(as.numeric(prices)) | is.na(pt$bp)] <- "NO BP"
  pt$postbp_val <- unlist(as.character(pt$postbp_val))
  pt$postbp_val[pt$postbp_val=="NO BP"] <- NA
  pt$postbp_val <- as.numeric(pt$postbp_val)

  ## need to find the number of zeroes that exist after the first breakpoint (or minus the breakpoint)
  ## this plus breakpoint will determine if there are additional reversals

  pt$final0 <- apply(pt[,prices], 1, function(x) sum(cumsum(rev(x)) == 0))

  ### THREE OPTIONS FOR REVERSALS FROM ZERO:

  ### i) No reversals allowed:
  ### zero_count =  bp_items

  if(rev_n==0){
    pt$reversals <- ifelse(pt$zero_count==pt$bp_items,FALSE,TRUE)
  }

  ### ii) 1 reversal of a single 0 allowed:
  ### zero_count = final0 + 1

  if(rev_n==1 & cons_0==1){
    pt$reversals <- ifelse(pt$zero_count==pt$final0,FALSE,
                           ifelse(pt$zero_count==pt$final0+1,FALSE,TRUE))
  }
  ### iii) 1 reversal of two consecutive 0s allowed:
  ### zero_count = final0 + 2 (distinct from 2 separate reversals of one 0)

  if(rev_n==1 & cons_0==2){
    pt$reversals <- ifelse(pt$zero_count==pt$final0,FALSE,
                           ifelse(pt$zero_count==pt$final0+1,FALSE,
                                  ifelse(pt$zero_count==pt$final0+2 & !is.na(pt$postbp_val) & pt$postbp_val==0,FALSE,TRUE)))
  }

  remove.id.reversal <- pt$id[pt$reversals==TRUE]

  qc <- pt[c("id","delta_q","bounce_global_val","bounce_local_val","reversals")]

  remove.id <- c(remove.id.trend,remove.id.bounce.global,remove.id.bounce.local,remove.id.reversal)

  pt <- pt[c("id",prices)]
  pt <- pt[(!pt$id %in% remove.id),]
  names(pt)[names(pt) == "id"] <- id_var
  names(qc)[names(qc) == "id"] <- id_var

  pt_final <- list(data = as.data.frame(pt), qc_data = as.data.frame(qc))

  if(length(remove.id.trend)==0) (remove.id.trend <- "NULL")
  if(length(remove.id.bounce.global)==0) (remove.id.bounce.global <- "NULL")
  if(length(remove.id.bounce.local)==0) (remove.id.bounce.local <- "NULL")
  if(length(remove.id.reversal)==0) (remove.id.reversal <- "NULL")

  message(rlang::format_error_bullets(c(i = c("IDs with a trend violation:"),
                                        " " = c(paste(remove.id.trend, collapse = ",")),
                                        i = c("IDs with a global bounce violation:"),
                                        " " = c(paste(remove.id.bounce.global, collapse = ",")),
                                        i = c("IDs with a local bounce violation:"),
                                        " " = c(paste(remove.id.bounce.local, collapse = ",")),
                                        i = c("IDs with a reversal violation:"),
                                        " " = c(paste(remove.id.reversal, collapse = ",")))))

  return(pt_final)
}

