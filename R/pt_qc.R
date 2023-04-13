#' PT QC
#'
#' This function helps users to conduct quality control on purchase task data to remove non-systematic data. Specifically, this function
#' identifies and removes IDs with: i) trend violations; ii) excessive bounce ratios; and iii) excessive reversals in responses. Quality
#' control options follow the proposed 3-criterion method by Stein et al. (2015), but also allow for customization. Additionally, quality
#' control can be applied to purchase task data in which not all prices were administered, such as in the case that administration of the
#' purchase task ceased after first breakpoint, or after zero consumption was reached within a price array.
#'
#' Stein, J. S., Koffarnus, M. N., Snider, S. E., Quisenberry, A. J., & Bickel, W. K. (2015).
#' Identification and management of nonsystematic purchase task data: Toward best practice.
#' Experimental and clinical psychopharmacology, 23(5), 377.
#'
#' @param pt A data frame consisting of the `id_var` and purchase task variables.
#' @param id_var The name of the unique identifier (ID) as identified in the data frame.
#' @param type The type of QC to apply, one of c("full","partial"). The "full" type of QC processing is used to process PT data which
#' administers all price points, regardless of consumption responses. The "partial" type of QC processing is used to process PT data
#' which uses consumption responses to determine when to end administration, either at the first instance when consumption reaches zero,
#' or zero consumption at the end of a price array. Additionally, for type "partial", zero responses will be added to items not
#' administered.
#' @param delta_q The log10-unit reduction in consumption from the first to last price. The default is set to 0.025 (suggested by Stein
#' et al., 2015), with values lower than 0.025 deemed non-systematic. If there is zero consumption at intensity (lowest price), participants
#' are not excluded, and instead will be given a delta Q value of NA.
#' @param bounce_type The type of bounce criterion applied, one of c("initial","p2p"). The default is "initial", which follows and
#' defines what constitutes a `jump`, with all subsequent prices in the purchase task being compared to the consumption at the first
#' price. The "p2p" option is a proposed alternative, which considers any increases in consumption from one price to the next to be a
#'  `jump` in consumption. Both methods make use of the `bounce_val` which will only exclude participants above a set threshold of jumps.
#' @param jump Expressed as a proportion, the jump value is the percent increase in consumption at first price used to define an
#' excessive increase in consumption. The default is 0.25 (25% as suggested by Stein et al., 2015), meaning that any consumption 25%
#' higher than consumption at first price would be considered an excessive jump in consumption when using the `bounce_type` "initial".
#' @param bounce_val Expressed as a proportion, the bounce value is used as a threshold to identify excessive inconsistencies in
#' responses. The default bounce value is 0.1. IDs exceeding this bounce value are removed.
#' @param rev_n The number of acceptable reversals from zero, one of c(0,1). The default number is 0, meaning no reversals from zero
#' are allowed.
#' @param cons_0 The number of consecutive zeroes to signify a reversal from zero, one of c(1,2). The default is 1.
#' @return A list consisting of two data frames: "data" which consists of the `id_var` and purchase task variables, and the
#' "qc_data" which provides details on the results of the quality control for all IDs.
#' @examples
#' ### ---------- PT PREP:
#' cpt_data <- price_prep(cpt_data, id_var = "ID", vars = c(paste0("cpt",1:15)),
#' price = c("0","0.05","0.10","0.20","0.30","0.40","0.50","0.75","1","2","3","4","5","7.5","10"))
#'
#' cpt_data <- pt_prep(cpt_data, id_var = "ID", partial = TRUE)
#'
#' ### ---------- PT QC:
#' PT <- pt_qc(cpt_data, id_var = "ID", type = "partial")
#' @export

pt_qc <- function(pt, id_var, type, delta_q = 0.025, bounce_type = "initial", jump = 0.25, bounce_val = 0.1, rev_n = 0, cons_0 = 1){

  pt_names <- names(pt)
  prices <- pt_names[pt_names!=id_var]
  names(pt)[names(pt) == id_var] <- "id"

  remove.id.trend = {}
  remove.id.bounce <- {}
  remove.id.reversal <- {}

  ########## ----- "FULL" QC (Trend & Bounce Ratio)

  if(type=="full"){

    ### WARNING: NA values should have been changed to 0 as outlined in the `pt_prep()` function
    if(any(is.na(pt))) stop("IDs with missing values")

    ### --- IDENTIFY & REMOVE IDs with a trend violation

    pt$dq_1 <- as.numeric(pt[,prices[1]])+0.001 ### quantity at price 1
    pt$dq_n <- as.numeric(pt[,prices[length(prices)]])+0.001 ### quantity at price n
    pt$dp_1 <- as.numeric(prices[1])+0.001 ### price 1 (intensity)
    pt$dp_n <- as.numeric(prices[length(prices)])+0.001 ## price n

    ### FORMULA: FORMULA: deltaQ = (log10(quantity at price 1) - log10(quantity at price n)) / (log10(price n) - log10(price 1))
    ### allow those with zero consumption at price 1 to pass (by making delta_q the value considered systematic)

    pt$delta_q <- (log10(pt$dq_1)-log10(pt$dq_n))/(log10(pt$dp_n)-log10(pt$dp_1))
    pt$delta_q <- ifelse(pt[,prices[1]]==0,delta_q,pt$delta_q)

    remove.id.trend <- pt$id[pt$delta_q<delta_q]

    pt$delta_q <- ifelse(pt[,prices[1]]==0,NA,pt$delta_q) ### Set delta Q to NA for those with zero consumption at intensity

    ### --- IDENTIFY & REMOVE IDs with a bounce violation

    if(bounce_type=="initial"){

      pt$first_cons <- pt[,prices[1]]*(1+jump)
      pt$jumps <-  rowSums(pt[c(prices)] > pt$first_cons)

      pt$bounce_val <- pt$jumps/(length(prices)-1)

      remove.id.bounce <- pt$id[pt$bounce_val> bounce_val]
    }

    if(bounce_type == "p2p"){

      for (id_num in pt$id){
        num.jumps <- 0
        for (j in seq(1,length(prices)-1,1)){
          if (pt[pt$id == id_num,prices[j]] < pt[pt$id == id_num,prices[j+1]]){
            num.jumps <- num.jumps + 1
          }
        }
        pt$jumps[pt$id == id_num] <- num.jumps
      }
    }

    pt$end_price <- apply(pt[,prices], 1, function(x) {names(x)[maxval(which(!is.na(x)))]})
    pt$end_cons <- match(pt$end_price,prices)
    pt$bounce_val <- pt$jumps/(pt$end_cons-1)
    remove.id.bounce <- pt$id[pt$bounce_val> bounce_val]

  }

  ########## ----- "PARTIAL" QC (Trend & Bounce Ratio)

  if(type=="partial"){

    ### --- IDENTIFY & REMOVE IDs with a trend violation

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
    pt$delta_q <- ifelse(pt[,prices[1]]==0,delta_q,pt$delta_q)

    remove.id.trend <- pt$id[pt$delta_q<delta_q]

    ### --- IDENTIFY & REMOVE IDs with a bounce violation

    if(bounce_type=="initial"){

      pt$first_cons <- pt[,prices[1]]*(1+jump)
      pt$jumps <- rowSums(!is.na(pt[c(prices)]) & pt[c(prices)] > pt$first_cons)

      pt$bounce_val <- pt$jumps/(pt$end_cons-1)

      remove.id.bounce <- pt$id[pt$bounce_val> bounce_val]
    }

    ### --- ASSIGN zero values
    ### For bounce violations ("p2p"), denominator will not be effected by presence of imputed zeroes (variable `end_cons` captures denominator above)
    ### For reversals, imputation of zeroes after first breakpoint will not affect formula to detect reversals

    for (id_num in pt$id){
      if (pt[prices][pt[,"id"]==id_num,][max(which(!is.na(pt[c(prices)][pt[,"id"]==id_num,])))] == 0){
        pt[prices][pt[,"id"]==id_num,][is.na(pt[c(prices)][pt[,"id"]==id_num,])] <- 0
      }
    }

    if(bounce_type == "p2p"){

      for (id_num in pt$id){
        num.jumps <- 0
        for (j in seq(1,length(prices)-1,1)){
          if (pt[pt$id == id_num,prices[j]] < pt[pt$id == id_num,prices[j+1]]){
            num.jumps <- num.jumps + 1
          }
        }
        pt$jumps[pt$id == id_num] <- num.jumps
      }

    }

    pt$bounce_val <- pt$jumps/(pt$end_cons-1)
    remove.id.bounce <- pt$id[pt$bounce_val> bounce_val]

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

  qc <- pt[c("id","delta_q","bounce_val","reversals")]

  remove.id <- c(remove.id.trend,remove.id.bounce,remove.id.reversal)

  pt <- pt[c("id",prices)]
  pt <- pt[(!pt$id %in% remove.id),]
  names(pt)[names(pt) == "id"] <- id_var

  pt_final <- list(data = as.data.frame(pt), qc_data = as.data.frame(qc))

  if(length(remove.id.trend)==0) (remove.id.trend <- "NULL")
  if(length(remove.id.bounce)==0) (remove.id.bounce <- "NULL")
  if(length(remove.id.reversal)==0) (remove.id.reversal <- "NULL")

  cat(" IDs with a trend violation: ",remove.id.trend,
      "\n","IDs with a bounce violation: ", remove.id.bounce,
      "\n","IDs with a reversal violation: ", remove.id.reversal,"\n")

  return(pt_final)
}

