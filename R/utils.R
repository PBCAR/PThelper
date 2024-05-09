### calculates standard error
#' `se()`
#'
#' @param x An R object.
#' @noRd

se <- function(x){sqrt(var(x[!is.na(x)])/length(x[!is.na(x)]))}

#' `maxval()`
#'
#' @param x An R object.
#' @noRd

### replaces max function to surpress warning
maxval <- function(x) {if (length(x)>0) max(x) else Inf}

#' `minval()`
#'
#' @param x An R object.
#' @noRd

### replaces min function to surpress warning
minval <- function(x) {if (length(x)>0) min(x) else Inf}

#' `skewness()`
#'
#' @param x An R object.
#' @noRd

### skewness calculation (as replacement for psych::skew)

skewness <- function(x){
  skew <- sum((x - mean(x, na.rm = TRUE))^3, na.rm = TRUE)/(length(x[!is.na(x)]) * stats::sd(x, na.rm = TRUE)^3)
  return(skew)
}

#' `kurtosis()`
#'
#' @param x An R object.
#' @noRd

### kurtosis calculation (as replacement for psych::kurtosi)

kurtosis <- function(x){
  kurt <- sum((x - mean(x, na.rm = TRUE))^4, na.rm = TRUE)/(length(x[!is.na(x)]) * stats::sd(x, na.rm = TRUE)^4)-3
  return(kurt)
}

#' `base_melt()`
#'
#' @param data An R data frame or matrix.
#' @noRd

### Similar to rehape2 function `melt()` to create row combinations of each pairwise correlation

base_melt <- function(data){

  dat_name <- c(colnames(data))

  dat <- data.frame(expand.grid(lapply(dim(data), seq_len)),value = as.vector(as.matrix(data)))
  dat <- dat[!is.na(dat$value),]
  dat$Var1 <- factor(dat$Var1, levels = c(seq(dat_name)), labels = c(dat_name))
  dat$Var2 <- factor(dat$Var2, levels = c(seq(dat_name)), labels = c(dat_name))

  return(dat)
}
