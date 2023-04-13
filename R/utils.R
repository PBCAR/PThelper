#' STANDARD ERROR
#'
#' @param x An R object.
#' @export

se <- function(x){sqrt(var(x[!is.na(x)])/length(x[!is.na(x)]))}

#' MAXIMUM VALUES
#'
#' @param x An R object.
#' @export

### replaces max function to surpress warning
maxval <- function(x) {if (length(x)>0) max(x) else Inf}

#' MINIMUM VALUES
#'
#' @param x An R object.
#' @export

### replaces min function to surpress warning
minval <- function(x) {if (length(x)>0) min(x) else Inf}
