% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/winsor_price.R
\name{winsor_price}
\alias{winsor_price}
\title{WINSOR PRICE}
\usage{
winsor_price(pt, id_var, type = "option3", z_val = 3.99, table = FALSE)
}
\arguments{
\item{pt}{A data frame consisting of the `id_var` and purchase task variables.}

\item{id_var}{The name of the unique identifier (ID) as identified in the data frame.}

\item{type}{The type of outlier management via winsorization. One of c("option1","option2","option3").
The default outlier management technique is "option3".}

\item{z_val}{The absolute z-value to identify outlying responses above/ below the negative z-score.}

\item{table}{If set to TRUE, an html table is provided, which is especially helpful for large data
sets. The default is FALSE, and a table in the console will be printed.}
}
\description{
This function is used as outlier management at the price level via winsorization.
Outliers are identified by z-scores higher than the `z_val` or lower than the
negative `z_val`. There are 3 different winsorization types: i) Option 1 replaces
outliers with the maximum non-outlying price rounded up; ii) Option 2 replaces
outliers with a price 1 higher than highest (or 1 lower than the lowest) non-outlying
value; or iii) outlying values are replaced with 1 value above the next highest
non-outlying value to maintain order.
}
\examples{
\dontrun{
pt <- winsor_price(pt, id_var = "ID", type = "option3")
}
}
