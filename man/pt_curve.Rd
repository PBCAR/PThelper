% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pt_curve.R
\name{pt_curve}
\alias{pt_curve}
\title{Exponentiated demand curve fitting}
\usage{
pt_curve(pt, id_var, type = NULL, k = NULL, group_var = NULL, n_starts = 10)
}
\arguments{
\item{pt}{A data frame consisting of the `id_var` and purchase task variables.}

\item{id_var}{The name of the unique identifier as identified in the data frame.}

\item{type}{The level for fitting the demand curves, one of c("overall","group","individual"). The default is "overall" which will calculate
overall demand for the entire data frame. For `type` "group", equation-derived demand indicators for each of the groups as identified by the `group_var`
are visualized. When `type` equals "individual", equation-derived demand indicators are calculated for each individual as identified by `id_var`.}

\item{k}{The k-value to use in the exponentiated demand equation. The default is NULL, in which the k-value is calculated from the entire sample,
with k representing the span of consumption in log units. Otherwise, a single numerical value can be given as the k-value,
allowing for comparisons across studies, since k influences the calculation of alpha. When k is calculated from the entire sample,
it is calculated using the mean average consumption at the lowest and highest price points and in instances when the mean average consumption
at the highest price point is 0, the lowest non-zero mean consumption is used.}

\item{group_var}{The name of the grouping variable when `type` equals "group".}

\item{n_starts}{The number of starting values used to calculate additional starting values of Q0 and alpha to use for fitting the demand curve when `type` equals "individual". The default is 10.}
}
\value{
A ggplot2 graphical object; For `type` "individual", the original pt data frame plus the derived values for each individual is returned.
}
\description{
This function derives demand functions using the Koffarnus et al. (2015) exponentiated equation, which is an exponentiated version of the exponential
derivation of demand elasticity (Hursh & Silberberg, 2008). The derived demand indicators from these models can be calculated for the entire sample,
for sub-groups (using a grouping variable), or for individuals. Visualizations are produced of the mean aggregate demand curves when the `type` is
either "overall" or "group". See details for additional information on the exponentiated demand equation.
}
\details{
The exponentiated demand curve function put forth by Koffarnus et al. (2015) is used to derive alpha, which is defined as the rate of change in elasticity (Gilroy et al. 2020).
It is also used to derive Q0 (intensity), and unit elasticity (Pmax). This equation accommodates values of zero consumption, meaning that these variables
do not need to have a small constant applied to them prior to being fit. Elasticity (epsilon or eta) cannot be derived from this or other non-linear equations directly,
as elasticity changes depending on the changes in price (Gilroy et al., 2020).

Unit elasticity is the point at which a one log-unit increase in price is associated with a >1 log-unit decrease in consumption. Prior to this point, consumption
persists or declines shallowly (<1 log-unit decrease) across increases in price (inelastic demand, sub-proportionate decreases in consumption relative to price), whilst
after the price associated with unit elasticity, consumption continues to decrease rapidly alongside increases in price (elastic demand, supra-proportionate
decreases in consumption relative to price) (Gilroy et al., 2020).

Gilroy, S. P., Kaplan, B. A., & Reed, D. D. (2020). Interpretation (s) of elasticity in operant demand. Journal of the Experimental Analysis of Behavior, 114(1), 106-115.

Hursh, S. R., & Silberberg, A. (2008). Economic demand and essential value. Psychological review, 115(1), 186.

Koffarnus, M. N., Franck, C. T., Stein, J., & Bickel, W. K. (2015). A modified exponential behavioral economic demand model to better describe consumption
data. Experimental and Clinical Psychopharmacology, 23(6), 504–512. https://doi.org/10.1161/CIRCRESAHA.116.303790
}
\examples{
### --- Load Data
data("cpt_data")

### --- Prep Data
pt <- price_prep(cpt_data, id_var = "ID", vars = c(paste0("cpt",1:15)),
prices = c("0","0.05","0.10","0.20","0.30","0.40","0.50", "0.75","1","2","3","4","5","7.5","10"))

pt2 <- pt_prep(pt, id_var = "ID", type = "partial", remove0 = TRUE, max_val = 99)
pt3 <- pt_qc(pt2, id_var = "ID", type = "partial")

### --- Function Example
pt4 <- pt_curve(pt3$data, id_var = "ID", type = "individual")

}
