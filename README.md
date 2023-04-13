[![R-CMD-check](https://github.com/PBCAR/PThelper/actions/workflows/r_check_standard.yml/badge.svg)](https://github.com/PBCAR/PThelper/actions/workflows/r_check_standard.yml) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5847710.svg)](https://doi.org/10.5281/zenodo.5847710)

## !! Updates for 2023 !!

This package is currently undergoing a transformation in a few key ways prior to being submitted to CRAN. These package changes can be found in the pt2023 branch of GitHub.

First of all, the utility of the existing functions are being altered to provide additional processing capabilities and information for reproducibility purposes. Second, the quality control function (`pt_qc()`) can now process two different types of purchase tasks: i) fully-administered purchase tasks; and ii) purchase tasks administered as an array or only until zero consumption is reached. Third, additional functions have been added such as a summary function (`pt_summary()`) and a correlation function (`pt_corr()`). Fourth, the package calculates the empirical purchase task indicators as well as elasticity and derived intensity internally without the use of external packages.

# The {PThelper} Package

This package is designed to walk users through the various steps required to clean, process, and summarize purchase task data. The benefit of this package is that it provides a standardized framework for processing purchase task data, improving reproducibility.

The {PThelper} package offers 10 different functions for 3 different stages of purchase task processing, plus post-processing descriptives statistics:

i)  The pre-processing stage of the raw data;

ii)  The calculation of elasticity and derived indicators stage via curve-fitting;

iii)  The index-level variable management; and

iv) The post-processing stage of data descriptives and summaries

Example Cigarette Purchase Task (CPT) data (`cpt_data`) are provided by this package. Examples using each function on this data set are demonstrated in detail below.

## Installation

To install the {PThelper} package, use the {devtools} package to download directly from this GitHub repository:

```
install.packages("devtools")
devtools::install_github("PBCAR/PThelper")
```

## Introduction to Purchase Tasks

These demand instruments are used to measure the relative reinforcement of a substance - the extent to which a value for a commodity is effected by increased cost. Greater demand (i.e. little sensitivity to changes in price) is often associated with substance-related problems and use disorders (see Bickel et al., 2011). The 5 most commonly used indicators generated from the purchase task are:

Breakpoint -- The first increment of cost with zero consumption

Intensity -- Consumption at zero (or negligible) cost

Omax -- The maximum expenditure

Pmax -- The price associated with the maximum total expenditure

!! Elasticity -- Measures sensitivity of consumption to increases in cost

## i) Pre-Processing

Purchase task processing requires preparation, and using the `price_prep()`, `pt_prep()`, and `pt_qc()` functions will ensure the proper cleaning of the data prior calculating the purchase task indicators.

The `plot_summary()` function can also be used at the end of the pre-processing stage to visualize the consumption and expenditure across the prices of the purchase task, prior to any outlier management.

The `pt_winsor()` function is an optional final pre-processing step which offers different outlier management techniques for the price-level data prior to curve-fitting and other purchase task processing.

#### a) Prices as Variable Names:

To process purchase task data, the variables need to be renamed as the prices they represent. The `price_prep()` function helps users do this within R easily by naming the variables and the equivalent prices.

#### b) Missing Data Identification:

**!! Update:** This function has been updated in 2023 to work in tandem with the `pt_qc()` function if the purchase task was administered using an array. As such, it no longer assigns zero values for prices after breakpoint for purchase tasks administered partially.

The `pt_prep()` function will identify and remove those with missing responses across all prices.

#### c) Assign Maximum Consumption Values (Optional):

The `pt_prep()` function gives the user the option to define a maximum value for the purchase task, which re-code values exceeding the `max_val` to the maximum value.

### d) Non-Zero Consumption At Final Price:

If the purchase task was not administered in full (i.e. ended after a consumption of 0 was reached within an array), participants who do not have zero consumption at their final price point will be identified and removed using the `pt_prep()` function. This is because it cannot be assumed that all subsequent responses would also be zeroes. However, individuals who move through all items without zero consumption reached are still included, as their breakpoint value is assigned as the maximum price point of the assessment rounded to the next integer.

#### e) Quality Control using the 3-Criterion Algorithm:

**!! Update:** This function has been updated in 2023 to specifically calculate trend violations (delta Q) and bounce values (ratios) for purchase tasks not administered in full (i.e. task is terminated after zero consumption is reached, or after an array with zero consumption is completed). Also updated are the criterion, as they are more accurately defined, and the defaults have been changed.

The `pt_qc()` function helps users to conduct quality control on purchase task data by using the 3-Criterion Algorithm proposed by Stein et al. (2015) to remove non-systematic data. The criterion can be applied to both fully and partially administered purchase tasks:

Specifically, this function identifies and removes IDs with:

i) Trend violations -- Those who start with non-zero consumption and do not exhibit a decelerating trend;

ii) Excessive bounce ratios -- Bounce ratios identify inconsistencies in consumption values given, specifically defined as any subsequent consumption values that are 25% higher than consumption at the first price. The default bounce ratio is 10%;

iii) Excessive reversals in responses -- Those who exceed a user-defined number of reversals from a consumption of zero. The default defines a reversal as 1 or more consecutive zeroes and removes those with 1 or more reversals.

These criteria can also be customized, allowing for modifications, including a different method of calculating bounces (price-to-price changes).

#### f) Price-Level Winsorization (Optional):

The `pt_winsor()` function can be used for outlier management at both the price level and indicator level. Outlying values are identified by z-scores higher than the `z_val` or lower than the negative `z_val` as chosen by the user. There are 3 different winsorization types:

i)  Option 1 replaces outliers with the maximum non-outlying price rounded up;

ii) Option 2 replaces outliers with a value 1 unit higher than highest (or 1 unit lower than the lowest) non-outlying value; and

iii) Option 3 replaces outliers with a value 1 unit above the next highest non-outlying value to maintain order.


## ii) Calculating Purchase Task Indicators:

There are two functions which help calculate both the empirical indicators as well as Elasticity for each individual.

#### a) Empircal Purchase Task Indicators:

The empirical values for Intensity, Breakpoint, Omax, and Pmax are processed using the `pt_empirical()` function, which are calculated from the data.

#### b) Fitting the Curve:

To calculate elasticity, a non-linear exponentiated curve by Koffarnus et al. (2015) is fit to the price-level data using the `pt_elasticity` function. Elasticity can either be calculated for the entire sample, or by individual, with the appropriate plots of this curve produced. Additionally, the derived Intensity value from the curve (Q0) is also calculated and retained.

In order to fit the curve, a k-value needs to be assigned in the formula. Either the empirical value of k can be calculated from the mean range of the overall sample, or the researcher can choose the k-value directly (to allow for direct comparisons with other research).

## iii) Final Processing of Purchase Task Indicators:

Additional optional processing are available by using the `pt_winsor()` and `plot_transform()` functions. These aid with outlier management and management of the distributional shape of the indicator-level purchase task variables.

#### a) Index-Level Winsorization (Optional):

The `pt_winsor()` function with the `level` argument set to "indicator", can be used to manage outlying data at the indicator-level. It offers the same 3 winsorization as those available for the price-level winsorization:

i)  Option 1 replaces outliers with the maximum non-outlying value rounded up;

ii) Option 2 replaces outliers with a value 1 unit higher than highest (or 1 unit lower than the lowest) non-outlying value; and

iii) Option 3 replaces outliers with a value 1 unit above the next highest non-outlying value to maintain order.

*NOTE:* The unit in Option 3 is user-defined via the `delta` argument. Due to the small nature of elasticity values, a value of 0.001 is recommended.

#### b) Transformation of Purchase Task Variables (Optional):

Optional transformation of index-level variables using the `plot_transform()` function, helps in diagnosing the best transformation to achieve an approximately normal distribution. This function uses the two most common transformations for purchase task indicators, specifically log (log of base 10) and square root due to the distribution often being right-skewed (positively-skewed).

## iv) Descriptive Summary of Purchase Task Indicators:

**!! Additions:** Added in 2023, descriptive statistics of the indicators are available using the `pt_summary()` function, and correlation coefficients and related heat map are available using the `pt_corr()` function.

#### a) Descriptive Statistics

The `pt_summary()` function provides summary statistics of purchase task indicators, with the ability to provide summary statistics by a grouping variable.

#### b) Pairwise Pearson Correlation

The `pt_corr()` function provides pairwise Pearson correlation coefficients and p-values for indicators (as well as any additional variables of interest). It also provides a heatmap, which displays the correlation coefficients as well as denotes significance with bold coefficient text.


## Example Data (CPT)

The mock data from this package is a Cigarette Purchase Task (CPT) consisting of 15 items in total. The prices range from 'FREE (\$0)' to '\$10' per cigarette. The items are administered to participants in arrays of 3 items. A zero consumption response within an array will end the instrument after the final item within the array.

There are N = 100 participants included in the sample data set, as identified by the unique identifier 'ID'. Most of these participants have CPT data (n = 92), and most are smokers (n = 87).

To import the sample data into the R Environment:

```
library(PThelper)
PT <- cpt_data
```

## i) Pre-Processing

#### The `price_prep()` Function:

First, the variables are renamed to represent their price using the `price_prep()` function:

```
PT <- price_prep(PT, id_var = "ID", vars = c("cpt1","cpt2","cpt3","cpt4","cpt5","cpt6","cpt7", "cpt8","cpt9","cpt10","cpt11","cpt12","cpt13","cpt14","cpt15"),
price = c("0","0.05","0.10","0.20","0.30","0.40","0.50","0.75","1","2","3","4","5","7.5","10"))
```

#### The `pt_prep()` Function:

Next, since the sample data mimics data that were administered in a 3-price array, the `pt_prep()` function with the 'partial' argument set to TRUE will identify and remove participants who ended on a non-zero consumption (except in the case of the last price administered). Additionally, participants with missing data on all items will be removed.

```
PT2 <- pt_prep(PT, id_var = "ID", partial = TRUE)
```

The IDs of those missing data or with a non-zero response will be printed to the console:

```
IDs with Missing Values: 11_A 22_B 24_B 28_A 36_A 46_B 49_A 74_A
IDs not reaching zero consumption (does not include IDs who reach max item): 19_A 30_B 43_B 53_A 62_A
```

#### The `pt_qc()` Function:

The quality control measures are then applied to the data using the `pt_qc()` function, with the 'type' argument set to TRUE. This customizes the calculation of delta Q used to identify trend violations, as well as the bounce ratio to be specific to the number of items that were administered for each individual.

```
PT3 <- pt_qc(PT2, id_var = "ID", type = "partial", bounce_type = "p2p")
```
Those in violation of any of the 3 quality control measures are removed, and are identified by ID in the console print out:

```
IDs with a trend violation: 2_A 43_A 68_A
IDs with a bounce violation: 3_A 10_A
IDs with a reversal violation: 7_B 10_B 14_A 38_B 45_A 51_A 53_B 66_B 73_B
```

Furthermore, delta Q values, bounce ratios, and presence of reversals are printed for all individuals (included or excluded), and can be seen in the second data frame of the returned list:

```
View(PT3[["qc_data"]])
```

#### The `winsor_price()` Function:

An optional step in pre-processing is outlier management of consumption values. Winsorization at the price level can be completed using the `winsor_price()` function:

```
PT4 <- winsor_price(PT3$data, id_var = "ID")
```

The changes made to consumption values are identified by ID and price, which can be seen in the second data frame of returned list:

```
View(PT4[["wins_table"]])
```

#### The `plot_summary()` Function:

The end of the pre-processing stage should finish with a visual inspection of both consumption and expenditure across the prices of the purchase task. This can be achieved using the `plot_summary()` function on either the unwinsorized or winsorized data:

```
plot_summary(PT4$data, id_var = "ID")
```

The visualization is printed to the 'Plots' pane:


## ii) Calculating Elasticity and Derived Values:

Calculating the elasticity curve is achieved using the `pt_elasticity()` function. There are two types of elasticity curves to calculate: Either an "overall"" curve using the mean data of the entire sample; or "individual" curves for each participant:

```
PT5 <- pt_elasticity(PT4$data, id_var = "ID", type = "overall")
```

Both types currently use the exponentiated (Koffarnus et al., 2015) equation to calculate demand. If a k-value is not provided by the user, then the best fitting k-value is determined and identified in the console printout:

```
Calculated k-value:  1.7 
R-squared value: 0.99968
```

An overall curve of the entire sample is provided with this function, which is printed to the 'Plots' pane:


When the type argument is set to "individual", both derived elasticity and intensity are calculated from the demand curve. Additionally, a spaghetti plot of demand curves is printed to the 'Plots' pane:


## iii) Final Processing of Purchase Task Indicators:

#### The `pt_winsor()` Function:

All, none, or some of the index-level variables can undergo winsorization to manage outlying values. Below, the `pt_winsor()` function (used previously for price-level winsorization), is used to manage outlying values of Intensity, Omax, and Breakpoint:

```
PT6 <- pt_winsor(PT5, id_var = "ID", level = "indicator", index_vars = c("Intensity","Omax","Breakpoint"), delta = 1)
```

Any changes made to Intensity, Omax, and Breakpoint are identified by ID and can be seen in the second data frame of returned list:

```
View(PT6[["wins_table"]])
```

#### The `plot_transform()` Function:

This function provides summary statistics as well as a visualization of distribution of the original variable, alongside two transformations (log10 and square root):

```
plot_transform(PT6$data, pt_var = "Intensity")
```

In the presence of zero values in the indicator, a small constant of 0.01 is added prior to log transformation. The visualization is printed to the 'Plots' pane:

## iv) The Post-Processing of Purchase Task Indicators:

#### The `pt_summary()` Function:

This function will provide summary descriptives (Mean +/- SE, Min, and Max) of the purchase task indicators simultaneously, and can provide summary descriptives by a grouping variable. The function produces a data frame that is easy to export:

```
OUT <- pt_summary(PT6$data,pt_vars = c("Intensity","Omax","Breakpoint"))
```

#### The `pt_corr()` Function:

This function will provide Pearson correlation coefficients and p-values of the purchase task indicators. It can also be used to produce a heatmap (default):

```
pt_corr()
```

## References

Bickel, Warren K, David P Jarmolowicz, E Terry Mueller, and Kirstin M Gatchalian. (2011). "The Behavioral Economics and Neuroeconomics of Reinforcer Pathologies: Implications for Etiology and Treatment of Addiction." Current Psychiatry Reports 13 (5): 406--15.

Hursh, S. R., & Silberberg, A. (2008). Economic demand and essential value. Psychological Review, 115 (1), 186-198. <http://dx.doi.org/10.1037/0033-295X.115.1.186>

Koffarnus, M. N., Franck, C. T., Stein, J. S., & Bickel, W. K. (2015). A modified exponential behavioral economic demand model to better describe consumption data. Experimental and Clinical Psychopharmacology, 23 (6), 504-512. <http://dx.doi.org/10.1037/pha0000045>

Stein, J. S., Koffarnus, M. N., Snider, S. E., Quisenberry, A. J., & Bickel, W. K. (2015). Identification and management of nonsystematic purchase task data: Toward best practice. Experimental and clinical psychopharmacology, 23(5), 377.
