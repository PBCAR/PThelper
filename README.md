## Version 3.0

This package has undergone a transformation in anticipation of its release on CRAN (*stay tuned*)! For previous releases, please refer to the [archived](https://github.com/PBCAR/PThelper_Archive) PThelper repository.

## The PThelper Package

The package offers several customizable functions for handling all aspects of purchase task processing:

i) Pre-processing of raw data including quality control;

ii) The calculation of empirical and derived indicators;

iii) Indicator-level variable management; and

iv) Descriptive statistics and visualizations.

## Installation

To install the *PThelper* package, use the *devtools* package to download directly from this GitHub repository:

```
install.packages("devtools")
devtools::install_github("PBCAR/PThelper", build_vignettes = TRUE)
```

## PThelper Examples

Example Cigarette Purchase Task (CPT) data (`cpt_data`) are provided by this package and mimic typical responses seen in purchase tasks (including non-systematic responses). This data consists of 100 fictional participants, 15 different prices ranging from free (\$0) to \$10 per cigarette, and follows a partially-administered method using arrays of 3.

In addition to detailed documentation for each function, there is also a vignette available which provides examples of each function using the CPT data from this package:

```
vignette("PThelper_Introduction", package = "PThelper")
```
