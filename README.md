
# PrometheeTools

<!-- badges: start -->
[![R-CMD-check](https://github.com/ifelipebj/PrometheeTools/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ifelipebj/PrometheeTools/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->


The goal of PrometheeTools is to provide PROMETHEE (Preference Ranking Organisation
METHod for Enrichment of Evaluations), a multicriteria decision-making method, to
handle a large number of alternatives to obtain partial and complete rankings.
Moreover, the package provides the GLNF (Global Local Net Flow) sorting algorithm to
classify alternatives into ordered categories and the SILS function to measure the
classification quality based on sorting methods. 

## Installation
You can install the released version of PrometheeTools from
[CRAN](https://CRAN.R-project.org/package=PrometheeTools) with:

```r
install.packages("PrometheeTools")
```

You can install the development version of PrometheeTools from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ifelipebj/PrometheeTools")
```
## Functions

#### PROMETHEE Outranking Method

PROMETHEE is a multicriteria method that quantifies preference relationships and
obtains the positive, negative and net flows of the alternatives, generating
rankings that reflect the decision-maker's preferences. This function applies
PROMETHEE I (partial ranking) and PROMETHEE II (complete ranking). This function 
can handle  large number of alternatives.

Usage:

``` r
PROMETHEEII(matrix_evaluation, data_criteria)
```
#### Global and Local Searches for Net Flows to Sort

This function applies the GLNF Sorting (Global Local Net Flow Sorting)
algorithm to classify the alternatives into ordered groups according to the
decision-maker's preferences in multiple criteria context. GLNF sorting is
based on PROMETHEE net flows and a set of limiting profiles. This algorithm
starts from a global classification (global search) that is enhanced by two
local searches, intra-categorical and inter-categorical.

Usage:

``` r
GLNF(matrix_evaluation, data_criteria)
```
#### Quality Index of Silhouette for Sorting

This function computes a quality index for `SILS` (Silhouette for Sorting),
which relies on PROMETHEE II net flows to assess the classifications produced
by PROMETHEE-based ordered sorting methods.

Usage:

``` r
SILS(matrix_evaluation, data_criteria, k, SILS_plot = TRUE)
```
## Evaluation matrix template

The evaluation matrix for the `evaluation_matrix` is formed by a set of alternatives
A = (a1,a2,...,a_i,...,a_n) and a set of criteria G = (g1,g2,...,g_j,...,g_m), it must
be structured as follows:

| Alternative   | Criterion 1 | Criterion 2 | ... | Criterion j | ... | Criterion m |
|---------------|-------------|-------------|-----|-------------|-----|-------------|
| Alternative 1 |             |             |     |             |     |             |
| Alternative 2 |             |             |     |             |     |             |
| ...           |             |             |     |             |     |             |
| Alternative i |             |             |     |             |     |             |
| ...           |             |             |     |             |     |             |
| Alternative n |             |             |     |             |     |             |


Where:
- `Alternative` are the names or identifiers of the alternatives being 
evaluated.
- `Criterion 1`, `Criterion 2`, etc. are placeholders for the specific names of 
your criteria.
- Each cell in the matrix represents the value of the corresponding criterion 
for a specific alternative.

The incorporation of limiting profiles that define the category in the
`matrix_evaluation` is a crucial step to apply the `GLNF` and `SILS` 
functions. These profiles are denoted using the syntax "r" followed by the 
profile number. For sorting into `k` groups, there should defined `k+1` 
limiting profiles with R=(r1,r2,...,r`k+1`) being the set of limiting profiles.
"r1" and "r2" represent the limiting profiles for the first group (most
preferred), and "r`k`" and "r`k+1`" denote the profiles for the `k`-th group 
(least preferred). Finally, the set A and R are added into a new set of 
alternatives Z = (a1,a2,...,a_n,r1,r2,...,r`k+1`).

To implement `SILS`,is necessary to add a new column `Category` at the end of 
the `evaluation_matrix` with the classification in (the alternatives assignment to
categories) to be evaluated. Write the classification in number format 
(e.g.: 1,2,3,...,`k`).

Replace the placeholders with the actual names of your criteria and provide
appropriate values for each alternative-criterion combination.


## Criteria parameters table template

The criteria parameters table for the `data_criteria` should be structured as follows:

| Parameter              | Criterion 1 | Criterion 2 | ... | Criterion j | ... | Criterion m |
|------------------------|-------------|-------------|-----|-------------|-----|-------------|
| Function Type          |             |             |     |             |     |             |
| Indifference Threshold |             |             |     |             |     |             |
| Preference Threshold   |             |             |     |             |     |             |
| Objective              |             |             |     |             |     |             |
| Weight                 |             |             |     |             |     |             |

Where:
- `Criterion 1`, `Criterion 2`, etc. are placeholders for the specific names of 
your criteria. 
- `Function Type` specifies the type of function for the criterion ("linear",
"v-shape", "usual","u-shape", "level" and "gaussian").
- `Indifference Threshold` is the indifference threshold for the criterion.
- `Preference Threshold` is the preference threshold for the criterion.
- `Objective` specifies whether the criterion's objective is to maximize ("max") 
or minimize ("min").
- `Weight` represents the weight of the criterion in the analysis. The sum of the
weights of all criteria must be equal to 1.

The preference and indifference thresholds depend on the type of function
selected. The preference threshold is non-zero for all functions except for
"usual" and "u-shaped". The indifference threshold is non-zero for "linear",
"level" and "u-shaped" functions.

Replace the placeholders with the actual names of your criteria and provide
appropriate values for each parameter.


## Example

This is an example which shows you how to solve a ranking and sorting problem:

``` r
# Load the PrometheeTools library
library(PrometheeTools)
# Example code corresponds to 30 customers to be evaluated with five 
# criteria for partial ranking (PROMETHEE I), complete ranking (PROMETHEE II) and 
# sorting into four groups ordered by decision-maker preferences (GLNF sorting).
# Finally the silhouette quality index for sorting (`SILS`) is calculated.

# The evaluation matrix is defined, and five limiting profiles are included 
# (r1,r2,r3,r4,r5) to define four categories.

#############

matrix_evaluation <- data.frame (
 
Alternative = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
                11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
                21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
                "r1", "r2", "r3", "r4", "r5"),
Monetary = c(21.52, 68.09, 184.94, 237.62, 14.29, 12.78, 91.53, 11.39, 264.79, 12.74,
             274.41, 3.75, 47.92, 34.5, 45.89, 39.92, 31.18, 273.23, 16.39, 3.91,
             20.09, 6.52, 26.62, 28.47, 7.57, 69.2, 420.95, 12.01, 85.88, 8.78,
             6816.80, 120, 40, 20, 0),
Recency = c(0, 0, 0, 0, 3, 5, 0, 6, 0, 3,
            1, 0, 1, 0, 0, 0, 0, 0, 2, 1,
            0, 0, 0, 0, 5, 1, 0, 0, 1, 4,
            0, 1, 7, 8, 12),
Frequency = c(7, 5, 12, 12, 1, 3, 9, 2, 12, 4,
              11, 3, 10, 10, 11, 11, 12, 12, 7, 1,
             5, 2, 9, 11, 4, 10, 12, 3, 10, 2,
             12, 10, 8, 4, 1),
Financial_score = c(66, 58, 83, 68, 68, 69, 77, 55, 77, 53,
                    78, 35, 84, 75, 71, 64, 56, 55, 52, 30,
                    66, 50, 65, 53, 54, 82, 68, 53, 62, 43,
                     100, 80, 75, 65, 0),
                     
  Length = c(4, 3, 3, 2, 2, 2, 2, 3, 2, 4,
            3, 3, 1, 1, 2, 5, 4, 2, 2, 5,
            4, 5, 1, 4, 2, 1, 5, 1, 1, 2,
            5, 4, 3, 2, 1))
  
# The matrix with the criteria parameters is defined
  
  data_criteria <- data.frame(
  Parameter = c("Function Type", "Indifference Threshold",
               "Preference Threshold","Objetive", "Weight"),
  Frequency = c("linear", 0, 3, "max", 0.2),
  Monetary = c("linear", 30.00, 120, "max", 0.4),
  Recency = c("usual", 0.00, 0.00, "min", 0.1),
  Financial_score = c("linear", 0.00, 10, "max", 0.2),
  Length = c("usual", 0.00, 0.00, "max", 0.1))

############# 

# The `PROMETHEEII` function is applied
RS <- PROMETHEEII(matrix_evaluation, data_criteria)
# Positive flows, negative flows and net flows
RS$NF
# Net flows by criteria
RS$NFC

#############

# The `GLNF` function is applied
RS <- GLNF(matrix_evaluation, data_criteria)
# Final classification of the 30 customers into four ordered groups
RS$Class
# It is possible to check the details of global and local searches.
# Show global classification results
RS$Global
# Show local classification results
RS$Local1
RS$Local2

#############

# Add the categories calculated with GLNF (or another method) to the last column 
# of `matrix_evaluation`. The limiting profiles have not been assigned to any
# group.

matrix_evaluation$Category = c(3, 3, 1, 1, 4, 3, 2, 4, 1, 3,
                                 1, 4, 2, 2, 2, 2, 2, 1, 3, 4,
                                 3, 3, 3, 2, 4, 2, 1, 4, 3, 4,
                                 NA, NA, NA, NA, NA)

# The number of categories or groups to be evaluated is defined.

k <- 4

# The function `SILS` is applied, where `SILS_plot = TRUE` to plot the results 
# of the silhouettes.

RS <- SILS(matrix_evaluation, data_criteria, k, SILS_plot = TRUE)
#Print silhouette values
print(RS)

```

