% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SILS.R
\name{SILS}
\alias{SILS}
\title{Quality Index of Silhouette for Sorting}
\usage{
SILS(matrix_evaluation, data_criteria, k, SILS_plot = FALSE)
}
\arguments{
\item{matrix_evaluation}{The matrix includes the values for all alternatives
and limiting profiles are rows and columns correspond to the evaluation
criteria. The last column indicates the alternative classification.}

\item{data_criteria}{Matrix with the parameter information (rows) for each
criterion (columns). The rows of parameters are in the following order:
Function Type, Indifference Threshold, Preference Threshold, Objective and
Weight.}

\item{k}{The number of categories to be evaluated.}

\item{SILS_plot}{Boolean value indicating whether to generate a stacked bar
chart representing the SILS values.}
}
\value{
\itemize{
\item A data frame with the SILS values for each alternative according to the
classification entered.
\item Stacked bar chart representing the SILS values for each alternative with
horizontal control limits.
}
}
\description{
This function computes the quality index for \code{SILS} (Silhouette for Sorting),
which relies on PROMETHEE II net flows to assess the classifications generated
by PROMETHEE-based ordered sorting methods.
}
\details{
\itemize{
\item The categories corresponding to the classifications to be assessed should
be indicated in the last column of \code{matrix_evaluation} in ordinal numbers,
where 1 is the most preferred group.
\item Enter the same criteria parameters and limiting profiles that you used to
obtain the classifications with PROMETHEE II.
\item The Limiting Profiles used must be presented as rows in the
matrix_evaluation. The name must start with the letter "r" followed by the
profile number (e.g., "r1", "r2"). For \code{k} categories, there
should define \code{k + 1} limiting profiles.
\item The types of preference function are as follows: "linear", "v-shape",
"usual", "u-shape", "level" and "gaussian".
\item The preference and indifference thresholds depend on the type of function
selected. The preference threshold requires definition (is non-zero) for all
functions except for "usual" and "u-shaped". The indifference threshold is
non-zero for "linear", "level" and "u-shaped" functions.
\item In the objective write "max" to maximize or "min" to minimize.
\item The sum of the weights of all criteria must be equal to 1.
}
}
\examples{
k <- 4
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
          5, 4, 3, 2, 1),
Category = c(3, 3, 1, 1, 4, 3, 2, 4, 1, 3,
              1, 4, 2, 2, 2, 2, 2, 1, 3, 4,
              3, 3, 3, 2, 4, 2, 1, 4, 3, 4,
              NA, NA, NA, NA, NA))
data_criteria <- data.frame(
 Parameter = c("Function Type", "Indifference Threshold",
               "Preference Threshold","Objetive", "Weight"),
 Frequency = c("linear", 0, 3, "max", 0.2),
 Monetary = c("linear", 30.00, 120, "max", 0.4),
 Recency = c("usual", 0.00, 0.00, "min", 0.1),
 Financial_score = c("linear", 0.00, 10, "max", 0.2),
 Length = c("usual", 0.00, 0.00, "max", 0.1))
RS <- SILS(matrix_evaluation, data_criteria, k, SILS_plot = TRUE)
print(RS)
}
\references{
Barrera, F., Segura, M., & Maroto, C. (2023) Online. Multicriteria sorting method
based on global and local search for supplier segmentation. International
Transactions in Operational Research. DOI:10.1111/itor.13288
}
\seealso{
\code{\link{PROMETHEEII}}
}
