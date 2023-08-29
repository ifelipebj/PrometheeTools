#' PROMETHEE Outranking Method
#'
#'PROMETHEE is a multicriteria method that quantifies preference relationships
#'and obtains net flows, generating a ranking that reflects the decision-maker's
#'preferences. This function applies PROMETHEE I (partial ranking) and
#'PROMETHEE II (full ranking). This function can be used with a large number of
#'alternatives.
#'
#' @param matrix_evaluation A matrix that encompasses the values of the
#' alternatives across various criteria, with the rows representing the
#' alternatives and the columns correspond to the evaluation criteria.
#' @param data_criteria A matrix with the parameter information (rows) for each
#' criterion (columns). The parameters rows are in the following order: Function
#' Type, Indifference Threshold, Preference Threshold, Objective and Weight.

#' @return
#' -`NF` A matrix with Positive and negative flows for preference
#' relations (PROMETHEE I) and Net flows for total outranking (PROMETHEE II).
#'
#' -`NFC` A Matrix of net flows by criterion.
#' @references
#' Brans, J.P.; De Smet, Y., (2016). PROMETHEE Methods. In: Multiple Criteria
#' Decision Analysis. State of the Art Surveys, Figuera, J., Greco, S.,
#' Ehrgott, M.; Springer: New York, USA, pp. 187-219. DOI: 10.1007/978-1-4939-3094-4_6.
#' @export PROMETHEEII
#' @details
#' - The preference function types are as follows: "linear", "v-shape", "usual",
#' "u-shape", "level" and "gaussian".
#' - The preference and indifference thresholds depend on the type of function
#' selected. The preference threshold is non-zero for alL functions except for
#' "usual" and "u-shaped". The indifference threshold is non-zero for "linear",
#' "level" and "u-shaped" functions.
#' - In the objective criterion write "max" to maximize or "min" to minimize.
#' - The sum of the weights of all the criteria must be equal to 1.
#' - Compared to other implementations such as `promethee123` and `PROMETHEE`,
#' this implementation of `PROMETHEEII` is designed to handle a larger number
#' of alternatives (it has been tested with 10,000 alternatives).
#' @examples
#'matrix_evaluation <- data.frame (
#'
#'Alternatives = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
#'                 11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
#'                 21, 22, 23, 24, 25, 26, 27, 28, 29, 30),
#'Monetary = c(21.52, 68.09, 184.94, 237.62, 14.29, 12.78, 91.53, 11.39, 264.79,12.74,
#'             274.41, 3.75, 47.92, 34.5, 45.89, 39.92, 31.18, 273.23, 16.39, 3.91,
#'             20.09, 6.52, 26.62, 28.47, 7.57, 69.2, 420.95, 12.01, 85.88, 8.78),
#'Recency = c(0, 0, 0, 0, 3, 5, 0, 6, 0, 3,
#'            1, 0, 1, 0, 0, 0, 0, 0, 2, 1,
#'            0, 0, 0, 0, 5, 1, 0, 0, 1, 4),
#'Frequency = c(7, 5, 12, 12, 1, 3, 9, 2, 12, 4,
#'              11, 3, 10, 10, 11, 11, 12, 12, 7, 1,
#'              5, 2, 9, 11, 4, 10, 12, 3, 10, 2),
#'Financial_score = c(66, 58, 83, 68, 68, 69, 77, 55, 77, 53,
#'                    78, 35, 84, 75, 71, 64, 56, 55, 52, 30,
#'                    66, 50, 65, 53, 54, 82, 68, 53, 62, 43),
#'Length = c(4, 3, 3, 2, 2, 2, 2, 3, 2, 4,
#'           3, 3, 1, 1, 2, 5, 4, 2, 2, 5,
#'           4, 5, 1, 4, 2, 1, 5, 1, 1, 2))
#'data_criteria <- data.frame(
#'Parameter = c("Function Type", "Indifference Threshold",
#'              "Preference Threshold","Objetive", "Weight"),
#'Frequency = c("linear", 0, 3, "max", 0.2),
#'Monetary = c("linear", 30.00, 120, "max", 0.4),
#'Recency = c("usual", 0.00, 0.00, "min", 0.1),
#'Financial_score = c("linear", 0.00, 10, "max", 0.2),
#'Length = c("usual", 0.00, 0.00, "max", 0.1))
#'
#'###############################
#'RS <- PROMETHEEII(matrix_evaluation, data_criteria)
#'RS$NF
#'RS$NFC
PROMETHEEII <- function(matrix_evaluation, data_criteria) {
  #identify columns of criteria and alternative column
  criteria_name <- colnames(matrix_evaluation)[-1]
  Alt_name <- colnames(matrix_evaluation)[1]
  # matrix initialization
  Nf_posi <- matrix(0, nrow = nrow(matrix_evaluation), ncol = 1)
  Nf_nega <- matrix(0, nrow = nrow(matrix_evaluation), ncol = 1)
  Netflow_criteria <- matrix(0, nrow = nrow(matrix_evaluation), ncol = length(criteria_name))
  # Net flow for each criterion j
  for (j in seq_along(criteria_name)) {
    # get the name of the criterion and its values
    criteria <- criteria_name[j]
    criteria_values <- matrix_evaluation[[criteria]]
    # read criteria parameters
    Type <- data_criteria[1, criteria]
    Objetive <- data_criteria[4, criteria]
    Weight <- as.numeric(data_criteria[5, criteria])
    q <- ifelse(Type %in% c("v-shape", "usual", "gaussian"), 0, as.numeric(data_criteria[2, criteria]))
    p <- ifelse(Type %in% c("usual", "u-shape"), 0, as.numeric(data_criteria[3, criteria]))
    # Calculate the deviation between each pair of alternatives.
    desvia <- outer(criteria_values, criteria_values, `-`)
    desvia <- if (Objetive == "min") -desvia else desvia
    # Calculate preference values
    if (Type %in% c("linear", "v-shape")) {
      m <- 1 / (p - q)
      b <- ifelse(q == 0, 0, 1 - (m * p))
      preference <- ifelse(desvia >= p, 1, ifelse(desvia <= q, 0, desvia * m + b))
    }else if (Type %in% c("usual", "u-shape")) {
      preference <- ifelse(desvia > q, 1, 0)
    }else if (Type == "level") {
      preference <- ifelse(desvia > p, 1, ifelse(desvia > q & desvia <= p, 0.5, 0))
    }else if (Type == "gaussian") {
      preference <- ifelse(desvia > q, 1 - exp(-desvia^2 / (2 * p^2)), 0)
    }
    # Calculate positive and negative flow
    Flow_posi <- rowSums(preference) / (ncol(preference) - 1)
    Flow_nega <- colSums(preference) / (nrow(preference) - 1)
    # Calculate net flow (Nf)
    Nf_cri <- (Flow_posi - Flow_nega)
    Nf_posi <- Nf_posi + (Flow_posi * Weight)
    Nf_nega <- Nf_nega + (Flow_nega * Weight)
    # construct net flows matrix by criterion
    Netflow_criteria <- cbind(Netflow_criteria, Nf_cri)
    Netflow_criteria <- Netflow_criteria[, -1]
  }
  #netflow total
  Netflow <- Nf_posi - Nf_nega
  # Add the names that correspond to the final matrices
  colnames(Netflow_criteria) <- criteria_name
  colnames(Netflow) <- paste("Phi")
  colnames(Nf_posi) <- paste("Phi+")
  colnames(Nf_nega) <- paste("Phi-")
  Netflow_criteria <- cbind((matrix_evaluation[, 1]), Netflow_criteria)
  colnames(Netflow_criteria)[1] <- Alt_name
  Netflow <- cbind((matrix_evaluation[, 1]), Nf_posi, Nf_nega, Netflow)
  colnames(Netflow)[1] <- Alt_name
  # Create a list of results (PR)
  RS <- list(
    NF = Netflow,
    NFC = Netflow_criteria
  )
  return(RS)
}
