#'Global and Local Searches for Net Flows to Sort
#'
#'This function applies the GLNF Sorting (Global Local Net Flow Sorting)
#'algorithm to classify the alternatives into ordered groups according to the
#'decision-maker's preferences in multiple criteria context. GLNF sorting is
#'based on PROMETHEE net flows and a set of limiting profiles. This algorithm
#'starts from a global classification (global search) that is enhanced by two
#'local searches, intra-categorical and inter-categorical.
#' @usage
#' GLNF(matrix_evaluation, data_criteria)
#' @param matrix_evaluation The matrix includes the values for all alternatives.
#' The alternatives and limiting profiles are row and columns correspond to
#' the evaluation criteria.
#' @param data_criteria Matrix with the parameter information (rows) for each
#' criterion (columns). The rows of parameters are in the following order:
#' Function Type, Indifference Threshold, Preference Threshold, Objective and
#' Weight.
#' @references
#' Barrera, F., Segura, M., & Maroto, C. (2023) Online. Multicriteria sorting method
#' based on global and local search for supplier segmentation. International
#' Transactions in Operational Research. DOI:10.1111/itor.13288
#' @return
#' -`Global` Matrix with the results of the global search where positive,
#' negative and net flow, and its preclassification are defined for each
#' alternative.
#'
#' -`Local1` Matrices with the results of the first local search. PROMETHEE is
#' applied to each group obtained in the global search. The alternatives are
#' divided according to their positive or negative sign from the net flows
#' obtained from PROMETHEE.
#'
#' -`Local2` Matrices with the results of the second local search, where the
#' alternatives are divided according to their sign from net flows are obtained
#' after applying PROMETHEE between each pair of neighbour categories.
#'
#' -`Class` Final classification of the alternatives results.
#' @export
#' @seealso \code{\link{PROMETHEEII}}
#' @details
#' - The Limiting Profiles should be presented as rows in the matrix_evaluation.
#' The name must start with the letter "r" followed by the profile number
#' (e.g., "r1", "r2").
#' - For k categories, there should be k + 1 limiting profiles. To create k
#' groups the set of limit profiles are defined, where r1 is preferred to
#' r2,...,preferred to r(k+1).
#' - The types of preference function are as follows: "linear", "v-shape",
#' "usual", "u-shape", "level" and "gaussian".
#' - The preference and indifference thresholds depend on the type of function
#' selected. The preference threshold requires definition (is non-zero) for all
#' functions except for "usual" and "u-shaped". The indifference threshold is
#' non-zero for "linear", "level" and "u-shaped" functions.
#' - In the objective write "max" to maximize or "min" to minimize.
#' - The sum of the weights of all criteria must be equal to 1.
#' @examples
#'matrix_evaluation <- data.frame (
#'
#'Alternative = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
#'                 11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
#'                 21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
#'                 "r1", "r2", "r3", "r4", "r5"),
#'Monetary = c(21.52, 68.09, 184.94, 237.62, 14.29, 12.78, 91.53, 11.39, 264.79, 12.74,
#'             274.41, 3.75, 47.92, 34.5, 45.89, 39.92, 31.18, 273.23, 16.39, 3.91,
#'             20.09, 6.52, 26.62, 28.47, 7.57, 69.2, 420.95, 12.01, 85.88, 8.78,
#'             6816.80, 120, 40, 20, 0),
#'Recency = c(0, 0, 0, 0, 3, 5, 0, 6, 0, 3,
#'            1, 0, 1, 0, 0, 0, 0, 0, 2, 1,
#'            0, 0, 0, 0, 5, 1, 0, 0, 1, 4,
#'            0, 1, 7, 8, 12),
#'Frequency = c(7, 5, 12, 12, 1, 3, 9, 2, 12, 4,
#'              11, 3, 10, 10, 11, 11, 12, 12, 7, 1,
#'              5, 2, 9, 11, 4, 10, 12, 3, 10, 2,
#'              12, 10, 8, 4, 1),
#'Financial_score = c(66, 58, 83, 68, 68, 69, 77, 55, 77, 53,
#'                    78, 35, 84, 75, 71, 64, 56, 55, 52, 30,
#'                    66, 50, 65, 53, 54, 82, 68, 53, 62, 43,
#'                    100, 80, 75, 65, 0),
#'Length = c(4, 3, 3, 2, 2, 2, 2, 3, 2, 4,
#'           3, 3, 1, 1, 2, 5, 4, 2, 2, 5,
#'           4, 5, 1, 4, 2, 1, 5, 1, 1, 2,
#'           5, 4, 3, 2, 1))
#'data_criteria <- data.frame(
#'Parameter = c("Function Type", "Indifference Threshold",
#'              "Preference Threshold","Objetive", "Weight"),
#'Frequency = c("linear", 0, 3, "max", 0.2),
#'Monetary = c("linear", 30.00, 120, "max", 0.4),
#'Recency = c("usual", 0.00, 0.00, "min", 0.1),
#'Financial_score = c("linear", 0.00, 10, "max", 0.2),
#'Length = c("usual", 0.00, 0.00, "max", 0.1))
#'RS <- GLNF(matrix_evaluation, data_criteria)
#'RS$Class
#'RS$Global
#'RS$Local1
#'RS$Local2
GLNF <- function(matrix_evaluation, data_criteria) {
  colnames(matrix_evaluation)[1] <- "Alternative"
  # Step2. Global search
  RS <- PROMETHEEII(matrix_evaluation, data_criteria)
  data <- as.data.frame(RS[[1]])
  data$Phi <- as.numeric(data$Phi)
  lim_prof <- data[grepl("^r\\d+", data$Alternative), ]
  lim_prof <- lim_prof[order(as.numeric(gsub("^r", "", lim_prof$Alternative))), ]
  num_categories <- nrow(lim_prof) - 1
  phi_prof <- lim_prof$Phi
  data <- data[!grepl("^r\\d+", data$Alternative), ]
  data$Category <- NA
  # Assigning categories according to Phi
  for (i in 1:(num_categories)) {
    lim_sup <- phi_prof[i]
    lim_inf <- phi_prof[i + 1]
    if (i == 1 && any(data$Phi == lim_sup)) {
      data$Category[data$Phi == lim_sup] <- i
    }
    data$Category[data$Phi >= lim_inf & data$Phi < lim_sup] <- i
  }
  Global_Search <- data
  lim_prof$Category <- NA
  Global_Search <- rbind(Global_Search, lim_prof)
  # Creating subarrays for each group according to the global search
  Global_Search_categories <- list()
  for (i in 1:(num_categories)){
    Alt_category <- data[data$Category == i, ]
    ids_category <- Alt_category$Alternative
    lim_prof_category <- lim_prof[grepl(paste0("^r", i, "|^r", i + 1), lim_prof$Alternative), ]
    ids_category <- union(ids_category, lim_prof_category$Alternative)
    category_matrix <- matrix_evaluation[matrix_evaluation$Alternative %in% ids_category, ]
    Global_Search_categories[[paste0("C", i)]] <- category_matrix
  }
  # Step 3. Local search 1
  Local1_categories <- list()
  for (i in seq_along(Global_Search_categories)) {
    matrix_name <- names(Global_Search_categories[i])
    ME <- matrix_evaluation
    matrix_evaluation <- Global_Search_categories[[i]]
    # PROMETHEE
    RS <- PROMETHEEII(matrix_evaluation, data_criteria)
    data <- as.data.frame(RS[[1]])
    data$Phi <- as.numeric(data$Phi)
    data <- data[!grepl("^r\\d+", data$Alternative), ]
    local_pos <- data[data$Phi >= 0, ]
    local_neg <- data[data$Phi < 0, ]
    Local1_categories[[paste0("LocalSearch1 ", matrix_name, "(+)")]] <- local_pos
    Local1_categories[[paste0("LocalSearch1 ", matrix_name, "(-)")]] <- local_neg
    matrix_evaluation <- ME
  }
  # Step 4. Local search 2
  Local2_categories <- list()
  for (i in 1:(num_categories - 1)) {
    category_higher <- paste0("LocalSearch1 C", i, "(-)")
    category_lower <- paste0("LocalSearch1 C", i + 1, "(+)")
    matrix_higher <- Local1_categories[[category_higher]]
    matrix_lower <- Local1_categories[[category_lower]]
    Alt_higher <- matrix_higher$Alternative
    Alt_lower <- matrix_lower$Alternative
    Alt_filtered <- c(Alt_higher, Alt_lower)
    ME <- matrix_evaluation
    matrix_evaluation <- matrix_evaluation[matrix_evaluation$Alternative %in% Alt_filtered, ]
    # PROMETHEE
    RS <- PROMETHEEII(matrix_evaluation, data_criteria)
    data <- as.data.frame(RS[[1]])
    data$Phi <- as.numeric(data$Phi)
    local_pos <- data[data$Phi >= 0, ]
    local_neg <- data[data$Phi < 0, ]
    Local2_categories[[paste0("LocalSearch2 C", i, "- & C", i + 1, "+ (+)")]] <- local_pos
    Local2_categories[[paste0("LocalSearch2 C", i, "- & C", i + 1, "+ (-)")]] <- local_neg
    matrix_evaluation <- ME
  }
  # Step 5. Final classification
  Classifica <- data.frame(Alternative = matrix_evaluation$Alternative, Category = NA)
  # Classification: C1, Ck, and other groups.
  for (i in seq_along(Global_Search_categories)) {
    if (i == 1) {
      matrix_higher <- Local1_categories[["LocalSearch1 C1(+)"]]
      matrix_lower <- Local2_categories[["LocalSearch2 C1- & C2+ (+)"]]
      Alt_higher <- matrix_higher$Alternative
      Alt_lower <- matrix_lower$Alternative
      Alt_filtered <- c(Alt_higher, Alt_lower)
      Classifica$Category[Classifica$Alternative %in% Alt_filtered] <- i
    } else if (i == length(Global_Search_categories)) {
      matrix_higher <- Local1_categories[[paste0("LocalSearch1 C", i, "(-)")]]
      matrix_lower <- Local2_categories[[paste0("LocalSearch2 C", i - 1, "- & C", i, "+ (-)")]]
      Alt_higher <- matrix_higher$Alternative
      Alt_lower <- matrix_lower$Alternative
      Alt_filtered <- c(Alt_higher, Alt_lower)
      Classifica$Category[Classifica$Alternative %in% Alt_filtered] <- i
    } else {
      matrix_higher <- Local2_categories[[paste0("LocalSearch2 C", i - 1, "- & C", i, "+ (-)")]]
      matrix_lower <- Local2_categories[[paste0("LocalSearch2 C", i, "- & C", i + 1, "+ (+)")]]
      Alt_higher <- matrix_higher$Alternative
      Alt_lower <- matrix_lower$Alternative
      Alt_filtered <- c(Alt_higher, Alt_lower)
      Classifica$Category[Classifica$Alternative %in% Alt_filtered] <- i
    }
  }
  RS <- list(
    Global = Global_Search,
    Local1 = Local1_categories,
    Local2 = Local2_categories,
    Class = Classifica
  )
  return(RS)
}
