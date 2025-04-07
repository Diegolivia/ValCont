#' @title Aiken's V Coefficient
#' @description Calculate Aiken's V coefficient for content validity with asymmetric confidence intervals.
#'
#' @param data dataframe, with the columns assigned to each rater, and the rows assigned to each evaluated item.
#' @param min numeric. Minimum possible rating value in the scale used by the judges.
#' @param max numeric. Maximum possible rating value in the scale used by the judges.
#' @param conf.level numeric. Confidence level for the confidence intervals (e.g., .90, .95, .99).
#'
#' @return
#' A dataframe with the V coefficients for all analyzed items, and their corresponding asymmetric confidence intervals.
#'
#' @details
#' This function calculates Aiken's V coefficient (Aiken, 1980, 1985) using the modified formula by Penfield & Giacobbi (2004),
#' and computes asymmetric confidence intervals based on Wilson's method. It provides results for multiple items simultaneously.
#' The function assumes that the dataset contains no missing values.
#'
#' @references
#' Aiken, L. R. (1980). Content validity and reliability of single items or questionnaires. *Educational and Psychological Measurement, 40*, 955–959. https://doi.org/10.1177/001316448004000419
#'
#' Aiken, L. R. (1985). Three coefficients for analyzing the reliability and validity of ratings. *Educational and Psychological Measurement, 45*, 131–142. https://doi.org/10.1177/0013164485451012
#'
#' Merino, C., & Livia, J. (2009). Intervalos de confianza asimétricos para el índice de validez de contenido: un programa Visual Basic para la V de Aiken. *Anales de Psicología, 25*(1), 169–171.
#'
#' Penfield, R. D., & Giacobbi, P. R. Jr. (2004). Applying a score confidence interval to Aiken’s item content-relevance index. *Measurement in Physical Education and Exercise Science, 8*(4), 213–225. https://doi.org/10.1207/s15327841mpee0804_3
#'
#' Wilson, E. B. (1927). Probable inference, the law of succession, and statistical inference. *Journal of the American Statistical Association, 22*, 209–212. https://doi.org/10.2307/2276774
#'
#' @seealso
#' \code{\link[PropCIs]{scoreci}} for score method confidence intervals, 
#' \code{\link[ValCont]{Haiken}} for score homogeneity of ratings.
#'
#' @examples
#' ### Example 1: Good content validity ---------------
#' data.ej <- data.frame(
#'   j1 = c(4, 1, 1, 1, 4),
#'   j2 = c(4, 1, 2, 2, 3),
#'   j3 = c(4, 1, 3, 3, 5),
#'   j4 = c(4, 1, 4, 5, 5),
#'   j5 = c(4, 1, 5, 5, 5),
#'   j6 = c(4, 1, 3, 5, 5))
#'   
#' rownames(data.ej) <- paste0("Item_", 1:5)
#' 
#' Run
#' Vaiken(data = data.ej, min = 1, max = 5, conf.level = .90)
#'
#' ### Example 2: Poor content validity ---------------
#' data.low <- data.frame(
#'   j1 = c(1, 2, 2, 3, 5),
#'   j2 = c(1, 3, 2, 1, 5),
#'   j3 = c(2, 1, 3, 1, 5),
#'   j4 = c(2, 1, 2, 1, 5),
#'   j5 = c(1, 4, 2, 2, 5))
#'   
#'rownames(data.low) <- c("Item_A", "Item_B", "Item_C", "Item_D", "Item_E")
#'
#' # Run
#' Vaiken(data = data.low, min = 1, max = 5, conf.level = .95)
#'
#' @author
#' Diego Livia-Ortiz (\email{diegolivia@hotmail.com})  
#' Cesar Merino-Soto (\email{sikayax@yahoo.com.ar})
#'
#' @export
Vaiken <- function(data, min, max, conf.level = 0.95) {
  # Validation
  if (!is.data.frame(data)) stop("'data' must be a data.frame.")
  if (!is.numeric(as.matrix(data))) stop("'data' must contain only numeric values.")
  if (!is.numeric(min) || !is.numeric(max)) stop("'min' and 'max' must be numeric values.")
  if (min >= max) stop("'min' must be less than 'max'.")
  if (!is.numeric(conf.level) || conf.level <= 0 || conf.level >= 1) stop("'conf.level' must be between 0 and 1.")
  if (any(is.na(data))) stop("'data' contains missing values. Please remove or impute them.")
  if (any(data < min | data > max)) stop("All scores must be between 'min' and 'max'.")
  
  n <- ncol(data)
  z <- qnorm(1 - (1 - conf.level) / 2)
  
  calcular_valores <- function(puntajes) {
    M <- mean(puntajes)
    V <- (M - min) / (max - min)
    nk <- n * (max - min)
    term1 <- 2 * nk * V + z^2
    term2 <- z * sqrt(4 * nk * V * (1 - V) + z^2)
    denom <- 2 * (nk + z^2)
    lwr.ci <- (term1 - term2) / denom
    upr.ci <- (term1 + term2) / denom
    c(V = V, lwr.ci = lwr.ci, upr.ci = upr.ci)
  }
  
  resultados <- t(apply(data, 1, calcular_valores))
  
  item_names <- if (!is.null(rownames(data)) && all(rownames(data) != "")) {
    rownames(data)
  } else {
    paste0("Item_", seq_len(nrow(data)))
  }
  
  resultados_df <- data.frame(
    Item = item_names,
    V = round(resultados[, "V"], 3),
    lwr.ci = round(resultados[, "lwr.ci"], 3),
    upr.ci = round(resultados[, "upr.ci"], 3),
    row.names = NULL
  )
  
  return(resultados_df)
}
