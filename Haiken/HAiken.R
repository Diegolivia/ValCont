#' @title H coefficient for homogeneity of response
#' @description Calculate the coefficient of homogeneity of response for each item (Aiken, 1980, 1985).
#' 
#' @param data dataframe, with the columns assigned to each judge, and the rows assigned to each evaluated item.
#' @param ncat number of response categories or options used in the rating.
#' @param conf.level confidence level for the confidence intervals (e.g., .90, .95, .99).
#' 
#' @return 
#' A dataframe with H coefficients for all items analyzed, and their confidence intervals.
#' 
#' @details
#' Compute the H coefficient (Aiken, 1980, 1985) to estimate the homogeneity of response of the judges/scorers to the items. 
#' To maintain consistency with the methods usually associated with content validity, 'HAiken' is proposed as an option.
#' 'HAiken' also computes asymmetric confidence intervals using the method of Wilson (1927), adapted by Penfield and 
#' Giacobbi (2004) for Aiken's V coefficient.
#'
#' The H coefficient can theoretically range from 0 (maximum disagreement among judges) to 1 (perfect agreement). 
#' However, due to the nature of the pairwise differences used in its calculation, the H value may occasionally fall 
#' below 0 in cases of extreme disagreement. These negative values should be interpreted as indicating very low 
#' homogeneity and are retained for transparency in exploratory analyses.
#'
#' In such cases, the asymmetric confidence interval (Wilson method) cannot be computed, since the H coefficient 
#' falls outside the bounds of a valid proportion (0–1). When this occurs, the confidence interval will be returned 
#' as NA. This behavior is expected and consistent with the statistical properties of the method.
#'
#' Users are advised to interpret such results as indicators of extremely low agreement among raters. If desired, 
#' H values can be truncated to the 0–1 range, but this should be done with caution and transparency.
#'
#' Note: The function does not currently handle missing values. Please remove or impute missing data before using it.
#' 
#' @references
#' Aiken, L. R. (1980). Content validity and reliability of single items or questionnaires. Educational and Psychological 
#' Measurement, 40, 955–959. https://doi.org/10.1177/001316448004000419
#' 
#' Aiken, L. R. (1985). Three coefficients for analyzing the reliability and validity of ratings. Educational and
#' Psychological Measurement, 45, 131–142. https://doi.org/10.1177/0013164485451012
#' 
#' Penfield, R. D., & Miller, J. M. (2004). Improving Content Validation Studies Using an Asymmetric Confidence Interval 
#' for the Mean of Expert Ratings. Applied Measurement in Education, 17(4), 359–370. https://doi.org/10.1207/s15324818ame1704_2
#' 
#' Wilson, E. B. (1927). Probable inference, the law of succession, and statistical inference. Journal of the American
#' Statistical Association, 22, 209–212. https://doi.org/10.2307/2276774
#' 
#' @seealso
#' \code{\link[PropCIs]{scoreci}} for score method confidence interval  
#' \code{\link[irr]{agree}} for Simple and extended percentage agreement  
#' \code{\link[irr]{kendall}} for Kendall's coefficient of concordance W  
#' \code{\link[irr]{meanrho}} for Mean of bivariate rank correlations between raters
#' 
#' @examples
#' Ej2 <- data.frame(
#'   j1 = c(4, 1, 1, 1, 4),
#'   j2 = c(4, 1, 2, 2, 3),
#'   j3 = c(4, 1, 3, 3, 5),
#'   j4 = c(4, 1, 4, 5, 5),
#'   j5 = c(4, 1, 5, 5, 5),
#'   j6 = c(4, 1, 3, 5, 5))
#' 
#' HAiken(Ej2, ncat = 5, conf.level = .90)
#' 
#' # If rows are judges and columns are items:
#' HAiken(as.data.frame(t(Ej2)), ncat = 5, conf.level = .90)
#' 
#' @author
#' Cesar Merino-Soto (\email{sikayax@yahoo.com.ar})
#' 
#' @keywords
#' content validity, homogeneity, agreement
#' 
#' @importFrom stats qnorm
#' 
#' @export
Haiken <- function(data, ncat, conf.level) {
  
  # Validations
  if (!is.data.frame(data)) stop("The 'data' argument must be a dataframe.")
  if (!is.numeric(ncat) || ncat < 2) stop("'ncat' must be a number greater than or equal to 2.")
  if (!is.numeric(conf.level) || conf.level <= 0 || conf.level >= 1) stop("'conf.level' must be a number between 0 and 1.")
  if (anyNA(data)) warning("Missing values detected. Please remove or impute missing data before using HAiken().")
  
  num_items <- nrow(data)
  n <- ncol(data)  # number of raters
  k <- ncat
  j <- ifelse(n %% 2 == 0, 0, 1)
  
  resultados <- data.frame(
    Item = 1:num_items,
    H = numeric(num_items),
    lwr.ci = numeric(num_items),
    upr.ci = numeric(num_items)
  )
  
  get_wilson_CI <- function(p_hat, n, conf.level) {
    SE_hat_sq <- p_hat * (1 - p_hat) / n
    crit <- qnorm(1 - conf.level / 2)
    omega <- n / (n + crit^2)
    A <- p_hat + crit^2 / (2 * n)
    B <- crit * sqrt(SE_hat_sq + crit^2 / (4 * n^2))
    CI <- c('lower' = omega * (A - B), 
            'upper' = omega * (A + B))
    return(CI)
  }
  
  for (i in 1:num_items) {
    fila <- data[i, ]
    
    diff_abs <- combn(colnames(fila), 2, function(pair) {
      abs(fila[[pair[1]]] - fila[[pair[2]]])
    }, simplify = TRUE)
    
    H <- 1 - (4 * sum(diff_abs)) / ((k - 1) * (n^2 - j))
    
    # Evaluar si el IC puede calcularse
    if (H < 0 || H > 1) {
      wilson_interval <- c(lower = NA, upper = NA)
    } else {
      wilson_interval <- get_wilson_CI(H, n, conf.level)
    }
    
    resultados[i, "H"] <- H
    resultados[i, "lwr.ci"] <- wilson_interval["lower"]
    resultados[i, "upr.ci"] <- wilson_interval["upper"]
  }
  
  return(round(resultados, 3))
}

