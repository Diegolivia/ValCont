#' @title  Mean of Expert Ratings (MER)
#' @description Calculates the mean rating of expert judges for each item, with asymmetric confidence intervals based on the Wilson score method adapted to rating scales.
#'
#' @param data A data.frame where columns represent judges and rows represent the items evaluated.
#' @param ncat Integer. Total number of ordinal rating categories (e.g., 5 or 6).
#' @param start Numeric. Minimum possible value of the scale (commonly 0 or 1).
#' @param conf.level Confidence level for the intervals (e.g., 0.90, 0.95, 0.99).
#'
#' @return A data.frame containing:
#' \itemize{
#'   \item \code{Item}: Item number.
#'   \item \code{MER}: Mean expert rating for each item.
#'   \item \code{lwr.ci}, \code{upr.ci}: Lower and upper bounds of the asymmetric confidence interval.
#' }
#'
#' @details
#' The function calculates the average rating given by a panel of judges for each item and computes an asymmetric confidence interval using the Wilson score interval adapted for ordinal scales (Penfield & Miller, 2004). 
#'
#' First, the mean is transformed to a proportion relative to the rating scale. The Wilson interval is then calculated on this proportion and re-scaled to the original scale of the items.
#'
#' This approach provides more accurate inference when the number of judges is small, as is typical in content validity studies. The MER and its confidence bounds help identify items with stronger or weaker expert consensus. Results should be interpreted alongside a measure of inter-rater agreement or variability.
#'
#' \strong{Note:} This function does not handle missing values. Users must ensure complete data (e.g., via imputation or listwise deletion) prior to using the function.
#'
#' @references
#' Aiken, L. R. (1980). Content validity and reliability of single items or questionnaires. \emph{Educational and Psychological Measurement, 40}, 955–959. https://doi.org/10.1177/001316448004000419
#'
#' Aiken, L. R. (1985). Three coefficients for analyzing the reliability and validity of ratings. \emph{Educational and Psychological Measurement, 45}, 131–142. https://doi.org/10.1177/0013164485451012
#'
#' Merino-Soto, C., & Livia-Segovia, J. (2022). Rating mean of expert judges and asymmetric confidence intervals in content validity: an SPSS syntax. \emph{Anales de Psicología, 38}(2), 395–398. https://doi.org/10.6018/analesps.489431
#'
#' Penfield, R. D., & Giacobbi, P. R. Jr. (2004). Applying a score confidence interval to Aiken’s item content-relevance index. \emph{Measurement in Physical Education and Exercise Science, 8}(4), 213–225. https://doi.org/10.1207/s15327841mpee0804_3
#'
#' Penfield, R. D., & Miller, J. M. (2004). Improving content validation studies using an asymmetric confidence interval for the mean of expert ratings. \emph{Applied Measurement in Education, 17}(4), 359–370. https://doi.org/10.1207/s15324818ame1704_2
#'
#' Wilson, E. B. (1927). Probable inference, the law of succession, and statistical inference. \emph{Journal of the American Statistical Association, 22}, 209–212. https://doi.org/10.2307/2276774
#'
#' @seealso
#' \code{\link{ValCont::Haiken}} for a homogeneity index
#' \code{\link{ValCont::Vaiken}} for a V index
#'
#' @examples
#' ### Example 1: High MER values -------------------
#' # Simulated data: 10 judges rated 6 items from 4 to 5 (high consensus, high relevance)
#' set.seed(123)
#' ratings_high <- data.frame(
#'   Item1 = sample(4:5, 10, replace = TRUE),
#'   Item2 = sample(4:5, 10, replace = TRUE),
#'   Item3 = sample(4:5, 10, replace = TRUE),
#'   Item4 = sample(4:5, 10, replace = TRUE),
#'   Item5 = sample(4:5, 10, replace = TRUE),
#'   Item6 = sample(4:5, 10, replace = TRUE))
#'   
#' # Transpose dataframe
#' ratings_high <- t(ratings_high)  # Rows = items, Columns = judges
#' 
#' # Run
#' MER(data = as.data.frame(ratings_high), ncat = 5, start = 1, conf.level = .95)
#'
#' ### Example 2: Low MER values -------------------
#' # Simulated data: 10 judges rated 6 items from 1 to 2 (low relevance)
#' set.seed(456)
#' ratings_low <- data.frame(
#'   Item1 = sample(1:2, 10, replace = TRUE),
#'   Item2 = sample(1:2, 10, replace = TRUE),
#'   Item3 = sample(1:2, 10, replace = TRUE),
#'   Item4 = sample(1:2, 10, replace = TRUE),
#'   Item5 = sample(1:2, 10, replace = TRUE),
#'   Item6 = sample(1:2, 10, replace = TRUE))
#'
#' # Transpose dataframe
#' ratings_low <- t(ratings_low)  # Rows = items, Columns = judges
#'
#' # Run
#' MER(data = as.data.frame(ratings_low), ncat = 5, start = 1, conf.level = .95)
#'
#' @author
#' Cesar Merino-Soto (\email{sikayax@yahoo.com.ar})
#'
#' @export
MER <- function(data, ncat, start, conf.level = 0.90) {
  # Verificaciones iniciales
  if (!is.data.frame(data)) stop("'data' debe ser un data.frame")
  if (!is.numeric(ncat) || ncat <= 1) stop("'ncat' debe ser un número mayor que 1")
  if (!is.numeric(start) || !start %in% c(0, 1)) stop("'start' debe ser 0 o 1")
  if (!is.numeric(conf.level) || conf.level <= 0 || conf.level >= 1) {
    stop("'conf.level' debe estar entre 0 y 1")
  }
  
  # Nivel crítico z
  alpha <- 1 - conf.level
  z <- qnorm(1 - alpha / 2)
  
  # Aplicar a cada ítem (fila)
  results <- apply(data, 1, function(item_ratings) {
    mean_rating <- mean(item_ratings) # Media del ítem
    n <- length(item_ratings)         # Número de jueces
    
    # Paso 1: transformar a proporción
    p_hat <- (mean_rating - start) / (ncat - 1)
    
    # Paso 2: intervalo Wilson modificado
    z2_n <- z^2 / n
    denominator <- 1 + z2_n
    center <- p_hat + z2_n / 2
    margin <- z * sqrt((p_hat * (1 - p_hat)) / n + z^2 / (4 * n^2))
    
    lwr_p <- (center - margin) / denominator
    upr_p <- (center + margin) / denominator
    
    # Paso 3: volver a escalar al rango original
    lwr <- lwr_p * (ncat - 1) + start
    upr <- upr_p * (ncat - 1) + start
    
    c(MER = round(mean_rating, 3),
      lwr.ci = round(lwr, 3),
      upr.ci = round(upr, 3))
  })
  
  # Formatear resultados
  results_df <- as.data.frame(t(results))
  results_df$Item <- seq_len(nrow(results_df))
  results_df <- results_df[, c("Item", "MER", "lwr.ci", "upr.ci")]
  
  return(results_df)
}
