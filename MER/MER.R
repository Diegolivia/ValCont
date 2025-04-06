#' @title  Mean of expert ratings (MER)
#' @description Calculate the average score, with asymmetric confidence intervals.
#' @param data dataframe, with the columns assigned to each judge, and the rows assigned to each evaluated item.
#' @param ncat Number of possible values or categories used in rating
#' @param start Minimum possible value (0 or 1)
#' @param conf.level Confidence level for confidence intervals (ex., .90, .95, .99)
#' 
#' @return
#' dataframe with MERs for all items analyzed, and confidence intervals.
#' 
#' @details
#' Calculate the average rating of the judges for each item, based on the proposal of 
#' Penfield & Miller (2004), and asymmetric confidence intervals (Wilson, 1927; Penfield, 2003; 
#' Penfield & Miller, 2004). MER' is a modification of the two syntax previous (Merino-Soto & 
#' Livia-Segovia, 2022; Penfield, & Miller, 2004).
#' Due to the usual small number of participants in content validity studies, asymmetric 
#' confidence intervals may be an optimal approach to inferentially assess item validity. 
#' The results should be supplemented with an estimator of inter-judge variability or agreement.
#' 
#' Note: The function has not yet been prepared to resolve missing values, so the user must remove or impute any missing values.
#' 
#' @references
#' Aiken, L. R. (1980). Content validity and reliability of single items or questionnaires. Educational and. Psychological Measurement, 40, 955-959. doi: 10.1177/001316448004000419
#' 
#' Aiken, L. R. (1985). Three coefficients for analyzing the reliability and validity of ratings. Educational and Psychological Measurement, 45, 131-142. doi: 10.1177/0013164485451012
#' 
#' Merino-Soto, C., & Livia-Segovia, J. (2022). Rating mean of expert judges and asymmetric confidence intervals in content validity: an SPSS syntax. Anales de Psicología, 38(2), 395-398. https://dx.doi.org/10.6018/analesps.489431
#' 
#' Miller, J. M., & Penfield, R. D. (2005). Using the score method to construct asymmetric confidence intervals: An SAS program for content validation in scale development. Behavior Research Methods, 37, 450-452. https://doi.org/10.3758/BF03192713
#' 
#' Penfield, R. D. (2003). A score method of constructing asymmetric confidence intervals for the mean of a rating scale item. Psychological methods, 8(2), 149-163. https://doi.org/10.1037/1082-989x.8.2.149 [ Links ] 
#' 
#' Penfield, R. D. & Giacobbi, P. R., Jr. (2004) Applying a score confidence interval to Aiken’s item content-relevance index. Measurement in Physical Education and Exercise Science, 8(4), 213-225. doi: 10.1207/s15327841mpee0804_3
#' 
#' Penfield, R. D., & Miller, J. M. (2004). Improving Content Validation Studies Using an Asymmetric Confidence Interval for the Mean of Expert Ratings. Applied Measurement in Education, 17(4), 359–370. https://doi.org/10.1207/s15324818ame1704_2
#' 
#' Wilson, E. B. (1927). Probable inference, the law of succession, and statistical inference. Journal of the American Statistical Association, 22, 209-212. doi: 10.2307/2276774
#' 
#' @seealso
#' \code{\link{ValCont::Haiken}} for a homogenety coefficient
#' 
#' @examples
#' 
### Example 1 -------------------
#' # Load data
#' 
#' # Run
#' mean_rating(data = ej1, ncat = 6, start = 1, conf.level = .90)
#' 
#' 
#' ### Example 2 -------------------
#' 
#' 
#' @author
#' Cesar Merino-Soto (\email: {sikayax@yahoo.com.ar})
#' 
#' @keywords
#' 
#' @export
MER <- function(data, ncat, start, conf.level = 0.90) {
  # Verificaciones iniciales
  if (!is.data.frame(data)) stop("'data' debe ser un data.frame")
  if (!is.numeric(ncat) || ncat <= 1) stop("'ncat' debe ser un numero mayor que 1")
  if (!is.numeric(start) || !start %in% c(0, 1)) stop("'start' debe ser 0 o 1")
  if (!is.numeric(conf.level) || conf.level <= 0 || conf.level >= 1) {
    stop("'conf.level' debe estar entre 0 y 1")
  }

  # Ajuste del nivel de confianza
  alpha <- 1 - conf.level
  
  # C?lculo de la media y del intervalo de confianza para cada ?tem
  results <- apply(data, 1, function(item_ratings) {
    mean_rating <- mean(item_ratings) # Media del ?tem
    n <- length(item_ratings)        # N?mero de jueces
    sd_rating <- sd(item_ratings)    # Desviaci?n est?ndar
    
    # C?lculo de los l?mites del IC asim?trico
    error_margin <- qt(1 - alpha / 2, df = n - 1) * (sd_rating / sqrt(n))
    lwr <- max(mean_rating - error_margin, start) # L?mite inferior ajustado al m?nimo posible
    upr <- min(mean_rating + error_margin, start + ncat - 1) # L?mite superior ajustado al m?ximo posible
    
    c(MER = round(mean_rating, 3), 
      lwr.ci = round(lwr, 3), 
      upr.ci = round(upr, 3))
  })
  
  # Transformar los resultados en un data.frame
  results_df <- as.data.frame(t(results))
  results_df$Item <- seq_len(nrow(results_df)) # Agregar la columna 'Item'
  results_df <- results_df[, c("Item", "MER", "lwr.ci", "upr.ci")] # Reordenar columnas
  
  return(results_df)
}
