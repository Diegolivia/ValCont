#' @title CVC
#' @description Calculates the content validity coefficient (CVC; Hernandez-Nieto, 2002). CVC makes an adjustment for random response.
#' @param data dataframe, with n columns (judges or scorers), and k rows (evaluated items).
#' @param max maximum possible rating value used.
#' @param conf.level confidence level for confidence intervals (eg., .90, .95, .99).
#' 
#' @return dataframe with CVC coefficients and confidence intervals.
#' 
#' @details
#' This function calculates the content validity coefficient CVC (Hernandez-Nieto, 2002). Asymmetric confidence intervals
#' are also calculated (Wilson, 1927; Penfield & Giacobbi, 2004). CVC' is the second coefficient that adjusts for 
#' possible random response of the raters, while another proposal for the CVI coefficient was created by
#'  Polit, & Beck (2007).
#' 
#' Note: The function has not yet been prepared to resolve missing values, so the user must remove or impute any missing values.
#' 
#' @references
#' Hernández-Nieto, R. A. (2002). Contributions to Statistical Analysis. Mérida, Venezuela: Universidad de Los Andes.
#' 
#' Penfield, R. D. & Giacobbi, P. R., Jr. (2004) Applying a score confidence interval to Aiken’s item content-relevance index. Measurement in Physical Education and Exercise Science, 8(4), 213-225. https://doi.org/10.1207/s15327841mpee0804_3
#' 
#' Polit DF, Beck CT, Owen SV.  Is the CVI an acceptable indicator of content validity?  Appraisal and recommendations. Research in Nursing & Health. 2007;30(4):459–67. https://doi.org/10.1002/nur.20199
#' 
#' Wilson, E. B. (1927). Probable inference, the law of succession, and statistical inference. Journal of the American Statistical Association, 22, 209-212. https://doi.org/10.2307/2276774
#' 
#' @seealso
#' \code{\link{PropCIs::scoreci}} for score method confidence interval
#' 
#' @examples
#' ### Example 1 ----------
#' ### Fictitious data (Ex1.txt): 15 judges, and 15 items evaluated by the judges, 
#' no missing data, no column headers.
#' 
#' ## Load data.
#' data(Ej1)
#' 
#' ## Run CVC.
#' CVC(Ej1, conf.level = .90)
#' 
#' ### Example 2 ----------
#' ## Artificial data 
#' random_data <- data.frame(replicate(n = 6,sample(1:5,12,rep=TRUE)))
#' 
#' ## Graph data
#' barplot(random_data)
#' 
#' ## Run CVC
#' CVC(random_data, conf.level = .90)
#' 
#' ### Example 3 ----------
#' ## Save data
#' Ej <- data.frame(
#' j1 = c(4, 1, 1, 1, 4),
#' j2 = c(4, 1, 2, 2, 3),
#' j3 = c(4, 1, 3, 3, 5),
#' j4 = c(4, 1, 4, 5, 5),
#' j5 = c(4, 1, 5, 5, 5),
#' j6 = c(4, 1, 3, 5, 5))
#' 
#' ## RUN
#' CVC(Ej, conf.level = .90)
#' 
#' @author
#' Cesar Merino-Soto (\email{sikayax@yahoo.com.ar})
#' 
#' @keywords
#' 
#' @export
CVC <- function(data, max, conf.level) {
  # Verificar si el data.frame contiene solo valores numéricos
  if (!all(sapply(data, is.numeric))) {
    stop("El data.frame debe contener solo valores numéricos.")
  }
  
  # Número de jueces (columnas)
  num_jueces <- ncol(data)
  
  # Calcular Pe según la fórmula dada
  Pe <- (1 / num_jueces) ^ num_jueces
  
  # Calcular la media de cada ítem (fila)
  medias_items <- rowMeans(data)
  
  # Calcular el CVC para cada ítem
  CVC_items <- round((medias_items - Pe) / max, 3)
  
  # Calcular los intervalos de confianza de Wilson para cada CVC

  get_wilson_CI <- function(x, n, conf.level) {
    p_hat <- x
    SE_hat_sq <- p_hat * (1 - p_hat) / n
    crit <- qnorm(1 - (1 - conf.level) / 2)
    omega <- n / (n + crit^2)
    A <- p_hat + crit^2 / (2 * n)
    B <- crit * sqrt(SE_hat_sq + crit^2 / (4 * n^2))
    CI <- c('lower' = omega * (A - B), 
            'upper' = omega * (A + B))
    return(CI)
  }
    intervalos_CI <- t(sapply(CVC_items, get_wilson_CI, n = num_jueces, conf.level = conf.level))
  
  # Crear un data.frame con los resultados
  resultado_df <- data.frame(
    Item = 1:nrow(data),
    CVC = round(CVC_items, 3),
    lwr.ci = round(intervalos_CI[, "lower"],3),
    upr.ci = round(intervalos_CI[, "upper"],3)
  )
  return(resultado_df)
}