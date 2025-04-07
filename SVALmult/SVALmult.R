#' @title Substantive validity: set of items
#' @description For a dataframe of items, this function calculates substantive coefficients ('psa' and 'svc' coefficients; Anderson & Garbin, 1991), more asymmetric confidence intervals.
#' @param data dataframe with the responses to several items. Each response represents a chosen construct, usually forming a multinomial variable.
#' @param columns vector of indexes or column names to be analyzed.
#' @param conf.level confidence level for asymmetric confidence intervals (ex., .90, .95, .99)
#' 
#' @return
#' A named list where each element corresponds to one of the analyzed items. Each element contains two data frames:
#' \itemize{
#'   \item \code{psa_results}: A data frame with the following columns for each category (construct):
#'     \describe{
#'       \item{\code{Cat}}{The category value (construct code).}
#'       \item{\code{Freq}}{The frequency of responses in that category.}
#'       \item{\code{psa}}{The proportion of substantive agreement.}
#'       \item{\code{lwr.ci}}{Lower bound of the Wilson confidence interval for \code{psa}.}
#'       \item{\code{up.ci}}{Upper bound of the Wilson confidence interval for \code{psa}.}
#'     }
#'
#'   \item \code{csv_results}: A data frame with the following columns for each category:
#'     \describe{
#'       \item{\code{Cat}}{The category value (construct code).}
#'       \item{\code{Freq}}{The frequency of responses in that category.}
#'       \item{\code{csv}}{The substantive validity coefficient.}
#'       \item{\code{lwr.ci}}{Lower bound of the Wilson confidence interval for \code{csv}.}
#'       \item{\code{up.ci}}{Upper bound of the Wilson confidence interval for \code{csv}.}
#'     }
#' }
#' 
#' @details
#' The function analyzes substantive validity using the approach of Anderson and Gerbin (1991).
#' #' The function evaluates the substantive validity of items based on the method by Anderson and Gerbing (1991). 
#' It estimates:
#' \itemize{
#'   \item \strong{psa}: Proportion of responses in favor of the target category.
#'   \item \strong{svc}: Substantive validity coefficient, calculated by comparing the frequency of the target category with the average of the non-target categories.
#' }
#' 
#' Both coefficients are calculated for each construct, in order
#' to compare the construct of interest (target construct) with the rest of the constructs
#' (non-target constructs).
#' Each calculated 'psa' and 'svc' is supplemented with asymmetric confidence intervals
#' (Penfield & Giacobbi, 2004; Wilson, 1927).
#' The input vector for 'item' can come from a 'data.frame', where the rows are the
#' participants (experiential judges or expert judges), and the columns are the
#' constructs evaluated. Each numerical value in a column represents the construct 
#' chosen. For example, if the study assesses substantive validity by comparing 
#' the target construct with 3 other constructs, then the distinct values may 
#' be: 1, 2, 3, 4. However, it can be some numerical sequence with distinct 
#' values (e.g., 2, 4, 10, 6).
#' Examples of results obtained with this procedure can be found in Cabedo-Peris 
#' et al. (2024), and Merino et al. (2021).
#' 
#' \strong{Note}: The function does not handle missing values. Users must impute or remove NAs before using this function.
#' \strong{Interpretation of Negative CSV Values}: Negative values of the \code{csv} coefficient are valid and interpretable. They indicate that the selected category was chosen less frequently than the average of the non-target categories, suggesting low substantive agreement for that construct.
#' 
#' @references
#' Anderson, J. C., & Gerbing, D. W. (1991). Predicting the performance of measures 
#' in a confirmatory factor analysis with a pretest assessment of their substantive 
#' validities. Journal of Applied Psychology, 76(5), 732–740. https://doi.org/10.1037/0021-9010.76.5.732
#' 
#' Cabedo-Peris, J., Merino-Soto, C., Chans, G.M., & Marti-Vilar, M. (2024). Exploring 
#' the Loss Aversion Scale’s psychometric properties in Spain. Scientific Reports, 
#' 14, 15756. https://doi.org/10.1038/s41598-024-66695-6
#' 
#' Merino-Soto,C., Calderón-De la Cruz,G., Gil-Monte,P., Juárez-García,A.(2021). 
#' Validez sustantiva en el marco de la validez de contenido: Aplicación en la 
#' escala de Carga de Trabajo. Revista Argentina de Ciencias del Comportamiento, 
#' 13(1), 81-92. https://revistas.unc.edu.ar/index.php/racc/article/view/20547/33426
#' 
#' Penfield, R. D. & Giacobbi, P. R., Jr. (2004) Applying a score confidence 
#' interval to Aiken’s item content-relevance index. Measurement in Physical Education 
#' and Exercise Science, 8(4), 213-225. https://doi.org/10.1207/s15327841mpee0804_3
#' 
#' Wilson, E. B. (1927). Probable inference, the law of succession, and statistical
#'  inference. Journal of the American Statistical Association, 22, 209-212. https://doi.org/10.2307/2276774
#' 
#' @seealso
#' \code{\link{PropCIs::scoreci}} for score method confidence interval
#' \code{\link{ValCont::SVALsingle}} for substantive validity for single items
#' 
#' @examples
#' ### Example 1 -----------------------
#' data.gais <- data.frame(
#'  gais2 = c(4,4,4,4,4,2,3,2,4,4,3,4,4,3,4,4,2,1,2,3,4,3,4,4,2,3,4,4,4),
#'  gais3 = c(4,4,4,4,4,1,4,2,1,4,3,4,4,3,4,4,4,1,2,3,3,3,2,4,4,3,4,4,4),
#'  gais4 = c(4,4,4,4,4,1,1,1,1,2,4,4,1,1,4,4,2,1,2,3,1,3,4,4,2,4,2,4,4),
#'  gais5 = c(4,4,4,2,4,1,2,1,1,4,2,1,4,4,4,4,2,1,2,3,3,3,4,1,1,4,1,3,4))
#'  
#' ## Run 
#' SVALmult(data = data.gais, columns = c("gais2", "gais3"), conf.level = .90)
#' 
#' SVALmult(data = data.gais, columns = c(1,2,3), conf.level = .90)
#' 
#' @author
#' Cesar Merino-Soto (\email: {sikayax@yahoo.cam.ar})
#' 
#' @keywords
#' 
#' @export
SVALmult <- function(data, columns, conf.level = 0.95) {
  # Subfunción para calcular intervalos de confianza con el método de Wilson
  scoreci <- function(x, n, conf.level) {
    zalpha <- abs(qnorm((1 - conf.level) / 2))
    phat <- x / n
    bound <- (zalpha * sqrt((phat * (1 - phat) + (zalpha^2) / (4 * n)) / n)) /
      (1 + (zalpha^2) / n)
    midpnt <- (phat + (zalpha^2) / (2 * n)) / (1 + (zalpha^2) / n)
    
    up.ci <- round(midpnt + bound, digits = 3)
    lwr.ci <- round(midpnt - bound, digits = 3)
    
    return(data.frame(lwr.ci = lwr.ci, up.ci = up.ci))
  }
    # Subfunción interna para calcular PSA y CSV
  vsub_sing_internal <- function(item, conf.level) {
    # Crear tabla de frecuencias
    freq_table <- as.data.frame(table(item))
    colnames(freq_table) <- c("Cat", "Freq")
    freq_table$Cat <- as.numeric(as.character(freq_table$Cat))  # Asegurar categorías numéricas
    
    # Total de respuestas
    Ntotal <- sum(freq_table$Freq)
    
    # PSA
    psa_results <- freq_table
    psa_results$psa <- round(freq_table$Freq / Ntotal, 3)
    psa_ci <- scoreci(freq_table$Freq, Ntotal, conf.level)
    psa_results <- cbind(psa_results, psa_ci)
    
    # CSV
    csv_results <- freq_table
    csv_results$csv <- round((freq_table$Freq - (Ntotal - freq_table$Freq)) / Ntotal, 3)
    
    # Evitar errores en el intervalo de confianza con magnitud
    csv_abs <- abs(csv_results$csv)
    csv_ci <- scoreci(csv_abs * Ntotal, Ntotal, conf.level)  # Usar valor absoluto para CI
    csv_ci <- csv_ci * sign(csv_results$csv)  # Restaurar el signo en los límites
    
    csv_results <- cbind(csv_results, csv_ci)
    
    # Resultados para PSA y CSV
    return(list(psa_results = psa_results, csv_results = csv_results))
  }
  
  # Si columns son índices, convertirlos a nombres
  if (is.numeric(columns)) {
    columns <- colnames(data)[columns]
  }
  
  # Aplicar la subfunción a cada columna especificada
  results <- lapply(columns, function(col) {
    item <- data[[col]]
    vsub_sing_internal(item, conf.level)
  })
  
  # Nombrar los resultados con los nombres de las columnas
  names(results) <- columns
  
  return(results)
}

# Subfunción para calcular intervalos de confianza con el método de Wilson
scoreci <- function(x, n, conf.level) {
  zalpha <- abs(qnorm((1 - conf.level) / 2))
  phat <- x / n
  bound <- (zalpha * sqrt((phat * (1 - phat) + (zalpha^2) / (4 * n)) / n)) /
    (1 + (zalpha^2) / n)
  midpnt <- (phat + (zalpha^2) / (2 * n)) / (1 + (zalpha^2) / n)
  
  up.ci <- round(midpnt + bound, digits = 3)
  lwr.ci <- round(midpnt - bound, digits = 3)
  
  return(data.frame(lwr.ci = lwr.ci, up.ci = up.ci))
}

