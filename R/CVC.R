#'@title CVC
#'@description Calculates the content validity coefficient (CVC; Hernandez-Nieto, 2002). CVC makes an adjustment for random response.
#'
#' @param data dataframe, with n columns (judges or scorers), and k rows (evaluated items).
#' @param max maximum possible rating value used.
#' @param min minimum possible rating value used (default = 1). Required for the MER method.
#' @param conf.level confidence level for confidence intervals (eg., .90, .95, .99).
#' @param na.rm Logical. If FALSE (default) the function stops when missing values are detected.
#'              If TRUE rows with missing values in the relevant columns are removed before processing.
#' @param overall Logical. If TRUE, computes an overall CVC.
#' @param overall.method Character. Method for the overall CVC:
#'        \itemize{
#'          \item \code{"global"}: treats the entire matrix as a single "super-item" (all ratings pooled).
#'          \item \code{"Hernandez"}: computes CVC for each item and then averages them (traditional approach).
#'        }
#' @param overall.ci Character. Method for the overall confidence interval:
#'        \itemize{
#'          \item \code{"MER"}: score interval for the mean of ratings, then transformed to CVC (Penfield, 2003). Recommended for \code{"global"}.
#'          \item \code{"Wilson"}: Wilson score interval directly on the CVC estimate.
#'        }
#'        Note: When \code{overall.method = "Hernandez"}, only \code{"Wilson"} is used (others ignored).
#'
#'@return dataframe with CVC coefficients and confidence intervals. If overall = TRUE, an extra row with the global index is included.
#'
#'@details
#'This function calculates the content validity coefficient CVC (Hernandez-Nieto, 2002). Asymmetric confidence intervals are also calculated (Wilson, 1927; Penfield & Giacobbi, 2004).
#'
#' **Overall CVC and confidence intervals**:
#' When `overall = TRUE`, the function computes a global CVC using one of two approaches:
#'
#' \itemize{
#'   \item With \code{overall.method = "global"}, the entire matrix of ratings is treated as a single "super-item". The workflow is:
#'         1. The matrix is flattened into a single vector (all items × all judges).
#'         2. The global mean of all ratings is computed.
#'         3. The random agreement correction (Pe) is applied using the original formula (1/J)^J.
#'         4. The global CVC is obtained as (Global Mean / Vmax) - Pe.
#'         5. For the confidence interval, the **MER method** is recommended because it respects the nature of the CVC as a linear transformation of the mean, avoids treating the CVC as an exact proportion when it is not, and is consistent with the MER approach already implemented for the global Aiken's V. The MER method first constructs an asymmetric confidence interval for the mean of all ratings (Penfield, 2003) and then transforms the lower and upper bounds to the CVC metric using the same linear transformation.
#'   \item With \code{overall.method = "Hernandez"}, the CVC is computed for each item and then averaged (traditional approach). The confidence interval is based on the Wilson score method applied to the mean CVC.
#' }
#'
#'@references
#'Hernandez-Nieto, R. A. (2002). \emph{Contributions to Statistical Analysis}. Merida, Venezuela: Universidad de Los Andes.
#'
#'Penfield, R. D. & Giacobbi, P. R., Jr. (2004) Applying a score confidence interval to Aiken's item content-relevance index. \emph{Measurement in Physical Education and Exercise Science, 8}(4), 213-225. \doi{10.1207/s15327841mpee0804_3}
#'
#'Polit, D.F., Beck, C.T. and Owen, S.V. (2007), Is the CVI an acceptable indicator of content validity? Appraisal and recommendations. \emph{Research in Nursing & Health, 30}, 459-467. \doi{10.1002/nur.20199}
#'
#'Wilson, E. B. (1927). Probable inference, the law of succession, and statistical inference. \emph{Journal of the American Statistical Association, 22}, 209-212. \doi{10.2307/2276774}
#'
#'@seealso
#'\code{\link[PropCIs:scoreci]{PropCIs::scoreci}} for score method confidence interval
#'
#' @author
#' Cesar Merino-Soto (\email{sikayax@yahoo.com.ar})
#'
#'@examples
#'Ej1 <- data.frame(
#'  J1 = c(5, 5, 6, 6, 6, 6, 6, 6, 5, 6, 5, 6, 3, 4, 4),
#'  J2 = c(5, 2, 6, 6, 6, 6, 6, 5, 4, 6, 5, 6, 4, 4, 3),
#'  J3 = c(5, 5, 6, 6, 6, 5, 6, 5, 4, 5, 5, 6, 5, 3, 5),
#'  J4 = c(5, 5, 6, 6, 6, 6, 6, 6, 5, 6, 3, 6, 5, 3, 5),
#'  J5 = c(5, 5, 6, 6, 6, 6, 6, 6, 6, 5, 5, 6, 3, 4, 5),
#'  J6 = c(5, 2, 6, 6, 6, 6, 6, 5, 6, 6, 3, 6, 4, 4, 4),
#'  J7 = c(2, 4, 5, 6, 6, 6, 6, 5, 6, 6, 5, 6, 5, 3, 6),
#'  J8 = c(5, 5, 6, 6, 6, 6, 6, 6, 5, 6, 6, 6, 5, 3, 5),
#'  J9 = c(4, 5, 5, 6, 5, 4, 6, 5, 5, 6, 4, 6, 5, 4, 4),
#'  J10 = c(4, 2, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 4, 4, 3),
#'  J11 = c(5, 4, 6, 6, 6, 6, 5, 6, 6, 6, 6, 6, 5, 4, 4),
#'  J12 = c(5, 4, 6, 6, 6, 6, 6, 6, 5, 6, 6, 6, 5, 4, 4),
#'  J13 = c(2, 4, 6, 6, 6, 6, 5, 6, 4, 2, 4, 6, 3, 3, 4),
#'  J14 = c(5, 5, 6, 6, 6, 6, 6, 5, 6, 6, 6, 6, 5, 4, 5),
#'  J15 = c(5, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 5, 3, 4))
#'
#'## Global CVC with MER CI
#'CVC(Ej1, max = 6, min = 1, conf.level = .90, overall = TRUE, overall.method = "global")
#'
#'## Traditional Hernandez CVC with Wilson CI
#'CVC(Ej1, max = 6, min = 1, conf.level = .90, overall = TRUE, overall.method = "Hernandez")
#'
#' @export
CVC <- function(data, max, min = 1, conf.level = 0.90, na.rm = FALSE,
                overall = FALSE,
                overall.method = c("global", "Hernandez"),
                overall.ci = c("MER", "Wilson")) {

  # Check whether the data.frame contains only numeric values
  if (!all(sapply(data, is.numeric))) {
    stop("The data.frame must contain only numeric values.")
  }

  # Detection of Missing Values
  if (!na.rm) {
    if (any(is.na(data))) {
      stop("Missing values detected. Use na.omit() first or set na.rm=TRUE.")
    }
  } else {
    data <- na.omit(data)
  }

  # Number of judges (columns) and items (rows)
  num_jueces <- ncol(data)
  n_items <- nrow(data)

  # Calculate Pe using the corrected formula (Hernandez-Nieto)
  # Pe = (1 / J) ^ J
  Pe <- (1 / num_jueces) ^ num_jueces

  # Calculate the average for each item (row)
  medias_items <- rowMeans(data)

  # Calculate the CVC for each item (CORRECTED FORMULA)
  CVC_items <- round((medias_items / max) - Pe, 3)

  # Calculate Wilson's confidence intervals for each CVC item
  get_wilson_CI <- function(x, n, conf.level) {
    p_hat <- max(1e-10, min(1 - 1e-10, x))
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

  # data.frame with the item-level results
  resultado_df <- data.frame(
    Item = 1:n_items,
    CVC = round(CVC_items, 3),
    lwr.ci = round(intervalos_CI[, "lower"], 3),
    upr.ci = round(intervalos_CI[, "upper"], 3)
  )

  # ---- Overall Calculation ----
  if (overall) {
    overall.method <- match.arg(overall.method)
    overall.ci <- match.arg(overall.ci)

    # If overall.method == "Hernandez", force overall.ci = "Wilson"
    if (overall.method == "Hernandez" && overall.ci != "Wilson") {
      warning("For overall.method = 'Hernandez', only 'Wilson' CI is available. Switching to 'Wilson'.")
      overall.ci <- "Wilson"
    }

    if (overall.method == "global") {
      # ---- Global super-item approach ----
      all_scores <- as.vector(as.matrix(data))
      M_total <- mean(all_scores)
      CVC_total <- (M_total / max) - Pe

      if (overall.ci == "Wilson") {
        # Wilson directo sobre CVC_total
        n <- num_jueces
        p_hat <- max(1e-10, min(1 - 1e-10, CVC_total))
        crit <- qnorm(1 - (1 - conf.level) / 2)
        SE_hat_sq <- p_hat * (1 - p_hat) / n
        omega <- n / (n + crit^2)
        A <- p_hat + crit^2 / (2 * n)
        B <- crit * sqrt(SE_hat_sq + crit^2 / (4 * n^2))
        lwr_total <- omega * (A - B)
        upr_total <- omega * (A + B)
        etiqueta <- "Total (global, Wilson)"

      } else { # overall.ci == "MER"
        z <- qnorm(1 - (1 - conf.level) / 2)
        k <- max - min

        p <- (M_total - min) / k
        p <- max(1e-10, min(1 - 1e-10, p))

        n <- num_jueces
        term1 <- 2 * p * n * k + z^2
        term2 <- z * sqrt(4 * n * k * p * (1 - p) + z^2)
        denom <- 2 * (n * k + z^2)
        pi_L <- (term1 - term2) / denom
        pi_U <- (term1 + term2) / denom

        LCL_mean <- M_total - z * sqrt(k * pi_L * (1 - pi_L) / n)
        UCL_mean <- M_total + z * sqrt(k * pi_U * (1 - pi_U) / n)

        LCL_mean <- max(min, min(max, LCL_mean))
        UCL_mean <- max(min, min(max, UCL_mean))

        lwr_total <- (LCL_mean / max) - Pe
        upr_total <- (UCL_mean / max) - Pe
        etiqueta <- "Total (global, MER)"
      }

    } else { # overall.method == "Hernandez"
      # ---- Traditional approach: average of item-level CVCs ----
      CVC_total <- mean(CVC_items)

      # Wilson CI on the mean CVC
      n <- num_jueces
      p_hat <- max(1e-10, min(1 - 1e-10, CVC_total))
      crit <- qnorm(1 - (1 - conf.level) / 2)
      SE_hat_sq <- p_hat * (1 - p_hat) / n
      omega <- n / (n + crit^2)
      A <- p_hat + crit^2 / (2 * n)
      B <- crit * sqrt(SE_hat_sq + crit^2 / (4 * n^2))
      lwr_total <- omega * (A - B)
      upr_total <- omega * (A + B)
      etiqueta <- "Total (Hernandez, Wilson)"
    }

    # Add overall row to results
    fila_total <- data.frame(
      Item = etiqueta,
      CVC = round(CVC_total, 3),
      lwr.ci = round(lwr_total, 3),
      upr.ci = round(upr_total, 3)
    )
    resultado_df <- rbind(resultado_df, fila_total)
  }

  return(resultado_df)
}
