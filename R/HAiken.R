#' Coefficient of Homogeneity of Response (Aiken's H)
#'
#' @description
#' Calculates Aiken's H coefficient of homogeneity for each item and,
#' optionally, an overall H (total) across all items, together with
#' bootstrap confidence intervals.
#'
#' @param data A data frame with judges in columns and items in rows.
#' @param ncat Number of response categories.
#' @param conf.level Confidence level for the intervals (e.g., .90, .95).
#' @param na.rm Logical. If TRUE, rows with missing values are removed.
#' @param overall Logical. If TRUE (default), the overall H is added as the
#'   last row.
#' @param B Integer. Number of bootstrap resamples (default = 1000).
#' @param ci.type Character. Confidence-interval method:
#'   \describe{
#'     \item{\code{"auto"}}{(default) Uses \code{"bca"} when the number of
#'       judges \eqn{m \ge 8}, and \code{"norm"} otherwise.}
#'     \item{\code{"bca"}}{Bias-corrected and accelerated bootstrap
#'       (Efron, 1987). Recommended for reliability coefficients when
#'       \eqn{m \ge 8}.}
#'     \item{\code{"norm"}}{Normal approximation using the bootstrap
#'       standard error, centered on the point estimate.}
#'     \item{\code{"perc"}}{Percentile bootstrap.}
#'     \item{\code{"basic"}}{Basic (reverse-percentile) bootstrap.}
#'   }
#'
#' @details
#' The H coefficient (Aiken, 1985, p. 140) is
#' \deqn{H = 1 - \frac{4S}{(c - 1)(k^2 - j)},}
#' where \eqn{S} is the sum of the \eqn{k(k-1)/2} absolute differences among
#' the \eqn{k} ratings, \eqn{c} is the number of categories, and
#' \eqn{j = 0} if \eqn{k} is even and \eqn{j = 1} if \eqn{k} is odd.
#' The coefficient ranges from 0 (maximum disagreement) to 1 (perfect
#' agreement).
#'
#' \strong{Confidence-interval methods.} With few judges, the bootstrap
#' distribution of \eqn{H} is discrete and biased upward (because each
#' resample omits on average \eqn{m/e \approx 0.37m} distinct judges). The
#' choice of interval method matters:
#' \itemize{
#'   \item \code{"perc"} inherits the bias; the lower limit can cling to
#'     the point estimate.
#'   \item \code{"basic"} reverses the percentiles around the estimate,
#'     moving the "peg" to the upper limit.
#'   \item \code{"norm"} recenters on the point estimate, forcing
#'     symmetry; may leave \eqn{[0,1]} for extreme cases.
#'   \item \code{"bca"} corrects both bias and skewness via jackknife;
#'     recommended when \eqn{m \ge 8}.
#' }
#' With fewer than 6 judges, all bootstrap intervals are unreliable and
#' should be interpreted as rough guides, not as formal inference.
#'
#' @return A data frame with columns:
#'   \item{Item}{Item number, or "Total".}
#'   \item{H}{Aiken's H coefficient.}
#'   \item{lwr.ci}{Lower confidence bound.}
#'   \item{upr.ci}{Upper confidence bound.}
#'   \item{n.jueces}{Number of judges.}
#'
#' \strong{Note on inference.} The normal-approximation bootstrap interval
#' is not the same as the Z-test for the mean of H described by Aiken
#' (1985). The former estimates a confidence interval for H centered on the
#' observed value; the latter tests a null hypothesis using the theoretical
#' mean and standard deviation of H under random ratings.
#'
#' More precisely, the theoretical mean of H under random ratings is a
#' fixed quantity,
#' \deqn{\mu_H = \frac{2(c + 1) + (k + j)(c - 2)}{3c(k + j)},}
#' which anchors hypothesis testing about the calculated H. It does not
#' enter the construction of bootstrap confidence intervals. The bias and
#' asymmetry observed in small-sample bootstrap intervals—such as the
#' lower limit coinciding with the point estimate—are properties of the
#' resampling distribution of H, not of the theoretical null value. The
#' two frameworks answer different questions: the Z-test asks whether the
#' observed H departs from what random ratings would produce, whereas the
#' bootstrap interval estimates the sampling variability of H around the
#' observed value.
#'
#' @references
#' Aiken, L. R. (1980). Content validity and reliability of single items
#'   or questionnaires. \emph{Educational and Psychological Measurement,
#'   40}, 955--959.
#'
#' Aiken, L. R. (1985). Three coefficients for analyzing the reliability
#'   and validity of ratings. \emph{Educational and Psychological
#'   Measurement, 45}, 131--142.
#'
#' Efron, B. (1987). Better bootstrap confidence intervals. \emph{Journal
#'   of the American Statistical Association, 82}(397), 171--185.
#'
#' Efron, B., & Tibshirani, R. J. (1993). \emph{An introduction to the
#'   bootstrap}. Chapman & Hall.
#'
#' @examples
#' \dontrun{
#' Haiken(randata_high, ncat = 7, conf.level = .90,
#'        overall = TRUE, B = 1000, ci.type = "auto")
#' }
#'
#' @export
#' @importFrom stats median quantile sd qnorm pnorm na.omit
Haiken <- function(data, ncat, conf.level = .95, na.rm = FALSE,
                   overall = TRUE, B = 1000,
                   ci.type = c("auto", "bca", "norm", "perc", "basic")) {

  # --- Validación ---------------------------------------------------------
  if (!is.data.frame(data)) data <- as.data.frame(data)
  if (!na.rm && anyNA(data))
    stop("Missing values found. Set na.rm=TRUE or handle them first.")
  if (na.rm) data <- na.omit(data)

  n_items <- nrow(data)
  m <- ncol(data)
  if (n_items < 1 || m < 2) stop("Need at least 1 item and 2 judges.")

  # --- Auto-selección de método -------------------------------------------
  if (ci.type == "auto") {
    ci.type <- if (m >= 8) "bca" else "norm"
    message("ci.type auto-selected: '", ci.type, "' (m = ", m, ")")
  }

  # --- Warnings de confiabilidad ------------------------------------------
  if (m < 6) {
    warning("Bootstrap CIs unreliable with < 6 judges; interpret as ",
            "rough guides, not formal inference.")
  }
  if (ci.type == "bca" && m < 8) {
    warning("BCa with < 8 judges has unstable acceleration; the interval ",
            "may be wide or asymmetric. Consider ci.type = 'norm'.")
  }

  # --- j = 0 si k par, j = 1 si k impar (Aiken 1985, p. 140) --------------
  .delta <- function(k) ifelse(k %% 2 == 0, 0, 1)

  # --- H para un panel ----------------------------------------------------
  calc_H <- function(mat) {
    ni <- nrow(mat); mi <- ncol(mat)
    delta_i <- .delta(mi)
    denom_i <- (ncat - 1) * (mi^2 - delta_i)
    denom_total_i <- denom_i * ni

    H_ind <- numeric(ni)
    sum_diffs_total <- 0
    all_valid <- TRUE

    for (i in 1:ni) {
      row_i <- as.numeric(mat[i, ])
      if (anyNA(row_i)) row_i <- as.numeric(as.character(mat[i, ]))
      if (anyNA(row_i)) row_i[is.na(row_i)] <- median(row_i, na.rm = TRUE)

      diffs <- tryCatch(
        combn(row_i, 2, function(p) abs(p[1] - p[2])),
        error = function(e) rep(NA, choose(mi, 2))
      )

      if (anyNA(diffs)) {
        all_valid <- FALSE; H_ind[i] <- NA; sum_diffs_total <- NA; break
      } else {
        sum_diffs <- sum(diffs, na.rm = TRUE)
        sum_diffs_total <- sum_diffs_total + sum_diffs
        H_ind[i] <- 1 - (4 * sum_diffs) / denom_i
      }
    }

    H_total <- if (anyNA(H_ind) || !all_valid) NA else
      1 - (4 * sum_diffs_total) / denom_total_i

    list(H_individual = H_ind, H_total = H_total, valid = all_valid)
  }

  # --- Estimaciones puntuales --------------------------------------------
  est <- calc_H(data)
  H_ind <- est$H_individual
  H_total <- est$H_total
  if (anyNA(H_ind)) stop("Point estimates contain NA. Check data and ncat.")
  if (overall && is.na(H_total)) stop("Total H is NA. Check data.")

  # --- Bootstrap ----------------------------------------------------------
  boot_H_ind <- matrix(NA, B, n_items)
  boot_H_total <- numeric(B)
  for (b in 1:B) {
    idx <- sample(1:m, m, replace = TRUE)
    est_b <- calc_H(data[, idx, drop = FALSE])
    boot_H_ind[b, ] <- est_b$H_individual
    boot_H_total[b] <- est_b$H_total
  }

  # --- Jackknife (solo para BCa) -----------------------------------------
  jack_ind <- NULL; jack_total <- NULL
  if (ci.type == "bca" && m >= 3) {
    jack_ind   <- matrix(NA, m, n_items)
    jack_total <- numeric(m)
    for (j in 1:m) {
      est_j <- calc_H(data[, -j, drop = FALSE])
      jack_ind[j, ] <- est_j$H_individual
      jack_total[j] <- est_j$H_total
    }
  }

  # --- Constructor de IC --------------------------------------------------
  get_ci <- function(boot_vals, theta_hat, jack_vals, conf.level, ci.type) {
    boot_vals <- boot_vals[is.finite(boot_vals)]
    if (length(boot_vals) < 20) return(c(lwr = NA, upr = NA))
    alpha <- 1 - conf.level

    if (ci.type == "perc") {
      lwr <- quantile(boot_vals, alpha/2,     names = FALSE)
      upr <- quantile(boot_vals, 1 - alpha/2, names = FALSE)

    } else if (ci.type == "basic") {
      q_lo <- quantile(boot_vals, alpha/2,     names = FALSE)
      q_hi <- quantile(boot_vals, 1 - alpha/2, names = FALSE)
      lwr  <- 2 * theta_hat - q_hi
      upr  <- 2 * theta_hat - q_lo

    } else if (ci.type == "norm") {
      se  <- sd(boot_vals)
      z   <- qnorm(1 - alpha/2)
      lwr <- theta_hat - z * se
      upr <- theta_hat + z * se

    } else { # bca
      z0 <- qnorm(sum(boot_vals < theta_hat) / length(boot_vals))
      z0 <- max(min(z0, 2), -2)

      if (!is.null(jack_vals) && length(jack_vals) >= 3 &&
          all(is.finite(jack_vals))) {
        d   <- mean(jack_vals) - jack_vals
        num <- sum(d^3)
        den <- 6 * (sum(d^2))^1.5
        a   <- if (den > 0) num / den else 0
        a   <- max(min(a, 0.5), -0.5)
      } else {
        a <- 0
      }

      z_lo <- qnorm(alpha/2)
      z_hi <- qnorm(1 - alpha/2)
      al <- pnorm(z0 + (z0 + z_lo) / (1 - a * (z0 + z_lo)))
      au <- pnorm(z0 + (z0 + z_hi) / (1 - a * (z0 + z_hi)))
      al <- max(min(al, 0.49), 0.001)
      au <- min(max(au, 0.51), 0.999)

      lwr <- quantile(boot_vals, al, names = FALSE)
      upr <- quantile(boot_vals, au, names = FALSE)
    }
    c(lwr = lwr, upr = upr)
  }

  # --- IC por ítem --------------------------------------------------------
  lwr_ind <- numeric(n_items); upr_ind <- numeric(n_items)
  for (i in 1:n_items) {
    jv <- if (!is.null(jack_ind)) jack_ind[, i] else NULL
    ci <- get_ci(boot_H_ind[, i], H_ind[i], jv, conf.level, ci.type)
    lwr_ind[i] <- ci["lwr"]; upr_ind[i] <- ci["upr"]
  }

  # --- IC del total -------------------------------------------------------
  if (overall) {
    jv <- if (!is.null(jack_total)) jack_total else NULL
    ci_total <- get_ci(boot_H_total, H_total, jv, conf.level, ci.type)
    lwr_total <- ci_total["lwr"]; upr_total <- ci_total["upr"]
  } else {
    lwr_total <- NA; upr_total <- NA
  }

  # --- Salida ------------------------------------------------------------
  resultados <- data.frame(
    Item     = 1:n_items,
    H        = round(H_ind, 3),
    lwr.ci   = round(lwr_ind, 3),
    upr.ci   = round(upr_ind, 3),
    n.jueces = m,
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  if (overall) {
    resultados <- rbind(resultados, data.frame(
      Item     = "Total",
      H        = round(H_total, 3),
      lwr.ci   = if (is.na(lwr_total)) NA else round(lwr_total, 3),
      upr.ci   = if (is.na(upr_total)) NA else round(upr_total, 3),
      n.jueces = m,
      stringsAsFactors = FALSE,
      row.names = NULL
    ))
  }

  attr(resultados, "ci.type")     <- ci.type
  attr(resultados, "B")           <- B
  attr(resultados, "conf.level")  <- conf.level
  return(resultados)
}
