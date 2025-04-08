#' Compute D2 Statistic for Content Validity Studies
#'
#' Calculates the D2 statistic for comparing distributions of ratings between two independent groups of raters.
#' This nonparametric method is particularly useful in content validity studies, where the number of raters is typically small.
#'
#' @param ratings1 A numeric vector (for a single item) or a data frame (for multiple items)
#'   containing ratings from the first group of raters. For single-item comparisons, this can be a numeric
#'   vector of any length, or even a single number.
#' @param ratings2 Same structure as `ratings1`, for the second group of raters. When both inputs are vectors,
#'   they can have different lengths (i.e., different number of raters per group)
#' @param c An integer indicating the number of categories in the rating scale (e.g., 5 for a Likert scale from 1 to 5).
#' @param tail A character string, either `"one"` for a one-tailed test or `"two"` for a two-tailed test. Default is `"two"`.
#' @param digits Number of decimal places to round the results. Default is 3.
#' @param CI Logical. If TRUE, computes bootstrap confidence intervals for the effect size r. Default is FALSE.
#' @param conf.level Confidence level for the interval (e.g., .95). Default is 0.95.
#' @param B Number of bootstrap replicates. Default is 500.
#'
#' @details
#' The data frame inputs (`ratings1` and `ratings2`) must be structured such that
#' **rows represent raters** and **columns represent items**. Each cell should contain
#' the rating that a rater gave to a specific item.
#' 
#' - If `ratings1` and `ratings2` are numeric vectors (or single numbers), they are interpreted as the ratings
#'   from two independent groups on a single item. In this case, they do **not** need to be of equal length.
#'
#' - If both are data frames (or matrices), each column represents an item, and each row is a rater. In this case,
#'   the number of columns (items) must match in both data frames.
#'
#' - The function computes the D2 statistic, a Z-transformed difference in means standardized by the rating scale
#'   and number of raters, and optionally estimates a correlation-type effect size with confidence intervals.
#'
#' The D2 statistic, proposed by Aiken and Aiken (1986), evaluates the extent to which two independent groups of raters
#' assign systematically different ratings to the same items. It is designed for use with ordinal rating scales (e.g., Likert-type),
#' and does not assume normality of the data, making it especially appropriate for content validity studies involving small samples.
#'
#' The effect size `r` is computed using the transformation `r = Z / sqrt(N)`, based on Rosenthal (1991).
#' When `CI = TRUE`, bootstrap confidence intervals for `r` are computed using the percentile method,
#' which is preferred when data are non-normal or samples are small (Bishara & Hittner, 2017).
#'
#' @return A data frame with the following columns:
#'   - `items`: Item labels (e.g., "Item 1", "Item 2", etc.).
#'   - `D2`: The computed D2 ratio.
#'   - `ZD2`: The corresponding Z-score.
#'   - `p.value`: The p-value for significance testing.
#'   - `ES`: The effect size r.
#'   - `ES.lower`, `ES.upper`: Lower and upper confidence limits for r (if `CI = TRUE`).
#'
#' @references
#' Aiken, L. R., & Aiken, T. A. (1986). Difference Tests for Distributions of Ratings.
#' Educational and Psychological Measurement, 46(4), 871–881. \doi{10.1177/001316448604600407}
#'
#' Rosenthal, R. (1991). Meta-analytic procedures for social research (revised). Sage: Newbury Park, CA.
#'
#' Bishara, A. J., & Hittner, J. B. (2017). Confidence intervals for correlations when data are not normal.
#' Behavior Research Methods, 49(1), 294–309. \doi{10.3758/s13428-016-0702-8}
#'
#' @examples
#' ratings1 <- data.frame(Item1 = c(1, 3, 2, 2, 1), Item2 = c(2, 2, 4, 4, 3))
#' ratings2 <- data.frame(Item1 = c(3, 5, 5, 4, 5), Item2 = c(5, 5, 4, 5, 5))
#' D2(ratings1, ratings2, c = 5, CI = TRUE)
#'
#' @export
D2 <- function(ratings1, ratings2, c, tail = "two", digits = 3, CI = FALSE, conf.level = 0.95, B = 500) {
  if (!requireNamespace("boot", quietly = TRUE)) {
    stop("Package 'boot' is required for bootstrap confidence intervals. Please install it.")
  }
  
  if (!is.numeric(as.matrix(ratings1)) | !is.numeric(as.matrix(ratings2))) {
    stop("Both ratings1 and ratings2 must contain only numeric values.")
  }
  
  if (is.vector(ratings1)) ratings1 <- as.data.frame(ratings1)
  if (is.vector(ratings2)) ratings2 <- as.data.frame(ratings2)
  
  if (!is.data.frame(ratings1) | !is.data.frame(ratings2)) {
    stop("Both ratings1 and ratings2 must be numeric vectors or data frames.")
  }
  
  if (ncol(ratings1) != ncol(ratings2)) {
    stop("ratings1 and ratings2 must have the same number of items (columns).")
  }
  
  if (!is.numeric(c) | c <= 1 | c != as.integer(c)) {
    stop("The number of categories (c) must be an integer greater than 1.")
  }
  
  if (!tail %in% c("one", "two")) {
    stop('The argument "tail" must be either "one" or "two".')
  }
  
  complete_cases <- complete.cases(ratings1, ratings2)
  ratings1 <- ratings1[complete_cases, , drop = FALSE]
  ratings2 <- ratings2[complete_cases, , drop = FALSE]
  
  if (nrow(ratings1) == 0 | nrow(ratings2) == 0) {
    stop("No valid data remaining after removing missing values.")
  }
  
  n1 <- nrow(ratings1)
  n2 <- nrow(ratings2)
  N <- n1 + n2
  
  compute_D2 <- function(x, y) {
    r1 <- mean(x)
    r2 <- mean(y)
    D2_value <- (r1 - r2) / (c - 1)
    ZD2 <- 2 * D2_value * sqrt(3 * n1 * n2 * (c - 1) / ((n1 + n2) * (c + 1)))
    p_value <- if (tail == "one") pnorm(ZD2, lower.tail = FALSE) else 2 * pnorm(abs(ZD2), lower.tail = FALSE)
    ES <- ZD2 / sqrt(N)
    return(c(D2 = D2_value, ZD2 = ZD2, p.value = p_value, ES = ES))
  }
  
  results <- mapply(compute_D2, ratings1, ratings2)
  results_df <- as.data.frame(t(results))
  results_df <- round(results_df, digits)
  
  item_names <- colnames(ratings1)
  if (is.null(item_names)) item_names <- paste0("Item ", seq_len(nrow(results_df)))
  results_df$items <- item_names
  results_df <- results_df[, c("items", "D2", "ZD2", "p.value", "ES")]
  rownames(results_df) <- NULL
  
  if (CI) {
    ci_list <- lapply(seq_along(ratings1), function(i) {
      x <- ratings1[[i]]
      y <- ratings2[[i]]
      data <- data.frame(group = rep(c("x", "y"), each = length(x)), rating = c(x, y))
      
      boot_fun <- function(d, idx) {
        d_resampled <- d[idx, ]
        r1 <- mean(d_resampled$rating[d_resampled$group == "x"])
        r2 <- mean(d_resampled$rating[d_resampled$group == "y"])
        D2_val <- (r1 - r2) / (c - 1)
        Z <- 2 * D2_val * sqrt(3 * n1 * n2 * (c - 1) / ((n1 + n2) * (c + 1)))
        r <- Z / sqrt(N)
        return(r)
      }
      
      boot_obj <- boot::boot(data, statistic = boot_fun, R = B)
      boot_ci <- boot::boot.ci(boot_obj, conf = conf.level, type = "perc")
      
      if (!is.null(boot_ci$percent)) {
        return(c(lower = boot_ci$percent[4], upper = boot_ci$percent[5]))
      } else {
        return(c(lower = NA, upper = NA))
      }
    })
    
    ci_df <- as.data.frame(do.call(rbind, ci_list))
    colnames(ci_df) <- c("ES.lower", "ES.upper")
    results_df <- cbind(results_df, round(ci_df, digits))
  }
  
  return(results_df)
}
