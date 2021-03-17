#' In-Text Summary Statistics
#'
#' Generate in-text summary statistics for posterior distributions from Bayesian models. Most useful when used in Rmarkdown documents.
#'
#' @param x The distribution to summarise
#' @param text A character string that follows and describes the point estimate.
#' @param suffix A character string that follows each estimate.
#' @param inside Should the median be inside or outside the brackets? Defaults to T.
#' @param probs The level at which to compute the lower and upper bounds. Defaults to 0.95.
#' @param digits How many digits to round the summary statistics to. Defaults to 1.
#' @return A text string.
#' @examples
#' in_text(rnorm(1e3, 0.5, .1))
#' @export

in_text <- function(x, text = NULL, suffix = NULL, inside = T, probs = .95, digits = 1){

  # Get median of the distribution and round to
  # desired number of digits

  m <- stats::median(x)
  m <- round(m, digits = digits)
  m <- format(m, nsmall = digits)


  # Get lower bound and round to desired digits

  l <- stats::quantile(x, probs = (1 - probs)/2)
  l <- round(l, digits = digits)
  l <- format(l, nsmall = digits)


  # Get upper bound and round to desired digits

  u <- stats::quantile(x, probs = probs + ((1 - probs)/2))
  u <- round(u, digits = digits)
  u <- format(u, nsmall = digits)


  # Add suffix if necessary

  if(is.null(suffix) == F){
    m <- paste0(m, suffix)
    l <- paste0(l, suffix)
    u <- paste0(u, suffix)
  }


  # Add in text if necessary

  if(is.null(text) == F){
    m <- paste0(m, text)
  }


  # Return in-text version

  if(inside == T){

    paste0("(", m, ", ", probs*100, "% ", "CI: ", l, " to ", u, ")")

  } else {

    paste0(m, " (", probs*100, "% ", "CI: ", l, " to ", u, ")")

  }

}
