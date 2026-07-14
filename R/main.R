
# ------------------------------------------------------
#' Print "foo"
#'
#' Prints the string \code{"foo"} to the console. A minimal *placeholder*
#' function, useful as a stub or for testing.
#'
#' @return Invisibly \code{NULL}. Called for its side effect of
#'   printing to the console.
#' @export
#' @importFrom ggplot2 ggplot theme_bw geom_point aes
#'
#' @examples
#' foo()

foo <- function() {

  # deal with no visible binding issue
  x <- NULL

  data.frame(x = 1:10) |>
    ggplot() + theme_bw() +
    geom_point(aes(x = x, y = 1))


}

# ------------------------------------------------------
#' Probability of Detecting at Least One Event
#'
#' Computes the probability of observing at least one event across
#' \code{n} independent trials, where each trial has an event
#' probability of \code{p}. This is the complement of observing zero
#' events in all \code{n} trials.
#'
#' @param n Integer. The number of independent trials (must be
#'   non-negative).
#' @param p Numeric. The per-trial probability of the event, in the
#'   range \eqn{[0, 1]}.
#'
#' @return A numeric value giving the probability of at least one
#'   detection, in the range \eqn{[0, 1]}.
#'
#' @details The calculation is based on the formula
#'   \eqn{1 - (1 - p)^n}. Assuming trials are independent and each has
#'   the same event probability \code{p}, \eqn{(1 - p)^n} is the
#'   probability of zero events, so its complement gives the
#'   probability of one or more events.
#'
#' @examples
#' # 90% detection probability example
#' power_detect(n = 10, p = 0.2)
#'
#' # A single trial simply returns p
#' power_detect(n = 1, p = 0.3)
#'
#' # No trials means zero detection probability
#' power_detect(n = 0, p = 0.5)
#'
#' @export
power_detect <- function(n, p) {
  checkmate::assert_integerish(n, lower = 0, any.missing = TRUE)
  checkmate::assert_numeric(p, lower = 0, upper = 1, any.missing = TRUE)

  1 - (1 - p)^n
}

