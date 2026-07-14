
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

