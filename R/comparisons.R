#' greater equal
#'
#' @param value value to compare to
#' @param tolerance allowable absolute tolerance
#'
#' @rdname comparisons
#'
#' @export
ge <- function(value, tolerance = 0) {

    Checkr(
        function(x) all(x >= value - tolerance),
        function(x) sprintf("%.3e < %.3e", x, value),
        broadcast = "array"
    )

}
