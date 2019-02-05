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
        function(x) {
            if (length(x) == 1) {
                sprintf(
                    "%s = %.3e < %.3e",
                    deparse(substitute(x, env = sys.frame(-1))),
                    x,
                    value
                )
            } else { # TODO, vectorization should be handled generically by evaluate()
                i  <- which(x < value - tolerance)
                xx <- x[i]
                do.call(paste, c(as.list(sprintf(
                        "%s[%i] = %.3e < %.3e",
                        deparse(substitute(x, env = sys.frame(-1))),
                        i,
                        xx,
                        value
                    )), list(sep = "\n\r"))
                )
            }
        }
    )

}
