#' Class for combining check() function with informative message
#'
#' @param isvalid isvalid(x) = TRUE if x qualifies, FALSE otherwise
#' @param message message(x) returns message if x is not valid
#'
#' @export
Checkr <- function(
    isvalid,
    message
) {

    if (length(args(isvalid)) != 1)
        stop("'isvalid' must accept exactly one argument")
    if (length(args(message)) != 1)
        stop("'message' must accept exactly one argument")

    res <- list(
        isvalid = isvalid,
        message = message
    )
    class(res) <- c("Checkr", class(res))

    return(res)

}

#' @export
evaluate <- function(chckr, x) UseMethod("evaluate", chckr)

#' @export
evaluate.Checkr <- function(chckr, x) {

    ISOK <- tryCatch(
        chckr$isvalid(x),
        error = function(e) {print(e); FALSE}
    )

    if (ISOK) {
        return(NULL)
    } else {
        return(chckr$message(x))
    }

}
