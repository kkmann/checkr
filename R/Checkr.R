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

#' Evaluate a Checkr object
#'
#' Evaluate executes the provided isvalid() function of a given Checkr object.
#' If it returns TRUE, NULL is returned, otherwiese a string generated from
#' message().
#'
#' @param chckr Checkr object
#' @param x object to check
#'
#' @return NULL or character
#'
#' @export
evaluate <- function(chckr, x) UseMethod("evaluate", chckr)



#' @describeIn Checkr
#'
#' @param chckr Checkr object
#' @param x object to check
#'
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
