#' Class for combining check() function with informative message
#'
#' @param isvalid isvalid(x) = TRUE if x qualifies, FALSE otherwise
#' @param message message(x) returns message if x is not valid
#' @param broadcast should isvalid be applied element wise to arrays or lists?
#'
#' @export
Checkr <- function(
    isvalid,
    message,
    broadcast = FALSE
) {

    if (length(args(isvalid)) != 1)
        stop("'isvalid' must accept exactly one argument")
    if (length(args(message)) != 1)
        stop("'message' must accept exactly one argument")

    res <- list(
        isvalid   = isvalid,
        message   = message
    )
    class(res) <- c("Checkr", class(res))
    if (broadcast == "list")
        class(res) <- c("CheckrList", class(res))
    if (broadcast == "array")
        class(res) <- c("CheckrArray", class(res))

    return(res)

}

#' Evaluate a Checkr object
#'
#' Evaluate executes the provided isvalid() function of a given Checkr object.
#' If it returns TRUE, NA is returned, otherwiese a string generated from
#' message().
#'
#' @param chckr Checkr object
#' @param x object to check
#' @param level deparse level to determine original name of x
#'
#' @return NA_character or character
#'
#' @export
evaluate <- function(chckr, x, level = -1) UseMethod("evaluate", chckr)



#' @describeIn Checkr
#'
#' @param chckr Checkr object
#' @param x object to check
#' @param level deparse level to determine original name of x
#'
#' @export
evaluate.Checkr <- function(chckr, x, level = -1) {

    ok <- tryCatch(
        chckr$isvalid(x),
        error = function(e) {cat(e); FALSE}
    )
    msg <- NA_character_
    if (!ok)
        msg <- sprintf("%s: %s", deparse(substitute(x, env = sys.frame(level))), chckr$message(x))

    return(msg)

}



#' @describeIn Checkr
#'
#' @export
evaluate.CheckrArray <- function(chckr, x, level = -1) {

    xname <- deparse(substitute(x, env = sys.frame(level)))

    x <- tryCatch(
        as.array(x),
        error = function(e) stop(sprintf("%s must be array-like", xname))
    )

    if (length(x) == 1)
        # use index free variant, need to use hihger deparse level
        return(evaluate.Checkr(chckr, x[1], level = -2))

    secure_check <- function(x) {
        tryCatch(
            chckr$isvalid(x),
            error = function(e) {print(e); FALSE}
        )}

    ok   <- apply(x, 1:length(dim(x)), secure_check)
    inds <- arrayInd(1:length(x), dim(x))
    msg  <- sapply(1:length(x), function(i) {
        if (ok[i]) {
            return(NA_character_)
        } else {
            return(sprintf("%s[%s]: %s", xname, paste(inds[i, ], collapse = ", "), chckr$message(x[i])))
        }
    })

    msg <- paste(msg[!is.na(msg)], collapse = "\n\r")

    if (msg == "")
        msg <- NA_character_

    return(msg)

}