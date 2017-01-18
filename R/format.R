#' Format a vector of values as a comma-delimited list.
#'
#' @param v vector of values
#' @param fn function to apply to `v` (or NULL to use `v` as-is)
#' @param ... additional arguments to `fn`
#' @param oxford use an oxford comma
fmt_list <- function(v, fn=NULL, ..., oxford=TRUE) {
    if (!is.null(fn)) {
        v <- sapply(v, fn, ...)
    }
    if (length(v) == 1) {
        v
    }
    else if (length(v) == 2) {
        paste(v, collapse=" and ")
    }
    else {
        s <- paste(v[-length(v)], collapse=", ")
        if (oxford) {
            s <- paste0(s, ",")
        }
        paste0(s, " and ", v[length(v)])
    }
}

#' Format numeric values as percentages.
#'
#' @param v vector of numeric values
#' @param digits number of digits to print
fmt_pct <- function(v, digits=0) {
    if (length(v) == 1) {
        paste0(round(v*100, digits), "%")
    }
    else {
        fmt.list(v, fmt.pct, digits=digits)
    }
}

#' Format a vector as a range of values.
#'
#' @param v numeric vector
#' @param digits number of digits to print (if values are not integers)
fmt_range <- function(v, digits=2, sep="-") {
    if (all(is.integer(v))) {
        digits <- 0
    }
    paste(round(range(v), digits), collapse=sep)
}

#' Format a logical vector as a sum (total number of true values) and percentage
#' (fraction of all values that are true).
#'
#' @param v logical vector
#' @param digits number of digits to print for percentage
fmt_true <- function(v, digits=2) {
    paste0(sum(v), " (", round((sum(v) / length(v)) * 100, 2), "%)")
}

#' Format the mean +/- the standard deviation for a vector.
#'
#' @param v numeric vector
#' @param digits number of digits to print
fmt_mean_sd <- function(v, digits=1) {
    m <- mean(v)
    s <- sd(v)
    paste(round(m, digits), "±", round(s, digits))
}

#' Format numeric values as currency. Note: this is very crude and probably won't
#' generate correct currency strings for all locales.
#'
#' @param v numeric vector
#' @param currency symbol ($ by default)
#' @param commas whether to separate every 3 digits with commas
#' @param cents whether to show cents amount after decimal
fmt_currency <- function(v, currency='$', commas=TRUE, cents=TRUE) {
    paste0(currency, formatC(v, format="f", digits=ifelse(cents, 2, 0), big.mark=ifelse(commas, ",", "")))
}
