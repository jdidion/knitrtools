#' Print a section label.
#'
#' @param label the section label
section.label <- local({
    function(label) {
        sprintf('# <a name="%s"/> %s', label, label)
    }    
})

#' Print a reference to a section (using the section label).
#'
#' @param label the section label
section.ref <- local({
    function(label) {
        sprintf('[%s](#%s)', label, label)
    }    
})

#' Print an anchor for the current page that can be referenced
#' with `page.ref`.
#'
#' @param print.num whether to print the page number.
page.anchor <- local({
    function(print.num=T) {
        if (print.num) {
            sprintf('\n-----\nPage-%i\n<a name="Page-%i"/>\n', pgcount(inc=F), pgcount())
        } else {
            sprintf('\n<a name="Page-%i"/>\n', pgcount())
        }
    }
})

#' Print a reference to a page (using the page number).
#'
#' @param n the page number
page.ref <- local({
    function(n) {
        sprintf('[Page-%i](#Page-%i)', n, n)
    }
})

#' Print the current page count.
#'
#' @param inc if TRUE, the current page number will be incremented. 
#' This can be used to number pages.
page.count <- local({
    pg <- 0
    function(inc=T) {
        if (inc) { pg <<- pg + 1 }
        pg
    }
})

# Add a hook to echo the chunk name
knit_hooks$set(echo.label=function(before, options, envir) {
    if (before) {
        sprintf('> %s', options$label)
    }
})

# Add a hook fo place a page break after the chunk
knit_hooks$set(pgbreak=function(before, options, envir) {
    if (!before) {
        pganchor();
    }
})

## Extensions to kfigr

#' Convenience function to get table references. Simply calls 
#' `figr.ref(labels, types="table", ...)
tabl.ref <- function(labels, ...) {
    figr.ref(labels, types="table", ...)
}

#' Wrap figr with two improvements:
#' 1. Prefixes printed with first character upper case (optionally)
#' 2. Ability to format a list of references
#'
#' @param labels character vector of labels to reference.
#' @param panels character vector of figure panels to reference; must either 
#' be of the same length as `labels` or NULL.
#' @param types character vector of label types; must either be of length 1
#' (meaning all labels are of the same type), or of the same length as `labels`.
#' @param group logical; if true, labels of the same type will be grouped together.
figr.ref <- function(labels, panels=NULL, types="figure", group=TRUE, first.char.upper=T) {
    stopifnot(length(types) %in% c(1, length(labels)))
    if (is.null(panels)) {
        panels <- rep(NA, length(labels))
    }
    else {
        stopifnot(length(panels) == length(labels))
    }
    .format.refs <- function(labels, panels, type) {
        refs <- sapply(1:length(labels), function(i) {
            ref = figr(labels[i], prefix=F, link=T, type=type)
            panel <- panels[i]
            ifelse(is.na(panel), ref, paste(ref, panel))
        })
        prefix <- type
        if (first.char.upper) {
            prefix <- paste0(toupper(substr(prefix,1,1)), substring(type,2))
        }
        if (length(refs) > 1) {
            prefix <- paste0(prefix, "s")
            refs <- paste(paste(refs[-length(refs)], collapse=", "), 
                          refs[length(refs)], sep=" and ")
        }
        paste(prefix, refs)
    }
    if (length(types) == 1) {
        .format.refs(labels, panels, types[1])
    }
    if (group) {
        groups <- sapply(unique(types), function(type) {
            w <- types == type
            .format.refs(labels[w], panels[w], type)
        })
        paste(groups, collapse="; ")
    }
    else {
        refs <- sapply(1:length(labels), function(i) 
            .format.refs(labels[i], panels[i], types[i]))
        paste(paste(refs[-length(refs)], collapse=", "), refs[length(refs)], sep=" and ")
    }
}
