#' Format one or more names as a set of initials
format.initial <- function(x, initial.sep='') {
    parts <- strsplit(x, ' ', fixed=T)[[1]]
    initials <- toupper(sapply(parts, substr, 1, 1))
    paste(initials, sep=initial.sep)
}

#' Format an author as a full name (First MI Last)
#' @param suffix.sep string separating the suffixe (e.g. Jr.) from the last name
#' @param initial.sep string separating consecutive initials
format.full.name <- function(fields, suffix.sep=' ', initial.sep='') {
    author.str <- authors$First
    if (!is.na(fields$Middle)) {
        middle <- format.initial(fields$Middle, initial.sep)
        author.str <- paste(author.str, middle)
    }
    author.str <- paste(author.str, fields$Last)
    if (!is.na(fields$Suffix)) {
        author.str <- paste(paste0(author.str, suffix.sep), fields$Suffix)
    }
    author.str
}

# Format an author as initials followed by last name (FM Last)
#' @param suffix.sep string separating the suffixe (e.g. Jr.) from the last name
#' @param initial.sep string separating consecutive initials
format.initial.first <- function(fields, suffix.sep=' ', initial.sep='') {
    init.str <- fmt.initial(fields$First, initial.sep)
    if (!is.na(fields$Middle)) {
        middle <- format.initial(fields$Middle, initial.sep)
        init.str <- paste0(init.str, middle)
    }
    author.str <- paste(init.str, fields$Last)
    if (!is.na(fields$Suffix)) {
        author.str <- paste(paste0(author.str, suffix.sep), fields$Suffix)
    }
    author.str
}

#' Format an author as a last name followed by initials (Last FM)
#' @param suffix.sep string separating the suffixe (e.g. Jr.) from the last name
#' @param initial.sep string separating consecutive initials
#' @parma first.last.sep when `naming == 'initial_after'`, the separtor between the
#' last name and the initials
format.initial.last <- function(fields, suffix.sep=' ', initial.sep='', first.last.sep=', ') {
    init.str <- fmt.initial(fields$First, initial.sep)
    if (!is.na(fields$Middle)) {
        middle <- format.initial(fields$Middle, initial.sep)
        init.str <- paste0(init.str, middle)
    }
    author.str <- authors$Last
    if (!is.na(fields$Suffix)) {
        author.str <- paste(paste0(author.str, suffix.sep), fields$Suffix)
    }
    author.str <- paste0(author.str, first.last.sep, init.str)
    author.str
}

#' Format an affiliation.
#' @param label.sep separator between afiliation label and string
format.affiliation <- function(label, fields, label.sep=". ", part.sep=", ") {
    fields <- fields[!is.na(fields)]
    paste(label, paste(fields, collapse=part.sep), sep=affil.label.sep)
}

#' Format a list of affiliation references.
#' @param affil.sep separator between multiple affiliations after an author name
#' @param affil.wrap function to wrap the list of an author's affiliations
format.affilation.refs <- function(labels, affil.sep=',', affil.wrap=NULL) {
    affil.str <- paste(labels, collapse=affil.sep)
    if (!is.null(affil.wrap)) {
        affil.str <- affil.wrap(affil.str)
    }
    affil.str
}

# Wrap function for an affiliation list to make it superscript
# in either HTML or Markdown.
markup.ss <- function(s) paste0("<sup>", s, "</sup>")

#' Format author and affiliation lists from two linked tables.
#' 
#' The author table must have the following columns: First, Middle, Last, Suffix,
#' Affiliations, Equal. 'Affiliations' is set of affiliation IDs without separators
#' (e.g. ABC). We recommend using capital letters (and lower-case if necessary), but
#' any single characters will work. The author rows should be ordered in the order you 
#' want them to appear in the final list.
#' 
#' The affiliation table must have the following columns: ID, Department, Institution,
#' City, Locality, Country. The ID is arbitrary but must be unique, and affiliations
#' can appear in any order.
#' 
#' @param authors path to Excel spreadsheet, or data.frame
#' @param affiliations NULL if `names` is a path, otherwise a data.frame
#' @param author.format function to format author names
#' @param author.format.args optional arguments to author name formatting function
#' @param affil.labeling function for converting an integer index into the label for the
#' affiliation
#' @param affil.format function to format an affiliation
#' @param affil.format.args optional arguments to affiliation formatting function
#' @param affil.refs.format function to format list of affiliation references
#' @param affil.refs.format.args optional arguments to affilation reference formatting function
#' @param equal.symbols vector of symbols to use to denote equal contributions
#' @param author.affil.ref.sep separator between author name and list of affiliation reference
#' @param author.sep separator to use when concatenating authors
#' @param affil.sep separator to use when concatenating affiliations
#' @param outfile 
#' 
#' @return 
#' A list with the following elements:
#' authors: A list of authors. Each author is a list with the following elements:
#'  * author: full author string
#'  * name: formatted name
#'  * affil.ref: vector of affiliation labels
#'  * equal.sym: the symbol used to denote equal contribution, or NA
#' affiliations: A list of affiliations. Each affiliation is a list with the following elements:
#'  * ID: unique affilaition ID
#'  * label: affiliation label
#'  * affiliation: full affiliation string
#' author.str: the formatted list of authors as a string
#' affiliation.str: the formatted list of affiliations of a string
format.authors <- function(authors, affiliations=NULL, 
                           author.format=format.full.name, author.format.args=NULL,
                           affil.labeling=identity, 
                           affil.format=format.affiliation, affil.format.args=NULL,
                           affil.refs.format=format.affiliation.refs, affil.refs.format.args=NULL,
                           equal.symbols='*', author.affil.ref.sep=' ', author.sep=", ", affil.sep="\n", 
                           outfile=NULL) {
    if (is.character(authors)) {
        infile <- authors
        authors <- read_excel(infile)
        if (is.null(affiliations)) {
            affiliations <- read_excel(infile, sheet=2)
        }
    }
    else if (is.null(authors) || is.null(affiliations)) {
        stop("authors and affiliations required")
    }

    id.order <- unique(unlist(strsplit(authors$Affiliations, '')))
    m <- match(id.order, affiliations$ID)
    if (any(is.na(m))) {
        stop(paste("Missing affiliations", paste(id.order[is.na(m)], sep=",")))
    }
    affiliations <- affiliations[m,]
    
    affil.list <- lapply(1:nrow(affiliations), function(i) {
        label <- affil.labeling(i)
        fields <- affiliations[i, c('Department','Institution','City','Locality','Country')]
        affil.str <- do.call(affil.format, c(list(label, fields), affil.format.args))
        list(ID=affiliations[i,'ID'], label=label, affiliation=affil.str)
    })
    names(affil.list) <- id.order
    
    affil.ref <- lapply(authors$Affiliations, function(x) {
        ids <- strsplit(x, '')[[1]]
        labels <- unlist(lapply(affil.list[ids], "[[", "label"))
        affil.str <- do.call(affil.refs.format, c(list(labels), affil.refs.format.args))
        list(ids=ids, affil.str=affil.str)
    })
    
    name.str <- apply(authors, 1, function(x) 
        do.call(author.format, c(list(x), author.format.args)))
    author.str <- paste(name.str, unlist(lapply(affil.ref, "[[", "affil.str")), 
                        sep=author.affil.ref.sep)
    
    eq <- unique(authors$Equal[!is.na(authors$Equal)])
    eq.sym <- rep(NA, length(author.str))
    stopifnot(length(equal.symbols) >= length(eq))
    
    for (i in 1:length(eq)) {
        symbol <- equal.symbols[i]
        w <- which(authors$Equal == eq[i])
        eq.sym[w] <- symbol
        author.str[w] <- paste0(author.str[w], symbol)
    }
    
    author.list <- lapply(1:length(author.str), function(i) list(
        author=author.str[i], name=name.str[i],
        affil.ref=affil.ref[i], equal.sym=eq.sym[i]
    ))
    
    retval <- list(authors=author.list, affiliations=affil.list, 
         author.str=paste(lapply(author.list, "[[", "author"), collapse=author.sep),
         affiliation.str=paste(lapply(affil.list, "[[", "affiliation"), collapse=affil.sep))
    
    if (!is.null(outfile)) {
        writeLines(c(retval$author.str, retval$affiliation.str), outfile)
    }
    
    invisible(retval)
}
