#' Format one or more names as a set of initials
#' @param initial.sep string separating consecutive initials
format.initial <- function(initial.sep='') {
    function(x) {
        parts <- strsplit(x, ' ', fixed=T)[[1]]
        initials <- toupper(sapply(parts, substr, 1, 1))
        paste(initials, sep=initial.sep)
    }
}

#' Format an author as a full name (First MI Last)
#' @param suffix.sep string separating the suffixe (e.g. Jr.) from the last name
format.full.name <- function(suffix.sep=' ') {
    function(first, middle, last, suffix, initial.format) {
        author.str <- first
        if (!is.na(middle)) {
            middle <- initial.format(middle)
            author.str <- paste(author.str, middle)
        }
        author.str <- paste(author.str, last)
        if (!is.na(suffix)) {
            author.str <- paste0(author.str, suffix.sep, suffix)
        }
        author.str
    }
}

# Format an author as initials followed by last name (FM Last)
#' @param suffix.sep string separating the suffixe (e.g. Jr.) from the last name
format.initial.first <- function(suffix.sep=' ') {
    function(first, middle, last, suffix, initial.format) {
        init.str <- fmt.initial(first)
        if (!is.na(middle)) {
            middle <- initial.format(middle)
            init.str <- paste0(init.str, middle)
        }
        author.str <- paste(init.str, last)
        if (!is.na(fields$Suffix)) {
            author.str <- paste0(author.str, suffix.sep, suffix)
        }
        author.str
    }
}

#' Format an author as a last name followed by initials (Last FM)
#' @param suffix.sep string separating the suffixe (e.g. Jr.) from the last name
#' @parma first.last.sep when `naming == 'initial_after'`, the separtor between the
#' last name and the initials
format.initial.last <- function(suffix.sep=' ', first.last.sep=', ') {
    function(first, middle, last, suffix, initial.format) {
        init.str <- fmt.initial(first)
        if (!is.na(middle)) {
            middle <- initial.format(middle)
            init.str <- paste0(init.str, middle)
        }
        author.str <- last
        if (!is.na(suffix)) {
            author.str <- paste0(author.str, suffix.sep, suffix)
        }
        author.str <- paste0(author.str, first.last.sep, init.str)
        author.str
    }
}

#' Format an affiliation.
#' @param label.sep separator between afiliation label and string
format.affiliation <- function(label.sep=". ", part.sep=", ") {
    function(label, ...) {
        fields <- list(...)
        fields <- fields[!is.na(fields)]
        paste(label, paste(fields, collapse=part.sep), sep=label.sep)
    }
}

#' Format a list of affiliation references.
#' @param affil.sep separator between multiple affiliations after an author name
#' @param affil.wrap function to wrap the list of an author's affiliations
format.affiliation.refs <- function(affil.sep=',', affil.wrap=NULL) {
    function(labels) {
        affil.str <- paste(labels, collapse=affil.sep)
        if (!is.null(affil.wrap)) {
            affil.str <- affil.wrap(affil.str)
        }
        affil.str
    }
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
#' @param affil.labeling function for converting an integer index into the label for the
#' affiliation
#' @param affil.format function to format an affiliation
#' @param affil.refs.format function to format list of affiliation references
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
                           author.format=format.full.name(),
                           initial.format=format.initial(),
                           affil.labeling=identity, 
                           affil.format=format.affiliation(), 
                           affil.refs.format=format.affiliation.refs(), 
                           equal.symbols='*', author.affil.ref.sep='') {
    if (is.character(authors)) {
        infile <- authors
        authors <- data.table(read_excel(infile))
        if (is.null(affiliations)) {
            affiliations <- data.table(read_excel(infile, sheet=2))
        }
    }
    else if (is.null(authors) || is.null(affiliations)) {
        stop("authors and affiliations required")
    }

    authors[,AffilList := strsplit(Affiliations, "")]
    authors[,FormattedName := author.format(
        First, Middle, Last, Suffix, initial.format),
        by=1:nrow(authors)]
    
    id.order <- unique(unlist(authors$AffilList))
    m <- match(id.order, affiliations$ID)
    if (any(is.na(m))) {
        stop(paste("Missing affiliations", paste(id.order[is.na(m)], sep=",")))
    }
    
    affiliations <- affiliations[m,]
    affiliations[,Label := affil.labeling(.I), by=1:nrow(affiliations)]
    affiliations[,Formatted := affil.format(
        Label, Department, Institution, City, Locality, Country), 
        by=1:nrow(affiliations)]
    
    .format.affil.list <- function(ids) {
        labels <- affiliations[ID %in% ids, Label]
        affil.refs.format(labels)
    }
    authors[,AffilRef := .format.affil.list(AffilList[[1]]), by=1:nrow(authors)]
    
    eq.ids <- unique(authors[!is.na(Equal),Equal])
    if (length(equal.symbols) > length(eq.ids)) {
        warning("Not enough equal symbols; recycling")
        equal.symbols <- rep_len(equal.symbols, length(eq.ids))
    }
    authors$Equal <- factor(authors$Equal, levels=eq.ids)
    authors[!is.na(Equal), EqualSym := equal.symbols[as.integer(Equal)], by=which(!is.na(Equal))]
    
    authors[,Formatted := paste(FormattedName, AffilRef, sep=author.affil.ref.sep)]
    authors[!is.na(Equal), Formatted := paste0(Formatted, EqualSym)]
    
    list(authors=authors, affiliations=affiliations)
}

.format.list <- function(dt, sep, outfile) {
    str <- paste(dt[,Formatted], collapse=sep)
    if (!is.null(outfile)) {
        writeLines(str, outfile)
    }
    str
}

format.author.list <- function(authors, sep=", ", outfile=NULL) {
    .format.list(authors, sep, outfile)
}

format.affiliation.list <- function(affiliations, sep="\n", outfile=NULL) {
    .format.list(affiliations, sep, outfile)
}
