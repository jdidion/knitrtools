# Deal with paths in knitr documents without having to setwd()

set_root <- function(dir) {
    assign("_root_", dir, envir=globalenv())
}

# Resolve a path relative to the current rootDir
rel_path <- function(...) {
    file.path(get("_root_", envir=globalenv()), ...)
}

# Load an R data file and return a list containing
# the contents
load_data <- function(..., return.list=FALSE, obj.names=NULL) {
    e <- new.env()
    if (is.null(obj.names)) {
        obj.names <- load(rel_path(...), envir=e)
    }
    else {
        load(f, envir=e)
    }
    if (length(obj.names) > 1 || return.list) {
        return(mget(obj.names, envir=e))
    }
    else {
        return(get(obj.names[1], envir=e))
    }
}

load_table <- function(filename, ...) {
    read.table(rel_path(filename), ...)
}

# Load an R file as a module
import_module <- function(...) {
    modules::import(rel_path(...))
}