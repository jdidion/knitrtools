# Deal with paths in knitr documents without having to setwd()

assign("rootDir", getwd(), envir = globalenv())

set_root <- function(dir) {
    e <- globalenv()
    unlockBinding("rootDir", e)
    assign("rootDir", dir, envir = e)
    lockBinding("rootDir", e)
}

# Resolve a path relative to the current rootDir
path <- function(...) {
    file.path(rootDir, ...)
}

# Load an R data file and return a list containing
# the contents
load_data <- function(..., obj.names=NULL, return.list=FALSE) {
    e <- baseenv()
    if (is.null(obj.names)) {
        obj.names <- load(path(...), envir=e)
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

# Load an R file as a module
import_module <- function(...) {
    modules::import(path(...))
}