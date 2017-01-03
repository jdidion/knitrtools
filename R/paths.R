# Deal with paths in knitr documents without having to setwd()

#' Set the root directory for your analysis.
#' 
#' @param dir path to directory
set_root <- function(dir) {
    assign("_root_", dir, envir=globalenv())
}

#' Resolve a path relative to the current rootDir
#' 
#' @param ... arguments to file.path to come after
#' the root directory.
rel_path <- function(...) {
    file.path(get("_root_", envir=globalenv()), ...)
}

#' Load an R data file and return a list containing
#' the contents.
#' 
#' @param ... arguments to rel_path
#' @param return.list whether to return a list for data files
#' that contain a single object.
#' @param obj.names names of objects to retrieve from the data
#' file. The default is to return all objects in the file.
load_data <- function(..., return.list=FALSE, obj.names=NULL) {
    e <- new.env()
    .names <- load(rel_path(...), envir=e)
    if (is.null(obj.names)) {
        obj.names <- .names
    }
    else {
        if (!all(obj.names %in% .names)) {
            stop(paste("One or more invalid names:", paste(obj.names, collapse=",")))
        }
    }
    if (length(obj.names) > 1 || return.list) {
        return(mget(obj.names, envir=e))
    }
    else {
        return(get(obj.names[1], envir=e))
    }
}

#' Read a table from a path relative to the root directory.
#' 
#' @param filename path to the table, realtive to the root dir.
#' @param sep table separator (default '\t')
#' @param header whether the table has a header row (default TRUE)
#' @param ... additional arguments to read.table
load_table <- function(filename, delim="\t", col_names=TRUE, ...) {
    readr::read_delim(rel_path(filename), delim=delim, col_names=col_names, ...)
}

#' Load an R file from a relative path as a module.
#' 
#' @param ... arguments to rel_path
import_module <- function(...) {
    modules::import(rel_path(...))
}

#' Render an external (png) image as if it were a normal figure.
#' 
#' @param ... arguments to rel_path
load_png <- function(..., render=FALSE) {
    library(png)
    img <- readPNG(rel_path(...))
    if (render) {
        library(grid)
        grid.raster(img)
    }
    else {
        img
    }
}
