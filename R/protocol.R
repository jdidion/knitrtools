# Convenience functions for making reagents
solutions <- list()

solution <- function(...) {
	s <- labmath::Solution(...)
	solutions[[s$name]] <- s
	invisible(s)
}

load.solutions <- function(data.file) {
    solutions <<- my.load(data.file, "solutions")
}

reagents <- function(...) {
	for (str in list(...)) {
		parts <- unlist(strsplit(str, " of ", fixed=TRUE))
		solution <- solutions[[parts[2]]]
		s <- solution$make(parts[1])
		print(s$format(markdown=TRUE))
	}
}

my.load <- function(f, obj.name) {
    e <- baseenv()
    load(f, envir=e)
    return(get(obj.names[1], envir=e))
}