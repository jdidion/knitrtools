knitr.defaults <- function(root.dir=NULL) {
    knitr::opts_chunk$set(echo=FALSE, cache=TRUE, warning=FALSE, message=FALSE,
                          dpi=150, fig.width=7.5, fig.height=6.5, fig.align='center', 
                          dev='pdf')
    if (!is.null(root.dir)) {
        opts_knit$set(root.dir=root.dir)
    }
    pander::panderOptions('table.split.table', Inf)
}