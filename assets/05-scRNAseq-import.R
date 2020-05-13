## From:
##   https://anvil.terra.bio/#workspaces/kco-incubator/COVID-19_cross_tissue_analysis
##   https://www.biorxiv.org/content/10.1101/2020.04.19.049254v1.full.pdf
##
## From the abstract: 'We identify specific subsets of respiratory
## epithelial cells as putative targets of viral infection, including
## subsets of epithelial cells in the nasal passages, lung and'

library(Matrix)
library(rhdf5)
library(dplyr)

.guess_dimnames <-
    function(file, verbose = FALSE)
{
    ls <- h5ls(file)
    paths <- paste0(sub("/$", "", ls$group), "/", ls$name)
    idx <- grep("^(/obs|/var).*/_index$", paths, value = TRUE)
    if (length(idx) != 2)
        idx <- grep("^(/obs|/var).*/index$", paths, value = TRUE)
    if (length(idx) == 2) {
        names(idx) <- substr(idx, 2, 4)
        list(
            as.character(h5read(file, idx[["var"]])),
            as.character(h5read(file, idx[["obs"]]))
        )
    } else {
        (!verbose) || { warning("unable to guess dimnames"); TRUE }
        list(NULL, NULL)
    }
}    

read_expression <-
    function(file, verbose = FALSE)
{
    dimnames <- .guess_dimnames(file, verbose)
    X <- h5read(file, "/X")
    ## sometimes 'h5ad' data are row-wise, sometimes column-wise
    by_column <- max(X$indices) + 1L != nrow(h5read(file, "/var")[[1]])
    if (by_column)
        dimnames <- rev(dimnames)
    m <- sparseMatrix(
        i = as.integer(X$indices) + 1L,
        p = as.integer(X$indptr),
        x = as.numeric(X$data),
        dimnames = dimnames
    )
    if (by_column)
        m <- t(m)
    m
}

read_expression_as_tibble <-
    function(file, verbose = FALSE)
{
    dimnames <- .guess_dimnames(file, verbose)
    X <- h5read(file, "/X")
    by_column <- max(X$indices) + 1L != nrow(h5read(file, "/var")[[1]])
}

.as_factor <-
    function(x, lvl)
{
    lvl <- trimws(lvl)
    value <- match(x, as.raw(seq_along(lvl) - 1))
    factor(lvl[value], levels = lvl)
}

read_obs <-
    function(file)
{
    obs <- h5read(file, "/obs")
    variables <- grep("^_", names(obs), value = TRUE, invert = TRUE)
    factors <- names(obs[["__categories"]])
    stopifnot(all(factors %in% variables))

    obs[factors] <- Map(.as_factor, obs[factors], obs[["__categories"]][factors])

    do.call(tibble, obs[variables])
}        
