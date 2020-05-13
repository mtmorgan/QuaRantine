## use the rentrez package to find the nucore id of the genbank entry,
## and to retrieve the full genbank recored.

suppressPackageStartupMessages({
    library(rentrez)
})

accession <- "NC_045512"
id <- entrez_search("nuccore", accession)$id
gb <- entrez_fetch("nuccore", id, rettype = "gbwithparts", retmode = "text")
writeLines(gb, file.path("assets", "05-NC_045512.gbk"))
