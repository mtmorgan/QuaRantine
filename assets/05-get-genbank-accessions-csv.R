## Downlaod and parse genbank accessions

suppressPackageStartupMessages({
    library(yaml)
    library(purrr)
    library(readr)
    library(dplyr)
    library(tibble)
    library(tidyr)
})

## input is a 'yaml' json file -- a nested list of entries
url <- "https://www.ncbi.nlm.nih.gov/projects/genome/sars-cov-2-seqs/ncov-sequences.yaml"
ncov <- yaml::read_yaml(url)

ncov$updated

## convert list-of-lists to a tidy tibble
genbank0 <- ncov$"genbank-sequences"
idx <- setNames(
    names(genbank0[[1]]),
    c("accession", "link", "date", "country")
)

## use the 'purrr' package to simply list structure. The original
## structure is a list where each element is itself a list with fields
## describinig the accession, link , date, and country of the
## sequence.
##
##     list(
##         list(accession = "MT447189", link = ...)
##         list(accession = ...)
##     )
##
## 'flatten' this to a list with four elements -- vectors describing
## accession, link, date, and country of the samples.
##
##     list(
##         accession = c("MT447189", ...),
##         link = c(...),
##         ...
##     )
## 
genbank1 <-
    map(idx, function(x, i) {
        ## for each entry, choose the ith element (e.g., 'country') or
        ## NA, if no field (some records don't include 'country')
        map(x, pluck, i, .default = NA_character_) %>%
            unlist()
    }, x = genbank0) %>%
    as_tibble()

genbank <-
    genbank1 %>%
    ## remove link markup and format date
    mutate(
        link = sub('<a href="(.*)".*', "\\1", link),
        date = as.Date(date, format = "%Y-%m-%d")
    ) %>%
    ## separate 'country' field somewhat; records could be 'USA',
    ## 'USA: New York', "USA: New York, Buffalo", we'll create a field
    ## for country, and anything else (e.g., "", "New York", 'New
    ## York, Buffalo")
    separate(country, c("country", "region"), ": ", fill = "right")

write_csv(genbank, "05-genbank-accessions.csv")
