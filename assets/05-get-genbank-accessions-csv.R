suppressPackageStartupMessages({
    library(yaml)
    library(purrr)
    library(readr)
    library(dplyr)
    library(tibble)
    library(tidyr)
})

url <- "https://www.ncbi.nlm.nih.gov/projects/genome/sars-cov-2-seqs/ncov-sequences.yaml"
ncov <- yaml::read_yaml(url)

ncov$updated

genbank0 <- ncov$"genbank-sequences"
idx <- setNames(
    names(genbank0[[1]]),
    c("accession", "link", "date", "country")
)

genbank <-
    map(idx, function(x, i) {
        map(x, pluck, i, .default = NA_character_) %>%
            unlist()
    }, x = genbank0) %>%
    as_tibble() %>%
    mutate(
        link = sub('<a href="(.*)".*', "\\1", link),
        date = as.Date(date, format = "%Y-%m-%d")
    ) %>%
    separate(country, c("country", "region"), ": ", fill = "right")

write_csv(genbank, "05-genbank-accessions.csv")
