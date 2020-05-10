## retrieve DNA sequences from all New York State genbank records

suppressPackageStartupMessages({
    library(dplyr)
    library(genbankr)
    library(rentrez)
    library(Biostrings)
    options(Biostrings.coloring = FALSE)
})

get_accession_sequence <-
    function(accession, pb = NULL)
{
    ## progress bar, from dplyr
    if (!is.null(pb))
        pb$tick()$print()
    tryCatch({
        ## try the following; if it fails note as a message but continue on
        suppressMessages({
            ## find the genbank record id from the accession number
            id <- entrez_search("nuccore", accession)$id
            ## fetch the genbank record to a character vector
            text <- entrez_fetch(
                "nuccore", id, rettype = "gbwithparts", retmode = "text"
            )
            ## translate the character vector to a 'GenBankRecord'
            result <- readGenBank(text = text)
            ## extract the sequence as a DNAStringSet
            getSeq(result)
        })
    }, error = function(e) {
        message("'", accession, "' failed")
    })
}

genbank <- readr::read_csv(file.path("assets", "05-genbank-accessions.csv"))

ny <- genbank %>%
    ## New York state records
    filter(country == "USA", region == "NY")

dna <-
    ny %>% {
        n <- nrow(.)                     # how many records?
        pb <- dplyr::progress_estimated(n) # start 'progress bar'
        ## get each accession
        map(.$accession, get_accession_sequence, pb)
    }

dna <- do.call(c, dna)
names(dna) <- ny$accession

fasta_file <- file.path("assets", "05-SARS-CoV-2-NY.fasta")
writeXStringSet(dna, fasta_file)
