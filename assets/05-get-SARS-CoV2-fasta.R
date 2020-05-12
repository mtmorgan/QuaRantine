## retrieve DNA sequences from all New York State genbank records

suppressPackageStartupMessages({
    library(dplyr)
    library(genbankr)
    library(rentrez)
    library(Biostrings)
    options(Biostrings.coloring = FALSE)
})

workdir <- "workdir"
destination <- file.path(workdir, "genbank_accessions")
if (!dir.exists(destination))
    dir.create(destination, recursive = TRUE)

genbank_accession <-
    function(accession, destination, pb = NULL, force = FALSE)
{
    ## progress bar, from dplyr
    if (!is.null(pb))
        pb$tick()$print()

    ## don't re-download existing records
    path <- file.path(destination, accession)
    if (file.exists(path) && !force)
        return(path)

    ## try the following; if it fails note as a message but continue on
    tryCatch(suppressMessages({
        ## find the genbank record id from the accession number
        id <- entrez_search("nuccore", accession)$id
        ## fetch the genbank record to a character vector
        text <-
            entrez_fetch("nuccore", id, rettype = "gbwithparts", retmode = "text")
        writeLines(text, path)
        path
    }), error = function(e) {
        message("'", accession, "' failed: ", conditionMessage(e))
        NA_character_
    })
}

genbank_sequence <-
    function(path, pb = NULL)
{
    if (!is.null(pb))
        pb$tick()$print()

    tryCatch(suppressMessages({
        gbk <- readGenBank(path)
        cds <- cds(gbk)
        ref <- getSeq(gbk)
        as.character(ref)
    }), error = function(e) {
        message("'", basename(path), "' failed: ", conditionMessage(e))
        NA_character_
    })
}

genbank_gene_sequence <-
    function(path, .gene, pb = NULL)
{
    if (!is.null(pb))
        pb$tick()$print()
    
    tryCatch(suppressMessages({
        gbk <- readGenBank(path)
        cds <- cds(gbk)
        if (!"gene" %in% names(mcols(cds)))
            ## no gene annotation for this record
            return(NA_character_)
        ref <- getSeq(gbk)
        seq <- getSeq(ref, subset(cds, gene == .gene))
        as.character(seq)
    }), error = function(e) {
        message("'", basename(path), "' failed: ", conditionMessage(e))
        NA_character_
    })
}

dnastringset <-
    function(genbank)
{
    genbank %>%
        filter( (!is.na(sequence)) | (nchar(sequence) == 0) ) %>%
        mutate(dna = setNames(.$sequence, .$accession)) %>%
        pull(dna) %>%
        DNAStringSet()
}

genbank <- readr::read_csv(file.path("assets", "05-genbank-accessions.csv"))

## download to disk

pb <- dplyr::progress_estimated(nrow(genbank)) # start 'progress bar'
genbank <-
    genbank %>%
    group_by(accession) %>%
    mutate(
        path = genbank_accession(accession, destination = destination, pb = pb)
    ) %>%
    ungroup()

## NY State sequences

pb <- dplyr::progress_estimated(nrow(genbank)) # start 'progress bar'
dna <-
    genbank %>%
    filter( country == "USA", region == "NY" ) %>%
    group_by(accession) %>%
    mutate(sequence = genbank_sequence(path, pb = pb)) %>%
    ungroup() %>%
    dnastringset()
    
writeXStringSet(dna, "assets/05-SARS-CoV2-NY.fasta")


## 'S' sequence

.gene = "S"
pb <- dplyr::progress_estimated(nrow(genbank)) # start 'progress bar'
dna <-
    genbank %>%
    mutate(path = file.path(destination, accession)) %>%
    group_by(accession) %>%
    mutate(sequence = genbank_gene_sequence(path, .gene, pb = pb)) %>%
    ungroup() %>%
    dnastringset()

writeXStringSet(dna, "assets/05-SARS-CoV2-S.fasta")
