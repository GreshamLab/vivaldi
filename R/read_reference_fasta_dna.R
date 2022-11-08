#' read_reference_fasta_dna
#'
#' Imports reference fasta, generates a dataframe with chroms and chrom lengths
#'
#' @name read_reference_fasta_dna
#' @param reference_fasta The name and location of the reference fasta file used for alignment
#' @return A dataframe containing the chroms and chrom lengths of a reference fasta
#' @export
read_reference_fasta_dna = function(reference_fasta){

    f = seqinr::read.fasta(file = reference_fasta, as.string = TRUE)

    fdf = as.data.frame(do.call(rbind, lapply(f, function(x) nchar(x[1]))))

    fdf$CHROM = row.names(fdf)

    row.names(fdf) = NULL

    fdf = fdf %>% dplyr::rename(CHROM_SIZE = V1) %>%
                    dplyr::select(CHROM, CHROM_SIZE) %>%
                    dplyr::arrange(dplyr::desc(CHROM_SIZE)) %>%
                    dplyr::mutate(GENOME_SIZE = sum(CHROM_SIZE))

    message("Number of chroms in fasta: ", nrow(fdf))

    return(fdf)

}
