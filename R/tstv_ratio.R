#' tstv_ratio
#'
#' Inputs a filtered and rearranged vcf dataframe and calculates the transition/transversion ratio
#'
#' @name tstv_ratio
#' @param df The filtered and rearranged variant dataframe
#' @param genome_size Size of whole genome being used
#' @return A dataframe containing the calculated transition/transversion ratio (R or basic_tstv)
#' @export
#' @examples
#' \dontrun{
#' tstv_ratio(df,genome_size)
#' }
tstv_ratio = function(df,genome_size){

    chrom_groups = c('sample','CHROM',"SegmentSize","tstv")

    purine = c("G","A")

    pyrimidine = c("T","C")

    df$tstv <- ifelse(df$major %in% purine &
                            df$minor %in% purine |
                            df$major %in% pyrimidine &
                            df$minor %in% pyrimidine,
                      "transition","transversion")

    tstv_chrom = tally_it(df, chrom_groups, "tstv_chrom_count")

    # count numbers of ts and tv
    tstv_df = tstv_chrom %>%
                dplyr::group_by(sample, tstv) %>%
                dplyr::mutate(tstv_genome_count = sum(tstv_chrom_count)) %>% dplyr::ungroup()

    tstv_df = tstv_df[!duplicated(tstv_df), ] %>% droplevels

    tstv_df =  tstv_df %>% tidyr::pivot_longer(cols = c("tstv_chrom_count","tstv_genome_count"),
                                        names_to = "chrom_or_genome",
                                        values_to = "tstv_count",
                                      values_drop_na = FALSE)

    tstv_df =  tstv_df %>% tidyr::pivot_wider(names_from = tstv, values_from = 'tstv_count', values_fill = 0)

    tstv_df$tstv_ratio = tstv_df$transition/tstv_df$transversion

    tstv_df$tstv_ratio_perkb = ifelse(tstv_df$chrom_or_genome == 'tstv_chrom_count',
                                      tstv_df$tstv_ratio/(tstv_df$SegmentSize/1000),
                                      tstv_df$tstv_ratio/(genome_size/1000))

    return(tstv_df)
}
