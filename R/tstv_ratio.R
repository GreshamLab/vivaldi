#' tstv_ratio
#'
#' Inputs a filtered and rearranged vcf dataframe and calculates the transition/transversion ratio
#'
#' @name tstv_ratio
#' @param df The filtered and rearranged variant dataframe
#' @param genome_sites The total size of the genome
#' @return A dataframe containing the calculated transition/transversion ratio (R or basic_tstv)
#' @export
#' @examples
#' tstv_ratio(df)
tstv_ratio = function(df){

    snpeff = snpeff_info()

    print

    chrom_groups = c('sample','CHROM',"CHROM_SIZE","GENOME_SIZE","tstv")

    purine = c("G","A")

    pyrimidine = c("T","C")

    if (length(intersect(colnames(df), snpeff)) > 0){

      df = df %>% select(!all_of(c(snpeff)))

      df = df[!duplicated(df), ] %>% droplevels()

    } else{

      df = df[!duplicated(df), ] %>% droplevels()

    }

    df$tstv <- ifelse(df$major %in% purine &
                            df$minor %in% purine |
                            df$major %in% pyrimidine &
                            df$minor %in% pyrimidine,
                      "transition","transversion")

    tstv_chrom = tally_it(df, chrom_groups, "tstv_chrom_count")

    # count numbers of ts and tv
    tstv_df = tstv_chrom %>%
                group_by(sample, tstv) %>%
                mutate(tstv_genome_count = sum(tstv_chrom_count)) %>% ungroup()

    tstv_df = tstv_df[!duplicated(tstv_df), ] %>% droplevels

    tstv_df =  tstv_df %>% pivot_longer(cols = c("tstv_chrom_count","tstv_genome_count"),
                                        names_to = "chrom_or_genome",
                                        values_to = "tstv_count",
                                      values_drop_na = FALSE)

    tstv_df =  tstv_df %>% pivot_wider(names_from = tstv, values_from = 'tstv_count', values_fill = 0)

    tstv_df$tstv_ratio = tstv_df$transition/tstv_df$transversion

    tstv_df$tstv_ratio_perkb = ifelse(tstv_df$chrom_or_genome == 'tstv_chrom_count',
                                      tstv_df$tstv_ratio/(tstv_df$CHROM_SIZE/1000),
                                      tstv_df$tstv_ratio/(tstv_df$GENOME_SIZE/1000))

    return(tstv_df)
}
