# vivaldi package
# Kate Johnson

TsTv_ratio = function(df, genome_sites){

    chrom_groups = c('sample','CHROM', "major", "minor","tstv")

    purine = c("G","A")

    pyrimidine = c("T","C")

    df$tstv <- ifelse(df$major %in% purine &
                            df$minor %in% purine |
                            df$major %in% pyrimidine &
                            df$minor %in% pyrimidine,
                      "transition","transversion")

    tstv_chrom = TallyIt(df, chrom_groups, "tstv_count")

    # count numbers of ts and tv
    tstv_df = tstv_chrom %>% #
                group_by(sample, tstv) %>%
                mutate(tstv_genome_sum = sum(tstv_count)) %>% ungroup()

    tstv_df$tstv_freq = tstv_df$tstv_genome_sum/genome_sites

    tstv_cast = tstv_df %>% select(sample, tstv, tstv_freq) %>% droplevels()

    tstv_cast = tstv_cast[!duplicated(tstv_cast), ] %>% droplevels

    tstv_cast = dcast(tstv_cast, sample~tstv, value.var = 'tstv_freq')

    # if no transitions transversions add 0
    tstv_cast$transversion[is.na(tstv_cast$transversion)] = 0

    tstv_cast$transition[is.na(tstv_cast$transition)] = 0

    # calculations of k2p distances
    tstv_cast$w1 = (1-2*tstv_cast$transition - tstv_cast$transversion)

    tstv_cast$w2 = 1-2*tstv_cast$transversion

    tstv_cast$s = -(0.5)*log(tstv_cast$w1) + 0.25*log(tstv_cast$w2)

    tstv_cast$v = -(0.5)*log(tstv_cast$w2)

    tstv_cast$R = tstv_cast$s/tstv_cast$v

    # calculating 'basic' ts/tv just to check k2p calculations removed tstv counts so mult by genome sites
    tstv_cast$basic_tstv = (tstv_cast$transition*genome_sites)/(tstv_cast$transversion*genome_sites)

    return(tstv_cast)
}
