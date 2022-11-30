#' arrange_gt_data
#'
#' Reads in a directory of VCF files and converts them into a single dataframe
#'
#' @name arrange_gt_data
#' @param vardir Directory path containing vcf files
#' @param reference_fasta Reference fasta file used for alignment
#' @param annotated Whether the VCF files are annotated using snpeff "yes" or "no" (default "yes")
#' @param ntlist Nucleotides (default A, T, G, C) used for finding multiple alt alleles
#' @return A large dataframe containing information from all input VCF files
#' @importFrom magrittr %>%
#' @export
arrange_gt_data = function(vardir, reference_fasta, annotated = 'yes', ntlist=c('A','G','T','C','-')){

    fix_list = c('ChromKey','CHROM','POS','ID','REF','ALT')

    gt_list = c("ChromKey","POS","gt_AD","gt_DP")

    sizes = read_reference_fasta_dna(reference_fasta)

    filelist = Sys.glob(glue::glue("{vardir}/*.vcf"))

    message("Length of input files: ", length(filelist))

    message("Input vcf files include: ", list(filelist))

    all_files = data.frame()  # empty df to build to

    for (filename in filelist){

        samplename = basename(tools::file_path_sans_ext(filename)) # grab sample name to append later

        message("Sample name is: ", samplename)

        vcf_all = vcfR::read.vcfR(file=filename)  # read in vcf file using vcfR

        vcf_tidy = vcfR::vcfR2tidy(vcf_all)  # change into a tidy dataframe

        if (annotated = 'yes'){

          # $fix contains the INFO fields
          vcf_fix = vcf_tidy$fix %>% dplyr::select(tidyselect::all_of(fix_list), 'ANN')

        } else{
          # $fix contains the INFO fields
          vcf_fix = vcf_tidy$fix %>% dplyr::select(tidyselect::all_of(fix_list))
        }

        # $gt contains the genotype information. grab info we want
        vcf_gt = vcf_tidy$gt %>% dplyr::select(tidyselect::all_of(gt_list))

        vcf_total = merge(vcf_fix, vcf_gt, by = c("ChromKey","POS"), all= TRUE)

        # if variants exist
        if (nrow(vcf_total) > 0){

            vcf_total$sample = samplename

            snp_df = vcf_total %>% dplyr::filter(REF %in% ntlist & ALT %in% ntlist) # only one ref and alt allele

            snp_df = snp_df %>% tidyr::separate(gt_AD, c("REF_COUNT","ALT_COUNT"), sep = '[,]') # IT WILL THROW AN ERROR IF THERE ARE MULT ALLELES

            mult_alt = vcf_total %>% dplyr::filter(REF %in% ntlist & !ALT %in% ntlist)  # mult alt alleles - need to use later!!

            if (nrow(snp_df) > 0){

                # one reference, one alt
                snp_df$REF_COUNT = as.numeric(as.character(snp_df$REF_COUNT))  # change to numeric to calc freq

                snp_df$ALT_COUNT = as.numeric(as.character(snp_df$ALT_COUNT))

                snp_df$REF_FREQ = snp_df$REF_COUNT/snp_df$gt_DP

                snp_df$ALT_FREQ = snp_df$ALT_COUNT/snp_df$gt_DP

                snp_df$ALT_TYPE <- ifelse(snp_df$ALT_FREQ < 0.50,
                        "minor","major")

                all_files = rbind(all_files, snp_df)

              } else{message("No snps for sample: ", samplename)}

            }else{message(glue::glue("No variant data: ", samplename))}

        }

    # rearranging the df
    all_files = all_files %>% dplyr::mutate(majorfreq = ifelse(ALT_TYPE == 'major', ALT_FREQ, REF_FREQ),
                                         minorfreq = ifelse(ALT_TYPE == 'minor', ALT_FREQ, REF_FREQ),
                                         major = ifelse(ALT_TYPE == 'major', ALT, REF),
                                         minor = ifelse(ALT_TYPE == 'minor', ALT, REF),
                                         majorcount = ifelse(ALT_TYPE == "major", ALT_COUNT,REF_COUNT),
                                         minorcount = ifelse(ALT_TYPE == "minor", ALT_COUNT,REF_COUNT))

    all_files = dplyr::select(all_files, c(sample,CHROM,POS,REF,ALT,ANN,
                                    gt_DP,REF_COUNT,ALT_COUNT,REF_FREQ,ALT_FREQ,ALT_TYPE,
                                    major,minor,majorcount,minorcount,majorfreq,minorfreq))

    all_files = all_files[!duplicated(all_files), ] %>% droplevels()


    return(all_files)
}
