#' arrange_no_gt_data
#'
#' Reads in a directory of VCF files and converts them into a single dataframe for tools without GT information
#'
#' @name arrange_gt_data
#' @param vardir Directory path containing vcf files
#' @param reference_fasta Reference fasta file used for alignment
#' @param annotated Whether the VCF files are annotated using snpeff "yes" or "no" (default "yes")
#' @param ntlist Nucleotides (default A, T, G, C) used for finding multiple alt alleles
#' @return A large dataframe containing information from all input VCF files
#' @export
#' @examples
#' arrange_gt_data(vardir, reference_fasta = reference, annotated = 'yes')
arrange_no_gt_data = function(vardir, reference_fasta, annotated = 'yes', ntlist=c('A','G','T','C','-')){
  
  fix_list = c('CHROM','POS','ID','REF','ALT')
  
  sizes = read_reference_fasta_dna(reference_fasta)
  
  filelist = Sys.glob(glue("{vardir}/*.vcf"))
  
  message("Length of input files: ", length(filelist))
  
  message("Input vcf files include: ", list(filelist))
  
  all_files = data.frame()  # empty df to build to
  
  for (filename in filelist){
    
    samplename = basename(file_path_sans_ext(filename)) # grab sample name to append later
    
    message("Sample name is: ", samplename)
    
    vcf_all = read.vcfR(file=filename)  # read in vcf file using vcfR
    
    vcf_tidy = vcfR2tidy(vcf_all, info_only = TRUE)  # change into a tidy dataframe
    
    vcf_fix = vcf_tidy$fix %>% select(all_of(fix_list))
    
    gt_DP = extract.info(x = vcf_all, element = c("DP"), as.numeric = TRUE, mask = FALSE) 
    length(gt_DP) == nrow(vcf_all) # checking to make sure that these contain the same number of variants
    DP_df = data.frame(gt_DP)
    
    AF = extract.info(x = vcf_all, element = c("AF"), as.numeric = TRUE, mask = FALSE)
    length(AF) == nrow(vcf_all)
    AF_df = data.frame(AF) 
    
    vcf_fix = cbind(vcf_fix, DP_df)
    vcf_fix = cbind(vcf_fix, AF_df)
    
    # if variants exist
    if (nrow(vcf_fix>0)){
      
      vcf_fix$sample = samplename
      
      snp_df = vcf_fix %>% filter(REF %in% ntlist & ALT %in% ntlist) # only one ref and alt allele
      
      mult_alt = vcf_fix %>% filter(REF %in% ntlist & !ALT %in% ntlist)  # mult alt alleles - need to use later!!
      
      if (nrow(snp_df) > 0){
        
        # one reference, one alt
        snp_df$gt_DP = as.numeric(as.character(snp_df$gt_DP))   # change to numeric to calc freq
        
        snp_df$ALT_FREQ = as.numeric(as.character(snp_df$AF)) # can't directly calculate AF ourselves based on depth, just have to go with what freebayes outputs
        
        snp_df$REF_FREQ =  as.numeric(as.character(1 - snp_df$ALT_FREQ))
        
        snp_df$ALT_TYPE <- ifelse(snp_df$ALT_FREQ < 0.50, "minor","major")
        
        all_files = rbind(all_files, snp_df)
        
      } else{message("No snps for sample: ", samplename)}
      
    }else{message(glue("No variant data: ", samplename))}
    
  }
  
  # rearranging the df
  all_files = all_files %>% mutate(majorfreq = ifelse(ALT_TYPE == 'major', ALT_FREQ, REF_FREQ),
                                   minorfreq = ifelse(ALT_TYPE == 'minor', ALT_FREQ, REF_FREQ),
                                   major = ifelse(ALT_TYPE == 'major', ALT, REF),
                                   minor = ifelse(ALT_TYPE == 'minor', ALT, REF))
  
  all_files = all_files[!duplicated(all_files), ] %>% droplevels()
  
  all_files = add_metadata(all_files, sizes, c('CHROM'), c('CHROM'))
  
  return(all_files)
}
