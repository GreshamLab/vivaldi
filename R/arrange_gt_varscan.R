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
#' @export
#' @examples
#' arrange_gt_data(vardir, reference_fasta = reference, annotated = 'yes')
arrange_gt_data_varscan = function(vardir, reference_fasta, annotated = 'yes', ntlist=c('A','G','T','C','-')){
  
  fix_list = c('CHROM','POS','ID','REF','ALT')
  
  gt_list = c("POS","gt_AD","gt_RD","gt_DP")
  
  sizes = read_reference_fasta_dna(reference_fasta)
  
  filelist = Sys.glob(glue("{vardir}/*.vcf"))
  
  message("Length of input files: ", length(filelist))
  
  message("Input vcf files include: ", list(filelist))
  
  all_files = data.frame()  # empty df to build to
  
  for (filename in filelist){
    
    samplename = basename(file_path_sans_ext(filename)) # grab sample name to append later
    
    message("Sample name is: ", samplename)
    
    vcf_all = read.vcfR(file=filename)  # read in vcf file using vcfR
    
    vcf_tidy = vcfR2tidy(vcf_all)  # change into a tidy dataframe
    
    if (annotated == 'yes'){
      
      # $fix contains the INFO fields
      vcf_fix = vcf_tidy$fix %>% select(all_of(fix_list), 'ANN')
      
    } else{
      # $fix contains the INFO fields
      vcf_fix = vcf_tidy$fix %>% select(all_of(fix_list))
    }
    
    # $gt contains the genotype information. grab info we want
    vcf_gt = vcf_tidy$gt %>% select(all_of(gt_list))
    
    vcf_total = merge(vcf_fix, vcf_gt, by = c("POS"), all= TRUE)
    
    # if variants exist
    if (nrow(vcf_total>0)){
      
      vcf_total$sample = samplename
      
      snp_df = vcf_total %>% filter(REF %in% ntlist & ALT %in% ntlist) # only one ref and alt allele
      
      #snp_df = snp_df %>% separate(gt_AD, c("REF_COUNT","ALT_COUNT"), sep = '[,]') # IT WILL THROW AN ERROR IF THERE ARE MULT ALLELES
      
      mult_alt = vcf_total %>% filter(REF %in% ntlist & !ALT %in% ntlist)  # mult alt alleles - need to use later!!
      
      if (nrow(snp_df) > 0){
        
        # one reference, one alt
        snp_df$REF_COUNT = as.numeric(as.character(snp_df$gt_RD))  # change to numeric to calc freq
        
        snp_df$ALT_COUNT = as.numeric(as.character(snp_df$gt_AD))
        
        snp_df$REF_FREQ = snp_df$REF_COUNT/snp_df$gt_DP
        
        snp_df$ALT_FREQ = snp_df$ALT_COUNT/snp_df$gt_DP
        
        snp_df$ALT_TYPE <- ifelse(snp_df$ALT_FREQ < 0.50,
                                  "minor","major")
        
        snp_df = select(snp_df, !c(gt_RD, gt_AD))
        
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
