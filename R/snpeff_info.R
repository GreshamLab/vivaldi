#' snpeff_info
#'
#' Returns vector containing information in snpeff annotations
#'
#' @name snpeff_info
#' @return Returns vector containing information in snpeff annotations
#' @export
snpeff_info = function(){

  return(c('allele','annotation','putative_impact','gene_name','gene_id',
      'feature_type','feature_id','transcript_biotype','rank_total','HGVS.c','HGVS.p',
      'cDNA_position','CDS_position','protein_position','distance_to_feature'))
}
