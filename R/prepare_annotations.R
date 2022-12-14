#' prepare_annotations
#'
#' Separates the SNPeff annotations found in an annotated and rearranged VCF dataframe (arranged using arrange_data)
#'
#' @name prepare_annotations
#' @param df A rearranged and annotated VCF dataframe
#' @return A dataframe containing each annotation on a separate column
#' @export
#' @examples
#' # Example 1: Shows the separation of the ANN column based on | delimiter.
#' test <- data.frame( ANN = c("A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P"))
#'
#' # The ANN column will be split based on the strings in `snpeff_info()` and
#' # an additional "error" column.
#' snpeff_info()
#'
#' # Split the SNPeff annotations in "ANN" column and save to dataframe `df`
#' df <- prepare_annotations(df)
#'
#' # The one "ANN" column is split into 16 columns
#' dim(test)
#' dim(df)
#'
#' # Example 2: Actual annotation data
#' test <- data.frame( sample = c("m1","m1","m1","m1"),
#'   CHROM = c("H1N1_HA","H1N1_HA","H1N1_HA","H1N1_HA"),
#'   POS = c(1007, 1145, 1293, 1319),
#'   REF = c("G", "T", "T", "C"),
#'   ALT = c("A", "A", "C", "T"),
#'   ANN = c("A|missense_variant|MODERATE|CDS_H1N1_HA_1_1701|H1N1_HA|transcript|H1N1_HA.1|protein_coding|1/1|c.1007G>A|p.Arg336Lys|1007/1701|1007/1701|336/566||",
#'   "A|missense_variant|MODERATE|CDS_H1N1_HA_1_1701|H1N1_HA|transcript|H1N1_HA.1|protein_coding|1/1|c.1145T>A|p.Leu382Gln|1145/1701|1145/1701|382/566||",
#'   "C|synonymous_variant|LOW|CDS_H1N1_HA_1_1701|H1N1_HA|transcript|H1N1_HA.1|protein_coding|1/1|c.1293T>C|p.Gly431Gly|1293/1701|1293/1701|431/566||",
#'   "T|missense_variant|MODERATE|CDS_H1N1_HA_1_1701|H1N1_HA|transcript|H1N1_HA.1|protein_coding|1/1|c.1319C>T|p.Ala440Val|1319/1701|1319/1701|440/566||")
#' )
#'
#' # Split the SNPeff annotations in "ANN" column and save to dataframe `df`
#' df <- prepare_annotations(test)
#'
#' # The one "ANN" column is split into 16 columns
#' dim(test)
#' dim(df)
#'
prepare_annotations = function(df){

    if (!"ANN" %in% colnames(df)){

      message("No annotations present in dataframe - to annotate use SNPeff")

    } else{
        # names of elements selected from snpeff website
        snpeff = snpeff_info()

        snpeff2 = c()

        for (i in snpeff){

          snpeff2 = c(snpeff2, glue::glue("{i}2"))

        }

        snpeff_multi = c(snpeff, snpeff2, 'errors')

        snpeff_length = length(snpeff)

        message(">2 annotations: ", list(levels(factor((df %>%
                    dplyr::filter(lengths(gregexpr("[|]", df$ANN)) > snpeff_length*2) %>%
                  droplevels())$sample))))

        # building just an annotation df
        # 16 different features provided by snpeff see website for more info
        single_anno = df %>%
                    dplyr::filter(lengths(gregexpr("[|]", df$ANN)) <= snpeff_length + 1) %>%
                  droplevels()

        single_anno = single_anno %>% tidyr::separate(ANN, c(snpeff, 'errors'), "[|]") %>% droplevels()

        # two annotations will be more than 16 elements but less than 45 (which would indicate 3). In total it should be 30 elements
        multi_anno = df %>%
                    dplyr::filter(lengths(gregexpr("[|]", df$ANN)) > snpeff_length + 1 &
                          lengths(gregexpr("[|]", df$ANN)) < snpeff_length*3) %>%
                  droplevels()

        multi_anno = multi_anno %>% tidyr::separate(ANN, snpeff_multi, "[|]") %>% droplevels()

        multi_anno1 = multi_anno %>% dplyr::select(!tidyselect::all_of(snpeff2)) # separate by first annotation

        multi_anno2 = multi_anno %>% dplyr::select(!tidyselect::all_of(snpeff)) # separate by second annotation

        colnames(multi_anno2) = c(colnames(multi_anno1)) # change col names to match

        multi_anno = rbind(multi_anno1, multi_anno2) # concat annotation dfs to make long df

        multi_anno = rbind(single_anno, multi_anno) # concat with single annotations to have all

        # deduplicate just in case
        multi_anno = multi_anno[!duplicated(multi_anno), ] %>% droplevels()

        return(multi_anno)
      }
}
