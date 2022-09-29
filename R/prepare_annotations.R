#' prepare_annotations
#'
#' Separates the SNPeff annotations found in an annotated and rearranged VCF dataframe (arranged using arrange_gt_data)
#'
#' @name prepare_annotations
#' @param df A rearranged and annotated VCF dataframe
#' @return A dataframe containing each annotation on a separate column
#' @export
#' @examples
#' \dontrun{
#' prepare_annotations(df)
#' }
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

        multi_anno1 = multi_anno %>% dplyr::select(!all_of(snpeff2)) # separate by first annotation

        multi_anno2 = multi_anno %>% dplyr::select(!all_of(snpeff)) # separate by second annotation

        colnames(multi_anno2) = c(colnames(multi_anno1)) # change col names to match

        multi_anno = rbind(multi_anno1, multi_anno2) # concat annotation dfs to make long df

        multi_anno = rbind(single_anno, multi_anno) # concat with single annotations to have all

        # deduplicate just in case
        multi_anno = multi_anno[!duplicated(multi_anno), ] %>% droplevels()

        return(multi_anno)
      }
}
