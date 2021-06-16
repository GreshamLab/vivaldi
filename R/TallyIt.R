# vivaldi package
# Kate Johnson

TallyIt = function(df, groupit, new_colname){
    #INPUT: dataframe and vector of variables want to group by to count
    #OUTPUT: count dataframe using the group variables

    df = df[!duplicated(df), ] %>% droplevels

    count_df = df %>% group_by_at(vars(all_of(groupit))) %>% tally()

    colnames(count_df)[colnames(count_df) == 'n'] = new_colname

    return(count_df)
}
