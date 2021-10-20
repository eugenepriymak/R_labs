get_subjects_df = function() {
  get_included_row_indices = function(all_row_indices,
                                      excluded_row_indices) {
    ind = 1
    included_row_indices = c()
    while (ind <= length(all_row_indices)) {
      if ( !(all_row_indices[ind] %in% excluded_row_indices) ) {
        included_row_indices = c(included_row_indices, all_row_indices[ind])
      }
      ind = ind + 1
    }
    return(included_row_indices)
  }
  
  input_env_path = "external_data/ExpImp.RData"
  input_env = load(input_env_path)
  all_row_indices = 1:nrow(ExpImp)
  excluded_row_indices = c(1, 2, 21, 24, 25, 35, 42, 50, 65, 68, 69, 74, 87)
  included_row_indices = get_included_row_indices(all_row_indices,
                                                  excluded_row_indices)
  return(ExpImp[included_row_indices,])
}


get_export_sums_col = function(df_subjects) {
  export_indices = c(2, 4, 6, 8, 10, 12)
  export_sums = c()
  
  row_ind = 1
  while(row_ind <= nrow(df_subjects)) {
    export_sum = 0.0
    i = 1
    while(i <= length(export_indices)) {
      col_ind = export_indices[i]
      if( !is.na(as.numeric(df_subjects[row_ind, col_ind])) ) {
        export_sum = export_sum + as.numeric(df_subjects[row_ind, col_ind])
      }
      i = i + 1
    }
    export_sums = c(export_sums, export_sum)
    
    row_ind = row_ind + 1
  }
  return(data.frame(export_sums_col=export_sums))
}


get_import_sums_col = function(df_subjects) {
  import_indices = c(3, 5, 7, 9, 11, 13)
  import_sums = c()
  
  row_ind = 1
  while(row_ind <= nrow(df_subjects)) {
    import_sum = 0.0
    i = 1
    while(i <= length(import_indices)) {
      col_ind = import_indices[i]
      if( !is.na(as.numeric(df_subjects[row_ind, col_ind])) ) {
        import_sum = import_sum + as.numeric(df_subjects[row_ind, col_ind])
      }
      i = i + 1
    }
    import_sums = c(import_sums, import_sum)
    
    row_ind = row_ind + 1
  }
  return(data.frame(import_sums_col=import_sums))
}


get_where_export_sum_greater = function(df_subjects,
                                        export_sum_col_ind,
                                        import_sum_col_ind) {
  found_subjects = c()
  row_ind = 1
  while(row_ind <= nrow(df_subjects)) {
    if(df_subjects[row_ind, export_sum_col_ind] >
       df_subjects[row_ind, import_sum_col_ind]) {
      found_subjects = c(found_subjects, df_subjects[row_ind, 1])
    }
    row_ind = row_ind + 1
  }
  return(found_subjects)
}


df_subjects = get_subjects_df()
export_sums_col = get_export_sums_col(df_subjects)
import_sums_col = get_import_sums_col(df_subjects)
df_subjects = cbind(df_subjects, export_sums_col, import_sums_col)
found_subject = get_where_export_sum_greater(df_subjects,
                                             ncol(df_subjects)-1,
                                             ncol(df_subjects))