split_string_into_row_cells = function(string) {
  
  read_to_next_double_quotes = function(from_ind, string) {
    len_to = 0
    ind = from_ind
    max_ind = nchar(string)
    while (ind <= max_ind) {
      if (substring(string, ind, ind) == '\"') {
        return(len_to)
      }
      ind = ind + 1
      len_to = len_to + 1
    }
    return(NA)
  }
  
  cell_values = list()
  cell_ind = 1
  string_length = nchar(string)
  ind = 1
  while (ind <= string_length) {
    len_to_dq = read_to_next_double_quotes(ind, string)
    if (is.na(len_to_dq)) {
      return(NA)
    }
    ind = ind + len_to_dq + 1
    
    len_to_dq = read_to_next_double_quotes(ind, string)
    if (is.na(len_to_dq)) {
      return(NA)
    }
    cell_value = substring(string, ind, ind+len_to_dq-1)
    ind = ind + len_to_dq + 1
    
    cell_values[[cell_ind]] = cell_value
    cell_ind = cell_ind + 1
  }
  return(cell_values)
}


transform_numeric_strings_to_numeric = function(tb) {
  
  remove_spaces_from_string = function(string) {
    ind = 1
    while (ind <= nchar(string)) {
      if (substring(string, ind, ind) == " " || substring(string, ind, ind) == "\t") {
        first_space_ind = ind
        while (substring(string, ind, ind) == " " || substring(string, ind, ind) == "\t" ) {
          ind = ind + 1
        }
        last_space_ind = ind - 1
        string_before_spaces = substring(string, 1, first_space_ind-1)
        string_after_spaces = substring(string, last_space_ind+1, nchar(string))
        string = paste0(string_before_spaces, string_after_spaces)
        ind = first_space_ind
      }
      else {
        ind = ind + 1
      }
    }
    return(string)
  }
  
  rows_number = length(tb)
  row_length = length(tb[[1]])
  row_ind = 1
  col_ind = 1
  while (row_ind <= rows_number) {
    while (col_ind <= row_length) {
      string = tb[[row_ind]][col_ind]
      string_without_spaces = remove_spaces_from_string(string)
      if ( !is.na(as.numeric(string_without_spaces)) ) {
        tb[[row_ind]][col_ind] = as.numeric( string_without_spaces )
      }
      col_ind = col_ind + 1
    }
    col_ind = 1
    row_ind = row_ind + 1
  }
  return(tb)
}


read_table_from_file = function(file_path) {
  ROW_LENGTH_IS_NOT_FILLED = -1
  rows = list()
  row_length = ROW_LENGTH_IS_NOT_FILLED
  row_ind = 1
  f = file(file_path, open="r")
  while (TRUE) {
    string = readLines(f, n=1)
    if (length(string) == 0) {
      break
    }
    
    row_cells = split_string_into_row_cells(string)
    if (length(row_cells) == 1 && is.na(row_cells)) {
      close(f)
      return(NA)
    }
    if (row_length == ROW_LENGTH_IS_NOT_FILLED) {
      row_length = length(row_cells)
    }
    else if (row_length != length(row_cells)) {
      close(f)
      return(NA)
    }
    
    rows[[row_ind]] = row_cells
    row_ind = row_ind + 1
  }
  close(f)
  return(rows)
}


fix_data = function(file_path) {
  tb = read_table_from_file(file_path)
  if (length(tb) == 1 && is.na(tb)) {
    return(NA)
  }
  tb = transform_numeric_strings_to_numeric(tb)
  return(tb)
}


tb = fix_data("resources/test_data_01.csv")