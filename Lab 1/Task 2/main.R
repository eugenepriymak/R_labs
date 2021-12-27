load("resources/data.RData")


init_df_mean_temp = function() {
  ids_of_1st_df
}


get_patient_temp = function(day_ind, patient_id) {
  row_ind = 1
  rows_number = nrow(data[[day_ind]])
  while (row_ind <= rows_number) {
    stored_id = data[[day_ind]][row_ind, 1]
    if (stored_id == patient_id) {
      patient_temp = data[[day_ind]][row_ind, 2]
      return(patient_temp)
    }
    row_ind = row_ind + 1
  }
  return(NA)
}


delete_row_from_df = function(df, row_ind) {
  return(df[-c(row_ind), ])
}


get_id = function() {
  df_mean_temp = data.frame(id=data[[1]]$id, mean_temp=data[[1]]$temp)
  day_ind = 2
  days_number = length(data)
  while (day_ind <= days_number) {
    patient_ind = 1
    while (patient_ind <= nrow(df_mean_temp)) {
      patient_id = df_mean_temp[patient_ind, 1]
      next_temp = get_patient_temp(day_ind, patient_id)
      if (is.na(next_temp)) {
        df_mean_temp = delete_row_from_df(df_mean_temp, patient_ind)
      }
      else if (day_ind < days_number) {
        df_mean_temp[patient_ind, 2] = df_mean_temp[patient_ind, 2] + next_temp
        patient_ind = patient_ind + 1
      }
      else {
        df_mean_temp[patient_ind, 2] = (df_mean_temp[patient_ind, 2] + next_temp) / days_number
        patient_ind = patient_ind + 1
      }
    }
    day_ind = day_ind + 1
  }
  return(df_mean_temp)
}


df_mean_temp = get_id()