install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyverse")
library("ggplot2")
library("dplyr")
library("tidyverse")


get_subjects_tib = function() {
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
  
  external_workspace_path = "external_data/ExpImp.RData"
  external_workspace = load(external_workspace_path)
  source_tib = ExpImp
  all_row_indices = 1:nrow(source_tib)
  excluded_row_indices = c(1, 2, 21, 24, 25, 35, 42, 50, 65, 68, 69, 74, 87)
  included_row_indices = get_included_row_indices(all_row_indices,
                                                  excluded_row_indices)
  return(source_tib[included_row_indices,])
}


make_tib_tidy = function(source_tib) {
  unite_exp_and_imp_by_cargo = function(source_tib) {
    tidy_tib = source_tib %>% unite("Продовольственный", "ПродЭкспорт", "ПродИмпорт", sep=exp_imp_sep)
    tidy_tib = tidy_tib %>% unite("ТЭК", "ТЭКЭкспорт", "ТЭКИмпорт", sep=exp_imp_sep)
    tidy_tib = tidy_tib %>% unite("Химический", "ХимЭкспорт", "ХимИмпорт", sep=exp_imp_sep)
    tidy_tib = tidy_tib %>% unite("Древесный", "ДревЭкспорт", "ДревИмпорт", sep=exp_imp_sep)
    tidy_tib = tidy_tib %>% unite("Металлургический", "МетЭкспорт", "МетИмпорт", sep=exp_imp_sep)
    tidy_tib = tidy_tib %>% unite("Машиностроение", "МашЭкспорт", "МашИмпорт", sep=exp_imp_sep)
    return(tidy_tib)
  }
  
  exp_imp_sep = "_"
  tidy_tib = unite_exp_and_imp_by_cargo(source_tib)
  tidy_tib = tidy_tib %>% pivot_longer(c("Продовольственный",
                                         "ТЭК",
                                         "Химический",
                                         "Древесный",
                                         "Металлургический",
                                         "Машиностроение"),
                                       names_to="Груз",
                                       values_to="Экспорт_Импорт")
  tidy_tib = tidy_tib %>% separate("Экспорт_Импорт",
                                   into=c("Экспорт", "Импорт"),
                                   sep=exp_imp_sep)
  tidy_tib = tidy_tib %>% pivot_longer(c("Экспорт", "Импорт"),
                                       names_to="Операция",
                                       values_to="Объём")
  return(tidy_tib)
}


plot_graph_1 = function(tidy_tib) {
  fill_NAs_by_zero = function() {
    ind = 1
    while (ind <= nrow(metal_exp_imp)) {
      if (metal_exp_imp$`Объём`[ind] == "-") {
        metal_exp_imp$`Объём`[ind] <<- "0"
      }
      ind = ind + 1
    }
  }
  
  metal_exp_imp = tidy_tib %>% filter(Груз=="Металлургический")
  fill_NAs_by_zero()
  graph = ggplot(data=metal_exp_imp,
                 aes(x=`Регион`, y=as.numeric(metal_exp_imp$`Объём`), fill=`Операция`))
  graph = graph + geom_col(position="Dodge")
  graph = graph + labs(title="Экспорт металлургической продукции в субъектах РФ",
                       x="Субъект РФ",
                       y="Объём перевозок (у. е.)")
  graph = graph + coord_flip()
  return(graph)
}


plot_graph_2 = function(tidy_tib) {
  fill_NAs_by_zero = function() {
    ind = 1
    while (ind <= nrow(metal_exp_imp)) {
      if (metal_exp_imp$`Объём`[ind] == "-") {
        metal_exp_imp$`Объём`[ind] <<- "0"
      }
      ind = ind + 1
    }
  }
  
  calc_percents_and_negate_import = function() {
    exports = metal_exp_imp %>% filter(Операция=="Экспорт")
    sum_export = sum( as.numeric(exports$`Объём`) )
    
    imports = metal_exp_imp %>% filter(Операция=="Экспорт")
    sum_import = sum( as.numeric(imports$`Объём`) )
    
    percents = vector("character", 0)
    ind = 1
    while (ind <= nrow(metal_exp_imp)) {
      amount = as.numeric(metal_exp_imp$`Объём`[ind])
      label = ""
      if (metal_exp_imp$`Операция`[ind] == "Экспорт") {
        percent = round(amount/sum_export*100, 1)
        if (percent >= min_shown_percent) {
          label = paste0(as.character(amount), " (", as.character(percent), " %)")
        }
      }
      else {
        percent = round(amount/sum_import*100, 1)
        if (percent >= min_shown_percent) {
          label = paste0(as.character(amount), " (", as.character(percent), " %)")
        }
        metal_exp_imp$`Объём`[ind] <<- -amount
      }
      percents = c(percents, label)
      ind = ind + 1
    }
    return(percents)
  }
  
  min_shown_percent = 1.0
  metal_exp_imp = tidy_tib %>% filter(Груз=="Металлургический")
  fill_NAs_by_zero()
  percents = calc_percents_and_negate_import()
  graph = ggplot(data=metal_exp_imp, aes(x=`Регион`,
                                         y=as.numeric(metal_exp_imp$`Объём`),
                                         fill=`Операция`,
                                         percents=percents))
  graph = graph + geom_col(position="Dodge")
  graph = graph + labs(title="Экспорт металлургической продукции в субъектах РФ",
                       x="Субъект РФ",
                       y="Объём перевозок (у. е.)")
  graph = graph + geom_text(aes(label=percents), nudge_y=-1000)
  graph = graph + coord_flip()
  return(graph)
}


subjects_tib = get_subjects_tib()
tidy_tib = make_tib_tidy(subjects_tib)
g_1 = plot_graph_1(tidy_tib)
g_2 = plot_graph_2(tidy_tib)
g_1
g_2