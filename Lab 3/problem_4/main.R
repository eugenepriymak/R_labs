install.packages("ggplot2")
install.packages("dplyr")
library("ggplot2")
library("dplyr")


EXTERNAL_DATA_PATH = "external_data/trades.RData"


get_exp_imp_compound_tables = function(external_data_path) {
  create_empty_table = function() {
    return( data.frame(sitc06=vector("character", 0),
                       partner=vector("character", 0),
                       time=vector("character", 0),
                       values=vector("numeric", 0)) )
  }
  
  fill_exp_imp_tables = function() {
    ext_table_ind = 1
    while( ext_table_ind <= length(trades) ) {
      ext_table = trades[[ext_table_ind]]
      necessary_col_indices = c(2, 4, 5, 6)
      
      exp_rows = ext_table %>% filter( indic_et=="Exports in million of ECU/EURO" )
      exp_rows = exp_rows[ seq(1, nrow(exp_rows)), necessary_col_indices ]
      exp_table <<- rbind(exp_table, exp_rows)
      
      imp_rows = ext_table %>% filter( indic_et=="Imports in million of ECU/EURO" )
      imp_rows = imp_rows[ seq(1, nrow(imp_rows)), necessary_col_indices ]
      imp_table <<- rbind(imp_table, imp_rows)
      
      ext_table_ind = ext_table_ind + 1
    }
  }
  
  res=load(EXTERNAL_DATA_PATH)
  exp_table = create_empty_table()
  imp_table = create_empty_table()
  fill_exp_imp_tables()
  
  return( list(exp_table, imp_table) )
}


plot_food_drinks_tobacco = function(exp_table, imp_table){
  generate_dates = function(year_from, year_to) {
    dates = vector("character", 0)
    year = year_from
    while (year <= year_to) {
      date = paste0(as.character(year), "-01-01")
      dates = c(dates, date)
      year = year + 1
    }
    return(dates)
  }
  
  calc_exp_imp_sums = function(dates, exp_table, imp_table) {
    exp_sums = vector("numeric", 0)
    imp_sums = vector("numeric", 0)
    
    date_ind = 1
    while (date_ind <= length(dates)) {
      date = dates[date_ind]
      
      exp_sum = exp_table %>% summarise( result=sum(values[time==date]) )
      exp_sums = c(exp_sums, exp_sum$result)
      
      imp_sum = imp_table %>% summarise( result=sum(values[time==date]) )
      imp_sums = c(imp_sums, imp_sum$result)
      
      date_ind = date_ind + 1
    }
    return( list(exp_sums, imp_sums) )
  }
  
  create_graph = function(year_to, year_from, dates, exp_sums, imp_sums) {
    get_is_min_col = function(vec) {
      is_min_col = vector("character", length(vec))
      min_elem = min(vec)
      ind = 1
      while (ind <= length(vec)) {
        if (vec[ind] == min_elem) {
          is_min_col[ind] = "Minimal"
        }
        else {
          is_min_col[ind] = ""
        }
        ind = ind + 1
      }
      return(is_min_col)
    }
    
    
    is_min_exp_col = get_is_min_col(exp_sums)
    is_min_imp_col = get_is_min_col(imp_sums)
    graph_points = data.frame(arg=as.character(year_from:year_to),
                              val = c(exp_sums, imp_sums),
                              is_min = c(is_min_exp_col, is_min_imp_col),
                              group=rep(c("Export", "Import"), each=length(dates)))
    
    graph = ggplot(data=graph_points, aes(x=arg, y=val, group=group, colour=group))
    graph = graph  + geom_line() +  geom_point()
    graph = graph + scale_color_discrete(name="Legend:")
    graph = graph + labs(title=paste0("Export and import of food, drinks, tabacco from ",
                                      year_from,
                                      " to ",
                                      year_to),
                         x="Year (January, 1st)",
                         y="Sum (in millions ECU/EURO)")
    graph = graph + geom_text(aes(label=is_min), nudge_y=-20000)
    
    return(graph)
  }
  
  year_from = 2008
  year_to = 2019
  dates = generate_dates(year_from, year_to)
  sums = calc_exp_imp_sums(dates, exp_table, imp_table)
  graph = create_graph(year_to, year_from, dates, exp_sums=sums[[1]], imp_sums=sums[[2]])
  
  return(graph)
}


tables = get_exp_imp_compound_tables(EXTERNAL_DATA_PATH)
exp_table = tables[[1]]
imp_table = tables[[2]]
graph = plot_food_drinks_tobacco(exp_table, imp_table)
graph