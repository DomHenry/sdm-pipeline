# Col names to lower case and replace whitespace --------------------------
low_rm_space <- function(x){
  x %>% 
    select_all(tolower) %>% 
    select_all(funs(str_replace_all(.," ","_")))
}
