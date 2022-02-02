library(tidyverse)
library(words)


reset <- function(){
  letters_in <<- NA
  letters_out <<- NA
  pos_1 <<- NA
  pos_2 <<- NA
  pos_3 <<- NA
  pos_4 <<- NA
  pos_5 <<- NA
  not_pos_1 <<-  NA
  not_pos_2 <<- NA
  not_pos_3 <<- NA
  not_pos_4 <<- NA
  not_pos_5 <<- NA
  only_1 <<- NA
  only_2_plus <<- NA
  only_2 <<- NA
  only_3 <<- NA
  
}

prep_words <- function(){  
  five_ltr_words <<- words::words %>% 
    filter(word_length==5) %>% 
    mutate(`1` = str_sub(word,1,1),
           `2` = str_sub(word,2,2),
           `3` = str_sub(word,3,3),
           `4` = str_sub(word,4,4),
           `5` = str_sub(word,5,5)) %>% 
    pivot_longer(cols = `1`:`5`, names_to = "position", values_to = "letter") 
  
  repeat_letter_words <- five_ltr_words %>%
    count(word,letter) %>%
    filter(n > 1) %>%
    select(word) %>%
    distinct() %>%
    pull()
  
  word_set <<- five_ltr_words
  five_ltr_words_clean <<- five_ltr_words %>% 
    filter(!word %in% repeat_letter_words) 
}

filter_duplicates <- function(dup_letter='o',type="only_1"){
  ltr_count <- word_set %>% 
    count(word,letter) 
  if(type=="only_1"){
    filtered_words <- 
      ltr_count %>% 
      filter(letter==dup_letter & n==1) %>% 
      pull(word)
    out <- word_set %>% 
      filter(word %in% filtered_words)
  }
  if(type=="only_2"){
    filtered_words <- 
      ltr_count %>% 
      filter(letter==dup_letter & n==2) %>% 
      pull(word)
    out <- word_set %>% 
      filter(word %in% filtered_words)
  }
  if(type=="only_2_plus"){
    filtered_words <- 
      ltr_count %>% 
      filter(letter==dup_letter & n>=2) %>% 
      pull(word)
    out <- word_set %>% 
      filter(word %in% filtered_words)
  }
  if(type=="only_3"){
    filtered_words <- 
      ltr_count %>% 
      filter(letter==dup_letter & n==3) %>% 
      pull(word)
    out <- word_set %>% 
      filter(word %in% filtered_words)
  }
  word_set <<- out
}

get_filtered_words <- function(){  
  word_set <<- 
    word_set %>% 
    pivot_wider(names_from = 'position',values_from="letter") %>% 
    filter(is.na(pos_1)[1] | `1` == pos_1,
           is.na(pos_2)[1] | `2` == pos_2, 
           is.na(pos_3)[1] | `3` == pos_3, 
           is.na(pos_4)[1] | `4` == pos_4, 
           is.na(pos_5)[1] | `5` == pos_5) %>% 
    filter( !(`1` %in% not_pos_1),
            !(`2` %in% not_pos_2),
            !(`3` %in% not_pos_3),
            !(`4` %in% not_pos_4),
            !(`5` %in% not_pos_5)) %>% 
    filter(is.na(letters_in)[1] | str_detect(word,str_c("(?=.*",str_c(letters_in, collapse =")(?=.*"),")"))) %>% #"(?=.*p)(?=.*a)(?=.*e)"
    filter(is.na(letters_out)[1] | !(str_detect(word,str_c("[",str_c(letters_out,collapse= ""),"]")))) %>% 
    pivot_longer(cols = c(`1`,`2`,`3`,`4`,`5`),names_to ="position",values_to = "letter")
    
  if(!is.na(only_1)) map(.x=only_1,filter_duplicates,type="only_1")
  if(!is.na(only_2)) map(.x=only_2,filter_duplicates,type="only_2")
  if(!is.na(only_2_plus)) map(.x=only_2_plus,filter_duplicates,type="only_2_plus")
  if(!is.na(only_3)) map(.x=only_3,filter_duplicates,type="only_3")
}

get_stats <- function(data = data){
  letter_stats <- data %>% 
    mutate(num_words = length(data$word)/5) %>% 
    left_join(data %>% count(letter,name = "num_letter")) %>% 
    mutate(prop_words = num_letter/num_words) %>% 
    left_join(data %>% count(letter,position,name = "num_letter_position")) %>% 
    mutate(prop_position = num_letter_position/num_words)
  
  letter_position_stats<- letter_stats %>% 
    select(-word,-word_length) %>% 
    distinct() %>% 
    pivot_wider(id_cols=c(position,letter), names_from = "position",values_from= "prop_position") %>% 
    left_join(letter_stats %>% 
                select(letter,prop_words) %>% 
                distinct()) %>% 
    arrange(desc(prop_words))
  return(letter_position_stats)
}

get_words <- function(){
  
  new_stats <- get_stats(word_set)
  
  out <- word_set %>% 
    # count(word,letter,name="ltr_count")
    pivot_wider(names_from = 'position',values_from="letter") %>% 
    mutate(prop_words = NA) %>% 
    left_join(new_stats %>% select(letter,`1`,prop_words),by=c(`1`="letter")) %>%  
    left_join(new_stats %>% select(letter,`2`,prop_words),by=c(`2`="letter")) %>% 
    left_join(new_stats %>% select(letter,`3`,prop_words),by=c(`3`="letter")) %>% 
    left_join(new_stats %>% select(letter,`4`,prop_words),by=c(`4`="letter")) %>% 
    left_join(new_stats %>% select(letter,`5`,prop_words),by=c(`5`="letter")) %>% 
    left_join(word_set %>% 
                count(word,letter,name="ltr_count") %>% 
                filter(ltr_count>1) %>% 
                group_by(word) %>% 
                summarize(repeat_ltrs = length(ltr_count))) %>%
    replace_na(list(repeat_ltrs = 0)) %>% 
    mutate(sum_pos_props = `1.y` + `2.y` + `3.y` + `4.y` + `5.y`,
           sum_prop_words = prop_words.y + prop_words.y.y + prop_words.y.y.y + prop_words.x.x + prop_words.x.x.x) %>%
    arrange(desc(sum_pos_props)) %>% 
    select(word,sum_pos_props,sum_prop_words,repeat_ltrs)
    return(out)
    
}

letter_color_check <- function(index=4) {
  letter <- str_sub(letters_attempt,index,index)
  color <- str_sub(colors_attempt,index,index)
  if(color=='b'){
    if(is.na(letters_out)[1]) {
      letters_out <<- letter 
    } else { letters_out <<- c(letters_out,letter)  }
  } else {
    if(color=='y') {
      if(is.na(letters_in)[1]){ 
        letters_in <<- letter
      } else{ letters_in <<- c(letters_in,letter) } 
      if(is.na(get(as.name(str_c("not_pos_",index))))) {
        assign(str_c("not_pos_",index), letter, envir = .GlobalEnv)
      } else { assign(str_c("not_pos_",index),c(get(as.name(str_c("not_pos_",index))),letter, envir = .GlobalEnv))  }
    } else {
      if(color=='g'){
        if(is.na(get(as.name(str_c("pos_",index))))){
          assign(str_c("pos_",index), letter, envir = .GlobalEnv)
        } else {
          assign(str_c("pos_",index),c(get(as.name(str_c("pos_",index))),letter, envir = .GlobalEnv)) 
        }
      } else {
        stop()
      }
    }
  }
}

check_duplicates <- function(dup_letter = "d"){
  ltr_break <- letters_attempt %>% 
    str_split("") %>% 
    unlist() 
  indices <- which(ltr_break==dup_letter)
  clr_break <- colors_attempt %>% 
    str_split("") %>% 
    unlist()
  num_pos_in <- sum(clr_break[indices]%in% c("g", "y"))
  if(num_pos_in==1) {
    if(is.na(only_1)[1]){ 
      only_1 <<- dup_letter
    } else { only_1 <<- c(only_1,dup_letter) }
  }
  if(num_pos_in==2 & length(indices)>2) {
    if(is.na(only_2)[1]){ 
      only_2 <<- dup_letter
    } else{ only_2 <<- c(only_2,dup_letter) }
  }
  if(num_pos_in==2 & length(indices)==2) {
    if(is.na(only_2_plus)[1]){ 
      only_2_plus <<- dup_letter
    } else{ only_2_plus <<- c(only_2_plus,dup_letter) }
  }
  if(num_pos_in==3) {
    if(is.na(only_3)[1]){ 
      only_3 <<- dup_letter
    } else{ only_3 <<- c(only_3,dup_letter) }
  }
  if(num_pos_in!=0){  
    for(i in indices){
      if(clr_break[i]=="b") clr_break[i]="y"
    }
  }
  colors_attempt <<- str_c(clr_break,collapse="")
}


run_attempt <- function(letters_attempt = letters_attempt,colors_attempt=colors_attempt){
  # if(letters_attempt=="" | colors_attempt==""){
  if(is.na(letters_attempt) | is.na(colors_attempt)){
    letters_attempt <<- letters_attempt
    colors_attempt <<- colors_attempt
    dup_letters <- letters_attempt %>% 
      str_split("") %>% 
      unlist() %>% 
      tibble(ltrs = .) %>% 
      count(ltrs) %>% 
      filter(n>1) %>% 
      pull(ltrs)
    
    map(.x=dup_letters,.f = check_duplicates)
    
    purrr::map(.x = 1:5,letter_color_check)
    get_filtered_words()
  }
  get_words()

}

