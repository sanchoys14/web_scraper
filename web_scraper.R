library(tidyverse)
library(lubridate)
library(glue)
library(rvest)

parse_dictionary <- function(dict) {
  
  r <- tibble()
  i <- 1
  
  for(word in dict) {
    
    p <- case_when(str_detect(word, '^_') ~ 'v',
                   T ~ 'n')
    
    w <- trim(str_replace(word, '_', ''))
    
    wiktionary_url <- glue('https://nl.wiktionary.org/wiki/{w}')
    woorden_url <- glue('https://www.woorden.org/woord/{w}')
    mijnwoordenboek_url <- glue('https://www.mijnwoordenboek.nl/werkwoord/{w}')
    
    errors <- ''
    
    wiktionary <- tryCatch(read_html(wiktionary_url), error = function(e){errors <<- c(errors, 'no audio'); return(NA)})
    woorden <- tryCatch(read_html(woorden_url), error = function(e){errors <<- c(errors, 'no transcription'); return(NA)})
    
    if(is.na(woorden)) {
      t <- ''
      a <- ''
      pl <- ''
      ref <- ''
      
      assign('errors', c(errors, 'woorden is NA'))
    } else {
      
      # heeft het uitgebreide woordinformatie?
      has_info <- woorden %>%
        html_nodes('div') %>%
        html_text() %>%
        str_detect(., 'heeft geen uitgebreide woordinformatie') %>%
        sum(.) == 0
      
      if(has_info) {
        t <- woorden %>%
          html_node('a.help') %>%
          html_text() %>%
          replace_na(., '')
        
        a <- woorden %>%
          html_nodes('.inline font') %>%
          html_text() %>%
          .[. %in% c('de ', 'het ')] %>%
          .[1] %>%
          replace_na(., '')
        
        pl <- woorden %>%
          html_node('table')
        
        if(is.na(pl)) {
          pl <- ''
        } else {
          pl <- pl %>%
            html_table() %>%
            filter(str_detect(X2, '(meerv.)')) %>%
            mutate(pl = str_replace(X2, ' \\(meerv\\.\\)', '')) %>%
            .$pl
          
          if(length(pl) == 0) pl <- '' else pl <- glue('\n\ntwee {pl}')
        }
        
        ref <- woorden %>%
          html_node('h2 i') %>%
          html_text() == 'zich'
        
        ref <- ifelse(replace_na(ref, F), 'zich ', '')
        
        if(t == '') assign('errors', c(errors, 'woorden has no info'))
        
      } else {
        t <- ''
        a <- ''
        pl <- ''
        ref <- ''
        
        assign('errors', c(errors, 'woorden has no info'))
      }
    }
    
    if(is.na(wiktionary)) {
      s <- NA
    } else {
      s <- wiktionary %>%
        html_node('a.internal') %>%
        html_attr('href') 
      
      if(is.na(s)) {
        #errors <<- c(errors, 'no audio') # This doesn't work
        assign('errors', c(errors, 'no audio'))
      } else {
        download.file(glue('https:{s}'), glue('speach/{w}.ogg'), mode = 'wb')
      }
    }
    
    c <- ''
    
    if(p == 'v') {
      
      mijnwoordenboek <- tryCatch(read_html(mijnwoordenboek_url), error = function(e){errors <<- c(errors, 'conjugation'); return(NA)})
      
      if(is.na(mijnwoordenboek)) {
        c <- ''
      } else {
        
        # Past participle
        pp <- mijnwoordenboek %>%
          html_nodes('td') %>%
          html_text2() %>%
          .[3] %>%
          str_replace('\\n', '') %>%
          replace_na(., '')
        
        # Past participle conjugations
        pp_c <- mijnwoordenboek %>%
          html_nodes('td') %>%
          html_text2() %>%
          .[7] %>%
          replace_na(., '')
        
        # Finite verb for past participle: 'zijn' or 'hebben'
        fv <- case_when(pp_c == '' ~ '',
                        str_detect(pp_c, 'zijn') ~ 'zijn ',
                        T ~ 'hebben ')
        
        # Simple past tense
        sp <- mijnwoordenboek %>%
          html_nodes('td') %>%
          html_text2() %>%
          .[9] %>%
          strsplit(., '\n') %>%
          .[[1]] %>%
          .[str_detect(., 'ik|wij')]
          #str_replace_all(., 'ik |wij ', '')
        
        sp <- ifelse(length(sp) == 2, glue('\n{paste(sp, collapse = " (")})\n\n\n'), '')
        
        # Conjugations
        c <- mijnwoordenboek %>%
          html_nodes('td') %>%
          html_text2() %>%
          .[5] %>%
          str_replace('\\n\\n$', '') %>%
          replace_na(., '')
        
        if(pp == '' | pp_c == '' | c == '' | sp == '') assign('errors', c(errors, 'mijnwoordenboek has no info'))
        
        c <- glue('\n\n{fv}{pp}{sp}{c}')
      }
    }
    
    cr <- tibble(front = w,
                back = glue('{ref}{a}{w}\n[{t}]\n{c}{pl}'))
    
    r <- r %>%
      rbind(cr)
    
    status <- ''
    
    if(length(errors) == 1) {
      status <- '✓'
    } else {
      errors <- errors[errors != '']
      errors <- paste(errors, collapse = ', ')
      status <- glue('✗ ({errors})')
    }
    
    
    print(glue('{i}. {w} {status}'))
    
    i <- i + 1
  }
  
  return(r)
}

# _ in front of a verb
dict <- c('_zijn', '_schikken')

r <- parse_dictionary(dict)

write_csv(r, 'dict.csv', col_names = F)

