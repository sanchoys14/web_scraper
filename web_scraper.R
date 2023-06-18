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
    
    wiktionary <- tryCatch(read_html(wiktionary_url), error = function(e){errors <<- c(errors, 'audio'); return(NA)})
    woorden <- tryCatch(read_html(woorden_url), error = function(e){errors <<- c(errors, 'transcription'); return(NA)})
    
    if(is.na(woorden)) {
      t <- NA
      a <- NA
      pl <- NA
      ref <- NA
    } else {
      t <- woorden %>%
        html_node('a.help') %>%
        html_text()
      
      a <- woorden %>%
        html_nodes('.inline font') %>%
        html_text() %>%
        .[. %in% c('de ', 'het ')] %>%
        .[1]
      
      if(is.na(a)) a <- ''
      
      pl <- woorden %>%
        html_node('table') %>%
        html_table() %>%
        filter(str_detect(X2, '(meerv.)')) %>%
        mutate(pl = str_replace(X2, ' \\(meerv\\.\\)', '')) %>%
        .$pl
      
      if(length(pl) == 0) pl <- '' else pl <- glue('\n\ntwee {pl}')
      
      ref <- woorden %>%
        html_node('h2 i') %>%
        html_text() == 'zich'
      
      ref <- ifelse(replace_na(ref, F), 'zich ', '')
        
    }
    
    if(is.na(wiktionary)) {
      s <- NA
    } else {
      s <- wiktionary %>%
        html_node('a.internal') %>%
        html_attr('href') 
      
      if(is.na(s)) {
        #errors <<- c(errors, 'audio') # This doesn't work
        assign('errors', c(errors, 'audio'))
      } else {
        download.file(glue('https:{s}'), glue('speach/{w}.ogg'), mode = 'wb')
      }
    }
    
    c <- ''
    
    if(p == 'v') {
      
      mijnwoordenboek <- tryCatch(read_html(mijnwoordenboek_url), error = function(e){errors <<- c(errors, 'conjugation'); return(NA)})
      
      if(is.na(mijnwoordenboek)) {
        c <- NA
      } else {
        
        # Past participle
        pp <- mijnwoordenboek %>%
          html_nodes('td') %>%
          html_text2() %>%
          .[3] %>%
          str_replace('\\n', '')
        
        # Past participle conjugations
        pp_c <- mijnwoordenboek %>%
          html_nodes('td') %>%
          html_text2() %>%
          .[7]
        
        # Finite verb for past participle: 'zijn' or 'hebben'
        fv <- if (str_detect(pp_c, 'zijn')) 'zijn ' else 'hebben '
        
        # Conjugations
        c <- mijnwoordenboek %>%
          html_nodes('td') %>%
          html_text2() %>%
          .[5] %>%
          str_replace('\\n\\n$', '')
        
        c <- glue('\n\n{fv}{pp}{c}')
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
dict <- c('_zijn')

r <- parse_dictionary(dict)

write_csv(r, 'dict.csv', col_names = F)



