library(tidyverse)
library(lubridate)
library(glue)
library(rvest)

# [Done] Remove line breaks after verbs and add a break before
# [Done] Handle 404 errors
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
    } else {
      t <- woorden %>%
        html_node('a.help') %>%
        html_text()
    }
    
    if(is.na(wiktionary)) {
      s <- NA
    } else {
      s <- wiktionary %>%
        html_node('a.internal') %>%
        html_attr('href') 
      
      download.file(glue('https:{s}'), glue('speach/{w}.ogg'), mode = 'wb')
    }
    
    c <- ''
    
    if(p == 'v') {
      
      mijnwoordenboek <- tryCatch(read_html(mijnwoordenboek_url), error = function(e){errors <<- c(errors, 'conjugation'); return(NA)})
      
      if(is.na(mijnwoordenboek)) {
        c <- NA
      } else {
        c <- mijnwoordenboek %>%
          html_nodes('td') %>%
          html_text2() %>%
          .[5] %>%
          str_replace('\\n\\n$', '')
        
        c <- glue('\n\n{c}')
      }
    }
    
    cr <- tibble(front = w,
                back = glue('{w}\n[{t}]\n{c}'))
    
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
dict <- c('t-shirt', '_dragen')

r <- parse_dictionary(dict)

write_csv(r, 'dict.csv', col_names = F)
