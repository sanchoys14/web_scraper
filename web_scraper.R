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
    
    wiktionary <- read_html(wiktionary_url)
    woorden <- read_html(woorden_url)
    
    t <- woorden %>%
      html_node('a.help') %>%
      html_text()
    
    # lots of exceptions and blanks
    t_alt <- wiktionary %>%
      html_node('.IPAtekst') %>%
      html_text() %>%
      str_replace_all('/', '')
    
    s <- wiktionary %>%
      html_node('a.internal') %>%
      html_attr('href') 
    
    download.file(glue('https:{s}'), glue('speach/{w}.ogg'), mode = 'wb')
    
    c <- ''
    
    if(p == 'v') {
      
      mijnwoordenboek <- read_html(mijnwoordenboek_url)
      
      c <- mijnwoordenboek %>%
        html_nodes('td') %>%
        html_text2() %>%
        .[5]
    }
    
    c <- tibble(front = w,
                back = glue('{w}\n[{t}]\n{c}'))
    
    r <- r %>%
      rbind(c)
    
    print(glue('{i}. {w} ✓'))
    
    i <- i + 1
  }
  
  return(r)
}


# _ in front of verbs
dict <- c('', '', '', '', '', 
          '', '', '', '', '', 
          '', '', '', '', '', 
          '', '', '', '', '', 
          '', '', '', '', '', 
          '', '', '', '', '')

r <- parse_dictionary(dict)

write_csv(r, 'dict.csv', col_names = F)

