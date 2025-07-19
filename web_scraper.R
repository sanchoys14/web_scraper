library(tidyverse)
library(lubridate)
library(glue)
library(rvest)
library(av)

parse_dictionary_nl <- function(dict) {
  
  r <- tibble()
  i <- 1
  
  for(word in dict) {
    
    trans_and_context <- str_extract(word, '(?<=\\[).*(?=\\])')
    trans <- replace_na(str_trim(str_split(trans_and_context, ';')[[1]][1]), '')
    context <- replace_na(str_trim(str_split(trans_and_context, ';')[[1]][2]), '')
    w <- str_extract(word, '[a-zA-Z\'-_@ ]+(?=\\[)')
    
    p <- case_when(str_detect(w, '^_') ~ 'v', # verb
                   str_detect(w, '^@') ~ 'p', # phrase
                   T ~ 'n')
    
    w <- trim(str_replace(w, '_|@', ''))
    
    # if a word is a phrase
    if(p == 'p') {
      context <- ifelse(context == '', '', glue('\n\n\n{context}'))
      cr <- tibble(front = trans,
                   back = glue('{w}{context}'))
      
      r <- r %>%
        rbind(cr)
      
      status <- '✓'
      print(glue('{i}. {w} {status}'))
      i <- i + 1
      
      next
    }
    
    wiktionary_url <- glue('https://nl.wiktionary.org/wiki/{str_replace_all(w, " ", "_")}')
    woorden_url <- glue('https://www.woorden.org/woord/{str_replace_all(w, " ", "%20")}')
    mijnwoordenboek_url <- glue('https://www.mijnwoordenboek.nl/werkwoord/{str_replace_all(w, " ", "+")}')
    
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
        
        if(p == 'v') a <- ''
        
        pls <- woorden %>%
          html_nodes('table')
        
        pl <- ''
        
        if(length(pls) > 0) {
          for(j in 1:length(pls)) {
            if(str_detect(pls[[j]] %>% html_text(), ' \\(meerv\\.\\)')) {
              pl <- pls[[j]] %>%
                html_table() %>%
                filter(str_detect(X2, '(meerv.)')) %>%
                mutate(pl = str_replace(X2, ' \\(meerv\\.\\)', '')) %>%
                .$pl
              
              break
            }
          }
          
          if(pl != '') pl <- glue('\n\ntwee {pl}')
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
        # Download original .ogg file
        download.file(glue('https:{s}'), glue('media/{w}.ogg'), mode = 'wb')
        
        # Convert .ogg to .mp3
        av_audio_convert(glue('media/{w}.ogg'), glue('media/{w}.mp3'))
        
        # Delete original .ogg file
        unlink(glue('media/{w}.ogg'))
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
        td_lag <- mijnwoordenboek %>%
          html_nodes('td') %>%
          html_text2() %>%
          lag() %>%
          replace_na('')
        
        conj_list <- mijnwoordenboek %>%
          html_nodes('td') %>%
          html_text2() %>%
          .[str_detect(td_lag, 'Onvoltooid verleden tijd')] %>%
          strsplit(., '\n')
        
        if(length(conj_list) == 0) sp <- '' else sp <- conj_list[[1]] %>% .[str_detect(., 'ik|wij')]
        
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
    
    cr <- tibble(front = trans,
                 back = glue('{ref}{a}{w} ({trans})\n[{t}]\n\n{context}\n{c}{pl}'))
    
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

parse_dictionary_pl <- function(dict) {
  
  r <- tibble()
  word_index <- 1
  
  for(word in dict) {

    trans_and_context <- str_extract(word, '(?<=\\[).*(?=\\])')
    trans <- replace_na(str_trim(str_split(trans_and_context, ';')[[1]][1]), '')
    context <- replace_na(str_trim(str_split(trans_and_context, ';')[[1]][2]), '')
    w <- str_trim(str_extract(word, '[\\p{L}\\d\'\\-_@!: ]+(?=\\[)'))
    
    p <- case_when(str_detect(w, '^_') ~ 'v', # verb
                   str_detect(w, '^@') ~ 'p', # phrase
                   T ~ 'n')
    
    w <- trim(str_replace(w, '_|@', ''))
    
    # if a word is a phrase
    if(p == 'p') {
      context <- ifelse(context == '', '', glue('<br><br>{context}'))
      cr <- tibble(front = trans,
                   back = glue('{w}{context}'))
      
      r <- r %>%
        rbind(cr)
      
      status <- '✓'
      print(glue('{word_index}. {w} {status}'))
      word_index <- word_index + 1
      
      next
    }
    
    wiktionary_url <- glue('https://pl.wiktionary.org/wiki/{str_replace_all(w, " ", "_")}')
    
    errors <- ''
    
    wiktionary <- tryCatch(read_html(wiktionary_url), error = function(e){errors <<- c(errors, 'no wiktionary'); return(NA)})
    
    if(is.na(wiktionary)) {
      s <- NA # This is for case when p == 'n'
      
      # Do something for p == 'v'
    } else {
      
      # Audio
      audio_source <- wiktionary %>%
        html_node('a.oo-ui-buttonElement-button') %>%
        html_attr('href') 
      
      audio <- '<br>'
      
      if(is.na(audio_source)) {
        assign('errors', c(errors, 'no audio'))
      } else {
        # Download original file. It might be .mp3 or .ogg, in both ways save it as .mp3
        download.file(glue('https:{audio_source}'), glue('media/{w}.mp3'), mode = 'wb')
        assign('audio', glue('[sound:{w}.mp3]<br>'))
      }
      
      # Image
      img_size <- wiktionary %>%
        html_nodes('img.mw-file-element') %>%
        html_attr('width') %>%
        as.numeric()
      
      image_source <- wiktionary %>%
        html_nodes('img.mw-file-element') %>%
        html_attr('src') %>%
        .[which(img_size > 200)[1]]
      
      image <- ''
      
      if(is.na(image_source)) {
        assign('errors', c(errors, 'no image'))
      } else {
        download.file(glue('https:{image_source}'), glue('media/{w}.jpg'), mode = 'wb')
        assign('image', glue('<img src="{w}.jpg"><br>'))
      }
      
      # if a word is a verb
      conj <- ''
      if(p == 'v') {
        
        gender <- ''
        context <- ifelse(context == '', '', glue('{context}<br>'))
        
        conj_dfs <- wiktionary %>%
          html_table() 
        
        if(length(conj_dfs) == 0) {
          # Do something
        } else {
          conj_df <- tibble()
          
          for(i in 1:length(conj_dfs)) {
            if('forma' %in% colnames(conj_dfs[[i]])) {
              conj_df <- conj_dfs %>%
                .[[i]] %>%
                .[,1:8]
              
              break
            }
          }
          
          if(length(conj_df) == 0) {
            # Do something
          } else {
            colnames(conj_df) <- make.names(colnames(conj_df), unique = T)
            
            conj_v <- conj_df %>%
              filter(forma %in% c('forma', 'czas teraźniejszy', 'czas przyszły prosty')) %>% # what if we have both teraźniejszy and przyszły?
              .[2,3:8] %>%
              unlist(., use.names = F) %>%
              str_extract(., '[\\p{L}+ \\/]+')
            
            if(length(conj_v) != 6) {
              # Do something
            } else {
              conj <- glue('<br>ja {conj_v[1]}<br>
ty {conj_v[2]}<br>
on {conj_v[3]}<br>
my {conj_v[4]}<br>
wy {conj_v[5]}<br>
oni {conj_v[6]}')
            }
          }
        }
      } else { # Meaning when p == 'n'
        
        # Gender (for nouns)
        gender <- ''
        if(p == 'n') {
          l <- wiktionary %>%
            html_nodes('p i') %>%
            html_text()
          
          l1 <- l[str_detect(l, 'rodzaj (męskorzeczowy|męskoosobowy|męski|żeński|nijaki)')][1]
          
          g <- str_extract(str_replace(l1, 'męskorzeczowy|męskoosobowy', 'męski'), 'rodzaj \\p{L}+')
          
          if(!is.na(g)) {gender <- glue('<br>{g}')}
        }
      }
    } 
    
    cr <- tibble(front = glue('{image}{trans}'),
                 back = glue('{w} ({trans}){gender}<br>{audio}{context}{conj}'))
    
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
    
    print(glue('{word_index}. {w} {status}'))
    
    word_index <- word_index + 1
  }
  
  return(r)
}

dict <- read_tsv('words.txt', col_names = F)$X1

r <- parse_dictionary_pl(dict)

# %APPDATA%\Anki2
write_csv(r, 'dict.csv', col_names = F)

# Handle errors when a word is a verbs
# problem with obserwować 

