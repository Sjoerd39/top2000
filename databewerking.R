library(tidyverse)
library(rvest)
library(janitor)
library(spotifyr)

source("auth.R")
#### basistabel ophalen en opschonen #####

url <- "https://nl.wikipedia.org/wiki/Lijst_van_Radio_2-Top_2000%27s"

top2000_wiki <- url %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/div/table') %>%
  html_table()

top2000_tabel <- top2000_wiki[[1]] %>%
  clean_names() %>%
  rename(uitgebracht = jaar) %>%
  gather(jaar, positie, -hp, -artiest, - titel, -uitgebracht) %>%
  mutate(jaar = as.numeric(ifelse(jaar == "x99", 1999, str_replace(jaar, "x", "20"))))

#### berekenen wetenswaardigheden ####
huidigjaar <- max(top2000_tabel$jaar)


verschillen <- top2000_tabel %>% filter(positie < 2001) %>% arrange(jaar) %>% group_by(artiest, titel) %>% 
  mutate(verschil_vorig_jaar = ifelse(jaar - 1 == lag(jaar), lag(positie) - positie, NA)) %>% 
  ungroup() %>% 
  left_join(top2000_tabel %>% filter(positie < 2001) %>% arrange(jaar) %>% group_by(artiest, titel) %>% 
              mutate(binnenkomer = ifelse(jaar - 1 != lag(jaar), 2000 - positie, NA)) %>% ungroup(),
            by = c("artiest", "titel", "jaar" ))

uitvallers <- top2000_tabel %>% arrange(jaar) %>% group_by(artiest, titel) %>% 
  mutate(uitvaller = ifelse(positie > 2000 & lag(positie) < 2001, lag(positie) - 2000, NA)) %>% ungroup()

aantal_rentrees <- top2000_tabel %>% 
  arrange(artiest, titel, jaar) %>% 
  mutate(in_top2000 = ifelse(positie < 2001, 1, 0),
         entree = ifelse(jaar == 1999 & in_top2000 == 1, 1, 
                         ifelse(in_top2000 == 1 & lag(in_top2000) == 0, 1, 0))) %>% 
  group_by(artiest, titel) %>% summarise(aantal_rentrees = sum(entree))

plus <- top2000_tabel %>% 
  rename(hoogste_notering = hp) %>% 
  left_join(top2000_tabel %>% group_by(artiest, titel, positie) %>% summarise(hoogste_notering_jaar = max(jaar)), 
            by = c("artiest", "titel", "hoogste_notering" = "positie")) %>% 
  left_join(top2000_tabel %>% filter(positie < 2001) %>% group_by(artiest, titel) %>% summarise(eerste_jaar_top2000 = min(jaar)),
            by = c("artiest", "titel" )) %>% 
  left_join(top2000_tabel %>% filter(positie < 2001) %>% group_by(artiest, titel) %>% summarise(laatste_jaar_top2000 = max(jaar)) %>%
              mutate(laatste_jaar_top2000 = ifelse(laatste_jaar_top2000 == huidigjaar, NA, laatste_jaar_top2000)),
            by = c("artiest", "titel" )) %>% 
  mutate(one_year_fly = ifelse(eerste_jaar_top2000 == laatste_jaar_top2000 & laatste_jaar_top2000 != huidigjaar, "ja", "nee")) %>% 
  select(-jaar, -uitgebracht, -positie) %>% 
  group_by(artiest, titel) %>% 
  summarise_all(last)

#### spotify data ophalen ####

# alle nrs in top2000

nrs <- unique(top2000_tabel$titel)

nrs_id <- list()
for (i in seq_along(nrs)) {
  id <- search_spotify(nrs[[i]], "track", limit = 1)
  
  if(length(id) > 0){
    nrs_id[[i]] <- data.frame(nr = nrs[[i]],
                              spotify_id = id)
  }
}

nr_ids <- bind_rows(nrs_id)


spotify_track_ids <- unique(nr_ids$spotify_id.id)

audio_features <- list()
for (i in seq_along(spotify_track_ids)) {
  
  af <- get_track_audio_features(spotify_track_ids[[i]])
  
  if(length(af) > 0){
    audio_features[[i]] <- af
  }
}

total_audio_features <- bind_rows(audio_features) %>% 
  left_join(nr_ids, by = c("id" = "spotify_id.id")) %>% 
  select(1:11, id, nr)







#### staat het nr in lijst 2019? ####

url <- "https://staatieindetop2000.nporadio2.nl/" 

nq <- top2000_tabel %>% 
  group_by(artiest, titel) %>% 
  summarise_all(first) %>% 
  ungroup() %>% 
  mutate(id = row_number())

artst <- nq$artiest %>% 
  tolower() %>% 
  str_replace_all(" ", "-") %>% 
  str_remove("/") %>% 
  str_remove("’|'") %>% 
  stri_trans_general(id = "Latin-ASCII") 

nmr <- nq$titel %>% 
  tolower() %>% 
  str_replace_all(" ", "-") %>% 
  str_replace_all("/", "-") %>% 
  str_remove("’|'") %>% 
  stri_trans_general(id = "Latin-ASCII") 

inlijst <- list()

for (i in seq_along(artst)) {
  
  a <- artst[[i]]
  n <- nmr[[i]]
  
  url_i <- paste0(url, a, "/", n)
  
  
  tryCatch({
    
    in19 <- url_i %>%
      read_html() 
    
    res <- xml_attrs(xml_child(xml_child(xml_child(in19, 2), 6), 1))[["data-inlist"]]
    
    
    inlijst[[i]] <- data.frame(in19 = res,
                               id = i,
                               stringsAsFactors = F)
    
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
  
}

inlijst2019 <- bind_rows(inlijst)

inlijst2019 <- inlijst2019 %>% 
  left_join(nq %>% select(artiest, titel, id), by = c("id")) %>% 
  mutate(in19 = ifelse(in19 == "positive", "in lijst", "waarschijnlijk niet")) %>% 
  select(-id)
  

#### samenvoegen en wegschrijven ####

top2000_tabel_plus <- top2000_tabel %>% 
  left_join(total_audio_features, by = c("titel" = "nr")) %>% 
  mutate(loudness = ifelse(positie < 2001, loudness, NA),
         danceability = ifelse(positie < 2001, danceability, NA),
         energy = ifelse(positie < 2001, energy, NA),
         speechiness = ifelse(positie < 2001, speechiness, NA),
         acousticness = ifelse(positie < 2001, acousticness, NA),
         instrumentalness = ifelse(positie < 2001, instrumentalness, NA),
         liveness = ifelse(positie < 2001, liveness, NA),
         valence = ifelse(positie < 2001, valence, NA),
         tempo = ifelse(positie < 2001, tempo, NA)) %>% 
  left_join(plus, by = c("artiest", "titel")) %>% 
  left_join(verschillen %>% select(-uitgebracht.x, -uitgebracht.y, -positie.y, -positie.x, -uitgebracht.x, -uitgebracht.y, -hp.x, -hp.y), 
            by = c("artiest", "titel", "jaar")) %>% 
  left_join(uitvallers %>% select(-uitgebracht, -positie, -hp), by = c("artiest", "titel", "jaar")) %>% 
  mutate(positie = ifelse(positie < 2001, positie, 9999)) %>% 
  left_join(aantal_rentrees, by = c("artiest", "titel")) %>% 
  left_join(top2000_tabel %>% filter(jaar == 2018) %>% select(artiest, titel, positie18 = positie), by = c("artiest", "titel")) %>% 
  left_join(inlijst2019, by = c("artiest", "titel")) %>% 
  mutate(positie18 = ifelse(is.na(positie18), 9999, ifelse(positie18 > 2000, 9999, positie18)))


write.csv2(top2000_tabel_plus, "top2000.csv")
