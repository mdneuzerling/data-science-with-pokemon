# data/updated-pokemon-raw.csvfrom https://gist.github.com/simsketch/1a029a8d7fca1e4c142cbfd043a68f19
# The last 5 to 10 rows needed to be manually fixed to have the correct number of columns
# data/evolutions-raw.csv is manually scraped from https://pokemon.fandom.com/wiki/List_of_Pok%C3%A9mon_by_evolution

library(tidyverse)

# Terminology note: a "base" of a Pokemon "family" is the unevolved form from
# which other Pokemon in the family come from. For example, Squirtle is the
# base of Wartortle and Blastoise, and these three are members of the "Squirtle"
# family (and possibly Mega Blastoise too)

pokemon_raw <- read_csv("data/updated-pokemon-raw.csv") %>% 
  janitor::clean_names() %>%
  mutate( # minor corrections
    name = gsub("Nidoran F", "Nidoran ♀", name),
    name = gsub("Nidoran M", "Nidoran ♂", name),
    name = gsub("Conviknight", "Corviknight", name),
    name = gsub("Convisquire", "Corvisquire", name),
    # also
    legendary = as.logical(legendary)
  )

evolutions_raw <- "data/evolutions-raw.csv" %>%
  read_csv() %>%
  separate_rows(first, second, sep = "\n")

rbind(
  # every base Pokemon is in its eponymous family
  evolutions_raw %>% transmute(base, evolution = base),
  evolutions_raw %>% transmute(base, evolution = first) %>% filter(!is.na(evolution)),
  evolutions_raw %>% transmute(base, evolution = second) %>% filter(!is.na(evolution))
) %>% 
  rename(family = base) %>% 
  distinct() %>% 
  write_csv("data/evolutions.csv")

# reload in case there were errors in (de)serialisation:
evolutions <- read_csv("data/evolutions.csv") 

# This is where it gets a bit weird because of mega-evolutions
# We join the Pokedex numbers from the pokemon dataset onto the evolution
# dataset, and then we join the families onto the pokemon dataset using the
# Pokedex numbers. This works because mega-evolutions share a Pokedex number
# with the Pokemon from which they mega-evolve.
# 
# Some may be missing here because the evolution dataset has more recent
# Pokemon than the pokemon dataset, eg. Corviknight

evolutions_with_numbers <- evolutions %>%
  left_join(select(pokemon_raw, name, number), by = c("evolution" = "name" )) %>% 
  select(number, family)

pokemon_raw %>%
  left_join(evolutions_with_numbers, by = "number") %>% 
  select(number, name, type1, type2, hp:total, family, legendary, color) %>%
  rowwise() %>% 
  mutate(
    family = ifelse(is.na(family), strsplit(name, " ")[[1]], family)
  ) %>% 
  ungroup() %>% 
  distinct() %>% 
  write_csv("data/pokemon.csv")
