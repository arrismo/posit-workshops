library("conflicted")
library("tidyverse")
library("readxl")
library("here")
library("fs")

## Aside
here("data/file.csv")

## Our turn
data1952 <- read_excel(here("data/gapminder/1952.xlsx"))
data1957 <- read_excel(here("data/gapminder/1957.xlsx"))
data1962 <- read_excel(here("data/gapminder/1952.xlsx"))
data1967 <- read_excel(here("data/gapminder/1967.xlsx"))

data_manual <- bind_rows(data1952, data1957, data1962, data1967)

# What problems do you see so far?
# (I see two "real" problems, one philosophical problem)

# Typo on line 13
# Repetitive
# There is no "Year" colum

# ?basename(), ?str_extract()
get_year <- function(x) {
  x %>% basename() %>% str_extract("^\\d+")
}

get_year("taylor/swift/1989.txt")

# ?as.list(), ?set_names()
paths <-
  # get the filepaths from the directory
  fs::dir_ls(here("data/gapminder")) |>
  # convert to list
  as.list() %>%
  # extract the year as names
  set_names(get_year) %>%
  print()

# ?read_excel(), ?list_rbind(), ?parse_number()
data <-
  paths |>
  # read each file from excel, into data frame
  map(read_excel) %>%
  # keep only non-null elements
  # set list-names as column `year`
  # bind into single data-frame
  list_rbind(names_to = "year") %>%
  # convert year to number
  mutate(year = parse_number(year)) %>%
  print()

# parse_number is a better alternative for Amplio


paths_party <-
  # get the filepaths from the directory
  fs::dir_ls(here("data/gapminder_party")) |>
  # convert to list
  as.list() |>
  # extract the year as names
  set_names(get_year) |>
  print()

# possibly
poss_read_csv <- possibly(read_csv, otherwise = NULL, quiet = FALSE)

poss_read_csv("not/a/file.csv")

poss_read_csv(I("a, b\n 1, 2"), col_types = "dd")

# modify read-function to return NULL, rather than throw error
possibly_read_excel <- possibly(read_excel,otherwise = NULL, quiet = FALSE) # we do the rest

data_party <-
  data_party <-
  paths_party |>
  # read each file from excel
  map(read_excel) |>
  # keep only non-null elements
  # set list-names as column `year`
  # bind into single data-frame
  list_rbind(names_to = "year") |>
  # convert year to number
  mutate(year = parse_number(year)) |>
  print()

# intermediate step - see which one failed
paths_party |>
  map(possibly_read_excel) |>
  keep(is.null)

identical(data_party, data)

## Horrible example

# keep only non-null elements
# set list-names as column `year`
# bind into single data-frame
list_rbind2 <- function(df, names_to) {
  df |>
    purrr::keep(\(x) !is.null(x)) |>
    purrr::imap(\(d, name) dplyr::mutate(d, "{names_to}" := name)) |>
    purrr::reduce(rbind)
}

data_horrible <-
  paths |>
  map(read_excel) |>
  list_rbind2(names_to = "year") |>
  mutate(year = parse_number(year))

identical(
  data_horrible |> select(year, everything()),
  data
)
