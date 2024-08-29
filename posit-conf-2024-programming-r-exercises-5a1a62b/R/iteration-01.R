library("tidyverse")
library(palmerpenguins)

data("penguins")

sd_error <- function(x){
  sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))
}

# bad way

penguins %>% summarise(
  se_bill_depth = sd_error(bill_depth_mm),
  se_bill_length = sd_error(bill_length_mm)
)

# good way

penguins %>% summarise(
  across(bill_length_mm:body_mass_g, sd_error)
)


# Anonymous Functions
# how to pass paramaters to functions called in across

penguins %>%
  summarise(across(ends_with("mm"),
                   function(x) mean(x, na.rm = T)))

# short hand
penguins %>%
  summarise(across(ends_with("mm"),
                   \(x) mean(x, na.rm = T)))


# if you want to call more than one function

penguins %>%
  summarise(across(ends_with("mm"), list(
    mean  = \(x) mean(x,na.rm = T),
    sd = \(x) sd(x,na.rm = T))))


penguins %>%
  summarise(across(ends_with("mm"), list(
    mean  = \(x) mean(x,na.rm = T),
    sd = \(x) sd(x,na.rm = T)),
    .names = "{.fn}_of_{.col}"))


to_z <- function(x, middle = 1) {
  trim = (1 - middle)/2
  (x - mean(x, na.rm = TRUE, trim = trim)) / sd(x, na.rm = TRUE)
}

# this will overwrite the columns in our dataframe
penguins |>
  mutate(across(ends_with("mm"),
                to_z)
  ) |>
  glimpse()

# Write a function that summarise multiple specified columns of a data frame


my_summary <- function(df,cols){
    df %>%
    summarise(across({{cols}},
      list( mean  = \(x) mean(x,na.rm = T),
            sd    = \(x) sd(x,na.rm = T))))
}

my_summary(penguins,ends_with("mm"))
