## ---------------------------
#| label: setup


library(knitr)
library(tidyverse)
library(gt)
library(pins)
library(tictoc)
library(tidymodels)
library(janitor)
library(googlesheets4)
library(googledrive)
library(RSQLite)
library(quarto)
library(here)

#ingesting from online source

data <- read_csv("https://go.wisc.edu/9pv6q1")


#manually clearning the data 
data <- data |>
    select(Species, 
           Island,
           `Culmen Length (mm)`,
           `Culmen Depth (mm)`,
           `Flipper Length (mm)`,
           `Body Mass (g)`, 
           Sex,
           `Date Egg`) |>
    mutate(species = word(Species, 1), 
           island = Island,
           bill_length_mm = `Culmen Length (mm)`,
           bill_depth_mm = `Culmen Depth (mm)`,
           flipper_length_mm = `Flipper Length (mm)`,
           body_mass_g = `Body Mass (g)`, 
           sex = Sex,
           year = year(`Date Egg`)) |> 
    select(species, 
           island,
           bill_length_mm,
           bill_depth_mm,
           flipper_length_mm,
           body_mass_g, 
           sex,
           year) |> 
  na.omit()




#p-quartiles() takes a df and bins the observations into the quartiles 
p_quartiles <- function(df) {
    df |> 
    select(body_mass_g) |> 
    as_vector() |> 
    quantile(probs = c(.25, .5, .75, 1),na.rm = TRUE)
}


quartile_data <- data |>
    split(data$species) |> 
#  group_split(species) |> #loses the grouping values 
    map(p_quartiles) |>
    bind_rows(.id = "id") |> 
    rename(species = id)





## ---------------------------
#| echo: true
quartile_data |> 
    gt() |>
    tab_header("Cutout values for weight per species")
    


## ---------------------------
#| echo: true
data <- data |> 
    group_by(species) |> 
    mutate(size = case_when(
        body_mass_g <= quartile_data[[cur_group_id(),2]] ~ "tiny",
        body_mass_g <= quartile_data[[cur_group_id(),3]] ~ "small",
        body_mass_g <= quartile_data[[cur_group_id(),4]] ~ "medium",
        body_mass_g > quartile_data[[cur_group_id(),4]] ~ "large")) |>
    ungroup() |> 
    mutate(size = factor(size, levels = c("tiny", "small", "medium", "large")))


## ----penguins-per-size-and-species----
#| echo: true

data |> 
    select(species, size) |>
    group_by(species) |> 
    reframe(count = fct_count(size |> as_vector())) |> 
    unnest(cols = c(count)) |> 
    rename(size = f,
           count = n) |> 
    group_by(species) |> 
    mutate(n = sum(count),
           percentage = round(count/n*100, 2)) |> 
    gt() |>
    tab_header("Sizes per species")



## ---------------------------
#| echo: true

data |> 
  ggplot(aes(x = species, y = body_mass_g, color = species)) +
  geom_jitter(alpha = 0.25) +
  geom_boxplot(alpha = 0.25) +
  theme_minimal() + 
  facet_grid( ~ island) +
  xlab("body mass in g")



## ----render-parametrized-report----
#| eval: false

## 
## quarto_render( here("reports/penguin-summary.qmd"),
##                output_format = "pdf",
##                execute_params = list(species = "Chinstrap"))
## 
## #check this post
## #https://stackoverflow.com/questions/73571919/how-to-pass-logical-parameters-with-the-quarto-r-package-to-the-knitr-chunk-opti
## 
## 
## 


## ---------------------------
#| echo: true
#| label: manual-ling


# mbl: mean bill length in mm 
# sdbl: standard deviation of bill length
# mbd: mean bill depth in mm
# sdbd: standard deviation of bill depth
# mfl: mean flipper length in mm
# sdfl: standard deviation of flipper length
# mbm: mean body mass in g
# sdbm: standard deviation of body mass

tic.clear()
tic("Manual calculations") 

data |> 
  filter(species == "Adelie") |> 
  summarize(
    species = unique(species),
    mbl = mean(bill_length_mm),
    sdbl = sd(bill_length_mm),
    mbd = mean(bill_depth_mm),
    sdbd = sd(bill_depth_mm),
    mfl = mean(flipper_length_mm),
    sdfl = sd(flipper_length_mm),
    mbm = mean(body_mass_g),
    sdbm = sd(body_mass_g)
    )

data |> 
  filter(species == "Gentoo") |> 
  summarize(
    species = unique(species),
    mbl = mean(bill_length_mm),
    sdbl = sd(bill_length_mm),
    mbd = mean(bill_depth_mm),
    sdbd = sd(bill_depth_mm),
    mfl = mean(flipper_length_mm),
    sdfl = sd(flipper_length_mm),
    mbm = mean(body_mass_g),
    sdbm = sd(body_mass_g)
    )

data |> 
  filter(species == "Chinstrap") |> 
  summarize(
    species = unique(species),
    mbl = mean(bill_length_mm),
    sdbl = sd(bill_length_mm),
    mbd = mean(bill_depth_mm),
    sdbd = sd(bill_depth_mm),
    mfl = mean(flipper_length_mm),
    sdfl = sd(flipper_length_mm),
    mbm = mean(body_mass_g),
    sdbm = sd(body_mass_g)
    )

manual_time <- toc()




## ---------------------------
#| echo: true
#| label: looping-ly
#| include: true
#| code-line-numbers: "4, 7-15"

tic.clear()
tic("For-loop calculations") 

for (penguin in c("Adelie", "Gentoo", "Chinstrap")) {
  data |> 
  filter(species == penguin) |> 
  summarize(species = unique(species),
            mbl = mean(bill_length_mm),
            sdbl = sd(bill_length_mm),
            mbd = mean(bill_depth_mm),
            sdbd = sd(bill_depth_mm),
            mfl = mean(flipper_length_mm),
            sdfl = sd(flipper_length_mm),
            mbm = mean(body_mass_g),
            sdbm = sd(body_mass_g)
            ) |> 
    print()

}
  
forloop_time <- toc()




## ---------------------------
#| echo: true
#| label: dplying


tic.clear()
tic("Dplyr calculations")

data |> 
  group_by(species) |> 
  summarize(mbl = mean(bill_length_mm),
            sdbl = sd(bill_length_mm),
            mbd = mean(bill_depth_mm),
            sdbd = sd(bill_depth_mm),
            mfl = mean(flipper_length_mm),
            sdfl = sd(flipper_length_mm),
            mbm = mean(body_mass_g),
            sdbm = sd(body_mass_g)
            )

dplyr_time <- toc()





## ---------------------------
#| echo: true

#encapsulating the code in a function
#the function takes the subset of the data as an dataframe argument 

summarize_penguin_population <- function(df) {

  df |>
    summarize(
      species = unique(species),
      mbl = mean(df$bill_length_mm),
      sdbl = sd(df$bill_length_mm),
      mbd = mean(df$bill_depth_mm),
      sdbd = sd(df$bill_depth_mm),
      mfl = mean(df$flipper_length_mm),
      sdfl = sd(df$flipper_length_mm),
      mbm = mean(df$body_mass_g),
      sdbm = sd(df$body_mass_g))

}




## ---------------------------
#| echo: true
#| label: purrr-ing


tic.clear()
tic("Map calculations") 

#this was my initial thought, 
data |>
  group_by(species) |>
  group_split() |>
  map(summarize_penguin_population) |> 
  bind_rows()

# data |>
#     split(data$species) |> 
#     map(summarize_penguin_population) |> 
#     bind_rows()

# an even shorter code relying on the dataframe specific map_df() functon 
# data |>
#     group_split(data$species) |>
#     map_df(summarize_penguin_population)


map_time <- toc()



## ---------------------------
#| include: true
#| label: calculations-by-method

#the factor function called and the explicit level assignment are needed to 
#prevent ggplot from reordering the values in alphabetic order

times_by_method <- tibble(method = factor(c("manual_ly",
                                      "forloop_ly",
                                      "group_by_ly",
                                      "map_ly"),
                                    levels = c("manual_ly",
                                      "forloop_ly",
                                      "group_by_ly",
                                      "map_ly")), 
                           duration = c(manual_time[[2]] - manual_time[[1]],
                                        forloop_time[[2]] - forloop_time[[1]],
                                        dplyr_time[[2]] - dplyr_time[[1]],
                                        map_time[[2]] - map_time[[1]] )) 
times_by_method |> 
  gt() |> 
  tab_header(
    title = "Time for calculations by method")

times_by_method |> 
  ggplot(aes(x = method, y = duration, fill = method))+
  geom_col() +
  ylab("duration in seconds")



## ---------------------------
#| echo: true
#| include: true



filtered_penguins <- read_csv(here("data/original_palmerpenguins.csv")) |>
  drop_na(ends_with("mm")) |> 
  select(species, ends_with("mm"), body_mass_g) |> 
  mutate(species = as_factor(species))

tic.clear()
tic("Fitting model")

model <- rand_forest() |> 
  set_engine("ranger") |> 
  set_mode("classification") |> 
  fit(species ~ bill_length_mm + bill_depth_mm + flipper_length_mm + body_mass_g, data = filtered_penguins)

fit_time <- toc()



## ---------------------------
# sim_data <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1HEQa23hR202egPcnyAtQHGyPRw1WWv61H6aHnAhjcrc/edit#gid=519943868")

janus_penguins <- read_csv("https://g-394ce9.dtn.globus.wisc.edu/public_data/1M_penguins.csv")


tic()

predict(model,
        new_data = slice_sample(.data = head(janus_penguins),
                                n = 10,
                                replace = FALSE),
        type = "prob")  #type = "class" returns a the type of penguin rather than the prob.,

toc()


## ---------------------------
sim_data_10 <- slice_sample(janus_penguins, n = 10)

tic("10 observations")
sim_data_10 |>
bind_cols(predict(model, new_data = sim_data_10)) |>
  count(species, .pred_class) |> 
  mutate(sample_size = nrow(sim_data_10))
obs_10 <- toc()


## ---------------------------
  ggplot() + 
  geom_density(mapping = aes(x = flipper_length_mm, color = species), 
               data = janus_penguins) +
    geom_density(mapping = aes(x = flipper_length_mm, color = species), 
               data = data)


## ---------------------------
#| label: slicing

sim_data_10 <- slice_sample(.data = janus_penguins, n = 10, replace = TRUE)
sim_data_100 <- slice_sample(.data = janus_penguins, n = 100, replace = TRUE)
sim_data_1k <- slice_sample(.data = janus_penguins, n = 1000, replace = TRUE)
sim_data_10k <- slice_sample(.data = janus_penguins, n = 10000, replace = TRUE)
sim_data_50k <- slice_sample(.data = janus_penguins, n = 50000, replace = TRUE)
sim_data_100k <- slice_sample(.data = janus_penguins, n = 100000, replace = TRUE)
sim_data_500k <- slice_sample(.data = janus_penguins, n = 500000, replace = TRUE)
sim_data_1M <-  slice_sample(.data = janus_penguins, n = 1000000, replace = TRUE)


## ---------------------------
#| echo: true
#| label: manualling-2

#####################################
# 100 points of data


tic.clear()
tic("Manual calculations 100") 

sim_data_100 |> 
  filter(species == "Adelie") |> 
  summarize(
    species = unique(species),
    mbl = mean(bill_length_mm),
    sdbl = sd(bill_length_mm),
    mbd = mean(bill_depth_mm),
    sdbd = sd(bill_depth_mm),
    mfl = mean(flipper_length_mm),
    sdfl = sd(flipper_length_mm),
    mbm = mean(body_mass_g),
    sdbm = sd(body_mass_g)
    )

sim_data_100 |> 
  filter(species == "Gentoo") |> 
  summarize(
    species = unique(species),
    mbl = mean(bill_length_mm),
    sdbl = sd(bill_length_mm),
    mbd = mean(bill_depth_mm),
    sdbd = sd(bill_depth_mm),
    mfl = mean(flipper_length_mm),
    sdfl = sd(flipper_length_mm),
    mbm = mean(body_mass_g),
    sdbm = sd(body_mass_g)
    )

sim_data_100 |> 
  filter(species == "Chinstrap") |> 
  summarize(
    species = unique(species),
    mbl = mean(bill_length_mm),
    sdbl = sd(bill_length_mm),
    mbd = mean(bill_depth_mm),
    sdbd = sd(bill_depth_mm),
    mfl = mean(flipper_length_mm),
    sdfl = sd(flipper_length_mm),
    mbm = mean(body_mass_g),
    sdbm = sd(body_mass_g)
    )

manual_time_100 <- toc()

#####################################
# 1000 points of data

tic.clear()
tic("Manual calculations 1000") 

sim_data_1k |> 
  filter(species == "Adelie") |> 
  summarize(
    species = unique(species),
    mbl = mean(bill_length_mm),
    sdbl = sd(bill_length_mm),
    mbd = mean(bill_depth_mm),
    sdbd = sd(bill_depth_mm),
    mfl = mean(flipper_length_mm),
    sdfl = sd(flipper_length_mm),
    mbm = mean(body_mass_g),
    sdbm = sd(body_mass_g)
    )

sim_data_1k |> 
  filter(species == "Gentoo") |> 
  summarize(
    species = unique(species),
    mbl = mean(bill_length_mm),
    sdbl = sd(bill_length_mm),
    mbd = mean(bill_depth_mm),
    sdbd = sd(bill_depth_mm),
    mfl = mean(flipper_length_mm),
    sdfl = sd(flipper_length_mm),
    mbm = mean(body_mass_g),
    sdbm = sd(body_mass_g)
    )

sim_data_1k |> 
  filter(species == "Chinstrap") |> 
  summarize(
    species = unique(species),
    mbl = mean(bill_length_mm),
    sdbl = sd(bill_length_mm),
    mbd = mean(bill_depth_mm),
    sdbd = sd(bill_depth_mm),
    mfl = mean(flipper_length_mm),
    sdfl = sd(flipper_length_mm),
    mbm = mean(body_mass_g),
    sdbm = sd(body_mass_g)
    )

manual_time_1k <- toc()

#####################################
# 10000 points of data

tic.clear()
tic("Manual calculations 10000") 

sim_data_10k |> 
  filter(species == "Adelie") |> 
  summarize(
    species = unique(species),
    mbl = mean(bill_length_mm),
    sdbl = sd(bill_length_mm),
    mbd = mean(bill_depth_mm),
    sdbd = sd(bill_depth_mm),
    mfl = mean(flipper_length_mm),
    sdfl = sd(flipper_length_mm),
    mbm = mean(body_mass_g),
    sdbm = sd(body_mass_g)
    )

sim_data_10k |> 
  filter(species == "Gentoo") |> 
  summarize(
    species = unique(species),
    mbl = mean(bill_length_mm),
    sdbl = sd(bill_length_mm),
    mbd = mean(bill_depth_mm),
    sdbd = sd(bill_depth_mm),
    mfl = mean(flipper_length_mm),
    sdfl = sd(flipper_length_mm),
    mbm = mean(body_mass_g),
    sdbm = sd(body_mass_g)
    )

sim_data_10k |> 
  filter(species == "Chinstrap") |> 
  summarize(
    species = unique(species),
    mbl = mean(bill_length_mm),
    sdbl = sd(bill_length_mm),
    mbd = mean(bill_depth_mm),
    sdbd = sd(bill_depth_mm),
    mfl = mean(flipper_length_mm),
    sdfl = sd(flipper_length_mm),
    mbm = mean(body_mass_g),
    sdbm = sd(body_mass_g)
    )

manual_time_10k <- toc()

#####################################
# 50000 points of data

tic.clear()
tic("Manual calculations 50k") 

sim_data_50k |> 
  filter(species == "Adelie") |> 
  summarize(
    species = unique(species),
    mbl = mean(bill_length_mm),
    sdbl = sd(bill_length_mm),
    mbd = mean(bill_depth_mm),
    sdbd = sd(bill_depth_mm),
    mfl = mean(flipper_length_mm),
    sdfl = sd(flipper_length_mm),
    mbm = mean(body_mass_g),
    sdbm = sd(body_mass_g)
    )

sim_data_50k |> 
  filter(species == "Gentoo") |> 
  summarize(
    species = unique(species),
    mbl = mean(bill_length_mm),
    sdbl = sd(bill_length_mm),
    mbd = mean(bill_depth_mm),
    sdbd = sd(bill_depth_mm),
    mfl = mean(flipper_length_mm),
    sdfl = sd(flipper_length_mm),
    mbm = mean(body_mass_g),
    sdbm = sd(body_mass_g)
    )

sim_data_50k |> 
  filter(species == "Chinstrap") |> 
  summarize(
    species = unique(species),
    mbl = mean(bill_length_mm),
    sdbl = sd(bill_length_mm),
    mbd = mean(bill_depth_mm),
    sdbd = sd(bill_depth_mm),
    mfl = mean(flipper_length_mm),
    sdfl = sd(flipper_length_mm),
    mbm = mean(body_mass_g),
    sdbm = sd(body_mass_g)
    )

manual_time_50k <- toc()

#####################################
# 100,000 points of data
# it took 3667.102 sec to simulate this on my laptop
# cci-workbench took 3693.41 sec elapsed. ACTUALLY LONGER 


tic.clear()
tic("Manual calculations 100k")
sim_data_100k |>
  filter(species == "Adelie") |>
  summarize(
    species = unique(species),
    mbl = mean(bill_length_mm),
    sdbl = sd(bill_length_mm),
    mbd = mean(bill_depth_mm),
    sdbd = sd(bill_depth_mm),
    mfl = mean(flipper_length_mm),
    sdfl = sd(flipper_length_mm),
    mbm = mean(body_mass_g),
    sdbm = sd(body_mass_g)
    )

sim_data_100k |>
  filter(species == "Gentoo") |>
  summarize(
    species = unique(species),
    mbl = mean(bill_length_mm),
    sdbl = sd(bill_length_mm),
    mbd = mean(bill_depth_mm),
    sdbd = sd(bill_depth_mm),
    mfl = mean(flipper_length_mm),
    sdfl = sd(flipper_length_mm),
    mbm = mean(body_mass_g),
    sdbm = sd(body_mass_g)
    )

sim_data_100k |>
  filter(species == "Chinstrap") |>
  summarize(
    species = unique(species),
    mbl = mean(bill_length_mm),
    sdbl = sd(bill_length_mm),
    mbd = mean(bill_depth_mm),
    sdbd = sd(bill_depth_mm),
    mfl = mean(flipper_length_mm),
    sdfl = sd(flipper_length_mm),
    mbm = mean(body_mass_g),
    sdbm = sd(body_mass_g)
    )

manual_time_100k <- toc()

#####################################
# 500,000 points of data
# it took 3667.102 sec to simulate this on my laptop
# cci-workbench took 3693.41 sec elapsed. ACTUALLY LONGER 


tic.clear()
tic("Manual calculations 500k")
sim_data_500k |>
  filter(species == "Adelie") |>
  summarize(
    species = unique(species),
    mbl = mean(bill_length_mm),
    sdbl = sd(bill_length_mm),
    mbd = mean(bill_depth_mm),
    sdbd = sd(bill_depth_mm),
    mfl = mean(flipper_length_mm),
    sdfl = sd(flipper_length_mm),
    mbm = mean(body_mass_g),
    sdbm = sd(body_mass_g)
    )

sim_data_500k |>
  filter(species == "Gentoo") |>
  summarize(
    species = unique(species),
    mbl = mean(bill_length_mm),
    sdbl = sd(bill_length_mm),
    mbd = mean(bill_depth_mm),
    sdbd = sd(bill_depth_mm),
    mfl = mean(flipper_length_mm),
    sdfl = sd(flipper_length_mm),
    mbm = mean(body_mass_g),
    sdbm = sd(body_mass_g)
    )

sim_data_500k |>
  filter(species == "Chinstrap") |>
  summarize(
    species = unique(species),
    mbl = mean(bill_length_mm),
    sdbl = sd(bill_length_mm),
    mbd = mean(bill_depth_mm),
    sdbd = sd(bill_depth_mm),
    mfl = mean(flipper_length_mm),
    sdfl = sd(flipper_length_mm),
    mbm = mean(body_mass_g),
    sdbm = sd(body_mass_g)
    )

manual_time_500k <- toc()

#####################################
# 1,00,000 points of data
# it took 3667.102 sec to simulate this on my laptop
# cci-workbench took 3693.41 sec elapsed. ACTUALLY LONGER 


tic.clear()
tic("Manual calculations 1M")
sim_data_1M |>
  filter(species == "Adelie") |>
  summarize(
    species = unique(species),
    mbl = mean(bill_length_mm),
    sdbl = sd(bill_length_mm),
    mbd = mean(bill_depth_mm),
    sdbd = sd(bill_depth_mm),
    mfl = mean(flipper_length_mm),
    sdfl = sd(flipper_length_mm),
    mbm = mean(body_mass_g),
    sdbm = sd(body_mass_g)
    )

sim_data_1M |>
  filter(species == "Gentoo") |>
  summarize(
    species = unique(species),
    mbl = mean(bill_length_mm),
    sdbl = sd(bill_length_mm),
    mbd = mean(bill_depth_mm),
    sdbd = sd(bill_depth_mm),
    mfl = mean(flipper_length_mm),
    sdfl = sd(flipper_length_mm),
    mbm = mean(body_mass_g),
    sdbm = sd(body_mass_g)
    )

sim_data_1M |>
  filter(species == "Chinstrap") |>
  summarize(
    species = unique(species),
    mbl = mean(bill_length_mm),
    sdbl = sd(bill_length_mm),
    mbd = mean(bill_depth_mm),
    sdbd = sd(bill_depth_mm),
    mfl = mean(flipper_length_mm),
    sdfl = sd(flipper_length_mm),
    mbm = mean(body_mass_g),
    sdbm = sd(body_mass_g)
    )

manual_time_1M <- toc()




## ---------------------------
#| include: true
#| label: calculations-times-manual-2

calculations_times_manual <- tibble(method = factor(c("100 data points",
                  "1k data points",
                  "10k data points",
                  "50k data points",
                  "100k data points",
                  "500k data points",
                  "1M data points"),
                  levels = c("100 data points",
                  "1k data points",
                  "10k data points",
                  "50k data points",
                  "100k data points",
                  "500k data points",
                  "1M data points")), 
       duration = c(manual_time_100[[2]] - manual_time_100[[1]],
                    manual_time_1k[[2]] - manual_time_1k[[1]], 
                    manual_time_10k[[2]] - manual_time_10k[[1]],
                    manual_time_50k[[2]] - manual_time_50k[[1]],
                    manual_time_100k[[2]] - manual_time_100k[[1]],
                    manual_time_500k[[2]] - manual_time_500k[[1]],
                    manual_time_1M[[2]] - manual_time_1M[[1]]))

calculations_times_manual |> 
  gt() |> 
  tab_header(
    title = "Calculation time as data increases",
    subtitle = "Calculation method: manual")

calculations_times_manual  |> 
  ggplot(aes(x = method, y = duration, fill = method))+
  geom_col() +
  ylab("duration in seconds") +
  coord_flip() + 
  scale_x_discrete(limits = rev(levels(calculations_times_manual$method)))



## ---------------------------
#| echo: true
#| label: looping-2
#| include: true



######################################
# 10 data points

tic.clear()
tic("For-loop-10") 

for (penguin in c("Adelie", "Gentoo", "Chinstrap")) {
  sim_data_10 |> 
  filter(species == penguin) |> 
  summarize(species = unique(species),
            mbl = mean(bill_length_mm),
            sdbl = sd(bill_length_mm),
            mbd = mean(bill_depth_mm),
            sdbd = sd(bill_depth_mm),
            mfl = mean(flipper_length_mm),
            sdfl = sd(flipper_length_mm),
            mbm = mean(body_mass_g),
            sdbm = sd(body_mass_g)
            ) |> 
    print()

}
  
forloop_time_10 <- toc()

######################################
# 100 data points

tic.clear()
tic("For-loop-100") 

for (penguin in c("Adelie", "Gentoo", "Chinstrap")) {
  sim_data_100 |> 
  filter(species == penguin) |> 
  summarize(species = unique(species),
            mbl = mean(bill_length_mm),
            sdbl = sd(bill_length_mm),
            mbd = mean(bill_depth_mm),
            sdbd = sd(bill_depth_mm),
            mfl = mean(flipper_length_mm),
            sdfl = sd(flipper_length_mm),
            mbm = mean(body_mass_g),
            sdbm = sd(body_mass_g)
            ) |> 
    print()

}
  
forloop_time_100 <- toc()

######################################
# 1000 data points

tic.clear()
tic("For-loop-1k") 

for (penguin in c("Adelie", "Gentoo", "Chinstrap")) {
  sim_data_1k |> 
  filter(species == penguin) |> 
  summarize(species = unique(species),
            mbl = mean(bill_length_mm),
            sdbl = sd(bill_length_mm),
            mbd = mean(bill_depth_mm),
            sdbd = sd(bill_depth_mm),
            mfl = mean(flipper_length_mm),
            sdfl = sd(flipper_length_mm),
            mbm = mean(body_mass_g),
            sdbm = sd(body_mass_g)
            ) |> 
    print()

}
  
forloop_time_1k <- toc()

######################################
# 10000 data points

tic.clear()
tic("For-loop-10k") 

for (penguin in c("Adelie", "Gentoo", "Chinstrap")) {
  sim_data_10k |> 
  filter(species == penguin) |> 
  summarize(species = unique(species),
            mbl = mean(bill_length_mm),
            sdbl = sd(bill_length_mm),
            mbd = mean(bill_depth_mm),
            sdbd = sd(bill_depth_mm),
            mfl = mean(flipper_length_mm),
            sdfl = sd(flipper_length_mm),
            mbm = mean(body_mass_g),
            sdbm = sd(body_mass_g)
            ) |> 
    print()

}
  
forloop_time_10k <- toc()

######################################
# 50000 data points

tic.clear()
tic("For-loop-50k") 

for (penguin in c("Adelie", "Gentoo", "Chinstrap")) {
  sim_data_50k |> 
  filter(species == penguin) |> 
  summarize(species = unique(species),
            mbl = mean(bill_length_mm),
            sdbl = sd(bill_length_mm),
            mbd = mean(bill_depth_mm),
            sdbd = sd(bill_depth_mm),
            mfl = mean(flipper_length_mm),
            sdfl = sd(flipper_length_mm),
            mbm = mean(body_mass_g),
            sdbm = sd(body_mass_g)
            ) |> 
    print()

}
  
forloop_time_50k <- toc()

######################################
# 100000 data points

tic.clear()
tic("For-loop-100k") 

for (penguin in c("Adelie", "Gentoo", "Chinstrap")) {
  sim_data_100k |> 
  filter(species == penguin) |> 
  summarize(species = unique(species),
            mbl = mean(bill_length_mm),
            sdbl = sd(bill_length_mm),
            mbd = mean(bill_depth_mm),
            sdbd = sd(bill_depth_mm),
            mfl = mean(flipper_length_mm),
            sdfl = sd(flipper_length_mm),
            mbm = mean(body_mass_g),
            sdbm = sd(body_mass_g)
            ) |> 
    print()

}
  
forloop_time_100k <- toc()

######################################
# 500000 data points

tic.clear()
tic("For-loop-500k") 

for (penguin in c("Adelie", "Gentoo", "Chinstrap")) {
  sim_data_500k |> 
  filter(species == penguin) |> 
  summarize(species = unique(species),
            mbl = mean(bill_length_mm),
            sdbl = sd(bill_length_mm),
            mbd = mean(bill_depth_mm),
            sdbd = sd(bill_depth_mm),
            mfl = mean(flipper_length_mm),
            sdfl = sd(flipper_length_mm),
            mbm = mean(body_mass_g),
            sdbm = sd(body_mass_g)
            ) |> 
    print()

}
  
forloop_time_500k <- toc()

######################################
# 1000000 data points

tic.clear()
tic("For-loop-1M") 

for (penguin in c("Adelie", "Gentoo", "Chinstrap")) {
  sim_data_1M |> 
  filter(species == penguin) |> 
  summarize(species = unique(species),
            mbl = mean(bill_length_mm),
            sdbl = sd(bill_length_mm),
            mbd = mean(bill_depth_mm),
            sdbd = sd(bill_depth_mm),
            mfl = mean(flipper_length_mm),
            sdfl = sd(flipper_length_mm),
            mbm = mean(body_mass_g),
            sdbm = sd(body_mass_g)
            ) |> 
    print()

}
  
forloop_time_1M <- toc()



## ---------------------------
#| include: true
#| label: calculations-times-for-loop-2



calculations_times_forloop <- tibble(method = factor(c("100 data points",
                  "1k data points",
                  "10k data points",
                  "50k data points",
                  "100k data points",
                  "500k data points",
                  "1M data points"),
                  levels = c("100 data points",
                  "1k data points",
                  "10k data points",
                  "50k data points",
                  "100k data points",
                  "500k data points",
                  "1M data points")), 
       duration = c(forloop_time_100[[2]] - forloop_time_100[[1]],
                    forloop_time_1k[[2]] - forloop_time_1k[[1]], 
                    forloop_time_10k[[2]] - forloop_time_10k[[1]],
                    forloop_time_50k[[2]] - forloop_time_50k[[1]],
                    forloop_time_100k[[2]] - forloop_time_100k[[1]],
                    forloop_time_500k[[2]] - forloop_time_500k[[1]],
                    forloop_time_1M[[2]] - forloop_time_1M[[1]]))

calculations_times_forloop |> 
  gt() |> 
  tab_header(
    title = "Calculation time as data increases",
    subtitle = "Calculation method: for-loop")

calculations_times_forloop  |> 
  ggplot(aes(x = method, y = duration, fill = method))+
  geom_col() +
  ylab("duration in seconds") +
  coord_flip() + 
  scale_x_discrete(limits = rev(levels(calculations_times_forloop$method)))



## ---------------------------
#| echo: true
#| label: dplying-2



########################################
# 10 points of data

tic.clear()
tic("Dplyr calculations 10 points of data")

sim_data_10 |> 
  group_by(species) |> 
  summarize(mbl = mean(bill_length_mm),
            sdbl = sd(bill_length_mm),
            mbd = mean(bill_depth_mm),
            sdbd = sd(bill_depth_mm),
            mfl = mean(flipper_length_mm),
            sdfl = sd(flipper_length_mm),
            mbm = mean(body_mass_g),
            sdbm = sd(body_mass_g)
            )

dplyr_time_10 <- toc()

########################################
# 100 points of data

tic.clear()
tic("Dplyr calculations 10 points of data")

sim_data_100 |> 
  group_by(species) |> 
  summarize(mbl = mean(bill_length_mm),
            sdbl = sd(bill_length_mm),
            mbd = mean(bill_depth_mm),
            sdbd = sd(bill_depth_mm),
            mfl = mean(flipper_length_mm),
            sdfl = sd(flipper_length_mm),
            mbm = mean(body_mass_g),
            sdbm = sd(body_mass_g)
            )

dplyr_time_100 <- toc()

########################################
# 1000 points of data

tic.clear()
tic("Dplyr calculations 1000 points of data")

sim_data_1k |> 
  group_by(species) |> 
  summarize(mbl = mean(bill_length_mm),
            sdbl = sd(bill_length_mm),
            mbd = mean(bill_depth_mm),
            sdbd = sd(bill_depth_mm),
            mfl = mean(flipper_length_mm),
            sdfl = sd(flipper_length_mm),
            mbm = mean(body_mass_g),
            sdbm = sd(body_mass_g)
            )

dplyr_time_1k <- toc()

########################################
# 10000 points of data

tic.clear()
tic("Dplyr calculations 10000 points of data")

sim_data_10k |>  
  group_by(species) |> 
  summarize(mbl = mean(bill_length_mm),
            sdbl = sd(bill_length_mm),
            mbd = mean(bill_depth_mm),
            sdbd = sd(bill_depth_mm),
            mfl = mean(flipper_length_mm),
            sdfl = sd(flipper_length_mm),
            mbm = mean(body_mass_g),
            sdbm = sd(body_mass_g)
            )

dplyr_time_10k <- toc()

########################################
# 50000 points of data

tic.clear()
tic("Dplyr calculations 50000 points of data")

sim_data_50k |> 
  group_by(species) |> 
  summarize(mbl = mean(bill_length_mm),
            sdbl = sd(bill_length_mm),
            mbd = mean(bill_depth_mm),
            sdbd = sd(bill_depth_mm),
            mfl = mean(flipper_length_mm),
            sdfl = sd(flipper_length_mm),
            mbm = mean(body_mass_g),
            sdbm = sd(body_mass_g)
            )

dplyr_time_50k <- toc()


########################################
# 100000 points of data

tic.clear()
tic("Dplyr calculations 100000 points of data")

sim_data_100k |>
  group_by(species) |> 
  summarize(mbl = mean(bill_length_mm),
            sdbl = sd(bill_length_mm),
            mbd = mean(bill_depth_mm),
            sdbd = sd(bill_depth_mm),
            mfl = mean(flipper_length_mm),
            sdfl = sd(flipper_length_mm),
            mbm = mean(body_mass_g),
            sdbm = sd(body_mass_g)
            )

dplyr_time_100k <- toc()

########################################
# 500000 points of data

tic.clear()
tic("Dplyr calculations 500000 points of data")

sim_data_500k |>
  group_by(species) |> 
  summarize(mbl = mean(bill_length_mm),
            sdbl = sd(bill_length_mm),
            mbd = mean(bill_depth_mm),
            sdbd = sd(bill_depth_mm),
            mfl = mean(flipper_length_mm),
            sdfl = sd(flipper_length_mm),
            mbm = mean(body_mass_g),
            sdbm = sd(body_mass_g)
            )

dplyr_time_500k <- toc()


########################################
# 100000 points of data

tic.clear()
tic("Dplyr calculations 1M points of data")

sim_data_1M |>
  group_by(species) |> 
  summarize(mbl = mean(bill_length_mm),
            sdbl = sd(bill_length_mm),
            mbd = mean(bill_depth_mm),
            sdbd = sd(bill_depth_mm),
            mfl = mean(flipper_length_mm),
            sdfl = sd(flipper_length_mm),
            mbm = mean(body_mass_g),
            sdbm = sd(body_mass_g)
            )

dplyr_time_1M <- toc()



## ---------------------------
#| include: true
#| label: calculations-times-for-dplyr-2

tibble(duration = c(dplyr_time_100,
                    dplyr_time_1k, 
                    dplyr_time_10k,
                    dplyr_time_50k,
                    dplyr_time_100k,
                    dplyr_time_500k,
                    dplyr_time_1M)) |> 
  gt() |> 
  tab_header(
    title = "Calculation time as data increases",
    subtitle = "Calculation method: for-loop")

calculations_times_dplyr <- tibble(method = factor(c("100 data points",
                  "1k data points",
                  "10k data points",
                  "50k data points",
                  "100k data points",
                  "500k data points",
                  "1M data points"),
                  levels = c("100 data points",
                  "1k data points",
                  "10k data points",
                  "50k data points",
                  "100k data points",
                  "500k data points",
                  "1M data points")), 
       duration = c(dplyr_time_100[[2]] - dplyr_time_100[[1]],
                    dplyr_time_1k[[2]] - dplyr_time_1k[[1]], 
                    dplyr_time_10k[[2]] - dplyr_time_10k[[1]],
                    dplyr_time_50k[[2]] - dplyr_time_50k[[1]],
                    dplyr_time_100k[[2]] - dplyr_time_100k[[1]],
                    dplyr_time_500k[[2]] - dplyr_time_500k[[1]],
                    dplyr_time_1M[[2]] - dplyr_time_1M[[1]]))

calculations_times_dplyr |> 
  gt() |> 
  tab_header(
    title = "Calculation time as data increases",
    subtitle = "Calculation method: dplyr")

calculations_times_dplyr  |> 
  ggplot(aes(x = method, y = duration, fill = method))+
  geom_col() +
  ylab("duration in seconds") +
  coord_flip() + 
  scale_x_discrete(limits = rev(levels(calculations_times_forloop$method)))



## ---------------------------
#| label: map-calculations

############################
# 10 points of data 
tic.clear()
tic("Map calculations 10 points of data ") 

sim_data_10 |> 
  group_by(species) |>
  group_split() |>
  map(summarize_penguin_population) |> 
  list_rbind() 

map_time_10 <- toc()

############################
# 100 points of data 
tic.clear()
tic("Map calculations 100 points of data ") 

sim_data_100 |> 
  group_by(species) |>
  group_split() |>
  map(summarize_penguin_population) |> 
  list_rbind()

map_time_100 <- toc()

############################
# 1000 points of data 
tic.clear()
tic("map() calculations 1000 points of data ") 

sim_data_1k |> 
  group_by(species) |>
  group_split() |>
  map(summarize_penguin_population) |> 
  list_rbind()

map_time_1k <- toc()

############################
# 10000 points of data 
tic.clear()
tic("map() calculations 10000 points of data ") 

sim_data_10k |> 
  group_by(species) |>
  group_split() |>
  map(summarize_penguin_population) |> 
  list_rbind()

map_time_10k <- toc()

############################
# 50000 points of data 
tic.clear()
tic("map() calculations 50000 points of data ") 

sim_data_50k |> 
  group_by(species) |>
  group_split() |>
  map(summarize_penguin_population) |> 
  list_rbind()

map_time_50k <- toc()

############################
# 100000 points of data 
tic.clear()
tic("map() calculations 100000 points of data ") 

sim_data_100k |> 
  group_by(species) |>
  group_split() |>
  map(summarize_penguin_population) |> 
  list_rbind()

map_time_100k <- toc()

############################
# 100000 points of data 
tic.clear()
tic("map() calculations 100000 points of data ") 

sim_data_100k |> 
  group_by(species) |>
  group_split() |>
  map(summarize_penguin_population) |> 
  list_rbind()

map_time_100k <- toc()

############################
# 500000 points of data 
tic.clear()
tic("map() calculations 500000 points of data ") 

sim_data_500k |> 
  group_by(species) |>
  group_split() |>
  map(summarize_penguin_population) |> 
  list_rbind()

map_time_500k <- toc()

############################
# 1000000 points of data 
tic.clear()
tic("map() calculations 1000000 points of data ") 

sim_data_1M |> 
  group_by(species) |>
  group_split() |>
  map(summarize_penguin_population) |> 
  list_rbind()

map_time_1M <- toc()



## ---------------------------
#| include: true
#| label: calculations-times-for-map-2



calculations_times_map <- tibble(method = factor(c("100 data points",
                  "1k data points",
                  "10k data points",
                  "50k data points",
                  "100k data points",
                  "500k data points",
                  "1M data points"),
                  levels = c("100 data points",
                  "1k data points",
                  "10k data points",
                  "50k data points",
                  "100k data points",
                  "500k data points",
                  "1M data points")), 
       duration = c(map_time_100[[2]] - map_time_100[[1]],
                    map_time_1k[[2]] - map_time_1k[[1]], 
                    map_time_10k[[2]] - map_time_10k[[1]],
                    map_time_50k[[2]] - map_time_50k[[1]],
                    map_time_100k[[2]] - map_time_100k[[1]],
                    map_time_500k[[2]] - map_time_500k[[1]],
                    map_time_1M[[2]] - map_time_1M[[1]]))

calculations_times_map |> 
  gt() |> 
  tab_header(
    title = "Calculation time as data increases",
    subtitle = "Calculation method: map")

calculations_times_map  |> 
  ggplot(aes(x = method, y = duration, fill = method))+
  geom_col() +
  ylab("duration in seconds") +
  coord_flip() + 
  scale_x_discrete(limits = rev(levels(calculations_times_map$method)))



## ---------------------------

# sim_data_100 <- slice_sample(.data = janus_penguins, n = 100, replace = TRUE)
# sim_data_1000 <- slice_sample(.data = janus_penguins, n = 1000, replace = TRUE)
# sim_data_10000 <- slice_sample(.data = janus_penguins, n = 10000, replace = TRUE)
# sim_data_50000 <- slice_sample(.data = janus_penguins, n = 50000, replace = TRUE)
tic.clear()
tic("100 observations")
model_perf_100 <- sim_data_100 |>
bind_cols(predict(model, new_data = sim_data_100)) |> 
  count(species, .pred_class) |> 
  mutate(sample_size = nrow(sim_data_100),
         pred_species = .pred_class) |> 
  select(-.pred_class)
obs_100 <- toc()

tic.clear()
tic("1000 observations")
model_perf_1k <- sim_data_1k |>
bind_cols(predict(model, new_data = sim_data_1k)) |> 
  count(species, .pred_class) |> 
  mutate(sample_size = nrow(sim_data_1k),
         pred_species = .pred_class) |> 
  select(-.pred_class)
obs_1k <- toc()

tic.clear()
tic("10k observations")
model_perf_10k <- sim_data_10k |>
bind_cols(predict(model, new_data = sim_data_10k)) |> 
  count(species, .pred_class) |> 
  mutate(sample_size = nrow(sim_data_10k),
         pred_species = .pred_class) |> 
  select(-.pred_class)
obs_10k <- toc()

tic.clear()
tic("50k observations")
model_perf_50k <- sim_data_50k |>
bind_cols(predict(model, new_data = sim_data_50k)) |> 
  count(species, .pred_class) |> 
  mutate(sample_size = nrow(sim_data_50k),
         pred_species = .pred_class) |> 
  select(-.pred_class)
obs_50k <- toc()


tic.clear()
tic("100k observations")
model_perf_100k <- sim_data_100k |>
bind_cols(predict(model, new_data = sim_data_100k)) |> 
  count(species, .pred_class) |> 
  mutate(sample_size = nrow(sim_data_100k),
         pred_species = .pred_class) |> 
  select(-.pred_class)
obs_100k <- toc()

tic.clear()
tic("500k observations")
model_perf_500k <- sim_data_500k |>
bind_cols(predict(model, new_data = sim_data_500k)) |> 
  count(species, .pred_class) |> 
  mutate(sample_size = nrow(sim_data_500k),
         pred_species = .pred_class) |> 
  select(-.pred_class)
obs_500k <- toc()

tic.clear()
tic("1M observations")
model_perf_1M <- sim_data_1M |>
bind_cols(predict(model, new_data = sim_data_1M)) |> 
  count(species, .pred_class) |> 
  mutate(sample_size = nrow(sim_data_1M),
         pred_species = .pred_class) |> 
  select(-.pred_class)
obs_1M <- toc()




## ---------------------------
#| include: true
#| label: calculations-model-performance



model_perf  <- tibble(method = factor(c("100 data points",
                  "1k data points",
                  "10k data points",
                  "50k data points",
                  "100k data points",
                  "500k data points",
                  "1M data points"),
                  levels = c("100 data points",
                  "1k data points",
                  "10k data points",
                  "50k data points",
                  "100k data points",
                  "500k data points",
                  "1M data points")), 
       miss_identifications = c(model_perf_100 |> filter(species != pred_species) |> pluck(2) |> sum(),
                                model_perf_1k |> filter(species != pred_species) |> pluck(2) |> sum(),
                                model_perf_10k |> filter(species != pred_species) |> pluck(2) |> sum(),
                                model_perf_50k |> filter(species != pred_species) |> pluck(2) |> sum(),
                                model_perf_100k |> filter(species != pred_species) |> pluck(2) |> sum(),
                                model_perf_500k |> filter(species != pred_species) |> pluck(2) |> sum(),
                                model_perf_1M |> filter(species != pred_species) |> pluck(2) |> sum()
                                ),
       perc_miss_id = c(model_perf_100 |> filter(species != pred_species) |> pluck(2) |> sum()/100*100,
                        model_perf_1k |> filter(species != pred_species) |> pluck(2) |> sum()/1000*100,
                        model_perf_10k |> filter(species != pred_species) |> pluck(2) |> sum()/10000*100,
                        model_perf_50k |> filter(species != pred_species) |> pluck(2) |> sum()/50000*100,
                        model_perf_100k |> filter(species != pred_species) |> pluck(2) |> sum()/100000*100,
                        model_perf_500k |> filter(species != pred_species) |> pluck(2) |> sum()/500000*100,
                        model_perf_1M |> filter(species != pred_species) |> pluck(2) |> sum()/1000000*100
                        ))

model_perf |> 
  mutate() |> 
  gt() |> 
  tab_header(
    title = "Model Performance")

calculations_times_map  |> 
  ggplot(aes(x = method, y = duration, fill = method))+
  geom_col() +
  ylab("duration in seconds") +
  coord_flip() + 
  scale_x_discrete(limits = rev(levels(calculations_times_map$method)))


