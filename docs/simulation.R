library(ggplot2)
library(dplyr)
library(tidyr)
source("R/inter_extra_polate.R")
source("R/step_interp.R")
set.seed(123)

#logistic with observation error
gen_logistic <- function(x = 1:100, obs_err = 2) {
  #not exactly sure what these parameters do, but fiddled around until I got mostly nice looking logistic growth curves.
  phi1 <- rnorm(1, 100, 5) #related to max height?
  phi2 <- rnorm(1, -10, 2) #related to starting height?
  phi3 <- rbeta(1, 1.2, 8) #related to growth rate?
  
  #logistic growth equation + random (normal) error
  y <- phi1 / (1 + exp(-(phi2 + phi3 * x))) + rnorm(x, sd = obs_err)
  y
}
x <- 1:100
plot(x, gen_tree(x, 2))



#process error

start <- 1
beta <- 1.1

n_t1 = exp(beta + log(start))
n_t1

## ricker model with process error and observation error
r <- log(1.1)
K <- 100
n_t <- start
n_t1 = n_t * exp(r*(1- n_t/K))
rs <- rep(log(1.1), 100)
purrr::accumulate(rs, function(n_t, r) n_t * exp(r * (1 - n_t/K)), .init = 1) |> plot()
exp(rbeta(1000, 3, 9)) |> hist()
gen_ricker <- function(x = 1:100, K = rnorm(1, 100, 20), obs_err = 2) {
  rs <- rbeta(length(x), 3, 9) #random growth rates each year
  purrr::accumulate(rs, function(n_t, r) n_t * exp(r * (1 - n_t/K)), .init = 5)[seq_along(x)] + rnorm(length(x), 0, obs_err)
}

## start simulation here
n_trees <- 50
n_plots <- 20
obs_err <- 4
sim_setup <-
  expand_grid(
    tree = seq_len(n_trees),
    plot = seq_len(n_plots)
  ) |> 
  #give each tree a unique ID and lifespan
  mutate(tree = paste0("p", plot, "t", tree)) |> 
  #give each tree a birth time and death time
  mutate(birth_time = sample(1:100, n(), replace = TRUE)) |> 
  mutate(death_time = birth_time + sample(50:150, n(), replace = TRUE)) 

#introduce mass mortality event that kills 20% of trees in plot 1 in year 73 (a non-inventory, not midpoint year) 
mass_mortality_trees <- sim_setup |> 
  group_by(tree) |> 
  filter(plot == 1, between(75, birth_time, death_time)) |> 
  ungroup() |> 
  slice_sample(prop = 0.2) |> pull(tree)
sim_setup <- sim_setup |> 
  mutate(death_time = if_else(tree %in% mass_mortality_trees, 75, death_time))

sim_setup
sim <- sim_setup |> 
  #run simulation 100 timesteps
  group_by(tree, plot) |> 
  expand(time = birth_time:death_time) |>
  arrange(plot, tree, time) |> 
  #grow trees
  group_by(tree) |> 
  mutate(ht = gen_ricker(time, obs_err = obs_err)) |> 
  mutate(mortyr = max(time)) |> 
  #only keep the middle years so birth and deaths kinda even out in this period
  filter(time >= 25, time <= 125)
sim


ggplot(sim, aes(x = time, y = ht, color = as.factor(plot), group = tree)) + 
  geom_point() +
  geom_line() +
  theme(legend.position = "none")

# keep only every 10 years, stagger plots by 1 years
#this should mean that every hear has the same number of plots?
period <- 10
periodic_expanded <- sim |>
  mutate(ht = if_else(time %in% seq(24 + unique(plot) %% period, 125, period), ht, NA)) |> 
  #remove leading NAs and dates after last inventory
  group_by(tree) |> 
  filter(!all(is.na(ht))) |> 
  filter(row_number() >= which.min(ht))

periodic <- periodic_expanded[complete.cases(periodic_expanded), ]
#double check there are the same number of plots in every year
periodic |> 
  group_by(time) |> 
  summarize(n_plots = length(unique(plot))) |> 
  group_by(n_plots) |> 
  summarize(n_time = sum(n_plots))

ggplot(periodic, aes(x = time, y = ht, color = as.factor(plot), group = tree)) + 
  geom_point() +
  geom_line() +
  theme(legend.position = "none")

# interpolate
#assuming we know the exact death time

interpolated <-
  periodic_expanded |> 
  #linearly interpolate ht
  mutate(ht = inter_extra_polate(time, ht)) |> 
  #filter dead trees
  filter(time < mortyr)

ggplot(interpolated, aes(x = time, y = ht, color = as.factor(plot), group = tree)) +
  geom_point() +
  geom_line() +
  theme(legend.position = "none")

#assume we don't know the exact death time
death <- periodic |> 
  group_by(tree) |> 
  mutate(dead = time >= mortyr) |> 
  select(tree, time, dead)

interpolated_midpt <-
  left_join(periodic_expanded, death) |> 
  #remove leading NAs
  group_by(tree) |> 
  filter(!all(is.na(ht))) |> 
  filter(time >= which.min(ht)) |> 
  #linearly interpolate ht
  mutate(ht = inter_extra_polate(time, ht)) |> 
  #use midpoint rule for death
  mutate(dead = step_interp(dead)) |> 
  filter(!dead)

ggplot(interpolated_midpt, aes(x = time, y = ht, color = as.factor(plot), group = tree)) +
  geom_point() +
  geom_line() +
  theme(legend.position = "none")



# means ± SD

sim_stat <- sim |> 
  group_by(time) |> 
  summarize(
    sum = sum(ht, na.rm = TRUE),
    mean = mean(ht, na.rm = TRUE),
    sd = sd(ht, na.rm = TRUE),
    sem = sd/sqrt(n())
  )

periodic_stat <- periodic |> 
  group_by(time) |> 
  summarize(
    sum = sum(ht, na.rm = TRUE),
    mean = mean(ht, na.rm = TRUE),
    sd = sd(ht, na.rm = TRUE),
    sem = sd/sqrt(n())
  )

interpolated_stat <- interpolated |> 
  group_by(time) |> 
  summarize(
    sum = sum(ht, na.rm = TRUE),
    mean = mean(ht, na.rm = TRUE),
    sd = sd(ht, na.rm = TRUE),
    sem = sd/sqrt(n())
  )

interpolated_midpt_stat <- interpolated_midpt |> 
  group_by(time) |> 
  summarize(
    sum = sum(ht, na.rm = TRUE),
    mean = mean(ht, na.rm = TRUE),
    sd = sd(ht, na.rm = TRUE),
    sem = sd/sqrt(n())
  )

p <- position_dodge(width = 0.4)

list(
  sim = sim_stat,
  periodic = periodic_stat,
  interpolated_midpt = interpolated_midpt_stat,
  interpolated = interpolated_stat
) |> 
  bind_rows(.id = "method") |> 
  ggplot(aes(x = time, y = mean, color = method)) +
  geom_point(position = p) +
  geom_line(position = p) +
  geom_ribbon(aes(ymin = mean - sem, ymax = mean + sem, fill = method), 
              color = NA, alpha = 0.2, position = p) +
  # scale_x_continuous(limits = c(40, 55)) +
  NULL

#tiny difference in time 75 due to mass mortality event.  Otherwise, two interpolation methods completely overlapping.
# both interpolation methods overestimate mean
# periodic here is I think like `method = "annual"` in rFIA and bounces around the true data.  Something like a moving average might improve this.
