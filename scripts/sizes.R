library(dplyr)

all_files <- list.files(here::here('data'),
                        recursive = TRUE, 
                        full.names = TRUE) |>
  file.info()

all_files <- all_files |>
  mutate(path = row.names(all_files)) 

all_files <- all_files |>
  mutate(path = gsub(here::here("data"), "", path)) |>
  group_by_all() |>
  mutate(extension = unlist(strsplit(split = "[.]", path))[length(unlist(strsplit(split = "[.]", path)))]) |>
  mutate(state = unlist(strsplit(split = "[.]", path))[length(unlist(strsplit(split = "[.]", path))) - 1]) |>
  mutate(state = substr(state, nchar(state) - 1, nchar(state))) |>
  mutate(state = ifelse(grepl("rawdat", path),
                        substr(path, nchar(path) - 10, nchar(path) - 9),
                        state)) |>
  mutate(state = ifelse(grepl("/parquet/tree_table", path),
                        substr(path, nchar("/parquet/tree_table") + 2, 
                               nchar("/parquet/tree_table") + 3),
                        state)) |>
  ungroup()

all_file_sizes <- all_files |>
  group_by(state, extension) |>
  summarize(total_size = sum(size)) |>
  ungroup() |>
  mutate(total_size_mb = total_size / 1E6) |>
  filter(state != "et")

library(ggplot2)

ggplot(all_file_sizes, aes(state, total_size_mb, color = extension)) +
  geom_point() +
  theme(legend.position = "bottom")

expected_actions_artifacts <- all_file_sizes |>
  filter(extension == "parquet")

sum(expected_actions_artifacts$total_size_mb)

