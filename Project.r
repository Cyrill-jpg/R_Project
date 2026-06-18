library(dplyr)
library(ggplot2)
library(plotly)

setwd(getwd())
getwd()
movies <- read.csv("/home/cyrill/Downloads/R_Project/movies.csv", fill = TRUE, header = TRUE) |>
  select(-description, -tagline) |>
  filter(!is.na(date), !is.na(rating), minute <= 240, date < 2024)

studios <- read.csv("/home/cyrill/Downloads/R_Project/studios.csv", fill = TRUE, header = TRUE)

releases <- read.csv("/home/cyrill/Downloads/R_Project/releases.csv", fill = TRUE, header = TRUE) |>
  filter(country == "USA") |>
  filter(type == "Theatrical" | type == "Digital") |>
  select(-date)|>
  distinct()|>
  rename(age_rating = rating)

head(movies)
summary(movies)

movies|>
  ggplot() + aes(x = date, y = minute) + 
  geom_boxplot(aes(group = date), outlier.alpha = 0.05, outlier.shape = 1) +
  theme(axis.text.x = element_text(angle = 90, size = 7)) +
  scale_x_continuous(breaks = seq(1872, 2024, by = 4), expand = c(0,1))

languages <- read.csv("/home/cyrill/Downloads/R_Project/languages.csv", fill = TRUE, header = TRUE)

genres <- read.csv("/home/cyrill/Downloads/R_Project/genres.csv", fill = TRUE, header = TRUE)

crew <- read.csv("/home/cyrill/Downloads/R_Project/crew.csv", fill = TRUE, header = TRUE)

actors <- read.csv("/home/cyrill/Downloads/R_Project/actors.csv", fill = TRUE, header = TRUE)

countries <- read.csv("/home/cyrill/Downloads/R_Project/countries.csv", fill = TRUE, header = TRUE)

head(movies, 20)


ggplot(movies) + aes(x = date) + geom_bar()

summary(movies)


movies_releases <- inner_join(movies, releases, by = "id")
head(movies_releases)

movies_releases_grouped <- movies_releases |>
  group_by(name, date) |>
  filter(date > 2012)|>
  filter(any(type == "Digital") && all(type != "Theatrical")) |>
  ungroup()|>
  inner_join(genres, by = "id")|>
  group_by(date, genre)|>
  summarise(n = n())
#head(movies_releases_grouped, 20)
p <- ggplot(movies_releases_grouped) + aes(x = date, y = n, group = genre) + geom_line(aes(color = genre))+ geom_point(size = 1, aes(color = genre))
ggplotly(p)

top10studios <- studios|>
  group_by(studio)|>
  summarise(n_movies = n())|>
  arrange(desc(n_movies))|>
  slice_head(n = 10)
head(top10studios, 10)


top10studios_rating <- inner_join(movies, studios, by = "id") |>
  group_by(studio)|>
  summarise(n = n(), mean_rating = mean(rating))|>
  filter(n > 4)|>
  arrange(desc(mean_rating))


head(top10studios_rating, 40)

top10studios_last10years <- inner_join(movies, studios, by = "id") |>
  filter(date >= 2014)|>
  group_by(studio)|>
  summarise(n_movies = n())|>
  arrange(desc(n_movies))|>
  slice_head(n = 10)

head(top10studios_last10years, 10)

movies_studios <- inner_join(movies, studios, by = "id") |>
  filter(studio %in% pull(top10studios, studio)) |>
  group_by(date, studio)|>
  summarise(mean_rating = mean(rating))

head(movies_studios, 10)

ggplot(movies_studios) + aes(x = date, y = mean_rating) + geom_line(aes(color = studio)) + facet_wrap(vars(studio), nrow = 5)

movies_studios <- inner_join(movies, studios, by = "id") |>
  group_by(date, studio)|>
  summarise(mean = mean(rating), n = n())|>
  ungroup()|>
  group_by(studio)|>
  summarise(n_movies = n()) |>
  filter(n_movies > 3) |>
  arrange(desc(n_movies))|>
  slice_head(n = 10)
head(movies_studios, 10)



movies_studios

ggplot(genres) + aes(x = genre) + geom_bar()

movies_genres <- inner_join(movies, genres, by = "id") |>
  filter(date > 2000)

movies_genres|>
  ggplot() + aes(x = date, y = rating, group = genre) + geom_point(aes(color = genre))+ facet_wrap(vars(genre), nrow = 5) + guides(color = FALSE)


movies_genres_grouped <- movies_genres |>
  group_by(date, genre) |>
  summarise(n = n())

head(movies_genres_grouped, 10)
ggplot(movies_genres_grouped) + aes(x = date, y = n, group = genre) + geom_line(aes(color = genre)) + facet_wrap(vars(genre), nrow = 5) + guides(color = FALSE)
