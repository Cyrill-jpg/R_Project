library(dplyr)
library(ggplot2)
library(plotly)
library(knitr)

imdb <- read.csv(file = "tmdb_6000_movie_dataset.csv", header = TRUE) |>
  select(budget, original_title, release_date, revenue, runtime) |>
  filter(budget > 100, revenue > 100)|>
  mutate(profitability = revenue/budget, year = as.numeric(format(as.Date(release_date, format="%Y-%m-%d"),"%Y"))) |>
  rename(title = original_title)

letterboxd <- read.csv(file = "Movie_Data_File.csv", header = TRUE) |>
  select(Film_title, Average_rating, Watches,List_appearances,Likes,R_0.5,R_1,R_1.5,R_2,R_2.5,R_3,R_3.5,R_4,R_4.5,R_5,Total_ratings) |>
  rename(title = Film_title)

movies <- imdb |> left_join(letterboxd, by = "title")

head(letterboxd)
head(imdb)
summary(imdb)
head(movies)
summary(movies)
