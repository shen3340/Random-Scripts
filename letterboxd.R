# packages and API key setup
# SEE IF YOU CAN ADD WHEN STUFF EXPIRES ON THEIR CURRENT STREAMING PLATFORM
library(readr)
library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(purrr)
library(furrr)
library(future)
readRenviron(".Renviron")
api_token <- Sys.getenv("MY_API_KEY")
plan(multisession, workers = 4) 

# Helper function to make TMDb API calls with authorization
get_tmdb_data <- function(url, query = NULL) {
  response <- VERB("GET",
                   url,
                   query = query,
                   add_headers(Authorization = api_token))
  fromJSON(content(response, "text", encoding = "UTF-8"))
}

# Helper function to collapse strings
clean_collapse <- function(x) {
  paste(unique(unlist(strsplit(x, ", "))), collapse = ", ")
}

# Core function to extract movie info for one title
get_movie_info <- function(title) {
  movie_data <- as.data.frame(get_tmdb_data("https://api.themoviedb.org/3/search/movie", query = list(query = title))$results)
  if (is_empty(movie_data)) {
    return(tibble(Name = title, runtime = NA, providers = "NOT FOUND", genres = NA))
  }
  
  # Only adds exact title matches to dataset
  clean_title <- tolower(trimws(title))
  movie_data <- movie_data |>
    filter(!is.na(release_date), as.Date(release_date) <= Sys.Date()) |>
    arrange(desc(popularity))
  exact_match <- movie_data |> filter(tolower(trimws(title)) == clean_title)
  movie_id <- exact_match$id[1] %||% NA_integer_
  if (is.na(movie_id)) {
    return(tibble(Name = title, runtime = NA, providers = "NOT FOUND", genres = NA))
  }
  
  # Provider and detail info
  base_url <- "https://api.themoviedb.org/3/movie/"
  provider_data <- get_tmdb_data(paste0(base_url, movie_id, "/watch/providers"))
  provider_names <- if (!is.null(provider_data$results$US$flatrate)) {
    paste(unique(as.data.frame(provider_data$results$US$flatrate)$provider_name), collapse = ", ")
  } else {
    NA_character_
  }
  
  # Safely extract genres
  detail_data <- get_tmdb_data(paste0(base_url, movie_id))
  genre_names <- if (!is.null(detail_data$genres) && length(detail_data$genres) > 0) {
    if (is.data.frame(detail_data$genres)) {
      paste(detail_data$genres$name, collapse = ", ")
    } else {
      paste(purrr::map_chr(detail_data$genres, ~ .x$name %||% NA_character_), collapse = ", ")
    }
  } else {
    NA_character_
  }
  
  tibble(
    Name = title,
    runtime = detail_data$runtime,
    providers = provider_names,
    genres = genre_names
  )
}

# Load watchlist and fetch movie info
movie_list <- read_csv("watchlist.csv", show_col_types = FALSE) |>
  pull(Name) |> 
  future_map_dfr(get_movie_info, .progress = TRUE) |> 
  filter(!is.na(providers), providers != "NOT FOUND")  |> 
  group_by(Name, runtime) |>
  summarise(
    providers = clean_collapse(providers),
    genres = clean_collapse(genres),
    .groups = "drop")

# View final table
View(movie_list)