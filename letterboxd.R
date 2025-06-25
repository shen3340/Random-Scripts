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
                   add_headers(Authorization = api_token),
                   content_type("application/octet-stream"),
                   accept("application/json"))
  fromJSON(content(response, "text", encoding = "UTF-8"))
}

# Core function to extract movie info for one title
get_movie_info <- function(title) {
  queryString <- list(query = title)
  movie_data_raw <- get_tmdb_data("https://api.themoviedb.org/3/search/movie", query = queryString)
  movie_data <- as.data.frame(movie_data_raw$results)
  
  if (nrow(movie_data) == 0) {
    return(tibble(Name = title, runtime = NA, providers = "NOT FOUND", genres = NA))
  }
  
  # Normalize both input and results for better comparison
  clean_title <- tolower(trimws(title))
  movie_data <- movie_data %>%
    mutate(clean_api_title = tolower(trimws(title))) %>%
    mutate(release_date = as.Date(release_date)) %>%
    filter(!is.na(release_date), release_date <= Sys.Date()) %>%
    arrange(desc(popularity))
  
  # Try to find an exact title match
  exact_match <- movie_data %>% filter(clean_api_title == clean_title)
  
  if (nrow(exact_match) == 0) {
    return(tibble(Name = title, runtime = NA, providers = "NOT FOUND", genres = NA))
  }
  
  movie_id <- exact_match$id[1]
  
  # Provider info
  provider_url <- paste0("https://api.themoviedb.org/3/movie/", movie_id, "/watch/providers")
  provider_data <- get_tmdb_data(provider_url)
  provider_names <- NA
  if (!is.null(provider_data$results$US$flatrate)) {
    flatrate_df <- as.data.frame(provider_data$results$US$flatrate)
    provider_names <- paste(unique(flatrate_df$provider_name), collapse = ", ")
  }
  
  # Details
  detail_url <- paste0("https://api.themoviedb.org/3/movie/", movie_id)
  detail_data <- get_tmdb_data(detail_url)
  runtime <- detail_data$runtime
  
  # Safely extract genres
  genre_names <- NA
  if (!is.null(detail_data$genres) && length(detail_data$genres) > 0) {
    # Handle both list and data.frame cases
    if (is.data.frame(detail_data$genres)) {
      genre_names <- paste(detail_data$genres$name, collapse = ", ")
    } else if (is.list(detail_data$genres)) {
      genre_names <- paste(purrr::map_chr(detail_data$genres, function(g) {
        if (!is.null(g$name)) g$name else NA_character_
      }), collapse = ", ")
    }
  }
  
  
  tibble(
    Name = title,
    runtime = runtime,
    providers = provider_names,
    genres = genre_names
  )
}

# Load watchlist and fetch movie info
watchlist <- read_csv("watchlist.csv") |> 
  select(Name)

movie_list <- watchlist$Name %>%
  future_map_dfr(get_movie_info, .progress = TRUE) |> 
  filter(!is.na(providers), providers != "NOT FOUND") %>%
  group_by(Name, runtime) %>%
  summarise(
    providers = paste(unique(unlist(strsplit(providers, ", "))), collapse = ", "),
    genres = paste(unique(unlist(strsplit(genres, ", "))), collapse = ", "),
    .groups = "drop"
  )

# View final table
View(movie_list)