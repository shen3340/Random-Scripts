library(readr)
library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
readRenviron(".Renviron")
# Load watchlist CSV
watchlist <- read_csv("watchlist.csv") |> 
  select(Name)
# Define the API token
api_token <- Sys.getenv("MY_API_KEY")
Sys.getenv()

# Initialize a vector to store provider information
providers_list <- vector("list", length = nrow(watchlist))

# Loop through each movie in the watchlist to fetch movie ID and providers
for (i in 1:nrow(watchlist)) {
  
  # Step 1: Fetch movie ID from the watchlist (same as your original code)
  queryString <- list(query = watchlist$Name[i])  # Movie title from the watchlist
  
  # API request to get movie ID
  movie_response <- VERB("GET", 
                         "https://api.themoviedb.org/3/search/movie", 
                         query = queryString, 
                         add_headers('Authorization' = api_token), 
                         content_type("application/octet-stream"), 
                         accept("application/json"))
  
  # Parse the JSON response for the movie ID
  movie_data <- fromJSON(content(movie_response, "text", encoding = "UTF-8"))
  movie_data <- as.data.frame(movie_data$results)
  
  # Step 2: Get movie ID (only if results exist)
  if (nrow(movie_data) > 0) {
    movie_id <- movie_data$id[1]  # Fetch the ID of the first movie in results
    
    # Step 3: Fetch providers for the movie using the movie ID
    provider_url <- paste0("https://api.themoviedb.org/3/movie/", movie_id, "/watch/providers")
    
    provider_response <- VERB("GET", 
                              provider_url, 
                              add_headers('Authorization' = api_token), 
                              content_type("application/octet-stream"),
                              accept("application/json"))
    
    # Parse the provider JSON response
    provider_data <- fromJSON(content(provider_response, "text", encoding = "UTF-8"))
    
    # Filter for US providers (if available)
    if (!is.null(provider_data$results$US)) {
      us_data <- provider_data$results$US  # Extract US providers
      
      # Extract flatrate provider information
      if (!is.null(us_data$flatrate)) {
        flatrate_data <- us_data$flatrate
        
        # Convert to a data frame and store the provider names
        flatrate_df <- as.data.frame(flatrate_data) %>%
          select(provider_name)
        
        # Store the provider information in the list
        providers_list[[i]] <- flatrate_df
      } else {
        providers_list[[i]] <- NA  # No providers found
      }
    } else {
      providers_list[[i]] <- NA  # No providers for US
    }
    
  } else {
    providers_list[[i]] <- NA  # No movie found for the given title
  }
  
}

# Add the provider information to the watchlist dataframe
watchlist$providers <- providers_list

watchlist <- watchlist |> 
  filter(providers != "NA") |> 
  unnest_longer(providers)

View(watchlist)
