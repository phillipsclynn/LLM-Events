
##### Description: Takes the cleaned top 10 EPP events LLM db and generates counts of the number 
#     of shocks in the shocks db that occur within the same country and specified timeframe of the
#     EPP occurrence

# install libraries
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse", repos = "https://cloud.r-project.org")
}

if (!requireNamespace("here", quietly = TRUE)) {
  install.packages("here", repos = "https://cloud.r-project.org")
}

library(tidyverse)
library(here)

# read in raw data

shocks <- read.csv(here("data/processed/shocks_cleaned.csv"))
llm_events <- read.csv(here("data/processed/llm_events_cleaned.csv"))


# define variables

matches_df <- llm_events
shock_cats <- names(shocks[5:length(names(shocks))])

# define a function to look up the # of shocks for a given shock category/country/year combination
look_up_count <- function(i, shocks_db, matches_df, match_type, year_col, shock_type_col, years_before, years_after) {
  
  # if an exact match is needed
  if (str_detect(match_type, "start")  | str_detect(match_type, "end")) {
    
    row_num <- which(shocks_db$UN_en == matches_df$Country[i] & shocks_db$Year == matches_df[i, year_col])
    
    # exact match won't work for llm events after 2013, as the shocks db's coverage ends in 2013
    if (identical(row_num, integer(0))) {
      return(NA)
    }
    else {
      return(shocks_db[row_num, shock_type_col])
    }
  }
  
  # match within a range
  else {
    row_nums <- which(shocks_db$UN_en == matches_df$Country[i] & 
                        (shocks_db$Year >= (as.numeric(matches_df$Start_Year[i]) - years_before) & 
                           shocks_db$Year <= (as.numeric(matches_df$End_Year[i]) + years_after)))
    
    # sum the number of shocks across all eligible years
    if (length(row_nums) > 1) {
      result <- sum(shocks_db[row_nums, shock_type_col], na.rm = TRUE)
      return(result)
    }
    
    else if (identical(row_nums, integer(0))) {
      return(NA)
    }
    
    else {
      return(shocks_db[row_nums, shock_type_col])
    }
  
  }
  
}

# define a function to create the match count columns
create_match_cols <- function(x, matches_df, shocks_db, match_type, year_col, years_before, years_after) {
  
  match_col_name <- paste(match_type, shock_cats[x], sep="_")

  shock_type_col <- which( colnames(shocks_db) == shock_cats[x] )

  
  matches_df_add <- matches_df %>%
    mutate("{match_col_name}" := sapply(X = seq_len(nrow(matches_df)), 
                                        FUN=look_up_count, 
                                        shocks_db=shocks_db,
                                        matches_df=matches_df,
                                        match_type=match_type,
                                        year_col=year_col, 
                                        shock_type_col=shock_type_col, 
                                        years_before=years_before,
                                        years_after=years_after) 
    )
  
  return(matches_df_add)
  
}


# function that calculates matches between the llm events db and shocks db (based on country and year)
calc_match <- function (matches_df, shocks_db, year_col_name, match_type, years_before, years_after) {
  
  # get the column # for the relevant year column (start_year or end_year if it is an exact match)
  if (!is.na(year_col_name)) {
    year_col <- which( colnames(matches_df) == year_col_name )
  }

  # for each shock category, fill in the number of matches
  
  for (curr_cat in 1:length(shock_cats)) {
    matches_df <- create_match_cols(curr_cat, matches_df, shocks_db, match_type, year_col, years_before, years_after)
  }
  
  return(matches_df)
}

# Call functions

# calculate matches based on the start year and end year

matches_df <- calc_match(matches_df, shocks, "Start_Year", "matches_start", 0, 0)
matches_df <- calc_match(matches_df, shocks, "End_Year", "matches_end", 0, 0)

# calculate matches between start and end year (inclusive)

matches_df <- calc_match(matches_df, shocks, NA, "matches_range", 0, 0)

# calculate matches 5 and 10 years before the start year, through until the end year (inclusive)

matches_df <- calc_match(matches_df, shocks, NA, "matches_5_before", 5, 0)
matches_df <- calc_match(matches_df, shocks, NA, "matches_10_before", 10, 0)

# calculate matches between the start year, through until 5 and 10 years after the end year (inclusive)
matches_df <- calc_match(matches_df, shocks, NA, "matches_5_after", 0, 5)
matches_df <- calc_match(matches_df, shocks, NA, "matches_10_after", 0, 10)


# write to output folder
write.csv(matches_df, here("outputs/merged_dbs.csv"))