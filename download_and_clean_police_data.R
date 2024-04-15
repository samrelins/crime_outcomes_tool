library(tidyverse)

load_police_data <- function(directory) {
    # Get a list of all the year directories
    year_dirs <- list.dirs(directory, recursive = FALSE)
    
    # Filter out the "checkpoints" directory
    year_dirs <- year_dirs[!grepl("checkpoints", year_dirs)]
    
    # Initialize empty lists to store the dataframes
    outcomes_list <- list()
    street_list <- list()
    
    # Iterate over each year directory
    for (year_dir in year_dirs) {
        # Get a list of all the CSV files in the year directory
        csv_files <- list.files(year_dir, pattern = "*.csv", full.names = TRUE)
        
        # Iterate over each CSV file
        for (csv_file in csv_files) {
            # Load the CSV file into a dataframe
            df <- read_csv(csv_file, show_col_types = FALSE)
            
            # Check if it's an "outcomes" or "street" file based on the filename
            if (grepl("outcomes", csv_file)) {
                outcomes_list[[length(outcomes_list) + 1]] <- df
            } else if (grepl("street", csv_file)) {
                street_list[[length(street_list) + 1]] <- df
            }
        }
    }
    
    # Concatenate all the "outcomes" dataframes
    outcomes_df <- bind_rows(outcomes_list)
    
    # Concatenate all the "street" dataframes
    street_df <- bind_rows(street_list)
    
    # Rename the columns of the "outcomes" dataframe
    outcomes_df <- outcomes_df %>%
        rename_with(~ tolower(gsub(" ", "_", .x)))
    
    # Rename the columns of the "street" dataframe
    street_df <- street_df %>%
        rename_with(~ tolower(gsub(" ", "_", .x)))
    
    # Return a list containing the concatenated dataframes
    return(list(outcomes = outcomes_df, street = street_df))
}

download_and_process_police_data <- function(generate_new=FALSE) {
    
    if (dir.exists("data") & !generate_new) {
        data_dirs <- list.dirs(path = "data", full.names = TRUE, recursive = FALSE)
        
        # Check if data_dirs is not empty
        if (length(data_dirs) > 0) {
            dates <- sapply(strsplit(basename(data_dirs), "_"), `[`, 1)
            date_objects <- as.Date(dates, format = "%Y-%m-%d")
            
            data_dirs <- data_dirs[order(date_objects, decreasing = TRUE)]
            
            for (dir in data_dirs) {
                checkpoint_path <- file.path(dir, "checkpoints")
                file_path <- file.path(checkpoint_path, "app_data.csv")
                if (file.exists(file_path)) {
                    print("Already downloaded and cleaned. Reading cleaned data csv")
                    return(read_csv(file_path))
                }
            }
        } 
    }
    
    # Get today's date
    today <- Sys.Date()
    
    # Create the "data" directory if it doesn't exist
    if (!dir.exists("data")) {
        dir.create("data")
    }
    
    # Create a directory for today's data inside the "data" directory
    data_dir <- file.path("data", paste0(today, "_police_data"))
    if (!dir.exists(data_dir)) {
        dir.create(data_dir)
    } else {
        stop("Error: Download directory already exists. Either set generate_new=False to use the existing data, or delete the existing directory to re-generate")
    }
    
    print("Beginning process - Note this can take over an hour")
    print("Step 1 of 7: Downloading data")
    # Download the zip file to the "data" directory 
    options(timeout=2400)
    download.file("https://data.police.uk/data/archive/latest.zip", 
                  file.path(data_dir, "latest.zip"))
    
    # Extract the contents of the zip file in the "data" directory
    print("Step 2 of 7: Unzipping data")
    unzip(file.path(data_dir, "latest.zip"), exdir = data_dir)
    
    # Delete the zip file
    file.remove(file.path(data_dir, "latest.zip"))
    
    print("Step 3 of 7: Concatenating individual Street/Outcome files")
    police_data <- load_police_data(data_dir)
    
    checkpoint_path <- file.path(data_dir, "checkpoints")
    dir.create(checkpoint_path)
    write_csv(police_data$street, file.path(checkpoint_path, "street.csv"))
    write_csv(police_data$outcomes, file.path(checkpoint_path, "outcomes.csv"))
    
    combine_outcome_categories <- function(data, column_name) {
        data %>%
            mutate({{ column_name }} := case_when(
                .data[[column_name]] %in% c(
                    "Formal action is not in the public interest",
                    "Further investigation is not in the public interest",
                    "Further action is not in the public interest"
                ) ~ "Further action not in the public interest",
                TRUE ~ .data[[column_name]]
            )) %>% 
            rename(outcome = {{ column_name }}) 
    }
    
    drop_cols <- c("falls_within", "longitude", "latitude", "location", "lsoa_code", "lsoa_name")
    
    print("Step 4 of 7: Cleaning Street Data")
    na_outcomes = c("Status update unavailable", "Court result unavailable", 
                    "Awaiting court outcome", "Under investigation")
    street_data <- police_data$street %>%
        select(-all_of(drop_cols), -context) %>%
        filter(!is.na(crime_id), 
               crime_type != "Anti-social behaviour") %>%
        mutate(last_outcome_category = ifelse(last_outcome_category %in% na_outcomes,  
                                              NA, last_outcome_category)) %>%
        combine_outcome_categories("last_outcome_category") %>%
        distinct()
    
    
    write_csv(street_data, file.path(checkpoint_path, "street_cleaned.csv"))
    
    print("Step 5 of 7: Cleaning Outcomes Data")
    outcomes_data <- police_data$outcomes %>%
        select(-all_of(drop_cols)) %>%
        filter(!is.na(crime_id)) %>%
        combine_outcome_categories("outcome_type") %>%
        distinct()
    
    write_csv(outcomes_data, file.path(checkpoint_path, "outcomes_cleaned.csv"))
    
    # Combine street_data and outcomes_data
    print("Step 6 of 7: Combining outcomes and cleaning")
    combined_data <- bind_rows(street_data, outcomes_data)
    
    crime_outcomes <- combined_data %>%
        filter(!is.na(outcome)) %>%
        group_by(crime_id) %>%
        summarise(n_outcomes = n_distinct(outcome),
                  outcome = first(outcome),
                  .groups = "drop") %>%
        filter(n_outcomes == 1) %>%
        select(-n_outcomes)
    
    write_csv(crime_outcomes, file.path(checkpoint_path, "crime_outcomes_cleaned.csv"))
    
    print("Step 7 of 7: Combining outcomes with other crime data")
    crime_details <- combined_data %>%
        arrange(month) %>%
        group_by(crime_id) %>%
        summarise(
            month = first(month),
            crime_type = last(crime_type[!is.na(crime_type)]),
            reported_by = last(reported_by[!is.na(reported_by)])
        )
    
    write_csv(crime_details, file.path(checkpoint_path, "crime_details.csv"))
    
    app_data <- crime_outcomes %>%
        left_join(crime_details, by = "crime_id") %>%
        mutate(crime_type = replace_na(crime_type, "Unknown")) %>%
        group_by(reported_by, month, outcome, crime_type) %>% 
        summarise(count = n(), .groups = "drop")
    
    write_csv(cleaned_outcomes_data, file.path(checkpoint_path, "app_data.csv"))
    return(cleaned_outcomes_data)
}


if (!interactive()) {
    process_police_data()
}