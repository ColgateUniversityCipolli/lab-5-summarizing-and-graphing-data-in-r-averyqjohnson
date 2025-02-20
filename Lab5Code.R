##############################################################################
# Lab 5
# Avery Johnson
##############################################################################
library("tidyverse")

# get data
essentia_data_allentown <- read_csv("data/essentia.data.allentown.csv")
essentia_data <- read_csv("data/essentia.data.csv")

##############################################################################
# Step 1: write a function to determine whether Allentown is out of range,
# unusual, or within the range for each band
##############################################################################

# make sure approach works for overall_loudness first
check_allentown_range <- function(data, allentown_data, feature) {
  
  # extract the Allentown value for the given feature
  allentown_value <- allentown_data[[feature]]
  
  summary_data <- data |>
    group_by(artist) |>
    
    summarize(
      # calculate min and max
      min = min(get(feature), na.rm=T),
      max = max(get(feature), na.rm=T),
      
      # get quartiles
      Q1 = quantile(get(feature), 0.25, na.rm=T),
      Q3 = quantile(get(feature), 0.75, na.rm=T),
      # compute IQR
      IQR = Q3 - Q1,
      
      # compute fences
      LF = Q1 - (1.5 * IQR),
      UF = Q3 + (1.5 * IQR),
    ) |>
    
    #ungroup() |>
    
    # add Allentown comparison column
    mutate(
      out.of.range = (allentown_value < min | allentown_value > max),
      unusual = allentown_value < LF | allentown_value > UF,
      description = case_when(
        out.of.range ~ "Out of Range",
        unusual ~ "Outlying",
        TRUE ~ "Within Range",
      )
    )
  
  return(summary_data)
    
}

# test function for overall_loudness
#result <- check_allentown_range(essentia_data, essentia_data_allentown, "overall_loudness")

# Only select the columns you want to display
#result <- result |>
  #select(artist, min, LF, UF, max, out.of.range, unusual, description)

#print(result)

##############################################################################
# Step 2: apply this function to the data
##############################################################################

# select only numeric columns
numeric_columns <- names(essentia_data)[sapply(essentia_data, is.numeric)]

# apply the function to all numeric columns
# we can use lapply because numeric_columns is a vector

results <- lapply(numeric_columns, function(feature) {
  result <- check_allentown_range(essentia_data, essentia_data_allentown, feature)
  result$feature <- feature  # Add the feature name as a new column in the DF
  return(result)
})

combined_results <- bind_rows(results)
view(combined_results)

# detect the 20 features we want to investigate
selected_features <-  c("positivewords", "OtherP", "Perception", "conj", "chords_strength",
                        "average_loudness", "barkbands_flatness_db","barkbands_skewness", "dissonance",
                        "erbbands_flatness_db", "erbbands_skewness", "melbands_flatness_db",
                        "melbands_spread", "spectral_centroid", "spectral_complexity",
                        "spectral_energyband_middle_high", "spectral_entropy",
                        "spectral_kurtosis", "spectral_rolloff", "spectral_skewness")

# filter results to only indicate these features
filtered_results <- combined_results |>
  filter(feature %in% selected_features)

view(filtered_results)

# create Latex table
library(xtable)
latex_table <- xtable(filtered_results, caption="Summary of Features Identifying Influencing Band")

print(latex_table, include.rownames = FALSE, caption.placement = "top")
                      