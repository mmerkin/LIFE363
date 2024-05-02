library(tidyverse)

#Make a vector of all the sample names from vcf file

sample <- read_tsv("sample_names.txt")
samples <- colnames(sample)

#Add a column with only the number at the start of each sample name

num_ <- str_extract(samples, "^.*\\d+_")
num <- str_sub(num_, 1, -2) # Get rid of the underscore
all_butterflies <- tibble("name" = samples, "Individual" = num)

#Add Location column from spreadsheet to tibble; n.b columns with missing data removed

full_df <- read_csv("sample_sexes.txt") %>%
  select(Individual, Sex, Location)
all_butterflies <- left_join(all_butterflies, full_df, by = "Individual")

# Filtering
refiltered_butterflies <- filter(all_butterflies, ! Individual %in% c("9", "97", "3", "26", "55"))
filtered_butterflies <- filter(all_butterflies, ! Individual %in% c("9", "97", "3", "26", "55")) %>%
  filter(Sex == "Male")

# Create new files with the full names at each location
Blue <- filter(all_butterflies, Location == "Fagne")
Pink <- filter(all_butterflies, Location == "Famenne")
write(Blue$name, "Blue.txt")
write(Pink$name, "Pink.txt")

# Create new files with filtered data
Fagne_filtered <- filter(filtered_butterflies, Location == "Fagne")
Famenne_filtered <- filter(filtered_butterflies, Location == "Famenne")
write(Fagne_filtered$name, "Fagne_filtered.txt")
write(Famenne_filtered$name, "Famenne_filtered.txt")

Fagne_refiltered <- filter(refiltered_butterflies, Location == "Fagne")
Famenne_refiltered <- filter(refiltered_butterflies, Location == "Famenne")
write(Fagne_refiltered$name, "Fagne_refiltered.txt")
write(Famenne_refiltered$name, "Famenne_refiltered.txt")

