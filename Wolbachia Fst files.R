library(tidyverse)

# Make a vector of all the sample names from vcf file

sample <- read.table("wolbach_sample_names.txt")
samples <- sample$V1

# Add a column with only the number at the start of each sample name

num_ <- str_extract(samples, "^.*\\d+_")
num <- str_sub(num_, 1, -2) # Get rid of the underscore
all_wolbachia <- tibble("name" = samples, "Individual" = num)

# Add Location column from spreadsheet to tibble; n.b columns with missing data removed

full_df <- read_csv("sample_sexes.txt")
filtered_df <- select(full_df, Individual, Sex, `Wolbachia Tree`)
all_wolbachia <- left_join(all_wolbachia, filtered_df, by = "Individual")

# Create new files with the full names at each location
blue_wolbachia <- filter(all_wolbachia, `Wolbachia Tree` == "Fagne")
pink_wolbachia <- filter(all_wolbachia, `Wolbachia Tree` == "Famenne main branch")
purple_wolbachia <- filter(all_wolbachia, `Wolbachia Tree` == "Famenne branch 2")
blue_basal_wolbachia <- filter(all_wolbachia, Individual %in% c("20", "UK6"))

write(blue_wolbachia$name, "blue_wolbachia.txt")
write(pink_wolbachia$name, "pink_wolbachia.txt")
write(purple_wolbachia$name, "purple_wolbachia.txt")
write(blue_basal_wolbachia$name, "blue_basal_wolbachia.txt")
