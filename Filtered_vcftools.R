# Setup

library(tidyverse)
library(qqman)

# Import data

fst_data <- read_tsv("Filtered_Fagne_vs_Famenne_FST_20kb.windowed.weir.fst")
FST <- filter(fst_data, str_detect(CHROM, "^O"))

#Add a column with each chromosome number

nums <- read_tsv("Chromosome_numbers.txt", col_names = FALSE) #read in number key
CHROM <- nums$X1[seq(1, length(nums$X1), 4)] #first column is chrom name
chrom_num <- nums$X1[seq(2, length(nums$X1) - 8, 4)] #second is number
chrom_num <- append(chrom_num, c(31, 32)) #replace Z and MT with 31 and 32
chrom_num <- as.numeric(chrom_num) #Convert from character to numeric
chrom_len <- nums$X1[seq(3, length(nums$X1), 4)]
chromosome_key <- tibble(CHROM, chrom_num, chrom_len)

FST <- left_join(FST, chromosome_key, by = "CHROM")

#Summary stats
summary(FST$MEAN_FST)
FST[which.max(FST$MEAN_FST),] # Highest is on 31
dim(filter(FST, chrom_num == 31))
print(count(FST, chrom_num), n = 31) 

# Is number of windows proportional to length?
window_data <- right_join(tibble(chrom_num, chrom_len = as.numeric(chrom_len)), count(FST, chrom_num),
                         by = "chrom_num")
ggplot(window_data, aes(x = chrom_len, y = n)) +
  geom_point() +
  theme_classic() +
  labs(x = "Chromosome length (Mb)", y = "Number of windows")
# Coverage of chromosome with windows: multiplications are conversions to bases
window_data$ratio <- (window_data$chrom_len * 1000000) / (window_data$n * 20000)
# Largely but not the Z chromosome (longest but only 444 windows)

# Plot the mean Fst for each 20kbp window on a specific chromosome
FST %>%
  mutate(WINDOW_POS = ((BIN_END-BIN_START)/2 + BIN_START) / 1000) %>%
  filter(chrom_num == 31) %>%
  ggplot(., aes(x = WINDOW_POS, y = MEAN_FST)) +
  geom_line() +
  labs(x = "Window position (kbp)", y = "Mean Fst", title = "Chromosome 31") +
  theme_classic()

# Make a Manhattan plot

FST$window <- paste("window", 1:nrow(FST)) # Create a column where each row has a unique id as a window
manhattan(FST, chr="chrom_num", bp = "BIN_START", p = "MEAN_FST", snp = "window",
          col = c("red", "blue"), logp=FALSE, ylab = "Windowed Fst", xlab = "Chromosome")
