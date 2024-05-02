# Setup 

library(tidyverse) 

# Import/inspect data 

FST <- read_tsv("filtered_fst.weir.fst") 
 
# Plot the Fst 
MP <- FST %>% 
  ggplot(., aes(x = POS / 1000, y = WEIR_AND_COCKERHAM_FST)) +
  geom_point() + 
  labs(x = "position (kb)", y = expression(italic("F")["ST"])) + 
  theme_classic()

MP +
  scale_x_continuous(n.breaks = 30)


# Plot with labels

MP_coloured <- FST %>% 
  ggplot(., aes(x = POS / 1000, y = WEIR_AND_COCKERHAM_FST)) +
  geom_rect(xmin = 1178.445, xmax = 1193.325, ymin = -Inf, ymax = Inf,
            fill = "yellow", alpha = 0.5) +
  geom_rect(xmin = 81.672, xmax = 91.993, ymin = -Inf, ymax = Inf,
            fill = "yellow", alpha = 0.5) +
  geom_point() + 
  labs(x = "position (kb)", y = expression(italic("F")["ST"])) + 
  theme_classic()
