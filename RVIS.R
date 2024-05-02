#Load packages

library(MASS)
library(tidyverse)
library(ggrepel)

#Read data and construct dataframe

data <- read_tsv("wPal_maf.txt")
head(data)
names(data)
filtered <- dplyr::select(data, "GeneName", 
                   "variants_effect_missense_variant",
                   "variants_effect_stop_gained",
                   "variants_effect_synonymous_variant")

regression_data <- mutate(filtered,
                          Y = variants_effect_missense_variant + variants_effect_stop_gained,
                          X = variants_effect_missense_variant +
                            variants_effect_stop_gained +
                            variants_effect_synonymous_variant)
regression_data <- filter(regression_data, GeneName != "wPal_01148", GeneName != "wPal_00101")

#Create a linear model and calculate the quantiles

model <- lm(Y ~ X, regression_data)
stud_residuals <- studres(model)
head(stud_residuals)
quants <- quantile(stud_residuals, c(0.01, 0.99))

#Assign the top and bottom 1% their own values for colouring

regression_data <- mutate(regression_data,
                          studres = stud_residuals)

regression_data$Quantile <- with(regression_data, factor(ifelse(studres < quants[1], 0,
                                                             ifelse(studres < quants[2], 1, 2))))

#Construct the graph

rvisplot <- ggplot(regression_data, aes(X, Y, colour = Quantile)) +
  geom_point() +
  scale_colour_manual(values = c("red", "black", "blue"), 
                      labels = c("<0.01", "0.01-0.99", ">0.99")) +
  geom_abline(slope = coef(model)[["X"]], 
              intercept = coef(model)[["(Intercept)"]]) +
  labs(y = "Number of non-synonymous SNPs", x = "Total number of SNPs") +
  theme_classic()

rvisplot

# Identify cif genes

gene_codes <- c("00097", "00099", "01146", "01147")
gene_list <- vector("character", length = length(gene_codes))
for (i in seq_along(gene_codes)){
  gene_list[[i]] = paste0("wPal_", gene_codes[[i]])
}

peak_genes <- filter(regression_data, GeneName %in% gene_list)

cif_genes <- tibble(gene = c("cifB pair 1", "cifA pair 2",
                             "cifA pair 1", "cifB pair 2"),
                    X = c(98, 83, 57, 109))

peak_genes <- dplyr::left_join(cif_genes, peak_genes)

sigma <- tibble(gene = paste0("\u03C3", "-", "70"),
                X = 181,
                GeneName = "wPal_00962",
                Y = 43,
                Quantile = "0")


# Add labels

rvisplot +
  geom_label_repel(data = peak_genes,
                   mapping = aes(label = gene),
                   size = 4,
                   nudge_y = 80 - peak_genes$Y,
                   box.padding = 0.5,
                   point.padding = 0.5,
                   force = 100,
                   segment.size = 0.2,
                   ylim = c(0, 210),
                   segment.color = "black",
                   direction = "x") +
  geom_label_repel(data = sigma,
                   mapping = aes(label = gene),
                   size = 4,
                   nudge_y = 20 - sigma$Y,
                   box.padding = 0.5,
                   point.padding = 0.5,
                   force = 100,
                   segment.size = 0.2,
                   segment.color = "black",
                   direction = "x")
