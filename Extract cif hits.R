library(tidyverse)

header = c("query_id", "query_length", "query_sequence", "subject_id", "subject_sequence", "subject_titles", "percent_identical", "evalue", "bitscore")
cif <- read_tsv("47_cif_hits.txt", col_names = header)
head(cif)

cifA_V <- filter(cif, subject_id == "cifA_V", query_length == 151)

numbers = seq_along(cifA_V$query_id)

for (i in numbers){
  x = cifA_V$query_id[[i]]
  y = cifA_V$query_sequence[[i]]
  writeLines(paste0(">", x, "\n", y, "\n"))
}
