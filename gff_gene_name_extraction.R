# this scripte is used to extract the gene information from the gff file
# the gff file is downloaded from PlasmoDB (https://plasmodb.org/common/
# downloads/Current_Release/PvivaxP01/). The file called PlasmoDB-68_PvivaxP01.gff
# rtrackelayer is a package from R Bioconductor

library("rtracklayer")
library("tidyverse")

gff <- import("PlasmoDB-68_PvivaxP01.gff")

gff_data <- as.data.frame(mcols(gff)) %>% 
  select("ID", "Name", "description", "ebi_biotype") %>% 
  filter(str_detect(ID, "^PVP01_\\d{7}$"))

write.table(gff_data, "PvivaxP01.gff_data.tsv",
            sep = "\t", quote = FALSE, row.names = FALSE)