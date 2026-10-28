rm(list = ls())
setwd("D:\\idiv2\\AP2_meta\\5.subproject20261015\\code")
getwd()

source("search_ris_wos.R")

sequencing_terms <- paste(
  "*sequenc* OR metabarcod* OR metagenom*",
  "OR Illumina OR MiSeq OR HiSeq OR NovaSeq OR PacBio OR Nanopore",
  "OR OTU OR OTUs OR zOTU OR zOTUs OR ASV OR ASVs",
  'OR "operational taxonomic unit*"'
)

seq_result <- search_ris_wos(
  ris_file = "asreview_relevant_SoilBEF_Synthesis_PaperScreening_26813.ris",
  query = paste0("TS=(", sequencing_terms, ")"),
  output_dir = "results_sequencing_12272",
  expected_n = 26813
)

# 显示原始记录数及命中数量
print(seq_result$summary)

# RIS自动保存至：
# results_sequencing_12272/search_results.ris





