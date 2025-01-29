#### LOADING DATA RNA ####

abrB1.2_table_load <- function(){
  abrB1.2_table4 <- read.csv("datasets/final_data/abrB1.2_table4.csv")
  abrB1.2_table4$add_variable <- "abrB1.2_table4"
  abrB1.2_table5 <- read.csv("datasets/final_data/abrB1.2_table5.csv")
  abrB1.2_table5$add_variable <- "abrB1.2_table5"
  
  
  abrB1.2_table <- rbind(abrB1.2_table4, abrB1.2_table5)
  abrB1.2_table$data_name <- "abrB1.2_table"
  return(abrB1.2_table)
}


data_hupAS_RNAseq_load <- function(){
  data_hupAS_RNAseq <- read.csv("datasets/final_data/data_hupAS_RNAseq.csv")
  data_hupAS_RNAseq$data_name <- "data_hupAS_RNAseq"
  return(data_hupAS_RNAseq)
}


RNAseq_Martyna_load <- function(){
  RNAseq_Martyna <- read.csv("datasets/final_data/RNAseq_Martyna.csv")
  return(RNAseq_Martyna)
}

data_szafran2019_load <- function(){
  data_szafran2019 <- read.csv("datasets/final_data/data_szafran2019.csv")
  return(data_szafran2019)
}

abrc3_load <- function(){
  abrc3 <- read.csv("datasets/final_data/abrc3.csv")
  abrc3$data_name <- "abrc3"
  return(abrc3)
}

aor1_rna_load <- function(){
  aor1_rna <- read.csv("datasets/final_data/aor1_rna.csv")
  aor1_rna$data_name <- "aor1_rna" 
  return(aor1_rna)
}


argR_2018_load <- function(){
  argR_2018 <- read.csv("datasets/final_data/argR_2018.csv")
  argR_2018$data_name <- "argR_2018"
  return(argR_2018)
}

bldD_scoe_load <- function(){
  bldD_scoe <- read.csv("datasets/final_data/bldD_scoe.csv")
  bldD_scoe$data_name <- "bldD_scoe"
  return(bldD_scoe)
}
data_bldC_sven_load <- function(){
  data_bldC_sven <- read.csv("datasets/final_data/data_bldC_sven.csv")
  data_bldC_sven$data_name <- "data_bldC_sven"
  return(data_bldC_sven)
}
draRK_scoe_load <- function(){
  draRK_scoe <- read.csv("datasets/final_data/draRK_scoe.csv")
  draRK_scoe$data_name <- "draRK_scoe"
  return(draRK_scoe)
}
ECF42s_sven_load <- function(){
  ECF42s_sven <- read.csv("datasets/final_data/ECF42s_sven.csv")
  ECF42s_sven$data_name <- "ECF42s_sven"
  return(ECF42s_sven)
}
glnr_sven_load <- function(){
  glnr_sven <- read.csv("datasets/final_data/glnr_sven.csv")
  glnr_sven$data_name <- "glnr_sven"
  return(glnr_sven)
}
hups_rnaseq_Strzalka_sven_load <- function(){
  hups_rnaseq_Strzalka_sven <- read.csv("datasets/final_data/hups_rnaseq_Strzalka_sven.csv")
  hups_rnaseq_Strzalka_sven$data_name <- "hups_rnaseq_Strzalka_sven"
  return(hups_rnaseq_Strzalka_sven)
}
ohkA_scoe_load <- function(){
  ohkA_scoe <- read.csv("datasets/final_data/ohkA_scoe.csv")
  ohkA_scoe$data_name <- "ohkA_scoe"
  return(ohkA_scoe)
}
osdR_2016_load <- function(){
  osdR_2016 <- read.csv("datasets/final_data/osdR_2016.csv")
  osdR_2016$data_name <- "osdR_2016"
  return(osdR_2016)
}
sigR_load <- function(){
  sigR <- read.csv("datasets/final_data/sigR.csv")
  sigR$data_name <- "sigR"
  return(sigR)
}
soxr_genes_load <- function(){
  soxr_genes <- read.csv("datasets/final_data/soxr_genes.csv")
  soxr_genes$data_name <- "soxr_genes"
  return(soxr_genes)
}
whiAH_scoe_load <- function(){
  whiAH_scoe <- read.csv("datasets/final_data/whiAH_scoe.csv")
  whiAH_scoe$data_name <- "whiAH_scoe"
  return(whiAH_scoe)
}
yague_2013_scoe_diff_load <- function(){
  yague_2013_scoe_diff <- read.csv("datasets/final_data/yague_2013_scoe_diff.csv")
  yague_2013_scoe_diff$data_name <- "yague_2013_scoe_diff"
  return(yague_2013_scoe_diff)
}
yeong_2016_load <- function(){
  yeong_2016 <- read.csv("datasets/final_data/yeong_2016.csv")
  yeong_2016$data_name <- "yeong_2016"
  return(yeong_2016)
}

NRRL_metab_RNAseq_sven_load <- function(){
  NRRL_metab_RNAseq_sven <- read.csv("datasets/final_data/NRRL_metab_RNAseq_sven.csv")
  NRRL_metab_RNAseq_sven$data_name <- "NRRL_metab_RNAseq_sven"
  return(NRRL_metab_RNAseq_sven)
}

#### LOADING DATA INTIME ####

abrc3_intime_load <- function(){
  abrc3 <- read.csv("datasets/final_data/data_intime/abrc3_intime.csv")
  return(abrc3)
}

argR_2018_intime_load <- function(){
  argR_2018 <- read.csv("datasets/final_data/data_intime/argR_2018_intime.csv")
  return(argR_2018)
}

draRK_scoe_intime_load <- function(){
  draRK_scoe <- read.csv("datasets/final_data/data_intime/draRK_scoe_intime.csv")
  return(draRK_scoe)
}

ohkA_scoe_intime_load <- function(){
  ohkA_scoe <- read.csv("datasets/final_data/data_intime/ohkA_scoe_intime.csv")
  return(ohkA_scoe)
}

osdR_2016_intime_load <- function(){
  osdR_2016 <- read.csv("datasets/final_data/data_intime/osdR_2016_intime.csv")
  return(osdR_2016)
}

whiAH_scoe_intime_load <- function(){
  whiAH_scoe <- read.csv("datasets/final_data/data_intime/whiAH_scoe_intime.csv")
  return(whiAH_scoe)
}

glnr_sven_intime_load <- function(){
  glnr_sven <- read.csv("datasets/final_data/data_intime/glnr_sven_intime.csv")
  return(glnr_sven)
}

yague_2013_scoe_diff_intime_load <- function(){
  yague_2013_scoe_diff <- read.csv("datasets/final_data/data_intime/yague_2013_scoe_diff_intime.csv")
  return(yague_2013_scoe_diff)
}

NRRL_metab_RNAseq_sven_intime_load <- function(){
  NRRL_metab_RNAseq_sven <- read.csv("datasets/final_data/data_intime/NRRL_metab_RNAseq_sven_intime.csv")
  return(NRRL_metab_RNAseq_sven)
}