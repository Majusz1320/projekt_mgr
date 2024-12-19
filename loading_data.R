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