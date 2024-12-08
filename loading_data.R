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


