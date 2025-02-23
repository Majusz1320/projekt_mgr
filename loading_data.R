#### LOADING DATA RNA ####

abrB1.2_table_load <- function(){
  abrB1.2_table <- read.csv("datasets/final_data/abrB1.2_table.csv")
  abrB1.2_table$data_name <- 'AbrB1_Nieta_2020'
  return(abrB1.2_table)
}


data_hupAS_RNAseq_load <- function(){
  data_hupAS_RNAseq <- read.csv("datasets/final_data/data_hupAS_RNAseq.csv")
  data_hupAS_RNAseq$data_name <- "hupAS_Strzalka_2024"
  return(data_hupAS_RNAseq)
}


RNAseq_Martyna_load <- function(){
  RNAseq_Martyna <- read.csv("datasets/final_data/RNAseq_Martyna.csv")
  RNAseq_Martyna$data_name <- 'SatKR_Gongerowska_2021'
  return(RNAseq_Martyna)
}

data_szafran2019_load <- function(){
  data_szafran2019 <- read.csv("datasets/final_data/data_szafran2019.csv")
  data_szafran2019$data_name <- 'TopA_Szafran_2019'
  return(data_szafran2019)
}

abrc3_load <- function(){
  abrc3 <- read.csv("datasets/final_data/abrc3.csv")
  abrc3$data_name <- "AbrC3_rico_2014"
  return(abrc3)
}

aor1_rna_load <- function(){
  aor1_rna <- read.csv("datasets/final_data/aor1_rna.csv")
  aor1_rna$data_name <- "Aor1_Antoraz_2017" 
  return(aor1_rna)
}


argR_2018_load <- function(){
  argR_2018 <- read.csv("datasets/final_data/argR_2018.csv")
  argR_2018$data_name <- "ArgR_Botas_2018"
  return(argR_2018)
}

bldD_scoe_load <- function(){
  bldD_scoe <- read.csv("datasets/final_data/bldD_scoe.csv")
  bldD_scoe$data_name <- "BldD_denHengst_2010"
  return(bldD_scoe)
}
data_bldC_sven_load <- function(){
  data_bldC_sven <- read.csv("datasets/final_data/data_bldC_sven.csv")
  data_bldC_sven$data_name <- "BldC_Bush_2018_sven"
  return(data_bldC_sven)
}
draRK_scoe_load <- function(){
  draRK_scoe <- read.csv("datasets/final_data/draRK_scoe.csv")
  draRK_scoe$data_name <- "DrarK_Yu_2014"
  return(draRK_scoe)
}
ECF42s_sven_load <- function(){
  ECF42s_sven <- read.csv("datasets/final_data/ECF42s_sven.csv")
  ECF42s_sven$data_name <- "ECF42_Liu_2018_sven"
  return(ECF42s_sven)
}
glnr_sven_load <- function(){
  glnr_sven <- read.csv("datasets/final_data/glnr_sven.csv")
  glnr_sven$data_name <- "GlnR_Pullan_2011"
  return(glnr_sven)
}

ohkA_scoe_load <- function(){
  ohkA_scoe <- read.csv("datasets/final_data/ohkA_scoe.csv")
  ohkA_scoe$data_name <- "OhkA_Lu_2011"
  return(ohkA_scoe)
}
osdR_2016_load <- function(){
  osdR_2016 <- read.csv("datasets/final_data/osdR_2016.csv")
  osdR_2016$data_name <- "OsdR_Urem_2016"
  return(osdR_2016)
}
sigR_load <- function(){
  sigR <- read.csv("datasets/final_data/sigR.csv")
  sigR$data_name <- "sigR_Kallifidas_2010"
  return(sigR)
}
soxr_genes_load <- function(){
  soxr_genes <- read.csv("datasets/final_data/soxr_genes.csv")
  soxr_genes$data_name <- "soxR_Naseer_2014"
  return(soxr_genes)
}
whiAH_scoe_load <- function(){
  whiAH_scoe <- read.csv("datasets/final_data/whiAH_scoe.csv")
  whiAH_scoe$data_name <- "whiAH_Salerno_2013"
  return(whiAH_scoe)
}
yague_2013_scoe_diff_load <- function(){
  yague_2013_scoe_diff <- read.csv("datasets/final_data/yague_2013_scoe_diff.csv")
  yague_2013_scoe_diff$data_name <- "SolidLiquidDiff_Yague_2013"
  return(yague_2013_scoe_diff)
}
yeong_2016_load <- function(){
  yeong_2016 <- read.csv("datasets/final_data/yeong_2016.csv")
  yeong_2016$data_name <- "growth_phases_Yeong_2016"
  return(yeong_2016)
}

NRRL_metab_RNAseq_sven_load <- function(){
  NRRL_metab_RNAseq_sven <- read.csv("datasets/final_data/NRRL_metab_RNAseq_sven.csv")
  NRRL_metab_RNAseq_sven$data_name <- "NRRL_Sekurova_2022_sven"
  return(NRRL_metab_RNAseq_sven)
}

#### LIST OF DATA LOAD FUNCTION RNA-SEQ WITH IN-APP DATA NAMES ####

data_in_app <- list(
  SatKR_Gongerowska_2021 = RNAseq_Martyna_load,
  TopA_Szafran_2019 = data_szafran2019_load,
  AbrB1_Nieta_2020 = abrB1.2_table_load,
  hupAS_Strzalka_2024 = data_hupAS_RNAseq_load,
  AbrC3_rico_2014 = abrc3_load,
  Aor1_Antoraz_2017 = aor1_rna_load,
  ArgR_Botas_2018 = argR_2018_load,
  BldD_denHengst_2010 = bldD_scoe_load,
  BldC_Bush_2018_sven = data_bldC_sven_load,
  DrarK_Yu_2014 = draRK_scoe_load,
  ECF42_Liu_2018_sven = ECF42s_sven_load,
  GlnR_Pullan_2011 = glnr_sven_load,
  OhkA_Lu_2011 = ohkA_scoe_load,
  OsdR_Urem_2016 = osdR_2016_load,
  sigR_Kallifidas_2010 = sigR_load,
  soxR_Naseer_2014 = soxr_genes_load,
  whiAH_Salerno_2013 = whiAH_scoe_load,
  SolidLiquidDiff_Yague_2013 = yague_2013_scoe_diff_load,
  growth_phases_Yeong_2016 = yeong_2016_load,
  NRRL_Sekurova_2022_sven = NRRL_metab_RNAseq_sven_load
)

#### LIST OF SVEN AND SCOE RNA-SEQ IN-APP DATA NAMES ####

data_sven <- c("BldC_Bush_2018_sven", "ECF42_Liu_2018_sven", 
               "GlnR_Pullan_2011", "NRRL_Sekurova_2022_sven")

data_scoe <- c("AbrB1_Nieta_2020", "hupAS_Strzalka_2024", "SatKR_Gongerowska_2021", 
               "TopA_Szafran_2019", "AbrC3_rico_2014", "Aor1_Antoraz_2017", 
               "ArgR_Botas_2018", "BldD_denHengst_2010", "DrarK_Yu_2014", 
               "OhkA_Lu_2011", "OsdR_Urem_2016", "sigR_Kallifidas_2010",
               "soxR_Naseer_2014", "whiAH_Salerno_2013", "SolidLiquidDiff_Yague_2013", 
               "growth_phases_Yeong_2016")

#### LOADING CHIP_SEQ ####

chipseq_smidova_load <- function(){
  chipseq_smirdova <- read.csv("datasets/final_data/data_chip_seq/chipseq_smirdova.csv")
  chipseq_smirdova$data_name <- "HrdB_chipseq_Smidova_2018"
  return(chipseq_smirdova)
}

hupAS_strzalka_chipseq_load <- function(){
  hupAS_strzalka_chipseq <- read.csv("datasets/final_data/data_chip_seq/hupAS_strzalka_chipseq.csv")
  hupAS_strzalka_chipseq$data_name <- "hupAS_chipseq_Strzalka_2024"
  return(hupAS_strzalka_chipseq)
}

hupS_chipseq_szafran_load <- function(){
  hupS_chipseq_szafran <- read.csv("datasets/final_data/data_chip_seq/hupS_chipseq_szafran.csv")
  hupS_chipseq_szafran$data_name <- "SMChupS_Szafran_2021"
  return(hupS_chipseq_szafran)
}



#### LIST OF DATA LOAD FUNCTION CHIP-SEQ WITH IN-APP DATA NAMES ####

data_load_chipseq <- list(
  HrdB_chipseq_Smidova_2018 = chipseq_smidova_load,
  hupAS_chipseq_Strzalka_2024 = hupAS_strzalka_chipseq_load,
  SMChupS_Szafran_2021 = hupS_chipseq_szafran_load
)

#### LIST OF SVEN AND SCOE CHIP-SEQ IN-APP DATA NAMES ####

data_chipseq <- c("HrdB_chipseq_Smidova_2018", "hupAS_chipseq_Strzalka_2024", "SMChupS_Szafran_2021")









#### TO TRZEBA ZMIENIĆ I WGL BO ZAPOMNIAŁEM O CO CHODZI Z TYM JAK MA TEN WYKRES KIEDYŚ WYGLĄDAĆ I Z CZEGO MA BYĆ ITP####
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


