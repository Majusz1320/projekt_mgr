library(shiny)
library(shinythemes)
library(ggplot2)
library(gggenes)
library(dplyr)
library(pheatmap)
library(patchwork)
library(tidyHeatmap)
library(BiocManager)
library(plotly)
library(bslib)
source("loading_data.R")

#### UI ####
abrB1.2_table4 <- read.csv("datasets/final_data/abrB1.2_table4.csv")
abrB1.2_table5 <- read.csv("datasets/final_data/abrB1.2_table5.csv")
data_hupAS_RNAseq <- read.csv("datasets/final_data/data_hupAS_RNAseq.csv")

loading_data_function()
data_loaded <- c("abrB1.2_table4", "abrB1.2_table5", "data_hupAS_RNAseq")
options_app <- c("genomeplot", "RNAplot", "CHIPplot", "logFCplot", "pvalueVulcano")
genelist <- read.csv("datasets/genes_scoelicolor.txt", sep = '')
gene_list_database <- c("all", genelist$gene)


ui <- fluidPage(
  theme = shinytheme("yeti"),
  navbarPage("Strepto NGS analysis",
             tabPanel("RNA-seq & ChIP-seq visualization",
                      sidebarLayout(
                        sidebarPanel(
                          tabsetPanel(type = "pills",
                                      tabPanel("Selection",
                                               selectInput("select_rna", label = h3("Choose from database"), 
                                                           choices = c("RNA_seq_data", "HupAmacs")),
                                               selectInput("select_chip", label = h3("Choose from database"), 
                                                           choices = c("CHIP_seq_data", "HupAmacs")),
                                               selectInput('options', 'Show/hide visualizations', options_app,
                                                           multiple=TRUE, selectize=TRUE,
                                                           selected = c('RNAplot')),
                                               selectInput("select_gene", label = h3("Choose gene from list"), 
                                                           choices = gene_list_database),
                                               selectInput("select_dataset", label = h3("Choose dataset from list"), 
                                                           choices = data_loaded),
                                      ),
                                      tabPanel("Plot settings",
                                               numericInput("lower_value", label = h3("Minimal value of plot"), value = 4000000, step = 10000),
                                               numericInput("higher_value", label = h3("Maximum value of plot"), value = 4200000, step = 10000),
                                               numericInput("higher_logFC", label = h3("Maximum value of logFC shown"), value = 0),
                                               numericInput("lower_logFC", label = h3("Minimal value of logFC shown"), value = 0),
                                               selectInput('wybor', label = 'Choose data for ChIP-seqplot', 
                                                           choices = c('edgeR', 'macs'), selected = c('edgeR'),
                                                           multiple = TRUE, selectize = TRUE)
                                      ),
                                      tabPanel("Plot & Graph Download",
                                               downloadButton('download_plot', 'Download png plot'),
                                               numericInput('width_hist', 'Plot width [cm]', 20, min = 5, max = 25),
                                               numericInput('height_hist', 'Plot height [cm]', 14, min = 5, max = 25),
                                               numericInput('res_hist', 'Resolution', 200, min = 100, max = 500)
                                      )
                          ), width = 2, open = FALSE
                        ),
                        mainPanel(
                          
                          tabsetPanel(type = "pills",
                                      tabPanel("Maing plots",
                                               plotOutput("all_plots", height = '1200px')
                                      ),
                                      tabPanel("Heatmap",
                                               plotOutput("heatmap")
                                      )),
                          tabsetPanel(type = 'pills',
                                      tabPanel("RNAseq in table",
                                               dataTableOutput("rna_table")
                                      ),
                                      tabPanel("CHIPseq in table",
                                               dataTableOutput("chip_table")
                                      ),
                                      tabPanel("gene related stuff",
                                               textOutput("gene_protein_data"))),
                          width = 10
                        )
                      )
             ),
             tabPanel("Summary of data",
                      sidebarLayout(
                        sidebarPanel(),
                        mainPanel()
                      )
             ),
             tabPanel("Help&Info",
                      sidebarLayout(
                        sidebarPanel(),
                        mainPanel()
                      )
             )
  )
)
