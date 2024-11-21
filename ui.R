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



data_loaded_rna <- c("abrB1.2_table4", "abrB1.2_table5", "data_hupAS_RNAseq")
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
                                               selectInput('options', 'Show/hide visualizations', options_app,
                                                           multiple=TRUE, selectize=TRUE,
                                                           selected = c('genomeplot', 'RNAplot')),
                                               selectInput("select_gene", label = h3("Choose gene from list"), 
                                                           choices = gene_list_database),
                                               selectInput('rna_select', label = 'Choose data for RNA-seqplot', 
                                                           choices = data_loaded_rna, selected = c('abrB1.2_table4'),
                                                           multiple = TRUE, selectize = TRUE),
                                               selectInput('wybor', label = 'Choose data for ChIP-seqplot', 
                                                           choices = c('edgeR', 'macs'), selected = c('edgeR'),
                                                           multiple = TRUE, selectize = TRUE),
                                               numericInput("lower_value", label = h3("Minimal value of plot"), value = 4000000, step = 10000),
                                               numericInput("higher_value", label = h3("Maximum value of plot"), value = 4200000, step = 10000),
                                      ),
                                      tabPanel("Plot settings",
                                               numericInput("higher_logFC", label = h3("Maximum value of logFC shown"), value = 0),
                                               numericInput("lower_logFC", label = h3("Minimal value of logFC shown"), value = 0),
                                               
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
