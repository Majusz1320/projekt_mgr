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



data_loaded_rna <- c("no data selected", "abrB1.2_table", "data_hupAS_RNAseq", "RNAseq_Martyna", "szafran2019", "user_uploaded_file")
options_app <- c("genome", "RNAplot", "CHIPplot", "logFCplot", "pvalueVulcano")
chip_choice <- c('data_hupA_chipseq_edgeR', 'data_hupA_chipseq_macs')
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
                                               fileInput("uploaded_file", "Choose a File"),
                                               selectInput('options', 'Show/hide visualizations', options_app,
                                                           multiple=TRUE, selectize=TRUE,
                                                           selected = c('genome', 'RNAplot')),
                                               selectInput("select_gene", label = "Choose gene from list", 
                                                           choices = gene_list_database),
                                               selectInput('rna_select_1', label = h3('Choose data for RNA-seqplot'), selected = "no data selected",
                                                           choices = data_loaded_rna, selectize = TRUE),
                                               conditionalPanel(condition = 'input.rna_select_1 != "no data selected"',
                                                                uiOutput('contrast_1')
                                               ),
                                               selectInput('rna_select_2', label = ' ',  selected = "no data selected",
                                                           choices = data_loaded_rna, selectize = TRUE),
                                               conditionalPanel(condition = 'input.rna_select_2 != "no data selected"',
                                                                uiOutput('contrast_2')
                                               ),
                                               selectInput('rna_select_3', label = ' ',  selected = "no data selected",
                                                           choices = data_loaded_rna, selectize = TRUE),
                                               conditionalPanel(condition = 'input.rna_select_3 != "no data selected"',
                                                                uiOutput('contrast_3')
                                               ),
                                               selectInput('wybor', label = 'Choose data for ChIP-seqplot', 
                                                           choices = chip_choice,
                                                           multiple = TRUE, selectize = TRUE),
                                               numericInput("lower_value", label = h3("Minimal value of plot"), value = 4000000, step = 10000),
                                               numericInput("higher_value", label = h3("Maximum value of plot"), value = 4200000, step = 10000),
                                               actionButton("apply_changes", "Apply Changes"),
                                      ),
                                      tabPanel("Plot settings",
                                               numericInput("higher_logFC", label = h3("Filter logFC \n- higher value"), value = 0),
                                               numericInput("lower_logFC", label = h3("-lower value"), value = 0),
                                               input_switch("my_switch", "Leave only significant genes (FDR <= 0.05)", value = FALSE),
                                               textOutput("switch_status")
                                               
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
                                               plotOutput("all_plots", height = '800px')
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
