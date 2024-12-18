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
library(ggvenn)
source("loading_data.R")

#### UI ####



options_app <- c("genome", "RNAplot", "CHIPplot", "logFCplot", "pvalueVulcano")
chip_choice <- c('data_hupA_chipseq_edgeR', 'data_hupA_chipseq_macs')


ui <- fluidPage(
  theme = shinytheme("yeti"),
  navbarPage("Strepto NGS analysis",
             tabPanel("RNA-seq & ChIP-seq visualization",
                      sidebarLayout(
                        sidebarPanel(
                          tabsetPanel(type = "pills",
                                      tabPanel("Selection",
                                               fileInput("uploaded_file", "Choose a File"),
                                               conditionalPanel(
                                                 # JavaScript condition to check if file input has data
                                                 condition = "output.fileUploaded",
                                                 textAreaInput("file_name", "Add name for your file")
                                               ),
                                               selectInput('options', 'Show/hide visualizations', options_app,
                                                           multiple=TRUE, selectize=TRUE,
                                                           selected = c('genome', 'RNAplot')),
                                               selectizeInput("select_gene", 
                                                              label = "Choose gene from list", 
                                                              choices = "all",
                                                              selected = "all",
                                                              options = list(maxOptions = 10000)),
                                               selectInput('rna_select_1', 
                                                           label = h3('Choose data for RNA-seqplot'), 
                                                           selected = "no data selected",
                                                           choices = c("no data selected"), # The server will update these choices
                                                           selectize = TRUE),
                                               conditionalPanel(condition = 'input.rna_select_1 != "no data selected"',
                                                                uiOutput('contrast_1')
                                               ),
                                               selectInput('rna_select_2', 
                                                           label = ' ',  
                                                           selected = "no data selected",
                                                           choices = c("no data selected"), # The server will update these choices
                                                           selectize = TRUE),
                                               conditionalPanel(condition = 'input.rna_select_2 != "no data selected"',
                                                                uiOutput('contrast_2')
                                               ),
                                               selectInput('rna_select_3', 
                                                           label = ' ',  
                                                           selected = "no data selected",
                                                           choices = c("no data selected"), # The server will update these choices
                                                           selectize = TRUE),
                                               conditionalPanel(condition = 'input.rna_select_3 != "no data selected"',
                                                                uiOutput('contrast_3')
                                               ),
                                               selectInput('wybor', label = 'Choose data for ChIP-seqplot', 
                                                           choices = chip_choice,
                                                           multiple = TRUE, selectize = TRUE),
                                               layout_columns(
                                                 col_widths = c(10, 10),
                                                 fill = FALSE,
                                                 fillable = FALSE,
                                                 gap = 100,
                                                 actionButton("btn_left", "Left", class = "w-100"),
                                                 actionButton("btn_right", "Right", class = "w-100")
                                               ),
                                               
                                               div(
                                                 style = "margin-top: 10px",
                                               layout_columns(
                                                 col_widths = c(3, 3),
                                                 fill = FALSE,
                                                 fillable = FALSE,
                                                 gap = 100,
                                                 actionButton("btn_in", "Zoom in", class = "w-100"),
                                                 actionButton("btn_out", "Zoom out", class = "w-100")
                                               )),
                                               div(
                                                 style = "margin-top: 20px",
                                                    actionButton("apply_changes", "Apply Changes")
                                               )
                                      ),
                                      tabPanel("Plot settings",
                                               numericInput("lower_value", label = ("Minimal value of plot"), value = 4260000, step = 10000),
                                               numericInput("higher_value", label = ("Maximum value of plot"), value = 4280000, step = 10000),
                                               h4("Filter logFC"),
                                               numericInput("higher_logFC", label = ("-higher value"), value = 0, step = 0.5),
                                               numericInput("lower_logFC", label = ("-lower value"), value = 0, step = 0.5),
                                               input_switch("my_switch", "Leave only significant genes (FDR <= 0.05)", value = FALSE),
                                               textOutput("switch_status")
                                               
                                      ),
                                      tabPanel("Plot Download",
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
             tabPanel("Comparison of data",
                      sidebarLayout(
                        sidebarPanel(
                          tabsetPanel(type = "pills",
                                      tabPanel("Selection",
                                               # Remove the nested selectInput
                                               selectInput('venn_select_1', 
                                                           label = h3('Choose data for comparison plots'), 
                                                           selected = "no data selected",
                                                           choices = c("no data selected"), # The server will update these choices
                                                           selectize = TRUE),
                                               conditionalPanel(
                                                 condition = 'input.venn_select_1 != "no data selected"',
                                                 uiOutput('contrast_venn_1')
                                               ),
                                               selectInput('venn_select_2', 
                                                           label = ' ',  
                                                           selected = "no data selected",
                                                           choices = c("no data selected"), # The server will update these choices
                                                           selectize = TRUE),
                                               conditionalPanel(
                                                 condition = 'input.venn_select_2 != "no data selected"',
                                                 uiOutput('contrast_venn_2')
                                               ),
                                               selectizeInput("select_gene_venn", 
                                                              label = "Choose gene from list", 
                                                              choices = "all",
                                                              selected = "all",
                                                              multiple = TRUE,
                                                              options = list(maxOptions = 10000))
                                      ),
                                      tabPanel("Plot Options",
                                               numericInput("higher_logFC_venn", label = ("higher logFC"), value = 0, step = 0.5),
                                               numericInput("lower_logFC_venn", label = ("lower logFC"), value = 0, step = 0.5),
                                               input_switch("my_switch_venn", "Leave only significant genes (FDR <= 0.05)", value = FALSE)),
                                      tabPanel("Plot Download")
                          )
                        ),
                        mainPanel(
                          tabsetPanel(
                            type='pills', 
                            tabPanel("Venn", plotOutput("venn_plot")), tabPanel("Heatmap", plotOutput("heatmap_plot"))                         )
                        )
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
