library(shiny)
library(shinythemes)
library(ggplot2)
library(gggenes)
library(dplyr)
library(tidyr)
library(pheatmap)
library(patchwork)
library(tidyHeatmap)
library(BiocManager)
library(plotly)
library(bslib)
library(ggvenn)
library(ggupset)
library(pdftools)
library(png)
source("loading_data.R")
source('functions.R')

#### UI ####



options_app <- c("genome", "RNAplot", "CHIPplot")
chip_choice <- c('data_hupA_chipseq_edgeR', 'data_hupA_chipseq_macs')


ui <- fluidPage(
  theme = shinytheme("yeti"),
  navbarPage("Strep.A.N",
             tabPanel("RNA-seq & ChIP-seq visualization",
                      sidebarLayout(
                        sidebarPanel(
                          tabsetPanel(type = "pills",
                                      tabPanel("Selection",
                                               bslib::input_switch("switch_species", "Select streptomyces species"),
                                               verbatimTextOutput("switch_value"),
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
                                                 gap = "1px",
                                                 actionButton("btn_left", "Left", class = "w-100"),
                                                 actionButton("btn_right", "Right", class = "w-100")
                                               ),
                                               
                                               div(
                                                 style = "margin-top: 10px",
                                               layout_columns(
                                                 col_widths = c(3, 3),
                                                 fill = FALSE,
                                                 fillable = FALSE,
                                                 gap = "10px",
                                                 actionButton("btn_in", "Zoom in", class = "w-100"),
                                                 actionButton("btn_out", "Zoom out", class = "w-100")
                                               )),
                                               
                                               numericInput("lower_value", label = ("Minimal value of plot"), value = 4260000, step = 10000),
                                               numericInput("higher_value", label = ("Maximum value of plot"), value = 4280000, step = 10000),
                                               div(
                                                 style = "margin-top: 20px",
                                                    actionButton("apply_changes", "Apply Changes")
                                               )
                                      ),
                                      tabPanel("Plot settings",
                                               h4("Filter logFC"),
                                               numericInput("higher_logFC", label = ("-higher value"), value = 0, step = 0.5, min = 0),
                                               numericInput("lower_logFC", label = ("-lower value"), value = 0, step = 0.5, max = 0),
                                               input_switch("my_switch", "Leave only significant genes (FDR <= 0.05)", value = FALSE),
                                               textOutput("switch_status")
                                               
                                      ),
                                      tabPanel("Plot Download",
                                               downloadButton('download_plot', 'Download png plot'),
                                               numericInput('width_plot', 'Plot width [cm]', 45, min = 5, max = 1000),
                                               numericInput('height_plot', 'Plot height [cm]', 22, min = 5, max = 1000),
                                               numericInput('res_plot', 'Resolution', 300, min = 100, max = 500)
                                      )
                          ), width = 2, open = FALSE
                        ),
                        mainPanel(
                          
                          tabsetPanel(type = "pills",
                                      tabPanel("Main plots",
                                               plotOutput("all_plots", height = '800px')
                                      )),
                          tabsetPanel(type = 'pills',
                                      tabPanel("RNAseq in table",
                                               dataTableOutput("rna_table")
                                      ),
                                      tabPanel("CHIPseq in table",
                                               dataTableOutput("chip_table")
                                      )),
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
                                               textAreaInput("gene_list", "Input gene names for heatmap")
                                      ),
                                      tabPanel("Venn/Heat Options",
                                               numericInput("higher_logFC_venn", label = ("higher logFC"), value = 1.5, step = 0.1, min = 0),
                                               numericInput("lower_logFC_venn", label = ("lower logFC"), value = -1.5, step = 0.1, max = 0)),
                                      tabPanel("Intime Options",
                                               fileInput("uploaded_intime_file", "Choose a File"),
                                               conditionalPanel(
                                                 # JavaScript condition to check if file input has data
                                                 condition = "output.fileintimeUploaded",
                                                 textAreaInput("file_intime_name", "Add name for your file")
                                               ),
                                               selectizeInput("select_gene_intime", 
                                                              label = "Choose gene from list", 
                                                              choices = "all",
                                                              selected = "all",
                                                              multiple = TRUE,
                                                              options = list(maxOptions = 10000)),
                                               selectInput('intime_select_1', 
                                                           label = h3('Choose data'), 
                                                           selected = "no data selected",
                                                           choices = c("no data selected"), # The server will update these choices
                                                           selectize = TRUE),
                                               conditionalPanel(
                                                 condition = 'input.intime_select_1 != "no data selected"',
                                                 uiOutput('contrast_intime_1')
                                               ),
                                               selectInput('intime_select_2', 
                                                           label = ' ',  
                                                           selected = "no data selected",
                                                           choices = c("no data selected"), # The server will update these choices
                                                           selectize = TRUE),
                                               conditionalPanel(
                                                 condition = 'input.intime_select_2 != "no data selected"',
                                                 uiOutput('contrast_intime_2')
                                               ),
                                               
                                      ),
                                      tabPanel("Plot Download Venn",
                                               downloadButton('download_plot_venn', 'Download png plot'),
                                               numericInput('width_venn', 'Plot width [cm]', 20, min = 5, max = 1000),
                                               numericInput('height_venn', 'Plot height [cm]', 14, min = 5, max = 1000),
                                               numericInput('res_venn', 'Resolution', 200, min = 100, max = 500)
                                      ),
                                      tabPanel("Plot Download Heat",
                                               downloadButton('download_plot_heat', 'Download png plot'),
                                               numericInput('width_heat', 'Plot width [cm]', 20, min = 5, max = 1000),
                                               numericInput('height_heat', 'Plot height [cm]', 14, min = 5, max = 1000),
                                               numericInput('res_heat', 'Resolution', 200, min = 100, max = 500)
                                      )
                          )
                        ),
                        mainPanel(
                          tabsetPanel(
                            type='pills', 
                            tabPanel("Venn", 
                                     plotOutput("venn_plot", height = '800px' ),
                                     dataTableOutput('venn_table_common')),
                            tabPanel("Heatmap", plotOutput("heatmap_plot", height = '800px' ),
                                     dataTableOutput('heatmap_table')),
                            tabPanel("In time comparison", plotOutput("intime_plot", height = '800px' )))
                        
                          
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
