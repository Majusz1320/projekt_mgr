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
source('plots_code.R')

#### UI ####


options_app <- c("genome", "RNAplot", "CHIPplot")



ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .sidebar-scroll {
        height: calc(100vh - 70px);
        overflow-y: auto;
      }
      .main-scroll {
        height: calc(100vh - 70px);
        overflow-y: auto;
      }
    "))
  ),
  theme = bs_theme(version = 5, bootswatch = "yeti"),
  navbarPage("Strep.A.N",
             tabPanel("RNA-seq & ChIP-seq visualization",
                      layout_sidebar(
                        sidebar = sidebar(
                          width = 500,
                          div(class = "sidebar-scroll",
                              accordion(
                                accordion_panel("Navigation", class = "mb-3",div(
                                  class = "d-grid gap-2",
                                  div(
                                    class = "btn-group w-100",
                                    actionButton("btn_left", "◀", class = "btn-primary"),
                                    actionButton("btn_right", "▶", class = "btn-primary")
                                  ),
                                  div(
                                    class = "btn-group w-100",
                                    actionButton("btn_in", "Zoom in 🔍", class = "btn-secondary"),
                                    actionButton("btn_out", "Zoom out 🔍", class = "btn-secondary")
                                  )
                                ),
                                numericInput("lower_value", "Minimal value of plot", value = 4260000, step = 10000),
                                numericInput("higher_value", "Maximum value of plot", value = 4280000, step = 10000),
                                actionButton("apply_changes", "Apply Changes", class = "btn-primary w-100 mt-3")
                                ),
                                accordion_panel("Selection",
                                                card(
                                                bslib::input_switch("switch_species", "Select streptomyces species"),
                                                verbatimTextOutput("switch_value"),
                                                selectInput('options', 'Show/hide visualizations', options_app,
                                                            multiple=TRUE, selectize=TRUE,
                                                            selected = c('genome', 'RNAplot')),
                                                selectizeInput("select_gene", 
                                                               label = "Choose gene from list", 
                                                               choices = "all",
                                                               selected = "all",
                                                               options = list(maxOptions = 10000))),
                                                card(card_header('RNA-seq Plot Data'),
                                                selectInput('rna_select_1', 
                                                            label = NULL,
                                                            selected = "no data selected",
                                                            choices = c("no data selected"),
                                                            selectize = TRUE),
                                                conditionalPanel(condition = 'input.rna_select_1 != "no data selected"',
                                                                 uiOutput('contrast_1')
                                                ),
                                                selectInput('rna_select_2', 
                                                            label = ' ',  
                                                            selected = "no data selected",
                                                            choices = c("no data selected"),
                                                            selectize = TRUE),
                                                conditionalPanel(condition = 'input.rna_select_2 != "no data selected"',
                                                                 uiOutput('contrast_2')
                                                ),
                                                selectInput('rna_select_3', 
                                                            label = ' ',  
                                                            selected = "no data selected",
                                                            choices = c("no data selected"),
                                                            selectize = TRUE),
                                                conditionalPanel(condition = 'input.rna_select_3 != "no data selected"',
                                                                 uiOutput('contrast_3')
                                                )),
                                                card(card_header("ChIP-seq Plot Data"),
                                                selectInput('chip_select', 
                                                            label = NULL,  
                                                            selected = "no data selected",
                                                            choices = c("no data selected"),
                                                            selectize = TRUE),
                                                conditionalPanel(condition = 'input.chip_select != "no data selected"',
                                                                 uiOutput('contrast_chip')
                                                ))
                                ),
                                accordion_panel("Plot settings",
                                                h4("LogFC Filtering"),
                                                numericInput("higher_logFC", label = ("-higher value"), value = 0, step = 0.5, min = 0),
                                                numericInput("lower_logFC", label = ("-lower value"), value = 0, step = 0.5, max = 0),
                                                input_switch("my_switch", "Leave only significant genes (FDR <= 0.05)", value = FALSE),
                                                textOutput("switch_status")
                                                
                                ),
                                accordion_panel("Upload data",
                                                fileInput("uploaded_file", "RNA-seq"),
                                                conditionalPanel(
                                                  condition = "output.fileUploaded",
                                                  textAreaInput("file_name", "Add name for your file")
                                                ),
                                                
                                                fileInput("uploaded_file_chip", "CHIP-seq"),
                                                conditionalPanel(
                                                  condition = "output.fileUploaded_chip",
                                                  textAreaInput("file_name_chip", "Add name for your file")
                                                )),
                                accordion_panel("Plot Download",
                                                downloadButton('download_plot', 'Download png plot'),
                                                numericInput('width_plot', 'Plot width [cm]', 45, min = 5, max = 1000),
                                                numericInput('height_plot', 'Plot height [cm]', 22, min = 5, max = 1000),
                                                numericInput('res_plot', 'Resolution', 300, min = 100, max = 500)
                                )
                              ))
                        ),
                        mainPanel(
                          div(class = "main-scroll",
                              tabsetPanel(type = "pills",
                                          tabPanel("Visualization",
                                                   plotOutput("all_plots", height = '800px')
                                          )),
                              tabsetPanel(type = 'pills',
                                          tabPanel("RNAseq in table",
                                                   dataTableOutput("rna_table")
                                          ),
                                          tabPanel("CHIPseq in table",
                                                   dataTableOutput("chip_table")
                                          ))), width = 12
                        )
                      )
             ),
             tabPanel("Comparison of data",
                      layout_sidebar(
                        sidebar = sidebar(
                          width = 500,
                          div(class = "sidebar-scroll",
                              accordion(
                                          accordion_panel("Selection",
                                                          card(card_header('Venn Diagram Data'),
                                                   selectInput('venn_select_1', 
                                                               label = NULL, 
                                                               selected = "no data selected",
                                                               choices = c("no data selected"),
                                                               selectize = TRUE),
                                                   conditionalPanel(
                                                     condition = 'input.venn_select_1 != "no data selected"',
                                                     uiOutput('contrast_venn_1')
                                                   ),
                                                   selectInput('venn_select_2', 
                                                               label = ' ',  
                                                               selected = "no data selected",
                                                               choices = c("no data selected"),
                                                               selectize = TRUE),
                                                   conditionalPanel(
                                                     condition = 'input.venn_select_2 != "no data selected"',
                                                     uiOutput('contrast_venn_2')
                                                   )),card(card_header("Heatmap gene name input"),
                                                   textAreaInput("gene_list", NULL))
                                          ),
                                          accordion_panel("Venn/Heat LogFC",
                                                   numericInput("higher_logFC_venn", label = ("higher logFC"), value = 1.5, step = 0.1, min = 0),
                                                   numericInput("lower_logFC_venn", label = ("lower logFC"), value = -1.5, step = 0.1, max = 0)),
                                          accordion_panel("Intime Options",
                                                   fileInput("uploaded_intime_file", "Choose a File"),
                                                   conditionalPanel(
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
                                                               choices = c("no data selected"),
                                                               selectize = TRUE),
                                                   conditionalPanel(
                                                     condition = 'input.intime_select_1 != "no data selected"',
                                                     uiOutput('contrast_intime_1')
                                                   ),
                                                   selectInput('intime_select_2', 
                                                               label = ' ',  
                                                               selected = "no data selected",
                                                               choices = c("no data selected"),
                                                               selectize = TRUE),
                                                   conditionalPanel(
                                                     condition = 'input.intime_select_2 != "no data selected"',
                                                     uiOutput('contrast_intime_2')
                                                   ),
                                                   
                                          ),
                                          accordion_panel("Plot download", 
                                                    card("Plot Download Venn",
                                                   downloadButton('download_plot_venn', 'Download png plot'),
                                                   numericInput('width_venn', 'Plot width [cm]', 20, min = 5, max = 1000),
                                                   numericInput('height_venn', 'Plot height [cm]', 14, min = 5, max = 1000),
                                                   numericInput('res_venn', 'Resolution', 200, min = 100, max = 500)),
                                                   card("Plot Download Heat",
                                                   downloadButton('download_plot_heat', 'Download png plot'),
                                                   numericInput('width_heat', 'Plot width [cm]', 20, min = 5, max = 1000),
                                                   numericInput('height_heat', 'Plot height [cm]', 14, min = 5, max = 1000),
                                                   numericInput('res_heat', 'Resolution', 200, min = 100, max = 500))
                                          )
                              ))
                        ),
                        mainPanel(
                          div(class = "main-scroll",
                              tabsetPanel(
                                type='pills', 
                                tabPanel("Venn", 
                                         plotOutput("venn_plot", height = '800px' ),
                                         dataTableOutput('venn_table_common'), dataTableOutput('venn_table_uncommon')), 
                                tabPanel("Heatmap", plotOutput("heatmap_plot", height = '800px' ),
                                         dataTableOutput('heatmap_table')),
                                tabPanel("In time comparison", plotOutput("intime_plot", height = '800px' )))), width = 12
                        )
                      )
             ),
             tabPanel("Help&Info",
                      sidebarLayout(
                        sidebarPanel(
                          div(class = "sidebar-scroll")
                        ),
                        mainPanel(
                          div(class = "main-scroll")
                        )
                      )
             )
  )
)