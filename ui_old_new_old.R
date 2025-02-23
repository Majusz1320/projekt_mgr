ui <- page_navbar(
  theme = bs_theme(
    version = 5,
    bootswatch = "yeti",
    primary = "#2C3E50",
    "navbar-bg" = "#2C3E50"
  ),
  title = "Strep.A.N",
  
  nav_panel("RNA-seq & ChIP-seq visualization",
            layout_sidebar(
              sidebar = sidebar(
                width = 300,
                accordion(
                  accordion_panel(
                    "Navigation",
                    class = "mb-3",
                    div(
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
                  
                  accordion_panel(
                    "Selection",
                    class = "mb-3",
                    input_switch("switch_species", "Select streptomyces species"),
                    verbatimTextOutput("switch_value"),
                    
                    card(
                      class = "mb-3",
                      fileInput("uploaded_file", "Choose a File"),
                      conditionalPanel(
                        condition = "output.fileUploaded",
                        textAreaInput("file_name", "Add name for your file")
                      )
                    ),
                    
                    selectInput('options', 'Show/hide visualizations', 
                                options_app,
                                multiple = TRUE, 
                                selectize = TRUE,
                                selected = c('genome', 'RNAplot')),
                    
                    selectizeInput("select_gene", 
                                   "Choose gene from list", 
                                   choices = "all",
                                   selected = "all",
                                   options = list(maxOptions = 10000)),
                    
                    card(
                      class = "mb-3",
                      card_header("RNA-seq Plot Data"),
                      selectInput('rna_select_1', 
                                  'Dataset 1', 
                                  selected = "no data selected",
                                  choices = c("no data selected")),
                      conditionalPanel(
                        condition = 'input.rna_select_1 != "no data selected"',
                        uiOutput('contrast_1')
                      ),
                      selectInput('rna_select_2', 
                                  'Dataset 2',  
                                  selected = "no data selected",
                                  choices = c("no data selected")),
                      conditionalPanel(
                        condition = 'input.rna_select_2 != "no data selected"',
                        uiOutput('contrast_2')
                      ),
                      selectInput('rna_select_3', 
                                  'Dataset 3',  
                                  selected = "no data selected",
                                  choices = c("no data selected")),
                      conditionalPanel(
                        condition = 'input.rna_select_3 != "no data selected"',
                        uiOutput('contrast_3')
                      )
                    ),
                    
                    card(
                      class = "mb-3",
                      card_header("ChIP-seq Plot Data"),
                      selectInput('chip_select', 
                                  'Select dataset',  
                                  selected = "no data selected",
                                  choices = c("no data selected")),
                      conditionalPanel(
                        condition = 'input.chip_select != "no data selected"',
                        uiOutput('contrast_chip')
                      )
                    )
                  ),
                  
                  accordion_panel(
                    "Plot settings",
                    class = "mb-3",
                    card(
                      card_header("LogFC Filtering"),
                      numericInput("higher_logFC", "Higher value", value = 0, step = 0.5, min = 0),
                      numericInput("lower_logFC", "Lower value", value = 0, step = 0.5, max = 0),
                      input_switch("my_switch", "Show significant genes only (FDR ≤ 0.05)", value = FALSE),
                      textOutput("switch_status")
                    )
                  ),
                  
                  accordion_panel(
                    "Plot Download",
                    downloadButton('download_plot', 'Download PNG', class = "btn-primary w-100 mb-3"),
                    numericInput('width_plot', 'Plot width [cm]', 45, min = 5, max = 1000),
                    numericInput('height_plot', 'Plot height [cm]', 22, min = 5, max = 1000),
                    numericInput('res_plot', 'Resolution', 300, min = 100, max = 500)
                  )
                )
              ),
              
              # Main panel content
              layout_column_wrap(
                width = 1,
                card(
                  card_header("Visualization"),
                  plotOutput("all_plots", height = '800px')
                ),
                navset_pill(
                  nav_panel("RNAseq Data", dataTableOutput("rna_table")),
                  nav_panel("CHIPseq Data", dataTableOutput("chip_table"))
                )
              )
            )
  ),
  
  nav_panel("Comparison of data",
            layout_sidebar(
              sidebar = sidebar(
                width = 300,
                navset_pill(
                  nav_panel("Selection",
                            selectInput('venn_select_1', 'Dataset 1', 
                                        selected = "no data selected",
                                        choices = c("no data selected")),
                            conditionalPanel(
                              condition = 'input.venn_select_1 != "no data selected"',
                              uiOutput('contrast_venn_1')
                            ),
                            selectInput('venn_select_2', 'Dataset 2',  
                                        selected = "no data selected",
                                        choices = c("no data selected")),
                            conditionalPanel(
                              condition = 'input.venn_select_2 != "no data selected"',
                              uiOutput('contrast_venn_2')
                            ),
                            textAreaInput("gene_list", "Input gene names for heatmap")
                  ),
                  nav_panel("Venn/Heat Options",
                            numericInput("higher_logFC_venn", "Higher logFC", value = 1.5, step = 0.1, min = 0),
                            numericInput("lower_logFC_venn", "Lower logFC", value = -1.5, step = 0.1, max = 0)
                  ),
                  nav_panel("Intime Options",
                            fileInput("uploaded_intime_file", "Choose a File"),
                            conditionalPanel(
                              condition = "output.fileintimeUploaded",
                              textAreaInput("file_intime_name", "Add name for your file")
                            ),
                            selectizeInput("select_gene_intime", 
                                           "Choose gene from list", 
                                           choices = "all",
                                           selected = "all",
                                           multiple = TRUE,
                                           options = list(maxOptions = 10000)),
                            selectInput('intime_select_1', 'Dataset 1', 
                                        selected = "no data selected",
                                        choices = c("no data selected")),
                            conditionalPanel(
                              condition = 'input.intime_select_1 != "no data selected"',
                              uiOutput('contrast_intime_1')
                            ),
                            selectInput('intime_select_2', 'Dataset 2',  
                                        selected = "no data selected",
                                        choices = c("no data selected")),
                            conditionalPanel(
                              condition = 'input.intime_select_2 != "no data selected"',
                              uiOutput('contrast_intime_2')
                            )
                  ),
                  nav_panel("Download Options",
                            card(
                              card_header("Venn Diagram"),
                              downloadButton('download_plot_venn', 'Download PNG', class = "btn-primary w-100 mb-3"),
                              numericInput('width_venn', 'Width [cm]', 20, min = 5, max = 1000),
                              numericInput('height_venn', 'Height [cm]', 14, min = 5, max = 1000),
                              numericInput('res_venn', 'Resolution', 200, min = 100, max = 500)
                            ),
                            card(
                              card_header("Heatmap"),
                              downloadButton('download_plot_heat', 'Download PNG', class = "btn-primary w-100 mb-3"),
                              numericInput('width_heat', 'Width [cm]', 20, min = 5, max = 1000),
                              numericInput('height_heat', 'Height [cm]', 14, min = 5, max = 1000),
                              numericInput('res_heat', 'Resolution', 200, min = 100, max = 500)
                            )
                  )
                )
              ),
              
              # Main panel content
              navset_pill(
                nav_panel("Venn", 
                          plotOutput("venn_plot", height = '800px'),
                          dataTableOutput('venn_table_common')
                ),
                nav_panel("Heatmap", 
                          plotOutput("heatmap_plot", height = '800px'),
                          dataTableOutput('heatmap_table')
                ),
                nav_panel("In time comparison", 
                          plotOutput("intime_plot", height = '800px')
                )
              )
            )
  ),
  
  nav_panel("Help & Info",
            layout_sidebar(
              sidebar = sidebar(),
              "Help content goes here"
            )
  )
)