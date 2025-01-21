#### SERVER #####




# AS added to allow shiny to load files bigger than default 5 Mb
options(shiny.maxRequestSize=30*1024^2) 
server <- function(input, output, session) {
  
  switch_status <- reactive({
    switch_status <- input$switch_species
    return(switch_status)
  })
  output$switch_value <- reactive({
    if (switch_status() == FALSE)
    {switch_text <- "SCOE"}
    else
    {switch_text <- "SVEN"}
    return(switch_text)
  })
  
  #### server-side select choice of genes ####
  # In the server function, add this at the beginning:
  observeEvent(session$clientData$url_protocol, {  # This ensures it runs when the app starts
    # Load the gene list
    if(switch_status() == FALSE)
    {
    genelist <- read.csv("datasets/genes_scoelicolor.txt", sep = '')
    gene_list_database <- c("all", genelist$gene)
    # Update the selectize input with all choices
    updateSelectizeInput(session, 
                         inputId = "select_gene", 
                         choices = gene_list_database,
                         selected = "all",
                         server = TRUE) }
    else
    {
      genelist <- read.csv("datasets/sven_genes_vnz.txt", sep = '')
      gene_list_database <- c("all", genelist$gene)
      updateSelectizeInput(session, 
                           inputId = "select_gene", 
                           choices = gene_list_database,
                           selected = "all",
                           server = TRUE) 
    }
  }, once = TRUE)  # once=TRUE means it only runs once when the app starts
  
  observeEvent(session$clientData$url_protocol, {  # This ensures it runs when the app starts
    # Load the gene list
    if(switch_status() == FALSE)
    {
    genelist <- read.csv("datasets/genes_scoelicolor.txt", sep = '')
    gene_list_database <- c("all", genelist$gene)
    # Update the selectize input with all choices
    updateSelectizeInput(session, 
                         inputId = "select_gene_venn", 
                         choices = gene_list_database,
                         selected = "all",
                         server = TRUE)}
    else
    {
      genelist <- read.csv("datasets/sven_genes_vnz.txt", sep = '')
      gene_list_database <- c("all", genelist$gene)
      # Update the selectize input with all choices
      updateSelectizeInput(session, 
                           inputId = "select_gene_venn", 
                           choices = gene_list_database,
                           selected = "all",
                           server = TRUE)
    }
  }, once = TRUE)  # once=TRUE means it only runs once when the app starts
  
  
  
  # Create a reactive value to store the trigger state
  changes_applied <- reactiveVal(FALSE)  # This will store whether the button has been pressed

  # Observe the action button press
  observeEvent(input$apply_changes, {
    changes_applied(TRUE)  # Set the value to TRUE when the button is pressed
  })
  
  
  #### APPLY CHANGES BUTTON ####

  changes_applied_lower <- eventReactive(c(input$apply_changes, input$btn_left, input$btn_right, input$btn_in, input$btn_out), {
    input$lower_value
  }, ignoreNULL = FALSE)
  
  changes_applied_higher <- eventReactive(c(input$apply_changes, input$btn_left, input$btn_right, input$btn_in, input$btn_out), {
    input$higher_value
  }, ignoreNULL = FALSE)
  
  
  #### ZOOM BUTTONS ####
  button_states <- reactiveValues(
    b1 = FALSE,
    b2 = FALSE,
    b3 = FALSE,
    b4 = FALSE
  )
  
  ### button left
  
  observeEvent(input$btn_left, {
    button_states$b1 <- TRUE
    new_value_low <- input$lower_value - 10000
    new_value_high <- input$higher_value - 10000
    updateNumericInput(
      session = session, inputId = "lower_value", value = new_value_low)
    updateNumericInput(
      session = session, inputId = "higher_value", value = new_value_high)
    button_states$b1 <- FALSE
  })
  
  ### button right
  
  observeEvent(input$btn_right, {
    button_states$b2 <- TRUE
    new_value_low <- input$lower_value + 10000
    new_value_high <- input$higher_value + 10000
    updateNumericInput(
      session = session, inputId = "lower_value", value = new_value_low)
    updateNumericInput(
      session = session, inputId = "higher_value", value = new_value_high)
    button_states$b2 <- FALSE
  })
  
  ### button in
  
  observeEvent(input$btn_in, {
    button_states$b3 <- TRUE
    new_value_low <- input$lower_value + 10000
    new_value_high <- input$higher_value - 10000
    updateNumericInput(
      session = session, inputId = "lower_value", value = new_value_low)
    updateNumericInput(
      session = session, inputId = "higher_value", value = new_value_high)
    button_states$b3 <- FALSE
  })
  
  ### button out
  
  observeEvent(input$btn_out, {
    button_states$b4 <- TRUE
    new_value_low <- input$lower_value - 10000
    new_value_high <- input$higher_value + 10000
    updateNumericInput(
      session = session, inputId = "lower_value", value = new_value_low)
    updateNumericInput(
      session = session, inputId = "higher_value", value = new_value_high)
    button_states$b4 <- FALSE
  })
  
  
  
  switch_state <- reactive({
    input$my_switch
  })
  
  output$switch_status <- renderText({
    if(switch_state()) {
      "ON"
    } else {
      "OFF"
    }
  })
  
  
  #### PLOT GENOME INPUT ####
  
  plotgenomeInput <- reactive({
    
    req(changes_applied())
    
    if(switch_status() == FALSE)
    {
    if(input$select_gene == "all") {
      plot_data <- read.csv("datasets/genes_scoelicolor.txt", sep = '')
    } else {
      input$select_gene -> selected_gene
      plot_data <- read.csv("datasets/genes_scoelicolor.txt", sep = '')
      plot_data <- plot_data %>% filter(gene == selected_gene)
    }}
    else{
      if(input$select_gene == "all") {
        plot_data <- read.csv("datasets/sven_genes_vnz.txt", sep = '')
      } else {
        input$select_gene -> selected_gene
        plot_data <- read.csv("datasets/sven_genes_vnz.txt", sep = '')
        plot_data <- plot_data %>% filter(gene == selected_gene)
      }
      
    }
    
    return(plot_data)
  })
  
  
  
  
  #### USER DATA UPLOAD ####
  
 user_data_upload <- reactive({
    req(changes_applied())
    
    user_file <- input$uploaded_file$datapath
    if(is.null(user_file)){
        return(NULL)
    } else {
        user_file <- read.csv(user_file, sep = "\t")
        # Use input$file_name directly here
        user_file$data_name <- input$file_name
        return(user_file)
    }
})
  
  

  
  #### MERGING USER DATA ####
  
  merged_user <- reactive({
    req(changes_applied())
    
    user_data <- user_data_upload()
    
    if (is.null(user_data)) {
      return(NULL)
    }
    
    # Determine which genome data to use based on switch status
    if (switch_status() == FALSE) {
      data_genome <- read.csv("datasets/genes_scoelicolor.txt", sep = '')
    } else {
      data_genome <- read.csv("datasets/sven_genes_vnz.txt", sep = '')
    }
    
    # Merge the data and remove NA values
    merged_data <- user_data %>%
      left_join(data_genome, by = "gene") %>%
      na.omit()
    
    return(merged_data)
  })
  
  
  
  output$fileUploaded <- reactive({
  !is.null(input$uploaded_file)
})
outputOptions(output, "fileUploaded", suspendWhenHidden = FALSE)

  
  
  #### LOADING RNA-SEQ DATA ####
  
data_loaded_rna <- reactive({
  if (!is.null(input$file_name)) {
    c("abrB1.2_table", "data_hupAS_RNAseq", "RNAseq_Martyna", "szafran2019", "abrc3", "aor1_rna","argR_2018", "bldD_scoe", "data_bldC_sven", "draRK_scoe", "ECF42s_sven", "glnr_sven", "hups_rnaseq_Strzalka_sven", "ohkA_scoe", "osdR_2016", "sigR", "soxr_genes", "whiAH_scoe", input$file_name)
  } else {
    c("abrB1.2_table", "data_hupAS_RNAseq", "RNAseq_Martyna", "szafran2019", "abrc3", "aor1_rna", "argR_2018", "bldD_scoe", "data_bldC_sven", "draRK_scoe", "ECF42s_sven", "glnr_sven", "hups_rnaseq_Strzalka_sven", "ohkA_scoe", "osdR_2016", "sigR", "soxr_genes", "whiAH_scoe")
  }
})

observe({
  # Get the current data options
  choices <- c("no data selected", data_loaded_rna())
  
  # Update all three select inputs
  updateSelectInput(session, "rna_select_1", choices = choices)
  updateSelectInput(session, "rna_select_2", choices = choices)
  updateSelectInput(session, "rna_select_3", choices = choices)
})



dataselection_rnaseq_before_LHfilter <- reactive({
  
  req(changes_applied())
  
  ###tutaj dopisujesz następne jak będą
  RNAseq_Martyna <- RNAseq_Martyna_load()
  user_data <- merged_user()
  szafran2019 <- data_szafran2019_load()
  abrB1.2_table <- abrB1.2_table_load()
  data_hupAS_RNAseq <- data_hupAS_RNAseq_load()
  abrc3 <- abrc3_load() 
  aor1_rna <- aor1_rna_load()
  argR_2018 <- argR_2018_load()
  bldD_scoe <- bldD_scoe_load()
  data_bldC_sven <- data_bldC_sven_load()
  draRK_scoe <- draRK_scoe_load()
  ECF42s_sven <- ECF42s_sven_load()
  glnr_sven <- glnr_sven_load()
  hups_rnaseq_Strzalka_sven <- hups_rnaseq_Strzalka_sven_load()
  ohkA_scoe <- ohkA_scoe_load()
  osdR_2016 <- osdR_2016_load()
  sigR <- sigR_load()
  soxr_genes <- soxr_genes_load()
  whiAH_scoe <- whiAH_scoe_load()
  
  # Create a list of data frames, handling the user data separately
  data_list <- list(
    abrB1.2_table = abrB1.2_table,
    data_hupAS_RNAseq = data_hupAS_RNAseq,
    RNAseq_Martyna = RNAseq_Martyna,
    szafran2019 = szafran2019,
    abrc3 = abrc3,
    aor1_rna = aor1_rna,
    argR_2018 = argR_2018,
    bldD_scoe = bldD_scoe,
    data_bldC_sven = data_bldC_sven,
    draRK_scoe = draRK_scoe,
    ECF42s_sven = ECF42s_sven,
    glnr_sven = glnr_sven,
    hups_rnaseq_Strzalka_sven = hups_rnaseq_Strzalka_sven,
    ohkA_scoe = ohkA_scoe,
    osdR_2016 = osdR_2016,
    sigR = sigR,
    soxr_genes = soxr_genes,
    whiAH_scoe = whiAH_scoe
  )
  
  # Add user data to the list if it exists
  if (!is.null(user_data)) {
    data_list[[input$file_name]] <- user_data
  }
  
  # Get selected datasets
  selected_datasets <- c(input$rna_select_1, input$rna_select_2, input$rna_select_3)
  selected_datasets <- selected_datasets[selected_datasets != "no data selected"]
  
  # Filter and combine the selected datasets
  selected_data <- data_list[selected_datasets]
  data_rna_final <- do.call(rbind, selected_data)
  
  return(data_rna_final)
})
  
  
 
  #### LOWER/HIGHERVALUE ####
  
  
  # lower_value <- reactive({
  #   input$lower_value
  #   
  # })
  

  observeEvent(input$select_gene,{
    if(input$select_gene != 'all'){
      updateNumericInput(
        session = session, inputId = "lower_value", value = plotgenomeInput()$start
      )
    }
  })
  

  #  higher_value <- reactive({
   # input$higher_value
    
#  })
  
  
  
  observeEvent(input$select_gene,{
    if(input$select_gene != 'all'){
      updateNumericInput(
        session = session, inputId = "higher_value", value = plotgenomeInput()$end
      )
    }
  })
  
  
  #### FDR FILTER ####
  
  
  dataselection_rnaseq_FDR_filter <- reactive({
    data_rna <- dataselection_rnaseq_before_LHfilter()
    if(switch_state()) {
      data_rna <- data_rna %>% filter(FDR <= 0.05)
      return(data_rna)
    } else {
      return(data_rna)
    }
  })
  
  
  #### RNA SELECTION LOW/HIGH, contrast FILTER ####
  
  dataselection_rnaseq <- reactive({
    
    req(changes_applied())
    
    data_rna <- dataselection_rnaseq_FDR_filter()
    
    lower <- changes_applied_lower()
    higher <- changes_applied_higher()
    
    data_rna <- data_rna %>% filter(start >= lower, end <= higher, add_variable %in% c(input$contrast_1, input$contrast_2, input$contrast_3))
    #print(data_rna)
    return(data_rna)
    })
  
  
  

  #### CONDITIONAL FOR CONTRASTS ####
  
  
  # AS tworzy selectInput do wyboru kontrastów na podstawie danych wybranych w rna_select_1
  output$contrast_1 <- renderUI({
    if (input$rna_select_1 == 'no data selected'){
      return(NULL)
    }
    
    data_rna <- dataselection_rnaseq_before_LHfilter()
    
    # AS wszystkie unikalne kontrasty w danych 
    grupy <- data_rna %>% filter(data_name == input$rna_select_1) %>% pull(add_variable) %>% unique()
    
    selectInput("contrast_1", "Choose contrasts for analysis",
                choices = grupy, selected = grupy[1], multiple = TRUE)
    
  })
  
  # AS tworzy selectInput do wyboru kontrastów na podstawie danych wybranych w rna_select_1
  output$contrast_2 <- renderUI({
    if (input$rna_select_2 == 'no data selected'){
      return(NULL)
    }
    
    data_rna <- dataselection_rnaseq_before_LHfilter()
    
    # AS wszystkie unikalne kontrasty w danych 
    grupy <- data_rna %>% filter(data_name == input$rna_select_2) %>% pull(add_variable) %>% unique()
    
    selectInput("contrast_2", "Choose contrasts for analysis",
                choices = grupy, selected = grupy[1], multiple = TRUE)
    
  })
  
  
  # AS tworzy selectInput do wyboru kontrastów na podstawie danych wybranych w rna_select_1
  output$contrast_3 <- renderUI({
    if (input$rna_select_3 == 'no data selected'){
      return(NULL)
    }
    
    data_rna <- dataselection_rnaseq_before_LHfilter()
    
    # AS wszystkie unikalne kontrasty w danych 
    grupy <- data_rna %>% filter(data_name == input$rna_select_3) %>% pull(add_variable) %>% unique()
    
    selectInput("contrast_3", "Choose contrasts for analysis",
                choices = grupy, selected = grupy[1], multiple = TRUE)
    
  })
  
  
  
  
  
  lower_logFC <- reactive({ input$lower_logFC })
  higher_logFC <- reactive({ input$higher_logFC })
  
  #### FILTERING GENOME DATA ####
  
  filtergenomedata <- reactive({
    
    req(changes_applied())
    
    lower <- changes_applied_lower()
    higher <- changes_applied_higher()
    
    if(switch_status() == FALSE)
    {
    plot_data_genome <- read.csv("datasets/genes_scoelicolor.txt", sep = '')
    }
    else{
      plot_data_genome <- read.csv("datasets/sven_genes_vnz.txt", sep = '')
    }
    
    plot_data_genome_filter <- plot_data_genome %>% filter(start >= lower, end <= higher)
    
    return(plot_data_genome_filter)
  })
  
 
  
  
  #### GENOMEPLOT ####
  
  genomeplot <- reactive({
    
    req(changes_applied())
    
    lower <- changes_applied_lower()
    higher <- changes_applied_higher()
    plot_data_genome <- filtergenomedata()
    plot_data_genome <- plot_data_genome %>% mutate(strand_plot = ifelse(strand == '+', 1, 0))
    plot_data_genome <- plot_data_genome %>% distinct(gene, .keep_all = TRUE)
    
    genome_plot <- ggplot(plot_data_genome, aes(
      xmin = start,
      xmax = end,
      y = "genes",
      label = gene,
      fill = strand,
      forward = strand_plot
    )) +
      geom_gene_arrow(arrowhead_height = grid::unit(10, "mm"),
                      arrow_body_height = grid::unit(8, "mm")) +
      geom_gene_label(grow = TRUE, height = grid::unit(5, "mm")) +
      scale_fill_brewer(palette = "Set3") +
      coord_cartesian(xlim = c(lower, higher), expand = FALSE) +  # Prevents ggplot from adding padding
      scale_x_continuous(expand = c(0, 0)) +  # Removes extra space on x-axis
      scale_y_discrete(expand = c(0, 0)) +  # Removes extra space on x-axis
      theme_classic() +
      theme(
        axis.title.y = element_blank(),       
        axis.text.y = element_blank(),        
        axis.ticks.y = element_blank(),       
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        axis.title = element_text(size = 16),        # Axis titles
        axis.text = element_text(size = 14),
        legend.position = "none",    # Legend title
        plot.title = element_text(size = 18, face = "bold"),  # Plot title
        strip.text = element_text(size = 14)
      )
     # theme(
      #  plot.margin = margin(5, 5, 5, 5),       # Adjust margins (top, right, bottom, left)
       # axis.title.x = element_blank(),         # Optionally remove axis labels if not necessary
        #axis.text.x = element_text(size = 10),
        #legend.position = "bottom",             # Adjust legend position to save space
        #legend.margin = margin(0, 0, 0, 0)
      #)
    return(genome_plot)
  })
  
  
  
  
  
  
  #### RNAPLOT ####
  
  RNAplot <- reactive({
    
    req(changes_applied())
    lowlogFC <- lower_logFC()
    highlogFC <- higher_logFC()
    lower <- changes_applied_lower()
    higher <- changes_applied_higher()
    plot_data_rna <- dataselection_rnaseq()
    plot_data_rna_highlog <- plot_data_rna %>% filter(logFC >= highlogFC)
    plot_data_rna_lowlog <- plot_data_rna %>% filter(logFC <= lowlogFC)
    plot_data_rna_logFC_filtered <- rbind(plot_data_rna_lowlog, plot_data_rna_highlog)
    plot_data_rna <- plot_data_rna_logFC_filtered %>% mutate(strand_plot = ifelse(strand == '-', 0, 1))
    #print(tail(plot_data_rna))
    
    
    rna_plot <- plot_data_rna %>% ggplot(aes(xmin = start, xmax = end, y = add_variable, label = gene, fill = logFC, forward = strand_plot)) +
      geom_gene_arrow(arrowhead_height = grid::unit(10, "mm"), arrow_body_height = grid::unit(8, "mm")) +
       facet_wrap(~data_name, scales = 'free', ncol = 1, strip.position = "right") +
       geom_gene_label(grow = TRUE, height = grid::unit(5, "mm")) +
      #scale_fill_gradient(low = "red", high = "blue")+
      scale_fill_distiller(palette = 'RdBu', direction = 1, limits = c(-2, 2), oob = scales::squish)+
      coord_cartesian(xlim = c(lower, higher)) +
      scale_x_continuous(expand = c(0,0))+
      #theme_genes()+
      theme_classic() +
      theme(
        axis.title.y = element_blank(),
        #axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        axis.title = element_text(size = 16),        # Axis titles
        axis.text = element_text(size = 14),         # Axis text labels
        plot.title = element_text(size = 18, face = "bold"),  # Plot title
        strip.text = element_text(size = 14)
      )
    return(rna_plot)
  })
  
  
  
  
  
  #### CHIPSEQ SELECTION ####
  
  plot_chip_edger <- read.csv("datasets/data_hupA_chipseq_edgeR.txt", sep = '')
  plot_chip_macs <- read.csv("datasets/data_hupA_chipseq_macs.txt", sep=" ") 
  
  filterCHIPedgerdata <- reactive({
    
    req(changes_applied())
    
    lower <- changes_applied_lower()
    higher <- changes_applied_higher()
    plot_data_chip_edger <- plot_chip_edger
    plot_data_chip_edger_filter <- plot_data_chip_edger %>% filter(start >= lower, end <= higher)
    return(plot_data_chip_edger_filter)
  })
  
  
  
  
  
  
  
  
  filterCHIPmacsdata <- reactive({
    
    req(changes_applied())
    
    lower <- changes_applied_lower()
    higher <- changes_applied_higher()
    plot_data_chip_macs <- plot_chip_macs
    plot_data_chip_macs_filter <- plot_data_chip_macs %>% filter(start >= lower, end <= higher)
    return(plot_data_chip_macs_filter)
  })
  
  chipedgerselect <- reactive({
    
    req(changes_applied())
    
    dane <- filterCHIPedgerdata()
    dane0.1 <- dane %>% select(start, end, best.pos, rodzaj)
    dane0.1 <- dane0.1 %>% mutate(rep = "edgeR")
    return(dane0.1)
  })
  
  chipmacsselect <- reactive({
    
    req(changes_applied())
    
    dane <- filterCHIPmacsdata()
    dane0.2 <- dane %>% select(start, end, best.pos, rodzaj)
    dane0.2 <- dane0.2 %>% mutate(rep = "macs")
    return(dane0.2)
  })
  
  
  options_chip <- c('data_hupA_chipseq_macs', 'data_hupA_chipseq_edgeR')
  
  dataselectionchipseq <- reactive({
    
    req(changes_applied())
    
    data_hupA_chipseq_macs <- chipmacsselect()
    data_hupA_chipseq_edgeR <- chipedgerselect()
    data_list <- list(data_hupA_chipseq_macs, data_hupA_chipseq_edgeR)
    choosen_data <- which(options_chip %in% input$wybor)
    choosen_data_list <- data_list[choosen_data]
    data_chip_final <- do.call(rbind, choosen_data_list)
    return(data_chip_final)
  })
  
  
  
  
  
  #### CHIP SEQ PLOT ####
  
  draw_chip_plot <- reactive({
    
    req(changes_applied())
    
    lower <- changes_applied_lower()
    higher <- changes_applied_higher()
    
    if(is.null(input$wybor)){
      return(NULL)}
    
    plot_chip_data_final <- dataselectionchipseq()
    plot_chip_data_final %>% ggplot( aes(x = start, xend = end, y = rep, yend = rep, color = best.pos)) +
      geom_segment(size = 5) +
      theme_classic() +
      scale_color_viridis_c(guide = 'none') +
      theme_classic()+
      theme(
        axis.title.y = element_blank(),       
        axis.text.y = element_blank(),        
        axis.ticks.y = element_blank(),       
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        axis.title = element_text(size = 16),        # Axis titles
        axis.text = element_text(size = 14),             # Legend title
        plot.title = element_text(size = 18, face = "bold"),  # Plot title
        strip.text = element_text(size = 14)
      )+
      coord_cartesian(xlim = c(lower, higher)) -> chip_seq_final_plot
    return(chip_seq_final_plot)
    
    
  })
  
  
  
  
  #### 'RPKM' PLOT, VOLCANO PLOT ####
  
  RPKMplot <- reactive({
    
    req(changes_applied())
    
    lower <- changes_applied_lower()
    higher <- changes_applied_higher()
    lowlogFC <- lower_logFC()
    highlogFC <- higher_logFC()
    plot_data_rna <- dataselection_rnaseq()
    plot_data_rna_highlog <- plot_data_rna %>% filter(logFC >= highlogFC)
    plot_data_rna_lowlog <- plot_data_rna %>% filter(logFC <= lowlogFC)
    plot_data_rna_logFC_filtered <- rbind(plot_data_rna_lowlog, plot_data_rna_highlog)
    rpkm_rna_plot <- ggplot(plot_data_rna_logFC_filtered, aes(x = (start+end)/2, y = logFC, xmax = (start+end)/2, ymin = 0, ymax = logFC, xmin = (start+end)/2, color = contrast, fill = contrast)) +
      theme_bw() +
      geom_point(position = position_dodge(width = 350)) +
      geom_linerange(position = position_dodge(width = 350)) +
      xlab('Genome position [bp]') +
      ylab('logFC')+
      coord_cartesian(xlim = c(lower, higher))
    
    return(rpkm_rna_plot)
  })
  
  
  Vulcanoplot <- reactive({
    
    req(changes_applied())
    
    lower <- changes_applied_lower()
    higher <- changes_applied_higher()
    lowlogFC <- lower_logFC()
    highlogFC <- higher_logFC()
    plot_data_rna <- dataselection_rnaseq()
    p_value_data <- plot_data_rna %>% mutate(logFC = ifelse(is.na(logFC), 0, logFC))
    p_value_above_logFC <- p_value_data %>% filter(logFC >= highlogFC)
    p_value_below_logFC <- p_value_data %>% filter(logFC <= lowlogFC)
    p_value_data_logFC_filtered <- rbind(p_value_below_logFC, p_value_above_logFC)
    plot_data_rna_pvalue_filtered <- p_value_data_logFC_filtered %>% mutate(pvalue_filtered = ifelse(FDR >= 0.05, 1, 2))
    vulcano_plot <- plot_data_rna_pvalue_filtered %>% ggplot(aes(x=logFC, y=-log10(PValue), group=contrast))+
      geom_point(aes(col=plot_data_rna_pvalue_filtered$pvalue_filtered, shape=contrast), size=3)
    return(vulcano_plot)
    
  })
  
  
  
  
  
  
  
  
  
  
  
  #### TABLES INPUT ####
  
  tableInput_rna <- reactive({
    
    req(changes_applied())
    
    table_data <- dataselection_rnaseq()
    lowlogFC <- lower_logFC()
    highlogFC <- higher_logFC()
    table_data_rna_lowlog <- table_data %>% filter(logFC >= highlogFC)
    table_data_rna_highlog <- table_data %>% filter(logFC <= lowlogFC)
    table_data_rna_logFC_filtered <- rbind(table_data_rna_highlog, table_data_rna_lowlog)
    return(table_data_rna_logFC_filtered)
  })
  output$rna_table <- renderDataTable({
    
    req(changes_applied())
    
    table_data <- tableInput_rna()
    return(table_data)
  })
  
  tableInput_chip <- reactive({
    
    req(changes_applied())
    
    table_data1 <- dataselectionchipseq()
    return(table_data1)
  })
  output$chip_table <- renderDataTable({
    
    req(changes_applied())
    
    table_data1 <- tableInput_chip()
    return(table_data1)
  })
  
  
  
  
  
  
  
  #### SELECTED GENE DESCRIPTION ####
  
  textInput_dataofgene <- reactive({
    
    req(changes_applied())
    
    if(input$select_gene == "all") {
      data_for_gene_info_end <- ("Sorry, no specific gene selected :/")
    }
    else{
      input$select_gene -> selected_gene
      dane1 <- read.csv("datasets/data_hupAS_RNAseq.txt", sep = '')
      dane2 <- read.csv("datasets/genes_scoelicolor.txt", sep = '')
      dane1 <- rename(dane1, gene = genes)
      zmergowane <- merge(dane1, dane2, by = "gene")
      data_for_gene_info <- zmergowane %>% filter(gene == selected_gene)
      gene_id <- data_for_gene_info %>% distinct(gene)
      gene_name <- data_for_gene_info %>% distinct(name)
      protein_name <- data_for_gene_info %>% distinct(protein_name)
      protein_product <- data_for_gene_info %>% distinct(product)
      data_for_gene_info_end <- c("gene id:", as.character(gene_id), "\n",
                                  "gene name:", as.character(gene_name), "\n",
                                  "protein name:", as.character(protein_name), "\n",
                                  "protein product:", as.character(protein_product))
    }
    return(data_for_gene_info_end)
  })
  
  output$gene_protein_data <- renderText({
    
    req(changes_applied())
    
    data_of_gene <- textInput_dataofgene()
    return(paste(data_of_gene, collapse = "\n"))
  })
  

  
  #### PATCHWORK PLOTING MAIN PLOTS ####
  
  plot_all_patchwork <- reactive({
    
    req(changes_applied())
    
    all_possible_choices <- c('genome', 'RNAplot', 'CHIPplot', 'logFCplot', 'pvalueVulcano')
    selected_plots <- input$options
    selected_number <- which(all_possible_choices %in% selected_plots)
    
    p_genome <- genomeplot()
    p_rnaplot <- RNAplot()
    p_chipplot <- draw_chip_plot()
    p_rpkmplot <- RPKMplot()
    p_vulcano_pvalue <- Vulcanoplot()
    
    plot_list <- list(`genome`= p_genome,
                      `RNAplot` = p_rnaplot,
                      `CHIPplot` = p_chipplot,
                      `logFCplot` = p_rpkmplot,
                      `pvalueVulcano` = p_vulcano_pvalue)
    
    heights <- c(1, 8, 2, 5)
    
    p_all <- patchwork::wrap_plots(plot_list[selected_number], ncol = 1, heights = heights[selected_number])
    
    return(p_all)
  })
  
  output$all_plots <- renderPlot({ plot_all_patchwork() })
  
  
  
  
  
  #### CONDITIONAL FOR CONTRASTS COMPARSION ####
  
  
  # AS tworzy selectInput do wyboru kontrastów na podstawie danych wybranych w rna_select_1
  output$contrast_venn_1 <- renderUI({
    if (input$venn_select_1 == 'no data selected'){
      return(NULL)
    }
    
    data_rna <- dataselection_venn()
    
    # AS wszystkie unikalne kontrasty w danych 
    grupy <- data_rna %>% filter(data_name == input$venn_select_1) %>% pull(add_variable) %>% unique()
    
    selectInput("contrast_venn_1", "Choose contrasts for analysis",
                choices = grupy, selected = grupy[1], multiple = TRUE)
    
  })
  
  # AS tworzy selectInput do wyboru kontrastów na podstawie danych wybranych w rna_select_1
  output$contrast_venn_2 <- renderUI({
    if (input$venn_select_2 == 'no data selected'){
      return(NULL)
    }
    
    data_rna <- dataselection_venn()
    
    # AS wszystkie unikalne kontrasty w danych 
    grupy <- data_rna %>% filter(data_name == input$venn_select_2) %>% pull(add_variable) %>% unique()
    
    selectInput("contrast_venn_2", "Choose contrasts for analysis",
                choices = grupy, selected = grupy[1], multiple = TRUE)
    
  })
  
  
  #### COMPARSION DATA LOAD ####
  
  
  ## takie już jest, nie trzeba drugi raz
#  data_loaded_rna <- reactive({
 #   if (!is.null(input$file_name)) {
  #    c("abrB1.2_table", "data_hupAS_RNAseq", "RNAseq_Martyna", "szafran2019", input$file_name)
   # } else {
    #  c("abrB1.2_table", "data_hupAS_RNAseq", "RNAseq_Martyna", "szafran2019")
  #  }
#})
  
  observe({
    # Get the current data options
    choices <- c("no data selected", data_loaded_rna())
    
    # Update all three select inputs
    updateSelectInput(session, "venn_select_1", choices = choices)
    updateSelectInput(session, "venn_select_2", choices = choices)
  })
  
  
  
  dataselection_venn <- reactive({
    
   
    
    ###tutaj dopisujesz następne jak będą
    RNAseq_Martyna <- RNAseq_Martyna_load()
    user_data <- merged_user()
    szafran2019 <- data_szafran2019_load()
    abrB1.2_table <- abrB1.2_table_load()
    data_hupAS_RNAseq <- data_hupAS_RNAseq_load()
    abrc3 <- abrc3_load() 
    aor1_rna <- aor1_rna_load()
    argR_2018 <- argR_2018_load()
    bldD_scoe <- bldD_scoe_load()
    data_bldC_sven <- data_bldC_sven_load()
    draRK_scoe <- draRK_scoe_load()
    ECF42s_sven <- ECF42s_sven_load()
    glnr_sven <- glnr_sven_load()
    hups_rnaseq_Strzalka_sven <- hups_rnaseq_Strzalka_sven_load()
    ohkA_scoe <- ohkA_scoe_load()
    osdR_2016 <- osdR_2016_load()
    sigR <- sigR_load()
    soxr_genes <- soxr_genes_load()
    whiAH_scoe <- whiAH_scoe_load()
    
    # Create a list of data frames, handling the user data separately
    data_list <- list(
      abrB1.2_table = abrB1.2_table,
      data_hupAS_RNAseq = data_hupAS_RNAseq,
      RNAseq_Martyna = RNAseq_Martyna,
      szafran2019 = szafran2019,
      abrc3 = abrc3,
      aor1_rna = aor1_rna,
      argR_2018 = argR_2018,
      bldD_scoe = bldD_scoe,
      data_bldC_sven = data_bldC_sven,
      draRK_scoe = draRK_scoe,
      ECF42s_sven = ECF42s_sven,
      glnr_sven = glnr_sven,
      hups_rnaseq_Strzalka_sven = hups_rnaseq_Strzalka_sven,
      ohkA_scoe = ohkA_scoe,
      osdR_2016 = osdR_2016,
      sigR = sigR,
      soxr_genes = soxr_genes,
      whiAH_scoe = whiAH_scoe
    )
    
    # Add user data to the list if it exists
    if (!is.null(user_data)) {
      data_list[[input$file_name]] <- user_data
    }
    
    # Get selected datasets
    selected_datasets <- c(input$venn_select_1, input$venn_select_2)
    selected_datasets <- selected_datasets[selected_datasets != "no data selected"]
    
    # Filter and combine the selected datasets
    selected_data <- data_list[selected_datasets]
    data_rna_final <- do.call(rbind, selected_data)
    
    return(data_rna_final)
  })
  
  
  
  #### FILTERING DATA FOR VENN AND HEAT #####
  
  filter_data_for_heatmap <- reactive({
    data_rna <- dataselection_venn()
    data_rna <- data_rna %>% filter(gene %in% c(input$select_gene_venn), add_variable %in% c(input$contrast_venn_1, input$contrast_venn_2))
    return(data_rna)
  })
  

  
  filter_data_for_venn <- reactive({
    higher_logFC <- input$higher_logFC_venn
    lower_logFC <- input$lower_logFC_venn
    data_rna <- dataselection_venn()
    data_rna1 <- data_rna %>% filter(logFC >= higher_logFC, add_variable %in% c(input$contrast_venn_1, input$contrast_venn_2))
    data_rna2 <- data_rna %>% filter(logFC <= lower_logFC)
    data_rna_filtered <- rbind(data_rna1, data_rna2)
      data_rna_filtered <- data_rna_filtered %>% filter(FDR <= 0.05)
      return(data_rna_filtered)
    
  })
  
  
  
  prep_data_venn <- reactive({
    data_set_venn <- filter_data_for_venn()
    gene_lists <- split(data_set_venn$gene, data_set_venn$data_name)
    return(gene_lists)
  })
  
  output$venn_plot <- renderPlot({
    gene_lists <- prep_data_venn()
    
    ggvenn(
      gene_lists,
      fill_color = c("#0073C2FF", "#EFC000FF"),
      stroke_size = 0.5,
      set_name_size = 4
    )
  })
  
  
  data_venn_table <- reactive({
  list_venn <- prep_data_venn()
  df_venn <- filter_data_for_venn()
  
  list1 <- list_venn[[1]]
  list2 <- list_venn[[2]]
  
  venn_table_same_genes <- intersect(list1, list2)
  
  filtered_genes_venn <- df_venn %>% filter(gene == venn_table_same_genes)
  print(filtered_genes_venn)
  })
  
  
  
 
  tableInput_venn <- reactive({
    
    req(changes_applied())
    
    table_data1 <- data_venn_table()
    return(table_data1)
  })
  output$venn_table <- renderDataTable({
    
    req(changes_applied())
    
    table_data1 <- tableInput_venn()
    return(table_data1)
  })
  
  
  
  #### HEATMAP ####
  
  output$heatmap_plot <- renderPlot({
    
    req(changes_applied())
    
    lowlogFC <- input$lower_logFC_venn
    highlogFC <- input$higher_logFC_venn
    heat_data <- filter_data_for_heatmap()
    heat_data <- heat_data %>% mutate(logFC = ifelse(is.na(logFC), 0, logFC))
    heat_above_logFC <- heat_data %>% filter(logFC >= highlogFC)
    heat_below_logFC <- heat_data %>% filter(logFC <= lowlogFC)
    heat_data_logFC_filtered <- rbind(heat_below_logFC, heat_above_logFC)
    tidyHeatmap::heatmap(.data = dplyr::tibble(heat_data_logFC_filtered),
                         .row = gene,
                         .column = add_variable,
                         .value = logFC,
                         palette_value = circlize::colorRamp2(
                           seq(-5, 5, length.out = 11),
                           RColorBrewer::brewer.pal(11, "RdBu"))) -> p_heat
    print(p_heat)
  })
  
  
  
}