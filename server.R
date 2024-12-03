#### SERVER #####

server <- function(input, output, session) {
  
  # Create a reactive value to store the trigger state
  changes_applied <- reactiveVal(FALSE)  # This will store whether the button has been pressed

  # Observe the action button press
  observeEvent(input$apply_changes, {
    changes_applied(TRUE)  # Set the value to TRUE when the button is pressed
  })

  changes_applied_lower <- eventReactive(input$apply_changes, {
    input$lower_value
  }, ignoreNULL = FALSE)
  
  changes_applied_higher <- eventReactive(input$apply_changes, {
    input$higher_value
  }, ignoreNULL = FALSE)
  
  plotgenomeInput <- reactive({
    
    req(changes_applied())
    
    if(input$select_gene == "all") {
      plot_data <- read.csv("datasets/genes_scoelicolor.txt", sep = '')
    } else {
      input$select_gene -> selected_gene
      plot_data <- read.csv("datasets/genes_scoelicolor.txt", sep = '')
      plot_data <- plot_data %>% filter(gene == selected_gene)
    }
    return(plot_data)
  })
  
  user_data_upload <- reactive({
    
    req(changes_applied())
    
    user_file <- input$uploaded_file$datapath
    if(is.null(user_file)){
      return(NULL)}
    else
    {
      user_file <- read.csv(user_file, sep = "\t")
      user_file$add_variable <- "user_file"
      user_file$data_name <- "user_file"
      return(user_file)
    }
  })
  
  merged_user <- reactive({
    
    req(changes_applied())
    
    user_data <- user_data_upload()
    print(class(user_data))
    data_genome <- plotgenomeInput()
    print(class(data_genome))
    
    if (is.null(user_data)) {
      return(NULL)
    } else {
      
      merged_data <- user_data %>%
        left_join(data_genome, by = "gene")
      
      return(merged_data)
    }
  })
  
  data_loaded_rna <- c("abrB1.2_table4", "abrB1.2_table5", "data_hupAS_RNAseq", "user_uploaded_file")
  
  dataselection_rnaseq_before_LHfilter <- reactive({
    
    req(changes_applied())
    
    ###tutaj dopisujesz następne jak będą
    user_uploaded_file <- merged_user()
    abrB1.2_table4 <- abrB1.2_table4_load()
    abrB1.2_table5 <- abrB1.2_table5_load()
    data_hupAS_RNAseq <- data_hupAS_RNAseq_load()
    data_list <- list(abrB1.2_table4, abrB1.2_table5, data_hupAS_RNAseq, user_uploaded_file)
    choosen_data <- which(data_loaded_rna %in% c(input$rna_select_1, input$rna_select_2, input$rna_select_3))
    choosen_data_list <- data_list[choosen_data]
    data_rna_final <- do.call(rbind, choosen_data_list)
    
    return(data_rna_final)
  })
  
  
  
 
  ##LOWER/HIGHERVALUE
  
  
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
  
  higher_value <- reactive({
    input$higher_value
    
  })
  
  
  
  observeEvent(input$select_gene,{
    if(input$select_gene != 'all'){
      updateNumericInput(
        session = session, inputId = "higher_value", value = plotgenomeInput()$end
      )
    }
  })
  
  
  
  dataselection_rnaseq <- reactive({
    
    req(changes_applied())
    
    data_rna <- dataselection_rnaseq_before_LHfilter()
    lower <- changes_applied_lower()
    higher <- changes_applied_higher()
    data_rna <- data_rna %>% filter(start >= lower, end <= higher)
    return(data_rna)
    })
  
  checkbox_list <- reactive({
    data_rna <- dataselection_rnaseq()
    data_list <- list(table(data_rna$add_variable))
    return(data_list)
  })
  
  
  lower_logFC <- reactive({ input$lower_logFC })
  higher_logFC <- reactive({ input$higher_logFC })
  
  #### FILTERING DATA ####
  
  filtergenomedata <- reactive({
    
    req(changes_applied())
    
    lower <- changes_applied_lower()
    higher <- changes_applied_higher()
    
    plot_data_genome <- read.csv("datasets/genes_scoelicolor.txt", sep = '')
    plot_data_genome_filter <- plot_data_genome %>% filter(start >= lower, end <= higher)
    
    return(plot_data_genome_filter)
  })
  
 
  #### PLOTS CODE ####
  
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
      geom_gene_label(align = "left") +
      scale_fill_brewer(palette = "Set3") +
      coord_cartesian(xlim = c(lower, higher), expand = FALSE) +  # Prevents ggplot from adding padding
      scale_x_continuous(expand = c(0, 0)) +  # Removes extra space on x-axis
      theme_classic() +
      theme(
        plot.margin = margin(5, 5, 5, 5),       # Adjust margins (top, right, bottom, left)
        axis.title.x = element_blank(),         # Optionally remove axis labels if not necessary
        axis.text.x = element_text(size = 10),
        legend.position = "bottom",             # Adjust legend position to save space
        legend.margin = margin(0, 0, 0, 0)
      )
    return(genome_plot)
  })
  
  RNAplot <- reactive({
    
    req(changes_applied())
    
    lower <- changes_applied_lower()
    higher <- changes_applied_higher()
    plot_data_rna <- dataselection_rnaseq()
    plot_data_rna <- plot_data_rna %>% mutate(strand_plot = ifelse(strand == '+', 1, 0))
    
    
    rna_plot <- plot_data_rna %>% ggplot(aes(xmin = start, xmax = end, y = add_variable, label = gene, fill = logFC, forward = strand_plot)) +
      geom_gene_arrow(arrowhead_height = grid::unit(10, "mm"), arrow_body_height = grid::unit(8, "mm")) +
      facet_wrap(~data_name, scales= 'free', ncol = 1) +
      geom_gene_label(align = "left") +
      scale_fill_gradient(low = "red", high = "blue")+
      coord_cartesian(xlim = c(lower, higher), expand = FALSE) +
      scale_x_continuous(expand = c(0,0))+
      theme_classic()+
       theme(
        plot.margin = margin(5, 5, 5, 5),       # Adjust margins (top, right, bottom, left)
        axis.title.x = element_blank(),         # Optionally remove axis labels if not necessary
        axis.text.x = element_text(size = 10),
        legend.position = "bottom",             # Adjust legend position to save space
        legend.margin = margin(0, 0, 0, 0)
      )
    return(rna_plot)
  })
  
  
  ####walkazchipseqwybor####
  
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
  
  
  options_chip <- c('edgeR', 'macs')
  
  dataselectionchipseq <- reactive({
    
    req(changes_applied())
    
    macs_data <- chipmacsselect()
    edger_data <- chipedgerselect()
    data_list <- list(macs_data, edger_data)
    choosen_data <- which(options_chip %in% input$wybor)
    choosen_data_list <- data_list[choosen_data]
    data_chip_final <- do.call(rbind, choosen_data_list)
    return(data_chip_final)
  })
  
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
      coord_cartesian(xlim = c(lower, higher)) -> chip_seq_final_plot
    return(chip_seq_final_plot)
    
    
  })
  
  
  
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
  
  
  #### HEATMAP? ####
  
  output$heatmap <- renderPlot({
    
    req(changes_applied())
    
    lowlogFC <- lower_logFC()
    highlogFC <- higher_logFC()
    heat_data <- dataselection_rnaseq()
    heat_data <- heat_data %>% mutate(logFC = ifelse(is.na(logFC), 0, logFC))
    heat_above_logFC <- heat_data %>% filter(logFC >= highlogFC)
    heat_below_logFC <- heat_data %>% filter(logFC <= lowlogFC)
    heat_data_logFC_filtered <- rbind(heat_below_logFC, heat_above_logFC)
    tidyHeatmap::heatmap(.data = dplyr::tibble(heat_data_logFC_filtered),
                         .row = gene,
                         .column = contrast,
                         .value = logFC,
                         palette_value = circlize::colorRamp2(
                           seq(-5, 5, length.out = 11),
                           RColorBrewer::brewer.pal(11, "RdBu"))) -> p_heat
    print(p_heat)
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
  
  output$genome_plot <- renderPlot({
    genome_plot_output <- genomeplot()
    return(genome_plot_output)
  })
  
  plot_all_patchwork <- reactive({
    
    req(changes_applied())
    
    all_possible_choices <- c('RNAplot', 'CHIPplot', 'logFCplot', 'pvalueVulcano')
    selected_plots <- input$options
    selected_number <- which(all_possible_choices %in% selected_plots)
    
    p_rnaplot <- RNAplot()
    p_chipplot <- draw_chip_plot()
    p_rpkmplot <- RPKMplot()
    p_vulcano_pvalue <- Vulcanoplot()
    
    plot_list <- list(`RNAplot` = p_rnaplot,
                      `CHIPplot` = p_chipplot,
                      `logFCplot` = p_rpkmplot,
                      `pvalueVulcano` = p_vulcano_pvalue)
    
    heights <- c(10, 5, 10, 10)
    
    p_all <- patchwork::wrap_plots(plot_list[selected_number], ncol = 1, heights = heights[selected_number])
    
    return(p_all)
  })
  
  output$all_plots <- renderPlot({ plot_all_patchwork() })
  
}