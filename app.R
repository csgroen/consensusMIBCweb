library(shiny)
library(shinyWidgets)

options(shiny.maxRequestSize = 50*1024^2)


# Define UI for app that draws a histogram ----
ui <- fluidPage(
    tags$head(
        tags$style(HTML("hr {border-top: 10px solid #d3d3d3;}
                        .clickable {color: #428bca}"))
    ),
    # App title ----
    titlePanel("Consensus Classification of Muscle-Invasive Bladder Cancer"),
    
    # Sidebar ----
    sidebarLayout(
        sidebarPanel(
            # Instructions
            h3("Help"),
            switchInput(inputId = "showInfo",
                        value = TRUE,
                        onLabel = "Show",
                        offLabel = "Hide",
                        size = "small", inline = TRUE, offStatus = "grey30"),
            hr(),
            
            h3("Classify Samples"),
            # Data input
            fileInput(inputId = "inFile",
                      label = "Sample input",
                      accept = c(
                          "text/csv",
                          "text/comma-separated-values,text/plain",
                          ".csv", "text/tsv",
                          ".tsv", ".xls", ".xlsx")),
            
            # Use example?
            checkboxInput(inputId = "useExample",
                          label = "Use example data? (TCGA-BLCA cohort)"),
            
            # Gene type
            radioButtons(inputId = "idType", 
                         label = "Gene ID Type",
                         choiceNames = c("ENTREZ ID", "HGNC Symbol", "ENSEMBL Gene ID"),
                         choiceValues = c("entrezgene", "hgnc_symbol", "ensembl_gene_id")
                         ),
            
            actionButton(inputId = "classify",
                         label = "Classify"),
            
            br(),
            br(),
            # Mincor
            h4("For advanced users"),
            sliderInput(inputId = "minCor",
                        label = "Minimum correlation",
                        min = 0,
                        max = 1,
                        value = 0.2)
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            # Instructions
            conditionalPanel("input.showInfo",
                             h2("Welcome!"),
                             HTML("<p>This web application is an implementation of the <b>consensusMIBC</b>
                               R package.</p> 
                               
                               <h3>Classification of MIBC samples provided by the user</h3>
                               
                               <p>Users can input new data to the app using the 
                               <b>Sample input selector</b>.</p>
                               
                               <p>The application requires a gene expression table with
                               <b>Samples</b> to be classified in the columns and <b>Gene IDs</b> 
                               in the rows. The first row of the input will be used as sample names 
                               and the first column will be used as gene IDs. The sidebar of the app
                               shows supported gene identifiers under <b class='clickable'>Gene ID Type</b>.
                               <p>For file input, we currently support Excel files <b>(.xlsx, .xls)</b>, 
                               Comma-separated values <b>(.csv, .txt)</b> and Tab-separated values 
                                <b>(.tsv, .txt)</b>.
                                <h3>Example</h3>
                                <p>To see an example of the results, please tick the
                               <b class='clickable'>Use example data?</b> option in the sidebar and run the 
                               application by clicking the <b class = 'clickable'>Classify</b> button.</p>
                               <h3>Note on Separation Level</h3>
                                The Separation Level shown in the boxplot gives a measure of how a sample is 
                                representative of its consensus class. It ranges from 0 to 1, with 0 meaning 
                                the sample is too close to other consensus classes to be confidently assigned 
                                one consensus class label, and 1 meaning the sample is highly representative 
                                of its consensus class and well separated from the other consensus classes. 
                               <hr><br>")),
            
            # Table title and download button
            fluidRow(
                column(8),
                column(4,
                       radioButtons(inputId = "downloadType", 
                                    label = "", 
                                    choices = c("xlsx", "tsv", "csv"),
                                    inline = TRUE)),
                align = "right"
            ),
            fluidRow(
                column(10,
                       htmlOutput("tabTitle")),
                column(2,
                       downloadButton("download", "Download results"), align = "right")
            ),
            
            # Output: Table
            dataTableOutput(outputId = "classTab"),
            
            # Output Plots
            htmlOutput("vizTitle"),
            fluidRow(
                #-- Barplot
                column(width = 7,
                       plotOutput("barplot", height = "350px")
                       ),
                #-- Boxplot
                column(width = 5,
                       plotOutput("boxplot", height = "350px")
                )
            )
        )
    )
)

server <- function(input, output, session) {
    
    library(consensusMIBC)
    library(tidyverse)
    library(readxl)
    library(openxlsx)
    
    #-- Create the classification table
    classes <- eventReactive(input$classify, {
        #-- Use Example
        if (input$useExample) {
            updateSwitchInput(session, inputId = "showInfo", value = FALSE)
            data(tcgadat)
            classes <- getConsensusClass(tcga.dat, minCor = input$minCor, gene_id = "entrezgene") %>%
                rownames_to_column("Sample ID")
        } else if (!is.null(input$inFile$datapath)) {
            
            
            #-- Input their own data
            updateSwitchInput(session, inputId = "showInfo", value = FALSE)
            if(str_detect(input$inFile$datapath, ".xls$|.xlsx$")) {
                udata <- read_excel(input$inFile$datapath, sheet = 1, col_names = TRUE)
            } else if(str_detect(input$inFile$datapath, ".tsv$")) {
                udata <- read_tsv(input$inFile$datapath)
            } else if(str_detect(input$inFile$datapath, ".csv$")) {
                udata <- read_csv(input$inFile$datapath)
            } else {
                udata <- read_table(input$inFile$datapath)
            }
            
            udata <- udata %>%
                as.data.frame() %>%
                tibble::column_to_rownames(colnames(udata)[1])
            
            #-- Check if the user's choice of ID is adequate
            validate(need(any(rownames(udata) %in% centroids[,input$idType]), 
            "We were unable to match gene IDs!
Please check the Gene ID Type options to see if it matches the gene IDs provided."))
            
            
            classes <- getConsensusClass(udata, minCor = input$minCor, gene_id = input$idType) %>%
                rownames_to_column("Sample ID")
            
        } else { #-- Blank
            NULL
        }
    })
    
    #-- Table title
    output$tabTitle <- renderText(if(!is.null(classes())) {
        "<h3>Classification results</h3><br>"
    })
    
    #-- Table output
    output$classTab <- renderDataTable(if(!is.null(classes())) {
        select(classes(), `Sample ID`, `Consensus Class` = consensusClass, 
               `Separation Level` = separationLevel,
               `P-value (cor)` = cor_pval)  
    }, options = list(pageLength = 5))
    
    
    #-- Plot title
    output$vizTitle <- renderText(if(!is.null(classes())) {
        "<h3>Visualization</h3><br>"
    })
    
    #-- Plot utils
    consensusColors <- c("LumP" = "#10ff66", "LumNS" = "#248b22",
                         "LumU" = "#0066ff", "Stroma-rich" = "#e0fa49",
                         "Ba/Sq" = "#ff0000", "NE-like" = "#c504f7",
                         "Unclassified" = "grey60")
    
    
    #-- Barplot output
    output$barplot <- renderPlot(if(!is.null(classes())) {
        plot_data <- classes() %>%
            mutate(consensusClass = factor(consensusClass, 
                                           levels = c("LumP", "LumNS", "LumU", 
                                                      "Stroma-rich", "Ba/Sq", "NE-like")),
                   consensusClass = forcats::fct_explicit_na(consensusClass, na_level = "Unclassified")) %>%
            group_by(consensusClass, .drop=FALSE) %>%
            summarize(n = n())
        ggplot(plot_data, aes(consensusClass, n, fill = consensusClass)) +
            geom_bar(stat = "identity") +
            geom_text(aes(label=n), vjust=-0.5) +
            theme_light() +
            scale_fill_manual(values = consensusColors) +
            labs(x = "Consensus Class", y = "Number of Samples") +
            guides(fill=guide_legend(title = "Consensus Class")) +
            theme(axis.text = element_text(size = 12),
                  axis.title = element_text(size = 14),
                  legend.text = element_text(size = 12))
    })
    
    #-- Boxplot output
    output$boxplot <- renderPlot(if(!is.null(classes())) {
        plot_data2 <- classes() %>%
            mutate(consensusClass = factor(consensusClass, 
                                           levels = c("LumP", "LumNS", "LumU", 
                                                      "Stroma-rich", "Ba/Sq", "NE-like")),
                   consensusClass = forcats::fct_explicit_na(consensusClass, na_level = "Unclassified")) %>%
            select(consensusClass, separationLevel)
        
        ggplot(plot_data2, aes(consensusClass, separationLevel, fill = consensusClass)) +
            geom_boxplot() +
            labs(x = "Consensus Class", y = "Separation Level") +
            ylim(0,1) +
            scale_fill_manual(values = consensusColors) +
            guides(fill=FALSE) +
            theme_light() +
            theme(axis.text = element_text(size = 12),
                  axis.title = element_text(size = 14),
                  legend.text = element_text(size = 12))
    })
    
    #-- Download 

    output$download <- downloadHandler(filename = 
                                           function() {
                                               paste0("MIBCclassificationResults_", 
                                                      Sys.Date(), ".", input$downloadType)
                                               },
                                       content = 
                                           # function (con) { write_tsv(classes(), con) }
                                           function(file) {
                                               switch(input$downloadType,
                                                      tsv = write_tsv(classes(), file),
                                                      csv = write_csv(classes(), file),
                                                      xlsx = write.xlsx(classes(), file)
                                                    )
                                           }
                                       )
    
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
