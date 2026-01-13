library(shiny)
library(shinyjs) #PACOTE ADICIONADO EM 18/02/2025 PARA ABERTURA DE LINKS NO SHINY
library(shinydashboard)
library(smotefamily)
library(readxl)
library(writexl)  
library(DT)
library(ggplot2)
library(reticulate)
library(shinycssloaders)
library(plotly)
library(UBL)
library(caret)
library(imbalance)
library(tibble)
#library(Imbalanced-learn)
#reticulate::py_install("imbalanced-learn")
#library(psych) #PACOTE PARA SUMMARIZAÇÃO MAIS COMPLETA DOS DADOS EM 17/01/2025
library(skimr) # PACOTE PARA DESCRIÇÃO DOS DADOS EM 21/01/2025
library(rrcov)  #PACOTE PCA ROBUSTA ACRÉSCIMO EM 17/01/2025
library(mdatools) #PACOTE PCA ACRÉSCIMO EM 17/02/2025
library(Rtsne) #PACOTE TSNE ACRÉSCIMO EM 17/01/2025
library(reshape2)  # PACOTE PARA MANIPULAÇÃO DE DADOS ACRÉSCIMO EM 17/01/2025
library(car)  # PACOTE PARA LEVENE'S TEST ACRÉSCIMO EM 17/01/2025
library(stats)  # PACOTE PARA BARTLETT'S TEST ACRÉSCIMO EM 17/01/2025
library(openxlsx) #PACOTE PARA EXPORTAÇÃO DE RESULTADOS EM EXCEL ACRÉSCIMO EM 15/02/2025
library(purrr) #ADICIONADO EM 24/02/2025 PARA TORNAR BARPLOT INTERATIVO EM DATA OVERVIEW
library(RColorBrewer) #ADICIONADO EM 24/02/2025 PARA TORNAR BARPLOT INTERATIVO EM DATA OVERVIEW
library(ellipse) #ADICIONADO EM 12/03/2025
library(MASS) #ADICIONADO EM 21/03/2025
library(robustbase) #ADICIONADO EM 21/03/2025
library(rgl)
library(tidyr)#ACRÉSCIMO EM 27/03/2025 PARA COMANDO PIVOT_LONGER
library(shinyWidgets) #ACRÉSCIMO EM 27/03/2025
library(philentropy) # ACRÉSCIMO EM 28/03/2025(Similarity and Distance Quantification Between Probability Functions)
library(Hotelling)  # ACRÉSCIMO PARA T² DE HOTELLING EM 29/03/2025
library(vegan) #ACRÉSCIMO PARA Generalized Procrustes Analysis
library(MVN) #ACRÉSCIMO em 30/03/2025 PARA teste de Normality multivariada: teste de Henze-Zirkler.
library(dplyr) #ACRÉSCIMO EM 04/04/2025 PARA MÉDIAS DO PLOT COMPARATIVO


##PACOTES PARA CRIAÇÃO DO EXECUTÁVEL EM 10/04/2025

library(RInno)
library(remotes)
library(devtools)

#####ACRÉSCIMO DE PACOTE EM 10/07/2025 PARA RODAR MÉTODO SBC
#"Pacote bimba — RomeroBarata/bimba"
#install.packages("remotes")
#remotes::install_github("RomeroBarata/bimba")

##PACOTE BIMBA USADO PARA DUAS CLASSES SOMENTE

library(scutr) #ADICIONADO EM 10/07/2025 PARA UNDERSAMPLING BASED ON CLUSTERING


#####ACRÉSCIMO DE PACOTE EM 10/07/2025 PARA RODAR MÉTODO SBC


###ACRÉSCIMO DE PACOTE "SMOTE_VARIANTS" PARA RODAR SPIDER E SMOTE IPF EM 11/07/2025

##REFERÊNCIAS: https://smote-variants.readthedocs.io/en/stable/installation.html

# https://thepythoncode.com/article/handling-imbalance-data-imblearn-smote-variants-python


options(shiny.maxRequestSize = 1000 * 1024^2)

ui <- dashboardPage(
  title = "Synthetic Sampling App",
  dashboardHeader(title = "LTAP_UERJ - Synthetic Sampling App", titleWidth = "400"),
  dashboardSidebar(
    
    #######ADICIONADO EM 24/02/2025
    
    #   selectizeInput("selected_classes", "Select Classes:", choices = NULL, multiple = TRUE)
    #  ),
    #######ADICIONADO EM 24/02/2025
    
    shiny::hr(),
    sidebarMenu(
      
      menuItem(tabName = "importtab", text = "Import Data", icon = icon("import", lib = "glyphicon")),
      menuItem(tabName = "fastprepro", text = "Pre-processing", icon = icon("filter", lib = "glyphicon")),
      menuItem(
        tabName = "synsamples",
        text = "Methods",
        icon = icon("list", lib = "glyphicon"),
        menuItem(
          tabName = "up",
          text = "Upsampling",
          icon = icon("plus-sign", lib = "glyphicon"),
          menuSubItem(
            text = "SMOTE",
            tabName = "smote",
            icon = icon("menu-right", lib = "glyphicon")),
          menuSubItem(
            #   text = "SMOTE-NC",
            #  tabName = "smote-nc",
            
            
            text = "SMOTE_NC", #ALTERADO PARA EVITAR ERRO DE SINTAXE EM 08/05/2025
            tabName = "smote_nc", #ALTERADO PARA EVITAR ERRO DE SINTAXE EM 08/05/2025
            
            
            icon = icon("menu-right", lib = "glyphicon")),
          menuSubItem(
            # text = "Borderline-SMOTE", 
            #  tabName = "borderline-smote", 
            
            text = "Borderline_SMOTE", #ALTERADO PARA EVITAR ERRO DE SINTAXE EM 02/06/2025
            tabName = "borderline_smote",#ALTERADO PARA EVITAR ERRO DE SINTAXE EM 02/06/2025
            
            icon = icon("menu-right", lib = "glyphicon")),
          menuSubItem(
            #text = "SVM-SMOTE",
            #tabName = "svm-smote",
            
            text = "SVM_SMOTE", #ALTERADO PARA EVITAR ERRO DE SINTAXE EM 06/06/2025
            tabName = "svm_smote", #ALTERADO PARA EVITAR ERRO DE SINTAXE EM 06/06/2025
            
            icon = icon("menu-right", lib = "glyphicon")),
          menuSubItem(
            text = "ADASYN",
            tabName = "adasyn",
            icon = icon("menu-right", lib = "glyphicon")),
          menuSubItem(
            text = "Random Upsampling",
            tabName = "ru",
            icon = icon("menu-right", lib = "glyphicon"))
        ),
        menuItem(
          tabName = "down",
          text = "Downsampling",
          icon = icon("minus-sign", lib = "glyphicon"),
          menuSubItem(
            #text = "TOMEK Links",
            text = "TOMEKLinks",  #ALTERAÇÃO DE NOME EM 09/06/2025
            tabName = "tl",
            icon = icon("menu-right", lib = "glyphicon")),
          menuSubItem(
            text = "NearMiss",
            tabName = "nm",
            icon = icon("menu-right", lib = "glyphicon")),
          menuSubItem(
            text = "Edited Nearest Neighbours",
            tabName = "enn",
            icon = icon("menu-right", lib = "glyphicon")),
          menuSubItem(
            text = "One Sided Selection",
            tabName = "oss",
            icon = icon("menu-right", lib = "glyphicon")),
          menuSubItem(
            text = "Random Downsampling",
            tabName = "rd",
            icon = icon("menu-right", lib = "glyphicon")),
        
          ###ACRÉSCIMO DE UNDERSAMPLING BASED ON CLUSTERING EM 08/07/2025
          menuSubItem(
            text = "Based on Clustering",
            tabName = "sbc",
            icon = icon("menu-right", lib = "glyphicon"))
          
          
          ###ACRÉSCIMO DE UNDERSAMPLING BASED ON CLUSTERING EM 08/07/2025
          
          
          ),
        menuItem(
          tabName = "hybrid",
          text = "Hybrid methods",
          icon = icon("retweet", lib = "glyphicon"),
          menuSubItem(
            #text = "SMOTE-TL",
            #tabName = "smote-tl",
            text = "SMOTE_TL", #ALTERADO EM 18/06/2025
            tabName = "smote_tl", #ALTERADO EM 18/06/2025
            icon = icon("menu-right", lib = "glyphicon")),
          menuSubItem(
            #  text = "SMOTE-ENN", 
            #  tabName = "smote-enn",
            
            text = "SMOTE_ENN", # MODIFICADO EM 03/07/2025
            tabName = "smote_enn", # MODIFICADO EM 03/07/2025
            
            icon = icon("menu-right", lib = "glyphicon")),
          
          menuSubItem(
         #   text = "SMOTE-IPF",
        #    tabName = "smote-ipf",
            
            
            text = "SMOTE_IPF", # MODIFICADO EM 11/07/2025
            tabName = "smote_ipf", # MODIFICADO EM 11/07/2025
            
            icon = icon("menu-right", lib = "glyphicon")),
          
          #ACRÉSCIMO DO MÉTODO HIBRIDO "SPIDER" EM 15/01/2025
          
          menuSubItem(
            text = "SPIDER",
            tabName = "spider",
            icon = icon("menu-right", lib = "glyphicon"))
          
        )
      ),
      
      
      #ALTERAÇÃO EM 10/02/2025 DE GUIA DE "Exploratory analysis" para "Dimensionality reduction"
      
      #menuItem(tabName = "redtab", text = "Exploratory analysis",
      
      
      ##RETIRADA DA GUIA REDUÇÃO DA DIMENSIONALIDADE EM 28/03/2025 JÁ INCLUSA EM DIAGNÓSTICO
      
      ##    menuItem(tabName = "redtab", text = "Dimensionality reduction",
      ##           icon = icon("wrench", lib = "glyphicon"),
      ##         menuSubItem(tabName = "pcatab", text = "PCA", 
      ##                   icon = icon("signal", lib = "glyphicon")),
      
      #ACRÉSCIMO EM 17/01/2025 PROJEÇÃO DE DADOS EM Dimension REDUZIDA(PCA ROBUSTA E T-SNE)
      
      ##     menuSubItem(tabName = "robpcatab", text = "Robust PCA", icon = icon("signal", lib = "glyphicon")),
      ##   menuSubItem(tabName = "tsnetab", text = "t-SNE", icon = icon("signal", lib = "glyphicon"))),
      
      
      
      
      
      ####ACRÉSCIMO EM 17/01/2025 DE "VISUALIZAÇÕES" E "RESULTADOS"  
      
      ##MODIFICAÇÃO DA GUIA "VISUALIZAÇÕES" PARA "DIAGNÓSTICO" em 27/03/2025
      
      # menuItem("Visualizações", tabName = "visual", icon = icon("chart-bar")),
      menuItem("Diagnostic", tabName = "visual", icon = icon("chart-bar")),
      
      
      menuItem("Results", tabName = "results", icon = icon("table")),
      
      #menuItem(
      # tabName = "linkstab",
      #text = "About the Interface",
      #icon = icon("info-sign", lib = "glyphicon"),
      
      ##########MODIFICADO EM 18/02/2025#################
      menuItem("About the Interface", tabName = "linkstab", icon = icon("info-sign", lib = "glyphicon"),
               #menuSubItem("LEAMS Group", tabName = "leams", icon = icon("link", lib = "glyphicon")),
               
               #MODIFICAÇÃO DE LEAMS PARA LTAP EM 20/02/2025
               menuSubItem("LTAP Group", tabName = "ltap", icon = icon("link", lib = "glyphicon")),
               
               menuSubItem("UERJ Institute of Chemistry", tabName = "uerj", icon = icon("link", lib = "glyphicon")),
               menuSubItem("Our GitHub", tabName = "github", icon = icon("github", lib = "font-awesome")
                           
               ),
               
               #OBS: A OUTRA PARTE FOI MOVIDA PARA "dashboardBody(" ABAIXO EM 18/02/2025
               ##########MODIFICADO EM 18/02/2025################# 
               
               menuSubItem(tabName = "creditstab", text = "Credits", icon = icon("user", lib = "glyphicon")),
               menuSubItem(tabName = "referencestab", text = "References", icon = icon("list", lib = "glyphicon"))),
      menuItem(
        tabName = "otherinterfaces",
        text = "Other interfaces by LTAP",
        icon = icon("plus-sign", lib = "glyphicon"),
        menuSubItem(
          text = "Data Handling",
          href = 'https://ltap.shinyapps.io/data_handling/',
          icon = icon("link", lib = "glyphicon")
        ),
        menuSubItem(
          text = "Dimensionality Reduction",
          href = 'https://ltap.shinyapps.io/dimensionality_reduction/',
          icon = icon("link", lib = "glyphicon")
        )
      )
    )
  ),
  dashboardBody(
    
    useShinyjs(),  # Necessário para abrir links no navegador em 18/02/2025
    tabItems(
      
      ######ADICIONADO PARA ABRIR LINKS EM 18/02/2025####
      
      # tabItem(tabName = "leams",
      #        h2("LEAMS Group"),
      #       tags$iframe(src = "https://www.leamsuerj.com/home", 
      #                  width = "100%", height = "800px", frameborder = 0)
      # ),
      
      #MODIFICAÇÃO de leams pata ltap em 20/02/2025 (NÃO ABRIU DIRETO COM SHINY)
      
      # tabItem(tabName = "ltap",
      #        h2("LTAP Group"),
      #       tags$iframe(src = "https://www.ltapuerj.com.br", 
      #                  width = "100%", height = "800px", frameborder = 0)
      # ),
      
      tabItem(tabName = "ltap",
              h2("LTAP Group"),
              p("Access the LTAP Group website by clicking the link below:"),
              #tags$a(href = "https://www.ltapuerj.com.br", target = "_blank", "Clique aqui para acessar o LTAP Group")
              
              ##MODIFICADO EM 13/04/2025
              tags$a(href = "https://www.ltapuerj.com.br", target = "_blank", "https://www.ltapuerj.com.br")
      ),
      
      
      #MODIFICADO EM 20/02/2025(IQ UERJ NÃO ABRIU DIRETO)
      
      #tabItem(tabName = "uerj",
      #       h2("UERJ Institute of Chemistry"),
      #      tags$iframe(src = "http://www.iq.uerj.br/", 
      #                 width = "100%", height = "800px", frameborder = 0)
      #),
      
      
      tabItem(
        tabName = "uerj",
        h2("UERJ Institute of Chemistry"),
        
        p("Access the UERJ Institute of Chemistry website here:"),
        p(a(href = "http://www.iq.uerj.br/", 
            ## "Click here to visit the UERJ Institute of Chemistry", 
            ##  target = "_blank"))
            
            ##MODIFICADO EM 13/04/2025
            "http://www.iq.uerj.br/", 
            target = "_blank"))
      ),
      
      
      
      #MODIFICADO EM 20/02/2025(GITHUB NÃO ABRIU DIRETO)
      
      
      #tabItem(tabName = "github",
      #       h2("Our GitHub"),
      #      tags$iframe(src = "https://github.com/Leams-UERJ-apps/", 
      #     width = "100%", height = "800px", frameborder = 0)
      
      
      # ),
      
      # MODIFICAÇÃO DO LINK DO GITHUB DE "LEAMS" PARA "LTAP" EM 20/02/2025
      
      # tabItem(
      #  tabName = "github",
      # h2("Our GitHub"),
      
      #  p("Access our GitHub repository here:"),
      # p(a(href = "https://github.com/Leams-UERJ-apps/", 
      #    "Click here to visit our GitHub", 
      #   target = "_blank"))
      # ),
      
      
      tabItem(
        tabName = "github",
        h2("Our GitHub"),
        
        p("Access our GitHub repository here:"),
        p(a(href = "https://github.com/LTAP-UERJ", 
            ##MODIFICADO EM 13/04/2025
            
            "https://github.com/LTAP-UERJ", 
            target = "_blank"))
        
        ## "Click here to visit our GitHub", 
        ##  target = "_blank"))
      ),
      
      
      ######ADICIONADO PARA ABRIR LINKS EM 18/02/2025####
      
      tabItem(tabName = "importtab", 
              h2("Import Data"), 
              shiny::hr(),
              fluidRow(
                box(title=h3("Enter Data Settings"), 
                    width = 4, 
                    status = "primary",
                    fileInput("file", "Data", buttonLabel = "Upload..."),
                    shiny::hr(),
                    checkboxInput("isspectra",strong("Check if the data is of spectral type.")),
                    shiny::hr(),
                    checkboxInput("classcol", strong(" Check if the data presents sample classes."), T),
                    conditionalPanel(condition = "input.classcol == true", numericInput("classpos", "Select the class column position: (for .RData files and DEMOS, please ignore)", 2)),
                    shiny::hr(),
                    selectizeInput("datatype", "Select data type:", 
                                   #c(".txt"="txt", ".csv"="csv", "Excel"="xlsx",  "Interface Standard"="itfstd", "Select file type"=""), 
                                   ##MODIFICAÇÃO DE "Excel" para ".xls" na escolha do usuário em 17/02/2025
                                   c(".txt"="txt", ".csv"="csv", ".xls"="xlsx",  "Interface Standard"="itfstd", "Select file type"=""),
                                   selected = ""),
                    shiny::hr(),
                    conditionalPanel(condition = "input.datatype == 'txt'||input.datatype == 'csv'||input.datatype == 'csv'||input.datatype == 'xls'|| input.datatype == 'xlsx'",
                                     checkboxInput("labels","Check if the variables have labels.",  F),
                                     checkboxInput("namerows", "Check if the first column presents sample names.", F),
                    ),
                    
                    conditionalPanel(condition = "input.datatype == 'txt'||input.datatype == 'csv'||input.datatype == 'csv'",
                                     selectInput("delim", "Delimiter:",
                                                 c("Comma"=",", "Semicolon"=";", "Tab" = "\t"), 
                                                 selected = ","),
                                     selectInput("dec", "Decimal mark:", 
                                                 c("Dot"=".", "Comma"=","), 
                                                 selected = ".")
                    ),
                    
                    conditionalPanel(condition = "input.datatype == 'xls'|| input.datatype == 'xlsx'",
                                     selectInput("excelsheet", "Select the sheet to be imported:", 
                                                 choices = "")
                    ),
                    
                    actionButton("preview" , "Preview and Use", class = "btn-block btn-success")
                    
                ),
                
                box(title = h3("Data Overview"), 
                    width = 8, 
                    status = "primary", 
                    
                    tabsetPanel(
                      
                      # tabPanel("Barplot",
                      #         plotOutput("barplot")),
                      
                      #######MODIFICADO EM 25/02/2025
                      
                      tabPanel("Barplot",
                               
                               #######MODIFICADO EM 26/02/2025 - PASSANDO A SELEÇÃO DE CLASSES PARA A LEGENDA
                               # **CORREÇÃO: selectizeInput configurado para permitir exclusão de classes após ter(em) sido adicionada(s)**
                               #      selectizeInput("selected_classes", "Select Classes:", choices = NULL, 
                               #                    multiple = TRUE, 
                               #                   options = list(plugins = c("remove_button"))),
                               plotlyOutput("barplot")
                               
                               
                      ),
                      
                      
                      #######MODIFICADO EM 25/02/2025
                      
                      tabPanel("Summary",
                               verbatimTextOutput("datasummary")),
                      tabPanel("Data Description",
                               tabsetPanel(id="dataPreview",
                                           type = "hidden",
                                           tabPanelBody("panelnormaldata", DTOutput("summary")),
                                           tabPanelBody("panelspectrum", plotOutput("spectrumpreview")%>%withSpinner()
                                           )
                               )
                      ),
                      tabPanel("NaN’s values positions", 
                               verbatimTextOutput("datanullvalues")
                      ),
                      tabPanel("Duplicated Values", 
                               verbatimTextOutput("checkdup")
                      ),
                      tabPanel("Remove Data",
                               shiny::hr(),
                               selectizeInput("removedatarow", "Select sample(s) to be removed from the dataset:", 
                                              choices=NULL, 
                                              multiple=T),
                               actionButton("removedatarowBT", "Remove Sample(s)", 
                                            class = "btn-block btn-danger", 
                                            width = "25%"),
                               shiny::hr(),
                               selectizeInput("removedatacol", "Select variable(s) to be removed from the dataset:", 
                                              choices=NULL,
                                              multiple=T),
                               actionButton("removedatacolBT", "Remove Variable(s)", class = "btn-block btn-danger", width = "25%")
                      )
                      
                      
                    )
                ),
              ),
              
              fluidRow(box(title=h3("Data preview"), width = 12, status = "primary",
                           shiny::hr(),
                           textOutput("spectramsn"),
                           DTOutput("preview1", width = "100%")
              )
              )
      ),
      
      #MODIFICAÇÃO EM 20/02/2025 DE LEAMS PATA LTAP EM DATA_HANDLING_APP 
      #tabItem(tabName = "fastprepro", 
      #       h2("Pre-Processing"), 
      #      shiny::hr(),
      #     fluidRow(
      #      box(h3("Select a pre-processing method"),
      #         shiny::hr(),
      #        width = 12,
      #       p("Selecting a method will apply it in every linear and non-linear models generated. For more complete pre-treatment capabilities please visit:"),
      #      p("https://leams-uerj-chemometrics.shinyapps.io/Data_Handling_app or download the data handling app at our GitHub at the 'about the interface panel'."),
      #     shiny::hr(),
      #    selectInput("prepropca","Fast Pre-processing:", c("None" = "none","Center" = "center","Autoscale" = "scale"), selected = "center")
      #)),
      
      
      tabItem(tabName = "fastprepro", 
              h2("Pre-Processing"), 
              shiny::hr(),
              fluidRow(
                box(h3("Select a pre-processing method"),
                    shiny::hr(),
                    width = 12,
                    p("Selecting a method will apply it in every linear and non-linear models generated. For more complete pre-treatment capabilities please visit:"),
                    p("https://ltap.shinyapps.io/data_handling/ or download the data handling app at our GitHub at the 'about the interface panel'."),
                    shiny::hr(),
                    selectInput("prepropca","Fast Pre-processing:", c("None" = "none","Center" = "center","Autoscale" = "scale"), selected = "center")
                )),
              
              
              fluidRow(
                box(width = 12,
                    h3("View Options"),
                    shiny::hr(),
                    fluidRow(
                      column(radioButtons("plotpretreatclasses" ,"Type of Coloring:",
                                          inline = T,
                                          choices = c("Show individually"="indpretreatplot", "Show by class"="classpretreatplot")
                      ), width = 4),
                      column(checkboxInput("selectpreprosamples", "Select some samples to plot? (Beware that classes will not be correctly colored)"), width = 4),
                      column(conditionalPanel("input.selectpreprosamples == true",selectizeInput("whichsamplesprepro", "Which Samples?", choices = c(), multiple=T)), width = 4)
                      ## column(conditionalPanel("input.selectpreprosamples == true",selectizeInput("whichsamplesprepro", "Which Samples?", choices = NULL, multiple=T)), width = 4)
                    ),
                    #actionButton("pretreatcopyBT", "Update Image Settings"),
                    
                    fluidRow(
                      column(width = 4, plotOutput("normaldata")),
                      column(width = 4, plotOutput("meancenterdata")),
                      column(width = 4, plotOutput("scaleddata"))
                    ),
                )
              )
      ),
      
      tabItem(tabName = "pcatab", h2("Principal Components Analysis:"), shiny::hr(),
              tabsetPanel( 
                
                tabPanel("PCA Model",
                         fluidRow(
                           box(title=h3("PCA Configurations"),width=6, status = "primary",
                               numericInput("ncomppca","Number of components", min = 1, max = 20, value = 10),
                               checkboxInput("removepcacolask", strong("Remove any columns from calculations? (All non-numeric columns are automatically removed)")),
                               conditionalPanel("input.removepcacolask == true", selectizeInput("pcaremovecol", "Wich columns?", choices = NULL, multiple=T)),
                               selectInput("methodpca", "Method to compute PCA:", c("svd"="svd","NIPALS"="nipals")),
                               #selectInput("limtypepca","Method applied to calculate outliers:", c( "Classical" = "ddmoments", "Robust" = "ddrobust", "chi-square distribution" = "chisq","Jackson-Mudholkar" = "jm"), selected = "ddmoments"),
                               actionButton("bPCA", "Run PCA",class = "btn-success",width = "25%")),
                           
                           box(title=h3("Model summary"),width = 6,verbatimTextOutput("SumPCA", placeholder = T), status = "primary")),
                         
                         fluidRow(box(h3("Plots"), width = 12, height = 900, status = "danger",
                                      checkboxInput("classcorespca", strong("Show classes on Scores plot (attention: colors of each class may differ in each graph):")),
                                      tabsetPanel(
                                        tabPanel("Variance",plotlyOutput("pcav", height = 650)),
                                        tabPanel("Cumulative Variance",plotlyOutput("pcacv", height = 650)),
                                        tabPanel("Loadings",
                                                 fluidRow(
                                                   column(width = 6,numericInput("npc1loadplotPCA", "1st Component to plot:", step = 1, value = 1, min = 1)),
                                                   column(width = 6,numericInput("npc2loadplotPCA", "2nd Component to plot:", step = 1, value = 2, min = 1))
                                                 ),
                                                 plotlyOutput("pcaload", height = 650)),
                                        tabPanel("Spectral Loadings",h3(""),numericInput("numnpcpca","Show n components", min = 1, max = 15, value = 2),plotlyOutput("pcaload1", height = 650)),
                                        tabPanel("Scores", h3(""),
                                                 fluidRow(
                                                   column(numericInput("siglimpca", "Set the confidence limit for the HoT Elipse:", max = 0.99, min = 0, value = 0.95, step = 0.01), width = 4),
                                                   column(numericInput("npc1scoresplotpca", "1st PC to plot:", step = 1, value = 1, min = 1), width = 4),
                                                   column(numericInput("npc2scoresplotpca", "2nd PC to plot:", step = 1, value = 2, min = 1), width = 4)
                                                 ),
                                                 plotlyOutput("pcascores", height = 650)
                                        ),
                                        tabPanel("3D Scores", h3(""),
                                                 fluidRow(
                                                   column(numericInput("siglim3Dpca", "Set the confidence limit for the HoT Elipse:", max = 0.99, min = 0, value = 0.95, step = 0.01), width = 3),
                                                   column(numericInput("npc1scores3dplotpca", "1st PC to plot:", step = 1, value = 1, min = 1), width = 3),
                                                   column(numericInput("npc2scores3dplotpca", "2nd PC to plot:", step = 1, value = 2, min = 1), width = 3),
                                                   column(numericInput("npc3scores3dplotpca", "3rd PC to plot:", step = 1, value = 3, min = 1), width = 3)
                                                 ),
                                                 plotlyOutput("pca3dscores", height = 650)%>%withSpinner()
                                        ),
                                        tabPanel("Residuals",h3(""),fluidRow(
                                          column(6,numericInput("numPCresipca","Principal Component Number",min = 1,value = 1),checkboxInput("logresidualspca","Apply Log", F)), plotOutput("pcaresiduals"))
                                        ),
                                        
                                        tabPanel("BiPlot", h3(""), 
                                                 fluidRow(
                                                   column(width = 6,numericInput("npc1biplotpca", "1st Component to plot:", step = 1, value = 1, min = 1)),
                                                   column(width = 6,numericInput("npc2biplotpca", "2nd Component to plot:", step = 1, value = 2, min = 1))
                                                 ),
                                                 plotlyOutput("biplotpca", height = 650)
                                        ),
                                        tabPanel("Outliers",
                                                 numericInput("numPCoutipca","Principal Component Number",min = 1,value = 1),
                                                 plotOutput("OutPCA", height = 650)),
                                      )
                         )
                         )
                ),
                tabPanel("Save and load model",
                         fluidRow(
                           box(title = h3("Save Model"), width = 6,
                               numericInput("numcompselpca", "Select final number of components:", value = 3, step = 1, min = 1),
                               textInput("namemodelpca", "Choose file name (will have 'PCA model' as ending)", placeholder = "My model name"),
                               downloadButton("savepcamodel", "Download model into .Rdata"),
                               shiny::hr(),
                               downloadButton("downloadscorespca", "Download Scores from PCA into '.csv'", class = "btn-block"),
                               downloadButton("downloadloadpca","Download Loadings from PCA into '.csv'", class = "btn-block")),
                           
                           box(title = h3("Import previous PCA model"), width = 6,
                               fileInput("searchmodelpca", "Select Model:", buttonLabel = "Search file..."),
                               actionButton("importpcamodel", "Import"))
                         ))
              )
      ),
      
      
      #ACRÉSCIMO DA PCA ROBUSTA EM 17/01/2025
      
      tabItem(tabName = "robpcatab", h2("Robust Principal Component Analysis"), shiny::hr(),
              tabsetPanel(
                
                tabPanel("Robust PCA model",
                         fluidRow(
                           box(title = h3("Robust PCA Configurations"), width = 6, status = "primary",
                               numericInput("ncompROBPCA","Number of components", min = 1, max = 20, value = 3),
                               checkboxInput("removeROBPCAcolask", "Remove any columns?"),
                               #selectInput("prepropca","Preprocessing method:", c("None" = "none","Center" = "center","Scale" = "scale"), selected = "scale"),
                               conditionalPanel("input.removeROBPCAcolask == true", selectInput("ROBPCAremovecol", "Wich columns?", choices = NULL, multiple=T)),
                               actionButton("bROBPCA", "Run Robust PCA",class = "btn-success",width = "25%")
                           ),
                           
                           box(title=h4("Model summary"),width = 6,verbatimTextOutput("SumROBPCA", placeholder = T), status = "primary"),
                         ),
                         
                         
                         fluidRow(box(h3("Plots"), width = 12, height = 900, status = "danger",
                                      checkboxInput("classcoresROBPCA", strong("Show classes on Scores plot (attention: colors of each class may differ in each graph):")),
                                      tabsetPanel(
                                        tabPanel("Variance",plotlyOutput("ROBPCAv", height = 650)),
                                        tabPanel("Cumulative Variance",plotlyOutput("ROBPCAcv", height = 650)),
                                        tabPanel("Loadings", h3(""),
                                                 fluidRow(
                                                   column(width = 6,numericInput("npc1loadplotROBPCA", "1st Component to plot:", step = 1, value = 1, min = 1)),
                                                   column(width = 6,numericInput("npc2loadplotROBPCA", "2nd Component to plot:", step = 1, value = 2, min = 1))
                                                 ),
                                                 plotlyOutput("ROBPCAload", height = 650)),
                                        tabPanel("Spectral Loadings",h3(""),plotlyOutput("ROBPCAload1", height = 650)),
                                        tabPanel("Scores",h3(""),
                                                 fluidRow(
                                                   column(width = 4,numericInput("siglimROBPCA", "Set the confidence limit for the HoT Elipse:", max = 0.99, min = 0, value = 0.95, step = 0.01)),
                                                   column(width = 4,numericInput("npc1scoresplotROBPCA", "1st Component to plot:", step = 1, value = 1, min = 1)),
                                                   column(width = 4,numericInput("npc2scoresplotROBPCA", "2nd Component to plot:", step = 1, value = 2, min = 1))
                                                 ),
                                                 plotlyOutput("ROBPCAscores", height = 650)
                                                 
                                        ),
                                        tabPanel("3D Scores", h3(""),
                                                 fluidRow(
                                                   column(width = 3,numericInput("siglim3DROBPCA", "Set the confidence limit for the HoT Elipse:", max = 0.99, min = 0, value = 0.95, step = 0.01)),
                                                   column(width = 3,numericInput("npc1scores3dplotROBPCA", "1st Component to plot:", step = 1, value = 1, min = 1)),
                                                   column(width = 3,numericInput("npc2scores3dplotROBPCA", "2nd Component to plot:", step = 1, value = 2, min = 1)),
                                                   column(width = 3,numericInput("npc3scores3dplotROBPCA", "3rd Component to plot:", step = 1, value = 3, min = 1))
                                                 ),
                                                 plotlyOutput("ROBPCA3dscores", height = 650)%>%withSpinner()
                                        ),
                                        tabPanel("Biplot", h3(""),
                                                 fluidRow(
                                                   column(width = 6,numericInput("npc1biplotROBPCA", "1st Component to plot:", step = 1, value = 1, min = 1)),
                                                   column(width = 6,numericInput("npc2biplotROBPCA", "2nd Component to plot:", step = 1, value = 2, min = 1))
                                                 ),
                                                 plotlyOutput("ROBPCAbiplot", height = 650)
                                        ),
                                        
                                        tabPanel("Outliers",
                                                 numericInput("numROBPCoutipca","Principal Component Number",min = 1,value = 1),
                                                 plotOutput("OutROBPCA", height = 650)),
                                        
                                      )))
                         
                         #input$npc1loadplotROBPCA
                         
                ),
                
                tabPanel("Save and load model",
                         fluidRow(
                           box(title = h3("Save Model"), width = 6,
                               numericInput("numcompselROBPCA", "Select final number of components:", value = 3, step = 1, min = 1),
                               textInput("namemodelROBPCA", "Choose file name (will have 'RobPCA model' as ending)", placeholder = "My model name"),
                               downloadButton("saveROBPCAmodel", "Download model into .Rdata"),
                               shiny::hr(),
                               downloadButton("downloadscoresROBPCA", "Download Scores from ROBPCA into '.csv'", class = "btn-block"),
                               downloadButton("downloadloadROBPCA","Download Loadings from ROBPCA into '.csv'", class = "btn-block")),
                           box(title = h3("Import previous ROBPCA model"), width = 6,
                               fileInput("searchmodelROBPCA", "Select Model:", buttonLabel = "Search file..."),
                               actionButton("importROBPCAmodel", "Import")),
                         ))
                
              )),
      
      
      
      #ACRÉSCIMO DO TSNE EM 17/01/2025  
      
      tabItem(tabName = "tsnetab", h2("t-distributed Stochastic Neighbour Embedding"), shiny::hr(),
              tabsetPanel(
                
                tabPanel("t-SNE projection",
                         fluidRow(
                           box(title = h3("t-SNE Configurations"), width = 6, status = "primary",
                               numericInput("ncompTSNE","Number of dimensions:", min = 1, max = 3, value = 3),
                               numericInput("perplexityTSNE", "Perplexity (perplexity must be less than '1/3(number of samples - 1)'):", min = 5, value = 20, step = 1),
                               numericInput("niterTSNE", "Number of iterations:", min = 100, value = 1000, step = 200),
                               numericInput("thetaTSNE", "Theta Value (speed/accuracy tradeoff):", min = 0.0, max = 1, value = 0.5, step = 0.01),
                               checkboxInput("removeTSNEcolask", "Remove any columns?"),
                               conditionalPanel("input.removeTSNEcolask == true", selectInput("TSNEremovecol", "Wich columns?", choices = NULL, multiple=T)),
                               actionButton("bTSNE", "Run t-SNE",class = "btn-success",width = "25%")
                           ),
                           
                           box(title=h3("New Values"),width = 6, DTOutput("SumTSNE"), status = "primary"),
                         ),
                         
                         
                         fluidRow(box(h3("Plots"), width = 12, height = 900, status = "danger",
                                      checkboxInput("classcoresTSNE", strong("Show classes on projection plot:")),
                                      tabsetPanel(
                                        tabPanel("Scores", h3(""),
                                                 numericInput("siglimTSNE", "Set the confidence limit for the HoT Elipse:", max = 0.99, min = 0, value = 0.95, step = 0.01),
                                                 numericInput("npc1scoresplotTSNE", "1st Component to plot:", step = 1, value = 1, min = 1),
                                                 numericInput("npc2scoresplotTSNE", "2nd Component to plot:", step = 1, value = 2, min = 1),
                                                 plotlyOutput("TSNEscores", height = 650)%>%withSpinner()
                                        ),
                                        tabPanel("3D Scores", h3(""),
                                                 numericInput("siglim3DTSNE", "Set the confidence limit for the HoT Elipse:", max = 0.99, min = 0, value = 0.95, step = 0.01),
                                                 numericInput("npc1scores3dplotTSNE", "1st Component to plot:", step = 1, value = 1, min = 1),
                                                 numericInput("npc2scores3dplotTSNE", "2nd Component to plot:", step = 1, value = 2, min = 1),
                                                 numericInput("npc3scores3dplotTSNE", "3rd Component to plot:", step = 1, value = 3, min = 1),
                                                 plotlyOutput("TSNE3dscores", height = 650)%>%withSpinner()
                                                 
                                        )
                                        
                                      )
                         )
                         )
                ),
                
                tabPanel("Save and load model",
                         fluidRow(
                           box(title = h3("Save Model"), width = 6,
                               #numericInput("numcompselTSNE", "Select final number of Components:", value = 3, step = 1, min = 1),
                               textInput("namemodelTSNE", "Choose file name (will have 'TSNE model' as ending)", placeholder = "My model name"),
                               downloadButton("saveTSNEmodel", "Download model into .Rdata", class = "btn-block"),
                               shiny::hr(),
                               downloadButton("downloadscoresTSNE", "Download projected data from TSNE into '.csv'", class = "btn-block"),
                               #downloadButton("downloadloadTSNE","Download Loadings from TSNE into '.csv'", class = "btn-block")),
                           ),
                           box(title = h3("Import previous TSNE model"), width = 6,
                               fileInput("searchmodelTSNE", "Select Model:", buttonLabel = "Search file..."),
                               actionButton("importTSNEmodel", "Import")),
                         ))
                
              )),
      
      
      #########################RESULTADOS PARA ANTES DE VISUALIZAÇÕES NA SINTAXE EM 07/03/2025
      # Resultados
      #ALTERAÇÃO EM 15/02/2025
      
      #ALTERAÇÃO DO NOME DO BOX de "Dados Originais" para "Original Samples"
      #ALTERAÇÃO DO NOME DO BOX de "Dados Combinados" para "Combined Samples"
      
      tabItem(
        tabName = "results",
        fluidRow(
          box(
            title = "Original Samples", width = 6, status = "warning", solidHeader = TRUE,
            DTOutput("original_data")
          ),
          
          ###########TRECHO OMITIDO EM 07/05/2025
          
          #   box(
          #    title = "Amostras Synthetics", width = 6, status = "warning", solidHeader = TRUE,
          #   DTOutput("synthetic_only_data")
          
          #  ),
          
          ###########TRECHO OMITIDO EM 07/05/2025
          
          box(
            title = "Combined Samples", width = 6, status = "warning", solidHeader = TRUE,
            DTOutput("synthetic_data")
            
          ),
          #  ),
          
          #FUNCIONALIDADE DE EXPORTAÇÃO DE RESULTADOS EM 15/02/2025
          
          #MODIFICAÇÃO DE "Data Export" PARA DENTRO DO FLUIDROW EM 07/03/2025 E DO TAMANHO DA CAIXA
          
          ## box(title = "Data Export", status = "info", solidHeader = TRUE, width = 12,
          box(title = "Data Export", status = "info", solidHeader = TRUE, width = 6,
              selectInput("dataset_choice", "Choose the dataset:", 
                          
                          ###########MODIFICADO EM 07/05/2025######################
                          #choices = c("Original Samples" = "original", "Amostras Synthetics" = "synthetic", "Combined Samples" = "combined")),
                          ##choices = c("Original Samples" = "original", "Combined Samples" = "combined")),
                          choices = c("Original Samples" = "original", "Combined Samples" = "combined", "Removed Samples" = "removed")),
              p(strong("Removed Samples:"), "Option valid only for Downsampling and Hybrid if samples have been removed") , #ADICIONADO EM 16/06/2025/ ATUALIZADO EM 02/07/2025
              ###########MODIFICADO EM 07/05/2025#####################
              
              selectInput("export_format", "Choose the format:", choices = c(".csv" = "csv", ".xls" = "xlsx")),
              downloadButton("download_selected", "Export Data")
          )
        )
      ),
      #TRECHO MOVIDO PARA GUIA DE VISUALIZAÇÕES EM 07/03/2025 
      #    fluidRow(
      #     box(
      #      title = "Variance Tests", width = 12, status = "primary", solidHeader = TRUE,
      #     selectInput("variance_test", "Choose the Variance Test:",
      #                #choices = c("Levene's Test", "Bartlett's Test", "Fligner-Killeen Test", "F-Test"),
      #               choices = c("Levene's Test", "Fligner-Killeen Test"),
      #              selected = "Levene's Test"),
      #  actionButton("run_variance_test", "Run Test", class = "btn-success"),
      #  verbatimTextOutput("variance_test_results")
      #  )
      #  )
      # ),
      #  # )
      #  # )
      # # )
      
      
      ###############################################################
      
      
      
      ################ACRÉSCIMO DE VISUALIZAÇÕES E RESULTADOS EM 17/01/2025###
      
      
      ####MODIFICAÇÃO DA ORDEM DE VISUALIZAÇÃO DAS JANELAS NA INTERFACE EM 07/03/2025
      # Visualizações
      tabItem(
        tabName = "visual",
        fluidRow(
          box(
            title = "Class Distribution", width = 6, status = "info", solidHeader = TRUE,
            #plotlyOutput("class_dist")
            
            #MODIFICADO EM 10/05/2025 PARA DEFINIR QUAL GRÁFICO APARECERÁ EM Distribution of CLASSES(SEMPRE O ÚLTIMO!)
            ##   plotlyOutput("class_dist",height = "500px"),
            ##  plotlyOutput("class_dist_nc",height = "500px") #ACRÉSCIMO EM 10/05/2025 PARA DISTRIB. DE CLASSES DO SMOTE_NC
            
            uiOutput("class_dist_switch")  # Exibe dinamicamente o gráfico correto em 10/05/2025
            #MODIFICADO EM 10/05/2025 PARA DEFINIR QUAL GRÁFICO APARECERÁ EM Distribution of CLASSES(SEMPRE O ÚLTIMO!)
            
            # )
          ),
          
          #  fluidRow(
          ##      box(
          ##      title = "Boxplot of Variables", width = 6, status = "info", solidHeader = TRUE,
          
          
          #plotlyOutput("boxplot_variables")
          
          ##    plotlyOutput("boxplot_variables",height = "500px")
          
          
          
          
          
          
          #  )
          ##    ),
          
          ##MODIFICAÇÃO EM 06/05/2025 - SELEÇÃO DE Variables PARA VISUALIZAR NO BOXPLOT 
          
          box(
            title = "Boxplot of Variables", width = 6, status = "info", solidHeader = TRUE,
            
            selectInput(
              inputId = "boxplot_vars",
              label = "Select variables for the boxplot:",
              choices = NULL,  # será preenchido dinamicamente
              multiple = TRUE, selected = NULL
            ),
            
            #  plotlyOutput("boxplot_variables", height = "500px")
            ## uiOutput("boxplot_switch")  # UI dinâmica para exibir o boxplot correto em 16/05/2025
            plotlyOutput("boxplot_output", height = "500px") #MODIFICADO EM 19/05/2025
          ),
          
          
          
          
          
          
          
          
          
          # fluidRow(
          
          
          ##ACRÉSCIMO DE PLOT COMPARATIVO DAS Original Samples E SyntheticS NA GUIA DIAGNÓSTICO EM 27/03/2025
          
          ##  box(
          ##    title = "Plot: Original Samples vs Synthetics", width = 12, status = "info", solidHeader = TRUE,
          
          # Filtro por tipo de amostra
          #  checkboxGroupInput("amostra_tipo", "Tipo de amostra:",
          #                    choices = c("Original", "Synthetic"),
          #                   selected = c("Original", "Synthetic"),
          #                  inline = TRUE),
          
          # Filtro por amostras (por texto) - MODIFICADO EM 27/03/2025
          ##    checkboxInput("enable_selection", "Select some Samples to plot?", FALSE),
          ##    conditionalPanel(
          ##      condition = "input.enable_selection == true",
          ##      textInput("sample_text_input", "Which Samples? (ex: 1, 5:10, 20):", value = "")
          ##    ),
          
          # Gráfico
          ##    plotlyOutput("comparison_plot", height = "550px")
          ##  ),
          
          
          box(
            ## title = "Data & Mean Centering Plot", width = 12, status = "info", solidHeader = TRUE,
            title = "Data & Class Mean Plot", width = 12, status = "info", solidHeader = TRUE, #MODIFICADO EM 11/04/2025
            
            fluidRow(
              column(6,
                     radioButtons("coloring_type", "Type of Coloring:",
                                  choices = c("Show individually" = "individual",
                                              "Show by class" = "class"),
                                  inline = TRUE,
                                  selected = "class")
              ),
              column(6,
                     checkboxInput("enable_selection", "Select some samples to plot? (Beware that classes will not be correctly colored)", FALSE),
                     conditionalPanel(
                       condition = "input.enable_selection == true",
                       selectizeInput("sample_selector_plotly", "Which Samples?",
                                      choices = NULL, multiple = TRUE, options = list(maxItems = 5000))
                     )
              )
            ),
            
            fluidRow(
              column(6, plotlyOutput("plotly_data_raw", height = "500px")),
              #column(6, plotlyOutput("plotly_data_centered", height = "500px"))
              column(6, plotlyOutput("plotly_data_class_mean", height = "500px")) #MODIFICADO EM 11/04/2025
            )
          ),
          
          
          
          
          
          
          ##ACRÉSCIMO DE PLOT COMPARATIVO DAS Original Samples E SyntheticS NA GUIA DIAGNÓSTICO EM 27/03/2025       
          
          
          
          #MODIFICAÇÕES PARA MAIOR FLEXIBILIDADE PARA OS USUÁRIOS COM PARÂMETROS DINÂMICOS EM 12/03/2025
          #MODIFICAÇÃO DE "HEIGHT" EM 2D Projection PARA SUPORTAR TAMANHO DE TSNE
          box(
            #title = "2D Projection", width = 6, height=800, status = "info", solidHeader = TRUE,
            #title = "2D Projection", width = 6, height=1600, status = "info", solidHeader = TRUE,
            ##title = "2D Projection", status = "info", solidHeader = TRUE,
            title = "2D Projection", width=12, status = "info", solidHeader = TRUE,
            selectInput("projection", "Choose Projection:", 
                        choices = c("PCA", "Robust PCA", "t-SNE"), selected = "PCA"),
            #selectInput("hot_confidence", "Set the confidence limit for the HoT Elipse:", 
            #choices = seq(0.50, 0.99, by = 0.01), selected = 0.95),
            numericInput("hot_confidence", "Set the confidence limit for the HoT Elipse:",  #MODIFICADO EM 13/03/2025          
                         max = 0.99, min = 0, value = 0.95, step = 0.01),  #MODIFICADO EM 13/03/2025
            
            ##LIMITANDO EM 30 O NÚMERO DE PCS EM 06/05/2025
            
            # numericInput("pc1_plot", "1st PC to plot:", value = 1, min = 1),
            # numericInput("pc2_plot", "2nd PC to plot:", value = 2, min = 1),
            numericInput("pc1_plot", "1st PC to plot:", value = 1, min = 1, max = 30),
            numericInput("pc2_plot", "2nd PC to plot:", value = 2, min = 1, max = 30),
            
            # Condições para t-SNE  ADICIONADO EM 20/03/2025
            conditionalPanel(
              condition = "input.projection == 't-SNE'",
              numericInput("tsne_dims", "Number of dimensions:", value = 3,max = 3, min = 1),
              numericInput("tsne_perplexity", "Perplexity (perplexity must be less than '1/3(number of samples - 1)'):", value = 20, min = 5, step = 1),
              numericInput("tsne_iter", "Number of iterations:", value = 1000, min = 100, step = 200),
              numericInput("tsne_theta", "Theta Value (speed/accuracy tradeoff):",max= 1 ,value = 0.5, min = 0.0, step = 0.01),
              #checkboxInput("remove_columns", "Remove any columns?", FALSE)
              checkboxInput("remove_columns", "Remove any columns?"),
              conditionalPanel("input.remove_columns == true", selectInput("TSNEremovecolumns", "Wich columns?", choices = NULL, multiple=T))
            ),
            # Condições para t-SNE  ADICIONADO EM 20/03/2025
            
            actionButton("run_projection", "Run Projection", class = "btn-success")
            ,
            ##plotlyOutput("projection_plot")
            plotlyOutput("projection_plot",height="700px")
          ),
          #   ),
          #  fluidRow(
          
          ######ACRÉSCIMO DA 3D Projection EM 20/03/2025############################ 
          
          box(
            ##title = "3D Projection", width = 6, height = 1600, status = "info", solidHeader = TRUE,
            title = "3D Projection", width = 12, status = "info", solidHeader = TRUE,
            #title = "3D Projection", width = 8,height = 1600, status = "info", solidHeader = TRUE,
            selectInput("projection_3d", "Choose Projection:", 
                        choices = c("PCA", "Robust PCA", "t-SNE"), selected = "PCA"),
            
            numericInput("hot_confidence_3d", "Set the confidence limit for the HoT Elipse:",  
                         max = 0.99, min = 0, value = 0.95, step = 0.01),
            
            ##LIMITANDO EM 30 O NÚMERO DE PCS EM 06/05/2025
            
            # numericInput("pc1_plot_3d", "1st PC to plot:", value = 1, min = 1),
            #  numericInput("pc2_plot_3d", "2nd PC to plot:", value = 2, min = 1),
            #  numericInput("pc3_plot_3d", "3rd PC to plot:", value = 3, min = 1), # Adicionando 3ª Dimension para 3D Projection
            
            
            
            numericInput("pc1_plot_3d", "1st PC to plot:", value = 1, min = 1, max = 30),
            numericInput("pc2_plot_3d", "2nd PC to plot:", value = 2, min = 1, max = 30),
            numericInput("pc3_plot_3d", "3rd PC to plot:", value = 3, min = 1, max = 30), # Adicionando 3ª Dimension para 3D Projection
            
            
            # Configurações para t-SNE
            conditionalPanel(
              condition = "input.projection_3d == 't-SNE'",
              numericInput("tsne_dims_3d", "Number of dimensions:", value = 3, max = 3, min = 1),
              numericInput("tsne_perplexity_3d", "Perplexity (perplexity must be less than '1/3(number of samples - 1)'):", 
                           value = 20, min = 5, step = 1),
              numericInput("tsne_iter_3d", "Number of iterations:", value = 1000, min = 100, step = 200),
              numericInput("tsne_theta_3d", "Theta Value (speed/accuracy tradeoff):", max= 1, value = 0.5, min = 0.0, step = 0.01),
              
              checkboxInput("remove_columns_3d", "Remove any columns?"),
              conditionalPanel("input.remove_columns_3d == true", 
                               selectInput("TSNEremovecolumns_3d", "Which columns?", choices = NULL, multiple = TRUE))
            ),
            
            actionButton("run_projection_3d", "Run Projection", class = "btn-success"),
            #div(style = "height: 300%"),
            #div(style = "height: calc(100% - 320px); overflow: hidden;",  # ajusta altura com base nos inputs acima (~320px de inputs)
            ##plotlyOutput("projection_plot_3d") # Saída para a 3D Projection
            plotlyOutput("projection_plot_3d",height="700px") # Saída para a 3D Projection
            #plotlyOutput("projection_plot_3d",height = "100%") # Saída para a 3D Projection
            # )
          ),
          ######ACRÉSCIMO DA 3D Projection EM 20/03/2025############################
          
          
          ##MODIFICAÇÃO TAMANHO DA JANELA SCCORES PCA EM 28/03/2025
          
          ##MODIFICAÇÃO DO HEIGHT PARA MELHORAR EXIBIÇÃO DO GRÁFICO(MELHOR CENTRALIZAÇÃO EM TELA)
          ##       box(
          ##       #title = "Boxplot of Scores (PCA)", width = 6, status = "info", solidHeader = TRUE,
          ##     #title = "Boxplot of Scores (PCA)", width = 10, status = "info", solidHeader = TRUE,
          ##    ##  title = "Boxplot of Scores (PCA)", width = 12,height = 650, status = "info", solidHeader = TRUE,
          ##    title = "Boxplot of Scores (PCA)", width = 12, status = "info", solidHeader = TRUE,
          ##   ## plotlyOutput("boxplot_scores")
          ##   plotlyOutput("boxplot_scores", height = "700px")
          ##    )
          ##    ),
          
          ##MODIFICAÇÃO EM 06/05/2025: QUAIS COMPONENTES SERÃO VISUALIZADAS? 
          
          box(
            title = "Boxplot of Scores (PCA)", width = 12, status = "info", solidHeader = TRUE,
            
            selectInput(
              inputId = "pcs_to_show",
              label = "Select the Principal Components (PCs) to display:",
              choices = NULL,
              multiple = TRUE,
              selected = NULL
            ),
            
            plotlyOutput("boxplot_scores", height = "700px")
          )
        ),
        
        
        
        
        
        
        ##BLOCO MODIFICAOD EM 03/04/2025#############
        
        ########ACRÉSCIMO DOS RESÍDUOS DA PCA EM VISUALIZAÇÕES EM 14/03/2025#####
        
        ##MODIFICAÇÃO DE WIDTH DE 06 PARA 12 EM 16/03/2025
        #    box(title = "Residuals (PCA)", status = "info", solidHeader = TRUE, width = 12,
        #       #numericInput("numPCresidCustom", "Principal Component Number", value = 1, min = 1),
        #      numericInput("numPCresidCustom", "Principal Component Number", min = 1,value = 1 ),
        #     checkboxInput("logResidualsCustom", "Apply Log", value = FALSE),
        #    actionButton("run_residuals", "View Residuals", class = "btn-success")
        #   ,
        #  plotOutput("pcaResidualsPlotCustom")
        #  ),
        
        
        # box(title = "Residuals (PCA)", status = "info", solidHeader = TRUE, width = 12,
        #    numericInput("numPCresidCustom", "Principal Component Number", min = 1, value = 1),
        #   checkboxInput("logResidualsCustom", "Apply Log", value = FALSE),
        #  actionButton("run_residuals", "View Residuals", class = "btn-success"),
        #  plotlyOutput("pcaResidualsPlotCustom", height = "700px")
        #  ),
        
        
        
        box(
          title = "Residuals (PCA)", status = "info", solidHeader = TRUE, width = 12,
          
          numericInput("numPCresidCustom", "Principal Component Number", min = 1, value = 1),
          checkboxInput("logResidualsCustom", "Apply Log", value = FALSE),
          actionButton("run_residuals", "View Residuals", class = "btn-success"),
          
          br(),
          plotlyOutput("pcaResidualsPlotCustom", height = "700px"),
          br(),
          uiOutput("residualsHelpTextUI")
        ),
        
        ##BLOCO MODIFICAOD EM 03/04/2025#############  
        
        ########ACRÉSCIMO DOS OUTLIERS DA PCA EM VISUALIZAÇÕES EM 14/03/2025#####
        ##MODIFICAÇÃO DE WIDTH DE 06 PARA 12 EM 16/03/2025
        #   box(title = "Outliers (PCA)", status = "info", solidHeader = TRUE, width = 12,
        
        #           numericInput("numPCoutipcaCustom","Principal Component Number",min = 1,value = 1),
        #     actionButton("run_outliers", "View Outliers", class = "btn-success")
        #        ,
        #        # plotOutput("OutPCACustom", height = 650)
        #   plotlyOutput("OutPCACustom", height = 650)
        #  ),
        
        
        
        
        box(
          title = "Outliers (PCA)", status = "info", solidHeader = TRUE, width = 12,
          
          numericInput("numPCoutipcaCustom", "Principal Component Number", min = 1, value = 1),
          actionButton("run_outliers", "View Outliers", class = "btn-success"),
          br(),
          plotlyOutput("OutPCACustom", height = 650),
          br(),
          uiOutput("outliersHelpTextUI")  
        ),
        
        ########ACRÉSCIMO DOS OUTLIERS DA PCA EM VISUALIZAÇÕES EM 14/03/2025#####        
        
        
        
        #######ACRÉSCIMO DA Jensen-Shannon divergence EM 28/03/2025#####
        
        
        #box(
        # title = "Jensen-Shannon divergence",
        # width = 12, status = "info", solidHeader = TRUE,
        # plotlyOutput("js_divergence_plot", height = "550px"),
        # hr(),
        # selectInput("classe_detalhe", "View distributions for the class:", choices = NULL),
        # selectInput("variavel_detalhe", "Variable:", choices = NULL),
        # plotlyOutput("js_distributions_plot", height = "400px")
        # ),
        
        
        ## ACRÉSCIMO DE TEXTO EXPLICATIVO EM 03/04/2025
        
        box(
          title = "Jensen-Shannon divergence",
          width = 12, status = "info", solidHeader = TRUE,
          
          # Gráfico principal
          plotlyOutput("js_divergence_plot", height = "550px"),
          hr(),
          
          # Texto explicativo
          HTML("<p style='font-size:15px; color:#555;'>
          
          
          
          The divergence is calculated between <strong>original</strong> and <strong>synthetic</strong> samples, 
          and is displayed only for the <strong>classes that were actually synthesized</strong>.
          </p>"),
          br(),
          
          # Seletores
          selectInput("classe_detalhe", "View distributions for the class:", choices = NULL),
          selectInput("variavel_detalhe", "Variable:", choices = NULL),
          
          # Gráfico detalhado
          plotlyOutput("js_distributions_plot", height = "400px")
        ),
        
        
        #######ACRÉSCIMO DE GRÁFICO DE PROCRUSTES GENERALIZADO EM 29/03/2025##
        
        
        box(
          title = "Generalized Procrustes Analysis", 
          status = "info", solidHeader = TRUE, width = 12,
          plotlyOutput("grafico_procrustes_multiclasse", height = "700px")
        ),
        
        #######ACRÉSCIMO DE GRÁFICO DE PROCRUSTES GENERALIZADO EM 29/03/2025##
        
        
        ###ACRÉSCIMO DE GRÁFICO DA Distance from Mahalanobis EM 29/03/2025##
        
        
        #box(
        ## #title = "Distance from Mahalanobis - Original vs Sintético por Classe",
        # ##MODIFICADO EM 31/03/2025
        #  #title = "Distance from Mahalanobis by class",
        #  title = "Distance from Mahalanobis",
        # status = "info", solidHeader = TRUE, width = 12,
        # plotlyOutput("grafico_mahalanobis_por_classe", height = "500px")
        # ),
        
        
        box(
          # Título atualizado e limpo
          title = "Distance from Mahalanobis",
          status = "info", solidHeader = TRUE, width = 12,
          
          # Gráfico principal
          plotlyOutput("grafico_mahalanobis_por_classe", height = "500px"),
          br(),
          
          # Texto explicativo
          HTML("<p style='font-size:15px; color:#555;'>
          
          
          
          The distance is calculated between <strong>original</strong> and <strong>synthetic</strong> samples, 
          and is displayed only for the <strong>classes that were actually synthesized</strong>.
         
       </p>")
        ),
        
        
        
        
        
        
        ##box(
        # # #title = "Distance from Mahalanobis - Original vs Sintético por Classe",
        ##  title = "Hotelling T² e Distance from Mahalanobis by class",
        # # status = "info", solidHeader = TRUE, width = 12,
        ##  plotlyOutput("grafico_mahalanobis_por_classe", height = "500px"),
        ##  br(),
        ##  #helpText("H₀: The vector (multivariate) means of the original and synthetic samples are equal."),
        ##  p(strong("Null hypothesis:"), " As médias vetoriais (multivariadas) das Original Samples e Synthetics são iguais"),
        ##  DTOutput("tabela_hotelling")
        ## ),
        
        
        #box(
        # title = "Hotelling T² e Distance from Mahalanobis by class",
        #  status = "info", solidHeader = TRUE, width = 12,
        
        #  # Gráfico principal
        #  plotlyOutput("grafico_mahalanobis_por_classe", height = "500px"),
        #  br(),
        
        #  # Sub-box para o teste Hotelling
        #  box(
        #    title = "Teste de Hotelling T²",
        #    status = "primary", solidHeader = TRUE, width = 12,
        #    p(strong("Null hypothesis:"), " The vector (multivariate) means of the original and synthetic samples are equal."),
        #    DTOutput("tabela_hotelling")
        #  )
        # ),
        
        
        
        ###ACRÉSCIMO DE GRÁFICO DA Distance from Mahalanobis EM 29/03/2025##
        
        
        
        ###TODOS OS TESTES PASSARAM A ESTAR DENTRO DA MESMA FLUIDROW EM 28/03/2025
        fluidRow(
          #######ACRÉSCIMO DA Jensen-Shannon divergence EM 28/03/2025#####
          
          #######ACRÉSCIMO DE TESTE DE KOLMOGOROV SMIRNOV EM 28/03/2025#######
          box(
            title = "Kolmogorov–Smirnov test",
            ##status = "primary", solidHeader = TRUE, width = 12, ##REDUÇÃO DO TAMANHO DA JANELA EM 28/03/2025
            status = "primary", solidHeader = TRUE, width = 6,
            #helpText("Comparação das distribuições das Original Samples e Synthetics."),
            p(strong("Null hypothesis:"), "The probability distribution of the original and synthetic samples are equal"),
            DTOutput("ks_por_classe")
          ),
          
          
          #######ACRÉSCIMO DE TESTE DE KOLMOGOROV SMIRNOV EM 28/03/2025#######
          
          
          ######ACRÉSCIMO DE Generalized Procrustes Test EM 28/03/2025####
          box(
            title = "Generalized Procrustes Test",
            width = 6, status = "primary", solidHeader = TRUE,
            ## width = 12, status = "primary", solidHeader = TRUE, ##REDUÇÃO DO TAMANHO DA JANELA EM 28/03/2025
            # helpText("Compara as estruturas das Original Samples e Synthetics usando o teste de Procrustes."),
            p(strong("Null hypothesis:"), "The structures of the original and synthetic samples are similar."),
            #p(strong("Obs:"), "Variables com desvio padrão igual a zero foram removidas."),
            p(em("Note: Variables with a standard deviation equal to zero were removed.")), # MODIFICADO EM 14/06/2025
            DTOutput("procrustes_tabela_teste")
          ),
          
          ######ACRÉSCIMO DE Generalized Procrustes Test EM 28/03/2025####
          
          
          
          
          ####ACRÉSCIMO DE T² DE HOTELLING E DIST.MAHALANOBIS EM 29/03/2025##
          
          ###MODIFICAÇÃO EM 30/03/2025 - ACRÉSCIMO DE TESTE DE Normality MULTIVARIADA
          
          #box(
          # #title = "Hotelling T² e Distance from Mahalanobis by class",
          #  title = "Hotelling's T² Test",
          #  width = 12, status = "primary", solidHeader = TRUE,
          
          #  #p(strong("Null hypothesis:"), " The multivariate distributions of the original and synthetic samples are the same for each class."),
          #  p(strong("Null hypothesis:"), "As médias vetoriais (multivariadas) das Original Samples e Synthetics são iguais"),
          
          
          #  shiny::hr(),
          
          ##  actionButton("run_hotelling", "Executar Comparação", class = "btn-success"),
          ##  shiny::hr(),
          
          #  DT::dataTableOutput("hotelling_table")
          #),
          
          
          #box(
          # title = "Hotelling's T² Test",
          #  width = 12, status = "primary", solidHeader = TRUE,
          
          #  p(strong("Null hypothesis:"), "The vector (multivariate) means of the original and synthetic samples are equal."),
          #  p(em("Observação: O Hotelling's T² Test só é válido se os dados apresentarem Normality multivariada para cada classe. Se essa condição não for atendida, os resultados não serão exibidos para a classe correspondente.")),
          #  p(em("Normality multivariada para cada classe: Teste de Henze-Zirkler.")),
          
          #  shiny::hr(),
          
          #  DT::dataTableOutput("hotelling_table")
          # ),
          
          ##ACRÉSCIMO DO TESTE DE ROYSTON(EXTENSÃO MULTIVARIADA DO TESTE DE SHAPIRO-WILK) EM 02/05/2025
          
          box(
            title = "Hotelling's T² Test",
            width = 12, status = "primary", solidHeader = TRUE,
            
            p(strong("Hotelling's T² Test null hypothesis:"), "The vector (multivariate) means of the original and synthetic samples are equal."),
            p(em("Note: Hotelling's T² Test is only valid if the data present multivariate normality for each class. If this condition is not met, the results are shown with a caveat.")),
            p(strong("Null hypothesis of the Henze-Zirkler test:"), "The data of the evaluated class follow a multivariate normal distribution."),
            p(strong("Null hypothesis of the Royston test (multivariate extension of the Shapiro-Wilk test):"), "The class variables jointly follow a multivariate normal distribution."),
            
            hr(),
            
            h4("Multivariate Normality Check by Class"),
            DT::dataTableOutput("Normality_table"),
            
            hr(),
            
            h4("Results by Class - Hotelling T²"),
            DT::dataTableOutput("hotelling_table")
          ),
          
          ##ACRÉSCIMO DO TESTE DE ROYSTON(EXTENSÃO MULTIVARIADA DO TESTE DE SHAPIRO-WILK) EM 02/05/2025
          ####ACRÉSCIMO DE T² DE HOTELLING E DIST.MAHALANOBIS EM 29/03/2025##
          
          
          ###ACRÉSCIMO DE PERMANOVA test EM 30/03/2025#####
          
          box(
            title = "PERMANOVA test", 
            width = 12, status = "primary", solidHeader = TRUE,
            
            p(strong("Null hypothesis:"),
              "The multivariate distributions of the original and synthetic samples are the same for each class."),
            p(em("Note: The PERMANOVA test (Permutation Multivariate Analysis of Variance) compares multivariate classes without assuming normality.")),
            p(em("Note: Variables with a standard deviation equal to zero were removed.")), # MODIFICADO EM 14/06/2025
            DT::dataTableOutput("permanova_table")
          ),
          
          
          ###ACRÉSCIMO DE PERMANOVA test EM 30/03/2025#####
          
          
          
          ############################################################################
          # Visualizações
          #    tabItem(
          #     tabName = "visual",
          #    fluidRow(
          #     box(
          #      title = "Class Distribution", width = 6, status = "info", solidHeader = TRUE,
          #     plotlyOutput("class_dist")
          #    # )
          #  ),
          
          #  # fluidRow(
          #  box(
          #   title = "2D Projection", width = 6, status = "info", solidHeader = TRUE,
          #  selectInput("projection", "Choose Projection:", 
          #             choices = c("PCA", "PCA Robusta", "t-SNE"), selected = "PCA"),
          #  plotlyOutput("projection_plot")
          #  )
          #  ),
          #  fluidRow(
          #   box(
          #    title = "Boxplot of Scores (PCA)", width = 6, status = "info", solidHeader = TRUE,
          #   plotlyOutput("boxplot_scores")
          #  ),
          # box(
          #  title = "Boxplot of Variables", width = 6, status = "info", solidHeader = TRUE,
          # plotlyOutput("boxplot_variables")
          #  ),
          
          ####MODIFICAÇÃO DOS TESTES DA GUIA RESULTADOS PARA A GUIA DE VISUALIZAÇÕES EM 07/03/2025
          
          #ACRÉSCIMO DA DESCRIÇÃO DA HIPÓTESE NULA EM Variance Tests EM 28/03/2025
          
          ##  fluidRow(
          box(
            title = "Variance Tests", width = 12, status = "primary", solidHeader = TRUE,
            p(strong("Null hypothesis:"), "The original and synthetic samples have equal variances"),
            
            selectInput("variance_test", "Choose the Variance Test:",
                        #choices = c("Levene's Test", "Bartlett's Test", "Fligner-Killeen Test", "F-Test"),
                        choices = c("Levene's Test", "Fligner-Killeen Test"),
                        selected = "Levene's Test"),
            actionButton("run_variance_test", "Run Test", class = "btn-success"),
            verbatimTextOutput("variance_test_results")
            
            
          ) 
        ) ,
        #    )
        
        
        
      ),
      
      #    # Resultados
      #   #ALTERAÇÃO EM 15/02/2025
      
      #    #ALTERAÇÃO DO NOME DO BOX de "Dados Originais" para "Original Samples"
      #   #ALTERAÇÃO DO NOME DO BOX de "Dados Combinados" para "Combined Samples"
      
      #  tabItem(
      #   tabName = "results",
      #  fluidRow(
      #   box(
      #    title = "Original Samples", width = 6, status = "warning", solidHeader = TRUE,
      #   DTOutput("original_data")
      #  ),
      
      # box(
      #  title = "Amostras Synthetics", width = 6, status = "warning", solidHeader = TRUE,
      # DTOutput("synthetic_only_data")
      
      #  ),
      # box(
      #  title = "Combined Samples", width = 6, status = "warning", solidHeader = TRUE,
      # DTOutput("synthetic_data")
      
      #  )
      #  ),
      
      #  #FUNCIONALIDADE DE EXPORTAÇÃO DE RESULTADOS EM 15/02/2025
      #  box(title = "Data Export", status = "info", solidHeader = TRUE, width = 12,
      #     selectInput("dataset_choice", "Choose the dataset:", 
      #                choices = c("Original Samples" = "original", "Amostras Synthetics" = "synthetic", "Combined Samples" = "combined")),
      #   selectInput("export_format", "Escolha o formato:", choices = c(".csv" = "csv", ".xls" = "xlsx")),
      #  downloadButton("download_selected", "Export Data")
      #  ),
      
      #  fluidRow(
      #   box(
      #    title = "Variance Tests", width = 12, status = "primary", solidHeader = TRUE,
      #   selectInput("variance_test", "Choose the Variance Test:",
      #              #choices = c("Levene's Test", "Bartlett's Test", "Fligner-Killeen Test", "F-Test"),
      #             choices = c("Levene's Test", "Fligner-Killeen Test"),
      #            selected = "Levene's Test"),
      #  actionButton("run_variance_test", "Run Test", class = "btn-success"),
      #  verbatimTextOutput("variance_test_results")
      #    )
      #  )
      #  ),
      #  # )
      #  # )
      #  # )
      
      ###########ATUALIZAÇÃO DOS CRÉDITOS EM 07/03/2025
      
      #  tabItem(
      #   tabName = "creditstab",
      #  h2("Credits"),
      # fluidRow(
      #  box(
      #   width = 12,
      #  h3("Credits"),
      # shiny::hr(),
      #  p(h4(strong("Gabriel Costa de Oliveira"))),
      #  p(a(href = "http://lattes.cnpq.br/0408944953044873", "http://lattes.cnpq.br/0408944953044873", target = "_blank")),
      #  p("costagabriel082000@gmail.com"),
      #  shiny::hr(),
      #  p(h4(strong("José Licarion Pinto Segundo Neto, Dsc."))),
      #  p(a(href = "http://lattes.cnpq.br/5267552018296169", "http://lattes.cnpq.br/5267552018296169", target = "_blank")),
      #  p("licarion@gmail.com"),
      #  shiny::hr(),
      
      #  #ATUALIZAÇÃO DA TITULAÇÃO DO ALEXANDER EM 17/01/2025
      
      # p(h4(strong("Alexânder de Paula Rodrigues, Dsc."))),
      #  p(a(href="http://lattes.cnpq.br/6201670166452372", "http://lattes.cnpq.br/6201670166452372",target="_blank")),
      #  p("alexanderdepaula@hotmail.com"),
      #  shiny::hr(),
      #  p(h4(strong("Paulo Sergio de Oliveira Cezario, Msc."))),
      #  p(a(href = "http://lattes.cnpq.br/5098915046998337", "http://lattes.cnpq.br/5098915046998337", target = "_blank")),
      #  p("paulcezario@gmail.com"),
      #  shiny::hr(),
      #  p(h4(strong("Bernardo Cardeal Goulart Darzé Santos"))),
      #  p(a(href="http://lattes.cnpq.br/0590620499595344", "http://lattes.cnpq.br/0590620499595344",target="_blank")),
      #  p("bernardocardeal@outlook.com"),
      #  shiny::hr(),
      
      # #ACRÉSCIMO DO AUTOR JULIO NOS CRÉDITOS EM 17/01/2025
      
      #  p(h4(strong("Julio Cesar Siqueira"))),
      # p(a(href="http://lattes.cnpq.br/1968914053746315", "http://lattes.cnpq.br/1968914053746315",target="_blank")),
      #  p("juliosiqueira86@hotmail.com")
      #  )
      #  ),
      
      
      tabItem(
        tabName = "creditstab",
        h2("Credits"),
        fluidRow(
          box(
            width = 12,
            h3("Credits"),
            shiny::hr(),
            p(h4(strong("Julio Cesar Siqueira"))),
            p(a(href="http://lattes.cnpq.br/1968914053746315", "http://lattes.cnpq.br/1968914053746315",target="_blank")),
            p("juliosiqueira86@hotmail.com"),
            shiny::hr(), #MODIFICADO EM 09/03/2025(LINHA ELEGANTE SEPARANDO OS NOMES)
            p(h4(strong("Aderval Severino Luna"))),
            p(a(href = "http://lattes.cnpq.br/0294676847895948", "http://lattes.cnpq.br/0294676847895948", target = "_blank")),
            p("adsluna@gmail.com"),
            shiny::hr(),  
            p(h4(strong("José Licarion Pinto Segundo Neto"))),
            p(a(href = "http://lattes.cnpq.br/5267552018296169", "http://lattes.cnpq.br/5267552018296169", target = "_blank")),
            p("licarion@gmail.com")
          )
        ),
        ###########ATUALIZAÇÃO DOS CRÉDITOS EM 07/03/2025
        fluidRow(
          box(
            width = 12,
            h3("Acknowledgements"),
            fluidRow(
              column(
                width = 12,
                "The authors are thankful to Fundação de Amparo à Pesquisa no Rio de Janeiro (FAPERJ) (grant number E-26/200.204/2023), Laboratório de Espectrometria de Massas e Espectrometria Atômica (LEAMS), Laboratório de Tecnologia Analítica de Processos (LTAP) and Universidade do Estado do Rio de Janeiro (UERJ), for the financial support, fellowships and facilities."
              )
            )
          )
        )
      ),
      
      #------Upsampling Methods
      
      tabItem(
        tabName = "smote",
        h2("SMOTE"),
        fluidRow(
          box(
            title = h3("SMOTE Parameters"),
            width = 4,
            status = "primary",
            numericInput('K', 'The number of nearest neighbors during sampling process:', 
                         value = 3, min = 1, max = Inf, step = 1),
            numericInput('dup_size', 'The maximum times of synthetic minority instances over original majority instances in the oversampling:', 
                         value = 1, min = 1, max = Inf, step = 1),
            shiny::hr(),
            uiOutput("target"),  # Seletor do atributo target em 12/02/2025
            uiOutput("class_in"),
            checkboxInput("smote_all_classes", "Perform SMOTE on the entire dataset", value = FALSE),
            
            ############TEXTO ADICIONADO EM 19/05/2025####################
            p(strong("Note: For databases with quantitative (numerical) and qualitative (categorical) variables, choose the SMOTE_NC method.")),
            
            ############TEXTO ADICIONADO EM 19/05/2025####################
            
            actionButton("runSMOTE", "Run SMOTE", class = "btn-block btn-success"),
            actionButton("confirmSMOTE", "Confirm SMOTE", class = "btn-block btn-success")
          ),
          box(title = h3("Syn_Data Overview"), 
              width = 8, 
              status = "primary", 
              tabsetPanel(
                tabPanel("Smote Summary", verbatimTextOutput("summary_gen")),
                tabPanel("Gen Data", tableOutput("gen_table")),
                #MODIFICADO EM 26/02/2025
                #tabPanel("Barplot", plotOutput("barplot_SMOTE"))
                tabPanel("Barplot", plotlyOutput("barplot_SMOTE")) 
              )
          )
        )
      ),
      
      
      ########SMOTE_NC ACRESCENTADO EM 08/05/2025###############
      
      tabItem(
        tabName = "smote_nc",
        h2("SMOTE_NC"),
        fluidRow(
          box(
            title = h3("SMOTE_NC Parameters"),
            width = 4,
            status = "primary",
            
            numericInput('K_nc', 'The number of nearest neighbors during sampling process:', 
                         value = 3, min = 1, max = Inf, step = 1),
            
            # Seletor único de estratégia de balanceamento
            selectInput(
              inputId = "smote_sampling_strategy",
              label = "Balancing type (sampling_strategy):",
              choices = c(
                "minority" = "minority",
                "not minority" = "not minority",
                "not majority" = "not majority",
                "all" = "all",
                "auto" = "auto"
              ),
              selected = "auto"
            ),
            #   helpText("Choose how classes should be balanced:
            #           \n- 'minority': only the minority class will be increased;
            #          \n- 'not minority': all except the minority;
            #         \n- 'not majority': all except the majority;
            #        \n- 'all': all classes;
            #       \n- 'auto': equivalent to 'not majority'."),
            
            p(strong("Choose how classes should be balanced:"), " "),
            p(strong(".minority"), em("only the minority class will be increased;")),
            p(strong(".not minority"), em("all except the minority;")),
            p(strong(".not majority"), em("all except the majority;")),
            p(strong(".all"), em("all classes;")),
            p(strong(".auto"), em("equivalent to 'not majority.")),
            
            
            shiny::hr(),
            uiOutput("target_nc"),  # Seletor do atributo target para SMOTE_NC
            ## uiOutput("class_in_nc"), ##OMITIDO EM 09/05/2025
            
            #COMANDO OMITIDO EM 08/05/2025(SAMPLING_ESTRATEGY RESOLVE ESSA QUESTÃO)
            ##checkboxInput("smote_all_classes_nc", "Perform SMOTE_NC on the entire dataset", value = FALSE),
            
            
            ############TEXTO ADICIONADO EM 19/05/2025####################
            p(strong("Note: For databases with all quantitative (numerical) variables, choose the SMOTE method.")),
            
            ############TEXTO ADICIONADO EM 19/05/2025####################
            
            actionButton("runSMOTENC", "Run SMOTE_NC", class = "btn-block btn-success"),
            actionButton("confirmSMOTENC", "Confirm SMOTE_NC", class = "btn-block btn-success")
          ),
          
          box(
            title = h3("Syn_Data NC Overview"), 
            width = 8, 
            status = "primary", 
            tabsetPanel(
              tabPanel("Smote_NC Summary", verbatimTextOutput("summary_gen_nc")),
              tabPanel("Gen_NC Data", tableOutput("gen_table_nc")),
              tabPanel("Barplot_NC", plotlyOutput("barplot_SMOTENC"))
            )
          )
        )
      ),
      
      
      ########SMOTE_NC ACRESCENTADO EM 08/05/2025###############
      
      
      
      ########BORDERLINE_SMOTE ACRESCENTADO EM 02/06/2025###############
      
      tabItem(
        tabName = "borderline_smote",
        h2("Borderline_SMOTE"),
        fluidRow(
          box(
            title = h3("Borderline_SMOTE Parameters"),
            width = 4,
            status = "primary",
            
            
            numericInput('K_border', 'Number of nearest neighbors during sampling process:', 
                         value = 5, min = 1, max = Inf, step = 1),
            
            numericInput('M_border', 'The nearest neighbors used to determine if a minority sample is in “danger”:', 
                         value = 10, min = 1, max = Inf, step = 1),  #PARÂMETRO M_BORDER OPÇÃO EXTRA DO MÉTODO ACRESCENTADO EM 02/06/2025
            
            
            
            # Seletor único de estratégia de balanceamento
            selectInput(
              inputId = "borderline_sampling_strategy",
              label = "Balancing type (sampling_strategy):",
              choices = c(
                "minority" = "minority",
                "not minority" = "not minority",
                "not majority" = "not majority",
                "all" = "all",
                "auto" = "auto"
              ),
              selected = "auto"
            ),
            
            p(strong("Choose how classes should be balanced:"), " "),
            p(strong(".minority"), em("only the minority class will be increased;")),
            p(strong(".not minority"), em("all except the minority;")),
            p(strong(".not majority"), em("all except the majority;")),
            p(strong(".all"), em("all classes;")),
            p(strong(".auto"), em("equivalent to 'not majority.")),
            
            shiny::hr(),
            uiOutput("target_borderline"),  # Seletor do atributo target para Borderline-SMOTE
            
            p(strong("Note: Borderline SMOTE applies only to quantitative (numerical) variables.")),
            
            actionButton("runBorderlineSMOTE", "Run Borderline_SMOTE", class = "btn-block btn-success"),
            actionButton("confirmBorderlineSMOTE", "Confirm Borderline_SMOTE", class = "btn-block btn-success")
          ),
          
          box(
            title = h3("Syn_Data Borderline Overview"), 
            width = 8, 
            status = "primary", 
            tabsetPanel(
              tabPanel("Borderline_SMOTE Summary", verbatimTextOutput("summary_gen_borderline")),
              tabPanel("Gen Borderline Data", tableOutput("gen_table_borderline")),
              tabPanel("Barplot Borderline", plotlyOutput("barplot_BorderlineSMOTE"))
            )
          )
        )
      ),
      
      ########BORDERLINE_SMOTE ACRESCENTADO EM 02/06/2025###############
      
      
      ########SVM_SMOTE ACRESCENTADO EM 06/06/2025###############
      
      tabItem(
        tabName = "svm_smote",
        h2("SVM_SMOTE"),
        fluidRow(
          box(
            title = h3("SVM_SMOTE Parameters"),
            width = 4,
            status = "primary",
            
            numericInput('K_svm', 'Number of nearest neighbors for sampling (k_neighbors):', 
                         value = 5, min = 1, max = Inf, step = 1),
            
            numericInput('M_svm', 'Number of majority class neighbors (m_neighbors):', 
                         value = 10, min = 1, max = Inf, step = 1),
            
            numericInput('SVM_estimator_C', 'SVM penalty parameter (C):', 
                         value = 1.0, min = 0.01, max = Inf, step = 0.1),
            
            selectInput(
              inputId = "svm_sampling_strategy",
              label = "Balancing type (sampling_strategy):",
              choices = c(
                "minority" = "minority",
                "not minority" = "not minority",
                "not majority" = "not majority",
                "all" = "all",
                "auto" = "auto"
              ),
              selected = "auto"
            ),
            
            p(strong("Choose how classes should be balanced:"), " "),
            p(strong(".minority"), em("only the minority class will be increased;")),
            p(strong(".not minority"), em("all except the minority;")),
            p(strong(".not majority"), em("all except the majority;")),
            p(strong(".all"), em("all classes;")),
            p(strong(".auto"), em("equivalent to 'not majority''.")),
            
            shiny::hr(),
            uiOutput("target_svm"),  # Seletor do atributo target para SVM_SMOTE
            
            p(strong("Note: SVM_SMOTE applies only to quantitative (numeric) variables.")),
            
            actionButton("runSVM_SMOTE", "Run SVM_SMOTE", class = "btn-block btn-success"),
            actionButton("confirmSVM_SMOTE", "Confirm SVM_SMOTE", class = "btn-block btn-success")
          ),
          
          box(
            title = h3("Syn_Data SVM Overview"), 
            width = 8, 
            status = "primary", 
            tabsetPanel(
              tabPanel("SVM_SMOTE Summary", verbatimTextOutput("summary_gen_svm")),
              tabPanel("Gen SVM Data", tableOutput("gen_table_svm")),
              tabPanel("Barplot SVM", plotlyOutput("barplot_SVM_SMOTE"))
            )
          )
        )
      ),
      
      ########SVM_SMOTE ACRESCENTADO EM 06/06/2025###############
      
      ######## ADASYN ADICIONADO EM 08/06/2025 ###############
      
      tabItem(
        tabName = "adasyn",
        h2("ADASYN"),
        fluidRow(
          box(
            title = h3("ADASYN Parameters"),
            width = 4,
            status = "primary",
            
            numericInput('K_adasyn', 'Number of nearest neighbors (n_neighbors):', 
                         value = 5, min = 1, max = Inf, step = 1),
            
            # Seletor único de estratégia de balanceamento
            selectInput(
              inputId = "adasyn_sampling_strategy",
              label = "Balancing type (sampling_strategy):",
              choices = c(
                "minority" = "minority",
                "not minority" = "not minority",
                "not majority" = "not majority",
                "all" = "all",
                "auto" = "auto"
              ),
              selected = "auto"
            ),
            
            p(strong("Choose how classes should be balanced:"), " "),
            p(strong(".minority"), em("only the minority class will be increased;")),
            p(strong(".not minority"), em("all except the minority;")),
            p(strong(".not majority"), em("all except the majority;")),
            p(strong(".all"), em("all classes;")),
            p(strong(".auto"), em("equivalent to 'not majority'.")),
            
            shiny::hr(),
            uiOutput("target_adasyn"),  # Seletor do atributo target para ADASYN
            
            p(strong("Note: ADASYN applies only to quantitative (numeric) variables.")),
            
            #OBSERVAÇÃO ADICIONADA EM 08/06/2025
            
            
            p(strong("Note:This method is similar to SMOTE but it generates different number of samples depending on an estimate of the local distribution of the class to be oversampled.")),
            
            
            actionButton("runADASYN", "Run ADASYN", class = "btn-block btn-success"),
            actionButton("confirmADASYN", "Confirm ADASYN", class = "btn-block btn-success")
          ),
          
          box(
            title = h3("Syn_Data ADASYN Overview"), 
            width = 8, 
            status = "primary", 
            tabsetPanel(
              tabPanel("ADASYN Summary", verbatimTextOutput("summary_gen_adasyn")),
              tabPanel("Gen ADASYN Data", tableOutput("gen_table_adasyn")),
              tabPanel("Barplot ADASYN", plotlyOutput("barplot_ADASYN"))
            )
          )
        )
      ),
      
      ######## ADASYN ADICIONADO EM 08/06/2025 ###############
      
      ######## RANDOM UPSAMPLING ADICIONADO EM 09/06/2025 ###############
      
      tabItem(
        tabName = "ru",
        h2("Random Upsampling"),
        fluidRow(
          box(
            title = h3("Random Upsampling Parameters"),
            width = 4,
            status = "primary",
            
            # Seletor de estratégia de balanceamento
            
            selectInput(
              inputId = "ru_sampling_strategy",
              label = "Balancing type (sampling_strategy):",
              choices = c(
                "minority" = "minority",
                "not minority" = "not minority",
                "not majority" = "not majority",
                "all" = "all",
                "auto" = "auto"
              ),
              selected = "auto"
            ),
            
            p(strong("Choose how classes should be balanced:"), " "),
            p(strong(".minority"), em("only the minority class will be increased;")),
            p(strong(".not minority"), em("all except the minority;")),
            p(strong(".not majority"), em("all except the majority;")),
            p(strong(".all"), em("all classes;")),
            p(strong(".auto"), em("equivalent to 'not majority'.")),
            
            shiny::hr(),
            
            # Seletor do target
            
            uiOutput("target_ru"),
            
            # Observações
            
            p(strong("Note:"), "Random Upsampling randomly replicates samples from the selected classes until they match the majority class."),
            p(strong("Note:"), "This method does not create new synthetic samples, it only replicates existing samples."),
            
            # Botões
            
            actionButton("runRU", "Run Random Upsampling", class = "btn-block btn-success"),
            actionButton("confirmRU", "Confirm Random Upsampling", class = "btn-block btn-success")
          ),
          
          box(
            title = h3("Syn_Data RU Overview"), 
            width = 8, 
            status = "primary", 
            tabsetPanel(
              tabPanel("RU Summary", verbatimTextOutput("summary_gen_ru")),
              tabPanel("Gen RU Data", tableOutput("gen_table_ru")),
              tabPanel("Barplot RU", plotlyOutput("barplot_RU"))
            )
          )
        )
      ),
      
      ######## RANDOM UPSAMPLING ADICIONADO EM 09/06/2025 ###############
      
      ######## TOMEK LINKS ADICIONADO EM 09/06/2025 ###############
      
      tabItem(
        tabName = "tl",
        h2("TOMEKLinks"),
        fluidRow(
          box(
            title = h3("Tomek Links Parameters"),
            width = 4,
            status = "primary",
            
            # Seletor de estratégia de balanceamento 
            
            selectInput(
              inputId = "tomek_sampling_strategy",
              label = "Balancing type (sampling_strategy):",
              choices = c(
                "auto" = "auto",
                "all" = "all",
                "majority" = "majority",
                "not minority" = "not minority",
                "not majority" = "not majority"
              ),
              selected = "auto"
            ),
            
            p(strong("Choose how classes should be treated upon removal:"), " "),
            p(strong(".auto"), em("removes samples from the majority class that form Tomek pairs;")),
            p(strong(".all"), em("remove both samples in each Tomek pair;")),
            p(strong(".majority"), em("remove only the majority class sample in the pairs;")),
            p(strong(".not minority"), em("remove all but the minority;")),
            p(strong(".not majority"), em("remove all but the majority.")),
            
            
            shiny::hr(),
            
            # Seletor do target
            uiOutput("target_tomek"),
            
            # Observações
            p(strong("Note:"), "Tomek Links is a downsampling method that removes noise and ambiguous samples near the class boundary."),
            p(strong("Note:"), "This method is especially useful for cleaning up regions of overlap between classes."),
            
            # Botões
            actionButton("runTomek", "Run Tomek Links", class = "btn-block btn-success"),
            actionButton("confirmTomek", "Confirm Tomek Links", class = "btn-block btn-success")
          ),
          
          box(
            title = h3("Tomek Links Overview"), 
            width = 8, 
            status = "primary", 
            tabsetPanel(
              tabPanel("Tomek Summary", verbatimTextOutput("summary_gen_tomek")),
              tabPanel("Gen Tomek Data", tableOutput("gen_table_tomek")),
              tabPanel("Barplot Tomek", plotlyOutput("barplot_Tomek"))
            )
          )
        )
      ),
      
      ######## TOMEK LINKS ADICIONADO EM 09/06/2025 ###############
      
      ######## NEARMISS ADICIONADO EM 16/06/2025 ########
      
      tabItem(
        tabName = "nm",
        h2("NearMiss Undersampling"),
        fluidRow(
          box(
            title = h3("NearMiss Parameters"),
            width = 4,
            status = "primary",
            
            # Estratégia de balanceamento
            
            selectInput(
              inputId = "nearmiss_sampling_strategy",
              label = "Balancing type (sampling_strategy):",
              choices = c(
                "auto" = "auto",
                "all" = "all",
                "majority" = "majority",
                "not minority" = "not minority",
                "not majority" = "not majority"
              ),
              selected = "auto"
            ),
            
            
            p(strong("Choose how classes should be treated in majority sample selection:")),
            p(strong(".auto:"), em("Remove samples from the majority class until it balances with the minority class;")),
            p(strong(".all:"), em("Remove samples from all classes until balanced across all;")),
            p(strong(".majority:"), em("Removes exclusively from the majority class;")),
            p(strong(".not minority:"), em("Removes from all classes except the minority;")),
            p(strong(".not majority:"), em("Removes from all classes except the majority.")),
            
            
            
            shiny::hr(),
            
            # Versão do NearMiss
            
            selectInput(
              inputId = "version_nearmiss",
              label = "NearMiss algorithm version:",
              choices = c("1", "2", "3"),
              selected = "1"
            ),
            
            # Número de vizinhos (versões 1 e 2)
            
            numericInput(
              inputId = "n_neighbors_nearmiss",
              label = "Number of neighbors (n_neighbors):",
              value = 3,
              min = 1
            ),
            
            # Número de vizinhos da minoria (versão 3)
            
            numericInput(
              inputId = "n_neighbors_ver3_nearmiss",
              label = "Neighbors for Minority (n_neighbors ver3 - version 3 only):",
              value = 3,
              min = 1
            ),
            
            # Número de threads (paralelização)
            
            numericInput(
              inputId = "n_jobs_nearmiss",
              label = "Number of threads (n_jobs):",
              value = -1,
              min = -1
            ),
            
            # Seletor da variável-alvo
            
            uiOutput("target_nearmiss"),
            
            hr(),
            
            # Explicações informativas
            
            p(strong("About NearMiss:")),
            p("Distance-based undersampling method."),
            p("Version 1 selects samples from the majority class with the smallest average distance to the k neighbors of the minority;"),
            p("Version 2 considers the average distance of the k most distant neighbors of the minority;"),
            p("Version 3 selects samples from the majority with the smallest distance to the k neighbors of the minority."),
            
            hr(),
            
            # Botões de ação
            
            actionButton("runNearMiss", "Run NearMiss", class = "btn-block btn-success"),
            actionButton("confirmNearMiss", "Confirm NearMiss", class = "btn-block btn-success")
          ),
          
          box(
            title = h3("NearMiss Overview"), 
            width = 8, 
            status = "primary", 
            tabsetPanel(
              tabPanel("NearMiss Summary", verbatimTextOutput("summary_gen_nearmiss")),
              tabPanel("Gen NearMiss Data", tableOutput("gen_table_nearmiss")),
              tabPanel("Barplot NearMiss", plotlyOutput("barplot_NearMiss"))
            )
          )
        )
      ),
      ######## NEARMISS ADICIONADO EM 16/06/2025 #########
      
      ######## EDITED NEAREST NEIGHBOURS ADICIONADO EM 17/06/2025 ########
      
      tabItem(
        tabName = "enn",
        h2("Edited Nearest Neighbours (ENN)"),
        fluidRow(
          box(
            title = h3("ENN Parameters"),
            width = 4,
            status = "primary",
            
            # Estratégia de balanceamento
            selectInput(
              inputId = "enn_sampling_strategy",
              label = "Balancing type (sampling_strategy):",
              choices = c(
                "auto" = "auto",
                "all" = "all",
                "majority" = "majority",
                "not minority" = "not minority",
                "not majority" = "not majority"
              ),
              selected = "auto"
            ),
            
            p(strong("Choose how classes should be treated in neighbor-based editing:")),
            p(strong(".auto:"), em("Remove samples from the majority classes until balanced with the minority;")),
            p(strong(".all:"), em("Remove samples from all classes until fully balanced;")),
            p(strong(".majority:"), em("Removes only from the majority class;")),
            p(strong(".not minority:"), em("Removes from all classes except the minority;")),
            p(strong(".not majority:"), em("Removes from all classes except the majority.")),
            
            shiny::hr(),
            
            # Número de vizinhos
            numericInput(
              inputId = "enn_n_neighbors",
              label = "Number of neighbors (n_neighbors):",
              value = 3,
              min = 1
            ),
            
            # Tipo de seleção
            selectInput(
              inputId = "enn_kind_sel",
              label = "Removal criteria (kind_sel):",
              
               ##BLOCO MODIFICADO EM 03/07/2025( 'MODE' DANDO QUEBRA NA INTERFACE)
              
           #   choices = c(
          #      "all" = "all",
          #      "mode" = "mode"
          #    ),
          #    selected = "all"
          #  ),
            
          choices = c("all" = "all"),
          selected = "all"
            ),
          
          
          
            ##BLOCO MODIFICADO EM 03/07/2025( 'MODE' DANDO QUEBRA NA INTERFACE)
            
            
            
            
            # Número de threads (paralelização)
            numericInput(
              inputId = "enn_n_jobs",
              label = "Number of threads (n_jobs):",
              value = -1,
              min = -1
            ),
            
            # Seletor da variável-alvo
            uiOutput("target_enn"),
            
            hr(),
            
            # Explicações informativas
            p(strong("About Edited Nearest Neighbors (ENN):")),
            p("Downsampling technique that removes samples based on disagreement with their nearest neighbors;"),
            p("Samples are removed if their class is different from most (or all) of their neighbors;"),
            p(strong("kind_sel = 'all':"), " removes samples whose neighbors all have different classes."),
           # p(strong("kind_sel = 'mode':"), " remove amostras cuja classe é diferente da moda dos vizinhos."), #retirado em  03/07/2025
            
            hr(),
            
            # Botões de ação
            actionButton("runENN", "Run ENN", class = "btn-block btn-success"),
            actionButton("confirmENN", "Confirm ENN", class = "btn-block btn-success")
          ),
          
          box(
            title = h3("ENN Overview"), 
            width = 8, 
            status = "primary", 
            tabsetPanel(
              tabPanel("ENN Summary", verbatimTextOutput("summary_gen_enn")),
              tabPanel("Gen ENN Data", tableOutput("gen_table_enn")),
              tabPanel("Barplot ENN", plotlyOutput("barplot_ENN"))
            )
          )
        )
      ),
      
      ######## EDITED NEAREST NEIGHBOURS ADICIONADO EM 17/06/2025 ########
      
      
      ######## ONE-SIDED SELECTION (OSS) ADICIONADO EM 18/06/2025 ########
      
      tabItem(
        tabName = "oss",
        h2("One-Sided Selection (OSS)"),
        fluidRow(
          box(
            title = h3("OSS Parameters"),
            width = 4,
            status = "primary",
            
            # Estratégia de balanceamento
            
            selectInput(
              inputId = "oss_sampling_strategy",
              label = "Balancing type (sampling_strategy):",
              choices = c(
                "auto" = "auto",
                "all" = "all",
                "majority" = "majority",
                "not minority" = "not minority",
                "not majority" = "not majority"
              ),
              selected = "auto"
            ),
            
            p(strong("Choose how classes should be treated in the removal process:")),
            p(strong(".auto:"), em("Remove samples from the majority classes until balanced with the minority.")),
            p(strong(".all:"), em("Remove samples from all classes until fully balanced;")),
            p(strong(".majority:"), em("Removes only from the majority class;")),
            p(strong(".not minority:"), em("Removes from all classes except the minority;")),
            p(strong(".not majority:"), em("Removes from all classes except the majority.")),
            
            shiny::hr(),
            
            # Número de vizinhos
            
            numericInput(
              inputId = "oss_n_neighbors",
              label = "Number of neighbors (n_neighbors):",
              value = 3,
              min = 1
            ),
            
            # Número de amostras SEEDS 
            
            numericInput(
              inputId = "oss_n_seeds_S",
              label = "Number of SEEDs samples (n_seeds_S):",
              value = 1,
              min = 1
            ),
            
            # Semente aleatória para reprodutibilidade
            
            numericInput(
              inputId = "oss_random_state",
              label = "Random seed (random_state):",
              value = 42
            ),
            
            # Número de threads (paralelização)
            
            numericInput(
              inputId = "oss_n_jobs",
              label = "Number of threads (n_jobs):",
              value = -1,
              min = -1
            ),
            
            # Seletor da variável-alvo
            
            uiOutput("target_oss"),
            
            hr(),
            
            # Explicações informativas
            
            p(strong("About One-Sided Selection (OSS):")),
            p("It combines noise removal (via ENN) with the idea of reducing instances of the majority class."),
            p("OSS uses minority samples as seeds (SEEDs) and removes unnecessary majority samples."),
            p("It is useful when you want to keep as many relevant examples from the minority as possible with less noise."),
            p(strong("n_neighbors:"), " defines how many neighbors the algorithm considers when deciding removal."),
            p(strong("n_seeds_S:"), " number of examples from the minority that are used as reference (seeds)."),
            p(strong("random_state:"), " controls randomness for reproducibility."),
            p(strong("n_jobs:"), " allows process parallelization. Use -1 to use all cores."),
            
            hr(),
            
            # Botões de ação
            
            actionButton("runOSS", "Run OSS", class = "btn-block btn-success"),
            actionButton("confirmOSS", "Confirm OSS", class = "btn-block btn-success")
          ),
          
          box(
            title = h3("OSS Overview"), 
            width = 8, 
            status = "primary", 
            tabsetPanel(
              tabPanel("OSS Summary", verbatimTextOutput("summary_gen_oss")),
              tabPanel("Gen OSS Data", tableOutput("gen_table_oss")),
              tabPanel("Barplot OSS", plotlyOutput("barplot_OSS"))
            )
          )
        )
      ),
      
      ######## ONE-SIDED SELECTION (OSS) ADICIONADO EM 18/06/2025 ########
      
      
      ######## RANDOM DOWNSAMPLING ADICIONADO EM 18/06/2025 ########
      
      tabItem(
        tabName = "rd",
        h2("Random Downsampling"),
        fluidRow(
          box(
            title = h3("Random Downsampling Parameters"),
            width = 4,
            status = "primary",
            
            # Estratégia de balanceamento
            
            selectInput(
              inputId = "rd_sampling_strategy",
              label = "Balancing type (sampling_strategy):",
              choices = c(
                "auto" = "auto",
                "all" = "all",
                "majority" = "majority",
                "not minority" = "not minority",
                "not majority" = "not majority"
              ),
              selected = "auto"
            ),
            
            p(strong("Choose how classes should be treated in the removal process:")),
            p(strong(".auto:"), em("Remove samples from the majority classes until balanced with the minority.")),
            p(strong(".all:"), em("Remove samples from all classes until fully balanced.")),
            p(strong(".majority:"), em("Removes only from the majority class.")),
            p(strong(".not minority:"), em("Removes from all classes except the minority.")),
            p(strong(".not majority:"), em("Removes from all classes except the majority.")),
            
            shiny::hr(),
            
            # Semente aleatória
            
            numericInput(
              inputId = "rd_random_state",
              label = "Random seed (random_state):",
              value = 42
            ),
            
            # Substituição (replacement)
            
            checkboxInput(
              inputId = "rd_replacement",
              label = "Allow replacement when subsampling?",
              value = FALSE
            ),
            
            # Seletor da variável-alvo
            
            uiOutput("target_rd"),
            
            hr(),
            
            # Explicações informativas
            
            p(strong("About Random Downsampling:")),
            p("Randomly remove samples from the majority classes to balance the dataset."),
            p("It is useful when the goal is to reduce bias caused by imbalanced classes, without introducing new data."),
            p(strong("sampling_strategy:"), " defines how balancing will be done between classes."),
            p(strong("random_state:"), " controls randomness for reproducibility."),
            p(strong("replacement:"), " if true, samples can be removed with replacement (repetition possible)."),
            
            hr(),
            
            # Botões de ação
            
            actionButton("runRD", "Run Random Downsampling", class = "btn-block btn-success"),
            actionButton("confirmRD", "Confirm Random Downsampling", class = "btn-block btn-success")
          ),
          
          box(
            title = h3("Random Downsampling Overview"), 
            width = 8, 
            status = "primary", 
            tabsetPanel(
              tabPanel("RD Summary", verbatimTextOutput("summary_gen_rd")),
              tabPanel("Gen RD Data", tableOutput("gen_table_rd")),
              tabPanel("Barplot RD", plotlyOutput("barplot_RD"))
            )
          )
        )
      ),
      
      ######## RANDOM DOWNSAMPLING ADICIONADO EM 18/06/2025 ########
      
      ######## SBC (Undersampling Based on Clustering) ADICIONADO EM 09/07/2025 ########
      
     tabItem(
        tabName = "sbc",
        h2("Undersampling Based on Clustering (SBC)"),
        fluidRow(
          box(
            title = h3("SBC Parameters"),
            width = 4,
            status = "primary",
            
            numericInput(
              inputId = "sbc_target_n",
              label = "Desired final size for each class (m):",
              value = 20,
              min = 1
            ),
            
            numericInput(
              inputId = "sbc_k",
              label = "Number of clusters k:",
              value = 2,
              min = 1
            ),
            
            uiOutput("target_sbc"),
            
            uiOutput("classes_to_undersample_sbc"),
            
            hr(),
            p(strong("About SBC (undersample k means):")),
            p("Performs cluster-based subsampling for the selected classes."),
            p("Unselected classes remain intact."),
            
            hr(),
            
            actionButton("runSBC", "Run SBC", class = "btn-block btn-success"),
            actionButton("confirmSBC", "Confirm SBC", class = "btn-block btn-success")
          ),
          
          box(
            title = h3("SBC Overview"),
            width = 8,
            status = "primary",
            tabsetPanel(
              tabPanel("SBC Summary", verbatimTextOutput("summary_gen_sbc")),
              tabPanel("Gen SBC Data", tableOutput("gen_table_sbc")),
              tabPanel("Barplot SBC", plotlyOutput("barplot_SBC"))
            )
          )
        )
      ),
      
      
      
            
      ######## SBC (Undersampling Based on Clustering) ADICIONADO EM 09/07/2025 ########
      
      
      
      ######## SMOTE TOMEK LINKS(SMOTE_TL) ADICIONADO EM 18/06/2025 ########
      
      tabItem(
        tabName = "smote_tl",
        h2("SMOTE Tomek Links"),
        fluidRow(
          box(
            title = h3("SMOTE Tomek Links Parameters"),
            width = 4,
            status = "primary",
            
            # Estratégia de amostragem
            
            selectInput(
              inputId = "tl_sampling_strategy",
              label = "Balancing type (sampling_strategy):",
              choices = c(
                "auto" = "auto",
                "all" = "all",
                "minority" = "minority",
                "not minority" = "not minority",
                "not majority" = "not majority"
              ),
              selected = "auto"
            ),
            
            p(strong("Choose how classes should be treated in the over + undersampling process:")),
            p(strong(".auto:"), em("Resample all classes to match the majority.")),
            p(strong(".all:"), em("Resample all classes for full balance.")),
            p(strong(".minority:"), em("Only the minority class is oversampled.")),
            p(strong(".not minority:"), em("All classes except the minority are treated.")),
            p(strong(".not majority:"), em("All classes except the majority are treated.")),
            
            shiny::hr(),
            
            # Número de vizinhos
            
            numericInput(
              inputId = "tl_k_neighbors",
              label = "Number of neighbors (k_neighbors):",
              value = 5,
              min = 1
            ),
            
            # Semente aleatória
            
            numericInput(
              inputId = "tl_random_state",
              label = "Random seed (random_state):",
              value = 42
            ),
            
            # Seletor da variável-alvo
            
            uiOutput("target_tl"),
            
            hr(),
            
            # Explicações informativas
            
            p(strong("About SMOTE Tomek Links:")),
            p("It combines SMOTE (generation of synthetic samples from the minority class) with Tomek Links (removal of ambiguous samples between classes)."),
            p("It is useful for improving separability between classes while reducing overlap."),
            p(strong("sampling_strategy:"), " defines how classes will be balanced."),
            p(strong("k_neighbors:"), " number of neighbors used to generate new synthetic samples."),
            p(strong("random_state:"), " sets the randomness seed for reproducibility."),
            
            hr(),
            
            # Botões de ação
            
            actionButton("runSMOTETL", "Run SMOTE Tomek Links", class = "btn-block btn-success"),
            actionButton("confirmSMOTETL", "Confirm SMOTE Tomek Links", class = "btn-block btn-success")
          ),
          
          box(
            title = h3("SMOTE Tomek Links Overview"), 
            width = 8, 
            status = "primary", 
            tabsetPanel(
              tabPanel("SMOTE TL Summary", verbatimTextOutput("summary_gen_tl")),
              tabPanel("Gen SMOTE TL Data", tableOutput("gen_table_tl")),
              tabPanel("Barplot SMOTE TL", plotlyOutput("barplot_TL"))
            )
          )
        )
      ),
      
      ######## SMOTE TOMEK LINKS(SMOTE_TL) ADICIONADO EM 18/06/2025 ########
      
      ######## SMOTE ENN (SMOTE_ENN) ADICIONADO EM 03/07/2025 ########
      
      tabItem(
        tabName = "smote_enn",
        h2("SMOTE ENN"),
        fluidRow(
          box(
            title = h3("SMOTE ENN Parameters"),
            width = 4,
            status = "primary",
            
            # Estratégia de amostragem
            
            selectInput(
              inputId = "enn_sampling_strategy2",
              label = "Balancing type (sampling_strategy)",
              choices = c(
                "auto" = "auto",
                "all" = "all",
                "minority" = "minority",
                "not minority" = "not minority",
                "not majority" = "not majority"
              ),
              selected = "auto"
            ),
            
            p(strong("Choose how classes should be treated in the over + undersampling process:")),
            p(strong(".auto:"), em("Resample all classes to match the majority.")),
            p(strong(".all:"), em("Resample all classes for full balance.")),
            p(strong(".minority:"), em("Only the minority class is oversampled.")),
            p(strong(".not minority:"), em("All classes except the minority.")),
            p(strong(".not majority:"), em("All classes except the majority.")),
            
            shiny::hr(),
            
            # Número de vizinhos
            
            numericInput(
              inputId = "enn_k_neighbors2",
              label = "Number of neighbors (k_neighbors):",
              value = 5,
              min = 1
            ),
            
            # Semente aleatória
            
            numericInput(
              inputId = "enn_random_state2",
              label = "Random seed (random_state):",
              value = 42
            ),
            
            # Seletor da variável-alvo
            
            uiOutput("target_enn2"),
            
            hr(),
            
            # Explicações informativas
            
            p(strong("About SMOTE ENN:")),
            p("It combines SMOTE (synthetic sample generation) with Edited Nearest Neighbors (removal of samples misclassified by their neighbors)."),
            p("It is useful for reducing noise and improving class separation."),
            p(strong("sampling_strategy:"), " defines how classes will be balanced."),
            p(strong("k_neighbors:"), " number of neighbors used to generate new synthetic samples and to apply ENN."),
            p(strong("random_state:"), " sets the random seed for reproducibility."),
            
            hr(),
            
            # Botões de ação
            
            actionButton("runSMOTEENN", "Run SMOTE ENN", class = "btn-block btn-success"),
            actionButton("confirmSMOTEENN", "Confirm SMOTE ENN", class = "btn-block btn-success")
          ),
          
          box(
            title = h3("SMOTE ENN Overview"), 
            width = 8, 
            status = "primary", 
            tabsetPanel(
              tabPanel("SMOTE ENN Summary", verbatimTextOutput("summary_gen_enn2")),
              tabPanel("Gen SMOTE ENN Data", tableOutput("gen_table_enn2")),
              tabPanel("Barplot SMOTE ENN", plotlyOutput("barplot_ENN2"))
            )
          )
        )
      ),
      
      ######## SMOTE ENN (SMOTE_ENN) ADICIONADO EM 03/07/2025 ########
      
 
     ######## SMOTE IPF (SMOTE_IPF) ADICIONADO EM 11/07/2025 ########
     
     tabItem(
       tabName = "smote_ipf",
       h2("SMOTE IPF (Iterative Partitioning Filter)"),
       fluidRow(
         box(
           title = h3("SMOTE IPF Parameters"),
           width = 4,
           status = "primary",
           
           # Número de vizinhos SMOTE
           
           numericInput(
             inputId = "ipf_k_neighbors",
             label = "Number of neighbors for SMOTE (k_neighbors):",
             value = 5,
             min = 1
           ),
           
           # Número de estimadores para o classificador de base (IPF)
           
           numericInput(
             inputId = "ipf_classifiers",
             label = "Number of estimators for the IPF filter (classifiers):",
             value = 5,
             min = 1
           ),
           
           # Número máximo de iterações do filtro IPF
           
           numericInput(
             inputId = "ipf_max_iter",
             label = "Maximum number of IPF iterations (max_iter):",
             value = 20,
             min = 1
           ),
           
           # Semente aleatória
           
           numericInput(
             inputId = "ipf_random_state",
             label = "Random seed (random_state):",
             value = 42
           ),
           
           # Seletor da variável-alvo
           
           uiOutput("target_ipf"),
           
           hr(),
           
           # Explicações informativas
           
           p(strong("About SMOTE IPF:")),
           p("Combines SMOTE with an IPF (Iterative Partitioning Filter) filter to iteratively remove noisy samples."),
           p("Useful when you want to improve the quality of synthetic samples by eliminating noise."),
           p(strong("k_neighbors:"), " number of neighbors used by SMOTE to generate samples."),
           p(strong("classifiers:"), " number of base estimators to identify noise."),
           p(strong("max_iter:"), " maximum number of iterations for the IPF filter."),
           p(strong("random_state:"), " sets the seed for reproducibility."),
           
           hr(),
           
           # Botões de ação
           
           actionButton("runSMOTEIPF", "Run SMOTE IPF", class = "btn-block btn-success"),
           actionButton("confirmSMOTEIPF", "Confirm SMOTE IPF", class = "btn-block btn-success")
         ),
         
         box(
           title = h3("SMOTE IPF Overview"),
           width = 8,
           status = "primary",
           tabsetPanel(
             tabPanel("SMOTE IPF Summary", verbatimTextOutput("summary_gen_ipf")),
             tabPanel("Gen SMOTE IPF Data", tableOutput("gen_table_ipf")),
             tabPanel("Barplot SMOTE IPF", plotlyOutput("barplot_IPF"))
           )
         )
       )
     ),
     
     ######## SMOTE IPF (SMOTE_IPF) ADICIONADO EM 11/07/2025 ########
     
     ######## SPIDER ADICIONADO EM 14/07/2025 ########
     
     tabItem(
       tabName = "spider",
       h2("SPIDER (Small disjuncts and Borderline examples elimination)"),
       fluidRow(
         box(
           title = h3("SPIDER Parameters"),
           width = 4,
           status = "primary",
           
           # Número de vizinhos Borderline-SMOTE
           numericInput(
             inputId = "spider_k_neighbors",
             label = "Number of neighbors for Borderline-SMOTE (k_neighbors):",
             value = 5,
             min = 1
           ),
           
           # Filtro de ruído: ENN fixo
           p(strong("Noise filter:"),
             " ENN (Edited Nearest Neighbors) will be used automatically after Borderline-SMOTE."),
           
           # Semente aleatória
           numericInput(
             inputId = "spider_random_state",
             label = "Random seed (random_state):",
             value = 42
           ),
           
           # Seletor da variável-alvo
           uiOutput("target_spider"),
           
           hr(),
           
           # Explicações informativas
           
           p(strong("About SPIDER:")),
           p("SPIDER combines Borderline-SMOTE with an ENN noise filter."),
           p("Generates new synthetic samples in borderline regions and removes noise (small disjunctions)."),
           p(strong("k_neighbors:"), " number of neighbors used by Borderline-SMOTE to generate samples."),
           p(strong("random_state:"), " sets the seed for reproducibility."),
           
           hr(),
           
           # Botões de ação
           
           actionButton("runSPIDER", "Run SPIDER", class = "btn-block btn-success"),
           actionButton("confirmSPIDER", "Confirm SPIDER", class = "btn-block btn-success")
         ),
         
         box(
           title = h3("SPIDER Overview"),
           width = 8,
           status = "primary",
           tabsetPanel(
             tabPanel("SPIDER Summary", verbatimTextOutput("summary_gen_spider")),
             tabPanel("Gen SPIDER Data", tableOutput("gen_table_spider")),
             tabPanel("Barplot SPIDER", plotlyOutput("barplot_spider"))
           )
         )
       )
     ),
     
     ######## SPIDER ADICIONADO EM 14/07/2025 ########
     
      
      
      
      
      #TRECHO INIBIDO EM 13/03/2025
      #  fluidRow(
      #   box(
      #    title = h3("SMOTE Results"),
      #   width = 12,
      #  status = "primary",
      # shiny::hr(),
      #  DTOutput("smoteTable"),
      #  downloadButton("downloadSmoteTable", "Download SMOTE Table (CSV)"),
      #  downloadButton("downloadSmoteTableExcel", "Download SMOTE Table (Excel)")
      #  )
      #  )
      # ),
      #TRECHO INIBIDO EM 13/03/2025
      
      #ACRÉSCIMO DOS PACOTES shinyjs, skimr,rrcov,mdatools,Rtsne,reshape2,car,stats,openxlsx 
      #LISTADOS ABAIXO EM 20/02/2025
      tabItem(
        tabName = "referencestab",
        h2("References"),
        fluidRow(
          box(
            width = 12,
            h3("Reference Packages"),
            shiny::hr(),
            p(h4(strong("shiny"))),
            p(a(href = "https://cran.r-project.org/web/packages/shiny/shiny.pdf", "https://cran.r-project.org/web/packages/shiny/shiny.pdf", target = "_blank")),
            shiny::hr(),
            p(h4(strong("shinyjs"))),
            p(a(href = "https://cran.r-project.org/web/packages/shinyjs/shinyjs.pdf", "https://cran.r-project.org/web/packages/shinyjs/shinyjs.pdf", target = "_blank")),
            shiny::hr(),
            p(h4(strong("shinydashboard"))),
            p(a(href = "https://cran.r-project.org/web/packages/shinydashboard/shinydashboard.pdf", "https://cran.r-project.org/web/packages/shinydashboard/shinydashboard.pdf", target = "_blank")),
            shiny::hr(),
            p(h4(strong("smotefamily"))),
            p(a(href = "https://cran.r-project.org/web/packages/smotefamily/smotefamily.pdf", "https://cran.r-project.org/web/packages/smotefamily/smotefamily.pdf", target = "_blank")), 
            shiny::hr(),
            p(h4(strong("readxl"))),
            p(a(href = "https://cran.r-project.org/web/packages/readxl/readxl.pdf", "https://cran.r-project.org/web/packages/readxl/readxl.pdf", target = "_blank")),
            shiny::hr(),
            p(h4(strong("writexl"))),
            p(a(href = "https://cran.r-project.org/web/packages/writexl/writexl.pdf", "https://cran.r-project.org/web/packages/writexl/writexl.pdf", target = "_blank")),
            shiny::hr(),
            p(h4(strong("DT"))),
            p(a(href = "https://cran.r-project.org/web/packages/DT/DT.pdf", "https://cran.r-project.org/web/packages/DT/DT.pdf", target = "_blank")),
            shiny::hr(),
            p(h4(strong("ggplot2"))),
            p(a(href = "https://cran.r-project.org/web/packages/ggplot2/ggplot2.pdf", "https://cran.r-project.org/web/packages/ggplot2/ggplot2.pdf", target = "_blank")),
            shiny::hr(),
            p(h4(strong("reticulate"))),
            p(a(href = "https://cran.r-project.org/web/packages/reticulate/reticulate.pdf", "https://cran.r-project.org/web/packages/reticulate/reticulate.pdf", target = "_blank")),
            shiny::hr(),
            p(h4(strong("shinycssloaders"))),
            p(a(href = "https://cran.r-project.org/web/packages/shinycssloaders/shinycssloaders.pdf", "https://cran.r-project.org/web/packages/shinycssloaders/shinycssloaders.pdf", target = "_blank")),
            shiny::hr(),
            p(h4(strong("plotly"))),
            p(a(href = "https://cran.r-project.org/web/packages/plotly/plotly.pdf", "https://cran.r-project.org/web/packages/plotly/plotly.pdf", target = "_blank")),
            shiny::hr(),
            p(h4(strong("UBL"))),
            p(a(href = "https://cran.r-project.org/web/packages/UBL/UBL.pdf", "https://cran.r-project.org/web/packages/UBL/UBL.pdf", target = "_blank")),
            shiny::hr(),
            p(h4(strong("caret"))),
            p(a(href = "https://cran.r-project.org/web/packages/caret/caret.pdf", "https://cran.r-project.org/web/packages/caret/caret.pdf", target = "_blank")),
            shiny::hr(),
            p(h4(strong("imbalance"))),
            p(a(href = "https://cran.r-project.org/web/packages/imbalance/imbalance.pdf", "https://cran.r-project.org/web/packages/imbalance/imbalance.pdf", target = "_blank")),
            shiny::hr(),
            p(h4(strong("tibble"))),
            p(a(href = "https://cran.r-project.org/web/packages/tibble/tibble.pdf", "https://cran.r-project.org/web/packages/tibble/tibble.pdf", target = "_blank")),
            shiny::hr(),
            p(h4(strong("Imbalanced-learn"))),
            p(a(href = "https://imbalanced-learn.org/stable/references/index.html", "https://imbalanced-learn.org/stable/references/index.html", target = "_blank")),
            shiny::hr(),
            p(h4(strong("psych"))),
            p(a(href = "https://cran.r-project.org/web/packages/psych/psych.pdf", "https://cran.r-project.org/web/packages/psych/psych.pdf", target = "_blank")),
            shiny::hr(),
            p(h4(strong("skimr"))),
            p(a(href = "https://cran.r-project.org/web/packages/skimr/skimr.pdf", "https://cran.r-project.org/web/packages/skimr/skimr.pdf", target = "_blank")),
            shiny::hr(),
            p(h4(strong("rrcov"))),
            p(a(href = "https://cran.r-project.org/web/packages/rrcov/rrcov.pdf", "https://cran.r-project.org/web/packages/rrcov/rrcov.pdf", target = "_blank")),
            shiny::hr(),
            p(h4(strong("mdatools"))),
            p(a(href = "https://cran.r-project.org/web/packages/mdatools/mdatools.pdf", "https://cran.r-project.org/web/packages/mdatools/mdatools.pdf", target = "_blank")),
            shiny::hr(),
            p(h4(strong("Rtsne"))),
            p(a(href = "https://cran.r-project.org/web/packages/Rtsne/Rtsne.pdf", "https://cran.r-project.org/web/packages/Rtsne/Rtsne.pdf", target = "_blank")),
            shiny::hr(),
            p(h4(strong("reshape2"))),
            p(a(href = "https://cran.r-project.org/web/packages/reshape2/reshape2.pdf", "https://cran.r-project.org/web/packages/reshape2/reshape2.pdf", target = "_blank")),
            shiny::hr(),
            p(h4(strong("car"))),
            p(a(href = "https://cran.r-project.org/web/packages/car/car.pdf", "https://cran.r-project.org/web/packages/car/car.pdf", target = "_blank")),
            shiny::hr(),
            p(h4(strong("stats"))),
            p(a(href = "https://cran.r-project.org/web/packages/stats/stats.pdf", "https://cran.r-project.org/web/packages/stats/stats.pdf", target = "_blank")),
            shiny::hr(),
            p(h4(strong("openxlsx"))),
            p(a(href = "https://cran.r-project.org/web/packages/openxlsx/openxlsx.pdf", "https://cran.r-project.org/web/packages/openxlsx/openxlsx.pdf", target = "_blank")),
            
            
            
            #PACOTES ADICIONADOS EM 31/03/2025
            
            shiny::hr(),
            p(h4(strong("purrr"))),
            p(a(href = "https://cran.r-project.org/web/packages/purrr/purrr.pdf", "https://cran.r-project.org/web/packages/purrr/purrr.pdf", target = "_blank")),
            shiny::hr(),
            p(h4(strong("RColorBrewer"))),
            p(a(href = "https://cran.r-project.org/web/packages/RColorBrewer/RColorBrewer.pdf", "https://cran.r-project.org/web/packages/RColorBrewer/RColorBrewer.pdf", target = "_blank")),
            shiny::hr(),
            p(h4(strong("ellipse"))),
            p(a(href = "https://cran.r-project.org/web/packages/ellipse/ellipse.pdf", "https://cran.r-project.org/web/packages/ellipse/ellipse.pdf", target = "_blank")),
            shiny::hr(),
            p(h4(strong("MASS"))),
            p(a(href = "https://cran.r-project.org/web/packages/MASS/MASS.pdf", "https://cran.r-project.org/web/packages/MASS/MASS.pdf", target = "_blank")),
            shiny::hr(),
            p(h4(strong("robustbase"))),
            p(a(href = "https://cran.r-project.org/web/packages/robustbase/robustbase.pdf", "https://cran.r-project.org/web/packages/robustbase/robustbase.pdf", target = "_blank")),
            shiny::hr(),
            p(h4(strong("rgl"))),
            p(a(href = "https://cran.r-project.org/web/packages/rgl/rgl.pdf", "https://cran.r-project.org/web/packages/rgl/rgl.pdf", target = "_blank")),
            shiny::hr(),
            p(h4(strong("tidyr"))),
            p(a(href = "https://cran.r-project.org/web/packages/tidyr/tidyr.pdf", "https://cran.r-project.org/web/packages/tidyr/tidyr.pdf", target = "_blank")),
            shiny::hr(),
            p(h4(strong("shinyWidgets"))),
            p(a(href = "https://cran.r-project.org/web/packages/shinyWidgets/shinyWidgets.pdf", "https://cran.r-project.org/web/packages/shinyWidgets/shinyWidgets.pdf", target = "_blank")),
            shiny::hr(),
            p(h4(strong("philentropy"))),
            p(a(href = "https://cran.r-project.org/web/packages/philentropy/philentropy.pdf", "https://cran.r-project.org/web/packages/philentropy/philentropy.pdf", target = "_blank")),
            shiny::hr(),
            p(h4(strong("Hotelling"))),
            p(a(href = "https://cran.r-project.org/web/packages/Hotelling/Hotelling.pdf", "https://cran.r-project.org/web/packages/Hotelling/Hotelling.pdf", target = "_blank")),
            shiny::hr(),
            p(h4(strong("vegan"))),
            p(a(href = "https://cran.r-project.org/web/packages/vegan/vegan.pdf", "https://cran.r-project.org/web/packages/vegan/vegan.pdf", target = "_blank")),
            shiny::hr(),
            p(h4(strong("MVN"))),
            p(a(href = "https://cran.r-project.org/web/packages/MVN/MVN.pdf", "https://cran.r-project.org/web/packages/MVN/MVN.pdf", target = "_blank")),
            shiny::hr(),
            p(h4(strong("dplyr"))),
            p(a(href = "https://cran.r-project.org/web/packages/dplyr/dplyr.pdf", "https://cran.r-project.org/web/packages/dplyr/dplyr.pdf", target = "_blank")),
            
            
            ##PACOTES ADICIONADOS EM 10/04/2025
            
            shiny::hr(),
            p(h4(strong("RInno"))),
            p(a(href = "https://cran.r-project.org/web/packages/RInno/RInno.pdf", "https://cran.r-project.org/web/packages/RInno/RInno.pdf", target = "_blank")),
            shiny::hr(),
            p(h4(strong("remotes"))),
            p(a(href = "https://cran.r-project.org/web/packages/remotes/remotes.pdf", "https://cran.r-project.org/web/packages/remotes/remotes.pdf", target = "_blank")),
            shiny::hr(),
            p(h4(strong("devtools"))),
            p(a(href = "https://cran.r-project.org/web/packages/devtools/devtools.pdf", "https://cran.r-project.org/web/packages/devtools/devtools.pdf", target = "_blank")),
            
            #PACOTE SCUTR ADICIONADO EM 10/07/2025 - PARA UNDERSAMPLING BASED ON CLUSTERING
            shiny::hr(),
            p(h4(strong("scutr"))),
            p(a(href = "https://cran.r-project.org/web/packages/scutr/scutr.pdf", "https://cran.r-project.org/web/packages/scutr/scutr.pdf", target = "_blank"))
            
            
            
            
          )
        )
      )
    )
  )
)


#########MODIFICAÇÃO DO BLOCO DE IMPORTAÇÃO EM .XLS .CSV E .TXT PARA FUNCIONAR UPLOAD DE ARQUIVO PELO USUÁRIO
#(ANTES NO BROWSER A INTERFACE TRAVAVA) - 08/04/2025


##SUBSTITUIÇÃO DAS CHAMADAS "as.character(t(variables()))" E "as.character(t(id()))" 
##POR RESPECTIVAMENTE "as.character(variables())" E "as.character(id())" 

##SUBSTITUIÇÃO FEITA EM 08/04/2025 - A INTERFACE FECHAVA APÓS O USUÁRIO FAZER UPLOAD DO ARQUIVO



server <- function(input, output, session) {
  
  #------Global Variables
  
  data<-reactiveVal()
  variables<-reactiveVal()
  numvar<-reactiveVal(1)
  id<-reactiveVal()
  sampleclasscolour<-reactiveVal()
  sampleclass<-reactiveVal()
  
  filedata<-reactiveVal()
  
  PCA<-reactiveVal()
  
  TSNE<-reactiveVal() #ATUALIZADO EM 20/01/2025
  
  SMOTE_Data<-reactiveVal()
  
  
  ##ADICIONADO EM 09/05/2025 AMBIENTE PYTHON##
  
  # Ativando ambiente Python com imbalanced-learn
  library(reticulate)
  
  use_virtualenv("r-reticulate", required = TRUE)  # ou use condaenv("nome_env") se preferir
  sklearn <- import("sklearn")
  imblearn <- import("imblearn.over_sampling")
  
  ##ADICIONADO EM 09/05/2025 AMBIENTE PYTHON##
  
 
 
 
  #-----------------------------------------------Standard Importation
  
  observeEvent(input$preview, {
    validate(need(input$datatype == "itfstd", message = "nope2"))
    req(input$datatype == "itfstd")
    req(input$file)
    load(input$file$datapath)
    
    data(data)
    variables(variables)
    id(id)
    sampleclasscolour(sampleclasscolour)
    sampleclass(sampleclass)
  })
  
  #------------------------------------------------------Start importation----------------------------------------------------------------------
  
  observeEvent(c(input$datatype, input$file), {
    req(input$file)
    req(input$datatype != "itfstd")
    req(input$datatype != "demos")
    
    if (input$datatype == 'xlsx' || input$datatype == 'xls') {
      updateSelectInput(inputId = "excelsheet", choices = c(""))
      validate(need(grepl(".xls", input$file$datapath) == TRUE, message = "Wrong type of file"))
      updateSelectInput(inputId = "excelsheet", choices = excel_sheets(input$file$datapath))
    }
  })
  
  observeEvent(input$datatype, {
    req(input$file)
    if (input$datatype == 'txt')
      updateSelectInput(inputId = "delim", selected = '\t')
  })
  
  newdata <- eventReactive(input$preview, {
    req(input$file)
    data <- reactiveVal()
    pretreatdata <- reactiveVal()
    
    validate(need(input$datatype != "sas", message = "nope"))
    validate(need(input$datatype != "itfstd", message = "nope"))
    
    # Copia o arquivo carregado para local seguro
    ext <- tools::file_ext(input$file$name)
    tmp_file <- tempfile(fileext = paste0(".", ext))
    file.copy(input$file$datapath, tmp_file, overwrite = TRUE)
    
    #-----------------------------------------------------------------Import csv, txt--------------------------------------
    if (input$datatype == "txt" || input$datatype == "csv") {
      decimal <- input$dec
      delim <- input$delim
      
      b <- read.table(tmp_file, fill = TRUE, sep = delim, dec = decimal, header = input$labels, check.names = FALSE)
      
      if (input$namerows) {
        c <- duplicated(b[, 1])
        validate(need(TRUE %in% c != TRUE, message = "Duplicated sample names are not permitted. Check the sample name option"),
                 need(ncol(b) > 1, message = "There is probably a delimiter error, only 1 column has been detected"))
        a <- t(b[, 1])
        b <- b[, -1]
        validate(need(nrow(b) == ncol(a), message = "Check the delimiter selected"))
        rownames(b) <- a
      } else {
        validate(need(ncol(b) > 1, message = "There is probably a delimiter error, only 1 column has been detected"))
        rownames(b) <- paste0("Sample ", 1:nrow(b))
      }
    }
    
    #------------------------------------------------------------------------Excel---------------------------------------------------------------------
    if (input$datatype == "xlsx") {
      validate(need(grepl(".xls", tmp_file) == TRUE, message = "Wrong type of file"))
      
      b <- read_excel(tmp_file, sheet = input$excelsheet, col_names = input$labels)
      
      if (input$namerows) {
        b <- column_to_rownames(b, var = colnames(b)[1])
      } else {
        validate(need(ncol(b) > 1, message = "There is probably a delimiter error, only 1 column has been detected"))
        colnames_orig <- colnames(b)
        b <- data.frame(paste0("Sample ", 1:nrow(b)), b)
        colnames(b) <- c("Name", colnames_orig)
        b <- column_to_rownames(b, var = colnames(b)[1])
      }
    }
    
    #-----------------------------------------------------------------------Final----------------------------------------------------------------------
    if (input$labels == FALSE) {
      colnames(b) <- 1:ncol(b)
    }
    
    b
  })
  
  #-------------------------------------------------------------------------Class--------------------------------------------------------------------
  
  observeEvent(input$preview, {
    req(input$datatype != "itfstd")
    req(input$datatype != "demos")
    req(input$file)
    
    
    if (input$classcol==T)
    {
      
      if(input$namerows == T){
        classpos<-input$classpos-1
      }
      
      if(input$namerows == F){
        classpos<-input$classpos
      }  
      
      matrix<-newdata()
      unique<-unique(matrix[[classpos]])
      length<-length(unique)
      numbers<-c(100:length)
      
      for (i in 1:length)
        matrix[[classpos]][matrix[[classpos]]==unique[[i]]]<-numbers[[i]]
      matrix[[classpos]]
      
      sampleclasscolour(matrix[[classpos]])}
    
    else
      sampleclasscolour(0)
    {}
    
    
  })
  
  observeEvent(input$preview,{
    req(input$datatype != "itfstd")
    req(input$datatype != "demos")
    req(input$file)
    
    if(input$namerows == T){
      classpos<-input$classpos-1
    }
    
    if(input$namerows == F){
      classpos<-input$classpos
    }
    
    
    
    if (input$classcol==T)
    {matrix<-newdata()[,classpos]
    
    if (is.data.frame(matrix)==F)
      sampleclass(matrix)
    
    if (is.data.frame(matrix)==T)
      sampleclass(matrix[[classpos]]) 
    
    }
    
    else
      sampleclass(0)
    {}
  })
  
  
  
  #----------------------------------------------------------------------------------End of data importation--------------------------------------------  
  observeEvent(input$preview,{req(input$file)
    
    if (input$classcol==T)
    {data(newdata()[,-1])}
    
    
    else
    {data(newdata())}
    
  })
  
  observeEvent(input$preview,{req(input$file)
    
    if(input$isspectra==T)
    {matrix<-data.frame(as.double(sub(',','.',colnames(data()))))
    }
    
    else
    {matrix<-data.frame(colnames(data()))}
    
    variables(matrix)
  })
  
  observeEvent(input$preview,{req(input$file)
    
    id<-data.frame(rownames(data()))
    rownames(id)<-rownames(data())
    id
    
    id(id)
    
  })
  
  observeEvent(input$preview, {
    req(input$file)
    
    if (input$isspectra==T)
    {validate(need(ncol(data())>1, message = "There is probably a delimiter error, only 1 column has been detected"))
      
      matrix<-data()
      colnames(matrix)<-t(variables())
      data(matrix)
      filedata(data.frame(sampleclass(), data()))}
    
    else
    {}
  })
  
  observeEvent(input$preview, {
    req(input$file)
    filedata(data.frame(sampleclass(), data()))})
  
  #----------------------------------------------------------------------------Remove data option----------------------------------------------------------
  observeEvent(data(), {
    req(data())
    
    
    ##MODIFICADO EM 15/05/2025
    
    updateSelectizeInput(inputId = "removedatacol", choices = list(as.character(t(variables()))), selected = NULL, server = T)
    updateSelectizeInput(inputId = "removedatarow", choices = list(as.character(t(id()))), selected = NULL, server = T)
    
    ## updateSelectizeInput(inputId = "removedatacol", choices = list(as.character(variables())), selected = NULL, server = TRUE)
    ##  updateSelectizeInput(inputId = "removedatarow", choices = list(as.character(id())), selected = NULL, server = TRUE)
    
    
    ##MODIFICADO EM 15/05/2025
    
    
    
    
    
  })
  
  
  observeEvent(input$removedatacolBT, {
    req(data())
    newdata<-data()[,-which(dimnames(data())[[2]] %in% input$removedatacol)]
    
    data(newdata)
    variables(colnames(data()))
    
  })
  
  observeEvent(input$removedatarowBT, {
    req(data())
    
    newdata<-data()[-which(dimnames(data())[[1]] %in% input$removedatarow),]
    matrix<-sampleclass()[-which(rownames(data()) %in% input$removedatarow)]
    matrix2<-sampleclasscolour()[-which(rownames(data()) %in% input$removedatarow)]
    
    data(newdata)
    id(rownames(data()))
    
    sampleclass(matrix)
    sampleclasscolour(matrix2)
    
  })
  
  #-----------------------------------------------------------------------------------------Summary and plot----------------------------------------------------------  
  
  observeEvent(input$preview, {
    req(data())
    
    output$datanullvalues<-renderPrint(which(is.na(data()), arr.ind = T))
    
    if(length(unique(duplicated(data())))!=1)
    {
      
      if(all(sapply (data(), is.numeric)))  
      {cor<-cor(t(data()))
      p<-which(cor>0.9999, arr.ind = T, useNames = F)
      dup<-p[which(p[1:nrow(p),1]!=p[1:nrow(p),2], useNames = T),]
      
      dupmatrix2<-matrix(c(rownames(data()[dup[,1],]),
                           rownames(data()[dup[,2],])),
                         nrow(dup),
                         2,
      )
      
      dupmatrix3<-dupmatrix2[order(dupmatrix2[,1]),]
      output$checkdup<-renderPrint(paste0("Samples ",list(rownames(unique(data()[which(duplicated(data())),]))), " are duplicate of others. It is advised to remove them"))  
      }
      
      showModal(modalDialog(title = "Warning" ,paste0("Samples ",list(rownames(unique(data()[which(duplicated(data())),]))), " have/are duplicates. Check which of then relate by viewing the 'Duplicated Values' tab at 'Data Overview'")
                            ,easyClose = T, footer = "Click anywhere to dismiss")) 
      
    }
    
  })
  
  observeEvent(input$preview,{
    req(data())
    
    if (input$isspectra == F)
    {updateTabsetPanel(inputId ="dataPreview", selected = "panelnormaldata")
      output$preview1 <- renderDT(data(),rownames = T,options = list(pageLength = 10), width = "200px")
      
      ##ATUALIZADO EM 21/01/2025 (ExpData COM ERRO - CORRIGIDO UTILIZANDO PACOTE SKIMR)
      #output$summary <- renderDT(data.frame(ExpData(data())),options = list(pageLength = 100))
      
      
      output$summary <- renderDT(skim(data()), options = list(pageLength = 100))
      
      
    }
    if (input$isspectra == T)
    {updateTabsetPanel(inputId ="dataPreview", selected = "panelspectrum")
      updateSelectInput(inputId ="methodpca", selected = "nipals")
      if (input$classcol==T)
      {output$spectrumpreview <- renderPlot({matplot(y=t(data()), x=variables(), type = "l", ylab = "", xlab = "", lty = 1, col = sampleclasscolour())
        legend(x="topright", legend = c(unique(sampleclass())), col = c(100:(100-length(sampleclass()))), lty=1)})}
      
      if (input$classcol==F)
      {output$spectrumpreview <- renderPlot(matplot(y=t(data()),x=variables(), type = "l", ylab = "", xlab = "", lty = 1, col = ))}
      
      output$spectramsn <- renderText("Showing first 5 variables.")
      output$preview1 <- renderDT(data()[,1:10],rownames = T, options = list(pageLength = 10), width = 200)}
    
    
  })
  #########################MODIFICADO EM 25/02/2025
  
  #  observeEvent(input$preview,{
  
  #   req(data)
  #  output$barplot <- renderPlot(barplot(table(sampleclass()), main = "Class Frequency", xlab = "Class", ylab = "Frequency"))
  
  # #MODIFICAÇÃO PARA UM PACOTE MAIS COMPLETO NA SUMMARIZAÇÃO EM 17/01/2025
  
  #  #output$datasummary <- renderPrint(summary(data()))
  #  output$datasummary <- renderPrint(psych::describe(data()))
  
  # })
  
  #### BLOCO MODIFICADO EM 15/05/2025 - INTERFACE ESTAVA FECHANDO NA PRIMEIRA EXECUÇÃO
  
  
  
  ##observeEvent(input$preview, {
  
  ## observeEvent( c(data(),input$preview), {
  
  observeEvent(data(), {
    
    
    req(data())  # Garante que data() existe
    req(sampleclass())  # Garante que sampleclass() existe
    
    
    
    
    # Proteção contra sampleclass vazio
    ##    if (length(sampleclass()) == 0) {
    ##      warning("sampleclass() está vazio. Nenhum gráfico será gerado.")
    ##      output$datasummary <- renderPrint({ "Nenhum dado para exibir." })
    ##      return(NULL)
    ##    }
    
    # Criando tabela de Frequencys com verificação
    class_freq <- as.data.frame(table(sampleclass()))
    
    # Validando que class_freq tem 2 colunas antes de aplicar colnames
    if (ncol(class_freq) == 2) {
      colnames(class_freq) <- c("Class", "Frequency")
    } else {
      warning(paste0("class_freq tem ", ncol(class_freq), " colunas. Esperava-se 2."))
      print(class_freq)  # Debug: vendo o que chegou
      output$datasummary <- renderPrint({ "Error: Invalid class frequency structure." })
      return(NULL)  # Interrompe se estrutura inválida
    }
    
    # Criando cores automáticas para cada classe
    num_classes <- length(unique(class_freq$Class))
    class_colors <- setNames(
      RColorBrewer::brewer.pal(min(num_classes, 12), "Set1"),
      unique(class_freq$Class)
    )
    
    # Criando gráfico interativo com Plotly
    output$barplot <- renderPlotly({
      plot_ly(
        data = class_freq,
        x = ~Class,
        y = ~Frequency,
        type = "bar",
        color = ~Class,
        colors = class_colors,
        text = ~Frequency,
        textposition = 'outside'
      ) %>%
        layout(
          title = "Class Frequency",
          xaxis = list(title = "Class"),
          yaxis = list(title = "Frequency"),
          showlegend = TRUE
        )
    })
    
    # Exibindo sumário do data() no output
    output$datasummary <- renderPrint({
      psych::describe(data())
    })
    
    
    
    output$datanullvalues<-renderPrint(which(is.na(data()), arr.ind = T))
    
    if(length(unique(duplicated(data())))!=1)
    {
      
      if(all(sapply (data(), is.numeric)))  
      {cor<-cor(t(data()))
      p<-which(cor>0.9999, arr.ind = T, useNames = F)
      dup<-p[which(p[1:nrow(p),1]!=p[1:nrow(p),2], useNames = T),]
      
      dupmatrix2<-matrix(c(rownames(data()[dup[,1],]),
                           rownames(data()[dup[,2],])),
                         nrow(dup),
                         2,
      )
      
      dupmatrix3<-dupmatrix2[order(dupmatrix2[,1]),]
      output$checkdup<-renderPrint(paste0("Samples ",list(rownames(unique(data()[which(duplicated(data())),]))), " are duplicate of others. It is advised to remove them"))  
      }
      
      showModal(modalDialog(title = "Warning" ,paste0("Samples ",list(rownames(unique(data()[which(duplicated(data())),]))), " have/are duplicates. Check which of then relate by viewing the 'Duplicated Values' tab at 'Data Overview'")
                            ,easyClose = T, footer = "Click anywhere to dismiss")) 
      
    }
    
    
    if (input$isspectra == F)
    {updateTabsetPanel(inputId ="dataPreview", selected = "panelnormaldata")
      output$preview1 <- renderDT(data(),rownames = T,options = list(pageLength = 10), width = "200px")
      
      ##ATUALIZADO EM 21/01/2025 (ExpData COM ERRO - CORRIGIDO UTILIZANDO PACOTE SKIMR)
      #output$summary <- renderDT(data.frame(ExpData(data())),options = list(pageLength = 100))
      
      
      output$summary <- renderDT(skim(data()), options = list(pageLength = 100))
      
      
    }
    if (input$isspectra == T)
    {updateTabsetPanel(inputId ="dataPreview", selected = "panelspectrum")
      updateSelectInput(inputId ="methodpca", selected = "nipals")
      if (input$classcol==T)
      {output$spectrumpreview <- renderPlot({matplot(y=t(data()), x=variables(), type = "l", ylab = "", xlab = "", lty = 1, col = sampleclasscolour())
        legend(x="topright", legend = c(unique(sampleclass())), col = c(100:(100-length(sampleclass()))), lty=1)})}
      
      if (input$classcol==F)
      {output$spectrumpreview <- renderPlot(matplot(y=t(data()),x=variables(), type = "l", ylab = "", xlab = "", lty = 1, col = ))}
      
      output$spectramsn <- renderText("Showing first 5 variables.")
      output$preview1 <- renderDT(data()[,1:10],rownames = T, options = list(pageLength = 10), width = 200)}
    
    
    
    
    
    
  })
  
  #### BLOCO MODIFICADO EM 15/05/2025 - INTERFACE ESTAVA FECHANDO NA PRIMEIRA EXECUÇÃO
  
  
  
  
  #########################MODIFICADO EM 25/02/2025 
  
  
  
  # # Render the summary only when the button is clicked
  # output$summarydata <- renderPrint({
  #   req(previewClicked(), filedata())
  #   summary(filedata())
  # })
  # 
  # output$barplot <- renderPlot({
  #   req(class_freq())
  #   barplot(class_freq(), main = "Class Frequency", xlab = "Class", ylab = "Frequency")
  # })
  #---------------------------------------------------------------------------------------------Pretreat Plots---------------------------------------------------------------------  
  observeEvent(data(), {
    req(data())
    
    ##MODIFICADO EM 06/05/2025 - SELETOR ERRADO EM PRÉ PROCESSAMENTO
    updateSelectizeInput(inputId = "whichsamplesprepro", choices = list(as.character(t(id()))), selected = NULL, server = T)
    ##updateSelectizeInput(inputId = "whichsamplesprepro", choices = list(as.character(id())), selected = NULL, server = T)
    
  })
  
  observeEvent(c(input$pretreatcopyBT, input$preview, input$plotpretreatclasses, input$whichsamplesprepro), ignoreInit = T, {
    
    lwd = 1
    cex.axis = 1
    
    if (input$selectpreprosamples == F) {
      data <- data()
    }
    
    if (input$selectpreprosamples == T) {
      req(input$whichsamplesprepro != "")
      data <- data()[input$whichsamplesprepro, ]
    }
    
    if (input$isspectra == T) {  
      
      if (input$plotpretreatclasses == "indpretreatplot") {
        output$normaldata <- renderPlot({
          
          matplot(y = t(data), x = variables(), lty = 1, col = 1:nrow(data), type = "l", main = "Data", xlab = "", ylab = "")
        })
        
        output$meancenterdata <- renderPlot({
          
          matplot(y = t(scale(data, scale = F)), x = variables(), lty = 1, col = 1:nrow(data), type = "l", main = "Mean Centered", xlab = "", ylab = "")
        })
        
        output$scaleddata <- renderPlot({
          
          matplot(y = t(scale(data)), x = variables(), lty = 1, col = 1:nrow(data), type = "l", main = "Scaled", xlab = "", ylab = "")
        })
      }
      
      if (input$plotpretreatclasses == "classpretreatplot") {
        output$normaldata <- renderPlot({
          par(mar = c(5, 4, 4, 10))  # Aumenta a margem direita para acomodar a legenda externa
          class_colors <- sampleclasscolour()  # Obtém as cores corretas para as classes
          unique_classes <- unique(sampleclass())  # Obtém classes únicas
          
          matplot(y = t(data), x = variables(), lty = 1, col = class_colors, type = "l", main = "Data", xlab = "", ylab = "")
          
          legend("topright", inset = c(-0.35, 0), legend = unique_classes, col = class_colors[match(unique_classes, sampleclass())], lty = 1, xpd = TRUE, cex = 0.8)
        })
        
        output$meancenterdata <- renderPlot({
          par(mar = c(5, 4, 4, 10))
          class_colors <- sampleclasscolour()
          unique_classes <- unique(sampleclass())
          
          matplot(y = t(scale(data, scale = F)), x = variables(), lty = 1, col = class_colors, type = "l", main = "Mean Centered", xlab = "", ylab = "")
          
          legend("topright", inset = c(-0.35, 0), legend = unique_classes, col = class_colors[match(unique_classes, sampleclass())], lty = 1, xpd = TRUE, cex = 0.8)
        })
        
        output$scaleddata <- renderPlot({
          par(mar = c(5, 4, 4, 10))
          class_colors <- sampleclasscolour()
          unique_classes <- unique(sampleclass())
          
          matplot(y = t(scale(data)), x = variables(), lty = 1, col = class_colors, type = "l", main = "Scaled", xlab = "", ylab = "")
          
          legend("topright", inset = c(-0.35, 0), legend = unique_classes, col = class_colors[match(unique_classes, sampleclass())], lty = 1, xpd = TRUE, cex = 0.8)
        })
      }
    }
    
    if (input$isspectra == F) {  
      if (input$plotpretreatclasses == "indpretreatplot") {
        
        
        #################MODIFICADO EM 15/05/2025(ARGUMENTOS NÃO ERAM MATRIZES)##################################  
        
        
        
        #   output$normaldata <- renderPlot({
        #   matplot(y = t(data), lty = 1, col = 1:nrow(data), type = "l", main = "Data", xlab = "", ylab = "")
        #  })
        
        #  output$meancenterdata <- renderPlot({
        #   matplot(y = t(scale(data, scale = F)), lty = 1, col = 1:nrow(data), type = "l", main = "Mean Centered", xlab = "", ylab = "")
        #  })
        
        #  output$scaleddata <- renderPlot({
        #   matplot(y = t(scale(data, scale = T)), lty = 1, col = 1:nrow(data), type = "l", main = "Scaled", xlab = "", ylab = "")
        #  })
        
        # Plot 1: Dados crus
        
        output$normaldata <- renderPlot({
          req(data())
          
          df <- data()
          
          # Selecionando apenas colunas numéricas
          df_numeric <- df[, sapply(df, is.numeric), drop = FALSE]
          
          # Verificando se tem colunas numéricas suficientes
          req(ncol(df_numeric) > 0)
          
          matplot(y = t(as.matrix(df_numeric)), lty = 1, col = 1:nrow(df_numeric), type = "l",
                  main = "Data", xlab = "", ylab = "")
        })
        
        # Plot 2: Mean Centered
        
        output$meancenterdata <- renderPlot({
          req(data())
          
          df <- data()
          
          df_numeric <- df[, sapply(df, is.numeric), drop = FALSE]
          req(ncol(df_numeric) > 0)
          
          matplot(y = t(scale(as.matrix(df_numeric), scale = FALSE)), lty = 1, col = 1:nrow(df_numeric), type = "l",
                  main = "Mean Centered", xlab = "", ylab = "")
        })
        
        # Plot 3: Scaled
        
        output$scaleddata <- renderPlot({
          req(data())
          
          df <- data()
          
          df_numeric <- df[, sapply(df, is.numeric), drop = FALSE]
          req(ncol(df_numeric) > 0)
          
          matplot(y = t(scale(as.matrix(df_numeric), scale = TRUE)), lty = 1, col = 1:nrow(df_numeric), type = "l",
                  main = "Scaled", xlab = "", ylab = "")
        })
        
        
        
        #################MODIFICADO EM 15/05/2025(ARGUMENTOS NÃO ERAM MATRIZES)##################################  
      }
      
      if (input$plotpretreatclasses == "classpretreatplot") {
        output$normaldata <- renderPlot({
          par(mar = c(5, 4, 4, 10))
          class_colors <- sampleclasscolour()
          unique_classes <- unique(sampleclass())
          
          matplot(y = t(data), lty = 1, col = class_colors, type = "l", main = "Data", xlab = "", ylab = "")
          legend("topright", inset = c(-0.35, 0), legend = unique_classes, col = class_colors[match(unique_classes, sampleclass())], lty = 1, xpd = TRUE, cex = 0.8)
        })
        
        output$meancenterdata <- renderPlot({
          par(mar = c(5, 4, 4, 10))
          class_colors <- sampleclasscolour()
          unique_classes <- unique(sampleclass())
          
          matplot(y = t(scale(data, scale = F)), lty = 1, col = class_colors, type = "l", main = "Mean Centering", xlab = "", ylab = "")
          legend("topright", inset = c(-0.35, 0), legend = unique_classes, col = class_colors[match(unique_classes, sampleclass())], lty = 1, xpd = TRUE, cex = 0.8)
        })
        
        output$scaleddata <- renderPlot({
          par(mar = c(5, 4, 4, 10))
          class_colors <- sampleclasscolour()
          unique_classes <- unique(sampleclass())
          
          matplot(y = t(scale(data)), lty = 1, col = class_colors, type = "l", main = "Scaled", xlab = "", ylab = "")
          legend("topright", inset = c(-0.35, 0), legend = unique_classes, col = class_colors[match(unique_classes, sampleclass())], lty = 1, xpd = TRUE, cex = 0.8)
        })
      }
    }
  })
  
  
  
  #----------------------------------------------------------------------------------------------PCA---------------------------------------------------------------------------------
  
  observeEvent(input$preview,{
    if(isTRUE(ncol(data())<10)==T)
    {updateNumericInput(inputId = "ncomppca", value = ncol(data()))}
    
    updateNumericInput(inputId = "ncomppca", max = ncol(data()))
    #updateSelectizeInput(inputId = "pcaremovecol", choices = list(as.character(t(variables()))), server = T)
    updateSelectizeInput(inputId = "pcaremovecol", choices = list(as.character(variables())), server = T)
  })
  
  
  observeEvent(input$bPCA, { 
    
    showModal(modalDialog(title = "Please wait:", "The calculation are running and may take several seconds. This window will be automatically closed when finished.", easyClose = F, footer = ""))
    
    validate(need(input$ncomppca!="", message = "Need to select de number of components")) #checar input ncomp
    
    
    if(input$removepcacolask==T)
    {pcaremovecolvar<-eventReactive(input$pcaremovecol,{input$pcaremovecol})
    pcadata1<-reactive(select(data(),-pcaremovecolvar()))}
    
    if(input$removepcacolask==F)
    {pcadata1<-reactive(data())}
    
    
    matrix<-pcadata1()
    matrix2<-which(sapply(matrix, is.numeric))
    pcadata<-pcadata1()[,matrix2]
    
    
    if(input$prepropca == "none")
    {PCA <- mdatools::pca(pcadata, method = input$methodpca, lim.type = "ddmoments", center = F, scale = F, ncomp = input$ncomppca)}
    
    if (input$prepropca == "center")
    {PCA <- mdatools::pca(pcadata, method = input$methodpca, lim.type = "ddmoments", center = T, scale = F, ncomp = input$ncomppca)}
    
    if (input$prepropca == "scale")
    {PCA <- mdatools::pca(pcadata, scale=T, method = input$methodpca, lim.type = "ddmoments", ncomp = input$ncomppca)}
    
    numvar(colnames(pcadata))
    PCA(PCA)
    
    removeModal()
    
  })
  
  observeEvent(c(input$importpcamodel,input$bPCA, input$classcorespca), ignoreInit = T ,{
    req()
    
    #Matrices list dowload tab
    {
      
      updateNumericInput(inputId = "numPCresi", value = PCA()$ncomp)
      updateNumericInput(inputId = "numPCresi", max = PCA()$ncomp)} 
    
    output$SumPCA <- renderPrint(summary(PCA()), width = 500)
    
    #output$preview2 <- renderDT(PCA()data[,1:2])
    
    ##--------------------------Plots--------------------------------------------------------------
    { 
      
      #-----------------------------------------Variancia
      
      #output$pcav <- renderPlot(plotVariance(PCA(),show.labels=T),res = 96, height = 800)
      output$pcav<- renderPlotly({
        
        variance <- PCA()$calres$expvar
        
        t <- list(
          family = "times",
          size = 12,
          color = toRGB("black"))
        
        plot_ly(type="scatter",
                x=1:length(variance),
                y=variance,
                mode="lines+markers",
                showlegend=F,
                text=paste("Component ", 1:length(variance), sep=""),
                hoverinfo="text y")%>%
          layout(title="<b>Variance</b>",
                 xaxis=list(title=paste0("Component"),zerolinecolor="black"),
                 yaxis=list(title=paste0("Proportion Explained"),zerolinecolor="black"))
        
      })
      
      #----------------------------------------Variancia acumulada
      
      #output$pcacv <- renderPlot(plotCumVariance(PCA(),show.labels=T),res = 96, height = 800)
      output$pcacv<- renderPlotly({
        
        variance <- PCA()$calres$cumexpvar
        
        t <- list(
          family = "times",
          size = 12,
          color = toRGB("black"))
        
        plot_ly(type="scatter",
                x=1:length(variance),
                y=variance,
                mode="lines+markers",
                showlegend=F,
                text=paste("Component ", 1:length(variance), sep=""),
                hoverinfo="text y")%>%
          layout(title="<b>Variance</b>",
                 xaxis=list(title=paste0("Component"),zerolinecolor="black"),
                 yaxis=list(title=paste0("Proportion Explained"),zerolinecolor="black"))
        
      })
      
      #---------------------------------------Loadings
      
      #output$pcaload <- renderPlot(plotLoadings(PCA(),show.labels=T),res = 96, height = 800)
      output$pcaload<-renderPlotly({
        
        loadings<-PCA()$loadings
        sampleclass<-sampleclass()
        id<-id()
        variables<-variables()
        data2<-numvar()
        
        if(input$isspectra == F){
          plot<-PlotNormalLoadings(loadings, input$npc1loadplotPCA, input$npc2loadplotPCA, variables, data2, "Principal Component ")
        }
        
        if(input$isspectra == T){
          plot<-PlotSpectralLoadings(loadings, input$npc1loadplotPCA, input$npc2loadplotPCA, variables, data2, "Principal Component ")
        }
        
        plot
        
      })
      
      #-------------------------------------Loadings individuais por PC
      
      #output$pcaload1 <- renderPlot(plotLoadings(PCA(),1:input$numnpcpca,type = "l"), res = 96, height = 800)
      output$pcaload1 <- renderPlotly({
        
        loadings<-PCA()$loadings
        sampleclass<-sampleclass()
        id<-id()
        variables<-variables()
        data2<-numvar()
        
        
        if(input$isspectra == F){
          plot<-PlotNormalNLoadings(loadings, variables, data2, "Principal Component ")
        }
        
        if(input$isspectra == T){
          plot<-PlotSpectralNLoadings(loadings, variables, data2, "Principal Component ")
        }
        
        plot
        
      })
      
      #---------------------------------------Scores
      
      if(input$classcorespca==F){
        output$pcascores <- renderPlotly({
          scores<-PCA()$calres$scores
          EXPPC<-PCA()$calres$expvar
          sampleclass<-sampleclass()
          id<-id()
          
          PlotScoresGeneral(scores,input$npc1scoresplotpca, input$npc2scoresplotpca, input$siglimpca, sampleclass, id, "Principal Component ", input$EXPPC)
        })
      }
      
      if(input$classcorespca==T){
        output$pcascores <- renderPlotly({
          scores<-PCA()$calres$scores
          sampleclass<-sampleclass()
          id<-id()
          
          PlotScoresClass(scores,input$npc1scoresplotpca, input$npc2scoresplotpca, input$siglimpca, sampleclass, id, "Principal Component ", " PC")
        })
      }
      
      #---------------------------------------Scores 3d  
      
      if(input$classcorespca==F){
        output$pca3dscores<- renderPlotly({
          
          scores<-PCA()$calres$scores
          sampleclass<-sampleclass()
          id<-id()
          
          Plot3DscoresGeneral(scores,input$npc1scores3dplotpca, input$npc2scores3dplotpca, input$npc3scores3dplotpca, input$siglim3Dpca, sampleclass, id, "Principal Component ", " PC")
        })
      }
      
      
      if(input$classcorespca==T){
        output$pca3dscores<- renderPlotly({
          
          scores<-PCA()$calres$scores
          sampleclass<-sampleclass()
          id<-id()
          
          Plot3DscoresClasses(scores,input$npc1scores3dplotpca, input$npc2scores3dplotpca, input$npc3scores3dplotpca, input$siglimpca, sampleclass, id, "Principal Component ", " PC")
        })
      }
      
      
      ##Residuals
      
      
      #ACRÉSCIMO DA LIBRARY "mdatools" para os resíduos serem plotados em 17/02/2025
      #  output$pcaresiduals <- renderPlot(plotResiduals(PCA(),show.labels=T, ncomp = input$numPCresipca, log = input$logresidualspca, lim.col = c("orange", "red"), lim.lwd = c(3,3),lim.lty = c(2, 2)),res = 96, height = 800)
      
      
      output$pcaLimleg <- renderText("Info,  Outside of red = Outlier & Outside of orange = extreme value")
      
      ##BiPlot
      output$biplotpca <- renderPlotly({
        
        scores<-PCA()$calres$scores
        id<-id()
        loadings<-PCA()$loadings
        variables<-variables()
        
        PlotBIPLOT(scores, loadings,input$npc1biplotpca, input$npc2biplotpca, id, variables, "Principal Component ", " PC")
        
        
        
      })
      
      #-----Outliers
      output$OutPCA <- renderPlot({
        
        if(input$removeROBPCAcolask==T)
        {pcaremovecolvar<-eventReactive(input$ROBPCAremovecol,{input$ROBPCAremovecol})
        pcadata1<-reactive(select(data(),-pcaremovecolvar()))}
        
        if(input$removeROBPCAcolask==F)
        {pcadata1<-reactive(data())}
        
        
        if(input$prepropca == "none"){
          matrix<-pcadata1()[,which(sapply(pcadata1(), is.numeric))]}
        
        if(input$prepropca == "center"){
          matrix<-data.frame(scale(pcadata1()[,which(sapply(pcadata1(), is.numeric))], scale = F))
        }
        
        if(input$prepropca == "scale"){
          matrix<-data.frame(scale(pcadata1()[,which(sapply(pcadata1(), is.numeric))]))
        }
        
        
        matrix2<-which(sapply(matrix, is.numeric))
        pcadata<-matrix[,matrix2]
        
        #plot(pca.distances(ROBPCA(), pcadata, rankMM(pcadata)))
        plot(pca.distances(rrcov::PcaClassic(pcadata, k = input$numPCoutipca), pcadata, rankMM(pcadata)))
        
      })
      
      
    }
    
    
    output$pcamatrices<-renderPrint("Run PCA to view")
    
    
    
    
  })
  
  #-------------------------Download Handlers-----------------------------------------------------
  
  {namefilespca <- reactive(input$downloadfilenamepca)
  
  output$downloadloadpca <- downloadHandler(
    filename = function() {
      paste0(input$namemodelpca,"_loadingsPCA", ".csv")
    },
    content = function(file) {
      write.csv(PCA()$loadings[,1:input$numcompselpca], file)
    }
  )
  
  output$downloadscorespca <- downloadHandler(
    filename = function() {
      paste0(input$namemodelpca,"_scoresPCA", ".csv")
    },
    content = function(file) {
      write.csv(PCA()$res$cal$scores[,1:input$numcompselpca], file)
    }
  )
  
  }
  
  ##Export model
  output$savepcamodel<-downloadHandler(
    filename = function() {
      paste0(input$namemodelpca, " PCA model", ".Rdata")
    },
    content = function(file){
      PCA1<-PCA()
      sampleclass<-sampleclass()
      id<-id()
      variables<-variables()
      data<-data.frame(PCA()$loadings[,1:input$numcompselpca])
      variables<-data.frame(colnames(data))
      PCAmodel<-selectCompNum(PCA1, input$numcompselpca)
      
      save(data,PCAmodel,sampleclass,id, variables, file = file)
    }
  )
  
  ##Import model
  observeEvent(input$importpcamodel, {validate(need(grepl(".Rdata", input$searchmodelpca$datapath)==T, message = "Wrong type of file"))
    load(input$searchmodelpca$datapath)
    PCA(PCAmodel)
    sampleclass(sampleclass)
  })
  
  
  #-----------------------------------------------------------------------------------Robust PCA
  
  # Atualização de colunas removíveis para seleção
  observeEvent(input$preview, {
    updateSelectizeInput(inputId = "ROBPCAremovecol", 
                         #choices = list(as.character(t(variables()))), 
                         choices = list(as.character(variables())),
                         server = TRUE)
  })
  
  #COMANDO ACRESCENTADO EM 17/01/2025
  # Reactive para armazenar o modelo de PCA robusto
  ROBPCA <- reactiveVal(NULL)  # Correção: Criando reativo para armazenar o modelo
  
  
  
  # Evento para rodar o PCA robusto
  observeEvent(input$bROBPCA, {
    # Exibir modal de carregamento
    showModal(modalDialog(
      title = "Please wait:", 
      "The calculation is running and may take several seconds. This window will be automatically closed when finished.", 
      easyClose = FALSE, 
      footer = NULL
    ))
    
    # Lógica para remoção de colunas (caso ativada)
    if (input$removeROBPCAcolask == TRUE) {
      pcaremovecolvar <- eventReactive(input$ROBPCAremovecol, { input$ROBPCAremovecol })
      pcadata1 <- reactive(select(data(), -pcaremovecolvar()))
    } else {
      pcadata1 <- reactive(data())
    }
    
    # Pré-processamento baseado na escolha do usuário
    if (input$prepropca == "none") {
      matrix <- pcadata1()[, which(sapply(pcadata1(), is.numeric))]
    } else if (input$prepropca == "center") {
      matrix <- data.frame(scale(pcadata1()[, which(sapply(pcadata1(), is.numeric))], scale = FALSE))
    } else if (input$prepropca == "scale") {
      matrix <- data.frame(scale(pcadata1()[, which(sapply(pcadata1(), is.numeric))]))
    }
    
    # Seleção apenas de colunas numéricas
    matrix2 <- which(sapply(matrix, is.numeric))
    pcadata <- matrix[, matrix2]
    
    # Cálculo do PCA robusto usando a função correta
    ROBPCA <- rrcov::PcaHubert(pcadata, k = input$ncompROBPCA)  # Corrigido: Substituí a função para a correta `PcaHubert`
    #ROBPCA <- PcaHubert(pcadata, k = input$ncompROBPCA)
    
    
    
    
    
    # Atualizando reativo com o modelo calculado
    numvar(colnames(pcadata))
    ROBPCA(ROBPCA)  # Corrigido: Usando a reatividade correta para armazenar o modelo
    
    # Remover modal de carregamento após conclusão
    removeModal()
  })
  
  # Lógica para lidar com saída e gráficos
  observeEvent(c(input$importROBPCAmodel, input$bROBPCA, input$classcoresROBPCA), ignoreInit = TRUE, {
    req(ROBPCA())  # Certifica que o modelo foi calculado
    
    # Renderizando sumário
    output$SumROBPCA <- renderPrint(summary(ROBPCA()))
    
    # Gráfico de variância
    output$ROBPCAv <- renderPlotly({
      variance <- ROBPCA()$eigenvalues / sum(ROBPCA()$eigenvalues) * 100  # Corrigido: Cálculo direto da variância
      plot_ly(type = "scatter",
              x = seq_along(variance),
              y = variance,
              mode = "lines+markers",
              showlegend = FALSE) %>%
        layout(title = "<b>Variance</b>",
               xaxis = list(title = "Component"),
               yaxis = list(title = "Proportion Explained"))
    })
    
    
    ###ATUALIZADO EM 20/01/2025
    #----------------------------Cumulative Variance
    
    output$ROBPCAcv <- renderPlotly({
      
      variance<-summary(ROBPCA())@importance[3,]
      t <- list(
        family = "times",
        size = 12,
        color = toRGB("black"))
      
      plot_ly(type="scatter",
              x=1:length(variance),
              y=variance,
              mode="lines+markers",
              showlegend=F,
              text=paste("Component ", 1:length(variance), sep=""),
              hoverinfo="text y")%>%
        layout(title="<b>Variance</b>",
               xaxis=list(title=paste0("Component"),zerolinecolor="black"),
               yaxis=list(title=paste0("Proportion Explained"),zerolinecolor="black"))
    }) 
    
    
    ###ATUALIZADO EM 20/01/2025
    
    #------------------------Loadings
    
    output$ROBPCAload<-renderPlotly({
      
      loadings<-ROBPCA()$loadings
      sampleclass<-sampleclass()
      id<-id()
      variables<-variables()
      data2<-numvar()
      
      if(input$isspectra == F){
        plot<-PlotNormalLoadings(loadings, input$npc1loadplotROBPCA, input$npc2loadplotROBPCA, variables, data2, "Principal Component ")
      }
      
      if(input$isspectra == T){
        plot<-PlotSpectralLoadings(loadings, input$npc1loadplotROBPCA, input$npc2loadplotROBPCA, variables, data2, "Principal Component ")
      }
      
      plot
      
    })
    
    
    ###ATUALIZADO EM 20/01/2025
    
    #------------------------nLoadings
    
    output$ROBPCAload1<-renderPlotly({
      
      #PlotSpectralNLoadings<-function(loadings, variables, data2, SpecialLabels){
      
      loadings<-ROBPCA()$loadings
      sampleclass<-sampleclass()
      id<-id()
      variables<-variables()
      data2<-numvar()
      
      
      if(input$isspectra == F){
        plot<-PlotNormalNLoadings(loadings, variables, data2, "Principal Component ")
      }
      
      if(input$isspectra == T){
        plot<-PlotSpectralNLoadings(loadings, variables, data2, "Principal Component ")
      }
      
      plot
      
    }) 
    
    
    #ATUALIZADO 20/01/2025  
    #------------------------Scores
    
    if(input$classcoresROBPCA==T){
      output$ROBPCAscores <- renderPlotly({
        scores<-ROBPCA()$scores
        sampleclass<-sampleclass()
        id<-id()
        
        PlotScoresClass(scores,input$npc1scoresplotROBPCA, input$npc2scoresplotROBPCA, input$siglimROBPCA, sampleclass, id, "Principal Component ", " PC")
      })
      
    }
    
    
    if(input$classcoresROBPCA==F){
      output$ROBPCAscores <- renderPlotly({
        scores<-ROBPCA()$scores
        sampleclass<-sampleclass()
        id<-id()
        
        PlotScoresGeneral(scores,input$npc1scoresplotROBPCA, input$npc2scoresplotROBPCA, input$siglimROBPCA, sampleclass, id, "Principal Component ", " PC")
        
      })
    }
    
    #ATUALIZADO 20/01/2025 
    
    #-------------------------------------------3D scores
    
    
    if(input$classcoresROBPCA==T){
      output$ROBPCA3dscores <- renderPlotly({
        scores<-ROBPCA()$scores
        sampleclass<-sampleclass()
        id<-id()
        
        Plot3DscoresClasses(scores, input$npc1scores3dplotROBPCA, input$npc2scores3dplotROBPCA, input$npc3scores3dplotROBPCA, input$siglim3DROBPCA, sampleclass, id, "Principal Component ", " PC")
        
      })
    }
    
    
    if(input$classcoresROBPCA==F){
      output$ROBPCA3dscores <- renderPlotly({
        scores<-ROBPCA()$scores
        sampleclass<-sampleclass()
        id<-id()
        
        Plot3DscoresGeneral(scores, input$npc1scores3dplotROBPCA, input$npc2scores3dplotROBPCA, input$npc3scores3dplotROBPCA, input$siglim3DROBPCA, sampleclass, id, "Principal Component ", " PC")
        
      }
      )}
    
    #ATUALIZADO 20/01/2025  
    
    #-----------------------------------Biplot
    
    output$ROBPCAbiplot<-renderPlotly({
      scores<-ROBPCA()$scores
      id<-id()
      loadings<-ROBPCA()$loadings
      variables<-variables()
      
      PlotBIPLOT(scores, loadings,input$npc1biplotROBPCA, input$npc2biplotROBPCA, id, variables, "Principal Component ", " PC")
      
      
    })
    
    #ATUALIZADO 20/01/2025 
    
    #-----------------------------------Outliers
    
    #tabPanel("Outliers",plotOutput("OutROBPCA", height = 650)),
    
    output$OutROBPCA <- renderPlot({
      
      if(input$removeROBPCAcolask==T)
      {pcaremovecolvar<-eventReactive(input$ROBPCAremovecol,{input$ROBPCAremovecol})
      pcadata1<-reactive(select(data(),-pcaremovecolvar()))}
      
      if(input$removeROBPCAcolask==F)
      {pcadata1<-reactive(data())}
      
      
      if(input$prepropca == "none"){
        matrix<-pcadata1()[,which(sapply(pcadata1(), is.numeric))]}
      
      if(input$prepropca == "center"){
        matrix<-data.frame(scale(pcadata1()[,which(sapply(pcadata1(), is.numeric))], scale = F))
      }
      
      if(input$prepropca == "scale"){
        matrix<-data.frame(scale(pcadata1()[,which(sapply(pcadata1(), is.numeric))]))
      }
      
      
      matrix2<-which(sapply(matrix, is.numeric))
      pcadata<-matrix[,matrix2]
      
      plot(pca.distances(rrcov::PcaHubert(pcadata, k = input$numROBPCoutipca), pcadata, rankMM(pcadata)))
      
    })
    #numericInput("numROBPCoutipca","Principal Component Number",min = 1,value = 1), 
    
    #})
    
    
  })
  
  # Exportar modelo robusto
  
  output$saveROBPCAmodel <- downloadHandler(
    filename = function() {
      paste0(input$namemodelROBPCA, " RobPCA model", ".Rdata")
    },
    content = function(file) {
      FA1 <- ROBPCA()
      sampleclass <- sampleclass()
      ROBPCAmodel <- FA1
      id <- id()
      variables <- variables()
      scores <- ROBPCA()$scores[, 1:input$numcompselROBPCA]
      data <- data.frame(scores)
      variables <- data.frame(colnames(data))
      save(data, ROBPCAmodel, sampleclass, id, variables, file = file)
    }
  )
  
  # Importar modelo robusto
  
  observeEvent(input$importROBPCAmodel, {
    validate(need(grepl(".Rdata", input$searchmodelROBPCA$datapath) == TRUE, message = "Wrong type of file"))
    load(input$searchmodelROBPCA$datapath)
    ROBPCA(ROBPCAmodel)
    sampleclass(sampleclass)
    id(id)
    variables(variables)
  })
  
  
  # Exportar CSVs de loadings e scores
  output$downloadloadROBPCA <- downloadHandler(
    filename = function() {
      #paste0(input$namemodelROBPCA, "_loadingsRobPCA", ".csv")
      paste0(input$namemodelROBPCA, "_scoresRobPCA", ".csv")
    },
    content = function(file) {
      load <- ROBPCA()$loadings[, 1:input$numcompselROBPCA]
      rownames(load) <- t(variables()[which(sapply(data(), is.numeric)), ])
      write.csv(load, file)
    }
  )
  
  output$downloadscoresROBPCA <- downloadHandler(
    filename = function() {
      paste0(input$namemodelROBPCA, "_scoresRobPCA", ".csv")
    },
    content = function(file) {
      scores <- ROBPCA()$scores[, 1:input$numcompselROBPCA]
      write.csv(scores, file)
    }
  )
  
  ##ATUALIZADO EM 20/01/2025
  
  #-------------------------------------- TSNE --------------------------------------
  
  observeEvent(input$preview, {
    updateSelectizeInput(
      inputId = "FAremovecol",
      #choices = as.character(t(variables())),
      choices = as.character(variables()),
      server = TRUE
    )
  })
  
  observeEvent(input$bTSNE, { 
    # Validar a perplexidade
    validate(
      need(
        3 * input$perplexityTSNE < (nrow(data()[, which(sapply(data(), is.numeric))]) - 1),
        message = "Perplexity is too high. Ensure it is less than one-third of the numeric sample size."
      )
    )
    
    showModal(modalDialog(
      title = "Please wait:",
      "The calculation is running and may take several seconds. This window will automatically close when finished.",
      easyClose = FALSE,
      footer = NULL
    ))
    
    # Preparação dos dados para TSNE
    pcadata1 <- reactive({
      if (input$removeTSNEcolask) {
        validate(need(input$FAremovecol, "Please select columns to remove."))
        select(data(), -all_of(input$FAremovecol))
      } else {
        data()
      }
    })
    
    matrix <- switch(
      input$prepropca,
      "none" = pcadata1()[, which(sapply(pcadata1(), is.numeric))],
      "center" = data.frame(scale(pcadata1()[, which(sapply(pcadata1(), is.numeric))], scale = FALSE)),
      "scale" = data.frame(scale(pcadata1()[, which(sapply(pcadata1(), is.numeric))]))
    )
    
    # Validando matriz numérica
    validate(need(ncol(matrix) > 0, "No numeric columns available for t-SNE analysis."))
    
    # Removendo duplicatas
    matrix <- unique(matrix)
    
    # Validando após remoção de duplicatas
    validate(need(nrow(matrix) > 0, "No unique rows left after removing duplicates."))
    
    # Executanto t-SNE
    tsne_result <- Rtsne(
      as.matrix(matrix),
      dims = input$ncompTSNE,
      perplexity = input$perplexityTSNE,
      max_iter = input$niterTSNE,
      theta = input$thetaTSNE
    )
    
    TSNE(tsne_result)
    removeModal()
  })
  
  #----------------------------------- PLOTS --------------------------------------
  
  observeEvent(c(input$importTSNEmodel, input$bTSNE, input$classcoresTSNE), ignoreInit = TRUE, {
    req(TSNE())
    
    # Resumo dos resultados t-SNE
    
    output$SumTSNE <- renderDT({
      tsne_results <- as.data.frame(TSNE()$Y)
      
      # Verificando consistência de ID e atribuindo nomes de linhas
      
      if (!is.null(id()) && length(id()) == nrow(tsne_results)) {
        rownames(tsne_results) <- id()
      } else {
        rownames(tsne_results) <- seq_len(nrow(tsne_results))
      }
      datatable(tsne_results, options = list(pageLength = 7))
    })
    
    # 2D Projection
    
    output$TSNEscores <- renderPlotly({
      scores <- as.data.frame(TSNE()$Y)
      # Garantindo que as dimensões sejam consistentes com a classe
      scores$Class <- if (!is.null(sampleclass()) && length(sampleclass()) == nrow(scores)) {
        sampleclass()
      } else {
        "Undefined"
      }
      
      if (input$classcoresTSNE) {
        plot_ly(scores, x = ~V1, y = ~V2, color = ~Class, type = "scatter", mode = 'markers') %>%
          layout(title = "t-SNE Projection with Class Labels",
                 xaxis = list(title = "Dimension 1"),
                 yaxis = list(title = "Dimension 2"))
      } else {
        plot_ly(scores, x = ~V1, y = ~V2, type = "scatter", mode = 'markers') %>%
          layout(title = "t-SNE Projection",
                 xaxis = list(title = "Dimension 1"),
                 yaxis = list(title = "Dimension 2"))
      }
    })
    
    # 3D Projection
    
    output$TSNE3dscores <- renderPlotly({
      scores <- as.data.frame(TSNE()$Y)
      
      # Garantindo que as dimensões sejam consistentes com a classe
      scores$Class <- if (!is.null(sampleclass()) && length(sampleclass()) == nrow(scores)) {
        sampleclass()
      } else {
        "Undefined"
      }
      
      plot_ly(scores, x = ~V1, y = ~V2, z = ~V3, color = ~Class, type = "scatter3d", mode = 'markers') %>%
        layout(title = "t-SNE 3D Projection",
               scene = list(
                 xaxis = list(title = "Dimension 1"),
                 yaxis = list(title = "Dimension 2"),
                 zaxis = list(title = "Dimension 3")
               ))
    })
  })
  
  #--------------------------- Exportar Modelo TSNE -------------------------------
  
  output$saveTSNEmodel <- downloadHandler(
    filename = function() {
      paste0(input$namemodelTSNE, "_TSNE_model.Rdata")
    },
    content = function(file) {
      save(list = c("TSNE", "sampleclass", "id", "variables"), file = file)
    }
  )
  
  #--------------------------- Importar Modelo TSNE -------------------------------
  
  observeEvent(input$importTSNEmodel, {
    validate(need(grepl("\\.Rdata$", input$searchmodelTSNE$datapath), "Invalid file type. Please upload an .Rdata file."))
    load(input$searchmodelTSNE$datapath)
    TSNE(TSNEmodel)
    sampleclass(sampleclass)
    id(id)
    variables(variables)
  })
  
  #------------------------- Exportar Projeção CSV -------------------------------
  
  output$downloadscoresTSNE <- downloadHandler(
    filename = function() {
      paste0(input$namemodelTSNE, "_projectionTSNE.csv")
    },
    content = function(file) {
      scores <- as.data.frame(TSNE()$Y)
      if (!is.null(id()) && length(id()) == nrow(scores)) {
        rownames(scores) <- id()
      } else {
        rownames(scores) <- seq_len(nrow(scores))
      }
      write.csv(scores, file)
    }
  )
  
  
  #######CORREÇÃO DO SMOTE PARA TODO DATASET#############################
  ####################ATUALIZADO EM 19/03/2025###########################
  
  
  ############ATUALIZADO EM 13/02/2025###################################
  
  # Define reactive values
  X <- reactiveVal(NULL)
  class <- reactiveVal(NULL)
  class_vec <- reactiveVal(NULL)
  previewClicked <- reactiveVal(FALSE)
  
  output$target <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    items <- names(df)
    names(items) <- items
    selectInput("target",
                "A vector of a target class attribute corresponding to a dataset X:",
                items)
  })
  
  output$class_in <- renderUI({
    df <- filedata()
    if (is.null(df) || is.null(input$target) || !(input$target %in% names(df))) return(NULL)
    
    classes <- unique(df[[input$target]])
    if (length(classes) == 0) return(NULL)
    
    selectInput("class", "Choose the class to be oversampled:",
                classes, multiple = FALSE)
  })
  
  observeEvent(input$preview, {
    previewClicked(TRUE)
  })
  
  output$table <- renderTable({
    req(previewClicked(), filedata())
    filedata()
  })
  
  class_freq <- reactive({
    req(previewClicked(), filedata(), input$target)
    table(filedata()[, input$target])
  })
  
  observe({
    if (!is.null(input$target) && !identical(input$target, "") && input$target %in% names(filedata())) {
      df <- filedata()
      X(subset(df, df[, input$target] == input$class, select = -which(names(df) == input$target)))
      class(subset(df, select = input$target))
      class_vec(subset(df, df[, input$target] == input$class, select = input$target))
    }
  })
  
  runSMOTE <- reactiveVal(FALSE)
  
  observeEvent(input$runSMOTE, {
    req(input$K, input$dup_size)
    
    imported_data <- filedata()
    if (is.null(imported_data) || is.null(input$target) || !(input$target %in% names(imported_data))) {
      showNotification("Error: Invalid data or target not found.", type = "error")
      return(NULL)
    }
    
    if (input$smote_all_classes) {
      set.seed(123)
      genData <- smotefamily::SMOTE(
        X = imported_data[, which(sapply(imported_data, is.numeric))],
        target = as.factor(imported_data[[input$target]]),
        K = input$K,
        dup_size = input$dup_size
      )
      
      if (is.null(genData$data) || nrow(genData$data) == 0) {
        showNotification("No synthetic samples were generated. Check SMOTE parameters.", type = "error")
        return(NULL)
      }
      
      SMOTE_Data(list(
        data = as.data.frame(genData$data),
        syn_data = as.data.frame(genData$syn_data)
      ))
    } else {
      if (is.null(X()) || is.null(class_vec()) || nrow(as.data.frame(X())) == 0) {
        showNotification("Error: Not enough samples to apply SMOTE.", type = "error")
        return(NULL)
      }
      
      set.seed(123)
      df_class_vec <- imported_data[[input$target]]
      X_selected <- imported_data[df_class_vec == input$class, which(sapply(imported_data, is.numeric)), drop = FALSE]
      
      if (nrow(X_selected) == 0) {
        showNotification("Error: Not enough samples to apply SMOTE.", type = "error")
        return(NULL)
      }
      
      genData <- smotefamily::SMOTE(
        X = X_selected,
        target = as.factor(df_class_vec[df_class_vec == input$class]),
        K = input$K,
        dup_size = input$dup_size
      )
      
      if (is.null(genData$data) || nrow(genData$data) == 0) {
        showNotification("No synthetic samples were generated. Check SMOTE parameters.", type = "error")
        return(NULL)
      }
      
      smote_synthetic_data <- as.data.frame(genData$syn_data)
      smote_synthetic_data$class <- input$class
      
      feature_cols <- setdiff(names(imported_data), input$target)
      smote_synthetic_data <- smote_synthetic_data[, feature_cols, drop = FALSE]
      smote_synthetic_data$class <- input$class
      
      previous_synthetic_data <- if (!is.null(SMOTE_Data())) SMOTE_Data()$syn_data else NULL
      
      if (!is.null(previous_synthetic_data)) {
        smote_synthetic_data <- rbind(previous_synthetic_data, smote_synthetic_data)
      }
      
      updated_data <- imported_data[, feature_cols, drop = FALSE]
      updated_data$class <- imported_data[[input$target]]
      
      results <- list(
        data = rbind(updated_data, smote_synthetic_data),
        syn_data = smote_synthetic_data,
        orig_N = updated_data[updated_data$class != input$class, , drop = FALSE],
        orig_P = updated_data[updated_data$class == input$class, , drop = FALSE]
      )
      
      rownames(results$data) <- NULL  
      rownames(results$syn_data) <- NULL  
      rownames(results$orig_N) <- NULL  
      rownames(results$orig_P) <- NULL  
      
      SMOTE_Data(results)
    }
  })
  
  #######CORREÇÃO DO SMOTE PARA TODO DATASET#############################
  ####################ATUALIZADO EM 19/03/2025###########################
  
  ###############ALTERAÇÃO EM 08/03/2025#######################################  
  
  
  #ACRÉSCIMO DE SINTAXE
  
  observeEvent(input$runSMOTE, {
    runSMOTENC(FALSE)  # Reset SMOTE-NC(EM 10/05/2025 PARA SOBRESCREVER GRÁFICO ANTERIOR CASO OUTRO MÉTODO TENHA SIDO EXECUTADO ANTES)
    runSMOTE(TRUE) # Set the reactive value to TRUE to trigger SMOTE_Data
    
    output$summary_gen <- renderPrint({
      req(SMOTE_Data()) # Ensure SMOTE_Data is available
      isolate({
        if (runSMOTE()) return(SMOTE_Data())
        NULL
      })
    })
    
    # Render the gen_table for SMOTE results when the button is clicked
    
    output$gen_table <- renderTable({
      #if (runSMOTE()) SMOTE_Data()$data
      if (runSMOTE()) SMOTE_Data()$syn_data  ##MODIFICADO EM 27/02/2025
    })
    
    # Render the smoteTable for SMOTE results when the button is clicked
    
    output$smoteTable <- renderDT({
      #if (runSMOTE()) SMOTE_Data()$data
      if (runSMOTE()) SMOTE_Data()$syn_data ##MODIFICADO EM 27/02/2025
    })
    
  })
  
  smoteTableData <- reactive({
    req(SMOTE_Data())
    #data <- SMOTE_Data()$data
    data <- SMOTE_Data()$syn_data ##MODIFICADO EM 27/02/2025
    # If you need to perform any modifications on the data before exporting, you can do it here.
    data
  })
  
  ##TRECHO INIBIDO EM 13/03/2025(NÃO HÁ NECESSIDADE DOS RESULTADOS EM CADA ALGORITMO)
  
  #  #  Download SMOTE Results Table (CSV) 
  #   output$downloadSmoteTable <- downloadHandler(
  #   filename = function() {
  #   paste("smote_table_", Sys.Date(), ".csv", sep = "")
  #  },
  #  content = function(file) {
  #   req(SMOTE_Data(), SMOTE_Data()$syn_data)
  
  #  if (nrow(SMOTE_Data()$syn_data) == 0) {
  #   showNotification("Erro: Nenhum dado sintético para exportação.", type = "error")
  #  return(NULL)
  #  }
  
  # options(scipen = 999)
  
  #  #MODIFICADO EM 27/02/2025 - ARGUMENTO INVÁLIDO
  # #write.csv2(SMOTE_Data()$syn_data, file, row.names = FALSE, dec = ",")
  #  write.csv2(SMOTE_Data()$syn_data, file, row.names = FALSE)
  #  }
  #  )
  
  ##    Download SMOTE Results Table (Excel) 
  #   output$downloadSmoteTableExcel <- downloadHandler(
  #   filename = function() {
  #   paste("smote_table_", Sys.Date(), ".xlsx", sep = "")
  # },
  # content = function(file) {
  # req(SMOTE_Data(), SMOTE_Data()$syn_data)
  
  #  if (nrow(SMOTE_Data()$syn_data) == 0) {
  #  showNotification("Erro: Nenhum dado sintético para exportação.", type = "error")
  #  return(NULL)
  #  }
  
  # options(scipen = 999)
  #  write_xlsx(SMOTE_Data()$syn_data, file)
  #     }
  #  )
  
  ##TRECHO INIBIDO EM 13/03/2025
  
  ##ATUALIZAÇÃO DO BARPLOT QUANDO CLICO EM "RUN SMOTE" E NÃO EM "CONFIRM SMOTE" EM 19/03/2025
  
  # Corrigindo o gráfico barplot
  
  #output$barplot_SMOTE <- renderPlot({
  output$barplot_SMOTE <- renderPlotly({
    req(SMOTE_Data())  
    
    # Obtendo a Count das classes originais antes do SMOTE
    
    original_counts <- table(as.factor(sampleclass()))  
    
    # Obtendo as amostras Synthetics acumuladas
    
    synthetic_counts <- if (!is.null(SMOTE_Data()$syn_data) && nrow(SMOTE_Data()$syn_data) > 0) {
      table(factor(SMOTE_Data()$syn_data$class, levels = names(original_counts)))
    } else {
      setNames(rep(0, length(original_counts)), names(original_counts))  
    }
    
    # Convertendo `synthetic_counts` para numérico e evitando NAs
    
    synthetic_counts <- as.numeric(synthetic_counts)
    synthetic_counts[is.na(synthetic_counts)] <- 0  
    
    
    
    df_plot <- data.frame(
      Class = names(original_counts),  
      Frequency = as.numeric(original_counts) + as.numeric(synthetic_counts)
    )
    
    num_classes <- length(unique(df_plot$Class))
    num_colors <- max(3, num_classes)
    class_colors <- RColorBrewer::brewer.pal(num_colors, "Set1")
    names(class_colors) <- unique(df_plot$Class)
    
    plot_ly(
      data = df_plot, 
      x = ~Class, 
      y = ~Frequency, 
      type = "bar",
      color = ~Class,  
      colors = class_colors,
      text = ~Frequency,
      textposition = 'outside'
    ) %>%
      layout(
        title = "Class Frequency",
        xaxis = list(title = "Class"),
        yaxis = list(title = "Frequency"),
        showlegend = TRUE
      )
  })
  
  #})  
  
  # Evento que executa o SMOTE ao clicar no botão
  observeEvent(input$runSMOTE, {
    runSMOTENC(FALSE)  # Reset SMOTE-NC((EM 10/05/2025 PARA SOBRESCREVER GRÁFICO ANTERIOR CASO OUTRO MÉTODO TENHA SIDO EXECUTADO ANTES))
    runSMOTE(TRUE) # Ativa o processo de SMOTE
    
    output$summary_gen <- renderPrint({
      req(SMOTE_Data())
      isolate({
        if (runSMOTE()) return(SMOTE_Data())
        NULL
      })
    })
    
    # Renderizando tabela de amostras Synthetics após SMOTE
    output$gen_table <- renderTable({
      if (runSMOTE()) SMOTE_Data()$syn_data
    })
    
    output$smoteTable <- renderDT({
      if (runSMOTE()) SMOTE_Data()$syn_data
    })
  })
  
  
  #  # Corrigindo o gráfico barplot
  
  #    #output$barplot_SMOTE <- renderPlot({
  #    output$barplot_SMOTE <- renderPlotly({
  #      req(SMOTE_Data())  
  
  #      # Obtendo a Count das classes originais antes do SMOTE
  
  #      original_counts <- table(as.factor(sampleclass()))  
  
  #      # Obtendo as amostras Synthetics acumuladas
  
  #      synthetic_counts <- if (!is.null(SMOTE_Data()$syn_data) && nrow(SMOTE_Data()$syn_data) > 0) {
  #        table(factor(SMOTE_Data()$syn_data$class, levels = names(original_counts)))
  #      } else {
  #        setNames(rep(0, length(original_counts)), names(original_counts))  
  #      }
  
  #      # Convertendo `synthetic_counts` para numérico e evitando NAs
  
  #      synthetic_counts <- as.numeric(synthetic_counts)
  #      synthetic_counts[is.na(synthetic_counts)] <- 0  
  
  
  
  #         df_plot <- data.frame(
  #          Class = names(original_counts),  
  #         Frequency = as.numeric(original_counts) + as.numeric(synthetic_counts)
  #        )
  
  #       num_classes <- length(unique(df_plot$Class))
  #        num_colors <- max(3, num_classes)
  #       class_colors <- RColorBrewer::brewer.pal(num_colors, "Set1")
  #        names(class_colors) <- unique(df_plot$Class)
  
  #       plot_ly(
  #        data = df_plot, 
  #       x = ~Class, 
  #        y = ~Frequency, 
  #       type = "bar",
  #        color = ~Class,  
  #       colors = class_colors,
  #        text = ~Frequency,
  #       textposition = 'outside'
  #        ) %>%
  #         layout(
  #          title = "Class Frequency",
  #       xaxis = list(title = "Class"),
  #        yaxis = list(title = "Frequency"),
  #       showlegend = TRUE
  #        )
  #       })
  
  #  })  
  
  
  
  
  #  observeEvent(input$runSMOTE, {
  #    runSMOTE(TRUE) # Set the reactive value to TRUE to trigger SMOTE_Data
  
  #    output$summary_gen <- renderPrint({
  #      req(SMOTE_Data()) # Ensure SMOTE_Data is available
  #      isolate({
  #        if (runSMOTE()) return(SMOTE_Data())
  #        NULL
  #      })
  #    })
  
  #    # Render the gen_table for SMOTE results when the button is clicked
  
  #    output$gen_table <- renderTable({
  #      #if (runSMOTE()) SMOTE_Data()$data
  #      if (runSMOTE()) SMOTE_Data()$syn_data  ##MODIFICADO EM 27/02/2025
  #    })
  
  #    # Render the smoteTable for SMOTE results when the button is clicked
  
  #    output$smoteTable <- renderDT({
  #      #if (runSMOTE()) SMOTE_Data()$data
  #      if (runSMOTE()) SMOTE_Data()$syn_data ##MODIFICADO EM 27/02/2025
  #    })
  
  #  })
  
  #  smoteTableData <- reactive({
  #    req(SMOTE_Data())
  #    #data <- SMOTE_Data()$data
  #    data <- SMOTE_Data()$syn_data ##MODIFICADO EM 27/02/2025
  #    # If you need to perform any modifications on the data before exporting, you can do it here.
  #    data
  #  })
  
  #  #  Download SMOTE Results Table (CSV) 
  #    output$downloadSmoteTable <- downloadHandler(
  #     filename = function() {
  #      paste("smote_table_", Sys.Date(), ".csv", sep = "")
  #    },
  #    content = function(file) {
  #     req(SMOTE_Data(), SMOTE_Data()$syn_data)
  
  #    if (nrow(SMOTE_Data()$syn_data) == 0) {
  #     showNotification("Erro: Nenhum dado sintético para exportação.", type = "error")
  #    return(NULL)
  #    }
  
  #   options(scipen = 999)
  
  #    #MODIFICADO EM 27/02/2025 - ARGUMENTO INVÁLIDO
  #   #write.csv2(SMOTE_Data()$syn_data, file, row.names = FALSE, dec = ",")
  #    write.csv2(SMOTE_Data()$syn_data, file, row.names = FALSE)
  #    }
  #    )
  
  #    Download SMOTE Results Table (Excel) 
  #    output$downloadSmoteTableExcel <- downloadHandler(
  #     filename = function() {
  #      paste("smote_table_", Sys.Date(), ".xlsx", sep = "")
  #   },
  #    content = function(file) {
  #     req(SMOTE_Data(), SMOTE_Data()$syn_data)
  
  #    if (nrow(SMOTE_Data()$syn_data) == 0) {
  #     showNotification("Erro: Nenhum dado sintético para exportação.", type = "error")
  #    return(NULL)
  #    }
  
  #    options(scipen = 999)
  #    write_xlsx(SMOTE_Data()$syn_data, file)
  #      }
  #    )
  
  
  
  #  observeEvent(input$confirmSMOTE, {
  #    #ACRÉSCIMO EM 19/01/2025
  #    #req(SMOTE_Data())
  #    req(data(), SMOTE_Data())
  
  #    data(SMOTE_Data()$data[, -ncol(SMOTE_Data()$data)])
  #    sampleclass(SMOTE_Data()$data[, ncol(SMOTE_Data()$data)])
  #    id(rownames(SMOTE_Data()$data))
  
  
  #  })
  
  ##################inibido em 08/03/2025
  
  
  
  
  ####SMOTE-NC ADICIONADO EM 09/05/2025
  
  # Reactive para armazenar o resultado do SMOTE-NC
  SMOTENC_Data <- reactiveVal(NULL)
  runSMOTENC <- reactiveVal(FALSE)
  
  output$target_nc <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    items <- names(df)
    names(items) <- items
    selectInput("target_nc",
                "A vector of a target class attribute corresponding to a dataset X:",
                items)
  })
  
  
  
  observeEvent(input$runSMOTENC, {
    runSMOTE(FALSE)  # Reset SMOTE(EM 10/05/2025 PARA SOBRESCREVER GRÁFICO ANTERIOR CASO OUTRO MÉTODO TENHA SIDO EXECUTADO ANTES)
    runSMOTENC(TRUE) 
    
    req(filedata(), input$target_nc, input$smote_sampling_strategy, input$K_nc)
    
    df <- filedata()
    target_col <- input$target_nc
    
    
    # Convertendo a variável de classe em fator
    y <- as.factor(df[[target_col]])
    levels(y) <- make.names(levels(y))
    
    # Removendo coluna de classe original para reconstruí-la como "class"
    df[[target_col]] <- NULL
    
    # Separando preditoras
    X <- df[, , drop = FALSE]
    
    # Detectando Variables categóricas
    categorical_cols <- names(X)[sapply(X, function(x) is.factor(x) || is.character(x))]
    
    # Se não houver, cria variável fake
    if (length(categorical_cols) == 0) {
      X$fake_cat <- factor(sample(c("A", "B"), size = nrow(X), replace = TRUE))
      categorical_cols <- "fake_cat"
      showNotification("No categorical variable found. 'fake_cat' created.", type = "warning")
    }
    
    # Índices para SMOTENC (0-based)
    cat_indices <- which(names(X) %in% categorical_cols) - 1
    cat_indices <- as.list(as.integer(cat_indices))
    
    # Verificando menor classe
    class_counts <- table(y)
    min_class_size <- min(class_counts)
    if (min_class_size < 2) {
      showNotification("The smallest class has less than 2 samples.", type = "error")
      return()
    }
    
    # Ajustando k
    k_value <- input$K_nc
    if (min_class_size <= k_value) {
      k_value <- min_class_size - 1
      if (k_value < 1) {
        showNotification("k neighbors too high for the smallest class.", type = "error")
        return()
      }
      showNotification(paste("k_neighbors adjusted for", k_value), type = "warning")
    }
    
    # SMOTENC via Python
    smote_nc <- imblearn$SMOTENC(
      categorical_features = cat_indices,
      sampling_strategy = input$smote_sampling_strategy,
      k_neighbors = k_value,
      random_state = 42L
    )
    
    result <- smote_nc$fit_resample(r_to_py(X), r_to_py(y))
    
    # Finalizando
    X_resampled <- as.data.frame(py_to_r(result[[1]]))
    y_resampled <- as.character(py_to_r(result[[2]]))
    
    # Removendo variável fake, se tiver sido criada
    if ("fake_cat" %in% names(X_resampled)) {
      X_resampled$fake_cat <- NULL
    }
    
    total_rows <- nrow(X_resampled)
    original_rows <- nrow(X)
    
    if (length(y_resampled) != total_rows) {
      stop("Número de labels e amostras não coincidem!")
    }
    
    df_resampled <- X_resampled
    df_resampled$class <- y_resampled
    
    
    
    df_resampled <- df_resampled[, !duplicated(names(df_resampled))]
    
    
    
    
    original_data <- df_resampled[seq_len(original_rows), , drop = FALSE]
    syn_data <- df_resampled[(original_rows + 1):total_rows, , drop = FALSE]
    
    original_data <- original_data[, c(setdiff(names(original_data), "class"), "class")]
    syn_data <- syn_data[, c(setdiff(names(syn_data), "class"), "class")]
    
    freq <- table(original_data$class)
    minority_class <- names(freq)[which.min(freq)]
    orig_P <- dplyr::filter(original_data, class == minority_class)
    orig_N <- dplyr::filter(original_data, class != minority_class)
    
    combined <- dplyr::bind_rows(
      dplyr::mutate(original_data, SampleID = paste0("Orig_", seq_len(nrow(original_data))), Origem = "Original"),
      dplyr::mutate(syn_data, SampleID = paste0("SMOTENC_", seq_len(nrow(syn_data))), Origem = "Sintético")
    )
    
    SMOTENC_Data(list(
      data = dplyr::bind_rows(original_data, syn_data),
      syn_data = syn_data,
      orig_P = orig_P,
      orig_N = orig_N
      # combined é interno(não exposto)
    ))
    
    runSMOTENC(TRUE)
  })
  
  #observeEvent(input$confirmSMOTENC, {
  # req(SMOTENC_Data(), runSMOTENC())
  #  prev_data <- smote_history$data
  #  new_data <- SMOTENC_Data()$syn_data
  #  new_data <- dplyr::anti_join(new_data, prev_data[, colnames(new_data), drop = FALSE],
  #                              by = intersect(colnames(new_data), colnames(prev_data)))
  #  smote_history$data <- dplyr::bind_rows(prev_data, new_data)
  #  showNotification("Amostras do SMOTE_NC confirmadas com sucesso!", type = "message")
  #  runSMOTENC(FALSE)
  # })
  
  output$summary_gen_nc <- renderPrint({
    req(SMOTENC_Data())
    isolate({
      if (runSMOTENC()) {
        obj <- SMOTENC_Data()
        obj$combined <- NULL  # Removendo para não mostrar
        return(obj)
      }
      NULL
    })
  })
  
  output$gen_table_nc <- renderTable({
    if (runSMOTENC()) SMOTENC_Data()$syn_data
  })
  
  output$smoteTable_nc <- renderDT({
    if (runSMOTENC()) SMOTENC_Data()$syn_data
  })
  
  output$barplot_SMOTENC <- renderPlotly({
    req(SMOTENC_Data())
    df <- SMOTENC_Data()$data
    validate(need("class" %in% colnames(df), "Coluna 'class' ausente no conjunto."))
    class_counts <- table(factor(df$class))
    df_plot <- data.frame(
      Class = names(class_counts),
      Frequency = as.numeric(class_counts)
    )
    num_classes <- length(df_plot$Class)
    class_colors <- RColorBrewer::brewer.pal(min(9, max(3, num_classes)), "Set1")
    names(class_colors) <- df_plot$Class
    plot_ly(
      data = df_plot,
      x = ~Class,
      y = ~Frequency,
      type = "bar",
      color = ~Class,
      colors = class_colors,
      text = ~Frequency,
      textposition = 'outside'
    ) %>%
      layout(
        title = "Class Frequency",
        xaxis = list(title = "Class"),
        yaxis = list(title = "Frequency"),
        showlegend = TRUE
      )
  })
  
  
  ####SMOTE-NC ADICIONADO EM 09/05/2025
  
  
  ####BORDERLINE SMOTE ADICIONADO EM 05/06/2025 
  
  
  
  # Importando BorderlineSMOTE corretamente
  
  BorderlineSMOTE <- import("imblearn.over_sampling")$BorderlineSMOTE
  
  # Reactive para armazenar o resultado do Borderline SMOTE
  
  BorderlineSMOTE_Data <- reactiveVal(NULL)
  runBorderlineSMOTE <- reactiveVal(FALSE)
  
  # Selecionando a variável target
  
  output$target_borderline <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    items <- names(df)
    names(items) <- items
    selectInput("target_borderline",
                "A vector of a target class attribute corresponding to a dataset X:",
                items)
  })
  
  # Executando o Borderline SMOTE
  # observeEvent(filedata(), {
  
  observeEvent(input$runBorderlineSMOTE, {
    runSMOTE(FALSE)
    runSMOTENC(FALSE)
    runBorderlineSMOTE(TRUE)
    
    
    
    req(filedata(), input$target_borderline, input$borderline_sampling_strategy, input$K_border, input$M_border)
    if (!runBorderlineSMOTE()) return()
    
    df <- filedata()
    target_col <- input$target_borderline
    
    y <- as.factor(df[[target_col]])
    levels(y) <- make.names(levels(y))
    df[[target_col]] <- NULL
    X <- df %>% dplyr::select(where(is.numeric))
    
    class_counts <- table(y)
    min_class_size <- min(class_counts)
    if (min_class_size < 2) {
      showNotification("The smallest class has less than 2 samples.", type = "error")
      return()
    }
    
    ###############BLOCO MODIFICADO EM 14/06/2025  
    
    #  k_value <- input$K_border
    #  if (min_class_size <= k_value) {
    #    k_value <- min_class_size - 1
    #    if (k_value < 1) {
    #      showNotification("k neighbors too high for the smallest class.", type = "error")
    #      return() 
    
    #    }
    #    showNotification(paste("k_neighbors adjusted for", k_value), type = "warning")
    #  }
    
    tryCatch({
      k_value <- input$K_border
      
      if (min_class_size <= k_value) {
        k_value <- min_class_size - 1
        if (k_value < 1) stop("k_neighbors inválido")
        showNotification(paste("k_neighbors adjusted for", k_value), type = "warning")
      }
      
      k_value <- as.integer(k_value)
      
    }, error = function(e) {
      showNotification("Error: k_neighbors too high. Adjustment needed.", type = "error")
      
      return(NULL)  
    })
    
    
    ###############BLOCO MODIFICADO EM 14/06/2025
    
    
    
    borderline <- BorderlineSMOTE(
      sampling_strategy = input$borderline_sampling_strategy,
      k_neighbors = k_value,
      m_neighbors = input$M_border,
      random_state = 42L
    )
    
    result <- borderline$fit_resample(r_to_py(X), r_to_py(y))
    X_resampled <- as.data.frame(py_to_r(result[[1]]))
    y_resampled <- as.character(py_to_r(result[[2]]))
    
    total_rows <- nrow(X_resampled)
    original_rows <- nrow(X)
    
    if (length(y_resampled) != total_rows) {
      stop("Número de labels e amostras não coincidem!")
    }
    
    df_resampled <- X_resampled
    df_resampled$class <- y_resampled
    
    original_data <- df_resampled[seq_len(original_rows), , drop = FALSE]
    syn_data <- df_resampled[(original_rows + 1):total_rows, , drop = FALSE]
    
    original_data <- original_data[, c(setdiff(names(original_data), "class"), "class")]
    syn_data <- syn_data[, c(setdiff(names(syn_data), "class"), "class")]
    
    freq <- table(original_data$class)
    minority_class <- names(freq)[which.min(freq)]
    orig_P <- dplyr::filter(original_data, class == minority_class)
    orig_N <- dplyr::filter(original_data, class != minority_class)
    
    BorderlineSMOTE_Data(list(
      data = dplyr::bind_rows(original_data, syn_data),
      syn_data = syn_data,
      orig_P = orig_P,
      orig_N = orig_N
    ))
  })
  
  
  observeEvent(input$runBorderlineSMOTE, {
    runSMOTE(FALSE)
    runSMOTENC(FALSE)
    runBorderlineSMOTE(TRUE)
  })
  
  
  output$summary_gen_borderline <- renderPrint({
    req(BorderlineSMOTE_Data())
    isolate({
      if (runBorderlineSMOTE()) {
        obj <- BorderlineSMOTE_Data()
        obj$combined <- NULL
        return(obj)
      }
      NULL
    })
  })
  
  output$gen_table_borderline <- renderTable({
    if (runBorderlineSMOTE()) BorderlineSMOTE_Data()$syn_data
  })
  
  output$barplot_BorderlineSMOTE <- renderPlotly({
    req(BorderlineSMOTE_Data())
    df <- BorderlineSMOTE_Data()$data
    plot_data <- df %>% count(class) %>% rename(Frequency = n, Class = class)
    
    if (ncol(plot_data) != 2 || any(is.na(plot_data))) {
      return(NULL)
    }
    
    num_classes <- length(unique(plot_data$Class))
    class_colors <- setNames(
      RColorBrewer::brewer.pal(min(9, max(3, num_classes)), "Set1"),
      unique(plot_data$Class)
    )
    
    plot_ly(
      data = plot_data,
      x = ~Class,
      y = ~Frequency,
      type = "bar",
      color = ~Class,
      colors = class_colors,
      text = ~Frequency,
      textposition = 'outside'
    ) %>%
      layout(
        title = "Class Frequency",
        xaxis = list(title = "Class"),
        yaxis = list(title = "Frequency"),
        showlegend = TRUE
      )
  })
  
  
  
  ####BORDERLINE SMOTE ADICIONADO EM 05/06/2025
  
  
  #### SVM_SMOTE ADICIONADO EM 06/06/2025 ####
  
  # Importando SVM_SMOTE corretamente
  
  SVM_SMOTE <- import("imblearn.over_sampling")$SVMSMOTE
  
  # Reactive para armazenar o resultado do SVM SMOTE
  
  SVM_SMOTE_Data <- reactiveVal(NULL)
  runSVM_SMOTE <- reactiveVal(FALSE)
  
  # UI: Seleção do alvo
  
  output$target_svm <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    items <- names(df)
    names(items) <- items
    selectInput("target_svm",
                "A vector of a target class attribute corresponding to a dataset X:",
                items)
  })
  
  # Executando o SVM SMOTE
  
  observeEvent(input$runSVM_SMOTE, {
    runSMOTE(FALSE)
    runSMOTENC(FALSE)
    runBorderlineSMOTE(FALSE)
    runSVM_SMOTE(TRUE)
    
    req(filedata(), input$target_svm, input$svm_sampling_strategy,
        input$K_svm, input$M_svm, input$SVM_estimator_C)
    
    df <- filedata()
    target_col <- input$target_svm
    y <- as.factor(df[[target_col]])
    levels(y) <- make.names(levels(y))
    df[[target_col]] <- NULL
    X <- df %>% dplyr::select(where(is.numeric))
    
    class_counts <- table(y)
    min_class_size <- min(class_counts)
    if (min_class_size < 2) {
      showNotification("The smallest class has less than 2 samples.", type = "error")
      return()
    }
    
    ###############BLOCO MODIFICADO EM 20/06/2025   
    
    # k_value <- input$K_svm
    #  if (min_class_size <= k_value) {
    #    k_value <- min_class_size - 1
    #    if (k_value < 1) {
    #      showNotification("k neighbors too high for the smallest class.", type = "error")
    #      return()
    #    }
    #    showNotification(paste("k_neighbors adjusted for", k_value), type = "warning")
    #  }
    
    
    tryCatch({
      k_value <- input$K_svm
      
      if (min_class_size <= k_value) {
        k_value <- min_class_size - 1
        if (k_value < 1) stop("k_neighbors inválido")
        showNotification(paste("k_neighbors adjusted for", k_value), type = "warning")
      }
      
      k_value <- as.integer(k_value)
      
    }, error = function(e) {
      showNotification("Error: k_neighbors too high. Adjustment needed.", type = "error")
      
      return(NULL)  
    })
    
    
    ###############BLOCO MODIFICADO EM 20/06/2025
    
    
    
    
    
    
    
    
    
    svm_smote <- SVM_SMOTE(
      sampling_strategy = input$svm_sampling_strategy,
      k_neighbors = k_value,
      m_neighbors = input$M_svm,
      svm_estimator = import("sklearn.svm")$SVC(C = input$SVM_estimator_C, probability = TRUE),
      random_state = 42L
    )
    
    result <- svm_smote$fit_resample(r_to_py(X), r_to_py(y))
    X_resampled <- as.data.frame(py_to_r(result[[1]]))
    y_resampled <- as.character(py_to_r(result[[2]]))
    
    total_rows <- nrow(X_resampled)
    original_rows <- nrow(X)
    
    if (length(y_resampled) != total_rows) {
      stop("Número de labels e amostras não coincidem!")
    }
    
    df_resampled <- X_resampled
    df_resampled$class <- y_resampled
    
    original_data <- df_resampled[seq_len(original_rows), , drop = FALSE]
    syn_data <- df_resampled[(original_rows + 1):total_rows, , drop = FALSE]
    
    original_data <- original_data[, c(setdiff(names(original_data), "class"), "class")]
    syn_data <- syn_data[, c(setdiff(names(syn_data), "class"), "class")]
    
    freq <- table(original_data$class)
    minority_class <- names(freq)[which.min(freq)]
    orig_P <- dplyr::filter(original_data, class == minority_class)
    orig_N <- dplyr::filter(original_data, class != minority_class)
    
    SVM_SMOTE_Data(list(
      data = dplyr::bind_rows(original_data, syn_data),
      syn_data = syn_data,
      orig_P = orig_P,
      orig_N = orig_N
    ))
  })
  
  output$summary_gen_svm <- renderPrint({
    req(SVM_SMOTE_Data())
    isolate({
      if (runSVM_SMOTE()) {
        obj <- SVM_SMOTE_Data()
        obj$combined <- NULL
        return(obj)
      }
      NULL
    })
  })
  
  output$gen_table_svm <- renderTable({
    if (runSVM_SMOTE()) SVM_SMOTE_Data()$syn_data
  })
  
  output$barplot_SVM_SMOTE <- renderPlotly({
    req(SVM_SMOTE_Data())
    df <- SVM_SMOTE_Data()$data
    plot_data <- df %>% count(class) %>% rename(Frequency = n, Class = class)
    
    if (ncol(plot_data) != 2 || any(is.na(plot_data))) {
      return(NULL)
    }
    
    num_classes <- length(unique(plot_data$Class))
    class_colors <- setNames(
      RColorBrewer::brewer.pal(min(9, max(3, num_classes)), "Set1"),
      unique(plot_data$Class)
    )
    
    plot_ly(
      data = plot_data,
      x = ~Class,
      y = ~Frequency,
      type = "bar",
      color = ~Class,
      colors = class_colors,
      text = ~Frequency,
      textposition = 'outside'
    ) %>%
      layout(
        title = "Class Frequency",
        xaxis = list(title = "Class"),
        yaxis = list(title = "Frequency"),
        showlegend = TRUE
      )
  })
  
  #### SVM_SMOTE ADICIONADO EM 06/06/2025 ####
  
  #### ADASYN ADICIONADO EM 08/06/2025 ####
  
  # Importando ADASYN corretamente
  
  ADASYN <- import("imblearn.over_sampling")$ADASYN
  
  # Reactive para armazenar o resultado do ADASYN
  
  ADASYN_Data <- reactiveVal(NULL)
  runADASYN <- reactiveVal(FALSE)
  
  # UI: Seleção do alvo
  
  output$target_adasyn <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    items <- names(df)
    names(items) <- items
    selectInput("target_adasyn",
                "A vector of a target class attribute corresponding to a dataset X:",
                items)
  })
  
  # Executando o ADASYN
  
  observeEvent(input$runADASYN, {
    runSMOTE(FALSE)
    runSMOTENC(FALSE)
    runBorderlineSMOTE(FALSE)
    runSVM_SMOTE(FALSE)
    runADASYN(TRUE)
    
    req(filedata(), input$target_adasyn, input$adasyn_sampling_strategy, input$K_adasyn)
    
    df <- filedata()
    target_col <- input$target_adasyn
    y <- as.factor(df[[target_col]])
    levels(y) <- make.names(levels(y))
    df[[target_col]] <- NULL
    X <- df %>% dplyr::select(where(is.numeric))
    
    class_counts <- table(y)
    min_class_size <- min(class_counts)
    if (min_class_size < 2) {
      showNotification("The smallest class has less than 2 samples.", type = "error")
      return()
    }
    
    #BLOCO MODIFICADO EM 20/06/2025    
    
    #   k_value <- input$K_adasyn
    #    if (min_class_size <= k_value) {
    #      k_value <- min_class_size - 1
    #      if (k_value < 1) {
    #        showNotification("k neighbors too high for the smallest class.", type = "error")
    #        return()
    #      }
    #      showNotification(paste("k_neighbors adjusted for", k_value), type = "warning")
    #    }
    
    tryCatch({
      k_value <- input$K_adasyn
      
      if (min_class_size <= k_value) {
        k_value <- min_class_size - 1
        if (k_value < 1) stop("k_neighbors inválido")
        showNotification(paste("k_neighbors adjusted for", k_value), type = "warning")
      }
      
      k_value <- as.integer(k_value)
      
    }, error = function(e) {
      showNotification("Error: k_neighbors too high. Adjustment needed.", type = "error")
      
      return(NULL)  
    })
    
    
    ###############BLOCO MODIFICADO EM 20/06/2025    
    
    
    
    adasyn <- ADASYN(
      sampling_strategy = input$adasyn_sampling_strategy,
      n_neighbors = k_value,
      random_state = 42L
    )
    
    result <- adasyn$fit_resample(r_to_py(X), r_to_py(y))
    X_resampled <- as.data.frame(py_to_r(result[[1]]))
    y_resampled <- as.character(py_to_r(result[[2]]))
    
    total_rows <- nrow(X_resampled)
    original_rows <- nrow(X)
    
    if (length(y_resampled) != total_rows) {
      stop("Número de labels e amostras não coincidem!")
    }
    
    df_resampled <- X_resampled
    df_resampled$class <- y_resampled
    
    original_data <- df_resampled[seq_len(original_rows), , drop = FALSE]
    syn_data <- df_resampled[(original_rows + 1):total_rows, , drop = FALSE]
    
    original_data <- original_data[, c(setdiff(names(original_data), "class"), "class")]
    syn_data <- syn_data[, c(setdiff(names(syn_data), "class"), "class")]
    
    freq <- table(original_data$class)
    minority_class <- names(freq)[which.min(freq)]
    orig_P <- dplyr::filter(original_data, class == minority_class)
    orig_N <- dplyr::filter(original_data, class != minority_class)
    
    ADASYN_Data(list(
      data = dplyr::bind_rows(original_data, syn_data),
      syn_data = syn_data,
      orig_P = orig_P,
      orig_N = orig_N
    ))
  })
  
  output$summary_gen_adasyn <- renderPrint({
    req(ADASYN_Data())
    isolate({
      if (runADASYN()) {
        obj <- ADASYN_Data()
        obj$combined <- NULL
        return(obj)
      }
      NULL
    })
  })
  
  output$gen_table_adasyn <- renderTable({
    if (runADASYN()) ADASYN_Data()$syn_data
  })
  
  output$barplot_ADASYN <- renderPlotly({
    req(ADASYN_Data())
    df <- ADASYN_Data()$data
    plot_data <- df %>% count(class) %>% rename(Frequency = n, Class = class)
    
    if (ncol(plot_data) != 2 || any(is.na(plot_data))) {
      return(NULL)
    }
    
    num_classes <- length(unique(plot_data$Class))
    class_colors <- setNames(
      RColorBrewer::brewer.pal(min(9, max(3, num_classes)), "Set1"),
      unique(plot_data$Class)
    )
    
    plot_ly(
      data = plot_data,
      x = ~Class,
      y = ~Frequency,
      type = "bar",
      color = ~Class,
      colors = class_colors,
      text = ~Frequency,
      textposition = 'outside'
    ) %>%
      layout(
        title = "Class Frequency",
        xaxis = list(title = "Class"),
        yaxis = list(title = "Frequency"),
        showlegend = TRUE
      )
  })
  
  #### ADASYN ADICIONADO EM 08/06/2025 ####
  
  #### RANDOM UPSAMPLING ADICIONADO EM 09/06/2025 ####
  
  # Importando RandomOverSampler corretamente
  
  RandomOverSampler <- import("imblearn.over_sampling")$RandomOverSampler
  
  # Reactive para armazenar o resultado do Random Upsampling
  
  RU_Data <- reactiveVal(NULL)
  runRU <- reactiveVal(FALSE)
  
  # UI: Seleção do alvo
  
  output$target_ru <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    items <- names(df)
    names(items) <- items
    selectInput("target_ru",
                "A vector of a target class attribute corresponding to a dataset X:",
                items)
  })
  
  # Executando o Random Upsampling
  
  observeEvent(input$runRU, {
    runSMOTE(FALSE)
    runSMOTENC(FALSE)
    runBorderlineSMOTE(FALSE)
    runSVM_SMOTE(FALSE)
    runADASYN(FALSE)
    runRU(TRUE)
    
    req(filedata(), input$target_ru, input$ru_sampling_strategy)
    
    df <- filedata()
    target_col <- input$target_ru
    y <- as.factor(df[[target_col]])
    levels(y) <- make.names(levels(y))
    df[[target_col]] <- NULL
    X <- df %>% dplyr::select(where(is.numeric))
    
    class_counts <- table(y)
    min_class_size <- min(class_counts)
    if (min_class_size < 2) {
      showNotification("The smallest class has less than 2 samples.", type = "error")
      return()
    }
    
    ros <- RandomOverSampler(
      sampling_strategy = input$ru_sampling_strategy,
      random_state = 42L
    )
    
    result <- ros$fit_resample(r_to_py(X), r_to_py(y))
    X_resampled <- as.data.frame(py_to_r(result[[1]]))
    y_resampled <- as.character(py_to_r(result[[2]]))
    
    total_rows <- nrow(X_resampled)
    original_rows <- nrow(X)
    
    if (length(y_resampled) != total_rows) {
      stop("Número de labels e amostras não coincidem!")
    }
    
    df_resampled <- X_resampled
    df_resampled$class <- y_resampled
    
    original_data <- df_resampled[seq_len(original_rows), , drop = FALSE]
    syn_data <- df_resampled[(original_rows + 1):total_rows, , drop = FALSE]
    
    original_data <- original_data[, c(setdiff(names(original_data), "class"), "class")]
    syn_data <- syn_data[, c(setdiff(names(syn_data), "class"), "class")]
    
    freq <- table(original_data$class)
    minority_class <- names(freq)[which.min(freq)]
    orig_P <- dplyr::filter(original_data, class == minority_class)
    orig_N <- dplyr::filter(original_data, class != minority_class)
    
    RU_Data(list(
      data = dplyr::bind_rows(original_data, syn_data),
      syn_data = syn_data,
      orig_P = orig_P,
      orig_N = orig_N
    ))
  })
  
  # Summary
  
  output$summary_gen_ru <- renderPrint({
    req(RU_Data())
    isolate({
      if (runRU()) {
        obj <- RU_Data()
        obj$combined <- NULL
        return(obj)
      }
      NULL
    })
  })
  
  # Tabela das amostras Synthetics
  
  output$gen_table_ru <- renderTable({
    if (runRU()) RU_Data()$syn_data
  })
  
  # Gráfico de barras interativo
  
  output$barplot_RU <- renderPlotly({
    req(RU_Data())
    df <- RU_Data()$data
    plot_data <- df %>% count(class) %>% rename(Frequency = n, Class = class)
    
    if (ncol(plot_data) != 2 || any(is.na(plot_data))) {
      return(NULL)
    }
    
    num_classes <- length(unique(plot_data$Class))
    class_colors <- setNames(
      RColorBrewer::brewer.pal(min(9, max(3, num_classes)), "Set1"),
      unique(plot_data$Class)
    )
    
    plot_ly(
      data = plot_data,
      x = ~Class,
      y = ~Frequency,
      type = "bar",
      color = ~Class,
      colors = class_colors,
      text = ~Frequency,
      textposition = 'outside'
    ) %>%
      layout(
        title = "Class Frequency",
        xaxis = list(title = "Class"),
        yaxis = list(title = "Frequency"),
        showlegend = TRUE
      )
  })
  
  #### RANDOM UPSAMPLING ADICIONADO EM 09/06/2025 ####
  
  #### TOMEK LINKS ADICIONADO EM 09/06/2025 ####
  
  # Importando TomekLinks corretamente
  
  TomekLinks <- import("imblearn.under_sampling")$TomekLinks
  
  # Reactive para armazenar o resultado do Tomek Links
  
  Tomek_Data <- reactiveVal(NULL)
  runTomek <- reactiveVal(FALSE)
  
  # UI: Seleção do alvo
  
  output$target_tomek <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    items <- names(df)
    names(items) <- items
    selectInput("target_tomek",
                "A vector of a target class attribute corresponding to a dataset X:",
                items)
  })
  
  # Executando o Tomek Links
  
  observeEvent(input$runTomek, {
    runSMOTE(FALSE)
    runSMOTENC(FALSE)
    runBorderlineSMOTE(FALSE)
    runSVM_SMOTE(FALSE)
    runADASYN(FALSE)
    runRU(FALSE)
    runTomek(TRUE)
    
    req(filedata(), input$target_tomek, input$tomek_sampling_strategy)
    
    df <- filedata()
    target_col <- input$target_tomek
    y <- as.factor(df[[target_col]])
    levels(y) <- make.names(levels(y))
    df[[target_col]] <- NULL
    X <- df %>% dplyr::select(where(is.numeric))
    
    class_counts <- table(y)
    if (any(class_counts < 2)) {
      showNotification("Some class has less than 2 samples.", type = "error")
      return()
    }
    
    tomek <- TomekLinks(
      sampling_strategy = input$tomek_sampling_strategy,
      n_jobs = as.integer(-1)
    )
    
    result <- tomek$fit_resample(r_to_py(X), r_to_py(y))
    X_resampled <- as.data.frame(py_to_r(result[[1]]))
    y_resampled <- as.character(py_to_r(result[[2]]))
    
    if (length(y_resampled) != nrow(X_resampled)) {
      stop("Número de labels e amostras não coincidem!")
    }
    
    df_resampled <- X_resampled
    df_resampled$class <- y_resampled
    
    # Comparando com dados originais para identificar amostras removidas
    
    original_data <- cbind(X, class = as.character(y))
    cleaned_data <- df_resampled
    
    original_data <- original_data[, c(setdiff(names(original_data), "class"), "class")]
    cleaned_data <- cleaned_data[, c(setdiff(names(cleaned_data), "class"), "class")]
    
    # Definindo amostras removidas como "Synthetics negativas"
    
    original_ids <- paste0("row_", do.call(paste, original_data))
    cleaned_ids <- paste0("row_", do.call(paste, cleaned_data))
    removed_mask <- !(original_ids %in% cleaned_ids)
    removed_data <- original_data[removed_mask, , drop = FALSE]
    
    freq <- table(cleaned_data$class)
    minority_class <- names(freq)[which.min(freq)]
    orig_P <- dplyr::filter(cleaned_data, class == minority_class)
    orig_N <- dplyr::filter(cleaned_data, class != minority_class)
    
    Tomek_Data(list(
      data = cleaned_data,
      removed_data = removed_data,
      orig_P = orig_P,
      orig_N = orig_N
    ))
  })
  
  # Summary
  
  output$summary_gen_tomek <- renderPrint({
    req(Tomek_Data())
    isolate({
      if (runTomek()) {
        obj <- Tomek_Data()
        obj$combined <- NULL
        return(obj)
      }
      NULL
    })
  })
  
  # Tabela das amostras removidas
  
  output$gen_table_tomek <- renderTable({
    if (runTomek()) Tomek_Data()$removed_data
  })
  
  # Gráfico de barras interativo
  
  output$barplot_Tomek <- renderPlotly({
    req(Tomek_Data())
    df <- Tomek_Data()$data
    plot_data <- df %>% count(class) %>% rename(Frequency = n, Class = class)
    
    if (ncol(plot_data) != 2 || any(is.na(plot_data))) {
      return(NULL)
    }
    
    num_classes <- length(unique(plot_data$Class))
    class_colors <- setNames(
      RColorBrewer::brewer.pal(min(9, max(3, num_classes)), "Set1"),
      unique(plot_data$Class)
    )
    
    plot_ly(
      data = plot_data,
      x = ~Class,
      y = ~Frequency,
      type = "bar",
      color = ~Class,
      colors = class_colors,
      text = ~Frequency,
      textposition = 'outside'
    ) %>%
      layout(
        title = "Class Frequency",
        xaxis = list(title = "Class"),
        yaxis = list(title = "Frequency"),
        showlegend = TRUE
      )
  })
  
  #### TOMEK LINKS ADICIONADO EM 09/06/2025 ####
  
  #### NEARMISS ADICIONADO EM 16/06/2025 ####
  
  # Importando NearMiss corretamente
  
  NearMiss <- import("imblearn.under_sampling")$NearMiss
  
  # Reactive para armazenar o resultado do NearMiss
  
  NearMiss_Data <- reactiveVal(NULL)
  runNearMiss <- reactiveVal(FALSE)
  
  # UI: Seleção do alvo
  
  output$target_nearmiss <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    items <- names(df)
    names(items) <- items
    selectInput("target_nearmiss",
                "A vector of a target class attribute corresponding to a dataset X:",
                items)
  })
  
  # Executando o NearMiss
  
  observeEvent(input$runNearMiss, {
    runSMOTE(FALSE)
    runSMOTENC(FALSE)
    runBorderlineSMOTE(FALSE)
    runSVM_SMOTE(FALSE)
    runADASYN(FALSE)
    runRU(FALSE)
    runTomek(FALSE)
    runNearMiss(TRUE)
    
    req(filedata(), input$target_nearmiss, input$nearmiss_sampling_strategy,
        input$n_neighbors_nearmiss, input$version_nearmiss, input$n_jobs_nearmiss)
    
    df <- filedata()
    target_col <- input$target_nearmiss
    y <- as.factor(df[[target_col]])
    levels(y) <- make.names(levels(y))
    df[[target_col]] <- NULL
    X <- df %>% dplyr::select(where(is.numeric))
    
    class_counts <- table(y)
    if (any(class_counts < 3)) {
      showNotification("Some class has less than 3 samples to use NearMiss.", type = "error")
      return()
    }
    
    version_selected <- as.integer(input$version_nearmiss)
    
    if (version_selected == 3) {
      nm <- NearMiss(
        sampling_strategy = input$nearmiss_sampling_strategy,
        version = version_selected,
        n_neighbors = as.integer(input$n_neighbors_ver3_nearmiss),
        n_jobs = as.integer(input$n_jobs_nearmiss)
      )
    } else {
      nm <- NearMiss(
        sampling_strategy = input$nearmiss_sampling_strategy,
        version = version_selected,
        n_neighbors = as.integer(input$n_neighbors_nearmiss),
        n_jobs = as.integer(input$n_jobs_nearmiss)
      )
    }
    
    result <- nm$fit_resample(r_to_py(X), r_to_py(y))
    X_resampled <- as.data.frame(py_to_r(result[[1]]))
    y_resampled <- as.character(py_to_r(result[[2]]))
    
    if (length(y_resampled) != nrow(X_resampled)) {
      stop("Número de labels e amostras não coincidem!")
    }
    
    df_resampled <- X_resampled
    df_resampled$class <- y_resampled
    
    original_data <- cbind(X, class = as.character(y))
    cleaned_data <- df_resampled
    
    original_data <- original_data[, c(setdiff(names(original_data), "class"), "class")]
    cleaned_data <- cleaned_data[, c(setdiff(names(cleaned_data), "class"), "class")]
    
    original_ids <- paste0("row_", do.call(paste, original_data))
    cleaned_ids <- paste0("row_", do.call(paste, cleaned_data))
    removed_mask <- !(original_ids %in% cleaned_ids)
    removed_data <- original_data[removed_mask, , drop = FALSE]
    
    freq <- table(cleaned_data$class)
    minority_class <- names(freq)[which.min(freq)]
    orig_P <- dplyr::filter(cleaned_data, class == minority_class)
    orig_N <- dplyr::filter(cleaned_data, class != minority_class)
    
    NearMiss_Data(list(
      data = cleaned_data,
      removed_data = removed_data,
      orig_P = orig_P,
      orig_N = orig_N
    ))
  })
  
  # Summary
  
  output$summary_gen_nearmiss <- renderPrint({
    req(NearMiss_Data())
    isolate({
      if (runNearMiss()) {
        obj <- NearMiss_Data()
        obj$combined <- NULL
        return(obj)
      }
      NULL
    })
  })
  
  # Tabela das amostras removidas
  
  output$gen_table_nearmiss <- renderTable({
    if (runNearMiss()) NearMiss_Data()$removed_data
  })
  
  # Gráfico de barras interativo
  
  output$barplot_NearMiss <- renderPlotly({
    req(NearMiss_Data())
    df <- NearMiss_Data()$data
    plot_data <- df %>% count(class) %>% rename(Frequency = n, Class = class)
    
    if (ncol(plot_data) != 2 || any(is.na(plot_data))) {
      return(NULL)
    }
    
    num_classes <- length(unique(plot_data$Class))
    class_colors <- setNames(
      RColorBrewer::brewer.pal(min(9, max(3, num_classes)), "Set1"),
      unique(plot_data$Class)
    )
    
    plot_ly(
      data = plot_data,
      x = ~Class,
      y = ~Frequency,
      type = "bar",
      color = ~Class,
      colors = class_colors,
      text = ~Frequency,
      textposition = 'outside'
    ) %>%
      layout(
        title = "Class Frequency",
        xaxis = list(title = "Class"),
        yaxis = list(title = "Frequency"),
        showlegend = TRUE
      )
  })
  
  #### NEARMISS ADICIONADO EM 16/06/2025 ####
  
  #### ENN ADICIONADO EM 17/06/2025 ####
  
  # Importando ENN corretamente
  
  EditedNearestNeighbours <- import("imblearn.under_sampling")$EditedNearestNeighbours
  
  # Reactive para armazenar o resultado do ENN
  
  ENN_Data <- reactiveVal(NULL)
  runENN <- reactiveVal(FALSE)
  
  # UI: Seleção do alvo
  
  output$target_enn <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    items <- names(df)
    names(items) <- items
    selectInput("target_enn",
                "A vector of a target class attribute corresponding to a dataset X:",
                items)
  })
  
  # Executando o ENN
  
  observeEvent(input$runENN, {
    runSMOTE(FALSE)
    runSMOTENC(FALSE)
    runBorderlineSMOTE(FALSE)
    runSVM_SMOTE(FALSE)
    runADASYN(FALSE)
    runRU(FALSE)
    runTomek(FALSE)
    runNearMiss(FALSE)
    runENN(TRUE)
    
    req(filedata(), input$target_enn, input$enn_sampling_strategy,
        input$enn_n_neighbors, input$enn_kind_sel, input$enn_n_jobs)
    
    df <- filedata()
    target_col <- input$target_enn
    y <- as.factor(df[[target_col]])
    levels(y) <- make.names(levels(y))
    df[[target_col]] <- NULL
    X <- df %>% dplyr::select(where(is.numeric))
    
    class_counts <- table(y)
    if (any(class_counts < 3)) {
      showNotification("Some class has less than 3 samples to use ENN.", type = "error")
      return()
    }
    
    enn <- EditedNearestNeighbours(
      sampling_strategy = input$enn_sampling_strategy,
      n_neighbors = as.integer(input$enn_n_neighbors),
      kind_sel = input$enn_kind_sel,
      n_jobs = as.integer(input$enn_n_jobs)
    )
    
    result <- enn$fit_resample(r_to_py(X), r_to_py(y))
    X_resampled <- as.data.frame(py_to_r(result[[1]]))
    y_resampled <- as.character(py_to_r(result[[2]]))
    
    if (length(y_resampled) != nrow(X_resampled)) {
      stop("Número de labels e amostras não coincidem!")
    }
    
    df_resampled <- X_resampled
    df_resampled$class <- y_resampled
    
    original_data <- cbind(X, class = as.character(y))
    cleaned_data <- df_resampled
    
    original_data <- original_data[, c(setdiff(names(original_data), "class"), "class")]
    cleaned_data <- cleaned_data[, c(setdiff(names(cleaned_data), "class"), "class")]
    
    original_ids <- paste0("row_", do.call(paste, original_data))
    cleaned_ids <- paste0("row_", do.call(paste, cleaned_data))
    removed_mask <- !(original_ids %in% cleaned_ids)
    removed_data <- original_data[removed_mask, , drop = FALSE]
    
    freq <- table(cleaned_data$class)
    minority_class <- names(freq)[which.min(freq)]
    orig_P <- dplyr::filter(cleaned_data, class == minority_class)
    orig_N <- dplyr::filter(cleaned_data, class != minority_class)
    
    ENN_Data(list(
      data = cleaned_data,
      removed_data = removed_data,
      orig_P = orig_P,
      orig_N = orig_N
    ))
  })
  
  
  # Summary
  
  output$summary_gen_enn <- renderPrint({
    req(ENN_Data())
    isolate({
      if (runENN()) {
        obj <- ENN_Data()
        obj$combined <- NULL
        return(obj)
      }
      NULL
    })
  })
  
  # Tabela das amostras removidas
  
  output$gen_table_enn <- renderTable({
    if (runENN()) ENN_Data()$removed_data
  })
  
  # Gráfico de barras interativo
  
  output$barplot_ENN <- renderPlotly({
    req(ENN_Data())
    df <- ENN_Data()$data
    plot_data <- df %>% count(class) %>% rename(Frequency = n, Class = class)
    
    if (ncol(plot_data) != 2 || any(is.na(plot_data))) {
      return(NULL)
    }
    
    num_classes <- length(unique(plot_data$Class))
    class_colors <- setNames(
      RColorBrewer::brewer.pal(min(9, max(3, num_classes)), "Set1"),
      unique(plot_data$Class)
    )
    
    plot_ly(
      data = plot_data,
      x = ~Class,
      y = ~Frequency,
      type = "bar",
      color = ~Class,
      colors = class_colors,
      text = ~Frequency,
      textposition = 'outside'
    ) %>%
      layout(
        title = "Class Frequency",
        xaxis = list(title = "Class"),
        yaxis = list(title = "Frequency"),
        showlegend = TRUE
      )
  })
  
  #### ENN ADICIONADO EM 17/06/2025 ####
  
  
  #### OSS ADICIONADO EM 18/06/2025 ####
  
  # Importando OSS corretamente
  
  OneSidedSelection <- import("imblearn.under_sampling")$OneSidedSelection
  
  # Reactive para armazenar o resultado do OSS
  
  OSS_Data <- reactiveVal(NULL)
  runOSS <- reactiveVal(FALSE)
  
  # UI: Seleção do alvo
  
  output$target_oss <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    items <- names(df)
    names(items) <- items
    selectInput("target_oss",
                "A vector of a target class attribute corresponding to a dataset X:",
                items)
  })
  
  # Executando o OSS
  
  observeEvent(input$runOSS, {
    runSMOTE(FALSE)
    runSMOTENC(FALSE)
    runBorderlineSMOTE(FALSE)
    runSVM_SMOTE(FALSE)
    runADASYN(FALSE)
    runRU(FALSE)
    runTomek(FALSE)
    runNearMiss(FALSE)
    runENN(FALSE)
    runOSS(TRUE)
    
    req(filedata(), input$target_oss, input$oss_sampling_strategy,
        input$oss_n_neighbors, input$oss_n_seeds_S, input$oss_random_state, input$oss_n_jobs)
    
    df <- filedata()
    target_col <- input$target_oss
    y <- as.factor(df[[target_col]])
    levels(y) <- make.names(levels(y))
    df[[target_col]] <- NULL
    X <- df %>% dplyr::select(where(is.numeric))
    
    class_counts <- table(y)
    if (any(class_counts < 3)) {
      showNotification("Some class has less than 3 samples to use OSS.", type = "error")
      return()
    }
    
    oss <- OneSidedSelection(
      sampling_strategy = input$oss_sampling_strategy,
      n_neighbors = as.integer(input$oss_n_neighbors),
      n_seeds_S = as.integer(input$oss_n_seeds_S),
      random_state = as.integer(input$oss_random_state),
      n_jobs = as.integer(input$oss_n_jobs)
    )
    
    result <- oss$fit_resample(r_to_py(X), r_to_py(y))
    X_resampled <- as.data.frame(py_to_r(result[[1]]))
    y_resampled <- as.character(py_to_r(result[[2]]))
    
    if (length(y_resampled) != nrow(X_resampled)) {
      stop("Número de labels e amostras não coincidem!")
    }
    
    df_resampled <- X_resampled
    df_resampled$class <- y_resampled
    
    original_data <- cbind(X, class = as.character(y))
    cleaned_data <- df_resampled
    
    original_data <- original_data[, c(setdiff(names(original_data), "class"), "class")]
    cleaned_data <- cleaned_data[, c(setdiff(names(cleaned_data), "class"), "class")]
    
    original_ids <- paste0("row_", do.call(paste, original_data))
    cleaned_ids <- paste0("row_", do.call(paste, cleaned_data))
    removed_mask <- !(original_ids %in% cleaned_ids)
    removed_data <- original_data[removed_mask, , drop = FALSE]
    
    freq <- table(cleaned_data$class)
    minority_class <- names(freq)[which.min(freq)]
    orig_P <- dplyr::filter(cleaned_data, class == minority_class)
    orig_N <- dplyr::filter(cleaned_data, class != minority_class)
    
    OSS_Data(list(
      data = cleaned_data,
      removed_data = removed_data,
      orig_P = orig_P,
      orig_N = orig_N
    ))
  })
  
  # Summary
  
  output$summary_gen_oss <- renderPrint({
    req(OSS_Data())
    isolate({
      if (runOSS()) {
        obj <- OSS_Data()
        obj$combined <- NULL
        return(obj)
      }
      NULL
    })
  })
  
  # Tabela das amostras removidas
  
  output$gen_table_oss <- renderTable({
    if (runOSS()) OSS_Data()$removed_data
  })
  
  # Gráfico de barras interativo
  
  output$barplot_OSS <- renderPlotly({
    req(OSS_Data())
    df <- OSS_Data()$data
    plot_data <- df %>% count(class) %>% rename(Frequency = n, Class = class)
    
    if (ncol(plot_data) != 2 || any(is.na(plot_data))) {
      return(NULL)
    }
    
    num_classes <- length(unique(plot_data$Class))
    class_colors <- setNames(
      RColorBrewer::brewer.pal(min(9, max(3, num_classes)), "Set1"),
      unique(plot_data$Class)
    )
    
    plot_ly(
      data = plot_data,
      x = ~Class,
      y = ~Frequency,
      type = "bar",
      color = ~Class,
      colors = class_colors,
      text = ~Frequency,
      textposition = 'outside'
    ) %>%
      layout(
        title = "Class Frequency",
        xaxis = list(title = "Class"),
        yaxis = list(title = "Frequency"),
        showlegend = TRUE
      )
  })
  
  #### OSS ADICIONADO EM 18/06/2025 ####
  
  #### RANDOM DOWNSAMPLING (RD) ADICIONADO EM 18/06/2025 ####
  
  # Importando RandomUnderSampler corretamente
  
  RandomUnderSampler <- import("imblearn.under_sampling")$RandomUnderSampler
  
  # Reactive para armazenar o resultado do RD
  
  RD_Data <- reactiveVal(NULL)
  runRD <- reactiveVal(FALSE)
  
  # UI: Seleção do alvo
  
  output$target_rd <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    items <- names(df)
    names(items) <- items
    selectInput("target_rd",
                "A vector of a target class attribute corresponding to a dataset X:",
                items)
  })
  
  # Executando o Random Downsampling
  
  observeEvent(input$runRD, {
    runSMOTE(FALSE)
    runSMOTENC(FALSE)
    runBorderlineSMOTE(FALSE)
    runSVM_SMOTE(FALSE)
    runADASYN(FALSE)
    runRU(FALSE)
    runTomek(FALSE)
    runNearMiss(FALSE)
    runENN(FALSE)
    runOSS(FALSE)
    runRD(TRUE)
    
    req(filedata(), input$target_rd, input$rd_sampling_strategy, input$rd_random_state)
    
    df <- filedata()
    target_col <- input$target_rd
    y <- as.factor(df[[target_col]])
    levels(y) <- make.names(levels(y))
    df[[target_col]] <- NULL
    X <- df %>% dplyr::select(where(is.numeric))
    
    class_counts <- table(y)
    if (any(class_counts < 2)) {
      showNotification("Some class has less than 2 samples to use Random Downsampling.", type = "error")
      return()
    }
    
    rus <- RandomUnderSampler(
      sampling_strategy = input$rd_sampling_strategy,
      random_state = as.integer(input$rd_random_state),
      replacement = input$rd_replacement
    )
    
    result <- rus$fit_resample(r_to_py(X), r_to_py(y))
    X_resampled <- as.data.frame(py_to_r(result[[1]]))
    y_resampled <- as.character(py_to_r(result[[2]]))
    
    if (length(y_resampled) != nrow(X_resampled)) {
      stop("Número de labels e amostras não coincidem!")
    }
    
    df_resampled <- X_resampled
    df_resampled$class <- y_resampled
    
    original_data <- cbind(X, class = as.character(y))
    cleaned_data <- df_resampled
    
    original_data <- original_data[, c(setdiff(names(original_data), "class"), "class")]
    cleaned_data <- cleaned_data[, c(setdiff(names(cleaned_data), "class"), "class")]
    
    original_ids <- paste0("row_", do.call(paste, original_data))
    cleaned_ids <- paste0("row_", do.call(paste, cleaned_data))
    removed_mask <- !(original_ids %in% cleaned_ids)
    removed_data <- original_data[removed_mask, , drop = FALSE]
    
    freq <- table(cleaned_data$class)
    minority_class <- names(freq)[which.min(freq)]
    orig_P <- dplyr::filter(cleaned_data, class == minority_class)
    orig_N <- dplyr::filter(cleaned_data, class != minority_class)
    
    RD_Data(list(
      data = cleaned_data,
      removed_data = removed_data,
      orig_P = orig_P,
      orig_N = orig_N
    ))
  })
  
  # Summary
  
  output$summary_gen_rd <- renderPrint({
    req(RD_Data())
    isolate({
      if (runRD()) {
        obj <- RD_Data()
        obj$combined <- NULL
        return(obj)
      }
      NULL
    })
  })
  
  # Tabela das amostras removidas
  
  output$gen_table_rd <- renderTable({
    if (runRD()) RD_Data()$removed_data
  })
  
  # Gráfico de barras interativo
  
  output$barplot_RD <- renderPlotly({
    req(RD_Data())
    df <- RD_Data()$data
    plot_data <- df %>% count(class) %>% rename(Frequency = n, Class = class)
    
    if (ncol(plot_data) != 2 || any(is.na(plot_data))) {
      return(NULL)
    }
    
    num_classes <- length(unique(plot_data$Class))
    class_colors <- setNames(
      RColorBrewer::brewer.pal(min(9, max(3, num_classes)), "Set1"),
      unique(plot_data$Class)
    )
    
    plot_ly(
      data = plot_data,
      x = ~Class,
      y = ~Frequency,
      type = "bar",
      color = ~Class,
      colors = class_colors,
      text = ~Frequency,
      textposition = 'outside'
    ) %>%
      layout(
        title = "Class Frequency",
        xaxis = list(title = "Class"),
        yaxis = list(title = "Frequency"),
        showlegend = TRUE
      )
  })
  
  #### RANDOM DOWNSAMPLING (RD) ADICIONADO EM 18/06/2025 ####
  
  
  #### SBC (Undersampling Based on Clustering) ADICIONADO EM 10/07/2025 ####
  
  
  
  # Reactive para armazenar resultado SBC
  
  SBC_Data <- reactiveVal(NULL)
  runSBC <- reactiveVal(FALSE)
  
  # Seletor da variável alvo
  
  output$target_sbc <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    items <- names(df)
    names(items) <- items
    selectInput(
      "target_sbc",
      "Select the target variable:",
      items
    )
  })
  
  # Seletor das classes a subamostrar
  
  output$classes_to_undersample_sbc <- renderUI({
    df <- filedata()
    if (is.null(df) || is.null(input$target_sbc)) return(NULL)
    y <- as.factor(df[[input$target_sbc]])
    cls <- unique(y)
    selectInput(
      "sbc_classes",
      "Select classes to subsample:",
      choices = cls,
      multiple = TRUE
    )
  })
  
  # Executando SBC
  
  observeEvent(input$runSBC, {
    runSBC(TRUE)
    
    req(filedata(), input$target_sbc, input$sbc_classes, input$sbc_target_n, input$sbc_k)
    
    df <- filedata()
    target_col <- input$target_sbc
    
    y <- as.factor(df[[target_col]])
    df[[target_col]] <- NULL
    X <- df %>% dplyr::select(where(is.numeric))
    df_all <- cbind(X, class = y)
    
    classes_selected <- input$sbc_classes
    target_n <- input$sbc_target_n
    safe_k <- input$sbc_k
    
    cleaned_blocks <- list()
    removed_blocks <- list()
    
    for (cls in classes_selected) {
      cls_size <- nrow(df_all[df_all$class == cls, ])
      safe_target <- min(target_n, cls_size)
      safe_k_cls <- min(safe_k, floor(cls_size / 2))
      if (safe_k_cls < 1) safe_k_cls <- 1
      
      sbc_res <- tryCatch({
        scutr::undersample_kmeans(
          data = df_all,        # dataframe completo
          cls = cls,            # classe específica
          cls_col = "class",    # coluna do rótulo
          m = safe_target,      # número final de amostras
          k = safe_k_cls
        )
      }, error = function(e) {
        showNotification(paste("Error in SBC for class", cls, ":", e$message), type = "error")
        return(NULL)
      })
      
      if (is.null(sbc_res)) {
        cleaned_blocks[[cls]] <- df_all[df_all$class == cls, ]
        next
      }
      
      cleaned_blocks[[cls]] <- sbc_res
      
      # Identificando removidos
      
      original_cls <- df_all[df_all$class == cls, ]
      original_ids <- paste0("row_", do.call(paste, original_cls))
      cleaned_ids <- paste0("row_", do.call(paste, sbc_res))
      removed_mask <- !(original_ids %in% cleaned_ids)
      removed_blocks[[cls]] <- original_cls[removed_mask, , drop = FALSE]
    }
    
    # Juntando intermediárias intactas
    
    intermediate_data <- df_all[!(df_all$class %in% classes_selected), ]
    final_cleaned <- dplyr::bind_rows(cleaned_blocks) %>% dplyr::bind_rows(intermediate_data)
    final_removed <- dplyr::bind_rows(removed_blocks)
    
    SBC_Data(list(
      data = final_cleaned,
      removed_data = final_removed
    ))
    
    showNotification("SBC executed successfully!", type = "message")
  })
  
  ###########COMENTÁRIO EM 10/07/2025
  
  # Confirmando SBC e propaga para onde precisar
  
#  observeEvent(input$confirmSBC, {
#    req(SBC_Data())
    
#    showNotification("SBC confirmado com sucesso!", type = "message")
#  })
  
  ###########COMENTÁRIO EM 10/07/2025
  
  
  
  # Summary
  
  output$summary_gen_sbc <- renderPrint({
    req(SBC_Data())
    isolate({
      if (runSBC()) {
        obj <- SBC_Data()
        return(obj)
      }
      NULL
    })
  })
  
  # Tabela removidas
  
  output$gen_table_sbc <- renderTable({
    if (runSBC()) SBC_Data()$removed_data
  })
  
  # Barplot SBC
  
  output$barplot_SBC <- renderPlotly({
    req(SBC_Data())
    df <- SBC_Data()$data
    plot_data <- df %>% count(class) %>% rename(Frequency = n, Class = class)
    
    if (ncol(plot_data) != 2 || any(is.na(plot_data))) return(NULL)
    
    num_classes <- length(unique(plot_data$Class))
    class_colors <- setNames(
      RColorBrewer::brewer.pal(min(9, max(3, num_classes)), "Set1"),
      unique(plot_data$Class)
    )
    
    plot_ly(
      data = plot_data,
      x = ~Class,
      y = ~Frequency,
      type = "bar",
      color = ~Class,
      colors = class_colors,
      text = ~Frequency,
      textposition = 'outside'
    ) %>%
      layout(
        title = "Class Frequency",
        xaxis = list(title = "Class"),
        yaxis = list(title = "Frequency"),
        showlegend = TRUE
      )
  })
  
  
  #### SBC (Undersampling Based on Clustering) ADICIONADO EM 10/07/2025 ####
  
  
  #### SMOTE TOMEK LINKS (SMOTE-TL) ADICIONADO EM 18/06/2025 ####
  
  # Importações corretas
  
  SMOTETomek <- import("imblearn.combine")$SMOTETomek
  SMOTE <- import("imblearn.over_sampling")$SMOTE
  TomekLinks <- import("imblearn.under_sampling")$TomekLinks
  
  # Reactive para armazenar o resultado
  
  SMOTETL_Data <- reactiveVal(NULL)
  runSMOTETL <- reactiveVal(FALSE)
  
  # UI: seleção do alvo
  
  output$target_tl <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    items <- names(df)
    names(items) <- items
    selectInput("target_tl",
                "A vector of a target class attribute corresponding to a dataset X:",
                items)
  })
  
  # Execução do SMOTE-TL
  
  observeEvent(input$runSMOTETL, {
    runSMOTE(FALSE)
    runSMOTENC(FALSE)
    runBorderlineSMOTE(FALSE)
    runSVM_SMOTE(FALSE)
    runADASYN(FALSE)
    runRU(FALSE)
    runTomek(FALSE)
    runNearMiss(FALSE)
    runENN(FALSE)
    runOSS(FALSE)
    runRD(FALSE)
    runSMOTETL(TRUE)
    
    req(filedata(), input$target_tl, input$tl_sampling_strategy, input$tl_random_state, input$tl_k_neighbors)
    
    df <- filedata()
    target_col <- input$target_tl
    y <- as.factor(df[[target_col]])
    levels(y) <- make.names(levels(y))
    df[[target_col]] <- NULL
    X <- df %>% dplyr::select(where(is.numeric))
    
    class_counts <- table(y)
    if (any(class_counts < 2)) {
      showNotification("Some class has less than 2 samples to apply SMOTE-TL.", type = "error")
      return()
    }
    
    
    # Detectando o número de amostras da menor classe
    
    minority_class_size <- min(table(y))
    k_safe <- min(as.integer(input$tl_k_neighbors), minority_class_size - 1)
    
    # Força para inteiro escalar único
    k_safe <- as.integer(k_safe)[1]
    
    if (k_safe < 1) {
      showNotification("Minority class has insufficient samples for the defined number of neighbors.", type = "error")
      return()
    }
    
    smote_obj <- SMOTE(
      sampling_strategy = input$tl_sampling_strategy,
      random_state = as.integer(input$tl_random_state),
      k_neighbors = r_to_py(k_safe)
    )
    
    
    
    
    # Criando objeto SMOTE-Tomek com SMOTE e TomekLinks
    
    smotetl <- SMOTETomek(
      smote = smote_obj,
      tomek = TomekLinks(),
      random_state = as.integer(input$tl_random_state)
    )
    
    # Aplicando o fit_resample
    
    result <- smotetl$fit_resample(r_to_py(X), r_to_py(y))
    X_resampled <- as.data.frame(py_to_r(result[[1]]))
    y_resampled <- as.character(py_to_r(result[[2]]))
    
    if (length(y_resampled) != nrow(X_resampled)) {
      stop("Número de labels e amostras não coincidem!")
    }
    
    df_resampled <- X_resampled
    df_resampled$class <- y_resampled
    
    original_data <- cbind(X, class = as.character(y))
    cleaned_data <- df_resampled
    
    original_data <- original_data[, c(setdiff(names(original_data), "class"), "class")]
    cleaned_data <- cleaned_data[, c(setdiff(names(cleaned_data), "class"), "class")]
    
    # Detectando amostras removidas
    
    original_ids <- paste0("row_", do.call(paste, original_data))
    cleaned_ids <- paste0("row_", do.call(paste, cleaned_data))
    removed_mask <- !(original_ids %in% cleaned_ids)
    removed_data <- original_data[removed_mask, , drop = FALSE]
    
    # Detectando amostras Synthetics
    
    synthetic_mask <- !(cleaned_ids %in% original_ids)
    syn_data <- cleaned_data[synthetic_mask, , drop = FALSE]
    
    # Separando minoritária/majoritária
    
    freq <- table(cleaned_data$class)
    minority_class <- names(freq)[which.min(freq)]
    orig_P <- dplyr::filter(cleaned_data, class == minority_class)
    orig_N <- dplyr::filter(cleaned_data, class != minority_class)
    
    # Armazenando tudo
    
    SMOTETL_Data(list(
      data = cleaned_data,
      removed_data = removed_data,
      syn_data = syn_data,
      orig_P = orig_P,
      orig_N = orig_N
    ))
    
  })
  
  # Summary
  
  output$summary_gen_tl <- renderPrint({
    req(SMOTETL_Data())
    isolate({
      if (runSMOTETL()) {
        obj <- SMOTETL_Data()
        obj$combined <- NULL
        return(obj)
      }
      NULL
    })
  })
  
  # Tabela de removidas e Synthetics(adicionadas)
  
  ####BLOCO MODIFICADO EM 28/06/2025
  
  output$gen_table_tl <- renderTable({
    if (isTRUE(runSMOTETL())) {
      data <- SMOTETL_Data()
      removed <- data$removed_data
      synthetic <- data$syn_data
      
      if (is.null(removed) || is.null(synthetic)) return(NULL)
      if (nrow(removed) == 0 && nrow(synthetic) == 0) return(NULL)
      
      if (nrow(removed) > 0) {
        removed$origem <- "Removed"
      } else {
        removed <- NULL
      }
      
      if (nrow(synthetic) > 0) {
        synthetic$origem <- "Synthetic"
      } else {
        synthetic <- NULL
      }
      
      # Fazendo bind_rows de quem existe
      
      out <- dplyr::bind_rows(removed, synthetic)
      if (nrow(out) == 0) return(NULL)
      return(out)
    }
  })
  
  
  
  
  ####BLOCO MODIFICADO EM 28/06/2025
  
  
  
  # Gráfico de barras
  
  output$barplot_TL <- renderPlotly({
    req(SMOTETL_Data())
    df <- SMOTETL_Data()$data
    plot_data <- df %>% count(class) %>% rename(Frequency = n, Class = class)
    
    if (ncol(plot_data) != 2 || any(is.na(plot_data))) {
      return(NULL)
    }
    
    num_classes <- length(unique(plot_data$Class))
    class_colors <- setNames(
      RColorBrewer::brewer.pal(min(9, max(3, num_classes)), "Set1"),
      unique(plot_data$Class)
    )
    
    plot_ly(
      data = plot_data,
      x = ~Class,
      y = ~Frequency,
      type = "bar",
      color = ~Class,
      colors = class_colors,
      text = ~Frequency,
      textposition = 'outside'
    ) %>%
      layout(
        title = "Class Frequency",
        xaxis = list(title = "Class"),
        yaxis = list(title = "Frequency"),
        showlegend = TRUE
      )
  })
  
  #### SMOTE TOMEK LINKS (SMOTE-TL) ADICIONADO EM 18/06/2025 ####
  
  #### SMOTE ENN (SMOTE_ENN) ADICIONADO EM 03/07/2025 ####
  
  # Importações corretas
  
  SMOTEENN <- import("imblearn.combine")$SMOTEENN
  SMOTE2 <- import("imblearn.over_sampling")$SMOTE
  EditedNearestNeighbours2 <- import("imblearn.under_sampling")$EditedNearestNeighbours
  
  # Reactive para armazenar o resultado
  
  SMOTEENN_Data <- reactiveVal(NULL)
  runSMOTEENN <- reactiveVal(FALSE)
  
  # UI: seleção do alvo
  
  output$target_enn2 <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    items <- names(df)
    names(items) <- items
    selectInput("target_enn2",
                "A vector of a target class attribute corresponding to a dataset X:",
                items)
  })
  
  # Execução do SMOTE-ENN
  
  observeEvent(input$runSMOTEENN, {
    # Resetando outros métodos
    runSMOTE(FALSE)
    runSMOTENC(FALSE)
    runBorderlineSMOTE(FALSE)
    runSVM_SMOTE(FALSE)
    runADASYN(FALSE)
    runRU(FALSE)
    runTomek(FALSE)
    runNearMiss(FALSE)
    runENN(FALSE)
    runOSS(FALSE)
    runRD(FALSE)
    runSMOTETL(FALSE)
    runSMOTEENN(TRUE)
    
    req(filedata(), input$target_enn2, input$enn_sampling_strategy2, input$enn_random_state2, input$enn_k_neighbors2)
    
    df <- filedata()
    target_col <- input$target_enn2
    y <- as.factor(df[[target_col]])
    levels(y) <- make.names(levels(y))
    df[[target_col]] <- NULL
    X <- df %>% dplyr::select(where(is.numeric))
    
    class_counts <- table(y)
    if (any(class_counts < 2)) {
      showNotification("Some class has less than 2 samples to apply SMOTE-ENN.", type = "error")
      return()
    }
    
    # Detectando o número de amostras da menor classe
    minority_class_size <- min(table(y))
    k_safe <- min(as.integer(input$enn_k_neighbors2), minority_class_size - 1)
    k_safe <- as.integer(k_safe)[1]
    
    if (k_safe < 1) {
      showNotification("Minority class has insufficient samples for the defined number of neighbors.", type = "error")
      return()
    }
    
    smote_obj <- SMOTE2(
      sampling_strategy = input$enn_sampling_strategy2,
      random_state = as.integer(input$enn_random_state2),
      k_neighbors = r_to_py(k_safe)
    )
    
    # Criando objeto SMOTE-ENN com SMOTE e ENN
    smoteenn <- SMOTEENN(
      smote = smote_obj,
      enn = EditedNearestNeighbours2(),
      random_state = as.integer(input$enn_random_state2)
    )
    
    # Aplicando o fit_resample
    result <- smoteenn$fit_resample(r_to_py(X), r_to_py(y))
    X_resampled <- as.data.frame(py_to_r(result[[1]]))
    y_resampled <- as.character(py_to_r(result[[2]]))
    
    if (length(y_resampled) != nrow(X_resampled)) {
      stop("Número de labels e amostras não coincidem!")
    }
    
    df_resampled <- X_resampled
    df_resampled$class <- y_resampled
    
    original_data <- cbind(X, class = as.character(y))
    cleaned_data <- df_resampled
    
    original_data <- original_data[, c(setdiff(names(original_data), "class"), "class")]
    cleaned_data <- cleaned_data[, c(setdiff(names(cleaned_data), "class"), "class")]
    
    # Detectando amostras removidas
    original_ids <- paste0("row_", do.call(paste, original_data))
    cleaned_ids <- paste0("row_", do.call(paste, cleaned_data))
    removed_mask <- !(original_ids %in% cleaned_ids)
    removed_data <- original_data[removed_mask, , drop = FALSE]
    
    # Detectando amostras Synthetics
    synthetic_mask <- !(cleaned_ids %in% original_ids)
    syn_data <- cleaned_data[synthetic_mask, , drop = FALSE]
    
    # Separando minoritária/majoritária
    freq <- table(cleaned_data$class)
    minority_class <- names(freq)[which.min(freq)]
    orig_P <- dplyr::filter(cleaned_data, class == minority_class)
    orig_N <- dplyr::filter(cleaned_data, class != minority_class)
    
    # Armazenando tudo
    SMOTEENN_Data(list(
      data = cleaned_data,
      removed_data = removed_data,
      syn_data = syn_data,
      orig_P = orig_P,
      orig_N = orig_N
    ))
    
  })
  
  # Summary
  
  output$summary_gen_enn2 <- renderPrint({
    req(SMOTEENN_Data())
    isolate({
      if (runSMOTEENN()) {
        obj <- SMOTEENN_Data()
        obj$combined <- NULL
        return(obj)
      }
      NULL
    })
  })
  
  # Tabela de removidas e Synthetics(adicionadas)
  
  output$gen_table_enn2 <- renderTable({
    if (isTRUE(runSMOTEENN())) {
      data <- SMOTEENN_Data()
      removed <- data$removed_data
      synthetic <- data$syn_data
      
      if (is.null(removed) && is.null(synthetic)) return(NULL)
      if (nrow(removed) == 0 && nrow(synthetic) == 0) return(NULL)
      
      if (nrow(removed) > 0) {
        removed$origem <- "Removed"
      } else {
        removed <- NULL
      }
      
      if (nrow(synthetic) > 0) {
        synthetic$origem <- "Synthetic"
      } else {
        synthetic <- NULL
      }
      
      out <- dplyr::bind_rows(removed, synthetic)
      if (nrow(out) == 0) return(NULL)
      return(out)
    }
  })
  
  # Gráfico de barras
  
  output$barplot_ENN2 <- renderPlotly({
    req(SMOTEENN_Data())
    df <- SMOTEENN_Data()$data
    plot_data <- df %>% count(class) %>% rename(Frequency = n, Class = class)
    
    
    if (ncol(plot_data) != 2 || any(is.na(plot_data))) {
      return(NULL)
    }
    
    num_classes <- length(unique(plot_data$Class))
    class_colors <- setNames(
        RColorBrewer::brewer.pal(min(9, max(3, num_classes)), "Set1"),
      # RColorBrewer::brewer.pal(min(12, max(3, num_classes)), "Set1"),
      unique(plot_data$Class)
    )
    
    plot_ly(
      data = plot_data,
      x = ~Class,
      y = ~Frequency,
      type = "bar",
      color = ~Class,
      colors = class_colors,
      text = ~Frequency,
      textposition = 'outside'
    ) %>%
      layout(
        title = "Class Frequency",
        xaxis = list(title = "Class"),
        yaxis = list(title = "Frequency"),
        showlegend = TRUE
      )
  })
  
  
  #### SMOTE ENN (SMOTE_ENN) ADICIONADO EM 03/07/2025 ####
  
  #### SMOTE IPF(SMOTE_IPF) ADICIONADO EM 13/07/2025 ####
  
  
  # IMPORTAÇÃO
  
  smote_variants <- reticulate::import("smote_variants")
  
  # Reactive
  
  SMOTEIPF_Data <- reactiveVal(NULL)
  runSMOTEIPF <- reactiveVal(FALSE)
  
  # UI Seletor
  
  output$target_ipf <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    items <- names(df)
    names(items) <- items
    selectInput(
      "target_ipf",
      "A vector of a target class attribute corresponding to a dataset X:",
      items
    )
  })
  
  # executando SMOTE IPF
  
  observeEvent(input$runSMOTEIPF, {
    runSMOTE(FALSE); runSMOTENC(FALSE); runBorderlineSMOTE(FALSE)
    runSVM_SMOTE(FALSE); runADASYN(FALSE); runRU(FALSE); runTomek(FALSE)
    runNearMiss(FALSE); runENN(FALSE); runOSS(FALSE); runRD(FALSE); runSBC(FALSE)
    runSMOTETL(FALSE); runSMOTEIPF(TRUE)
    
    req(filedata(), input$target_ipf, input$ipf_k_neighbors, input$ipf_classifiers, input$ipf_max_iter, input$ipf_random_state)
    
    df <- filedata()
    target_col <- input$target_ipf
    y <- as.factor(df[[target_col]])
    original_levels <- levels(y)
    df[[target_col]] <- NULL
    X <- df %>% dplyr::select(where(is.numeric))
    classes <- levels(y)
    
    data_list <- list()
    removed_list <- list()
    syn_list <- list()
    
    for (cls in classes) {
      y_bin <- ifelse(y == cls, 1L, 0L)
      minority_count <- sum(y_bin == 1)
      k_safe <- min(as.integer(input$ipf_k_neighbors), minority_count - 1)
      if (k_safe < 1) next
      
      smote_ipf <- smote_variants$SMOTE_IPF(
        k = as.integer(k_safe),
        classifiers = as.integer(input$ipf_classifiers),
        max_iter = as.integer(input$ipf_max_iter),
        random_state = as.integer(input$ipf_random_state)
      )
      
      result <- smote_ipf$sample(
        reticulate::r_to_py(as.matrix(X)),
        reticulate::r_to_py(y_bin)
      )
      
      X_resampled <- as.data.frame(reticulate::py_to_r(result[[1]]))
      colnames(X_resampled) <- colnames(X)
      
      y_res_bin <- py_to_r(result[[2]])
      y_res_bin <- factor(ifelse(y_res_bin == 1, cls, "Other"))
      
      df_resampled <- X_resampled
      df_resampled$class <- y_res_bin
      
      original_data <- cbind(X, class = ifelse(y_bin == 1, cls, "Other"))
      cleaned_data <- df_resampled
      
      original_ids <- apply(original_data, 1, paste0, collapse = "_")
      cleaned_ids <- apply(cleaned_data, 1, paste0, collapse = "_")
      
      removed_mask <- !(original_ids %in% cleaned_ids)
      removed_data <- original_data[removed_mask, , drop = FALSE] %>% dplyr::filter(class == cls)
      
      synthetic_mask <- !(cleaned_ids %in% original_ids)
      syn_data <- cleaned_data[synthetic_mask, , drop = FALSE] %>% dplyr::filter(class == cls)
      
      # Final DATA = originais não removidas + Synthetics
      
      final_data <- dplyr::anti_join(original_data, removed_data, by = colnames(original_data)) %>%
        dplyr::filter(class == cls) %>%
        bind_rows(syn_data)
      
      data_list[[cls]] <- final_data
      removed_list[[cls]] <- removed_data
      syn_list[[cls]] <- syn_data
    }
    
    final_data <- bind_rows(data_list)
    final_data$class <- factor(final_data$class, levels = original_levels)
    
    final_removed <- bind_rows(removed_list)
    final_removed$class <- factor(final_removed$class, levels = original_levels)
    
    final_syn <- bind_rows(syn_list)
    final_syn$class <- factor(final_syn$class, levels = original_levels)
    
    SMOTEIPF_Data(list(
      data = final_data,
      removed_data = if (nrow(final_removed) > 0) final_removed else NULL,
      syn_data = if (nrow(final_syn) > 0) final_syn else NULL
    ))
  })
  
  # summary 
  
  output$summary_gen_ipf <- renderPrint({
    req(SMOTEIPF_Data())
    isolate({
      if (runSMOTEIPF()) SMOTEIPF_Data() else NULL
    })
  })
  
  # tabela gen 
  
  output$gen_table_ipf <- renderTable({
    if (isTRUE(runSMOTEIPF())) {
      data <- SMOTEIPF_Data()
      removed <- data$removed_data
      synthetic <- data$syn_data
      
      if (!is.null(removed)) removed$origem <- "Removed"
      if (!is.null(synthetic)) synthetic$origem <- "Synthetic"
      
      out <- dplyr::bind_rows(removed, synthetic)
      if (nrow(out) == 0) return(NULL)
      
      vars <- setdiff(names(out), c("class", "origem"))
      out <- out[, c(vars, "class", "origem")]
      return(out)
    }
  })
  
  # barplot
  
  output$barplot_IPF <- renderPlotly({
    req(SMOTEIPF_Data())
    df <- SMOTEIPF_Data()$data
    plot_data <- df %>% count(class) %>% rename(Frequency = n, Class = class)
    
    if (ncol(plot_data) != 2 || any(is.na(plot_data))) return(NULL)
    
    num_classes <- length(unique(plot_data$Class))
    class_colors <- setNames(
      RColorBrewer::brewer.pal(min(9, max(3, num_classes)), "Set1"),
      unique(plot_data$Class)
    )
    
    plot_ly(
      data = plot_data,
      x = ~Class,
      y = ~Frequency,
      type = "bar",
      color = ~Class,
      colors = class_colors,
      text = ~Frequency,
      textposition = 'outside'
    ) %>%
      layout(
        title = "Class Frequency",
        xaxis = list(title = "Class"),
        yaxis = list(title = "Frequency"),
        showlegend = TRUE
      )
  })
  

  #### SMOTE IPF(SMOTE_IPF) ADICIONADO EM 13/07/2025 ####
  

  #### SPIDER ADICIONADO EM 14/07/2025 ####
  
  # IMPORTAÇÃO
  
  smote_variants <- reticulate::import("smote_variants")
  np <- reticulate::import("numpy")  
  
  # Reactive
  
  SPIDER_Data <- reactiveVal(NULL)
  runSPIDER <- reactiveVal(FALSE)
  
  # UI Seletor
  
  output$target_spider <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    items <- names(df)
    names(items) <- items
    selectInput(
      "target_spider",
      "A vector of a target class attribute corresponding to a dataset X:",
      items
    )
  })
  
  # Executando SPIDER (Borderline-SMOTE + EditedNearestNeighbours)
  
  observeEvent(input$runSPIDER, {
    runSMOTE(FALSE); runSMOTENC(FALSE); runBorderlineSMOTE(FALSE)
    runSVM_SMOTE(FALSE); runADASYN(FALSE); runRU(FALSE); runTomek(FALSE)
    runNearMiss(FALSE); runENN(FALSE); runOSS(FALSE); runRD(FALSE); runSBC(FALSE)
    runSMOTETL(FALSE); runSMOTEIPF(FALSE); runSPIDER(TRUE)
    
    req(filedata(), input$target_spider, input$spider_k_neighbors, input$spider_random_state)
    
    df <- filedata()
    target_col <- input$target_spider
    y <- as.factor(df[[target_col]])
    original_levels <- levels(y)
    df[[target_col]] <- NULL
    X <- df %>% dplyr::select(where(is.numeric))
    classes <- levels(y)
    
    data_list <- list()
    removed_list <- list()
    syn_list <- list()
    
    for (cls in classes) {
      y_bin <- ifelse(y == cls, 1L, 0L)
      y_bin <- as.integer(y_bin)
      y_bin_py <- np$array(y_bin, dtype = "int64")  
      
      minority_count <- sum(y_bin == 1)
      k_safe <- min(as.integer(input$spider_k_neighbors), minority_count - 1)
      if (k_safe < 1) next
      
      # 1) Borderline-SMOTE
      
      borderline <- smote_variants$Borderline_SMOTE1(
        k = as.integer(k_safe),
        random_state = as.integer(input$spider_random_state)
      )
      
      result_bl <- borderline$sample(
        reticulate::r_to_py(as.matrix(X)),
        y_bin_py
      )
      
      X_bl <- as.data.frame(reticulate::py_to_r(result_bl[[1]]))
      colnames(X_bl) <- colnames(X)
      y_bl <- py_to_r(result_bl[[2]])
      y_bl <- as.integer(y_bl)
      y_bl_py <- np$array(y_bl, dtype = "int64")  
      
      # 2) Edited Nearest Neighbours
      
      enn <- smote_variants$noise_removal$EditedNearestNeighbors(remove = "both")
      
      
      result_enn <- enn$remove_noise(
        reticulate::r_to_py(as.matrix(X_bl)),
        y_bl_py
      )
      
      X_clean <- as.data.frame(reticulate::py_to_r(result_enn[[1]]))
      colnames(X_clean) <- colnames(X)
      y_clean <- py_to_r(result_enn[[2]])
      y_clean <- factor(ifelse(y_clean == 1, cls, "Other"))
      
      df_resampled <- X_clean
      df_resampled$class <- y_clean
      
      original_data <- cbind(X, class = ifelse(y_bin == 1, cls, "Other"))
      cleaned_data <- df_resampled
      
      original_ids <- apply(original_data, 1, paste0, collapse = "_")
      cleaned_ids <- apply(cleaned_data, 1, paste0, collapse = "_")
      
      removed_mask <- !(original_ids %in% cleaned_ids)
      removed_data <- original_data[removed_mask, , drop = FALSE] %>% dplyr::filter(class == cls)
      
      synthetic_mask <- !(cleaned_ids %in% original_ids)
      syn_data <- cleaned_data[synthetic_mask, , drop = FALSE] %>% dplyr::filter(class == cls)
      
      final_data <- dplyr::anti_join(original_data, removed_data, by = colnames(original_data)) %>%
        dplyr::filter(class == cls) %>%
        bind_rows(syn_data)
      
      data_list[[cls]] <- final_data
      removed_list[[cls]] <- removed_data
      syn_list[[cls]] <- syn_data
    }
    
    final_data <- bind_rows(data_list)
    final_data$class <- factor(final_data$class, levels = original_levels)
    
    final_removed <- bind_rows(removed_list)
    final_removed$class <- factor(final_removed$class, levels = original_levels)
    
    final_syn <- bind_rows(syn_list)
    final_syn$class <- factor(final_syn$class, levels = original_levels)
    
    SPIDER_Data(list(
      data = final_data,
      removed_data = if (nrow(final_removed) > 0) final_removed else NULL,
      syn_data = if (nrow(final_syn) > 0) final_syn else NULL
    ))
  })
  
  # Summary
  
  output$summary_gen_spider <- renderPrint({
    req(SPIDER_Data())
    isolate({
      if (runSPIDER()) SPIDER_Data() else NULL
    })
  })
  
  # Tabela gen
  
  output$gen_table_spider <- renderTable({
    if (isTRUE(runSPIDER())) {
      data <- SPIDER_Data()
      removed <- data$removed_data
      synthetic <- data$syn_data
      
      if (!is.null(removed)) removed$origem <- "Removed"
      if (!is.null(synthetic)) synthetic$origem <- "Synthetic"
      
      out <- dplyr::bind_rows(removed, synthetic)
      if (nrow(out) == 0) return(NULL)
      
      vars <- setdiff(names(out), c("class", "origem"))
      out <- out[, c(vars, "class", "origem")]
      return(out)
    }
  })
  
  # Barplot
  
  output$barplot_spider <- renderPlotly({
    req(SPIDER_Data())
    df <- SPIDER_Data()$data
    plot_data <- df %>% count(class) %>% rename(Frequency = n, Class = class)
    
    if (ncol(plot_data) != 2 || any(is.na(plot_data))) return(NULL)
    
    num_classes <- length(unique(plot_data$Class))
    class_colors <- setNames(
      RColorBrewer::brewer.pal(min(9, max(3, num_classes)), "Set1"),
      unique(plot_data$Class)
    )
    
    plot_ly(
      data = plot_data,
      x = ~Class,
      y = ~Frequency,
      type = "bar",
      color = ~Class,
      colors = class_colors,
      text = ~Frequency,
      textposition = 'outside'
    ) %>%
      layout(
        title = "Class Frequency",
        xaxis = list(title = "Class"),
        yaxis = list(title = "Frequency"),
        showlegend = TRUE
      )
  })
  
  #### SPIDER ADICIONADO EM 14/07/2025 ####
  
  
  
  
    
  ####MODIFICADO EM 27/02/2025 ######
  
  ####### ATUALIZADO EM 10/02/2025 ########
  
  ####### ATUALIZADO EM 09/03/2025 ######## 
  #OBS: BARPLOT ATUALIZA QUANDO CLICO "RUN SMOTE"
  
  # Visualização: Class Distribution
  
  # Diagnóstico: Class Distribution # MUDANÇA DA GUIA EM 27/03/2025
  ##MODIFICADO EM 09/04/2025 - ACRÉSCIMO DOS VALORES ACIMA DAS BARRAS
  
  output$class_dist <- renderPlotly({
    req(data(), SMOTE_Data())  # Garante que os dados originais e do SMOTE estão disponíveis
    
    # Count das classes originais (essas nunca mudam)
    original_counts <- table(as.factor(sampleclass()))  
    df_original <- data.frame(
      Classe = names(original_counts),
      Count = as.numeric(original_counts),
      Tipo = names(original_counts)  # Cada classe terá sua própria cor
    )
    
    # Verificando se os dados sintéticos existem e possuem a coluna 'class'
    smote_data <- SMOTE_Data()$data
    if (is.null(smote_data) || !"class" %in% colnames(smote_data)) {
      
      # Se os dados do SMOTE ainda não foram gerados, retorna apenas os dados originais
      df_plot <- df_original
    } else {
      # Count das classes Synthetics (somente as que receberam amostras do SMOTE)
      synthetic_counts <- table(as.factor(smote_data$class))
      
      # Criando estrutura inicial SEM valores sintéticos no começo
      synthetic_aligned <- setNames(rep(0, length(original_counts)), names(original_counts))
      
      # Atualizando apenas as classes que receberam amostras Synthetics
      synthetic_aligned[names(synthetic_counts)] <- as.numeric(synthetic_counts) - original_counts[names(synthetic_counts)]
      
      # Criando DataFrame para os dados sintéticos (somente as classes que realmente ganharam amostras)
      df_synthetic <- data.frame(
        Classe = paste0(names(synthetic_aligned), " Synthetic"),  # Adicionando palavra "Synthetic" no nome
        Count = as.numeric(synthetic_aligned),
        Tipo = paste0(names(synthetic_aligned), " Synthetic")  # Cada classe Synthetic terá sua própria cor
      )
      
      # Removendo classes que ainda não receberam amostras Synthetics
      df_synthetic <- df_synthetic[df_synthetic$Count > 0, ]
      
      # Combinando os DataFrames originais e sintéticos
      df_plot <- rbind(df_original, df_synthetic)
    }
    
    # Gerando cores únicas para cada classe original e Synthetic
    unique_classes <- unique(df_plot$Tipo)
    num_classes <- length(unique_classes)
    class_colors <- RColorBrewer::brewer.pal(min(12, num_classes), "Set1")
    names(class_colors) <- unique_classes
    
    # Criando gráfico com valores em cima das barras
    plot_ly(data = df_plot) %>%
      add_trace(
        x = ~Classe,
        y = ~Count,
        type = "bar",
        text = ~Count,
        textposition = "outside",
        color = ~Tipo,
        colors = class_colors
      ) %>%
      layout(
        barmode = "group",
        title = 'Class Distribution', 
        xaxis = list(title = 'Class'),
        yaxis = list(title = 'Frequency'),
        legend = list(title = list(text = "")) #,
        # uniformtext = list(minsize = 10, mode = "hide")
      )
  })
  
  
  ####### ATUALIZADO EM 09/03/2025 ######## 
  
  
  # Diagnóstico: Class Distribution para SMOTE-NC EM 10/05/2025
  
  output$class_dist_nc <- renderPlotly({
    req(filedata(), SMOTENC_Data())
    
    # Count das classes originais
    
    
    
    original_y <- as.factor(filedata()[[input$target_nc]])
    
    
    
    original_counts <- table(original_y)
    df_original <- data.frame(
      Classe = names(original_counts),
      Count = as.numeric(original_counts),
      Tipo = names(original_counts)
    )
    
    # Dados do SMOTE-NC
    smote_data <- SMOTENC_Data()$data
    if (is.null(smote_data) || !"class" %in% colnames(smote_data)) {
      df_plot <- df_original
    } else {
      # Count total nas classes APÓS o SMOTE-NC
      smote_counts <- table(as.factor(smote_data$class))
      
      # Count Synthetic = total - original
      synthetic_aligned <- setNames(rep(0, length(original_counts)), names(original_counts))
      synthetic_aligned[names(smote_counts)] <- as.numeric(smote_counts) - original_counts[names(smote_counts)]
      
      # Criar DataFrame das Synthetics
      df_synthetic <- data.frame(
        Classe = paste0(names(synthetic_aligned), " Synthetic"),
        Count = as.numeric(synthetic_aligned),
        Tipo = paste0(names(synthetic_aligned), " Synthetic")
      )
      
      # Remover classes com 0 amostras Synthetics
      df_synthetic <- df_synthetic[df_synthetic$Count > 0, ]
      
      # Combina original + Synthetic
      df_plot <- rbind(df_original, df_synthetic)
    }
    
    # Gerando cores por classe
    unique_classes <- unique(df_plot$Tipo)
    num_classes <- length(unique_classes)
    class_colors <- RColorBrewer::brewer.pal(min(12, max(3, num_classes)), "Set1")
    names(class_colors) <- unique_classes
    
    # Gráfico
    plot_ly(data = df_plot) %>%
      add_trace(
        x = ~Classe,
        y = ~Count,
        type = "bar",
        text = ~Count,
        textposition = "outside",
        color = ~Tipo,
        colors = class_colors
      ) %>%
      layout(
        barmode = "group",
        title = "Class Distribution",
        xaxis = list(title = "Class"),
        yaxis = list(title = "Frequency"),
        legend = list(title = list(text = ""))
      )
  })
  
  
  # Diagnóstico: Class Distribution para SMOTE-NC EM 10/05/2025
  
  
  
  # Diagnóstico: Class Distribution para BorderlineSMOTE EM 05/06/2025
  
  output$class_dist_borderline <- renderPlotly({
    req(filedata(), BorderlineSMOTE_Data())
    
    # Count das classes originais
    
    original_y <- as.factor(filedata()[[input$target_borderline]])
    original_counts <- table(original_y)
    df_original <- data.frame(
      Classe = names(original_counts),
      Count = as.numeric(original_counts),
      Tipo = names(original_counts)
    )
    
    # Dados do BorderlineSMOTE
    
    smote_data <- BorderlineSMOTE_Data()$data
    if (is.null(smote_data) || !"class" %in% colnames(smote_data)) {
      df_plot <- df_original
    } else { 
      
      # Count total após o BorderlineSMOTE
      
      smote_counts <- table(as.factor(smote_data$class))
      
      # Count Synthetic = total - original
      
      synthetic_aligned <- setNames(rep(0, length(original_counts)), names(original_counts))
      synthetic_aligned[names(smote_counts)] <- as.numeric(smote_counts) - original_counts[names(smote_counts)]
      
      # DataFrame para Synthetics
      
      df_synthetic <- data.frame(
        Classe = paste0(names(synthetic_aligned), " Synthetic"),
        Count = as.numeric(synthetic_aligned),
        Tipo = paste0(names(synthetic_aligned), " Synthetic")
      )
      
      # Removendo classes sem amostras Synthetics
      
      df_synthetic <- df_synthetic[df_synthetic$Count > 0, ]
      
      # Combinando original + Synthetic
      
      df_plot <- rbind(df_original, df_synthetic)
    }
    
    # Gerando cores
    
    unique_classes <- unique(df_plot$Tipo)
    num_classes <- length(unique_classes)
    class_colors <- RColorBrewer::brewer.pal(min(12, max(3, num_classes)), "Set1")
    names(class_colors) <- unique_classes
    
    # Gráfico
    
    plot_ly(data = df_plot) %>%
      add_trace(
        x = ~Classe,
        y = ~Count,
        type = "bar",
        text = ~Count,
        textposition = "outside",
        color = ~Tipo,
        colors = class_colors
      ) %>%
      layout(
        barmode = "group",
        title = "Class Distribution",
        xaxis = list(title = "Class"),
        yaxis = list(title = "Frequency"),
        legend = list(title = list(text = ""))
      )
  })
  
  
  # Diagnóstico: Class Distribution para SVM_SMOTE EM 06/06/2025
  
  output$class_dist_svm <- renderPlotly({
    req(filedata(), SVM_SMOTE_Data())
    
    # Count das classes originais
    
    original_y <- as.factor(filedata()[[input$target_svm]])
    original_counts <- table(original_y)
    df_original <- data.frame(
      Classe = names(original_counts),
      Count = as.numeric(original_counts),
      Tipo = names(original_counts)
    )
    
    # Dados do SVM_SMOTE
    
    smote_data <- SVM_SMOTE_Data()$data
    if (is.null(smote_data) || !"class" %in% colnames(smote_data)) {
      df_plot <- df_original
    } else { 
      # Count total após o SVM_SMOTE
      
      smote_counts <- table(as.factor(smote_data$class))
      
      # Count Synthetic = total - original
      
      synthetic_aligned <- setNames(rep(0, length(original_counts)), names(original_counts))
      synthetic_aligned[names(smote_counts)] <- as.numeric(smote_counts) - original_counts[names(smote_counts)]
      
      # DataFrame para Synthetics
      
      df_synthetic <- data.frame(
        Classe = paste0(names(synthetic_aligned), " Synthetic"),
        Count = as.numeric(synthetic_aligned),
        Tipo = paste0(names(synthetic_aligned), " Synthetic")
      )
      
      # Removendo classes sem amostras Synthetics
      
      df_synthetic <- df_synthetic[df_synthetic$Count > 0, ]
      
      # Combinando original + Synthetic
      
      df_plot <- rbind(df_original, df_synthetic)
    }
    
    # Gerando cores
    
    unique_classes <- unique(df_plot$Tipo)
    num_classes <- length(unique_classes)
    class_colors <- RColorBrewer::brewer.pal(min(12, max(3, num_classes)), "Set1")
    names(class_colors) <- unique_classes
    
    # Gráfico
    
    plot_ly(data = df_plot) %>%
      add_trace(
        x = ~Classe,
        y = ~Count,
        type = "bar",
        text = ~Count,
        textposition = "outside",
        color = ~Tipo,
        colors = class_colors
      ) %>%
      layout(
        barmode = "group",
        title = "Class Distribution",
        xaxis = list(title = "Class"),
        yaxis = list(title = "Frequency"),
        legend = list(title = list(text = ""))
      )
  })
  
  
  
  # Diagnóstico: Class Distribution para adasyn EM 08/06/2025
  
  output$class_dist_adasyn <- renderPlotly({
    req(filedata(), ADASYN_Data())
    
    # Count das classes originais
    
    original_y <- as.factor(filedata()[[input$target_adasyn]])
    original_counts <- table(original_y)
    df_original <- data.frame(
      Classe = names(original_counts),
      Count = as.numeric(original_counts),
      Tipo = names(original_counts)
    )
    
    # Dados do ADASYN
    
    smote_data <- ADASYN_Data()$data
    if (is.null(smote_data) || !"class" %in% colnames(smote_data)) {
      df_plot <- df_original
    } else { 
      # Count total após o SVM_SMOTE
      
      smote_counts <- table(as.factor(smote_data$class))
      
      # Count Synthetic = total - original
      
      synthetic_aligned <- setNames(rep(0, length(original_counts)), names(original_counts))
      synthetic_aligned[names(smote_counts)] <- as.numeric(smote_counts) - original_counts[names(smote_counts)]
      
      # DataFrame para Synthetics
      
      df_synthetic <- data.frame(
        Classe = paste0(names(synthetic_aligned), " Synthetic"),
        Count = as.numeric(synthetic_aligned),
        Tipo = paste0(names(synthetic_aligned), " Synthetic")
      )
      
      # Removendo classes sem amostras Synthetics
      
      df_synthetic <- df_synthetic[df_synthetic$Count > 0, ]
      
      # Combinando original + Synthetic
      
      df_plot <- rbind(df_original, df_synthetic)
    }
    
    # Gerando cores
    
    unique_classes <- unique(df_plot$Tipo)
    num_classes <- length(unique_classes)
    class_colors <- RColorBrewer::brewer.pal(min(12, max(3, num_classes)), "Set1")
    names(class_colors) <- unique_classes
    
    # Gráfico
    
    plot_ly(data = df_plot) %>%
      add_trace(
        x = ~Classe,
        y = ~Count,
        type = "bar",
        text = ~Count,
        textposition = "outside",
        color = ~Tipo,
        colors = class_colors
      ) %>%
      layout(
        barmode = "group",
        title = "Class Distribution",
        xaxis = list(title = "Class"),
        yaxis = list(title = "Frequency"),
        legend = list(title = list(text = ""))
      )
  })
  
  
  # Diagnóstico: Class Distribution para Random Upsampling EM 09/06/2025
  
  output$class_dist_ru <- renderPlotly({
    req(filedata(), RU_Data())
    
    # Count das classes originais
    
    original_y <- as.factor(filedata()[[input$target_ru]])
    original_counts <- table(original_y)
    df_original <- data.frame(
      Classe = names(original_counts),
      Count = as.numeric(original_counts),
      Tipo = names(original_counts)
    )
    
    # Dados do Random Upsampling
    
    smote_data <- RU_Data()$data
    if (is.null(smote_data) || !"class" %in% colnames(smote_data)) {
      df_plot <- df_original
    } else {
      # Count total após o Random Upsampling
      
      smote_counts <- table(as.factor(smote_data$class))
      
      # Count Synthetic = total - original
      
      synthetic_aligned <- setNames(rep(0, length(original_counts)), names(original_counts))
      synthetic_aligned[names(smote_counts)] <- as.numeric(smote_counts) - original_counts[names(smote_counts)]
      
      # DataFrame para Synthetics
      
      df_synthetic <- data.frame(
        Classe = paste0(names(synthetic_aligned), " Synthetic"),
        Count = as.numeric(synthetic_aligned),
        Tipo = paste0(names(synthetic_aligned), " Synthetic")
      )
      
      # Removendo classes sem amostras Synthetics
      
      df_synthetic <- df_synthetic[df_synthetic$Count > 0, ]
      
      # Combinando original + Synthetic
      
      df_plot <- rbind(df_original, df_synthetic)
    }
    
    # Gerando cores
    
    unique_classes <- unique(df_plot$Tipo)
    num_classes <- length(unique_classes)
    class_colors <- RColorBrewer::brewer.pal(min(12, max(3, num_classes)), "Set1")
    names(class_colors) <- unique_classes
    
    # Gráfico
    
    plot_ly(data = df_plot) %>%
      add_trace(
        x = ~Classe,
        y = ~Count,
        type = "bar",
        text = ~Count,
        textposition = "outside",
        color = ~Tipo,
        colors = class_colors
      ) %>%
      layout(
        barmode = "group",
        title = "Class Distribution",
        xaxis = list(title = "Class"),
        yaxis = list(title = "Frequency"),
        legend = list(title = list(text = ""))
      )
  })
  
  
  # Diagnóstico: Class Distribution para Tomek Links EM 09/06/2025
  
  output$class_dist_tomek <- renderPlotly({
    req(filedata(), Tomek_Data())
    
    # Count das classes originais
    
    original_y <- as.factor(filedata()[[input$target_tomek]])
    original_counts <- table(original_y)
    df_original <- data.frame(
      Classe = names(original_counts),
      Count = as.numeric(original_counts),
      Tipo = names(original_counts)
    )
    
    # Dados após Tomek Links
    
    cleaned_data <- Tomek_Data()$data
    if (is.null(cleaned_data) || !"class" %in% colnames(cleaned_data)) {
      df_plot <- df_original
    } else {
      # Count total após remoção
      
      cleaned_counts <- table(as.factor(cleaned_data$class))
      
      # Diferença = amostras removidas
      
      removed_aligned <- setNames(rep(0, length(original_counts)), names(original_counts))
      removed_aligned[names(cleaned_counts)] <- original_counts[names(cleaned_counts)] - as.numeric(cleaned_counts)
      
      # DataFrame para removidas
      
      df_removed <- data.frame(
        # Classe = paste0(names(removed_aligned), " Removida"),
        Classe = paste0(names(removed_aligned), " Removed"), #ALTERADO EM 29/06/2025
        Count = as.numeric(removed_aligned),
        #  Tipo = paste0(names(removed_aligned), " Removida")
        Tipo = paste0(names(removed_aligned), " Removed") #ALTERADO EM 29/06/2025
      )
      
      # Removendo classes sem remoção
      
      df_removed <- df_removed[df_removed$Count > 0, ]
      
      # Combinando original + removidas
      
      df_plot <- rbind(df_original, df_removed)
    }
    
    # Gerando cores
    
    unique_classes <- unique(df_plot$Tipo)
    num_classes <- length(unique_classes)
    class_colors <- RColorBrewer::brewer.pal(min(12, max(3, num_classes)), "Set1")
    names(class_colors) <- unique_classes
    
    # Gráfico
    
    plot_ly(data = df_plot) %>%
      add_trace(
        x = ~Classe,
        y = ~Count,
        type = "bar",
        text = ~Count,
        textposition = "outside",
        color = ~Tipo,
        colors = class_colors
      ) %>%
      layout(
        barmode = "group",
        title = "Class Distribution",
        xaxis = list(title = "Class"),
        yaxis = list(title = "Frequency"),
        legend = list(title = list(text = ""))
      )
  })
  
  
  # Diagnóstico: Class Distribution para NearMiss EM 16/06/2025
  
  output$class_dist_nearmiss <- renderPlotly({
    req(filedata(), NearMiss_Data())
    
    # Count das classes originais
    
    original_y <- as.factor(filedata()[[input$target_nearmiss]])
    original_counts <- table(original_y)
    df_original <- data.frame(
      Classe = names(original_counts),
      Count = as.numeric(original_counts),
      Tipo = names(original_counts)
    )
    
    # Dados após NearMiss
    
    cleaned_data <- NearMiss_Data()$data
    if (is.null(cleaned_data) || !"class" %in% colnames(cleaned_data)) {
      df_plot <- df_original
    } else {
      # Count total após remoção
      
      cleaned_counts <- table(as.factor(cleaned_data$class))
      
      # Diferença: amostras removidas
      
      removed_aligned <- setNames(rep(0, length(original_counts)), names(original_counts))
      removed_aligned[names(cleaned_counts)] <- original_counts[names(cleaned_counts)] - as.numeric(cleaned_counts)
      
      # DataFrame para removidas
      
      df_removed <- data.frame(
        Classe = paste0(names(removed_aligned), " Removed"), #ALTERADO EM 29/06/2025
        Count = as.numeric(removed_aligned),
        Tipo = paste0(names(removed_aligned), " Removed") #ALTERADO EM 29/06/2025
      )
      
      # Removendo classes sem remoção
      
      df_removed <- df_removed[df_removed$Count > 0, ]
      
      # Combinando original + removidas
      
      df_plot <- rbind(df_original, df_removed)
    }
    
    # Gerando cores
    
    unique_classes <- unique(df_plot$Tipo)
    num_classes <- length(unique_classes)
    class_colors <- RColorBrewer::brewer.pal(min(12, max(3, num_classes)), "Set1")
    names(class_colors) <- unique_classes
    
    # Gráfico
    
    plot_ly(data = df_plot) %>%
      add_trace(
        x = ~Classe,
        y = ~Count,
        type = "bar",
        text = ~Count,
        textposition = "outside",
        color = ~Tipo,
        colors = class_colors
      ) %>%
      layout(
        barmode = "group",
        title = "Class Distribution",
        xaxis = list(title = "Class"),
        yaxis = list(title = "Frequency"),
        legend = list(title = list(text = ""))
      )
  })
  
  # Diagnóstico: Class Distribution para ENN EM 17/06/2025
  
  output$class_dist_enn <- renderPlotly({
    req(filedata(), ENN_Data())
    
    # Count das classes originais
    
    original_y <- as.factor(filedata()[[input$target_enn]])
    original_counts <- table(original_y)
    df_original <- data.frame(
      Classe = names(original_counts),
      Count = as.numeric(original_counts),
      Tipo = names(original_counts)
    )
    
    # Dados após ENN
    
    cleaned_data <- ENN_Data()$data
    if (is.null(cleaned_data) || !"class" %in% colnames(cleaned_data)) {
      df_plot <- df_original
    } else {
      # Count total após remoção
      
      cleaned_counts <- table(as.factor(cleaned_data$class))
      
      # Diferença: amostras removidas
      
      removed_aligned <- setNames(rep(0, length(original_counts)), names(original_counts))
      removed_aligned[names(cleaned_counts)] <- original_counts[names(cleaned_counts)] - as.numeric(cleaned_counts)
      
      # DataFrame para removidas
      
      df_removed <- data.frame(
        Classe = paste0(names(removed_aligned), " Removed"), #ALTERADO EM 29/06/2025
        Count = as.numeric(removed_aligned),
        Tipo = paste0(names(removed_aligned), " Removed") #ALTERADO EM 29/06/2025
      )
      
      # Removendo classes sem remoção
      
      df_removed <- df_removed[df_removed$Count > 0, ]
      
      # Combinando original + removidas
      
      df_plot <- rbind(df_original, df_removed)
    }
    
    # Gerando cores
    
    unique_classes <- unique(df_plot$Tipo)
    num_classes <- length(unique_classes)
    class_colors <- RColorBrewer::brewer.pal(min(12, max(3, num_classes)), "Set1")
    names(class_colors) <- unique_classes
    
    # Gráfico
    
    plot_ly(data = df_plot) %>%
      add_trace(
        x = ~Classe,
        y = ~Count,
        type = "bar",
        text = ~Count,
        textposition = "outside",
        color = ~Tipo,
        colors = class_colors
      ) %>%
      layout(
        barmode = "group",
        title = "Class Distribution",
        xaxis = list(title = "Class"),
        yaxis = list(title = "Frequency"),
        legend = list(title = list(text = ""))
      )
  })
  
  
  # Diagnóstico: Class Distribution para OSS EM 18/06/2025
  
  output$class_dist_oss <- renderPlotly({
    req(filedata(), OSS_Data())
    
    # Count das classes originais
    
    original_y <- as.factor(filedata()[[input$target_oss]])
    original_counts <- table(original_y)
    df_original <- data.frame(
      Classe = names(original_counts),
      Count = as.numeric(original_counts),
      Tipo = names(original_counts)
    )
    
    # Dados após OSS
    
    cleaned_data <- OSS_Data()$data
    if (is.null(cleaned_data) || !"class" %in% colnames(cleaned_data)) {
      df_plot <- df_original
    } else {
      # Count total após remoção
      
      cleaned_counts <- table(as.factor(cleaned_data$class))
      
      # Diferença: amostras removidas
      
      removed_aligned <- setNames(rep(0, length(original_counts)), names(original_counts))
      removed_aligned[names(cleaned_counts)] <- original_counts[names(cleaned_counts)] - as.numeric(cleaned_counts)
      
      # DataFrame para removidas
      
      df_removed <- data.frame(
        Classe = paste0(names(removed_aligned), " Removed"), #ALTERADO EM 29/06/2025
        Count = as.numeric(removed_aligned),
        Tipo = paste0(names(removed_aligned), " Removed") #ALTERADO EM 29/06/2025
      )
      
      # Removendo classes sem remoção
      
      df_removed <- df_removed[df_removed$Count > 0, ]
      
      # Combinando original + removidas
      
      df_plot <- rbind(df_original, df_removed)
    }
    
    # Gerando cores
    
    unique_classes <- unique(df_plot$Tipo)
    num_classes <- length(unique_classes)
    class_colors <- RColorBrewer::brewer.pal(min(12, max(3, num_classes)), "Set1")
    names(class_colors) <- unique_classes
    
    # Gráfico
    
    plot_ly(data = df_plot) %>%
      add_trace(
        x = ~Classe,
        y = ~Count,
        type = "bar",
        text = ~Count,
        textposition = "outside",
        color = ~Tipo,
        colors = class_colors
      ) %>%
      layout(
        barmode = "group",
        title = "Class Distribution",
        xaxis = list(title = "Class"),
        yaxis = list(title = "Frequency"),
        legend = list(title = list(text = ""))
      )
  })
  
  
  # Diagnóstico: Class Distribution para RD EM 18/06/2025
  
  output$class_dist_rd <- renderPlotly({
    req(filedata(), RD_Data())
    
    # Count das classes originais
    
    original_y <- as.factor(filedata()[[input$target_rd]])
    original_counts <- table(original_y)
    df_original <- data.frame(
      Classe = names(original_counts),
      Count = as.numeric(original_counts),
      Tipo = names(original_counts)
    )
    
    # Dados após RD
    
    cleaned_data <- RD_Data()$data
    if (is.null(cleaned_data) || !"class" %in% colnames(cleaned_data)) {
      df_plot <- df_original
    } else {
      
      # Count total após remoção
      
      cleaned_counts <- table(as.factor(cleaned_data$class))
      
      # Diferença: amostras removidas
      
      removed_aligned <- setNames(rep(0, length(original_counts)), names(original_counts))
      removed_aligned[names(cleaned_counts)] <- original_counts[names(cleaned_counts)] - as.numeric(cleaned_counts)
      
      # DataFrame para removidas
      
      df_removed <- data.frame(
        Classe = paste0(names(removed_aligned), " Removed"),
        Count = as.numeric(removed_aligned),
        Tipo = paste0(names(removed_aligned), " Removed")
      )
      
      # Removendo classes sem remoção
      
      df_removed <- df_removed[df_removed$Count > 0, ]
      
      # Combinando original + removidas
      
      df_plot <- rbind(df_original, df_removed)
    }
    
    # Gerando cores
    
    unique_classes <- unique(df_plot$Tipo)
    num_classes <- length(unique_classes)
    class_colors <- RColorBrewer::brewer.pal(min(12, max(3, num_classes)), "Set1")
    names(class_colors) <- unique_classes
    
    # Gráfico
    
    plot_ly(data = df_plot) %>%
      add_trace(
        x = ~Classe,
        y = ~Count,
        type = "bar",
        text = ~Count,
        textposition = "outside",
        color = ~Tipo,
        colors = class_colors
      ) %>%
      layout(
        barmode = "group",
        title = "Class Distribution",
        xaxis = list(title = "Class"),
        yaxis = list(title = "Frequency"),
        legend = list(title = list(text = ""))
      )
  })  
  
  
  # Diagnóstico: Class Distribution para SBC em 10/07/2025
  
  output$class_dist_sbc <- renderPlotly({
    req(filedata(), SBC_Data())
    
    # Contando as classes originais
    
    original_y <- as.factor(filedata()[[input$target_sbc]])
    original_counts <- table(original_y)
    df_original <- data.frame(
      Classe = names(original_counts),
      Count = as.numeric(original_counts),
      Tipo = names(original_counts)
    )
    
    # Dados após SBC
    
    cleaned_data <- SBC_Data()$data
    if (is.null(cleaned_data) || !"class" %in% colnames(cleaned_data)) {
      df_plot <- df_original
    } else {
      cleaned_counts <- table(as.factor(cleaned_data$class))
      
      # Diferença: amostras removidas
      
      removed_aligned <- setNames(rep(0, length(original_counts)), names(original_counts))
      removed_aligned[names(cleaned_counts)] <- original_counts[names(cleaned_counts)] - as.numeric(cleaned_counts)
      
      df_removed <- data.frame(
        Classe = paste0(names(removed_aligned), " Removed"),
        Count = as.numeric(removed_aligned),
        Tipo = paste0(names(removed_aligned), " Removed")
      )
      
      df_removed <- df_removed[df_removed$Count > 0, ]
      
      # Combinando original + removidas
      
      df_plot <- rbind(df_original, df_removed)
    }
    
    # Gerando cores
    
    unique_classes <- unique(df_plot$Tipo)
    num_classes <- length(unique_classes)
    class_colors <- RColorBrewer::brewer.pal(min(12, max(3, num_classes)), "Set1")
    names(class_colors) <- unique_classes
    
    # Gráfico
    
    plot_ly(data = df_plot) %>%
      add_trace(
        x = ~Classe,
        y = ~Count,
        type = "bar",
        text = ~Count,
        textposition = "outside",
        color = ~Tipo,
        colors = class_colors
      ) %>%
      layout(
        barmode = "group",
        title = "Class Distribution",
        xaxis = list(title = "Class"),
        yaxis = list(title = "Frequency"),
        legend = list(title = list(text = ""))
      )
  })
  
  
  
  
  # Diagnóstico: Class Distribution para SMOTE-TL EM 18/06/2025
  
  output$class_dist_tl <- renderPlotly({
    req(filedata(), SMOTETL_Data())
    
    # Count das classes originais
    
    original_y <- as.factor(filedata()[[input$target_tl]])
    original_counts <- table(original_y)
    df_original <- data.frame(
      Classe = names(original_counts),
      Count = as.numeric(original_counts),
      Tipo = names(original_counts)
    )
    
    # Dados após SMOTE-TL
    
    data_obj <- SMOTETL_Data()
    cleaned_data <- data_obj$data
    removed_data <- data_obj$removed_data
    syn_data <- data_obj$syn_data
    
    # Iniciando com df_original
    
    df_plot <- df_original
    
    # Amostras Synthetics
    
    if (!is.null(syn_data) && nrow(syn_data) > 0) {
      syn_counts <- table(as.factor(syn_data$class))
      df_synthetic <- data.frame(
        Classe = paste0(names(syn_counts), " Synthetic"),
        Count = as.numeric(syn_counts),
        Tipo = paste0(names(syn_counts), " Synthetic")
      )
      df_plot <- rbind(df_plot, df_synthetic)
    }
    
    # Amostras removidas
    
    if (!is.null(removed_data) && nrow(removed_data) > 0) {
      removed_counts <- table(as.factor(removed_data$class))
      df_removed <- data.frame(
        Classe = paste0(names(removed_counts), " Removed"), #ALTERADO EM 29/06/2025
        Count = as.numeric(removed_counts),
        Tipo = paste0(names(removed_counts), " Removed") #ALTERADO EM 29/06/2025
      )
      df_plot <- rbind(df_plot, df_removed)
    }
    
    # Cores
    
    unique_classes <- unique(df_plot$Tipo)
    num_classes <- length(unique_classes)
    class_colors <- RColorBrewer::brewer.pal(min(12, max(3, num_classes)), "Set1")
    names(class_colors) <- unique_classes
    
    # Gráfico
    
    plot_ly(data = df_plot) %>%
      add_trace(
        x = ~Classe,
        y = ~Count,
        type = "bar",
        text = ~Count,
        textposition = "outside",
        color = ~Tipo,
        colors = class_colors
      ) %>%
      layout(
        barmode = "group",
        title = "Class Distribution",
        xaxis = list(title = "Class"),
        yaxis = list(title = "Frequency"),
        legend = list(title = list(text = ""))
      )
  })
  
  # Diagnóstico: Class Distribution para SMOTE-ENN EM 03/07/2025
  
  output$class_dist_smotenn <- renderPlotly({
    req(filedata(), SMOTEENN_Data())
    
    # Count das classes originais
    
    original_y <- as.factor(filedata()[[input$target_enn2]])
    original_counts <- table(original_y)
    df_original <- data.frame(
      Classe = names(original_counts),
      Count = as.numeric(original_counts),
      Tipo = names(original_counts)
    )
    
    # Dados após SMOTE-ENN
    
    data_obj <- SMOTEENN_Data()
    cleaned_data <- data_obj$data
    removed_data <- data_obj$removed_data
    syn_data <- data_obj$syn_data
    
    # Iniciando com df_original
    
    df_plot <- df_original
    
    # Amostras Synthetics
    
    if (!is.null(syn_data) && nrow(syn_data) > 0) {
      syn_counts <- table(as.factor(syn_data$class))
      df_synthetic <- data.frame(
        Classe = paste0(names(syn_counts), " Synthetic"),
        Count = as.numeric(syn_counts),
        Tipo = paste0(names(syn_counts), " Synthetic")
      )
      df_plot <- rbind(df_plot, df_synthetic)
    }
    
    # Amostras removidas
    
    if (!is.null(removed_data) && nrow(removed_data) > 0) {
      removed_counts <- table(as.factor(removed_data$class))
      df_removed <- data.frame(
        Classe = paste0(names(removed_counts), " Removed"), #ALTERADO EM 29/06/2025
        Count = as.numeric(removed_counts),
        Tipo = paste0(names(removed_counts), " Removed") #ALTERADO EM 29/06/2025
      )
      df_plot <- rbind(df_plot, df_removed)
    }
    
    
    # Cores
    
    unique_classes <- unique(df_plot$Tipo)
    num_classes <- length(unique_classes)
    paleta_base <- RColorBrewer::brewer.pal(9, "Set1")
    class_colors <- colorRampPalette(paleta_base)(num_classes)
    names(class_colors) <- unique_classes
    
    
    
    # Gráfico
    
    plot_ly(data = df_plot) %>%
      add_trace(
        x = ~Classe,
        y = ~Count,
        type = "bar",
        text = ~Count,
        textposition = "outside",
        color = ~Tipo,
        colors = class_colors
      ) %>%
      layout(
        barmode = "group",
        title = "Class Distribution",
        xaxis = list(title = "Class"),
        yaxis = list(title = "Frequency"),
        legend = list(title = list(text = ""))
      )
  })
  
  # Diagnóstico: Class Distribution para SMOTE IPF EM 13/07/2025
  
  output$class_dist_smoteipf <- renderPlotly({
    req(filedata(), SMOTEIPF_Data())
    
    # Count das classes originais
    
    original_y <- as.factor(filedata()[[input$target_ipf]])
    original_counts <- table(original_y)
    df_original <- data.frame(
      Classe = names(original_counts),
      Count = as.numeric(original_counts),
      Tipo = names(original_counts)
    )
    
    # Dados após SMOTE IPF
    
    data_obj <- SMOTEIPF_Data()
    cleaned_data <- data_obj$data
    removed_data <- data_obj$removed_data
    syn_data <- data_obj$syn_data
    
    # Iniciando com df_original
    
    df_plot <- df_original
    
    # Amostras Synthetics
    
    if (!is.null(syn_data) && nrow(syn_data) > 0) {
      syn_counts <- table(as.factor(syn_data$class))
      df_synthetic <- data.frame(
        Classe = paste0(names(syn_counts), " Synthetic"),
        Count = as.numeric(syn_counts),
        Tipo = paste0(names(syn_counts), " Synthetic")
      )
      df_plot <- rbind(df_plot, df_synthetic)
    }
    
    # Amostras removidas
    
    if (!is.null(removed_data) && nrow(removed_data) > 0) {
      removed_counts <- table(as.factor(removed_data$class))
      df_removed <- data.frame(
        Classe = paste0(names(removed_counts), " Removed"), #ALTERADO EM 29/06/2025
        Count = as.numeric(removed_counts),
        Tipo = paste0(names(removed_counts), " Removed")    #ALTERADO EM 29/06/2025
      )
      df_plot <- rbind(df_plot, df_removed)
    }
    
    # Cores
    
    unique_classes <- unique(df_plot$Tipo)
    num_classes <- length(unique_classes)
    paleta_base <- RColorBrewer::brewer.pal(9, "Set1")
    class_colors <- colorRampPalette(paleta_base)(num_classes)
    names(class_colors) <- unique_classes
    
    # Gráfico
    
    plot_ly(data = df_plot) %>%
      add_trace(
        x = ~Classe,
        y = ~Count,
        type = "bar",
        text = ~Count,
        textposition = "outside",
        color = ~Tipo,
        colors = class_colors
      ) %>%
      layout(
        barmode = "group",
        title = "Class Distribution",
        xaxis = list(title = "Class"),
        yaxis = list(title = "Frequency"),
        legend = list(title = list(text = ""))
      )
  })
  
  # Diagnóstico: Class Distribution para SPIDER EM 14/07/2025
  
  output$class_dist_spider <- renderPlotly({
    req(filedata(), SPIDER_Data())
    
    # Count das classes originais
    
    original_y <- as.factor(filedata()[[input$target_spider]])
    original_counts <- table(original_y)
    df_original <- data.frame(
      Classe = names(original_counts),
      Count = as.numeric(original_counts),
      Tipo = names(original_counts)
    )
    
    # Dados após SPIDER
    
    data_obj <- SPIDER_Data()
    cleaned_data <- data_obj$data
    removed_data <- data_obj$removed_data
    syn_data <- data_obj$syn_data
    
    # Iniciando com df_original
    
    df_plot <- df_original
    
    # Amostras Synthetics
    
    if (!is.null(syn_data) && nrow(syn_data) > 0) {
      syn_counts <- table(as.factor(syn_data$class))
      df_synthetic <- data.frame(
        Classe = paste0(names(syn_counts), " Synthetic"),
        Count = as.numeric(syn_counts),
        Tipo = paste0(names(syn_counts), " Synthetic")
      )
      df_plot <- rbind(df_plot, df_synthetic)
    }
    
    # Amostras removidas
    
    if (!is.null(removed_data) && nrow(removed_data) > 0) {
      removed_counts <- table(as.factor(removed_data$class))
      df_removed <- data.frame(
        Classe = paste0(names(removed_counts), " Removed"),
        Count = as.numeric(removed_counts),
        Tipo = paste0(names(removed_counts), " Removed")
      )
      df_plot <- rbind(df_plot, df_removed)
      
      #filtrando contagens maiores que zero
      
      df_plot <- df_plot[df_plot$Count > 0, , drop = FALSE]
    }
    
    # Cores
    
    unique_classes <- unique(df_plot$Tipo)
    num_classes <- length(unique_classes)
    paleta_base <- RColorBrewer::brewer.pal(9, "Set1")
    class_colors <- colorRampPalette(paleta_base)(num_classes)
    names(class_colors) <- unique_classes
    
    # Gráfico
    
    plot_ly(data = df_plot) %>%
      add_trace(
        x = ~Classe,
        y = ~Count,
        type = "bar",
        text = ~Count,
        textposition = "outside",
        color = ~Tipo,
        colors = class_colors
      ) %>%
      layout(
        barmode = "group",
        title = "Class Distribution",
        xaxis = list(title = "Class"),
        yaxis = list(title = "Frequency"),
        legend = list(title = list(text = ""))
      )
  })
  
  
  
  ##PARA DEFINIR QUAL GRÁFICO APARECERÁ NA ÁREA GRÁFICA DE Distribution of CLASSES EM 10/05/2025
  
  #ATUALIZADO EM 05/06/2025(BORDELINE_SMOTE), 06/06/2025(SVM_SMOTE), 08/06/2025(ADASYN), 09/06/2025(Random upsampling)
  # 09/06/2025(Tomek links), 16/06/2025(Near Miss), 17/06/2025(ENN), 18/06/2025(OSS),18/06/2025(RD), 18/06/2025(SMOTE TL),
  # 03/07/2025(SMOTE ENN) # 10/07/2025 (SBC) # 13/07/2025 (SMOTE IPF) # 14/07/2025 (SPIDER)
  
  
  
  # Diagnóstico: Escolha dinâmica do gráfico com base no método executado
  
  output$class_dist_switch <- renderUI({
    if (isTRUE(runSMOTENC())) {
      plotlyOutput("class_dist_nc", height = "500px")
    } else if (isTRUE(runSMOTE())) {
      plotlyOutput("class_dist", height = "500px")
    } else if (isTRUE(runBorderlineSMOTE())) {
      plotlyOutput("class_dist_borderline", height = "500px")
    } else if (isTRUE(runSVM_SMOTE())) {
      plotlyOutput("class_dist_svm", height = "500px")
    } else if (isTRUE(runADASYN())) {
      plotlyOutput("class_dist_adasyn", height = "500px")
    } else if (isTRUE(runRU())) {
      plotlyOutput("class_dist_ru", height = "500px")
    } else if (isTRUE(runTomek())) {
      plotlyOutput("class_dist_tomek", height = "500px")
    } else if (isTRUE(runNearMiss())) {
      plotlyOutput("class_dist_nearmiss", height = "500px")
    } else if (isTRUE(runENN())) {
      plotlyOutput("class_dist_enn", height = "500px")  
    } else if (isTRUE(runOSS())) {
      plotlyOutput("class_dist_oss", height = "500px")
    } else if (isTRUE(runRD())) {
      plotlyOutput("class_dist_rd", height = "500px")
    } else if (isTRUE(runSBC())) { #ADICIONADO EM 10/07/2025
      plotlyOutput("class_dist_sbc", height = "500px")  
    } else if (isTRUE(runSMOTETL())) {
      plotlyOutput("class_dist_tl", height = "500px")
    } else if (isTRUE(runSMOTEENN())) {
      plotlyOutput("class_dist_smotenn", height = "500px")
    } else if (isTRUE(runSMOTEIPF())) { #ADICIONADO EM 13/07/2025
      plotlyOutput("class_dist_smoteipf", height = "500px")
    } else if (isTRUE(runSPIDER())) { #ADICIONADO EM 14/07/2025
      plotlyOutput("class_dist_spider", height = "500px")
      
  
    } else {
      #  h5("Nenhuma execução de SMOTE realizada ainda.")
      h5("No methods executed yet.") # MODIFICADO EM 14/06/2025 
    }
  })
  
  
  
  
  # ATUALIZADO EM 17/02/2025
  
  # Criando um objeto reativo para armazenar o histórico das rodadas do SMOTE
  smote_history <- reactiveValues(data = NULL)
  
  # Atualizando o histórico das rodadas do SMOTE
  observeEvent(SMOTE_Data(), {
    req(filedata())  # Garantindo que os dados originais estão disponíveis
    
    # Obtendo os dados originais
    original_data <- filedata()
    
    # Renomeando a variável de classe para "class"
    class_col <- input$target
    names(original_data)[names(original_data) == class_col] <- "class"
    
    # Obtendo os dados sintéticos do SMOTE
    synthetic_data <- SMOTE_Data()$syn_data
    
    # Se os dados sintéticos existem, adiciona "Synthetic" às classes Synthetics
    if (!is.null(synthetic_data) && "class" %in% colnames(synthetic_data)) {
      synthetic_data$class <- paste(synthetic_data$class, "Synthetic")
    }
    
    # Se o histórico ainda não existe, inicializa com os dados originais
    if (is.null(smote_history$data)) {
      smote_history$data <- original_data
    }
    
    # Se houver novos dados sintéticos, adiciona ao histórico sem sobrescrever
    if (!is.null(synthetic_data) && nrow(synthetic_data) > 0) {
      smote_history$data <- rbind(smote_history$data, synthetic_data)
    }
  })
  
  ###MODIFICAÇÃO DE TSNE(ACRÉSCIMO DE PARÂMETROS DINÂMICOS PRO USUÁRIO) - 20/03/2025
  
  # Visualização: Projeções
  
  # Diagnóstico: Projeções MUDANÇA DA GUIA EM 27/03/2025
  
  ##ACRÉSCIMO DE VARIÂNCIA EXPLICADA NOS EIXOS DA PCA E PCA ROBUSTA(EXPVAR) -  22/03/2025
  
  ##visualização 2D - MODIFICADA EM 20/03/2025
  
  ##ACRÉSCIMO DE LABELS NAS AMOSTRAS NA LEGENDA EM 12/04/2025
  
  ##ACRÉSCIMO DE SMOTE NC EM 28/05/2025
  
  ##ACRÉSCIMO DE BORDERLINE SMOTE EM 05/06/2025 
  
  ##ACRÉSCIMO DE SVM SMOTE EM 06/06/2025
  
  ##ACRÉSCIMO DE ADASYN EM 08/06/2025
  
  ##ACRÉSCIMO DE RANDOM UPSAMPLING EM 09/06/2025
  
  ##ACRÉSCIMO DE TOMEK LINKS EM 09/06/2025 
  
  ##ACRÉSCIMO DE NEAR MISS EM 16/06/2025 
  
  ##ACRÉSCIMO DE ENN EM 17/06/2025 
  
  ##ACRÉSCIMO DE OSS EM 18/06/2025
  
  ##ACRÉSCIMO DE RANDOM DOWNSAMPLING EM 18/06/2025
  
  ##ACRÉSCIMO DE SMOTE TL EM 20/06/2025
  
  ##ACRÉSCIMO DE SMOTE ENN EM 03/07/2025
  
  ##ACRÉSCIMO DE SBC EM 10/07/2025
  
  ##ACRÉSCIMO DE SMOTE IPF EM 13/07/2025
  
  ##ACRÉSCIMO DE SPIDER EM 14/07/2025
  
  
  output$projection_plot <- renderPlotly({
    req(input$run_projection)
    
    original_data <- as.data.frame(filedata())
    smote_data <- NULL
    class_col <- NULL
    
    if (isTRUE(runSMOTENC()) && !is.null(SMOTENC_Data()) && !is.null(input$target_nc)) {
      smote_data <- SMOTENC_Data()$syn_data
      class_col <- input$target_nc
    } else if (isTRUE(runSMOTE()) && !is.null(SMOTE_Data()) && !is.null(input$target)) {
      smote_data <- SMOTE_Data()$syn_data
      class_col <- input$target
    } else if (isTRUE(runBorderlineSMOTE()) && !is.null(BorderlineSMOTE_Data()) && !is.null(input$target_borderline)) {
      smote_data <- BorderlineSMOTE_Data()$syn_data
      class_col <- input$target_borderline
    } else if (isTRUE(runSVM_SMOTE()) && !is.null(SVM_SMOTE_Data()) && !is.null(input$target_svm)) {
      smote_data <- SVM_SMOTE_Data()$syn_data
      class_col <- input$target_svm
    } else if (isTRUE(runADASYN()) && !is.null(ADASYN_Data()) && !is.null(input$target_adasyn)) {
      smote_data <- ADASYN_Data()$syn_data
      class_col <- input$target_adasyn
    } else if (isTRUE(runRU()) && !is.null(RU_Data()) && !is.null(input$target_ru)) {
      smote_data <- RU_Data()$syn_data
      class_col <- input$target_ru
    } else if (isTRUE(runTomek()) && !is.null(Tomek_Data()) && !is.null(input$target_tomek)) {
      smote_data <- Tomek_Data()$removed_data
      class_col <- input$target_tomek
    } else if (isTRUE(runNearMiss()) && !is.null(NearMiss_Data()) && !is.null(input$target_nearmiss)) {
      smote_data <- NearMiss_Data()$removed_data
      class_col <- input$target_nearmiss
    } else if (isTRUE(runENN()) && !is.null(ENN_Data()) && !is.null(input$target_enn)) {
      smote_data <- ENN_Data()$removed_data
      class_col <- input$target_enn
    } else if (isTRUE(runOSS()) && !is.null(OSS_Data()) && !is.null(input$target_oss)) {
      smote_data <- OSS_Data()$removed_data
      class_col <- input$target_oss  
    } else if (isTRUE(runRD()) && !is.null(RD_Data()) && !is.null(input$target_rd)) {
      smote_data <- RD_Data()$removed_data
      class_col <- input$target_rd
    
     ##BLOCO ADICIONADO EM 10/07/2025
      
    } else if (isTRUE(runSBC()) && !is.null(SBC_Data()) && !is.null(input$target_sbc)) {
      smote_data <- SBC_Data()$removed_data
      class_col <- input$target_sbc
      
     ##BLOCO ADICIONADO EM 10/07/2025
      
      
      ######BLOCO ADICIONADO EM 20/06/2025
      
    } 
    
    ######BLOCO MODIFICADO EM 28/06/2025
    else if (isTRUE(runSMOTETL()) && !is.null(SMOTETL_Data()) && !is.null(input$target_tl)) {
      class_col <- input$target_tl
      added_data <- SMOTETL_Data()$syn_data
      removed_data <- SMOTETL_Data()$removed_data
      
      # Protegendo se NULL ou vazio
      if (is.null(added_data) || nrow(added_data) == 0) {
        added_data <- NULL
      } else {
        if ("class" %in% names(added_data) || class_col %in% names(added_data)) {
          # Renomeando se precisar
          if (class_col %in% names(added_data)) {
            names(added_data)[names(added_data) == class_col] <- "class"
          }
          added_data$class <- paste(added_data$class, "Synthetic")
        }
      }
      
      if (is.null(removed_data) || nrow(removed_data) == 0) {
        removed_data <- NULL
      } else {
        if ("class" %in% names(removed_data) || class_col %in% names(removed_data)) {
          if (class_col %in% names(removed_data)) {
            names(removed_data)[names(removed_data) == class_col] <- "class"
          }
          removed_data$class <- paste(removed_data$class, " Removed") #ALTERADO EM 29/06/2025
        }
      }
      
      smote_data <- dplyr::bind_rows(added_data, removed_data)
      
      if (nrow(smote_data) == 0)  # {
        return(NULL)
      }
   # }
    
    
      ######BLOCO MODIFICADO EM 28/06/2025
    
    
    
      ######BLOCO ADICIONADO EM 20/06/2025
      
      
      ##########BLOCO ADICIONADO EM 03/07/2025
    
      else if (isTRUE(runSMOTEENN()) && !is.null(SMOTEENN_Data()) && !is.null(input$target_enn2)) {
        class_col <- input$target_enn2
        added_data <- SMOTEENN_Data()$syn_data
        removed_data <- SMOTEENN_Data()$removed_data
        
        # Protegendo se NULL ou vazio
        
        if (is.null(added_data) || nrow(added_data) == 0) {
          added_data <- NULL
        } else {
          if (class_col %in% names(added_data)) {
            names(added_data)[names(added_data) == class_col] <- "class"
          }
          added_data$class <- paste(added_data$class, "Synthetic")
        }
        
        if (is.null(removed_data) || nrow(removed_data) == 0) {
          removed_data <- NULL
        } else {
          if (class_col %in% names(removed_data)) {
            names(removed_data)[names(removed_data) == class_col] <- "class"
          }
          removed_data$class <- paste(removed_data$class, " Removed")
        }
        
        smote_data <- dplyr::bind_rows(added_data, removed_data)
        
        if (nrow(smote_data) == 0) #  {
          return(NULL)
        }
   #   }
     
      ##########BLOCO ADICIONADO EM 03/07/2025
  
        
  
        ########## BLOCO SMOTE-IPF EM 13/07/2025
        
        else if (isTRUE(runSMOTEIPF()) && !is.null(SMOTEIPF_Data()) && !is.null(input$target_ipf)) {
          class_col <- input$target_ipf
          added_data <- SMOTEIPF_Data()$syn_data
          removed_data <- SMOTEIPF_Data()$removed_data
          
          # Protegendo se NULL ou vazio
          
          if (is.null(added_data) || nrow(added_data) == 0) {
            added_data <- NULL
          } else {
            if (class_col %in% names(added_data)) {
              names(added_data)[names(added_data) == class_col] <- "class"
            }
            added_data$class <- paste(added_data$class, "Synthetic")
          }
          
          if (is.null(removed_data) || nrow(removed_data) == 0) {
            removed_data <- NULL
          } else {
            if (class_col %in% names(removed_data)) {
              names(removed_data)[names(removed_data) == class_col] <- "class"
            }
            removed_data$class <- paste(removed_data$class, " Removed")
          }
          
          smote_data <- dplyr::bind_rows(added_data, removed_data)
          
          if (nrow(smote_data) == 0) # {
            return(NULL)
          }
     #   }
        
        ########## FIM BLOCO SMOTE-IPF EM 13/07/2025
       
          ########## BLOCO SPIDER EM 14/07/2025
          
          else if (isTRUE(runSPIDER()) && !is.null(SPIDER_Data()) && !is.null(input$target_spider)) {
            class_col <- input$target_spider
            added_data <- SPIDER_Data()$syn_data
            removed_data <- SPIDER_Data()$removed_data
            
            # Protegendo se NULL ou vazio
            
            if (is.null(added_data) || nrow(added_data) == 0) {
              added_data <- NULL
            } else {
              if (class_col %in% names(added_data)) {
                names(added_data)[names(added_data) == class_col] <- "class"
              }
              added_data$class <- paste(added_data$class, "Synthetic")
            }
            
            if (is.null(removed_data) || nrow(removed_data) == 0) {
              removed_data <- NULL
            } else {
              if (class_col %in% names(removed_data)) {
                names(removed_data)[names(removed_data) == class_col] <- "class"
              }
              removed_data$class <- paste(removed_data$class, " Removed")
            }
            
            smote_data <- dplyr::bind_rows(added_data, removed_data)
            
            if (nrow(smote_data) == 0) {
              return(NULL)
            }
         # }
          
          ########## FIM BLOCO SPIDER EM 14/07/2025
     
      
    } else {
      return(NULL)
    }
    
    if (is.null(smote_data) || !(class_col %in% names(original_data))) return(NULL)
    
    names(original_data)[names(original_data) == class_col] <- "class"
    names(smote_data)[names(smote_data) == class_col] <- "class"
    
    #############BLOCO MODIFICADO EM 20/06/2025
    
    #smote_data$class <- paste(smote_data$class, "Synthetic")
    
  ##  if (!isTRUE(runSMOTETL()) && #MODIFICADO EM 03/07/2025
    
  ##  if (!isTRUE(runSMOTETL()) && !isTRUE(runSMOTEENN()) &&  #MODIFICADO EM 03/07/2025
        
      ###  if (!isTRUE(runSMOTETL()) && !isTRUE(runSMOTEENN()) && !isTRUE(runSMOTEIPF()) &&    #MODIFICADO EM 13/07/2025
    if (!isTRUE(runSMOTETL()) && !isTRUE(runSMOTEENN()) && !isTRUE(runSMOTEIPF()) && !isTRUE(runSPIDER()) &&    #MODIFICADO EM 14/07/2025    
        (isTRUE(runSMOTE()) || isTRUE(runSMOTENC()) || isTRUE(runBorderlineSMOTE()) ||
         isTRUE(runSVM_SMOTE()) || isTRUE(runADASYN()) || isTRUE(runRU()))) {
      smote_data$class <- paste(smote_data$class, "Synthetic")
    }
    
    
    #############BLOCO MODIFICADO EM 20/06/2025
    
    
    
    #  if (isTRUE(runTomek())) {
    #    smote_data$class <- paste(smote_data$class, "Removida")
    #  } else if (isTRUE(runNearMiss())) {
    #    smote_data$class <- paste(smote_data$class, "Removida")
    #  } else if (isTRUE(runENN())) {
    #    smote_data$class <- paste(smote_data$class, "Removida") 
    #  } else if (isTRUE(runOSS())) {
    #    smote_data$class <- paste(smote_data$class, "Removida") 
    #  } else if (isTRUE(runRD())) {
    #    smote_data$class <- paste(smote_data$class, "Removida")   
    #  }
    
    #############BLOCO MODIFICADO EM 20/06/2025
    
    if (isTRUE(runTomek())) {
    
     smote_data$class <- paste(smote_data$class, "Removed") #ALTERADO EM 29/06/2025
     
    } else if (isTRUE(runNearMiss())) {
   
      smote_data$class <- paste(smote_data$class, "Removed") #ALTERADO EM 29/06/2025
    } else if (isTRUE(runENN())) { 
   
      smote_data$class <- paste(smote_data$class, "Removed") #ALTERADO EM 29/06/2025
    } else if (isTRUE(runOSS())) { 
    
      smote_data$class <- paste(smote_data$class, "Removed") #ALTERADO EM 29/06/2025
    } else if (isTRUE(runRD())) { 
     
      smote_data$class <- paste(smote_data$class, "Removed")   #ALTERADO EM 29/06/2025
    } else if (isTRUE(runSBC())) { 
      
      smote_data$class <- paste(smote_data$class, "Removed")   #ADICIONADO EM 10/07/2025
    }
    
    
    #############BLOCO MODIFICADO EM 20/06/2025
    
    
    
    
    smote_data$fake_cat <- NULL
    
    feature_cols <- intersect(names(original_data), names(smote_data))
    original_data <- original_data[, feature_cols, drop = FALSE]
    smote_data <- smote_data[, feature_cols, drop = FALSE]
    
    original_data$Tipo <- "Original"
    
    
    
    smote_data$Tipo <- "Synthetic"  
    
    
    
    
    dataset <- dplyr::bind_rows(original_data, smote_data)
    validate(need("class" %in% colnames(dataset), "A coluna 'class' não está presente nos dados!"))
    dataset$class <- as.factor(dataset$class)
    
    original_dataset <- dataset[!grepl("Synthetic", dataset$class), ]
    synthetic_dataset <- dataset[grepl("Synthetic", dataset$class), ]
    if (nrow(synthetic_dataset) > 0) {
      synthetic_dataset$OriginalClass <- gsub(" Synthetic", "", synthetic_dataset$class)
    }
    
    compute_ellipse <- function(data, level = 0.95) {
      if (nrow(data) < 2) return(NULL)
      data_numeric <- data[, sapply(data, is.numeric), drop = FALSE]
      if (ncol(data_numeric) < 2) return(NULL)
      mean_vals <- colMeans(data_numeric, na.rm = TRUE)
      cov_matrix <- cov(data_numeric, use = "complete.obs")
      ellipse_pts <- data.frame(ellipse::ellipse(cov_matrix, centre = mean_vals, level = level))
      return(ellipse_pts)
    }
    
    explained_var <- c(NA, NA)
    xlab <- "Componente 1"
    ylab <- "Componente 2"
    
    if (input$projection == "PCA") {
      pca <- prcomp(dataset[, sapply(dataset, is.numeric)], center = TRUE, scale. = TRUE)
      explained_var <- round(100 * summary(pca)$importance[2, c(input$pc1_plot, input$pc2_plot)], 1)
      scores <- data.frame(PC1 = pca$x[, input$pc1_plot], PC2 = pca$x[, input$pc2_plot], Class = dataset$class)
      xlab <- paste0("PC", input$pc1_plot, " (Expvar: ", explained_var[1], "%)")
      ylab <- paste0("PC", input$pc2_plot, " (Expvar: ", explained_var[2], "%)")
    } else if (input$projection == "Robust PCA") {
      pca_robust <- rrcov::PcaHubert(dataset[, sapply(dataset, is.numeric)], k = 2)
      eigenvalues <- pca_robust@eigenvalues
      total_var <- sum(eigenvalues)
      explained_var <- round(100 * eigenvalues[c(input$pc1_plot, input$pc2_plot)] / total_var, 1)
      scores <- data.frame(PC1 = pca_robust@scores[, input$pc1_plot], PC2 = pca_robust@scores[, input$pc2_plot], Class = dataset$class)
      xlab <- paste0("RPC", input$pc1_plot, " (Expvar: ", explained_var[1], "%)")
      ylab <- paste0("RPC", input$pc2_plot, " (Expvar: ", explained_var[2], "%)")
    } else if (input$projection == "t-SNE") {
      
      
      
      #########BLOCO MODIFICADO EM 09/06/2025 - PROBLEMA TSNE PARA RANDOM UPSAMPLING  
      #ACRÉSCIMO DE TOMEK LINKS EM 09/06/2025
      
      
      # dataset_unique <- dataset[!duplicated(dataset[, sapply(dataset, is.numeric)]), ]
      
      
      #if (input$projection == "t-SNE" && isTRUE(runRU())) {
      #  dataset_unique <- dataset
      #  } else {
      
      if (input$projection == "t-SNE" && (isTRUE(runRU()) || isTRUE(runTomek()) ||
                                          isTRUE(runNearMiss()) ||  isTRUE(runENN()) ||  isTRUE(runOSS())  ||  isTRUE(runRD()) || isTRUE(runSBC())
                                        ##  ||  isTRUE(runSMOTETL())  )) { #MODIFICADO EM 03/07/2025
                                      ##  ||  isTRUE(runSMOTETL()) || isTRUE(runSMOTEENN())   )) { #MODIFICADO EM 13/07/2025
                                    ##  ||  isTRUE(runSMOTETL()) || isTRUE(runSMOTEENN())   || isTRUE(runSMOTEIPF())      )) { #MODIFICADO EM 14/07/2025
                                    ||  isTRUE(runSMOTETL()) || isTRUE(runSMOTEENN())   || isTRUE(runSMOTEIPF()) || isTRUE(runSPIDER())      )) {
        dataset_unique <- dataset
      } else {
        
        dataset_unique <- dataset[!duplicated(dataset[, sapply(dataset, is.numeric)]), ]
      }
      
      #ACRÉSCIMO DE TOMEK LINKS EM 09/06/2025
      #########BLOCO MODIFICADO EM 09/06/2025 - PROBLEMA TSNE PARA RANDOM UPSAMPLING  
      
      
      
      if (nrow(dataset_unique) < 2) {
        showNotification("Error: Not enough samples for t-SNE!", type = "error")
        return()
      }
      
      
      tsne_input <- as.matrix(dataset_unique[, sapply(dataset_unique, is.numeric)])
      max_perplexity <- floor((nrow(tsne_input) - 1) / 3)
      perplexity_value <- min(input$tsne_perplexity, max_perplexity)
      tsne_result <- Rtsne(tsne_input, dims = input$tsne_dims, perplexity = perplexity_value, max_iter = input$tsne_iter, theta = input$tsne_theta, verbose = TRUE, check_duplicates = FALSE)
      if (!is.null(tsne_result$Y) && nrow(tsne_result$Y) > 0) {
        scores <- data.frame(PC1 = tsne_result$Y[, 1], PC2 = tsne_result$Y[, 2], Class = dataset_unique$class)
        xlab <- "Dimension 1"
        ylab <- "Dimension 2"
      } else {
        showNotification("Error: t-SNE failed to generate projection!", type = "error")
        return()
      }
    }
    
    scores$SampleID <- seq_len(nrow(scores))
    ellipse_list <- lapply(unique(original_dataset$class), function(cls) {
      subset_data <- scores[scores$Class == cls, c("PC1", "PC2"), drop = FALSE]
      ellipse_df <- compute_ellipse(subset_data, level = as.numeric(input$hot_confidence))
      if (!is.null(ellipse_df)) ellipse_df$Class <- cls
      return(ellipse_df)
    })
    ellipse_df <- do.call(rbind, ellipse_list)
    
    synthetic_scores <- scores[scores$Class %in% synthetic_dataset$class, ]
 ##   original_classes <- sort(unique(as.character(original_dataset$class))) #MODIFICADO 14/07/2025
    
   original_classes <- sort(unique(gsub(" Synthetic$| Removed$", "", as.character(scores$Class))))

    
    all_classes <- unique(as.character(scores$Class))
    synthetic_classes <- setdiff(all_classes, original_classes)
    ordered_classes <- c(original_classes, sort(synthetic_classes))
    
  ##  scores$Class <- factor(scores$Class, levels = ordered_classes) 
    
    base_palette <- RColorBrewer::brewer.pal(9, "Set1")
    paleta <- setNames(rep(base_palette, length.out = length(ordered_classes)), ordered_classes)
    
    p <- plot_ly()
    
    for (cls in ordered_classes) {
      if (!is.null(ellipse_df)) {
        df_ell <- ellipse_df[ellipse_df$Class == cls, ]
        if (nrow(df_ell) > 0) {
          p <- p %>% add_trace(data = df_ell, x = ~PC1, y = ~PC2, type = 'scatter', mode = 'lines',
                               name = cls, legendgroup = paste0(cls, "_elipse"), showlegend = TRUE,
                               line = list(width = 1, dash = "solid", color = paleta[cls]),
                               hoverinfo = "skip")
        }
      }
    }
    
    for (cls in ordered_classes) {
      df_cls <- scores[scores$Class == cls, ]
      if (cls %in% synthetic_dataset$class) {
        marker_style <- list(size = 5, symbol = "diamond", opacity = 0.9, color = paleta[cls])
      } else {
        marker_style <- list(size = 6, symbol = "circle", opacity = 0.75, color = paleta[cls])
      }
      p <- p %>% add_trace(data = df_cls, x = ~PC1, y = ~PC2,
                           type = 'scatter', mode = 'markers',
                           name = cls, legendgroup = paste0(cls, "_pontos"), showlegend = TRUE,
                           marker = marker_style,
                           text = ~paste("Class:", Class, "<br>SampleID:", SampleID),
                           hoverinfo = "text")
    }
    
    for (cls in ordered_classes) {
      df_cls <- scores[scores$Class == cls, ]
      p <- p %>% add_trace(data = df_cls, x = ~PC1, y = ~PC2,
                           type = 'scatter', mode = 'text',
                           text = ~SampleID, textposition = "top center",
                          name = cls, legendgroup = paste0(cls, "_labels"), showlegend = TRUE, 
                          textfont = list(family = "Arial", size = 10, color = "gray30"),
                           hoverinfo = "none")
    }
    
    p <- layout(p,
                title = list(text = paste("Projection", input$projection, "from Samples"),
                             font = list(size = 16, family = "Arial, bold")),
                xaxis = list(title = xlab, showgrid = FALSE, zeroline = FALSE, showline = TRUE, linewidth = 1.2),
                yaxis = list(title = ylab, showgrid = FALSE, zeroline = FALSE, showline = TRUE, linewidth = 1.2),
                legend = list(title = list(text = ""), font = list(size = 12, family = "Arial")),
                plot_bgcolor = "white")
    
    return(p)
      
  })
  
  ##ACRÉSCIMO DE SMOTE NC EM 28/05/2025
  
  ##ACRÉSCIMO DE BORDERLINE SMOTE EM 05/06/2025 
  
  ##ACRÉSCIMO DE SVM SMOTE EM 06/06/2025
  
  ##ACRÉSCIMO DE ADASYN EM 08/06/2025
  
  ##ACRÉSCIMO DE RANDOM UPSAMPLING EM 09/06/2025
  
  ##ACRÉSCIMO DE TOMEK LINKS EM 09/06/2025 
  
  ##ACRÉSCIMO DE NEAR MISS EM 16/06/2025
  
  ##ACRÉSCIMO DE ENN EM 17/06/2025
  
  ##ACRÉSCIMO DE OSS EM 18/06/2025
  
  ##ACRÉSCIMO DE RANDOM DOWNSAMPLING EM 18/06/2025
  
  ##ACRÉSCIMO DE SMOTE TL EM 20/06/2025
  
  ##ACRÉSCIMO DE SMOTE ENN EM 03/07/2025
  
  ##ACRÉSCIMO DE SBC EM 10/07/2025
  
  ##ACRÉSCIMO DE SMOTE IPF EM 13/07/2025
  
  ##ACRÉSCIMO DE SPIDER EM 14/07/2025
  
  
  ##visualização 3D - ACRÉSCIMO EM 22/03/2025
  
  # Visualização: 3D Projection refinada com múltiplas elipses
  
  ##ACRÉSCIMO DE LABELS NAS AMOSTRAS NA LEGENDA EM 12/04/2025
  
  
  ##ACRÉSCIMO DE SMOTE NC EM 28/05/2025
  
  ##ACRÉSCIMO DE BORDERLINE SMOTE EM 05/06/2025
  
  ##ACRÉSCIMO DE SVM SMOTE EM 06/06/2025
  
  ##ACRÉSCIMO DE ADASYN EM 08/06/2025
  
  ##ACRÉSCIMO DE RANDOM UPSAMPLING EM 09/06/2025
  
  ##ACRÉSCIMO DE TOMEK LINKS EM 10/06/2025
  
  ##ACRÉSCIMO DE NEAR MISS EM 16/06/2025
  
  ##ACRÉSCIMO DE ENN EM 17/06/2025
  
  ##ACRÉSCIMO DE OSS EM 18/06/2025
  
  ##ACRÉSCIMO DE RANDOM DOWNSAMPLING EM 18/06/2025
  
  ##ACRÉSCIMO DE SMOTE TL EM 20/06/2025
  
  ##ACRÉSCIMO DE SMOTE ENN EM 03/07/2025
  
  ##ACRÉSCIMO DE SBC EM 10/07/2025
  
  ##ACRÉSCIMO DE SMOTE IPF EM 13/07/2025
  
  ##ACRÉSCIMO DE SPIDER EM 14/07/2025
  
  
  output$projection_plot_3d <- renderPlotly({
    req(input$run_projection_3d)
    
    original_data <- as.data.frame(filedata())
    smote_data <- NULL
    class_col <- NULL
    
    if (isTRUE(runSMOTENC()) && !is.null(SMOTENC_Data()) && !is.null(input$target_nc)) {
      smote_data <- SMOTENC_Data()$syn_data
      class_col <- input$target_nc
    } else if (isTRUE(runSMOTE()) && !is.null(SMOTE_Data()) && !is.null(input$target)) {
      smote_data <- SMOTE_Data()$syn_data
      class_col <- input$target
    } else if (isTRUE(runBorderlineSMOTE()) && !is.null(BorderlineSMOTE_Data()) && !is.null(input$target_borderline)) {
      smote_data <- BorderlineSMOTE_Data()$syn_data
      class_col <- input$target_borderline
    } else if (isTRUE(runSVM_SMOTE()) && !is.null(SVM_SMOTE_Data()) && !is.null(input$target_svm)) {
      smote_data <- SVM_SMOTE_Data()$syn_data
      class_col <- input$target_svm
    } else if (isTRUE(runADASYN()) && !is.null(ADASYN_Data()) && !is.null(input$target_adasyn)) {
      smote_data <- ADASYN_Data()$syn_data
      class_col <- input$target_adasyn
    } else if (isTRUE(runRU()) && !is.null(RU_Data()) && !is.null(input$target_ru)) {
      smote_data <- RU_Data()$syn_data
      class_col <- input$target_ru 
    } else if (isTRUE(runTomek()) && !is.null(Tomek_Data()) && !is.null(input$target_tomek)) {
      smote_data <- Tomek_Data()$removed_data
      class_col <- input$target_tomek 
    } else if (isTRUE(runNearMiss()) && !is.null(NearMiss_Data()) && !is.null(input$target_nearmiss)) {
      smote_data <- NearMiss_Data()$removed_data
      class_col <- input$target_nearmiss
    } else if (isTRUE(runENN()) && !is.null(ENN_Data()) && !is.null(input$target_enn)) {
      smote_data <- ENN_Data()$removed_data
      class_col <- input$target_enn  
    } else if (isTRUE(runOSS()) && !is.null(OSS_Data()) && !is.null(input$target_oss)) {
      smote_data <- OSS_Data()$removed_data
      class_col <- input$target_oss 
    } else if (isTRUE(runRD()) && !is.null(RD_Data()) && !is.null(input$target_rd)) {
      smote_data <- RD_Data()$removed_data
      class_col <- input$target_rd  
      
      
    ###BLOCO ADICIONADO EM 10/07/2025  
    } else if (isTRUE(runSBC()) && !is.null(SBC_Data()) && !is.null(input$target_sbc)) {
      smote_data <- SBC_Data()$removed_data
      class_col <- input$target_sbc
      
      
    ###BLOCO ADICIONADO EM 10/07/2025  
      
      
      ######BLOCO ADICIONADO EM 20/06/2025
      
    } 
    
    ################### BLOCO MODIFICADO EM 28/06/2025
    else if (isTRUE(runSMOTETL()) && !is.null(SMOTETL_Data()) && !is.null(input$target_tl)) {
      class_col <- input$target_tl
      added_data <- SMOTETL_Data()$syn_data
      removed_data <- SMOTETL_Data()$removed_data
      
      # Protege se NULL ou vazio
      if (is.null(added_data) || nrow(added_data) == 0) {
        added_data <- NULL
      } else {
        if ("class" %in% names(added_data) || class_col %in% names(added_data)) {
          # Renomeia se precisar
          if (class_col %in% names(added_data)) {
            names(added_data)[names(added_data) == class_col] <- "class"
          }
          added_data$class <- paste(added_data$class, "Synthetic")
        }
      }
      
      if (is.null(removed_data) || nrow(removed_data) == 0) {
        removed_data <- NULL
      } else {
        if ("class" %in% names(removed_data) || class_col %in% names(removed_data)) {
          if (class_col %in% names(removed_data)) {
            names(removed_data)[names(removed_data) == class_col] <- "class"
          }
          removed_data$class <- paste(removed_data$class, "Removed") #ALTERADO EM 29/06/2025
        }
      }
      
      smote_data <- dplyr::bind_rows(added_data, removed_data)
      
      ####ADICIONADO EM 03/07/2025
      if (nrow(smote_data) == 0) {
        return(NULL)
      }
    } 
    ####ADICIONADO EM 03/07/2025
      
      ############ BLOCO ADICIONADO EM 03/07/2025
      
      else if (isTRUE(runSMOTEENN()) && !is.null(SMOTEENN_Data()) && !is.null(input$target_enn2)) {
        class_col <- input$target_enn2
        added_data <- SMOTEENN_Data()$syn_data
        removed_data <- SMOTEENN_Data()$removed_data
        
        # Protege se NULL ou vazio
        if (is.null(added_data) || nrow(added_data) == 0) {
          added_data <- NULL
        } else {
          if (class_col %in% names(added_data)) {
            names(added_data)[names(added_data) == class_col] <- "class"
          }
          added_data$class <- paste(added_data$class, "Synthetic")
        }
        
        if (is.null(removed_data) || nrow(removed_data) == 0) {
          removed_data <- NULL
        } else {
          if (class_col %in% names(removed_data)) {
            names(removed_data)[names(removed_data) == class_col] <- "class"
          }
          #removed_data$class <- paste(removed_data$class, " Removida")
          removed_data$class <- paste(removed_data$class, "Removed")
        }
        
        smote_data <- dplyr::bind_rows(added_data, removed_data)
      
      
      ############ BLOCO ADICIONADO EM 03/07/2025
      
  
  
        ############ BLOCO SMOTE-IPF EM 13/07/2025
        
      }
        
        else if (isTRUE(runSMOTEIPF()) && !is.null(SMOTEIPF_Data()) && !is.null(input$target_ipf)) {
          class_col <- input$target_ipf
          added_data <- SMOTEIPF_Data()$syn_data
          removed_data <- SMOTEIPF_Data()$removed_data
          
          # Protege se NULL ou vazio
          if (is.null(added_data) || nrow(added_data) == 0) {
            added_data <- NULL
          } else {
            if (class_col %in% names(added_data)) {
              names(added_data)[names(added_data) == class_col] <- "class"
            }
            added_data$class <- paste(added_data$class, "Synthetic")
          }
          
          if (is.null(removed_data) || nrow(removed_data) == 0) {
            removed_data <- NULL
          } else {
            if (class_col %in% names(removed_data)) {
              names(removed_data)[names(removed_data) == class_col] <- "class"
            }
            removed_data$class <- paste(removed_data$class, "Removed")
          }
          
          smote_data <- dplyr::bind_rows(added_data, removed_data)
    #    }
        
        ############ FIM BLOCO SMOTE-IPF EM 13/07/2025
        
       
          ########## BLOCO SPIDER EM 14/07/2025
          
         } else if (isTRUE(runSPIDER()) && !is.null(SPIDER_Data()) && !is.null(input$target_spider)) {
            class_col <- input$target_spider
            added_data <- SPIDER_Data()$syn_data
            removed_data <- SPIDER_Data()$removed_data
            
            # Protegendo se NULL ou vazio
            
            if (is.null(added_data) || nrow(added_data) == 0) {
              added_data <- NULL
            } else {
              if (class_col %in% names(added_data)) {
                names(added_data)[names(added_data) == class_col] <- "class"
              }
              added_data$class <- paste(added_data$class, "Synthetic")
            }
            
            if (is.null(removed_data) || nrow(removed_data) == 0) {
              removed_data <- NULL
            } else {
              if (class_col %in% names(removed_data)) {
                names(removed_data)[names(removed_data) == class_col] <- "class"
              }
              #removed_data$class <- paste(removed_data$class, " Removida")
              removed_data$class <- paste(removed_data$class, "Removed")
            }
            
            smote_data <- dplyr::bind_rows(added_data, removed_data)
            
            
            
            ########## FIM BLOCO SPIDER EM 14/07/2025  
          
         
          
      
      if (nrow(smote_data) == 0)   {
        return(NULL)
      }
  #  }
    
    
    
    ################### BLOCO MODIFICADO EM 28/06/2025
    
    
      
      ######BLOCO ADICIONADO EM 20/06/2025
      
      
      
    } 
  else {
      showNotification("No data found for plotting.", type = "error")
      return(NULL)
    }
    
    if (is.null(smote_data) || !(class_col %in% names(original_data))) return(NULL)
    
    names(original_data)[names(original_data) == class_col] <- "class"
    names(smote_data)[names(smote_data) == class_col] <- "class"
    
    
    
    #############BLOCO MODIFICADO EM 20/06/2025
    
    #smote_data$class <- paste(smote_data$class, "Synthetic")
    
  ##  if (!isTRUE(runSMOTETL()) && #MODIFICADO EM 03/07/2025
        
      ##  if (!isTRUE(runSMOTETL()) && !isTRUE(runSMOTEENN()) &&  #MODIFICADO EM 03/07/2025
  ##  if (!isTRUE(runSMOTETL()) && !isTRUE(runSMOTEENN()) && !isTRUE(runSMOTEIPF()) &&    #MODIFICADO EM 13/07/2025
    if (!isTRUE(runSMOTETL()) && !isTRUE(runSMOTEENN()) && !isTRUE(runSMOTEIPF()) && !isTRUE(runSPIDER()) &&    #MODIFICADO EM 14/07/2025    
        
        (isTRUE(runSMOTE()) || isTRUE(runSMOTENC()) || isTRUE(runBorderlineSMOTE()) ||
         isTRUE(runSVM_SMOTE()) || isTRUE(runADASYN()) || isTRUE(runRU()))) {
      smote_data$class <- paste(smote_data$class, "Synthetic")
    }
    
    
    #############BLOCO MODIFICADO EM 20/06/2025
    
    
    
    
    #  if (isTRUE(runTomek())) {
    #   smote_data$class <- paste(smote_data$class, "Removida")
    #  } else if (isTRUE(runNearMiss())) {
    #    smote_data$class <- paste(smote_data$class, "Removida")
    #  } else if (isTRUE(runENN())) {
    #    smote_data$class <- paste(smote_data$class, "Removida") 
    #  } else if (isTRUE(runOSS())) {
    #    smote_data$class <- paste(smote_data$class, "Removida")
    #  } else if (isTRUE(runRD())) {
    #    smote_data$class <- paste(smote_data$class, "Removida")
    #  }
    
    
    
    
    #############BLOCO MODIFICADO EM 20/06/2025
    
    if (isTRUE(runTomek())) {
      smote_data$class <- paste(smote_data$class, "Removed")
    } else if (isTRUE(runNearMiss())) {
      smote_data$class <- paste(smote_data$class, "Removed")
    } else if (isTRUE(runENN())) {
      smote_data$class <- paste(smote_data$class, "Removed") 
    } else if (isTRUE(runOSS())) {
      smote_data$class <- paste(smote_data$class, "Removed") 
   } else if (isTRUE(runRD())) {
      smote_data$class <- paste(smote_data$class, "Removed") 
   } else if (isTRUE(runSBC())) { 
      smote_data$class <- paste(smote_data$class, "Removed")   #ADICIONADO EM 10/07/2025
     
    }
    
    
    #############BLOCO MODIFICADO EM 20/06/2025
    
    
    
    smote_data$fake_cat <- NULL
    
    feature_cols <- intersect(names(original_data), names(smote_data))
    original_data <- original_data[, feature_cols, drop = FALSE]
    smote_data <- smote_data[, feature_cols, drop = FALSE]
    
    original_data$Tipo <- "Original"
    smote_data$Tipo <- "Synthetic"
    
    dataset <- dplyr::bind_rows(original_data, smote_data)
    validate(need("class" %in% colnames(dataset), "A coluna 'class' não está presente nos dados!"))
    dataset$class <- as.factor(dataset$class)
    
    compute_ellipse_3d <- function(data, level = 0.95) {
      if (nrow(data) < 5) return(NULL)
      data_numeric <- data[, sapply(data, is.numeric), drop = FALSE]
      if (ncol(data_numeric) < 3) return(NULL)
      
      mean_vals <- colMeans(data_numeric, na.rm = TRUE)
      cov_matrix <- cov(data_numeric, use = "complete.obs")
      
      ellipse_PC1_PC2 <- ellipse::ellipse(cov_matrix[1:2, 1:2], level = level, centre = mean_vals[1:2])
      ellipse_PC1_PC3 <- ellipse::ellipse(cov_matrix[c(1,3), c(1,3)], level = level, centre = mean_vals[c(1,3)])
      ellipse_PC2_PC3 <- ellipse::ellipse(cov_matrix[2:3, 2:3], level = level, centre = mean_vals[2:3])
      
      return(list(
        PC1 = data.frame(PC1 = ellipse_PC1_PC2[,1], PC2 = ellipse_PC1_PC2[,2], PC3 = mean_vals[3]),
        PC2 = data.frame(PC1 = ellipse_PC1_PC3[,1], PC2 = mean_vals[2], PC3 = ellipse_PC1_PC3[,2]),
        PC3 = data.frame(PC1 = mean_vals[1], PC2 = ellipse_PC2_PC3[,1], PC3 = ellipse_PC2_PC3[,2])
      ))
    }
    
    dataset_numeric <- dataset[, sapply(dataset, is.numeric), drop = FALSE]
    validate(need(ncol(dataset_numeric) >= 3, "Pelo menos 3 Variables numéricas são necessárias para 3D Projection."))
    
    explained_var <- c(NA, NA, NA)
    xlab <- ""
    ylab <- ""
    zlab <- ""
    
    if (input$projection_3d == "PCA") {
      pca <- prcomp(dataset_numeric, center = TRUE, scale. = TRUE)
      explained_var <- round(100 * summary(pca)$importance[2, c(input$pc1_plot_3d, input$pc2_plot_3d, input$pc3_plot_3d)], 1)
      scores <- data.frame(
        PC1 = pca$x[, input$pc1_plot_3d],
        PC2 = pca$x[, input$pc2_plot_3d],
        PC3 = pca$x[, input$pc3_plot_3d],
        Class = dataset$class
      )
      xlab <- paste0("PC", input$pc1_plot_3d, " (Expvar: ", explained_var[1], "%)")
      ylab <- paste0("PC", input$pc2_plot_3d, " (Expvar: ", explained_var[2], "%)")
      zlab <- paste0("PC", input$pc3_plot_3d, " (Expvar: ", explained_var[3], "%)")
    } else if (input$projection_3d == "Robust PCA") {
      pca_robust <- rrcov::PcaHubert(dataset_numeric, k = 3)
      eigenvalues <- pca_robust@eigenvalues
      total_var <- sum(eigenvalues)
      explained_var <- round(100 * eigenvalues[c(input$pc1_plot_3d, input$pc2_plot_3d, input$pc3_plot_3d)] / total_var, 1)
      scores <- data.frame(
        PC1 = pca_robust@scores[, input$pc1_plot_3d],
        PC2 = pca_robust@scores[, input$pc2_plot_3d],
        PC3 = pca_robust@scores[, input$pc3_plot_3d],
        Class = dataset$class
      )
      xlab <- paste0("RPC", input$pc1_plot_3d, " (Expvar: ", explained_var[1], "%)")
      ylab <- paste0("RPC", input$pc2_plot_3d, " (Expvar: ", explained_var[2], "%)")
      zlab <- paste0("RPC", input$pc3_plot_3d, " (Expvar: ", explained_var[3], "%)")
    } else if (input$projection_3d == "t-SNE") {
      
      
      #########BLOCO MODIFICADO EM 09/06/2025 - PROBLEMA TSNE PARA RANDOM UPSAMPLING  
      
      # dataset_unique <- dataset[!duplicated(dataset[, sapply(dataset, is.numeric)]), ]
      
      
      #   if (input$projection == "t-SNE" && isTRUE(runRU())) {
      #    dataset_unique <- dataset
      #  } else {
      
      
      if (input$projection_3d == "t-SNE" && (isTRUE(runRU()) || isTRUE(runTomek()) ||
                                             isTRUE(runNearMiss()) ||  isTRUE(runENN()) ||  isTRUE(runOSS())  ||  isTRUE(runRD()) || isTRUE(runSBC())
                                             #  ||  isTRUE(runSMOTETL())  )) { #MODIFICADO EM 03/07/2025
        ##  ||  isTRUE(runSMOTETL()) || isTRUE(runSMOTEENN())   )) { #MODIFICADO EM 13/07/2025
     ##   ||  isTRUE(runSMOTETL()) || isTRUE(runSMOTEENN())   || isTRUE(runSMOTEIPF())      )) { #MODIFICADO EM 14/07/2025
        ||  isTRUE(runSMOTETL()) || isTRUE(runSMOTEENN())   || isTRUE(runSMOTEIPF())  || isTRUE(runSPIDER())    )) {
        
        
        dataset_unique <- dataset 
        
      }else {
        
        
        dataset_unique <- dataset[!duplicated(dataset[, sapply(dataset, is.numeric)]), ]
      }
      
      #########BLOCO MODIFICADO EM 09/06/2025 - PROBLEMA TSNE PARA RANDOM UPSAMPLING  
      
      
      
      if (nrow(dataset_unique) < 5) {
        showNotification("Error: Not enough samples for t-SNE!", type = "error")
        return()
      }
      
      
      tsne_input <- as.matrix(dataset_unique[, sapply(dataset_unique, is.numeric)])
      max_perplexity <- floor((nrow(tsne_input) - 1) / 3)
      perplexity_value <- min(input$tsne_perplexity_3d, max_perplexity)
      tsne_result <- Rtsne(tsne_input, dims = 3, perplexity = perplexity_value,
                           max_iter = input$tsne_iter_3d, theta = input$tsne_theta_3d,
                           verbose = TRUE, check_duplicates = FALSE)
      if (!is.null(tsne_result$Y) && nrow(tsne_result$Y) > 0) {
        scores <- data.frame(
          PC1 = tsne_result$Y[, 1],
          PC2 = tsne_result$Y[, 2],
          PC3 = tsne_result$Y[, 3],
          Class = dataset_unique$class
        )
        xlab <- "Dimension 1"
        ylab <- "Dimension 2"
        zlab <- "Dimension 3"
      } else {
        showNotification("Error: t-SNE failed to generate projection!", type = "error")
        return()
      }
    } else {
      showNotification("Unrecognized projection method!", type = "error")
      return(NULL)
    }
    
    scores$OriginalClass <- ifelse(grepl("Synthetic", scores$Class), gsub(" Synthetic", "", scores$Class), as.character(scores$Class))
    scores$SampleID <- seq_len(nrow(scores))
    
    
    
    ordered_levels <- sort(unique(as.character(original_data$class)))
    
    #SBC ADICIONADO EM 10/07/2025
    
    if (isTRUE(runTomek()) || isTRUE(runNearMiss()) || isTRUE(runENN()) || isTRUE(runOSS()) || isTRUE(runRD()) || isTRUE(runSBC())) {
      sinteticas <- paste0(ordered_levels, " Removed") #ALTERADO EM 29/06/2025
  #  } else if (isTRUE(runSMOTETL())) { #MODIFICADA EM 03/07/2025
  ##  } else if (isTRUE(runSMOTETL()) || isTRUE(runSMOTEENN())   ) {
  ##  } else if (isTRUE(runSMOTETL()) || isTRUE(runSMOTEENN()) || isTRUE(runSMOTEIPF())   ) {  #MODIFICADA EM 13/07/2025
    } else if (isTRUE(runSMOTETL()) || isTRUE(runSMOTEENN()) || isTRUE(runSMOTEIPF()) || isTRUE(runSPIDER())   ) {  #MODIFICADA EM 14/07/2025
      sinteticas <- c(paste0(ordered_levels, " Synthetic"),
                      paste0(ordered_levels, " Removed")) #ALTERADO EM 29/06/2025
    } else if (
      isTRUE(runSMOTE()) || isTRUE(runSMOTENC()) || isTRUE(runBorderlineSMOTE()) ||
      isTRUE(runSVM_SMOTE()) || isTRUE(runADASYN()) || isTRUE(runRU())
    ) {
      sinteticas <- paste0(ordered_levels, " Synthetic")
    } else {
      sinteticas <- character(0)  
    }
    
    
    
    scores$Class <- factor(scores$Class, levels = c(ordered_levels, sinteticas))
    
    
    
    scores$OriginalClass <- factor(scores$OriginalClass, levels = ordered_levels)
    
    base_colors <- RColorBrewer::brewer.pal(max(3, length(ordered_levels)), "Set1")
    #  cores_classes <- unlist(lapply(seq_along(ordered_levels), function(i) {
    #    cls <- ordered_levels[i]; cor <- base_colors[i]
    #    setNames(c(cor, cor), c(cls, paste0(cls, " Synthetic")))
    
    
    
    
    cores_classes <- unlist(lapply(seq_along(ordered_levels), function(i) {
      cls <- ordered_levels[i]
      cor <- base_colors[i]
      setNames(c(cor, cor), c(cls, sinteticas[i]))
    }))
    
    
    
    ellipse_data <- data.frame()
    for (cls in ordered_levels) {
      subset_data <- scores[scores$OriginalClass == cls, c("PC1", "PC2", "PC3"), drop = FALSE]
      elps <- compute_ellipse_3d(subset_data, level = as.numeric(input$hot_confidence_3d))
      if (!is.null(elps)) {
        for (pc in names(elps)) {
          temp <- elps[[pc]]
          temp$EllipseGroup <- paste0(cls, "_", pc)
          temp$EllipseLabel <- paste0(cls, " ", pc)
          ellipse_data <- rbind(ellipse_data, temp)
        }
      }
    }
    
    p <- plot_ly()
    
    for (grp in unique(ellipse_data$EllipseGroup)) {
      cls_data <- ellipse_data[ellipse_data$EllipseGroup == grp, ]
      p <- add_trace(p, data = cls_data, x = ~PC1, y = ~PC2, z = ~PC3,
                     type = "scatter3d", mode = "lines",
                     name = unique(cls_data$EllipseLabel),
                     legendgroup = grp, showlegend = TRUE, 
                  # legendgroup = grp, showlegend = FALSE,
                     line = list(color = cores_classes[strsplit(grp, "_")[[1]][1]], width = 1.0))
    }
    
    for (cls in levels(scores$Class)) {
      
      
      data_cls <- scores[scores$Class == cls, ]
      
    
      
      symbol <- ifelse(grepl("Synthetic", cls), "diamond", "circle")
      p <- add_trace(p, data = data_cls, x = ~PC1, y = ~PC2, z = ~PC3,
                     type = "scatter3d", mode = "markers",
                   ##  name = cls, legendgroup = paste0(cls, "_pontos"), showlegend = TRUE, 
                   name = cls, legendgroup = paste0(cls, "_pontos"), showlegend = TRUE,
                     marker = list(size = 4, symbol = symbol, opacity = 0.9, color = cores_classes[cls]),
                     text = ~paste("Class:", Class, "<br>SampleID:", SampleID), hoverinfo = "text")
    }
    
    for (cls in levels(scores$Class)) {
      data_cls <- scores[scores$Class == cls, ]
      p <- add_trace(p, data = data_cls, x = ~PC1, y = ~PC2, z = ~PC3,
                     type = "scatter3d", mode = "text",
                     text = ~SampleID, textposition = "top center",
                     ## name = paste0(cls, " labels"), legendgroup = paste0(cls, "_labels"), showlegend = TRUE,
                     name = cls, legendgroup = paste0(cls, "_labels"), showlegend = TRUE,
                     textfont = list(family = "Arial", size = 10, color = "gray30"), hoverinfo = "none")
    }
    
    p <- layout(p,
                title = list(text = paste("3D Projection -", input$projection_3d),
                             font = list(size = 16, family = "Arial, bold")),
                scene = list(xaxis = list(title = xlab),
                             yaxis = list(title = ylab),
                             zaxis = list(title = zlab),
                             aspectmode = "manual",
                             domain = list(x = c(0, 1), y = c(0, 1)),
                             camera = list(eye = list(x = 1.7, y = 1.7, z = 1.7))),
                legend = list(title = list(text = ""), font = list(size = 12, family = "Arial"),
                              itemclick = TRUE, itemdoubleclick = TRUE, tracegroupgap = 0),
                plot_bgcolor = "white")
    
    return(p)
  })
  
  ##ACRÉSCIMO DE SMOTE NC EM 28/05/2025
  
  ##ACRÉSCIMO DE BORDERLINE SMOTE EM 05/06/2025
  
  ##ACRÉSCIMO DE SVM SMOTE EM 06/06/2025
  
  ##ACRÉSCIMO DE ADASYN EM 08/06/2025
  
  ##ACRÉSCIMO DE RANDOM UPSAMPLING EM 09/06/2025
  
  ##ACRÉSCIMO DE TOMEK LINKS EM 10/06/2025
  
  ##ACRÉSCIMO DE NEAR MISS EM 16/06/2025
  
  ##ACRÉSCIMO DE ENN EM 17/06/2025
  
  ##ACRÉSCIMO DE OSS EM 18/06/2025
  
  ##ACRÉSCIMO DE RANDOM DOWNSAMPLING EM 18/06/2025
  
  ##ACRÉSCIMO DE SMOTE TL EM 20/06/2025
  
  ##ACRÉSCIMO DE SMOTE ENN EM 03/07/2025

  ##ACRÉSCIMO DE SBC EM 10/07/2025

  ##ACRÉSCIMO DE SMOTE IPF EM 13/07/2025

  ##ACRÉSCIMO DE SPIDER EM 14/07/2025

  
  ###############ATUALIZADO EM 18/02/2025
  

  
  ##MODIFICAÇÃO DA ORDEM DE EXIBIÇÃO DAS CLASSES NAS LEGENDAS EM 01/04/2025
  
  # Visualização: Boxplot dos Scores do PCA
  ##  output$boxplot_scores <- renderPlotly({
  ##    req(smote_history$data)  # Garantindo que os dados acumulados (originais + sintéticos) estejam disponíveis
  
  # Obtendo o dataset atualizado (dados originais + todas as rodadas do SMOTE)
  ##    dataset <- smote_history$data
  
  # Ordenando os níveis da variável 'class' garantindo que:
  # - classes originais venham primeiro
  # - classes Synthetics venham logo após, na mesma ordem
  ##    original_classes <- unique(gsub(" Synthetic$", "", dataset$class[!grepl("Synthetic", dataset$class)]))
  ##    synthetic_classes <- paste(original_classes, "Synthetic")
  ##    ordered_levels <- c(original_classes, synthetic_classes)
  
  # Garantindo que 'class' seja fator com os níveis na ordem desejada
  ##    dataset$class <- factor(dataset$class, levels = ordered_levels)
  
  # Realizando PCA (removendo a coluna 'class')
  ##    pca <- prcomp(dataset[, -which(names(dataset) == "class")], center = TRUE, scale. = TRUE)
  
  # Extraindo os scores e adicionando a identificação da classe
  ##    scores <- as.data.frame(pca$x)
  ##    scores$Class <- dataset$class
  
  # Transformação dos dados para formato longo
  ##    melted_scores <- reshape2::melt(scores, id.vars = "Class")
  
  # Criando boxplot lado a lado com `boxmode = "group"`
  ##    plot_ly(melted_scores, x = ~variable, y = ~value, color = ~Class, type = "box",
  ##            boxpoints = "outliers") %>%
  ##      layout(title = "Boxplot of Scores (PCA)",
  ##             xaxis = list(title = "Principal Components"),
  ##             yaxis = list(title = "Score Values"),
  ##             boxmode = "group")
  ##  })
  
  
  
  # Visualização: Boxplot dos Scores do PCA
  
  
  ##MODIFICAÇÃO EM 06/05/2025: QUAIS COMPONENTES SERÃO VISUALIZADAS?
  
  ##ACRÉSCIMO DO SMOTENC EM 28/05/2025
  
  ##ACRÉSCIMO DE BORDERLINE SMOTE EM 05/06/2025
  
  ##ACRÉSCIMO DE SVM SMOTE EM 06/06/2025
  
  ##ACRÉSCIMO DE ADASYN EM 08/06/2025
  
  ##ACRÉSCIMO DE RANDOM UPSAMPLING EM 09/06/2025
  
  ##ACRÉSCIMO DE TOMEK LINKS EM 10/06/2025
  
  ##ACRÉSCIMO DE NEAR MISS EM 16/06/2025
  
  ##ACRÉSCIMO DE ENN EM 17/06/2025
  
  ##ACRÉSCIMO DE OSS EM 18/06/2025
  
  ##ACRÉSCIMO DE RANDOM DOWNSAMPLING EM 18/06/2025
  
  ##ACRÉSCIMO DE SMOTE TL EM 21/06/2025
  
  ##ACRÉSCIMO DE SMOTE ENN EM 04/07/2025

  ##ACRÉSCIMO DE SBC EM 10/07/2025

  ##ACRÉSCIMO DE SMOTE IPF EM 13/07/2025

  ##ACRÉSCIMO DE SPIDER EM 14/07/2025


  observe({
    dataset <- NULL
    
    
    if (isTRUE(runSMOTENC()) && !is.null(SMOTENC_Data()) && !is.null(input$target_nc)) {
      original_data <- as.data.frame(filedata())
      smote_data <- SMOTENC_Data()$syn_data
      class_col <- input$target_nc
    } else if (isTRUE(runSMOTE()) && !is.null(SMOTE_Data()) && !is.null(input$target)) {
      original_data <- as.data.frame(filedata())
      smote_data <- SMOTE_Data()$syn_data
      class_col <- input$target
    } else if (isTRUE(runBorderlineSMOTE()) && !is.null(BorderlineSMOTE_Data()) && !is.null(input$target_borderline)) {
      original_data <- as.data.frame(filedata())
      smote_data <- BorderlineSMOTE_Data()$syn_data
      class_col <- input$target_borderline
    } else if (isTRUE(runSVM_SMOTE()) && !is.null(SVM_SMOTE_Data()) && !is.null(input$target_svm)) {
      original_data <- as.data.frame(filedata())
      smote_data <- SVM_SMOTE_Data()$syn_data
      class_col <- input$target_svm
    } else if (isTRUE(runADASYN()) && !is.null(ADASYN_Data()) && !is.null(input$target_adasyn)) {
      original_data <- as.data.frame(filedata())
      smote_data <- ADASYN_Data()$syn_data
      class_col <- input$target_adasyn
    } else if (isTRUE(runRU()) && !is.null(RU_Data()) && !is.null(input$target_ru)) {
      original_data <- as.data.frame(filedata())
      smote_data <- RU_Data()$syn_data
      class_col <- input$target_ru
    } else if (isTRUE(runTomek()) && !is.null(Tomek_Data()) && !is.null(input$target_tomek)) {
      original_data <- as.data.frame(filedata())
      smote_data <- Tomek_Data()$removed_data
      class_col <- input$target_tomek
    } else if (isTRUE(runNearMiss()) && !is.null(NearMiss_Data()) && !is.null(input$target_nearmiss)) {
      original_data <- as.data.frame(filedata())
      smote_data <- NearMiss_Data()$removed_data
      class_col <- input$target_nearmiss
    } else if (isTRUE(runENN()) && !is.null(ENN_Data()) && !is.null(input$target_enn)) {
      original_data <- as.data.frame(filedata())
      smote_data <- ENN_Data()$removed_data
      class_col <- input$target_enn
    } else if (isTRUE(runOSS()) && !is.null(OSS_Data()) && !is.null(input$target_oss)) {
      original_data <- as.data.frame(filedata())
      smote_data <- OSS_Data()$removed_data
      class_col <- input$target_oss
    } else if (isTRUE(runRD()) && !is.null(RD_Data()) && !is.null(input$target_rd)) {
      original_data <- as.data.frame(filedata())
      smote_data <- RD_Data()$removed_data
      class_col <- input$target_rd
      
      
      ###BLOCO ADICIONADO EM 10/07/2025
      
    } else if (isTRUE(runSBC()) && !is.null(SBC_Data()) && !is.null(input$target_sbc)) { 
      original_data <- as.data.frame(filedata())
      smote_data <- SBC_Data()$removed_data
      class_col <- input$target_sbc
      
      
      ###BLOCO ADICIONADO EM 10/07/2025
      
    } else if (isTRUE(runSMOTETL()) && !is.null(SMOTETL_Data()) && !is.null(input$target_tl)) {
      
      original_data <- as.data.frame(filedata())
      class_col <- input$target_tl
      
      added_data <- SMOTETL_Data()$syn_data
      removed_data <- SMOTETL_Data()$removed_data
      
      # Inicializando estrutura base para comparação
      
      template <- original_data
      
      # Renomeando no template também
      
      if (class_col %in% names(template)) {
        names(template)[names(template) == class_col] <- "class"
      }
      
      # Garantindo estrutura se null ou vazio
      
      if (is.null(added_data) || nrow(added_data) == 0) {
        added_data <- template[0, , drop = FALSE]
      }
      if (is.null(removed_data) || nrow(removed_data) == 0) {
        removed_data <- template[0, , drop = FALSE]
      }
      
      # Renomeando colunas dos reais também
      
      if (class_col %in% names(added_data)) {
        names(added_data)[names(added_data) == class_col] <- "class"
      }
      if (class_col %in% names(removed_data)) {
        names(removed_data)[names(removed_data) == class_col] <- "class"
      }
      
      # Fazendo as marcações só se tiver linhas
      
      if (nrow(added_data) > 0) {
        added_data$class <- paste(added_data$class, "Synthetic")
      }
      if (nrow(removed_data) > 0) {
        removed_data$class <- paste(removed_data$class, "Removed") #ALTERADO EM 29/06/2025
      }
      
      # Reordenando colunas iguais
      
      added_data <- added_data[, names(template), drop = FALSE]
      removed_data <- removed_data[, names(template), drop = FALSE]
      
      smote_data <- dplyr::bind_rows(added_data, removed_data)
      
      if (nrow(smote_data) == 0) {
        message("Nada para exibir: nenhum dado sintético ou removido.")
        return(NULL)
      }
    
    #####################BLOCO ADICIONADO EM 04/07/2025
      
    } else if (isTRUE(runSMOTEENN()) && !is.null(SMOTEENN_Data()) && !is.null(input$target_enn2)) {
      
      original_data <- as.data.frame(filedata())
      class_col <- input$target_enn2
      
      added_data <- SMOTEENN_Data()$syn_data
      removed_data <- SMOTEENN_Data()$removed_data
      
      # Inicializando estrutura base para comparação
      
      template <- original_data
      
      # Renomeando no template também
      
      if (class_col %in% names(template)) {
        names(template)[names(template) == class_col] <- "class"
      }
      
      # Garantindo estrutura se null ou vazio
      
      if (is.null(added_data) || nrow(added_data) == 0) {
        added_data <- template[0, , drop = FALSE]
      }
      if (is.null(removed_data) || nrow(removed_data) == 0) {
        removed_data <- template[0, , drop = FALSE]
      }
      
      # Renomeando colunas dos reais também
      
      if (class_col %in% names(added_data)) {
        names(added_data)[names(added_data) == class_col] <- "class"
      }
      if (class_col %in% names(removed_data)) {
        names(removed_data)[names(removed_data) == class_col] <- "class"
      }
      
      # Fazendo as marcações só se tiver linhas
      
      if (nrow(added_data) > 0) {
        added_data$class <- paste(added_data$class, "Synthetic")
      }
      if (nrow(removed_data) > 0) {
        removed_data$class <- paste(removed_data$class, "Removed") #ALTERADO EM 29/06/2025
      }
      
      # Reordenando colunas iguais
      
      added_data <- added_data[, names(template), drop = FALSE]
      removed_data <- removed_data[, names(template), drop = FALSE]
      
      smote_data <- dplyr::bind_rows(added_data, removed_data)
      
      if (nrow(smote_data) == 0) {
        message("Nada para exibir: nenhum dado sintético ou removido.")
        return(NULL)
      }
      
  
      #####################BLOCO ADICIONADO EM 04/07/2025  
      
    #####################BLOCO ADICIONADO EM 13/07/2025
    
  } else if (isTRUE(runSMOTEIPF()) && !is.null(SMOTEIPF_Data()) && !is.null(input$target_ipf)) {
    
    original_data <- as.data.frame(filedata())
    class_col <- input$target_ipf
    
    added_data <- SMOTEIPF_Data()$syn_data
    removed_data <- SMOTEIPF_Data()$removed_data
    
    # Inicializando estrutura base para comparação
    
    template <- original_data
    
    # Renomeando no template também
    
    if (class_col %in% names(template)) {
      names(template)[names(template) == class_col] <- "class"
    }
    
    # Garantindo estrutura se null ou vazio
    
    if (is.null(added_data) || nrow(added_data) == 0) {
      added_data <- template[0, , drop = FALSE]
    }
    if (is.null(removed_data) || nrow(removed_data) == 0) {
      removed_data <- template[0, , drop = FALSE]
    }
    
    # Renomeando colunas dos reais também
    
    if (class_col %in% names(added_data)) {
      names(added_data)[names(added_data) == class_col] <- "class"
    }
    if (class_col %in% names(removed_data)) {
      names(removed_data)[names(removed_data) == class_col] <- "class"
    }
    
    # Fazendo as marcações só se tiver linhas
    
    if (nrow(added_data) > 0) {
      added_data$class <- paste(added_data$class, "Synthetic")
    }
    if (nrow(removed_data) > 0) {
      removed_data$class <- paste(removed_data$class, "Removed") #ALTERADO EM 29/06/2025
    }
    
  
    # Reordenando colunas iguais
    
    added_data <- added_data[, names(template), drop = FALSE]
    removed_data <- removed_data[, names(template), drop = FALSE]
    
    smote_data <- dplyr::bind_rows(added_data, removed_data)
    
 
    
    if (nrow(smote_data) == 0) {
      message("Nada para exibir: nenhum dado sintético ou removido.")
      return(NULL)
    }
    
    
    #####################BLOCO ADICIONADO EM 13/07/2025

      
    } 
    
    
    ##################### BLOCO SPIDER ADICIONADO EM 14/07/2025
    
   else if (isTRUE(runSPIDER()) && !is.null(SPIDER_Data()) && !is.null(input$target_spider)) {
    
    original_data <- as.data.frame(filedata())
    class_col <- input$target_spider
    
    added_data <- SPIDER_Data()$syn_data
    removed_data <- SPIDER_Data()$removed_data
    
    # Inicializando estrutura base para comparação
    
    template <- original_data
    
    # Renomeando no template também
    
    if (class_col %in% names(template)) {
      names(template)[names(template) == class_col] <- "class"
    }
    
    # Garantindo estrutura se null ou vazio
    
    if (is.null(added_data) || nrow(added_data) == 0) {
      added_data <- template[0, , drop = FALSE]
    }
    if (is.null(removed_data) || nrow(removed_data) == 0) {
      removed_data <- template[0, , drop = FALSE]
    }
    
    # Renomeando colunas dos reais também
    
    if (class_col %in% names(added_data)) {
      names(added_data)[names(added_data) == class_col] <- "class"
    }
    if (class_col %in% names(removed_data)) {
      names(removed_data)[names(removed_data) == class_col] <- "class"
    }
    
    # Fazendo as marcações só se tiver linhas
    
    if (nrow(added_data) > 0) {
      added_data$class <- paste(added_data$class, "Synthetic")
    }
    if (nrow(removed_data) > 0) {
      removed_data$class <- paste(removed_data$class, "Removed") #ALTERADO EM 29/06/2025
    }
    
    # Reordenando colunas iguais
    
    added_data <- added_data[, names(template), drop = FALSE]
    removed_data <- removed_data[, names(template), drop = FALSE]
    
    smote_data <- dplyr::bind_rows(added_data, removed_data)
    
    if (nrow(smote_data) == 0) {
      message("Nada para exibir: nenhum dado sintético ou removido.")
      return(NULL)
    }
    
  }
  
  ##################### FIM BLOCO SPIDER ADICIONADO EM 14/07/2025
  

    else {   
      return()
    }
  

    
    if (is.null(smote_data) || !(class_col %in% names(original_data)) || nrow(smote_data) == 0) return()
    
    names(original_data)[names(original_data) == class_col] <- "class"
    names(smote_data)[names(smote_data) == class_col] <- "class"
    
   ## if (!isTRUE(runSMOTETL()) &&
    if (!isTRUE(runSMOTETL()) && !isTRUE(runSMOTEENN()) &&  #MODIFICADO EM 04/07/2025
        (isTRUE(runSMOTE()) || isTRUE(runSMOTENC()) || isTRUE(runBorderlineSMOTE()) ||
         isTRUE(runSVM_SMOTE()) || isTRUE(runADASYN()) || isTRUE(runRU()))) {
      smote_data$class <- paste(smote_data$class, "Synthetic")
    }
    
    if (isTRUE(runTomek()) || isTRUE(runNearMiss()) || isTRUE(runENN()) || isTRUE(runOSS()) || isTRUE(runRD()) || isTRUE(runSBC())  ) {
      smote_data$class <- paste(smote_data$class, "Removed") #ALTERADO EM 29/06/2025
    }
    
    smote_data$fake_cat <- NULL
    feature_cols <- intersect(names(original_data), names(smote_data))
    original_data <- original_data[, feature_cols, drop = FALSE]
    smote_data <- smote_data[, feature_cols, drop = FALSE]
    original_data$Tipo <- "Original"
    smote_data$Tipo <- "Synthetic"
    
    if (isTRUE(runTomek()) || isTRUE(runNearMiss()) || isTRUE(runENN()) || isTRUE(runOSS()) || isTRUE(runRD()) || isTRUE(runSBC()) ) {
      smote_data$Tipo <- "Removed" #ALTERADO EM 29/06/2025
    }
    
    dataset <- dplyr::bind_rows(original_data, smote_data)
    
    num_vars <- names(dataset)[sapply(dataset, is.numeric)]
    num_vars <- setdiff(num_vars, "SampleID")
    if (length(num_vars) < 2) return()
    
    dados_filtrados <- dataset[, num_vars, drop = FALSE]
    if (any(!is.finite(as.matrix(dados_filtrados)))) return()
    
    pca_temp <- prcomp(dados_filtrados, center = TRUE, scale. = TRUE)
    pcs_all <- colnames(pca_temp$x)
    var_exp <- summary(pca_temp)$importance["Proportion of Variance", ]
    pcs_labels <- sprintf("%s (%.1f%%)", pcs_all, 100 * var_exp)
    choices_named <- setNames(pcs_all, pcs_labels)
    
    updateSelectInput(
      session,
      inputId = "pcs_to_show",
      choices = c("All PCs" = "ALL", choices_named),
      selected = NULL
    )
  })
  
  output$boxplot_scores <- renderPlotly({
    dataset <- NULL
    
    if (isTRUE(runSMOTENC()) && !is.null(SMOTENC_Data()) && !is.null(input$target_nc)) {
      original_data <- as.data.frame(filedata())
      smote_data <- SMOTENC_Data()$syn_data
      class_col <- input$target_nc
    } else if (isTRUE(runSMOTE()) && !is.null(SMOTE_Data()) && !is.null(input$target)) {
      original_data <- as.data.frame(filedata())
      smote_data <- SMOTE_Data()$syn_data
      class_col <- input$target
    } else if (isTRUE(runBorderlineSMOTE()) && !is.null(BorderlineSMOTE_Data()) && !is.null(input$target_borderline)) {
      original_data <- as.data.frame(filedata())
      smote_data <- BorderlineSMOTE_Data()$syn_data
      class_col <- input$target_borderline
    } else if (isTRUE(runSVM_SMOTE()) && !is.null(SVM_SMOTE_Data()) && !is.null(input$target_svm)) {
      original_data <- as.data.frame(filedata())
      smote_data <- SVM_SMOTE_Data()$syn_data
      class_col <- input$target_svm
    } else if (isTRUE(runADASYN()) && !is.null(ADASYN_Data()) && !is.null(input$target_adasyn)) {
      original_data <- as.data.frame(filedata())
      smote_data <- ADASYN_Data()$syn_data
      class_col <- input$target_adasyn
    } else if (isTRUE(runRU()) && !is.null(RU_Data()) && !is.null(input$target_ru)) {
      original_data <- as.data.frame(filedata())
      smote_data <- RU_Data()$syn_data
      class_col <- input$target_ru
    } else if (isTRUE(runTomek()) && !is.null(Tomek_Data()) && !is.null(input$target_tomek)) {
      original_data <- as.data.frame(filedata())
      smote_data <- Tomek_Data()$removed_data
      class_col <- input$target_tomek
    } else if (isTRUE(runNearMiss()) && !is.null(NearMiss_Data()) && !is.null(input$target_nearmiss)) {
      original_data <- as.data.frame(filedata())
      smote_data <- NearMiss_Data()$removed_data
      class_col <- input$target_nearmiss
    } else if (isTRUE(runENN()) && !is.null(ENN_Data()) && !is.null(input$target_enn)) {
      original_data <- as.data.frame(filedata())
      smote_data <- ENN_Data()$removed_data
      class_col <- input$target_enn
    } else if (isTRUE(runOSS()) && !is.null(OSS_Data()) && !is.null(input$target_oss)) {
      original_data <- as.data.frame(filedata())
      smote_data <- OSS_Data()$removed_data
      class_col <- input$target_oss
    } else if (isTRUE(runRD()) && !is.null(RD_Data()) && !is.null(input$target_rd)) {
      original_data <- as.data.frame(filedata())
      smote_data <- RD_Data()$removed_data
      class_col <- input$target_rd
     
    
    
    ####BLOCO ADICIONADO EM 10/07/2025
    
  } else if (isTRUE(runSBC()) && !is.null(SBC_Data()) && !is.null(input$target_sbc)) {
    original_data <- as.data.frame(filedata())
    smote_data <- SBC_Data()$removed_data
    class_col <- input$target_sbc
    
    
    ####BLOCO ADICIONADO EM 10/07/2025
    
    #############BLOCO MODIFICADO EM 28/06/2025
  }  else if (isTRUE(runSMOTETL()) && !is.null(SMOTETL_Data()) && !is.null(input$target_tl)) {
      original_data <- as.data.frame(filedata())
      class_col <- input$target_tl
      added_data <- SMOTETL_Data()$syn_data
      removed_data <- SMOTETL_Data()$removed_data
      
      # Protegendo se NULL ou vazio
      if (is.null(added_data) || nrow(added_data) == 0) {
        added_data <- NULL
      } else {
        if ("class" %in% names(added_data) || class_col %in% names(added_data)) {
          # Renomeia se precisar
          if (class_col %in% names(added_data)) {
            names(added_data)[names(added_data) == class_col] <- "class"
          }
          added_data$class <- paste(added_data$class, "Synthetic")
        }
      }
      
      if (is.null(removed_data) || nrow(removed_data) == 0) {
        removed_data <- NULL
      } else {
        if ("class" %in% names(removed_data) || class_col %in% names(removed_data)) {
          if (class_col %in% names(removed_data)) {
            names(removed_data)[names(removed_data) == class_col] <- "class"
          }
          removed_data$class <- paste(removed_data$class, "Removed") #ALTERADO EM 29/06/2025
        }
      }
      
      smote_data <- dplyr::bind_rows(added_data, removed_data)
      
      if (nrow(smote_data) == 0) {
        return(NULL)
      }
    }
   # }
    
    #####################BLOCO ADICIONADO EM 04/07/2025
    
      else if (isTRUE(runSMOTEENN()) && !is.null(SMOTEENN_Data()) && !is.null(input$target_enn2)) {
        original_data <- as.data.frame(filedata())
        class_col <- input$target_enn2
        added_data <- SMOTEENN_Data()$syn_data
        removed_data <- SMOTEENN_Data()$removed_data
        
        # Protegendo se NULL ou vazio
        if (is.null(added_data) || nrow(added_data) == 0) {
          added_data <- NULL
        } else {
          if ("class" %in% names(added_data) || class_col %in% names(added_data)) {
            # Renomeia se precisar
            if (class_col %in% names(added_data)) {
              names(added_data)[names(added_data) == class_col] <- "class"
            }
            added_data$class <- paste(added_data$class, "Synthetic")
          }
        }
        
        if (is.null(removed_data) || nrow(removed_data) == 0) {
          removed_data <- NULL
        } else {
          if ("class" %in% names(removed_data) || class_col %in% names(removed_data)) {
            if (class_col %in% names(removed_data)) {
              names(removed_data)[names(removed_data) == class_col] <- "class"
            }
            removed_data$class <- paste(removed_data$class, "Removed") #ALTERADO EM 29/06/2025
          }
        }
        
        smote_data <- dplyr::bind_rows(added_data, removed_data)
        
        if (nrow(smote_data) == 0) {
          return(NULL)
        }
        # }  
      
      
      
        #####################BLOCO ADICIONADO EM 04/07/2025
    
    
    #############BLOCO MODIFICADO EM 28/06/2025
      
    
  #  } 
    
    #####################BLOCO ADICIONADO EM 13/07/2025
    
  } else if (isTRUE(runSMOTEIPF()) && !is.null(SMOTEIPF_Data()) && !is.null(input$target_ipf)) {
    
    original_data <- as.data.frame(filedata())
    class_col <- input$target_ipf
    
    added_data <- SMOTEIPF_Data()$syn_data
    removed_data <- SMOTEIPF_Data()$removed_data
    
    # Inicializando estrutura base para comparação
    
    template <- original_data
    
    # Renomeando no template também
    
    if (class_col %in% names(template)) {
      names(template)[names(template) == class_col] <- "class"
    }
    
    # Garantindo estrutura se null ou vazio
    
    if (is.null(added_data) || nrow(added_data) == 0) {
      added_data <- template[0, , drop = FALSE]
    }
    if (is.null(removed_data) || nrow(removed_data) == 0) {
      removed_data <- template[0, , drop = FALSE]
    }
    
    # Renomeando colunas dos reais também
    
    if (class_col %in% names(added_data)) {
      names(added_data)[names(added_data) == class_col] <- "class"
    }
    if (class_col %in% names(removed_data)) {
      names(removed_data)[names(removed_data) == class_col] <- "class"
    }
    
    # Fazendo as marcações só se tiver linhas
    
    if (nrow(added_data) > 0) {
      added_data$class <- paste(added_data$class, "Synthetic")
    }
    if (nrow(removed_data) > 0) {
      removed_data$class <- paste(removed_data$class, "Removed") #ALTERADO EM 29/06/2025
    }
    
    # Reordenando colunas iguais
    
    added_data <- added_data[, names(template), drop = FALSE]
    removed_data <- removed_data[, names(template), drop = FALSE]
    
    smote_data <- dplyr::bind_rows(added_data, removed_data)
    
    if (nrow(smote_data) == 0) {
      message("Nada para exibir: nenhum dado sintético ou removido.")
      return(NULL)
    }
    
  }
    #####################BLOCO ADICIONADO EM 13/07/2025
    
    
    
    
    ##################### BLOCO SPIDER ADICIONADO EM 14/07/2025
    
 else if (isTRUE(runSPIDER()) && !is.null(SPIDER_Data()) && !is.null(input$target_spider)) {
    
    original_data <- as.data.frame(filedata())
    class_col <- input$target_spider
    
    added_data <- SPIDER_Data()$syn_data
    removed_data <- SPIDER_Data()$removed_data
    
    # Inicializando estrutura base para comparação
    
    template <- original_data
    
    # Renomeando no template também
    
    if (class_col %in% names(template)) {
      names(template)[names(template) == class_col] <- "class"
    }
    
    # Garantindo estrutura se null ou vazio
    
    if (is.null(added_data) || nrow(added_data) == 0) {
      added_data <- template[0, , drop = FALSE]
    }
    if (is.null(removed_data) || nrow(removed_data) == 0) {
      removed_data <- template[0, , drop = FALSE]
    }
    
    # Renomeando colunas dos reais também
    
    if (class_col %in% names(added_data)) {
      names(added_data)[names(added_data) == class_col] <- "class"
    }
    if (class_col %in% names(removed_data)) {
      names(removed_data)[names(removed_data) == class_col] <- "class"
    }
    
    # Fazendo as marcações só se tiver linhas
    
    if (nrow(added_data) > 0) {
      added_data$class <- paste(added_data$class, "Synthetic")
    }
    if (nrow(removed_data) > 0) {
      removed_data$class <- paste(removed_data$class, "Removed") #ALTERADO EM 29/06/2025
    }
    
    # Reordenando colunas iguais
    
    added_data <- added_data[, names(template), drop = FALSE]
    removed_data <- removed_data[, names(template), drop = FALSE]
    
    smote_data <- dplyr::bind_rows(added_data, removed_data)
    
    if (nrow(smote_data) == 0) {
      message("Nada para exibir: nenhum dado sintético ou removido.")
      return(NULL)
    }
  }
  
  ##################### FIM BLOCO SPIDER ADICIONADO EM 14/07/2025
  
  
    else {
      return(NULL)
    }
  
    if (is.null(smote_data) || !(class_col %in% names(original_data))) return(NULL)
    
    names(original_data)[names(original_data) == class_col] <- "class"
    names(smote_data)[names(smote_data) == class_col] <- "class"
    
    
  ##  if (!isTRUE(runSMOTETL()) &&
    if (!isTRUE(runSMOTETL()) && !isTRUE(runSMOTEENN()) &&  #MODIFICADO EM 04/07/2025
        (isTRUE(runSMOTE()) || isTRUE(runSMOTENC()) || isTRUE(runBorderlineSMOTE()) ||
         isTRUE(runSVM_SMOTE()) || isTRUE(runADASYN()) || isTRUE(runRU()))) {
      smote_data$class <- paste(smote_data$class, "Synthetic")
    }
    
    if (isTRUE(runTomek()) || isTRUE(runNearMiss()) || isTRUE(runENN()) ||
        isTRUE(runOSS()) || isTRUE(runRD())  
        || isTRUE(runSBC())      ) {
      smote_data$class <- paste(smote_data$class, "Removed") #ALTERADO EM 29/06/2025
    }
    
    smote_data$fake_cat <- NULL
    
    # Seleção das colunas numéricas em comum
    
    feature_cols <- intersect(names(original_data), names(smote_data))
    original_data <- original_data[, feature_cols, drop = FALSE]
    smote_data <- smote_data[, feature_cols, drop = FALSE]
    
    original_data$Tipo <- "Original"
    
    if (!"Tipo" %in% names(smote_data)) {
      smote_data$Tipo <- "Synthetic"
    }
    
    if (isTRUE(runTomek()) || isTRUE(runNearMiss()) || isTRUE(runENN()) ||
        isTRUE(runOSS()) || isTRUE(runRD()) 
        || isTRUE(runSBC())        ) {
      smote_data$Tipo <- "Removed" #ALTERADO EM 29/06/2025
    }
    
    dataset <- dplyr::bind_rows(original_data, smote_data)
    
    req(input$pcs_to_show)
    selected_input <- input$pcs_to_show
    if (is.null(selected_input) || length(selected_input) == 0) return(NULL)
    
    num_vars <- names(dataset)[sapply(dataset, is.numeric)]
    num_vars <- setdiff(num_vars, "SampleID")
    if (length(num_vars) < 2) return(NULL)
    
    dados_filtrados <- dataset[, num_vars, drop = FALSE]
    if (any(!is.finite(as.matrix(dados_filtrados)))) return(NULL)
    
    pca_temp <- prcomp(dados_filtrados, center = TRUE, scale. = TRUE)
    scores <- as.data.frame(pca_temp$x)
    scores$Class <- dataset$class
    
    original_classes <- unique(gsub(" (Synthetic|Removed)$", "", scores$Class[!grepl("Synthetic", scores$Class)]))
    derived_classes <- c(paste(original_classes, "Synthetic"), paste(original_classes, "Removed")) #ALTERADO EM 29/06/2025
    ordered_levels <- c(original_classes, intersect(derived_classes, unique(scores$Class)))
    scores$Class <- factor(scores$Class, levels = ordered_levels)
    
    pcs_all <- colnames(pca_temp$x)
    var_exp <- summary(pca_temp)$importance["Proportion of Variance", ]
    pcs_labels <- sprintf("%s (%.1f%%)", pcs_all, 100 * var_exp)
    label_map <- setNames(pcs_labels, pcs_all)
    
    selected_pcs <- if ("ALL" %in% selected_input) pcs_all else selected_input
    if (!all(selected_pcs %in% pcs_all)) return(NULL)
    
    scores_filtered <- scores[, c(selected_pcs, "Class"), drop = FALSE]
    names(scores_filtered) <- c(label_map[selected_pcs], "Class")
    
    melted_scores <- reshape2::melt(scores_filtered, id.vars = "Class", variable.name = "PC", value.name = "Score")
    melted_scores$PC <- factor(melted_scores$PC, levels = label_map[selected_pcs])
    
    plot_ly(
      data = melted_scores,
      x = ~PC, y = ~Score, color = ~Class, type = "box",
      boxpoints = "outliers"
    ) %>%
      layout(
        title = "Boxplot of Scores (PCA)",
        xaxis = list(title = "Principal Components"),
        yaxis = list(title = "Score Values"),
        boxmode = "group"
      )
  })
  
  
  ##ACRÉSCIMO DO SMOTENC EM 28/05/2025
  
  ##ACRÉSCIMO DE BORDERLINE SMOTE EM 05/06/2025
  
  ##ACRÉSCIMO DE SVM SMOTE EM 06/06/2025
  
  ##ACRÉSCIMO DE ADASYN EM 08/06/2025
  
  ##ACRÉSCIMO DE RANDOM UPSAMPLING EM 09/06/2025
  
  ##ACRÉSCIMO DE TOMEK LINKS EM 10/06/2025
  
  ##ACRÉSCIMO DE NEAR MISS EM 16/06/2025
  
  ##ACRÉSCIMO DE ENN EM 17/06/2025
  
  ##ACRÉSCIMO DE OSS EM 18/06/2025
  
  ##ACRÉSCIMO DE RANDOM DOWNSAMPLING EM 18/06/2025
  
  ##ACRÉSCIMO DE SMOTE TL EM 21/06/2025
  
  ##ACRÉSCIMO DE SMOTE ENN EM 04/07/2025
  
  ##ACRÉSCIMO DE SBC EM 10/07/2025
  
  ##ACRÉSCIMO DE SMOTE IPF EM 13/07/2025
  
  ##ACRÉSCIMO DE SPIDER EM 14/07/2025
  
  ###############ATUALIZADO EM 10/02/2025
  
  ## VISUALIZAÇÃO: Boxplot of Variables
  ## MODIFICAÇÃO EM 06/05/2025 - SELEÇÃO DE Variables PARA VISUALIZAR NO BOXPLOT
  ## MODIFICAÇÃO EM 19/05/2025 - ACRÉSCIMO DE SMOTENC
  ## MODIFICAÇÃO EM 05/06/2025 - ACRÉSCIMO DE BORDERLINE SMOTE
  ## MODIFICAÇÃO EM 06/06/2025 - ACRÉSCIMO DE SVM SMOTE
  ## MODIFICAÇÃO EM 08/06/2025 - ACRÉSCIMO DE ADASYN
  ## MODIFICAÇÃO EM 09/06/2025 - ACRÉSCIMO DE RANDOM UPSAMPLING
  ## MODIFICAÇÃO EM 10/06/2025 - ACRÉSCIMO DE TOMEK LINKS
  ## MODIFICAÇÃO EM 16/06/2025 - ACRÉSCIMO DE NEAR MISS
  ## MODIFICAÇÃO EM 17/06/2025 - ACRÉSCIMO DE ENN
  ## MODIFICAÇÃO EM 18/06/2025 - ACRÉSCIMO DE OSS
  ## MODIFICAÇÃO EM 18/06/2025 - ACRÉSCIMO DE RANDOM DOWNSAMPLING
  ## MODIFICAÇÃO EM 21/06/2025 - ACRÉSCIMO DE SMOTE TL
  ## MODIFICAÇÃO EM 04/07/2025 - ACRÉSCIMO DE SMOTE ENN
  ## MODIFICAÇÃO EM 10/07/2025 - ACRÉSCIMO DE SBC
  ## MODIFICAÇÃO EM 13/07/2025 - ACRÉSCIMO DE SMOTE IPF
  ## MODIFICAÇÃO EM 14/07/2025 - ACRÉSCIMO DE SPIDER
  
  
  
  # Atualização do seletor de Variables com base no método executado
  
  observeEvent(c(runSMOTE(), runSMOTENC(), runBorderlineSMOTE(), runSVM_SMOTE(), runADASYN(), runRU(),
                # runTomek(), runNearMiss(), runENN(), runOSS(), runRD(), runSMOTETL()), {
              #  runTomek(), runNearMiss(), runENN(), runOSS(), runRD(), runSBC() ,runSMOTETL(), runSMOTEENN() ), { #MODIFICADO EM 10/07/2025
              # runTomek(), runNearMiss(), runENN(), runOSS(), runRD(), runSBC() ,runSMOTETL(), runSMOTEENN(), runSMOTEIPF() ), { #MODIFICADO EM 13/07/2025
              runTomek(), runNearMiss(), runENN(), runOSS(), runRD(), runSBC() ,runSMOTETL(), runSMOTEENN(), runSMOTEIPF(), runSPIDER() ), { #MODIFICADO EM 14/07/2025
                   req(data())
                   
                   dataset <- NULL
                   
                   if (isTRUE(runSMOTENC()) && !is.null(SMOTENC_Data())) {
                     dataset <- SMOTENC_Data()$data
                   } else if (isTRUE(runSMOTE()) && !is.null(SMOTE_Data())) {
                     dataset <- SMOTE_Data()$data
                   } else if (isTRUE(runBorderlineSMOTE()) && !is.null(BorderlineSMOTE_Data())) {
                     dataset <- BorderlineSMOTE_Data()$data
                   } else if (isTRUE(runSVM_SMOTE()) && !is.null(SVM_SMOTE_Data())) {
                     dataset <- SVM_SMOTE_Data()$data
                   } else if (isTRUE(runADASYN()) && !is.null(ADASYN_Data())) {
                     dataset <- ADASYN_Data()$data
                   } else if (isTRUE(runRU()) && !is.null(RU_Data())) {
                     dataset <- RU_Data()$data
                   } else if (isTRUE(runTomek()) && !is.null(Tomek_Data())) {
                     dataset <- Tomek_Data()$data
                   } else if (isTRUE(runNearMiss()) && !is.null(NearMiss_Data())) {
                     dataset <- NearMiss_Data()$data
                   } else if (isTRUE(runENN()) && !is.null(ENN_Data())) {
                     dataset <- ENN_Data()$data
                   } else if (isTRUE(runOSS()) && !is.null(OSS_Data())) {
                     dataset <- OSS_Data()$data
                   } else if (isTRUE(runRD()) && !is.null(RD_Data())) {
                     dataset <- RD_Data()$data
                     
                     
                     ###BLOCO ADICIONADO EM 10/07/2025
                     
                   } else if (isTRUE(runSBC()) && !is.null(SBC_Data())) {
                     dataset <- SBC_Data()$data
                     
                     
                     ###BLOCO ADICIONADO EM 10/07/2025
                     
                     
                     
                     
                   } else if (isTRUE(runSMOTETL()) && !is.null(SMOTETL_Data())) {
                     dataset <- dplyr::bind_rows(SMOTETL_Data()$syn_data, SMOTETL_Data()$removed_data)
                   
                   
                   
                   ########BLOCO ADICIONADO EM 04/07/2025
                   
                } else if (isTRUE(runSMOTEENN()) && !is.null(SMOTEENN_Data())) {
                  dataset <- dplyr::bind_rows(SMOTEENN_Data()$syn_data, SMOTEENN_Data()$removed_data)
                
                   
                  ########BLOCO ADICIONADO EM 04/07/2025
                   
                   
                   
                   ########BLOCO ADICIONADO EM 13/07/2025
                   
              } else if (isTRUE(runSMOTEIPF()) && !is.null(SMOTEIPF_Data())) {
                dataset <- dplyr::bind_rows(SMOTEIPF_Data()$syn_data, SMOTEIPF_Data()$removed_data)
              
              
                   ########BLOCO ADICIONADO EM 13/07/2025
                   
                   
                   ######## BLOCO SPIDER ADICIONADO EM 14/07/2025
                   
              } else if (isTRUE(runSPIDER()) && !is.null(SPIDER_Data())) {
                dataset <- dplyr::bind_rows(SPIDER_Data()$syn_data, SPIDER_Data()$removed_data)
              }
              
              ######## FIM BLOCO SPIDER ADICIONADO EM 14/07/2025
    
                   
                   
                   ####BLOCO MODIFICADO EM 17/10/2025         
                   
                   #       if (is.null(dataset)) return()
                   
                   #       original <- as.data.frame(data())
                   #       synthetic <- as.data.frame(dataset)
                   
                   #       common_columns <- intersect(names(original), names(synthetic))
                   #       numeric_columns <- common_columns[sapply(original[, common_columns], is.numeric)]
                   
                   #       updateSelectInput(
                   #         session,
                   #         inputId = "boxplot_vars",
                   #         choices = c("All Variables" = "ALL", numeric_columns),
                   #         selected = NULL
                   #       )
                   #     })
                   
                   
                   
                   ######## FIM BLOCO SPIDER ADICIONADO EM 14/07/2025
                   
                   if (is.null(dataset)) return()
                   
                   original <- as.data.frame(data())
                   synthetic <- as.data.frame(dataset)
                   
                   # Garantindo que common_columns é um vetor de caracteres
                   common_columns <- intersect(names(original), names(synthetic))
                   common_columns <- as.character(common_columns)
                   
                   # Se não houver colunas comuns, evita erro
                   if (length(common_columns) == 0) {
                     numeric_columns <- character(0)
                   } else {
                     df_sub <- original[, common_columns, drop = FALSE]
                     numeric_columns <- names(df_sub)[sapply(df_sub, is.numeric)]
                   }
                   
                   updateSelectInput(
                     session,
                     inputId = "boxplot_vars",
                     choices = c("All Variables" = "ALL", numeric_columns),
                     selected = NULL
                   )
              })
  
  
  
  ####BLOCO MODIFICADO EM 17/10/2025             
  
  # Renderização do boxplot com SMOTE-TL separado em 3 grupos
  
  output$boxplot_output <- renderPlotly({
    req(data())
    
    original_data <- as.data.frame(data())
    synthetic_data <- NULL
    removed_data <- NULL
    
    if (isTRUE(runSMOTENC()) && !is.null(SMOTENC_Data())) {
      synthetic_data <- SMOTENC_Data()$data
    } else if (isTRUE(runSMOTE()) && !is.null(SMOTE_Data())) {
      synthetic_data <- SMOTE_Data()$data
    } else if (isTRUE(runBorderlineSMOTE()) && !is.null(BorderlineSMOTE_Data())) {
      synthetic_data <- BorderlineSMOTE_Data()$data
    } else if (isTRUE(runSVM_SMOTE()) && !is.null(SVM_SMOTE_Data())) {
      synthetic_data <- SVM_SMOTE_Data()$data 
    } else if (isTRUE(runADASYN()) && !is.null(ADASYN_Data())) {
      synthetic_data <- ADASYN_Data()$data 
    } else if (isTRUE(runRU()) && !is.null(RU_Data())) {
      synthetic_data <- RU_Data()$data 
    } else if (isTRUE(runTomek()) && !is.null(Tomek_Data())) {
      removed_data <- Tomek_Data()$data
    } else if (isTRUE(runNearMiss()) && !is.null(NearMiss_Data())) {
      removed_data <- NearMiss_Data()$data
    } else if (isTRUE(runENN()) && !is.null(ENN_Data())) {
      removed_data <- ENN_Data()$data
    } else if (isTRUE(runOSS()) && !is.null(OSS_Data())) {
      removed_data <- OSS_Data()$data
    } else if (isTRUE(runRD()) && !is.null(RD_Data())) {
      removed_data <- RD_Data()$data
      
      
      ####BLOCO ADICIONADO EM 10/07/2025
      
    } else if (isTRUE(runSBC()) && !is.null(SBC_Data())) {
      removed_data <- SBC_Data()$data
      
      ####BLOCO ADICIONADO EM 10/07/2025
      
      
    } else if (isTRUE(runSMOTETL()) && !is.null(SMOTETL_Data())) {
      synthetic_data <- SMOTETL_Data()$syn_data
      removed_data <- SMOTETL_Data()$removed_data
    
    
    
    ########BLOCO ADICIONADO EM 04/07/2025
    } else if (isTRUE(runSMOTEENN()) && !is.null(SMOTEENN_Data())) {
      synthetic_data <- SMOTEENN_Data()$syn_data
      removed_data <- SMOTEENN_Data()$removed_data
    
    
    
    
    ########BLOCO ADICIONADO EM 04/07/2025
    

    
    ########BLOCO ADICIONADO EM 13/07/2025
    
  } else if (isTRUE(runSMOTEIPF()) && !is.null(SMOTEIPF_Data())) {
    synthetic_data <- SMOTEIPF_Data()$syn_data
    removed_data <- SMOTEIPF_Data()$removed_data
    
  
  
  
   ########BLOCO ADICIONADO EM 13/07/2025
  
  ######## BLOCO ADICIONADO EM 14/07/2025
    
  } else if (isTRUE(runSPIDER()) && !is.null(SPIDER_Data())) {
    synthetic_data <- SPIDER_Data()$syn_data
    removed_data <- SPIDER_Data()$removed_data
  }
  
  ######## BLOCO ADICIONADO EM 14/07/2025
  
   req(input$boxplot_vars)
    
    generate_boxplot_independente(original_data, synthetic_data, removed_data, isolate(input$boxplot_vars))
  })
  
  # Função para gerar boxplot com separação total entre Original / Synthetic / Removida #ALTERADO EM 29/06/2025
  
  generate_boxplot_independente <- function(original, synthetic = NULL, removed = NULL, selected_vars, 
                                            colors = c("Original" = "darkblue", 
                                                       "Synthetic" = "lightgreen", 
                                                     ##  "Removed Synthetic" = "red")) { #MODIFICADO EM 08/07/2025
                                                     "Removed" = "red")) {
    if (is.null(selected_vars) || length(selected_vars) == 0) return(NULL)
    
    datasets <- list()
    
    # Normalize function
    
    normalize <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
    
    # Valid numeric vars
    
    common_cols <- names(original)
    if (!is.null(synthetic)) common_cols <- intersect(common_cols, names(synthetic))
    if (!is.null(removed)) common_cols <- intersect(common_cols, names(removed))
    
    numeric_vars <- common_cols[sapply(original[, common_cols, drop = FALSE], is.numeric)]
    selected_vars <- if ("ALL" %in% selected_vars) numeric_vars else intersect(selected_vars, numeric_vars)
    if (length(selected_vars) == 0) return(NULL)
    
    # Original
    
    original_copy <- original[, selected_vars, drop = FALSE]
    original_copy <- as.data.frame(lapply(original_copy, normalize))
    original_copy$type <- "Original"
    datasets[["original"]] <- original_copy
    
    # Synthetic (if exists)
    
    if (!is.null(synthetic) && nrow(synthetic) > 0) {
      syn_copy <- synthetic[, selected_vars, drop = FALSE]
      syn_copy <- as.data.frame(lapply(syn_copy, normalize))
      syn_copy$type <- "Synthetic"
      datasets[["synthetic"]] <- syn_copy
    }
    
    # Removed Synthetic (if exists)
    
    if (!is.null(removed) && nrow(removed) > 0) {
      rem_copy <- removed[, selected_vars, drop = FALSE]
      rem_copy <- as.data.frame(lapply(rem_copy, normalize))
    ##  rem_copy$type <- "Removed Synthetic" ##MODIFICADO EM 08/07/2025
      rem_copy$type <- "Removed"
      datasets[["removed"]] <- rem_copy
    }
    
    # Unindo e plotando
    
    combined <- do.call(rbind, datasets)
    melted <- reshape2::melt(combined, id.vars = "type", variable.name = "variable", value.name = "value")
    
    plot_ly(
      data = melted,
      x = ~variable,
      y = ~value,
      color = ~type,
      type = "box",
      colors = colors
    ) %>%
      layout(
        title = "Boxplot of normalized variables",
        xaxis = list(title = "Variables"),
        yaxis = list(title = "Normalized Values"),
        boxmode = "group"
      )
  }
  
  
  
  ##  observe({
  ##    req(data(), SMOTE_Data())
  
  ##    original <- as.data.frame(data())
  ##    synthetic <- as.data.frame(SMOTE_Data()$data)
  
  ##    common_columns <- intersect(names(original), names(synthetic))
  ##    numeric_columns <- common_columns[sapply(original[, common_columns], is.numeric)]
  
  ##    updateSelectInput(
  ##     session,
  ##      inputId = "boxplot_vars",
  ##      choices = c("All Variables" = "ALL", numeric_columns),
  ##      selected = NULL
  ##    )
  ##  })
  
  ##  output$boxplot_variables <- renderPlotly({
  ##    req(data(), SMOTE_Data())
  
  ##    selected_vars <- input$boxplot_vars
  ##    if (is.null(selected_vars) || length(selected_vars) == 0) return(NULL)
  
  ##    original <- as.data.frame(data())
  ##    synthetic <- as.data.frame(SMOTE_Data()$data)
  
  ##    common_columns <- intersect(names(original), names(synthetic))
  ##    numeric_columns <- common_columns[sapply(original[, common_columns], is.numeric)]
  
  # Seleção válida
  ##    selected_vars <- if ("ALL" %in% selected_vars) numeric_columns else intersect(selected_vars, numeric_columns)
  ##    if (length(selected_vars) == 0) return(NULL)
  
  ##    normalize <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
  
  ##    original[, selected_vars] <- lapply(original[, selected_vars, drop = FALSE], normalize)
  ##    synthetic[, selected_vars] <- lapply(synthetic[, selected_vars, drop = FALSE], normalize)
  
  ##    original$type <- "Original"
  ##    synthetic$type <- "Synthetic"
  
  ##    combined <- rbind(
  ##      original[, c(selected_vars, "type"), drop = FALSE],
  ##      synthetic[, c(selected_vars, "type"), drop = FALSE]
  ##    )
  
  # Transformação segura para long format com uma variável
  ##    melted <- reshape2::melt(as.data.frame(combined), id.vars = "type", variable.name = "variable", value.name = "value")
  
  ##    plot_ly(
  ##      data = melted, x = ~variable, y = ~value, color = ~type, type = "box",
  ##      colors = c("Original" = "darkblue", "Synthetic" = "lightgreen")
  ##    ) %>%
  ##      layout(
  ##        title = "Boxplot of normalized variables",
  ##        xaxis = list(title = "Variables"),
  ##        yaxis = list(title = "Normalized Values"),
  ##        boxmode = "group"
  ##      )
  ##  })
  
  
  
  ##ACRÉSCIMO DE PLOT COMPARATIVO DAS Original Samples E SyntheticS NA GUIA DIAGNÓSTICO EM 27/03/2025
  ##MODIFICADO EM 09/04/2025 - EM WHICH SAMPLES NÃO ESTAVA COMPUTANDO A QUANTIDADE CERTA DE AMOSTRAS APÓS SMOTE
  ##MODIFICAÇÃO EM 10/04/2025 - AS AMOSTRAS SyntheticS QUANDO PLOTADAS SOZINHAS EM WHICH SAMPLES AGORA FUNCIONAM
  
  
  
  
  ########BLOCO MODIFICADO EM 18/05/2025 - ACRÉSCIMO DE SMOTE_NC EM PLOTS COMPARATIVOS
  ########BLOCO MODIFICADO EM 05/06/2025 - ACRÉSCIMO DE BORDERLINE SMOTE EM PLOTS COMPARATIVOS
  ########BLOCO MODIFICADO EM 06/06/2025 - ACRÉSCIMO DE SVM SMOTE EM PLOTS COMPARATIVOS
  ########BLOCO MODIFICADO EM 08/06/2025 - ACRÉSCIMO DE ADASYN EM PLOTS COMPARATIVOS
  ########BLOCO MODIFICADO EM 09/06/2025 - ACRÉSCIMO DE RANDOM UPSAMPLING EM PLOTS COMPARATIVOS
  ########BLOCO MODIFICADO EM 10/06/2025 - ACRÉSCIMO DE TOMEK LINKS EM PLOTS COMPARATIVOS
  ########BLOCO MODIFICADO EM 16/06/2025 - ACRÉSCIMO DE NEAR MISS EM PLOTS COMPARATIVOS
  ########BLOCO MODIFICADO EM 17/06/2025 - ACRÉSCIMO DE ENN EM PLOTS COMPARATIVOS
  ########BLOCO MODIFICADO EM 18/06/2025 - ACRÉSCIMO DE OSS EM PLOTS COMPARATIVOS
  ########BLOCO MODIFICADO EM 18/06/2025 - ACRÉSCIMO DE RANDOM DOWNSAMPLING EM PLOTS COMPARATIVOS
  ########BLOCO MODIFICADO EM 21/06/2025 - ACRÉSCIMO DE SMOTE TL EM PLOTS COMPARATIVOS
  ########BLOCO MODIFICADO EM 04/07/2025 - ACRÉSCIMO DE SMOTE ENN EM PLOTS COMPARATIVOS
  ########BLOCO MODIFICADO EM 10/07/2025 - ACRÉSCIMO DE SBC EM PLOTS COMPARATIVOS
  ########BLOCO MODIFICADO EM 13/07/2025 - ACRÉSCIMO DE SMOTE IPF EM PLOTS COMPARATIVOS
  ########BLOCO MODIFICADO EM 14/07/2025 - ACRÉSCIMO DE SPIDER EM PLOTS COMPARATIVOS
  
  
  # VALORES REATIVOS PRINCIPAIS
  
  original_data_fixed <- reactiveVal()
  combined_data <- reactiveVal()
  
  # ATUALIZANDO combined_data() para SMOTE, SMOTE-NC, BORDERLINE SMOTE, SVM SMOTE, ADASYN, RANDOM UPSAMPLING,
  #TOMEK LINKS, NEAR MISS, ENN, OSS, RANDOM DOWNSAMPLING, SMOTE TL, SMOTE ENN, SBC, SMOTE IPF E SPIDER
  
  
  
  ##########BLOCO MODIFICADO EM 10/06/2025
  
  ###### observe({
  ########    req(filedata())
  
  
  observeEvent(c(runSMOTE(), runSMOTENC(), runBorderlineSMOTE(), runSVM_SMOTE(), runADASYN(), runRU(),
                 # runTomek(), runNearMiss(), runENN(), runOSS(), runRD(), runSMOTETL()), {
                 # runTomek(), runNearMiss(), runENN(), runOSS(), runRD(), runSBC() ,runSMOTETL(), runSMOTEENN()  ), { #MODIFICADO EM 10/07/2025
                 # runTomek(), runNearMiss(), runENN(), runOSS(), runRD(), runSBC() ,runSMOTETL(), runSMOTEENN(), runSMOTEIPF()   ), { #MODIFICADO EM 13/07/2025
                 runTomek(), runNearMiss(), runENN(), runOSS(), runRD(), runSBC() ,runSMOTETL(), runSMOTEENN(), runSMOTEIPF(), runSPIDER()   ), { #MODIFICADO EM 14/07/2025
                   req(data())
                   
                   smote_data <- NULL
                   syn_data_tl <- NULL
                   removed_data_tl <- NULL
                   
                   
                   #######ADICIONADO EM 04/07/2025
                   
                   ##OBS: VERSIONADO COM "2" PARA MANTER MESMA DINÂMICA(CASO TENHA TAMBÉM EM ENN, AQUI OBJETIVA TRATAR SMOTE ENN)
                   
                   syn_data_enn2 <- NULL
                   removed_data_enn2 <- NULL
                   
                   
                   
                   #######ADICIONADO EM 04/07/2025
                   
                   
                   #######ADICIONADO EM 14/07/2025
                   syn_data_ipf <- NULL
                   removed_data_ipf <- NULL
                   
                   
                   syn_data_spider <- NULL
                   removed_data_spider <- NULL
                   
                   #######ADICIONADO EM 14/07/2025
                   
                   
                   
                   
                   
                   
                   
                   class_col <- NULL
                   
                   # Detectando o método executado
                   
                   if (isTRUE(runSMOTENC()) && !is.null(SMOTENC_Data())) {
                     smote_data <- SMOTENC_Data()$syn_data
                     class_col <- input$target_nc
                   } else if (isTRUE(runSMOTE()) && !is.null(SMOTE_Data())) {
                     smote_data <- SMOTE_Data()$syn_data
                     class_col <- input$target
                   } else if (isTRUE(runBorderlineSMOTE()) && !is.null(BorderlineSMOTE_Data())) {
                     smote_data <- BorderlineSMOTE_Data()$syn_data
                     class_col <- input$target_borderline
                   } else if (isTRUE(runSVM_SMOTE()) && !is.null(SVM_SMOTE_Data())) {
                     smote_data <- SVM_SMOTE_Data()$syn_data
                     class_col <- input$target_svm
                   } else if (isTRUE(runADASYN()) && !is.null(ADASYN_Data())) {
                     smote_data <- ADASYN_Data()$syn_data
                     class_col <- input$target_adasyn
                   } else if (isTRUE(runRU()) && !is.null(RU_Data())) {
                     smote_data <- RU_Data()$syn_data
                     class_col <- input$target_ru
                   } else if (isTRUE(runTomek()) && !is.null(Tomek_Data())) {
                     removed_data_tl <- Tomek_Data()$removed_data  ## ADICIONADO EM 21/06/2025
                     class_col <- input$target_tomek
                   } else if (isTRUE(runNearMiss()) && !is.null(NearMiss_Data())) {
                     removed_data_tl <- NearMiss_Data()$removed_data ## ADICIONADO EM 21/06/2025
                     class_col <- input$target_nearmiss
                   } else if (isTRUE(runENN()) && !is.null(ENN_Data())) {
                     removed_data_tl <- ENN_Data()$removed_data ## ADICIONADO EM 21/06/2025
                     class_col <- input$target_enn
                   } else if (isTRUE(runOSS()) && !is.null(OSS_Data())) {
                     removed_data_tl <- OSS_Data()$removed_data ## ADICIONADO EM 21/06/2025
                     class_col <- input$target_oss
                   } else if (isTRUE(runRD()) && !is.null(RD_Data())) {
                     removed_data_tl <- RD_Data()$removed_data ## ADICIONADO EM 21/06/2025
                     class_col <- input$target_rd
                     
                     
                     ######BLOCO ADICIONADO EM 10/07/2025
                     
                   } else if (isTRUE(runSBC()) && !is.null(SBC_Data())) {
                     removed_data_tl <- SBC_Data()$removed_data 
                     class_col <- input$target_sbc
                     
                     ######BLOCO ADICIONADO EM 10/07/2025
                     
                   } else if (isTRUE(runSMOTETL()) && !is.null(SMOTETL_Data())) {
                     syn_data_tl <- SMOTETL_Data()$syn_data
                     removed_data_tl <- SMOTETL_Data()$removed_data
                     class_col <- input$target_tl
                   }
                   
                   
                   ##############BLOCO ADICIONADO EM 04/07/2025
                   
                   else if (isTRUE(runSMOTEENN()) && !is.null(SMOTEENN_Data())) {
                     syn_data_enn2 <- SMOTEENN_Data()$syn_data
                     removed_data_enn2 <- SMOTEENN_Data()$removed_data
                     class_col <- input$target_enn2
                   }
                   
                   ##############BLOCO ADICIONADO EM 04/07/2025
                   
                   
                   ##############BLOCO ADICIONADO EM 13/07/2025
                   
                   else if (isTRUE(runSMOTEIPF()) && !is.null(SMOTEIPF_Data())) {
                     syn_data_ipf <- SMOTEIPF_Data()$syn_data
                     removed_data_ipf <- SMOTEIPF_Data()$removed_data
                     class_col <- input$target_ipf
                   }
                   
                   ##############BLOCO ADICIONADO EM 13/07/2025
                   
      
                   ############## BLOCO ADICIONADO EM 14/07/2025
                   
                   else if (isTRUE(runSPIDER()) && !is.null(SPIDER_Data())) {
                     syn_data_spider <- SPIDER_Data()$syn_data
                     removed_data_spider <- SPIDER_Data()$removed_data
                     class_col <- input$target_spider
                   }
                   
                   ############## BLOCO ADICIONADO EM 14/07/2025
                   
                   
                   
                   req(class_col)
                   original_data <- as.data.frame(filedata())
                   names(original_data)[names(original_data) == class_col] <- "class"
                   feature_cols <- setdiff(names(original_data), "class")
                   original_data <- original_data[, c(feature_cols, "class"), drop = FALSE]
                   
                   ########BLOCO MODIFICADO EM 28/06/2025
                   
                   
                   # Processando dados do SMOTE-TL com proteção
                   
                   if (!is.null(syn_data_tl) && nrow(syn_data_tl) > 0) {
                     tryCatch({
                       names(syn_data_tl)[names(syn_data_tl) == class_col] <- "class"
                       syn_data_tl <- syn_data_tl[, c(feature_cols, "class"), drop = FALSE]
                       syn_data_tl$class <- paste(syn_data_tl$class, "Synthetic")
                       syn_data_tl$fake_cat <- NULL
                     }, error = function(e) {
                       message("Error processing syn_data_tl: ", e$message)
                     })
                   }
                   
                   if (!is.null(removed_data_tl) && nrow(removed_data_tl) > 0) {
                     tryCatch({
                       names(removed_data_tl)[names(removed_data_tl) == class_col] <- "class"
                       removed_data_tl <- removed_data_tl[, c(feature_cols, "class"), drop = FALSE]
                       removed_data_tl$class <- paste(removed_data_tl$class, "Removed") #ALTERADO EM 29/06/2025
                       removed_data_tl$SampleID <- NA
                       removed_data_tl$fake_cat <- NULL
                     }, error = function(e) {
                       message("Error processing removed_data_tl: ", e$message)
                     })
                   }
                   
                   
                   ##############BLOCO ADICIONADO EM 04/07/2025
                   
                   # Processando dados do SMOTE-ENN com proteção
                   
                   if (!is.null(syn_data_enn2) && nrow(syn_data_enn2) > 0) {
                     tryCatch({
                       names(syn_data_enn2)[names(syn_data_enn2) == class_col] <- "class"
                       syn_data_enn2 <- syn_data_enn2[, c(feature_cols, "class"), drop = FALSE]
                       syn_data_enn2$class <- paste(syn_data_enn2$class, "Synthetic")
                       syn_data_enn2$fake_cat <- NULL
                     }, error = function(e) {
                       message("Error processing syn_data_enn2: ", e$message)
                     })
                   }
                   
                   if (!is.null(removed_data_enn2) && nrow(removed_data_enn2) > 0) {
                     tryCatch({
                       names(removed_data_enn2)[names(removed_data_enn2) == class_col] <- "class"
                       removed_data_enn2 <- removed_data_enn2[, c(feature_cols, "class"), drop = FALSE]
                       removed_data_enn2$class <- paste(removed_data_enn2$class, "Removed") #ALTERADO EM 29/06/2025
                       removed_data_enn2$SampleID <- NA
                       removed_data_enn2$fake_cat <- NULL
                     }, error = function(e) {
                       message("Error processing removed_data_enn2: ", e$message)
                     })
                   }
                   
                   ##############BLOCO ADICIONADO EM 04/07/2025
                   
                   
                   ##############BLOCO ADICIONADO EM 13/07/2025
                   
                   # Processando dados do SMOTE-IPF com proteção
                   
                   if (!is.null(syn_data_ipf) && nrow(syn_data_ipf) > 0) {
                     tryCatch({
                       names(syn_data_ipf)[names(syn_data_ipf) == class_col] <- "class"
                       syn_data_ipf <- syn_data_ipf[, c(feature_cols, "class"), drop = FALSE]
                       syn_data_ipf$class <- paste(syn_data_ipf$class, "Synthetic")
                       syn_data_ipf$fake_cat <- NULL
                     }, error = function(e) {
                       message("Error processing syn_data_ipf: ", e$message)
                     })
                   }
                   
                   if (!is.null(removed_data_ipf) && nrow(removed_data_ipf) > 0) {
                     tryCatch({
                       names(removed_data_ipf)[names(removed_data_ipf) == class_col] <- "class"
                       removed_data_ipf <- removed_data_ipf[, c(feature_cols, "class"), drop = FALSE]
                       removed_data_ipf$class <- paste(removed_data_ipf$class, "Removed") #ALTERADO EM 29/06/2025
                       removed_data_ipf$SampleID <- NA
                       removed_data_ipf$fake_cat <- NULL
                     }, error = function(e) {
                       message("Error processing removed_data_ipf: ", e$message)
                     })
                   }
                   
                   ##############BLOCO ADICIONADO EM 13/07/2025
                
                   ############## BLOCO ADICIONADO EM 14/07/2025
                   
                   # Processando dados do SPIDER com proteção
                   
                   if (!is.null(syn_data_spider) && nrow(syn_data_spider) > 0) {
                     tryCatch({
                       names(syn_data_spider)[names(syn_data_spider) == class_col] <- "class"
                       syn_data_spider <- syn_data_spider[, c(feature_cols, "class"), drop = FALSE]
                       syn_data_spider$class <- paste(syn_data_spider$class, "Synthetic")
                       syn_data_spider$fake_cat <- NULL
                     }, error = function(e) {
                       message("Error processing syn_data_spider: ", e$message)
                     })
                   }
                   
                   if (!is.null(removed_data_spider) && nrow(removed_data_spider) > 0) {
                     tryCatch({
                       names(removed_data_spider)[names(removed_data_spider) == class_col] <- "class"
                       removed_data_spider <- removed_data_spider[, c(feature_cols, "class"), drop = FALSE]
                       removed_data_spider$class <- paste(removed_data_spider$class, "Removed") #ALTERADO EM 29/06/2025
                       removed_data_spider$SampleID <- NA
                       removed_data_spider$fake_cat <- NULL
                     }, error = function(e) {
                       message("Error processing removed_data_spider: ", e$message)
                     })
                   }
                   
                   ############## BLOCO ADICIONADO EM 14/07/2025
                   
                   
                   ########BLOCO MODIFICADO EM 28/06/2025
                   
                   
                   # Processando smote_data (demais métodos)
                   
                   if (!is.null(smote_data)) {
                     names(smote_data)[names(smote_data) == class_col] <- "class"
                     smote_data <- smote_data[, c(feature_cols, "class"), drop = FALSE]
                     smote_data$class <- paste(smote_data$class, "Synthetic")
                     smote_data$fake_cat <- NULL
                   }
                   
                   # Fixando os dados originais apenas uma vez
                   
                   if (is.null(original_data_fixed())) {
                     original_data_fixed(original_data)
                   }
                   
                   prev_synthetics <- if (!is.null(combined_data())) {
                     combined_data()[grepl("Synthetic", combined_data()$class), ]
                   } else NULL
                   
                   # Removendo duplicatas
                   
                   if (!is.null(prev_synthetics) && !is.null(smote_data) && nrow(smote_data) > 0) {
                     new_synthetics <- dplyr::anti_join(
                       mutate(smote_data, .id = seq_len(nrow(smote_data))),
                       prev_synthetics[, c(feature_cols, "class"), drop = FALSE],
                       by = c(feature_cols, "class")
                     )
                     smote_data <- new_synthetics[, setdiff(names(new_synthetics), ".id"), drop = FALSE]
                   }
                  
                   
                   ##############BLOCO MODIFICADO EM 04/07/2025
                   ##############BLOCO MODIFICADO EM 13/07/2025
                   ##############BLOCO MODIFICADO EM 14/07/2025
                   # Unificando colunas
                   
              ##     all_names <- union(
              ##       names(original_data_fixed()),
              ##       union(
              ##         names(prev_synthetics),
              ##         union(
              ##           names(smote_data),
                      
                         ##############BLOCO MODIFICADO EM 04/07/2025
                         ##############BLOCO MODIFICADO EM 13/07/2025
                         ##############BLOCO MODIFICADO EM 14/07/2025
                   
                          #  union(names(syn_data_tl), names(removed_data_tl))
              ##         union(names(syn_data_tl), names(removed_data_tl),
              ##        union(names(syn_data_enn2), names(removed_data_enn2)))
                      
                      ##############BLOCO MODIFICADO EM 04/07/2025
                      ##############BLOCO MODIFICADO EM 13/07/2025
                      ##############BLOCO MODIFICADO EM 14/07/2025
              ##         )
              ##       )
              ##     )
                   
               ###    all_names <- union(
              ###       names(original_data_fixed()),
              ###       union(
              ###         names(prev_synthetics),
              ###         union(
              ###           names(smote_data),
              ###           union(
              ###             union(names(syn_data_tl), names(removed_data_tl)),
              ###             union(names(syn_data_enn2), names(removed_data_enn2))
              ###           )
              ###         )
              ###       )
              ###     )
                 
               ####    all_names <- union(
               ####       names(original_data_fixed()),
               ####       union(
               ####         names(prev_synthetics),
               ####         union(
               ####           names(smote_data),
               ####           union(
               ####             union(names(syn_data_tl), names(removed_data_tl)),
               ####             union(
               ####              union(names(syn_data_enn2), names(removed_data_enn2)),
               ####               union(names(syn_data_ipf), names(removed_data_ipf))
               ####             )
               ####           )
               ####        )
               ####       )
               ####     )
                   
                   all_names <- union(
                     names(original_data_fixed()),
                     union(
                       names(prev_synthetics),
                       union(
                         names(smote_data),
                         union(
                           union(names(syn_data_tl), names(removed_data_tl)),
                           union(
                             union(names(syn_data_enn2), names(removed_data_enn2)),
                             union(
                               union(names(syn_data_ipf), names(removed_data_ipf)),
                               union(names(syn_data_spider), names(removed_data_spider))
                             )
                           )
                         )
                       )
                     )
                   )
                   
                   
                   
                   ##############BLOCO MODIFICADO EM 04/07/2025
                   ##############BLOCO MODIFICADO EM 13/07/2025
                   ##############BLOCO MODIFICADO EM 14/07/2025
                   
                   padronizar <- function(df) {
                     if (is.null(df) || nrow(df) == 0) {
                       return(as.data.frame(matrix(ncol = length(all_names), nrow = 0, dimnames = list(NULL, all_names))))
                     }
                     for (col in setdiff(all_names, names(df))) {
                       df[[col]] <- NA
                     }
                     df <- df[, all_names, drop = FALSE]
                     return(df)
                   }
                   
                   
                   original_pad <- padronizar(original_data_fixed())
                   
                   
                   
                   prev_synthetics_pad <- padronizar(prev_synthetics)
                   synthetic_pad <- padronizar(smote_data)
                   syn_tl_pad <- padronizar(syn_data_tl)
                   rem_tl_pad <- padronizar(removed_data_tl)
                   
                   ###############ADICIONADO EM 04/07/2025
                   ###############ADICIONADO EM 13/07/2025
                   ##############ADICIONADO  EM 14/07/2025
                   
                   syn_enn2_pad <- padronizar(syn_data_enn2)
                   rem_enn2_pad <- padronizar(removed_data_enn2)
                   
                   syn_ipf_pad <- padronizar(syn_data_ipf)
                   rem_ipf_pad <- padronizar(removed_data_ipf)
                   
                   syn_spider_pad <- padronizar(syn_data_spider)
                   rem_spider_pad <- padronizar(removed_data_spider)
                   
                   
                  # combined_all <- rbind(original_pad, prev_synthetics_pad, synthetic_pad, syn_tl_pad, rem_tl_pad)
  ##combined_all <- rbind(original_pad, prev_synthetics_pad, synthetic_pad, syn_tl_pad, 
    ##                    rem_tl_pad, syn_enn2_pad, rem_enn2_pad )
      
                   
                   combined_all <- rbind(
                     original_pad,
                     prev_synthetics_pad,
                     synthetic_pad,
                     syn_tl_pad,
                     rem_tl_pad,
                     syn_enn2_pad,
                     rem_enn2_pad,
                     syn_ipf_pad,
                     rem_ipf_pad,
                     syn_spider_pad,
                     rem_spider_pad
                   )
                   
                   
                   ###############ADICIONADO EM 04/07/2025
                   ###############ADICIONADO EM 13/07/2025
                   ###############ADICIONADO EM 14/07/2025
                   
                   
                   
                   combined_all$SampleID <- seq_len(nrow(combined_all))
                   combined_all$class <- as.character(combined_all$class)
                   if ("fake_cat" %in% names(combined_all)) combined_all$fake_cat <- NULL
                   
                   combined_data(combined_all)
                 })
  
  # Atualizando seletor de amostras
  
  observeEvent(combined_data(), {
    df <- combined_data()
    if (is.null(df)) {
      req(filedata())
      n_orig <- suppressWarnings(as.integer(nrow(filedata())))
      updateSelectizeInput(session, "sample_selector_plotly",
                           choices = as.character(seq_len(n_orig)),
                           server = TRUE)
    } else {
      sample_ids <- df$SampleID
      updateSelectizeInput(session, "sample_selector_plotly",
                           choices = as.character(sample_ids),
                           server = TRUE)
    }
  }, ignoreNULL = FALSE)
  
  # Dados filtrados para os plots
  
  dados_filtrados_plotly <- reactive({
    df <- combined_data()
    req(df)
    
    df$class <- as.character(df$class)
    
    # df$Tipo <- ifelse(grepl("Synthetic", df$class), "Synthetic", "Original")
    
    #BLOCO MODIFICADO EM 10/06/2025
    
    df$Tipo <- ifelse(grepl("Synthetic", df$class), "Synthetic",
                      ifelse(grepl("Removed", df$class), "Removed", "Original")) #ALTERADO EM 29/06/2025
    
    #BLOCO MODIFICADO EM 10/06/2025
    
    
    if (input$enable_selection && !is.null(input$sample_selector_plotly)) {
      ids <- as.numeric(input$sample_selector_plotly)
      df <- df[df$SampleID %in% ids, ]
    }
    
    df[order(df$SampleID), ]
  })
  
  # Gerando os gráficos 
  
  plot_data_matrix <- function(centered = FALSE, y_label = "samples - raw data", titulo = "Data") {
    df <- dados_filtrados_plotly()
    req(nrow(df) > 0)
    
    vars <- setdiff(names(Filter(is.numeric, df)), "SampleID")
    
    if (!centered) {
      mat <- as.matrix(df[, vars])
    } else {
      df$class <- as.character(df$class)
      mat <- df %>%
        dplyr::group_by(class) %>%
        dplyr::summarise(across(all_of(vars), mean, na.rm = TRUE)) %>%
        dplyr::ungroup()
      
      df <- data.frame(
        SampleID = seq_len(nrow(mat)),
        class = mat$class,
        
        
        ##BLOCO MODIFICADO EM 10/06/2025
        
        #  Tipo = ifelse(grepl("Synthetic", mat$class), "Synthetic", "Original")
        
        
        Tipo = ifelse(grepl("Synthetic", mat$class), "Synthetic",
                      ifelse(grepl("Removed", mat$class), "Removed", "Original")) #ALTERADO EM 29/06/2025
        
        
      )
      mat <- as.matrix(mat[, vars])
    }
    
    df$class <- as.character(df$class)
    
    # df$Tipo <- ifelse(grepl("Synthetic", df$class), "Synthetic", "Original")
    
    
    df$Tipo <- ifelse(grepl("Synthetic", df$class), "Synthetic",
                      ifelse(grepl("Removed", df$class), "Removed", "Original")) #ALTERADO EM 29/06/2025
    
    
    # original_classes <- unique(gsub(" Synthetic$", "", df$class[grepl("Synthetic", df$class)]))
    
    original_classes <- unique(gsub(" Synthetic$| Removed$", "", df$class)) #ALTERADO EM 29/06/2025
    
    
    #orig_classes_all <- unique(gsub(" Synthetic$", "", combined_data()$class[!grepl("Synthetic", combined_data()$class)]))
    
    #orig_classes_all <- unique(gsub(" Synthetic$| Removida$", "", combined_data()$class[!grepl("Synthetic| Removida", combined_data()$class)])) #ALTERADO EM 29/06/2025
    
    
    orig_classes_all <- unique(gsub(" Synthetic$| Removed$", "", combined_data()$class))
    
    
    
    
    # all_classes <- unique(c(orig_classes_all, paste0(orig_classes_all, " Synthetic")))
    
    
    all_classes <- unique(c(orig_classes_all, paste0(orig_classes_all, " Synthetic"), paste0(orig_classes_all, "Removed"))) #ALTERADO EM 29/06/2025
    
    
    
    # ordered_levels <- c(rbind(orig_classes_all, paste0(orig_classes_all, " Synthetic")))
    
    
    # ordered_levels <- c(rbind(orig_classes_all, paste0(orig_classes_all, " Synthetic"), paste0(orig_classes_all, "Removida"))) #ALTERADO EM 29/06/2025
    
    
    ordered_levels <- c(rbind(orig_classes_all,
                              paste0(orig_classes_all, " Synthetic"),
                              paste0(orig_classes_all, " Removed"))) #ALTERADO EM 29/06/2025
    
    
    
    
    ordered_levels <- ordered_levels[ordered_levels %in% unique(c(df$class, all_classes))]
    
    df$class <- factor(df$class, levels = ordered_levels)
    class_levels <- levels(df$class)
    
    df_long <- data.frame(
      SampleID = rep(df$SampleID, each = length(vars)),
      class = rep(as.character(df$class), each = length(vars)),
      Tipo = rep(df$Tipo, each = length(vars)),
      Variavel = factor(rep(vars, times = nrow(df)), levels = vars),
      Valor = as.vector(t(mat))
    )
    
    base_palette <- RColorBrewer::brewer.pal(min(9, length(class_levels)), "Set1")
    base_palette[6] <- "grey"
    paleta <- colorRampPalette(base_palette)(length(class_levels))
    names(paleta) <- class_levels
    
    if (input$coloring_type == "class") {
      p <- plot_ly()
      legenda_marcada <- character(0)
      for (classe in class_levels) {
        df_sub <- df_long[df_long$class == classe, ]
        cor_classe <- paleta[[classe]]
        #  dash_style <- ifelse(grepl("Synthetic", classe), "dot", "solid")
        
        
        dash_style <- ifelse(grepl("Synthetic", classe), "dot",
                             ifelse(grepl("Removed", classe), "dash", "solid")) #ALTERADO EM 29/06/2025
        
        
        
        mostrar_legenda <- !(classe %in% legenda_marcada)
        
        p <- add_trace(p,
                       data = df_sub,
                       x = ~Variavel,
                       y = ~Valor,
                       type = 'scatter', mode = 'lines',
                       line = list(color = cor_classe, width = 1.5, dash = dash_style),
                       name = classe,
                       legendgroup = classe,
                       showlegend = mostrar_legenda,
                       hoverinfo = "text",
                       text = ~paste("Class:", class,
                                     "<br>Type:", Tipo,
                                     "<br>SampleID:", SampleID,
                                     "<br>Variable:", Variavel,
                                     "<br>Value:", round(Valor, 3)))
        legenda_marcada <- c(legenda_marcada, classe)
      }
      p <- layout(p,
                  title = titulo,
                  xaxis = list(title = "Variable", type = "category"),
                  yaxis = list(title = y_label),
                  legend = list(x = 1.02, y = 1, orientation = "v", xanchor = "left"),
                  margin = list(r = 120, t = 60),
                  plot_bgcolor = "white")
    } else {
      p <- plot_ly()
      df_long <- df_long[order(df_long$SampleID, df_long$Variavel), ]
      for (i in unique(df_long$SampleID)) {
        df_sub <- df_long[df_long$SampleID == i, ]
        classe_i <- df_sub$class[1]
        cor_classe <- paleta[as.character(classe_i)]
        # dash_style <- ifelse(grepl("Synthetic", classe_i), "dot", "solid")
        
        dash_style <- ifelse(grepl("Synthetic", classe_i), "dot",
                             ifelse(grepl("Removed", classe_i), "dash", "solid")) #ALTERADO EM 29/06/2025
        
        
        
        p <- add_trace(p,
                       data = df_sub,
                       x = ~Variavel,
                       y = ~Valor,
                       type = 'scatter', mode = 'lines',
                       line = list(color = cor_classe, width = 1.5, dash = dash_style),
                       name = NULL,
                       showlegend = FALSE,
                       hoverinfo = "text",
                       text = ~paste("Class:", class,
                                     "<br>Type:", Tipo,
                                     "<br>SampleID:", SampleID,
                                     "<br>Variable:", Variavel,
                                     "<br>Value:", round(Valor, 3)))
      }
      p <- layout(p,
                  title = titulo,
                  xaxis = list(title = "Variable", type = "category"),
                  yaxis = list(title = y_label),
                  showlegend = FALSE,
                  margin = list(r = 10, t = 60),
                  plot_bgcolor = "white")
    }
    return(p)
  }
  
  output$plotly_data_raw <- renderPlotly({
    plot_data_matrix(FALSE, y_label = "samples - raw data", titulo = "Data")
  })
  
  output$plotly_data_class_mean <- renderPlotly({
    plot_data_matrix(TRUE, y_label = "class mean - raw data", titulo = "Class Mean Plot")
  })
  
  
  ########BLOCO MODIFICADO EM 19/05/2025 - ACRÉSCIMO DE SMOTE_NC EM PLOTS COMPARATIVOS
  ########BLOCO MODIFICADO EM 05/06/2025 - ACRÉSCIMO DE BORDERLINE SMOTE EM PLOTS COMPARATIVOS 
  ########BLOCO MODIFICADO EM 06/06/2025 - ACRÉSCIMO DE SVM SMOTE EM PLOTS COMPARATIVOS
  ########BLOCO MODIFICADO EM 08/06/2025 - ACRÉSCIMO DE ADASYN EM PLOTS COMPARATIVOS
  ########BLOCO MODIFICADO EM 09/06/2025 - ACRÉSCIMO DE RANDOM UPSAMPLING EM PLOTS COMPARATIVOS
  ########BLOCO MODIFICADO EM 10/06/2025 - ACRÉSCIMO DE TOMEK LINKS EM PLOTS COMPARATIVOS
  ########BLOCO MODIFICADO EM 17/06/2025 - ACRÉSCIMO DE NEAR MISS EM PLOTS COMPARATIVOS
  ########BLOCO MODIFICADO EM 17/06/2025 - ACRÉSCIMO DE ENN EM PLOTS COMPARATIVOS
  ########BLOCO MODIFICADO EM 18/06/2025 - ACRÉSCIMO DE OSS EM PLOTS COMPARATIVOS
  ########BLOCO MODIFICADO EM 18/06/2025 - ACRÉSCIMO DE RANDOM DOWNSAMPLING EM PLOTS COMPARATIVOS
  ########BLOCO MODIFICADO EM 21/06/2025 - ACRÉSCIMO DE SMOTE TL EM PLOTS COMPARATIVOS
  ########BLOCO MODIFICADO EM 04/07/2025 - ACRÉSCIMO DE SMOTE ENN EM PLOTS COMPARATIVOS
  ########BLOCO MODIFICADO EM 10/07/2025 - ACRÉSCIMO DE SBC EM PLOTS COMPARATIVOS
  ########BLOCO MODIFICADO EM 13/07/2025 - ACRÉSCIMO DE SMOTE IPF EM PLOTS COMPARATIVOS
  ########BLOCO MODIFICADO EM 14/07/2025 - ACRÉSCIMO DE SPIDER EM PLOTS COMPARATIVOS
  
  
  
  ##ACRÉSCIMO DE PLOT COMPARATIVO DAS Original Samples E SyntheticS NA GUIA DIAGNÓSTICO EM 27/03/2025
  
  #######ACRÉSCIMO DA Jensen-Shannon divergence EM 28/03/2025#####
  
  ####### Jensen-Shannon divergence – SMOTE + SMOTE-NC - MODIFICADA EM 19/05/2025 #######
  
  #ACRÉSCIMO DE BORDERLINE SMOTE EM 05/06/2025
  #ACRÉSCIMO DE SVM SMOTE EM 06/06/2025
  #ACRÉSCIMO DE ADASYN EM 08/06/2025
  #ACRÉSCIMO DE RANDOM UPSAMPLING EM 09/06/2025
  #ACRÉSCIMO DE TOMEK LINKS EM 12/06/2025
  #ACRÉSCIMO DE NEAR MISS EM 17/06/2025
  #ACRÉSCIMO DE ENN EM 17/06/2025
  #ACRÉSCIMO DE OSS EM 18/06/2025
  #ACRÉSCIMO DE RANDOM DOWNSAMPLING EM 18/06/2025
  #ACRÉSCIMO DE SMOTE TL EM 25/06/2025
  #ACRÉSCIMO DE SMOTE ENN EM 04/07/2025
  #ACRÉSCIMO DE SBC EM 10/07/2025
  #ACRÉSCIMO DE SMOTE IPF EM 13/07/2025
  #ACRÉSCIMO DE SPIDER EM 14/07/2025
  
  
  # Função JS
  
  js_divergence <- function(x, y, bins = 30) {
    # if (length(x) < 2 || length(y) < 2) return(NA)
    if (length(x) < 1 || length(y) < 1) return(NA)
    
    if (is.numeric(x)) {
      breaks <- pretty(c(x, y), n = bins)
      hist_x <- hist(x, breaks = breaks, plot = FALSE)$density
      hist_y <- hist(y, breaks = breaks, plot = FALSE)$density
    } else {
      x <- as.character(x)
      y <- as.character(y)
      levels_all <- union(unique(x), unique(y))
      hist_x <- table(factor(x, levels = levels_all))
      hist_y <- table(factor(y, levels = levels_all))
      hist_x <- as.numeric(hist_x)
      hist_y <- as.numeric(hist_y)
    }
    
    if (sum(hist_x) == 0 || sum(hist_y) == 0) return(NA)
    
    px <- hist_x / sum(hist_x)
    py <- hist_y / sum(hist_y)
    
    suppressWarnings(philentropy::distance(rbind(px, py), method = "jensen-shannon"))
  }
  
  # Reactive com suporte a SMOTE, SMOTE-NC, BORDERLINE SMOTE, SVM SMOTE, ADASYN, RANDOM UPSAMPLING
  # TOMEK LINKS, NEAR MISS, ENN, OSS, RANDOM DOWNSAMPLING, SMOTE TL, SMOTE ENN, SBC, SMOTE IPF E SPIDER
  
  js_div_data <- reactive({
    req(filedata())
    
    original_data <- as.data.frame(filedata())
    smote_data <- NULL
    class_col <- NULL
    
    resultado <- NULL  #ADICIONADO EM 25/06/2025
    
    
    # Detectando método executado
    
    #    if (isTRUE(runSMOTENC()) && !is.null(SMOTENC_Data()) && !is.null(input$target_nc)) {
    #      smote_data <- SMOTENC_Data()$syn_data
    #      class_col <- input$target_nc
    #    } else if (isTRUE(runSMOTE()) && !is.null(SMOTE_Data()) && !is.null(input$target)) {
    #      smote_data <- SMOTE_Data()$syn_data
    #      class_col <- input$target
    #    } else {
    #      return(NULL)
    #    }
    
    
    
    if (isTRUE(runSMOTENC()) && !is.null(SMOTENC_Data()) && !is.null(input$target_nc)) {
      smote_data <- SMOTENC_Data()$syn_data
      class_col <- input$target_nc
    } else if (isTRUE(runSMOTE()) && !is.null(SMOTE_Data()) && !is.null(input$target)) {
      smote_data <- SMOTE_Data()$syn_data
      class_col <- input$target
    }
    else if (isTRUE(runBorderlineSMOTE()) && !is.null(BorderlineSMOTE_Data()) && !is.null(input$target_borderline)) {
      smote_data <- BorderlineSMOTE_Data()$syn_data
      class_col <- input$target_borderline
      
    } else if (isTRUE(runSVM_SMOTE()) && !is.null(SVM_SMOTE_Data()) && !is.null(input$target_svm)) {
      smote_data <- SVM_SMOTE_Data()$syn_data
      class_col <- input$target_svm  
      
      
    } else if (isTRUE(runADASYN()) && !is.null(ADASYN_Data()) && !is.null(input$target_adasyn)) {
      smote_data <- ADASYN_Data()$syn_data
      class_col <- input$target_adasyn   
      
      
    } else if (isTRUE(runRU()) && !is.null(RU_Data()) && !is.null(input$target_ru)) {
      smote_data <- RU_Data()$syn_data
      class_col <- input$target_ru   
      
      
      
      
      #########ADICIONADO EM 11/06/2025
      #  } else if (isTRUE(runTomek()) && !is.null(Tomek_Data()) && !is.null(input$target_tomek)) {
      #   smote_data <- Tomek_Data()$removed_data
      #  class_col <- input$target_tomek
      
    } else if (isTRUE(runTomek()) && !is.null(Tomek_Data()) && !is.null(input$target_tomek)) {
      removed <- Tomek_Data()$removed_data
      if (is.null(removed) || !is.data.frame(removed) || nrow(removed) == 0) return(NULL)
      smote_data <- removed
      class_col <- input$target_tomek
      
      
      
      #########ADICIONADO EM 11/06/2025
      
      
      #########ADICIONADO EM 17/06/2025   
    } else if (isTRUE(runNearMiss()) && !is.null(NearMiss_Data()) && !is.null(input$target_nearmiss)) {
      removed <- NearMiss_Data()$removed_data
      if (is.null(removed) || !is.data.frame(removed) || nrow(removed) == 0) return(NULL)
      smote_data <- removed
      class_col <- input$target_nearmiss
      
      
    } else if (isTRUE(runENN()) && !is.null(ENN_Data()) && !is.null(input$target_enn)) {
      removed <- ENN_Data()$removed_data
      if (is.null(removed) || !is.data.frame(removed) || nrow(removed) == 0) return(NULL)
      smote_data <- removed
      class_col <- input$target_enn
      
      
    } else if (isTRUE(runOSS()) && !is.null(OSS_Data()) && !is.null(input$target_oss)) {
      removed <- OSS_Data()$removed_data
      if (is.null(removed) || !is.data.frame(removed) || nrow(removed) == 0) return(NULL)
      smote_data <- removed
      class_col <- input$target_oss    
      
    } else if (isTRUE(runRD()) && !is.null(RD_Data()) && !is.null(input$target_rd)) {
      removed <- RD_Data()$removed_data
      if (is.null(removed) || !is.data.frame(removed) || nrow(removed) == 0) return(NULL)
      smote_data <- removed
      class_col <- input$target_rd  
      
      
      ####BLOCO ADICIONADO EM 10/07/2025
      
    } else if (isTRUE(runSBC()) && !is.null(SBC_Data()) && !is.null(input$target_sbc)) {
      removed <- SBC_Data()$removed_data
      if (is.null(removed) || !is.data.frame(removed) || nrow(removed) == 0) return(NULL)
      smote_data <- removed
      class_col <- input$target_sbc  
      
      ####BLOCO ADICIONADO EM 10/07/2025
      
      
      
      
      
    } else if (isTRUE(runSMOTETL()) && !is.null(SMOTETL_Data()) && !is.null(input$target_tl)) {
      original_data <- as.data.frame(filedata())
      original_data$Tipo <- "Original"
      class_col <- input$target_tl
      added_data <- SMOTETL_Data()$syn_data
      removed_data <- SMOTETL_Data()$removed_data
      
      if (is.null(added_data) || nrow(added_data) == 0) {
        added_data <- original_data[0, , drop = FALSE]
      }
      if (is.null(removed_data) || nrow(removed_data) == 0) {
        removed_data <- original_data[0, , drop = FALSE]
      }
      
      names(original_data)[names(original_data) == class_col] <- "class"
      names(added_data)[names(added_data) == class_col] <- "class"
      names(removed_data)[names(removed_data) == class_col] <- "class"
      
      if (nrow(added_data) > 0) {
        added_data$class <- paste(added_data$class, "Synthetic")
        added_data$Tipo <- "Synthetic"
      }
      
      if (nrow(removed_data) > 0) {
        removed_data$class <- paste(removed_data$class, "Removed") #ALTERADO EM 29/06/2025
        removed_data$Tipo <- "Removed" #ALTERADO EM 29/06/2025
      }
      
      df_tl <- dplyr::bind_rows(original_data, added_data, removed_data)
      if (nrow(df_tl) == 0) return(NULL)
      
      df_tl$class <- as.character(df_tl$class)
      df_tl$Tipo <- as.character(df_tl$Tipo)
      df_tl$classe_base <- gsub(" Synthetic$", "", gsub(" Removed$", "", df_tl$class)) #ALTERADO EM 29/06/2025
      
      classes <- unique(df_tl$classe_base)
      variaveis <- setdiff(names(df_tl), c("class", "Tipo", "classe_base"))
      if (length(variaveis) == 0) return(NULL)
      
      resultado <- data.frame()
      for (cl in classes) {
        df_o <- df_tl %>% dplyr::filter(classe_base == cl & Tipo == "Original")
        df_a <- df_tl %>% dplyr::filter(classe_base == cl & Tipo == "Synthetic")
        df_r <- df_tl %>% dplyr::filter(classe_base == cl & Tipo == "Removed") #ALTERADO EM 29/06/2025
        
        grupo_comparado <- df_o
        if (nrow(df_a) > 0) {
          grupo_comparado <- dplyr::bind_rows(grupo_comparado, df_a)
        }
        if (nrow(df_r) > 0) {
          grupo_comparado <- dplyr::anti_join(grupo_comparado, df_r, by = intersect(names(df_o), names(df_r)))
        }
        
        if (nrow(df_o) >= 1 && nrow(grupo_comparado) >= 1 && all(sapply(df_o[variaveis], is.numeric)) && all(sapply(grupo_comparado[variaveis], is.numeric))) {
          for (var in variaveis) {
            if (length(df_o[[var]]) >= 1 && length(grupo_comparado[[var]]) >= 1) {
              js <- js_divergence(df_o[[var]], grupo_comparado[[var]])
              if (!is.na(js)) {
                resultado <- rbind(resultado, data.frame(Classe = cl, Variável = var, Divergência = js))
              }
            }
          }
        }
      }
      
      if (nrow(resultado) > 0) {
        resultado$Comparado_com <- "Synthetic and/or Removed" #ALTERADO EM 29/06/2025
        resultado <- resultado[!is.na(resultado$Divergência), ]
        if (nrow(resultado) == 0) return(NULL)
        resultado$Classe <- factor(resultado$Classe, levels = unique(df_tl$classe_base))
        resultado$Variável <- factor(resultado$Variável, levels = unique(resultado$Variável))
        assign("df_tl_js_distributions", df_tl, envir = .GlobalEnv)
        return(resultado)
      }
  #  }
    
      
  
          
        # Atualizando seletores MODIFICADO EM 11/06/2025
        
        
        observe({
          df_tl <- js_div_data()
          req(df_tl)
          
          
          
          
          updateSelectInput(session, "classe_detalhe", choices = unique(df_tl$Classe))
          updateSelectInput(session, "variavel_detalhe", choices = unique(df_tl$Variável))
        })
        
        
        
        # Atualizando seletores MODIFICADO EM 11/06/2025      
        
        # Gráfico principal JS
        
        output$js_divergence_plot <- renderPlotly({
          df_tl <- js_div_data()
          req(df_tl)
          
          if (is.null(df_tl) || nrow(df_tl) == 0) {
            return(plotly_empty(type = "scatter", mode = "markers") %>%
                     layout(title = "Sem divergência calculada para as classes selecionadas"))
          }
          
          df_wide <- df_tl %>%
            dplyr::select(Classe, Variável, Divergência) %>%
            tidyr::pivot_wider(names_from = Classe, values_from = Divergência)
          
          df_wide$Variável <- as.character(df_wide$Variável)
          
          if (ncol(df_wide) == 1) {
            return(plotly_empty(type = "scatter", mode = "markers") %>%
                     layout(title = "Nenhuma divergência calculada para as Variables"))
          }
          
          classe_cols <- setdiff(names(df_wide), "Variável")
          classe_cols_validas <- classe_cols[classe_cols %in% levels(df_tl$Classe)]
          
          if (length(classe_cols_validas) == 0) {
            return(plotly_empty(type = "scatter", mode = "lines") %>%
                     layout(title = "Nenhuma divergência foi calculada."))
          }
          
          df_wide <- df_wide[, c("Variável", classe_cols_validas)]
          
          variaveis <- df_wide$Variável
          classe_cols <- setdiff(names(df_wide), "Variável")
          
          n_classes <- length(classe_cols)
          cores <- RColorBrewer::brewer.pal(pmin(n_classes, 8), "Set1")
          
          plot <- plot_ly()
          for (i in seq_along(classe_cols)) {
            classe_nome <- classe_cols[i]
            
            if (!(classe_nome %in% names(df_wide))) next
            
            comparado_com <- ifelse(any(df_tl$Classe == classe_nome),
                                    df_tl$Comparado_com[df_tl$Classe == classe_nome][1],
                                    "Indefinido")
            
            plot <- plot %>%
              add_trace(
                x = variaveis,
                y = df_wide[[classe_nome]],
                type = "scatter",
                mode = "lines+markers",
                name = classe_nome,
                line = list(width = 2),
                marker = list(size = 6),
                hoverinfo = "text",
                text = paste0(
                  "<b>Class:</b> ", classe_nome,
                  "<br><b>Compared to:</b> Synthetic and Removed",
                  "<br><b>Variable:</b> ", variaveis,
                  "<br><b>JS Divergence:</b> ", ifelse(is.na(df_wide[[classe_nome]]), "NA", round(as.numeric(df_wide[[classe_nome]]), 4))
                ),
                color = I(cores[(i - 1) %% length(cores) + 1])
              )
          }
          
          plot %>% layout(
            title = list(text = "Jensen-Shannon divergence: Original vs. Synthetic and Removed", font = list(size = 18)),
            xaxis = list(title = "Variable", tickangle = -45),
            yaxis = list(title = "JS Divergence", range = c(0, 1)),
            legend = list(title = list(text = " ")),
            plot_bgcolor = "#ffffff",
            hoverlabel = list(bgcolor = "white", font = list(size = 12))
          )
                    
          
          
        })  
        
      
  #  }  
      
     
      
    }    ############# FECHANDO SMOTE TL
    
################################################BLOCO ADICIONADO EM 04/07/2025
    else if (isTRUE(runSMOTEENN()) && !is.null(SMOTEENN_Data()) && !is.null(input$target_enn2)) {
      original_data <- as.data.frame(filedata())
      original_data$Tipo <- "Original"
      class_col <- input$target_enn2
      added_data <- SMOTEENN_Data()$syn_data
      removed_data <- SMOTEENN_Data()$removed_data
      
      if (is.null(added_data) || nrow(added_data) == 0) {
        added_data <- original_data[0, , drop = FALSE]
      }
      if (is.null(removed_data) || nrow(removed_data) == 0) {
        removed_data <- original_data[0, , drop = FALSE]
      }
      
      names(original_data)[names(original_data) == class_col] <- "class"
      names(added_data)[names(added_data) == class_col] <- "class"
      names(removed_data)[names(removed_data) == class_col] <- "class"
      
      if (nrow(added_data) > 0) {
        added_data$class <- paste(added_data$class, "Synthetic")
        added_data$Tipo <- "Synthetic"
      }
      
      if (nrow(removed_data) > 0) {
        removed_data$class <- paste(removed_data$class, "Removed") #ALTERADO EM 29/06/2025
        removed_data$Tipo <- "Removed" #ALTERADO EM 29/06/2025
      }
      
      df_enn2 <- dplyr::bind_rows(original_data, added_data, removed_data)
      if (nrow(df_enn2) == 0) return(NULL)
      
      df_enn2$class <- as.character(df_enn2$class)
      df_enn2$Tipo <- as.character(df_enn2$Tipo)
      df_enn2$classe_base <- gsub(" Synthetic$", "", gsub(" Removed$", "", df_enn2$class)) #ALTERADO EM 29/06/2025
      
      classes <- unique(df_enn2$classe_base)
      variaveis <- setdiff(names(df_enn2), c("class", "Tipo", "classe_base"))
      if (length(variaveis) == 0) return(NULL)
      
      resultado <- data.frame()
      for (cl in classes) {
        df_o <- df_enn2 %>% dplyr::filter(classe_base == cl & Tipo == "Original")
        df_a <- df_enn2 %>% dplyr::filter(classe_base == cl & Tipo == "Synthetic")
        df_r <- df_enn2 %>% dplyr::filter(classe_base == cl & Tipo == "Removed") #ALTERADO EM 29/06/2025
        
        grupo_comparado <- df_o
        if (nrow(df_a) > 0) {
          grupo_comparado <- dplyr::bind_rows(grupo_comparado, df_a)
        }
        if (nrow(df_r) > 0) {
          grupo_comparado <- dplyr::anti_join(grupo_comparado, df_r, by = intersect(names(df_o), names(df_r)))
        }
        
        if (nrow(df_o) >= 1 && nrow(grupo_comparado) >= 1 && all(sapply(df_o[variaveis], is.numeric)) && all(sapply(grupo_comparado[variaveis], is.numeric))) {
          for (var in variaveis) {
            if (length(df_o[[var]]) >= 1 && length(grupo_comparado[[var]]) >= 1) {
              js <- js_divergence(df_o[[var]], grupo_comparado[[var]])
              if (!is.na(js)) {
                resultado <- rbind(resultado, data.frame(Classe = cl, Variável = var, Divergência = js))
              }
            }
          }
        }
      }
      
      if (nrow(resultado) > 0) {
        resultado$Comparado_com <- "Synthetic and/or Removed" #ALTERADO EM 29/06/2025
        resultado <- resultado[!is.na(resultado$Divergência), ]
        if (nrow(resultado) == 0) return(NULL)
        resultado$Classe <- factor(resultado$Classe, levels = unique(df_enn2$classe_base))
        resultado$Variável <- factor(resultado$Variável, levels = unique(resultado$Variável))
        assign("df_enn2_js_distributions", df_enn2, envir = .GlobalEnv)
        return(resultado)
      }
      #  }
      
      
      
      
      # Atualizando seletores MODIFICADO EM 11/06/2025
      
      
      observe({
        df_enn2 <- js_div_data()
        req(df_enn2)
        
        
        
        
        updateSelectInput(session, "classe_detalhe", choices = unique(df_enn2$Classe))
        updateSelectInput(session, "variavel_detalhe", choices = unique(df_enn2$Variável))
      })
      
      
      
      # Atualizando seletores MODIFICADO EM 11/06/2025      
      
      # Gráfico principal JS
      
      output$js_divergence_plot <- renderPlotly({
        df_enn2 <- js_div_data()
        req(df_enn2)
        
        if (is.null(df_enn2) || nrow(df_enn2) == 0) {
          return(plotly_empty(type = "scatter", mode = "markers") %>%
                   layout(title = "Sem divergência calculada para as classes selecionadas"))
        }
        
        df_wide <- df_enn2 %>%
          dplyr::select(Classe, Variável, Divergência) %>%
          tidyr::pivot_wider(names_from = Classe, values_from = Divergência)
        
        df_wide$Variável <- as.character(df_wide$Variável)
        
        if (ncol(df_wide) == 1) {
          return(plotly_empty(type = "scatter", mode = "markers") %>%
                   layout(title = "Nenhuma divergência calculada para as Variables"))
        }
        
        classe_cols <- setdiff(names(df_wide), "Variável")
        classe_cols_validas <- classe_cols[classe_cols %in% levels(df_enn2$Classe)]
        
        if (length(classe_cols_validas) == 0) {
          return(plotly_empty(type = "scatter", mode = "lines") %>%
                   layout(title = "Nenhuma divergência foi calculada."))
        }
        
        df_wide <- df_wide[, c("Variável", classe_cols_validas)]
        
        variaveis <- df_wide$Variável
        classe_cols <- setdiff(names(df_wide), "Variável")
        
        n_classes <- length(classe_cols)
        cores <- RColorBrewer::brewer.pal(pmin(n_classes, 8), "Set1")
        
        plot <- plot_ly()
        for (i in seq_along(classe_cols)) {
          classe_nome <- classe_cols[i]
          
          if (!(classe_nome %in% names(df_wide))) next
          
          comparado_com <- ifelse(any(df_enn2$Classe == classe_nome),
                                  df_enn2$Comparado_com[df_enn2$Classe == classe_nome][1],
                                  "Indefinido")
          
          plot <- plot %>%
            add_trace(
              x = variaveis,
              y = df_wide[[classe_nome]],
              type = "scatter",
              mode = "lines+markers",
              name = classe_nome,
              line = list(width = 2),
              marker = list(size = 6),
              hoverinfo = "text",
              text = paste0(
                "<b>Class:</b> ", classe_nome,
                "<br><b>Compared to:</b> Synthetic and Removed",
                "<br><b>Variable:</b> ", variaveis,
                "<br><b>JS Divergence:</b> ", ifelse(is.na(df_wide[[classe_nome]]), "NA", round(as.numeric(df_wide[[classe_nome]]), 4))
              ),
              color = I(cores[(i - 1) %% length(cores) + 1])
            )
        }
        
        plot %>% layout(
          title = list(text = "Jensen-Shannon divergence: Original vs. Synthetic and Removed", font = list(size = 18)),
          xaxis = list(title = "Variable", tickangle = -45),
          yaxis = list(title = "JS Divergence", range = c(0, 1)),
          legend = list(title = list(text = " ")),
          plot_bgcolor = "#ffffff",
          hoverlabel = list(bgcolor = "white", font = list(size = 12))
        )
        
        
        
      })  
      
      
      #  }  
      
      
      
    }    ############# FECHANDO SMOTE ENN    
    
    
    
    
    
    
################################################BLOCO ADICIONADO EM 04/07/2025    
    

    ################################################BLOCO ADICIONADO EM 13/07/2025
    else if (isTRUE(runSMOTEIPF()) && !is.null(SMOTEIPF_Data()) && !is.null(input$target_ipf)) {
      original_data <- as.data.frame(filedata())
      original_data$Tipo <- "Original"
      class_col <- input$target_ipf
      added_data <- SMOTEIPF_Data()$syn_data
      removed_data <- SMOTEIPF_Data()$removed_data
      
      if (is.null(added_data) || nrow(added_data) == 0) {
        added_data <- original_data[0, , drop = FALSE]
      }
      if (is.null(removed_data) || nrow(removed_data) == 0) {
        removed_data <- original_data[0, , drop = FALSE]
      }
      
      names(original_data)[names(original_data) == class_col] <- "class"
      names(added_data)[names(added_data) == class_col] <- "class"
      names(removed_data)[names(removed_data) == class_col] <- "class"
      
      if (nrow(added_data) > 0) {
        added_data$class <- paste(added_data$class, "Synthetic")
        added_data$Tipo <- "Synthetic"
      }
      
      if (nrow(removed_data) > 0) {
        removed_data$class <- paste(removed_data$class, "Removed") #ALTERADO EM 29/06/2025
        removed_data$Tipo <- "Removed" #ALTERADO EM 29/06/2025
      }
      
      df_ipf <- dplyr::bind_rows(original_data, added_data, removed_data)
      if (nrow(df_ipf) == 0) return(NULL)
      
      df_ipf$class <- as.character(df_ipf$class)
      df_ipf$Tipo <- as.character(df_ipf$Tipo)
      df_ipf$classe_base <- gsub(" Synthetic$", "", gsub(" Removed$", "", df_ipf$class)) #ALTERADO EM 29/06/2025
      
      classes <- unique(df_ipf$classe_base)
      variaveis <- setdiff(names(df_ipf), c("class", "Tipo", "classe_base"))
      if (length(variaveis) == 0) return(NULL)
      
      resultado <- data.frame()
      for (cl in classes) {
        df_o <- df_ipf %>% dplyr::filter(classe_base == cl & Tipo == "Original")
        df_a <- df_ipf %>% dplyr::filter(classe_base == cl & Tipo == "Synthetic")
        df_r <- df_ipf %>% dplyr::filter(classe_base == cl & Tipo == "Removed") #ALTERADO EM 29/06/2025
        
        grupo_comparado <- df_o
        if (nrow(df_a) > 0) {
          grupo_comparado <- dplyr::bind_rows(grupo_comparado, df_a)
        }
        if (nrow(df_r) > 0) {
          grupo_comparado <- dplyr::anti_join(grupo_comparado, df_r, by = intersect(names(df_o), names(df_r)))
        }
        
        if (nrow(df_o) >= 1 && nrow(grupo_comparado) >= 1 && all(sapply(df_o[variaveis], is.numeric)) && all(sapply(grupo_comparado[variaveis], is.numeric))) {
          for (var in variaveis) {
            if (length(df_o[[var]]) >= 1 && length(grupo_comparado[[var]]) >= 1) {
              js <- js_divergence(df_o[[var]], grupo_comparado[[var]])
              if (!is.na(js)) {
                resultado <- rbind(resultado, data.frame(Classe = cl, Variável = var, Divergência = js))
              }
            }
          }
        }
      }
      
      if (nrow(resultado) > 0) {
        resultado$Comparado_com <- "Synthetic and/or Removed" #ALTERADO EM 29/06/2025
        resultado <- resultado[!is.na(resultado$Divergência), ]
        if (nrow(resultado) == 0) return(NULL)
        resultado$Classe <- factor(resultado$Classe, levels = unique(df_ipf$classe_base))
        resultado$Variável <- factor(resultado$Variável, levels = unique(resultado$Variável))
        assign("df_ipf_js_distributions", df_ipf, envir = .GlobalEnv)
        return(resultado)
      }
      #  }
      
      
      # Atualizando seletores MODIFICADO EM 11/06/2025
      
      observe({
        df_ipf <- js_div_data()
        req(df_ipf)
        
        updateSelectInput(session, "classe_detalhe", choices = unique(df_ipf$Classe))
        updateSelectInput(session, "variavel_detalhe", choices = unique(df_ipf$Variável))
      })
      
      
      # Atualizando seletores MODIFICADO EM 11/06/2025      
      
      # Gráfico principal JS
      
      output$js_divergence_plot <- renderPlotly({
        df_ipf <- js_div_data()
        req(df_ipf)
        
        if (is.null(df_ipf) || nrow(df_ipf) == 0) {
          return(plotly_empty(type = "scatter", mode = "markers") %>%
                   layout(title = "Sem divergência calculada para as classes selecionadas"))
        }
        
        df_wide <- df_ipf %>%
          dplyr::select(Classe, Variável, Divergência) %>%
          tidyr::pivot_wider(names_from = Classe, values_from = Divergência)
        
        df_wide$Variável <- as.character(df_wide$Variável)
        
        if (ncol(df_wide) == 1) {
          return(plotly_empty(type = "scatter", mode = "markers") %>%
                   layout(title = "Nenhuma divergência calculada para as Variables"))
        }
        
        classe_cols <- setdiff(names(df_wide), "Variável")
        classe_cols_validas <- classe_cols[classe_cols %in% levels(df_ipf$Classe)]
        
        if (length(classe_cols_validas) == 0) {
          return(plotly_empty(type = "scatter", mode = "lines") %>%
                   layout(title = "Nenhuma divergência foi calculada."))
        }
        
        df_wide <- df_wide[, c("Variável", classe_cols_validas)]
        
        variaveis <- df_wide$Variável
        classe_cols <- setdiff(names(df_wide), "Variável")
        
        n_classes <- length(classe_cols)
        cores <- RColorBrewer::brewer.pal(pmin(n_classes, 8), "Set1")
        
        plot <- plot_ly()
        for (i in seq_along(classe_cols)) {
          classe_nome <- classe_cols[i]
          
          if (!(classe_nome %in% names(df_wide))) next
          
          comparado_com <- ifelse(any(df_ipf$Classe == classe_nome),
                                  df_ipf$Comparado_com[df_ipf$Classe == classe_nome][1],
                                  "Indefinido")
          
          plot <- plot %>%
            add_trace(
              x = variaveis,
              y = df_wide[[classe_nome]],
              type = "scatter",
              mode = "lines+markers",
              name = classe_nome,
              line = list(width = 2),
              marker = list(size = 6),
              hoverinfo = "text",
              text = paste0(
                "<b>Class:</b> ", classe_nome,
                "<br><b>Compared to:</b> Synthetic and Removed",
                "<br><b>Variable:</b> ", variaveis,
                "<br><b>JS Divergence:</b> ", ifelse(is.na(df_wide[[classe_nome]]), "NA", round(as.numeric(df_wide[[classe_nome]]), 4))
              ),
              color = I(cores[(i - 1) %% length(cores) + 1])
            )
        }
        
        plot %>% layout(
          title = list(text = "Jensen-Shannon divergence: Original vs. Synthetic and Removed", font = list(size = 18)),
          xaxis = list(title = "Variable", tickangle = -45),
          yaxis = list(title = "JS Divergence", range = c(0, 1)),
          legend = list(title = list(text = " ")),
          plot_bgcolor = "#ffffff",
          hoverlabel = list(bgcolor = "white", font = list(size = 12))
        )
      })  
    } ############# FECHANDO SMOTE IPF
    ################################################BLOCO ADICIONADO EM 13/07/2025
    
  
    ################################################ BLOCO SPIDER ADAPTADO EM 14/07/2025
    else if (isTRUE(runSPIDER()) && !is.null(SPIDER_Data()) && !is.null(input$target_spider)) {
      original_data <- as.data.frame(filedata())
      original_data$Tipo <- "Original"
      class_col <- input$target_spider
      added_data <- SPIDER_Data()$syn_data
      removed_data <- SPIDER_Data()$removed_data
      
      if (is.null(added_data) || nrow(added_data) == 0) {
        added_data <- original_data[0, , drop = FALSE]
      }
      if (is.null(removed_data) || nrow(removed_data) == 0) {
        removed_data <- original_data[0, , drop = FALSE]
      }
      
      names(original_data)[names(original_data) == class_col] <- "class"
      names(added_data)[names(added_data) == class_col] <- "class"
      names(removed_data)[names(removed_data) == class_col] <- "class"
      
      if (nrow(added_data) > 0) {
        added_data$class <- paste(added_data$class, "Synthetic")
        added_data$Tipo <- "Synthetic"
      }
      
      if (nrow(removed_data) > 0) {
        removed_data$class <- paste(removed_data$class, "Removed") #ALTERADO EM 29/06/2025
        removed_data$Tipo <- "Removed" #ALTERADO EM 29/06/2025
      }
      
      df_spider <- dplyr::bind_rows(original_data, added_data, removed_data)
      if (nrow(df_spider) == 0) return(NULL)
      
      df_spider$class <- as.character(df_spider$class)
      df_spider$Tipo <- as.character(df_spider$Tipo)
      df_spider$classe_base <- gsub(" Synthetic$", "", gsub(" Removed$", "", df_spider$class)) #ALTERADO EM 29/06/2025
      
      classes <- unique(df_spider$classe_base)
      variaveis <- setdiff(names(df_spider), c("class", "Tipo", "classe_base"))
      if (length(variaveis) == 0) return(NULL)
      
      resultado <- data.frame()
      for (cl in classes) {
        df_o <- df_spider %>% dplyr::filter(classe_base == cl & Tipo == "Original")
        df_a <- df_spider %>% dplyr::filter(classe_base == cl & Tipo == "Synthetic")
        df_r <- df_spider %>% dplyr::filter(classe_base == cl & Tipo == "Removed") #ALTERADO EM 29/06/2025
        
        grupo_comparado <- df_o
        if (nrow(df_a) > 0) {
          grupo_comparado <- dplyr::bind_rows(grupo_comparado, df_a)
        }
        if (nrow(df_r) > 0) {
          grupo_comparado <- dplyr::anti_join(grupo_comparado, df_r, by = intersect(names(df_o), names(df_r)))
        }
        
        if (nrow(df_o) >= 1 && nrow(grupo_comparado) >= 1 && all(sapply(df_o[variaveis], is.numeric)) && all(sapply(grupo_comparado[variaveis], is.numeric))) {
          for (var in variaveis) {
            if (length(df_o[[var]]) >= 1 && length(grupo_comparado[[var]]) >= 1) {
              js <- js_divergence(df_o[[var]], grupo_comparado[[var]])
              if (!is.na(js)) {
                resultado <- rbind(resultado, data.frame(Classe = cl, Variável = var, Divergência = js))
              }
            }
          }
        }
      }
      
      if (nrow(resultado) > 0) {
        resultado$Comparado_com <- "Synthetic and/or Removed" #ALTERADO EM 29/06/2025
        resultado <- resultado[!is.na(resultado$Divergência), ]
        if (nrow(resultado) == 0) return(NULL)
        resultado$Classe <- factor(resultado$Classe, levels = unique(df_spider$classe_base))
        resultado$Variável <- factor(resultado$Variável, levels = unique(resultado$Variável))
        assign("df_spider_js_distributions", df_spider, envir = .GlobalEnv)
        return(resultado)
      }
      
      # Atualizando seletores MODIFICADO EM 11/06/2025
      observe({
        df_spider <- js_div_data()
        req(df_spider)
        
        updateSelectInput(session, "classe_detalhe", choices = unique(df_spider$Classe))
        updateSelectInput(session, "variavel_detalhe", choices = unique(df_spider$Variável))
      })
      
      # Gráfico principal JS
      output$js_divergence_plot <- renderPlotly({
        df_spider <- js_div_data()
        req(df_spider)
        
        if (is.null(df_spider) || nrow(df_spider) == 0) {
          return(plotly_empty(type = "scatter", mode = "markers") %>%
                   layout(title = "Sem divergência calculada para as classes selecionadas"))
        }
        
        df_wide <- df_spider %>%
          dplyr::select(Classe, Variável, Divergência) %>%
          tidyr::pivot_wider(names_from = Classe, values_from = Divergência)
        
        df_wide$Variável <- as.character(df_wide$Variável)
        
        if (ncol(df_wide) == 1) {
          return(plotly_empty(type = "scatter", mode = "markers") %>%
                   layout(title = "Nenhuma divergência calculada para as Variables"))
        }
        
        classe_cols <- setdiff(names(df_wide), "Variável")
        classe_cols_validas <- classe_cols[classe_cols %in% levels(df_spider$Classe)]
        
        if (length(classe_cols_validas) == 0) {
          return(plotly_empty(type = "scatter", mode = "lines") %>%
                   layout(title = "Nenhuma divergência foi calculada."))
        }
        
        df_wide <- df_wide[, c("Variável", classe_cols_validas)]
        
        variaveis <- df_wide$Variável
        classe_cols <- setdiff(names(df_wide), "Variável")
        
        n_classes <- length(classe_cols)
        cores <- RColorBrewer::brewer.pal(pmin(n_classes, 8), "Set1")
        
        plot <- plot_ly()
        for (i in seq_along(classe_cols)) {
          classe_nome <- classe_cols[i]
          
          if (!(classe_nome %in% names(df_wide))) next
          
          plot <- plot %>%
            add_trace(
              x = variaveis,
              y = df_wide[[classe_nome]],
              type = "scatter",
              mode = "lines+markers",
              name = classe_nome,
              line = list(width = 2),
              marker = list(size = 6),
              hoverinfo = "text",
              text = paste0(
                "<b>Class:</b> ", classe_nome,
                "<br><b>Compared to:</b> Synthetic and Removed",
                "<br><b>Variable:</b> ", variaveis,
                "<br><b>JS Divergence:</b> ", ifelse(is.na(df_wide[[classe_nome]]), "NA", round(as.numeric(df_wide[[classe_nome]]), 4))
              ),
              color = I(cores[(i - 1) %% length(cores) + 1])
            )
        }
        
        plot %>% layout(
          title = list(text = "Jensen-Shannon divergence: Original vs. Synthetic and Removed", font = list(size = 18)),
          xaxis = list(title = "Variable", tickangle = -45),
          yaxis = list(title = "JS Divergence", range = c(0, 1)),
          legend = list(title = list(text = " ")),
          plot_bgcolor = "#ffffff",
          hoverlabel = list(bgcolor = "white", font = list(size = 12))
        )
      })
    } ############# FECHANDO SPIDER
    ################################################ BLOCO SPIDER ADAPTADO EM 14/07/2025
    
      
    
    
    
    

    #########ADICIONADO EM 17/06/2025   
    
    
   else {
      return(NULL)
      
    }
    
    
    
    if (is.null(smote_data) || ncol(smote_data) == 0 || is.null(class_col) || !(class_col %in% names(original_data))) {
      return(NULL)
    }
    
    names(original_data)[names(original_data) == class_col] <- "class"
    names(smote_data)[names(smote_data) == class_col] <- "class"
    smote_data$fake_cat <- NULL
    
    #########MODIFICADO EM 11/06/2025
    
    #smote_data$class <- paste(smote_data$class, "Synthetic")
    suffix <- if (isTRUE(runTomek()) || isTRUE(runNearMiss()) 
                  || isTRUE(runENN())  || isTRUE(runOSS())  || isTRUE(runRD()) || isTRUE(runSBC())     ) "Removed" else "Synthetic" #ALTERADO EM 10/07/2025
    smote_data$class <- paste(smote_data$class, suffix)
    
    #########MODIFICADO EM 11/06/2025
    
    
    feature_cols <- intersect(names(original_data), names(smote_data))
    original_data <- original_data[, feature_cols, drop = FALSE]
    smote_data <- smote_data[, feature_cols, drop = FALSE]
    
    original_data$Tipo <- "Original"
    
    
    #########MODIFICADO EM 11/06/2025
    
    #  smote_data$Tipo <- "Synthetic"
    
    # if (isTRUE(runTomek())) {
    if (isTRUE(runTomek()) || isTRUE(runNearMiss()) || isTRUE(runENN())  || isTRUE(runOSS())  || isTRUE(runRD()) || isTRUE(runSBC()) ) {
      smote_data$Tipo <- "Removed"
    } else {
      smote_data$Tipo <- "Synthetic"
    }
    #########MODIFICADO EM 11/06/2025
    
    
    df <- dplyr::bind_rows(original_data, smote_data)
    
    
    #BLOCO ADICIONADO EM 11/06/2025
    # Reconverte todas Variables para numéricas (exceto class, Tipo, classe_base)
    #  num_vars <- setdiff(names(df), c("class", "Tipo", "classe_base"))
    # df[num_vars] <- lapply(df[num_vars], function(col) suppressWarnings(as.numeric(col)))
    
    #BLOCO ADICIONADO EM 11/06/2025
    
    
    
    df$class <- as.character(df$class)
    df$Tipo <- as.character(df$Tipo)
    
    #########MODIFICADO EM 11/06/2025
    
    # df$classe_base <- gsub(" Synthetic$", "", df$class)
    df$classe_base <- gsub(" Synthetic$| Removed$", "", df$class) #ALTERADO EM 29/06/2025
    #########MODIFICADO EM 11/06/2025
    
    
    
    # Adaptação para o método usado
    
    #   if (isTRUE(runSMOTENC())) {
    #    classes <- unique(smote_data$class)
    #    classes <- gsub(" Synthetic$", "", classes)
    #  } else {
    #    classes <- unique(df$classe_base)
    #  }
    
    
    #  if (isTRUE(runBorderlineSMOTE())) {
    #    classes <- unique(smote_data$class)
    #    classes <- gsub(" Synthetic$", "", classes)
    #  } else {
    #    classes <- unique(df$classe_base)
    #  } 
    
    
    #  if (isTRUE(runSVM_SMOTE())) {
    #    classes <- unique(smote_data$class)
    #    classes <- gsub(" Synthetic$", "", classes)
    #  } else {
    #    classes <- unique(df$classe_base)
    #  }
    
    
    
    #  if (isTRUE(runADASYN())) {
    #    classes <- unique(smote_data$class)
    #    classes <- gsub(" Synthetic$", "", classes)
    #  } else {
    #    classes <- unique(df$classe_base)
    #  }
    
    
    
    #  if (isTRUE(runRU())) {
    #    classes <- unique(smote_data$class)
    #    classes <- gsub(" Synthetic$", "", classes)
    #  } else {
    #    classes <- unique(df$classe_base)
    #  }
    
    classes <- unique(df$classe_base) #LINHA ADICIONADA EM 11/06/2025
    
    
    variaveis <- setdiff(names(df), c("class", "Tipo", "classe_base"))
    if (length(variaveis) == 0) return(NULL)
    
    resultado <- data.frame()
    for (cl in classes) {
      df_o <- df %>% dplyr::filter(classe_base == cl & Tipo == "Original")
      
      #####MODIFICADO EM 11/06/2025
      
      #df_s <- df %>% dplyr::filter(classe_base == cl & Tipo == "Synthetic")
      
      ## tipo_sintetico <- if (isTRUE(runTomek())) "Removed" else "Synthetic"
      tipo_sintetico <- if (isTRUE(runTomek()) || isTRUE(runNearMiss()) 
                            || isTRUE(runENN()) || isTRUE(runOSS()) || isTRUE(runRD()) || isTRUE(runSBC()) ) "Removed" else "Synthetic"
      
      df_s <- df %>% dplyr::filter(classe_base == cl & Tipo == tipo_sintetico)
      
      
      #####MODIFICADO EM 11/06/2025
      
      
      
      # if (nrow(df_o) > 1 && nrow(df_s) > 1) {
      ##   if (nrow(df_o) >= 1 && nrow(df_s) >= 1) {
      
      if (nrow(df_o) >= 1 && nrow(df_s) >= 1 && all(sapply(df_o[variaveis], is.numeric)) && all(sapply(df_s[variaveis], is.numeric))) {
        
        
        for (var in variaveis) {
          ## js <- js_divergence(df_o[[var]], df_s[[var]])
          ##  resultado <- rbind(resultado, data.frame(Classe = cl, Variável = var, Divergência = js))
          
          if (length(df_o[[var]]) >= 1 && length(df_s[[var]]) >= 1) {
            js <- js_divergence(df_o[[var]], df_s[[var]])
            if (!is.na(js)) {
              resultado <- rbind(resultado, data.frame(Classe = cl, Variável = var, Divergência = js))
            }
          }
          
          
          
        }
      }
    }
    
    
    ########BLOCO ADICIONADO EM 11/06/2025
    
    if (nrow(resultado) > 0) {  #LINHA ADICIONADA EM 20/06/2025
      resultado$Comparado_com <- NA_character_ #LINHA ADICIONADA EM 20/06/2025
      
      # resultado$Comparado_com <- NA  #LINHA OMITIDA EM 20/06/2025
      
      
      
      for (i in seq_len(nrow(resultado))) {
        classe_i <- as.character(resultado$Classe[i])
        tipos <- unique(df$Tipo[df$classe_base == classe_i & df$Tipo != "Original"])
        if ("Removed" %in% tipos) {
          resultado$Comparado_com[i] <- "Removed" #ALTERADO EM 29/06/2025
        } else if ("Synthetic" %in% tipos) {
          resultado$Comparado_com[i] <- "Synthetic"
        } else {
          resultado$Comparado_com[i] <- "Indefinido"
        }
      }
      
    }  #LINHA ADICIONADA EM 20/06/2025 
    
    ########BLOCO ADICIONADO EM 11/06/2025
    
    
    resultado <- resultado[!is.na(resultado$Divergência), ]
    if (nrow(resultado) == 0) return(NULL)
    
    # resultado$Classe <- factor(resultado$Classe)
    
    # Garantindo que todas as classes com pelo menos 1 comparação apareçam no seletor
    resultado$Classe <- factor(resultado$Classe, levels = unique(df$classe_base)) #MODIFICADO 11/06/2025
    
    
    resultado$Variável <- factor(resultado$Variável, levels = unique(resultado$Variável))
    resultado
  })
  
  # Atualizando seletores MODIFICADO EM 11/06/2025
  
  ## observe({
  ##    df <- js_div_data()
  #  req(df)
  
  
  ##    if (!is.null(df)) {
  ##    updateSelectInput(session, "classe_detalhe", choices = unique(df$Classe))
  ##    updateSelectInput(session, "variavel_detalhe", choices = unique(df$Variável))
  ##    }
  ##  })
  
  
  ##  observe({
  ##    df <- js_div_data()
  ##    req(filedata(), input$target_tomek)
  
  ##    # Pegando todas as classes do original (mesmo que não entrem no resultado)
  ##    todas_classes <- unique(as.character(filedata()[[input$target_tomek]]))
  ##    todas_classes[is.na(todas_classes)] <- "Classe_Nula"
  
  ##    variaveis <- if (!is.null(df)) unique(df$Variável) else names(filedata())
  
  ##    updateSelectInput(session, "classe_detalhe", choices = todas_classes)
  ##    updateSelectInput(session, "variavel_detalhe", choices = variaveis)
  ##  })
  
  observe({
    df <- js_div_data()
    req(df)
    
    updateSelectInput(session, "classe_detalhe", choices = unique(df$Classe))
    updateSelectInput(session, "variavel_detalhe", choices = unique(df$Variável))
  })
  
  
  
  
  # Atualizando seletores MODIFICADO EM 11/06/2025  
  
  
  
  # Gráfico principal JS
  
  output$js_divergence_plot <- renderPlotly({
    df <- js_div_data()
    req(df)
    
    
    ###########BLOCO ADICIONADO EM 11/06/2025
    
    if (is.null(df) || nrow(df) == 0) {
      return(plotly_empty(type = "scatter", mode = "markers") %>%
               layout(title = "Sem divergência calculada para as classes selecionadas"))
    }
    
    
    ###########BLOCO ADICIONADO EM 11/06/2025
    
    
    # df_wide <- tidyr::pivot_wider(df, names_from = Classe, values_from = Divergência)
    
    df_wide <- df %>%
      dplyr::select(Classe, Variável, Divergência) %>%
      tidyr::pivot_wider(names_from = Classe, values_from = Divergência)
    
    
    df_wide$Variável <- as.character(df_wide$Variável)
    
    
    # Removendo colunas totalmente NA (classes sem divergência)
    
    
    if (ncol(df_wide) == 1) {
      return(plotly_empty(type = "scatter", mode = "markers") %>%
               layout(title = "Nenhuma divergência calculada para as Variables"))
    }
    
    
    
    #df_wide <- df_wide[, c("Variável", which(colSums(!is.na(df_wide)) > 0))] #linha adicionada em 11/06/2025
    
    
    classe_cols <- setdiff(names(df_wide), "Variável")
    #classe_cols_validas <- classe_cols[colSums(!is.na(df_wide[classe_cols])) > 0]
    
    classe_cols_validas <- classe_cols[classe_cols %in% levels(df$Classe)]  # apenas classes válidas
    
    
    
    if (length(classe_cols_validas) == 0) {
      return(plotly_empty(type = "scatter", mode = "lines") %>%
               layout(title = "Nenhuma divergência foi calculada."))
    }
    
    df_wide <- df_wide[, c("Variável", classe_cols_validas)]
    
    
    
    variaveis <- df_wide$Variável
    ##classe_cols <- names(df_wide)[-1]
    classe_cols <- setdiff(names(df_wide), "Variável")
    
    #cores <- RColorBrewer::brewer.pal(max(3, length(classe_cols)), "Set1")
    
    n_classes <- length(classe_cols)
    cores <- RColorBrewer::brewer.pal(pmin(n_classes, 8), "Set1")
    
    
    
    plot <- plot_ly()
    for (i in seq_along(classe_cols)) {
      classe_nome <- classe_cols[i]
      #comparado_com <- if (any(grepl("Removida", classe_nome))) "Removida" else "Synthetic" #ADICIONADA EM 11/06/2025 #ALTERADO EM 29/06/2025
      ## tipo_classe <- unique(df$Tipo[df$classe_base == classe_nome & df$Tipo != "Original"])
      ## comparado_com <- tipo_classe[1]
      
      if (!(classe_nome %in% names(df_wide))) next  # linha adicionada em 11/06/2025
      
      #comparado_com <- df$Comparado_com[df$Classe == classe_nome][1]
      
      
      
      comparado_com <- ifelse(any(df$Classe == classe_nome),
                              df$Comparado_com[df$Classe == classe_nome][1],
                              "Indefinido")
      
      
      
      plot <- plot %>%
        add_trace(
          x = variaveis,
          y = df_wide[[classe_nome]],
          type = "scatter",
          mode = "lines+markers",
          name = classe_nome,
          line = list(width = 2),
          marker = list(size = 6),
          hoverinfo = "text",
          text = paste0(
            "<b>Class:</b> ", classe_nome,
            #  "<br><b>Compared to:</b> ", classe_nome, " Synthetic",
            # "<br><b>Compared to:</b> ", classe_nome, " ", comparado_com,
            
            ##  "<br><b>Compared to:</b> ", comparado_com,
            
            "<br><b>Compared to:</b> ", classe_nome , " " , comparado_com,
            
            "<br><b>Variable:</b> ", variaveis,
            
            #"<br><b>JS Divergence:</b> ", round(df_wide[[classe_nome]], 4)
            "<br><b>JS Divergence:</b> ", ifelse(is.na(df_wide[[classe_nome]]), "NA", round(as.numeric(df_wide[[classe_nome]]), 4))
          ),
          color = I(cores[(i - 1) %% length(cores) + 1])
        )
    }
    
    plot %>% layout(
      title = list(text = "Jensen-Shannon divergence: Originals vs. Synthetics", font = list(size = 18)),
      xaxis = list(title = "Variable", tickangle = -45),
      yaxis = list(title = "JS Divergence", range = c(0, 1)),
      legend = list(title = list(text = " ")),
      plot_bgcolor = "#ffffff",
      hoverlabel = list(bgcolor = "white", font = list(size = 12))
    )
  })
  
  # Gráfico auxiliar de distribuições
  
  
  output$js_distributions_plot <- renderPlotly({
    req(js_div_data(), input$classe_detalhe, input$variavel_detalhe)
    
    df <- js_div_data()
    classe <- as.character(input$classe_detalhe)
    variavel <- input$variavel_detalhe
    
    original_data <- as.data.frame(filedata())
    smote_data <- NULL
    class_col <- NULL
    
    
    if (isTRUE(runSMOTENC()) && !is.null(SMOTENC_Data()) && !is.null(input$target_nc)) {
      smote_data <- SMOTENC_Data()$syn_data
      class_col <- input$target_nc
    } else if (isTRUE(runSMOTE()) && !is.null(SMOTE_Data()) && !is.null(input$target)) {
      smote_data <- SMOTE_Data()$syn_data
      class_col <- input$target
    } else if (isTRUE(runBorderlineSMOTE()) && !is.null(BorderlineSMOTE_Data()) && !is.null(input$target_borderline)) {
      smote_data <- BorderlineSMOTE_Data()$syn_data
      class_col <- input$target_borderline
    } else if (isTRUE(runSVM_SMOTE()) && !is.null(SVM_SMOTE_Data()) && !is.null(input$target_svm)) {
      smote_data <- SVM_SMOTE_Data()$syn_data
      class_col <- input$target_svm
    } else if (isTRUE(runADASYN()) && !is.null(ADASYN_Data()) && !is.null(input$target_adasyn)) {
      smote_data <- ADASYN_Data()$syn_data
      class_col <- input$target_adasyn
    } else if (isTRUE(runRU()) && !is.null(RU_Data()) && !is.null(input$target_ru)) {
      smote_data <- RU_Data()$syn_data
      class_col <- input$target_ru
    } else if (isTRUE(runTomek()) && !is.null(Tomek_Data()) && !is.null(input$target_tomek)) {
      smote_data <- Tomek_Data()$removed_data
      class_col <- input$target_tomek
    } else if (isTRUE(runNearMiss()) && !is.null(NearMiss_Data()) && !is.null(input$target_nearmiss)) {
      smote_data <- NearMiss_Data()$removed_data
      class_col <- input$target_nearmiss
    } else if (isTRUE(runENN()) && !is.null(ENN_Data()) && !is.null(input$target_enn)) {
      smote_data <- ENN_Data()$removed_data
      class_col <- input$target_enn
    } else if (isTRUE(runOSS()) && !is.null(OSS_Data()) && !is.null(input$target_oss)) {
      smote_data <- OSS_Data()$removed_data
      class_col <- input$target_oss
    } else if (isTRUE(runRD()) && !is.null(RD_Data()) && !is.null(input$target_rd)) {
      smote_data <- RD_Data()$removed_data
      class_col <- input$target_rd
      
      ##BLOCO ADICIONADO EM 10/07/2025
      
    } else if (isTRUE(runSBC()) && !is.null(SBC_Data()) && !is.null(input$target_sbc)) {
      smote_data <- SBC_Data()$removed_data
      class_col <- input$target_sbc
      
      ##BLOCO ADICIONADO EM 10/07/2025
      
    }
    
    ##### SMOTE-Tomek Links #####
    
    if (isTRUE(runSMOTETL()) && !is.null(SMOTETL_Data()) && !is.null(input$target_tl)) {
      class_col <- input$target_tl
      smote_data_added <- SMOTETL_Data()$syn_data
      smote_data_removed <- SMOTETL_Data()$removed_data
      
      names(original_data)[names(original_data) == class_col] <- "class"
      original_data$Tipo <- "Original"
      
      if (!is.null(smote_data_added) && nrow(smote_data_added) > 0) {
        names(smote_data_added)[names(smote_data_added) == class_col] <- "class"
        smote_data_added$class <- paste(smote_data_added$class, "Synthetic")
        smote_data_added$Tipo <- "Synthetic"
      } else {
        smote_data_added <- original_data[0, , drop = FALSE]
      }
      
      if (!is.null(smote_data_removed) && nrow(smote_data_removed) > 0) {
        names(smote_data_removed)[names(smote_data_removed) == class_col] <- "class"
        smote_data_removed$class <- paste(smote_data_removed$class, "Removed") #ALTERADO EM 29/06/2025
        smote_data_removed$Tipo <- "Removed"
      } else {
        smote_data_removed <- original_data[0, , drop = FALSE]
      }
      
      smote_data <- dplyr::bind_rows(smote_data_added, smote_data_removed)
      feature_cols <- intersect(names(original_data), names(smote_data))
      original_data <- original_data[, feature_cols, drop = FALSE]
      smote_data <- smote_data[, feature_cols, drop = FALSE]
      
      df_full <- dplyr::bind_rows(original_data, smote_data)
      df_full$class <- as.character(df_full$class)
      df_full$classe_base <- gsub(" Synthetic$| Removed$", "", df_full$class) #ALTERADO EM 29/06/2025
      
    } 
    
    ##### SMOTE-Tomek Links #####
    
    ##### SMOTE-ENN ADICIONADO EM 04/07/2025 #####
    
    if (isTRUE(runSMOTEENN()) && !is.null(SMOTEENN_Data()) && !is.null(input$target_enn2)) {
      class_col <- input$target_enn2
      smote_data_added <- SMOTEENN_Data()$syn_data
      smote_data_removed <- SMOTEENN_Data()$removed_data
      
      names(original_data)[names(original_data) == class_col] <- "class"
      original_data$Tipo <- "Original"
      
      if (!is.null(smote_data_added) && nrow(smote_data_added) > 0) {
        names(smote_data_added)[names(smote_data_added) == class_col] <- "class"
        smote_data_added$class <- paste(smote_data_added$class, "Synthetic")
        smote_data_added$Tipo <- "Synthetic"
      } else {
        smote_data_added <- original_data[0, , drop = FALSE]
      }
      
      if (!is.null(smote_data_removed) && nrow(smote_data_removed) > 0) {
        names(smote_data_removed)[names(smote_data_removed) == class_col] <- "class"
        smote_data_removed$class <- paste(smote_data_removed$class, "Removed") #ALTERADO EM 29/06/2025
        smote_data_removed$Tipo <- "Removed"
      } else {
        smote_data_removed <- original_data[0, , drop = FALSE]
      }
      
      smote_data <- dplyr::bind_rows(smote_data_added, smote_data_removed)
      feature_cols <- intersect(names(original_data), names(smote_data))
      original_data <- original_data[, feature_cols, drop = FALSE]
      smote_data <- smote_data[, feature_cols, drop = FALSE]
      
      df_full <- dplyr::bind_rows(original_data, smote_data)
      df_full$class <- as.character(df_full$class)
      df_full$classe_base <- gsub(" Synthetic$| Removed$", "", df_full$class) #ALTERADO EM 29/06/2025
      
    }
    
    ##### SMOTE-ENN ADICIONADO EM 04/07/2025 #####
    

    ##### SMOTE-IPF ADICIONADO EM 13/07/2025 #####
    
    if (isTRUE(runSMOTEIPF()) && !is.null(SMOTEIPF_Data()) && !is.null(input$target_ipf)) {
      class_col <- input$target_ipf
      smote_data_added <- SMOTEIPF_Data()$syn_data
      smote_data_removed <- SMOTEIPF_Data()$removed_data
      
      names(original_data)[names(original_data) == class_col] <- "class"
      original_data$Tipo <- "Original"
      
      if (!is.null(smote_data_added) && nrow(smote_data_added) > 0) {
        names(smote_data_added)[names(smote_data_added) == class_col] <- "class"
        smote_data_added$class <- paste(smote_data_added$class, "Synthetic")
        smote_data_added$Tipo <- "Synthetic"
      } else {
        smote_data_added <- original_data[0, , drop = FALSE]
      }
      
      if (!is.null(smote_data_removed) && nrow(smote_data_removed) > 0) {
        names(smote_data_removed)[names(smote_data_removed) == class_col] <- "class"
        smote_data_removed$class <- paste(smote_data_removed$class, "Removed") #ALTERADO EM 29/06/2025
        smote_data_removed$Tipo <- "Removed"
      } else {
        smote_data_removed <- original_data[0, , drop = FALSE]
      }
      
      smote_data <- dplyr::bind_rows(smote_data_added, smote_data_removed)
      feature_cols <- intersect(names(original_data), names(smote_data))
      original_data <- original_data[, feature_cols, drop = FALSE]
      smote_data <- smote_data[, feature_cols, drop = FALSE]
      
      df_full <- dplyr::bind_rows(original_data, smote_data)
      df_full$class <- as.character(df_full$class)
      df_full$classe_base <- gsub(" Synthetic$| Removed$", "", df_full$class) #ALTERADO EM 29/06/2025
    }
    
    ##### SMOTE-IPF ADICIONADO EM 13/07/2025 #####

    
    ##### SPIDER ADICIONADO EM 14/07/2025 #####
    
    if (isTRUE(runSPIDER()) && !is.null(SPIDER_Data()) && !is.null(input$target_spider)) {
      class_col <- input$target_spider
      smote_data_added <- SPIDER_Data()$syn_data
      smote_data_removed <- SPIDER_Data()$removed_data
      
      names(original_data)[names(original_data) == class_col] <- "class"
      original_data$Tipo <- "Original"
      
      if (!is.null(smote_data_added) && nrow(smote_data_added) > 0) {
        names(smote_data_added)[names(smote_data_added) == class_col] <- "class"
        smote_data_added$class <- paste(smote_data_added$class, "Synthetic")
        smote_data_added$Tipo <- "Synthetic"
      } else {
        smote_data_added <- original_data[0, , drop = FALSE]
      }
      
      if (!is.null(smote_data_removed) && nrow(smote_data_removed) > 0) {
        names(smote_data_removed)[names(smote_data_removed) == class_col] <- "class"
        smote_data_removed$class <- paste(smote_data_removed$class, "Removed") #ALTERADO EM 29/06/2025
        smote_data_removed$Tipo <- "Removed"
      } else {
        smote_data_removed <- original_data[0, , drop = FALSE]
      }
      
      smote_data <- dplyr::bind_rows(smote_data_added, smote_data_removed)
      feature_cols <- intersect(names(original_data), names(smote_data))
      original_data <- original_data[, feature_cols, drop = FALSE]
      smote_data <- smote_data[, feature_cols, drop = FALSE]
      
      df_full <- dplyr::bind_rows(original_data, smote_data)
      df_full$class <- as.character(df_full$class)
      df_full$classe_base <- gsub(" Synthetic$| Removed$", "", df_full$class) #ALTERADO EM 29/06/2025
    }
    
    ##### SPIDER ADICIONADO EM 14/07/2025 #####
    
    
    
    
    
    
     
    else if (!is.null(smote_data) && !is.null(class_col)) {
      names(original_data)[names(original_data) == class_col] <- "class"
      original_data$Tipo <- "Original"
      
      smote_data$class <- paste(smote_data$class, "Synthetic")
      smote_data$Tipo <- "Synthetic"
      
      feature_cols <- intersect(names(original_data), names(smote_data))
      original_data <- original_data[, feature_cols, drop = FALSE]
      smote_data <- smote_data[, feature_cols, drop = FALSE]
      
      df_full <- dplyr::bind_rows(original_data, smote_data)
      df_full$class <- as.character(df_full$class)
      df_full$classe_base <- gsub(" Synthetic$", "", df_full$class)
      
    } else {
      return(plotly_empty(type = "scatter", mode = "markers") %>%
               layout(title = "Sem método de amostragem executado ou dados ausentes"))
    }
    
    df_sub <- df_full %>% dplyr::filter(classe_base == classe)
    if (nrow(df_sub) == 0) {
      return(plotly_empty(type = "scatter", mode = "markers") %>%
               layout(title = paste("Sem dados disponíveis para a classe", classe)))
    }
    
    tipo_var <- if (is.numeric(df_sub[[variavel]])) "numerica" else "categorica"
    tipos_unicos <- unique(df_sub$Tipo)
    
    if (length(tipos_unicos) == 1 && tipos_unicos == "Original") {
      showNotification("Only Original data available for this class", type = "message")
      
      if (tipo_var == "numerica") {
        return(
          plot_ly(df_sub, x = ~get(variavel), type = "histogram", opacity = 0.6, nbinsx = 30) %>%
            layout(title = paste("Distribution of", variavel, "for", classe),
                   xaxis = list(title = variavel),
                   yaxis = list(title = "Count"))
        )
      } else {
        df_counts <- df_sub %>%
          dplyr::group_by(Valor = .data[[variavel]]) %>%
          dplyr::summarise(Freq = n(), .groups = "drop")
        return(
          plot_ly(df_counts, x = ~Valor, y = ~Freq, type = "bar") %>%
            layout(title = paste("Distribution of", variavel, "for", classe),
                   xaxis = list(title = variavel),
                   yaxis = list(title = "Count"))
        )
      }
    } else {
      if (tipo_var == "numerica") {
        return(
          plot_ly(df_sub, x = ~get(variavel), color = ~Tipo, type = "histogram",
                  opacity = 0.6, nbinsx = 30) %>%
            layout(barmode = "overlay",
                   title = paste("Distribution of", variavel, "for", classe),
                   xaxis = list(title = variavel),
                   yaxis = list(title = "Count"))
        )
      } else {
        df_counts <- df_sub %>%
          dplyr::group_by(Tipo, Valor = .data[[variavel]]) %>%
          dplyr::summarise(Freq = n(), .groups = "drop")
        return(
          plot_ly(df_counts, x = ~Valor, y = ~Freq, type = "bar", color = ~Tipo,
                  barmode = "group") %>%
            layout(title = paste("Distribution of", variavel, "for", classe),
                   xaxis = list(title = variavel),
                   yaxis = list(title = "Count"))
        )
      }
    }
  })
  
  
  
  
  #######ACRÉSCIMO DA Jensen-Shannon divergence EM 28/03/2025#####
  ####### Jensen-Shannon divergence – SMOTE + SMOTE-NC - MODIFICADA EM 19/05/2025 ####### 
  
  #ACRÉSCIMO DE BORDERLINE SMOTE EM 05/06/2025
  #ACRÉSCIMO DE SVM SMOTE EM 06/06/2025
  #ACRÉSCIMO DE ADASYN EM 08/06/2025
  #ACRÉSCIMO DE RANDOM UPSAMPLING EM 09/06/2025
  #ACRÉSCIMO DE TOMEK LINKS EM 12/06/2025
  #ACRÉSCIMO DE NEAR MISS EM 16/06/2025
  #ACRÉSCIMO DE ENN EM 17/06/2025
  #ACRÉSCIMO DE OSS EM 18/06/2025
  #ACRÉSCIMO DE RANDOM DOWNSAMPLING EM 18/06/2025
  #ACRÉSCIMO DE SMOTE TL EM 25/06/2025
  #ACRÉSCIMO DE SMOTE ENN EM 04/07/2025
  #ACRÉSCIMO DE SBC EM 10/07/2025
  #ACRÉSCIMO DE SMOTE IPF EM 13/07/2025
  #ACRÉSCIMO DE SPIDER EM 14/07/2025
  
  #######ACRÉSCIMO DE GRÁFICO DE PROCRUSTES GENERALIZADO EM 29/03/2025##  
  
  #######ACRÉSCIMO DE GRÁFICO DE PROCRUSTES GENERALIZADO – SMOTE + SMOTE-NC - MODIFICADA EM 28/05/2025 ##
  #ACRÉSCIMO DE BORDERLINE SMOTE EM 05/06/2025
  #ACRÉSCIMO DE SVM SMOTE EM 06/06/2025
  #ACRÉSCIMO DE ADASYN EM 08/06/2025  
  #ACRÉSCIMO DE RANDOM UPSAMPLING EM 09/06/2025
  #ACRÉSCIMO DE TOMEK LINKS EM 13/06/2025  
  #ACRÉSCIMO DE NEAR MISS EM 16/06/2025  
  #ACRÉSCIMO DE ENN EM 17/06/2025  
  #ACRÉSCIMO DE OSS EM 18/06/2025
  #ACRÉSCIMO DE RANDOM DOWNSAMPLING EM 18/06/2025  
  #ACRÉSCIMO DE SMOTE TL EM 30/06/2025  
  #ACRÉSCIMO DE SMOTE ENN EM 04/07/2025
  #ACRÉSCIMO DE SBC EM 10/07/2025
  #ACRÉSCIMO DE SMOTE IPF EM 13/07/2025
  #ACRÉSCIMO DE SPIDER EM 14/07/2025
  
  output$grafico_procrustes_multiclasse <- renderPlotly({
    req(filedata())
    
    original_data <- as.data.frame(filedata())
    smote_data <- NULL
    class_col <- NULL
    
    
    if (isTRUE(runSMOTETL()) && !is.null(SMOTETL_Data()) && !is.null(input$target_tl)) {
      class_col <- input$target_tl
      smote_data_added <- SMOTETL_Data()$syn_data
      smote_data_removed <- SMOTETL_Data()$removed_data
      
      names(original_data)[names(original_data) == class_col] <- "class"
      original_data$Tipo <- "Original"
      
      if (!is.null(smote_data_added) && nrow(smote_data_added) > 0) {
        names(smote_data_added)[names(smote_data_added) == class_col] <- "class"
        smote_data_added$class <- paste(smote_data_added$class, "Synthetic")
        smote_data_added$Tipo <- "Synthetic"
      } else {
        smote_data_added <- original_data[0, , drop = FALSE]
      }
      
      if (!is.null(smote_data_removed) && nrow(smote_data_removed) > 0) {
        names(smote_data_removed)[names(smote_data_removed) == class_col] <- "class"
        smote_data_removed$class <- paste(smote_data_removed$class, "Removed")
        smote_data_removed$Tipo <- "Removed"
      } else {
        smote_data_removed <- original_data[0, , drop = FALSE]
      }
      
      smote_data <- dplyr::bind_rows(smote_data_added, smote_data_removed)
      
      feature_cols <- intersect(names(original_data), names(smote_data))
      original_data <- original_data[, feature_cols, drop = FALSE]
      smote_data <- smote_data[, feature_cols, drop = FALSE]
      
      dados <- dplyr::bind_rows(original_data, smote_data)
      dados$class <- as.character(dados$class)
      dados$classe_base <- gsub(" Synthetic$| Removed$", "", dados$class)
      dados$SampleID <- 1:nrow(dados)
      
    } 
    
    ##########################BLOCO ADICIONADO EM 04/07/2025
    else {
    
    if (isTRUE(runSMOTEENN()) && !is.null(SMOTEENN_Data()) && !is.null(input$target_enn2)) {
      class_col <- input$target_enn2
      smote_data_added <- SMOTEENN_Data()$syn_data
      smote_data_removed <- SMOTEENN_Data()$removed_data
      
      names(original_data)[names(original_data) == class_col] <- "class"
      original_data$Tipo <- "Original"
      
      if (!is.null(smote_data_added) && nrow(smote_data_added) > 0) {
        names(smote_data_added)[names(smote_data_added) == class_col] <- "class"
        smote_data_added$class <- paste(smote_data_added$class, "Synthetic")
        smote_data_added$Tipo <- "Synthetic"
      } else {
        smote_data_added <- original_data[0, , drop = FALSE]
      }
      
      if (!is.null(smote_data_removed) && nrow(smote_data_removed) > 0) {
        names(smote_data_removed)[names(smote_data_removed) == class_col] <- "class"
        smote_data_removed$class <- paste(smote_data_removed$class, "Removed")
        smote_data_removed$Tipo <- "Removed"
      } else {
        smote_data_removed <- original_data[0, , drop = FALSE]
      }
      
      smote_data <- dplyr::bind_rows(smote_data_added, smote_data_removed)
      
      feature_cols <- intersect(names(original_data), names(smote_data))
      original_data <- original_data[, feature_cols, drop = FALSE]
      smote_data <- smote_data[, feature_cols, drop = FALSE]
      
      dados <- dplyr::bind_rows(original_data, smote_data)
      dados$class <- as.character(dados$class)
      dados$classe_base <- gsub(" Synthetic$| Removed$", "", dados$class)
      dados$SampleID <- 1:nrow(dados)
      
    }
    
    
    ##########################BLOCO ADICIONADO EM 04/07/2025
      
    ##########################BLOCO ADICIONADO EM 13/07/2025
      
      else {
      
        if (isTRUE(runSMOTEIPF()) && !is.null(SMOTEIPF_Data()) && !is.null(input$target_ipf)) {
          class_col <- input$target_ipf
          smote_data_added <- SMOTEIPF_Data()$syn_data
          smote_data_removed <- SMOTEIPF_Data()$removed_data
          
          names(original_data)[names(original_data) == class_col] <- "class"
          original_data$Tipo <- "Original"
          
          if (!is.null(smote_data_added) && nrow(smote_data_added) > 0) {
            names(smote_data_added)[names(smote_data_added) == class_col] <- "class"
            smote_data_added$class <- paste(smote_data_added$class, "Synthetic")
            smote_data_added$Tipo <- "Synthetic"
          } else {
            smote_data_added <- original_data[0, , drop = FALSE]
          }
          
          if (!is.null(smote_data_removed) && nrow(smote_data_removed) > 0) {
            names(smote_data_removed)[names(smote_data_removed) == class_col] <- "class"
            smote_data_removed$class <- paste(smote_data_removed$class, "Removed")
            smote_data_removed$Tipo <- "Removed"
          } else {
            smote_data_removed <- original_data[0, , drop = FALSE]
          }
          
          smote_data <- dplyr::bind_rows(smote_data_added, smote_data_removed)
          
          feature_cols <- intersect(names(original_data), names(smote_data))
          original_data <- original_data[, feature_cols, drop = FALSE]
          smote_data <- smote_data[, feature_cols, drop = FALSE]
          
          dados <- dplyr::bind_rows(original_data, smote_data)
          dados$class <- as.character(dados$class)
          dados$classe_base <- gsub(" Synthetic$| Removed$", "", dados$class)
          dados$SampleID <- 1:nrow(dados)
        }
        
        
      
        ##########################BLOCO ADICIONADO EM 13/07/2025
      
        
    
        ##########################BLOCO ADICIONADO EM 14/07/2025
        
        else {
          
          if (isTRUE(runSPIDER()) && !is.null(SPIDER_Data()) && !is.null(input$target_spider)) {
            class_col <- input$target_spider
            smote_data_added <- SPIDER_Data()$syn_data
            smote_data_removed <- SPIDER_Data()$removed_data
            
            names(original_data)[names(original_data) == class_col] <- "class"
            original_data$Tipo <- "Original"
            
            if (!is.null(smote_data_added) && nrow(smote_data_added) > 0) {
              names(smote_data_added)[names(smote_data_added) == class_col] <- "class"
              smote_data_added$class <- paste(smote_data_added$class, "Synthetic")
              smote_data_added$Tipo <- "Synthetic"
            } else {
              smote_data_added <- original_data[0, , drop = FALSE]
            }
            
            if (!is.null(smote_data_removed) && nrow(smote_data_removed) > 0) {
              names(smote_data_removed)[names(smote_data_removed) == class_col] <- "class"
              smote_data_removed$class <- paste(smote_data_removed$class, "Removed")
              smote_data_removed$Tipo <- "Removed"
            } else {
              smote_data_removed <- original_data[0, , drop = FALSE]
            }
            
            smote_data <- dplyr::bind_rows(smote_data_added, smote_data_removed)
            
            feature_cols <- intersect(names(original_data), names(smote_data))
            original_data <- original_data[, feature_cols, drop = FALSE]
            smote_data <- smote_data[, feature_cols, drop = FALSE]
            
            dados <- dplyr::bind_rows(original_data, smote_data)
            dados$class <- as.character(dados$class)
            dados$classe_base <- gsub(" Synthetic$| Removed$", "", dados$class)
            dados$SampleID <- 1:nrow(dados)
          }
          
        
        
        ##########################BLOCO ADICIONADO EM 14/07/2025
    
        
    else {
      
      if (isTRUE(runSMOTENC()) && !is.null(SMOTENC_Data())) {
        smote_data <- SMOTENC_Data()$syn_data
        class_col <- input$target_nc
        
      } else if (isTRUE(runSMOTE()) && !is.null(SMOTE_Data())) {
        smote_data <- SMOTE_Data()$syn_data
        class_col <- input$target
        
      } else if (isTRUE(runBorderlineSMOTE()) && !is.null(BorderlineSMOTE_Data())) {
        smote_data <- BorderlineSMOTE_Data()$syn_data
        class_col <- input$target_borderline
        
      } else if (isTRUE(runSVM_SMOTE()) && !is.null(SVM_SMOTE_Data())) {
        smote_data <- SVM_SMOTE_Data()$syn_data
        class_col <- input$target_svm
        
      } else if (isTRUE(runADASYN()) && !is.null(ADASYN_Data())) {
        smote_data <- ADASYN_Data()$syn_data
        class_col <- input$target_adasyn
        
      } else if (isTRUE(runRU()) && !is.null(RU_Data())) {
        smote_data <- RU_Data()$syn_data
        class_col <- input$target_ru
        
      } else if (isTRUE(runTomek()) && !is.null(Tomek_Data()) && !is.null(input$target_tomek)) {
        smote_data <- Tomek_Data()$removed_data
        class_col <- input$target_tomek
        
      } else if (isTRUE(runNearMiss()) && !is.null(NearMiss_Data()) && !is.null(input$target_nearmiss)) {
        smote_data <- NearMiss_Data()$removed_data
        class_col <- input$target_nearmiss
        
      } else if (isTRUE(runENN()) && !is.null(ENN_Data()) && !is.null(input$target_enn)) {
        smote_data <- ENN_Data()$removed_data
        class_col <- input$target_enn
        
      } else if (isTRUE(runOSS()) && !is.null(OSS_Data()) && !is.null(input$target_oss)) {
        smote_data <- OSS_Data()$removed_data
        class_col <- input$target_oss
        
      } else if (isTRUE(runRD()) && !is.null(RD_Data()) && !is.null(input$target_rd)) {
        smote_data <- RD_Data()$removed_data
        class_col <- input$target_rd
        
        
        #####BLOCO ADICIONADO EM 10/07/2025
        
      } else if (isTRUE(runSBC()) && !is.null(SBC_Data()) && !is.null(input$target_sbc)) {
        smote_data <- SBC_Data()$removed_data
        class_col <- input$target_sbc
        
        
        #####BLOCO ADICIONADO EM 10/07/2025
        
      }
      
      
      if (!is.null(smote_data) && !is.null(class_col) && class_col %in% names(original_data)) {
        names(original_data)[names(original_data) == class_col] <- "class"
        names(smote_data)[names(smote_data) == class_col] <- "class"
        
        suffix <- if (isTRUE(runTomek()) || isTRUE(runNearMiss()) || isTRUE(runOSS()) ||
                      isTRUE(runENN()) || isTRUE(runRD())  || isTRUE(runSBC())  ) "Removed" else "Synthetic"
        
        smote_data$class <- paste(smote_data$class, suffix)
        smote_data$Tipo <- suffix
        original_data$Tipo <- "Original"
        
        colunas_aux <- c("fake_cat", "..group..", "..id..")
        smote_data <- smote_data[, setdiff(names(smote_data), colunas_aux), drop = FALSE]
        
        feature_cols <- intersect(names(original_data), names(smote_data))
        original_data <- original_data[, feature_cols, drop = FALSE]
        smote_data <- smote_data[, feature_cols, drop = FALSE]
        
        dados <- dplyr::bind_rows(original_data, smote_data)
        dados$class <- as.character(dados$class)
        dados$classe_base <- gsub(" Synthetic$| Removed$", "", dados$class)
        dados$SampleID <- 1:nrow(dados)
        
      } else {
        return(NULL)
      }
    }
        
    } #ADICIONADO EM 04/07/2025
  } #######AQUI   #ADICIONADO EM 13/07/2025
  } #######AQUI   #ADICIONADO EM 14/07/2025
  
    num_vars <- names(Filter(is.numeric, dados))
    num_vars <- setdiff(num_vars, "SampleID")
    validate(need(length(num_vars) >= 2, "Pelo menos 2 Variables numéricas são necessárias."))
    
    classes_originais <- unique(dados$classe_base[dados$Tipo == "Original"])
    classes_sinteticas <- unique(dados$classe_base[dados$Tipo == "Synthetic"])
    classes_removidas  <- unique(dados$classe_base[dados$Tipo == "Removed"])
    
    p <- plot_ly()
    alguma_classe_foi_plotada <- FALSE
    
   # if (isTRUE(runSMOTETL())) {
    
  #  if (isTRUE(runSMOTETL()) || isTRUE(runSMOTEENN()) ) { #MODIFICADO EM 05/07/2025
    
      # if (isTRUE(runSMOTETL()) || isTRUE(runSMOTEENN())  || isTRUE(runSMOTEIPF())   ) { #MODIFICADO EM 13/07/2025
      
    if (isTRUE(runSMOTETL()) || isTRUE(runSMOTEENN())  || isTRUE(runSMOTEIPF()) || isTRUE(runSPIDER())   ) { #MODIFICADO EM 14/07/2025
      
      for (classe in intersect(classes_originais, classes_sinteticas)) {
        p <- compara_procrustes(p, dados, classe, num_vars, "Synthetic")
        alguma_classe_foi_plotada <- TRUE
      }
      for (classe in intersect(classes_originais, classes_removidas)) {
        p <- compara_procrustes(p, dados, classe, num_vars, "Removed")
        alguma_classe_foi_plotada <- TRUE
      }
    } else if (isTRUE(runTomek()) || isTRUE(runNearMiss()) || isTRUE(runOSS()) ||
               isTRUE(runENN()) || isTRUE(runRD())  || isTRUE(runSBC())    ) {
      for (classe in intersect(classes_originais, classes_removidas)) {
        p <- compara_procrustes(p, dados, classe, num_vars, "Removed")
        alguma_classe_foi_plotada <- TRUE
      }
    } else {
      for (classe in intersect(classes_originais, classes_sinteticas)) {
        p <- compara_procrustes(p, dados, classe, num_vars, "Synthetic")
        alguma_classe_foi_plotada <- TRUE
      }
    }
    
    validate(need(alguma_classe_foi_plotada, "Nenhuma classe possui dados suficientes para visualização."))
    
    p <- layout(p,
                title = "Generalized Procrustes by class",
                xaxis = list(title = "Dimension 1"),
                yaxis = list(title = "Dimension 2"),
                legend = list(orientation = "v", x = 1, xanchor = "left",
                              y = 1, yanchor = "top"),
                margin = list(r = 220)) %>%
      config(displayModeBar = FALSE) %>%
      layout(colorway = RColorBrewer::brewer.pal(12, "Set1"))
    
    p
  })
  
  
  compara_procrustes <- function(p, dados, classe, num_vars, tipo) {
    originais <- subset(dados, Tipo == "Original" & classe_base == classe)
    comparado <- subset(dados, Tipo == tipo & classe_base == classe)
    if (nrow(originais) >= 1 && nrow(comparado) >= 1) {
      n <- min(nrow(originais), nrow(comparado))
      vars_comuns <- intersect(num_vars, intersect(names(originais), names(comparado)))
      if (n >= 2 && length(vars_comuns) >= 2) {
        X <- scale(originais[1:n, vars_comuns, drop = FALSE])
        Y <- scale(comparado[1:n, vars_comuns, drop = FALSE])
        if (any(!is.finite(X)) || any(!is.finite(Y))) return(p)
        proc <- vegan::procrustes(X, Y, scale = TRUE)
        X_df <- as.data.frame(proc$X[, 1:2])
        Y_df <- as.data.frame(proc$Yrot[, 1:2])
        names(X_df) <- names(Y_df) <- c("Dim1", "Dim2")
        X_df$ClasseLeg <- classe
        Y_df$ClasseLeg <- paste(classe, tipo)
        X_df$SampleID <- seq_len(nrow(X_df))
        Y_df$SampleID <- seq_len(nrow(Y_df))
        
        for (i in 1:n) {
          linha_df <- data.frame(Dim1 = c(X_df$Dim1[i], Y_df$Dim1[i]),
                                 Dim2 = c(X_df$Dim2[i], Y_df$Dim2[i]))
          p <- add_trace(p, data = linha_df, x = ~Dim1, y = ~Dim2,
                         type = "scatter", mode = "lines",
                         line = list(color = "rgba(100,100,100,0.3)", width = 1),
                         hoverinfo = "none", showlegend = FALSE)
        }
        
        p <- add_trace(p, data = X_df, x = ~Dim1, y = ~Dim2,
                       type = "scatter", mode = "markers",
                       marker = list(size = 9),
                       name = unique(X_df$ClasseLeg),
                       hoverinfo = "text",
                       text = ~paste("Class:", ClasseLeg, "<br>SampleID:", SampleID),
                       showlegend = TRUE)
        
        p <- add_trace(p, data = Y_df, x = ~Dim1, y = ~Dim2,
                       type = "scatter", mode = "markers",
                       marker = list(size = 9, symbol = "circle-open"),
                       name = unique(Y_df$ClasseLeg),
                       hoverinfo = "text",
                       text = ~paste("Class:", ClasseLeg, "<br>SampleID:", SampleID),
                       showlegend = TRUE)
      }
    }
    return(p)
  }
  
 
  
  #######ACRÉSCIMO DE GRÁFICO DE PROCRUSTES GENERALIZADO EM 29/03/2025##
  
  #######ACRÉSCIMO DE GRÁFICO DE PROCRUSTES GENERALIZADO – SMOTE + SMOTE-NC - MODIFICADA EM EM 28/05/2025 ##
  
  
  # ACRÉSCIMO DE BORDERLINE SMOTE EM 05/06/2025
  
  # ACRÉSCIMO DE SVM SMOTE EM 06/06/2025
  
  # ACRÉSCIMO DE ADASYN EM 08/06/2025
  
  #ACRÉSCIMO DE RANDOM UPSAMPLING EM 09/06/2025
  
  #ACRÉSCIMO DE TOMEK LINKS EM 13/06/2025
  
  #ACRÉSCIMO DE NEAR MISS EM 17/06/2025
  
  #ACRÉSCIMO DE ENN EM 17/06/2025
  
  #ACRÉSCIMO DE OSS EM 18/06/2025
  
  #ACRÉSCIMO DE RANDOM DOWNSAMPLING EM 18/06/2025
  
  #ACRÉSCIMO DE SMOTE TL EM 30/06/2025 
  
  #ACRÉSCIMO DE SMOTE ENN EM 05/07/2025 
  
  #ACRÉSCIMO DE SBC EM 10/07/2025
  
  #ACRÉSCIMO DE SMOTE IPF EM 13/07/2025 
  
  #ACRÉSCIMO DE SPIDER EM 14/07/2025 
  
  
  ###ACRÉSCIMO DE GRÁFICO DA Distance from Mahalanobis EM 29/03/2025###
  
  #ACRÉSCIMO DE SMOTE NC NO GRÁFICO DA Distance from Mahalanobis EM 28/05/2025
  #ACRÉSCIMO DE BORDERLINE SMOTE EM 05/06/2025
  #ACRÉSCIMO DE SVM SMOTE EM 06/06/2025
  #ACRÉSCIMO DE ADASYN EM 08/06/2025
  #ACRÉSCIMO DE RANDOM UPSAMPLING EM 09/06/2025
  #ACRÉSCIMO DE TOMEK LINKS EM 14/06/2025
  #ACRÉSCIMO DE NEAR MISS EM 16/06/2025
  #ACRÉSCIMO DE ENN EM 17/06/2025
  #ACRÉSCIMO DE OSS EM 18/06/2025
  #ACRÉSCIMO DE RANDOM DOWNSAMPLING EM 18/06/2025
  #ACRÉSCIMO DE SMOTE TL EM 01/07/2025
  #ACRÉSCIMO DE SMOTE ENN EM 05/07/2025
  #ACRÉSCIMO DE SBC EM 10/07/2025
  #ACRÉSCIMO DE SMOTE IPF EM 13/07/2025
  #ACRÉSCIMO DE SPIDER EM 14/07/2025
  
  
  # Reativo compartilhado entre gráfico e tabela
  
  mahalanobis_resultados <- reactive({
    req(filedata())
    original_data <- as.data.frame(filedata())
    smote_data <- NULL
    class_col <- NULL
    
    # SMOTE-TL
    if (isTRUE(runSMOTETL()) && !is.null(SMOTETL_Data()) && !is.null(input$target_tl)) {
      class_col <- input$target_tl
      smote_data_added <- SMOTETL_Data()$syn_data
      smote_data_removed <- SMOTETL_Data()$removed_data
      
      names(original_data)[names(original_data) == class_col] <- "class"
      original_data$Tipo <- "Original"
      
      if (!is.null(smote_data_added) && nrow(smote_data_added) > 0) {
        names(smote_data_added)[names(smote_data_added) == class_col] <- "class"
        smote_data_added$class <- paste(smote_data_added$class, "Synthetic")
        smote_data_added$Tipo <- "Synthetic"
      } else {
        smote_data_added <- original_data[0, , drop = FALSE]
      }
      
      if (!is.null(smote_data_removed) && nrow(smote_data_removed) > 0) {
        names(smote_data_removed)[names(smote_data_removed) == class_col] <- "class"
        smote_data_removed$class <- paste(smote_data_removed$class, "Removed")
        smote_data_removed$Tipo <- "Removed"
      } else {
        smote_data_removed <- original_data[0, , drop = FALSE]
      }
      
      smote_data <- dplyr::bind_rows(smote_data_added, smote_data_removed)
      
      feature_cols <- intersect(names(original_data), names(smote_data))
      original_data <- original_data[, feature_cols, drop = FALSE]
      smote_data <- smote_data[, feature_cols, drop = FALSE]
      
      dados <- dplyr::bind_rows(original_data, smote_data)
      dados$class <- as.character(dados$class)
      dados$Tipo <- ifelse(grepl("Removed$", dados$class), "Removed",
                           ifelse(grepl("Synthetic$", dados$class), "Synthetic", "Original"))
      dados$classe_base <- gsub(" Synthetic$| Removed$", "", dados$class)
      
    } 
    
    ##############ADICIONADO EM 05/07/2025
    else {
      
      # SMOTE-ENN
      
      if (isTRUE(runSMOTEENN()) && !is.null(SMOTEENN_Data()) && !is.null(input$target_enn2)) {
        class_col <- input$target_enn2
        smote_data_added <- SMOTEENN_Data()$syn_data
        smote_data_removed <- SMOTEENN_Data()$removed_data
        
        names(original_data)[names(original_data) == class_col] <- "class"
        original_data$Tipo <- "Original"
        
        if (!is.null(smote_data_added) && nrow(smote_data_added) > 0) {
          names(smote_data_added)[names(smote_data_added) == class_col] <- "class"
          smote_data_added$class <- paste(smote_data_added$class, "Synthetic")
          smote_data_added$Tipo <- "Synthetic"
        } else {
          smote_data_added <- original_data[0, , drop = FALSE]
        }
        
        if (!is.null(smote_data_removed) && nrow(smote_data_removed) > 0) {
          names(smote_data_removed)[names(smote_data_removed) == class_col] <- "class"
          smote_data_removed$class <- paste(smote_data_removed$class, "Removed")
          smote_data_removed$Tipo <- "Removed"
        } else {
          smote_data_removed <- original_data[0, , drop = FALSE]
        }
        
        smote_data <- dplyr::bind_rows(smote_data_added, smote_data_removed)
        
        feature_cols <- intersect(names(original_data), names(smote_data))
        original_data <- original_data[, feature_cols, drop = FALSE]
        smote_data <- smote_data[, feature_cols, drop = FALSE]
        
        dados <- dplyr::bind_rows(original_data, smote_data)
        dados$class <- as.character(dados$class)
        dados$Tipo <- ifelse(grepl("Removed$", dados$class), "Removed",
                             ifelse(grepl("Synthetic$", dados$class), "Synthetic", "Original"))
        dados$classe_base <- gsub(" Synthetic$| Removed$", "", dados$class)
        
      } 
      
    
    ##############ADICIONADO EM 05/07/2025
    
      
      ##############ADICIONADO EM 13/07/2025
      else {
        
        # SMOTE-IPF
        
        if (isTRUE(runSMOTEIPF()) && !is.null(SMOTEIPF_Data()) && !is.null(input$target_ipf)) {
          class_col <- input$target_ipf
          smote_data_added <- SMOTEIPF_Data()$syn_data
          smote_data_removed <- SMOTEIPF_Data()$removed_data
          
          names(original_data)[names(original_data) == class_col] <- "class"
          original_data$Tipo <- "Original"
          
          if (!is.null(smote_data_added) && nrow(smote_data_added) > 0) {
            names(smote_data_added)[names(smote_data_added) == class_col] <- "class"
            smote_data_added$class <- paste(smote_data_added$class, "Synthetic")
            smote_data_added$Tipo <- "Synthetic"
          } else {
            smote_data_added <- original_data[0, , drop = FALSE]
          }
          
          if (!is.null(smote_data_removed) && nrow(smote_data_removed) > 0) {
            names(smote_data_removed)[names(smote_data_removed) == class_col] <- "class"
            smote_data_removed$class <- paste(smote_data_removed$class, "Removed")
            smote_data_removed$Tipo <- "Removed"
          } else {
            smote_data_removed <- original_data[0, , drop = FALSE]
          }
          
          smote_data <- dplyr::bind_rows(smote_data_added, smote_data_removed)
          
          feature_cols <- intersect(names(original_data), names(smote_data))
          original_data <- original_data[, feature_cols, drop = FALSE]
          smote_data <- smote_data[, feature_cols, drop = FALSE]
          
          dados <- dplyr::bind_rows(original_data, smote_data)
          dados$class <- as.character(dados$class)
          dados$Tipo <- ifelse(grepl("Removed$", dados$class), "Removed",
                               ifelse(grepl("Synthetic$", dados$class), "Synthetic", "Original"))
          dados$classe_base <- gsub(" Synthetic$| Removed$", "", dados$class)
          
        } 
        
   #   }
      ##############ADICIONADO EM 13/07/2025
      
    
        ##############ADICIONADO EM 14/07/2025
        else {
          
          # SPIDER
          
          if (isTRUE(runSPIDER()) && !is.null(SPIDER_Data()) && !is.null(input$target_spider)) {
            class_col <- input$target_spider
            smote_data_added <- SPIDER_Data()$syn_data
            smote_data_removed <- SPIDER_Data()$removed_data
            
            names(original_data)[names(original_data) == class_col] <- "class"
            original_data$Tipo <- "Original"
            
            if (!is.null(smote_data_added) && nrow(smote_data_added) > 0) {
              names(smote_data_added)[names(smote_data_added) == class_col] <- "class"
              smote_data_added$class <- paste(smote_data_added$class, "Synthetic")
              smote_data_added$Tipo <- "Synthetic"
            } else {
              smote_data_added <- original_data[0, , drop = FALSE]
            }
            
            if (!is.null(smote_data_removed) && nrow(smote_data_removed) > 0) {
              names(smote_data_removed)[names(smote_data_removed) == class_col] <- "class"
              smote_data_removed$class <- paste(smote_data_removed$class, "Removed")
              smote_data_removed$Tipo <- "Removed"
            } else {
              smote_data_removed <- original_data[0, , drop = FALSE]
            }
            
            smote_data <- dplyr::bind_rows(smote_data_added, smote_data_removed)
            
            feature_cols <- intersect(names(original_data), names(smote_data))
            original_data <- original_data[, feature_cols, drop = FALSE]
            smote_data <- smote_data[, feature_cols, drop = FALSE]
            
            dados <- dplyr::bind_rows(original_data, smote_data)
            dados$class <- as.character(dados$class)
            dados$Tipo <- ifelse(grepl("Removed$", dados$class), "Removed",
                                 ifelse(grepl("Synthetic$", dados$class), "Synthetic", "Original"))
            dados$classe_base <- gsub(" Synthetic$| Removed$", "", dados$class)
          } 
          
       # }
        ##############ADICIONADO EM 14/07/2025
        
    
    else {
      
      if (isTRUE(runSMOTENC()) && !is.null(SMOTENC_Data()) && !is.null(input$target_nc)) {
        smote_data <- SMOTENC_Data()$syn_data
        class_col <- input$target_nc
      } else if (isTRUE(runSMOTE()) && !is.null(SMOTE_Data()) && !is.null(input$target)) {
        smote_data <- SMOTE_Data()$syn_data
        class_col <- input$target
      } else if (isTRUE(runBorderlineSMOTE()) && !is.null(BorderlineSMOTE_Data()) && !is.null(input$target_borderline)) {
        smote_data <- BorderlineSMOTE_Data()$syn_data
        class_col <- input$target_borderline
      } else if (isTRUE(runSVM_SMOTE()) && !is.null(SVM_SMOTE_Data()) && !is.null(input$target_svm)) {
        smote_data <- SVM_SMOTE_Data()$syn_data
        class_col <- input$target_svm
      } else if (isTRUE(runADASYN()) && !is.null(ADASYN_Data()) && !is.null(input$target_adasyn)) {
        smote_data <- ADASYN_Data()$syn_data
        class_col <- input$target_adasyn
      } else if (isTRUE(runRU()) && !is.null(RU_Data()) && !is.null(input$target_ru)) {
        smote_data <- RU_Data()$syn_data
        class_col <- input$target_ru
      } else if (isTRUE(runTomek()) && !is.null(Tomek_Data()) && !is.null(input$target_tomek)) {
        smote_data <- Tomek_Data()$removed_data
        class_col <- input$target_tomek
      } else if (isTRUE(runNearMiss()) && !is.null(NearMiss_Data()) && !is.null(input$target_nearmiss)) {
        smote_data <- NearMiss_Data()$removed_data
        class_col <- input$target_nearmiss
      } else if (isTRUE(runENN()) && !is.null(ENN_Data()) && !is.null(input$target_enn)) {
        smote_data <- ENN_Data()$removed_data
        class_col <- input$target_enn
      } else if (isTRUE(runOSS()) && !is.null(OSS_Data()) && !is.null(input$target_oss)) {
        smote_data <- OSS_Data()$removed_data
        class_col <- input$target_oss
      } else if (isTRUE(runRD()) && !is.null(RD_Data()) && !is.null(input$target_rd)) {
        smote_data <- RD_Data()$removed_data
        class_col <- input$target_rd
        
        
        #####BLOCO ADICIONADO EM 10/07/2025
        
      } else if (isTRUE(runSBC()) && !is.null(SBC_Data()) && !is.null(input$target_sbc)) {
        smote_data <- SBC_Data()$removed_data
        class_col <- input$target_sbc
        
        
        
        #####BLOCO ADICIONADO EM 10/07/2025
        
      } else {
        return(data.frame())
      }
      
      if (is.null(smote_data) || !(class_col %in% names(original_data))) return(data.frame())
      
      names(original_data)[names(original_data) == class_col] <- "class"
      names(smote_data)[names(smote_data) == class_col] <- "class"
      
      suffix <- if (isTRUE(runTomek()) || isTRUE(runNearMiss()) || isTRUE(runENN()) ||
                    isTRUE(runOSS()) || isTRUE(runRD())  || isTRUE(runSBC())      ) "Removed" else "Synthetic"
      
      smote_data$class <- paste(smote_data$class, suffix)
      smote_data$Tipo <- suffix
      original_data$Tipo <- "Original"
      
      feature_cols <- intersect(names(original_data), names(smote_data))
      original_data <- original_data[, feature_cols, drop = FALSE]
      smote_data <- smote_data[, feature_cols, drop = FALSE]
      
      dados <- dplyr::bind_rows(original_data, smote_data)
      dados$class <- as.character(dados$class)
      dados$Tipo <- ifelse(grepl(suffix, dados$class), suffix, "Original")
      dados$classe_base <- gsub(paste0(" ", suffix), "", dados$class)
    }
    
  } # ADICIONADO EM 05/07/2025
  } #  ADICIONADO EM 13/07/2025
  } #  ADICIONADO EM 14/07/2025
    
    # Calculando dist. Mahalanobis
    
    num_vars <- names(Filter(is.numeric, dados))
    num_vars <- setdiff(num_vars, "SampleID")
    validate(need(length(num_vars) >= 2, "Pelo menos 2 Variables numéricas são necessárias."))
    
    classes <- unique(dados$classe_base)
    resultados <- data.frame()
    
    for (classe in classes) {
      dados_ori <- subset(dados, Tipo == "Original" & classe_base == classe)
      
    ####  if (isTRUE(runSMOTETL())) {
      
      # if (isTRUE(runSMOTETL()) ||    isTRUE(runSMOTEENN())  ) { #MODIFICADO EM 05/07/2025
      #  if (isTRUE(runSMOTETL()) ||    isTRUE(runSMOTEENN()) ||  isTRUE(runSMOTEIPF())  ) { #MODIFICADO EM 13/07/2025
      if (isTRUE(runSMOTETL()) ||    isTRUE(runSMOTEENN()) ||  isTRUE(runSMOTEIPF()) ||  isTRUE(runSPIDER())  ) { #MODIFICADO EM 14/07/2025
        tipos <- c("Synthetic", "Removed")
      } else if (isTRUE(runTomek()) || isTRUE(runNearMiss()) || isTRUE(runENN()) ||
                 isTRUE(runOSS()) || isTRUE(runRD())  || isTRUE(runSBC())      ) {
        tipos <- "Removed"
      } else {
        tipos <- "Synthetic"
      }
      
      for (tipo in tipos) {
        dados_syn <- subset(dados, Tipo == tipo & classe_base == classe)
        
        vars_comuns <- intersect(names(dados_ori), names(dados_syn))
        vars_comuns <- intersect(vars_comuns, num_vars)
        
        if (length(vars_comuns) >= 2 && nrow(dados_ori) >= 2 && nrow(dados_syn) >= 1) {
          X_ori <- dados_ori[, vars_comuns, drop = FALSE]
          X_syn <- dados_syn[, vars_comuns, drop = FALSE]
          
          if (nrow(X_syn) == 1) X_syn <- rbind(X_syn, X_syn)
          if (nrow(X_ori) == 1) X_ori <- rbind(X_ori, X_ori)
          
          mu_ori <- colMeans(X_ori)
          mu_syn <- colMeans(X_syn)
          cov_pool <- (cov(X_ori) + cov(X_syn)) / 2
          
          
          
          ###################BLOCO MODIFICADO EM 01/07/2025
          
          
         # dist <- NA
        #  if (det(cov_pool) > .Machine$double.eps) {
        #    dist <- sqrt(mahalanobis(mu_syn, center = mu_ori, cov = cov_pool))
        #  }
         
          
          
          dist <- NA
          if (det(cov_pool) > .Machine$double.eps) {
            dist <- sqrt(mahalanobis(mu_syn, center = mu_ori, cov = cov_pool))
          } else {
            # pseudo-inversa - problemas de singularidade
            diff <- as.matrix(mu_syn - mu_ori)
            dist <- sqrt(t(diff) %*% MASS::ginv(cov_pool) %*% diff)
            dist <- as.numeric(dist)
          }
          
          
          
          ###################BLOCO MODIFICADO EM 01/07/2025
          
          
          n1 <- nrow(X_ori)
          n2 <- nrow(X_syn)
          S_pooled <- ((n1 - 1) * cov(X_ori) + (n2 - 1) * cov(X_syn)) / (n1 + n2 - 2)
          diff_mean <- as.matrix(mu_ori - mu_syn)
          t2 <- NA
          p_val <- NA
          if (det(S_pooled) > .Machine$double.eps) {
          #  t2 <- (n1 * n2) / (n1 + n2) * t(diff_mean) %*% solve(S_pooled) %*% diff_mean
            
            t2 <- (n1 * n2) / (n1 + n2) * t(diff_mean) %*% MASS::ginv(S_pooled) %*% diff_mean #modificado 01/07/2025
            
            
            t2 <- as.numeric(t2)
            p_val <- 1 - pf(t2 * (n1 + n2 - length(vars_comuns) - 1) / ((n1 + n2 - 2) * length(vars_comuns)),
                            df1 = length(vars_comuns),
                            df2 = n1 + n2 - length(vars_comuns) - 1)
          }
          
          resultados <- rbind(resultados, data.frame(
            Classe = classe,
            Tipo = tipo,
            Mahalanobis_Dist = round(dist, 4),
            Hotelling_T2 = round(t2, 4),
            p_valor = signif(p_val, 4)
          ))
        }
      }
    }
    
    resultados
  })
  
  # Gráfico Plotly 
  
  output$grafico_mahalanobis_por_classe <- renderPlotly({
    resultados <- mahalanobis_resultados()
    validate(need(nrow(resultados) > 0, "Não há dados suficientes para análise."))
    
    limite_aceitavel <- 1.5
    cores <- RColorBrewer::brewer.pal(12, "Set1")
    resultados$ClasseTipo <- paste(resultados$Classe, resultados$Tipo)
    resultados$ClasseTipo <- factor(resultados$ClasseTipo, levels = unique(resultados$ClasseTipo))
    
    p <- plot_ly()
    
    for (i in 1:nrow(resultados)) {
      p <- p %>%
        add_trace(
          x = resultados$ClasseTipo[i],
          y = resultados$Mahalanobis_Dist[i],
          type = "bar",
          name = as.character(resultados$ClasseTipo[i]),
          showlegend = TRUE,
          marker = list(color = cores[(i - 1) %% length(cores) + 1]),
          text = paste("Mahalanobis_Dist:", resultados$Mahalanobis_Dist[i]),
          textposition = "auto",
          hoverinfo = "text"
        )
    }
    
    p <- layout(p,
                title = "Distance from Mahalanobis by class",
                xaxis = list(title = "Class"),
                yaxis = list(title = "Mahalanobis Distance"),
                legend = list(
                  orientation = "v",
                  x = 1.02,
                  xanchor = "left",
                  y = 1,
                  yanchor = "top"
                ),
                margin = list(r = 250, l = 60, b = 80, t = 60),
                shapes = list(
                  list(
                    type = "line",
                    y0 = limite_aceitavel,
                    y1 = limite_aceitavel,
                    x0 = 0,
                    x1 = 1,
                    xref = "paper",
                    yref = "y",
                    line = list(color = "rgba(0,0,0,0.5)", dash = "dash", width = 2)
                  )
                ),
                annotations = list(
                  list(
                    x = 0.95,
                    y = limite_aceitavel + 0.15,
                    xref = "paper",
                    yref = "y",
                    text = "Acceptable limit",
                    showarrow = FALSE,
                    xanchor = "right",
                    font = list(size = 12, color = "rgba(0,0,0,0.6)")
                  )
                )
    )
    
    p
  })
  
  
  
  
  #ACRÉSCIMO DE SMOTE NC EM 28/05/2025
  
  
  # Tabela com Hotelling T² e p-valor
  #  output$tabela_hotelling <- renderDT({
  #   resultados <- mahalanobis_resultados()
  #  validate(need(nrow(resultados) > 0, "Sem resultados para mostrar."))
  #  #datatable(resultados, rownames = FALSE, options = list(dom = 'tip', pageLength = 10))
  #  DT::datatable(resultados, rownames = FALSE,
  #               options = list(pageLength = 10),
  #              caption = htmltools::tags$caption(
  #               style = 'caption-side: top; text-align: left; font-weight: bold;',
  #              "Results by Class - Hotelling T² e Distance from Mahalanobis"
  #           ))
  # })
  
  
  #ACRÉSCIMO DE SMOTE NC NO GRÁFICO DA Distance from Mahalanobis EM 28/05/2025
  #ACRÉSCIMO DE BORDERLINE SMOTE EM 05/06/2025
  #ACRÉSCIMO DE SVM SMOTE EM 06/06/2025
  #ACRÉSCIMO DE ADASYN EM 08/06/2025
  #ACRÉSCIMO DE RANDOM UPSAMPLING EM 09/06/2025
  #ACRÉSCIMO DE NEAR MISS EM 16/06/2025
  #ACRÉSCIMO DE ENN EM 17/06/2025
  #ACRÉSCIMO DE OSS EM 18/06/2025
  #ACRÉSCIMO DE RANDOM DOWNSAMPLING EM 18/06/2025
  #ACRÉSCIMO DE SMOTE TL EM 01/07/2025
  #ACRÉSCIMO DE SMOTE ENN EM 05/07/2025
  #ACRÉSCIMO DE SBC EM 10/07/2025
  #ACRÉSCIMO DE SMOTE IPF EM 13/07/2025
  #ACRÉSCIMO DE SPIDER EM 14/07/2025
  
  
  
  
  ###ACRÉSCIMO DE GRÁFICO DA Distance from Mahalanobis EM 29/03/2025##
  
  
  
  #######ACRÉSCIMO DE TESTE DE KOLMOGOROV SMIRNOV EM 28/03/2025########
  
  #######ACRÉSCIMO DE SMOTE_NC EM TESTE DE KOLMOGOROV SMIRNOV EM 29/05/2025 #######
  #######ACRÉSCIMO DE BORDERLINE SMOTE EM TESTE DE KOLMOGOROV SMIRNOV EM 05/06/2025 #######
  #######ACRÉSCIMO DE SVM SMOTE EM TESTE DE KOLMOGOROV SMIRNOV EM 06/06/2025 #######
  #######ACRÉSCIMO DE ADASYN EM TESTE DE KOLMOGOROV SMIRNOV EM 08/06/2025 #######
  #######ACRÉSCIMO DE RANDOM UPSAMPLING EM TESTE DE KOLMOGOROV SMIRNOV EM 09/06/2025 #######
  #######ACRÉSCIMO DE TOMEK LINKS EM TESTE DE KOLMOGOROV SMIRNOV EM 14/06/2025 #######
  #######ACRÉSCIMO DE NEAR MISS EM TESTE DE KOLMOGOROV SMIRNOV EM 17/06/2025 #######
  #######ACRÉSCIMO DE ENN EM TESTE DE KOLMOGOROV SMIRNOV EM 17/06/2025 #######
  #######ACRÉSCIMO DE OSS EM TESTE DE KOLMOGOROV SMIRNOV EM 18/06/2025 #######
  #######ACRÉSCIMO DE RANDOM DOWNSAMPLING EM TESTE DE KOLMOGOROV SMIRNOV EM 18/06/2025 #######
  #######ACRÉSCIMO DE SMOTE TL EM TESTE DE KOLMOGOROV SMIRNOV EM 02/07/2025 #######
  #######ACRÉSCIMO DE SMOTE ENN EM TESTE DE KOLMOGOROV SMIRNOV EM 05/07/2025 #######
  #######ACRÉSCIMO DE SBC EM TESTE DE KOLMOGOROV SMIRNOV EM 10/07/2025 #######
  #######ACRÉSCIMO DE SMOTE IPF EM TESTE DE KOLMOGOROV SMIRNOV EM 14/07/2025 #######
  #######ACRÉSCIMO DE SPIDER EM TESTE DE KOLMOGOROV SMIRNOV EM 14/07/2025 #######
  
  
  output$ks_por_classe <- renderDT({
    original_data <- as.data.frame(filedata())
    smote_data <- NULL
    class_col <- NULL
    
    if (isTRUE(runSMOTENC()) && !is.null(SMOTENC_Data()) && !is.null(input$target_nc)) {
      smote_data <- SMOTENC_Data()$syn_data
      class_col <- input$target_nc
      
    } else if (isTRUE(runSMOTE()) && !is.null(SMOTE_Data()) && !is.null(input$target)) {
      smote_data <- SMOTE_Data()$syn_data
      class_col <- input$target
      
    } else if (isTRUE(runBorderlineSMOTE()) && !is.null(BorderlineSMOTE_Data()) && !is.null(input$target_borderline)) {
      smote_data <- BorderlineSMOTE_Data()$syn_data
      class_col <- input$target_borderline
      
    } else if (isTRUE(runSVM_SMOTE()) && !is.null(SVM_SMOTE_Data()) && !is.null(input$target_svm)) {
      smote_data <- SVM_SMOTE_Data()$syn_data
      class_col <- input$target_svm
      
    } else if (isTRUE(runADASYN()) && !is.null(ADASYN_Data()) && !is.null(input$target_adasyn)) {
      smote_data <- ADASYN_Data()$syn_data
      class_col <- input$target_adasyn
      
    } else if (isTRUE(runRU()) && !is.null(RU_Data()) && !is.null(input$target_ru)) {
      smote_data <- RU_Data()$syn_data
      class_col <- input$target_ru
      
    } else if (isTRUE(runTomek()) && !is.null(Tomek_Data()) && !is.null(input$target_tomek)) {
      smote_data <- Tomek_Data()$removed_data
      class_col <- input$target_tomek
      
    } else if (isTRUE(runNearMiss()) && !is.null(NearMiss_Data()) && !is.null(input$target_nearmiss)) {
      smote_data <- NearMiss_Data()$removed_data
      class_col <- input$target_nearmiss
      
    } else if (isTRUE(runENN()) && !is.null(ENN_Data()) && !is.null(input$target_enn)) {
      smote_data <- ENN_Data()$removed_data
      class_col <- input$target_enn
      
    } else if (isTRUE(runOSS()) && !is.null(OSS_Data()) && !is.null(input$target_oss)) {
      smote_data <- OSS_Data()$removed_data
      class_col <- input$target_oss
      
    } else if (isTRUE(runRD()) && !is.null(RD_Data()) && !is.null(input$target_rd)) {
      smote_data <- RD_Data()$removed_data
      class_col <- input$target_rd
  
      
      #####BLOCO ADICIONADO EM 10/07/2025
      
    } else if (isTRUE(runSBC()) && !is.null(SBC_Data()) && !is.null(input$target_sbc)) {
      smote_data <- SBC_Data()$removed_data
      class_col <- input$target_sbc
      
      
      #####BLOCO ADICIONADO EM 10/07/2025
          
    } else if (isTRUE(runSMOTETL()) && !is.null(SMOTETL_Data()) && !is.null(input$target_tl)) {

      # SMOTE-TL
      
      class_col <- input$target_tl
      smote_data_added <- SMOTETL_Data()$syn_data
      smote_data_removed <- SMOTETL_Data()$removed_data
      
      names(original_data)[names(original_data) == class_col] <- "class"
      original_data$Tipo <- "Original"
      
      if (!is.null(smote_data_added) && nrow(smote_data_added) > 0) {
        names(smote_data_added)[names(smote_data_added) == class_col] <- "class"
        smote_data_added$class <- paste(smote_data_added$class, "Synthetic")
        smote_data_added$Tipo <- "Synthetic"
      } else {
        smote_data_added <- original_data[0, , drop = FALSE]
      }
      
      if (!is.null(smote_data_removed) && nrow(smote_data_removed) > 0) {
        names(smote_data_removed)[names(smote_data_removed) == class_col] <- "class"
        smote_data_removed$class <- paste(smote_data_removed$class, "Removed")
        smote_data_removed$Tipo <- "Removed"
      } else {
        smote_data_removed <- original_data[0, , drop = FALSE]
      }
      
      smote_data <- dplyr::bind_rows(smote_data_added, smote_data_removed)
      
      feature_cols <- intersect(names(original_data), names(smote_data))
      original_data <- original_data[, feature_cols, drop = FALSE]
      smote_data <- smote_data[, feature_cols, drop = FALSE]
      
      dados <- dplyr::bind_rows(original_data, smote_data)
      dados$class <- as.character(dados$class)
      dados$Tipo <- ifelse(grepl("Removed$", dados$class), "Removed",
                           ifelse(grepl("Synthetic$", dados$class), "Synthetic", "Original"))
      dados$classe_base <- gsub(" Synthetic$| Removed$", "", dados$class)
      
    } 
    
   ############################BLOCO ADICIONADO EM 05/07/2025 
    
    else if (isTRUE(runSMOTEENN()) && !is.null(SMOTEENN_Data()) && !is.null(input$target_enn2)) {
      
      # SMOTE-ENN
      
      class_col <- input$target_enn2
      smote_data_added <- SMOTEENN_Data()$syn_data
      smote_data_removed <- SMOTEENN_Data()$removed_data
      
      names(original_data)[names(original_data) == class_col] <- "class"
      original_data$Tipo <- "Original"
      
      if (!is.null(smote_data_added) && nrow(smote_data_added) > 0) {
        names(smote_data_added)[names(smote_data_added) == class_col] <- "class"
        smote_data_added$class <- paste(smote_data_added$class, "Synthetic")
        smote_data_added$Tipo <- "Synthetic"
      } else {
        smote_data_added <- original_data[0, , drop = FALSE]
      }
      
      if (!is.null(smote_data_removed) && nrow(smote_data_removed) > 0) {
        names(smote_data_removed)[names(smote_data_removed) == class_col] <- "class"
        smote_data_removed$class <- paste(smote_data_removed$class, "Removed")
        smote_data_removed$Tipo <- "Removed"
      } else {
        smote_data_removed <- original_data[0, , drop = FALSE]
      }
      
      smote_data <- dplyr::bind_rows(smote_data_added, smote_data_removed)
      
      feature_cols <- intersect(names(original_data), names(smote_data))
      original_data <- original_data[, feature_cols, drop = FALSE]
      smote_data <- smote_data[, feature_cols, drop = FALSE]
      
      dados <- dplyr::bind_rows(original_data, smote_data)
      dados$class <- as.character(dados$class)
      dados$Tipo <- ifelse(grepl("Removed$", dados$class), "Removed",
                           ifelse(grepl("Synthetic$", dados$class), "Synthetic", "Original"))
      dados$classe_base <- gsub(" Synthetic$| Removed$", "", dados$class)
      
    } 
    
    
  ############################BLOCO ADICIONADO EM 05/07/2025
   
  ############################BLOCO ADICIONADO EM 14/07/2025 
    
    else if (isTRUE(runSMOTEIPF()) && !is.null(SMOTEIPF_Data()) && !is.null(input$target_ipf)) {
      
      # SMOTE-IPF
      
      class_col <- input$target_ipf
      smote_data_added <- SMOTEIPF_Data()$syn_data
      smote_data_removed <- SMOTEIPF_Data()$removed_data
      
      names(original_data)[names(original_data) == class_col] <- "class"
      original_data$Tipo <- "Original"
      
      if (!is.null(smote_data_added) && nrow(smote_data_added) > 0) {
        names(smote_data_added)[names(smote_data_added) == class_col] <- "class"
        smote_data_added$class <- paste(smote_data_added$class, "Synthetic")
        smote_data_added$Tipo <- "Synthetic"
      } else {
        smote_data_added <- original_data[0, , drop = FALSE]
      }
      
      if (!is.null(smote_data_removed) && nrow(smote_data_removed) > 0) {
        names(smote_data_removed)[names(smote_data_removed) == class_col] <- "class"
        smote_data_removed$class <- paste(smote_data_removed$class, "Removed")
        smote_data_removed$Tipo <- "Removed"
      } else {
        smote_data_removed <- original_data[0, , drop = FALSE]
      }
      
      smote_data <- dplyr::bind_rows(smote_data_added, smote_data_removed)
      
      feature_cols <- intersect(names(original_data), names(smote_data))
      original_data <- original_data[, feature_cols, drop = FALSE]
      smote_data <- smote_data[, feature_cols, drop = FALSE]
      
      dados <- dplyr::bind_rows(original_data, smote_data)
      dados$class <- as.character(dados$class)
      dados$Tipo <- ifelse(grepl("Removed$", dados$class), "Removed",
                           ifelse(grepl("Synthetic$", dados$class), "Synthetic", "Original"))
      dados$classe_base <- gsub(" Synthetic$| Removed$", "", dados$class)
      
    } 
    
    
    ############################BLOCO ADICIONADO EM 14/07/2025
    
     
    ############################BLOCO ADICIONADO EM 14/07/2025 
    
    else if (isTRUE(runSPIDER()) && !is.null(SPIDER_Data()) && !is.null(input$target_spider)) {
      
      # SPIDER
      
      class_col <- input$target_spider
      smote_data_added <- SPIDER_Data()$syn_data
      smote_data_removed <- SPIDER_Data()$removed_data
      
      names(original_data)[names(original_data) == class_col] <- "class"
      original_data$Tipo <- "Original"
      
      if (!is.null(smote_data_added) && nrow(smote_data_added) > 0) {
        names(smote_data_added)[names(smote_data_added) == class_col] <- "class"
        smote_data_added$class <- paste(smote_data_added$class, "Synthetic")
        smote_data_added$Tipo <- "Synthetic"
      } else {
        smote_data_added <- original_data[0, , drop = FALSE]
      }
      
      if (!is.null(smote_data_removed) && nrow(smote_data_removed) > 0) {
        names(smote_data_removed)[names(smote_data_removed) == class_col] <- "class"
        smote_data_removed$class <- paste(smote_data_removed$class, "Removed")
        smote_data_removed$Tipo <- "Removed"
      } else {
        smote_data_removed <- original_data[0, , drop = FALSE]
      }
      
      smote_data <- dplyr::bind_rows(smote_data_added, smote_data_removed)
      
      feature_cols <- intersect(names(original_data), names(smote_data))
      original_data <- original_data[, feature_cols, drop = FALSE]
      smote_data <- smote_data[, feature_cols, drop = FALSE]
      
      dados <- dplyr::bind_rows(original_data, smote_data)
      dados$class <- as.character(dados$class)
      dados$Tipo <- ifelse(grepl("Removed$", dados$class), "Removed",
                           ifelse(grepl("Synthetic$", dados$class), "Synthetic", "Original"))
      dados$classe_base <- gsub(" Synthetic$| Removed$", "", dados$class)
    } 
    
    ############################BLOCO ADICIONADO EM 14/07/2025
    
   
    
    else {
      return(data.frame())
    }
    
   # if (!isTRUE(runSMOTETL())) {
    
    ## if (!isTRUE(runSMOTETL()) && !isTRUE(runSMOTEENN()) ) {  #MODIFICADO EM 05/07/2025
    
  ##  if (!isTRUE(runSMOTETL()) && !isTRUE(runSMOTEENN()) && !isTRUE(runSMOTEIPF())  ) {  #MODIFICADO EM 14/07/2025
      
    if (!isTRUE(runSMOTETL()) && !isTRUE(runSMOTEENN()) && !isTRUE(runSMOTEIPF()) && !isTRUE(runSPIDER())  ) {  #MODIFICADO EM 14/07/2025
      
      if (is.null(smote_data) || !(class_col %in% names(original_data))) return(data.frame())
      
      names(original_data)[names(original_data) == class_col] <- "class"
      names(smote_data)[names(smote_data) == class_col] <- "class"
      
      suffix <- if (isTRUE(runTomek()) || isTRUE(runNearMiss()) || isTRUE(runENN()) ||
                    isTRUE(runOSS()) || isTRUE(runRD())   || isTRUE(runSBC())       ) "Removed" else "Synthetic"
      
      smote_data$class <- paste(smote_data$class, suffix)
      smote_data$fake_cat <- NULL
      
      feature_cols <- intersect(names(original_data), names(smote_data))
      original_data <- original_data[, feature_cols, drop = FALSE]
      smote_data <- smote_data[, feature_cols, drop = FALSE]
      
      original_data$Tipo <- "Original"
      smote_data$Tipo <- suffix
      
      dados <- dplyr::bind_rows(original_data, smote_data)
      dados$class <- as.character(dados$class)
      dados$Tipo <- ifelse(grepl(suffix, dados$class), suffix, "Original")
      dados$classe_base <- gsub(paste0(" ", suffix), "", dados$class)
    }
    
    num_vars <- names(Filter(is.numeric, dados))
    num_vars <- setdiff(num_vars, "SampleID")
    validate(need(length(num_vars) >= 2, "Pelo menos 2 Variables numéricas são necessárias."))
    
    classes <- unique(dados$classe_base)
    resultados <- data.frame()
    
    
    ####MODIFICADO EM 05/07/2025
    ####MODIFICADO EM 14/07/2025
    
  #  tipos <- if (isTRUE(runSMOTETL())) c("Synthetic", "Removida") else unique(dados$Tipo[dados$Tipo != "Original"])
    
    ## tipos <- if (isTRUE(runSMOTETL()) || isTRUE(runSMOTEENN())  ) c("Synthetic", "Removida") else unique(dados$Tipo[dados$Tipo != "Original"])
    
    
    tipos <- if (isTRUE(runSMOTETL()) || isTRUE(runSMOTEENN())  || isTRUE(runSMOTEIPF()) || isTRUE(runSPIDER())   ) c("Synthetic", "Removed") else unique(dados$Tipo[dados$Tipo != "Original"])
    
    
    
    ####MODIFICADO EM 05/07/2025
    ####MODIFICADO EM 14/07/2025
    
    
    
    for (cl in classes) {
      for (tipo in tipos) {
        df_o <- dados %>% filter(classe_base == cl & Tipo == "Original")
        df_s <- dados %>% filter(classe_base == cl & Tipo == tipo)
        
        if (nrow(df_o) >= 2 && nrow(df_s) >= 1) {
          if (nrow(df_s) == 1) df_s <- df_s[rep(1, 2), ]
          vec_o <- unlist(df_o[num_vars])
          vec_s <- unlist(df_s[num_vars])
          ks <- suppressWarnings(ks.test(vec_o, vec_s))
          
          resultados <- rbind(resultados, data.frame(
            Class = cl,
            Type = tipo,
            p_value = signif(ks$p.value, 4)
          ))
        }
      }
    }
    
    if (nrow(resultados) == 0) return(NULL)
    
    datatable(resultados, rownames = FALSE, options = list(
      pageLength = 10,
      dom = 'tip',
      scrollX = TRUE
    ))
  })
  
  
  
  #######ACRÉSCIMO DE SMOTE_NC EM TESTE DE KOLMOGOROV SMIRNOV EM 29/05/2025 #######
  #######ACRÉSCIMO DE BORDERLINE SMOTE EM TESTE DE KOLMOGOROV SMIRNOV EM 05/06/2025 #######
  #######ACRÉSCIMO DE SVM SMOTE EM TESTE DE KOLMOGOROV SMIRNOV EM 06/06/2025 #######
  #######ACRÉSCIMO DE ADASYN EM TESTE DE KOLMOGOROV SMIRNOV EM 08/06/2025 #######
  #######ACRÉSCIMO DE RANDOM UPSAMPLING EM TESTE DE KOLMOGOROV SMIRNOV EM 09/06/2025 #######
  #######ACRÉSCIMO DE TOMEK LINKS EM TESTE DE KOLMOGOROV SMIRNOV EM 14/06/2025 #######
  #######ACRÉSCIMO DE NEAR MISS EM TESTE DE KOLMOGOROV SMIRNOV EM 17/06/2025 #######
  #######ACRÉSCIMO DE ENN EM TESTE DE KOLMOGOROV SMIRNOV EM 17/06/2025 #######
  #######ACRÉSCIMO DE OSS EM TESTE DE KOLMOGOROV SMIRNOV EM 18/06/2025 #######
  #######ACRÉSCIMO DE RANDOM DOWSAMPLING EM TESTE DE KOLMOGOROV SMIRNOV EM 18/06/2025 #######
  #######ACRÉSCIMO DE SMOTE TL EM TESTE DE KOLMOGOROV SMIRNOV EM 02/07/2025 #######
  #######ACRÉSCIMO DE SMOTE ENN EM TESTE DE KOLMOGOROV SMIRNOV EM 05/07/2025 #######
  #######ACRÉSCIMO DE SBC EM TESTE DE KOLMOGOROV SMIRNOV EM 10/07/2025 #######
  #######ACRÉSCIMO DE SMOTE IPF EM TESTE DE KOLMOGOROV SMIRNOV EM 14/07/2025 #######
  #######ACRÉSCIMO DE SPIDER EM TESTE DE KOLMOGOROV SMIRNOV EM 14/07/2025 #######
  
  #######ACRÉSCIMO DE TESTE DE KOLMOGOROV SMIRNOV EM 28/03/2025#######
  
  
  ######ACRÉSCIMO DE Generalized Procrustes Test EM 28/03/2025####
  
  ######ACRÉSCIMO DE SMOTE_NC EM Generalized Procrustes Test EM 29/05/2025######
  
  ######ACRÉSCIMO DE BORDERLINE SMOTE EM Generalized Procrustes Test EM 05/06/2025######
  
  ######ACRÉSCIMO DE SVM SMOTE EM Generalized Procrustes Test EM 06/06/2025######
  
  ######ACRÉSCIMO DE ADASYN EM Generalized Procrustes Test EM 08/06/2025######
  
  ######ACRÉSCIMO DE RANDOM UPSAMPLING EM Generalized Procrustes Test EM 09/06/2025######
  
  #######ACRÉSCIMO DE TOMEK LINKS EM Generalized Procrustes Test EM 14/06/2025 #######
  
  #######ACRÉSCIMO DE NEAR MISS EM Generalized Procrustes Test EM 17/06/2025 #######
  
  #######ACRÉSCIMO DE ENN EM Generalized Procrustes Test EM 17/06/2025 #######
  
  #######ACRÉSCIMO DE OSS EM Generalized Procrustes Test EM 18/06/2025 #######
  
  ######ACRÉSCIMO DE RANDOM DOWNSAMPLING EM Generalized Procrustes Test EM 18/06/2025######
  
  ######ACRÉSCIMO DE SMOTE TL EM Generalized Procrustes Test EM 02/07/2025######
  
  ######ACRÉSCIMO DE SMOTE ENN EM Generalized Procrustes Test EM 05/07/2025######
  
  ######ACRÉSCIMO DE SBC EM Generalized Procrustes Test EM 10/07/2025######
  
  ######ACRÉSCIMO DE SMOTE IPF EM Generalized Procrustes Test EM 14/07/2025######
  
  ######ACRÉSCIMO DE SPIDER EM Generalized Procrustes Test EM 14/07/2025######
  
  
  output$procrustes_tabela_teste <- renderDT({
    original_data <- as.data.frame(filedata())
    smote_data <- NULL
    class_col <- NULL
    
    if (isTRUE(runSMOTENC()) && !is.null(SMOTENC_Data()) && !is.null(input$target_nc)) {
      smote_data <- SMOTENC_Data()$syn_data
      class_col <- input$target_nc
      
    } else if (isTRUE(runSMOTE()) && !is.null(SMOTE_Data()) && !is.null(input$target)) {
      smote_data <- SMOTE_Data()$syn_data
      class_col <- input$target
      
    } else if (isTRUE(runBorderlineSMOTE()) && !is.null(BorderlineSMOTE_Data()) && !is.null(input$target_borderline)) {
      smote_data <- BorderlineSMOTE_Data()$syn_data
      class_col <- input$target_borderline
      
    } else if (isTRUE(runSVM_SMOTE()) && !is.null(SVM_SMOTE_Data()) && !is.null(input$target_svm)) {
      smote_data <- SVM_SMOTE_Data()$syn_data
      class_col <- input$target_svm
      
    } else if (isTRUE(runADASYN()) && !is.null(ADASYN_Data()) && !is.null(input$target_adasyn)) {
      smote_data <- ADASYN_Data()$syn_data
      class_col <- input$target_adasyn
      
    } else if (isTRUE(runRU()) && !is.null(RU_Data()) && !is.null(input$target_ru)) {
      smote_data <- RU_Data()$syn_data
      class_col <- input$target_ru
      
    } else if (isTRUE(runTomek()) && !is.null(Tomek_Data()) && !is.null(input$target_tomek)) {
      smote_data <- Tomek_Data()$removed_data
      class_col <- input$target_tomek
      
    } else if (isTRUE(runNearMiss()) && !is.null(NearMiss_Data()) && !is.null(input$target_nearmiss)) {
      smote_data <- NearMiss_Data()$removed_data
      class_col <- input$target_nearmiss
      
    } else if (isTRUE(runENN()) && !is.null(ENN_Data()) && !is.null(input$target_enn)) {
      smote_data <- ENN_Data()$removed_data
      class_col <- input$target_enn
      
    } else if (isTRUE(runOSS()) && !is.null(OSS_Data()) && !is.null(input$target_oss)) {
      smote_data <- OSS_Data()$removed_data
      class_col <- input$target_oss
      
    } else if (isTRUE(runRD()) && !is.null(RD_Data()) && !is.null(input$target_rd)) {
      smote_data <- RD_Data()$removed_data
      class_col <- input$target_rd
      
      
      ###BLOCO ADICIONADO EM 10/07/2025
      
    } else if (isTRUE(runSBC()) && !is.null(SBC_Data()) && !is.null(input$target_sbc)) {
      smote_data <- SBC_Data()$removed_data
      class_col <- input$target_sbc
      
      
      ###BLOCO ADICIONADO EM 10/07/2025
      
      
    } else if (isTRUE(runSMOTETL()) && !is.null(SMOTETL_Data()) && !is.null(input$target_tl)) {

      # BLOCO SMOTE-TL 
      
      class_col <- input$target_tl
      smote_data_added <- SMOTETL_Data()$syn_data
      smote_data_removed <- SMOTETL_Data()$removed_data
      
      names(original_data)[names(original_data) == class_col] <- "class"
      original_data$Tipo <- "Original"
      
      if (!is.null(smote_data_added) && nrow(smote_data_added) > 0) {
        names(smote_data_added)[names(smote_data_added) == class_col] <- "class"
        smote_data_added$class <- paste(smote_data_added$class, "Synthetic")
        smote_data_added$Tipo <- "Synthetic"
      } else {
        smote_data_added <- original_data[0, , drop = FALSE]
      }
      
      if (!is.null(smote_data_removed) && nrow(smote_data_removed) > 0) {
        names(smote_data_removed)[names(smote_data_removed) == class_col] <- "class"
        smote_data_removed$class <- paste(smote_data_removed$class, "Removed")
        smote_data_removed$Tipo <- "Removed"
      } else {
        smote_data_removed <- original_data[0, , drop = FALSE]
      }
      
      smote_data <- dplyr::bind_rows(smote_data_added, smote_data_removed)
      
      feature_cols <- intersect(names(original_data), names(smote_data))
      original_data <- original_data[, feature_cols, drop = FALSE]
      smote_data <- smote_data[, feature_cols, drop = FALSE]
      
      dados <- dplyr::bind_rows(original_data, smote_data)
      dados$SampleID <- 1:nrow(dados)
      dados$class <- as.character(dados$class)
      dados$Tipo <- ifelse(grepl("Removed$", dados$class), "Removed",
                           ifelse(grepl("Synthetic$", dados$class), "Synthetic", "Original"))
      dados$classe_base <- gsub(" Synthetic$| Removed$", "", dados$class)
      
    } 
    
    
    #############BLOCO ADICIONADO EM 05/07/2025
    
    else if (isTRUE(runSMOTEENN()) && !is.null(SMOTEENN_Data()) && !is.null(input$target_enn2)) {
      
      # BLOCO SMOTE-ENN 
      
      class_col <- input$target_enn2
      smote_data_added <- SMOTEENN_Data()$syn_data
      smote_data_removed <- SMOTEENN_Data()$removed_data
      
      names(original_data)[names(original_data) == class_col] <- "class"
      original_data$Tipo <- "Original"
      
      if (!is.null(smote_data_added) && nrow(smote_data_added) > 0) {
        names(smote_data_added)[names(smote_data_added) == class_col] <- "class"
        smote_data_added$class <- paste(smote_data_added$class, "Synthetic")
        smote_data_added$Tipo <- "Synthetic"
      } else {
        smote_data_added <- original_data[0, , drop = FALSE]
      }
      
      if (!is.null(smote_data_removed) && nrow(smote_data_removed) > 0) {
        names(smote_data_removed)[names(smote_data_removed) == class_col] <- "class"
        smote_data_removed$class <- paste(smote_data_removed$class, "Removed")
        smote_data_removed$Tipo <- "Removed"
      } else {
        smote_data_removed <- original_data[0, , drop = FALSE]
      }
      
      smote_data <- dplyr::bind_rows(smote_data_added, smote_data_removed)
      
      feature_cols <- intersect(names(original_data), names(smote_data))
      original_data <- original_data[, feature_cols, drop = FALSE]
      smote_data <- smote_data[, feature_cols, drop = FALSE]
      
      dados <- dplyr::bind_rows(original_data, smote_data)
      dados$SampleID <- 1:nrow(dados)
      dados$class <- as.character(dados$class)
      dados$Tipo <- ifelse(grepl("Removed$", dados$class), "Removed",
                           ifelse(grepl("Synthetic$", dados$class), "Synthetic", "Original"))
      dados$classe_base <- gsub(" Synthetic$| Removed$", "", dados$class)
      
    } 
    
    
    #############BLOCO ADICIONADO EM 05/07/2025
  
    #############BLOCO ADICIONADO EM 14/07/2025
    
    else if (isTRUE(runSMOTEIPF()) && !is.null(SMOTEIPF_Data()) && !is.null(input$target_ipf)) {
      
      # BLOCO SMOTE-IPF 
      
      class_col <- input$target_ipf
      smote_data_added <- SMOTEIPF_Data()$syn_data
      smote_data_removed <- SMOTEIPF_Data()$removed_data
      
      names(original_data)[names(original_data) == class_col] <- "class"
      original_data$Tipo <- "Original"
      
      if (!is.null(smote_data_added) && nrow(smote_data_added) > 0) {
        names(smote_data_added)[names(smote_data_added) == class_col] <- "class"
        smote_data_added$class <- paste(smote_data_added$class, "Synthetic")
        smote_data_added$Tipo <- "Synthetic"
      } else {
        smote_data_added <- original_data[0, , drop = FALSE]
      }
      
      if (!is.null(smote_data_removed) && nrow(smote_data_removed) > 0) {
        names(smote_data_removed)[names(smote_data_removed) == class_col] <- "class"
        smote_data_removed$class <- paste(smote_data_removed$class, "Removed")
        smote_data_removed$Tipo <- "Removed"
      } else {
        smote_data_removed <- original_data[0, , drop = FALSE]
      }
      
      smote_data <- dplyr::bind_rows(smote_data_added, smote_data_removed)
      
      feature_cols <- intersect(names(original_data), names(smote_data))
      original_data <- original_data[, feature_cols, drop = FALSE]
      smote_data <- smote_data[, feature_cols, drop = FALSE]
      
      dados <- dplyr::bind_rows(original_data, smote_data)
      dados$SampleID <- 1:nrow(dados)
      dados$class <- as.character(dados$class)
      dados$Tipo <- ifelse(grepl("Removed$", dados$class), "Removed",
                           ifelse(grepl("Synthetic$", dados$class), "Synthetic", "Original"))
      dados$classe_base <- gsub(" Synthetic$| Removed$", "", dados$class)
      
    } 
    
    
    #############BLOCO ADICIONADO EM 14/07/2025
    
    
    
    #############BLOCO ADICIONADO EM 14/07/2025
    
    else if (isTRUE(runSPIDER()) && !is.null(SPIDER_Data()) && !is.null(input$target_spider)) {
      
      # BLOCO SPIDER 
      
      class_col <- input$target_spider
      smote_data_added <- SPIDER_Data()$syn_data
      smote_data_removed <- SPIDER_Data()$removed_data
      
      names(original_data)[names(original_data) == class_col] <- "class"
      original_data$Tipo <- "Original"
      
      if (!is.null(smote_data_added) && nrow(smote_data_added) > 0) {
        names(smote_data_added)[names(smote_data_added) == class_col] <- "class"
        smote_data_added$class <- paste(smote_data_added$class, "Synthetic")
        smote_data_added$Tipo <- "Synthetic"
      } else {
        smote_data_added <- original_data[0, , drop = FALSE]
      }
      
      if (!is.null(smote_data_removed) && nrow(smote_data_removed) > 0) {
        names(smote_data_removed)[names(smote_data_removed) == class_col] <- "class"
        smote_data_removed$class <- paste(smote_data_removed$class, "Removed")
        smote_data_removed$Tipo <- "Removed"
      } else {
        smote_data_removed <- original_data[0, , drop = FALSE]
      }
      
      smote_data <- dplyr::bind_rows(smote_data_added, smote_data_removed)
      
      feature_cols <- intersect(names(original_data), names(smote_data))
      original_data <- original_data[, feature_cols, drop = FALSE]
      smote_data <- smote_data[, feature_cols, drop = FALSE]
      
      dados <- dplyr::bind_rows(original_data, smote_data)
      dados$SampleID <- 1:nrow(dados)
      dados$class <- as.character(dados$class)
      dados$Tipo <- ifelse(grepl("Removed$", dados$class), "Removed",
                           ifelse(grepl("Synthetic$", dados$class), "Synthetic", "Original"))
      dados$classe_base <- gsub(" Synthetic$| Removed$", "", dados$class)
    } 
    
    #############BLOCO ADICIONADO EM 14/07/2025
    
    
    
    
    
    
    
    
    else {
      return(data.frame())
    }
    
  ##  if (!isTRUE(runSMOTETL())) {
    
    ## if (!isTRUE(runSMOTETL()) && !isTRUE(runSMOTEENN()) ) {  #MODIFICADO EM 05/07/2025  
    
   ## if (!isTRUE(runSMOTETL()) && !isTRUE(runSMOTEENN()) && !isTRUE(runSMOTEIPF()) ) {  #MODIFICADO EM 14/07/2025  
    
    
    if (!isTRUE(runSMOTETL()) && !isTRUE(runSMOTEENN()) && !isTRUE(runSMOTEIPF()) && !isTRUE(runSPIDER()) ) {  #MODIFICADO EM 14/07/2025  
    
      if (is.null(smote_data) || !(class_col %in% names(original_data))) return(data.frame())
      
      names(original_data)[names(original_data) == class_col] <- "class"
      names(smote_data)[names(smote_data) == class_col] <- "class"
      
      suffix <- if (isTRUE(runTomek()) || isTRUE(runNearMiss()) || isTRUE(runENN()) ||
                    isTRUE(runOSS()) || isTRUE(runRD())   || isTRUE(runSBC())           ) "Removed" else "Synthetic"
      
      smote_data$class <- paste(smote_data$class, suffix)
      smote_data$fake_cat <- NULL
      
      feature_cols <- intersect(names(original_data), names(smote_data))
      original_data <- original_data[, feature_cols, drop = FALSE]
      smote_data <- smote_data[, feature_cols, drop = FALSE]
      
      original_data$Tipo <- "Original"
      smote_data$Tipo <- suffix
      
      dados <- dplyr::bind_rows(original_data, smote_data)
      dados$SampleID <- 1:nrow(dados)
      dados$class <- as.character(dados$class)
      dados$Tipo <- ifelse(grepl(suffix, dados$class), suffix, "Original")
      dados$classe_base <- gsub(paste0(" ", suffix), "", dados$class)
    }
    
    num_vars <- names(Filter(is.numeric, dados))
    num_vars <- setdiff(num_vars, "SampleID")
    validate(need(length(num_vars) >= 2, "É necessário ao menos 2 Variables numéricas para o teste de Procrustes."))
    
    resultados <- data.frame()
    
    
    ####MODIFICADO EM 05/07/2025
    ####MODIFICADO EM 14/07/2025
    
    #  tipos <- if (isTRUE(runSMOTETL())) c("Synthetic", "Removida") else unique(dados$Tipo[dados$Tipo != "Original"])
    
    ## tipos <- if (isTRUE(runSMOTETL()) || isTRUE(runSMOTEENN())  ) c("Synthetic", "Removida") else unique(dados$Tipo[dados$Tipo != "Original"])
    
    tipos <- if (isTRUE(runSMOTETL()) || isTRUE(runSMOTEENN()) || isTRUE(runSMOTEIPF()) || isTRUE(runSPIDER())  ) c("Synthetic", "Removed") else unique(dados$Tipo[dados$Tipo != "Original"])
    
    
    ####MODIFICADO EM 05/07/2025
    ####MODIFICADO EM 14/07/2025
    
    
    for (classe in unique(dados$classe_base)) {
      for (tipo in tipos) {
        originais <- subset(dados, Tipo == "Original" & classe_base == classe)
        sinteticas <- subset(dados, Tipo == tipo & classe_base == classe)
        
        n <- min(nrow(originais), nrow(sinteticas))
        if (n >= 2) {
          vars_comuns <- intersect(names(originais), names(sinteticas))
          vars_comuns <- intersect(vars_comuns, num_vars)
          
          # Removendo Variables com desvio padrão zero ou NA
          
          variancias <- sapply(originais[, vars_comuns, drop = FALSE], sd, na.rm = TRUE)
          vars_validas <- vars_comuns[is.finite(variancias) & variancias > 0]
          
          if (length(vars_validas) >= 2) {
            X <- scale(originais[1:n, vars_validas])
            Y <- scale(sinteticas[1:n, vars_validas])
            
            test <- vegan::protest(X, Y, permutations = 999)
            
            resultados <- rbind(resultados, data.frame(
              Class = classe,
              Type = tipo,
              p_value = signif(test$signif, 4)
            ))
          }
        }
      }
    }
    
    if (nrow(resultados) == 0) return(NULL)
    
    datatable(resultados, rownames = FALSE, options = list(
      pageLength = 10,
      dom = 'tip'
    ))
  })
  
  
  
  
  
  ##obs: https://cran.r-project.org/web/packages/vegan/vegan.pdf
  
  ######ACRÉSCIMO DE Generalized Procrustes Test EM 28/03/2025####
  
  
  
  ######ACRÉSCIMO DE SMOTE_NC EM Generalized Procrustes Test EM 29/05/2025######
  
  ######ACRÉSCIMO DE BORDERLINE SMOTE EM Generalized Procrustes Test EM 05/06/2025######
  
  ######ACRÉSCIMO DE SVM SMOTE EM Generalized Procrustes Test EM 06/06/2025######
  
  ######ACRÉSCIMO DE ADASYN EM Generalized Procrustes Test EM 08/06/2025######
  
  ######ACRÉSCIMO DE RANDOM UPSAMPLING EM Generalized Procrustes Test EM 09/06/2025######
  
  #######ACRÉSCIMO DE TOMEK LINKS EM Generalized Procrustes Test EM 14/06/2025 #######
  
  #######ACRÉSCIMO DE NEAR MISS EM Generalized Procrustes Test EM 17/06/2025 #######
  
  #######ACRÉSCIMO DE ENN EM Generalized Procrustes Test EM 17/06/2025 #######
  
  #######ACRÉSCIMO DE OSS EM Generalized Procrustes Test EM 17/06/2025 #######
  
  ######ACRÉSCIMO DE RANDOM DOWNSAMPLING EM Generalized Procrustes Test EM 18/06/2025######
  
  ######ACRÉSCIMO DE SMOTE TL EM Generalized Procrustes Test EM 02/07/2025######
  
  ######ACRÉSCIMO DE SMOTE ENN EM Generalized Procrustes Test EM 02/07/2025######
  
  ######ACRÉSCIMO DE SBC EM Generalized Procrustes Test EM 10/07/2025######
  
  ######ACRÉSCIMO DE SMOTE IPF EM Generalized Procrustes Test EM 14/07/2025######
  
  ######ACRÉSCIMO DE SPIDER EM Generalized Procrustes Test EM 14/07/2025######
  
  
  
  ##ACRÉSCIMO DE T² DE HOTELLING E DIST.MAHALANOBIS EM 29/03/2025##
  
  ###MODIFICAÇÃO EM 30/03/2025 - ACRÉSCIMO DE TESTE DE Normality MULTIVARIADA
  
  
  ##ACRÉSCIMO DO TESTE DE ROYSTON(EXTENSÃO MULTIVARIADA DO TESTE DE SHAPIRO-WILK) EM 02/05/2025
  #observeEvent(input$run_hotelling, {
  # req(smote_history$data)
  
  ##  output$hotelling_table <- renderDT({
  ##    req(smote_history$data)
  
  ######ACRÉSCIMO DE SMOTE_NC EM TESTE DE ROYSTON e TESTE DE Normality MULTIVARIADA EM 29/05/2025
  
  ######ACRÉSCIMO DE BORDERLINE SMOTE EM TESTE DE ROYSTON e TESTE DE Normality MULTIVARIADA EM 05/06/2025
  
  ######ACRÉSCIMO DE SVM SMOTE EM TESTE DE ROYSTON e TESTE DE Normality MULTIVARIADA EM 06/06/2025
  
  ######ACRÉSCIMO DE ADASYN EM TESTE DE ROYSTON e TESTE DE Normality MULTIVARIADA EM 08/06/2025
  
  ######ACRÉSCIMO DE RANDOM UPSAMPLING EM TESTE DE ROYSTON e TESTE DE Normality MULTIVARIADA EM 09/06/2025
  
  ######ACRÉSCIMO DE TOMEK LINKS EM TESTE DE ROYSTON e TESTE DE Normality MULTIVARIADA EM 14/06/2025
  
  ######ACRÉSCIMO DE NEAR MISS EM TESTE DE ROYSTON e TESTE DE Normality MULTIVARIADA EM 17/06/2025
  
  ######ACRÉSCIMO DE ENN EM TESTE DE ROYSTON e TESTE DE Normality MULTIVARIADA EM 17/06/2025
  
  ######ACRÉSCIMO DE OSS EM TESTE DE ROYSTON e TESTE DE Normality MULTIVARIADA EM 18/06/2025
  
  ######ACRÉSCIMO DE RANDOM DOWNSAMPLING EM TESTE DE ROYSTON e TESTE DE Normality MULTIVARIADA EM 18/06/2025
  
  ######ACRÉSCIMO DE SMOTE TL EM TESTE DE ROYSTON e TESTE DE Normality MULTIVARIADA EM 02/07/2025
  
  ######ACRÉSCIMO DE SMOTE ENN EM TESTE DE ROYSTON e TESTE DE Normality MULTIVARIADA EM 05/07/2025
  
  ######ACRÉSCIMO DE SBC EM TESTE DE ROYSTON e TESTE DE Normality MULTIVARIADA EM 10/07/2025
  
  ######ACRÉSCIMO DE SMOTE IPF EM TESTE DE ROYSTON e TESTE DE Normality MULTIVARIADA EM 14/07/2025
  
  ######ACRÉSCIMO DE SPIDER EM TESTE DE ROYSTON e TESTE DE Normality MULTIVARIADA EM 14/07/2025
  
  
  observe({
    library(MVN)
    
    original_data <- as.data.frame(filedata())
    smote_data <- NULL
    class_col <- NULL
    
    if (isTRUE(runSMOTENC()) && !is.null(SMOTENC_Data()) && !is.null(input$target_nc)) {
      smote_data <- SMOTENC_Data()$syn_data
      class_col <- input$target_nc
      
    } else if (isTRUE(runSMOTE()) && !is.null(SMOTE_Data()) && !is.null(input$target)) {
      smote_data <- SMOTE_Data()$syn_data
      class_col <- input$target
      
    } else if (isTRUE(runBorderlineSMOTE()) && !is.null(BorderlineSMOTE_Data()) && !is.null(input$target_borderline)) {
      smote_data <- BorderlineSMOTE_Data()$syn_data
      class_col <- input$target_borderline
      
    } else if (isTRUE(runSVM_SMOTE()) && !is.null(SVM_SMOTE_Data()) && !is.null(input$target_svm)) {
      smote_data <- SVM_SMOTE_Data()$syn_data
      class_col <- input$target_svm
      
    } else if (isTRUE(runADASYN()) && !is.null(ADASYN_Data()) && !is.null(input$target_adasyn)) {
      smote_data <- ADASYN_Data()$syn_data
      class_col <- input$target_adasyn
      
    } else if (isTRUE(runRU()) && !is.null(RU_Data()) && !is.null(input$target_ru)) {
      smote_data <- RU_Data()$syn_data
      class_col <- input$target_ru
      
    } else if (isTRUE(runTomek()) && !is.null(Tomek_Data()) && !is.null(input$target_tomek)) {
      smote_data <- Tomek_Data()$removed_data
      class_col <- input$target_tomek
      
    } else if (isTRUE(runNearMiss()) && !is.null(NearMiss_Data()) && !is.null(input$target_nearmiss)) {
      smote_data <- NearMiss_Data()$removed_data
      class_col <- input$target_nearmiss
      
    } else if (isTRUE(runENN()) && !is.null(ENN_Data()) && !is.null(input$target_enn)) {
      smote_data <- ENN_Data()$removed_data
      class_col <- input$target_enn
      
    } else if (isTRUE(runOSS()) && !is.null(OSS_Data()) && !is.null(input$target_oss)) {
      smote_data <- OSS_Data()$removed_data
      class_col <- input$target_oss
      
    } else if (isTRUE(runRD()) && !is.null(RD_Data()) && !is.null(input$target_rd)) {
      smote_data <- RD_Data()$removed_data
      class_col <- input$target_rd
      
      ####BLOCO ADICIONADO EM 10/07/2025
      
    } else if (isTRUE(runSBC()) && !is.null(SBC_Data()) && !is.null(input$target_sbc)) {
      smote_data <- SBC_Data()$removed_data
      class_col <- input$target_sbc
      
      
      ####BLOCO ADICIONADO EM 10/07/2025
      
      
      
    } else if (isTRUE(runSMOTETL()) && !is.null(SMOTETL_Data()) && !is.null(input$target_tl)) {
      
      ## BLOCO SMOTE-TL
      
      class_col <- input$target_tl
      smote_data_added <- SMOTETL_Data()$syn_data
      smote_data_removed <- SMOTETL_Data()$removed_data
      
      names(original_data)[names(original_data) == class_col] <- "class"
      original_data$Tipo <- "Original"
      
      if (!is.null(smote_data_added) && nrow(smote_data_added) > 0) {
        names(smote_data_added)[names(smote_data_added) == class_col] <- "class"
        smote_data_added$class <- paste(smote_data_added$class, "Synthetic")
        smote_data_added$Tipo <- "Synthetic"
      } else {
        smote_data_added <- original_data[0, , drop = FALSE]
      }
      
      if (!is.null(smote_data_removed) && nrow(smote_data_removed) > 0) {
        names(smote_data_removed)[names(smote_data_removed) == class_col] <- "class"
        smote_data_removed$class <- paste(smote_data_removed$class, "Removed")
        smote_data_removed$Tipo <- "Removed"
      } else {
        smote_data_removed <- original_data[0, , drop = FALSE]
      }
      
      smote_data <- dplyr::bind_rows(smote_data_added, smote_data_removed)
      
      feature_cols <- intersect(names(original_data), names(smote_data))
      original_data <- original_data[, feature_cols, drop = FALSE]
      smote_data <- smote_data[, feature_cols, drop = FALSE]
      
      dados <- dplyr::bind_rows(original_data, smote_data)
      dados$SampleID <- 1:nrow(dados)
      dados$class <- as.character(dados$class)
      dados$Tipo <- ifelse(grepl("Removed$", dados$class), "Removed",
                           ifelse(grepl("Synthetic$", dados$class), "Synthetic", "Original"))
      dados$classe_base <- gsub(" Synthetic$| Removed$", "", dados$class)
      
    } 
    
    
    ######BLOCO ADICIONADO EM 05/07/2025
    else if (isTRUE(runSMOTEENN()) && !is.null(SMOTEENN_Data()) && !is.null(input$target_enn2)) {
      
      ## BLOCO SMOTE-ENN
      
      class_col <- input$target_enn2
      smote_data_added <- SMOTEENN_Data()$syn_data
      smote_data_removed <- SMOTEENN_Data()$removed_data
      
      names(original_data)[names(original_data) == class_col] <- "class"
      original_data$Tipo <- "Original"
      
      if (!is.null(smote_data_added) && nrow(smote_data_added) > 0) {
        names(smote_data_added)[names(smote_data_added) == class_col] <- "class"
        smote_data_added$class <- paste(smote_data_added$class, "Synthetic")
        smote_data_added$Tipo <- "Synthetic"
      } else {
        smote_data_added <- original_data[0, , drop = FALSE]
      }
      
      if (!is.null(smote_data_removed) && nrow(smote_data_removed) > 0) {
        names(smote_data_removed)[names(smote_data_removed) == class_col] <- "class"
        smote_data_removed$class <- paste(smote_data_removed$class, "Removed")
        smote_data_removed$Tipo <- "Removed"
      } else {
        smote_data_removed <- original_data[0, , drop = FALSE]
      }
      
      smote_data <- dplyr::bind_rows(smote_data_added, smote_data_removed)
      
      feature_cols <- intersect(names(original_data), names(smote_data))
      original_data <- original_data[, feature_cols, drop = FALSE]
      smote_data <- smote_data[, feature_cols, drop = FALSE]
      
      dados <- dplyr::bind_rows(original_data, smote_data)
      dados$SampleID <- 1:nrow(dados)
      dados$class <- as.character(dados$class)
      dados$Tipo <- ifelse(grepl("Removed$", dados$class), "Removed",
                           ifelse(grepl("Synthetic$", dados$class), "Synthetic", "Original"))
      dados$classe_base <- gsub(" Synthetic$| Removed$", "", dados$class)
      
    } 
    
    
    ######BLOCO ADICIONADO EM 05/07/2025
    
    ######BLOCO ADICIONADO EM 14/07/2025
    else if (isTRUE(runSMOTEIPF()) && !is.null(SMOTEIPF_Data()) && !is.null(input$target_ipf)) {
      
      ## BLOCO SMOTE-IPF
      
      class_col <- input$target_ipf
      smote_data_added <- SMOTEIPF_Data()$syn_data
      smote_data_removed <- SMOTEIPF_Data()$removed_data
      
      names(original_data)[names(original_data) == class_col] <- "class"
      original_data$Tipo <- "Original"
      
      if (!is.null(smote_data_added) && nrow(smote_data_added) > 0) {
        names(smote_data_added)[names(smote_data_added) == class_col] <- "class"
        smote_data_added$class <- paste(smote_data_added$class, "Synthetic")
        smote_data_added$Tipo <- "Synthetic"
      } else {
        smote_data_added <- original_data[0, , drop = FALSE]
      }
      
      if (!is.null(smote_data_removed) && nrow(smote_data_removed) > 0) {
        names(smote_data_removed)[names(smote_data_removed) == class_col] <- "class"
        smote_data_removed$class <- paste(smote_data_removed$class, "Removed")
        smote_data_removed$Tipo <- "Removed"
      } else {
        smote_data_removed <- original_data[0, , drop = FALSE]
      }
      
      smote_data <- dplyr::bind_rows(smote_data_added, smote_data_removed)
      
      feature_cols <- intersect(names(original_data), names(smote_data))
      original_data <- original_data[, feature_cols, drop = FALSE]
      smote_data <- smote_data[, feature_cols, drop = FALSE]
      
      dados <- dplyr::bind_rows(original_data, smote_data)
      dados$SampleID <- 1:nrow(dados)
      dados$class <- as.character(dados$class)
      dados$Tipo <- ifelse(grepl("Removed$", dados$class), "Removed",
                           ifelse(grepl("Synthetic$", dados$class), "Synthetic", "Original"))
      dados$classe_base <- gsub(" Synthetic$| Removed$", "", dados$class)
      
    } 
    
    
    ######BLOCO ADICIONADO EM 14/07/2025
    
    
    
    ###### BLOCO ADICIONADO EM 14/07/2025
    
    else if (isTRUE(runSPIDER()) && !is.null(SPIDER_Data()) && !is.null(input$target_spider)) {
      
      ## BLOCO SPIDER
      
      class_col <- input$target_spider
      smote_data_added <- SPIDER_Data()$syn_data
      smote_data_removed <- SPIDER_Data()$removed_data
      
      names(original_data)[names(original_data) == class_col] <- "class"
      original_data$Tipo <- "Original"
      
      if (!is.null(smote_data_added) && nrow(smote_data_added) > 0) {
        names(smote_data_added)[names(smote_data_added) == class_col] <- "class"
        smote_data_added$class <- paste(smote_data_added$class, "Synthetic")
        smote_data_added$Tipo <- "Synthetic"
      } else {
        smote_data_added <- original_data[0, , drop = FALSE]
      }
      
      if (!is.null(smote_data_removed) && nrow(smote_data_removed) > 0) {
        names(smote_data_removed)[names(smote_data_removed) == class_col] <- "class"
        smote_data_removed$class <- paste(smote_data_removed$class, "Removed")
        smote_data_removed$Tipo <- "Removed"
      } else {
        smote_data_removed <- original_data[0, , drop = FALSE]
      }
      
      smote_data <- dplyr::bind_rows(smote_data_added, smote_data_removed)
      
      feature_cols <- intersect(names(original_data), names(smote_data))
      original_data <- original_data[, feature_cols, drop = FALSE]
      smote_data <- smote_data[, feature_cols, drop = FALSE]
      
      dados <- dplyr::bind_rows(original_data, smote_data)
      dados$SampleID <- 1:nrow(dados)
      dados$class <- as.character(dados$class)
      dados$Tipo <- ifelse(grepl("Removed$", dados$class), "Removed",
                           ifelse(grepl("Synthetic$", dados$class), "Synthetic", "Original"))
      dados$classe_base <- gsub(" Synthetic$| Removed$", "", dados$class)
      
    }
    
    ###### BLOCO ADICIONADO EM 14/07/2025
    
    
    else {
      return()
    }
    
   # if (!isTRUE(runSMOTETL())) {
    
   ## if (!isTRUE(runSMOTETL()) && !isTRUE(runSMOTEENN()) ) {  #MODIFICADO EM 05/07/2025
    
    
  ##  if (!isTRUE(runSMOTETL()) && !isTRUE(runSMOTEENN()) && !isTRUE(runSMOTEIPF()) ) {  #MODIFICADO EM 14/07/2025
    
    
    if (!isTRUE(runSMOTETL()) && !isTRUE(runSMOTEENN()) && !isTRUE(runSMOTEIPF()) && !isTRUE(runSPIDER()) ) {  #MODIFICADO EM 14/07/2025
    
    
      if (is.null(smote_data) || !(class_col %in% names(original_data))) return()
      
      names(original_data)[names(original_data) == class_col] <- "class"
      names(smote_data)[names(smote_data) == class_col] <- "class"
      
      suffix <- if (isTRUE(runTomek()) || isTRUE(runNearMiss()) || isTRUE(runENN()) || 
                    isTRUE(runOSS()) || isTRUE(runRD())  || isTRUE(runSBC())        ) "Removed" else "Synthetic"
      
      if (nrow(smote_data) > 0) {
        smote_data$class <- paste(smote_data$class, suffix)
        smote_data$fake_cat <- NULL
      } else {
        showNotification("No Synthetic/Removed samples were generated for this method.", type = "warning")
        return()
      }
      
      feature_cols <- intersect(names(original_data), names(smote_data))
      original_data <- original_data[, feature_cols, drop = FALSE]
      smote_data <- smote_data[, feature_cols, drop = FALSE]
      
      original_data$Tipo <- "Original"
      smote_data$Tipo <- suffix
      
      dados <- dplyr::bind_rows(original_data, smote_data)
      dados$SampleID <- 1:nrow(dados)
      dados$class <- as.character(dados$class)
      dados$Tipo <- ifelse(grepl(suffix, dados$class), suffix, "Original")
      dados$classe_base <- gsub(paste0(" ", suffix), "", dados$class)
    }
    
    
    classes_originais <- unique(dados$classe_base)
    classes_com_sintetica <- classes_originais
    
    if (length(classes_com_sintetica) == 0) {
      output$Normality_table <- renderDataTable({ datatable(data.frame()) })
      output$hotelling_table <- renderDataTable({ datatable(data.frame()) })
      return()
    }
    
    # Normality Multivariada 
    
    resultados_Normality <- lapply(classes_com_sintetica, function(classe) {
      
      ####MODIFICADO EM 05/07/2025
      ####MODIFICADO EM 14/07/2025
      
    #  classe_sint <- paste(classe, ifelse(isTRUE(runSMOTETL()), "Synthetic", suffix))
      
      
     ## classe_sint <- paste(classe, ifelse(isTRUE(runSMOTETL() ||  isTRUE(runSMOTEENN()) ), "Synthetic", suffix))
      
      classe_sint <- paste(classe, ifelse(isTRUE(runSMOTETL() ||  isTRUE(runSMOTEENN()) ||  isTRUE(runSMOTEIPF()) ||  isTRUE(runSPIDER()) ), "Synthetic", suffix))
      
      ####MODIFICADO EM 05/07/2025
      ####MODIFICADO EM 14/07/2025
      
      subset_classe <- subset(dados, grepl(paste0("^", classe), class))
      
      num_vars <- names(Filter(is.numeric, subset_classe))
      num_vars <- setdiff(num_vars, "SampleID")
      subset_classe <- subset_classe[, num_vars, drop = FALSE]
      vars_validas <- num_vars[sapply(subset_classe[, num_vars, drop=FALSE], function(x) sd(x, na.rm=TRUE) > 0 && all(is.finite(x)))]
      
      if (length(vars_validas) < 2) return(data.frame())
      
      X <- subset_classe[, vars_validas, drop = FALSE]
      
      hz_val <- p_hz <- NA; status_hz <- "Error"
      roy_val <- p_roy <- NA; status_roy <- "Error"
      
      try({
        resultado_hz <- suppressWarnings(mvn(X, mvnTest = "hz"))
        linha_hz <- resultado_hz$multivariateNormality
        if (is.data.frame(linha_hz)) {
          hz_val <- as.numeric(linha_hz[1, "HZ"])
          p_hz <- as.numeric(linha_hz[1, "p value"])
          status_hz <- if (!is.na(p_hz) && p_hz > 0.05) "Normality OK" else "Violated normality"
        }
      }, silent = TRUE)
      
      try({
        resultado_roy <- suppressWarnings(mvn(X, mvnTest = "royston"))
        linha_royston <- resultado_roy$multivariateNormality
        linha_royston <- linha_royston[linha_royston$Test == "Royston", , drop = FALSE]
        if (nrow(linha_royston) == 1) {
          roy_val <- as.numeric(linha_royston[[2]])
          p_roy <- as.numeric(linha_royston[[3]])
          status_roy <- if (!is.na(p_roy) && p_roy > 0.05) "Normality OK" else "Violated normality"
        }
      }, silent = TRUE)
      
      data.frame(
        Class = classe,
        Statistics_HZ = round(hz_val, 4),
        p_value_HZ = round(p_hz, 4),
        Status_HZ = status_hz,
        Royston_Statistics = round(roy_val, 4),
        p_value_Royston = round(p_roy, 4),
        Status_Royston = status_roy
      )
    })
    
    df_Normality <- do.call(rbind, resultados_Normality)
    output$Normality_table <- DT::renderDataTable({
      DT::datatable(df_Normality, rownames = FALSE, options = list(pageLength = 10))
    })
    
    # Hotelling T²
    resultados_hotelling <- lapply(classes_com_sintetica, function(classe) {
      
      
      ####MODIFICADO EM 05/07/2025
      ####MODIFICADO EM 14/07/2025
      
      #  classe_sint <- paste(classe, ifelse(isTRUE(runSMOTETL()), "Synthetic", suffix))
      
      
      ## classe_sint <- paste(classe, ifelse(isTRUE(runSMOTETL() ||  isTRUE(runSMOTEENN()) ), "Synthetic", suffix))
      
      classe_sint <- paste(classe, ifelse(isTRUE(runSMOTETL() ||  isTRUE(runSMOTEENN()) ||  isTRUE(runSMOTEIPF()) ||  isTRUE(runSPIDER())  ), "Synthetic", suffix))
      
      
      
      ####MODIFICADO EM 05/07/2025
      ####MODIFICADO EM 14/07/2025
     
      subset_classe <- subset(dados, grepl(paste0("^", classe), class))
      
      X1 <- subset(subset_classe, class == classe)
      X2 <- subset(subset_classe, class == classe_sint)
      
      if (nrow(X1) < 2 || nrow(X2) < 2) {
        return(data.frame(Class = classe, Hotelling_T2 = NA, p_value = NA, Note = "Insufficient samples"))
      }
      
      num_vars <- names(Filter(is.numeric, subset_classe))
      num_vars <- setdiff(num_vars, "SampleID")
      vars_validas <- num_vars[sapply(subset_classe[, num_vars, drop=FALSE], function(x) sd(x, na.rm=TRUE) > 0 && all(is.finite(x)))]
      
      if (length(vars_validas) < 2) {
        return(data.frame(Class = classe, Hotelling_T2 = NA, p_value = NA, Note = "Insufficient numeric variables"))
      }
      
      X1 <- X1[, vars_validas, drop = FALSE]
      X2 <- X2[, vars_validas, drop = FALSE]
      
      resultado <- tryCatch({
        n1 <- nrow(X1); n2 <- nrow(X2); p <- ncol(X1)
        S1 <- cov(X1); S2 <- cov(X2)
        Spooled <- ((n1 - 1) * S1 + (n2 - 1) * S2) / (n1 + n2 - 2)
        mean_diff <- colMeans(X1) - colMeans(X2)
        T2_stat <- as.numeric((n1 * n2) / (n1 + n2) * t(mean_diff) %*% solve(Spooled + diag(1e-6, p)) %*% mean_diff)
        df1 <- p; df2 <- n1 + n2 - p - 1
        F_stat <- (df2 * T2_stat) / (df1 * (n1 + n2 - 2))
        pval <- pf(F_stat, df1, df2, lower.tail = FALSE)
        data.frame(Class = classe, Hotelling_T2 = round(T2_stat, 4), p_value = round(pval, 4), Note = "OK")
      }, error = function(e) {
        data.frame(Class = classe, Hotelling_T2 = NA, p_value = NA, Note = "Calculation error")
      })
      
      return(resultado)
    })
    
    df_hotelling <- do.call(rbind, resultados_hotelling)
    output$hotelling_table <- DT::renderDataTable({
      DT::datatable(df_hotelling, rownames = FALSE, options = list(pageLength = 10))
    })
  })
  
  
  
  ######ACRÉSCIMO DE SMOTE_NC EM TESTE DE ROYSTON e TESTE DE Normality MULTIVARIADA EM 29/05/2025
  
  
  ######ACRÉSCIMO DE BORDERLINE SMOTE EM TESTE DE ROYSTON e TESTE DE Normality MULTIVARIADA EM 05/06/2025
  
  ######ACRÉSCIMO DE SVM SMOTE EM TESTE DE ROYSTON e TESTE DE Normality MULTIVARIADA EM 06/06/2025
  
  ######ACRÉSCIMO DE ADASYN EM TESTE DE ROYSTON e TESTE DE Normality MULTIVARIADA EM 08/06/2025
  
  ######ACRÉSCIMO DE RANDOM UPSAMPLING EM TESTE DE ROYSTON e TESTE DE Normality MULTIVARIADA EM 09/06/2025
  
  
  ######ACRÉSCIMO DE TOMEK LINKS EM TESTE DE ROYSTON e TESTE DE Normality MULTIVARIADA EM 14/06/2025
  
  ######ACRÉSCIMO DE NEAR MISS EM TESTE DE ROYSTON e TESTE DE Normality MULTIVARIADA EM 17/06/2025
  
  ######ACRÉSCIMO DE ENN EM TESTE DE ROYSTON e TESTE DE Normality MULTIVARIADA EM 17/06/2025
  
  ######ACRÉSCIMO DE OSS EM TESTE DE ROYSTON e TESTE DE Normality MULTIVARIADA EM 18/06/2025
  
  ######ACRÉSCIMO DE RANDOM DOWNSAMPLING EM TESTE DE ROYSTON e TESTE DE Normality MULTIVARIADA EM 09/06/2025
  
  ######ACRÉSCIMO DE SMOTE TL EM TESTE DE ROYSTON e TESTE DE Normality MULTIVARIADA EM 02/07/2025
  
  ######ACRÉSCIMO DE SMOTE ENN EM TESTE DE ROYSTON e TESTE DE Normality MULTIVARIADA EM 05/07/2025
  
  ######ACRÉSCIMO DE SBC EM TESTE DE ROYSTON e TESTE DE Normality MULTIVARIADA EM 10/07/2025
  
  ######ACRÉSCIMO DE SMOTE IPF EM TESTE DE ROYSTON e TESTE DE Normality MULTIVARIADA EM 14/07/2025
  
  ######ACRÉSCIMO DE SPIDER EM TESTE DE ROYSTON e TESTE DE Normality MULTIVARIADA EM 14/07/2025
  
  
  
  ##ACRÉSCIMO DE T² DE HOTELLING E DIST.MAHALANOBIS EM 29/03/2025##
  
  ###ACRÉSCIMO DE PERMANOVA test EM 30/03/2025#####
  
  ######ACRÉSCIMO DE SMOTE_NC EM PERMANOVA test EM 29/05/2025
  
  ######ACRÉSCIMO DE BORDERLINE SMOTE EM PERMANOVA test EM 05/06/2025
  
  ######ACRÉSCIMO DE SVM SMOTE EM PERMANOVA test EM 06/06/2025
  
  ######ACRÉSCIMO DE ADASYN EM PERMANOVA test EM 08/06/2025
  
  ######ACRÉSCIMO DE RANDOM UPSAMPLING EM PERMANOVA test EM 09/06/2025
  
  ######ACRÉSCIMO DE TOMEK LINKS EM PERMANOVA test EM 14/06/2025
  
  ######ACRÉSCIMO DE NEAR MISS EM PERMANOVA test EM 17/06/2025
  
  ######ACRÉSCIMO DE ENN EM PERMANOVA test EM 17/06/2025
  
  ######ACRÉSCIMO DE OSS EM PERMANOVA test EM 18/06/2025
  
  ######ACRÉSCIMO DE RANDOM DOWNSAMPLING EM PERMANOVA test EM 18/06/2025
  
  ######ACRÉSCIMO DE SMOTE TL EM PERMANOVA test EM 02/07/2025
  
  ######ACRÉSCIMO DE SMOTE ENN EM PERMANOVA test EM 02/07/2025
  
  ######ACRÉSCIMO DE SBC EM PERMANOVA test EM 10/07/2025
  
  ######ACRÉSCIMO DE SMOTE IPF EM PERMANOVA test EM 14/07/2025
  
  ######ACRÉSCIMO DE SPIDER EM PERMANOVA test EM 14/07/2025
  
  
  observe({
    library(vegan)
    
    original_data <- as.data.frame(filedata())
    smote_data <- NULL
    class_col <- NULL
    
    # Identificando o método ativo
    
    if (isTRUE(runSMOTENC()) && !is.null(SMOTENC_Data()) && !is.null(input$target_nc)) {
      smote_data <- SMOTENC_Data()$syn_data
      class_col <- input$target_nc
      
    } else if (isTRUE(runSMOTE()) && !is.null(SMOTE_Data()) && !is.null(input$target)) {
      smote_data <- SMOTE_Data()$syn_data
      class_col <- input$target
      
    } else if (isTRUE(runBorderlineSMOTE()) && !is.null(BorderlineSMOTE_Data()) && !is.null(input$target_borderline)) {
      smote_data <- BorderlineSMOTE_Data()$syn_data
      class_col <- input$target_borderline
      
    } else if (isTRUE(runSVM_SMOTE()) && !is.null(SVM_SMOTE_Data()) && !is.null(input$target_svm)) {
      smote_data <- SVM_SMOTE_Data()$syn_data
      class_col <- input$target_svm
      
    } else if (isTRUE(runADASYN()) && !is.null(ADASYN_Data()) && !is.null(input$target_adasyn)) {
      smote_data <- ADASYN_Data()$syn_data
      class_col <- input$target_adasyn
      
    } else if (isTRUE(runRU()) && !is.null(RU_Data()) && !is.null(input$target_ru)) {
      smote_data <- RU_Data()$syn_data
      class_col <- input$target_ru
      
    } else if (isTRUE(runTomek()) && !is.null(Tomek_Data()) && !is.null(input$target_tomek)) {
      smote_data <- Tomek_Data()$removed
      class_col <- input$target_tomek
      
    } else if (isTRUE(runNearMiss()) && !is.null(NearMiss_Data()) && !is.null(input$target_nearmiss)) {
      smote_data <- NearMiss_Data()$removed
      class_col <- input$target_nearmiss
      
    } else if (isTRUE(runENN()) && !is.null(ENN_Data()) && !is.null(input$target_enn)) {
      smote_data <- ENN_Data()$removed_data
      class_col <- input$target_enn
      
    } else if (isTRUE(runOSS()) && !is.null(OSS_Data()) && !is.null(input$target_oss)) {
      smote_data <- OSS_Data()$removed_data
      class_col <- input$target_oss
      
    } else if (isTRUE(runRD()) && !is.null(RD_Data()) && !is.null(input$target_rd)) {
      smote_data <- RD_Data()$removed_data
      class_col <- input$target_rd
      
      
      ###BLOCO ADICIONADO EM 10/07/2025
      
    } else if (isTRUE(runSBC()) && !is.null(SBC_Data()) && !is.null(input$target_sbc)) {
      smote_data <- SBC_Data()$removed_data
      class_col <- input$target_sbc
      
      ###BLOCO ADICIONADO EM 10/07/2025
      
      
      
    } else if (isTRUE(runSMOTETL()) && !is.null(SMOTETL_Data()) && !is.null(input$target_tl)) {
      
      ## BLOCO SMOTE-TL
      
      class_col <- input$target_tl
      smote_data_added <- SMOTETL_Data()$syn_data
      smote_data_removed <- SMOTETL_Data()$removed_data
      
      names(original_data)[names(original_data) == class_col] <- "class"
      original_data$Tipo <- "Original"
      
      if (!is.null(smote_data_added) && nrow(smote_data_added) > 0) {
        names(smote_data_added)[names(smote_data_added) == class_col] <- "class"
        smote_data_added$class <- paste(smote_data_added$class, "Synthetic")
        smote_data_added$Tipo <- "Synthetic"
      } else {
        smote_data_added <- original_data[0, , drop = FALSE]
      }
      
      if (!is.null(smote_data_removed) && nrow(smote_data_removed) > 0) {
        names(smote_data_removed)[names(smote_data_removed) == class_col] <- "class"
        smote_data_removed$class <- paste(smote_data_removed$class, "Removed")
        smote_data_removed$Tipo <- "Removed"
      } else {
        smote_data_removed <- original_data[0, , drop = FALSE]
      }
      
      smote_data <- dplyr::bind_rows(smote_data_added, smote_data_removed)
      
      feature_cols <- intersect(names(original_data), names(smote_data))
      original_data <- original_data[, feature_cols, drop = FALSE]
      smote_data <- smote_data[, feature_cols, drop = FALSE]
      
      dados <- dplyr::bind_rows(original_data, smote_data)
      dados$SampleID <- 1:nrow(dados)
      dados$class <- as.character(dados$class)
      dados$Tipo <- ifelse(grepl("Removed$", dados$class), "Removed",
                           ifelse(grepl("Synthetic$", dados$class), "Synthetic", "Original"))
      dados$classe_base <- gsub(" Synthetic$| Removed$", "", dados$class)
      
    } 
    
    ###############BLOCO ADICIONADO EM 05/07/2025
    
    else if (isTRUE(runSMOTEENN()) && !is.null(SMOTEENN_Data()) && !is.null(input$target_enn2)) {
      
      ## BLOCO SMOTE-ENN
      
      class_col <- input$target_enn2
      smote_data_added <- SMOTEENN_Data()$syn_data
      smote_data_removed <- SMOTEENN_Data()$removed_data
      
      names(original_data)[names(original_data) == class_col] <- "class"
      original_data$Tipo <- "Original"
      
      if (!is.null(smote_data_added) && nrow(smote_data_added) > 0) {
        names(smote_data_added)[names(smote_data_added) == class_col] <- "class"
        smote_data_added$class <- paste(smote_data_added$class, "Synthetic")
        smote_data_added$Tipo <- "Synthetic"
      } else {
        smote_data_added <- original_data[0, , drop = FALSE]
      }
      
      if (!is.null(smote_data_removed) && nrow(smote_data_removed) > 0) {
        names(smote_data_removed)[names(smote_data_removed) == class_col] <- "class"
        smote_data_removed$class <- paste(smote_data_removed$class, "Removed")
        smote_data_removed$Tipo <- "Removed"
      } else {
        smote_data_removed <- original_data[0, , drop = FALSE]
      }
      
      smote_data <- dplyr::bind_rows(smote_data_added, smote_data_removed)
      
      feature_cols <- intersect(names(original_data), names(smote_data))
      original_data <- original_data[, feature_cols, drop = FALSE]
      smote_data <- smote_data[, feature_cols, drop = FALSE]
      
      dados <- dplyr::bind_rows(original_data, smote_data)
      dados$SampleID <- 1:nrow(dados)
      dados$class <- as.character(dados$class)
      dados$Tipo <- ifelse(grepl("Removed$", dados$class), "Removed",
                           ifelse(grepl("Synthetic$", dados$class), "Synthetic", "Original"))
      dados$classe_base <- gsub(" Synthetic$| Removed$", "", dados$class)
      
    }
    
    
    
    ###############BLOCO ADICIONADO EM 05/07/2025
    
    ###############BLOCO ADICIONADO EM 14/07/2025
    
    else if (isTRUE(runSMOTEIPF()) && !is.null(SMOTEIPF_Data()) && !is.null(input$target_ipf)) {
      
      ## BLOCO SMOTE-IPF
      
      class_col <- input$target_ipf
      smote_data_added <- SMOTEIPF_Data()$syn_data
      smote_data_removed <- SMOTEIPF_Data()$removed_data
      
      names(original_data)[names(original_data) == class_col] <- "class"
      original_data$Tipo <- "Original"
      
      if (!is.null(smote_data_added) && nrow(smote_data_added) > 0) {
        names(smote_data_added)[names(smote_data_added) == class_col] <- "class"
        smote_data_added$class <- paste(smote_data_added$class, "Synthetic")
        smote_data_added$Tipo <- "Synthetic"
      } else {
        smote_data_added <- original_data[0, , drop = FALSE]
      }
      
      if (!is.null(smote_data_removed) && nrow(smote_data_removed) > 0) {
        names(smote_data_removed)[names(smote_data_removed) == class_col] <- "class"
        smote_data_removed$class <- paste(smote_data_removed$class, "Removed")
        smote_data_removed$Tipo <- "Removed"
      } else {
        smote_data_removed <- original_data[0, , drop = FALSE]
      }
      
      smote_data <- dplyr::bind_rows(smote_data_added, smote_data_removed)
      
      feature_cols <- intersect(names(original_data), names(smote_data))
      original_data <- original_data[, feature_cols, drop = FALSE]
      smote_data <- smote_data[, feature_cols, drop = FALSE]
      
      dados <- dplyr::bind_rows(original_data, smote_data)
      dados$SampleID <- 1:nrow(dados)
      dados$class <- as.character(dados$class)
      dados$Tipo <- ifelse(grepl("Removed$", dados$class), "Removed",
                           ifelse(grepl("Synthetic$", dados$class), "Synthetic", "Original"))
      dados$classe_base <- gsub(" Synthetic$| Removed$", "", dados$class)
      
    }
    
    
    ###############BLOCO ADICIONADO EM 14/07/2025
    
    
    
    
    ############### BLOCO ADICIONADO EM 14/07/2025
    
    else if (isTRUE(runSPIDER()) && !is.null(SPIDER_Data()) && !is.null(input$target_spider)) {
      
      ## BLOCO SPIDER
      
      class_col <- input$target_spider
      smote_data_added <- SPIDER_Data()$syn_data
      smote_data_removed <- SPIDER_Data()$removed_data
      
      names(original_data)[names(original_data) == class_col] <- "class"
      original_data$Tipo <- "Original"
      
      if (!is.null(smote_data_added) && nrow(smote_data_added) > 0) {
        names(smote_data_added)[names(smote_data_added) == class_col] <- "class"
        smote_data_added$class <- paste(smote_data_added$class, "Synthetic")
        smote_data_added$Tipo <- "Synthetic"
      } else {
        smote_data_added <- original_data[0, , drop = FALSE]
      }
      
      if (!is.null(smote_data_removed) && nrow(smote_data_removed) > 0) {
        names(smote_data_removed)[names(smote_data_removed) == class_col] <- "class"
        smote_data_removed$class <- paste(smote_data_removed$class, "Removed")
        smote_data_removed$Tipo <- "Removed"
      } else {
        smote_data_removed <- original_data[0, , drop = FALSE]
      }
      
      smote_data <- dplyr::bind_rows(smote_data_added, smote_data_removed)
      
      feature_cols <- intersect(names(original_data), names(smote_data))
      original_data <- original_data[, feature_cols, drop = FALSE]
      smote_data <- smote_data[, feature_cols, drop = FALSE]
      
      dados <- dplyr::bind_rows(original_data, smote_data)
      dados$SampleID <- 1:nrow(dados)
      dados$class <- as.character(dados$class)
      dados$Tipo <- ifelse(grepl("Removed$", dados$class), "Removed",
                           ifelse(grepl("Synthetic$", dados$class), "Synthetic", "Original"))
      dados$classe_base <- gsub(" Synthetic$| Removed$", "", dados$class)
      
    }
    
    ############### BLOCO ADICIONADO EM 14/07/2025
    
    
    
    
    else {
      return()
    }
    
   # if (!isTRUE(runSMOTETL())) {

    ## if (!isTRUE(runSMOTETL()) && !isTRUE(runSMOTEENN()) ) {  #MODIFICADO EM 05/07/2025
    
    
  ##  if (!isTRUE(runSMOTETL()) && !isTRUE(runSMOTEENN()) && !isTRUE(runSMOTEIPF()) ) {  #MODIFICADO EM 14/07/2025
    
    if (!isTRUE(runSMOTETL()) && !isTRUE(runSMOTEENN()) && !isTRUE(runSMOTEIPF()) && !isTRUE(runSPIDER()) ) {  #MODIFICADO EM 14/07/2025
    
    
    if (is.null(smote_data) || !(class_col %in% names(original_data))) return()
      
      names(original_data)[names(original_data) == class_col] <- "class"
      names(smote_data)[names(smote_data) == class_col] <- "class"
      
      suffix <- if (isTRUE(runTomek()) || isTRUE(runNearMiss()) || isTRUE(runENN()) ||
                    isTRUE(runOSS()) || isTRUE(runRD())   || isTRUE(runSBC())     ) "Removed" else "Synthetic"
      
      if (nrow(smote_data) == 0) {
        
        ##MODIFICADO EM 08/07/2025 - USUÁRIO NÃO VERIFIQUE DIAGNÓSTICO
        showNotification("No synthetic/removed samples found.", type = "warning", duration = 60)
        showNotification("Don't display the diagnosis. Please use another method.", type = "warning", duration = 60)
        output$permanova_table <- renderDataTable({
          datatable(data.frame(Class = character(0), F_model = numeric(0), R2 = numeric(0), p_value = numeric(0)),
                    caption = "No synthetic/removed samples were generated.")
        })
        return()
      }
      
      smote_data$class <- paste(smote_data$class, suffix)
      smote_data$fake_cat <- NULL
      
      feature_cols <- intersect(names(original_data), names(smote_data))
      original_data <- original_data[, feature_cols, drop = FALSE]
      smote_data <- smote_data[, feature_cols, drop = FALSE]
      
      original_data$Tipo <- "Original"
      smote_data$Tipo <- "Synthetic"
      
      dados <- dplyr::bind_rows(original_data, smote_data)
      dados$SampleID <- 1:nrow(dados)
      dados$class <- as.character(dados$class)
      dados$Tipo <- ifelse(grepl("Synthetic", dados$class), "Synthetic", "Original")
      dados$classe_base <- gsub(paste0(" ", suffix, ".*$"), "", dados$class)
    }
    
    classes_com_sintetica <- unique(dados$classe_base[dados$Tipo != "Original"])
    
    if (length(classes_com_sintetica) == 0) {
      output$permanova_table <- renderDataTable({
        datatable(data.frame(Class = character(0), F_model = numeric(0), R2 = numeric(0), p_value = numeric(0)),
                  caption = "No classes have synthetic/removed samples for comparison.")
      })
      return()
    }
    
    resultados_permanova <- lapply(classes_com_sintetica, function(classe) {
      
      
      ###MODIFICADO EM 05/07/2025
      ###MODIFICADO EM 14/07/2025
      
      
      
     # tipos <- if (isTRUE(runSMOTETL())) c("Synthetic", "Removida") else "Synthetic"
      
      
      ## tipos <- if (isTRUE(runSMOTETL()) || isTRUE(runSMOTEENN()) ) c("Synthetic", "Removed") else "Synthetic"
      tipos <- if (isTRUE(runSMOTETL()) || isTRUE(runSMOTEENN()) || isTRUE(runSMOTEIPF()) || isTRUE(runSPIDER()) ) c("Synthetic", "Removed") else "Synthetic"
      
      
      
      
      ###MODIFICADO EM 05/07/2025
      ###MODIFICADO EM 14/07/2025
      
      res_list <- lapply(tipos, function(tipo) {
        classe_sint <- unique(dados$class[dados$classe_base == classe & dados$Tipo == tipo])
        subset_classe <- subset(dados, classe_base == classe & (Tipo %in% c("Original", tipo)))
        grupo <- subset_classe$Tipo
        
        num_vars <- names(Filter(is.numeric, subset_classe))
        num_vars <- setdiff(num_vars, "SampleID")
        
        X <- subset_classe[, num_vars, drop = FALSE]
        X <- X[, apply(X, 2, function(col) sd(col, na.rm = TRUE) > 0 && all(is.finite(col))), drop = FALSE]
        
        if (ncol(X) < 2) {
          return(data.frame(Class = classe, Type = tipo, F_model = NA, p_value = NA))
        }
        
        resultado <- tryCatch({
          permanova <- adonis2(X ~ grupo, permutations = 999)
          data.frame(
            Class = classe,
            Type = tipo,
            F_model = round(permanova$F[1], 4),
            p_value = round(permanova$`Pr(>F)`[1], 4)
          )
        }, error = function(e) {
          data.frame(Class = classe, Type = tipo, F_model = NA, p_value = NA)
        })
        
        return(resultado)
      })
      
      do.call(rbind, res_list)
    })
    
    output$permanova_table <- DT::renderDataTable({
      df_final <- do.call(rbind, resultados_permanova)
      DT::datatable(df_final, rownames = FALSE,
                    options = list(pageLength = 10),
                    caption = htmltools::tags$caption(
                      style = 'caption-side: top; text-align: left; font-weight: bold;',
                      "Results by Class - PERMANOVA"
                    ))
    })
  })
  
  
  
  
  
  
  ######ACRÉSCIMO DE SMOTE_NC EM PERMANOVA test EM 29/05/2025
  
  ######ACRÉSCIMO DE BORDERLINE SMOTE EM PERMANOVA test EM 05/06/2025
  
  ######ACRÉSCIMO DE SVM SMOTE EM PERMANOVA test EM 06/06/2025
  
  ######ACRÉSCIMO DE ADASYN EM PERMANOVA test EM 08/06/2025
  
  ######ACRÉSCIMO DE RANDOM UPSAMPLING EM PERMANOVA test EM 09/06/2025
  
  ######ACRÉSCIMO DE TOMEK LINKS EM PERMANOVA test EM 14/06/2025
  
  ######ACRÉSCIMO DE NEAR MISS EM PERMANOVA test EM 17/06/2025
  
  ######ACRÉSCIMO DE ENN EM PERMANOVA test EM 17/06/2025
  
  ######ACRÉSCIMO DE OSS EM PERMANOVA test EM 18/06/2025
  
  ######ACRÉSCIMO DE RANDOM DOWNSAMPLING EM PERMANOVA test EM 18/06/2025
  
  ######ACRÉSCIMO DE SMOTE TL EM PERMANOVA test EM 02/07/2025
  
  ######ACRÉSCIMO DE SMOTE ENN EM PERMANOVA test EM 05/07/2025
  
  ######ACRÉSCIMO DE SBC EM PERMANOVA test EM 10/07/2025
  
  ######ACRÉSCIMO DE SMOTE IPF EM PERMANOVA test EM 14/07/2025
  
  ######ACRÉSCIMO DE SPIDER EM PERMANOVA test EM 14/07/2025
  
  
  
  
  ###ACRÉSCIMO DE PERMANOVA test EM 30/03/2025#####
  
  
  ###ATUALIZADO EM 13/02/2025######
  
  
  ## INÍCIO DO BLOCO ATUALIZADO COM SUPORTE A SMOTE E SMOTE-NC ATUALIZADO EM 29/05/2025
  
  
  ####################MODIFICAÇÃO DA GUIA IMPORT DATA APÓS CONFIRM SMOTE EM 07/05/2025
  
  # Armazenando os dados combinados após clique em Confirm SMOTE, Confirm SMOTE-NC,
  # Confirm BORDERLINE SMOTE, Confirm SVM SMOTE, Confirm ADASYN, Confirm RANDOM UPSAMPLING,
  # Confirm TOMEK LINKS, Confirm NEAR MISS, Confirm ENN, Confirm OSS, Confirm RANDOM DOWNSAMPLING, 
  # Confirm SMOTE TL, Confirm SMOTE ENN, Confirm SBC, Confirm SMOTE IPF, Confirm SPIDER
  
  
  combined_data_final <- reactiveVal(NULL)
  
  # Original Samples
  
  output$original_data <- renderDT({
    req(data())
    df <- filedata()
    class_col <- input$target
    names(df)[names(df) == class_col] <- "class"
    
    if ("class" %in% colnames(df)) {
      feature_cols <- setdiff(names(df), "class")
      df <- df[, c(feature_cols, "class"), drop = FALSE]
    }
    
    datatable(df, options = list(pageLength = 5))
  })
  
  # Amostras Synthetics
  
  output$synthetic_only_data <- renderDT({
    req(SMOTE_Data())
    if ("data" %in% names(SMOTE_Data())) {
      synthetic_data <- SMOTE_Data()$syn_data
      if ("class" %in% colnames(synthetic_data)) {
        synthetic_data$class <- paste(synthetic_data$class, "Synthetic")
      }
      datatable(synthetic_data, options = list(pageLength = 5))
    } else {
      datatable(data.frame(Mensagem = "Synthetic data is not available or has invalid structure."), 
                options = list(pageLength = 5))
    }
  })
  
  # Confirmação do SMOTE
  
  observeEvent(input$confirmSMOTE, {
    req(filedata(), SMOTE_Data()$syn_data)
    
    original <- filedata()
    synthetic <- SMOTE_Data()$syn_data
    
    class_col <- input$target
    if ("class" %in% names(original)) names(original)[names(original) == "class"] <- "sampleclass.."
    if (class_col %in% names(original)) names(original)[names(original) == class_col] <- "sampleclass.."
    if (class_col %in% names(synthetic)) names(synthetic)[names(synthetic) == class_col] <- "sampleclass.."
    if ("class" %in% names(synthetic)) names(synthetic)[names(synthetic) == "class"] <- "sampleclass.."
    
    # Removendo colunas auxiliares se existirem 
    
    synthetic$fake_cat <- NULL
    
    cols <- intersect(names(original), names(synthetic))
    original <- original[, cols, drop = FALSE]
    synthetic <- synthetic[, cols, drop = FALSE]
    synthetic$sampleclass.. <- paste(synthetic$sampleclass.., "Synthetic") #ADICIONADO EM 14/06/2025
    confirmed <- dplyr::bind_rows(original, synthetic)
    
    combined_data_final(confirmed)
    
    confirmed_numerics <- confirmed %>% dplyr::select(where(is.numeric))
    confirmed_numerics <- confirmed_numerics[, setdiff(names(confirmed_numerics), "sampleclass.."), drop = FALSE]
    
    output$preview1 <- renderDT({
      req(confirmed)
      datatable(confirmed_numerics, options = list(scrollX = TRUE))
    })
    
    output$datasummary <- renderPrint({
      req(confirmed)
      psych::describe(confirmed_numerics)
    })
    
    output$summary <- renderDT({
      req(confirmed)
      skimr::skim(confirmed_numerics)
    })
    
    output$barplot <- renderPlotly({
      req(confirmed)
      df <- confirmed
      req("sampleclass.." %in% names(df))
      df <- df[!is.na(df$sampleclass..), ]
      
      class_freq <- df %>%
        dplyr::count(sampleclass..) %>%
        dplyr::rename(Class = sampleclass.., Frequency = n)
      
      num_classes <- length(unique(class_freq$Class))
      num_colors <- max(3, num_classes)
      class_colors <- RColorBrewer::brewer.pal(num_colors, "Set1")
      
      plot_ly(
        data = class_freq,
        x = ~Class,
        y = ~Frequency,
        type = "bar",
        color = ~Class,
        colors = class_colors,
        text = ~Frequency,
        textposition = 'outside'
      ) %>%
        layout(
          title = "Class Frequency",
          xaxis = list(title = "Class"),
          yaxis = list(title = "Frequency"),
          showlegend = TRUE
        )
    })
    # Atualizanddo choices de remoção de amostras e Variables
    
    updateSelectizeInput(session, "removedatarow", choices = rownames(confirmed), server = TRUE)
    updateSelectizeInput(session, "removedatacol", choices = names(confirmed), server = TRUE)
  })
  
  
  # Confirmação do SMOTE-NC
  
  observeEvent(input$confirmSMOTENC, {
    req(filedata(), SMOTENC_Data()$syn_data)
    original <- filedata()
    synthetic <- SMOTENC_Data()$syn_data
    
    if ("class" %in% names(original)) names(original)[names(original) == "class"] <- "sampleclass.."
    if ("class" %in% names(synthetic)) names(synthetic)[names(synthetic) == "class"] <- "sampleclass.."
    
    cols <- intersect(names(original), names(synthetic))
    original <- original[, cols, drop = FALSE]
    synthetic <- synthetic[, cols, drop = FALSE]
    synthetic$sampleclass.. <- paste(synthetic$sampleclass.., "Synthetic") #ADICIONADO EM 14/06/2025
    confirmed <- dplyr::bind_rows(original, synthetic)
    
    combined_data_final(confirmed)
    
    confirmed_numerics <- confirmed %>% dplyr::select(where(is.numeric))
    confirmed_numerics <- confirmed_numerics[, setdiff(names(confirmed_numerics), "sampleclass.."), drop = FALSE]
    
    output$preview1 <- renderDT({
      req(confirmed)
      datatable(confirmed_numerics, options = list(scrollX = TRUE))
    })
    
    output$datasummary <- renderPrint({
      req(confirmed)
      psych::describe(confirmed_numerics)
    })
    
    output$summary <- renderDT({
      req(confirmed)
      skimr::skim(confirmed_numerics)
    })
    
    output$barplot <- renderPlotly({
      req(confirmed)
      df <- confirmed
      req("sampleclass.." %in% names(df))
      df <- df[!is.na(df$sampleclass..), ]
      
      class_freq <- df %>%
        dplyr::count(sampleclass..) %>%
        dplyr::rename(Class = sampleclass.., Frequency = n)
      
      num_classes <- length(unique(class_freq$Class))
      num_colors <- max(3, num_classes)
      class_colors <- RColorBrewer::brewer.pal(num_colors, "Set1")
      
      plot_ly(
        data = class_freq,
        x = ~Class,
        y = ~Frequency,
        type = "bar",
        color = ~Class,
        colors = class_colors,
        text = ~Frequency,
        textposition = 'outside'
      ) %>%
        layout(
          title = "Class Frequency",
          xaxis = list(title = "Class"),
          yaxis = list(title = "Frequency"),
          showlegend = TRUE
        )
    })
    
    # Atualizanddo choices de remoção de amostras e Variables
    
    updateSelectizeInput(session, "removedatarow", choices = rownames(confirmed), server = TRUE)
    updateSelectizeInput(session, "removedatacol", choices = names(confirmed), server = TRUE)
  })
  
  
  
  
  
  # Confirmação do Borderline SMOTE
  
  observeEvent(input$confirmBorderlineSMOTE, {
    req(filedata(), BorderlineSMOTE_Data()$syn_data)
    original <- filedata()
    synthetic <- BorderlineSMOTE_Data()$syn_data
    
    if ("class" %in% names(original)) names(original)[names(original) == "class"] <- "sampleclass.."
    if ("class" %in% names(synthetic)) names(synthetic)[names(synthetic) == "class"] <- "sampleclass.."
    
    cols <- intersect(names(original), names(synthetic))
    original <- original[, cols, drop = FALSE]
    synthetic <- synthetic[, cols, drop = FALSE]
    synthetic$sampleclass.. <- paste(synthetic$sampleclass.., "Synthetic") #ADICIONADO EM 14/06/2025
    confirmed <- dplyr::bind_rows(original, synthetic)
    
    combined_data_final(confirmed)
    
    confirmed_numerics <- confirmed %>% dplyr::select(where(is.numeric))
    confirmed_numerics <- confirmed_numerics[, setdiff(names(confirmed_numerics), "sampleclass.."), drop = FALSE]
    
    output$preview1 <- renderDT({
      req(confirmed)
      datatable(confirmed_numerics, options = list(scrollX = TRUE))
    })
    
    output$datasummary <- renderPrint({
      req(confirmed)
      psych::describe(confirmed_numerics)
    })
    
    output$summary <- renderDT({
      req(confirmed)
      skimr::skim(confirmed_numerics)
    })
    
    output$barplot <- renderPlotly({
      req(confirmed)
      df <- confirmed
      req("sampleclass.." %in% names(df))
      df <- df[!is.na(df$sampleclass..), ]
      
      class_freq <- df %>%
        dplyr::count(sampleclass..) %>%
        dplyr::rename(Class = sampleclass.., Frequency = n)
      
      num_classes <- length(unique(class_freq$Class))
      num_colors <- max(3, num_classes)
      class_colors <- RColorBrewer::brewer.pal(num_colors, "Set1")
      
      plot_ly(
        data = class_freq,
        x = ~Class,
        y = ~Frequency,
        type = "bar",
        color = ~Class,
        colors = class_colors,
        text = ~Frequency,
        textposition = 'outside'
      ) %>%
        layout(
          title = "Class Frequency",
          xaxis = list(title = "Class"),
          yaxis = list(title = "Frequency"),
          showlegend = TRUE
        )
    })
    
    # Atualizando choices de remoção de amostras e Variables
    updateSelectizeInput(session, "removedatarow", choices = rownames(confirmed), server = TRUE)
    updateSelectizeInput(session, "removedatacol", choices = names(confirmed), server = TRUE)
  })
  
  
  # Confirmação do SVM SMOTE
  
  
  
  observeEvent(input$confirmSVM_SMOTE, {
    req(filedata(), SVM_SMOTE_Data()$syn_data)
    original <- filedata()
    synthetic <- SVM_SMOTE_Data()$syn_data
    
    if ("class" %in% names(original)) names(original)[names(original) == "class"] <- "sampleclass.."
    if ("class" %in% names(synthetic)) names(synthetic)[names(synthetic) == "class"] <- "sampleclass.."
    
    cols <- intersect(names(original), names(synthetic))
    original <- original[, cols, drop = FALSE]
    synthetic <- synthetic[, cols, drop = FALSE]
    synthetic$sampleclass.. <- paste(synthetic$sampleclass.., "Synthetic") #ADICIONADO EM 14/06/2025
    confirmed <- dplyr::bind_rows(original, synthetic)
    
    combined_data_final(confirmed)
    
    confirmed_numerics <- confirmed %>% dplyr::select(where(is.numeric))
    confirmed_numerics <- confirmed_numerics[, setdiff(names(confirmed_numerics), "sampleclass.."), drop = FALSE]
    
    output$preview1 <- renderDT({
      req(confirmed)
      datatable(confirmed_numerics, options = list(scrollX = TRUE))
    })
    
    output$datasummary <- renderPrint({
      req(confirmed)
      psych::describe(confirmed_numerics)
    })
    
    output$summary <- renderDT({
      req(confirmed)
      skimr::skim(confirmed_numerics)
    })
    
    output$barplot <- renderPlotly({
      req(confirmed)
      df <- confirmed
      req("sampleclass.." %in% names(df))
      df <- df[!is.na(df$sampleclass..), ]
      
      class_freq <- df %>%
        dplyr::count(sampleclass..) %>%
        dplyr::rename(Class = sampleclass.., Frequency = n)
      
      num_classes <- length(unique(class_freq$Class))
      num_colors <- max(3, num_classes)
      class_colors <- RColorBrewer::brewer.pal(num_colors, "Set1")
      
      plot_ly(
        data = class_freq,
        x = ~Class,
        y = ~Frequency,
        type = "bar",
        color = ~Class,
        colors = class_colors,
        text = ~Frequency,
        textposition = 'outside'
      ) %>%
        layout(
          title = "Class Frequency",
          xaxis = list(title = "Class"),
          yaxis = list(title = "Frequency"),
          showlegend = TRUE
        )
    })
    
    # Atualizando choices de remoção de amostras e Variables
    updateSelectizeInput(session, "removedatarow", choices = rownames(confirmed), server = TRUE)
    updateSelectizeInput(session, "removedatacol", choices = names(confirmed), server = TRUE)
  })
  
  
  # Confirmação do ADASYN
  
  
  
  observeEvent(input$confirmADASYN, {
    req(filedata(), ADASYN_Data()$syn_data)
    original <- filedata()
    synthetic <- ADASYN_Data()$syn_data
    
    if ("class" %in% names(original)) names(original)[names(original) == "class"] <- "sampleclass.."
    if ("class" %in% names(synthetic)) names(synthetic)[names(synthetic) == "class"] <- "sampleclass.."
    
    cols <- intersect(names(original), names(synthetic))
    original <- original[, cols, drop = FALSE]
    synthetic <- synthetic[, cols, drop = FALSE]
    synthetic$sampleclass.. <- paste(synthetic$sampleclass.., "Synthetic") #ADICIONADO EM 14/06/2025
    confirmed <- dplyr::bind_rows(original, synthetic)
    
    combined_data_final(confirmed)
    
    confirmed_numerics <- confirmed %>% dplyr::select(where(is.numeric))
    confirmed_numerics <- confirmed_numerics[, setdiff(names(confirmed_numerics), "sampleclass.."), drop = FALSE]
    
    output$preview1 <- renderDT({
      req(confirmed)
      datatable(confirmed_numerics, options = list(scrollX = TRUE))
    })
    
    output$datasummary <- renderPrint({
      req(confirmed)
      psych::describe(confirmed_numerics)
    })
    
    output$summary <- renderDT({
      req(confirmed)
      skimr::skim(confirmed_numerics)
    })
    
    output$barplot <- renderPlotly({
      req(confirmed)
      df <- confirmed
      req("sampleclass.." %in% names(df))
      df <- df[!is.na(df$sampleclass..), ]
      
      class_freq <- df %>%
        dplyr::count(sampleclass..) %>%
        dplyr::rename(Class = sampleclass.., Frequency = n)
      
      num_classes <- length(unique(class_freq$Class))
      num_colors <- max(3, num_classes)
      class_colors <- RColorBrewer::brewer.pal(num_colors, "Set1")
      
      plot_ly(
        data = class_freq,
        x = ~Class,
        y = ~Frequency,
        type = "bar",
        color = ~Class,
        colors = class_colors,
        text = ~Frequency,
        textposition = 'outside'
      ) %>%
        layout(
          title = "Class Frequency",
          xaxis = list(title = "Class"),
          yaxis = list(title = "Frequency"),
          showlegend = TRUE
        )
    })
    
    # Atualizando choices de remoção de amostras e Variables
    updateSelectizeInput(session, "removedatarow", choices = rownames(confirmed), server = TRUE)
    updateSelectizeInput(session, "removedatacol", choices = names(confirmed), server = TRUE)
  })  
  
  # Confirmação do RANDOM UPSAMPLING
  
  
  
  observeEvent(input$confirmRU, {
    req(filedata(), RU_Data()$syn_data)
    original <- filedata()
    synthetic <- RU_Data()$syn_data
    
    if ("class" %in% names(original)) names(original)[names(original) == "class"] <- "sampleclass.."
    if ("class" %in% names(synthetic)) names(synthetic)[names(synthetic) == "class"] <- "sampleclass.."
    
    cols <- intersect(names(original), names(synthetic))
    original <- original[, cols, drop = FALSE]
    synthetic <- synthetic[, cols, drop = FALSE]
    synthetic$sampleclass.. <- paste(synthetic$sampleclass.., "Synthetic") #ADICIONADO EM 14/06/2025
    confirmed <- dplyr::bind_rows(original, synthetic)
    
    combined_data_final(confirmed)
    
    confirmed_numerics <- confirmed %>% dplyr::select(where(is.numeric))
    confirmed_numerics <- confirmed_numerics[, setdiff(names(confirmed_numerics), "sampleclass.."), drop = FALSE]
    
    output$preview1 <- renderDT({
      req(confirmed)
      datatable(confirmed_numerics, options = list(scrollX = TRUE))
    })
    
    output$datasummary <- renderPrint({
      req(confirmed)
      psych::describe(confirmed_numerics)
    })
    
    output$summary <- renderDT({
      req(confirmed)
      skimr::skim(confirmed_numerics)
    })
    
    output$barplot <- renderPlotly({
      req(confirmed)
      df <- confirmed
      req("sampleclass.." %in% names(df))
      df <- df[!is.na(df$sampleclass..), ]
      
      class_freq <- df %>%
        dplyr::count(sampleclass..) %>%
        dplyr::rename(Class = sampleclass.., Frequency = n)
      
      num_classes <- length(unique(class_freq$Class))
      num_colors <- max(3, num_classes)
      class_colors <- RColorBrewer::brewer.pal(num_colors, "Set1")
      
      plot_ly(
        data = class_freq,
        x = ~Class,
        y = ~Frequency,
        type = "bar",
        color = ~Class,
        colors = class_colors,
        text = ~Frequency,
        textposition = 'outside'
      ) %>%
        layout(
          title = "Class Frequency",
          xaxis = list(title = "Class"),
          yaxis = list(title = "Frequency"),
          showlegend = TRUE
        )
    })
    
    # Atualizando choices de remoção de amostras e Variables
    updateSelectizeInput(session, "removedatarow", choices = rownames(confirmed), server = TRUE)
    updateSelectizeInput(session, "removedatacol", choices = names(confirmed), server = TRUE)
  })  
  
  # Confirmação do TOMEK LINKS (Downsampling com remoção de amostras)
  
  observeEvent(input$confirmTomek, {
    req(filedata(), Tomek_Data()$removed_data)
    original <- filedata()
    removidas <- Tomek_Data()$removed_data  # Aqui as "Synthetics" são as removidas
    
    if ("class" %in% names(original)) names(original)[names(original) == "class"] <- "sampleclass.."
    if ("class" %in% names(removidas)) names(removidas)[names(removidas) == "class"] <- "sampleclass.."
    
    cols <- intersect(names(original), names(removidas))
    original <- original[, cols, drop = FALSE]
    removidas <- removidas[, cols, drop = FALSE]
    
    # Removendo as amostras removidas da base original
    
    original_cleaned <- dplyr::anti_join(original, removidas, by = cols)
    confirmed <- original_cleaned  # agora só com as não-removidas
    
    combined_data_final(confirmed)
    
    confirmed_numerics <- confirmed %>% dplyr::select(where(is.numeric))
    confirmed_numerics <- confirmed_numerics[, setdiff(names(confirmed_numerics), "sampleclass.."), drop = FALSE]
    
    output$preview1 <- renderDT({
      req(confirmed)
      datatable(confirmed_numerics, options = list(scrollX = TRUE))
    })
    
    output$datasummary <- renderPrint({
      req(confirmed)
      psych::describe(confirmed_numerics)
    })
    
    output$summary <- renderDT({
      req(confirmed)
      skimr::skim(confirmed_numerics)
    })
    
    output$barplot <- renderPlotly({
      req(confirmed)
      df <- confirmed
      req("sampleclass.." %in% names(df))
      df <- df[!is.na(df$sampleclass..), ]
      
      class_freq <- df %>%
        dplyr::count(sampleclass..) %>%
        dplyr::rename(Class = sampleclass.., Frequency = n)
      
      num_classes <- length(unique(class_freq$Class))
      num_colors <- max(3, num_classes)
      class_colors <- RColorBrewer::brewer.pal(num_colors, "Set1")
      
      plot_ly(
        data = class_freq,
        x = ~Class,
        y = ~Frequency,
        type = "bar",
        color = ~Class,
        colors = class_colors,
        text = ~Frequency,
        textposition = 'outside'
      ) %>%
        layout(
          title = "Class Frequency",
          xaxis = list(title = "Class"),
          yaxis = list(title = "Frequency"),
          showlegend = TRUE
        )
    })
    
    # Atualizando choices de remoção de amostras e Variables
    updateSelectizeInput(session, "removedatarow", choices = rownames(confirmed), server = TRUE)
    updateSelectizeInput(session, "removedatacol", choices = names(confirmed), server = TRUE)
  })
  
  # Confirmação do NEAR MISS
  
  observeEvent(input$confirmNearMiss, {
    req(filedata(), NearMiss_Data()$removed_data)
    original <- filedata()
    removidas <- NearMiss_Data()$removed_data  # Aqui as "Synthetics" são as removidas
    
    if ("class" %in% names(original)) names(original)[names(original) == "class"] <- "sampleclass.."
    if ("class" %in% names(removidas)) names(removidas)[names(removidas) == "class"] <- "sampleclass.."
    
    cols <- intersect(names(original), names(removidas))
    original <- original[, cols, drop = FALSE]
    removidas <- removidas[, cols, drop = FALSE]
    
    # Removendo as amostras removidas da base original
    original_cleaned <- dplyr::anti_join(original, removidas, by = cols)
    confirmed <- original_cleaned  # agora só com as não-removidas
    
    combined_data_final(confirmed)
    
    confirmed_numerics <- confirmed %>% dplyr::select(where(is.numeric))
    confirmed_numerics <- confirmed_numerics[, setdiff(names(confirmed_numerics), "sampleclass.."), drop = FALSE]
    
    output$preview1 <- renderDT({
      req(confirmed)
      datatable(confirmed_numerics, options = list(scrollX = TRUE))
    })
    
    output$datasummary <- renderPrint({
      req(confirmed)
      psych::describe(confirmed_numerics)
    })
    
    output$summary <- renderDT({
      req(confirmed)
      skimr::skim(confirmed_numerics)
    })
    
    output$barplot <- renderPlotly({
      req(confirmed)
      df <- confirmed
      req("sampleclass.." %in% names(df))
      df <- df[!is.na(df$sampleclass..), ]
      
      class_freq <- df %>%
        dplyr::count(sampleclass..) %>%
        dplyr::rename(Class = sampleclass.., Frequency = n)
      
      num_classes <- length(unique(class_freq$Class))
      num_colors <- max(3, num_classes)
      class_colors <- RColorBrewer::brewer.pal(num_colors, "Set1")
      
      plot_ly(
        data = class_freq,
        x = ~Class,
        y = ~Frequency,
        type = "bar",
        color = ~Class,
        colors = class_colors,
        text = ~Frequency,
        textposition = 'outside'
      ) %>%
        layout(
          title = "Class Frequency",
          xaxis = list(title = "Class"),
          yaxis = list(title = "Frequency"),
          showlegend = TRUE
        )
    })
    
    # Atualizando choices de remoção de amostras e Variables
    updateSelectizeInput(session, "removedatarow", choices = rownames(confirmed), server = TRUE)
    updateSelectizeInput(session, "removedatacol", choices = names(confirmed), server = TRUE)
  })
  
  
  # Confirmação do ENN
  
  observeEvent(input$confirmENN, {
    req(filedata(), ENN_Data()$removed_data)
    original <- filedata()
    removidas <- ENN_Data()$removed_data  # Aqui as "Synthetics" são as removidas
    
    if ("class" %in% names(original)) names(original)[names(original) == "class"] <- "sampleclass.."
    if ("class" %in% names(removidas)) names(removidas)[names(removidas) == "class"] <- "sampleclass.."
    
    cols <- intersect(names(original), names(removidas))
    original <- original[, cols, drop = FALSE]
    removidas <- removidas[, cols, drop = FALSE]
    
    # Removendo as amostras removidas da base original
    
    original_cleaned <- dplyr::anti_join(original, removidas, by = cols)
    confirmed <- original_cleaned  # agora só com as não-removidas
    
    combined_data_final(confirmed)
    
    confirmed_numerics <- confirmed %>% dplyr::select(where(is.numeric))
    confirmed_numerics <- confirmed_numerics[, setdiff(names(confirmed_numerics), "sampleclass.."), drop = FALSE]
    
    output$preview1 <- renderDT({
      req(confirmed)
      datatable(confirmed_numerics, options = list(scrollX = TRUE))
    })
    
    output$datasummary <- renderPrint({
      req(confirmed)
      psych::describe(confirmed_numerics)
    })
    
    output$summary <- renderDT({
      req(confirmed)
      skimr::skim(confirmed_numerics)
    })
    
    output$barplot <- renderPlotly({
      req(confirmed)
      df <- confirmed
      req("sampleclass.." %in% names(df))
      df <- df[!is.na(df$sampleclass..), ]
      
      class_freq <- df %>%
        dplyr::count(sampleclass..) %>%
        dplyr::rename(Class = sampleclass.., Frequency = n)
      
      num_classes <- length(unique(class_freq$Class))
      num_colors <- max(3, num_classes)
      class_colors <- RColorBrewer::brewer.pal(num_colors, "Set1")
      
      plot_ly(
        data = class_freq,
        x = ~Class,
        y = ~Frequency,
        type = "bar",
        color = ~Class,
        colors = class_colors,
        text = ~Frequency,
        textposition = 'outside'
      ) %>%
        layout(
          title = "Class Frequency",
          xaxis = list(title = "Class"),
          yaxis = list(title = "Frequency"),
          showlegend = TRUE
        )
    })
    
    # Atualizando choices de remoção de amostras e Variables
    updateSelectizeInput(session, "removedatarow", choices = rownames(confirmed), server = TRUE)
    updateSelectizeInput(session, "removedatacol", choices = names(confirmed), server = TRUE)
  })
  
  
  # Confirmação do OSS
  
  observeEvent(input$confirmOSS, {
    req(filedata(), OSS_Data()$removed_data)
    original <- filedata()
    removidas <- OSS_Data()$removed_data  # Aqui as "Synthetics" são as removidas
    
    if ("class" %in% names(original)) names(original)[names(original) == "class"] <- "sampleclass.."
    if ("class" %in% names(removidas)) names(removidas)[names(removidas) == "class"] <- "sampleclass.."
    
    cols <- intersect(names(original), names(removidas))
    original <- original[, cols, drop = FALSE]
    removidas <- removidas[, cols, drop = FALSE]
    
    # Removendo as amostras removidas da base original
    
    original_cleaned <- dplyr::anti_join(original, removidas, by = cols)
    confirmed <- original_cleaned  # agora só com as não-removidas
    
    combined_data_final(confirmed)
    
    confirmed_numerics <- confirmed %>% dplyr::select(where(is.numeric))
    confirmed_numerics <- confirmed_numerics[, setdiff(names(confirmed_numerics), "sampleclass.."), drop = FALSE]
    
    output$preview1 <- renderDT({
      req(confirmed)
      datatable(confirmed_numerics, options = list(scrollX = TRUE))
    })
    
    output$datasummary <- renderPrint({
      req(confirmed)
      psych::describe(confirmed_numerics)
    })
    
    output$summary <- renderDT({
      req(confirmed)
      skimr::skim(confirmed_numerics)
    })
    
    output$barplot <- renderPlotly({
      req(confirmed)
      df <- confirmed
      req("sampleclass.." %in% names(df))
      df <- df[!is.na(df$sampleclass..), ]
      
      class_freq <- df %>%
        dplyr::count(sampleclass..) %>%
        dplyr::rename(Class = sampleclass.., Frequency = n)
      
      num_classes <- length(unique(class_freq$Class))
      num_colors <- max(3, num_classes)
      class_colors <- RColorBrewer::brewer.pal(num_colors, "Set1")
      
      plot_ly(
        data = class_freq,
        x = ~Class,
        y = ~Frequency,
        type = "bar",
        color = ~Class,
        colors = class_colors,
        text = ~Frequency,
        textposition = 'outside'
      ) %>%
        layout(
          title = "Class Frequency",
          xaxis = list(title = "Class"),
          yaxis = list(title = "Frequency"),
          showlegend = TRUE
        )
    })
    
    # Atualizando choices de remoção de amostras e Variables
    updateSelectizeInput(session, "removedatarow", choices = rownames(confirmed), server = TRUE)
    updateSelectizeInput(session, "removedatacol", choices = names(confirmed), server = TRUE)
  })
  
  
  
  # Confirmação do RD
  
  observeEvent(input$confirmRD, {
    req(filedata(), RD_Data()$removed_data)
    original <- filedata()
    removidas <- RD_Data()$removed_data  # Aqui as "Synthetics" são as removidas
    
    if ("class" %in% names(original)) names(original)[names(original) == "class"] <- "sampleclass.."
    if ("class" %in% names(removidas)) names(removidas)[names(removidas) == "class"] <- "sampleclass.."
    
    cols <- intersect(names(original), names(removidas))
    original <- original[, cols, drop = FALSE]
    removidas <- removidas[, cols, drop = FALSE]
    
    # Removendo as amostras removidas da base original
    
    original_cleaned <- dplyr::anti_join(original, removidas, by = cols)
    confirmed <- original_cleaned  # agora só com as não-removidas
    
    combined_data_final(confirmed)
    
    confirmed_numerics <- confirmed %>% dplyr::select(where(is.numeric))
    confirmed_numerics <- confirmed_numerics[, setdiff(names(confirmed_numerics), "sampleclass.."), drop = FALSE]
    
    output$preview1 <- renderDT({
      req(confirmed)
      datatable(confirmed_numerics, options = list(scrollX = TRUE))
    })
    
    output$datasummary <- renderPrint({
      req(confirmed)
      psych::describe(confirmed_numerics)
    })
    
    output$summary <- renderDT({
      req(confirmed)
      skimr::skim(confirmed_numerics)
    })
    
    output$barplot <- renderPlotly({
      req(confirmed)
      df <- confirmed
      req("sampleclass.." %in% names(df))
      df <- df[!is.na(df$sampleclass..), ]
      
      class_freq <- df %>%
        dplyr::count(sampleclass..) %>%
        dplyr::rename(Class = sampleclass.., Frequency = n)
      
      num_classes <- length(unique(class_freq$Class))
      num_colors <- max(3, num_classes)
      class_colors <- RColorBrewer::brewer.pal(num_colors, "Set1")
      
      plot_ly(
        data = class_freq,
        x = ~Class,
        y = ~Frequency,
        type = "bar",
        color = ~Class,
        colors = class_colors,
        text = ~Frequency,
        textposition = 'outside'
      ) %>%
        layout(
          title = "Class Frequency",
          xaxis = list(title = "Class"),
          yaxis = list(title = "Frequency"),
          showlegend = TRUE
        )
    })
    
    # Atualizando choices de remoção de amostras e Variables
    updateSelectizeInput(session, "removedatarow", choices = rownames(confirmed), server = TRUE)
    updateSelectizeInput(session, "removedatacol", choices = names(confirmed), server = TRUE)
  })

  
  # Confirmação do SBC - ADICIONADO EM 10/07/2025
  
  observeEvent(input$confirmSBC, {
    req(SBC_Data())
    
    # Usando o dataset final já processado pelo SBC:
    
    confirmed <- SBC_Data()$data
    
    # Salvando no combinado final
    
    combined_data_final(confirmed)
    
    # Numeric-only para preview
    
    confirmed_numerics <- confirmed %>% dplyr::select(where(is.numeric))
    
    output$preview1 <- renderDT({
      datatable(confirmed_numerics, options = list(scrollX = TRUE))
    })
    
    output$datasummary <- renderPrint({
      psych::describe(confirmed_numerics)
    })
    
    output$summary <- renderDT({
      skimr::skim(confirmed_numerics)
    })
    
    output$barplot <- renderPlotly({
      req("class" %in% names(confirmed) | "sampleclass.." %in% names(confirmed))
      class_col <- if ("sampleclass.." %in% names(confirmed)) "sampleclass.." else "class"
      
      df <- confirmed
      df <- df[!is.na(df[[class_col]]), ]
      
      class_freq <- df %>%
        dplyr::count(.data[[class_col]]) %>%
        dplyr::rename(Class = !!class_col, Frequency = n)
      
      num_classes <- length(unique(class_freq$Class))
      num_colors <- max(3, num_classes)
      class_colors <- RColorBrewer::brewer.pal(num_colors, "Set1")
      
      plot_ly(
        data = class_freq,
        x = ~Class,
        y = ~Frequency,
        type = "bar",
        color = ~Class,
        colors = class_colors,
        text = ~Frequency,
        textposition = 'outside'
      ) %>%
        layout(
          title = "Class Frequency",
          xaxis = list(title = "Class"),
          yaxis = list(title = "Frequency"),
          showlegend = TRUE
        )
    })
    
    updateSelectizeInput(session, "removedatarow", choices = rownames(confirmed), server = TRUE)
    updateSelectizeInput(session, "removedatacol", choices = names(confirmed), server = TRUE)
  })
  

    
  # Confirmação do SMOTE-TL - ADICIONADO EM 02/07/2025
  
  observeEvent(input$confirmSMOTETL, {
    req(filedata(), SMOTETL_Data())
    
    original <- filedata()
    added <- SMOTETL_Data()$syn_data
    removed <- SMOTETL_Data()$removed_data
    
    class_col <- input$target_tl
    if ("class" %in% names(original)) names(original)[names(original) == "class"] <- "sampleclass.."
    if (class_col %in% names(original)) names(original)[names(original) == class_col] <- "sampleclass.."
    
    if (!is.null(added) && class_col %in% names(added)) names(added)[names(added) == class_col] <- "sampleclass.."
    if (!is.null(added) && "class" %in% names(added)) names(added)[names(added) == "class"] <- "sampleclass.."
    
    if (!is.null(removed) && class_col %in% names(removed)) names(removed)[names(removed) == class_col] <- "sampleclass.."
    if (!is.null(removed) && "class" %in% names(removed)) names(removed)[names(removed) == "class"] <- "sampleclass.."
    
    if (!is.null(added)) added$fake_cat <- NULL
    if (!is.null(removed)) removed$fake_cat <- NULL
    
    
    
    ###BLOCO MODIFICADO EM 08/07/2025
    
    ## cols <- names(original)
    
    ## if (!is.null(added)) {
    ##  added <- added[, intersect(cols, names(added)), drop = FALSE]
    ##  added$sampleclass.. <- paste(added$sampleclass.., "Synthetic")
  ##  }
    
  ##  if (!is.null(removed)) {
  ##    removed <- removed[, intersect(cols, names(removed)), drop = FALSE]
  ##    removed$sampleclass.. <- paste(removed$sampleclass.., "Removida")
  ##  }
    
  ##  confirmed <- dplyr::bind_rows(original, added, removed)
    
    
    cols <- names(original)
    
    if (!is.null(added) && nrow(added) > 0) {
      added <- added[, intersect(cols, names(added)), drop = FALSE]
      added$sampleclass.. <- paste(added$sampleclass.., "Synthetic")
    } else {
      added <- NULL  # Garantindo que o bind_rows não falha
    }
    
    if (!is.null(removed) && nrow(removed) > 0) {
      removed <- removed[, intersect(cols, names(removed)), drop = FALSE]
      removed$sampleclass.. <- paste(removed$sampleclass.., "Removed")
    } else {
      removed <- NULL  # Garantindo que o bind_rows não falha
    }
    
    confirmed <- dplyr::bind_rows(original, added, removed)
    confirmed_final <- dplyr::bind_rows(original, added) #MODIFICADO EM 08/07/2025
    
    
    ###BLOCO MODIFICADO EM 08/07/2025
    
    
    
    
   # combined_data_final(confirmed)
    combined_data_final(confirmed_final) #MODIFICADO EM 08/07/2025
    
    
    
    
    confirmed_numerics <- confirmed %>%
      dplyr::select(where(is.numeric))
    
    if ("sampleclass.." %in% names(confirmed_numerics)) {
      confirmed_numerics <- confirmed_numerics %>% dplyr::select(-sampleclass..)
    }
    
    
    output$preview1 <- renderDT({
      req(confirmed)
      datatable(confirmed_numerics, options = list(scrollX = TRUE))
    })
    
    output$datasummary <- renderPrint({
      req(confirmed)
      psych::describe(confirmed_numerics)
    })
    
    output$summary <- renderDT({
      req(confirmed)
      skimr::skim(confirmed_numerics)
    })
    
    output$barplot <- renderPlotly({
      req(confirmed)
      df <- confirmed
      req("sampleclass.." %in% names(df))
      df <- df[!is.na(df$sampleclass..), ]
      
      class_freq <- df %>%
        dplyr::count(sampleclass..) %>%
        dplyr::rename(Class = sampleclass.., Frequency = n)
      
      num_classes <- length(unique(class_freq$Class))
      num_colors <- max(3, num_classes)
      class_colors <- RColorBrewer::brewer.pal(num_colors, "Set1")
      
      plot_ly(
        data = class_freq,
        x = ~Class,
        y = ~Frequency,
        type = "bar",
        color = ~Class,
        colors = class_colors,
        text = ~Frequency,
        textposition = 'outside'
      ) %>%
        layout(
          title = "Class Frequency",
          xaxis = list(title = "Class"),
          yaxis = list(title = "Frequency"),
          showlegend = TRUE
        )
    })
    
    updateSelectizeInput(session, "removedatarow", choices = rownames(confirmed), server = TRUE)
    updateSelectizeInput(session, "removedatacol", choices = names(confirmed), server = TRUE)
  })
  
  # Confirmação do SMOTE-ENN - ADICIONADO EM 05/07/2025
  
  observeEvent(input$confirmSMOTEENN, {
    req(filedata(), SMOTEENN_Data())
    
    original <- filedata()
    added <- SMOTEENN_Data()$syn_data
    removed <- SMOTEENN_Data()$removed_data
    
    class_col <- input$target_enn2
    if ("class" %in% names(original)) names(original)[names(original) == "class"] <- "sampleclass.."
    if (class_col %in% names(original)) names(original)[names(original) == class_col] <- "sampleclass.."
    
    if (!is.null(added) && class_col %in% names(added)) names(added)[names(added) == class_col] <- "sampleclass.."
    if (!is.null(added) && "class" %in% names(added)) names(added)[names(added) == "class"] <- "sampleclass.."
    
    if (!is.null(removed) && class_col %in% names(removed)) names(removed)[names(removed) == class_col] <- "sampleclass.."
    if (!is.null(removed) && "class" %in% names(removed)) names(removed)[names(removed) == "class"] <- "sampleclass.."
    
    if (!is.null(added)) added$fake_cat <- NULL
    if (!is.null(removed)) removed$fake_cat <- NULL
    
    cols <- names(original)
    
    if (!is.null(added)) {
      added <- added[, intersect(cols, names(added)), drop = FALSE]
      added$sampleclass.. <- paste(added$sampleclass.., "Synthetic")
    }
    
    if (!is.null(removed)) {
      removed <- removed[, intersect(cols, names(removed)), drop = FALSE]
      removed$sampleclass.. <- paste(removed$sampleclass.., "Removed")
    }
    
    confirmed <- dplyr::bind_rows(original, added, removed)
    confirmed_final <- dplyr::bind_rows(original, added) #MODIFICADO EM 08/07/2025
    
   # combined_data_final(confirmed)
    combined_data_final(confirmed_final) #MODIFICADO EM 08/07/2025
    
    
    
    
    confirmed_numerics <- confirmed %>%
      dplyr::select(where(is.numeric))
    
    if ("sampleclass.." %in% names(confirmed_numerics)) {
      confirmed_numerics <- confirmed_numerics %>% dplyr::select(-sampleclass..)
    }
    
    
    output$preview1 <- renderDT({
      req(confirmed)
      datatable(confirmed_numerics, options = list(scrollX = TRUE))
    })
    
    output$datasummary <- renderPrint({
      req(confirmed)
      psych::describe(confirmed_numerics)
    })
    
    output$summary <- renderDT({
      req(confirmed)
      skimr::skim(confirmed_numerics)
    })
    
    output$barplot <- renderPlotly({
      req(confirmed)
      df <- confirmed
      req("sampleclass.." %in% names(df))
      df <- df[!is.na(df$sampleclass..), ]
      
      class_freq <- df %>%
        dplyr::count(sampleclass..) %>%
        dplyr::rename(Class = sampleclass.., Frequency = n)
      
      num_classes <- length(unique(class_freq$Class))
      num_colors <- max(3, num_classes)
      class_colors <- RColorBrewer::brewer.pal(num_colors, "Set1")
      
      plot_ly(
        data = class_freq,
        x = ~Class,
        y = ~Frequency,
        type = "bar",
        color = ~Class,
        colors = class_colors,
        text = ~Frequency,
        textposition = 'outside'
      ) %>%
        layout(
          title = "Class Frequency",
          xaxis = list(title = "Class"),
          yaxis = list(title = "Frequency"),
          showlegend = TRUE
        )
    })
    
    updateSelectizeInput(session, "removedatarow", choices = rownames(confirmed), server = TRUE)
    updateSelectizeInput(session, "removedatacol", choices = names(confirmed), server = TRUE)
  })
  


  # Confirmação do SMOTE-IPF - ADICIONADO EM 14/07/2025
  
  observeEvent(input$confirmSMOTEIPF, {
    req(filedata(), SMOTEIPF_Data())
    
    original <- filedata()
    added <- SMOTEIPF_Data()$syn_data
    removed <- SMOTEIPF_Data()$removed_data
    
    class_col <- input$target_ipf
    if ("class" %in% names(original)) names(original)[names(original) == "class"] <- "sampleclass.."
    if (class_col %in% names(original)) names(original)[names(original) == class_col] <- "sampleclass.."
    
    if (!is.null(added) && class_col %in% names(added)) names(added)[names(added) == class_col] <- "sampleclass.."
    if (!is.null(added) && "class" %in% names(added)) names(added)[names(added) == "class"] <- "sampleclass.."
    
    if (!is.null(removed) && class_col %in% names(removed)) names(removed)[names(removed) == class_col] <- "sampleclass.."
    if (!is.null(removed) && "class" %in% names(removed)) names(removed)[names(removed) == "class"] <- "sampleclass.."
    
    if (!is.null(added)) added$fake_cat <- NULL
    if (!is.null(removed)) removed$fake_cat <- NULL
    
    cols <- names(original)
    
    if (!is.null(added)) {
      added <- added[, intersect(cols, names(added)), drop = FALSE]
      added$sampleclass.. <- paste(added$sampleclass.., "Synthetic")
    }
    
    if (!is.null(removed)) {
      removed <- removed[, intersect(cols, names(removed)), drop = FALSE]
      removed$sampleclass.. <- paste(removed$sampleclass.., "Removed")
    }
    
    confirmed <- dplyr::bind_rows(original, added, removed)
    confirmed_final <- dplyr::bind_rows(original, added) #MODIFICADO EM 08/07/2025
    
    # combined_data_final(confirmed)
    combined_data_final(confirmed_final) #MODIFICADO EM 08/07/2025
    
    confirmed_numerics <- confirmed %>%
      dplyr::select(where(is.numeric))
    
    if ("sampleclass.." %in% names(confirmed_numerics)) {
      confirmed_numerics <- confirmed_numerics %>% dplyr::select(-sampleclass..)
    }
    
    output$preview1 <- renderDT({
      req(confirmed)
      datatable(confirmed_numerics, options = list(scrollX = TRUE))
    })
    
    output$datasummary <- renderPrint({
      req(confirmed)
      psych::describe(confirmed_numerics)
    })
    
    output$summary <- renderDT({
      req(confirmed)
      skimr::skim(confirmed_numerics)
    })
    
    output$barplot <- renderPlotly({
      req(confirmed)
      df <- confirmed
      req("sampleclass.." %in% names(df))
      df <- df[!is.na(df$sampleclass..), ]
      
      class_freq <- df %>%
        dplyr::count(sampleclass..) %>%
        dplyr::rename(Class = sampleclass.., Frequency = n)
      
      num_classes <- length(unique(class_freq$Class))
      num_colors <- max(3, num_classes)
      class_colors <- RColorBrewer::brewer.pal(num_colors, "Set1")
      
      plot_ly(
        data = class_freq,
        x = ~Class,
        y = ~Frequency,
        type = "bar",
        color = ~Class,
        colors = class_colors,
        text = ~Frequency,
        textposition = 'outside'
      ) %>%
        layout(
          title = "Class Frequency",
          xaxis = list(title = "Class"),
          yaxis = list(title = "Frequency"),
          showlegend = TRUE
        )
    })
    
    updateSelectizeInput(session, "removedatarow", choices = rownames(confirmed), server = TRUE)
    updateSelectizeInput(session, "removedatacol", choices = names(confirmed), server = TRUE)
  })
  

  
  
  # Confirmação do SPIDER - ADICIONADO EM 14/07/2025
  
  observeEvent(input$confirmSPIDER, {
    req(filedata(), SPIDER_Data())
    
    original <- filedata()
    added <- SPIDER_Data()$syn_data
    removed <- SPIDER_Data()$removed_data
    
    class_col <- input$target_spider
    if ("class" %in% names(original)) names(original)[names(original) == "class"] <- "sampleclass.."
    if (class_col %in% names(original)) names(original)[names(original) == class_col] <- "sampleclass.."
    
    if (!is.null(added) && class_col %in% names(added)) names(added)[names(added) == class_col] <- "sampleclass.."
    if (!is.null(added) && "class" %in% names(added)) names(added)[names(added) == "class"] <- "sampleclass.."
    
    if (!is.null(removed) && class_col %in% names(removed)) names(removed)[names(removed) == class_col] <- "sampleclass.."
    if (!is.null(removed) && "class" %in% names(removed)) names(removed)[names(removed) == "class"] <- "sampleclass.."
    
    if (!is.null(added)) added$fake_cat <- NULL
    if (!is.null(removed)) removed$fake_cat <- NULL
    
    cols <- names(original)
    
    if (!is.null(added)) {
      added <- added[, intersect(cols, names(added)), drop = FALSE]
      added$sampleclass.. <- paste(added$sampleclass.., "Synthetic")
    }
    
    if (!is.null(removed)) {
      removed <- removed[, intersect(cols, names(removed)), drop = FALSE]
      removed$sampleclass.. <- paste(removed$sampleclass.., "Removed")
    }
    
    confirmed <- dplyr::bind_rows(original, added, removed)
    confirmed_final <- dplyr::bind_rows(original, added) 
    
    combined_data_final(confirmed_final) 
    
    confirmed_numerics <- confirmed %>%
      dplyr::select(where(is.numeric))
    
    if ("sampleclass.." %in% names(confirmed_numerics)) {
      confirmed_numerics <- confirmed_numerics %>% dplyr::select(-sampleclass..)
    }
    
    output$preview1 <- renderDT({
      req(confirmed)
      datatable(confirmed_numerics, options = list(scrollX = TRUE))
    })
    
    output$datasummary <- renderPrint({
      req(confirmed)
      psych::describe(confirmed_numerics)
    })
    
    output$summary <- renderDT({
      req(confirmed)
      skimr::skim(confirmed_numerics)
    })
    
    output$barplot <- renderPlotly({
      req(confirmed)
      df <- confirmed
      req("sampleclass.." %in% names(df))
      df <- df[!is.na(df$sampleclass..), ]
      
      class_freq <- df %>%
        dplyr::count(sampleclass..) %>%
        dplyr::rename(Class = sampleclass.., Frequency = n)
      
      num_classes <- length(unique(class_freq$Class))
      num_colors <- max(3, num_classes)
      class_colors <- RColorBrewer::brewer.pal(num_colors, "Set1")
      
      plot_ly(
        data = class_freq,
        x = ~Class,
        y = ~Frequency,
        type = "bar",
        color = ~Class,
        colors = class_colors,
        text = ~Frequency,
        textposition = 'outside'
      ) %>%
        layout(
          title = "Class Frequency",
          xaxis = list(title = "Class"),
          yaxis = list(title = "Frequency"),
          showlegend = TRUE
        )
    })
    
    updateSelectizeInput(session, "removedatarow", choices = rownames(confirmed), server = TRUE)
    updateSelectizeInput(session, "removedatacol", choices = names(confirmed), server = TRUE)
  })
  
  
  
  
  
  # Combined Samples (Originais + Sintéticos) — atualiza somente após confirmSMOTE ou confirmSMOTENC OU Confirm BORDERLINE SMOTE ou Confirm SVM SMOTE
  #ou Confirm ADASYN OU Confirm RANDOM UPSAMPLING OU Confirm TOMEK OU Confirm Near Miss OU Confirm ENN OU Confirm OSS OU Confirm RD
  #ou Confirm SMOTE TL ou Confirm SMOTE ENN ou Confirm SBC ou Confirm SMOTE IPF ou Confirm SPIDER
  
  
  output$synthetic_data <- renderDT({
    req(combined_data_final())
    datatable(combined_data_final(), options = list(pageLength = 5))
    
  })
  
  

  output$download_selected <- downloadHandler(
    filename = function() {
      dataset_name <- switch(input$dataset_choice,
                             original = "original_samples",
                             synthetic = "synthetic_samples",
                             combined = "combined_samples",
                             removed = "removed_samples")
      paste(dataset_name, ifelse(input$export_format == "csv", ".csv", ".xlsx"), sep = "")
    },
    content = function(file) {
      dataset <- switch(input$dataset_choice,
                        
                        ## Original
                        original = {
                          df <- filedata()
                          if ("sampleclass.." %in% colnames(df)) {
                            names(df)[names(df) == "sampleclass.."] <- "Class"
                          }
                          if ("class" %in% colnames(df)) {
                            names(df)[names(df) == "class"] <- "Class"
                          }
                          df <- df[, c(setdiff(names(df), "Class"), "Class")]
                          df
                        },
                        
                        ## Synthetics
                        synthetic = {
                          synthetic_data <- NULL
                          
                          if (isTRUE(runSMOTE()) && !is.null(SMOTE_Data())) {
                            synthetic_data <- SMOTE_Data()$syn_data
                          } else if (isTRUE(runSMOTENC()) && !is.null(SMOTENC_Data())) {
                            synthetic_data <- SMOTENC_Data()$syn_data
                          } else if (isTRUE(runBorderlineSMOTE()) && !is.null(BorderlineSMOTE_Data())) {
                            synthetic_data <- BorderlineSMOTE_Data()$syn_data
                          } else if (isTRUE(runSVM_SMOTE()) && !is.null(SVM_SMOTE_Data())) {
                            synthetic_data <- SVM_SMOTE_Data()$syn_data
                          } else if (isTRUE(runADASYN()) && !is.null(ADASYN_Data())) {
                            synthetic_data <- ADASYN_Data()$syn_data
                          } else if (isTRUE(runRU()) && !is.null(RU_Data())) {
                            synthetic_data <- RU_Data()$syn_data
                          } else if (isTRUE(runSMOTETL()) && !is.null(SMOTETL_Data()$syn_data)) {
                            synthetic_data <- SMOTETL_Data()$syn_data
                          }
                          
                          #######BLOCO ADICIONADO EM 05/07/2025
                          
                          else if (isTRUE(runSMOTEENN()) && !is.null(SMOTEENN_Data()$syn_data)) {
                            synthetic_data <- SMOTEENN_Data()$syn_data
                          }
                          
                          #######BLOCO ADICIONADO EM 05/07/2025
                          
                          #######BLOCO ADICIONADO EM 14/07/2025
                          
                          else if (isTRUE(runSMOTEIPF()) && !is.null(SMOTEIPF_Data()$syn_data)) {
                            synthetic_data <- SMOTEIPF_Data()$syn_data
                          }
                          #######BLOCO ADICIONADO EM 14/07/2025
                          
                          
                          #######BLOCO ADICIONADO EM 14/07/2025
                          
                          else if (isTRUE(runSPIDER()) && !is.null(SPIDER_Data()$syn_data)) {
                            synthetic_data <- SPIDER_Data()$syn_data
                          }
                          
                          #######BLOCO ADICIONADO EM 14/07/2025
                          
                          
                          
                         if (is.null(synthetic_data)) return(NULL)
                          
                          if ("class" %in% colnames(synthetic_data)) {
                            names(synthetic_data)[names(synthetic_data) == "class"] <- "Class"
                          }
                          if ("sampleclass.." %in% colnames(synthetic_data)) {
                            names(synthetic_data)[names(synthetic_data) == "sampleclass.."] <- "Class"
                          }
                          synthetic_data$Class <- paste(synthetic_data$Class, "Synthetic")
                          synthetic_data <- synthetic_data[, c(setdiff(names(synthetic_data), "Class"), "Class")]
                          synthetic_data
                        },
                       
                        
                        
                        ## Combinadas
                        combined = {
                           df <- combined_data_final()
                          if ("sampleclass.." %in% colnames(df)) {
                            names(df)[names(df) == "sampleclass.." ] <- "Class"
                          }
                          if ("class" %in% colnames(df)) {
                            names(df)[names(df) == "class"] <- "Class"
                          }
                          df <- df[, c(setdiff(names(df), "Class"), "Class")]
                          df
                        },
                    
                  
                        
                        ## Removidas
                        removed = {
                          removed_data <- NULL
                          
                          if (isTRUE(runTomek()) && !is.null(Tomek_Data()$removed_data)) {
                            removed_data <- Tomek_Data()$removed_data
                          } else if (isTRUE(runNearMiss()) && !is.null(NearMiss_Data()$removed_data)) {
                            removed_data <- NearMiss_Data()$removed_data
                          } else if (isTRUE(runENN()) && !is.null(ENN_Data()$removed_data)) {
                            removed_data <- ENN_Data()$removed_data
                          } else if (isTRUE(runOSS()) && !is.null(OSS_Data()$removed_data)) {
                            removed_data <- OSS_Data()$removed_data
                          } else if (isTRUE(runRD()) && !is.null(RD_Data()$removed_data)) {
                            removed_data <- RD_Data()$removed_data
                            
                            
                            ###BLOCO ADICIONADO EM 10/07/2025
                            
                          } else if (isTRUE(runSBC()) && !is.null(SBC_Data()$removed_data)) {
                            removed_data <- SBC_Data()$removed_data
                            
                            ###BLOCO ADICIONADO EM 10/07/2025
                            
                            
                          } else if (isTRUE(runSMOTETL()) && !is.null(SMOTETL_Data()$removed_data)) {
                            removed_data <- SMOTETL_Data()$removed_data
                          }
                          
                          #########BLOCO ADICIONADO EM 05/07/2025
                          else if (isTRUE(runSMOTEENN()) && !is.null(SMOTEENN_Data()$removed_data)) {
                            removed_data <- SMOTEENN_Data()$removed_data
                          }
                          
                          #########BLOCO ADICIONADO EM 05/07/2025
                        
                          
                          #########BLOCO ADICIONADO EM 14/07/2025
                          else if (isTRUE(runSMOTEIPF()) && !is.null(SMOTEIPF_Data()$removed_data)) {
                            removed_data <- SMOTEIPF_Data()$removed_data
                          }
                          #########BLOCO ADICIONADO EM 14/07/2025
                          
                          
                          #########BLOCO ADICIONADO EM 14/07/2025
                          else if (isTRUE(runSPIDER()) && !is.null(SPIDER_Data()$removed_data)) {
                            removed_data <- SPIDER_Data()$removed_data
                          }
                          #########BLOCO ADICIONADO EM 14/07/2025
                          
                          
                          
                          
                          if (is.null(removed_data)) return(NULL)
                          
                          if ("sampleclass.." %in% colnames(removed_data)) {
                            names(removed_data)[names(removed_data) == "sampleclass.." ] <- "Class"
                          }
                          if ("class" %in% colnames(removed_data)) {
                            names(removed_data)[names(removed_data) == "class"] <- "Class"
                          }
                          removed_data$Class <- paste(removed_data$Class, "removed")
                          removed_data <- removed_data[, c(setdiff(names(removed_data), "Class"), "Class")]
                          removed_data
                        }
      )
      
      if (input$export_format == "csv") {
        dataset <- dataset %>%
          mutate(across(where(is.numeric), ~ format(., decimal.mark = ",", scientific = FALSE, trim = TRUE)))
        write.csv2(dataset, file, row.names = FALSE, fileEncoding = "Latin1", quote = FALSE)
      } else {
        openxlsx::write.xlsx(dataset, file)
      }
    }
  )
  
  
  ## FIM DO BLOCO ATUALIZADO COM SUPORTE A SMOTE E SMOTE-NC ATUALIZADO EM 29/05/2025
  
  #FUNCIONALIDADE DE EXPORTAÇÃO DE RESULTADOS ACRÉSCIMO EM 15/02/2025
  
  ##BLOCO MODIFICAOD EM 03/04/2025############
  ##BLOCO MODIFICADO EM 13/04/2025############
  
  ########ACRÉSCIMO DOS RESÍDUOS DA PCA EM VISUALIZAÇÕES EM 14/03/2025##### 
  
  ##ATUALIZADO EM 29/05/2025 - ACRÉSCIMO DE SMOTE NC
  
  ##ATUALIZADO EM 05/06/2025 - ACRÉSCIMO DE BORDERLINE SMOTE
  
  ##ATUALIZADO EM 06/06/2025 - ACRÉSCIMO DE SVM SMOTE
  
  ##ATUALIZADO EM 08/06/2025 - ACRÉSCIMO DE ADASYN 
  
  ##ATUALIZADO EM 09/06/2025 - ACRÉSCIMO DE RANDOM UPSAMPLING
  
  ##ATUALIZADO EM 14/06/2025 - ACRÉSCIMO DE TOMEK LINKS
  
  ##ATUALIZADO EM 17/06/2025 - ACRÉSCIMO DE NEAR MISS
  
  ##ATUALIZADO EM 17/06/2025 - ACRÉSCIMO DE ENN
  
  ##ATUALIZADO EM 18/06/2025 - ACRÉSCIMO DE OSS
  
  ##ATUALIZADO EM 18/06/2025 - ACRÉSCIMO DE RANDOM DOWNSAMPLING
  
  ##ATUALIZADO EM 02/07/2025 - ACRÉSCIMO DE SMOTE TL
  
  ##ATUALIZADO EM 05/07/2025 - ACRÉSCIMO DE SMOTE ENN
  
  ##ATUALIZADO EM 10/07/2025 - ACRÉSCIMO DE SBC

  ##ATUALIZADO EM 14/07/2025 - ACRÉSCIMO DE SMOTE IPF
    
  ##ATUALIZADO EM 14/07/2025 - ACRÉSCIMO DE SPIDER
  
  # Valor reativo para controle do botão
  
  residuos_ativado <- reactiveVal(FALSE)
  
  observeEvent(input$run_residuals, {
    residuos_ativado(TRUE)
  })
  
  observe({
    req(residuos_ativado())
    req(input$numPCresidCustom)
    
    combined_data <- NULL
    original_data <- as.data.frame(filedata())
    smote_data <- NULL
    class_col <- NULL
    
    if (!is.null(SMOTETL_Data())) {
    
      class_col <- input$target_tl
      
      smote_data_added <- SMOTETL_Data()$syn_data
      smote_data_removed <- SMOTETL_Data()$removed_data
      
      names(original_data)[names(original_data) == class_col] <- "class"
      
      if (!is.null(smote_data_added) && nrow(smote_data_added) > 0) {
        names(smote_data_added)[names(smote_data_added) == class_col] <- "class"
        smote_data_added$class <- paste(smote_data_added$class, "Synthetic")
        smote_data_added$Tipo <- "Synthetic"
      } else {
        smote_data_added <- original_data[0, , drop = FALSE]
      }
      
      if (!is.null(smote_data_removed) && nrow(smote_data_removed) > 0) {
        names(smote_data_removed)[names(smote_data_removed) == class_col] <- "class"
        smote_data_removed$class <- paste(smote_data_removed$class, "Removed")
        smote_data_removed$Tipo <- "Removed"
      } else {
        smote_data_removed <- original_data[0, , drop = FALSE]
      }
      
      feature_cols <- intersect(names(original_data),
                                union(names(smote_data_added), names(smote_data_removed)))
      original_data <- original_data[, feature_cols, drop = FALSE]
      smote_data_added <- smote_data_added[, feature_cols, drop = FALSE]
      smote_data_removed <- smote_data_removed[, feature_cols, drop = FALSE]
      
      original_data$Tipo <- "Original"
      smote_data <- dplyr::bind_rows(smote_data_added, smote_data_removed)
      
    } 
    
    ###########################BLOCO ADICIONADO EM 05/07/2025
  
  else  if (!is.null(SMOTEENN_Data())) {
      class_col <- input$target_enn2
      
      smote_data_added <- SMOTEENN_Data()$syn_data
      smote_data_removed <- SMOTEENN_Data()$removed_data
      
      names(original_data)[names(original_data) == class_col] <- "class"
      
      if (!is.null(smote_data_added) && nrow(smote_data_added) > 0) {
        names(smote_data_added)[names(smote_data_added) == class_col] <- "class"
        smote_data_added$class <- paste(smote_data_added$class, "Synthetic")
        smote_data_added$Tipo <- "Synthetic"
      } else {
        smote_data_added <- original_data[0, , drop = FALSE]
      }
      
      if (!is.null(smote_data_removed) && nrow(smote_data_removed) > 0) {
        names(smote_data_removed)[names(smote_data_removed) == class_col] <- "class"
        smote_data_removed$class <- paste(smote_data_removed$class, "Removed")
        smote_data_removed$Tipo <- "Removed"
      } else {
        smote_data_removed <- original_data[0, , drop = FALSE]
      }
      
      feature_cols <- unique(names(original_data))
      smote_data_added <- smote_data_added[, intersect(feature_cols, names(smote_data_added)), drop = FALSE]
      smote_data_removed <- smote_data_removed[, intersect(feature_cols, names(smote_data_removed)), drop = FALSE]
      
      original_data <- original_data[, feature_cols, drop = FALSE]
      
      smote_data <- dplyr::bind_rows(smote_data_added, smote_data_removed)
      
      original_data$Tipo <- "Original"
      
      }
    
   
      
    ###########################BLOCO ADICIONADO EM 05/07/2025
    
    
    ###########################BLOCO ADICIONADO EM 14/07/2025
    
    else if (!is.null(SMOTEIPF_Data())) {
      class_col <- input$target_ipf
      
      smote_data_added <- SMOTEIPF_Data()$syn_data
      smote_data_removed <- SMOTEIPF_Data()$removed_data
      
      names(original_data)[names(original_data) == class_col] <- "class"
      
      if (!is.null(smote_data_added) && nrow(smote_data_added) > 0) {
        names(smote_data_added)[names(smote_data_added) == class_col] <- "class"
        smote_data_added$class <- paste(smote_data_added$class, "Synthetic")
        smote_data_added$Tipo <- "Synthetic"
      } else {
        smote_data_added <- original_data[0, , drop = FALSE]
      }
      
      if (!is.null(smote_data_removed) && nrow(smote_data_removed) > 0) {
        names(smote_data_removed)[names(smote_data_removed) == class_col] <- "class"
        smote_data_removed$class <- paste(smote_data_removed$class, "Removed")
        smote_data_removed$Tipo <- "Removed"
      } else {
        smote_data_removed <- original_data[0, , drop = FALSE]
      }
      
      feature_cols <- unique(names(original_data))
      smote_data_added <- smote_data_added[, intersect(feature_cols, names(smote_data_added)), drop = FALSE]
      smote_data_removed <- smote_data_removed[, intersect(feature_cols, names(smote_data_removed)), drop = FALSE]
      
      original_data <- original_data[, feature_cols, drop = FALSE]
      
      smote_data <- dplyr::bind_rows(smote_data_added, smote_data_removed)
      
      original_data$Tipo <- "Original"
    }
    
    ###########################BLOCO ADICIONADO EM 14/07/2025
    
    
    
    ###########################BLOCO ADICIONADO EM 14/07/2025
    
    else if (!is.null(SPIDER_Data())) {
      class_col <- input$target_spider
      
      smote_data_added <- SPIDER_Data()$syn_data
      smote_data_removed <- SPIDER_Data()$removed_data
      
      names(original_data)[names(original_data) == class_col] <- "class"
      
      if (!is.null(smote_data_added) && nrow(smote_data_added) > 0) {
        names(smote_data_added)[names(smote_data_added) == class_col] <- "class"
        smote_data_added$class <- paste(smote_data_added$class, "Synthetic")
        smote_data_added$Tipo <- "Synthetic"
      } else {
        smote_data_added <- original_data[0, , drop = FALSE]
      }
      
      if (!is.null(smote_data_removed) && nrow(smote_data_removed) > 0) {
        names(smote_data_removed)[names(smote_data_removed) == class_col] <- "class"
        smote_data_removed$class <- paste(smote_data_removed$class, "Removed")
        smote_data_removed$Tipo <- "Removed"
      } else {
        smote_data_removed <- original_data[0, , drop = FALSE]
      }
      
      feature_cols <- unique(names(original_data))
      smote_data_added <- smote_data_added[, intersect(feature_cols, names(smote_data_added)), drop = FALSE]
      smote_data_removed <- smote_data_removed[, intersect(feature_cols, names(smote_data_removed)), drop = FALSE]
      
      original_data <- original_data[, feature_cols, drop = FALSE]
      
      smote_data <- dplyr::bind_rows(smote_data_added, smote_data_removed)
      
      original_data$Tipo <- "Original"
    }
    
    ###########################BLOCO ADICIONADO EM 14/07/2025
    
    
    else {
      if (!is.null(SMOTENC_Data())) {
        smote_data <- SMOTENC_Data()$syn_data
        class_col <- input$target_nc
        
      } else if (!is.null(SMOTE_Data())) {
        smote_data <- SMOTE_Data()$syn_data
        class_col <- input$target
        
      } else if (!is.null(BorderlineSMOTE_Data())) {
        smote_data <- BorderlineSMOTE_Data()$syn_data
        class_col <- input$target_borderline
        
      } else if (!is.null(SVM_SMOTE_Data())) {
        smote_data <- SVM_SMOTE_Data()$syn_data
        class_col <- input$target_svm
        
      } else if (!is.null(ADASYN_Data())) {
        smote_data <- ADASYN_Data()$syn_data
        class_col <- input$target_adasyn
        
      } else if (!is.null(RU_Data())) {
        smote_data <- RU_Data()$syn_data
        class_col <- input$target_ru
        
      } else if (!is.null(Tomek_Data())) {
        smote_data <- Tomek_Data()$removed_data
        class_col <- input$target_tomek
        
      } else if (!is.null(NearMiss_Data())) {
        smote_data <- NearMiss_Data()$removed_data
        class_col <- input$target_nearmiss
        
      } else if (!is.null(ENN_Data())) {
        smote_data <- ENN_Data()$removed_data
        class_col <- input$target_enn
        
      } else if (!is.null(OSS_Data())) {
        smote_data <- OSS_Data()$removed_data
        class_col <- input$target_oss
        
      } else if (!is.null(RD_Data())) {
        smote_data <- RD_Data()$removed_data
        class_col <- input$target_rd
      
        
        ####BLOCO ADICIONADO EM 10/07/2025
        
      } else if (!is.null(SBC_Data())) {
        smote_data <- SBC_Data()$removed_data
        class_col <- input$target_sbc

        
        ####BLOCO ADICIONADO EM 10/07/2025
          
      } else {
        return()
      }
 
      if (is.null(smote_data) || !(class_col %in% names(original_data))) return()
      
      names(original_data)[names(original_data) == class_col] <- "class"
      names(smote_data)[names(smote_data) == class_col] <- "class"
      
      suffix <- if (isTRUE(runTomek()) || isTRUE(runNearMiss()) || isTRUE(runENN()) ||
                    isTRUE(runOSS()) || isTRUE(runRD())  || isTRUE(runSBC())       ) "Removed" else "Synthetic"
      
      smote_data$class <- paste(smote_data$class, suffix)
      smote_data$Tipo <- suffix
      smote_data$fake_cat <- NULL
      
      feature_cols <- intersect(names(original_data), names(smote_data))
      original_data <- original_data[, feature_cols, drop = FALSE]
      smote_data <- smote_data[, feature_cols, drop = FALSE]
      
      original_data$Tipo <- "Original"
    }

    
    
    ##########ADICIONADO EM 05/07/2025
    
    if (nrow(smote_data) == 0) {
      message("smote_data vazio, nada para combinar.")
      return()
    }
    
    feature_cols <- unique(names(original_data))
    
    
    ##MODIFICADO EM 07/07/2025
    
  #  smote_data_added <- smote_data_added[, intersect(feature_cols, names(smote_data_added)), drop = FALSE]
   
    
    if (exists("smote_data_added") && !is.null(smote_data_added) && nrow(smote_data_added) > 0) {
      smote_data_added <- smote_data_added[, intersect(feature_cols, names(smote_data_added)), drop = FALSE]
    }
    
    ##MODIFICADO EM 07/07/2025
    
     
    
    ##########ADICIONADO EM 05/07/2025
    
    
  ##  combined_data <- dplyr::bind_rows(original_data, smote_data)
  ##  combined_data$class <- as.character(combined_data$class)
    
    
    combined_data <- dplyr::bind_rows(original_data, smote_data)
    if (nrow(combined_data) == 0) {
      message("combined_data vazio! Nada para calcular.")
      return()
    }
    combined_data$class <- as.character(combined_data$class)
    
    
    if (!"Tipo" %in% names(combined_data)) {
      combined_data$Tipo <- ifelse(grepl("Synthetic", combined_data$class), "Synthetic",
                                   ifelse(grepl("Removed", combined_data$class), "Removed", "Original"))
    }
    
    numeric_cols <- sapply(combined_data, is.numeric)
    pcaData <- combined_data[, numeric_cols, drop = FALSE]
    
    pca_model <- prcomp(pcaData, center = TRUE, scale. = TRUE)
    scores <- pca_model$x[, 1:input$numPCresidCustom, drop = FALSE]
    loadings <- pca_model$rotation[, 1:input$numPCresidCustom, drop = FALSE]
    
    reconstructed <- scores %*% t(loadings)
    if (!isFALSE(pca_model$scale)) {
      reconstructed <- scale(reconstructed, center = FALSE, scale = 1 / pca_model$scale)
    }
    if (!isFALSE(pca_model$center)) {
      reconstructed <- scale(reconstructed, center = -pca_model$center, scale = FALSE)
    }
    
    q <- rowSums((pcaData - reconstructed)^2)
    h <- rowSums(scores^2)
    
    q0 <- mean(q) + 3 * sd(q)
    h0 <- quantile(h, probs = 0.975)
    
    h_norm <- h / h0
    q_norm <- q / q0
    
    x_base <- h_norm
    y_base <- q_norm
    
    use_log <- isTRUE(input$logResidualsCustom)
    x <- if (use_log) log1p(x_base) else x_base
    y <- if (use_log) log1p(y_base) else y_base
    
    df <- data.frame(
      SampleID = rownames(pcaData),
      X = x,
      Y = y,
      Classe = combined_data$class
    )
    
    classes_ordered <- sort(unique(df$Classe))
    base_classes <- gsub(" Synthetic.*$| Removed.*$", "", classes_ordered)
    classes_ordered <- unique(unlist(lapply(unique(base_classes), function(cls) {
      c(cls, paste0(cls, " Synthetic"), paste0(cls, " Removed"))
    })))
    classes_ordered <- classes_ordered[classes_ordered %in% unique(df$Classe)]
    
    ##MODIFICADO EM 05/07/2025
    
   ## cores_paleta <- RColorBrewer::brewer.pal(n = max(3, length(classes_ordered)), name = "Set1")
  ##  names(cores_paleta) <- classes_ordered
    
    
    cores_paleta <- RColorBrewer::brewer.pal(
      n = min(max(3, length(classes_ordered)), 9),
      name = "Set1"
    )
    
    if (length(classes_ordered) > length(cores_paleta)) {
      cores_paleta <- rep(cores_paleta, length.out = length(classes_ordered))
    }
    
    names(cores_paleta) <- classes_ordered
    
    
    ##MODIFICADO EM 05/07/2025
    
    
    p <- plot_ly(df, x = ~X, y = ~Y, type = "scatter", mode = "markers+text",
                 color = ~Classe, colors = cores_paleta,
                 marker = list(size = 5),
                 text = ~SampleID,
                 textposition = 'top center',
                 textfont = list(color = 'lightgray', size = 10),
                 hoverinfo = 'text',
                 hovertext = ~paste("Sample:", SampleID,
                                    "<br>Class:", Classe,
                                    "<br>Score distance:", round(X, 3),
                                    "<br>Orthogonal distance:", round(Y, 3))
    )
    
    if (use_log) {
      h_norm_curve <- seq(0, 3, length.out = 300)
      q_norm_orange <- 1 - h_norm_curve
      q_norm_red <- 3 * (1 - h_norm_curve / 3)
      
      x_laranja <- log1p(h_norm_curve)
      y_laranja <- log1p(q_norm_orange)
      
      x_vermelha <- log1p(h_norm_curve)
      y_vermelha <- log1p(q_norm_red)
      
      p <- p %>%
        add_trace(x = x_laranja, y = y_laranja,
                  type = "scatter", mode = "lines",
                  line = list(color = "orange", width = 1.5, dash = "dash"),
                  showlegend = FALSE, inherit = FALSE) %>%
        add_trace(x = x_vermelha, y = y_vermelha,
                  type = "scatter", mode = "lines",
                  line = list(color = "red", width = 1.5, dash = "dash"),
                  showlegend = FALSE, inherit = FALSE)
    } else {
      p <- p %>%
        add_trace(x = c(0, 1), y = c(1, 0),
                  type = "scatter", mode = "lines",
                  line = list(color = "orange", width = 1.5, dash = "dash"),
                  showlegend = FALSE, inherit = FALSE) %>%
        add_trace(x = c(0, 3), y = c(3, 0),
                  type = "scatter", mode = "lines",
                  line = list(color = "red", width = 1.5, dash = "dash"),
                  showlegend = FALSE, inherit = FALSE)
    }
    
    xmax <- max(x, if (use_log) log1p(3) else 3) * 1.05
    ymax <- max(y, if (use_log) log1p(3) else 3) * 1.05
    
    output$pcaResidualsPlotCustom <- renderPlotly({
      p %>%
        layout(
          title = paste0("Distances (ncomp = ", input$numPCresidCustom, ")"),
          xaxis = list(
            title = if (use_log) "Score Distance, log(1 + h/h0)" else "Score Distance, h/h0",
            range = c(0, xmax), zeroline = TRUE
          ),
          yaxis = list(
            title = if (use_log) "Orthogonal Distance, log(1 + q/q0)" else "Orthogonal Distance, q/q0",
            range = c(0, ymax), zeroline = TRUE
          ),
          hovermode = "closest",
          legend = list(title = list(text = " "), x = 1.05, y = 1),
          margin = list(r = 150)
        )
    })
  
  
    
    output$residualsHelpTextUI <- renderUI({
      HTML(
        "<div style='font-size:15px; line-height:1.5;'>
  <p><strong>Graph interpretation:</strong></p>
  <p>This chart shows the relationship between the <strong>orthogonal distance (q/q0)</strong> and the <strong>score distance (h/h0)</strong> for each sample in the PCA model.</p>
  <ul>
    <li><span style='color:orange;'><strong>Orange line</strong></span>: critical limit (straight line or curve in log mode).</li>
    <li><span style='color:red;'><strong>Red line</strong></span>: extreme outlier limit.</li>
  </ul>
  <p>In log mode, <code>log(1 + x)</code> is applied for better visualization.</p>
  </ul>
  <p>The distances are calculated based on the reconstruction of the PCA model considering the specified <strong>n</strong> principal components. The <em>Score Distance</em> (h) represents the deviation in the principal components space, while the <em>Orthogonal Distance</em> (q) represents the reconstruction error.</p>
  <p>In the chart, the normalized forms of these metrics are displayed: <code>h/h0</code> and <code>q/q0</code>, where <code>h0 = quantile(h, 0.975)</code> and <code>q0 = mean(q) + 3 * sd(q)</code>. When <em>log</em> is enabled, <code>log(1 + h/h0)</code> and <code>log(1 + q/q0)</code> are applied for better visualization.</p>
</div>"
      )
    })
    
  })
  
  
  
  
  
  
  
  
  ##ATUALIZADO EM 29/05/2025 - ACRÉSCIMO DE SMOTE NC
  ##ATUALIZADO EM 05/06/2025 - ACRÉSCIMO DE BORDERLINE SMOTE  
  ##ATUALIZADO EM 06/06/2025 - ACRÉSCIMO DE SVM SMOTE    
  ##ATUALIZADO EM 08/06/2025 - ACRÉSCIMO DE ADASYN  
  ##ATUALIZADO EM 09/06/2025 - ACRÉSCIMO DE RANDOM UPSAMPLING  
  ##ATUALIZADO EM 14/06/2025 - ACRÉSCIMO DE TOMEK LINKS 
  ##ATUALIZADO EM 17/06/2025 - ACRÉSCIMO DE NEAR MISS    
  ##ATUALIZADO EM 17/06/2025 - ACRÉSCIMO DE ENN
  ##ATUALIZADO EM 18/06/2025 - ACRÉSCIMO DE OSS  
  ##ATUALIZADO EM 18/06/2025 - ACRÉSCIMO DE RANDOM DOWNSAMPLING  
  ##ATUALIZADO EM 02/07/2025 - ACRÉSCIMO DE SMOTE TL  
  ##ATUALIZADO EM 05/07/2025 - ACRÉSCIMO DE SMOTE ENN  
  ##ATUALIZADO EM 10/07/2025 - ACRÉSCIMO DE SBC  
  ##ATUALIZADO EM 14/07/2025 - ACRÉSCIMO DE SMOTE IPF  
  ##ATUALIZADO EM 14/07/2025 - ACRÉSCIMO DE SPIDER  
  
  ########ACRÉSCIMO DOS RESÍDUOS DA PCA EM VISUALIZAÇÕES EM 14/03/2025#####  
  
  
  ##BLOCO MODIFICADO EM 03/04/2025
  
  
  ########ACRÉSCIMO DOS OUTLIERS DA PCA EM VISUALIZAÇÕES EM 14/03/2025#####
  # Criando um objeto reativo para armazenar combined_data
  # Reativo: dados combinados
  
  ##ATUALIZADO EM 29/05/2025 COM ACRÉSCIMO DE SMOTE-NC EM OUTLIERS DA PCA
  ##ATUALIZADO EM 05/06/2025 COM ACRÉSCIMO DE BORDERLINE SMOTE EM OUTLIERS DA PCA
  ##ATUALIZADO EM 08/06/2025 COM ACRÉSCIMO DE ADASYN EM OUTLIERS DA PCA
  ##ATUALIZADO EM 09/06/2025 COM ACRÉSCIMO DE RANDOM UPSAMPLING EM OUTLIERS DA PCA
  ##ATUALIZADO EM 14/06/2025 COM ACRÉSCIMO DE TOMEK LINKS EM OUTLIERS DA PCA
  ##ATUALIZADO EM 17/06/2025 COM ACRÉSCIMO DE NEAR MISS EM OUTLIERS DA PCA
  ##ATUALIZADO EM 17/06/2025 COM ACRÉSCIMO DE ENN EM OUTLIERS DA PCA
  ##ATUALIZADO EM 18/06/2025 COM ACRÉSCIMO DE OSS EM OUTLIERS DA PCA
  ##ATUALIZADO EM 18/06/2025 COM ACRÉSCIMO DE RANDOM DOWNSAMPLING EM OUTLIERS DA PCA
  ##ATUALIZADO EM 02/07/2025 COM ACRÉSCIMO DE SMOTE TL EM OUTLIERS DA PCA  
  ##ATUALIZADO EM 05/07/2025 COM ACRÉSCIMO DE SMOTE ENN EM OUTLIERS DA PCA
  ##ATUALIZADO EM 10/07/2025 COM ACRÉSCIMO DE SBC EM OUTLIERS DA PCA
  ##ATUALIZADO EM 14/07/2025 COM ACRÉSCIMO DE SMOTE IPF EM OUTLIERS DA PCA
  ##ATUALIZADO EM 14/07/2025 COM ACRÉSCIMO DE SPIDER EM OUTLIERS DA PCA
  
  
  ########ACRÉSCIMO DO GRÁFICO DE OUTLIERS COM SUPORTE A SMOTE E SMOTE-NC#####
  
  # combined_data_reactive <- reactive({
  # if (!is.null(SMOTENC_Data())) {
  #  original_data <- filedata()
  #  synthetic_data <- SMOTENC_Data()$syn_data
  #  class_col <- input$target_nc
  #  } else if (!is.null(SMOTE_Data())) {
  #    original_data <- filedata()
  #    synthetic_data <- SMOTE_Data()$syn_data
  #    class_col <- input$target
  #  } else {
  #    return(NULL)
  #  }
  
  
  combined_data_reactive <- reactive({
    if (!is.null(SMOTETL_Data())) {
      
      original_data <- filedata()
      class_col <- input$target_tl
      
      smote_data_added <- SMOTETL_Data()$syn_data
      smote_data_removed <- SMOTETL_Data()$removed_data
      
      names(original_data)[names(original_data) == class_col] <- "class"
      
      if (!is.null(smote_data_added) && nrow(smote_data_added) > 0) {
        names(smote_data_added)[names(smote_data_added) == class_col] <- "class"
        smote_data_added$class <- paste0(smote_data_added$class, " Synthetic")
        smote_data_added$Tipo <- "Synthetic"
      } else {
        smote_data_added <- original_data[0, , drop = FALSE]
      }
      
      if (!is.null(smote_data_removed) && nrow(smote_data_removed) > 0) {
        names(smote_data_removed)[names(smote_data_removed) == class_col] <- "class"
        smote_data_removed$class <- paste0(smote_data_removed$class, " Removed")
        smote_data_removed$Tipo <- "Removed"
      } else {
        smote_data_removed <- original_data[0, , drop = FALSE]
      }
      
      feature_cols <- intersect(
        names(original_data),
        union(names(smote_data_added), names(smote_data_removed))
      )
      
      original_data <- original_data[, feature_cols, drop = FALSE]
      smote_data_added <- smote_data_added[, feature_cols, drop = FALSE]
      smote_data_removed <- smote_data_removed[, feature_cols, drop = FALSE]
      
      combined_data <- dplyr::bind_rows(original_data, smote_data_added, smote_data_removed)
      rownames(combined_data) <- seq_len(nrow(combined_data))
      return(combined_data)
      
    } 
    
   
    ###########################BLOCO ADICIONADO EM 05/07/2025
    
    if (!is.null(SMOTEENN_Data())) {
      
      original_data <- filedata()
      class_col <- input$target_enn2
      
      smote_data_added <- SMOTEENN_Data()$syn_data
      smote_data_removed <- SMOTEENN_Data()$removed_data
      
      names(original_data)[names(original_data) == class_col] <- "class"
      
      if (!is.null(smote_data_added) && nrow(smote_data_added) > 0) {
        names(smote_data_added)[names(smote_data_added) == class_col] <- "class"
        smote_data_added$class <- paste0(smote_data_added$class, " Synthetic")
        smote_data_added$Tipo <- "Synthetic"
      } else {
        smote_data_added <- original_data[0, , drop = FALSE]
      }
      
      if (!is.null(smote_data_removed) && nrow(smote_data_removed) > 0) {
        names(smote_data_removed)[names(smote_data_removed) == class_col] <- "class"
        smote_data_removed$class <- paste0(smote_data_removed$class, " Removed")
        smote_data_removed$Tipo <- "Removed"
      } else {
        smote_data_removed <- original_data[0, , drop = FALSE]
      }
      
      feature_cols <- intersect(
        names(original_data),
        union(names(smote_data_added), names(smote_data_removed))
      )
      
      original_data <- original_data[, feature_cols, drop = FALSE]
      smote_data_added <- smote_data_added[, feature_cols, drop = FALSE]
      smote_data_removed <- smote_data_removed[, feature_cols, drop = FALSE]
      
      combined_data <- dplyr::bind_rows(original_data, smote_data_added, smote_data_removed)
      rownames(combined_data) <- seq_len(nrow(combined_data))
      return(combined_data)
      
    } 
    
    
    
    ###########################BLOCO ADICIONADO EM 05/07/2025
    
    
    
    ###########################BLOCO ADICIONADO EM 14/07/2025
    
    if (!is.null(SMOTEIPF_Data())) {
      
      original_data <- filedata()
      class_col <- input$target_ipf
      
      smote_data_added <- SMOTEIPF_Data()$syn_data
      smote_data_removed <- SMOTEIPF_Data()$removed_data
      
      names(original_data)[names(original_data) == class_col] <- "class"
      
      if (!is.null(smote_data_added) && nrow(smote_data_added) > 0) {
        names(smote_data_added)[names(smote_data_added) == class_col] <- "class"
        smote_data_added$class <- paste0(smote_data_added$class, " Synthetic")
        smote_data_added$Tipo <- "Synthetic"
      } else {
        smote_data_added <- original_data[0, , drop = FALSE]
      }
      
      if (!is.null(smote_data_removed) && nrow(smote_data_removed) > 0) {
        names(smote_data_removed)[names(smote_data_removed) == class_col] <- "class"
        smote_data_removed$class <- paste0(smote_data_removed$class, " Removed")
        smote_data_removed$Tipo <- "Removed"
      } else {
        smote_data_removed <- original_data[0, , drop = FALSE]
      }
      
      feature_cols <- intersect(
        names(original_data),
        union(names(smote_data_added), names(smote_data_removed))
      )
      
      original_data <- original_data[, feature_cols, drop = FALSE]
      smote_data_added <- smote_data_added[, feature_cols, drop = FALSE]
      smote_data_removed <- smote_data_removed[, feature_cols, drop = FALSE]
      
      combined_data <- dplyr::bind_rows(original_data, smote_data_added, smote_data_removed)
      rownames(combined_data) <- seq_len(nrow(combined_data))
      return(combined_data)
      
    } 
    
    ###########################BLOCO ADICIONADO EM 14/07/2025
    
    
    
    ###########################BLOCO ADICIONADO EM 14/07/2025
    
    if (!is.null(SPIDER_Data())) {
      
      original_data <- filedata()
      class_col <- input$target_spider
      
      smote_data_added <- SPIDER_Data()$syn_data
      smote_data_removed <- SPIDER_Data()$removed_data
      
      names(original_data)[names(original_data) == class_col] <- "class"
      
      if (!is.null(smote_data_added) && nrow(smote_data_added) > 0) {
        names(smote_data_added)[names(smote_data_added) == class_col] <- "class"
        smote_data_added$class <- paste0(smote_data_added$class, " Synthetic")
        smote_data_added$Tipo <- "Synthetic"
      } else {
        smote_data_added <- original_data[0, , drop = FALSE]
      }
      
      if (!is.null(smote_data_removed) && nrow(smote_data_removed) > 0) {
        names(smote_data_removed)[names(smote_data_removed) == class_col] <- "class"
        smote_data_removed$class <- paste0(smote_data_removed$class, " Removed")
        smote_data_removed$Tipo <- "Removed"
      } else {
        smote_data_removed <- original_data[0, , drop = FALSE]
      }
      
      feature_cols <- intersect(
        names(original_data),
        union(names(smote_data_added), names(smote_data_removed))
      )
      
      original_data <- original_data[, feature_cols, drop = FALSE]
      smote_data_added <- smote_data_added[, feature_cols, drop = FALSE]
      smote_data_removed <- smote_data_removed[, feature_cols, drop = FALSE]
      
      combined_data <- dplyr::bind_rows(original_data, smote_data_added, smote_data_removed)
      rownames(combined_data) <- seq_len(nrow(combined_data))
      return(combined_data)
      
    } 
    
    ###########################BLOCO ADICIONADO EM 14/07/2025
    
    
    else if (!is.null(SMOTENC_Data())) {
      original_data <- filedata()
      synthetic_data <- SMOTENC_Data()$syn_data
      class_col <- input$target_nc
      sint_type <- "Synthetic"
      
    } else if (!is.null(SMOTE_Data())) {
      original_data <- filedata()
      synthetic_data <- SMOTE_Data()$syn_data
      class_col <- input$target
      sint_type <- "Synthetic"
      
    } else if (!is.null(BorderlineSMOTE_Data())) {
      original_data <- filedata()
      synthetic_data <- BorderlineSMOTE_Data()$syn_data
      class_col <- input$target_borderline 
      sint_type <- "Synthetic"
      
    } else if (!is.null(SVM_SMOTE_Data())) {
      original_data <- filedata()
      synthetic_data <- SVM_SMOTE_Data()$syn_data
      class_col <- input$target_svm     
      sint_type <- "Synthetic"
      
    } else if (!is.null(ADASYN_Data())) {
      original_data <- filedata()
      synthetic_data <- ADASYN_Data()$syn_data
      class_col <- input$target_adasyn       
      sint_type <- "Synthetic"
      
    } else if (!is.null(RU_Data())) {
      original_data <- filedata()
      synthetic_data <- RU_Data()$syn_data
      class_col <- input$target_ru  
      sint_type <- "Synthetic"
      
    } else if (!is.null(Tomek_Data())) {
      original_data <- filedata()
      synthetic_data <- Tomek_Data()$removed_data
      class_col <- input$target_tomek
      sint_type <- "Removed" #ALTERADO EM 29/06/2025
      
    } else if (!is.null(NearMiss_Data())) {
      original_data <- filedata()
      synthetic_data <- NearMiss_Data()$removed_data
      class_col <- input$target_nearmiss
      sint_type <- "Removed" #ALTERADO EM 29/06/2025
      
    } else if (!is.null(ENN_Data())) {
      original_data <- filedata()
      synthetic_data <- ENN_Data()$removed_data
      class_col <- input$target_enn
      sint_type <- "Removed"  #ALTERADO EM 29/06/2025
      
    } else if (!is.null(OSS_Data())) {
      original_data <- filedata()
      synthetic_data <- OSS_Data()$removed_data
      class_col <- input$target_oss
      sint_type <- "Removed"  #ALTERADO EM 29/06/2025
      
    } else if (!is.null(RD_Data())) {
      original_data <- filedata()
      synthetic_data <- RD_Data()$removed_data
      class_col <- input$target_rd
      sint_type <- "Removed"    #ALTERADO EM 29/06/2025
      
    
      ####BLOCO ADICIONADO EM 10/07/2025
        
    } else if (!is.null(SBC_Data())) {
      original_data <- filedata()
      synthetic_data <- SBC_Data()$removed_data
      class_col <- input$target_sbc
      sint_type <- "Removed"    #ALTERADO EM 29/06/2025
      
      
      ####BLOCO ADICIONADO EM 10/07/2025
      
      
    } else {
      return(NULL)
    }
    
    names(original_data)[names(original_data) == class_col] <- "class"
    names(synthetic_data)[names(synthetic_data) == class_col] <- "class"
    
    feature_cols <- intersect(names(original_data), names(synthetic_data))
    original_data <- original_data[, feature_cols, drop = FALSE]
    synthetic_data <- synthetic_data[, feature_cols, drop = FALSE]
    
    synthetic_data$class <- paste0(synthetic_data$class, " ", sint_type)
    
    combined_data <- rbind(original_data, synthetic_data)
    rownames(combined_data) <- seq_len(nrow(combined_data))
    return(combined_data)
  })

  # Controlando a ativação do gráfico
  
  outliers_ativado <- reactiveVal(FALSE)
  
  observeEvent(input$run_outliers, {
    outliers_ativado(TRUE)
  })
  
  observe({
    req(outliers_ativado())
    req(combined_data_reactive(), input$numPCoutipcaCustom)
    
    combined_data <- combined_data_reactive()
    numeric_cols <- sapply(combined_data, is.numeric)
    pcaData <- combined_data[, numeric_cols, drop = FALSE]
    
    if (ncol(pcaData) < 2) {
      output$OutPCACustom <- renderPlotly({
        plot_ly() %>% layout(title = "Error: Insufficient data for PCA")
      })
      return()
    }
    
    PCAmodel <- tryCatch(
      { rrcov::PcaClassic(pcaData, k = input$numPCoutipcaCustom) },
      error = function(e) {
        output$OutPCACustom <- renderPlotly({
          plot_ly() %>% layout(title = "Error calculating PCA")
        })
        return(NULL)
      }
    )
    
    if (is.null(PCAmodel)) return()
    
    df_plot <- data.frame(
      SD = PCAmodel@sd,
      OD = PCAmodel@od,
      SampleID = rownames(pcaData),
      Classe = combined_data$class
    )
    
    # Linhas de corte
    
    sd_cutoff <- sqrt(qchisq(0.975, df = input$numPCoutipcaCustom))
    od_cutoff <- sqrt(PCAmodel@cutoff.od)
    
    # Ordenando classes
    
    #########BLOCO MODIFICADO EM 15/06/2025
    
    classes <- sort(unique(df_plot$Classe))
    # base_classes <- gsub(" Synthetic", "", classes)
    base_classes <- gsub(" Removed| Synthetic", "", classes) #MODIFICADO EM 15/06/2025 #ALTERADO EM 29/06/2025
    
    
    # classes_ordenadas <- unique(unlist(lapply(unique(base_classes), function(cls) {
    #    c(cls, paste0(cls, " Synthetic"))
    #  })))
    
    
    classes_ordenadas <- unique(unlist(lapply(unique(base_classes), function(cls) {
      c(
        cls,
        paste0(cls, " Synthetic"),
        paste0(cls, " Removed") #ALTERADO EM 29/06/2025
      )
    })))
    
    
    
    
    df_plot$Classe <- factor(df_plot$Classe, levels = classes_ordenadas)
    
    # cores <- RColorBrewer::brewer.pal(n = max(3, length(classes_ordenadas)), name = "Set1")
    #  names(cores) <- classes_ordenadas
    
    n_classes <- length(classes_ordenadas)
    paleta_base <- RColorBrewer::brewer.pal(9, "Set1")
    cores <- colorRampPalette(paleta_base)(n_classes)
    names(cores) <- classes_ordenadas
    
    
    #########BLOCO MODIFICADO EM 15/06/2025
    
    output$OutPCACustom <- renderPlotly({
      xmax <- max(c(df_plot$SD, sd_cutoff)) * 1.1
      ymax <- max(c(df_plot$OD, od_cutoff)) * 1.1
      
      plot_ly(
        data = df_plot,
        x = ~SD,
        y = ~OD,
        type = "scatter",
        mode = "markers+text",
        color = ~Classe,
        colors = cores,
        text = ~SampleID,
        textposition = "top center",
        textfont = list(color = "lightgray", size = 10),
        marker = list(size = 5),
        hoverinfo = "text",
        hovertext = ~paste0(
          "<b>Sample:</b> ", SampleID,
          "<br><b>Class:</b> ", Classe,
          "<br><b>Score Distance (SD):</b> ", round(SD, 3),
          "<br><b>Orthogonal Distance (OD):</b> ", round(OD, 3)
        )
      ) %>%
        # Linha vertical de corte SD
        add_segments(
          x = sd_cutoff, xend = sd_cutoff,
          y = 0, yend = ymax,
          line = list(color = "red", width = 1, dash = "dash"),
          showlegend = FALSE,
          inherit = FALSE
        ) %>%
        # Linha horizontal de corte OD
        add_segments(
          x = 0, xend = xmax,
          y = od_cutoff, yend = od_cutoff,
          line = list(color = "red", width = 1, dash = "dash"),
          showlegend = FALSE,
          inherit = FALSE
        ) %>%
        layout(
          title = list(
            text = paste0("PCA Distances (ncomp = ", input$numPCoutipcaCustom, ")"),
            font = list(size = 18)
          ),
          xaxis = list(title = "Score Distance", range = c(0, xmax)),
          yaxis = list(title = "Orthogonal Distance", range = c(0, ymax)),
          legend = list(title = list(text = " "), x = 1.05, y = 1),
          margin = list(r = 140)
        )
    })
    
    output$outliersHelpTextUI <- renderUI({
      HTML(
        "<div style='font-size:15px; line-height:1.5;'>
    <p><strong>Graph interpretation:</strong></p>
    <p>This chart shows the position of each sample in relation to the <strong>Score Distance</strong> and the <strong>Orthogonal Distance</strong> in the PCA model.</p>
    <ul>
      <li><strong>Red vertical dashed line:</strong> upper acceptable limit for the Score Distance.</li>
      <li><strong>Red horizontal dashed line:</strong> upper acceptable limit for the Orthogonal Distance.</li>
    </ul>
    <p>Samples that are beyond these lines may be considered potential <strong>outliers</strong> and should be analyzed.</p>
  </div>"
      )
    })
    
  })
  
  
  
  ##ATUALIZADO EM 29/05/2025 COM ACRÉSCIMO DE SMOTE-NC EM OUTLIERS DA PCA
  ##ATUALIZADO EM 05/06/2025 COM ACRÉSCIMO DE BORDERLINE SMOTE EM OUTLIERS DA PCA
  ##ATUALIZADO EM 06/06/2025 COM ACRÉSCIMO DE SVM SMOTE EM OUTLIERS DA PCA
  ##ATUALIZADO EM 08/06/2025 COM ACRÉSCIMO DE ADASYN EM OUTLIERS DA PCA
  ##ATUALIZADO EM 09/06/2025 COM ACRÉSCIMO DE RANDOM UPSAMPLING EM OUTLIERS DA PCA
  ##ATUALIZADO EM 14/06/2025 COM ACRÉSCIMO DE TOMEK LINKS EM OUTLIERS DA PCA
  ##ATUALIZADO EM 17/06/2025 COM ACRÉSCIMO DE NEAR MISS EM OUTLIERS DA PCA
  ##ATUALIZADO EM 17/06/2025 COM ACRÉSCIMO DE ENN EM OUTLIERS DA PCA
  ##ATUALIZADO EM 18/06/2025 COM ACRÉSCIMO DE OSS EM OUTLIERS DA PCA
  ##ATUALIZADO EM 18/06/2025 COM ACRÉSCIMO DE RANDOM DOWNSAMPLING EM OUTLIERS DA PCA
  ##ATUALIZADO EM 02/07/2025 COM ACRÉSCIMO DE SMOTE TL EM OUTLIERS DA PCA  
  ##ATUALIZADO EM 05/07/2025 COM ACRÉSCIMO DE SMOTE ENN EM OUTLIERS DA PCA  
  ##ATUALIZADO EM 10/07/2025 COM ACRÉSCIMO DE SBC EM OUTLIERS DA PCA  
  ##ATUALIZADO EM 14/07/2025 COM ACRÉSCIMO DE SMOTE IPF EM OUTLIERS DA PCA  
  ##ATUALIZADO EM 14/07/2025 COM ACRÉSCIMO DE SPIDER EM OUTLIERS DA PCA  
  
  
  ########ACRÉSCIMO DOS OUTLIERS DA PCA EM VISUALIZAÇÕES EM 14/03/2025#####
  
  # Variance Tests - ACRÉSCIMO DE SMOTE_NC EM 30/05/2025
  
  # Variance Tests - ACRÉSCIMO DE BORDERLINE SMOTE 05/06/2025  
  
  # Variance Tests - ACRÉSCIMO DE SVM SMOTE 06/06/2025    
  
  # Variance Tests - ACRÉSCIMO DE ADASYN 08/06/2025  
  
  # Variance Tests - ACRÉSCIMO DE RANDOM UPSAMPLING 09/06/2025  
  
  # Variance Tests - ACRÉSCIMO DE TOMEK LINKS 15/06/2025   
  
  # Variance Tests - ACRÉSCIMO DE NEAR MISS 17/06/2025     
  
  # Variance Tests - ACRÉSCIMO DE ENN 17/06/2025
  
  # Variance Tests - ACRÉSCIMO DE OSS 18/06/2025  
  
  # Variance Tests - ACRÉSCIMO DE RANDOM DOWNSAMPLING 18/06/2025 
  
  # Variance Tests - ACRÉSCIMO DE SMOTE TL 02/07/2025 
  
  # Variance Tests - ACRÉSCIMO DE SMOTE ENN 05/07/2025 
  
  # Variance Tests - ACRÉSCIMO DE SBC 10/07/2025
  
  # Variance Tests - ACRÉSCIMO DE SMOTE IPF 14/07/2025 
  
  # Variance Tests - ACRÉSCIMO DE SPIDER 14/07/2025 
  
  observeEvent(input$run_variance_test, {
    original_data <- as.data.frame(filedata())
    smote_data <- NULL
    class_col <- NULL
    sint_type <- NULL
    
    if (isTRUE(runSMOTETL()) && !is.null(SMOTETL_Data()) && !is.null(input$target_tl)) {
      class_col <- input$target_tl
      
      smote_data_added <- SMOTETL_Data()$syn_data
      smote_data_removed <- SMOTETL_Data()$removed_data
      
      names(original_data)[names(original_data) == class_col] <- "class"
      
      if (!is.null(smote_data_added) && nrow(smote_data_added) > 0) {
        names(smote_data_added)[names(smote_data_added) == class_col] <- "class"
        smote_data_added$class <- paste(smote_data_added$class, "Synthetic")
        smote_data_added$Tipo <- "Synthetic"
      } else {
        smote_data_added <- original_data[0, , drop = FALSE]
      }
      
      if (!is.null(smote_data_removed) && nrow(smote_data_removed) > 0) {
        names(smote_data_removed)[names(smote_data_removed) == class_col] <- "class"
        smote_data_removed$class <- paste(smote_data_removed$class, "Removed")
        smote_data_removed$Tipo <- "Removed"
      } else {
        smote_data_removed <- original_data[0, , drop = FALSE]
      }
      
      feature_cols <- intersect(
        names(original_data),
        union(names(smote_data_added), names(smote_data_removed))
      )
      
      original_data <- original_data[, feature_cols, drop = FALSE]
      smote_data_added <- smote_data_added[, feature_cols, drop = FALSE]
      smote_data_removed <- smote_data_removed[, feature_cols, drop = FALSE]
      
      original_data$Tipo <- "Original"
      
      smote_data <- dplyr::bind_rows(smote_data_added, smote_data_removed)
      
    } 
    
    ##############BLOCO ADICIONADO EM 05/07/2025
  
    
    else if (isTRUE(runSMOTEENN()) && !is.null(SMOTEENN_Data()) && !is.null(input$target_enn2)) {
      class_col <- input$target_enn2
      
      smote_data_added <- SMOTEENN_Data()$syn_data
      smote_data_removed <- SMOTEENN_Data()$removed_data
      
      names(original_data)[names(original_data) == class_col] <- "class"
      
      if (!is.null(smote_data_added) && nrow(smote_data_added) > 0) {
        names(smote_data_added)[names(smote_data_added) == class_col] <- "class"
        smote_data_added$class <- paste(smote_data_added$class, "Synthetic")
        smote_data_added$Tipo <- "Synthetic"
      } else {
        smote_data_added <- original_data[0, , drop = FALSE]
      }
      
      if (!is.null(smote_data_removed) && nrow(smote_data_removed) > 0) {
        names(smote_data_removed)[names(smote_data_removed) == class_col] <- "class"
        smote_data_removed$class <- paste(smote_data_removed$class, "Removed")
        smote_data_removed$Tipo <- "Removed"
      } else {
        smote_data_removed <- original_data[0, , drop = FALSE]
      }
      
      feature_cols <- intersect(
        names(original_data),
        union(names(smote_data_added), names(smote_data_removed))
      )
      
      original_data <- original_data[, feature_cols, drop = FALSE]
      smote_data_added <- smote_data_added[, feature_cols, drop = FALSE]
      smote_data_removed <- smote_data_removed[, feature_cols, drop = FALSE]
      
      original_data$Tipo <- "Original"
      
      smote_data <- dplyr::bind_rows(smote_data_added, smote_data_removed)
      
    }
    
    
    ##############BLOCO ADICIONADO EM 05/07/2025
 

    ##############BLOCO ADICIONADO EM 14/07/2025
    
    else if (isTRUE(runSMOTEIPF()) && !is.null(SMOTEIPF_Data()) && !is.null(input$target_ipf)) {
      class_col <- input$target_ipf
      
      smote_data_added <- SMOTEIPF_Data()$syn_data
      smote_data_removed <- SMOTEIPF_Data()$removed_data
      
      names(original_data)[names(original_data) == class_col] <- "class"
      
      if (!is.null(smote_data_added) && nrow(smote_data_added) > 0) {
        names(smote_data_added)[names(smote_data_added) == class_col] <- "class"
        smote_data_added$class <- paste(smote_data_added$class, "Synthetic")
        smote_data_added$Tipo <- "Synthetic"
      } else {
        smote_data_added <- original_data[0, , drop = FALSE]
      }
      
      if (!is.null(smote_data_removed) && nrow(smote_data_removed) > 0) {
        names(smote_data_removed)[names(smote_data_removed) == class_col] <- "class"
        smote_data_removed$class <- paste(smote_data_removed$class, "Removed")
        smote_data_removed$Tipo <- "Removed"
      } else {
        smote_data_removed <- original_data[0, , drop = FALSE]
      }
      
      feature_cols <- intersect(
        names(original_data),
        union(names(smote_data_added), names(smote_data_removed))
      )
      
      original_data <- original_data[, feature_cols, drop = FALSE]
      smote_data_added <- smote_data_added[, feature_cols, drop = FALSE]
      smote_data_removed <- smote_data_removed[, feature_cols, drop = FALSE]
      
      original_data$Tipo <- "Original"
      
      smote_data <- dplyr::bind_rows(smote_data_added, smote_data_removed)
      
    }
    
    ##############BLOCO ADICIONADO EM 14/07/2025
    
    
    ##############BLOCO ADICIONADO EM 14/07/2025
    
    else if (isTRUE(runSPIDER()) && !is.null(SPIDER_Data()) && !is.null(input$target_spider)) {
      class_col <- input$target_spider
      
      smote_data_added <- SPIDER_Data()$syn_data
      smote_data_removed <- SPIDER_Data()$removed_data
      
      names(original_data)[names(original_data) == class_col] <- "class"
      
      if (!is.null(smote_data_added) && nrow(smote_data_added) > 0) {
        names(smote_data_added)[names(smote_data_added) == class_col] <- "class"
        smote_data_added$class <- paste(smote_data_added$class, "Synthetic")
        smote_data_added$Tipo <- "Synthetic"
      } else {
        smote_data_added <- original_data[0, , drop = FALSE]
      }
      
      if (!is.null(smote_data_removed) && nrow(smote_data_removed) > 0) {
        names(smote_data_removed)[names(smote_data_removed) == class_col] <- "class"
        smote_data_removed$class <- paste(smote_data_removed$class, "Removed")
        smote_data_removed$Tipo <- "Removed"
      } else {
        smote_data_removed <- original_data[0, , drop = FALSE]
      }
      
      feature_cols <- intersect(
        names(original_data),
        union(names(smote_data_added), names(smote_data_removed))
      )
      
      original_data <- original_data[, feature_cols, drop = FALSE]
      smote_data_added <- smote_data_added[, feature_cols, drop = FALSE]
      smote_data_removed <- smote_data_removed[, feature_cols, drop = FALSE]
      
      original_data$Tipo <- "Original"
      
      smote_data <- dplyr::bind_rows(smote_data_added, smote_data_removed)
    }
    
    ##############BLOCO ADICIONADO EM 14/07/2025
    
    
    else if (isTRUE(runSMOTENC()) && !is.null(SMOTENC_Data()) && !is.null(input$target_nc)) {
      smote_data <- SMOTENC_Data()$syn_data
      class_col <- input$target_nc
      sint_type <- "Synthetic"
      
    } else if (isTRUE(runSMOTE()) && !is.null(SMOTE_Data()) && !is.null(input$target)) {
      smote_data <- SMOTE_Data()$syn_data
      class_col <- input$target
      sint_type <- "Synthetic"
      
    } else if (isTRUE(runBorderlineSMOTE()) && !is.null(BorderlineSMOTE_Data()) && !is.null(input$target_borderline)) {
      smote_data <- BorderlineSMOTE_Data()$syn_data
      class_col <- input$target_borderline  
      sint_type <- "Synthetic"
      
    } else if (isTRUE(runSVM_SMOTE()) && !is.null(SVM_SMOTE_Data()) && !is.null(input$target_svm)) {
      smote_data <- SVM_SMOTE_Data()$syn_data
      class_col <- input$target_svm  
      sint_type <- "Synthetic"
      
    } else if (isTRUE(runADASYN()) && !is.null(ADASYN_Data()) && !is.null(input$target_adasyn)) {
      smote_data <- ADASYN_Data()$syn_data
      class_col <- input$target_adasyn   
      sint_type <- "Synthetic"
      
    } else if (isTRUE(runRU()) && !is.null(RU_Data()) && !is.null(input$target_ru)) {
      smote_data <- RU_Data()$syn_data
      class_col <- input$target_ru   
      sint_type <- "Synthetic"
      
    } else if (isTRUE(runTomek()) && !is.null(Tomek_Data()) && !is.null(input$target_tomek)) {
      smote_data <- Tomek_Data()$removed_data
      class_col <- input$target_tomek
      sint_type <- "Removed"
      
    } else if (isTRUE(runNearMiss()) && !is.null(NearMiss_Data()) && !is.null(input$target_nearmiss)) {
      smote_data <- NearMiss_Data()$removed_data
      class_col <- input$target_nearmiss
      sint_type <- "Removed"
      
    } else if (isTRUE(runENN()) && !is.null(ENN_Data()) && !is.null(input$target_enn)) {
      smote_data <- ENN_Data()$removed_data
      class_col <- input$target_enn   
      sint_type <- "Removed"
      
    } else if (isTRUE(runOSS()) && !is.null(OSS_Data()) && !is.null(input$target_oss)) {
      smote_data <- OSS_Data()$removed_data
      class_col <- input$target_oss 
      sint_type <- "Removed"
      
    } else if (isTRUE(runRD()) && !is.null(RD_Data()) && !is.null(input$target_rd)) {
      smote_data <- RD_Data()$removed_data
      class_col <- input$target_rd
      sint_type <- "Removed"
      
      
      ###BLOCO ADICIONADO EM 10/07/2025
      
    } else if (isTRUE(runSBC()) && !is.null(SBC_Data()) && !is.null(input$target_sbc)) {
      smote_data <- SBC_Data()$removed_data
      class_col <- input$target_sbc
      sint_type <- "Removed"
      
      
      ###BLOCO ADICIONADO EM 10/07/2025
      
      
      
    } else {
      showNotification("Perform some sampling method before running the test.", type = "error")
      return(NULL)
    }
    
  ##  if (is.null(smote_data) || !(class_col %in% names(original_data))) {
  ##    showNotification("Coluna de classe não encontrada nos dados originais ou sintéticos.", type = "error")
  ##    return(NULL)
  ##  }
    
 ##   if (is.null(smote_data) || !("class" %in% names(original_data)) || !("class" %in% names(smote_data))) {
##      showNotification("Coluna de classe não encontrada nos dados originais ou sintéticos.", type = "error")
##      return(NULL)
##    }
  
    
  
    # Renomeaando data original
    
    names(original_data)[names(original_data) == class_col] <- "class"
    
    # Se não for SMOTE-TL: renomeando smote_data
    
    if (!isTRUE(runSMOTETL())) {
      names(smote_data)[names(smote_data) == class_col] <- "class"
    }
    
    ###BLOCO ADICIONADO EM 05/07/2025
    
    # Se não for SMOTE-ENN: renomeando smote_data
    
    if (!isTRUE(runSMOTEENN())) {
      names(smote_data)[names(smote_data) == class_col] <- "class"
    } 
    
    
    ##BLOCO ADICIONADO EM 05/07/2025
    
 
    ###BLOCO ADICIONADO EM 14/07/2025
    
    # Se não for SMOTE-IPF: renomeando smote_data
    
    if (!isTRUE(runSMOTEIPF())) {
      names(smote_data)[names(smote_data) == class_col] <- "class"
    } 
    
    ##BLOCO ADICIONADO EM 14/07/2025
    
    
    ###BLOCO ADICIONADO EM 14/07/2025
    
    # Se não for SPIDER: renomeando smote_data
    
    if (!isTRUE(runSPIDER())) {
      names(smote_data)[names(smote_data) == class_col] <- "class"
    } 
    
    ##BLOCO ADICIONADO EM 14/07/2025
    
    
    
    # Checagem final
    
    if (nrow(smote_data) == 0 || !"class" %in% names(smote_data)) {
      showNotification("No synthetic/pruned samples generated or missing class column.", type = "error")
      return(NULL)
    }
    
    
    
   ## names(original_data)[names(original_data) == class_col] <- "class"
  ##  if (!isTRUE(runSMOTETL())) names(smote_data)[names(smote_data) == class_col] <- "class"
    
    
    
    
    #if (isTRUE(runSMOTETL())) {
    
    ## if (isTRUE(runSMOTETL()) || isTRUE(runSMOTEENN()) ) { #MODIFICADO EM 05/07/2025
      
  ##  if (isTRUE(runSMOTETL()) || isTRUE(runSMOTEENN()) || isTRUE(runSMOTEIPF())  ) { #MODIFICADO EM 14/07/2025
    
    if (isTRUE(runSMOTETL()) || isTRUE(runSMOTEENN()) || isTRUE(runSMOTEIPF()) || isTRUE(runSPIDER())  ) { #MODIFICADO EM 14/07/2025
    
    } else {
      smote_data$class <- paste(smote_data$class, sint_type)
      smote_data$fake_cat <- NULL
    }
    
    feature_cols <- intersect(names(original_data), names(smote_data))
    original_data <- original_data[, feature_cols, drop = FALSE]
    smote_data <- smote_data[, feature_cols, drop = FALSE]
    
    original_data$Tipo <- "Original"
    
    #if (isTRUE(runSMOTETL())) {
    
    ##if (isTRUE(runSMOTETL()) || isTRUE(runSMOTEENN()) ) { #MODIFICADO EM 05/07/2025
      
    
    ## if (isTRUE(runSMOTETL()) || isTRUE(runSMOTEENN()) || isTRUE(runSMOTEIPF()) ) { #MODIFICADO EM 14/07/2025
    
    if (isTRUE(runSMOTETL()) || isTRUE(runSMOTEENN()) || isTRUE(runSMOTEIPF()) || isTRUE(runSPIDER()) ) { #MODIFICADO EM 14/07/2025
    
    } else {
      smote_data$Tipo <- sint_type
    }
    
    dataset <- dplyr::bind_rows(original_data, smote_data)
    validate(need("class" %in% colnames(dataset), "A coluna 'class' não está presente nos dados!"))
    dataset$class <- as.factor(dataset$class)
    
    original_dataset <- dataset[!grepl("Synthetic|Removed", dataset$class), ]
    synthetic_dataset <- dataset[grepl("Synthetic|Removed", dataset$class), ]
    
    if (nrow(synthetic_dataset) > 0) {
      synthetic_dataset$OriginalClass <- gsub(" Removed| Synthetic", "", synthetic_dataset$class)
    }
    
    numeric_cols <- names(Filter(is.numeric, dataset))
    numeric_cols <- setdiff(numeric_cols, c("SampleID"))
    
    if (length(numeric_cols) == 0) {
      showNotification("No numeric variables available for the test.", type = "error")
      return(NULL)
    }
    
    results <- lapply(numeric_cols, function(col) {
      original <- original_dataset[[col]]
      synthetic <- synthetic_dataset[[col]]
      
      if (length(original) == 0 || length(synthetic) == 0) {
        return(list(variable = col, test = NA, p_value = NA, message = "Dados insuficientes"))
      }
      
      if (input$variance_test == "Levene's Test") {
        test <- car::leveneTest(value ~ group, data = data.frame(
          value = c(original, synthetic),
          group = rep(c("Original", "Synthetic"), times = c(length(original), length(synthetic)))
        ))
        return(list(variable = col, test = "Levene's Test", p_value = test$`Pr(>F)`[1], message = NULL))
        
      } else if (input$variance_test == "Fligner-Killeen Test") {
        test <- fligner.test(list(original, synthetic))
        return(list(variable = col, test = "Fligner-Killeen Test", p_value = test$p.value, message = NULL))
        
      } else {
        return(list(variable = col, test = "Desconhecido", p_value = NA, message = "Teste inválido"))
      }
    })
    
    output$variance_test_results <- renderPrint({
      results_df <- do.call(rbind, lapply(results, function(res) {
        data.frame(
          Variable = res$variable,
          Test = res$test,
          P_Value = round(res$p_value, 4),
          Message = ifelse(is.null(res$message), "Success", res$message)
        )
      }))
      results_df
    })
  })
}


# Internal Functions


#---Scores
PlotScoresGeneral<-function(scores, PC1, PC2, confidenceinterval, sampleclass, id, SpecialLabel, LabelABV){
  
  t <- list(
    family = "times",
    size = 9.5,
    color = toRGB("grey50"))
  
  
  ellipse <- ellipse::ellipse(cov(x=scores[,PC1], y=scores[,PC2]
  ),
  scale = c(sd(scores[,PC1]),
            sd(scores[,PC2])
  ),
  level = confidenceinterval,
  centre = c(mean(scores[,PC1]),
             mean(scores[,PC2]))
  )
  
  
  
  
  {plot_ly(type="scatter", 
           x=scores[,PC1],
           y=scores[,PC2],
           mode="markers",
           showlegend=T,
           name = "Markers",
           #text=as.character(t(id)),
           text=as.character(id()),
           hoverinfo="skip")%>%
      add_text(textposition="top", textfont=t, name = "Labels", showlegend=T)%>%
      layout(title="<b>Scores</b>",
             xaxis=list(title=paste0(SpecialLabel,PC1),zerolinecolor="lightgrey"),
             yaxis=list(title=paste0(SpecialLabel,PC2),zerolinecolor="lightgrey")
      )%>%
      
      add_trace(x=ellipse[,1], 
                y=ellipse[,2],
                type="scatter",
                mode="lines",
                colors="black",
                text = "",
                hoverinfo = "skip",
                line = list(color="red", width=.8),
                inherit = F,
                showlegend = T,
                name = "Ellipse"
      )
  }
}

#---Scores with classes
PlotScoresClass<-function(scores, PC1, PC2, confidenceinterval, sampleclass, id, SpecialLabel, LabelABV){
  
  cov<-cov(x=data.frame(scores))
  ellipseclass<-list()
  convexhullpos<-list()
  con.hull<-list()
  
  
  for (i in 1:length(unique(sampleclass))) {
    
    pos<-which(sampleclass==unique(sampleclass)[[i]])
    ellipseclass[[i]]<-ellipse::ellipse(cov(x=scores[pos, c(PC1,PC2)]),
                                        level = confidenceinterval,
                                        centre = c(mean(scores[pos,PC1]),
                                                   mean(scores[pos,PC2])
                                        ),
    )
    
  }
  
  t <- list(
    family = "times",
    size = 12,
    color = toRGB("grey50"))
  
  Plot<-plot_ly(type="scatter", 
                x=scores[,PC1],
                y=scores[,PC2],
                mode="markers",
                showlegend=T,
                color = factor(sampleclass),
                symbol = factor(sampleclass),
                colors = "Set1",
                #text=as.character(t(id)),
                text=as.character(id()),
                hoverinfo="")%>%
    
    add_text(textposition="top", 
             textfont=t, 
             showlegend=T, 
             inherit = F, 
             x=scores[,PC1], 
             y=scores[,PC2], 
             #text=as.character(t(id)),
             text=as.character(id()),
             color = factor(sampleclass))%>%
    
    layout(title="<b>Scores</b>",
           xaxis=list(title=paste0(SpecialLabel,PC1),zerolinecolor="lightgrey"),
           yaxis=list(title=paste0(SpecialLabel,PC2),zerolinecolor="lightgrey")
    )
  
  for (i in 1:length(unique(sampleclass))) {
    
    Plot<-add_trace(Plot,
                    x=ellipseclass[[i]][,1], 
                    y=ellipseclass[[i]][,2],
                    type="scatter",
                    mode="lines",
                    colors="Set1",
                    color = factor(unique(sampleclass))[[i]],
                    hoverinfo = "skip",
                    #text = unique(sampleclass)[[i]],
                    line = list(color=factor(unique(sampleclass))[[i]], width=.8, colors = "Set1"),
                    name=paste0("HoT Ellipse ",unique(sampleclass)[[i]]),
                    inherit = F
    )
    
    
  }
  
  
  for (i in 1:length(unique(sampleclass))) {
    
    pos<-which(sampleclass==unique(sampleclass)[[i]]) 
    
    convexhullpos[[i]]<-pos[chull(scores[pos,PC1], scores[pos,PC2])]
    
    con.hull[[i]]<-rbind(scores[convexhullpos[[i]],],scores[convexhullpos[[i]],]) # get coordinates for convex hull
  }
  
  
  
  for (i in 1:length(unique(sampleclass))) {
    Plot<-add_trace(Plot,
                    x=con.hull[[i]][,PC1],
                    y=con.hull[[i]][,PC2],
                    type="scatter",
                    mode="lines+markers",
                    inherit = F,
                    color = factor(unique(sampleclass))[[i]],
                    line = list(color=factor(unique(sampleclass))[[i]], width=.8, colors = "Set1"),
                    name=paste0("Convex Hull ",unique(sampleclass)[[i]]),
                    visible = "legendonly"
    )
    
    
  }
  
  Plot}
#---Plot Scores 3D

Plot3DscoresGeneral<-function(scores, PC1, PC2, PC3, confidenceinterval, sampleclass, id, SpecialLabel, LabelABV){
  
  cov<-cov(x=data.frame(scores))
  
  ellipse1<- ellipse::ellipse(cov(x=scores), scale = c(sd(scores[,PC1]), sd(scores[,PC2])),level = confidenceinterval
  )
  ellipse2<- ellipse::ellipse(cov(x=scores), scale = c(sd(scores[,PC1]), sd(scores[,PC3])),level = confidenceinterval
  )
  ellipse3<- ellipse::ellipse(cov(x=scores), scale = c(sd(scores[,PC2]), sd(scores[,PC3])),level = confidenceinterval
  )
  
  
  t <- list(
    family = "times",
    size = 12,
    color = toRGB("grey50"))
  
  
  Plot<-plot_ly(type="scatter3d", 
                x=scores[,PC1],
                y=scores[,PC2],
                z=scores[,PC3],
                mode="markers",
                showlegend=T,
                #color = factor(sampleclass),
                colors = "Set1",
                name = "Markers",
                #text=as.character(t(id)),
                text=as.character(id()),
                marker = list(size=4, showlegend=T),
                hoverinfo="")
  
  Plot<-add_text(Plot,
                 textposition="top", textfont=t, showlegend=T, mode="text", inherit = F,
                 x=scores[,PC1],
                 y=scores[,PC2],
                 z=scores[,PC3],
                 #text=as.character(t(id)),
                 text=as.character(id()),
                 #color = factor(sampleclass),
                 name = "Labels",
                 colors = "Set1"
  )
  
  Plot<-layout(Plot,
               title="<b>Scores</b>",
               xaxis=list(title=paste0(SpecialLabel,PC1),zerolinecolor="lightgrey"),
               yaxis=list(title=paste0(SpecialLabel,PC2),zerolinecolor="lightgrey"),
               yaxis=list(title=paste0(SpecialLabel,PC3),zerolinecolor="lightgrey")
  )
  
  Plot<-add_trace(Plot,
                  x=ellipse1[,1],
                  y=ellipse1[,2],
                  z=0,
                  type="scatter3d",
                  mode="lines",
                  colors="black",
                  name=paste0(LabelABV,PC1),
                  text = "",
                  hoverinfo = "skip",
                  line = list(color="red", width=1.5),
                  inherit = F
  )
  Plot<-add_trace(Plot,
                  x=ellipse2[,1],
                  y=0,
                  z=ellipse2[,2],
                  type="scatter3d",
                  mode="lines",
                  colors="black",
                  name=paste0(LabelABV,PC2),
                  hoverinfo = "skip",
                  line = list(color="red", width=1.5),
                  inherit = F
  )
  
  Plot<-add_trace(Plot,
                  x=0,
                  y=ellipse3[,1],
                  z=ellipse3[,2],
                  type="scatter3d",
                  mode="lines",
                  colors="black",
                  name=paste0(LabelABV,PC3),
                  text = "",
                  hoverinfo = "skip",
                  line = list(color="red", width=1.5),
                  inherit = F
  )
  
  
  Plot
}

#---Plot Scores 3D Class

Plot3DscoresClasses<-function(scores, PC1, PC2, PC3, confidenceinterval, sampleclass, id, SpecialLabel, LabelABV){
  
  cov<-cov(x=data.frame(scores))
  
  t <- list(
    family = "times",
    size = 12,
    color = toRGB("grey50"))
  
  ellipseclass1<- c()
  ellipseclass2<- c()
  ellipseclass3<- c()
  
  
  for (i in 1:length(unique(sampleclass))) {
    
    pos<-which(sampleclass==unique(sampleclass)[[i]])
    
    ellipseclass1[[i]]<-ellipse::ellipse(
      cov(x=scores[pos, c(PC1,PC2)]),
      level = confidenceinterval,
      centre = c(mean(scores[pos,PC1]),
                 mean(scores[pos,PC2])
      ),
    )
    
    ellipseclass2[[i]]<-ellipse::ellipse(
      cov(x=scores[pos, c(PC1,PC3)]),
      level = confidenceinterval,
      centre = c(mean(scores[pos,PC1]),
                 mean(scores[pos,PC3])
      ),
    )
    
    
    ellipseclass3[[i]]<-ellipse::ellipse(
      cov(x=scores[pos, c(PC2,PC3)]),
      level = confidenceinterval,
      centre = c(mean(scores[pos,PC2]),
                 mean(scores[pos,PC3])
      ),
    )
    
  }
  
  
  Plot<-plot_ly(type="scatter3d", 
                x=scores[,PC1],
                y=scores[,PC2],
                z=scores[,PC3],
                mode="markers",
                showlegend=T,
                color = factor(sampleclass),
                colors = "Set1",
                #text=as.character(t(id)),
                text=as.character(id()),
                marker = list(size=4, showlegend=T),
                hoverinfo="")
  
  Plot<-add_text(Plot,
                 textposition="top", textfont=t, showlegend=T, mode="text", inherit = F,
                 x=scores[,PC1],
                 y=scores[,PC2],
                 z=scores[,PC3],
                 #text=as.character(t(id)),
                 text=as.character(id()),
                 color = factor(sampleclass),
                 colors = "Set1"
  )
  
  Plot<-layout(Plot,
               title="<b>Scores</b>",
               xaxis=list(title=paste0(SpecialLabel,PC1),zerolinecolor="lightgrey"),
               yaxis=list(title=paste0(SpecialLabel,PC2),zerolinecolor="lightgrey"),
               yaxis=list(title=paste0(SpecialLabel,PC3),zerolinecolor="lightgrey")
  )
  
  
  for (i in 1:length(unique(sampleclass))) {
    
    pos<-which(sampleclass==unique(sampleclass)[[i]])
    
    Plot<-add_trace(Plot,
                    x=ellipseclass1[[i]][,1],
                    y=ellipseclass1[[i]][,2],
                    z=mean(scores[pos,PC3]),
                    type="scatter3d",
                    mode="lines",
                    colors="Set1",
                    color = factor(unique(sampleclass))[[i]],
                    name=paste0(unique(sampleclass)[[i]],LabelABV,PC1),
                    text = "",
                    hoverinfo = "skip",
                    line = list(color=factor(unique(sampleclass)[[i]]), width=1.5),
                    inherit = F
    )
    Plot<-add_trace(Plot,
                    x=ellipseclass2[[i]][,1],
                    y=mean(scores[pos,PC2]),
                    z=ellipseclass2[[i]][,2],
                    type="scatter3d",
                    mode="lines",
                    colors="Set1",
                    color = factor(unique(sampleclass))[[i]],
                    name=paste0(unique(sampleclass)[[i]],LabelABV,PC2),
                    hoverinfo = "skip",
                    line = list(color=factor(unique(sampleclass)[[i]]), width=1.5),
                    inherit = F
    )
    # 
    Plot<-add_trace(Plot,
                    x=mean(scores[pos,PC1]),
                    y=ellipseclass3[[i]][,1],
                    z=ellipseclass3[[i]][,2],
                    type="scatter3d",
                    mode="lines",
                    colors="Set1",
                    color = factor(unique(sampleclass))[[i]],
                    name=paste0(unique(sampleclass)[[i]],LabelABV,PC3),
                    text = "",
                    hoverinfo = "skip",
                    line = list(color=factor(unique(sampleclass)[[i]]), width=1.5),
                    inherit = F
    )
    
    
  }
  
  Plot
  
}


#---Biplot
PlotBIPLOT<-function(scores, loadings, PC1, PC2, id, variables, SpecialLabel, LabelABV){
  
  full<-rbind((scores), (loadings))
  names<-c(t(id), t(variables))
  
  ScoresLAB<-rep("Scores", nrow(scores))
  LoadLAB<-rep("Loadings", nrow(loadings))
  LABs<-t(c(ScoresLAB, LoadLAB))
  
  
  t <- list(
    family = "times",
    size = 12,
    color = toRGB("grey50"))
  
  plot<-plot_ly(
    
    type="scatter", 
    x=full[,PC1],
    y=full[,PC2],
    mode="markers",
    showlegend=T,
    color = factor(LABs),
    colors = "Set1",
    text=as.character(names),
    hoverinfo="")%>%
    
    add_text(textposition="top", textfont=t, showlegend=T, mode="text", inherit = T)%>%
    
    layout(title="<b>Biplot</b>",
           xaxis=list(title=paste0(SpecialLabel,PC1),zerolinecolor="lightgrey"),
           yaxis=list(title=paste0(SpecialLabel,PC2),zerolinecolor="lightgrey"))
  
  plot<-add_segments(plot,
                     x=0,
                     y=0,
                     xend=full[((nrow(scores)+1):nrow(full)),PC1],
                     yend=full[((nrow(scores)+1):nrow(full)),PC2],
                     inherit = F,
                     line = list(color="red", width=1.5),
                     name="Loadings Lines"
  )
  
  
  plot
}


#----Loadings PC1 x PC2


PlotSpectralLoadings<-function(loadings, PC1, PC2, variables, data2, SpecialLabels){
  
  t <- list(
    family = "times",
    size = 12,
    color = toRGB("grey50"))
  
  plot_ly(type="scatter", 
          x=loadings[,PC1],
          y=loadings[,PC2],
          mode="lines",
          showlegend=F,
          text="",
          hoverinfo="x , y")%>%
    add_text(textposition="top", textfont=t)%>%
    layout(title="<b>Loadings</b>",
           xaxis=list(title=paste0(SpecialLabels,PC1),zerolinecolor="lightgrey"),
           yaxis=list(title=paste0(SpecialLabels,PC2),zerolinecolor="lightgrey"))
} 


PlotNormalLoadings<-function(loadings, PC1, PC2, variables, data2, SpecialLabels){
  
  t <- list(
    family = "times",
    size = 12,
    color = toRGB("grey50"))
  
  plot_ly(type="scatter", 
          x=loadings[,PC1],
          y=loadings[,PC2],
          mode="markers",
          showlegend=F,
          text = data2,
          hoverinfo="skip")%>%
    add_text(textposition="top", textfont=t)%>%
    layout(title="<b>Loadings</b>",
           xaxis=list(title=paste0(SpecialLabels,PC1),zerolinecolor="lightgrey"),
           yaxis=list(title=paste0(SpecialLabels,PC2),zerolinecolor="lightgrey"))
  
  
}

PlotSpectralNLoadings<-function(loadings, variables, data2, SpecialLabels){
  
  t <- list(
    family = "times",
    size = 12,
    color = toRGB("grey50"))
  
  Plot <-  plot_ly(type="scatter",
                   x=1:nrow(loadings),
                   y=loadings[,1],
                   mode="lines",
                   showlegend=T,
                   #text=data2,
                   text="",
                   color = colnames(loadings)[[1]],
                   hoverinfo="x , y")%>%
    #add_text(textposition="topright", textfont=t)%>%
    layout(title="<b>Loadings</b>",
           xaxis=list(title=paste0("Loadings"),zerolinecolor="lightgrey"),
           yaxis=list(title=paste0("Variables"),zerolinecolor="lightgrey"))
  
  
  for (i in 2:ncol(loadings)) {
    
    Plot<-add_trace(Plot,
                    x=1:nrow(loadings),
                    y=loadings[,i],
                    mode="lines",
                    showlegend=T,
                    #text=data2,
                    text="",
                    color = colnames(loadings)[[i]],
                    hoverinfo="x , y")%>%
      #add_text(textposition="topright", textfont=t)%>%
      layout(title="<b>Loadings</b>",
             xaxis=list(title=paste0("Loadings"),zerolinecolor="lightgrey"),
             yaxis=list(title=paste0("Variables"),zerolinecolor="lightgrey"))
    
    
  }
  
  
  Plot
}

PlotNormalNLoadings<-function(loadings, variables, data2, SpecialLabels){
  
  t <- list(
    family = "times",
    size = 12,
    color = toRGB("grey50"))
  
  Plot <-  plot_ly(type="scatter",
                   x=data2,
                   y=loadings[,1],
                   mode="markers+lines",
                   showlegend=T,
                   #text=data2,
                   text="",
                   color = colnames(loadings)[[1]],
                   hoverinfo="text, y")%>%
    # add_text(textposition="topright", textfont=t)%>%
    layout(title="<b>Loadings</b>",
           xaxis=list(title=paste0("Loadings"),zerolinecolor="lightgrey", categoryorder="array" ,categoryarray=data2),
           yaxis=list(title=paste0("Variables"),zerolinecolor="lightgrey"))
  
  
  for (i in 2:ncol(loadings)) {
    
    Plot<-add_trace(Plot,
                    x=data2,
                    y=loadings[,i],
                    mode="markers+lines",
                    showlegend=T,
                    #text=data2,
                    text="",
                    color = colnames(loadings)[[i]],
                    hoverinfo="text, y")%>%
      #add_text(textposition="topright", textfont=t)%>%
      layout(title="<b>Loadings</b>",
             xaxis=list(title=paste0("Loadings"),zerolinecolor="lightgrey", categoryorder="array" ,categoryarray=data2),
             yaxis=list(title=paste0("Variables"),zerolinecolor="lightgrey"))
    
    
  }
  
  
  Plot
  
  
}



shinyApp(ui = ui, server = server)