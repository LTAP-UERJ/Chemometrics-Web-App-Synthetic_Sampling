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
library(vegan) #ACRÉSCIMO PARA ANÁLISE DE PROCRUSTES GENERALIZADO
library(MVN) #ACRÉSCIMO em 30/03/2025 PARA teste de normalidade multivariada: teste de Henze-Zirkler.
library(dplyr) #ACRÉSCIMO EM 04/04/2025 PARA MÉDIAS DO PLOT COMPARATIVO


##PACOTES PARA CRIAÇÃO DO EXECUTÁVEL EM 10/04/2025

#library(RInno)
library(remotes)
library(devtools)





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
            text = "TOMEK Links",
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
            icon = icon("menu-right", lib = "glyphicon"))
        ),
        menuItem(
          tabName = "hybrid",
          text = "Hybrid methods",
          icon = icon("retweet", lib = "glyphicon"),
          menuSubItem(
            text = "SMOTE-TL",
            tabName = "smote-tl",
            icon = icon("menu-right", lib = "glyphicon")),
          menuSubItem(
            text = "SMOTE-ENN",
            tabName = "smote-enn",
            icon = icon("menu-right", lib = "glyphicon")),
          
          menuSubItem(
            text = "SMOTE-IPF",
            tabName = "smote-ipf",
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
      
      #ACRÉSCIMO EM 17/01/2025 PROJEÇÃO DE DADOS EM DIMENSÃO REDUZIDA(PCA ROBUSTA E T-SNE)
      
      ##     menuSubItem(tabName = "robpcatab", text = "Robust PCA", icon = icon("signal", lib = "glyphicon")),
      ##   menuSubItem(tabName = "tsnetab", text = "t-SNE", icon = icon("signal", lib = "glyphicon"))),
      
      
      
      
      
      ####ACRÉSCIMO EM 17/01/2025 DE "VISUALIZAÇÕES" E "RESULTADOS"  
      
      ##MODIFICAÇÃO DA GUIA "VISUALIZAÇÕES" PARA "DIAGNÓSTICO" em 27/03/2025
      
      # menuItem("Visualizações", tabName = "visual", icon = icon("chart-bar")),
      menuItem("Diagnóstico", tabName = "visual", icon = icon("chart-bar")),
      
      
      menuItem("Resultados", tabName = "results", icon = icon("table")),
      
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
              p("Acesse o site do LTAP Group clicando no link abaixo:"),
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
      
      #ALTERAÇÃO DO NOME DO BOX de "Dados Originais" para "Amostras originais"
      #ALTERAÇÃO DO NOME DO BOX de "Dados Combinados" para "Amostras combinadas"
      
      tabItem(
        tabName = "results",
        fluidRow(
          box(
            title = "Amostras Originais", width = 6, status = "warning", solidHeader = TRUE,
            DTOutput("original_data")
          ),
          
          ###########TRECHO OMITIDO EM 07/05/2025
          
          #   box(
          #    title = "Amostras Sintéticas", width = 6, status = "warning", solidHeader = TRUE,
          #   DTOutput("synthetic_only_data")
          
          #  ),
          
          ###########TRECHO OMITIDO EM 07/05/2025
          
          box(
            title = "Amostras Combinadas", width = 6, status = "warning", solidHeader = TRUE,
            DTOutput("synthetic_data")
            
          ),
          #  ),
          
          #FUNCIONALIDADE DE EXPORTAÇÃO DE RESULTADOS EM 15/02/2025
          
          #MODIFICAÇÃO DE "EXPORTAÇÃO DE DADOS" PARA DENTRO DO FLUIDROW EM 07/03/2025 E DO TAMANHO DA CAIXA
          
          ## box(title = "Exportação de Dados", status = "info", solidHeader = TRUE, width = 12,
          box(title = "Exportação de Dados", status = "info", solidHeader = TRUE, width = 6,
              selectInput("dataset_choice", "Escolha o conjunto de dados:", 
                          
                          ###########MODIFICADO EM 07/05/2025#####################
                          #choices = c("Amostras Originais" = "original", "Amostras Sintéticas" = "synthetic", "Amostras Combinadas" = "combined")),
                          choices = c("Amostras Originais" = "original", "Amostras Combinadas" = "combined")),
              ###########MODIFICADO EM 07/05/2025#####################
              
              selectInput("export_format", "Escolha o formato:", choices = c(".csv" = "csv", ".xls" = "xlsx")),
              downloadButton("download_selected", "Exportar Dados")
          )
        )
      ),
      #TRECHO MOVIDO PARA GUIA DE VISUALIZAÇÕES EM 07/03/2025 
      #    fluidRow(
      #     box(
      #      title = "Testes de Variância", width = 12, status = "primary", solidHeader = TRUE,
      #     selectInput("variance_test", "Escolha o Teste de Variância:",
      #                #choices = c("Levene's Test", "Bartlett's Test", "Fligner-Killeen Test", "F-Test"),
      #               choices = c("Levene's Test", "Fligner-Killeen Test"),
      #              selected = "Levene's Test"),
      #  actionButton("run_variance_test", "Executar Teste", class = "btn-success"),
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
            title = "Distribuição das Classes", width = 6, status = "info", solidHeader = TRUE,
            #plotlyOutput("class_dist")
            
            #MODIFICADO EM 10/05/2025 PARA DEFINIR QUAL GRÁFICO APARECERÁ EM DISTRIBUIÇÃO DE CLASSES(SEMPRE O ÚLTIMO!)
            ##   plotlyOutput("class_dist",height = "500px"),
            ##  plotlyOutput("class_dist_nc",height = "500px") #ACRÉSCIMO EM 10/05/2025 PARA DISTRIB. DE CLASSES DO SMOTE_NC
            
            uiOutput("class_dist_switch")  # Exibe dinamicamente o gráfico correto em 10/05/2025
            #MODIFICADO EM 10/05/2025 PARA DEFINIR QUAL GRÁFICO APARECERÁ EM DISTRIBUIÇÃO DE CLASSES(SEMPRE O ÚLTIMO!)
            
            # )
          ),
          
          #  fluidRow(
          ##      box(
          ##      title = "Boxplot das Variáveis", width = 6, status = "info", solidHeader = TRUE,
          
          
          #plotlyOutput("boxplot_variables")
          
          ##    plotlyOutput("boxplot_variables",height = "500px")
          
          
          
          
          
          
          #  )
          ##    ),
          
          ##MODIFICAÇÃO EM 06/05/2025 - SELEÇÃO DE VARIÁVEIS PARA VISUALIZAR NO BOXPLOT 
          
          box(
            title = "Boxplot das Variáveis", width = 6, status = "info", solidHeader = TRUE,
            
            selectInput(
              inputId = "boxplot_vars",
              label = "Selecione as variáveis para o boxplot:",
              choices = NULL,  # será preenchido dinamicamente
              multiple = TRUE, selected = NULL
            ),
            
            #  plotlyOutput("boxplot_variables", height = "500px")
            ## uiOutput("boxplot_switch")  # UI dinâmica para exibir o boxplot correto em 16/05/2025
            plotlyOutput("boxplot_output", height = "500px") #MODIFICADO EM 19/05/2025
          ),
          
          
          
          
          
          
          
          
          
          # fluidRow(
          
          
          ##ACRÉSCIMO DE PLOT COMPARATIVO DAS AMOSTRAS ORIGINAIS E SINTÉTICAS NA GUIA DIAGNÓSTICO EM 27/03/2025
          
          ##  box(
          ##    title = "Plot: Amostras Originais vs Sintéticas", width = 12, status = "info", solidHeader = TRUE,
          
          # Filtro por tipo de amostra
          #  checkboxGroupInput("amostra_tipo", "Tipo de amostra:",
          #                    choices = c("Original", "Sintética"),
          #                   selected = c("Original", "Sintética"),
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
          
          
          
          
          
          
          ##ACRÉSCIMO DE PLOT COMPARATIVO DAS AMOSTRAS ORIGINAIS E SINTÉTICAS NA GUIA DIAGNÓSTICO EM 27/03/2025       
          
          
          
          #MODIFICAÇÕES PARA MAIOR FLEXIBILIDADE PARA OS USUÁRIOS COM PARÂMETROS DINÂMICOS EM 12/03/2025
          #MODIFICAÇÃO DE "HEIGHT" EM PROJEÇÃO 2D PARA SUPORTAR TAMANHO DE TSNE
          box(
            #title = "Projeção 2D", width = 6, height=800, status = "info", solidHeader = TRUE,
            #title = "Projeção 2D", width = 6, height=1600, status = "info", solidHeader = TRUE,
            ##title = "Projeção 2D", status = "info", solidHeader = TRUE,
            title = "Projeção 2D", width=12, status = "info", solidHeader = TRUE,
            selectInput("projection", "Escolha a Projeção:", 
                        choices = c("PCA", "PCA Robusta", "t-SNE"), selected = "PCA"),
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
            
            actionButton("run_projection", "Executar Projeção", class = "btn-success")
            ,
            ##plotlyOutput("projection_plot")
            plotlyOutput("projection_plot",height="700px")
          ),
          #   ),
          #  fluidRow(
          
          ######ACRÉSCIMO DA PROJEÇÃO 3D EM 20/03/2025############################ 
          
          box(
            ##title = "Projeção 3D", width = 6, height = 1600, status = "info", solidHeader = TRUE,
            title = "Projeção 3D", width = 12, status = "info", solidHeader = TRUE,
            #title = "Projeção 3D", width = 8,height = 1600, status = "info", solidHeader = TRUE,
            selectInput("projection_3d", "Escolha a Projeção:", 
                        choices = c("PCA", "PCA Robusta", "t-SNE"), selected = "PCA"),
            
            numericInput("hot_confidence_3d", "Set the confidence limit for the HoT Elipse:",  
                         max = 0.99, min = 0, value = 0.95, step = 0.01),
            
            ##LIMITANDO EM 30 O NÚMERO DE PCS EM 06/05/2025
            
            # numericInput("pc1_plot_3d", "1st PC to plot:", value = 1, min = 1),
            #  numericInput("pc2_plot_3d", "2nd PC to plot:", value = 2, min = 1),
            #  numericInput("pc3_plot_3d", "3rd PC to plot:", value = 3, min = 1), # Adicionando 3ª dimensão para projeção 3D
            
            
            
            numericInput("pc1_plot_3d", "1st PC to plot:", value = 1, min = 1, max = 30),
            numericInput("pc2_plot_3d", "2nd PC to plot:", value = 2, min = 1, max = 30),
            numericInput("pc3_plot_3d", "3rd PC to plot:", value = 3, min = 1, max = 30), # Adicionando 3ª dimensão para projeção 3D
            
            
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
            
            actionButton("run_projection_3d", "Executar Projeção", class = "btn-success"),
            #div(style = "height: 300%"),
            #div(style = "height: calc(100% - 320px); overflow: hidden;",  # ajusta altura com base nos inputs acima (~320px de inputs)
            ##plotlyOutput("projection_plot_3d") # Saída para a projeção 3D
            plotlyOutput("projection_plot_3d",height="700px") # Saída para a projeção 3D
            #plotlyOutput("projection_plot_3d",height = "100%") # Saída para a projeção 3D
            # )
          ),
          ######ACRÉSCIMO DA PROJEÇÃO 3D EM 20/03/2025############################
          
          
          ##MODIFICAÇÃO TAMANHO DA JANELA SCCORES PCA EM 28/03/2025
          
          ##MODIFICAÇÃO DO HEIGHT PARA MELHORAR EXIBIÇÃO DO GRÁFICO(MELHOR CENTRALIZAÇÃO EM TELA)
          ##       box(
          ##       #title = "Boxplot dos Scores (PCA)", width = 6, status = "info", solidHeader = TRUE,
          ##     #title = "Boxplot dos Scores (PCA)", width = 10, status = "info", solidHeader = TRUE,
          ##    ##  title = "Boxplot dos Scores (PCA)", width = 12,height = 650, status = "info", solidHeader = TRUE,
          ##    title = "Boxplot dos Scores (PCA)", width = 12, status = "info", solidHeader = TRUE,
          ##   ## plotlyOutput("boxplot_scores")
          ##   plotlyOutput("boxplot_scores", height = "700px")
          ##    )
          ##    ),
          
          ##MODIFICAÇÃO EM 06/05/2025: QUAIS COMPONENTES SERÃO VISUALIZADAS? 
          
          box(
            title = "Boxplot dos Scores (PCA)", width = 12, status = "info", solidHeader = TRUE,
            
            selectInput(
              inputId = "pcs_to_show",
              label = "Selecione os Componentes Principais (PCs) a exibir:",
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
        #    box(title = "Resíduos (PCA)", status = "info", solidHeader = TRUE, width = 12,
        #       #numericInput("numPCresidCustom", "Principal Component Number", value = 1, min = 1),
        #      numericInput("numPCresidCustom", "Principal Component Number", min = 1,value = 1 ),
        #     checkboxInput("logResidualsCustom", "Apply Log", value = FALSE),
        #    actionButton("run_residuals", "Visualizar Resíduos", class = "btn-success")
        #   ,
        #  plotOutput("pcaResidualsPlotCustom")
        #  ),
        
        
        # box(title = "Resíduos (PCA)", status = "info", solidHeader = TRUE, width = 12,
        #    numericInput("numPCresidCustom", "Principal Component Number", min = 1, value = 1),
        #   checkboxInput("logResidualsCustom", "Apply Log", value = FALSE),
        #  actionButton("run_residuals", "Visualizar Resíduos", class = "btn-success"),
        #  plotlyOutput("pcaResidualsPlotCustom", height = "700px")
        #  ),
        
        
        
        box(
          title = "Resíduos (PCA)", status = "info", solidHeader = TRUE, width = 12,
          
          numericInput("numPCresidCustom", "Principal Component Number", min = 1, value = 1),
          checkboxInput("logResidualsCustom", "Apply Log", value = FALSE),
          actionButton("run_residuals", "Visualizar Resíduos", class = "btn-success"),
          
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
        #     actionButton("run_outliers", "Visualizar Outliers", class = "btn-success")
        #        ,
        #        # plotOutput("OutPCACustom", height = 650)
        #   plotlyOutput("OutPCACustom", height = 650)
        #  ),
        
        
        
        
        box(
          title = "Outliers (PCA)", status = "info", solidHeader = TRUE, width = 12,
          
          numericInput("numPCoutipcaCustom", "Principal Component Number", min = 1, value = 1),
          actionButton("run_outliers", "Visualizar Outliers", class = "btn-success"),
          br(),
          plotlyOutput("OutPCACustom", height = 650),
          br(),
          uiOutput("outliersHelpTextUI")  
        ),
        
        ########ACRÉSCIMO DOS OUTLIERS DA PCA EM VISUALIZAÇÕES EM 14/03/2025#####        
        
        
        
        #######ACRÉSCIMO DA DIVERGÊNCIA DE JENSEN-SHANNON EM 28/03/2025#####
        
        
        #box(
        # title = "Divergência de Jensen-Shannon",
        # width = 12, status = "info", solidHeader = TRUE,
        # plotlyOutput("js_divergence_plot", height = "550px"),
        # hr(),
        # selectInput("classe_detalhe", "Visualizar distribuições para a classe:", choices = NULL),
        # selectInput("variavel_detalhe", "Variável:", choices = NULL),
        # plotlyOutput("js_distributions_plot", height = "400px")
        # ),
        
        
        ## ACRÉSCIMO DE TEXTO EXPLICATIVO EM 03/04/2025
        
        box(
          title = "Divergência de Jensen-Shannon",
          width = 12, status = "info", solidHeader = TRUE,
          
          # Gráfico principal
          plotlyOutput("js_divergence_plot", height = "550px"),
          hr(),
          
          # Texto explicativo
          HTML("<p style='font-size:15px; color:#555;'>
        A divergência é calculada entre amostras <strong>originais</strong> e <strong>sintéticas</strong>, 
        sendo exibida apenas para as <strong>classes que foram efetivamente sintetizadas</strong>.
      </p>"),
          br(),
          
          # Seletores
          selectInput("classe_detalhe", "Visualizar distribuições para a classe:", choices = NULL),
          selectInput("variavel_detalhe", "Variável:", choices = NULL),
          
          # Gráfico detalhado
          plotlyOutput("js_distributions_plot", height = "400px")
        ),
        
        
        #######ACRÉSCIMO DE GRÁFICO DE PROCRUSTES GENERALIZADO EM 29/03/2025##
        
        
        box(
          title = "Análise de Procrustes Generalizado", 
          status = "info", solidHeader = TRUE, width = 12,
          plotlyOutput("grafico_procrustes_multiclasse", height = "700px")
        ),
        
        #######ACRÉSCIMO DE GRÁFICO DE PROCRUSTES GENERALIZADO EM 29/03/2025##
        
        
        ###ACRÉSCIMO DE GRÁFICO DA DISTÂNCIA DE MAHALANOBIS EM 29/03/2025##
        
        
        #box(
        ## #title = "Distância de Mahalanobis - Original vs Sintético por Classe",
        # ##MODIFICADO EM 31/03/2025
        #  #title = "Distância de Mahalanobis por Classe",
        #  title = "Distância de Mahalanobis",
        # status = "info", solidHeader = TRUE, width = 12,
        # plotlyOutput("grafico_mahalanobis_por_classe", height = "500px")
        # ),
        
        
        box(
          # Título atualizado e limpo
          title = "Distância de Mahalanobis",
          status = "info", solidHeader = TRUE, width = 12,
          
          # Gráfico principal
          plotlyOutput("grafico_mahalanobis_por_classe", height = "500px"),
          br(),
          
          # Texto explicativo
          HTML("<p style='font-size:15px; color:#555;'>
         A distância é calculada entre amostras <strong>originais</strong> e <strong>sintéticas</strong>, 
         sendo exibida apenas para as <strong>classes que foram efetivamente sintetizadas</strong>.
       </p>")
        ),
        
        
        
        
        
        
        ##box(
        # # #title = "Distância de Mahalanobis - Original vs Sintético por Classe",
        ##  title = "Hotelling T² e Distância de Mahalanobis por Classe",
        # # status = "info", solidHeader = TRUE, width = 12,
        ##  plotlyOutput("grafico_mahalanobis_por_classe", height = "500px"),
        ##  br(),
        ##  #helpText("H₀: As médias vetoriais (multivariadas) das amostras originais e sintéticas são iguais."),
        ##  p(strong("Hipótese nula:"), " As médias vetoriais (multivariadas) das amostras originais e sintéticas são iguais"),
        ##  DTOutput("tabela_hotelling")
        ## ),
        
        
        #box(
        # title = "Hotelling T² e Distância de Mahalanobis por Classe",
        #  status = "info", solidHeader = TRUE, width = 12,
        
        #  # Gráfico principal
        #  plotlyOutput("grafico_mahalanobis_por_classe", height = "500px"),
        #  br(),
        
        #  # Sub-box para o teste Hotelling
        #  box(
        #    title = "Teste de Hotelling T²",
        #    status = "primary", solidHeader = TRUE, width = 12,
        #    p(strong("Hipótese nula:"), " As médias vetoriais (multivariadas) das amostras originais e sintéticas são iguais."),
        #    DTOutput("tabela_hotelling")
        #  )
        # ),
        
        
        
        ###ACRÉSCIMO DE GRÁFICO DA DISTÂNCIA DE MAHALANOBIS EM 29/03/2025##
        
        
        
        ###TODOS OS TESTES PASSARAM A ESTAR DENTRO DA MESMA FLUIDROW EM 28/03/2025
        fluidRow(
          #######ACRÉSCIMO DA DIVERGÊNCIA DE JENSEN-SHANNON EM 28/03/2025#####
          
          #######ACRÉSCIMO DE TESTE DE KOLMOGOROV SMIRNOV EM 28/03/2025#######
          box(
            title = "Teste de Kolmogorov–Smirnov",
            ##status = "primary", solidHeader = TRUE, width = 12, ##REDUÇÃO DO TAMANHO DA JANELA EM 28/03/2025
            status = "primary", solidHeader = TRUE, width = 6,
            #helpText("Comparação das distribuições das amostras originais e sintéticas."),
            p(strong("Hipótese nula:"), "A distribuição de probabilidade das amostras originais e sintéticas são iguais"),
            DTOutput("ks_por_classe")
          ),
          
          
          #######ACRÉSCIMO DE TESTE DE KOLMOGOROV SMIRNOV EM 28/03/2025#######
          
          
          ######ACRÉSCIMO DE TESTE DE PROCRUSTE GENERALIZADO EM 28/03/2025####
          box(
            title = "Teste de Procruste Generalizado",
            width = 6, status = "primary", solidHeader = TRUE,
            ## width = 12, status = "primary", solidHeader = TRUE, ##REDUÇÃO DO TAMANHO DA JANELA EM 28/03/2025
            # helpText("Compara as estruturas das amostras originais e sintéticas usando o teste de Procrustes."),
            p(strong("Hipótese nula:"), "As estruturas das amostras originais e sintéticas são semelhantes."),
            DTOutput("procrustes_tabela_teste")
          ),
          
          ######ACRÉSCIMO DE TESTE DE PROCRUSTE GENERALIZADO EM 28/03/2025####
          
          
          
          
          ####ACRÉSCIMO DE T² DE HOTELLING E DIST.MAHALANOBIS EM 29/03/2025##
          
          ###MODIFICAÇÃO EM 30/03/2025 - ACRÉSCIMO DE TESTE DE NORMALIDADE MULTIVARIADA
          
          #box(
          # #title = "Hotelling T² e Distância de Mahalanobis por Classe",
          #  title = "Teste T² de Hotelling",
          #  width = 12, status = "primary", solidHeader = TRUE,
          
          #  #p(strong("Hipótese nula:"), " As distribuições multivariadas das amostras originais e sintéticas são iguais para cada classe."),
          #  p(strong("Hipótese nula:"), "As médias vetoriais (multivariadas) das amostras originais e sintéticas são iguais"),
          
          
          #  shiny::hr(),
          
          ##  actionButton("run_hotelling", "Executar Comparação", class = "btn-success"),
          ##  shiny::hr(),
          
          #  DT::dataTableOutput("hotelling_table")
          #),
          
          
          #box(
          # title = "Teste T² de Hotelling",
          #  width = 12, status = "primary", solidHeader = TRUE,
          
          #  p(strong("Hipótese nula:"), "As médias vetoriais (multivariadas) das amostras originais e sintéticas são iguais."),
          #  p(em("Observação: O teste T² de Hotelling só é válido se os dados apresentarem normalidade multivariada para cada classe. Se essa condição não for atendida, os resultados não serão exibidos para a classe correspondente.")),
          #  p(em("Normalidade multivariada para cada classe: Teste de Henze-Zirkler.")),
          
          #  shiny::hr(),
          
          #  DT::dataTableOutput("hotelling_table")
          # ),
          
          ##ACRÉSCIMO DO TESTE DE ROYSTON(EXTENSÃO MULTIVARIADA DO TESTE DE SHAPIRO-WILK) EM 02/05/2025
          
          box(
            title = "Teste T² de Hotelling",
            width = 12, status = "primary", solidHeader = TRUE,
            
            p(strong("Hipótese nula do Teste T² de Hotelling:"), "As médias vetoriais (multivariadas) das amostras originais e sintéticas são iguais."),
            p(em("Observação: O teste T² de Hotelling só é válido se os dados apresentarem normalidade multivariada para cada classe. Se essa condição não for atendida, os resultados são mostrados com ressalva.")),
            p(strong("Hipótese nula do Teste de Henze-Zirkler:"), "Os dados da classe avaliada seguem uma distribuição normal multivariada."),
            p(strong("Hipótese nula do Teste de Royston(extensão multivariada do teste de Shapiro-Wilk):"), "As variáveis da classe seguem conjuntamente uma distribuição normal multivariada."),
            
            hr(),
            
            h4("Verificação da Normalidade Multivariada por Classe"),
            DT::dataTableOutput("normalidade_table"),
            
            hr(),
            
            h4("Resultados por Classe - Hotelling T²"),
            DT::dataTableOutput("hotelling_table")
          ),
          
          ##ACRÉSCIMO DO TESTE DE ROYSTON(EXTENSÃO MULTIVARIADA DO TESTE DE SHAPIRO-WILK) EM 02/05/2025
          ####ACRÉSCIMO DE T² DE HOTELLING E DIST.MAHALANOBIS EM 29/03/2025##
          
          
          ###ACRÉSCIMO DE TESTE PERMANOVA EM 30/03/2025#####
          
          box(
            title = "Teste PERMANOVA", 
            width = 12, status = "primary", solidHeader = TRUE,
            
            p(strong("Hipótese nula:"),
              "As distribuições multivariadas das amostras originais e sintéticas são iguais para cada classe."),
            p(em("Observação: O teste PERMANOVA(Permutation Multivariate Analysis of Variance) compara classes multivariadas sem assumir normalidade.")),
            DT::dataTableOutput("permanova_table")
          ),
          
          
          ###ACRÉSCIMO DE TESTE PERMANOVA EM 30/03/2025#####
          
          
          
          ############################################################################
          # Visualizações
          #    tabItem(
          #     tabName = "visual",
          #    fluidRow(
          #     box(
          #      title = "Distribuição das Classes", width = 6, status = "info", solidHeader = TRUE,
          #     plotlyOutput("class_dist")
          #    # )
          #  ),
          
          #  # fluidRow(
          #  box(
          #   title = "Projeção 2D", width = 6, status = "info", solidHeader = TRUE,
          #  selectInput("projection", "Escolha a Projeção:", 
          #             choices = c("PCA", "PCA Robusta", "t-SNE"), selected = "PCA"),
          #  plotlyOutput("projection_plot")
          #  )
          #  ),
          #  fluidRow(
          #   box(
          #    title = "Boxplot dos Scores (PCA)", width = 6, status = "info", solidHeader = TRUE,
          #   plotlyOutput("boxplot_scores")
          #  ),
          # box(
          #  title = "Boxplot das Variáveis", width = 6, status = "info", solidHeader = TRUE,
          # plotlyOutput("boxplot_variables")
          #  ),
          
          ####MODIFICAÇÃO DOS TESTES DA GUIA RESULTADOS PARA A GUIA DE VISUALIZAÇÕES EM 07/03/2025
          
          #ACRÉSCIMO DA DESCRIÇÃO DA HIPÓTSE NULA EM TESTES DE VARIÂNCIA EM 28/03/2025
          
          ##  fluidRow(
          box(
            title = "Testes de Variância", width = 12, status = "primary", solidHeader = TRUE,
            p(strong("Hipótese nula:"), "As amostras originais e sintéticas têm variâncias iguais"),
            
            selectInput("variance_test", "Escolha o Teste de Variância:",
                        #choices = c("Levene's Test", "Bartlett's Test", "Fligner-Killeen Test", "F-Test"),
                        choices = c("Levene's Test", "Fligner-Killeen Test"),
                        selected = "Levene's Test"),
            actionButton("run_variance_test", "Executar Teste", class = "btn-success"),
            verbatimTextOutput("variance_test_results")
            
            
          ) 
        ) ,
        #    )
        
        
  
      ),
      
      #    # Resultados
      #   #ALTERAÇÃO EM 15/02/2025
      
      #    #ALTERAÇÃO DO NOME DO BOX de "Dados Originais" para "Amostras originais"
      #   #ALTERAÇÃO DO NOME DO BOX de "Dados Combinados" para "Amostras combinadas"
      
      #  tabItem(
      #   tabName = "results",
      #  fluidRow(
      #   box(
      #    title = "Amostras Originais", width = 6, status = "warning", solidHeader = TRUE,
      #   DTOutput("original_data")
      #  ),
      
      # box(
      #  title = "Amostras Sintéticas", width = 6, status = "warning", solidHeader = TRUE,
      # DTOutput("synthetic_only_data")
      
      #  ),
      # box(
      #  title = "Amostras Combinadas", width = 6, status = "warning", solidHeader = TRUE,
      # DTOutput("synthetic_data")
      
      #  )
      #  ),
      
      #  #FUNCIONALIDADE DE EXPORTAÇÃO DE RESULTADOS EM 15/02/2025
      #  box(title = "Exportação de Dados", status = "info", solidHeader = TRUE, width = 12,
      #     selectInput("dataset_choice", "Escolha o conjunto de dados:", 
      #                choices = c("Amostras Originais" = "original", "Amostras Sintéticas" = "synthetic", "Amostras Combinadas" = "combined")),
      #   selectInput("export_format", "Escolha o formato:", choices = c(".csv" = "csv", ".xls" = "xlsx")),
      #  downloadButton("download_selected", "Exportar Dados")
      #  ),
      
      #  fluidRow(
      #   box(
      #    title = "Testes de Variância", width = 12, status = "primary", solidHeader = TRUE,
      #   selectInput("variance_test", "Escolha o Teste de Variância:",
      #              #choices = c("Levene's Test", "Bartlett's Test", "Fligner-Killeen Test", "F-Test"),
      #             choices = c("Levene's Test", "Fligner-Killeen Test"),
      #            selected = "Levene's Test"),
      #  actionButton("run_variance_test", "Executar Teste", class = "btn-success"),
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
            p(strong("obs: Para base de dados com variáveis quantitativas(numéricas) e qualitativas(categóricas) opte pelo método SMOTE_NC")),
            
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
              label = "Tipo de balanceamento (sampling_strategy):",
              choices = c(
                "minority" = "minority",
                "not minority" = "not minority",
                "not majority" = "not majority",
                "all" = "all",
                "auto" = "auto"
              ),
              selected = "auto"
            ),
            #   helpText("Escolha como as classes devem ser balanceadas:
            #           \n- 'minority': apenas a classe minoritária será aumentada;
            #          \n- 'not minority': todas exceto a minoritária;
            #         \n- 'not majority': todas exceto a majoritária;
            #        \n- 'all': todas as classes;
            #       \n- 'auto': equivalente a 'not majority'."),
            
            p(strong("Escolha como as classes devem ser balanceadas:"), " "),
            p(strong(".minority"), em("apenas a classe minoritária será aumentada;")),
            p(strong(".not minority"), em("todas exceto a minoritária;")),
            p(strong(".not majority"), em("todas exceto a majoritária;")),
            p(strong(".all"), em("todas as classes;")),
            p(strong(".auto"), em("equivalente a 'not majority.")),
            
            
            shiny::hr(),
            uiOutput("target_nc"),  # Seletor do atributo target para SMOTE_NC
            ## uiOutput("class_in_nc"), ##OMITIDO EM 09/05/2025
            
            #COMANDO OMITIDO EM 08/05/2025(SAMPLING_ESTRATEGY RESOLVE ESSA QUESTÃO)
            ##checkboxInput("smote_all_classes_nc", "Perform SMOTE_NC on the entire dataset", value = FALSE),
            
            
            ############TEXTO ADICIONADO EM 19/05/2025####################
            p(strong("obs: Para base de dados com todas as variáveis quantitativas(numéricas) opte pelo método SMOTE")),
            
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
              label = "Tipo de balanceamento (sampling_strategy):",
              choices = c(
                "minority" = "minority",
                "not minority" = "not minority",
                "not majority" = "not majority",
                "all" = "all",
                "auto" = "auto"
              ),
              selected = "auto"
            ),
            
            p(strong("Escolha como as classes devem ser balanceadas:"), " "),
            p(strong(".minority"), em("apenas a classe minoritária será aumentada;")),
            p(strong(".not minority"), em("todas exceto a minoritária;")),
            p(strong(".not majority"), em("todas exceto a majoritária;")),
            p(strong(".all"), em("todas as classes;")),
            p(strong(".auto"), em("equivalente a 'not majority.")),
            
            shiny::hr(),
            uiOutput("target_borderline"),  # Seletor do atributo target para Borderline-SMOTE
            
            p(strong("obs: Borderline_SMOTE se aplica apenas a variáveis quantitativas(numéricas).")),
            
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
              label = "Tipo de balanceamento (sampling_strategy):",
              choices = c(
                "minority" = "minority",
                "not minority" = "not minority",
                "not majority" = "not majority",
                "all" = "all",
                "auto" = "auto"
              ),
              selected = "auto"
            ),
            
            p(strong("Escolha como as classes devem ser balanceadas:"), " "),
            p(strong(".minority"), em("apenas a classe minoritária será aumentada;")),
            p(strong(".not minority"), em("todas exceto a minoritária;")),
            p(strong(".not majority"), em("todas exceto a majoritária;")),
            p(strong(".all"), em("todas as classes;")),
            p(strong(".auto"), em("equivalente a 'not majority'.")),
            
            shiny::hr(),
            uiOutput("target_svm"),  # Seletor do atributo target para SVM_SMOTE
            
            p(strong("obs: SVM_SMOTE se aplica apenas a variáveis quantitativas (numéricas).")),
            
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
              label = "Tipo de balanceamento (sampling_strategy):",
              choices = c(
                "minority" = "minority",
                "not minority" = "not minority",
                "not majority" = "not majority",
                "all" = "all",
                "auto" = "auto"
              ),
              selected = "auto"
            ),
            
            p(strong("Escolha como as classes devem ser balanceadas:"), " "),
            p(strong(".minority"), em("apenas a classe minoritária será aumentada;")),
            p(strong(".not minority"), em("todas exceto a minoritária;")),
            p(strong(".not majority"), em("todas exceto a majoritária;")),
            p(strong(".all"), em("todas as classes;")),
            p(strong(".auto"), em("equivalente a 'not majority'.")),
            
            shiny::hr(),
            uiOutput("target_adasyn"),  # Seletor do atributo target para ADASYN
            
            p(strong("obs: ADASYN se aplica apenas a variáveis quantitativas (numéricas).")),
            
            #OBSERVAÇÃO ADICIONADA EM 08/06/2025
            
            
            p(strong("obs:This method is similar to SMOTE but it generates different number of samples depending on an estimate of the local distribution of the class to be oversampled.")),
            
            
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
              label = "Tipo de balanceamento (sampling_strategy):",
              choices = c(
                "minority" = "minority",
                "not minority" = "not minority",
                "not majority" = "not majority",
                "all" = "all",
                "auto" = "auto"
              ),
              selected = "auto"
            ),
            
            p(strong("Escolha como as classes devem ser balanceadas:"), " "),
            p(strong(".minority"), em("apenas a classe minoritária será aumentada;")),
            p(strong(".not minority"), em("todas exceto a minoritária;")),
            p(strong(".not majority"), em("todas exceto a majoritária;")),
            p(strong(".all"), em("todas as classes;")),
            p(strong(".auto"), em("equivalente a 'not majority'.")),
            
            shiny::hr(),
            
            # Seletor do target
            
            uiOutput("target_ru"),
            
            # Observações
            
            p(strong("obs:"), "Random Upsampling replica aleatoriamente as amostras das classes selecionadas até igualar à classe majoritária."),
            p(strong("obs:"), "Este método não cria novas amostras sintéticas, apenas replica amostras existentes."),
            
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
            p(a(href = "https://cran.r-project.org/web/packages/devtools/devtools.pdf", "https://cran.r-project.org/web/packages/devtools/devtools.pdf", target = "_blank"))
            
            
            
            
            
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
    
    # Criando tabela de frequências com verificação
    class_freq <- as.data.frame(table(sampleclass()))
    
    # Validando que class_freq tem 2 colunas antes de aplicar colnames
    if (ncol(class_freq) == 2) {
      colnames(class_freq) <- c("Class", "Frequency")
    } else {
      warning(paste0("class_freq tem ", ncol(class_freq), " colunas. Esperava-se 2."))
      print(class_freq)  # Debug: vendo o que chegou
      output$datasummary <- renderPrint({ "Erro: Estrutura da frequência de classes inválida." })
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
    
    # Projeção 2D
    
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
    
    # Projeção 3D
    
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
      showNotification("Erro: Dados inválidos ou target não encontrado.", type = "error")
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
        showNotification("Nenhuma amostra sintética foi gerada. Verifique os parâmetros do SMOTE.", type = "error")
        return(NULL)
      }
      
      SMOTE_Data(list(
        data = as.data.frame(genData$data),
        syn_data = as.data.frame(genData$syn_data)
      ))
    } else {
      if (is.null(X()) || is.null(class_vec()) || nrow(as.data.frame(X())) == 0) {
        showNotification("Erro: Não há amostras suficientes para aplicar SMOTE.", type = "error")
        return(NULL)
      }
      
      set.seed(123)
      df_class_vec <- imported_data[[input$target]]
      X_selected <- imported_data[df_class_vec == input$class, which(sapply(imported_data, is.numeric)), drop = FALSE]
      
      if (nrow(X_selected) == 0) {
        showNotification("Erro: Não há amostras suficientes para aplicar SMOTE.", type = "error")
        return(NULL)
      }
      
      genData <- smotefamily::SMOTE(
        X = X_selected,
        target = as.factor(df_class_vec[df_class_vec == input$class]),
        K = input$K,
        dup_size = input$dup_size
      )
      
      if (is.null(genData$data) || nrow(genData$data) == 0) {
        showNotification("Nenhuma amostra sintética foi gerada. Verifique os parâmetros do SMOTE.", type = "error")
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
    
    # Obtendo a contagem das classes originais antes do SMOTE
    
    original_counts <- table(as.factor(sampleclass()))  
    
    # Obtendo as amostras sintéticas acumuladas
    
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
    
    # Renderizando tabela de amostras sintéticas após SMOTE
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
  
  #      # Obtendo a contagem das classes originais antes do SMOTE
  
  #      original_counts <- table(as.factor(sampleclass()))  
  
  #      # Obtendo as amostras sintéticas acumuladas
  
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
    
    # Detectando variáveis categóricas
    categorical_cols <- names(X)[sapply(X, function(x) is.factor(x) || is.character(x))]
    
    # Se não houver, cria variável fake
    if (length(categorical_cols) == 0) {
      X$fake_cat <- factor(sample(c("A", "B"), size = nrow(X), replace = TRUE))
      categorical_cols <- "fake_cat"
      showNotification("Nenhuma variável categórica encontrada. 'fake_cat' criada.", type = "warning")
    }
    
    # Índices para SMOTENC (0-based)
    cat_indices <- which(names(X) %in% categorical_cols) - 1
    cat_indices <- as.list(as.integer(cat_indices))
    
    # Verificando menor classe
    class_counts <- table(y)
    min_class_size <- min(class_counts)
    if (min_class_size < 2) {
      showNotification("A menor classe tem menos de 2 amostras.", type = "error")
      return()
    }
    
    # Ajustando k
    k_value <- input$K_nc
    if (min_class_size <= k_value) {
      k_value <- min_class_size - 1
      if (k_value < 1) {
        showNotification("k_neighbors muito alto para a menor classe.", type = "error")
        return()
      }
      showNotification(paste("k_neighbors ajustado para", k_value), type = "warning")
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
      showNotification("A menor classe tem menos de 2 amostras.", type = "error")
      return()
    }
    
    k_value <- input$K_border
    if (min_class_size <= k_value) {
      k_value <- min_class_size - 1
      if (k_value < 1) {
        showNotification("k_neighbors muito alto para a menor classe.", type = "error")
        return()
      }
      showNotification(paste("k_neighbors ajustado para", k_value), type = "warning")
    }
    
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
      showNotification("A menor classe tem menos de 2 amostras.", type = "error")
      return()
    }
    
    k_value <- input$K_svm
    if (min_class_size <= k_value) {
      k_value <- min_class_size - 1
      if (k_value < 1) {
        showNotification("k_neighbors muito alto para a menor classe.", type = "error")
        return()
      }
      showNotification(paste("k_neighbors ajustado para", k_value), type = "warning")
    }
    
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
      showNotification("A menor classe tem menos de 2 amostras.", type = "error")
      return()
    }
    
    k_value <- input$K_adasyn
    if (min_class_size <= k_value) {
      k_value <- min_class_size - 1
      if (k_value < 1) {
        showNotification("k_neighbors muito alto para a menor classe.", type = "error")
        return()
      }
      showNotification(paste("k_neighbors ajustado para", k_value), type = "warning")
    }
    
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
      showNotification("A menor classe tem menos de 2 amostras.", type = "error")
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
  
  # Tabela das amostras sintéticas
  
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
  
  
  
  
  
  ####MODIFICADO EM 27/02/2025 ######
  
  ####### ATUALIZADO EM 10/02/2025 ########
  
  ####### ATUALIZADO EM 09/03/2025 ######## 
  #OBS: BARPLOT ATUALIZA QUANDO CLICO "RUN SMOTE"
  
  # Visualização: Distribuição das Classes
  
  # Diagnóstico: Distribuição das Classes # MUDANÇA DA GUIA EM 27/03/2025
  ##MODIFICADO EM 09/04/2025 - ACRÉSCIMO DOS VALORES ACIMA DAS BARRAS
  
  output$class_dist <- renderPlotly({
    req(data(), SMOTE_Data())  # Garante que os dados originais e do SMOTE estão disponíveis
    
    # Contagem das classes originais (essas nunca mudam)
    original_counts <- table(as.factor(sampleclass()))  
    df_original <- data.frame(
      Classe = names(original_counts),
      Contagem = as.numeric(original_counts),
      Tipo = names(original_counts)  # Cada classe terá sua própria cor
    )
    
    # Verificando se os dados sintéticos existem e possuem a coluna 'class'
    smote_data <- SMOTE_Data()$data
    if (is.null(smote_data) || !"class" %in% colnames(smote_data)) {
      
      # Se os dados do SMOTE ainda não foram gerados, retorna apenas os dados originais
      df_plot <- df_original
    } else {
      # Contagem das classes sintéticas (somente as que receberam amostras do SMOTE)
      synthetic_counts <- table(as.factor(smote_data$class))
      
      # Criando estrutura inicial SEM valores sintéticos no começo
      synthetic_aligned <- setNames(rep(0, length(original_counts)), names(original_counts))
      
      # Atualizando apenas as classes que receberam amostras sintéticas
      synthetic_aligned[names(synthetic_counts)] <- as.numeric(synthetic_counts) - original_counts[names(synthetic_counts)]
      
      # Criando DataFrame para os dados sintéticos (somente as classes que realmente ganharam amostras)
      df_synthetic <- data.frame(
        Classe = paste0(names(synthetic_aligned), " Sintética"),  # Adicionando palavra "Sintética" no nome
        Contagem = as.numeric(synthetic_aligned),
        Tipo = paste0(names(synthetic_aligned), " Sintética")  # Cada classe sintética terá sua própria cor
      )
      
      # Removendo classes que ainda não receberam amostras sintéticas
      df_synthetic <- df_synthetic[df_synthetic$Contagem > 0, ]
      
      # Combinando os DataFrames originais e sintéticos
      df_plot <- rbind(df_original, df_synthetic)
    }
    
    # Gerando cores únicas para cada classe original e sintética
    unique_classes <- unique(df_plot$Tipo)
    num_classes <- length(unique_classes)
    class_colors <- RColorBrewer::brewer.pal(min(12, num_classes), "Set1")
    names(class_colors) <- unique_classes
    
    # Criando gráfico com valores em cima das barras
    plot_ly(data = df_plot) %>%
      add_trace(
        x = ~Classe,
        y = ~Contagem,
        type = "bar",
        text = ~Contagem,
        textposition = "outside",
        color = ~Tipo,
        colors = class_colors
      ) %>%
      layout(
        barmode = "group",
        title = 'Distribuição das Classes', 
        xaxis = list(title = 'Classes'),
        yaxis = list(title = 'Frequência'),
        legend = list(title = list(text = "")) #,
        # uniformtext = list(minsize = 10, mode = "hide")
      )
  })
  
  
  ####### ATUALIZADO EM 09/03/2025 ######## 
  
  
  # Diagnóstico: Distribuição das Classes para SMOTE-NC EM 10/05/2025
  
  output$class_dist_nc <- renderPlotly({
    req(filedata(), SMOTENC_Data())
    
    # Contagem das classes originais
    
    
    
    original_y <- as.factor(filedata()[[input$target_nc]])
    
    
    
    original_counts <- table(original_y)
    df_original <- data.frame(
      Classe = names(original_counts),
      Contagem = as.numeric(original_counts),
      Tipo = names(original_counts)
    )
    
    # Dados do SMOTE-NC
    smote_data <- SMOTENC_Data()$data
    if (is.null(smote_data) || !"class" %in% colnames(smote_data)) {
      df_plot <- df_original
    } else {
      # Contagem total nas classes APÓS o SMOTE-NC
      smote_counts <- table(as.factor(smote_data$class))
      
      # Contagem sintética = total - original
      synthetic_aligned <- setNames(rep(0, length(original_counts)), names(original_counts))
      synthetic_aligned[names(smote_counts)] <- as.numeric(smote_counts) - original_counts[names(smote_counts)]
      
      # Criar DataFrame das sintéticas
      df_synthetic <- data.frame(
        Classe = paste0(names(synthetic_aligned), " Sintética"),
        Contagem = as.numeric(synthetic_aligned),
        Tipo = paste0(names(synthetic_aligned), " Sintética")
      )
      
      # Remover classes com 0 amostras sintéticas
      df_synthetic <- df_synthetic[df_synthetic$Contagem > 0, ]
      
      # Combina original + sintética
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
        y = ~Contagem,
        type = "bar",
        text = ~Contagem,
        textposition = "outside",
        color = ~Tipo,
        colors = class_colors
      ) %>%
      layout(
        barmode = "group",
        title = "Distribuição das Classes",
        xaxis = list(title = "Classe"),
        yaxis = list(title = "Frequência"),
        legend = list(title = list(text = ""))
      )
  })
  
  
  # Diagnóstico: Distribuição das Classes para SMOTE-NC EM 10/05/2025
  
  
  
  # Diagnóstico: Distribuição das Classes para BorderlineSMOTE EM 05/06/2025
  
  output$class_dist_borderline <- renderPlotly({
    req(filedata(), BorderlineSMOTE_Data())
    
    # Contagem das classes originais
    
    original_y <- as.factor(filedata()[[input$target_borderline]])
    original_counts <- table(original_y)
    df_original <- data.frame(
      Classe = names(original_counts),
      Contagem = as.numeric(original_counts),
      Tipo = names(original_counts)
    )
    
    # Dados do BorderlineSMOTE
    
    smote_data <- BorderlineSMOTE_Data()$data
    if (is.null(smote_data) || !"class" %in% colnames(smote_data)) {
      df_plot <- df_original
    } else { 
      
      # Contagem total após o BorderlineSMOTE
      
      smote_counts <- table(as.factor(smote_data$class))
      
      # Contagem sintética = total - original
      
      synthetic_aligned <- setNames(rep(0, length(original_counts)), names(original_counts))
      synthetic_aligned[names(smote_counts)] <- as.numeric(smote_counts) - original_counts[names(smote_counts)]
      
      # DataFrame para sintéticas
      
      df_synthetic <- data.frame(
        Classe = paste0(names(synthetic_aligned), " Sintética"),
        Contagem = as.numeric(synthetic_aligned),
        Tipo = paste0(names(synthetic_aligned), " Sintética")
      )
      
      # Removendo classes sem amostras sintéticas
      
      df_synthetic <- df_synthetic[df_synthetic$Contagem > 0, ]
      
      # Combinando original + sintética
      
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
        y = ~Contagem,
        type = "bar",
        text = ~Contagem,
        textposition = "outside",
        color = ~Tipo,
        colors = class_colors
      ) %>%
      layout(
        barmode = "group",
        title = "Distribuição das Classes",
        xaxis = list(title = "Classe"),
        yaxis = list(title = "Frequência"),
        legend = list(title = list(text = ""))
      )
  })
  
  
  # Diagnóstico: Distribuição das Classes para SVM_SMOTE EM 06/06/2025
  
  output$class_dist_svm <- renderPlotly({
    req(filedata(), SVM_SMOTE_Data())
    
    # Contagem das classes originais
    
    original_y <- as.factor(filedata()[[input$target_svm]])
    original_counts <- table(original_y)
    df_original <- data.frame(
      Classe = names(original_counts),
      Contagem = as.numeric(original_counts),
      Tipo = names(original_counts)
    )
    
    # Dados do SVM_SMOTE
    
    smote_data <- SVM_SMOTE_Data()$data
    if (is.null(smote_data) || !"class" %in% colnames(smote_data)) {
      df_plot <- df_original
    } else { 
      # Contagem total após o SVM_SMOTE
      
      smote_counts <- table(as.factor(smote_data$class))
      
      # Contagem sintética = total - original
      
      synthetic_aligned <- setNames(rep(0, length(original_counts)), names(original_counts))
      synthetic_aligned[names(smote_counts)] <- as.numeric(smote_counts) - original_counts[names(smote_counts)]
      
      # DataFrame para sintéticas
      
      df_synthetic <- data.frame(
        Classe = paste0(names(synthetic_aligned), " Sintética"),
        Contagem = as.numeric(synthetic_aligned),
        Tipo = paste0(names(synthetic_aligned), " Sintética")
      )
      
      # Removendo classes sem amostras sintéticas
      
      df_synthetic <- df_synthetic[df_synthetic$Contagem > 0, ]
      
      # Combinando original + sintética
      
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
        y = ~Contagem,
        type = "bar",
        text = ~Contagem,
        textposition = "outside",
        color = ~Tipo,
        colors = class_colors
      ) %>%
      layout(
        barmode = "group",
        title = "Distribuição das Classes",
        xaxis = list(title = "Classe"),
        yaxis = list(title = "Frequência"),
        legend = list(title = list(text = ""))
      )
  })
  

  
  # Diagnóstico: Distribuição das Classes para adasyn EM 08/06/2025
  
  output$class_dist_adasyn <- renderPlotly({
    req(filedata(), ADASYN_Data())
    
    # Contagem das classes originais
    
    original_y <- as.factor(filedata()[[input$target_adasyn]])
    original_counts <- table(original_y)
    df_original <- data.frame(
      Classe = names(original_counts),
      Contagem = as.numeric(original_counts),
      Tipo = names(original_counts)
    )
    
    # Dados do ADASYN
    
    smote_data <- ADASYN_Data()$data
    if (is.null(smote_data) || !"class" %in% colnames(smote_data)) {
      df_plot <- df_original
    } else { 
      # Contagem total após o SVM_SMOTE
      
      smote_counts <- table(as.factor(smote_data$class))
      
      # Contagem sintética = total - original
      
      synthetic_aligned <- setNames(rep(0, length(original_counts)), names(original_counts))
      synthetic_aligned[names(smote_counts)] <- as.numeric(smote_counts) - original_counts[names(smote_counts)]
      
      # DataFrame para sintéticas
      
      df_synthetic <- data.frame(
        Classe = paste0(names(synthetic_aligned), " Sintética"),
        Contagem = as.numeric(synthetic_aligned),
        Tipo = paste0(names(synthetic_aligned), " Sintética")
      )
      
      # Removendo classes sem amostras sintéticas
      
      df_synthetic <- df_synthetic[df_synthetic$Contagem > 0, ]
      
      # Combinando original + sintética
      
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
        y = ~Contagem,
        type = "bar",
        text = ~Contagem,
        textposition = "outside",
        color = ~Tipo,
        colors = class_colors
      ) %>%
      layout(
        barmode = "group",
        title = "Distribuição das Classes",
        xaxis = list(title = "Classe"),
        yaxis = list(title = "Frequência"),
        legend = list(title = list(text = ""))
      )
  })
  
  
  # Diagnóstico: Distribuição das Classes para Random Upsampling EM 09/06/2025
  
  output$class_dist_ru <- renderPlotly({
    req(filedata(), RU_Data())
    
    # Contagem das classes originais
    
    original_y <- as.factor(filedata()[[input$target_ru]])
    original_counts <- table(original_y)
    df_original <- data.frame(
      Classe = names(original_counts),
      Contagem = as.numeric(original_counts),
      Tipo = names(original_counts)
    )
    
    # Dados do Random Upsampling
    
    smote_data <- RU_Data()$data
    if (is.null(smote_data) || !"class" %in% colnames(smote_data)) {
      df_plot <- df_original
    } else {
      # Contagem total após o Random Upsampling
      
      smote_counts <- table(as.factor(smote_data$class))
      
      # Contagem sintética = total - original
      
      synthetic_aligned <- setNames(rep(0, length(original_counts)), names(original_counts))
      synthetic_aligned[names(smote_counts)] <- as.numeric(smote_counts) - original_counts[names(smote_counts)]
      
      # DataFrame para sintéticas
      
      df_synthetic <- data.frame(
        Classe = paste0(names(synthetic_aligned), " Sintética"),
        Contagem = as.numeric(synthetic_aligned),
        Tipo = paste0(names(synthetic_aligned), " Sintética")
      )
      
      # Removendo classes sem amostras sintéticas
      
      df_synthetic <- df_synthetic[df_synthetic$Contagem > 0, ]
      
      # Combinando original + sintética
      
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
        y = ~Contagem,
        type = "bar",
        text = ~Contagem,
        textposition = "outside",
        color = ~Tipo,
        colors = class_colors
      ) %>%
      layout(
        barmode = "group",
        title = "Distribuição das Classes",
        xaxis = list(title = "Classe"),
        yaxis = list(title = "Frequência"),
        legend = list(title = list(text = ""))
      )
  })
  
  
  
  
  
  
  
    
  
  
  ##PARA DEFINIR QUAL GRÁFICO APARECERÁ NA ÁREA GRÁFICA DE DISTRIBUIÇÃO DE CLASSES EM 10/05/2025
  
#ATUALIZADO EM 05/06/2025(BORDELINE_SMOTE), 06/06/2025(SVM_SMOTE), 08/06/2025(ADASYN) E 09/06/2025(Random upsampling)
  
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
    } else {
      h5("Nenhuma execução de SMOTE realizada ainda.")
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
    
    # Se os dados sintéticos existem, adiciona "Sintética" às classes sintéticas
    if (!is.null(synthetic_data) && "class" %in% colnames(synthetic_data)) {
      synthetic_data$class <- paste(synthetic_data$class, "Sintética")
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
    } else {
      return(NULL)
    }
    
    if (is.null(smote_data) || !(class_col %in% names(original_data))) return(NULL)
    
    names(original_data)[names(original_data) == class_col] <- "class"
    names(smote_data)[names(smote_data) == class_col] <- "class"
    smote_data$class <- paste(smote_data$class, "Sintética")
    smote_data$fake_cat <- NULL
    
    feature_cols <- intersect(names(original_data), names(smote_data))
    original_data <- original_data[, feature_cols, drop = FALSE]
    smote_data <- smote_data[, feature_cols, drop = FALSE]
    
    original_data$Tipo <- "Original"
    smote_data$Tipo <- "Sintética"
    
    dataset <- dplyr::bind_rows(original_data, smote_data)
    validate(need("class" %in% colnames(dataset), "A coluna 'class' não está presente nos dados!"))
    dataset$class <- as.factor(dataset$class)
    
    original_dataset <- dataset[!grepl("Sintética", dataset$class), ]
    synthetic_dataset <- dataset[grepl("Sintética", dataset$class), ]
    if (nrow(synthetic_dataset) > 0) {
      synthetic_dataset$OriginalClass <- gsub(" Sintética", "", synthetic_dataset$class)
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
    } else if (input$projection == "PCA Robusta") {
      pca_robust <- rrcov::PcaHubert(dataset[, sapply(dataset, is.numeric)], k = 2)
      eigenvalues <- pca_robust@eigenvalues
      total_var <- sum(eigenvalues)
      explained_var <- round(100 * eigenvalues[c(input$pc1_plot, input$pc2_plot)] / total_var, 1)
      scores <- data.frame(PC1 = pca_robust@scores[, input$pc1_plot], PC2 = pca_robust@scores[, input$pc2_plot], Class = dataset$class)
      xlab <- paste0("RPC", input$pc1_plot, " (Expvar: ", explained_var[1], "%)")
      ylab <- paste0("RPC", input$pc2_plot, " (Expvar: ", explained_var[2], "%)")
    } else if (input$projection == "t-SNE") {
      
      
      
    #########BLOCO MODIFICADO EM 09/06/2025 - PROBLEMA TSNE PARA RANDOM UPSAMPLING  
      
     # dataset_unique <- dataset[!duplicated(dataset[, sapply(dataset, is.numeric)]), ]
  
      
      if (input$projection == "t-SNE" && isTRUE(runRU())) {
        dataset_unique <- dataset
      } else {
        dataset_unique <- dataset[!duplicated(dataset[, sapply(dataset, is.numeric)]), ]
      }
      
    #########BLOCO MODIFICADO EM 09/06/2025 - PROBLEMA TSNE PARA RANDOM UPSAMPLING  
      
      
      
      
      
      if (nrow(dataset_unique) < 2) {
        showNotification("Erro: Não há amostras suficientes para t-SNE!", type = "error")
        return()
      }
      
      
      tsne_input <- as.matrix(dataset_unique[, sapply(dataset_unique, is.numeric)])
      max_perplexity <- floor((nrow(tsne_input) - 1) / 3)
      perplexity_value <- min(input$tsne_perplexity, max_perplexity)
      tsne_result <- Rtsne(tsne_input, dims = input$tsne_dims, perplexity = perplexity_value, max_iter = input$tsne_iter, theta = input$tsne_theta, verbose = TRUE, check_duplicates = FALSE)
      if (!is.null(tsne_result$Y) && nrow(tsne_result$Y) > 0) {
        scores <- data.frame(PC1 = tsne_result$Y[, 1], PC2 = tsne_result$Y[, 2], Class = dataset_unique$class)
        xlab <- "Dimensão 1"
        ylab <- "Dimensão 2"
      } else {
        showNotification("Erro: t-SNE falhou ao gerar projeção!", type = "error")
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
    original_classes <- sort(unique(as.character(original_dataset$class)))
    all_classes <- unique(as.character(scores$Class))
    synthetic_classes <- setdiff(all_classes, original_classes)
    ordered_classes <- c(original_classes, sort(synthetic_classes))
    
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
                           text = ~paste("Classe:", Class, "<br>SampleID:", SampleID),
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
                title = list(text = paste("Projeção", input$projection, "das Amostras"),
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
  
  ##visualização 3D - ACRÉSCIMO EM 22/03/2025
  
  # Visualização: Projeção 3D refinada com múltiplas elipses
  
  ##ACRÉSCIMO DE LABELS NAS AMOSTRAS NA LEGENDA EM 12/04/2025
  
  
  ##ACRÉSCIMO DE SMOTE NC EM 28/05/2025
  
  ##ACRÉSCIMO DE BORDERLINE SMOTE EM 05/06/2025
  
  ##ACRÉSCIMO DE SVM SMOTE EM 06/06/2025
  
  ##ACRÉSCIMO DE ADASYN EM 08/06/2025
  
  ##ACRÉSCIMO DE RANDOM UPSAMPLING EM 09/06/2025
  
  
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
    
    } else {
      showNotification("Nenhum dado encontrado para plotagem.", type = "error")
      return(NULL)
    }
    
    if (is.null(smote_data) || !(class_col %in% names(original_data))) return(NULL)
    
    names(original_data)[names(original_data) == class_col] <- "class"
    names(smote_data)[names(smote_data) == class_col] <- "class"
    smote_data$class <- paste(smote_data$class, "Sintética")
    smote_data$fake_cat <- NULL
    
    feature_cols <- intersect(names(original_data), names(smote_data))
    original_data <- original_data[, feature_cols, drop = FALSE]
    smote_data <- smote_data[, feature_cols, drop = FALSE]
    
    original_data$Tipo <- "Original"
    smote_data$Tipo <- "Sintética"
    
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
    validate(need(ncol(dataset_numeric) >= 3, "Pelo menos 3 variáveis numéricas são necessárias para projeção 3D."))
    
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
    } else if (input$projection_3d == "PCA Robusta") {
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
      
      
      if (input$projection == "t-SNE" && isTRUE(runRU())) {
        dataset_unique <- dataset
      } else {
        dataset_unique <- dataset[!duplicated(dataset[, sapply(dataset, is.numeric)]), ]
      }
      
      #########BLOCO MODIFICADO EM 09/06/2025 - PROBLEMA TSNE PARA RANDOM UPSAMPLING  
      
      
      
       if (nrow(dataset_unique) < 5) {
        showNotification("Erro: Não há amostras suficientes para t-SNE!", type = "error")
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
        xlab <- "Dimensão 1"
        ylab <- "Dimensão 2"
        zlab <- "Dimensão 3"
      } else {
        showNotification("Erro: t-SNE falhou ao gerar projeção!", type = "error")
        return()
      }
    } else {
      showNotification("Método de projeção não reconhecido!", type = "error")
      return(NULL)
    }
    
    scores$OriginalClass <- ifelse(grepl("Sintética", scores$Class), gsub(" Sintética", "", scores$Class), as.character(scores$Class))
    scores$SampleID <- seq_len(nrow(scores))
    
    ordered_levels <- sort(unique(as.character(original_data$class)))
    scores$Class <- factor(scores$Class, levels = c(ordered_levels, paste0(ordered_levels, " Sintética")))
    scores$OriginalClass <- factor(scores$OriginalClass, levels = ordered_levels)
    
    base_colors <- RColorBrewer::brewer.pal(max(3, length(ordered_levels)), "Set1")
    cores_classes <- unlist(lapply(seq_along(ordered_levels), function(i) {
      cls <- ordered_levels[i]; cor <- base_colors[i]
      setNames(c(cor, cor), c(cls, paste0(cls, " Sintética")))
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
                     line = list(color = cores_classes[strsplit(grp, "_")[[1]][1]], width = 1.0))
    }
    
    for (cls in levels(scores$Class)) {
      data_cls <- scores[scores$Class == cls, ]
      symbol <- ifelse(grepl("Sintética", cls), "diamond", "circle")
      p <- add_trace(p, data = data_cls, x = ~PC1, y = ~PC2, z = ~PC3,
                     type = "scatter3d", mode = "markers",
                     name = cls, legendgroup = paste0(cls, "_pontos"), showlegend = TRUE,
                     marker = list(size = 4, symbol = symbol, opacity = 0.9, color = cores_classes[cls]),
                     text = ~paste("Classe:", Class, "<br>SampleID:", SampleID), hoverinfo = "text")
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
                title = list(text = paste("Projeção 3D -", input$projection_3d),
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
  
  
  ###############ATUALIZADO EM 18/02/2025
  
  
  ##MODIFICAÇÃO DA ORDEM DE EXIBIÇÃO DAS CLASSES NAS LEGENDAS EM 01/04/2025
  
  # Visualização: Boxplot dos Scores do PCA
  ##  output$boxplot_scores <- renderPlotly({
  ##    req(smote_history$data)  # Garantindo que os dados acumulados (originais + sintéticos) estejam disponíveis
  
  # Obtendo o dataset atualizado (dados originais + todas as rodadas do SMOTE)
  ##    dataset <- smote_history$data
  
  # Ordenando os níveis da variável 'class' garantindo que:
  # - classes originais venham primeiro
  # - classes sintéticas venham logo após, na mesma ordem
  ##    original_classes <- unique(gsub(" Sintética$", "", dataset$class[!grepl("Sintética", dataset$class)]))
  ##    synthetic_classes <- paste(original_classes, "Sintética")
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
  ##      layout(title = "Boxplot dos Scores (PCA)",
  ##             xaxis = list(title = "Componentes Principais"),
  ##             yaxis = list(title = "Valores dos Scores"),
  ##             boxmode = "group")
  ##  })
  
  
  
  # Visualização: Boxplot dos Scores do PCA
  
  
  ##MODIFICAÇÃO EM 06/05/2025: QUAIS COMPONENTES SERÃO VISUALIZADAS?
  
  ##ACRÉSCIMO DO SMOTENC EM 28/05/2025
  
  ##ACRÉSCIMO DE BORDERLINE SMOTE EM 05/06/2025
  
  ##ACRÉSCIMO DE SVM SMOTE EM 06/06/2025
  
  ##ACRÉSCIMO DE ADASYN EM 08/06/2025
  
  ##ACRÉSCIMO DE RANDOM UPSAMPLING EM 09/06/2025
  
  
  
  observe({
    dataset <- NULL
    
#   if (isTRUE(runSMOTENC()) && !is.null(SMOTENC_Data()) && !is.null(input$target_nc)) {
#      original_data <- as.data.frame(filedata())
#      smote_data <- SMOTENC_Data()$syn_data
#      class_col <- input$target_nc
#    } else if (isTRUE(runSMOTE()) && !is.null(SMOTE_Data()) && !is.null(input$target)) {
#      original_data <- as.data.frame(filedata())
#      smote_data <- SMOTE_Data()$syn_data
#      class_col <- input$target
#    } else {
#      return()
#    }

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
     
      
    } else {
      return()
    }    
  
    if (is.null(smote_data) || !(class_col %in% names(original_data))) return()
    
    names(original_data)[names(original_data) == class_col] <- "class"
    names(smote_data)[names(smote_data) == class_col] <- "class"
    smote_data$class <- paste(smote_data$class, "Sintética")
    smote_data$fake_cat <- NULL
    
    feature_cols <- intersect(names(original_data), names(smote_data))
    original_data <- original_data[, feature_cols, drop = FALSE]
    smote_data <- smote_data[, feature_cols, drop = FALSE]
    
    original_data$Tipo <- "Original"
    smote_data$Tipo <- "Sintética"
    
    dataset <- dplyr::bind_rows(original_data, smote_data)
    
    num_vars <- names(dataset)[sapply(dataset, is.numeric)]
    num_vars <- setdiff(num_vars, "SampleID")
    
    if (length(num_vars) < 2) return()
    
    ####################ACRÉSCIMO DE VALIDAÇÃO PARA PRCOMP QUANDO NÃO TINHA VARIÁVEL CATEGÓRICA 29/05/2025
    
    dados_filtrados <- dataset[, num_vars, drop = FALSE]
    
    # Proteção contra NA, Inf, -Inf
    if (any(!is.finite(as.matrix(dados_filtrados)))) return()
    
    # Executa a PCA temporária
    pca_temp <- prcomp(dados_filtrados, center = TRUE, scale. = TRUE)
    
    ## pca_temp <- prcomp(dataset[, num_vars], center = TRUE, scale. = TRUE)
    
    ####################ACRÉSCIMO DE VALIDAÇÃO PARA PRCOMP QUANDO NÃO TINHA VARIÁVEL CATEGÓRICA 29/05/2025
    
    
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
    
#    if (isTRUE(runSMOTENC()) && !is.null(SMOTENC_Data()) && !is.null(input$target_nc)) {
#      original_data <- as.data.frame(filedata())
#      smote_data <- SMOTENC_Data()$syn_data
#      class_col <- input$target_nc
#    } else if (isTRUE(runSMOTE()) && !is.null(SMOTE_Data()) && !is.null(input$target)) {
#      original_data <- as.data.frame(filedata())
#      smote_data <- SMOTE_Data()$syn_data
#      class_col <- input$target
#    } else {
#      return(NULL)
#    }
    
    
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
      
      
    } else {
      return(NULL)
    }        
    
     if (is.null(smote_data) || !(class_col %in% names(original_data))) return(NULL)
    
    names(original_data)[names(original_data) == class_col] <- "class"
    names(smote_data)[names(smote_data) == class_col] <- "class"
    smote_data$class <- paste(smote_data$class, "Sintética")
    smote_data$fake_cat <- NULL
    
    feature_cols <- intersect(names(original_data), names(smote_data))
    original_data <- original_data[, feature_cols, drop = FALSE]
    smote_data <- smote_data[, feature_cols, drop = FALSE]
    
    original_data$Tipo <- "Original"
    smote_data$Tipo <- "Sintética"
    
    dataset <- dplyr::bind_rows(original_data, smote_data)
    
    req(input$pcs_to_show)
    selected_input <- input$pcs_to_show
    if (is.null(selected_input) || length(selected_input) == 0) return(NULL)
    
    num_vars <- names(dataset)[sapply(dataset, is.numeric)]
    num_vars <- setdiff(num_vars, "SampleID")
    if (length(num_vars) < 2) return(NULL)
    
    
    ####################ACRÉSCIMO DE VALIDAÇÃO PARA PRCOMP QUANDO NÃO TINHA VARIÁVEL CATEGÓRICA 29/05/2025
    
    dados_filtrados <- dataset[, num_vars, drop = FALSE]
    
    # Proteção contra NA, Inf, -Inf
    if (any(!is.finite(as.matrix(dados_filtrados)))) return()
    
    # Executa a PCA temporária
    pca_temp <- prcomp(dados_filtrados, center = TRUE, scale. = TRUE)
    
    ## pca <- prcomp(dataset[, num_vars], center = TRUE, scale. = TRUE)
    
    ####################ACRÉSCIMO DE VALIDAÇÃO PARA PRCOMP QUANDO NÃO TINHA VARIÁVEL CATEGÓRICA 29/05/2025
    
    
    
    #scores <- as.data.frame(pca$x)
    scores <- as.data.frame(pca_temp$x)  #ALTERAÇÃO DO OBJETO PCA EM PRCOMP EM 29/05/2025
    
    scores$Class <- dataset$class
    
    original_classes <- unique(gsub(" Sintética$", "", scores$Class[!grepl("Sintética", scores$Class)]))
    synthetic_classes <- paste(original_classes, "Sintética")
    ordered_levels <- c(original_classes, synthetic_classes)
    scores$Class <- factor(scores$Class, levels = ordered_levels)
    
    # pcs_all <- colnames(pca$x)
    #  var_exp <- summary(pca)$importance["Proportion of Variance", ]
    
    pcs_all <- colnames(pca_temp$x)  #ALTERAÇÃO DO OBJETO PCA EM PRCOMP EM 29/05/2025
    var_exp <- summary(pca_temp)$importance["Proportion of Variance", ]  #ALTERAÇÃO DO OBJETO PCA EM PRCOMP EM 29/05/2025
    
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
        title = "Boxplot dos Scores (PCA)",
        xaxis = list(title = "Componentes Principais"),
        yaxis = list(title = "Valores dos Scores"),
        boxmode = "group"
      )
  })
  
  ##ACRÉSCIMO DO SMOTENC EM 28/05/2025
  
  ##ACRÉSCIMO DE BORDERLINE SMOTE EM 05/06/2025
  
  ##ACRÉSCIMO DE SVM SMOTE EM 06/06/2025
  
  ##ACRÉSCIMO DE ADASYN EM 08/06/2025
  
  ##ACRÉSCIMO DE RANDOM UPSAMPLING EM 09/06/2025
  
  
  ###############ATUALIZADO EM 10/02/2025
  
  ## VISUALIZAÇÃO: BOXPLOT DAS VARIÁVEIS
  ## MODIFICAÇÃO EM 06/05/2025 - SELEÇÃO DE VARIÁVEIS PARA VISUALIZAR NO BOXPLOT
  ## MODIFICAÇÃO EM 19/05/2025 - ACRÉSCIMO DE SMOTENC
  ## MODIFICAÇÃO EM 05/06/2025 - ACRÉSCIMO DE BORDERLINE SMOTE
  ## MODIFICAÇÃO EM 06/06/2025 - ACRÉSCIMO DE SVM SMOTE
  ## MODIFICAÇÃO EM 08/06/2025 - ACRÉSCIMO DE ADASYN
  ## MODIFICAÇÃO EM 09/06/2025 - ACRÉSCIMO DE RANDOM UPSAMPLING
  
  
  
  # Atualizando opções de variáveis numéricas para o boxplot
  
  observeEvent(c(runSMOTE(), runSMOTENC(), runBorderlineSMOTE(),runSVM_SMOTE(), runADASYN(), runRU() ), {
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
    }
    
    
    if (is.null(dataset)) return()
    
    original <- as.data.frame(data())
    synthetic <- as.data.frame(dataset)
    
    common_columns <- intersect(names(original), names(synthetic))
    numeric_columns <- common_columns[sapply(original[, common_columns], is.numeric)]
    
    updateSelectInput(
      session,
      inputId = "boxplot_vars",
      choices = c("All Variables" = "ALL", numeric_columns),
      selected = NULL
    )
  })
  
  # Boxplot único para qualquer tipo de SMOTE
  
  output$boxplot_output <- renderPlotly({
    req(data())
    
    synthetic <- NULL
    if (isTRUE(runSMOTENC()) && !is.null(SMOTENC_Data())) {
      synthetic <- SMOTENC_Data()$data
    } else if (isTRUE(runSMOTE()) && !is.null(SMOTE_Data())) {
      synthetic <- SMOTE_Data()$data
    } else if (isTRUE(runBorderlineSMOTE()) && !is.null(BorderlineSMOTE_Data())) {
      synthetic <- BorderlineSMOTE_Data()$data
    } else if (isTRUE(runSVM_SMOTE()) && !is.null(SVM_SMOTE_Data())) {
      synthetic <- SVM_SMOTE_Data()$data 
    } else if (isTRUE(runADASYN()) && !is.null(ADASYN_Data())) {
      synthetic <- ADASYN_Data()$data 
    } else if (isTRUE(runRU()) && !is.null(RU_Data())) {
      synthetic <- RU_Data()$data  
    
    }
    
    req(synthetic, input$boxplot_vars)
    
    # Gerando o boxplot
    
    generate_boxplot_independente(data(), synthetic, isolate(input$boxplot_vars))
  })
  
  # Função segura e isolada para gerar o boxplot
  
  generate_boxplot_independente <- function(original, synthetic, selected_vars) {
    if (is.null(selected_vars) || length(selected_vars) == 0) return(NULL)
    
    common_columns <- intersect(names(original), names(synthetic))
    numeric_columns <- common_columns[sapply(original[, common_columns], is.numeric)]
    
    selected_vars <- if ("ALL" %in% selected_vars) numeric_columns else intersect(selected_vars, numeric_columns)
    if (length(selected_vars) == 0) return(NULL)
    
    normalize <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
    
    original_copy <- original
    synthetic_copy <- synthetic
    
    original_copy[, selected_vars] <- lapply(original_copy[, selected_vars, drop = FALSE], normalize)
    synthetic_copy[, selected_vars] <- lapply(synthetic_copy[, selected_vars, drop = FALSE], normalize)
    
    original_copy$type <- "Original"
    synthetic_copy$type <- "Synthetic"
    
    combined <- rbind(
      original_copy[, c(selected_vars, "type"), drop = FALSE],
      synthetic_copy[, c(selected_vars, "type"), drop = FALSE]
    )
    
    melted <- reshape2::melt(combined, id.vars = "type", variable.name = "variable", value.name = "value")
    
    plot_ly(
      data = melted, x = ~variable, y = ~value, color = ~type, type = "box",
      colors = c("Original" = "darkblue", "Synthetic" = "lightgreen")
    ) %>%
      layout(
        title = "Boxplot das Variáveis Normalizadas",
        xaxis = list(title = "Variáveis"),
        yaxis = list(title = "Valores Normalizados"),
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
  ##        title = "Boxplot das Variáveis Normalizadas",
  ##        xaxis = list(title = "Variáveis"),
  ##        yaxis = list(title = "Valores Normalizados"),
  ##        boxmode = "group"
  ##      )
  ##  })
  
  
  
  ##ACRÉSCIMO DE PLOT COMPARATIVO DAS AMOSTRAS ORIGINAIS E SINTÉTICAS NA GUIA DIAGNÓSTICO EM 27/03/2025
  ##MODIFICADO EM 09/04/2025 - EM WHICH SAMPLES NÃO ESTAVA COMPUTANDO A QUANTIDADE CERTA DE AMOSTRAS APÓS SMOTE
  ##MODIFICAÇÃO EM 10/04/2025 - AS AMOSTRAS SINTÉTICAS QUANDO PLOTADAS SOZINHAS EM WHICH SAMPLES AGORA FUNCIONAM
  
  
  
  
  ########BLOCO MODIFICADO EM 18/05/2025 - ACRÉSCIMO DE SMOTE_NC EM PLOTS COMPARATIVOS
  ########BLOCO MODIFICADO EM 05/06/2025 - ACRÉSCIMO DE BORDERLINE SMOTE EM PLOTS COMPARATIVOS
  ########BLOCO MODIFICADO EM 06/06/2025 - ACRÉSCIMO DE SVM SMOTE EM PLOTS COMPARATIVOS
  ########BLOCO MODIFICADO EM 08/06/2025 - ACRÉSCIMO DE ADASYN EM PLOTS COMPARATIVOS
  ########BLOCO MODIFICADO EM 09/06/2025 - ACRÉSCIMO DE RANDOM UPSAMPLING EM PLOTS COMPARATIVOS
  
  
  
  
  # VALORES REATIVOS PRINCIPAIS
  
  original_data_fixed <- reactiveVal()
  combined_data <- reactiveVal()
  
  # ATUALIZANDO combined_data() para SMOTE, SMOTE-NC, BORDERLINE SMOTE, SVM SMOTE, ADASYN E RANDOM UPSAMPLING
  
  observe({
    req(filedata())
    
    # DETECTANDO SE SMOTE, SMOTE-NC, BORDERLINE SMOTE, SVM SMOTE, ADASYN OU RANDOM UPSAMPLING FOI EXECUTADO POR ÚLTIMO
    
    smote_data <- NULL
    class_col <- NULL
    
 #   if (isTRUE(runSMOTENC()) && !is.null(SMOTENC_Data())) {
  #    smote_data <- SMOTENC_Data()$syn_data
  #    class_col <- input$target_nc
  #  } else if (isTRUE(runSMOTE()) && !is.null(SMOTE_Data())) {
  #    smote_data <- SMOTE_Data()$syn_data
  #    class_col <- input$target
  #  }
    
    
    
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
 
    }
    
  
    req(class_col)
    original_data <- filedata()
    
    # Corrigindo nome da coluna de classe para "class"
    
    names(original_data)[names(original_data) == class_col] <- "class"
    feature_cols <- setdiff(names(original_data), "class")
    original_data <- original_data[, c(feature_cols, "class"), drop = FALSE]
    
    if (!is.null(smote_data)) {
      names(smote_data)[names(smote_data) == class_col] <- "class"
      smote_data <- smote_data[, c(feature_cols, "class"), drop = FALSE]
      smote_data$class <- paste(smote_data$class, "Sintética")
      smote_data$fake_cat <- NULL  # Remove fake_cat se presente
    }
    
    # Armazenando original_data uma única vez
    
    if (is.null(original_data_fixed())) {
      original_data_fixed(original_data)
    }
    
    prev_synthetics <- if (!is.null(combined_data())) {
      combined_data()[grepl("Sintética", combined_data()$class), ]
    } else {
      NULL
    }
    
    # Removendo duplicadas
    
    if (!is.null(prev_synthetics) && !is.null(smote_data) && nrow(smote_data) > 0) {
      new_synthetics <- dplyr::anti_join(
        mutate(smote_data, .id = seq_len(nrow(smote_data))),
        prev_synthetics[, c(feature_cols, "class"), drop = FALSE],
        by = c(feature_cols, "class")
      )
      smote_data <- new_synthetics[, setdiff(names(new_synthetics), ".id"), drop = FALSE]
      
      
      ########TRECHO RETIRADO EM 19/05/2025 - NÃO ESTÁ FUNCIONAL#############################
      #  if (nrow(smote_data) == 0) {
      #   showNotification("Nenhuma nova amostra sintética foi adicionada nesta rodada — todas já existiam nas execuções anteriores.",
      #                   type = "warning", duration = 6)
      #  }
      ########TRECHO RETIRADO EM 19/05/2025 - NÃO ESTÁ FUNCIONAL#############################
      
    }
    
    all_names <- union(names(original_data_fixed()), union(names(prev_synthetics), names(smote_data)))
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
    
    combined_all <- rbind(original_pad, prev_synthetics_pad, synthetic_pad)
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
    df$Tipo <- ifelse(grepl("Sintética", df$class), "Sintética", "Original")
    
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
        Tipo = ifelse(grepl("Sintética", mat$class), "Sintética", "Original")
      )
      mat <- as.matrix(mat[, vars])
    }
    
    df$class <- as.character(df$class)
    df$Tipo <- ifelse(grepl("Sintética", df$class), "Sintética", "Original")
    
    original_classes <- unique(gsub(" Sintética$", "", df$class[grepl("Sintética", df$class)]))
    orig_classes_all <- unique(gsub(" Sintética$", "", combined_data()$class[!grepl("Sintética", combined_data()$class)]))
    all_classes <- unique(c(orig_classes_all, paste0(orig_classes_all, " Sintética")))
    ordered_levels <- c(rbind(orig_classes_all, paste0(orig_classes_all, " Sintética")))
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
        dash_style <- ifelse(grepl("Sintética", classe), "dot", "solid")
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
                       text = ~paste("Classe:", class,
                                     "<br>Tipo:", Tipo,
                                     "<br>SampleID:", SampleID,
                                     "<br>Variável:", Variavel,
                                     "<br>Valor:", round(Valor, 3)))
        legenda_marcada <- c(legenda_marcada, classe)
      }
      p <- layout(p,
                  title = titulo,
                  xaxis = list(title = "Variável", type = "category"),
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
        dash_style <- ifelse(grepl("Sintética", classe_i), "dot", "solid")
        p <- add_trace(p,
                       data = df_sub,
                       x = ~Variavel,
                       y = ~Valor,
                       type = 'scatter', mode = 'lines',
                       line = list(color = cor_classe, width = 1.5, dash = dash_style),
                       name = NULL,
                       showlegend = FALSE,
                       hoverinfo = "text",
                       text = ~paste("Classe:", class,
                                     "<br>Tipo:", Tipo,
                                     "<br>SampleID:", SampleID,
                                     "<br>Variável:", Variavel,
                                     "<br>Valor:", round(Valor, 3)))
      }
      p <- layout(p,
                  title = titulo,
                  xaxis = list(title = "Variável", type = "category"),
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
  
  
  
  
  
  ##ACRÉSCIMO DE PLOT COMPARATIVO DAS AMOSTRAS ORIGINAIS E SINTÉTICAS NA GUIA DIAGNÓSTICO EM 27/03/2025
  
  #######ACRÉSCIMO DA DIVERGÊNCIA DE JENSEN-SHANNON EM 28/03/2025#####
  
  ####### DIVERGÊNCIA DE JENSEN-SHANNON – SMOTE + SMOTE-NC - MODIFICADA EM 19/05/2025 #######
  
  #ACRÉSCIMO DE BORDERLINE SMOTE EM 05/06/2025
  #ACRÉSCIMO DE SVM SMOTE EM 06/06/2025
  #ACRÉSCIMO DE ADASYN EM 08/06/2025
  #ACRÉSCIMO DE RANDOM UPSAMPLING EM 09/06/2025
  
  
  
  
  # Função JS
  
  js_divergence <- function(x, y, bins = 30) {
    if (length(x) < 2 || length(y) < 2) return(NA)
    
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
  
  # Reactive com suporte a SMOTE, SMOTE-NC, BORDERLINE SMOTE, SVM SMOTE, ADASYN E RANDOM UPSAMPLING
  
  js_div_data <- reactive({
    req(filedata())
    
    original_data <- as.data.frame(filedata())
    smote_data <- NULL
    class_col <- NULL
    
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
         
     
    }
    else {
    return(NULL)
    }
    
  
    
    if (is.null(smote_data) || ncol(smote_data) == 0 || is.null(class_col) || !(class_col %in% names(original_data))) {
      return(NULL)
    }
    
    names(original_data)[names(original_data) == class_col] <- "class"
    names(smote_data)[names(smote_data) == class_col] <- "class"
    smote_data$fake_cat <- NULL
    smote_data$class <- paste(smote_data$class, "Sintética")
    
    feature_cols <- intersect(names(original_data), names(smote_data))
    original_data <- original_data[, feature_cols, drop = FALSE]
    smote_data <- smote_data[, feature_cols, drop = FALSE]
    
    original_data$Tipo <- "Original"
    smote_data$Tipo <- "Sintética"
    
    df <- dplyr::bind_rows(original_data, smote_data)
    df$class <- as.character(df$class)
    df$Tipo <- as.character(df$Tipo)
    df$classe_base <- gsub(" Sintética$", "", df$class)
    
    # Adaptação para o método usado
    
    if (isTRUE(runSMOTENC())) {
      classes <- unique(smote_data$class)
      classes <- gsub(" Sintética$", "", classes)
    } else {
      classes <- unique(df$classe_base)
    }
    
    
    if (isTRUE(runBorderlineSMOTE())) {
      classes <- unique(smote_data$class)
      classes <- gsub(" Sintética$", "", classes)
    } else {
      classes <- unique(df$classe_base)
    } 
    
    
    if (isTRUE(runSVM_SMOTE())) {
      classes <- unique(smote_data$class)
      classes <- gsub(" Sintética$", "", classes)
    } else {
      classes <- unique(df$classe_base)
    }
    
    
    
    if (isTRUE(runADASYN())) {
      classes <- unique(smote_data$class)
      classes <- gsub(" Sintética$", "", classes)
    } else {
      classes <- unique(df$classe_base)
    }
    

    
    if (isTRUE(runRU())) {
      classes <- unique(smote_data$class)
      classes <- gsub(" Sintética$", "", classes)
    } else {
      classes <- unique(df$classe_base)
    }
    
    
    
    variaveis <- setdiff(names(df), c("class", "Tipo", "classe_base"))
    if (length(variaveis) == 0) return(NULL)
    
    resultado <- data.frame()
    for (cl in classes) {
      df_o <- df %>% dplyr::filter(classe_base == cl & Tipo == "Original")
      df_s <- df %>% dplyr::filter(classe_base == cl & Tipo == "Sintética")
      
      if (nrow(df_o) > 1 && nrow(df_s) > 1) {
        for (var in variaveis) {
          js <- js_divergence(df_o[[var]], df_s[[var]])
          resultado <- rbind(resultado, data.frame(Classe = cl, Variável = var, Divergência = js))
        }
      }
    }
    
    resultado <- resultado[!is.na(resultado$Divergência), ]
    if (nrow(resultado) == 0) return(NULL)
    
    resultado$Classe <- factor(resultado$Classe)
    resultado$Variável <- factor(resultado$Variável, levels = unique(resultado$Variável))
    resultado
  })
  
  # Atualizando seletores
  
  observe({
    df <- js_div_data()
    req(df)
    updateSelectInput(session, "classe_detalhe", choices = unique(df$Classe))
    updateSelectInput(session, "variavel_detalhe", choices = unique(df$Variável))
  })
  
  # Gráfico principal JS
  
  output$js_divergence_plot <- renderPlotly({
    df <- js_div_data()
    req(df)
    
    df_wide <- tidyr::pivot_wider(df, names_from = Classe, values_from = Divergência)
    variaveis <- df_wide$Variável
    classe_cols <- names(df_wide)[-1]
    cores <- RColorBrewer::brewer.pal(max(3, length(classe_cols)), "Set1")
    
    plot <- plot_ly()
    for (i in seq_along(classe_cols)) {
      classe_nome <- classe_cols[i]
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
            "<b>Classe:</b> ", classe_nome,
            "<br><b>Comparada com:</b> ", classe_nome, " Sintética",
            "<br><b>Variável:</b> ", variaveis,
            "<br><b>Divergência JS:</b> ", round(df_wide[[classe_nome]], 4)
          ),
          color = I(cores[(i - 1) %% length(cores) + 1])
        )
    }
    
    plot %>% layout(
      title = list(text = "Divergência de Jensen-Shannon: Originais vs Sintéticas", font = list(size = 18)),
      xaxis = list(title = "Variável", tickangle = -45),
      yaxis = list(title = "Divergência JS", range = c(0, 1)),
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
    
    original_vals <- df %>% dplyr::filter(Classe == classe, Variável == variavel)
    if (nrow(original_vals) == 0) return(NULL)
    
    # Reconstruindo os dados usados no cálculo para visualização
    
    original_data <- as.data.frame(filedata())
    smote_data <- NULL
    class_col <- NULL
    
 #   if (isTRUE(runSMOTENC()) && !is.null(SMOTENC_Data()) && !is.null(input$target_nc)) {
 #      smote_data <- SMOTENC_Data()$syn_data
 #      class_col <- input$target_nc
 #    } else if (isTRUE(runSMOTE()) && !is.null(SMOTE_Data()) && !is.null(input$target)) {
 #     smote_data <- SMOTE_Data()$syn_data
 #      class_col <- input$target
 #   } else {
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
    }
    
    else if (isTRUE(runSVM_SMOTE()) && !is.null(SVM_SMOTE_Data()) && !is.null(input$target_svm)) {
      smote_data <- SVM_SMOTE_Data()$syn_data
      class_col <- input$target_svm
      
    }  
    else if (isTRUE(runADASYN()) && !is.null(ADASYN_Data()) && !is.null(input$target_adasyn)) {
      smote_data <- ADASYN_Data()$syn_data
      class_col <- input$target_adasyn 
   
    } 
    else if (isTRUE(runRU()) && !is.null(RU_Data()) && !is.null(input$target_ru)) {
      smote_data <- RU_Data()$syn_data
      class_col <- input$target_ru  
      
   
      
    }
    else {
      return(NULL)
    }  
    
    
    if (is.null(smote_data) || !(class_col %in% names(original_data))) return(NULL)
    
    names(original_data)[names(original_data) == class_col] <- "class"
    names(smote_data)[names(smote_data) == class_col] <- "class"
    smote_data$class <- paste(smote_data$class, "Sintética")
    smote_data$fake_cat <- NULL
    
    feature_cols <- intersect(names(original_data), names(smote_data))
    original_data <- original_data[, feature_cols, drop = FALSE]
    smote_data <- smote_data[, feature_cols, drop = FALSE]
    
    original_data$Tipo <- "Original"
    smote_data$Tipo <- "Sintética"
    
    df_full <- dplyr::bind_rows(original_data, smote_data)
    df_full$class <- as.character(df_full$class)
    df_full$classe_base <- gsub(" Sintética$", "", df_full$class)
    df_sub <- df_full %>% dplyr::filter(classe_base == classe)
    req(nrow(df_sub) > 0)
    
    tipo_var <- if (is.numeric(df_sub[[variavel]])) "numerica" else "categorica"
    
    if (tipo_var == "numerica") {
      plot_ly(df_sub, x = ~get(variavel), color = ~Tipo, type = "histogram",
              opacity = 0.6, nbinsx = 30) %>%
        layout(barmode = "overlay",
               title = paste("Distribuição de", variavel, "para", classe),
               xaxis = list(title = variavel),
               yaxis = list(title = "Contagem"))
    } else {
      df_counts <- df_sub %>%
        dplyr::group_by(Tipo, Valor = .data[[variavel]]) %>%
        dplyr::summarise(Freq = n(), .groups = "drop")
      
      plot_ly(df_counts, x = ~Valor, y = ~Freq, type = "bar", color = ~Tipo,
              barmode = "group") %>%
        layout(title = paste("Distribuição de", variavel, "para", classe),
               xaxis = list(title = variavel),
               yaxis = list(title = "Contagem"))
    }
  })
  
  
  #######ACRÉSCIMO DA DIVERGÊNCIA DE JENSEN-SHANNON EM 28/03/2025#####
  ####### DIVERGÊNCIA DE JENSEN-SHANNON – SMOTE + SMOTE-NC - MODIFICADA EM 19/05/2025 ####### 
  
  #ACRÉSCIMO DE BORDERLINE SMOTE EM 05/06/2025
  #ACRÉSCIMO DE SVM SMOTE EM 06/06/2025
  #ACRÉSCIMO DE ADASYN EM 08/06/2025
  #ACRÉSCIMO DE RANDOM UPSAMPLING EM 09/06/2025
  
  
  
#######ACRÉSCIMO DE GRÁFICO DE PROCRUSTES GENERALIZADO EM 29/03/2025##  
  
#######ACRÉSCIMO DE GRÁFICO DE PROCRUSTES GENERALIZADO – SMOTE + SMOTE-NC - MODIFICADA EM 28/05/2025 ##
#ACRÉSCIMO DE BORDERLINE SMOTE EM 05/06/2025
#ACRÉSCIMO DE SVM SMOTE EM 06/06/2025
#ACRÉSCIMO DE ADASYN EM 08/06/2025  
#ACRÉSCIMO DE RANDOM UPSAMPLING EM 09/06/2025
  
  output$grafico_procrustes_multiclasse <- renderPlotly({
    req(filedata())
    
    original_data <- as.data.frame(filedata())
    smote_data <- NULL
    class_col <- NULL
    
 #   if (isTRUE(runSMOTENC()) && !is.null(SMOTENC_Data())) {
 #      smote_data <- SMOTENC_Data()$syn_data
 #     class_col <- input$target_nc
 #    } else if (isTRUE(runSMOTE()) && !is.null(SMOTE_Data())) {
 #      smote_data <- SMOTE_Data()$syn_data
 #     class_col <- input$target
 #    }
    
    
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
      
  
    }  
  
    
  if (is.null(smote_data) || is.null(class_col) || !(class_col %in% names(original_data))) return(NULL)
    
    names(original_data)[names(original_data) == class_col] <- "class"
    names(smote_data)[names(smote_data) == class_col] <- "class"
    smote_data$class <- paste(smote_data$class, "Sintética")
    
    # Removendo colunas auxiliares
    
    colunas_aux <- c("fake_cat", "..group..", "..id..")
    smote_data <- smote_data[, setdiff(names(smote_data), colunas_aux), drop = FALSE]
    
    feature_cols <- intersect(names(original_data), names(smote_data))
    original_data <- original_data[, feature_cols, drop = FALSE]
    smote_data <- smote_data[, feature_cols, drop = FALSE]
    
    original_data$Tipo <- "Original"
    smote_data$Tipo <- "Sintética"
    
    dados <- dplyr::bind_rows(original_data, smote_data)
    dados$class <- as.character(dados$class)
    dados$classe_base <- gsub(" Sintética$", "", dados$class)
    dados$SampleID <- 1:nrow(dados)
    
    num_vars <- names(Filter(is.numeric, dados))
    num_vars <- setdiff(num_vars, "SampleID")
    validate(need(length(num_vars) >= 2, "Pelo menos 2 variáveis numéricas são necessárias."))
    
    todas_classes <- unique(dados$classe_base)
    p <- plot_ly()
    alguma_classe_foi_plotada <- FALSE
    
    for (classe in todas_classes) {
      originais <- subset(dados, Tipo == "Original" & classe_base == classe)
      sinteticas <- subset(dados, Tipo == "Sintética" & classe_base == classe)
      
      if (nrow(originais) >= 2 && nrow(sinteticas) >= 2) {
        n <- min(nrow(originais), nrow(sinteticas))
        
        vars_comuns <- intersect(names(originais), names(sinteticas))
        vars_comuns <- intersect(vars_comuns, num_vars)
        
        if (length(vars_comuns) >= 2) {
          X <- scale(originais[1:n, vars_comuns])
          Y <- scale(sinteticas[1:n, vars_comuns])
          colnames(Y) <- colnames(X)
          
          proc <- vegan::procrustes(X, Y, scale = TRUE)
          
          X_df <- as.data.frame(proc$X[, 1:2])
          Y_df <- as.data.frame(proc$Yrot[, 1:2])
          names(X_df) <- names(Y_df) <- c("Dim1", "Dim2")
          
          X_df$Tipo <- "Original"
          Y_df$Tipo <- "Sintética"
          X_df$ClasseLeg <- classe
          Y_df$ClasseLeg <- paste(classe, "Sintética")
          X_df$SampleID <- originais$SampleID[1:n]
          Y_df$SampleID <- sinteticas$SampleID[1:n]
          
          for (i in 1:n) {
            linha_df <- data.frame(
              Dim1 = c(X_df$Dim1[i], Y_df$Dim1[i]),
              Dim2 = c(X_df$Dim2[i], Y_df$Dim2[i])
            )
            
            p <- add_trace(p,
                           data = linha_df,
                           x = ~Dim1, y = ~Dim2,
                           type = "scatter", mode = "lines",
                           line = list(color = "rgba(100,100,100,0.3)", width = 1),
                           hoverinfo = "none",
                           showlegend = FALSE)
          }
          
          p <- add_trace(p,
                         data = X_df,
                         x = ~Dim1, y = ~Dim2,
                         type = "scatter", mode = "markers",
                         marker = list(size = 9),
                         name = unique(X_df$ClasseLeg),
                         hoverinfo = "text",
                         text = ~paste("Classe:", ClasseLeg, "<br>SampleID:", SampleID),
                         showlegend = TRUE)
          
          p <- add_trace(p,
                         data = Y_df,
                         x = ~Dim1, y = ~Dim2,
                         type = "scatter", mode = "markers",
                         marker = list(size = 9, symbol = "circle-open"),
                         name = unique(Y_df$ClasseLeg),
                         hoverinfo = "text",
                         text = ~paste("Classe:", ClasseLeg, "<br>SampleID:", SampleID),
                         showlegend = TRUE)
          
          alguma_classe_foi_plotada <- TRUE
        }
      }
    }
    
    validate(need(alguma_classe_foi_plotada, "Nenhuma classe possui dados suficientes para visualização."))
    
    p <- layout(p,
                title = "Procrustes Generalizado por classe",
                xaxis = list(title = "Dimensão 1"),
                yaxis = list(title = "Dimensão 2"),
                legend = list(
                  orientation = "v",
                  x = 1, xanchor = "left",
                  y = 1, yanchor = "top"
                ),
                margin = list(r = 220)) %>%
      config(displayModeBar = FALSE) %>%
      layout(colorway = RColorBrewer::brewer.pal(12, "Set1"))
    
    p
  })
  
  
  
  #######ACRÉSCIMO DE GRÁFICO DE PROCRUSTES GENERALIZADO EM 29/03/2025##
  
  #######ACRÉSCIMO DE GRÁFICO DE PROCRUSTES GENERALIZADO – SMOTE + SMOTE-NC - MODIFICADA EM EM 28/05/2025 ##
  
  
  # ACRÉSCIMO DE BORDERLINE SMOTE EM 05/06/2025
  
  # ACRÉSCIMO DE SVM SMOTE EM 06/06/2025
  
  # ACRÉSCIMO DE ADASYN EM 08/06/2025
  
  #ACRÉSCIMO DE RANDOM UPSAMPLING EM 09/06/2025
  
  
  
  ###ACRÉSCIMO DE GRÁFICO DA DISTÂNCIA DE MAHALANOBIS EM 29/03/2025###
  
  #ACRÉSCIMO DE SMOTE NC NO GRÁFICO DA DISTÂNCIA DE MAHALANOBIS EM 28/05/2025
  #ACRÉSCIMO DE BORDERLINE SMOTE EM 05/06/2025
  #ACRÉSCIMO DE SVM SMOTE EM 06/06/2025
  #ACRÉSCIMO DE ADASYN EM 08/06/2025
  #ACRÉSCIMO DE RANDOM UPSAMPLING EM 09/06/2025
  
  # Reativo compartilhado entre gráfico e tabela
  
  mahalanobis_resultados <- reactive({
    original_data <- as.data.frame(filedata())
    smote_data <- NULL
    class_col <- NULL
    
   # if (isTRUE(runSMOTENC()) && !is.null(SMOTENC_Data()) && !is.null(input$target_nc)) {
  #    smote_data <- SMOTENC_Data()$syn_data
  #    class_col <- input$target_nc
  #  } else if (isTRUE(runSMOTE()) && !is.null(SMOTE_Data()) && !is.null(input$target)) {
  #    smote_data <- SMOTE_Data()$syn_data
  #    class_col <- input$target
  #  } else {
  #    return(data.frame())
  #  }
    
    
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
      
      
      
    } else {
      return(data.frame())
    }
    
   if (is.null(smote_data) || !(class_col %in% names(original_data))) return(data.frame())
    
    names(original_data)[names(original_data) == class_col] <- "class"
    names(smote_data)[names(smote_data) == class_col] <- "class"
    smote_data$class <- paste(smote_data$class, "Sintética")
    smote_data$fake_cat <- NULL
    
    feature_cols <- intersect(names(original_data), names(smote_data))
    original_data <- original_data[, feature_cols, drop = FALSE]
    smote_data <- smote_data[, feature_cols, drop = FALSE]
    
    original_data$Tipo <- "Original"
    smote_data$Tipo <- "Sintética"
    
    dados <- dplyr::bind_rows(original_data, smote_data)
    dados$class <- as.character(dados$class)
    dados$Tipo <- ifelse(grepl(" Sintética$", dados$class), "Sintética", "Original")
    dados$classe_base <- gsub(" Sintética$", "", dados$class)
    
    num_vars <- names(Filter(is.numeric, dados))
    num_vars <- setdiff(num_vars, "SampleID")
    validate(need(length(num_vars) >= 2, "Pelo menos 2 variáveis numéricas são necessárias."))
    
    classes <- unique(dados$classe_base)
    resultados <- data.frame()
    
    for (classe in classes) {
      dados_ori <- subset(dados, Tipo == "Original" & classe_base == classe)
      dados_syn <- subset(dados, Tipo == "Sintética" & classe_base == classe)
      
      if (nrow(dados_ori) >= 2 && nrow(dados_syn) >= 2) {
        vars_comuns <- intersect(names(dados_ori), names(dados_syn))
        vars_comuns <- intersect(vars_comuns, num_vars)
        
        if (length(vars_comuns) >= 2) {
          X_ori <- dados_ori[, vars_comuns]
          X_syn <- dados_syn[, vars_comuns]
          
          mu_ori <- colMeans(X_ori)
          mu_syn <- colMeans(X_syn)
          cov_pool <- (cov(X_ori) + cov(X_syn)) / 2
          
          dist <- sqrt(mahalanobis(mu_syn, center = mu_ori, cov = cov_pool))
          
          # Hotelling T²
          n1 <- nrow(X_ori)
          n2 <- nrow(X_syn)
          S_pooled <- ((n1 - 1) * cov(X_ori) + (n2 - 1) * cov(X_syn)) / (n1 + n2 - 2)
          diff_mean <- as.matrix(mu_ori - mu_syn)
          t2 <- (n1 * n2) / (n1 + n2) * t(diff_mean) %*% solve(S_pooled) %*% diff_mean
          t2 <- as.numeric(t2)
          p_val <- 1 - pf(t2 * (n1 + n2 - length(vars_comuns) - 1) / ((n1 + n2 - 2) * length(vars_comuns)),
                          df1 = length(vars_comuns),
                          df2 = n1 + n2 - length(vars_comuns) - 1)
          
          resultados <- rbind(resultados, data.frame(
            Classe = classe,
            Mahalanobis_Dist = round(dist, 4),
            Hotelling_T2 = round(t2, 4),
            p_valor = signif(p_val, 4)
          ))
        }
      }
    }
    
    resultados
  })
  
  # Gráfico interativo
  
  output$grafico_mahalanobis_por_classe <- renderPlotly({
    resultados <- mahalanobis_resultados()
    validate(need(nrow(resultados) > 0, "Não há dados suficientes para análise."))
    
    limite_aceitavel <- 1.5
    cores <- RColorBrewer::brewer.pal(12, "Set1")
    resultados$Classe <- factor(resultados$Classe, levels = unique(resultados$Classe))
    
    p <- plot_ly()
    
    for (i in 1:nrow(resultados)) {
      p <- p %>%
        add_trace(
          x = resultados$Classe[i],
          y = resultados$Mahalanobis_Dist[i],
          type = "bar",
          name = as.character(resultados$Classe[i]),
          showlegend = TRUE,
          marker = list(color = cores[(i - 1) %% length(cores) + 1]),
          text = paste("Mahalanobis_Dist:", resultados$Mahalanobis_Dist[i]),
          textposition = "auto",
          hoverinfo = "text"
        )
    }
    
    p <- layout(p,
                title = "Distância de Mahalanobis por classe",
                xaxis = list(title = "Classe"),
                yaxis = list(title = "Distância Mahalanobis"),
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
                    text = "Limite aceitável",
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
  #              "Resultados por Classe - Hotelling T² e Distância de Mahalanobis"
  #           ))
  # })
  
  
  #ACRÉSCIMO DE SMOTE NC NO GRÁFICO DA DISTÂNCIA DE MAHALANOBIS EM 28/05/2025
  #ACRÉSCIMO DE BORDERLINE SMOTE EM 05/06/2025
  #ACRÉSCIMO DE SVM SMOTE EM 06/06/2025
  #ACRÉSCIMO DE ADASYN EM 08/06/2025
  #ACRÉSCIMO DE RANDOM UPSAMPLING EM 09/06/2025
  
  ###ACRÉSCIMO DE GRÁFICO DA DISTÂNCIA DE MAHALANOBIS EM 29/03/2025##
  
  
  
  #######ACRÉSCIMO DE TESTE DE KOLMOGOROV SMIRNOV EM 28/03/2025#######
  
  #######ACRÉSCIMO DE SMOTE_NC EM TESTE DE KOLMOGOROV SMIRNOV EM 29/05/2025 #######
  #######ACRÉSCIMO DE BORDERLINE SMOTE EM TESTE DE KOLMOGOROV SMIRNOV EM 05/06/2025 #######
  #######ACRÉSCIMO DE SVM SMOTE EM TESTE DE KOLMOGOROV SMIRNOV EM 06/06/2025 #######
  #######ACRÉSCIMO DE ADASYN EM TESTE DE KOLMOGOROV SMIRNOV EM 08/06/2025 #######
  #######ACRÉSCIMO DE RANDOM UPSAMPLING EM TESTE DE KOLMOGOROV SMIRNOV EM 09/06/2025 #######
  
  output$ks_por_classe <- renderDT({
    original_data <- as.data.frame(filedata())
    smote_data <- NULL
    class_col <- NULL
    
  #  if (isTRUE(runSMOTENC()) && !is.null(SMOTENC_Data()) && !is.null(input$target_nc)) {
  #    smote_data <- SMOTENC_Data()$syn_data
  #    class_col <- input$target_nc
  #  } else if (isTRUE(runSMOTE()) && !is.null(SMOTE_Data()) && !is.null(input$target)) {
  #    smote_data <- SMOTE_Data()$syn_data
  #    class_col <- input$target
  #  } else {
  #    return(data.frame())
  #  }
    
    
    
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
    
    } else {
      return(data.frame())
    }  
    
    
    if (is.null(smote_data) || !(class_col %in% names(original_data))) return(data.frame())
    
    names(original_data)[names(original_data) == class_col] <- "class"
    names(smote_data)[names(smote_data) == class_col] <- "class"
    smote_data$class <- paste(smote_data$class, "Sintética")
    smote_data$fake_cat <- NULL
    
    feature_cols <- intersect(names(original_data), names(smote_data))
    original_data <- original_data[, feature_cols, drop = FALSE]
    smote_data <- smote_data[, feature_cols, drop = FALSE]
    
    original_data$Tipo <- "Original"
    smote_data$Tipo <- "Sintética"
    
    dados <- dplyr::bind_rows(original_data, smote_data)
    dados$class <- as.character(dados$class)
    dados$Tipo <- ifelse(grepl(" Sintética$", dados$class), "Sintética", "Original")
    dados$classe_base <- gsub(" Sintética$", "", dados$class)
    
    num_vars <- names(Filter(is.numeric, dados))
    num_vars <- setdiff(num_vars, "SampleID")
    validate(need(length(num_vars) >= 2, "Pelo menos 2 variáveis numéricas são necessárias."))
    
    classes <- unique(dados$classe_base)
    resultados <- data.frame()
    
    for (cl in classes) {
      df_o <- dados %>% filter(classe_base == cl & Tipo == "Original")
      df_s <- dados %>% filter(classe_base == cl & Tipo == "Sintética")
      
      if (nrow(df_o) >= 2 && nrow(df_s) >= 2) {
        vec_o <- unlist(df_o[num_vars])
        vec_s <- unlist(df_s[num_vars])
        ks <- suppressWarnings(ks.test(vec_o, vec_s))
        
        resultados <- rbind(resultados, data.frame(
          Classe = cl,
          p_valor = signif(ks$p.value, 4)
        ))
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
  
  
  
  #######ACRÉSCIMO DE TESTE DE KOLMOGOROV SMIRNOV EM 28/03/2025#######
  
  
  ######ACRÉSCIMO DE TESTE DE PROCRUSTE GENERALIZADO EM 28/03/2025####
  
  ######ACRÉSCIMO DE SMOTE_NC EM TESTE DE PROCRUSTE GENERALIZADO EM 29/05/2025######
  
  ######ACRÉSCIMO DE BORDERLINE SMOTE EM TESTE DE PROCRUSTE GENERALIZADO EM 05/06/2025######
  
  ######ACRÉSCIMO DE SVM SMOTE EM TESTE DE PROCRUSTE GENERALIZADO EM 06/06/2025######
  
  ######ACRÉSCIMO DE ADASYN EM TESTE DE PROCRUSTE GENERALIZADO EM 08/06/2025######
  
  ######ACRÉSCIMO DE RANDOM UPSAMPLING EM TESTE DE PROCRUSTE GENERALIZADO EM 09/06/2025######
  
  
  
  
  output$procrustes_tabela_teste <- renderDT({
    original_data <- as.data.frame(filedata())
    smote_data <- NULL
    class_col <- NULL
    
  #  if (isTRUE(runSMOTENC()) && !is.null(SMOTENC_Data()) && !is.null(input$target_nc)) {
   #   smote_data <- SMOTENC_Data()$syn_data
    #  class_col <- input$target_nc
  #  } else if (isTRUE(runSMOTE()) && !is.null(SMOTE_Data()) && !is.null(input$target)) {
  #    smote_data <- SMOTE_Data()$syn_data
  #    class_col <- input$target
  #  } else {
  #    return(data.frame())
  #  }
    
    
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
      
      
    } else {
      return(data.frame())
    }   
    
    
    
    if (is.null(smote_data) || !(class_col %in% names(original_data))) return(data.frame())
    
    # Padronizando nomes de colunas
    
    names(original_data)[names(original_data) == class_col] <- "class"
    names(smote_data)[names(smote_data) == class_col] <- "class"
    
    smote_data$class <- paste(smote_data$class, "Sintética")
    smote_data$fake_cat <- NULL  # Removendo colunas auxiliares, se existirem
    
    # Alinhando colunas numéricas
    
    feature_cols <- intersect(names(original_data), names(smote_data))
    original_data <- original_data[, feature_cols, drop = FALSE]
    smote_data <- smote_data[, feature_cols, drop = FALSE]
    
    original_data$Tipo <- "Original"
    smote_data$Tipo <- "Sintética"
    
    dados <- dplyr::bind_rows(original_data, smote_data)
    dados$SampleID <- 1:nrow(dados)
    dados$class <- as.character(dados$class)
    dados$Tipo <- ifelse(grepl(" Sintética$", dados$class), "Sintética", "Original")
    dados$classe_base <- gsub(" Sintética$", "", dados$class)
    
    num_vars <- names(Filter(is.numeric, dados))
    num_vars <- setdiff(num_vars, "SampleID")
    validate(need(length(num_vars) >= 2, "É necessário ao menos 2 variáveis numéricas para o teste de Procrustes."))
    
    resultados <- data.frame()
    
    for (classe in unique(dados$classe_base)) {
      originais <- subset(dados, Tipo == "Original" & classe_base == classe)
      sinteticas <- subset(dados, Tipo == "Sintética" & classe_base == classe)
      
      if (nrow(originais) >= 2 && nrow(sinteticas) >= 2) {
        n <- min(nrow(originais), nrow(sinteticas))
        
        X <- scale(originais[1:n, num_vars])
        Y <- scale(sinteticas[1:n, num_vars])
        
        test <- vegan::protest(X, Y, permutations = 999)
        
        resultados <- rbind(resultados, data.frame(
          Classe = classe,
          # Correlacao = round(test$t0, 4),  ## RETIRADO PARA EVITAR CONFUSÃO
          p_valor = signif(test$signif, 4)
        ))
      }
    }
    
    if (nrow(resultados) == 0) return(NULL)
    
    datatable(resultados, rownames = FALSE, options = list(
      pageLength = 10,
      dom = 'tip'
    ))
  })
  
  
  ##obs: https://cran.r-project.org/web/packages/vegan/vegan.pdf
  
  ######ACRÉSCIMO DE TESTE DE PROCRUSTE GENERALIZADO EM 28/03/2025####
  
  
  
  ######ACRÉSCIMO DE SMOTE_NC EM TESTE DE PROCRUSTE GENERALIZADO EM 29/05/2025######
  
  ######ACRÉSCIMO DE BORDERLINE SMOTE EM TESTE DE PROCRUSTE GENERALIZADO EM 05/06/2025######
  
  ######ACRÉSCIMO DE SVM SMOTE EM TESTE DE PROCRUSTE GENERALIZADO EM 06/06/2025######
  
  ######ACRÉSCIMO DE ADASYN EM TESTE DE PROCRUSTE GENERALIZADO EM 08/06/2025######
  
  ######ACRÉSCIMO DE RANDOM UPSAMPLING EM TESTE DE PROCRUSTE GENERALIZADO EM 09/06/2025######
  
  
  
  
  
  ##ACRÉSCIMO DE T² DE HOTELLING E DIST.MAHALANOBIS EM 29/03/2025##
  
  ###MODIFICAÇÃO EM 30/03/2025 - ACRÉSCIMO DE TESTE DE NORMALIDADE MULTIVARIADA
  
  
  ##ACRÉSCIMO DO TESTE DE ROYSTON(EXTENSÃO MULTIVARIADA DO TESTE DE SHAPIRO-WILK) EM 02/05/2025
  #observeEvent(input$run_hotelling, {
  # req(smote_history$data)
  
  ##  output$hotelling_table <- renderDT({
  ##    req(smote_history$data)
  
  ######ACRÉSCIMO DE SMOTE_NC EM TESTE DE ROYSTON e TESTE DE NORMALIDADE MULTIVARIADA EM 29/05/2025
  
  ######ACRÉSCIMO DE BORDERLINE SMOTE EM TESTE DE ROYSTON e TESTE DE NORMALIDADE MULTIVARIADA EM 05/06/2025
  
  ######ACRÉSCIMO DE SVM SMOTE EM TESTE DE ROYSTON e TESTE DE NORMALIDADE MULTIVARIADA EM 06/06/2025
  
  ######ACRÉSCIMO DE ADASYN EM TESTE DE ROYSTON e TESTE DE NORMALIDADE MULTIVARIADA EM 08/06/2025
  
  ######ACRÉSCIMO DE RANDOM UPSAMPLING EM TESTE DE ROYSTON e TESTE DE NORMALIDADE MULTIVARIADA EM 09/06/2025
  
  
  
  observe({
    library(MVN)
    
    original_data <- as.data.frame(filedata())
    smote_data <- NULL
    class_col <- NULL
    
  #  if (isTRUE(runSMOTENC()) && !is.null(SMOTENC_Data()) && !is.null(input$target_nc)) {
   #   smote_data <- SMOTENC_Data()$syn_data
    #  class_col <- input$target_nc
  #  } else if (isTRUE(runSMOTE()) && !is.null(SMOTE_Data()) && !is.null(input$target)) {
   #   smote_data <- SMOTE_Data()$syn_data
  #    class_col <- input$target
  #  } else {
   #   return()
  #  }
    
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
      
      
    } else {
      return()
    }   
    
    
    if (is.null(smote_data) || !(class_col %in% names(original_data))) return()
    
    names(original_data)[names(original_data) == class_col] <- "class"
    names(smote_data)[names(smote_data) == class_col] <- "class"
    smote_data$class <- paste(smote_data$class, "Sintética")
    smote_data$fake_cat <- NULL
    
    feature_cols <- intersect(names(original_data), names(smote_data))
    original_data <- original_data[, feature_cols, drop = FALSE]
    smote_data <- smote_data[, feature_cols, drop = FALSE]
    
    original_data$Tipo <- "Original"
    smote_data$Tipo <- "Sintética"
    
    dados <- dplyr::bind_rows(original_data, smote_data)
    dados$SampleID <- 1:nrow(dados)
    dados$class <- as.character(dados$class)
    dados$Tipo <- ifelse(grepl("Sintética", dados$class), "Sintética", "Original")
    
    classes_originais <- unique(dados$class[!grepl("Sintética", dados$class)])
    classes_com_sintetica <- classes_originais[
      paste0(classes_originais, " Sintética") %in% dados$class
    ]
    
    if (length(classes_com_sintetica) == 0) {
      output$normalidade_table <- renderDataTable({
        datatable(data.frame(
          Classe = character(0),
          Estatistica_HZ = numeric(0),
          p_valor_HZ = numeric(0),
          Status_HZ = character(0),
          Estatistica_Royston = numeric(0),
          p_valor_Royston = numeric(0),
          Status_Royston = character(0)
        ), caption = "Nenhuma classe possui amostras sintéticas para avaliação de normalidade.")
      })
      
      output$hotelling_table <- renderDataTable({
        datatable(data.frame(
          Classe = character(0), Hotelling_T2 = numeric(0), p_valor = numeric(0), Nota = character(0)),
          caption = "Nenhuma classe possui amostras sintéticas para comparação.")
      })
      return()
    }
    
    # Normalidade multivariada
    
    resultados_normalidade <- lapply(classes_com_sintetica, function(classe) {
      classe_sint <- paste(classe, "Sintética")
      subset_classe <- subset(dados, class %in% c(classe, classe_sint))
      
      num_vars <- names(Filter(is.numeric, subset_classe))
      num_vars <- setdiff(num_vars, "SampleID")
      X <- subset_classe[, num_vars, drop = FALSE]
      
      hz_val <- p_hz <- NA
      status_hz <- "Erro"
      
      roy_val <- p_roy <- NA
      status_roy <- "Erro"
      
      try({
        resultado_hz <- suppressWarnings(mvn(X, mvnTest = "hz"))
        linha_hz <- resultado_hz$multivariateNormality
        if (is.data.frame(linha_hz)) {
          colnames(linha_hz) <- trimws(colnames(linha_hz))
          if ("HZ" %in% colnames(linha_hz) && "p value" %in% colnames(linha_hz)) {
            hz_val <- as.numeric(linha_hz[1, "HZ"])
            p_hz <- as.numeric(linha_hz[1, "p value"])
            status_hz <- if (!is.na(p_hz) && p_hz > 0.05) "Normalidade OK" else "Normalidade violada"
          }
        }
      }, silent = TRUE)
      
      try({
        resultado_roy <- suppressWarnings(mvn(X, mvnTest = "royston"))
        linha_roy <- resultado_roy$multivariateNormality
        if (is.data.frame(linha_roy)) {
          linha_roy$Test <- trimws(as.character(linha_roy$Test))
          linha_royston <- linha_roy[linha_roy$Test == "Royston", , drop = FALSE]
          if (nrow(linha_royston) == 1) {
            roy_val <- as.numeric(linha_royston[[2]])
            p_roy <- as.numeric(linha_royston[[3]])
            status_roy <- if (!is.na(p_roy) && p_roy > 0.05) "Normalidade OK" else "Normalidade violada"
          }
        }
      }, silent = TRUE)
      
      data.frame(
        Classe = classe,
        Estatistica_HZ = round(hz_val, 4),
        p_valor_HZ = round(p_hz, 4),
        Status_HZ = status_hz,
        Estatistica_Royston = round(roy_val, 4),
        p_valor_Royston = round(p_roy, 4),
        Status_Royston = status_roy
      )
    })
    
    df_normalidade <- do.call(rbind, resultados_normalidade)
    
    output$normalidade_table <- DT::renderDataTable({
      DT::datatable(df_normalidade, rownames = FALSE,
                    options = list(pageLength = 10),
                    caption = htmltools::tags$caption(
                      style = 'caption-side: top; text-align: left; font-weight: bold;',
                      "Resultado dos Testes de Normalidade Multivariada por Classe (HZ e Royston)"
                    ))
    })
    
    # Teste de Hotelling
    resultados_hotelling <- lapply(classes_com_sintetica, function(classe) {
      classe_sint <- paste(classe, "Sintética")
      subset_classe <- subset(dados, class %in% c(classe, classe_sint))
      
      X1 <- subset_classe[subset_classe$class == classe, ]
      X2 <- subset_classe[subset_classe$class == classe_sint, ]
      
      if (nrow(X1) < 2 || nrow(X2) < 2) {
        return(data.frame(Classe = classe, Hotelling_T2 = NA, p_valor = NA, Nota = "Amostras insuficientes"))
      }
      
      num_vars <- names(Filter(is.numeric, subset_classe))
      num_vars <- setdiff(num_vars, "SampleID")
      if (length(num_vars) < 2) {
        return(data.frame(Classe = classe, Hotelling_T2 = NA, p_valor = NA, Nota = "Variáveis numéricas insuficientes"))
      }
      
      X1 <- X1[, num_vars, drop = FALSE]
      X2 <- X2[, num_vars, drop = FALSE]
      X_comb <- rbind(X1, X2)
      
      normalidade_ok <- FALSE
      try({
        resultado <- suppressWarnings(mvn(X_comb, mvnTest = "hz"))
        linha <- resultado$multivariateNormality
        colnames(linha) <- trimws(colnames(linha))
        if ("p value" %in% colnames(linha)) {
          p_val <- as.numeric(linha[1, "p value"])
          normalidade_ok <- !is.na(p_val) && p_val > 0.05
        }
      }, silent = TRUE)
      
      nota <- if (!normalidade_ok) {
        "Hipótese de normalidade violada (teste T² de Hotelling não recomendável)"
      } else {
        "OK"
      }
      
      resultado <- tryCatch({
        n1 <- nrow(X1)
        n2 <- nrow(X2)
        p <- ncol(X1)
        
        S1 <- cov(X1)
        S2 <- cov(X2)
        Spooled <- ((n1 - 1) * S1 + (n2 - 1) * S2) / (n1 + n2 - 2)
        mean_diff <- colMeans(X1) - colMeans(X2)
        
        T2_stat <- as.numeric((n1 * n2) / (n1 + n2) * t(mean_diff) %*% solve(Spooled + diag(1e-6, p)) %*% mean_diff)
        df1_val <- p
        df2_val <- n1 + n2 - p - 1
        
        F_stat <- (df2_val * T2_stat) / (df1_val * (n1 + n2 - 2))
        pval_stat <- pf(F_stat, df1_val, df2_val, lower.tail = FALSE)
        
        data.frame(
          Classe = classe,
          Hotelling_T2 = round(T2_stat, 4),
          p_valor = round(pval_stat, 4),
          Nota = nota
        )
      }, error = function(e) {
        data.frame(Classe = classe, Hotelling_T2 = NA, p_valor = NA, Nota = "Erro no cálculo")
      })
      
      return(resultado)
    })
    
    output$hotelling_table <- DT::renderDataTable({
      df_final <- do.call(rbind, resultados_hotelling)
      DT::datatable(df_final, rownames = FALSE,
                    options = list(pageLength = 10),
                    caption = htmltools::tags$caption(
                      style = 'caption-side: top; text-align: left; font-weight: bold;',
                      "Resultados por Classe - Hotelling T²"
                    ))
    })
  })
  
  
  ######ACRÉSCIMO DE SMOTE_NC EM TESTE DE ROYSTON e TESTE DE NORMALIDADE MULTIVARIADA EM 29/05/2025
  
  
  ######ACRÉSCIMO DE BORDERLINE SMOTE EM TESTE DE ROYSTON e TESTE DE NORMALIDADE MULTIVARIADA EM 05/06/2025
  
  ######ACRÉSCIMO DE SVM SMOTE EM TESTE DE ROYSTON e TESTE DE NORMALIDADE MULTIVARIADA EM 06/06/2025
  
  ######ACRÉSCIMO DE ADASYN EM TESTE DE ROYSTON e TESTE DE NORMALIDADE MULTIVARIADA EM 08/06/2025
  
  ######ACRÉSCIMO DE RANDOM UPSAMPLING EM TESTE DE ROYSTON e TESTE DE NORMALIDADE MULTIVARIADA EM 09/06/2025
  
  
  ##ACRÉSCIMO DE T² DE HOTELLING E DIST.MAHALANOBIS EM 29/03/2025##
  
  ###ACRÉSCIMO DE TESTE PERMANOVA EM 30/03/2025#####
  
  ######ACRÉSCIMO DE SMOTE_NC EM TESTE PERMANOVA EM 29/05/2025
  
  ######ACRÉSCIMO DE BORDERLINE SMOTE EM TESTE PERMANOVA EM 05/06/2025
  
  ######ACRÉSCIMO DE SVM SMOTE EM TESTE PERMANOVA EM 06/06/2025
  
  ######ACRÉSCIMO DE ADASYN EM TESTE PERMANOVA EM 08/06/2025
  
  ######ACRÉSCIMO DE RANDOM UPSAMPLING EM TESTE PERMANOVA EM 09/06/2025
  
  
  
  observe({
    library(vegan)
    
    original_data <- as.data.frame(filedata())
    smote_data <- NULL
    class_col <- NULL
    
    # Verificando se foi rodado SMOTE ou SMOTE-NC OU BORDERLINE SMOTE
    
  #  if (isTRUE(runSMOTENC()) && !is.null(SMOTENC_Data()) && !is.null(input$target_nc)) {
  #    smote_data <- SMOTENC_Data()$syn_data
  #    class_col <- input$target_nc
  #  } else if (isTRUE(runSMOTE()) && !is.null(SMOTE_Data()) && !is.null(input$target)) {
  #    smote_data <- SMOTE_Data()$syn_data
  #    class_col <- input$target
  #  } else {
  #    return()
  #  }
    
    
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
      
      
    } else {
      return()
    }   
    
    
    # Verificações de segurança
    
    if (is.null(smote_data) || !(class_col %in% names(original_data))) return()
    
    # Renomeando coluna de classe para "class"
    
    names(original_data)[names(original_data) == class_col] <- "class"
    names(smote_data)[names(smote_data) == class_col] <- "class"
    
    # Preparando classes sintéticas
    
    smote_data$class <- paste(smote_data$class, "Sintética")
    smote_data$fake_cat <- NULL  # Removendo coluna auxiliar se existir
    
    # Garantindo interseção de colunas numéricas
    
    feature_cols <- intersect(names(original_data), names(smote_data))
    original_data <- original_data[, feature_cols, drop = FALSE]
    smote_data <- smote_data[, feature_cols, drop = FALSE]
    
    # Adicionando rótulo de tipo
    
    original_data$Tipo <- "Original"
    smote_data$Tipo <- "Sintética"
    
    # Combinando dados
    
    dados <- dplyr::bind_rows(original_data, smote_data)
    dados$SampleID <- 1:nrow(dados)
    dados$class <- as.character(dados$class)
    dados$Tipo <- ifelse(grepl("Sintética", dados$class), "Sintética", "Original")
    
    # Identificando classes originais com correspondentes sintéticas
    
    classes_originais <- unique(dados$class[!grepl("Sintética", dados$class)])
    classes_com_sintetica <- classes_originais[
      paste0(classes_originais, " Sintética") %in% dados$class
    ]
    
    if (length(classes_com_sintetica) == 0) {
      output$permanova_table <- renderDataTable({
        datatable(data.frame(Classe = character(0), F_model = numeric(0), R2 = numeric(0), p_valor = numeric(0)),
                  caption = "Nenhuma classe possui amostras sintéticas para comparação.")
      })
      return()
    }
    
    # Loop por classe para executar PERMANOVA
    
    resultados_permanova <- lapply(classes_com_sintetica, function(classe) {
      classe_sint <- paste(classe, "Sintética")
      subset_classe <- subset(dados, class %in% c(classe, classe_sint))
      
      grupo <- subset_classe$Tipo
      num_vars <- names(Filter(is.numeric, subset_classe))
      num_vars <- setdiff(num_vars, "SampleID")
      
      if (length(num_vars) < 2) {
        return(data.frame(Classe = classe, F_model = NA, R2 = NA, p_valor = NA))
      }
      
      X <- subset_classe[, num_vars, drop = FALSE]
      
      resultado <- tryCatch({
        permanova <- adonis2(X ~ grupo, permutations = 999)
        data.frame(
          Classe = classe,
          F_model = round(permanova$F[1], 4),
          # R2 = round(permanova$R2[1], 4),  # R² REMOVIDO conforme anotação de 31/03/2025
          p_valor = round(permanova$`Pr(>F)`[1], 4)
        )
      }, error = function(e) {
        data.frame(Classe = classe, F_model = NA, R2 = NA, p_valor = NA)
      })
      
      return(resultado)
    })
    
    # Exibição da tabela final
    
    output$permanova_table <- DT::renderDataTable({
      df_final <- do.call(rbind, resultados_permanova)
      DT::datatable(df_final, rownames = FALSE,
                    options = list(pageLength = 10),
                    caption = htmltools::tags$caption(
                      style = 'caption-side: top; text-align: left; font-weight: bold;',
                      "Resultados por Classe - PERMANOVA"
                    ))
    })
  })
  
  ######ACRÉSCIMO DE SMOTE_NC EM TESTE PERMANOVA EM 29/05/2025
  
  ######ACRÉSCIMO DE BORDERLINE SMOTE EM TESTE PERMANOVA EM 05/06/2025
  
  ######ACRÉSCIMO DE SVM SMOTE EM TESTE PERMANOVA EM 06/06/2025
  
  ######ACRÉSCIMO DE ADASYN EM TESTE PERMANOVA EM 08/06/2025
  
  ######ACRÉSCIMO DE RANDOM UPSAMPLING EM TESTE PERMANOVA EM 09/06/2025
  
  
  
  ###ACRÉSCIMO DE TESTE PERMANOVA EM 30/03/2025#####
  
  
  ###ATUALIZADO EM 13/02/2025#####
  
  
  ## INÍCIO DO BLOCO ATUALIZADO COM SUPORTE A SMOTE E SMOTE-NC ATUALIZADO EM 29/05/2025
  
  
  ####################MODIFICAÇÃO DA GUIA IMPORT DATA APÓS CONFIRM SMOTE EM 07/05/2025
  
  # Armazenando os dados combinados após clique em Confirm SMOTE, Confirm SMOTE-NC,
  # Confirm BORDERLINE SMOTE, Confirm SVM SMOTE, Confirm ADASYN, Confirm RANDOM UPSAMPLING
  
  combined_data_final <- reactiveVal(NULL)
  
  # Amostras Originais
  
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
  
  # Amostras Sintéticas
  
  output$synthetic_only_data <- renderDT({
    req(SMOTE_Data())
    if ("data" %in% names(SMOTE_Data())) {
      synthetic_data <- SMOTE_Data()$syn_data
      if ("class" %in% colnames(synthetic_data)) {
        synthetic_data$class <- paste(synthetic_data$class, "Sintética")
      }
      datatable(synthetic_data, options = list(pageLength = 5))
    } else {
      datatable(data.frame(Mensagem = "Os dados sintéticos não estão disponíveis ou possuem estrutura inválida."), 
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
    # Atualizanddo choices de remoção de amostras e variáveis
    
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
    
    # Atualizanddo choices de remoção de amostras e variáveis
    
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
    
    # Atualizando choices de remoção de amostras e variáveis
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
    
    # Atualizando choices de remoção de amostras e variáveis
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
    
    # Atualizando choices de remoção de amostras e variáveis
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
    
    # Atualizando choices de remoção de amostras e variáveis
    updateSelectizeInput(session, "removedatarow", choices = rownames(confirmed), server = TRUE)
    updateSelectizeInput(session, "removedatacol", choices = names(confirmed), server = TRUE)
  })  
  
  
  
  
  # Amostras Combinadas (Originais + Sintéticos) — atualiza somente após confirmSMOTE ou confirmSMOTENC OU Confirm BORDERLINE SMOTE ou Confirm SVM SMOTE
  #ou Confirm ADASYN OU Confirm RANDOM UPSAMPLING
  
  output$synthetic_data <- renderDT({
    req(combined_data_final())
    datatable(combined_data_final(), options = list(pageLength = 5))
  })
  
  # Exportação
  
  output$download_selected <- downloadHandler(
    filename = function() {
      dataset_name <- switch(input$dataset_choice,
                             original = "amostras_originais",
                             synthetic = "amostras_sintéticas",
                             combined = "amostras_combinadas")
      paste(dataset_name, ifelse(input$export_format == "csv", ".csv", ".xlsx"), sep = "")
    },
    content = function(file) {
      
      
      ###############BLOCO MODIFICAOD EM 06/06/2025 - "A PALAVRA "SINTÉTICA" SÓ APARECIA NO SMOTE
      
   #   dataset <- switch(input$dataset_choice,
    #                    original = filedata(),
     #                   synthetic = { 
      #                    synthetic_data <- SMOTE_Data()$syn_data
       #                   if ("class" %in% colnames(synthetic_data)) {
        #                    synthetic_data$class <- paste(synthetic_data$class, "Sintética")
         #                 }
          #                synthetic_data
           #             },
            #            combined = {
             #             req(combined_data_final())
              #            combined_data_final()
               #         })
  
      
      dataset <- switch(input$dataset_choice,
                        original = filedata(),
                        
                        synthetic = {
                          synthetic_data <- NULL
                          
                          # Ordem de prioridade: SMOTE > SMOTE-NC > Borderline > SVM_SMOTE
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
                            
                            
                          }
                          
                          if (is.null(synthetic_data)) return(NULL)
                          
                          if ("class" %in% colnames(synthetic_data)) {
                            synthetic_data$class <- paste(synthetic_data$class, "Sintética")
                          }
                          synthetic_data
                        },
                        
                        combined = {
                          req(combined_data_final())
                          combined_data_final()
                        }
      )
      
      
      
      
      
      
      ###############BLOCO MODIFICAOD EM 06/06/2025 - "A PALAVRA "SINTÉTICA" SÓ APARECIA NO SMOTE
      
      
          
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
  
  
  # Valor reativo para controle do botão
  
  residuos_ativado <- reactiveVal(FALSE)
  
  observeEvent(input$run_residuals, {
    residuos_ativado(TRUE)
  })
  
  observe({
    req(residuos_ativado())
    req(input$numPCresidCustom)
    
    combined_data <- NULL
    
  #  if (!is.null(SMOTENC_Data())) {
  #    original_data <- as.data.frame(filedata())
  #    smote_data <- SMOTENC_Data()$syn_data
  #    class_col <- input$target_nc
  #  } else if (!is.null(SMOTE_Data())) {
  #    original_data <- as.data.frame(filedata())
  #    smote_data <- SMOTE_Data()$syn_data
  #    class_col <- input$target
  #  } else {
  #    return()
  #  }
    
    
    if (!is.null(SMOTENC_Data())) {
      original_data <- as.data.frame(filedata())
      smote_data <- SMOTENC_Data()$syn_data
      class_col <- input$target_nc
    } else if (!is.null(SMOTE_Data())) {
      original_data <- as.data.frame(filedata())
      smote_data <- SMOTE_Data()$syn_data
      class_col <- input$target
    } else if (!is.null(BorderlineSMOTE_Data())) {
      original_data <- as.data.frame(filedata())
      smote_data <- BorderlineSMOTE_Data()$syn_data
      class_col <- input$target_borderline
      
    } else if (!is.null(SVM_SMOTE_Data())) {
      original_data <- as.data.frame(filedata())
      smote_data <- SVM_SMOTE_Data()$syn_data
      class_col <- input$target_svm  
      
      
    } else if (!is.null(ADASYN_Data())) {
      original_data <- as.data.frame(filedata())
      smote_data <- ADASYN_Data()$syn_data
      class_col <- input$target_adasyn    
      
      
    } else if (!is.null(RU_Data())) {
      original_data <- as.data.frame(filedata())
      smote_data <- RU_Data()$syn_data
      class_col <- input$target_ru   
      
      
      
    } else {
      return()
    }           
    
    
   if (is.null(smote_data) || !(class_col %in% names(original_data))) return()
    
    names(original_data)[names(original_data) == class_col] <- "class"
    names(smote_data)[names(smote_data) == class_col] <- "class"
    smote_data$class <- paste(smote_data$class, "Sintética")
    smote_data$fake_cat <- NULL
    
    feature_cols <- intersect(names(original_data), names(smote_data))
    original_data <- original_data[, feature_cols, drop = FALSE]
    smote_data <- smote_data[, feature_cols, drop = FALSE]
    
    original_data$Tipo <- "Original"
    smote_data$Tipo <- "Sintética"
    
    combined_data <- dplyr::bind_rows(original_data, smote_data)
    combined_data$class <- as.character(combined_data$class)
    combined_data$Tipo <- ifelse(grepl(" Sintética$", combined_data$class), "Sintética", "Original")
    
    numeric_cols <- sapply(combined_data, is.numeric)
    pcaData <- combined_data[, numeric_cols, drop = FALSE]
    
    # PCA
    
    pca_model <- prcomp(pcaData, center = TRUE, scale. = TRUE)
    scores <- pca_model$x[, 1:input$numPCresidCustom, drop = FALSE]
    loadings <- pca_model$rotation[, 1:input$numPCresidCustom, drop = FALSE]
    
    # Reconstrução
    
    reconstructed <- scores %*% t(loadings)
    if (!isFALSE(pca_model$scale)) {
      reconstructed <- scale(reconstructed, center = FALSE, scale = 1 / pca_model$scale)
    }
    if (!isFALSE(pca_model$center)) {
      reconstructed <- scale(reconstructed, center = -pca_model$center, scale = FALSE)
    }
    
    # Distâncias
    
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
    base_classes <- gsub(" Sintética", "", classes_ordered)
    classes_ordered <- unique(unlist(lapply(unique(base_classes), function(cls) {
      c(cls, paste0(cls, " Sintética"))
    })))
    classes_ordered <- classes_ordered[classes_ordered %in% unique(df$Classe)]
    cores_paleta <- RColorBrewer::brewer.pal(n = max(3, length(classes_ordered)), name = "Set1")
    names(cores_paleta) <- classes_ordered
    
    p <- plot_ly(df, x = ~X, y = ~Y, type = "scatter", mode = "markers+text",
                 color = ~Classe, colors = cores_paleta,
                 marker = list(size = 5),
                 text = ~SampleID,
                 textposition = 'top center',
                 textfont = list(color = 'lightgray', size = 10),
                 hoverinfo = 'text',
                 hovertext = ~paste("Sample:", SampleID,
                                    "<br>Classe:", Classe,
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
    <p><strong>Interpretação do gráfico:</strong></p>
    <p>Este gráfico mostra a relação entre a <strong>distância ortogonal (q/q0)</strong> e a <strong>distância de projeção (h/h0)</strong> para cada amostra no modelo PCA.</p>
    <ul>
      <li><span style='color:orange;'><strong>Linha laranja</strong></span>: limite crítico (reta ou curva no modo log).</li>
      <li><span style='color:red;'><strong>Linha vermelha</strong></span>: limite extremo de outlier.</li>
    </ul>
    <p>No modo log, aplica-se <code>log(1 + x)</code> para melhor visualização.</p>
    </ul>
    <p>As distâncias são calculadas com base na reconstrução do modelo PCA considerando os <strong>n</strong> componentes principais informados. A <em>Score Distance</em> (h) representa o desvio no espaço de componentes principais, enquanto a <em>Orthogonal Distance</em> (q) representa o erro de reconstrução.</p>
    <p>No gráfico, são exibidas as formas normalizadas dessas métricas: <code>h/h0</code> e <code>q/q0</code>, sendo <code>h0 = quantile(h, 0.975)</code> e <code>q0 = mean(q) + 3 * sd(q)</code>. Com <em>log</em> ativado, aplica-se <code>log(1 + h/h0)</code> e <code>log(1 + q/q0)</code> para melhor visualização.</p>
  </div>"
      )
    })
  })
  
  
##ATUALIZADO EM 29/05/2025 - ACRÉSCIMO DE SMOTE NC
##ATUALIZADO EM 05/06/2025 - ACRÉSCIMO DE BORDERLINE SMOTE  
##ATUALIZADO EM 06/06/2025 - ACRÉSCIMO DE SVM SMOTE    
##ATUALIZADO EM 08/06/2025 - ACRÉSCIMO DE ADASYN  
##ATUALIZADO EM 09/06/2025 - ACRÉSCIMO DE RANDOM UPSAMPLING  
  
    
########ACRÉSCIMO DOS RESÍDUOS DA PCA EM VISUALIZAÇÕES EM 14/03/2025#####  
  
  
  ##BLOCO MODIFICADO EM 03/04/2025
  
  
  ########ACRÉSCIMO DOS OUTLIERS DA PCA EM VISUALIZAÇÕES EM 14/03/2025#####
  # Criando um objeto reativo para armazenar combined_data
  # Reativo: dados combinados
  
  ##ATUALIZADO EM 29/05/2025 COM ACRÉSCIMO DE SMOTE-NC EM OUTLIERS DA PCA
  ##ATUALIZADO EM 05/06/2025 COM ACRÉSCIMO DE BORDERLINE SMOTE EM OUTLIERS DA PCA
  ##ATUALIZADO EM 08/06/2025 COM ACRÉSCIMO DE ADASYN EM OUTLIERS DA PCA
  ##ATUALIZADO EM 09/06/2025 COM ACRÉSCIMO DE RANDOM UPSAMPLING EM OUTLIERS DA PCA
  
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
      if (!is.null(SMOTENC_Data())) {
        original_data <- filedata()
        synthetic_data <- SMOTENC_Data()$syn_data
        class_col <- input$target_nc
      } else if (!is.null(SMOTE_Data())) {
        original_data <- filedata()
        synthetic_data <- SMOTE_Data()$syn_data
        class_col <- input$target
       } else if (!is.null(BorderlineSMOTE_Data())) {
          original_data <- filedata()
          synthetic_data <- BorderlineSMOTE_Data()$syn_data
          class_col <- input$target_borderline 
          
       } else if (!is.null(SVM_SMOTE_Data())) {
         original_data <- filedata()
         synthetic_data <- SVM_SMOTE_Data()$syn_data
         class_col <- input$target_svm     
          
        
       } else if (!is.null(ADASYN_Data())) {
         original_data <- filedata()
         synthetic_data <- ADASYN_Data()$syn_data
         class_col <- input$target_adasyn       
         
        
       } else if (!is.null(RU_Data())) {
         original_data <- filedata()
         synthetic_data <- RU_Data()$syn_data
         class_col <- input$target_ru  
         
         
          
           
      } else {
        return(NULL)
      }  
    
    
    names(original_data)[names(original_data) == class_col] <- "class"
    names(synthetic_data)[names(synthetic_data) == class_col] <- "class"
    
    feature_cols <- intersect(names(original_data), names(synthetic_data))
    original_data <- original_data[, feature_cols, drop = FALSE]
    synthetic_data <- synthetic_data[, feature_cols, drop = FALSE]
    synthetic_data$class <- paste0(synthetic_data$class, " Sintética")
    
    combined_data <- rbind(original_data, synthetic_data)
    rownames(combined_data) <- seq_len(nrow(combined_data))
    return(combined_data)
  })
  
  # Controle para ativação do gráfico
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
        plot_ly() %>% layout(title = "Erro: Dados insuficientes para PCA")
      })
      return()
    }
    
    PCAmodel <- tryCatch(
      { rrcov::PcaClassic(pcaData, k = input$numPCoutipcaCustom) },
      error = function(e) {
        output$OutPCACustom <- renderPlotly({
          plot_ly() %>% layout(title = "Erro ao calcular PCA")
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
    
    classes <- sort(unique(df_plot$Classe))
    base_classes <- gsub(" Sintética", "", classes)
    classes_ordenadas <- unique(unlist(lapply(unique(base_classes), function(cls) {
      c(cls, paste0(cls, " Sintética"))
    })))
    df_plot$Classe <- factor(df_plot$Classe, levels = classes_ordenadas)
    
    cores <- RColorBrewer::brewer.pal(n = max(3, length(classes_ordenadas)), name = "Set1")
    names(cores) <- classes_ordenadas
    
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
          "<br><b>Classe:</b> ", Classe,
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
    <p><strong>Interpretação do gráfico:</strong></p>
    <p>Este gráfico mostra a posição de cada amostra com relação à <strong>distância de projeção (Score Distance)</strong> e à <strong>distância ortogonal (Orthogonal Distance)</strong> no modelo PCA.</p>
    <ul>
      <li><strong>Linha tracejada vertical vermelha:</strong> limite superior aceitável para a Score Distance.</li>
      <li><strong>Linha tracejada horizontal vermelha:</strong> limite superior aceitável para a Orthogonal Distance.</li>
    </ul>
    <p>Amostras que estão além dessas linhas podem ser consideradas potenciais <strong>outliers</strong> e devem ser analisadas.</p>
  </div>"
      )
    })
  })
  
  
  
  ##ATUALIZADO EM 29/05/2025 COM ACRÉSCIMO DE SMOTE-NC EM OUTLIERS DA PCA
  ##ATUALIZADO EM 05/06/2025 COM ACRÉSCIMO DE BORDERLINE SMOTE EM OUTLIERS DA PCA
  ##ATUALIZADO EM 06/06/2025 COM ACRÉSCIMO DE SVM SMOTE EM OUTLIERS DA PCA
  ##ATUALIZADO EM 08/06/2025 COM ACRÉSCIMO DE ADASYN EM OUTLIERS DA PCA
  ##ATUALIZADO EM 09/06/2025 COM ACRÉSCIMO DE RANDOM UPSAMPLING EM OUTLIERS DA PCA
  
  
    
  ########ACRÉSCIMO DOS OUTLIERS DA PCA EM VISUALIZAÇÕES EM 14/03/2025#####
  
# Testes de variância - ACRÉSCIMO DE SMOTE_NC EM 30/05/2025
  
# Testes de variância - ACRÉSCIMO DE BORDERLINE SMOTE 05/06/2025  
  
# Testes de variância - ACRÉSCIMO DE SVM SMOTE 06/06/2025    
  
# Testes de variância - ACRÉSCIMO DE ADASYN 08/06/2025  

# Testes de variância - ACRÉSCIMO DE RANDOM UPSAMPLING 09/06/2025  
 
  observeEvent(input$run_variance_test, {
    original_data <- as.data.frame(filedata())
    smote_data <- NULL
    class_col <- NULL
    
#    if (isTRUE(runSMOTENC()) && !is.null(SMOTENC_Data()) && !is.null(input$target_nc)) {
#      smote_data <- SMOTENC_Data()$syn_data
#      class_col <- input$target_nc
#    } else if (isTRUE(runSMOTE()) && !is.null(SMOTE_Data()) && !is.null(input$target)) {
#      smote_data <- SMOTE_Data()$syn_data
#      class_col <- input$target
#    } else {
#      showNotification("Execute SMOTE ou SMOTE-NC antes de rodar o teste.", type = "error")
#      return(NULL)
#    }
    
    
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
      
      
      
       
     } else {
      showNotification("Execute SMOTE, SMOTE-NC, BORDERLINE_SMOTE, SVM_SMOTE, ADASYN OU RANDOM UPSAMPLING antes de rodar o teste.", type = "error")
      return(NULL)
    }
  
    
    if (is.null(smote_data) || !(class_col %in% names(original_data))) {
      showNotification("Coluna de classe não encontrada nos dados originais ou sintéticos.", type = "error")
      return(NULL)
    }
    
    names(original_data)[names(original_data) == class_col] <- "class"
    names(smote_data)[names(smote_data) == class_col] <- "class"
    smote_data$class <- paste(smote_data$class, "Sintética")
    smote_data$fake_cat <- NULL
    
    feature_cols <- intersect(names(original_data), names(smote_data))
    original_data <- original_data[, feature_cols, drop = FALSE]
    smote_data <- smote_data[, feature_cols, drop = FALSE]
    
    original_data$Tipo <- "Original"
    smote_data$Tipo <- "Sintética"
    
    dataset <- dplyr::bind_rows(original_data, smote_data)
    validate(need("class" %in% colnames(dataset), "A coluna 'class' não está presente nos dados!"))
    dataset$class <- as.factor(dataset$class)
    
    original_dataset <- dataset[!grepl("Sintética", dataset$class), ]
    synthetic_dataset <- dataset[grepl("Sintética", dataset$class), ]
    if (nrow(synthetic_dataset) > 0) {
      synthetic_dataset$OriginalClass <- gsub(" Sintética", "", synthetic_dataset$class)
    }
    
    # Detectando variáveis numéricas 
     
    numeric_cols <- names(Filter(is.numeric, dataset))
    numeric_cols <- setdiff(numeric_cols, c("SampleID"))
    
    if (length(numeric_cols) == 0) {
      showNotification("Nenhuma variável numérica disponível para o teste.", type = "error")
      return(NULL)
    }
    
    # Rodando testes de variância
    
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
    
    # Exibindo os resultados formatados
    
    output$variance_test_results <- renderPrint({
      results_df <- do.call(rbind, lapply(results, function(res) {
        data.frame(
          Variable = res$variable,
          Test = res$test,
          P_Value = round(res$p_value, 4),
          Message = ifelse(is.null(res$message), "Sucesso", res$message)
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
