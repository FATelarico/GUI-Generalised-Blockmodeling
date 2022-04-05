if(!require(shiny)) install.packages('shiny')
if(!require(htmlwidgets)) install.packages('htmlwidgets')
if(!require(shinythemes))install.packages('shinythemes')
if(!require(network)) install.packages('network')
if(!require(blockmodeling)) install.packages('blockmodeling')
if(!require(igraph)) install.packages('igraph')
if(!require(visNetwork))install.packages('visNetwork')
if(!require(intergraph))install.packages('intergraph')
if(!require(DT))install.packages('DT')
if(!require(shinybusy))install.packages('shinybusy')

library(shiny)
library(htmlwidgets)
library(shinythemes)
library(blockmodeling)
library(intergraph)
library(igraph)
library(network)
library(visNetwork)
library(DT)
library(shinybusy)


# §1 Inputs ####
ui <- fluidPage(
  includeCSS("./style.css"), # CSS Styles
  theme = shinytheme("united"),
  tags$head(
    tags$link(
      rel = "shortcut icon",
      href = "https://www.fdv.uni-lj.si/App_Themes/Fdv/TemplateImages/icons/favicon.ico")),
  tags$head(
    tags$title("Generalised blockmodeling")
    ),



    # 0. Decorations with HTML/CSS ####
    titlePanel({
      withTags({
        div(class="Header",
            table(
              tr(
                td(width = "267px",
                  img(class = "HeaderLogo",
                      src="https://www.uni-lj.si/images/mobile/logo_m.png"),
                  ),
                td(
                  div(style="h1","Generalised blockmodeling"),
                  ),
                ), # /tr
              ), # /table
            div(class="titleRect",
                div(class="title2",
                    p('This app provides some useful tools for the analysis of one-mode networks withstructurally-equivalent nodes. Besides offering general summary info and a visualisation, this app allows also to perform a one-mode, generalised blockmodeling using the "homogeinity" method'),
                    'To lean more about how this can be helpful to you, you can check the descriptions of the packages employed here:',
                    a(href="https://cran.r-project.org/package=blockmodeling", "blockmodeling"),
                    'by Ale\u0161 \u017Diberna and',
                    a(href="https://cran.r-project.org/package=network", "network"),
                    'by Carter Butts and others',
                    )# /div "title2"
                )# /div "titleRect"
            ) # div "Header"
        })
    }),

  add_busy_spinner(
    spin = 'breeding-rhombus',
    color = '#978E83',
    timeout = 100,
    position = 'top-right',
    onstart = TRUE,
    margins = c(10, 10)
  ),
  
  
    # Sidebar 
        tabsetPanel(
            tabPanel(title = "Data upload",
                     fluidRow(
                       column(3,
                              # 1. File-upload options ####
                              # "FileInputOpt1"
                              withTags({
                                div(h4(b("File-upload options")))
                              }),
                              ## 1.1 Use a sample ####
                              ### "Sample" 
                              checkboxInput(inputId = "Sample",
                                            label = 'Use a sample',
                                            value = F),
                              conditionalPanel(
                                condition = "input.Sample == false",
                                ## 1.2 Select type of input ####
                                ## "type"
                                selectInput(inputId = "type",
                                            label = "Type of data?",
                                            choice = c("Adjacency Matrix*"=1,"Edges list"=2, "Incidence matrix"=3,"Pajek"=4),
                                            selected = 1,
                                            multiple = FALSE
                                ),
                                conditionalPanel(
                                  condition = "input.type != 4",
                                  ### 1.2.1 Upload list ####
                                  ### "List"
                                  fileInput(inputId = "List",
                                            label = "Upload the selected list as a plain-text file",
                                            multiple = F,
                                            accept = c("text/plain", ".csv",".tab"),
                                            buttonLabel = "Browse",
                                            placeholder = "Your list here"),
                                  
                                  ### 1.2.2 Headers edge list file ####
                                  ### "ListHeader"
                                  checkboxInput(inputId = "ListHeader",
                                                label = 'Headers?',
                                                value = T,
                                                ),
                                  )
                                )
                              ),
                       column(4,offset = 1,
                              # 2. Customise file elaboration ####
                              # "FileInputOpt2"
                              withTags({
                                div(h4(b("Customise file elaboration")))
                              }),
                              ## 2.1 Separator ####
                              ## "sep"
                              radioButtons(inputId = "sep",
                                           label = "Separator",
                                           choiceNames = c("tab","comma","semicolon","other"),
                                           choiceValues = c("\t",",",";","Other")
                              ),
                              conditionalPanel(
                                condition = "input.sep == 'Other'",
                                ### 2.1.1 Other separator ####
                                ### "OtherSep"
                                textAreaInput(inputId = "OtherSep",
                                              label = 'You selected "other", please indicate the right separator',
                                              cols = 3,
                                              value = "",
                                ),
                              ),
                              ## 2.2 Trim blanks ####
                              ## "whites"
                              checkboxInput(inputId = "whites",
                                            label = 'Trim extra blanks',
                                            value = TRUE),
                              conditionalPanel(
                                condition = "input.type == 4",
                                ## 2.3 Type of Pajek file ####
                                ## "PajekInput"
                                radioButtons(inputId = "PajekInput",
                                             label = "Type of Pajek file",
                                             selected = ".net",
                                             choiceNames = c(".mat",".net"),
                                             choiceValues = c("PajekMatrix","PajekNetwork")
                                ),
                                ## 2.4 Upload Pajek file ####
                                ## "PajekFile"
                                fileInput(inputId = "PajekFile",
                                          label = "Upload a Pajek file",
                                          multiple = F,
                                          buttonLabel = "Browse",
                                          placeholder = "Your Pajek file here",
                                          accept = c(".mat",".net")
                                ),
                              ), # if input.type == 4
                              ), # columnt 3 
                     column(4,
                            #3. Specify network properties ####
                            # "NetworkOpt"
                            withTags({
                              div(h4(b("Specify network properties")))
                            }),
                            
                            ## 3.1 Values/Weights ####
                            ## "ValuedMatrix"
                            checkboxInput(inputId = "ValuedMatrix",
                                          label = 'Valued/Weighted network',
                                          value = TRUE,
                            ),
                            ### 3.1.1 Values/Weights name ####
                            # "ValuesName"
                            # conditionalPanel(
                            #   condition = "input.ValuedMatrix == true",
                            #   textInput(inputId = "ValuesName",
                            #             label = "How do you want the weights to be called?",
                            #             value = "weights",
                            #             placeholder = "weights, values, counts, etc...")
                            #   ),
                            withTags(div(
                              h6(b(i("Only change the settings below if needed"))),
                            )),
                            ### 3.2 Direction ####
                            # "directionality"
                            checkboxInput(inputId = "directionality",
                                          label = 'Directional edges',
                                          value = TRUE,
                            ),
                            ### 3.3 Self-links ####
                            # "loops"
                            checkboxInput(inputId = "loops",
                                          label = 'Self-links',
                                          value = TRUE,
                            ),
                            ### 3.4 Add attribute ####
                            conditionalPanel(
                              condition = "input.Sample == false",
                              ### "AddAttr"
                              checkboxInput(inputId = 'AddAttr',
                                            label = 'Add a vertex attribute?',
                                            value = F),
                              conditionalPanel(
                                condition = 'input.AddAttr==true',
                                conditionalPanel(
                                  condition = 'input.Sample==false',
                                  #### 3.4.1 Attribute values
                                  #### "AddAttrFile"
                                  fileInput(inputId = "AddAttrFile",
                                            label = 'Values from file',
                                            placeholder = 'A text file containining the values',
                                            multiple = F,
                                            accept = 'text/plain'
                                  ),
                                  #### 3.4.2 Attribute name
                                  #### 'AddAttrName'
                                  textInput(inputId = "AddAttrName",
                                            label = 'Attribute name',
                                            placeholder = 'Do not use blanks',
                                            value = NULL),
                                  
                                ),
                              ),
                            ),
                            
                            
                            ),
                     ### 3.5 Button "Read data" ####
                     # "aj"
                     actionButton(inputId = "aj",
                                  label = "Read Data",
                                  icon = icon(name = "upload",
                                              lib = "font-awesome")
                     )
                     ),# Fluid row
                     # 4. Show the network's summary ####
                     # "summary"
                     verbatimTextOutput("summary"),
                     
                     ## 4.1  Summary with adj ####
                     ## "NetworkSummaryOpt"
                     withTags({
                       div(h4(b("An extra option")))
                     }),
                     
                     # "IncludeAdj"
                     checkboxInput(inputId = "IncludeAdj",
                                   label = 'Include the edgelist matrix?',
                                   value = F,
                                   width = "100%"
                     ),
                     ), # Tab panel Data
            tabPanel(title = "Generalised blockmodeling",
                     # 5. blockmodeling
                     sidebarLayout(
                       sidebarPanel(
                         ## 5.1 blockmodeling options ####
                         ## "blckmdlngOpt"
                         withTags({
                           div(h4(b("Customise blockmodeling")))
                         }),
                         hr(),
                         fluidRow(
                           column(6,
                                  
                                  ### 5.1.1 blockmodeling approach ####
                                  ### "blckmdlngApproach"
                                  selectInput(inputId = "blckmdlngApproach",
                                              label = "Approach?",
                                              choices = c("Binary"="bin",
                                                           "Valued"="val",
                                                           "Sum of squares homogeneity"="ss",
                                                           "Absolute deviations homogeneity"="ad"),
                                              multiple=FALSE
                                  ),
                                  
                                  #### 5.1.1 (A) M parameter for valued blockmodeling ####
                                  #### "ParamM"
                                  conditionalPanel(
                                    condition ="input.blckmdlngApproach == 'val'",
                                    numericInput(inputId = "ParamM",
                                                 label = 'Select the M parameter',
                                                 value = NULL,
                                                 min = 0,
                                                 step = 1
                                    )
                                  ),
                                  
                                  #### 5.1.1 (B) Threshold parameter for binary blockmodeling ####
                                  #### "ParamThreshold", "ThresholdSelected"
                                  conditionalPanel(
                                    condition ="input.blckmdlngApproach == 'bin'",
                                    # Asks whether the user wants to set a threshold
                                    checkboxInput(inputId = "ThresholdSelected",
                                                  label = 'Use a binarisation threshold',
                                                  value = F,
                                                  width = "100%"
                                    ),
                                  ),
                                  conditionalPanel(
                                    condition ="input.ThresholdSelected == true && input.blckmdlngApproach == 'bin'",
                                    numericInput(inputId = "ParamThreshold",
                                                 label = 'Threshold parameter',
                                                 value = NULL,
                                                 min = 0,
                                                 step = 1)
                                    
                                  ),
                                  
                                  
                                  ### 5.1.2 Types of of allowed blocktypes ####
                                  
                                  #### 5.1.2 (A) Pre-specified? ####
                                  #### "blckmdlngPrespecifiedYN"
                                  checkboxInput(inputId = "blckmdlngPrespecifiedYN",
                                                label = 'Pre-specify the allowed blocktypes?',
                                                value = FALSE
                                  ),
                                  
                                  #### 5.1.2 (B) Non pre-specified ####
                                  #### "blckmdlngBlockTypes"
                                  conditionalPanel(
                                    condition = 'input.blckmdlngPrespecifiedYN == false',
                                    selectInput(inputId = "blckmdlngBlockTypes",
                                                label = "Allowed blocktypes",
                                                choices = c("null or empty block"="nul",
                                                            "complete block"="com",
                                                            # "row-dominant (binary and valued approach only)"="rdo",
                                                            # "column-dominant (binary and valued approach only)"="cdo",
                                                            "(f-)regular block"="reg",
                                                            "row (f-)regular"="rre",
                                                            "column (f-)regular"= "cre",
                                                            # "row dominant (binary, valued only)"="rfn",
                                                            # "column dominant (binary, valued only)"= "cfn",
                                                            # "density block (binary approach only)"="den",
                                                            # "average block (valued approach only)"="avg",
                                                            "do not care block (the error is always zero)"="dnc"),
                                                selected = c("nul","com"),
                                                multiple = TRUE
                                    ), 
                                    
                                    ### 5.1.3 Number of clusters ####
                                    ### "blckmdlngNumClusters"
                                    numericInput(inputId = "blckmdlngNumClusters",
                                                 label = 'How many clusters to use in the generation of partitions?',
                                                 value = 3,
                                                 min = 1,
                                                 step = 1
                                                 ),
                                    ), # Conditional blckmdlngPrespecifiedYN
                                  ### 5.1.4 Number of repetitions ####
                                  ### "blckmdlngRepetitions"
                                  numericInput(inputId = "blckmdlngRepetitions",
                                               label = 'How many repetitions/different starting partitions to check?',
                                               value = 2,
                                               min = 1,
                                               step = 1
                                               ),
                                  ### 5.1.5 Saving initial parameters ####
                                  withTags(i("Saving the additional parameters can take up more memory, but also preserve precious information")),
                                  ### "blckmdlngInitialParams"
                                  checkboxInput(inputId = "blckmdlngInitialParams",
                                                label = 'Should the initial parameters be saved?',
                                                value = TRUE
                                                ),
                                  ),
                           column(6,
                                  
                                  ### 5.1.6 Number of results to save ####
                                  ### "blckmdlngMaxSavedResults"
                                  numericInput(inputId = "blckmdlngMaxSavedResults",
                                               label = 'How many results to save?',
                                               value = 10,
                                               min = 1,
                                               step = 10
                                  ),
                                  
                                  
                                  ### 5.1.7 Returning  all ####
                                  ### "blckmdlngAll"
                                  checkboxInput(inputId = "blckmdlngAll",
                                                label = 'Should solution be shown for all partitions (not only the best one)?*',
                                                value = TRUE
                                  ),
                                  withTags(i("Disable for very complex calculation and/or low-end machines")),
                                  
                                  ### 5.1.8 Random Seed ####
                                  ### "blckmdlngRandomSeed"
                                  numericInput(inputId = "blckmdlngRandomSeed",
                                               label = 'Insert a random seed to use it',
                                               value = NULL,
                                               min = 0,
                                               step = 1
                                  ),
                                  
                                  ### 5.1.9 Printing extra info ####
                                  ### "blckmdlngPrintRep"
                                  checkboxInput(inputId = "blckmdlngPrintRep",
                                                label = 'Should some information about each optimization be printed?',
                                                value = TRUE
                                  ),
                                  
                                  ### 5.1.10 Multi-corep processing ####
                                  ### "MultiCore"
                                  checkboxInput(inputId = "MultiCore",
                                                label = 'Should the parallel computation be used?',
                                                value = FALSE
                                                ),
                                  ), # Column 4 
                           ), # Col layout
                         ), # Sidebar panel
                       mainPanel(
                         sidebarLayout(
                           
                             ### 5.1.12 Pre-specified block types ####
                             conditionalPanel(
                               condition = 'input.blckmdlngPrespecifiedYN == true',
                               sidebarPanel(
                                 width = 12,
                                 # checkboxInput(inputId = "ManualPrespecified",
                                 #               label = 'Pre-specify custom blockmodeling parameters manually?',
                                 #               value = TRUE
                                 #               ),
                                 
                                   
                                     # tableOutput(outputId = 'CustomBlockModel'),
                                     DT::dataTableOutput(outputId = 'CustomBlockModel',width = '100%'),
                                     fluidRow(
                                       column(width = 3,
                                              hr(),
                                              ),
                                       column(width = 4,
                                              actionButton(inputId = 'ResetSelectionDT',
                                                           label = 'Reset selection',
                                                           icon = icon(name = 'recycle',
                                                                       lib = "font-awesome")),
                                              ),
                                       column(width = 3,
                                              actionButton(inputId = 'SelectAllDT',
                                                           label = 'Select all',
                                                           icon = icon(name = 'check',
                                                                       lib = "font-awesome")),
                                              ),
                                       column(width = 2,
                                              hr(),
                                              ),
                                                  ), # fluidrow
                                   
                                   #### 5.1.12(A) Hot table and inputs ####
                                 # conditionalPanel(
                                 #   condition = 'input.ManualPrespecified == true',
                                   hr(),
                                   fluidRow(
                                     column(width = 6,
                                            ##### Clusters' size ####
                                            ##### 'CustoomBlockModel_NumberCluster'
                                            numericInput(inputId = "CustoomBlockModel_NumberCluster",
                                                         label = 'Number of cluster',
                                                         value = 3,
                                                         min = 2,
                                                         step = 1),
                                            actionButton(inputId = 'SetSizeDT',
                                                         label = 'Confirm',
                                                         icon = icon(name = "window-maximize",
                                                                     lib = "font-awesome")
                                            ),
          
                                     ), # column
                                     column(width = 6,
                                            ##### Block types ####
                                            ##### 'TowardsDT'
                                            selectInput(inputId = "TowardsDT",
                                                        label = "Select the allowed blocktypes",
                                                        choices = c("null or empty"="nul",
                                                                    "complete"="com",
                                                                    # "row-dominant (binary and valued approach only)"="rdo",
                                                                    # "column-dominant (binary and valued approach only)"="cdo",
                                                                    "(f-)regular"="reg",
                                                                    "row (f-)regular"="rre",
                                                                    "column (f-)regular"= "cre",
                                                                    # "row dominant (binary, valued only)"="rfn",
                                                                    # "column dominant (binary, valued only)"= "cfn",
                                                                    # "density (binary approach only)"="den",
                                                                    # "average (valued approach only)"="avg",
                                                                    "do not care (the error is always zero)"="dnc"),
                                                        selected = c("nul","com"),
                                                        multiple = TRUE
                                            ),
                                            actionButton(inputId = "LoadBlocksIntoDT",
                                                         label = "Load blocks",
                                                         icon = icon(name = "clone",
                                                                     lib = "font-awesome")
                                            ),
                                     ),# End of column with inputs
                                   ), # End of fluid row
                                   #ManualPrespecified ), # End of Conditional panel 
                                 
                                 hr(),
                                 
                                 #### 5.1.12(C) File upload for pre-specified block-types' array
                                 fluidRow(
                                   column(width = 6,
                                          # ###### Use a sample ####
                                          # ###### "Sample" 
                                          # checkboxInput(inputId = "SampleArray",
                                          #               label = 'Use a sample',
                                          #               value = F),
                                          
                                          # conditionalPanel(
                                          #   condition = 'input.SampleArray == false',
                                            ###### Type of uploaded array ####
                                            # "ArrayInput"
                                            radioButtons(inputId = "ArrayInput",
                                                         label = "Type of file to upload?",
                                                         selected = ".RDS",
                                                         choiceValues = c(".RDS",".RData"),
                                                         choiceNames = c("R Data Serialized","R Data"),
                                                         inline = T
                                            ),
                                            
                                            ###### Upload array as RDS file ####
                                            conditionalPanel(
                                              condition = 'input.ArrayInput==".RDS"',
                                              fileInput(inputId = "PrespecifiedArrayRDS",
                                                        label = "Upload a RDS file",
                                                        multiple = F,
                                                        buttonLabel = "Browse",
                                                        placeholder = "Your R file here",
                                                        accept = c(".RDS")
                                              ),
                                            ),# Conditional panel RDS
                                            
                                            ###### Upload array as RData file ####
                                            conditionalPanel(
                                              condition = 'input.ArrayInput==".RData"',
                                              fileInput(inputId = "PrespecifiedArrayRData",
                                                        label = "Upload a RData file",
                                                        multiple = F,
                                                        buttonLabel = "Browse",
                                                        placeholder = "Your R file here",
                                                        accept = c(".RData")
                                                        ),
                                              ), # Conditional panel RData
                                            # ), # Conditional panel SampleArray
                                          ), #Column
                                   column(width = 6,
                                          withTags(i("An array with four dimensions. The first is as long as the maximum number of allowed block types for a given block. The second dimension is the number of relations. The third and the fourth represent rows' and columns' clusters. For more information see", a(href="https://cran.r-project.org/web/packages/blockmodeling/blockmodeling.pdf#page=10",'here',target="_blank"))),
                                          hr(),
                                          actionButton(inputId = 'UploadArray',
                                                       label = 'Load the array to see the block model',
                                                       icon = icon(name = 'upload',
                                                                   lib = 'font-awesome')
                                                      ),
                                          checkboxInput(inputId = 'EditUploadedArray',
                                                        label = 'Ignore the uploaded array',
                                                         value = F
                                                        ),
                                          withTags(h5('Only turn on',i('after'),'loading a block model from file')),
                                          ),
                                   ),
                                 ),# conditional panel blckmdlngPrespecifiedYN==T
                               ), # Sidebar
                             mainPanel(width = 0),
                             ),
                         
                         fluidRow(
                           column(4,
                             ### 5.1.13 (A) Start blockmodeling ####
                             # "blckmdlngRun"
                             withTags(h5(b("Start blockmodeling"))),
                             
                             actionButton(inputId = "blckmdlngRun",
                                          label = "Process data",
                                          icon = icon(name = "calculator",
                                                      lib = "font-awesome")
                                          ),
                           ),
                           column(4,p('')),
                           column(4,
                                  ### 5.1.13(B) Which best partition to print ####
                                  ### "whichIM"
                                  condition = "input.blckmdlngAll==true",
                                  numericInput(inputId = "whichIM",
                                               label = 'Which "best" partition should be printed?',
                                               value = 1,
                                               min = 1,
                                               step = 1,
                                               ),
                                  withTags(i('Affects also error matrix and mean matrix')),
                                  ),
                         ),
                         hr(),
                         
                         
                         fluidRow(
                           column(4,
                                  withTags(h5(b("Upload results"))),
                                  ### 5.1.14 Load blockmodeling results from RDS ####
                                  ### "blckmdlngRDS", "blckmdlngFileRDS"

                                  #### Upload results as RDS file
                                  conditionalPanel(
                                    condition = 'input.blckmdlngRDS==true',
                                    fileInput(inputId = "blckmdlngFileRDS",
                                              label = "Upload a RDS file",
                                              multiple = F,
                                              buttonLabel = "Browse",
                                              placeholder = "Your R file here",
                                              accept = c(".RDS")
                                    ),
                                    withTags(h5(i('Use the "Read Data" button under the "Data upload" tab to read the matrix from this file'))),
                                  ),# Conditional panel RDS

                                  checkboxInput(inputId = "blckmdlngRDS",
                                               label = "Upload blockmodelling results",
                                               value = F),

                           ),
                           column(8,
                                  ### 5.1.15 Download blockmodeling RDS ####
                                  ### "DownloadBlckRDS"
                                  withTags(h5(b("Downloads"))),
                                  p(
                                    downloadButton(outputId = "DownloadBlckRDS",
                                                   label = "Download blockmodeling results",
                                                   icon = icon(name = "download",
                                                               lib = "font-awesome")
                                                   ),
                                    conditionalPanel(
                                      condition = 'input.blckmdlngPrespecifiedYN==true',
                                      withTags(i('After processing the data it will be possible to download the custom blockmodel')),  
                                      downloadButton(outputId = 'downloadCustomBlck',
                                                       label = 'Download custom blockmodel',
                                                       icon = icon(name = "download",
                                                                   lib = "font-awesome")
                                                       ),
                                      ),
                                  ),

                                 p(
                                   ### 5.1.16 Download vector partitions ####
                                   ### "DownloadClu"
                                   downloadButton(outputId = "DownloadClu",
                                                  label = "Download partitions as vector",
                                                  icon = icon(name = "download",
                                                              lib = "font-awesome")
                                   ),
                                 ),
                                  ### 5.1.17 Download image matrix ####
                                  conditionalPanel(
                                    condition = "input.dropIM == true",
                                    p(
                                      # "DownloadIMtext"
                                      downloadButton(outputId = "DownloadIMtext",
                                                     label = "Download image matrix as txt",
                                                     inline=T,
                                                     icon = icon(name = "table",
                                                                 lib = "font-awesome")
                                                     ),

                                    ),

                                  ),
                                 # "DownloadIMrds"
                                  p(
                                    downloadButton(outputId = "DownloadIMrds",
                                                   label = "Download image matrix as RDS",
                                                   inline=T,
                                                   icon = icon(name = "table",
                                                               lib = "font-awesome")
                                    ),
                                  ),
                                  # "DropIM"
                                  checkboxInput(inputId = "dropIM",
                                                label = 'Drop one-element dimensions',
                                                value = TRUE,
                                                width = '100%'
                                  ),

                           ) # Col 8
                         ), # Col layout
                         
                         
                         
                         
                         ## 5.2 Show the blockmodeling's summary ####
                         # "Tableblckmdlng", "Summaryblckmdlng"
                         withTags({
                           div(h4(b("Summary of blockmodeling results")))
                         }),
                         tabsetPanel(
                           tabPanel(title = "Table",
                                    tableOutput("Tableblckmdlng"),
                           ),
                           tabPanel(title = "Summary",
                                    verbatimTextOutput("Summaryblckmdlng"),
                           ),
                           tabPanel(title = "Image matrix",
                                    tableOutput("TableIM")
                                    ),
                           tabPanel(title = "Error matrix",
                                    tableOutput("TableEM")
                           ),
                           tabPanel(title = "Mean matrix",
                                    tableOutput("TableMean")
                           ),
                           ),
                         ), # mainPanel
                       ), # Sidebar layout
                     ), # Tab panel2
          
            # 6. Show the Adjacency Matrix ####
            # "adjOptType","adj","adjPlot"
            tabPanel(title = "Adjacency matrix",
                     sidebarLayout(
                       sidebarPanel(width = 4,
                                    # 6.1 Select network ####
                                    withTags(h4(b("Select matrix"))),
                                    radioButtons(inputId = "adjSelector",
                                                 label = "Which matrix do you want to use?",
                                                 choiceNames = c("original","partitioned"),
                                                 choiceValues = c(1,2)
                                    ),
                                    ## 6.2 Select type of output ####
                                    conditionalPanel(
                                      condition = "input.adjSelector == 1",
                                             withTags(h4(b("Select output"))),
                                             radioButtons(inputId = "adjOptType",
                                                          label = "Type of visualisation",
                                                          choiceNames = c("table","plot"),
                                                          choiceValues = c("t","p")
                                                          ),
                                      conditionalPanel(
                                        condition = "input.adjOptType == 't'",
                                        withTags(h4(b("Export adjacency matrix"))),
                                        ## 6.4 Download adjacency matrix ####
                                        downloadButton(outputId = "downloadAdj",
                                                       label = 'Download',
                                                       icon = icon(name = "download",
                                                                   lib = "font-awesome")
                                                       ),
                                        ), # /input.adjOptType
                                      ), # /input.adjSelector
                                    
                                    ### 6.3 Which best partition to print ####
                                    ### "whichIM_adjPlot"
                                    conditionalPanel(
                                      condition = "input.blckmdlngAll==true",
                                      conditionalPanel(
                                        condition = 'input.adjSelector==2',
                                        numericInput(inputId = "whichIM_adjPlot",
                                                     label = 'Which "best" partition should be printed?',
                                                     value = 1,
                                                     min = 1,
                                                     step = 1,
                                                     ),
                                        ),
                                      ),
                                    
                                    
                                    ),
                       mainPanel(
                         ## 6.3 Table output original matrix ####
                         tableOutput("adj"),
                         ## 6.4 Output plot-matrix ####
                         plotOutput(outputId = "adjPlot"),
                       ),
                       ), #</Sidebarlayout>
                     
                       
                     
                     ), # Tabpanel
            
            tabPanel(title = "Network Plot",
                     # 7 Various sys of network plots ####
                     conditionalPanel(
                       ## 7.1 "Network" and "igraph" sys ####
                       condition = "input.PlotSys != 3",
                       plotOutput("NetworkPlot",
                                  height = 640,width = 800),
                      ),
                     conditionalPanel(
                       condition = "input.PlotSys == 3",
                       ## 7.2 "visNetwork" sys ####
                       visNetworkOutput("igraphPlot",
                                        height = 640,width = 800),
                     ),
                     
                     # 8. Plotting options####
                     # "PlotOpt"
                     withTags({
                       div(h4(b("Plotting options")))
                     }),
 
                     hr(),
                     fluidRow(
                       ## 8.1 Select matrix to plot ####
                       ## "PlotSelector"
                       column(3,
                              withTags(i("Select network")),
                              radioButtons(inputId = "PlotSelector",
                                           label = "Which matrix to use?",
                                           choiceNames = c("original","partitioned"),
                                           choiceValues = c(1,2)
                              ),
                       ),
                       ## 8.2 Select plotting sys ####
                       ## "PlotSys"
                       column(3,
                              withTags(i("Select output")),
                              radioButtons(inputId = "PlotSys",
                                           label = "Which package to use for plotting?",
                                           choiceNames = c("network","igraph","visNetwork"),
                                           choiceValues = c(1,2,3),
                                           inline = TRUE,
                                           selected = 2
                              ),
                         ),
                       column(3,
                       conditionalPanel(
                         condition = 'input.PlotSelector==2',
                         ### 8.3 Which best partition to print ####
                         ### "whichIM_Plot"
                         numericInput(inputId = "whichIM_Plot",
                                      label = 'Which "best" partition should be used for plotting?',
                                      value = 1,
                                      min = 1,
                                      step = 1,
                                      ),
                         ), # /conditionalPanel
                       ), # /column
                     ), # /fluidRow
      
                     ## 8.3 Options for the "network" plotting sys ####
                     conditionalPanel(
                       condition = "input.PlotSys == 1",
                       withTags({
                         div(h4(b("network Plotting Options")))
                       }),
                       ## Layout with mutiple coloumns
                    hr(),
                    fluidRow(
                       column(3,
                              withTags(h5(b("General options"))),
                              ## 8.3.1 Mode ####
                              ## "PlotMode"
                              radioButtons(inputId = "PlotMode",
                                           label = 'Nodes arrangement',
                                           choiceNames = c("Fruchterman-Reingold algorithm","Circle"),
                                           choiceValues = c("fruchtermanreingold","circle"),
                                           inline = TRUE
                                           ),
                              
                              ### 8.3.2 Isolate ####
                              ### "PlotIsolate"
                              checkboxInput(inputId = "PlotIsolate",
                                            label = 'Isolated nodes',
                                            value = TRUE
                              ),
                              
                              ### 8.3.3 Interactive ####
                              ### "PlotInteractive"
                              # checkboxInput(inputId = "PlotInteractive",
                              #               label = 'Should the plot be plot be interactive?*',
                              #               value = FALSE
                              # ),
                              # withTags({
                              #   div(b("(* Can be very slow!)"))
                              # }),
                       ),
                       column(5,
                              h4("Aesthetic options"),
                              conditionalPanel(
                                condition ="input.directionality == true",
                                ### 8.3.4 Arrows ####
                                #### 8.3.4 (A) Whether to override arrows ####
                                # "OverridePlotArrows"
                                checkboxInput(inputId = "OverridePlotArrows",
                                              label = 'Override default arrows*',
                                              value = FALSE
                                ),
                                withTags(p('*',i('By default arrows are shown for directional networks'))),
                                conditionalPanel(
                                  condition ="input.OverridePlotArrows == true",
                                #### 8.3.4 (B) Overriding plot arrows
                                #### "PlotArrows"
                                checkboxInput(inputId = "PlotArrows",
                                              label = 'Display arrows',
                                              value = FALSE
                                              ),
                                ),
                                  #### 8.3.4 (C) Arrow size ####
                                  #### "PlotArrowSize"
                                  sliderInput(inputId = "PlotArrowSize",
                                              ticks = TRUE,
                                              label = 'Dimension of plot\'s arrows',
                                              value = 1,
                                              min = .5,
                                              max = 20,
                                              step = .5
                                              ),
                                ),
                              #### 8.3.4 (D) Message "No directionality, No arrows" ####
                              conditionalPanel(
                                condition ="input.directionality == false",
                                withTags( 
                                  h5(
                                    i(style="color:red;", "Arrows ",u("cannot")," be set"),
                                    i("because they do not make sense for non-directional networks"),
                                  )
                                )
                              ),
                              ### 8.3.5 Node Labels ####
                              #### 8.3.5 (A) Hide labels
                              #### 'NetworkNodeLabelsHide'
                              withTags(h5(b('Hide the nodes\' labels'))),
                              checkboxInput(inputId = 'NetworkNodeLabelsHide',
                                            label = 'Check to hide',
                                            value = T),
                              conditionalPanel(
                                condition = 'input.NetworkNodeLabelsHide==false',
                                #### 8.3.5 (B) Labels' size
                                #### "PlotLabelSize"
                                sliderInput(inputId = "PlotLabelSize",
                                            ticks = TRUE,
                                            label = 'Dimension of plot\'s labels',
                                            value = 1,
                                            min = .5,
                                            max = 20,
                                            step = .5
                                            ),
                                ),
                              
                              ### 8.3.6 Nodes size ####
                              ### "PlotNodeSize"
                              sliderInput(inputId = "PlotNodeSize",
                                          ticks = TRUE,
                                          label = 'Dimension of plot\'s nodes',
                                          value = 5,
                                          min = .5,
                                          max = 10,
                                          step = .5
                                          ),
                              )
                       )
                    ),# END Conditional panel1 : network Plotting Options"
                    conditionalPanel(
                      condition = "input.PlotSys == 2",
                      ## 8.4 Options for the "igraph" plotting sys ####
                      withTags({
                        h4(b("igraph Plotting Options"))
                      }),
                      
                      ## Layout with multiple coloumns
                      hr(),
                      fluidRow(
                        column(3,
                               withTags({
                                 h4("Vertex")
                               }),
                               ### 8.4.1 Size of the node ####
                               ### "PlotVertexSize"
                               sliderInput(inputId = "PlotVertexSize",
                                           ticks = TRUE,
                                           label = 'Dimension of plot\'s nodes',
                                           value = 5,
                                           min = .5,
                                           max = 20,
                                           step = .5
                                           ),
                               ### 8.4.2 Color of the node's frame ####
                               ### "PlotVertexFrameColour"
                               selectInput(inputId = "PlotVertexFrameColour",
                                         label = 'Color of the nodes\' frame',
                                         choices = palette.colors(palette = palette.pals()[16]),
                                         selected = '#3283FE'
                               ),
                               
                               #### 8.4.3 Shape ###
                               #### "PlotVertexShape"
                               selectInput(inputId = "PlotVertexShape",
                                           label = "Shape of the plot's nodes?",
                                           choices = c("Circle"="circle",
                                                       "Square"="square",
                                                       "Rectangle"="rectangle",
                                                       # "Circle and Square"="csquare",
                                                       # "Circle and Rectangle"="crectangle",
                                                       "Vertical Rectangle"="vrectangle",
                                                       "Sphere"="sphere","None"="none"),
                                           selected = 'cicle',
                                           multiple = F
                               ),
                               ### 8.4.4 Hide the nodes' labels
                               #### 'GraphNodeLabelsHide'
                               withTags(h5(b('Hide the nodes\' labels'))),
                               checkboxInput(inputId = 'GraphNodeLabelsHide',
                                             label = 'Check to hide',
                                             value = T),
                                             
                               conditionalPanel(
                                 condition = 'input.GraphNodeLabelsHide==false',
                                 ### 8.4.5 Font Family of the nodes' labels ####
                                 ### "PlotVertexLabelFontFamily"
                                 radioButtons(inputId = "PlotVertexLabelFontFamily",
                                              label = "Node labels' font",
                                              choices = c("Serif"="serif","Sans serif"="sans"),
                                              inline = TRUE
                                 ),
                                 
                                 ### 8.4.6 Size of the node's labels ####
                                 ### "PlotVertexLabelSize"
                                 sliderInput(inputId = "PlotVertexLabelSize",
                                             ticks = TRUE,
                                             label = 'Dimension of node\'s labels',
                                             value = 1,
                                             min = .5,
                                             max = 20,
                                             step = .5
                                 ),
                                 
                                 ### 8.4.7 Distance of the node's labels ####
                                 ### "PlotVertexLabelDist"
                                 sliderInput(inputId = "PlotVertexLabelDist",
                                             ticks = TRUE,
                                             label = 'Labels\' distance from the node',
                                             value = 0.5,
                                             min = .5,
                                             max = 3,
                                             step = .5
                                 ),
                                 ### 8.4.8 Colour of the node's labels ####
                                 ### "PlotVertexLabelColour"
                                 selectInput(inputId = "PlotVertexLabelColour",
                                             label = 'Color of the  nodes\' labels',
                                             choices = palette.colors(palette = palette.pals()[13]),
                                             selected = '#BAB0AC'
                                             ),
                                 ),
                               
                               ),
                        column(4, offset = 1,
                               h4("Edges"),
                               conditionalPanel(
                                 condition = "input.ValuedMatrix == true",
                               
                              ### 8.4.8 Edges width (manual/valued) ####
                               checkboxInput(inputId = "igraphPlotEdgeWidthValues",
                                             label = 'Edges\' width shows the network\'s values',
                                             value = FALSE
                               ),
                               conditionalPanel(
                                 condition = "input.igraphPlotEdgeWidthValues == true",
                                 #### 8.4.8 (A) Max width of the edges ####
                                 #### "PlotEdgeWidth"
                                 sliderInput(inputId = "igraphPlotEdgeMaxWidth",
                                             ticks = TRUE,
                                             label = 'Max width of plot\'s edges',
                                             value = 1,
                                             min = .5,
                                             max = 20,
                                             step = .5),
                               ),
                               ),
                               conditionalPanel(
                                 condition = "input.igraphPlotEdgeWidthValues == false",
                               #### 8.4.8 (B) Width of the edge ####
                               #### "PlotEdgeWidth"
                               sliderInput(inputId = "igraphPlotEdgeWidth",
                                           ticks = TRUE,
                                           label = 'Width of plot\'s edges',
                                           value = .5,
                                           min = .5,
                                           max = 20,
                                           step = .5),
                               ),

                               ### 8.4.9 Colour of the edge ####
                               ### "PlotEdgeColour", 'igraphPlotEdgeShadeValues'
                              
                              checkboxInput(inputId = "igraphPlotEdgeShadeValues",
                                            label = 'Edges\' colour shows the network\'s values',
                                            value = FALSE
                              ),
                              
                              conditionalPanel(
                                condition ="input.igraphPlotEdgeShadeValues == false",
                                selectInput(inputId = "PlotEdgeColour",
                                          label = 'Color of the plot\'s edges',
                                          choices = palette.colors(palette = palette.pals()[13]),
                                          selected = '#BAB0AC'
                                ),
                                ),
                              
                              
                               conditionalPanel(
                                 condition = "input.directionality == true",
                                 ### 8.4.10 Arrows ####
                                 #### 8.4.10 (A) Whether to override arrows ####
                                 #### "OverrideigraphPlotArrows"
                                 checkboxInput(inputId = "OverrideigraphPlotArrows",
                                               label = 'Ovveriding defaultarrow settings?',
                                               value = FALSE
                                 ),
                                 conditionalPanel(
                                   condition ="input.OverrideigraphPlotArrows == true",
                                 #### 8.4.10 (B) Setting overidden plot arrows
                                 #### "igraphPlotArrows"
                                 checkboxInput(inputId = "igraphPlotArrow",
                                               label = 'Display arrows',
                                               value = FALSE
                                               ),
                                 ),
                                   #### 8.4.10 (C) Size of the edge's arrows ####
                                   #### "igraphPlotArrowSize"
                                   sliderInput(inputId = "igraphPlotArrowSize",
                                               label = 'Size of the edge\'s arrows',
                                               value = 1,
                                               min = .5,
                                               max = 20,
                                               step = .5
                                               ),
                                   #### 8.4.10 (D) Width of the edge's arrows
                                   #### "PlotArrowWidth"
                                   # sliderInput(inputId = "PlotArrowWidth",
                                   #           label = 'Size of the edge\'s width',
                                   #           value = 1,
                                   #           min = .5,
                                   #           max = 20,
                                   #           step = .5
                                   #           ),
                                 ),
                               #### 8.4.10 (B) Message "No directionality, No arrows" ####
                               conditionalPanel(
                                 condition ="input.directionality == false",
                                 withTags( 
                                   h5(
                                     i(style="color:red;", "Arrows ",u("cannot")," be set"),
                                     i("because they do not make sense for non-directional networks"),
                                   )
                                 )
                               ),
                               ### 8.4.12 Font Family of the edges' labels ####
                               ### "PlotEdgeLabelFontFamily"
                               radioButtons(inputId = "PlotEdgeLabelFontFamily",
                                            label = "Edge labels' font",
                                            choices = c("Serif"="serif","Sans serif"="sans"),
                                            inline = TRUE
                               ),
                               ### 8.4.13 Size of the edges's labels ####
                               ### "PlotEdgeLabelSize"
                               # sliderInput(inputId = "PlotEdgeLabelSize",
                               #             ticks = TRUE,
                               #             label = 'Dimension of edge\'s labels',
                               #             value = 1,
                               #             min = .5,
                               #             max = 20,
                               #             step = .5
                               # ),
                               ### 8.4.14 Distance of the edges's labels ####
                               ### "PlotEdgeLabelDist"
                               # sliderInput(inputId = "PlotEdgeLabelDist",
                               #             ticks = TRUE,
                               #             label = 'Labels\' distance from edges',
                               #             value = 0.5,
                               #             min = .5,
                               #             max = 3,
                               #             step = .5
                               # ),
                               ### 8.4.15 Color of the edges' labels ####
                               ### "PlotEdgeLabelColour"
                              selectInput(inputId = "PlotEdgeLabelColour",
                                         label = 'Colot of the plot\'s edges',
                                         choices = palette.colors(palette = palette.pals()[13]),
                                         selected = '#BAB0AC'
                              ),
                        ),
                        column(4,
                               h4("Aesthetic option"),
                               ### 8.4.16 Curved ####
                               ### "PlotEdgeCurved"
                               checkboxInput(inputId = "PlotEdgeCurved",
                                             label = 'Curved edges',
                                             value = FALSE
                               ), 
                               
                               
                               conditionalPanel(
                                 condition = "input.PlotSelector==1",
                                 conditionalPanel(
                                   condition = "input.AddAttr==true",
                                   ### 8.4.1 Color of the nodes ####
                                   checkboxInput(inputId = 'AttrVertexColYN',
                                                 label = 'Colour nodes from attribute',
                                                 value = F),
                                 ),
                                 
                                 conditionalPanel(
                                   condition = 'input.AttrVertexColYN==false',
                                   #### 8.4.1 (A) Color of the node without partitions ####
                                   #### "PlotVertexColour"
                                   selectInput(inputId = "PlotVertexColour",
                                             label = 'Color of the plot\'s nodes',
                                             choices = palette.colors(palette = palette.pals()[16]),
                                             selected = '#3283FE'
                                   ),
                                 ),
                                 ),
                                 conditionalPanel(
                                   condition = "input.PlotSelector==1&&input.AttrVertexColYN==true",
                                   selectInput(inputId = 'NodePaletteGraph',
                                               label = 'Select palette* for nodes\' colour',
                                               choices = palette.pals(),
                                               selected = palette.pals()[2],
                                               multiple = F
                                   ),
                                 ),
                               conditionalPanel(
                                 condition = "input.PlotSelector==2",
                                 #### 8.4.1 (B) Colour of the partitions ####
                                 selectInput(inputId = 'PlotPaletteGraph',
                                             label = 'Select palette* for clusters\' colour',
                                             choices = palette.pals(),
                                             selected = palette.pals()[2],
                                             multiple = F
                                 ),
                                 div(textOutput(outputId = 'WarningNumColoursGraph'),style='color:red;background-color: #DADADA;	margin-top: 5px; margin-right: 5px; margin-bottom: 5px; margin-left: 5px;text-align: center'),
                                 # Identify palettes |Commented out| ####
                                 # WRN<-warnings(
                                 #   for(i in 1:length(palette.pals())){
                                 #     palette.colors(n = 50,palette = palette.pals()[i])
                                 #   })
                                 # 
                                 # Palettes8Cols<-
                                 #   paste(palette.pals()[grep(names(WRN),pattern = '8')], collapse = ', ')
                                 # Palettes9Cols<-
                                 #   paste(palette.pals()[grep(names(WRN),pattern = '9')],collapse = ', ')
                                 # Palettes10Cols<-
                                 #   paste(palette.pals()[grep(names(WRN),pattern = '10')], collapse = ', ')
                                 # Palettes26Cols<-
                                 #   paste(palette.pals()[grep(names(WRN),pattern = '26')], collapse = ', ')
                                 # Palettes36Cols<-
                                 #   paste(palette.pals()[grep(names(WRN),pattern = '36')], collapse = ', ')
                                 
                                 withTags(div(b("8 colours"),':',i("R3, R4, ggplot2, Accent, Dark 2, Pastel 2, Set 2"))),
                                 withTags(div(b("9 colours"),':',i("Okabe-Ito, Pastel 1, Set 1"))),
                                 withTags(div(b("10 colours"),':',i("Paired, Set 3, Tableau 10, Classic Tableau"))),
                                 withTags(div(b("26 colours"),':',i("Alphabet"))),
                                 withTags(p(b("36 colours"),':',i("Polychrome 36"))),
                                 
                               ),
                               
                        ),
                      ),
                    ),# END Conditional panel2
                    conditionalPanel(
                      condition = "input.PlotSys == 3",
                      ## 8.5 Options for the "visNetwork" plotting sys ####
                      withTags({
                        h4(b("visNetwork Plotting Options"))
                      }),
                      # Layout with mutiple coloumns
                      hr(),
                      fluidRow(
                        column(3,
                               withTags({
                                 h4("Plotting options")
                               }),
                               ### 8.5.1 Title of the plot ####
                               ### "visTitle"
                               textInput(inputId = "visTitle",
                                         label = 'Title of the plot',
                                         value = NULL
                               ),
                               ### 8.5.2 Subtitle of the plot ####
                               ### "visSubtitle"
                               textInput(inputId = "visSubtitle",
                                         label = 'Subtitle of the plot',
                                         value = NULL
                               ),
                               ### 8.5.3 Color background ####
                               ### "visBackground"
                               selectInput(inputId = "visBackground",
                                         label = 'Color of the plot\'s background',
                                         choices = c('peach'='#FBB4AE','pastel light blue'='#B3CDE3','pastel  green'='#CCEBC5','pastel purple'='#DECBE4','pastel orange'='#FED9A6','pastel yellow'='#FFFFCC','pastel brown'='#E5D8BD','pastel pink'='#FDDAEC','pastel grey'='#F2F2F2','white'='#FFFFFF'),
                                         selected = '#FFFFFF'
                               ),
                        ),
                        column(4, offset = 1,
                               h4("Hierarchy"),
                               ### 8.5.4 Hierarchy ####
                               ### "visHier"
                               checkboxInput(inputId = "visHier",
                                             label = 'Hierarchical network',
                                             value = FALSE
                               ),
                               conditionalPanel(
                                 condition = "input.visHier == 1",
                                 #### 8.5.4 (A) Direction of the nodes ####
                                 #### "visHierDirection"
                                 radioButtons(inputId = "visHierDirection",
                                              label = "Direction",
                                              choices = c("up-down"="UD", "down-up"="DU",
                                                          "left-right"="LR", "right-left"="RL"),
                                              inline = TRUE
                                 ),
                                 #### 8.5.4 (B) Parent centralisation ####
                                 #### "visHierCentralisation"
                                 checkboxInput(inputId = "visHierCentralisation",
                                               label = 'Centralise parent nodes',
                                               value = FALSE
                                 ),
                               ),
                        ),
                        column(4,
                               h4("Aestetics"),
                               
                               withTags(h5(b("Nodes"))),
                               ### 8.5.5 Nodes' colours ####
                               conditionalPanel(
                                 condition = "input.PlotSelector==1",
                                 conditionalPanel(
                                   condition = "input.AddAttr==true",
                                   checkboxInput(inputId = 'visNetworkAttrVertexColYN',
                                                 label = 'Colour nodes from attribute',
                                                 value = F),
                                 ),
                                 conditionalPanel(
                                   condition = 'input.visNetworkAttrVertexColYN==false',
                                   #### "visNetworkNodeColour"
                                   selectInput(inputId = "visNetworkNodeColour",
                                             label = 'Color of the plot\'s nodes',
                                             choices = palette.colors(palette = palette.pals()[16]),
                                             selected = '#3283FE'
                                   ),
                                   #### "visNetworkNodeBorder"
                                   selectInput(inputId = "visNetworkNodeBorder",
                                             label = 'Color of the nodes\' border',
                                             choices = palette.colors(palette = palette.pals()[13]),
                                             selected = '#BAB0AC'
                                   ),
                                 ),
                               ),
                               conditionalPanel(
                                 condition = "input.PlotSelector==1&&input.visNetworkAttrVertexColYN==true",
                                 selectInput(inputId = 'visNetworkAttrPalette',
                                             label = 'Select palette* for nodes\' colour',
                                             choices = palette.pals(),
                                             selected = palette.pals()[2],
                                             multiple = F
                                 ),
                               ),
                               conditionalPanel(
                                 condition = 'input.PlotSelector==2',
                                 ### 'PlotPaletteVIS'
                                 selectInput(inputId = 'PlotPaletteVIS',
                                             label = 'Select palette for clusters\' colour',
                                             choices = palette.pals(),
                                             selected = palette.pals()[6],
                                             multiple = F),
                                 hr(),
                                 div(textOutput(outputId = 'WarningNumColoursVIS'),style='color:red;background-color: #DADADA;	margin-top: 5px; margin-right: 5px; margin-bottom: 5px; margin-left: 5px;text-align: center'),
                                 hr(),
                                 withTags(div(b("8 colours"),':',i("R3, R4, ggplot2, Accent, Dark 2, Pastel 2, Set 2"))),
                                 withTags(div(b("9 colours"),':',i("Okabe-Ito, Pastel 1, Set 1"))),
                                 withTags(div(b("10 colours"),':',i("Paired, Set 3, Tableau 10, Classic Tableau"))),
                                 withTags(div(b("26 colours"),':',i("Alphabet"))),
                                 withTags(p(b("36 colours"),':',i("Polychrome 36"))),
                                 ),
                               
                               
                               ### 8.5.6 Nodes' shape
                               selectInput(inputId = 'visNetworkNodeShape',
                                           label = 'Shape',
                                           choices = c('Square'="square", 'Triangle'="triangle",
                                                       'Box'="box", 'Circle'="circle", 'Dot'="dot",
                                                       'Star'="star", 'Ellipse'="ellipse", 'Database'="database",
                                                       'Diamond'="diamond"),
                                           selected = 'circle',
                                           multiple = F),
                               
                               ### 8.5.6 Nodes' size
                               ### "visNetworkNodeSize"
                               sliderInput(inputId = "visNetworkNodeSize",
                                           ticks = TRUE,
                                           label = 'Dimension of plot\'s nodes',
                                           value = 5,
                                           min = .5,
                                           max = 20,
                                           step = .5
                                           ),
                               ### 8.5.6 Nodes' shadow
                               ### 'visNetworkNodeShadow'
                               checkboxInput(inputId = 'visNetworkNodeShadow',
                                             label = 'Draw a shadow?',
                                             value = T),
                               conditionalPanel(
                                 condition = 'input.visNetworkNodeShadow==true',
                                 ### 'visNetworkNodeShadowSize'
                                 sliderInput(inputId = "visNetworkNodeShadowSize",
                                             ticks = TRUE,
                                             label = 'Dimension of nodes\' shadows',
                                             value = 5,
                                             min = .5,
                                             max = 20,
                                             step = .5
                                             ),
                                 ),
                               
                               
                               withTags(h5(b("Edges"))),
                               
                               ### 8.5.7 Edges' colour
                               ### "visNetworkEdgeColour"
                               textInput(inputId = "visNetworkEdgeColour",
                                         label = 'Color of the plot\'s edges',
                                         value = "SkyBlue"
                               ),
                               
                               ### 8.5.7 Edges' highlight colour
                               ### "visNetworkEdgeHighlight"
                               textInput(inputId = "visNetworkEdgeHighlight",
                                         label = 'Color of the higlighted edge',
                                         value = "yellow"
                               ),
                               
                               ### 8.5.8 Edges' shadow
                               ### 'visNetworkNodeShadow'
                               checkboxInput(inputId = 'visNetworkEdgeShadow',
                                             label = 'Draw a shadow?',
                                             value = T),
                               
                               ),
                        ),
                      )# Conditional panel3
                    ), # Tab panel4
            ),# Tabset panel
    )# ui

# §2 Output ####
server <- function(input, output, session) {
  
  # 0. Reactive values ####
  Tbl<-reactiveValues(Current = NULL,Rows=NULL,Cols=NULL)
  Blck<-reactiveValues(RunAlready = FALSE,Custom=NULL)
  
  # 0.1 Reset 'Blck$RunAlready' if it becomes NULL
  observeEvent(eventExpr = c(Blck$RunAlready),handlerExpr = {
    YN<-Blck$RunAlready
    if(is.null(YN)){
      Blck$RunAlready<<-FALSE
    }
  })
  
  # 1. Reading data ####
  # "aj"
  ReadData<-eventReactive(input$aj,{
    ## 1.1 Preloaded data 
    if(input$blckmdlngRDS){
      
      ### 1.1.1 From block model results
      dat<-mdllng()$initial.param$M
      MatrixType<-"adjacency"
      
      #### Blockmodeling was run beforehand
      Blck$RunAlready<<-TRUE
      
    } else if(input$Sample){
      ### 1.1.2 From sample
      dat<-readRDS(file = "./Sample.rds")
      MatrixType<-"adjacency"
      
      #### Blockmodeling wasn't run yet
      Blck$RunAlready<<-FALSE
    } else {
      
      #### Blockmodeling wasn't run yet
      Blck$RunAlready<<-FALSE
      
      ## 1.2 Options for text/plain files ####
      if(input$type!=4){
        
        ### 1.2.1 If the separator is 'other' ####
        ### "OtherSep"
        if(input$OtherSep!="")input$sep<-input$OtherSep
        
        ### Notification "Reading list in progress"
        showNotification(ui = "Reading data from uploaded list",
                         type = 'default', id = 'ReadingList',
                         duration = NULL, closeButton = F)
        
        ### 1.2.3 Determine type of file provided ####
        if(input$type==1){
          MatrixType<-"adjacency"
          # For adj matrix, the row names should always be
          # in the first column
          ListRowNames<-1
        } else {
          ListRowNames<-NULL
          if(input$type==2)MatrixType<-"edgelist"
          if(input$type==3)MatrixType<-"incidence"
        }
        
        #### 1.2.4 Reads the data from file ####
        #### "List", "sep", "whites",
        UploadedFile<-input$List
        dat <- read.delim(file = UploadedFile$datapath,
                          sep = input$sep,
                          strip.white = input$whites,
                          # row.names = 1,
                          row.names = ListRowNames,
                          header = input$ListHeader)
        dat<-as.matrix(x = dat)
        
        removeNotification('ReadingList')
        
      } else {
        ## 1.3 Pajek input ####
        ## "PajekFile", "PajekInput"
        
        ### Notification "Reading list in progress"
        showNotification(ui = 'Reading Pajek file',
                         type = 'default', id = 'ReadingPajek',
                         duration = NULL, closeButton = F)
        
        
        
        MatrixType<-"adjacency"
        UploadedFile<-input$PajekFile
        
        ### 1.3.1 Reads the data from Pajek .net file ####
        if(input$PajekInput=="PajekNetwork"){
          dat <- loadnetwork(filename = UploadedFile$datapath,
                             useSparseMatrix = F)
        }
        
        ### 1.3.2 Reads the data from Pajek .mat file ####
        if(input$PajekInput=="PajekMatrix"){
          loadmatrix(filename = UploadedFile$datapath)
          dat <- loadmatrix(filename = UploadedFile$datapath)
        }
        
        removeNotification('ReadingPajek')
      }
    }
    
    dat
    })

  # 2. Create network object ####
  NW<-eventReactive(ReadData(),{
    dat<-ReadData()
    ## 2.1 Determine type of file provided
    if(input$type== 1){
      MatrixType<-"adjacency"
      # For adj matrix, the row names should always be
      # in the first column
      ListRowNames<-1
    } else {
      ListRowNames<-NULL
      if(input$type==2)MatrixType<-"edgelist"
      if(input$type==3)MatrixType<-"incidence"
      if(input$type==4)MatrixType<-"adjacency"
    }
    
    ## 2.2 Checks for valued networks ####
    ## "ValuedMatrix", "ValuesName"
    if(input$ValuedMatrix){
      IgnoreEval<-FALSE
      ValuesName<-"weights"
    } else {
      IgnoreEval<-TRUE
      ValuesName<-NULL
    }
    
    ## 2.3 Turn the matrix into a network ####
    ## "directionality","loops", "parallel"
    dat <- network::network(x = dat,
                            directed = input$directionality,
                            loops = input$loops,
                            # multiple = input$parallel,
                            matrix.type = MatrixType,
                            ignore.eval = IgnoreEval,
                            names.eval = ValuesName)
    
    ### 2.3.1 Notification "Multiplex matrix"
    if(is.multiplex(dat)){
      showNotification(ui = "The uploaded list contains a multiplex matrix. The matrix may need to be simplified by removing both loops and multiple edges.",
                       type = 'warning',
                       duration = 5, closeButton = T)
    }
    
    ### 2.3.2 Notification "Bipartite matrix"
    if(network::is.bipartite(dat)){
      showNotification(ui = "The uploaded list contains a bipartite matrix. Bipartition will be ignored",
                       type = 'warning',
                       duration = 5, closeButton = T)
    }
    
    ### 2.3.3 Notification "Reading file completed"
    showNotification(ui = "Elaboration of uploaded file completed",
                     type = 'default',
                     duration = 10, closeButton = T)
    
    ## 2.4 Add attributes ####
    if(input$AddAttr){
      AddAttrVal<-read.table(file = input$AddAttrFile$datapath,
                             header = F,quote = "",col.names = F)
      network::set.vertex.attribute(x = dat,attrname = input$AddAttrName,
                                    value = AddAttrVal)
    }
    
    dat
  })
  
  # 3. Get adjacency matrix ####
  # Converts edge lists and incidence matrices in adjacency matrix
  
  GetAdjacencyMatrix<-eventReactive(ReadData(),{
    ## 3.1 data from file ####
      
      ### Determine type of file provided
      if(input$type==2||input$type==3){
        
        ### 3.1.1 For edge lists and incidence matrices ####
        if(input$type==2)MatrixType<-"edgelist"
        if(input$type==3)MatrixType<-"incidence"
        
        #### Reads the data as a network object
        M<-NW()
        
        #### Converts the network in an adjacency matrix
        if(input$ValuedMatrix){
          ### 3.1.1 (A) for valued networks ####
          M<-as.matrix.network(x = dat,matrix.type = "adjacency",
                               attrname = "weights")
        } else {
          #### 3.1.1 (B) for non-valued networks ####
          M<-as.matrix.network(x = dat,matrix.type = "adjacency")
        }
      } else {
        ## 3.2 Otherwise, the data is already in the right format
        M<-ReadData()
      }
    M
  })
  
  # 4. Outputting summary text ####
  # "summary", "IncludeAdj"
  
  output$summary <- renderPrint({
    dat<-NW()
    network::summary.network(dat,print.adj = input$IncludeAdj)
    })
  
  # # Multiplex-network warnings
  # output$AdjWarning<-renderText({
  #   dat<-NW()
  #   # igraph version
  #   igraphDat <- intergraph::asIgraph(dat)
  #   
  #   # Manages multiplex networks
  #   if(is.multiplex(dat)){
  #     ###
  #   }
  #   })
  
  # 5. Plotting adjacency matrix ####
  # "adjPlot", "adjOptType", "adjSelector"
  
  output$adjPlot<-renderPlot({
    
    # Checks if the user selected the original (= 1) or the
    # partitioned (= 2) matrix
    if(input$adjSelector==2){
      
      ## 5.1 Plotting the partitioned adjacency matrix ####
      ## Loads blockmodeling's result
      if(Blck$RunAlready==TRUE){
        dat<-mdllng()
        output<-plot(dat,main="",which = input$whichIM_adjPlot)
      } else {
        dat<-GetAdjacencyMatrix()
        dat<<-dat
        ## Plots the original matrix
        output<-plotMat(x = dat,ylab = '',xlab = '',plot.legend = F,
                main = '',title.line = '')
        
        ## Notification "Plotting original matrix instead of partititoned" 
        showNotification(ui = 'Plotting original matrix instead of partitioned because blockmodeling had not been ran yet',
                         type = 'warning',
                         duration = 20, closeButton = T)
      }
      ## Plots the partitioned matrix
      
    } else {
      
      ## 5.2 Checks if the user selected "plot" or "table" for the original matrix
      if(input$adjOptType=="t") return(NULL)
      
      ## 5.3 Prints original adjacency matrix ####
        
        ## Load the matrix
        dat<-GetAdjacencyMatrix()
        
        ## Plots the original matrix
        output<-plotMat(x = dat,ylab = '',xlab = '',plot.legend = F,
                main = '',title.line = '')
    }
    output
  },height = 600,width = 800,res = 128)
  
  # 6. Outputting the adjacency table ####
  # "adj"  
  output$adj <- renderTable({
    
    ## 6.1 Check what output was requested ####
    if(input$adjOptType=="p") return(NULL)
    if(input$adjSelector==2) return(NULL)
      
    ## 6.2 Reads matrix ####
    dat<-GetAdjacencyMatrix()
    
    ## 6.3 Prints the matrix ####
    dat
    },rownames = TRUE) 
  
  # 7. Non interactive plots ####
  # "NetworkPlot", "PlotSys"
  output$NetworkPlot<-renderPlot({
    
    ## Checks the plotting system
    if(input$PlotSys==1){
      ## 7.1 Plotting with network ####
      
      ## 7.1.1 Adds the partitions if needed
      
      if(input$PlotSelector==2){
        ## Reads data in
        dat<-NW()
        clu<-blockmodeling::clu(res=mdllng(),which = input$whichIM_Plot)
        dat<-
          network::set.vertex.attribute(x = dat,
                                        attrname = "cluster",
                                        value = clu)
      } else {
        dat<-NW()
      }
      
      ### 7.1.2 Checks setting for the arrows ####
      if (input$OverridePlotArrows){
        PlotArrows<-input$PlotArrows
      } else {
        PlotArrows<-input$directionality
      }
      
      
      ### 7.1.3 With or without partitions?
      if(input$PlotSelector==2){
        VertexCol<-"cluster"
      } else {
        VertexCol<-2
      }
      
     ### Plotting
      network::plot.network(x = dat,
                            usearrows = PlotArrows,
                            mode = input$PlotMode,
                            displayisolates = input$PlotIsolate,
                            # interactive = PlotInteractive,
                            arrowhead.cex = input$PlotArrowSize,
                            label.cex = input$PlotLabelSize,
                            vertex.cex = input$PlotNodeSize,
                            vertex.col= VertexCol,
                            label=network.vertex.names(dat),
                            displaylabels= input$NetworkNodeLabelsHide
      )
      
      } else {
        ## 7.2 Plotting with igraph ###
        ### Create graph
        {
          if(input$directionality){
            iGraphDir<-'directed'
          } else {
            iGraphDir<-'undirected'
          }
          
          if(input$ValuedMatrix){
            iGraphValued<-TRUE
          } else {
            iGraphValued<-NULL
          }
          
          dat2<-
            igraph::graph.adjacency(adjmatrix = GetAdjacencyMatrix(),
                                    weighted = iGraphValued,
                                    mode = iGraphDir,
                                    add.rownames = TRUE
            )
          ## Add attributes
          if(input$AddAttr){
            AddAttrVal<-read.table(file = input$AddAttrFile$datapath,
                                   header = F,quote = "",col.names = F)
            V(dat2)$Added.Attr<-AddAttrVal[[1]]
          }
          
          ### 7.3.3 With or without partitions?
          if(input$PlotSelector==2){
            V(dat2)$cluster<-clu(res = mdllng(),which = input$whichIM_Plot)
            # Assigns colours to each partition
            
            NodesColours <-
              palette.colors(n = length(unique(V(dat2)$cluster)),
                             palette = input$PlotPaletteGraph)
            
            V(dat2)$color <- NodesColours[V(dat2)$cluster]
            
          } else if(input$AttrVertexColYN){
            NodesColours <- palette.colors(n = length(unique(V(dat2)$Added.Attr)),
                                           palette = input$NodePaletteGraph)
            for(i in 1:length(unique(V(dat2)$Added.Attr))){
              V(dat2)$Added.Attr<-gsub(pattern = unique(V(dat2)$Added.Attr)[i],
                                       replacement = i, x = V(dat2)$Added.Attr)
            }
            
            V(dat2)$color <- NodesColours[as.numeric(V(dat2)$Added.Attr)]
          } else {
            
            V(dat2)$color <- input$PlotVertexColour
          }
        }
        
        ### 7.2.1 Edges width (manual/valued) ####
        # Checks if the user wants the edges' width to
        # represent the network's value
        if(input$igraphPlotEdgeWidthValues==FALSE){
          igraphPlotEdgeWidth <- input$igraphPlotEdgeWidth
        } else {
          temp <- igraph::get.edge.attribute(dat2)$weight
          MaxTemp<- max(temp)
          igraphPlotEdgeWidth <- input$igraphPlotEdgeMaxWidth/MaxTemp*temp
        }
        
        ### 7.2.2 Arrow setting ####
        if(input$OverrideigraphPlotArrows){
          #### Overriding arrows
          if(input$igraphPlotArrow==FALSE) igraphPlotArrow <- 0
          if(input$igraphPlotArrow==TRUE) igraphPlotArrow <- 2
        } else {
          #### Default settings
          if(input$directionality==FALSE) igraphPlotArrow <- 0
          if(input$directionality==TRUE) igraphPlotArrow <- 2
        }
        
        ### 7.2.1 Edges shade (manual/valued) ####
        # Checks if the user show the edges' values as
        # a shade of the colour of the network's edges
        if(input$igraphPlotEdgeShadeValues==FALSE){
          igraphPlotEdgeColour <- input$PlotEdgeColour
        } else {
          # Prepare the final vector
          igraphPlotEdgeColour <- E(dat2)$weight
          # Extract unique weights values
          WhichWeights <- unique(igraphPlotEdgeColour)
          # Determine ratio min/max
          FairestGrey<-min(WhichWeights)/max(WhichWeights)
          # Create adequate greyscale
          greys <- grey.colors(n = length(WhichWeights),
                               start = 1-FairestGrey,
                               end = 0)
          # Extendd grey scale to the whole series of weights
          for(i in 1:length(WhichWeights)){
            igraphPlotEdgeColour[E(dat2)$weight==WhichWeights[i]]<-greys[i]
          }
        }
        
        ### 7.2.3 Checks if the user wants to hide the nodes' labels
        if(input$GraphNodeLabelsHide){
          iGraphLabels<-NA
        } else {
          iGraphLabels<-V(dat2)$name
        }
        
        if (input$PlotSys==2) {
          # If the user selected igraph

          
          ### Plots igraph
          igraph::plot.igraph(x = dat2,
                              vertex.label= iGraphLabels,
                              vertex.size = input$PlotVertexSize,
                              # vertex.color= ,
                              vertex.frame.color = input$PlotVertexFrameColour,
                              vertex.shape = input$PlotVertexShape,
                              vertex.label.family = input$PlotVertexLabelFontFamily,
                              vertex.label.cex = input$PlotVertexLabelSize,
                              vertex.label.dist = input$PlotVertexLabelDist,
                              vertex.label.color = input$PlotVertexLabelColour,
                              edge.color = igraphPlotEdgeColour,
                              edge.width = igraphPlotEdgeWidth,
                              edge.arrow.mode = igraphPlotArrow,
                              edge.arrow.size = input$igraphPlotArrowSize,
                              # arrow.width = input$PlotArrowWidth,
                              edge.label.family = input$PlotEdgeLabelFontFamily,
                              # edge.label.cex = input$PlotEdgeLabelSize,
                              # edge.label.dist = input$PlotEdgeLabelDist,
                              edge.label.color = input$PlotEdgeLabelColour,
                              edge.curved = input$PlotEdgeCurved
                              )
          } else {
            # If the user selected VisNetwork
            return(NULL)
            } # else of if PlotSystem != 2
        } # else of if PlotSystem == 1
    },height = 800,width = 600,res = 128)
  
  ## 7.3 Warning for short palette
  warningGraph<-eventReactive(input$PlotPaletteGraph,{
    if(length(palette.colors(palette = input$PlotPaletteGraph))<length(unique(clu(res = mdllng(),which = input$whichIM_Plot)))){
      wrn<-paste('Select a palette supporting at least', length(unique(clu(res = mdllng(),which = input$whichIM_Plot))), 'colours!')
    } else {wrn<-NULL}
    return(wrn)
  })
  output$WarningNumColoursGraph<-renderText({
    wrn<-warningGraph()
    wrn
  })
  
  # 8. Plotting with VisNetwork ####
  output$igraphPlot<-renderVisNetwork({
    if(input$PlotSys==3){
      ## If the user selected VisNetwork
      ## Create graph
      ### Create graph
      {
        if(input$directionality){
          iGraphDir<-'directed'
        } else {
          iGraphDir<-'undirected'
        }
        
        if(input$ValuedMatrix){
          iGraphValued<-TRUE
        } else {
          iGraphValued<-NULL
        }
        
        dat2<-
          igraph::graph.adjacency(adjmatrix = GetAdjacencyMatrix(),
                                  weighted = iGraphValued,
                                  mode = iGraphDir,
                                  add.rownames = TRUE
          )
        ## Add attributes
        if(input$AddAttr){
          AddAttrVal<-read.table(file = input$AddAttrFile$datapath,
                                 header = F,quote = "",col.names = F)
          V(dat2)$Added.Attr<-AddAttrVal[[1]]
        }
        
        ### 8.1 With or without partitions? ####
        if(input$PlotSelector==2){
          V(dat2)$cluster<-clu(res = mdllng(),which = input$whichIM_Plot)
          # Assigns colours to each partition
          
          NodesColours <-
            palette.colors(n = length(unique(V(dat2)$cluster)),
                           palette = input$PlotPaletteVIS)
          
          V(dat2)$color <- NodesColours[V(dat2)$cluster]
          
        } else if(input$visNetworkAttrVertexColYN){
          NodesColours <- palette.colors(n = length(unique(V(dat2)$Added.Attr)),
                                         palette = input$visNetworkAttrPalette)
          for(i in 1:length(unique(V(dat2)$Added.Attr))){
            V(dat2)$Added.Attr<-gsub(pattern = unique(V(dat2)$Added.Attr)[i],
                                     replacement = i, x = V(dat2)$Added.Attr)
          }
          
          V(dat2)$color <- NodesColours[as.numeric(V(dat2)$Added.Attr)]
        } else {
          V(dat2)$color <- input$visNetworkNodeColour
        }
      }
      
      ## Converts to visNetwork
      dat3<-toVisNetworkData(dat2)
      dat3<<-dat3
      
      ## adds correct labels
      dat3$nodes$label<-dat3$nodes$name
      
      visNetwork(nodes = dat3$nodes, edges = dat3$edges,
                 main = input$visTitle,
                 submain = input$visSubtitle,
                 background=input$visBackground)%>%
        visOptions(nodesIdSelection = F,,
                   height = 600,width = 800,
                   manipulation = F)%>%
        visNodes(shape = input$visNetworkNodeShape,
                 size = input$visNetworkNodeSize,
                 color = list(border = input$visNetworkNodeBorder),
                 shadow = list(enabled = input$visNetworkNodeShadow,
                               size = input$visNetworkNodeShadowSize))%>%
        visEdges(shadow = input$visNetworkEdgeShadow,
                 color = list(color = input$visNetworkEdgeColour,
                              highlight = input$visNetworkEdgeHighlight))%>%
        visHierarchicalLayout(enabled = input$visHier, direction = input$visHierDirection,
                              parentCentralization = input$visHierCentralisation)
      
    }
  })
  
  ## 8.4 Warning for short palette
  warningVIS<-eventReactive(input$PlotPaletteVIS,{
    if(length(palette.colors(palette = input$PlotPaletteVIS))<length(unique(clu(res = mdllng(),which = input$whichIM_Plot)))){
      wrn<-paste('Select a palette supporting at least', length(unique(clu(res = mdllng(),which = input$whichIM_Plot))), 'colours!')
    } else {wrn<-NULL}
    return(wrn)
  })
  output$WarningNumColoursVIS<-renderText({
    wrn<-warningVIS()
    wrn
  })
  
  # 9. Operating blockmodeling ####
  # "blckmdlng"
  
  mdllng <- eventReactive(input$blckmdlngRun, {
    
    if(input$blckmdlngRDS){
      ## 9.1 Blockmodeling from RDS file
      
      ## Notification "Reading block-model results from RDS" 
      showNotification(ui = "Reading blockmodeling's results from file",
                       type = 'message',id = 'FileCustomBlckmdlng',
                       duration = 10, closeButton = T)
      
      ## Reading RDS file
      UploadedResults<-input$blckmdlngFileRDS
      blck<-readRDS(file = UploadedResults$datapath)
    } else {
      ## Loads data
      M<-GetAdjacencyMatrix()
      
      ## 9.2 Checks if the M parameter should be considered ####
      ## "paramM"
      if(input$blckmdlngApproach=="val"){
        # For valued blockmodeling
        ParamM<-input$ParamM
        usePreSpecM<-T
      } else if(input$ThresholdSelected==TRUE&&input$blckmdlngApproach=="bin"){
        # For binary blockmodeling, if chosen
        ParamM<-input$ParamThreshold
        usePreSpecM<-T
      } else {
        # For all other options, including binary blockmodeling with M not chosen
        ParamM<-NULL
        usePreSpecM<-NULL
      }
      
      ## 9.3 Checks if multi-core was allowed ####
      if(input$MultiCore){
        MultiCore<-0
      } else {
        MultiCore<-1
      }
      
      ## 9.4 Checks customised blockmodeling ####
      if(input$blckmdlngPrespecifiedYN|input$EditUploadedArray){
        ### Use DT block-model
        condition<-magrittr::and(is.null(input$PrespecifiedArrayRDS),
                                 is.null(input$PrespecifiedArrayRData))
        if(condition&input$EditUploadedArray){
          showNotification(ui = 'Ignoring the uploaded array was activated, but no array had been uploaded. Please, correct!',
                           duration = 10,type = 'warning')
        }
        
        condition<-magrittr::or(condition,
                                input$EditUploadedArray)
        
        if(condition){
          ### 9.4.2 Table block-model ####
          
          #### Notification "Reading the manually imputed, custom block-model" 
          showNotification(ui = "Reading the manually imputed, custom block-model",
                           type = "message",id = 'ManualCustomBlckmdlng',
                           duration = 10, closeButton = T)
          
          #### Loads table from reactive
          df<-Tbl$Current
          
          #### 9.4.2(A) Finds out which is the first dimension of the array ####
          #### Prepare a shadow matrix
          num<-matrix(NA,ncol=ncol(df),nrow=nrow(df))
          ##### counts the length of the blocktypes in each cell. Block types are
          #### all three character long and always separated by a comma
          for(i in 1:nrow(df)){
            for(j in 1:ncol(df)){
              num[i,j]<-nchar(df[i,j])
            }
          }
          #### The position in the shadow matrix with the most character
          #### is the cell in the reactive table with the most block types
          WhereIsTheLongest<-which(num[,]==max(num[,]))[1]
          
          #### Finding out how many block types are in the fullest cell
          FirstDimension<-
            length(
              unlist(
                strsplit(split = ",",
                         unlist(df)[WhereIsTheLongest]
                )
              )
            )
          
          #### 9.4.2(B) Turning into an array ####
          # Creates an empty array of the right dimension
          BlockTypes<-array(NA,dim = c(FirstDimension,1,nrow=nrow(df),ncol=ncol(df)))
          
          # Fills it by layer, ...
          for(k in 1:FirstDimension){
            # ... then by column,...
            for(i in 1:nrow(df)){
              # ... and finally by row
              for(j in 1:ncol(df)){
                # if the specific cell contains less block types than the max
                # the cell, the extra layers are filled with NAs
                if(length(unlist(strsplit(df[i,j],split = ",")))<k){
                  BlockTypes[k,1,i,j]<-NA
                } else {
                  BlockTypes[k,1,i,j]<-unlist(strsplit(df[i,j],split = ","))[k]
                }
              }
            }
            
            if(dim(BlockTypes)[1]==1){
              BlockTypes<-BlockTypes[1,1,,]
            }
            
            
            
            removeNotification('ManualCustomBlckmdlng')
          }
        } else {
          if(input$ArrayInput==".RDS"){
            ### 9.4.3 Bloc-model from RDS ####
            
            ### Notification "Reading the custom block-model from RDS" 
            showNotification(ui = "Reading the custom block-model from file",
                             type = "message",id = 'FileCustomBlckmdlng',
                             duration = 10, closeButton = T)
            
            # Reading RDS file
            UploadedFile<-input$PrespecifiedArrayRDS
            BlockTypes<-readRDS(file = UploadedFile$datapath)
            
          } else if (input$ArrayInput==".RData"){
            ### 9.4.4 Bloc-model from RData ####
            
            ### Notification "Reading the custom block-model from RData" 
            showNotification(ui = "Reading the custom block-model from RData",
                             type = "message",
                             duration = 10, closeButton = T)
            
            # Reading RData file
            UploadedFile<-input$PrespecifiedArrayRData
            load(file = UploadedFile$datapath)
            ImportedArray<-load(file = UploadedFile$datapath)
            BlockTypes<-eval(parse(text = ImportedArray))
          } # else if RData
          
          removeNotification('FileCustomBlckmdlng')
        } # else if DT
        NumClusters<-dim(BlockTypes)[length(dim(BlockTypes))]
      } else {
        BlockTypes<-input$blckmdlngBlockTypes
        NumClusters<-input$blckmdlngNumClusters
      }
      
      
      
      
      ## Notification "Executing blockmodeling" 
      showNotification(ui = 'Blockmodeling started succesfully!',
                       type = "default",
                       duration = 10, closeButton = T)
      
      ## Modal spinner, show
      show_modal_spinner(spin = 'semipolar',
                         color = "#978E83",
                         text = 'Computing clusters, please wait...')
      
      Blck$Custom<<-BlockTypes
      
      ## 9.5 Executes blockmodeling ####
      blck<-
        optRandomParC(M = M,
                      k = NumClusters,
                      approaches = input$blckmdlngApproach,
                      blocks = BlockTypes,
                      rep = input$blckmdlngRepetitions,
                      save.initial.param.opt = input$blckmdlngInitialParams,
                      deleteMs = T,
                      max.iden = input$blckmdlngMaxSavedResults,
                      return.all = input$blckmdlngAll,
                      return.err = T,
                      RandomSeed = input$blckmdlngRandomSeed,
                      printRep = input$blckmdlngPrintRep,
                      usePreSpecM = usePreSpecM,
                      preSpecM = ParamM,
                      nCores = MultiCore
        )
      
      ## Modal spinner, remove
      remove_modal_spinner()
      
      ## Remember that the blockmodel was run
      Blck$RunAlready<<-TRUE
      
      # Notification "Blockmodeling completed"
      showNotification(ui = "Blockmodeling completed",
                       type = "message",
                       duration = 2, closeButton = T)
    }
    
    
    
    blck
    })
  
  # 10. Outputs blockmodeling ####
  
  ## 10.1 Blockmodeling output in a table ####
  TableBlockmdllng<-eventReactive(mdllng(),{
    ValueFromName<-
      function(Var.Name,collapse=F,sep=","){
        x<-eval(parse(text = Var.Name))
        if(collapse){
          paste(x,collapse = sep)
        }
      }
    
    blck<-mdllng()
    
    tbl<-matrix(data=NA,byrow = F,ncol=5,
                nrow = length(blck$best))
    
    colnames(tbl)<-c("Network size","Approaches",
                     "Blocks", "Clusters size",
                     # "Clusters columns",
                     "Error")
    
    tbl[1,1]<-nrow(blck$initial.param$M)
    tbl[1,2]<-paste(blck$initial.param$approaches,collapse = ",")
    tbl[1,3]<-paste(blck$initial.param$blocks,collapse = ",")
    if(nrow(tbl)!=1){
      tbl[2:nrow(tbl),1:3]<-""
    }
    
    for(i in 1:nrow(tbl)){
      Var.Name<-paste0("blck$best$best",i,"$resC$nUnitsRowClu")
      tbl[i,4]<-ValueFromName(Var.Name = Var.Name,collapse = T)
      # Var.Name<-paste0("blck$best$best",i,"$resC$nUnitsColClu")
      # tbl[i,5]<-ValueFromName(Var.Name = Var.Name,collapse = T)
      Var.Name<-paste0("blck$best$best",i,"$err")
      tbl[i,5]<-ValueFromName(Var.Name = Var.Name,collapse = T)
    }
    tbl
  })
  
  ## 10.2 Renders blockmodeling output in a table ####

  output$Tableblckmdlng<- renderTable({
    TableBlockmdllng()
  },colnames = T,rownames = F,striped = F,hover = T,bordered = T,
  spacing = "s",width = "auto",align = "c",digits = 0,quoted = F)
  
  ## 10.3 Renders blockmodeling output as summary ####
  output$Summaryblckmdlng <- renderPrint({
    blck<-mdllng()
    blck
  })
  
  ## 10.4 Image matrix (IM) ####
  ### 10.4.1 Disassembles image matrix as tables ####
  IM<-eventReactive(c(input$whichIM,mdllng()),{
    Disassemble.Array<-
      function(array){
          
          for(i in 1:dim(array)[3]){
            list[[i]]<-array[,,i]
          }

      }
    
    list<-list()
    
    for(i in 1:length(mdllng()$best)){
      list[[i]]<-
        blockmodeling::IM(res = mdllng(),
                          drop = input$dropIM,
                          which = i)
    }
    
    matrix<-list[[input$whichIM]]
    matrix<-as.data.frame(matrix)
    colnames(matrix)<-1:ncol(matrix)
    return(matrix)
  })
  
  ### 10.4.2 Renders image matrix as tables ####
  output$TableIM<- renderTable({
    IM()
  },colnames = T,rownames = T,striped = T,hover = T,bordered = T,
  spacing = "s",width = "auto",align = "c",digits = 0,quoted = F)
  
  ### 10.5 Renders error matrix as a table ####
  output$TableEM<- renderTable({
        EM_Table<-EM(res = mdllng(),
                     which = input$whichIM)
        colnames(EM_Table)<-1:ncol(EM_Table)
        EM_Table
  },colnames = T,rownames = T,striped = T,hover = T,bordered = T,
  spacing = "s",width = "auto",align = "c",digits = 0,quoted = F)
  
  ### 10.6 Rendex mean matrix as a table ####
  output$TableMean<- renderTable({
    Mean_Table<-blockmodeling::funByBlocks(x = mdllng(),
                                           which=input$whichIM,
                                           FUN='mean',na.rm=T)
    colnames(Mean_Table)<-1:ncol(Mean_Table)
    Mean_Table
  },colnames = T,rownames = T,striped = T,hover = T,bordered = T,
  spacing = "s",width = "auto",align = "c",digits = 0,quoted = F)
  
  # 11. Download blockmodeling results to file ####
  output$DownloadBlckRDS <- downloadHandler(
    filename = "Blockmodeling results.RDS",
    content = function(file) {
      saveRDS(object = mdllng(),file = file)
    }
  )
  
  # 11. Download clusters to file ####
  output$DownloadClu <- downloadHandler(
    filename = "partitions.clu",
    content = function(file) {
      blockmodeling::savevector(v = clu(res = mdllng(),which = input$whichIM),
                                filename = file)
      }
    )
  
  # 12. Download image matrix ####
  
  ## 12.1 As plain text ####
  ## dropIM, whichIM
  output$DownloadIMtext <- downloadHandler(
    filename = "image.txt",
    content = function(file) {
      IM<-blockmodeling::IM(res = mdllng(),
                        drop = input$dropIM,
                        which = input$whichIM
                        )
      write.table(x = IM,file = file,append = F,quote = F)
      }
    )
  
  ## 12.2 As RDS ####
  ## dropIM, whichIM
  output$DownloadIMrds <- downloadHandler(
    filename = "image.RDS",
    content = function(file) {
      IM<-blockmodeling::IM(res = mdllng(),
                            drop = input$dropIM,
                            which = input$whichIM
                            )
      saveRDS(object = IM,file = file,compress = F)
      }
    )
  
  # 13. Block-model from file/sample ####
    observeEvent(input$UploadArray,{
      ## Prepares condition
      conditionArray<-
        magrittr::or(!is.null(input$PrespecifiedArrayRDS),
                     !is.null(input$PrespecifiedArrayRData))
      ## Checks condition
      if(conditionArray&!input$EditUploadedArray){
        if(input$ArrayInput==".RDS"){
          ## 13.1 Reading RDS file ####
          UploadedFile<-input$PrespecifiedArrayRDS
          Layers<-readRDS(file = UploadedFile$datapath)
        } else if(input$ArrayInput==".RData"){
          ## 13.2 Reading RData file ####
          UploadedFile<-input$PrespecifiedArrayRData
          load(file = UploadedFile$datapath)
          ImportedArray<-load(file = UploadedFile$datapath)
          Layers<-eval(parse(text = ImportedArray))
        }
        
        ## 13.3 Unmaking array ####
        
        ## Preparing data frame
        UnmakingArray<-matrix(NA,nrow = dim(Layers)[4],ncol = dim(Layers)[3])
        colnames(UnmakingArray)<-1:ncol(UnmakingArray)
        
        # Filling  by column ...
        for(i in 1:nrow(UnmakingArray)){
          # ... then by row
          for(j in 1:ncol(UnmakingArray)){
            # if the specific cell contains less block types than the max
            # the cell, the extra layers are filled with NAs
            UnmakingArray[i,j]<-paste(unlist(strsplit(Layers[,1,i,j],split = ",")),collapse = ",")
          }
        }
        Tbl$Current<<-UnmakingArray
      }
    })
  
  # 14. Cells' selection ####
  observeEvent(input$CustomBlockModel_cell_clicked,{
    Tbl$Rows<<-c(Tbl$Rows,input$CustomBlockModel_cell_clicked$row)
    Tbl$Cols<<-c(Tbl$Cols,input$CustomBlockModel_cell_clicked$col)
    
    ## 14.1 Checks for de-selection ####
    if(length(Tbl$Rows!=1)){
      pairs<-rep(0,length(Tbl$Rows))
      for(i in 1:length(pairs)){
        pairs[i]<-paste(Tbl$Rows[i],Tbl$Cols[i],sep = '_')
        ### Comment
          # Uses text with separator to avoid confusing combinations like:
          # Row 1, cell 22 and Row 12, cell 2
          # as a single number both are '122' and could be deleted
          # as text they are different: '1_22' and and '12_2'
      }
      hit<-which(
        grepl(pattern = pairs[length(pairs)],
              x = pairs[-length(pairs)])
        )
      if(length(hit)>0){
        hit<-c(hit,length(pairs))
        Tbl$Rows<-Tbl$Rows[-hit]
        Tbl$Cols<-Tbl$Cols[-hit]
      }
    }
  })
  
  # 15. Reset and select All ####
  proxy=dataTableProxy(outputId = 'CustomBlockModel')
  
  ## 15.1 Reset ####
  observeEvent(input$ResetSelectionDT,{
    reloadData(proxy = proxy,Tbl$Current,clearSelection = 'all')
    Tbl$Cols<<-Tbl$Rows<<-NULL
  })
  
  # 15.2 Select all ####
  observeEvent(input$SelectAllDT,{
    selectAll<-matrix(NA,ncol = 2,nrow = nrow(Tbl$Current)*ncol(Tbl$Current))
    Tbl$Rows<<-selectAll[,1]<-rep(1:nrow(Tbl$Current),each=ncol(Tbl$Current))
    Tbl$Cols<<-selectAll[,2]<-rep(1:ncol(Tbl$Current),nrow(Tbl$Current))
    
    selectCells(proxy = proxy,selected = selectAll)
  })
  
  # 16.  (Re)Initialise TblCurrent if empty ####
    observeEvent(c(input$LoadBlocksIntoDT,input$UploadArray,input$SetSizeDT),{
      TblCurrent<<-Tbl$Current
      if(is.null(TblCurrent)){
        TblCurrent<-matrix(NA,nrow = input$CustoomBlockModel_NumberCluster,
                           ncol = input$CustoomBlockModel_NumberCluster)
        colnames(TblCurrent)<-1:ncol(TblCurrent)
        rownames(TblCurrent)<-NULL
        Tbl$Current<<-TblCurrent
      } else {
        Tbl$Current<<-TblCurrent
      }
    })
  
  # 17. Change block-model size ####
  observeEvent(input$SetSizeDT,{
      if(ncol(TblCurrent)!=input$CustoomBlockModel_NumberCluster){
        if(ncol(TblCurrent)<input$CustoomBlockModel_NumberCluster){
          ### Add columns and rows
          AddCols<-input$CustoomBlockModel_NumberCluster-ncol(TblCurrent)
          
          EmptyData<-rep(NA,nrow(TblCurrent))
          for(i in 1:AddCols){  
            TblCurrent<-cbind(TblCurrent,EmptyData)
          }
          
          EmptyData<-rep(NA,ncol(TblCurrent))
          for(i in 1:AddCols){  
            TblCurrent<-rbind(TblCurrent,EmptyData)
          }
        } else if(ncol(TblCurrent)>input$CustoomBlockModel_NumberCluster){
          ### Remove columns and rows
          DelCols<-(input$CustoomBlockModel_NumberCluster+1):ncol(TblCurrent)
          TblCurrent<-TblCurrent[-DelCols,-DelCols]
        }
        colnames(TblCurrent)<-1:ncol(TblCurrent)
        rownames(TblCurrent)<-NULL
        Tbl$Current<<-TblCurrent
      }
    })
  
  # 18. Loading imputed data into table ####
  observeEvent(input$LoadBlocksIntoDT,{
      HitRows<<-Tbl$Rows; HitCols<<-Tbl$Cols
      if(!is.null(Tbl$Rows)){
        for(i in 1:length(HitRows)){
          TblCurrent[HitRows[i],HitCols[i]]<-paste(input$TowardsDT,collapse = ",")
          Tbl$Rows<<-NULL
          Tbl$Cols<<-NULL
        }
        colnames(TblCurrent)<-1:ncol(TblCurrent)
        rownames(TblCurrent)<-NULL
        Tbl$Current<<-TblCurrent
      }
    })

  
  # 19. Visualise table ####
  output$CustomBlockModel<-
    DT::renderDataTable({
      ## Read from reactive
      TblCurrent<<-Tbl$Current
      ## Converts to data frame in order to show row numbers
      TblCurrent<-as.data.frame(TblCurrent)
      ## Outputs
      TblCurrent
    },selection = list(mode="multiple",target='cell',selectable=matrix(c(-1:-nrow(Tbl$Current),rep(0,nrow(Tbl$Current))),ncol = 2)),
    options = list(paging =FALSE, searching=FALSE,ordering=FALSE),style='bootstrap4')
  
  # 20. Download adjacency matrix ####
  output$downloadAdj<-
    downloadHandler(
      filename = "Adjacency Matrix.txt",
      content = function(file) {
        write.table(x = GetAdjacencyMatrix(),
                    file = file)
      },
      contentType = 'text/csv'
    )
  
  # 21. Download custom blockmodel ####
  output$downloadCustomBlck<-
    downloadHandler(
      filename = "Pre-specified Blockmodel.RDS",
      content = function(file) {
        saveRDS(object = Blck$Custom,file = file)
      }
    )
  
}

# Run the application 
shinyApp(ui = ui, server = server)