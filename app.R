
library(DT)
library(tools)
library(shiny)
library(shinyjs)
library(data.table)
library(shinyWidgets)
library(shinydashboard)
library(dplyr)
library("readxl")


useShinyjs(rmd = TRUE)




ui <- dashboardPage(
        skin="black",
        title="SiDCo",
        dashboardHeader(
          title = tags$a(id="logomainlink",tags$img(src = "homeimage.png",style="width:20px"),href='https://complimet.ca/',"CompLiMet"),
          titleWidth = 250,
          tags$li(
            class = "dropdown",
            tags$style(".main-header {font-family: Montserrat;text-align: center;}"),
            tags$style(".main-header .logo {font-family: Montserrat;text-align: center;}"),
			      tags$style(".logomainlink {font-family: Montserrat;text-align: center;}"),
			      tags$head(
			        tags$style(HTML("#tbs li a[data-value = 'tab_4'], #tbs li a[data-value = 'tab_5'] {disabled;}"))
			      )
          )
        ), 
        dashboardSidebar(
          width=250,
          tags$style(".left-side, .main-sidebar {padding-top: 180px;font-family: Montserrat;}"),
          div(
            actionLink("link_to_ins",h4("1. Instructions"),class="smenu"),
            actionLink("link_to_ana",h4("2. Analysis"),class="smenu")
          )
        ),
        dashboardBody(
          shinyjs::useShinyjs(),
          extendShinyjs(
            script = "https://complimet.ca/shiny/sidco/anujava.js",
            functions = c("enableTab","disableTab","updateVis")
          ),
          tags$link(rel = "stylesheet", type = "text/css", href = "metaboa.css"),# Note: 'sidebar-toggle' is a class name (i.e. HTML class attribute) of Shiny icon 
          tags$script("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'hidden';"),
          fluidRow(
            box(
              width = 12,
              tabsetPanel(
                type = "tabs",
                id = "tbs",       
		            tabPanel(
		              "Quick Start",
		              value="tab_0",
		              column(
		                width = 12,
                    box(
                      id="omb",
                      width = 12,
	                    tags$table(id = "qstable",
					              tags$tr(
					                tags$td(
					                  align="left",
					                  valign="top",
					                  width="50%",
		                        tags$div(
		                          class = "verticaltop",
	                            h4(tags$b("Overview of SiDCo")),br(),
		                          p(strong("SiDCo "),em("(SIgned Distance COrrelation)")," calculates pairwise  ",
		                            tags$a(href='https://en.wikipedia.org/wiki/Distance_correlation',"distance correlation",target="_blank"),
		                            " coefficients between all columns of a .xlsx datasheet. The primary use of SiDCo is in metabolomics and lipidomics although this site provides seamless application of signed distance correlation and partial distance correlation for any dataset."),
		
		p(
		  "The main advantage of distance correlation is the ability to quantify linear and non-linear correlations simultaneously, while allowing for comparisons of matrices of different dimensions through the calculation of distance covariances. Due to this unique ability, distance correlation can be used to calculate one-to-all (linear and non-linear correlations between each feature and all the other features) or one-to-one correlations (pairwise correlations between individual features). Both options are available in SiDCo."
		
		),br(),
		
		
		
		p("SiDCo capitalizes on the Gaussian Graphical Model (GGM) method to determine pair-wise associations while removing the confounding effects of other variables. The GGM method calculates the inverse of the distance covariance to remove orthogonal contributions without any matrix shrinkage. If distance covariance matrix is singular, this inverse is calculated using the ",tags$a(href='https://en.wikipedia.org/wiki/Moore%E2%80%93Penrose_inverse',"(Moore–Penrose inverse - Wikipedia)",target="_blank"),"."),br(),
		
		p("SiDCo is implemented in Python with a RShiny front-end. Two analytical tabs allow users to choose between signed distance correlation or partial distance correlation: "),br(),
		
		p(strong("Tab dCor:"),tags$ul(
            tags$li(tags$span(
             "Calculates either the distance correlation and p-value between each feature and all the other features combined (n",tags$sub("i"),"  to ",tags$sub("(∀j≠i)"),"n",tags$sub("j")," ) in a sense of one-to-all (correlation with the network) or pairwise distance correlations between individual features (n",tags$sub("i"),"  to n",tags$sub("(∀j≠i)"),") in a sense of one-to-one comparisons."
            )),
            tags$li(tags$span(
              "For one to one pairwise comparisons, SidCo additionally inputs to the distance correlation the sign of the Pearson correlation. The sign (positive or negative) only indicates the overall linear trend. The sign does not suggest significant linear correlation."),
              br(),br(),tags$span("Outputs are:",br(),"- For the one-to-all comparison, the output is an excel file that includes the distance correlation value for each feature to all the other features and the corresponding p-value for this calculation."),br(),tags$span("- For the one-to-one calculation, the output is an excel file that includes the signed distance correlation values and corresponding p-values as well as the Pearson and Spearman correlation values with their corresponding p-values. Distance correlation values are set to zero if their absolute value is below the user-defined threshold value or their corresponding p-value is above the user-defined p-value.")),
			
          )),br(),
		
		p(strong("Tab pdCor:"),tags$ul(

            tags$li(tags$span(
              "Partial distance correlation is calculated using the Gaussian Graphical Model (GGM) with p-value determined from the cumulative normal distribution function of Fisher z-transformed correlations."),br(),br(),tags$span("Outputs are:"),br(),tags$span("- An Excel spreadsheet with partial distance correlation values and their corresponding p-values."),br()),
			
          )),br(),
		p(span("In both cases data is preprocessed by z-score normalization of features across all samples. Any missing values must be imputed by the user prior to analysis or SiDCo will not function.")),br(),span("Distances calculation running time is typically a function of N², where N is the sample size. For typical datasets in metabolomics and lipidomics (~500 x 500) both dCor and pdCor calculations take less than 2 minutes. For extremely large datasets of more then 1M elements, calculations can be time-consuming."),br(),br()
		
		
	   )),
	   tags$td(align = "center", valign="top",width="50%",
	   tags$div(class="verticalcenter",tags$img(src = "Figure1_outline_v3.svg",style="width:550px"),br(),br(),"SiDCo workflow.")
	   
	   
	   )
	   
	   ))
	   ))),#tab1
	   tabPanel("Data Format",value="tab_1",column(width = 12,
       box(id="omb", width = 12,
	   tags$table(id = "dftable",
					   tags$tr(
					   tags$td(colspan=2,align="left",valign="top",width="100%",	
		h4(tags$b("Preparing your data for SiDCo")),br(),
		
		p("SidCo calculates distance correlation between features listed in columns using data across rows. The SidCo input must be a single .xlsx file with features (for example metabolites or lipids) in columns and samples in rows. The file should contain column names in the top row and row names in the first column (column A). Additional information can be included and user can specify start row and column as well as stop row. All numeric data should be below and to the right of the specified start column and start row. If there is any non-numeric data to the right of the specified start column, the analysis will abort. Because distance correlation calculations cannot work with data that have missing values, users should impute missing data with a method that is the most appropriate for their dataset prior to using SiDCo."),br(),
		
		h4(tags$b("Sample Data")),br(),
		p("The sample datasets are provided in both allowed input formats (.csv and .xlsx) with features (metabolites or lipids) in columns and samples in rows. Note, column A includes group names. Row 1 includes feature names. To calculate distance correlations in separate groups set, the user should input for Group 1: Start Column: B; First Row: 2 (or -1 indicating first data row); Last Row: 31. For analysis of Group 2, the user should input: Start Column: B; First Row: 32; Last Row: 46 or -1 (stating last row)."),br(),
		p(
		   h4(tags$b("Sample Data")),br(),
		    tags$ol(
            tags$li(tags$span(
              tags$a(href='https://complimet.ca/shiny/sidco/exampleinput.xlsx',"exampleinput.xlsx")
            ))
          )
		
		
		))
	   )
	   )))),#tab2
	   
	   
	   		tabPanel("Troubleshooting",value="tab_6",column(width = 12,
       box(id="omt", width = 12,
            p(
			"When troubleshooting, please review this list of common reasons for SiDCo failing to run. If you are still experiencing difficulties, please contact ",tags$a(href="mailto: ldomic@uottawa.ca", "ldomic@uottawa.ca")," for further assistance. Please include your input dataset and a description of the problem that you experienced. We will reproduce the problem and provide you with a solution."
			
			),
			p(
			tags$ol(
            tags$li(tags$span(
              h5(tags$b("My file loads but does not output any analysis ")),
			  p("SiDCo only accepts comma-delimited .xlsx files as input. Tab-delimited files (.txt) can be read but not analyzed and will not produce any results. Please convert your input data into .csv format before running SiDCo. Additionally, ensure that your column and row information to the right and below your start cell are numeric values. Data can start from any row and column; however, all data must be numeric to the right and down from the user-defined start point. Make sure that there are no missing data in your input as they will prevent SiDCo calculations.")
            )),
			tags$li(tags$span(
              h5(tags$b("All obtained values are zero")),
			  p("Revise your tolerance, i.e., threshold information and p-value. SiDCo sets to zero values that are below the correlation value threshold or above the specified p value. If you prefer to see all values please enter 0 for distance correlation threshold and 1 for p-value threshold.")
            )),
			tags$li(tags$span(
              h5(tags$b("Why are there are no negative values for one-to-all distance correlations")),
			  p("As Pearson correlation cannot be calculated for vectors of different lengths, it is not possible to determine a linear sign in the one-to -all distance calculation.")
            )),
			tags$li(tags$span(
              h5(tags$b("Why are there no Person and Spearman values for one-to-all correlation")),
			  p("Pearson and Spearman correlations can not be calculated for the one-to-all set and thus can not be included in this output.")
            )),
            
         	tags$li(tags$span(
              h5(tags$b("I get result from dCor but not from pdCor tab")),
			  p("pdCor analysis is based on the inversion of the distance covariance matrix. If this matrix is singular, inversion is not possible. This occurs when the input has fewer samples than features or if there are some features that can only be obtained as a linear combination of other features in the dataset. To address these issues, add more sample measurements or reduce the number of features in your pdCor analysis.")
            ))
			
          )
			
			
			)
			
	   ))),
	   #tab3
	   tabPanel("Cite SiDCo",value="tab_3",column(width = 12,
       box(id="cmb", width = 12,
	   
	   strong("Contact Us"),
        p(tags$a(href="mailto: ldomic@uottawa.ca", "ldomic@uottawa.ca")
        ),
        br(),
        strong("Cite your use of SiDCo in a publication"),
        p(
          "F. Monti,   D. Stewart,   A. Surendra,   I. Alecu,   T. Nguyen-Tran,   S. A. L Bennett,   M. Čuperlović-Culf, Signed Distance Correlation (SiDCo): an online implementation of distance correlation and partial distance correlation for data-driven network analysis, Bioinformatics, Volume 39, Issue 5, May 2023, btad210,",tags$a(href="https://doi.org/10.1093/bioinformatics/btad210",target="_blank", "https://doi.org/10.1093/bioinformatics/btad210 "),"(",tags$a(href="btad210.pdf",target="_blank", "Download"),")",br()
        ),
        br(),
        strong("Public Server"),
        p(
          "SiDCo:",tags$a(href="https://complimet.ca/SiDCo/",target="_blank", "https://complimet.ca/SiDCo/")
        ),
        br(),
		strong("Software License"),
        p(
          "SiDCo is free software. You can redistribute it and/or modify it under the terms of the ",tags$a(href="https://www.gnu.org/licenses/",target="_blank", "GNU General Public License")," v3 (or later versions) as published by the Free Software Foundation. As per the GNU General Public License, SiDCo is distributed as a bioinformatic tool to assist users WITHOUT ANY WARRANTY and without any implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. All limitations of warranty are indicated in the GNU General Public License.
"
        )
	   
	   
	   ))),
		    tabPanel("dCor",value="tab_4",width = 12,
		      box(id="cqf2", width = 12,
		        div(id="fileInput",
              fileInput("file1", "Choose XLSX file ",multiple = FALSE,accept = c(".xlsx",".txt")),
              textInput("startCol","Start Column","B"),
              numericInput("firstRow","First Row",-1),
              numericInput("lastRow","Last Row",-1),
              numericInput("pValTolerance","P-Value Tolerance",0.01),
              numericInput("dCorTolerance","Distance Correlation Tolerance",0.6),
              checkboxInput("oneToAll","One-to-all comparison")
            ),
		        textOutput("text"),
            div(id = 'calculate', actionButton("calculate", "Calculate")),
            hidden(
              div(id = 'calculating',
                p("Calculating. This might take a minute..."),
                p("Remember that the sign of the coefficient is coming from Pearson's correlation."),
                p("eg. a high coefficient with a negative sign does NOT mean a significant negative trend."),
                p("It only indicates a strong correlation, with some negative overall, linear trend also detected")
              )
            ),
            div(id = 'download'),
            hidden(div(id = 'back', actionButton("newCalculation", "Make another calculation")))
          )
		    ),#tab2
		    tabPanel("pdCor",value="tab_5",width=12,
		      box(id="cqf2",width=12,
		        div(id="inputs",
		          fileInput("file","Choose XLSX file",multiple=FALSE,accept=c(".xlsx")),
		          textInput("startColumn","Start Column","B"),
		          numericInput("startRow","First Row",-1),
		          numericInput("endRow","Last Row",-1),
		          numericInput("pValueTolerance","P-Value Tolerance",0.01,step=0.01)
		        ),
		        textOutput("errorText"),
		        div(id='calculateBtn', actionButton("calculateBtn", "Calculate")),
		        hidden(
		          div(id = 'calculating2',
		              p("Calculating. This might take a minute..."),
		          )
		        ),
		        div(id = 'download2'),
		        hidden(div(id = 'back2', actionButton("newCalculation2", "Make another calculation")))
		      )
		    ),
																							
		span(style="visibility:hidden",textInput( "condition", "", "0")),
		span(style="visibility:hidden",textInput( "viscondition", "", "10")),
		span(style="visibility:hidden",textInput( "ts", "", "NULL")),
		span(style="visibility:hidden",textInput( "outdir", "", "NULL"))
																						   )
                                           
                                  )),
                                  
                                  tags$head(tags$style(HTML(
                                    '.main-header .logo,.myClass { 
        font-size: 20px;
		font-family: Montserrat;
        font-weight: 900;
        line-height: 50px;
        text-align: left;
        overflow: hidden;
        color: black;
      }
	  
       .sidebar-toggle .myClass { 
        background-color: white !important;
      }
	  
	  
	  #plottab,#plottable2,td, th, tr, table {
    border: 0 !important;
    border-spacing:0 !important;
	padding: 30px;
  }
    '))),
                                  tags$script(HTML('
      $(document).ready(function() {
        $("header").find("nav").append(\'<img src="SiDCo_image_v2.png" alt="" height="50px"><span class="myClass"> SiDCo: Signed Distance Correlation </span>\');
      })
     ')),
                                                          tags$head(
                                    tags$style(HTML(".main-sidebar .logo {background-color: #222d32 !important;}"))
									
                                  ),
								   tags$head(
                                    tags$style(HTML(".tabPanel .box {padding-left:5%;padding-right:15%;}"))
									
                                  ),
                                  
                    )
                    
)



## Server logic
server <- function(input, output, session) {
  
  options(shiny.maxRequestSize=20000*1024^2)
  rv <- reactiveValues()
  
  observeEvent(input$link_to_ins, {
     shinyjs::js$disableTab("tab_4")
     shinyjs::js$disableTab("tab_5")
     shinyjs::js$enableTab("tab_0")
     shinyjs::js$enableTab("tab_1")
     shinyjs::js$enableTab("tab_2")
     shinyjs::js$enableTab("tab_3")
     shinyjs::js$enableTab("tab_6")
     updateTabsetPanel(session, "tbs",selected = "tab_0")
  })
  
  observeEvent(input$link_to_ana, {
    shinyjs::js$enableTab("tab_4")
    shinyjs::js$enableTab("tab_5")
    shinyjs::js$disableTab("tab_0")
    shinyjs::js$disableTab("tab_1")
    shinyjs::js$disableTab("tab_2")
    shinyjs::js$disableTab("tab_3")
    shinyjs::js$disableTab("tab_6")
    updateTabsetPanel(session, "tbs",selected = "tab_4")
  })
  
  #observeEvent when the actionbutton is pushed
  observer1 <- observeEvent(input$calculate, {
    
    inputPresent <- !is.null(input$file1$datapath)
    print("HELLO1")
	
    #create date instance
    date <- as.POSIXct(Sys.time(), format = "%m/%d/%Y %H:%M:%S")
    #create timestamp
    ts <- format(date, format = "%m%d%Y%H%M%S")
    
    #remove path from the filepath
    nf <- gsub(".*/","",input$file1$datapath)
    #remove file extension
    nf <- gsub(".*\\.",paste(ts,".",sep=""),nf)
    #add temporary filepath to filename
    nffn <- paste(getwd(),"tmp",nf,sep="/")
    
    #file copy to the temporary directory
    file.copy(input$file1$datapath,nffn, overwrite = TRUE)
    print("HELLO2")
    
    outputPath <- nffn
    print(outputPath)
    
    nfext <- gsub(".*/","",input$file1$datapath)
	  print("HELLO3")
    #remove file extension
    nfext <- gsub(".*\\.","",nfext)
    print(identical(tolower(nfext), tolower("xlsx")))
	

      

	  #input$startCol,input$firstRow,input$lastRow
	  #nums <- unlist(lapply(x, is.numeric), use.names = FALSE)
	  #length(which(nums==FALSE))
    
    
	  colstart <- match(tolower(input$startCol), letters[1:26])
	  nums <- NULL
	  nas <- NULL
	 
    
	  if(identical(tolower(nfext), tolower("xlsx"))){
	    tmp.dm <- as.data.frame(read_excel(outputPath,col_names=T))
	    firstrow <- ifelse(input$firstRow<0,1,input$firstRow)
	    lastrow <- ifelse(input$lastRow<0,dim(tmp.dm)[1],input$lastRow)
	    
	    tmp.dm <- tmp.dm[firstrow:lastrow,colstart:dim(tmp.dm)[2]]
	    print("HELLO")
	    print(tmp.dm)
	    
	    nums <- unlist(lapply(tmp.dm, is.numeric), use.names = FALSE)
	    #nas <- unlist(sapply(tmp.dm, function(x) all(is.na(x) | x == '' )), use.names = FALSE)
	    #sapply(tmp.dm, function(x) all(is.na(x) | x == '' ))
	    g1<-which(is.na(tmp.dm), arr.ind=TRUE)
	    g2<-which(tmp.dm=="", arr.ind=TRUE)
	    nas <- ifelse(dim(rbind(g1,g2))[1]>0,TRUE,FALSE)
	
	    
	  }else if(identical(tolower(nfext), tolower("csv"))){
	    tmp.dm <- as.data.frame(read.delim(outputPath,header=T,as.is=T, sep=","),as.is=T)
	    firstrow <- ifelse(input$firstRow<0,1,input$firstRow)
	    lastrow <- ifelse(input$lastRow<0,dim(tmp.dm)[1],input$lastRow)
	    
	    tmp.dm <- tmp.dm[firstrow:lastrow,colstart:dim(tmp.dm)[2]]
	    print("HELLO")
	    print(tmp.dm)
	    
	    nums <- unlist(lapply(tmp.dm, is.numeric), use.names = FALSE)
	    g1<-which(is.na(tmp.dm), arr.ind=TRUE)
	    g2<-which(tmp.dm=="", arr.ind=TRUE)
		g3<-which(tmp.dm==" ", arr.ind=TRUE)
		
	    nas <- ifelse(dim(rbind(g1,g2,g3))[1]>0,TRUE,FALSE)
	  }
	  



	if(!identical(tolower(nfext), tolower("xlsx")) && !identical(tolower(nfext), tolower("csv"))){
	  inputPresent <- FALSE
	   showModal(modalDialog("Your file is not in csv or xlsx format.",easyClose = TRUE,
        footer = modalButton("Dismiss")
      ))
	}else if(!is.null(nums) && length(which(nums==FALSE))>0){
	  inputPresent <- FALSE
	  showModal(modalDialog(paste("There are non-numeric characters in your dataset.",sep=""),easyClose = TRUE,
	                        footer = modalButton("Dismiss")
	  ))
	  
	}else if(nas){
	  inputPresent <- FALSE
	  showModal(modalDialog(paste("Please impute the missing values in your dataset.",sep=""),easyClose = TRUE,
	                        footer = modalButton("Dismiss")
	  ))
	  
	}
	  
	
	
    
    if(inputPresent){
      
      toggle(
        selector = '#calculate'
      )
      
      toggle(
        selector = '#fileInput'
      )
      
      toggle(
        selector='#calculating'
      )
      
      system(paste("cd ",getwd()))
      
      #using the system command to call the python script    
      pyout <- system(paste(paste(getwd() ,"/sidcobash.sh ", sep=""), nffn, outputPath, input$startCol,input$firstRow,input$lastRow,input$pValTolerance,input$dCorTolerance,input$oneToAll),intern=T,show.output.on.console = T)
      
      toggle(
        selector='#back'
      )
      
      toggle(
        selector='#calculating'
      )
      
      insertUI(  
        selector = '#download',
        ui=downloadButton("DLButton","Download DistanceCorrelation.xlsx")
      )
      
      output$DLButton <- downloadHandler(
        filename = function() {
          "DistanceCorrelation.xlsx"
        },
        content = function(file) {
          if(file.exists(outputPath)){
            file.copy(outputPath,file)
          }else{
            output$text<-renderText("There was a problem creating the output file. Please ensure all parameters are correct.")
            removeUI(
              selector = "#DLButton"
            )
          }
          
        }
      )
    }else{
      output$text <- renderText("Please select an excel file")
    }
	
	#observer1$destroy()
    
  })
  
  
    observer2 <- observeEvent(input$newCalculation, {
    
    removeUI(
      selector = "#DLButton"
    )
    
    toggle(
      selector='#back'
    )
    
    toggle(
      selector = "#fileInput"
    )
    
    toggle(
      selector = '#calculate'
    )
    output$text <- renderText("")
	
	#observer2$destroy()
  })
  
  output$exampleXLSX <- downloadHandler(
    filename = function() {
      "exampleinput.xlsx"
    },
    content = function(file) {
      file.copy(paste(getwd(),"/exampleinput.xlsx", sep=""),file)
    }
  )
  
  output$exampleCSV <- downloadHandler(
    filename = function() {
      "exampleinput.csv"
    },
    content = function(file) {
      file.copy(paste(getwd(),"/exampleinput.csv", sep=""),file)
    }
  )
  
  
  # Partial Distance Correlation Logic
  observer3 <- observeEvent(input$calculateBtn, {
    
    inputPresent <- !is.null(input$file$datapath)
	
	    print("HELLO1")
	
    #create date instance
    date <- as.POSIXct(Sys.time(), format = "%m/%d/%Y %H:%M:%S")
    #create timestamp
    ts <- format(date, format = "%m%d%Y%H%M%S")
    
    #remove path from the filepath
    nf <- gsub(".*/","",input$file$datapath)
    #remove file extension
    nf <- gsub(".*\\.",paste(ts,".",sep=""),nf)
    #add temporary filepath to filename
    nffn <- paste(getwd(),"tmp",nf,sep="/")
    
    #file copy to the temporary directory
    file.copy(input$file$datapath,nffn, overwrite = TRUE)
    print("HELLO2")
    
    outputPath <- nffn
    print(outputPath)
    
    nfext <- gsub(".*/","",input$file$datapath)
	  print("HELLO3")
    #remove file extension
    nfext <- gsub(".*\\.","",nfext)
    print(identical(tolower(nfext), tolower("xlsx")))
	

      

	  #input$startCol,input$firstRow,input$lastRow
	  #nums <- unlist(lapply(x, is.numeric), use.names = FALSE)
	  #length(which(nums==FALSE))
      #textInput("startColumn","Start Column","B"),
      #numericInput("startRow","First Row",-1),
	  #numericInput("endRow","Last Row",-1),
    
	  colstart <- match(tolower(input$startColumn), letters[1:26])
	  nums <- NULL
	  nas <- NULL
	 
    
	  if(identical(tolower(nfext), tolower("xlsx"))){
	    tmp.dm <- as.data.frame(read_excel(outputPath,col_names=T))
	    firstrow <- ifelse(input$startRow<0,1,input$startRow)
	    lastrow <- ifelse(input$endRow<0,dim(tmp.dm)[1],input$endRow)
	    
	    tmp.dm <- tmp.dm[firstrow:lastrow,colstart:dim(tmp.dm)[2]]
	    print("HELLO")
	    print(tmp.dm)
	    
	    nums <- unlist(lapply(tmp.dm, is.numeric), use.names = FALSE)
	    #nas <- unlist(sapply(tmp.dm, function(x) all(is.na(x) | x == '' )), use.names = FALSE)
	    #sapply(tmp.dm, function(x) all(is.na(x) | x == '' ))
	    g1<-which(is.na(tmp.dm), arr.ind=TRUE)
	    g2<-which(tmp.dm=="", arr.ind=TRUE)
	    g3<-which(tmp.dm==" ", arr.ind=TRUE)
		
	    nas <- ifelse(dim(rbind(g1,g2,g3))[1]>0,TRUE,FALSE)
	
	    
	  }else if(identical(tolower(nfext), tolower("csv"))){
	    tmp.dm <- as.data.frame(read.delim(outputPath,header=T,as.is=T, sep=","),as.is=T)
	    firstrow <- ifelse(input$startRow<0,1,input$startRow)
	    lastrow <- ifelse(input$endRow<0,dim(tmp.dm)[1],input$endRow)
	    
	    tmp.dm <- tmp.dm[firstrow:lastrow,colstart:dim(tmp.dm)[2]]
	    print("HELLO")
	    print(tmp.dm)
	    
	    nums <- unlist(lapply(tmp.dm, is.numeric), use.names = FALSE)
	    g1<-which(is.na(tmp.dm), arr.ind=TRUE)
	    g2<-which(tmp.dm=="", arr.ind=TRUE)
	    nas <- ifelse(dim(rbind(g1,g2))[1]>0,TRUE,FALSE)
	  }
	  
	 if(!identical(tolower(nfext), tolower("xlsx")) && !identical(tolower(nfext), tolower("csv"))){
	  inputPresent <- FALSE
	   showModal(modalDialog("Your file is not in csv or xlsx format.",easyClose = TRUE,
        footer = modalButton("Dismiss")
      ))
	}else if(!is.null(nums) && length(which(nums==FALSE))>0){
	  inputPresent <- FALSE
	  showModal(modalDialog(paste("There are non-numeric characters in your dataset.",sep=""),easyClose = TRUE,
	                        footer = modalButton("Dismiss")
	  ))
	  
	}else if(nas){
	  inputPresent <- FALSE
	  showModal(modalDialog(paste("Please impute the missing values in your dataset.",sep=""),easyClose = TRUE,
	                        footer = modalButton("Dismiss")
	  ))
	  
	}

    
    if (inputPresent) {
      
      toggle(
        selector = '#inputs'
      )
      
      toggle(
        selector = '#calculateBtn'
      )
      
      toggle(
        selector='#calculating2'
      )
      

      
      system(paste("cd ",getwd()))
	  print(paste(paste(getwd() ,"/sidcobash2.sh ", sep=""), nffn, outputPath, input$startColumn,input$startRow,input$endRow,input$pValueTolerance))
      pyout <- system(paste(paste(getwd() ,"/sidcobash2.sh ", sep=""), nffn, outputPath, input$startColumn,input$startRow,input$endRow,input$pValueTolerance),intern=T,show.output.on.console = T)
      
      toggle(
        selector='#calculating2'
      )
      
      toggle(
        selector='#back2'
      )
      
      insertUI(  
        selector = '#download2',
        ui=downloadButton("DLButton2","Download PartialDistanceCorrelation.xlsx")
      )
      
      output$DLButton2 <- downloadHandler(
        
        filename = function() {
          "PartialDistanceCorrelation.xlsx"
        },
        content = function(file) {
          if (file.exists(outputPath)) {
            file.copy(outputPath,file)
          } 
          else {
            output$text<-renderText("There was a problem creating the output file. Please ensure all parameters are correct.")
            removeUI(
              selector = "#DLButton2"
            )
          }
        }
      )
      
    }
    else {
      output$errorText <- renderText("Please select an excel file")
    }
    #observer3$destroy()
    
  })
  
  
  observer4<- observeEvent(input$newCalculation2, {
    
    output$errorText <- renderText("")
    
    removeUI(
      selector = "#DLButton2"
    )
    
    toggle(
      selector='#back2'
    )
    
    toggle(
      selector = "#inputs"
    )
    
    toggle(
      selector = '#calculateBtn'
    )
	#observer4$destroy()
    
  })

}

shinyApp(ui, server)
