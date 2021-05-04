#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
#library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting



############################################## End of package requirements



LerNetCDF<-function(File){
  
  nc_data <- nc_open(File)
  # Save the print(nc) dump to a text file
  {
    sink(paste0(File,".txt")  )
    print(nc_data)
    sink()
  }
  return(nc_data)
  
}

FindDimensionNames<-function(NetCdfFile){
  Dimensoes=names(NetCdfFile$dim)
  return(Dimensoes)
}

FindVariableNames<-function(NetCdfFile){
  Variaveis=names(NetCdfFile$var)
  return(Variaveis)
}

FindLongVariableNames<-function(NetCdfFile){
  ShortNames=FindVariableNames(NetCdfFile)
  Dicionario=list()
  for(i in ShortNames)
    Dicionario[[i]]=NetCdfFile$var[[i]][['longname']]
  return(unlist(Dicionario))
}

ExtractArrayBack<-function(NetCdfFile,Variable){
  Data=ncvar_get(NetCdfFile,Variable)
  FillValue <- ncatt_get(NetCdfFile, Variable, "_FillValue")
  if(FillValue$hasatt)
    Data[Data==FillValue$value]=NA
  return(Data)
}


ExtractArray<-function(NetCdfFile,Variable){
  out <- tryCatch(
    {
    Data=ncvar_get(NetCdfFile,Variable)
    FillValue <- ncatt_get(NetCdfFile, Variable, "_FillValue")
    if(FillValue$hasatt)
      Data[Data==FillValue$value]=NA
    Data
  },
error=function(cond){
  #print('erro')
})
  return(out)
}


ExtractDimensionNamesFromVariable<-function(NetCdfFile,Variable){
  vec=c()
  print(length(NetCdfFile$var[[Variable]]$dim))
  for(j in 1:length(NetCdfFile$var[[Variable]]$dim))
    vec[j]=NetCdfFile$var[[Variable]]$dim[[j]]$name
  print(vec)
  return(vec)
}

########################################################################## End of function definitions

#require(bslib)
require(shinythemes)

# Define UI for application that draws a histogram
source('ClassicUI.R')

# Define server logic required to draw a histogram
server <- function(input, output) {
##################################################
  options(shiny.maxRequestSize=30*1024^3)  
  Leitura<-reactive({
    Dado=NA
    if(!is.null(input$Arquivo) )
      Dado=LerNetCDF(input$Arquivo$datapath)
    return(Dado)  
  })
  
  output$GenerateDimensionX<-renderUI({
    Let=Leitura()
    if(!is.na(Let))
    {
      DimensionNames=ExtractDimensionNamesFromVariable(NetCdfFile = Let,input$Variables)
      selectInput('DimensionX','Select Dimension of x axis',choices=DimensionNames,multiple = FALSE)
    }
  })
  
  output$GenerateDimensionY<-renderUI({
    Let=Leitura()
    if(!is.na(Let))
    {
      DimensionNames=ExtractDimensionNamesFromVariable(NetCdfFile = Let,input$Variables)
      selectInput('DimensionY','Select Dimension of y axis',choices=DimensionNames,multiple = FALSE)
    }
  })
  
   output$GenerateFilters<-renderUI({
	Let=Leitura()
	if(!is.na(Let)){
		      DimensionNames=ExtractDimensionNamesFromVariable(NetCdfFile = Let,input$Variables)
		      #DimensionNames=DimensionNames[DimensionNames!=input$DimensionX]
		      #DimensionNames=DimensionNames[DimensionNames!=input$DimensionY]
		      lapply(1:length(DimensionNames), function(i) {
		      Valores=ExtractArray(NetCdfFile = Let,Variable = DimensionNames[i])
			    sliderInput(inputId = DimensionNames[i], label = paste("Value to filter variable", DimensionNames[i] ),
                	    min=min(Valores), max =max(Valores), value = c(min(Valores),max(Valores) ) ,step=min(diff(Valores)) )
  		      })
		      #for(i in 1:length(DimensionNames))
		       # numericInput(inputId = DimensionNames[i], label = paste("Value to filter variable", DimensionNames[i] ),
		        #             min = -1e10, max =1e10, value = 0)
		         
	}		
   })
   
   GenerateFilterNames<-reactive({
     Let=Leitura()
     if(!is.na(Let)){
       DimensionNames=ExtractDimensionNamesFromVariable(NetCdfFile = Let,input$Variables)
      # DimensionNames=DimensionNames[DimensionNames!=input$DimensionX]
       #DimensionNames=DimensionNames[DimensionNames!=input$DimensionY]
        return(DimensionNames)
       }
     
     
   })
   
   ExploreGeneratedHTML<-eventReactive(input$Botao,{
        DimensionNames=GenerateFilterNames()
         for(i in DimensionNames)
           print(c(i,input[[i]] )  )
         #lapply(1:length(DimensionNames), function(i) {
        #   numericInput(inputId = DimensionNames[i], label = paste("Value to filter variable", DimensionNames[i] ),
         #               min = -1e10, max =1e10, value = 0)
         
         
       })		
     
   

   
   output$TableArray<-renderDataTable({
     Let=Leitura()
     if(!is.na(Let)){
       #Array=ReactiveExtractArray()
       #require(reshape2)
       #df=melt(Array)
       #Nomes=ExtractDimensionNamesFromVariable(Let,input$Variables)
       #names(df)[1:(ncol(df) -1) ]=Nomes
       #names(df)[ncol(df)]= Let$var[[input$Variables]][['longname']]#  input$Variables
       
       #for(i in 1:length(Nomes  ) ){
      #   print('Teste')
      #   MapeamentoDimensoes=ExtractArray(Let,Nomes[i]  )
      #   print(length(MapeamentoDimensoes))
      #   if(length(MapeamentoDimensoes))
      #    df[,i]=MapeamentoDimensoes[df[,i]]
      # }
       df=Filtro()
       df=df$Data
       df
     }
     
   })
  output$GenerateVariables<-renderUI({
    Let=Leitura()
    if(!is.na(Let))
    {
      DimensionNames=FindVariableNames(NetCdfFile = Let)
      selectInput('Variables','Select an attribute to visualize',choices=DimensionNames,multiple=FALSE)
      
    }
  })
  
  
  ReactiveExtractArray<-reactive({
    Let=Leitura()
    Data=ExtractArray(Let,Variable = input$Variables)
    return(Data)
  })
  
  
  
  Filtro<-reactive({
    Let=Leitura()
    require(reshape2)
    Data=ReactiveExtractArray()
    Data=melt(Data) #Convertendo para dataframe
    
    Nomes=ExtractDimensionNamesFromVariable(Let,input$Variables)
    names(Data)[1:(ncol(Data) -1) ]=Nomes
    names(Data)[ncol(Data)]=input$Variables# Let$var[[input$Variables]][['longname']]
    
    
    
    DimensionNames=Nomes#ExtractDimensionNamesFromVariable(NetCdfFile =Let,Variable = input$Variables ) #Obtendo Dimensoes que descrevem a variavel
    #ExploreGeneratedHTML()
    
    DimensionsToFilter=GenerateFilterNames()
    DimensionsToFilter=DimensionsToFilter[DimensionsToFilter %in% DimensionNames] #Filtrando para considerar apenas
    
    Nomes=ExtractDimensionNamesFromVariable(Let,input$Variables)
    
    for(i in 1:length(Nomes  ) ){
      # print('Teste')
      MapeamentoDimensoes=ExtractArray(Let,Nomes[i]  )
      print(length(MapeamentoDimensoes))
      if(length(MapeamentoDimensoes))
        Data[,i]=MapeamentoDimensoes[Data[,i]]
    }
    
    
    
    for(i in DimensionsToFilter){
      Entrada=input[[i]]
      print(i)
      print(names(Data))
      print(head(Data))
      print(input[[i]])
      Values=input[[i]]
      if(length(Values)==1){
        Data=Data[Data[[i]]==Values[1],   ]
      }else{
        Data=Data[Data[[i]]>=Values[1] & Data[[i]]<=Values[2],   ]
      }
    }
    return(list(Data=Data,Dimensions=DimensionsToFilter  ) )
  })
  
  output$VisualizeArray<-renderPlot({
    Data=Filtro()
    DimensionsToFilter=Data$Dimensions
    Data=Data$Data
    
    
    
    
    #Finalizando o filtro fazemos o grafico
    require(ggplot2)
    Data=Data[complete.cases(Data),]
    print(head(Data))
    DimensionsToFilter=DimensionsToFilter[DimensionsToFilter!=input$DimensionX]
    DimensionsToFilter=DimensionsToFilter[DimensionsToFilter!=input$DimensionY]
    for(fatorizacao in DimensionsToFilter){
		Data[fatorizacao]=paste0(fatorizacao,"=",Data[[fatorizacao]]   )
	}
    figura=ggplot(Data) + aes_string(x=input$DimensionX,y=input$DimensionY,fill=input$Variables ) + geom_tile()
    if(length(DimensionsToFilter)>0){
    string=paste(DimensionsToFilter,collapse = "+")
    string=paste('~',string)
    figura=figura+facet_wrap(as.formula(string)  ) 
	}
    #print(string)
    print(figura +  scale_fill_gradientn(colours=rainbow(20)) )
    
  })
  
  
  output$Report<-renderText({
    
    Dados=ReactiveExtractArray()
   # NomesDimensoes=ExtractDimensionNamesFromVariable(NetCdfFile = Leitura(),Variable = input$Variables)
    Dimensao=paste(dim(Dados),collapse = " ")
    #Nomes=
   # print('hue')
    print(paste('The variable',input$Variables,'has dimensions',Dimensao," 
                Its longname is:",Leitura()$var[[input$Variables]][['longname']],"
                It has as a mean value:",mean(Dados,na.rm=TRUE)," 
                Standart deviation:", sd(Dados,na.rm=TRUE),"
                Its percentage of missing values is:",mean(is.na(Dados)),"
                Its dimensions are named:",paste(ExtractDimensionNamesFromVariable(NetCdfFile = Leitura(),Variable = input$Variables)  ,collapse=" ") ) )
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

