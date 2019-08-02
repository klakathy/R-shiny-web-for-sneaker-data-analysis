library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(plotly)
library(wordcloud)
library(DT)
library(readxl)
library(readr)
library(tidyverse)
Sys.setlocale("LC_TIME", "C")

Shoess=read.csv('www/csv/Shoes_Norepeart.csv')
saless=read.csv('www/csv/Sales_Norepeat.csv')

######## for visualization page  ########################
shoes3<-Shoess
shoes3$release<-as.POSIXct(shoes3$releaseDate,format="%Y-%m-%d %H:%M:%S",tz="GMT")
shoes3$cate2<-as.character(shoes3$category)
shoes3$cate2[shoes3$brand=="Nike"] <- "Other"
shoes3$cate2[grep("Air Force",shoes3$category, ignore.case=TRUE, fixed=FALSE)] <- "Air Force"
shoes3$cate2[grep("Air Max",shoes3$category, ignore.case=TRUE, fixed=FALSE)] <- "Air Max"
shoes3$cate2[grep("Basketball",shoes3$category, ignore.case=TRUE, fixed=FALSE)] <- "Basketball"
shoes3$cate2[grep("Foamposite",shoes3$category, ignore.case=TRUE, fixed=FALSE)] <- "Foamposite"
shoes3$cate2[grep("KD",shoes3$category, ignore.case=TRUE, fixed=FALSE)] <- "KD"
shoes3$cate2[grep("Kobe",shoes3$category, ignore.case=TRUE, fixed=FALSE)] <- "Kobe"
shoes3$cate2[grep("LeBron",shoes3$category, ignore.case=TRUE, fixed=FALSE)] <- "LeBron"
shoes3$highestBid<-as.numeric(as.character(shoes3$highestBid))
#sum(is.na(shoes3$highestBid)) ##63
shoes3$lowestAsk<-as.numeric(as.character(shoes3$lowestAsk))
#sum(is.na(shoes3$lowestAsk)) ##72
shoes3$averageDeadstockPrice<-as.numeric(as.character(shoes3$averageDeadstockPrice))
#sum(is.na(shoes3$averageDeadstockPrice)) ##67
#shoes3%>%group_by(cate2)%>%summarise(n=n())

brands<-shoes3%>%group_by(brand,cate2)%>%summarize()

sales3<-saless
sales3$time<-as.POSIXct(sales3$time,format="%Y-%m-%d %H:%M:%S",tz="GMT")
sales3$size<-as.numeric(gsub("W|Y|K|C","",sales3$size))
#sum(is.na(sales3))

ids<-sales3%>%group_by(id)%>%summarise(n=n(),max=max(time),min=min(time),df=as.numeric(max-min)+1,total=sum(amount),f1=n/df,f2=total/df)
brands<-shoes3%>%group_by(brand,cate2)%>%summarize()
########################shiny start########################

ui <- dashboardPagePlus(  
  skin = "black",
  header = dashboardHeaderPlus(
    title = "Group Project"
  ),
  tags$head(
    tags$script(type="text/javascript",'function show2(x){Shiny.setInputValue("in5",x);}'),
    # tags$script(type="text/javascript",src="js/jquery.js"),
    tags$script(type="text/javascript",src="js/script.js"),
    tags$script(type="text/javascript",src="js/getcanvaspixelcolor1.js"),
    
    tags$script(type='text/javascript','function show(){
            c1 = document.getElementById("draw").getPixelColor(120, 180).rgb;
              c2 = document.getElementById("draw").getPixelColor(240, 180).rgb;
              c3 = document.getElementById("draw").getPixelColor(170, 260).rgb;
              c4 = document.getElementById("draw").getPixelColor(110, 280).rgb;
              c5 = document.getElementById("draw").getPixelColor(320, 300).rgb;
              c6 = document.getElementById("draw").getPixelColor(330, 240).rgb;
              c7 = document.getElementById("draw").getPixelColor(380, 310).rgb;
              c8 = document.getElementById("draw").getPixelColor(420, 235).rgb;
              c9 = document.getElementById("draw").getPixelColor(140, 160).rgb;
              c10 = document.getElementById("draw").getPixelColor(100, 200).rgb;
              c11 = document.getElementById("draw").getPixelColor(120, 300).rgb;
              c12 = document.getElementById("draw").getPixelColor(160, 190).rgb;
              c13 = document.getElementById("draw").getPixelColor(230, 290).rgb;
              c14 = document.getElementById("draw").getPixelColor(280, 200).rgb;
              c15 = document.getElementById("draw").getPixelColor(325, 260).rgb;
              c16 = document.getElementById("draw").getPixelColor(350, 300).rgb;
              c17 = document.getElementById("draw").getPixelColor(355, 270).rgb;
              c18 = document.getElementById("draw").getPixelColor(340, 280).rgb;
              
              c888 = [c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14,c15,c16,c17,c18];console.log(c888);
              
              Shiny.setInputValue("in1",c888);Shiny.setInputValue("in2",c888.join());console.log(c888.toString());
              }')
    
  ),
  
####################### other ####   
  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem(
        text = "Home", 
        tabName = "page1", 
        icon = icon("home")
      ),
      menuItem(
        text = "Data Table", 
        tabName = "Tables",
        icon = icon("cog", lib="glyphicon")
      ),
      menuItem(
        text = "Data Visualization", 
        tabName = "page3",
        icon = icon("cog", lib="glyphicon")
      ),
      menuItem(
        text = "Color Matching", 
        tabName = "shoes",
        icon = icon("cog", lib="glyphicon")
      )
    )
  ),
  body = dashboardBody(
    tabItems(
######################## home page##############
      tabItem(
        tabName = "page1",
        fluidRow(
          column(1),
          column(10,
            h3("Description"),
            p(
            "The inspiration for this project is from the ted talk why sneakers are a great investment by Josh Luber, the creator of StockX the online sneaker marketplace. This is the kind of investment that you can control and that can pay out in dividends much greater than the historic 9% per year. In fact, it’s not uncommon to make 100-200% on one single pair of shoes. For example, Off-White Air Jordan 1 returns a price premium of 942.1% over the original retail price. However, a lot of sneakers don’t have any investment values. Our project will help our target customer understand which types/colors/sizes/styles of the upcoming sneakers are better investments through the historical data stored in our database and the value evaluation module."
            ),
            br(),
            img(src='stockx.png'),
            br(),
            br(),
            h3("Research Questions"),
            p(
            span("What shoes have the greater sales potential and better investment value based on the information on"), 
            a(href="https://stockx.com", target="_blank", "stockx"),
            span("a ecommerce platform with an emphasis on the sneaker resale market.")
            ),
            br(), # adds an empty line
            img(src='adidas.png', height= 100,width=150),
            HTML('&nbsp;'),
            img(src='Jordan.png', height= 100,width=100),
            HTML('&nbsp;'),
            img(src='nike.png', height= 100,width=200),
            br(),
            br(),
            h3("Methods"),
            p("The main methods in this small project are Web Scrapping, Color Matching, Wordcloud Visualization, Image matching"),
            br(),
            h4("Web Scrapping"),
            p("We use webscrapping methods to collect all the data and images from the stockx websites."),
            br(),
            
            h4("Wordcloud Visualization"),
            p(
            code("Wordcould"), 
            span("package in R has been used to visualize the most frequent words.")
            ),
            br(),
            h4("Color matching"),
            p("We compare the color customer painted on the shoes to match the sneakers in the system"),
            br(),
            h4("Image matching"),
            p("We use the sneaker brand and category of customer choose to calculated the best sale and best earn sneakers and display the related products to the customer match the sneakers in the system"),
            br(),
            
            h3("Tabs"),
            h4("Data Table"), 
            p("User are able to see the data set we use in this project"),
            h4("Data Visualization"), 
            p("Users are able to choose brand and category of the shose and see the associated results. The realted plot designed in this page that helps users know more about the information of their interesed sneakers. The features avaible are as follows:"),
            # we create an unorder list using ul and li.
            tags$ul(
            tags$li("Side column displaying brand and category name that helps users to make decision"),
            tags$li("Price movement of different shoes by its size"),
            tags$li("Wordcloud of the popular shoes by users' choice")
            
            ),
            h4("Color Matching"), 
            p("User are able choose the color and paint the shoes as their preferrence and see the related output based upon the color matching"),
            br(),

            br(), 
            h3("___________")
          )
        )
      ),
      
      
####################### color page ####      
      tabItem(
        style="overflow-y:scroll; max-height: 750px;overflow-x:hidden;",
        tabName = "shoes",
        fluidRow(
          column(1),
          column(10,
            h1('Drawing To match'),
            # tags$canvas(id="draw",style="width:550px;height:450px;background-image:url('images/zm.jpg');background-repeat:no-repeat;background-size:100% 100%;-moz-background-size:100% 100%;"),
            includeHTML("www/Draw.html"),
            # htmlOutput("draw1"),
            tags$button(onclick='show()','Search'),
            # tags$button(onclick='show7()','Search2222'),
            uiOutput("preview")
            )
          )
      ),
######################## table page###########      
      tabItem(
        tabName = "Tables",
        fluidRow(style = "overflow-x:scroll; max-width: 100%",
            tabBox(
              width = 12,
              tabPanel(
              title = "Shoes Chart",
              dataTableOutput("myShoes")
              
              ),
              tabPanel(
              title = "sales Chart",
              dataTableOutput("mySales")
              )
          )
        )
      ),
######################## visual page ###########
      tabItem(
        tabName = "page3",
        fluidRow(
            column(
            width = 2,
            p("Choose by brand and category to view visualization"),
            tabPanel("brand",
            checkboxGroupInput("brand1","adidas",brands[brands$brand=="adidas",]$cate2),
            checkboxGroupInput("brand2","Nike",brands[brands$brand=="Nike",]$cate2),
            checkboxGroupInput("brand3","Jordan",brands[brands$brand=="Jordan",]$cate2)
            )
            ),
            
            column(width = 6,
            h1("Information Overview"),
            mainPanel(width = "600px",
            tabsetPanel(
            tabPanel("View table",dataTableOutput('out2'),dataTableOutput('out22')),
            tabPanel("Price visualizaion",style="overflow-x:hidden;",
             radioButtons(inputId = "s7","Variable",c("Highest bid price"="highestBid","Lowest ask rice"="lowestAsk","Average sales price"="averageDeadstockPrice"),selected = "highestBid"),
             radioButtons(inputId = "s8","Method",c("mean","max","min","median","mode"),selected = "mean"),
             plotlyOutput("out7",height="600px")
             ),
            tabPanel("Popularity visualization",
            box(width = "600px",style="overflow-x:hidden;",
            radioButtons(inputId = "s9","Method",c("Best Sell (Calculated by selling amount)"="Best Sell","Best Earn (calculated by total revenue)"="Best Earn"),selected = "Best Sell"),
            plotOutput(outputId = "out3",width = "600px",height=500)
            )
            ),
            tabPanel("Top shoes",style="overflow-y:scroll; max-height: 750px;overflow-x:hidden;",uiOutput("out1",width = "600px"))
            )
            )
            
            ),
            column(width=4,
            height=NULL,
            h1("Detail of Shoes"),
            p("choose in Top shoes TAB to view detail"),
            uiOutput("shoename"),
            wellPanel(
            style="overflow-y:scroll; max-height: 400px;overflow-x:hidden;",
            plotlyOutput("out6"),
            plotlyOutput("out5"),
            plotlyOutput("out4")
            )
          )
        )
      )
#################### end visual page #############
    )
  )
)



server <- function(input, output) {

  output$myShoes = renderDataTable(Shoess, options = list(pageLength = 6))
  output$mySales = renderDataTable(saless, options = list(pageLength = 6))
  
  ######## for visualization page  ########################
  
  ids2<-reactive({
    if(input$s9=="Best Sell"){return(ids[order(ids$f1,decreasing = T),])}
    else{return(ids[order(ids$f2,decreasing = T),])}
  })

  shoes2<-reactive({
    if((length(input$brand1)+length(input$brand2)+length(input$brand3))==0){
      return(shoes3)
    }else{
      return(shoes3[(shoes3$cate2%in%c(input$brand1,input$brand2,input$brand3)),])
    }
  })
  
  ### column2 information1: table
  output$out2 <- renderDataTable(merge(ids2(),shoes2(),by="id",all=F,sort=F)[,c('brand','category','name')],options = list(pageLength = 6))
  
  ### column2 information2: mean
  getmean<- function(shoes,a,b) {
    shoes2<-shoes[,c("category",a)]
    names(shoes2)[2]<-"mean"
    if(b=="mean"){
      mean<-shoes2%>%group_by(category)%>%summarise(mean=mean(mean,na.rm=TRUE))
    }else if(b=="min"){
      mean<-shoes2%>%group_by(category)%>%summarise(mean=min(mean,na.rm=TRUE))
    }else if(b=="max"){
      mean<-shoes2%>%group_by(category)%>%summarise(mean=max(mean,na.rm=TRUE))
    }else if(b=="median"){
      mean<-shoes2%>%group_by(category)%>%summarise(mean=median(mean,na.rm=TRUE))
    }else{
      getmode1 <- function(mode) {
        uniqv <- unique(mode)
        uniqv[which.max(tabulate(match(mode, uniqv)))]
      }
      mean<-shoes2%>%filter(mean!="NA")%>%group_by(category)%>%summarise(mean=getmode1(mean))
    }
    mean
  }
  
  
  show3<-function(t){
    t<-t[,c('category','mean')]
    names(t)<-c('category','price')
    t$price<-as.integer(t$price)
    t<-t%>%arrange(desc(price))
    t$category<-reorder(t$category,t$price)
    t%>%ggplot(mapping = aes(x=category, y=price)) + geom_bar(stat = "identity", aes(fill=category),width=0.6)+theme_light() +coord_flip() + labs(title="Sneaker Brands Comparison", x="Brand", y="Price (USD)")+geom_text(aes(label=price),vjust=0.4,hjust=1.5,color="black",position=position_dodge(0.9),size=4)+guides(fill = FALSE)
  }
  output$out7<-renderPlotly({
    show3(getmean(shoes2(),input$s7,input$s8))
  })
  
  ### column2 information3: cloud
  c2<-function(f0,ids,shoes){
    if(f0=="Best Sell"){freq=(ids$f1*(10/max(ids$f2)))^2}
    #freq=replace(ids$f1/10,ids$f1/10>50,50)
    else freq=replace(ids$f2/1000,ids$f2/1000>50,50)
    wordcloud(words = merge(ids,shoes,by="id",all=F,sort=F)[,'name'],freq = freq,scale=c(4,.5),max.words = 30,random.order = F,rot.per = 0.35,min.freq = 1,colors=brewer.pal(8, "Dark2"))
  }
  output$out3 <- renderPlot({c2(input$s9,ids2(),shoes2())})
  
  ### column2 information4: boxes
  output$out1 <- renderUI({
    t2<-merge(ids2(),shoes2(),by="id",all=F,sort=F)[1:6,]
    fluidRow(
      column(12, id="columns",
             apply(t2,1, function(item) {
               wellPanel(
                 style='display:inline-block;height:250px;width:250px;background-color:rgb(255,255,255);margin-right:10px;',
                 HTML(paste0("<div onclick=show2('",item['id'],"') >",
                             "<h2>",item['brand'],"</h2>",
                             "<p>",item['name'],"</p>",
                             "<img src='",item['img'],"' style='height:100px;width:150px;' /></div>")
                 )
               )
             })
      )
    )
  })
  
  
  sales2<-reactive({
    if(length(input$in5)==0){
      return(sales3)
    }else{
      return(sales3[sales3$id==input$in5,])
    }
  })
  ### column3 detail0: showname 
  output$shoename<-renderUI({
    if(length(input$in5)==0){return()}
    else{
      item<-shoes3[shoes3$id==input$in5,]
      tmptitle<-item$category
      if(item$brand=="Nike")tmptitle<-paste0(item$brand," ",item$category)
      return(
        box(width="100%",
            h4(tmptitle),
            h2(item$name),
            p(paste(item$brand,item$name,"was released on",item$releaseDate,"After", as.integer(sales2()$time[which.max(sales2()$amount)]-item$release), "days, it reaches the highest bid price: $", max(sales2()$amount),sep=" ")),
            hr(),
            sliderInput("sizeSlider", "Shoes Size:", 
                        min = 5, max =18, value = 11, step = 0.5,width="100%",
                        animate = animationOptions(interval =2000, loop = TRUE))
        )
      )
    }
  })
  
  
  ### column3 detail1: by size animation
  output$out6<-renderPlotly({
    
    if(length(input$in5)==0){return()}
    else{
      sales4<-sales2()[(sales2()$size==input$sizeSlider),]
      sales4$size<-as.character(sales4$size)
      ggplot(data = sales4,mapping=aes(x=time,y=amount,color=size))+geom_point(size=0.1,alpha=0.7)+ geom_smooth(method = "loess",se = FALSE)+labs(title="Sneaker Price movement in 2019", x="month", y="Price (USD)")+xlim(min(sales2()$time),max(sales2()$time))+ylim(0,max(sales2()$amount))+guides(fill = FALSE)
    }
  })
  ### column3 detail2: all sales
  output$out5<-renderPlotly({
    
    if(length(input$in5)==0){return()}
    else{
      sales4<-sales2()
      sales4$size<-as.character(sales4$size)
      ggplot(data = sales4,mapping=aes(x=time,y=amount,color=size))+geom_point(size=2,alpha=0.7)
    }
  })
  ### column3 detail3: box plot
  output$out4<-renderPlotly({
    if(length(input$in5)==0){return()}
    else
      ggplot(data = sales2(),mapping=aes(x=size,y=amount,fill=as.character(size)))+geom_boxplot()+labs(fill='size')
  })
  ######## end visualization page  ########################
  
  ############ color page function #######################
  Colors=read.csv('www/csv/PIX2.csv')
  
  output$preview <- renderUI({
    if(length(input$in2)==0){return()}
    else{
      a01<-matrix(as.numeric(unlist(strsplit(input$in2,","))),nrow=18,ncol=3)
      Colors=Colors[,-1]
      Colors_2=matrix(as.numeric(as.matrix(Colors[,-2])),nrow=nrow(Colors))
      na<-matrix(a01,nrow = dim(Colors_2)[1],ncol = 54,byrow = FALSE)
      Res=abs(Colors_2[,-1]-na)
      Sum<-matrix(rowSums(Res),nrow = dim(Colors_2)[1],ncol = 1,byrow = FALSE)
      Sim=cbind(as.matrix(Colors)[,2],as.numeric(Sum))
      Sim=data.frame(Sim)
      colnames(Sim)=c('id','S')
      Sim$S<-as.numeric(as.character(Sim$S))
      Sim=Sim[sort(Sim$S,index.return=TRUE)$ix,]
      IMG=Shoess[Shoess$id%in%Sim[1:20,1],]$img
      NAME=Shoess[Shoess$id%in%Sim[1:20,1],]$name
      COLORWAY=Shoess[Shoess$id%in%Sim[1:20,1],]$colorway

      vv<-NULL
      for (i in 1:9) {
        vv[[i]]<- box(width = 4,height = "40%",status = "primary",
                    title = HTML(
                        paste0(
                        "<h2>",NAME[i],"</h2>",
                        "<h4> style:",COLORWAY[i],"</h2>",
                        "<p> simularity:",round(i-Sim[i,2]/4000,1),"</p>",

                        '<img style="width: 100%" src=',IMG[i],'>'
                        )
                    )
        )
      }

      return(
        fluidRow(
          fluidRow(vv[[9]],vv[[8]],vv[[7]]),
          fluidRow(vv[[6]],vv[[5]],vv[[4]]),
          fluidRow(vv[[3]],vv[[2]],vv[[1]])
        )
      )

    }

    # a01<-matrix(0,nrow=18,ncol=3)
    # a01=as.matrix(a01)
    # Colors=Colors[,-1]
    # Colors_2=matrix(as.numeric(as.matrix(Colors[,-2])),nrow=nrow(Colors))
    # na<-matrix(a01,nrow = dim(Colors_2)[1],ncol = 54,byrow = FALSE)
    # Res=abs(Colors_2[,-1]-na)
    # Sum<-matrix(rowSums(Res),nrow = dim(Colors_2)[1],ncol = 1,byrow = FALSE)
    # Sim=cbind(as.matrix(Colors)[,2],as.numeric(Sum))
    # Sim=data.frame(Sim)
    # colnames(Sim)=c('id','S')
    # Sim$S<-as.numeric(as.character(Sim$S))
    # Sim=Sim[sort(Sim$S,index.return=TRUE)$ix,]
    # 
    # IMG=Shoess[Shoess$id%in%Sim[1:20,1],]$img
    # NAME=Shoess[Shoess$id%in%Sim[1:20,1],]$name
    # COLORWAY=Shoess[Shoess$id%in%Sim[1:20,1],]$colorway
    # 
    # vv<-NULL
    # for (i in 1:9) {
    #   vv[[i]]<- box(width = 4,height = "40%",status = "primary",
    #               title = HTML(
    #                   paste0(
    #                   "<h2>",NAME[i],"</h2>",
    #                   "<h4> style:",COLORWAY[i],"</h2>",
    #                   "<p> simularity:",round(i-Sim[i,2]/4000,1),"</p>",
    # 
    #                   '<img style="width: 100%" src=',IMG[i],'>'
    #                   )
    #               )
    #   )
    # }
    # 
    # fluidRow(
    #          style="padding:20px;background-color:lightblue;",
    #   fluidRow(vv[[1]],vv[[4]],vv[[7]]),
    #   fluidRow(vv[[2]],vv[[5]],vv[[8]]),
    #   fluidRow(vv[[3]],vv[[6]],vv[[9]])
    # )

  })
  ############ end color function #######################
}




shinyApp(ui = ui, server = server)
