#if("shiny" %in% rownames(installed.packages()) == FALSE) {install.packages("shiny")}
library(shiny)
#if("tidyverse" %in% rownames(installed.packages()) == FALSE) {install.packages("tidyverse")}
library(tidyverse)
#if("NLP" %in% rownames(installed.packages()) == FALSE) {install.packages("shiny")}
library(NLP)
#if("tm" %in% rownames(installed.packages()) == FALSE) {install.packages("tidyverse")}
library(tm)
#if("RColorBrewer" %in% rownames(installed.packages()) == FALSE) {install.packages("shiny")}
library(RColorBrewer)
#if("wordcloud" %in% rownames(installed.packages()) == FALSE) {install.packages("tidyverse")}
library(wordcloud)

#loading the data

bb=read_csv("billboard_lyrics_1964-2015.csv")
str(bb)
summary(bb)

bb1965=bb%>%select(Song,Lyrics,Year)%>%filter(Year==1965)
bb2015=bb%>%select(Song,Lyrics,Year)%>%filter(Year==2015)
options(warn = -1)
#TextPrepocessing
TextPrepocessing= function(x) {
  x=Corpus(VectorSource(x))  
  x=tm_map(x,removePunctuation)
  x=tm_map(x,removeNumbers)
  x=tm_map(x,stripWhitespace)
  x=tm_map(x,removeWords,stopwords("SMART"))
  x=tm_map(x, removeWords, c("dont","know","get","youre","ive","aint","ill","wont","cant","can","got","thy", "gotta", "gonna", "the", "and", "but","yeah","wanna","want"))
}

#Word frequency function
wordfreq= function(x) {
  x=TextPrepocessing(x)
  x=TermDocumentMatrix(x)
  x=as.matrix(x)
  x= sort(rowSums(x),decreasing=TRUE)
  x= data.frame(word = names(x),freq=x)
}

#getertmatrix
getTermMatrix =function(x) {
  
  myCorpus=TextPrepocessing(x)
  myDTM = TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 1))
  
  m = as.matrix(myDTM)
  stopwords("SMART")
  sort(rowSums(m), decreasing = TRUE)
}

#part 1
x=count(bb,Artist)
x1=merge(bb,x,by="Artist")
x1=x1%>%filter(n!=0)

#part 1
ml=bb$Lyrics
ml1=as.list(ml)
ml2=wordfreq(ml1)

#part c
mt=bb$Song
mt1=as.list(mt)
mt2=wordfreq(mt1)

#app
ui <- fluidPage(
   
   # Application title
  titlePanel("Billboard Year-End Hot 100 songs between 1965 and 2015"),

  # Show a plot of the generated distribution
  mainPanel(
    tabsetPanel ( type = "tabs",
                  tabPanel("Number of Hits of a Given Rank",
                           sliderInput("rank",
                                              "Rank",
                                                    min = 1,
                                                    max = 100,
                                                   value = 1,step=5), plotOutput("distplot",height=1000,width = "100%")),
                  tabPanel("Frequency in Lyrics",plotOutput("lyrics"),
                                    sliderInput("year1",
                                                "year1",
                                                min = 1965,
                                                max = 2015,
                                                value = 1,step=5),plotOutput("lyrics_1"), 
                                    sliderInput("year2",
                                                "year2",
                                                min = 1965,
                                                max = 2015,
                                                value = 1,step=5),plotOutput("lyrics_2")),
                  tabPanel("Frequency in Song Title",plotOutput("song"),
                           sliderInput("Year1",
                                       "Year1",
                                       min = 1965,
                                       max = 2015,
                                       value = 1,step=5),plotOutput("song_1"), 
                           sliderInput("Year2",
                                       "Year2",
                                       min = 1965,
                                       max = 2015,
                                       value = 1,step=5),plotOutput("song_2")),
                  tabPanel("Word Cloud for Song Title and Lyrics",
                           sliderInput("freq",
                                       "Minimum Frequency:",
                                       min = 1,  max = 50, value = 15,step=5),
                           sliderInput("max",
                                       "Maximum Number of Words:",
                                       min = 1,  max = 1000,value = 100,step=100),
                           plotOutput("wordcloud"),
                           plotOutput("wordcloud_2"))
                  
    )))
   
server <- function(input, output) {

#q1  
  
  output$distplot <- renderPlot({
    req(input$rank)
    x2=x1%>%
      filter(input$rank==x1$Rank)
    ggplot(x2, aes(Artist, y=n)) +
      geom_bar(stat="identity",width = 0.5,fill="blue")+ ylab ("Frequency")+xlab ( "Rank by Artist")+
      ggtitle("Number of Hits of a Given Rank")+theme_minimal()+coord_flip()
  })
  
#q2  
  
  output$lyrics <- renderPlot({
    
    ggplot(data=ml2[1:25,], aes(x=word, y=freq)) +
      geom_bar(stat="identity",width = 0.5,fill="black")+ ylab ("Word Frequencies")+xlab ( "Frequent Words")+
      ggtitle("Most Frequent Words in Lyrics")+theme_minimal()
  })

  output$lyrics_1 <- renderPlot({
    
    data_1<-bb[,c("Lyrics","Year")]
    req(input$year1)
    t1<-data_1%>%
      filter(input$year1==data_1$Year)
    
    mly=t1$Lyrics
    mly1=wordfreq(mly)
    ggplot(data=mly1[1:25,], aes(x=word, y=freq)) +
      geom_bar(stat="identity",width = 0.5,fill="black")+ ylab ("Word Frequencies")+xlab ( "Frequent Words")+
      ggtitle("Most Frequent Words in Lyrics")+theme_minimal()
    
      })
  output$lyrics_2 <- renderPlot({
    
    data_2<-bb[,c("Lyrics","Year")]
    req(input$year2)
    t2<-data_2%>%
      filter(input$year2==data_2$Year)
    mly=t2$Lyrics
    mly2=wordfreq(mly)
    ggplot(data=mly2[1:25,], aes(x=word, y=freq)) +
      geom_bar(stat="identity",width = 0.5,fill="black")+ ylab ("Word Frequencies")+xlab ( "Frequent Words")+
      ggtitle("Most Frequent Words in Lyrics")+theme_minimal()
  })
#q3
  output$song <- renderPlot({
    
    ggplot(data=mt2[1:25,], aes(x=word, y=freq)) +
      geom_bar(stat="identity",width = 0.5,fill="black")+ ylab ("Word Frequencies")+xlab ( "Frequent Words")+
      ggtitle("Most Frequent Words in Song Title")+theme_minimal()
  })
  
  output$song_1 <- renderPlot({
    
    data_12<-bb[,c("Song","Year")]
    req(input$Year1)
    t12<-data_12%>%
      filter(input$Year1==data_12$Year)
    mty=t12$Song
    mty1=wordfreq(mty)
    ggplot(data=mty1[1:25,], aes(x=word, y=freq)) +
      geom_bar(stat="identity",width = 0.5,fill="black")+ ylab ("Word Frequencies")+xlab ( "Frequent Words")+
      ggtitle("Most Frequent Words in Song Titel")+theme_minimal()
    
    
  })
  output$song_2 <- renderPlot({
    
    data_22<-bb[,c("Song","Year")]
    req(input$Year2)
    t22<-data_22%>%
      filter(input$Year2==data_22$Year)
    mty=t22$Song
    mty2=wordfreq(mty)
    ggplot(data=mty2[1:25,], aes(x=word, y=freq)) +
      geom_bar(stat="identity",width = 0.5,fill="black")+ ylab ("Word Frequencies")+xlab ( "Frequent Words")+
      ggtitle("Most Frequent Words in SOng Title")+theme_minimal()
  })
  
#q4  
  
    output$wordcloud <- renderPlot ({
        terms <- reactive({
          a1=bb$Song
          a2=as.list(a1)
          getTermMatrix(a2)
        })
      
      v <- terms()
      wordcloud_rep <- repeatable(wordcloud)
      wordcloud_rep(names(v), v, scale=c(2.1,1),
                    min.freq = input$freq, max.words=input$max,
                    colors=brewer.pal(8, "Dark2"),random.color=T,random.order=F)
      
    })
    
    output$wordcloud_2 <- renderPlot ({
      terms_1 <- reactive({
        a11=bb$Lyrics
        a21=as.list(a11)
        getTermMatrix(a21)
      })
      
      v1 <- terms_1()
      wordcloud_rep_1 <- repeatable(wordcloud)
      wordcloud_rep_1(names(v1), v1, scale=c(2.1,1),
                    min.freq = input$freq, max.words=input$max,
                    colors=brewer.pal(8, "Dark2"),random.color=T,random.order=F)
      
    })
    
    
    
  }

# Run the application 
shinyApp(ui = ui, server = server)




