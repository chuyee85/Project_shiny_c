library(shiny)
library(DT)
library(dplyr)
library(data.table)
library(shinythemes)
library(RJSONIO)
library(RCurl)
library(flexdashboard)
library(BSDA)
library(gsheet)
#中區面板
options(DT.options = list(paging = FALSE, lengthChange = FALSE, info = FALSE, searching = FALSE))


WEEKDATE <- read.csv("~", colClasses = c("Date","character")) %>%
  filter(between(date,as.Date("2017-01-01"),Sys.Date())) 


ui<- navbarPage(theme=shinytheme("cosmo"),
                
                "中區定點醫師",
                
                tabPanel("即時分析",
                         sidebarPanel(
                           
                           selectInput("dist", "定點醫師所在縣市：",
                                       c("全選","台中市","南投縣","彰化縣")),
                           
                           selectInput("yw","填報年週：",c(sort(unique(as.character(WEEKDATE$yearweek)), decreasing = T))),
                           
                           helpText("註：使用Sign Test檢定當週定點醫師回覆結果是否達統計顯著高於或低於「持平」，未達統計顯著則為「持平」。檢定結果僅限全區，單選縣市時，不以縣市別檢定。"),
                           
                           width = 2),
                         
                         mainPanel(
                           tabsetPanel(
                             tabPanel("統計圖表",
                                      fluidRow(h1(textOutput("docno"))),
									  fluidRow(h2("病例數流行趨勢：")),
                                      fluidRow(column(4, align="center", h3("類流感"), gaugeOutput("gauge_ili_t", height = "120%"),h5(htmlOutput("test_ili_t"))),
                                               column(4, align="center", h3("腸病毒"), gaugeOutput("gauge_ev_t", height = "120%"),h5(htmlOutput("test_ev_t"))),
                                               column(4, align="center", h3("腹瀉"), gaugeOutput("gauge_di_t", height = "120%"),h5(htmlOutput("test_di_t")))
                                      ),
                                      hr(),
                                      fluidRow(h2("病例嚴重度：")),
                                      fluidRow(column(4, align="center", h3("類流感"), gaugeOutput("gauge_ili_v", height = "120%"),h5(htmlOutput("test_ili_v"))),
                                               column(4, align="center", h3("腸病毒"), gaugeOutput("gauge_ev_v", height = "120%"),h5(htmlOutput("test_ev_v"))),
                                               column(4, align="center", h3("腹瀉"), gaugeOutput("gauge_di_v", height = "120%"),h5(htmlOutput("test_di_v")))
                                      )
                             ),
                             
                             tabPanel("回饋意見",
                                      fluidRow(column(6,h3("是否有其他特殊症狀或傳染病出現流行疫情："),dataTableOutput("if_epi")),
                                               column(6,h3("傳染病政策建議或異常傳染病相關訊息："),dataTableOutput("oth_sug")))
                                      
                             )
                           )
                         )
                         
                )
)



server <- function(input, output, session) {
  
 
  
  google_sheet<-gsheet2tbl("~") %>% as.data.frame()
  google_sheet<-google_sheet[,1:12]
  
  
  RC <- google_sheet %>%
    setNames(.,c("TIME", "DATE", "DR_ID", "ILI_TREND", "ILI_VOL", "EV_TREND", "EV_VOL", "DI_TREND", "DI_VOL", "IF_EPI", "OTHER_SUGG", "DR_COUNTY")) %>%
    mutate(DATE= as.Date(DATE)) %>% left_join(WEEKDATE, by= c("DATE"="date"))
  epi <- RC
  
  output$if_epi <-  DT::renderDataTable(DT::datatable({
    if (input$dist != "全選") {
      epi <- epi[epi$DR_COUNTY == input$dist,]
    }
    
    epi %>% filter(yearweek==input$yw & IF_EPI!="無") %>% select(IF_EPI) 
  } , rownames = FALSE , colnames = NULL
  ))
  
  
  
  output$oth_sug <-  DT::renderDataTable(DT::datatable({
    if (input$dist != "全選") {
      epi <- epi[epi$DR_COUNTY == input$dist,]
    }
    
    epi %>% filter(yearweek==input$yw & OTHER_SUGG!="無") %>% select(OTHER_SUGG) 
  } , rownames = FALSE , colnames = NULL
  ))
  
  
  output$gauge_ili_t <- renderGauge({
    if (input$dist != "全選") {
      ili_t <- round(mean(epi %>% filter(yearweek==input$yw & DR_COUNTY == input$dist) %>% select(ILI_TREND) %>% .$ILI_TREND),1)
    } else {
      ili_t <- round(mean(epi %>% filter(yearweek==input$yw) %>% select(ILI_TREND) %>% .$ILI_TREND),1)
    }
    
    gauge(ili_t, 
          min = 0, 
          max = 10, 
          sectors = gaugeSectors(success = c(0, 5), 
                                 warning = c(5, 7),
                                 danger = c(7, 10)))
  })
  
  
  output$gauge_ev_t <- renderGauge({
    if (input$dist != "全選") {
      ev_t <- round(mean(epi %>% filter(yearweek==input$yw & DR_COUNTY == input$dist) %>% select(EV_TREND) %>% .$EV_TREND),1)
    } else {
      ev_t <- round(mean(epi %>% filter(yearweek==input$yw) %>% select(EV_TREND) %>% .$EV_TREND),1)
    }
    
    gauge(ev_t, 
          min = 0, 
          max = 10, 
          sectors = gaugeSectors(success = c(0, 5), 
                                 warning = c(5, 7),
                                 danger = c(7, 10)))
  })
  
  
  output$gauge_di_t <- renderGauge({
    if (input$dist != "全選") {
      di_t <- round(mean(epi %>% filter(yearweek==input$yw & DR_COUNTY == input$dist) %>% select(DI_TREND) %>% .$DI_TREND),1)
    } else {
      di_t <- round(mean(epi %>% filter(yearweek==input$yw) %>% select(DI_TREND) %>% .$DI_TREND),1)
    }
    
    gauge(di_t, 
          min = 0, 
          max = 10, 
          sectors = gaugeSectors(success = c(0, 5), 
                                 warning = c(5, 7),
                                 danger = c(7, 10)))
  })
  
  
  
  
  output$gauge_ili_v <- renderGauge({
    if (input$dist != "全選") {
      ili_v <- round(mean(epi %>% filter(yearweek==input$yw & DR_COUNTY == input$dist) %>% select(ILI_VOL) %>% .$ILI_VOL),1)
    } else {
      ili_v <- round(mean(epi %>% filter(yearweek==input$yw) %>% select(ILI_VOL) %>% .$ILI_VOL),1)
    }
    
    gauge(ili_v, 
          min = 0, 
          max = 10, 
          sectors = gaugeSectors(success = c(0, 5), 
                                 warning = c(5, 7),
                                 danger = c(7, 10)))
  })
  
  
  output$gauge_ev_v <- renderGauge({
    if (input$dist != "全選") {
      ev_v <- round(mean(epi %>% filter(yearweek==input$yw & DR_COUNTY == input$dist) %>% select(EV_VOL) %>% .$EV_VOL),1)
    } else {
      ev_v <- round(mean(epi %>% filter(yearweek==input$yw) %>% select(EV_VOL) %>% .$EV_VOL),1)
    }
    
    gauge(ev_v, 
          min = 0, 
          max = 10, 
          sectors = gaugeSectors(success = c(0, 5), 
                                 warning = c(5, 7),
                                 danger = c(7, 10)))
  })
  
  
  output$gauge_di_v <- renderGauge({
    if (input$dist != "全選") {
      di_v <- round(mean(epi %>% filter(yearweek==input$yw & DR_COUNTY == input$dist) %>% select(DI_VOL) %>% .$DI_VOL),1)
    } else {
      di_v <- round(mean(epi %>% filter(yearweek==input$yw) %>% select(DI_VOL) %>% .$DI_VOL),1)
    }
    
    gauge(di_v, 
          min = 0, 
          max = 10, 
          sectors = gaugeSectors(success = c(0, 5), 
                                 warning = c(5, 7),
                                 danger = c(7, 10)))
  })
  
  
  
  output$test_ili_t <- renderText({ 
    i_t <- epi %>% filter(yearweek==input$yw) %>% select(ILI_TREND) %>% .$ILI_TREND
    ili_t_test <- SIGN.test(i_t, md=5)
    
    if (ili_t_test$p.value < 0.05 & mean(i_t) >5) {
      HTML("<div style='color:#ff0000'>","顯著增加","</div>")
    } else if (ili_t_test$p.value < 0.05 & mean(i_t) <=5) {
      HTML("<div style='color:#009933'>","顯著減少","</div>")
    } else {
      HTML("<div style='color:#1a1a1a'>","趨勢持平","</div>")
    }
    
  })
  
  
  output$test_ev_t <- renderText({ 
    e_t <- epi %>% filter(yearweek==input$yw) %>% select(EV_TREND) %>% .$EV_TREND
    ev_t_test <- SIGN.test(e_t, md=5)
    
    if (ev_t_test$p.value < 0.05 & mean(e_t) >5) {
      HTML("<div style='color:#ff0000'>","顯著增加","</div>")
    } else if (ev_t_test$p.value < 0.05 & mean(e_t) <=5) {
      HTML("<div style='color:#009933'>","顯著減少","</div>")
    } else {
      HTML("<div style='color:#1a1a1a'>","趨勢持平","</div>")
    }
    
  })
  
  
  output$test_di_t <- renderText({ 
    d_t <- epi %>% filter(yearweek==input$yw) %>% select(DI_TREND) %>% .$DI_TREND
    di_t_test <- SIGN.test(d_t, md=5)
    
    if (di_t_test$p.value < 0.05 & mean(d_t) >5) {
      HTML("<div style='color:#ff0000'>","顯著增加","</div>")
    } else if (di_t_test$p.value < 0.05 & mean(d_t) <=5) {
      HTML("<div style='color:#009933'>","顯著減少","</div>")
    } else {
      HTML("<div style='color:#1a1a1a'>","趨勢持平","</div>")
    }
    
  })
  
  
  output$test_ili_v <- renderText({ 
    i_v <- epi %>% filter(yearweek==input$yw) %>% select(ILI_VOL) %>% .$ILI_VOL
    ili_v_test <- SIGN.test(i_v, md=5)
    
    if (ili_v_test$p.value < 0.05 & mean(i_v) >5) {
      HTML("<div style='color:#ff0000'>","顯著嚴重","</div>")
    } else if (ili_v_test$p.value < 0.05 & mean(i_v) <=5) {
      HTML("<div style='color:#009933'>","顯著輕微","</div>")
    } else {
      HTML("<div style='color:#1a1a1a'>","大抵持平","</div>")
    }
    
  })
  
  
  output$test_ev_v <- renderText({ 
    e_v <- epi %>% filter(yearweek==input$yw) %>% select(EV_VOL) %>% .$EV_VOL
    ev_v_test <- SIGN.test(e_v, md=5)
    
    if (ev_v_test$p.value < 0.05 & mean(e_v) >5) {
      HTML("<div style='color:#ff0000'>","顯著嚴重","</div>")
    } else if (ev_v_test$p.value < 0.05 & mean(e_v) <=5) {
      HTML("<div style='color:#009933'>","顯著輕微","</div>")
    } else {
      HTML("<div style='color:#1a1a1a'>","大抵持平","</div>")
    }
    
  })
  
  
  output$test_di_v <- renderText({ 
    d_v <- epi %>% filter(yearweek==input$yw) %>% select(DI_VOL) %>% .$DI_VOL
    di_v_test <- SIGN.test(d_v, md=5)
    
    if (di_v_test$p.value < 0.05 & mean(d_v) >5) {
      HTML("<div style='color:#ff0000'>","顯著嚴重","</div>")
    } else if (di_v_test$p.value < 0.05 & mean(d_v) <=5) {
      HTML("<div style='color:#009933'>","顯著輕微","</div>")
    } else {
      HTML("<div style='color:#1a1a1a'>","大抵持平","</div>")
    }
    
  })

   output$docno <- renderText({ 
    epi <- epi[epi$yearweek==input$yw,]
    
    if (input$dist != "全選") {
      epi <- epi[epi$DR_COUNTY == input$dist,]
    } 
    paste0("共訪問  ", nrow(epi),"  位定點醫師")
  }) 
  
}



# setwd("~")
#runApp(list(ui = ui, server = server), port = ~, host = "~", launch.browser = FALSE)
shinyApp(ui = ui, server = server)



