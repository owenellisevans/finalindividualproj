library(shiny)
library(fpp3)
library(dplyr)
library(ggplot2)
library(ggeasy)
library(plotly)
library(shinyWidgets)
library(shinydashboard)
library(forecast)

data("tourism")

unique(tourism$Region)
ui <- fluidPage(
  titlePanel("Australian Domestic Overnight Trips By State"),
  p("Welcome! The displays show dataset containing the quarterly overnight trips from 1998 Q1 to 2016 Q4 across Australia. Below are graphs which you can manipulate using the boxes blow. You can filter by State, Purpose of Visit, and choose between Seasonality, Autocorrelation, and Decomposition."),
 p(" The following regions coincide:"),
  p("South Australia- Adelaide, Adelaide Hills, Barossa, Clare Valley, Eyre Peninsula, Fleurieu Peninsula, Flinders Ranges and Outback, Kangaroo Island, Limestone Coast, Murraylands, North Coast NSW, Riverland, Yorke Peninsula."),
  p(" Northern Territory- Alice Springs, Barkly, Darwin, Kakadu Arnhem, Katherine Daly, Lasseter, MacDonnell "),
  p("  Western Australia- Australia's Coral Coast, Australia's Golden Outback, Australia's North West, Australia's South West, Experience Perth, Lakes "), 
  p("Victoria- Ballarat, Bendigo Loddon, Central Highlands, Central Murray, Geelong and the Bellarine, Gippsland, Goulburn, Great Ocean Road, High Country, Macedon, Mallee, Melbourne, Melbourne East, Murray East, Peninsula, Phillip Island, Spa Country, Upper Yarra, Western Grampians, Wimmera"), 
  p("New South Wales- Blue Mountains, Capital Country, Central Coast, Central NSW, Hunter, New England North West, Outback NSW, Riverina, Snowy Mountains, South Coast, Sydney, The Murray"),
  p("Queensland- Gold Coast, Brisbane, Bundaberg, Central Queensland, Darling Downs, Fraser Coast, Mackay, Northern, Outback, Sunshine Coast, Tropical North Queensland, Whitsundays"), 
  p("ACT- Canberra"),
  p("Tasmania- East Coast, Hobart and the South, Launceston, Tamar and the North, North West, Wilderness West"),
  p("Please ensure the correct Region coincides with the correct State!"),
  
  
  setBackgroundColor(
    color = "lightblue",
    gradient = c("linear", "radial"),
    direction = c("bottom", "top", "right", "left"),
    shinydashboard = FALSE
  ),
  
  plotOutput("Graph"),
  plotOutput("Graph2"),
  
  radioGroupButtons(
    inputId = "Choice",
    label = "Choices", 
    choices = c("Seasonality", "Autocorrelation", "Decomposition"),
    status = "primary"
  ),
  
  
  pickerInput(
    inputId = "State",
    label = "Choose a State", 
    choices = c("South Australia" ,   "Northern Territory",
                 "Western Australia" ,"Victoria"    ,      
                 "New South Wales",    "Queensland" ,       
                 "ACT"     ,   "Tasmania")
  ),
  pickerInput(
    inputId = "Purpose",
    label = "Choose a Purpose", 
    choices = c("Business", "Holiday",  "Other"  ,  "Visiting")
  ),
  pickerInput(
    inputId = "Region",
    label = "Pick a Region",
    choices = c("Adelaide", "Adelaide Hills" ,"Alice Springs" ,"Australia's Coral Coast","Australia's Golden Outback","Australia's North West","Australia's South West","Ballarat" ,"Barkly", "Barossa","Bendigo Loddon", "Blue Mountains" ,"Brisbane","Bundaberg","Canberra","Capital Country", "Central Coast", "Central Highlands" ,"Central Murray","Central NSW","Central Queensland","Clare Valley","Darling Downs","Darwin", "East Coast","Experience Perth","Eyre Peninsula","Fleurieu Peninsula", "Flinders Ranges and Outback","Fraser Coast","Geelong and the Bellarine","Gippsland", "Gold Coast", "Goulburn","Great Ocean Road","High Country", "Hobart and the South","Hunter","Kakadu Arnhem","Kangaroo Island","Katherine Daly","Lakes" ,"Lasseter" ,"Launceston, Tamar and the North","Limestone Coast", "MacDonnell" ,"Macedon","Mackay","Mallee","Melbourne", "Melbourne East","Murray East","Murraylands","New England North West" ,"North Coast NSW", "North West","Northern","Outback" ,"Outback NSW","Peninsula" ,"Phillip Island","Riverina","Riverland","Snowy Mountains","South Coast" ,"Spa Country","Sunshine Coast" ,"Sydney","The Murray" ,"Tropical North Queensland","Upper Yarra","Western Grampians","Whitsundays","Wilderness West","Wimmera","Yorke Peninsula")
  ),
  p("Seasonality: We see fairly large seasonality throughout the graphs with highs numbers during summer months.
  Autocorrelation: We don't see instinces of randomness throughout the dataset. 
  Decomposition: There are familiar trends in our seris of data. We see large spikes during the beginning of Q1, when summer takes place in Australia."),
 
   radioGroupButtons(
    inputId = "Choice1",
    label = "Choose which plot you would like to see", 
    choices = c("Holts", "Holts/Winters"),
    status = "primary"
  ),
  
  
  pickerInput(
    inputId = "State1",
    label = "Choose a State", 
    choices = c("South Australia" ,   "Northern Territory",
                "Western Australia" ,"Victoria"    ,      
                "New South Wales",    "Queensland" ,       
                "ACT"     ,   "Tasmania")
  ),
  pickerInput(
    inputId = "Purpose1",
    label = "Choose a Purpose", 
    choices = c("Business", "Holiday",  "Other"  ,  "Visiting")
  ),
  pickerInput(
    inputId = "Region1",
    label = "Pick a Region",
    choices = c("Adelaide", "Adelaide Hills" ,"Alice Springs" ,"Australia's Coral Coast","Australia's Golden Outback","Australia's North West","Australia's South West","Ballarat" ,"Barkly", "Barossa","Bendigo Loddon", "Blue Mountains" ,"Brisbane","Bundaberg","Canberra","Capital Country", "Central Coast", "Central Highlands" ,"Central Murray","Central NSW","Central Queensland","Clare Valley","Darling Downs","Darwin", "East Coast","Experience Perth","Eyre Peninsula","Fleurieu Peninsula", "Flinders Ranges and Outback","Fraser Coast","Geelong and the Bellarine","Gippsland", "Gold Coast", "Goulburn","Great Ocean Road","High Country", "Hobart and the South","Hunter","Kakadu Arnhem","Kangaroo Island","Katherine Daly","Lakes" ,"Lasseter" ,"Launceston, Tamar and the North","Limestone Coast", "MacDonnell" ,"Macedon","Mackay","Mallee","Melbourne", "Melbourne East","Murray East","Murraylands","New England North West" ,"North Coast NSW", "North West","Northern","Outback" ,"Outback NSW","Peninsula" ,"Phillip Island","Riverina","Riverland","Snowy Mountains","South Coast" ,"Spa Country","Sunshine Coast" ,"Sydney","The Murray" ,"Tropical North Queensland","Upper Yarra","Western Grampians","Whitsundays","Wilderness West","Wimmera","Yorke Peninsula")
  ),
  plotOutput("Graph3"),
  
  
  radioGroupButtons(
    inputId = "Choice2",
    label = "Choose which simple model you would like to see", 
    choices = c("Naive", "Seasonal Naive", "Mean", "Drift"),
    status = "primary"
  ),
  
  
  pickerInput(
    inputId = "State2",
    label = "Choose a State", 
    choices = c("South Australia" ,   "Northern Territory",
                "Western Australia" ,"Victoria"    ,      
                "New South Wales",    "Queensland" ,       
                "ACT"     ,   "Tasmania")
  ),
  pickerInput(
    inputId = "Purpose2",
    label = "Choose a Purpose", 
    choices = c("Business", "Holiday",  "Other"  ,  "Visiting")
  ),
  pickerInput(
    inputId = "Region2",
    label = "Pick a Region",
    choices = c("Adelaide", "Adelaide Hills" ,"Alice Springs" ,"Australia's Coral Coast","Australia's Golden Outback","Australia's North West","Australia's South West","Ballarat" ,"Barkly", "Barossa","Bendigo Loddon", "Blue Mountains" ,"Brisbane","Bundaberg","Canberra","Capital Country", "Central Coast", "Central Highlands" ,"Central Murray","Central NSW","Central Queensland","Clare Valley","Darling Downs","Darwin", "East Coast","Experience Perth","Eyre Peninsula","Fleurieu Peninsula", "Flinders Ranges and Outback","Fraser Coast","Geelong and the Bellarine","Gippsland", "Gold Coast", "Goulburn","Great Ocean Road","High Country", "Hobart and the South","Hunter","Kakadu Arnhem","Kangaroo Island","Katherine Daly","Lakes" ,"Lasseter" ,"Launceston, Tamar and the North","Limestone Coast", "MacDonnell" ,"Macedon","Mackay","Mallee","Melbourne", "Melbourne East","Murray East","Murraylands","New England North West" ,"North Coast NSW", "North West","Northern","Outback" ,"Outback NSW","Peninsula" ,"Phillip Island","Riverina","Riverland","Snowy Mountains","South Coast" ,"Spa Country","Sunshine Coast" ,"Sydney","The Murray" ,"Tropical North Queensland","Upper Yarra","Western Grampians","Whitsundays","Wilderness West","Wimmera","Yorke Peninsula")
  ),
    plotOutput("Graph4"),
 
 radioGroupButtons(
   inputId = "Choice3",
   label = "Arima", 
   choices = c("Manually Selected", "Auto Selected"),
   status = "primary"
 ),
 
 
 pickerInput(
   inputId = "State3",
   label = "Choose a State", 
   choices = c("South Australia" ,   "Northern Territory",
               "Western Australia" ,"Victoria"    ,      
               "New South Wales",    "Queensland" ,       
               "ACT"     ,   "Tasmania")
 ),
 pickerInput(
   inputId = "Purpose3",
   label = "Choose a Purpose", 
   choices = c("Business", "Holiday",  "Other"  ,  "Visiting")
 ),
 pickerInput(
   inputId = "Region3",
   label = "Pick a Region",
   choices = c("Adelaide", "Adelaide Hills" ,"Alice Springs" ,"Australia's Coral Coast","Australia's Golden Outback","Australia's North West","Australia's South West","Ballarat" ,"Barkly", "Barossa","Bendigo Loddon", "Blue Mountains" ,"Brisbane","Bundaberg","Canberra","Capital Country", "Central Coast", "Central Highlands" ,"Central Murray","Central NSW","Central Queensland","Clare Valley","Darling Downs","Darwin", "East Coast","Experience Perth","Eyre Peninsula","Fleurieu Peninsula", "Flinders Ranges and Outback","Fraser Coast","Geelong and the Bellarine","Gippsland", "Gold Coast", "Goulburn","Great Ocean Road","High Country", "Hobart and the South","Hunter","Kakadu Arnhem","Kangaroo Island","Katherine Daly","Lakes" ,"Lasseter" ,"Launceston, Tamar and the North","Limestone Coast", "MacDonnell" ,"Macedon","Mackay","Mallee","Melbourne", "Melbourne East","Murray East","Murraylands","New England North West" ,"North Coast NSW", "North West","Northern","Outback" ,"Outback NSW","Peninsula" ,"Phillip Island","Riverina","Riverland","Snowy Mountains","South Coast" ,"Spa Country","Sunshine Coast" ,"Sydney","The Murray" ,"Tropical North Queensland","Upper Yarra","Western Grampians","Whitsundays","Wilderness West","Wimmera","Yorke Peninsula")
 ),
 plotOutput("Graph5"),
)



server <- function(input, output, session) {
  output$Graph <- renderPlot({
    tourism %>% 
      filter(Region %in% input$Region,
             State %in% input$State,
             Purpose %in% input$Purpose) %>% 
      autoplot()
  }
  )
    output$Graph2 <- renderPlot({
      if(input$Choice == "Seasonality"){
        tourism %>%
          filter(Region %in% input$Region,
            State %in% input$State, 
                 Purpose %in% input$Purpose) %>%
          gg_subseries()
      } else 
        if (input$Choice == "Autocorrelation"){
          tourism %>% 
            filter(Region %in% input$Region,
              State %in% input$State, 
                   Purpose %in% input$Purpose) %>%
            ACF(Trips, lag_max = 4) %>%
            autoplot()
        }else 
          if(input$Choice == "Decomposition"){
            tourism %>%
              filter(Region %in% input$Region,
                State %in% input$State, 
                     Purpose %in% input$Purpose) %>%
              model(
                classical_decomposition(Trips, type = "additive")
              ) %>%
              components()%>%
              autoplot()}
  })
    
    output$Graph3 <- renderPlot({
      
      if(input$Choice1 == "Holts"){
        AusTourism <- tourism %>% 
          filter(Region %in% input$Region1,
            State %in% input$State1,
                 Purpose %in% input$Purpose1) %>%
          summarise(Trips = sum(Trips) / 1e3)
        autoplot(AusTourism, Trips) 
        
      } else
          if(input$Choice1 == "Holts/Winters"){
            AusTourism <- tourism %>% 
              filter(Region %in% input$Region1,
                State %in% input$State1,
                     Purpose %in% input$Purpose1) %>%
              summarise(Trips = sum(Trips) / 1e3)
            fit <- AusTourism %>%
              model(
                additive = ETS(Trips~ error("A") + trend("A") + season("A")),
                multiplicative = ETS(Trips ~ error("M") + trend("M") + season("M"))
              )
            Forecst <- fit %>% forecast(h = "3 years")
            Forecst %>% 
              autoplot(AusTourism, level = NULL)
      
       }
      })
    
      output$Graph4 <- renderPlot({
        if(input$Choice2 == "Naive"){
          train <- tourism %>% 
            filter(Region %in% input$Region2,
              State %in% input$State2,
                   Purpose %in% input$Purpose2)
          naivefit <- train %>% 
            model('Naive' = NAIVE(Trips))
          naivefc <- naivefit %>% forecast(h = 12)
          naivefc %>% 
            autoplot(train, level = NULL)
          
        } else
            if(input$Choice2 == "Seasonal Naive"){
              train <- tourism %>%
                filter(Region %in% input$Region2,
                  State %in% input$State2,
                       Purpose %in% input$Purpose2)
              Seasonalnaivefit <- train %>%
                model(SNAIVE(Trips ~ lag("year")))
              Seasonalnaivefc <- Seasonalnaivefit %>% forecast(h = 12)
              Seasonalnaivefc %>%
                autoplot(train, level= NULL)
              
        } else
            if(input$Choice2 == "Mean"){
                train <- tourism %>%
                  filter(Region %in% input$Region2,
                    State %in% input$State2,
                         Purpose %in% input$Purpose2)
                meanfit <- train %>% 
                  model(MEAN(Trips))
                meanfc <- meanfit %>% forecast(h = 12)
                meanfc %>%
                  autoplot(train, level=NULL)
      } else 
        if(input$Choice2 == "Drift"){
          train <- tourism %>%
            filter(Region %in% input$Region2,
              State %in% input$State2,
                   Purpose %in% input$Purpose2)
          driftfit <- train %>%
            model(RW(Trips ~ drift()))
          driftfc <- driftfit %>% forecast(h = 12)
          driftfc %>%
            autoplot(train, level=NULL)

        }})
      
      output$Graph5 <- renderPlot({
        if(input$Choice3 == "Manually Selected"){
          train1 <- tourism %>% 
            filter(Region %in% input$Region3,
                   State %in% input$State3,
                   Purpose %in% input$Purpose3)
          Arfit <- train1 %>% 
            model(arima210 = ARIMA(Trips ~ pdq(2,1,0)),
                  arima013 = ARIMA(Trips ~ pdq(0,1,3)),
                  stepwise = ARIMA(Trips),
                  search = ARIMA(Trips, stepwise=FALSE))
          arfc <- Arfit %>%
            forecast(h=12)
          arfc %>%
            autoplot(train1, level = NULL)
        } else 
          if(input$Choice3 == "Auto Selected"){
            train1 <- tourism %>% 
              filter(Region %in% input$Region3,
                     State %in% input$State3,
                     Purpose %in% input$Purpose3)
            Ar_fit <- train1 %>% 
              model(ARIMA(Trips))
            ar_fc <- Ar_fit %>%
              forecast(h=12)
            ar_fc %>%
              autoplot(train1, level = NULL)
          }
        })
      
      
      
}
shinyApp(ui, server)
