#path = "C:/Users/HJTom/Desktop/CSLS/shiny_project/3610048001.csv"
path2 <- file.path("data", "3610048001.csv")
df <- read.csv(path2)




# Say if we want to add territories into this list, 
# we will also need to change User Interface.
provinces <- c("Canada",
               "Newfoundland and Labrador",
               "Prince Edward Island",
               "Nova Scotia",
               "New Brunswick",
               "Quebec",
               "Ontario",
               "Manitoba",
               "Saskatchewan",
               "Alberta",
               "British Columbia")

#to figure out how many years of data have been loaded
where_is_years <- match("Industry 3", df[,1])
tot_obs <- match(provinces[3], df[,1])-
  match(provinces[2], df[,1])
start_year <- df[where_is_years+1,1]
end_year <- df[where_is_years+tot_obs,1]
Years <- (as.numeric(start_year):as.numeric(end_year))

library(plotly)
library(shiny)
library(bslib)
library(shinyTree)                  
# define a User Interface
ui <- page_sidebar(
  
 title = "Labour Productivity Dashboard",
  
 sidebar = sidebar(
   
   # this is the drop down menu
   # everything we need to do now is to bring it up to the UI.
   # its logic will be coded in the server downside
   # UI
   shinyTree(
     "tree",
     checkbox = FALSE,
   ),
   
   
   # this input will be stored in the variable name: checkGroup.
   # for example, if the user tick Canada, PEI and BC.
   # the input will be a list of characters: ("1","3","11").
   checkboxGroupInput(
     "checkGroup",
     "Select provinces",
     choices = list("Canada" = 1, 
                    "Newfoundland and Labrador" = 2, 
                    "Prince Edward Island" = 3,
                    "Nova Scotia"= 4,
                    "New Brunswick"= 5,
                    "Quebec"= 6,
                    "Ontario"= 7,
                    "Manitoba"= 8,
                    "Saskatchewan"= 9,
                    "Alberta"= 10,
                    "British Columbia"= 11
                    ),
     selected = 1
   ),
   
   sliderInput(
     inputId = "year",           
     label   = "Years:",  
     value = c(1997,2024),
     min     = 1997,
     max = 2024,   
     step    = 1,               
     ticks   = TRUE,           
     sep = ""            
   ),
   
   checkboxInput("extra","Click here to calculate growth rates"),
   
   conditionalPanel(
     condition = "input.extra == true",
     sliderInput(
       inputId = "cagr_year",           
       label   = "Calculate CAGR for the period of:",  
       value = c(1997,2024),
       min     = 1997,
       max = 2024,   
       step    = 1,               
       ticks   = TRUE,           
       sep = ""            
     ),
   ),
 ),
 
  plotlyOutput("line"),
 
 
 conditionalPanel(
   condition = "input.extra == true",
   card(
     div(
       id = "cagr_container",
       class = "cagr-table",
       style = "max-height: 280px; overflow-y: auto; overflow-x: auto; margin-top: 6px;",
       tableOutput("cagr")
     )
   )
   
 ),
 
 
 tags$style(HTML("
  .cagr-table table {
    font-size: 12px;      
  }
  .cagr-table th {
    font-size: 12px;
    font-weight: 600;
  }
  .cagr-table td {
    font-size: 12px;
  }
"))

)

library(ggplot2) 
library(tidyr)
library(ggthemes)
library(scales)

server <- function(input, output) {
  
  # display the tree
  output$tree <- renderTree({
    list(
      "All industries" = list(
        "Business sector industries" = list(
          "Goods-producing businesses" = list(
            "Agriculture, forestry, fishing and hunting" = list(
              "Crop and animal production" = list(
                "Crop production" = list(
                  "Greenhouse, nursery and floriculture production" = "",
                  "Crop production (except greenhouse, nursery and floriculture production)" = ""
                ),
                "Animal production" = list(
                  "Aquaculture" = "",
                  "Animal production (except aquaculture)" = ""
                )
              ),
              "Forestry and logging" = "",
              "Fishing, hunting and trapping" = "",
              "Support activities for agriculture and forestry" =list(
                "Support activities for forestry" = "",
                "Support activities for agriculture" = ""
              )
            ),
            "Mining and oil and gas extraction" = list(
              "Oil and gas extraction" = list(
                "Conventional oil and gas extraction" = "",
                "Non-conventional oil extraction" = ""
              ),
              "Mining and quarrying (except oil and gas)" = list(
                "Coal mining" = "",
                "Metal ore mining" = list(
                  "Iron ore mining" = "",
                  "Gold and silver ore mining" = "",
                  "Copper, nickel, lead and zinc ore mining" = "",
                  "Other metal ore mining" = ""
                ),
                "Non-metallic mineral mining and quarrying" = list(
                  "Stone mining and quarrying" = "",
                  "Sand, gravel, clay, and ceramic and refractory minerals mining and quarrying" ="",
                  "Diamond mining" = "",
                  "Potash mining" = "",
                  "Other non-metallic mineral mining and quarrying (except diamond and potash)" = ""
                )
              ),
              "Support activities for mining and oil and gas extraction" = list(
                "Support activities for oil and gas extraction" = "",
                "Support activities for mining" = ""
              )
            ),
            "Utilities" = list(
              "Electric power generation, transmission and distribution" = "",
              "Natural gas distribution, water, sewage and other systems" = list(
                "Natural gas distribution" = "",
                "Water, sewage and other systems" = ""
              )
            ),
            "Construction" = list(
              "Residential building construction" = "",
              "Non-residential building construction" = "",
              "Engineering construction" = list(
                "Transportation engineering construction" = "",
                "Oil and gas engineering construction" =  "",
                "Electric power engineering construction" = "",
                "Communication engineering construction" = "",
                "Other engineering construction" = ""
              ),
              "Repair construction" = "",
              "Other activities of the construction industry" = ""
            ),
            "Manufacturing" = list(
              "Food manufacturing" = list(
                "Animal food manufacturing" = "",
                "Sugar and confectionery product manufacturing" = "",
                "Fruit and vegetable preserving and specialty food manufacturing" = "",
                "Dairy product manufacturing" = "",
                "Meat product manufacturing" = "",
                "Seafood product preparation and packaging" = "",
                "Miscellaneous food manufacturing" = list(
                  "Grain and oilseed milling" = "",
                  "Bakeries and tortilla manufacturing" = "",
                  "Other food manufacturing" = ""
                )
              ),
              "Beverage and tobacco product manufacturing" = list(
                "Soft drink and ice manufacturing" = "",
                "Breweries" = "",
                "Wineries and distilleries" = "",
                "Tobacco manufacturing" = ""
              ),
              "Textile and textile product mills" = "",
              "Clothing and leather and allied product manufacturing" = "",
              "Wood product manufacturing" = list(
                "Sawmills and wood preservation" = "",
                "Veneer, plywood and engineered wood product manufacturing" = "",
                "Other wood product manufacturing" = ""
              ),
              "Paper manufacturing" = list(
                "Pulp, paper and paperboard mills" = "",
                "Converted paper product manufacturing" = ""
              ),
              "Printing and related support activities" = "",
              "Petroleum and coal product manufacturing" = list(
                "Petroleum refineries" = "",
                "Petroleum and coal products manufacturing (except petroleum refineries)" = ""
              ),
              "Chemical manufacturing" = list(
                "Basic chemical manufacturing" = "",
                "Resin, synthetic rubber, and artificial and synthetic fibres and filaments manufacturing" = "",
                "Pesticide, fertilizer and other agricultural chemical manufacturing" = "",
                "Miscellaneous chemical product manufacturing" = list(
                  "Paint, coating and adhesive manufacturing" = "",
                  "Soap, cleaning compound and toilet preparation manufacturing" = "",
                  "Other chemical product manufacturing" = ""
                )
              ),
              "Plastics and rubber products manufacturing" = list(
                "Plastic product manufacturing" = "",
                "Rubber product manufacturing" = ""
              ),
              "Non-metallic mineral product manufacturing" = list(
                "Cement and concrete product manufacturing" = "",
                "Non-metallic mineral product manufacturing (except cement and concrete products)" = ""
              ),
              "Primary metal manufacturing" = list(
                "Iron and steel mills and ferro-alloy manufacturing" = "",
                "Steel product manufacturing from purchased steel" = "",
                "Alumina and aluminum production and processing" = "",
                "Non-ferrous metal (except aluminum) production and processing" = "",
                "Foundries" = ""
              ),
              "Fabricated metal product manufacturing" = list(
                "Forging and stamping" = "",
                "Architectural and structural metals manufacturing" = ""
              ),
              "Machinery manufacturing" = list(
                "Agricultural, construction and mining machinery manufacturing" = "",
                "Industrial, commercial and service industry machinery manufacturing" = list(
                  "Industrial machinery manufacturing" = "",
                  "Commercial and service industry machinery manufacturing" = ""
                ),
                "Ventilation, heating, air-conditioning and commercial refrigeration equipment manufacturing" = "",
                "Metalworking machinery manufacturing" = "",
                "Engine, turbine and power transmission equipment manufacturing" = "",
                "Other general-purpose machinery manufacturing" = ""
              ),
              "Computer and electronic product manufacturing" = list(
                "Computer and peripheral equipment manufacturing" = "",
                "Communications equipment manufacturing" = "",
                "Semiconductor and other electronic component manufacturing" = "",
                "Other electronic product manufacturing" = ""
              ),
              "Electrical equipment, appliance and component manufacturing" = list(
                "Electric lighting equipment manufacturing" = "",
                "Household appliance manufacturing" = "",
                "Electrical equipment manufacturing" = "",
                "Other electrical equipment and component manufacturing" = ""
              ),
              "Transportation equipment manufacturing" = list(
                "Motor vehicle manufacturing" = list(
                  "Automobile and light-duty motor vehicle manufacturing" = "",
                  "Heavy-duty truck manufacturing" = ""
                ),
                "Motor vehicle body and trailer manufacturing" = "",
                "Motor vehicle parts manufacturing" = list(
                  "Motor vehicle gasoline engine and engine parts manufacturing" = "",
                  "Motor vehicle electrical and electronic equipment manufacturing" = "",
                  "Motor vehicle steering and suspension components (except spring) manufacturing" = "",
                  "Motor vehicle brake system manufacturing" = "",
                  "Motor vehicle transmission and power train parts manufacturing" = "",
                  "Motor vehicle seating and interior trim manufacturing" = "",
                  "Motor vehicle metal stamping" = "",
                  "Other motor vehicle parts manufacturing" = ""
                )
              ),
              "Furniture and related product manufacturing" = list(
                "Household and institutional furniture and kitchen cabinet manufacturing" = "",
                "Office furniture (including fixtures) manufacturing" = "",
                "Other furniture-related product manufacturing" = ""
              ),
              "Miscellaneous manufacturing" = list(
                "Medical equipment and supplies manufacturing" = "",
                "Other miscellaneous manufacturing" = ""
              )
            )
          ),
          "Service-producing businesses" = list(
            "Wholesale trade" = list(
              "Farm product wholesaler-distributors" = "",
              "Petroleum product wholesaler-distributors" = "",
              "Food, beverage and tobacco wholesaler-distributors" = "",
              "Personal and household goods wholesaler-distributors" = "",
              "Motor vehicle and parts wholesaler-distributors" = "",
              "Building material and supplies wholesaler-distributors" = "",
              "Machinery, equipment and supplies wholesaler-distributors" = "",
              "Miscellaneous wholesaler-distributors" = "",
              "Wholesale electronic markets, and agents and brokers" = ""
            ),
            "Retail trade" = list(
              "Motor vehicle and parts dealer" = "",
              "Furniture and home furnishings stores" = "",
              "Electronics and appliance stores" = "",
              "Building material and garden equipment and supplies dealers" = "",
              "Food and beverage stores" = "",
              "Health and personal care stores" = "",
              "Gasoline stations" = "",
              "Clothing and clothing accessories stores" = "",
              "Sporting goods, hobby, book and music stores" = "",
              "General merchandise stores" = "",
              "Miscellaneous store retailers" = "",
              "Non-store retailers" = ""
            ),
            "Transportation and warehousing" = list(
              "Air transportation" = "",
              "Rail transportation" = "",
              "Water transportation" = "",
              "Truck transportation" = "",
              "Transit, ground passenger and scenic and sightseeing transportation" = list(
                "Urban transit systems" = "",
                "Taxi and limousine service" = "",
                "Other transit and ground passenger transportation and scenic and sightseeing transportation" = ""
              ),
              "Support activities for transportation" = "",
              "Pipeline transportation" = list(
                "Pipeline transportation of natural gas" = "",
                "Crude oil and other pipeline transportation" = ""
              ),
              "Postal service and couriers and messengers" = list(
                "Postal service" = "",
                "Couriers and messengers" = ""
              ),
              "Warehousing and storage" = ""
            ),
            "Information and cultural industries"= list(
              "Publishing industries (except internet)" = list(
                "Newspaper publishers" = "",
                "Periodical, book and directory publishers" = "",
                "Software publishers" = ""
              ),
              "Motion picture and sound recording industries"=list(
                "Motion picture and video exhibition" = "",
                "Motion picture and video industries (except exhibition)" = "",
                "Sound recording industries" = ""
              ),
              "Broadcasting (except internet)"= list(
                "Radio and television broadcasting" = "",
                "Pay and specialty television" = ""
              ),
              "Telecommunications" = "",
              "Data processing, hosting, and related services"="",
              "Other information services" = ""
            ),
            "Finance and insurance" = list(
              "Depository credit intermediation and monetary authorities" = list(
                "Monetary authorities - central bank" = "",
                "Local credit unions" = "",
                "Banking and other depository credit intermediation" = ""
              ),
              "Non-depository credit intermediation"="",
              "Activities related to credit intermediation"= "",
              "Insurance carriers and related activities" = list(
                "Insurance carriers" = "",
                "Agencies, brokerages and other insurance related activities" = ""
              ),
              "Financial investment services, funds and other financial vehicles" = ""
            ),
            "Real estate, rental and leasing" = list(
              "Real estate" = list(
                "Lessors of real estate" = "",
                "Offices of real estate agents and brokers and activities related to real estate" = ""
              ),
              "Rental and leasing services"=list(
                "Automotive equipment rental and leasing" = "",
                "Rental and leasing services (except automotive equipment)" = ""
              ),
              "Lessors of non-financial intangible assets" = ""
            ),
            "Professional, scientific and technical services" = list(
              "Legal, accounting and related services" = list(
                "Legal services" = "",
                "Accounting, tax preparation, bookkeeping and payroll services" = ""
              ),
              "Architectural, engineering and related services"="",
              "Other professional, scientific and technical services"= list(
                "Specialized design services" = "",
                "Management, scientific and technical consulting services" = "",
                "Scientific research and development services" = "",
                "Other professional, scientific and technical services" = ""
              ),
              "Computer systems design and related services" = "",
              "Advertising, public relations, and related services" = ""
            ),
            "Holding companies" = "",
            "Administrative and support, waste management and remediation services" = list(
              "Administrative and support services" = list(
                "Office administrative services" = "",
                "Employment services" = "",
                "Business support services" = "",
                "Travel arrangement and reservation services" = "",
                "Investigation and security services" = "",
                "Services to buildings and dwellings" = "",
                "Facilities and other support services" = ""
              ),
              "Waste management and remediation services" = ""
            ),
            "Educational services" = list(
              "Private educational services" = ""
            ),
            "Health care and social assistance" = list(
              "Health care" = list(
                "Ambulatory health care services" = list(
                  "Offices of physicians" = "",
                  "Offices of dentists" = "",
                  "Miscellaneous ambulatory health care services" = ""
                ),
                "Nursing and residential care facilities" = ""
              ),
              "Social assistance" = ""
            ),
            "Arts, entertainment and recreation" = list(
              "Performing arts, spectator sports and related industries, and heritage institutions" = "",
              "Amusement, gambling and recreation industries" = list(
                "Gambling industries" = "",
                "Amusement and recreation industries" = ""
              )
            ),
            "Accommodation and food services" = list(
              "Accommodation services" = list(
                "Traveller accommodation" = "",
                "RV (recreational vehicle) parks, recreational camps, and rooming and boarding houses" = ""
              ),
              "Food services and drinking places" = ""
            ),
            "Other private services" = list(
              "Repair and maintenance" = list(
                "Automotive repair and maintenance" = "",
                "Repair and maintenance (except automotive)" = ""
              ),
              "Personal services and private households" = list(
                "Personal and laundry services" = list(
                  "Funeral services" = "",
                  "Dry cleaning and laundry services" = "",
                  "Personal care services and other personal services" = ""
                ),
                "Private households" = ""
              ),
              "Professional and similar organizations" = ""
            )
          )
        ),
        "Business sector industries, excluding farms" = "",
        "Business sector industries, excluding private households" = "",
        "Service-producing businesses, excluding private households" = "",
        "Industrial production" = "",
        "Non-durable manufacturing industries" = "",
        "Durable manufacturing industries" = "",
        "Energy sector" = "",
        "Information and communication sector"= list(
          "Information and communication technology, manufacturing" = "",
          "Information and communication technology, services" = ""
        ),
        "Finance and insurance, and holding companies" = "",
        "Non-business sector industries" = list(
          "Non-profit institutions serving households" = list(
            "Non-profit institutions" = list(
              "Non-profit education institutions" = "",
              "Ambulatory health care services" = "",
              "Non-profit welfare organizations" = "",
              "Non-profit arts, entertainment and recreation" = "",
              "Grant-making, civic, and professional and similar organizations" = "",
              "Religious organizations" = "",
              "Other non-profit institutions serving households" =""
            )
          ),
          "Government sector" = list(
            "Government educational services" = list(
              "Elementary and secondary schools" = "",
              "Community colleges and C.E.G.E.P.s" = "",
              "Universities" = "",
              "Other educational services" = ""
            ),
            "Government health services" = list(
              "Hospitals"= "",
              "Nursing and residential care facilities" = ""
            ),
            "Federal government services" = list(
              "Federal government services (excluding defence)" = "",
              "Defence services"=""
            ),
            "Provincial and territorial government services" = "",
            "Local, municipal and Indigenous government services" = list(
              "Municipal government services" = "",
              "Indigenous government services" = ""
            )
          )
        )
      )
      
    )
  })
  
  

  
  
  #determine the location of the sector selected in the csv file
  sector_selected <-reactive({
    req(input$tree)
    sel <- get_selected(input$tree, format = "names")
    if (is.null(sel) || length(sel) == 0){
      search_for <- "All industries"
    } else{
      extract_last <- function(x) {
        if (is.list(x) && length(x) > 0) extract_last(x[[1]]) else as.character(x)
      }
      search_for <- extract_last(sel)
      }
    
    search_for
  })
  
  ## function df_2plot is to generate a cleaned data frame
  ## to plot the chart requested by user
  df_2plot <- reactive({
    
    #call in inputs
    req(input$tree,input$year, input$checkGroup)

    #search for the the sector selected
    #determine where is this sector in the csv file
    search_for <- sector_selected()
    for (i in 1:nrow(df)) {
      if (startsWith(df[i, 1], search_for)) {
        r_num <- i
        break
      }
    }
    
    #determine where the data is for selected provinces 
    horizon <- input$year[1]:input$year[2]
    num_obs <- length(horizon)
    checks <- as.numeric(input$checkGroup)
    loc <- (checks-1)*tot_obs
    
    #determine where the data is for selected years
    start <- input$year[1]-Years[1]
    shift <- loc + start
    
    ##get the data we wanted from the csv
    out <- data.frame(matrix(nrow=num_obs, ncol=0))
    out$Years <- horizon
    for (i in 1:length(shift)){
      t0 <- r_num+shift[i]+1
      t1 <- r_num+shift[i]+num_obs
      out[[provinces[checks[i]]]] <- as.numeric(df[t0:t1,1])
    }

    ##return what we find
    out
  })
  
  ##plot the chart
  output$line <- renderPlotly({
    d <- df_2plot()
    df_long <- pivot_longer(d,cols=-Years, names_to = "Province", values_to = "Productivity")
    tit <- paste0("Labor Productivity in ", sector_selected())
    p <-ggplot(df_long, aes(x = Years, y = Productivity, color = Province)) +
        geom_point(size = 1.25)+
        geom_line(linewidth = 1, linetype = "dashed") +   
        labs(
          color = "Province",
          title = tit,
          subtitle = "Output per hour worked, chained 2017 Canadian dollars",
          x = NULL, y = NULL,
          caption = "Source: Statistics Canada Table 36-10-0480-01"
        )+
        scale_color_stata()+
        theme_bw()+
        theme(
          plot.title = element_text(size = 18, face = "bold"),
          plot.subtitle = element_text(size = 14),
          axis.text.x  = element_text(size = 12),
          axis.text.y  = element_text(size = 12),
          legend.title = element_text(size = 12),
          legend.text  = element_text(size = 10),
          plot.caption = element_text(hjust = 0, size = 10) 
        )
    
    if ("Canada" %in% df_long$Province) {
      canada_data <- filter(df_long, Province == "Canada")
      if (nrow(canada_data) > 0) {
        p <- p + geom_line(data = canada_data,
                           linewidth = 1.25,
                           linetype  = "solid",
                           na.rm     = TRUE)
      }
    }
    ggplotly(p)
  })
  

  # same logic as df_2plot
  # but return a data_frame of annual growth rate
  df_2table <- reactive({
    
    #call in inputs
    req(input$cagr_year, input$checkGroup)
    
    #search for the the sector selected
    #determine where is this sector in the csv file
    search_for <- sector_selected()
    for (i in 1:nrow(df)) {
      if (startsWith(df[i, 1], search_for)) {
        r_num <- i
        break
      }
    }
    
    #determine the location of data for selected provinces 
    horizon <- input$cagr_year[1]:input$cagr_year[2]
    num_obs <- length(horizon)
    checks <- as.numeric(input$checkGroup)
    loc <- (checks-1)*tot_obs
    
    #determine the location of data for selected years
    start <- input$cagr_year[1]-Years[1]
    shift <- loc + start
    
    ##get the data wanted from the csv
    out <- data.frame(matrix(nrow=1, ncol=0))
    for (i in 1:length(shift)){
      t0 <- r_num+shift[i]+1
      t1 <- r_num+shift[i]+num_obs
      temp <- as.numeric(df[t1,1])/as.numeric(df[t0,1])
      temp <- temp^(1/(input$cagr_year[2]-input$cagr_year[1]))-1
      out[[provinces[checks[i]]]] <- percent(temp,accuracy=0.01)
      
    }
    row.names(out) <- paste0("CAGR, ", input$cagr_year[1], "–", input$cagr_year[2])

    ##return what we find
    out
  })

  
  
  # display the table
  # with some instruction of the size of table so that it does not occupy the whole page
  output$cagr <- renderTable({
    df_2table() 
    }, 
    rownames = TRUE,
    align = rep("c", ncol(df)),
    options = list(
      pageLength = 5,      
      scrollX = TRUE        
    )
    )
}

shinyApp(ui, server)

#library(rsconnect)

#connect to the site
#rsconnect::setAccountInfo(name='tomzhu',
#                          +                           token='3A10C1ED9ABB83E4B63C1776EE6105CE',
#                          +                           secret='<SECRET>')
# upload 
#  rsconnect::deployApp(appDir = "C:/Users/HJTom/Desktop/CSLS/shiny_project/app",appMode = "shiny",appName = "csls-provincial-dashboard",   appPrimaryDoc="CompareProvinces.R")



