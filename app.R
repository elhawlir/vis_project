
options(warn = -1)

library(dplyr)
library(networkD3)
library(tidyr)
library(shiny)

# read in the data for countries we're interested in
au <- read.csv("aus_challenges.csv") #Australia
ca <- read.csv("ca_challenges.csv") #Canada
us <- read.csv("us_challenges.csv") #America
uk <- read.csv("uk_challenges.csv") #England

# narrowing down df to relevant data for viz
au_challenges <- select(au, c(2, 8, 12))
ca_challenges <- select(ca, c(2, 8, 12))
us_challenges <- select(us, c(2, 8, 12))
uk_challenges <- select(uk, c(2, 8, 12))

# changing column names to make df easier to manipulate
colnames(au_challenges) <- c("Country", "Row.item.label", "Value")
colnames(ca_challenges) <- c("Country", "Row.item.label", "Value")
colnames(us_challenges) <- c("Country", "Row.item.label", "Value")
colnames(uk_challenges) <- c("Country", "Row.item.label", "Value")

# replacing 'NA' for 0
au_challenges$Value[is.na(au_challenges$Value)] <- 0
ca_challenges$Value[is.na(ca_challenges$Value)] <- 0
us_challenges$Value[is.na(us_challenges$Value)] <- 0
uk_challenges$Value[is.na(uk_challenges$Value)] <- 0

# combining the data frames together
semi_merge <- rbind.data.frame(au_challenges, ca_challenges)
semi_merge <- rbind.data.frame(semi_merge, us_challenges)
results <- rbind.data.frame(semi_merge, uk_challenges)

# group dataframe by country and challenge
# preps dataframe for sankey diagram format
chal <- results %>% 
  group_by(Country, Row.item.label) %>%
  summarise(Value = sum(Value))

# create all nations nodes df
country <- unique(as.character(chal$Country))
challenges <- unique(as.character(chal$Row.item.label))
nodes <- data.frame(node = c(0:17),
                    name = c(country, challenges))

# australian nodes df
country_au <- unique(as.character(au_challenges$Country))
challenges_au <- unique(as.character(au_challenges$Row.item.label))
nodes_au <- data.frame(node = c(0:14),
                       name = c(country_au, challenges_au))

# australian links df
aus_chal <- au_challenges %>%
  left_join(nodes_au, by = c("Country" = "name")) %>%
  rename(ctry_id = node) %>%
  left_join(nodes_au, by = c("Row.item.label" = "name")) %>%
  rename(chng_id = node)

# australian sankey
sankey_challengeau <- sankeyNetwork(Links = aus_chal, Nodes = nodes_au, Source = "ctry_id", Target = "chng_id", Value = "Value", NodeID = "name", fontSize = 14, nodeWidth = 30)

# us nodes df
country_us <- unique(as.character(us_challenges$Country))
challenges_us <- unique(as.character(us_challenges$Row.item.label))
nodes_us <- data.frame(node = c(0:14),
                       name = c(country_us, challenges_us))

# us links df
us_chal <- us_challenges %>%
  left_join(nodes_us, by = c("Country" = "name")) %>%
  rename(ctry_id = node) %>%
  left_join(nodes_us, by = c("Row.item.label" = "name")) %>%
  rename(chng_id = node)

# us sankey
sankey_challengeus <- sankeyNetwork(Links = us_chal, Nodes = nodes_us, Source = "ctry_id", Target = "chng_id", Value = "Value", NodeID = "name", fontSize = 14, nodeWidth = 30)

# canadian nodes df
country_ca <- unique(as.character(ca_challenges$Country))
challenges_ca <- unique(as.character(ca_challenges$Row.item.label))
nodes_ca <- data.frame(node = c(0:14),
                       name = c(country_ca, challenges_ca))

# canadian links df
ca_chal <- ca_challenges %>%
  left_join(nodes_ca, by = c("Country" = "name")) %>%
  rename(ctry_id = node) %>%
  left_join(nodes_ca, by = c("Row.item.label" = "name")) %>%
  rename(chng_id = node)

# canadian sankey
sankey_challengeca <- sankeyNetwork(Links = ca_chal, Nodes = nodes_ca, Source = "ctry_id", Target = "chng_id", Value = "Value", NodeID = "name", fontSize = 14, nodeWidth = 30)

# uk nodes df
country_uk <- unique(as.character(uk_challenges$Country))
challenges_uk <- unique(as.character(uk_challenges$Row.item.label))
nodes_uk <- data.frame(node = c(0:14),
                       name = c(country_uk, challenges_uk))

# uk links df
uk_chal <- uk_challenges %>%
  left_join(nodes_uk, by = c("Country" = "name")) %>%
  rename(ctry_id = node) %>%
  left_join(nodes_uk, by = c("Row.item.label" = "name")) %>%
  rename(chng_id = node)

# uk sankey
sankey_challengeuk <- sankeyNetwork(Links = uk_chal, Nodes = nodes_uk, Source = "ctry_id", Target = "chng_id", Value = "Value", NodeID = "name", fontSize = 14, nodeWidth = 30)

# create all nations links df
chal1 <- chal %>%
  left_join(nodes, by = c("Country" = "name")) %>%
  rename(ctry_id = node) %>%
  left_join(nodes, by = c("Row.item.label" = "name")) %>%
  rename(chng_id = node)


# draw sankey diagram
sankey_challenge <- sankeyNetwork(Links = chal1, Nodes = nodes,
                                  Source = "ctry_id", Target = "chng_id",
                                  Value = "Value", NodeID = "name",
                                  fontSize = 14, nodeWidth = 30)

# make sankey diagram a web app using Shiny

ui <- fluidPage(
  
  # Application title
  titlePanel("SME Challenges in Australia, Canada, US and UK"),
  
  inputPanel(
    selectInput("country", "Country",c("AU", "CA", "US", "UK", "All Nations"))
  ),
  
  #dimensions of the sankey in the browser
  sankeyNetworkOutput("plot", width ="1000px", height = "650px"))

server <- function(input, output, session) {
  #rendering sankey to the server
  output$plot = renderSankeyNetwork({
    if (input$country == "AU") {
      print(sankey_challengeau)
    } else if (input$country == "CA") {
      print(sankey_challengeca)
    } else if (input$country == "US") {
      print(sankey_challengeus)
    } else if (input$country == "UK") {
      print(sankey_challengeuk)
    } else if (input$country == "All Nations") {
      print(sankey_challenge)
    }
    
  })
}

shinyApp(ui, server)

