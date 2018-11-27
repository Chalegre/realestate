# realestate
#Project to apply the knowledge acquired in R for a "real" example

#Packages needed to run
install.packages("robotstxt")
library(robotstxt)
library(jsonlite)
library(tidyverse)
library(rvest)
library("xml2")
library("httr")
library("RSelenium")

#Check installed pacakages
installed.packages()

#Real Estate Begin

##Check if robots are allowed
address <- "https://www.point2homes.com/"
rob <- robotstxt(address)
rob$permissions
robotstxt::get_robotstxt(address)
robotstxt::paths_allowed(address)

##Accessing the webpage
library(RSelenium)
robotstxt::paths_allowed()

remDr <- remoteDriver(remoteServerAddr = "127.0.1.1",
                      port = 4445L,
                      browser = "chrome" )

remDr$open(silent = TRUE)
remDr$navigate(address)

#Find, highlight and click search box
webElem <- remDr$findElement("xpath", '//input[starts-with(@class, "location-field")]')
Sys.sleep(3)
webElem$highlightElement()
webElem$clickElement()

#Write the search and hit enter
#webElem$sendKeysToElement(list("Calgary"))
Sys.sleep(3)
webElem$sendKeysToElement(list(key = "enter"))

#Funtion to move to web element
scrollTo <- function(remDr, webElem){
  remDr$executeScript("arguments[0].scrollIntoView(true);", args = list(webElem))
  webElem$highlightElement()
}

#Define the page button in at the end of the page as an element
webElem <- remDr$findElement('css', '.curr')

#Navigate to the part where the page button is
Sys.sleep(2)
scrollTo(remDr, webElem)

#Done with Selenium. Now use *rvest* to collect data and *stringr* to massage it 
library(rvest)
#install.packages("stringr")
library(stringr)
pg <- remDr$getPageSource() %>% .[[1]] %>% read_html()

#Function to get the page, then get me all the nodes, then turn into a text
collect_realEstate <- function(pg){
  #This pipe gets the address. *Data Massage needed*
  address <- pg %>% html_nodes(".address-container") %>% 
    html_text()
  
  #Add a line in case of error: "Column must be lenght 1 or ..., not ..."
  #farms <- c(farms, NA)
  
  #This pipe gets the price.
  price <- pg %>% html_nodes(".price") %>% 
    html_text() %>% gsub("[^0-9]", "",.) %>% as.numeric
  
  #This pipe gets the characteristics. *Data Massage needed*
  characteristics <- pg %>% html_nodes(".characteristics-cnt") %>% 
    html_text() 
  
 #This pipe gets the agency. Need a fix issue with column lenght using a function
  #agency <- pg %>% html_nodes(".agent-company") %>% 
    #html_text() %>% str_replace_all("\n", "") %>% str_trim()
  
  #Add a line in case of error: "Column must be lenght 1 or ..., not ..."
  #agency <- c(agency, NA)
  
  tibble(address, price, characteristics)
}

#Use the function to get the first page
pg_realestate001 <- collect_realEstate(pg)

#Define the page 2 button at the end of the page as an element
webElem <- remDr$findElement("xpath", '//a[starts-with(@title, "Calgary listings page 2")]')
#Navigate to the part where the page button is
Sys.sleep(2)
scrollTo(remDr, webElem)
webElem$highlightElement()
webElem$clickElement()

#page pops up "New listings notification" 
webElem <- remDr$findElement("xpath", '//a[starts-with(@id, "xButton")]')
#Navigate to the part where the page button is
Sys.sleep(3)
#scrollTo(remDr, webElem)
webElem$highlightElement()
webElem$highlightElement()
webElem$clickElement()

#Get page 2 data
pg002 <- remDr$getPageSource() %>% .[[1]] %>% read_html()

#Use the function to get the second page
pg_realestate002 <- collect_realEstate(pg002)

#Next: Turn this into a function :(

#####################################Deprecated: GoogleMaps now requires users to register for and use an api key. There are an update on ggmap is dev available
install.packages("ggmap")
library(ggmap)

#Calgary coordinates
calgary_coord <- c(long = -114.062019, lat = 51.044270)

#Get map
calgary_map <- get_map(location = calgary_coord, zoom = 11, scale = 2)

#Plot map
ggmap(calgary_map)
#####################################Deprecated

#End of Real Estate
