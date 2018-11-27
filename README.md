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

#Start of mess
library("xml2")
library("httr")
library("RSelenium")
library("rvest")

installed.packages()

address <- "https://www.remax.ca/"
webpage <- read_html(address)
rank_data_html <- html_nodes(webpage, css = ".price")
rank_data <- html_text(rank_data_html)
head(rank_data)

#Lego Movie
library(rvest)
#> Loading required package: xml2
html <- read_html("http://www.imdb.com/title/tt1490017/")
cast <- html_nodes(html, css = ".summary_text")
cast_text <- html_text(cast)
as.character(cast_text)

odd <- html_nodes(html, css = "#titleCast td:nth-child(2)")
odd_text <- html_text(odd)
odd_text
length(odd_text)

#Realtor
address <- "https://www.realtor.ca/"
rm <- remoteDriver$new(port = 4445L)
rm$open(silent = FALSE)
rm$show()
rm$screenshot()
rm$navigate(address)
#rm$getActiveElement("price")
rm$getTitle()
#rm$getClass()
remDr$screenshot(display = TRUE)
rm$close()

#VNC Test
remDr <- remoteDriver(port = 4445L)
remDr$open(silent = TRUE)
remDr$navigate("http://www.google.com/ncr")
webElem <- remDr$findElement(using = "css", "[name = 'q']")
webElem$sendKeysToElement(list("real estate in calgary"))
webElem$sendKeysToElement(list(key = "enter"))
remDr$close()



#RSelenium
remDr <- remoteDriver(port = 4445L)
remDr$open(silent = TRUE)
remDr$navigate("http://www.google.com/ncr")
webElem <- remDr$findElement(using = "css", "[name = 'q']")
webElem$sendKeysToElement(list("R Cran", "\uE007")) #"\uE007" Enter
page <- xml2::read_html(remDr$getPageSource()[[1]])

rvest::html_nodes(page, css = ".LC20lb")

%>% rvest::html_nodes("LC20lb")
rvest::html_nodes("#ddlCountry") %>%
  rvest::html_children() %>%
  rvest::html_text() %>%
  dplyr::data_frame(country_name = .)


webElems <- remDr$findElement(using = 'css selector', "h3.LC20lb")

resHeaders <- unlist(lapply(webElems, function(x){x$getElementText()}))
resHeaders



#Trial 3
library(RSelenium)
library(rvest)
library("xml2")
library(tidyverse)
library(dplyr)


remDr <- remoteDriver(port = 4445L)
remDr$open(silent = TRUE)
remDr$navigate("http://apps.who.int/bloodproducts/snakeantivenoms/database/SearchFrm.aspx")

#Creating A List Of Countries
xml2::read_html(remDr$getPageSource()[[1]])

snake_countries <- xml2::read_html(remDr$getPageSource()[[1]]) %>%
  rvest::html_nodes("#ddlCountry") %>%
  rvest::html_children() %>%
  rvest::html_text() %>%
  dplyr::data_frame(country_name = .)

snake_countries <- snake_countries %>%
  dplyr::mutate(list_position = 1:160,
                x = stringr::str_c("#ddlCountry > option:nth-child(",list_position, ")"))

# We chop off our first one as we are never going to navigate to there
snake_countries <- snake_countries[-1,]


#Navigating to a specific country
element<- remDr$findElement(using = 'css selector', "#ddlCountry > option:nth-child(65)")
element$clickElement()

#Printing
html <- xml2::read_html(remDr$getPageSource()[[1]])
xml2::write_html(html, "india.html")


#End of mess



#Where I learn how to use RSelenium and Rvest
# https://www.youtube.com/watch?v=OxbvFiYxEzI
# https://www.youtube.com/watch?v=JcIeWiljQG4

#Packages needed to run
install.packages("robotstxt")
library(robotstxt)
library(jsonlite)
library(tidyverse)
library(rvest)

rob <- robotstxt("www.google.com")
rob$permissions
rob$crawl_delay
get_robotstxt("www.google.com")
paths_allowed("www.google.com")
Sys.sleep(runif(1,2,3))

######################################################
#Function to know your IP address
get_ip <- function(){
  read_html("https://api.ipify.org?format=json") %>%
    html_text(., trim = TRUE) %>%
    jsonlite::fromJSON(.)
}

get_ip()
######################################################
ad1 <- "http://ask.com"
ask <- read_html(ad1)
ask %>% html_nodes("div")

ask %>% html_node(".search-bar")
ask %>% html_nodes(".PartialHome-wrapper-logo")
######################################################

pg <- read_html("https://en.wikipedia.org/wiki/Rugby_World_Cup")
pg %>% html_text()
rugby<- pg %>% html_table(., fill = TRUE)
rugby_table <- rugby[[3]]
str(rugby_tb)

######################################################
pg <- read_html("https://www.ecstasydata.org/index.php?sort=DatePublishedU+desc&start=0&search_field=-&m1=-1&m2=-1&datefield=tested&max=200&field_test=1")

#Extract the table
pg_table <- pg %>% html_table(., fill = TRUE) %>% .[[2]]

# Notice that the  names are too long. It also contains the descriptions
names(pg_table) %>% head

name_table <- funtion(x){
  names(x) <- c("Photo", "Name", "Substance", "Amounts",
                "Date_published", "Date_tested", "Location",
                "Sample_size", "Data_source")
  x %>% tbl_df
}

pg_table <- pg_table %<% 
  name_table %>%
  tbl_df %>% slice(-1)

######################################################
#remDr <- remoteDriver(port = 4445L)
remDr <- remoteDriver(remoteServerAddr = "127.0.1.1", port = 4445L, browser = "chrome" )
remDr$open(silent = TRUE)
remDr$navigate("http://www.google.com")
remDr$navigate("http://www.bing.com")
remDr$goBack()
remDr$goForward()


remDr

class(remDr)

remDr$findElements

remDr$navigate("https://www.fbi.gov")
remDr$sendKeysToActiveElement

#Get a list with all the names under selKeys
RSelenium:::selKeys %>% names()

remDr$sendKeysToActiveElement(list(key = "home"))
remDr$sendKeysToActiveElement(list(key = "end"))
remDr$sendKeysToActiveElement(list(key = "page_down"))

#Using java scripts
remDr$sendKeysToActiveElement(list(key = "page_down"))
remDr$executeScript("return window.scrollY" , args = list (1))

remDr$sendKeysToActiveElement(list(key = "end"))
remDr$executeScript("return window.scrollY" , args = list (1)) #you can use conditionals to know whne you hit the end

remDr$executeScript("return document.body.scrollHeight" , args = list(1))

remDr$executeScript("return window.innerHeight", args = list(1))
remDr$executeScript("return window.innerWidth", args = list(1))

#Interacting with the DOM
remDr$getPageSource() %>% .[[1]] %>% read_html()

get_html <- function(remDr){
  remDr$getPageSource() %>%
    .[[1]] %>% 
    read_html()
}

remDr %>% get_html(.)

remDr$navigate("https://www.google.com")
webElem <- remDr$findElement(using = 'class', 'gsfi') #find the question box
Sys.sleep(3)
webElem$highlightElement()

webElem$clickElement()
Sys.sleep(3)
remDr$mouseMoveToLocation(webElement = webElem)
remDr$executeScript("arguments[0].scrollIntoView(true);", args = list(webElem))

webElem$sendKeysToActiveElement(list('world cup 2018', key = 'enter'))

##Nice to have funtions
remDr$maxWindowSize()
remDr$getTitle()
remDr$screenshot(display = TRUE)

##Funtion for navigation
navi <- function(remDr, site = "https://www.google.com"){
  remDr$navigate(site)
}

remDr %>% navi(., "https://realtor.ca")

#Vivino

##Check if robots are allowed
rob <- robotstxt("https://www.vivino.com/")
rob$permissions
robotstxt::get_robotstxt("https://www.vivino.com/")
robotstxt::paths_allowed("https://www.vivino.com/")

##Accessing the webpage
library(RSelenium)
robotstxt::paths_allowed()

remDr <- remoteDriver(remoteServerAddr = "127.0.1.1",
                      port = 4445L,
                      browser = "chrome" )

remDr$open(silent = TRUE)
remDr$navigate("https://www.vivino.com/explore")

#Find, highlight and click element
#webElem <- remDr$findElement('css', '.explore-widget__main__submit__button')
#webElem$highlightElement()
#webElem$clickElement()

#Funtion to move to web element
scrollTo <- function(remDr, webElem){
  remDr$executeScript("arguments[0].scrollIntoView(true);", args = list(webElem))
  webElem$highlightElement()
}

#Define the search boxes as an element
webElem <- remDr$findElements("xpath", '//input[starts-with(@class, "filterPills")]')

#Navigate to a specific search box (the numbers define which one). Click and input info.
Sys.sleep(2)
scrollTo(remDr, webElem[[3]])
webElem[[3]]$clickElement()
webElem[[3]]$sendKeysToActiveElement(list("Australia"))

#Gather the pills where the Australia element is in
webElem <- remDr$findElements("css", ".pill__inner--2uty5")

##Function to find the exactly location of the Australia. 
country_elem <- webElem %>% sapply(.,function(x) x$getElementText()) %>% 
  reduce(c) %>% 
  grepl("Australia", .) %>% 
  which()

##Scroll to the pill Australia and click on it
Sys.sleep(5)
scrollTo(remDr, webElem[[country_elem]])

webElem <- webElem[[country_elem]]

Sys.sleep(5)
webElem$clickElement()

##Coming back to the first position of the page
remDr$sendKeysToActiveElement(list(key = "home"))

#Go to the end and load more items
##Calculate the number of pixels of the page. You know how bi your document page is.
remDr$executeScript("return document.body.scrollHeight", args = list(1))

##Go to the end of the page
remDr$sendKeysToActiveElement(list(key = "end"))
#remDr$executeScript("return window.scrollY", args = list(1))

#Done with Selenium. Now use rvest to collect data
pg <- remDr$getPageSource() %>% .[[1]] %>% read_html()

#Get the page, then get me all the nodes, then turn into a text

collect_wine <- function(pg){
  farms <- pg %>% html_nodes(".vintageTitle__winery--2YoIr") %>% 
    html_text()

  #Adiconar uma linha caso de o erro "Column must be lenght 1 or 75, not 74
  #farms <- c(farms, NA)
  
  wine <- pg %>% html_nodes(".vintageTitle__wine--U7t9G") %>% 
    html_text()
  
  rating <- pg %>% html_nodes(".vivinoRating__rating--4Oti3") %>% 
    html_text() %>% as.numeric()
  
  rating_count <- pg %>% html_nodes(".vivinoRating__ratingCount--NmiVg") %>% 
    html_text() %>% gsub("[^0-9]", "",.) %>% as.numeric
  
  tibble(farms, wine, rating, rating_count)
}


#Use the function
pg_wines <- collect_wine(pg)
