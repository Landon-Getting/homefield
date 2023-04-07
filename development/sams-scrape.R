# Introduction to Selenium using R
# https://www.youtube.com/watch?v=U1BrIPmhx10&ab_channel=SamerHijjazi

# then start a container
# docker run -d -p 4445:4444 selenium/standalone-chrome
# docker ps
# now you should see a docker instance for selenium

library(RSelenium)
library(netstat)
library(rvest)
library(xml2)
library(tidyverse)

remDr <- RSelenium::remoteDriver(remoteServerAddr = "localhost",
                                 port = 4445L,
                                 browserName = "chrome")

remDr$open()

remDr$navigate("https://www.samsclub.com/locator")

remDr$screenshot(display = TRUE)

search_box <- remDr$findElement(using = "id", "Search")
search_box$sendKeysToElement(list("50501", key = "enter"))

remDr$screenshot(display = TRUE)

whole_page <- remDr$findElement(using = "class", "sc-human-challenge-content")

# how to recognize button is there?
# how to get coordinates of button consistently
#521 by 426


remDr$mouseMoveToLocation(webElement = whole_page)
#remDr$buttondown()


size <- remDr$getWindowSize(windowId = "current")




