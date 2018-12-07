##############################################################
# Scrape Ours to Shape 
# Using RSelenium 
# Michele Claibourn
# Acquire data: through December 7, 2018
# 1. Scrape comments: RSelenium approach
# 2. Format comments for later analysis
# 3. Save the data
##############################################################
# potentially useful tutorial: https://callumgwtaylor.github.io/blog/2018/02/01/using-rselenium-and-docker-to-webscrape-in-r-using-the-who-snake-database/


# install.packages("devtools")
# devtools::install_github("ropensci/RSelenium")
library(RSelenium)
library(tidyverse)
# library(stringr)
library(rvest)

setwd("~/Box Sync/mpc/textanalysis2018/ourstoshape/")

# 1. Scrape comments: RSelenium approach -------
# Restarted the server for each set -- 
# the elements I'd painstakingly located apparently changed after the prior action;
# rather than find them again, I chose to start with a fresh instance.

# a. Community comments
url <- "https://ourstoshape.virginia.edu/comments"
# Start the server and connect to it
system('docker run -d -p 4445:4444 selenium/standalone-firefox') # start a server
remDr <- remoteDriver(remoteServerAddr = "localhost", 
                      port = 4445L, browserName = "firefox") # connect to running server
remDr$open() # open connections

# Open web page
remDr$navigate(url) # navigate to url; not opening

# Ways of verifying webpage: getTitle() 
remDr$getCurrentUrl()

# Click Load More X times (default is "Community" comments)
# As of 12/7, 48 clicks gets to end (34 on 9/10, 40 on 10/10)
counter <- 0
while(counter<48){
  morebutton <- remDr$findElement(using = 'css', value = "a.button")
  morebutton$clickElement()
  Sys.sleep(1) # add time between clicks
  counter <- sum(counter, 1)
}

# Grab content
commpage <- xml2::read_html(remDr$getPageSource()[[1]])
comm_comm <- commpage %>% html_nodes(".comment") %>% html_text()
names_comm <- commpage %>% html_nodes(".first-name") %>% html_text()
role_comm <- commpage %>% html_nodes(".affiliations") %>% html_text()

# When done, stop the docker container to avoid issues with blocked ports
system('docker stop $(docker ps -q)')


# b. Discovery comments
# Start the server and connect to it
system('docker run -d -p 4445:4444 selenium/standalone-firefox') # start a server
remDr <- remoteDriver(remoteServerAddr = "localhost", 
                      port = 4445L, browserName = "firefox") # connect to running server
remDr$open() # open connections

# Open web page
remDr$navigate(url) # navigate to url; not opening
remDr$getCurrentUrl()

# Navigate to select "Discovery" comments
applybutton <- remDr$findElement(using = 'id', value = "edit-submit-webform-comments")
select_disc <- remDr$findElement(using = "css selector", 
                                 value = ".uva-topic-select.webform-entity-select.form-select option[value='4']")
select_disc$getElementAttribute("value") # checking
select_disc$clickElement()
applybutton$clickElement() # Click Apply # 

# As of 12/7 21 clicks gets to end (16 on 9/10, 18 on 10/10)
counter <- 0
while(counter<21){
  morebutton <- remDr$findElement(using = 'css', value = "a.button")
  morebutton$clickElement()
  Sys.sleep(1) # add time between clicks
  counter <- sum(counter, 1)
}

# Grab content
discpage <- xml2::read_html(remDr$getPageSource()[[1]])
comm_disc <- discpage %>% html_nodes(".comment") %>% html_text()
names_disc <- discpage %>% html_nodes(".first-name") %>% html_text()
role_disc <- discpage %>% html_nodes(".affiliations") %>% html_text()

# When done, stop the docker container to avoid issues with blocked ports
system('docker stop $(docker ps -q)')


# c. Service comments
# Start the server and connect to it
system('docker run -d -p 4445:4444 selenium/standalone-firefox') # start a server
remDr <- remoteDriver(remoteServerAddr = "localhost", 
                      port = 4445L, browserName = "firefox") # connect to running server
remDr$open() # open connections

# Open web page
remDr$navigate(url) # navigate to url; not opening
remDr$getCurrentUrl()

# Navigate to select "Service" comments
applybutton <- remDr$findElement(using = 'id', value = "edit-submit-webform-comments")
select_serv <- remDr$findElement(using = "css selector", 
                                 value = ".uva-topic-select.webform-entity-select.form-select option[value='3']")
select_serv$getElementAttribute("value") # checking
select_serv$clickElement()
applybutton$clickElement() # Click Apply # 

# As of 12/7, 15 clicks gets to end (12 on 9/10, 14 on 10/10)
counter <- 0
while(counter<15){
  morebutton <- remDr$findElement(using = 'css', value = "a.button")
  morebutton$clickElement()
  Sys.sleep(1) # add time between clicks
  counter <- sum(counter, 1)
}

# Grab content
servpage <- xml2::read_html(remDr$getPageSource()[[1]])
comm_serv <- servpage %>% html_nodes(".comment") %>% html_text()
names_serv <- servpage %>% html_nodes(".first-name") %>% html_text()
role_serv <- servpage %>% html_nodes(".affiliations") %>% html_text()

# When done, stop the docker container to avoid issues with blocked ports
system('docker stop $(docker ps -q)')


# 2. Format comments for later analysis -------
comm1 <- bind_cols(list(comm_comm, role_comm, names_comm))
comm1 <- comm1 %>% rename(comment = V1, 
         role = V2,
         name = V3)

comm2 <- bind_cols(list(comm_disc, role_disc, names_disc))
comm2 <- comm2 %>% rename(comment = V1, 
                          role = V2,
                          name = V3)

comm3 <- bind_cols(list(comm_serv, role_serv, names_serv))
comm3 <- comm3 %>% rename(comment = V1, 
                          role = V2,
                          name = V3)

comments <- bind_rows("community" = comm1, "discovery" = comm2, "service" = comm3, .id = "type")
comments <- comments %>% 
  mutate(type = as.factor(type),
         name = str_remove_all(name, "[^[:alnum:]]"),
         role = str_replace_all(role, " &", ","),
         role = str_trim(role),
         comment = str_trim(comment))


# Save the data -------
save.image("ots_comments_all.RData")
# load("ots_comments_all.RData")
write_excel_csv(comments, "ots_comments.csv")


# # NOT USED -----
# # initially tried rvest, but needed to be able to "load more"
# url <- "https://ourstoshape.virginia.edu/"
# ots_session <- html_session(url)
# ots_source <- read_html(url)
# ots_form1 <- html_form(ots_session)[[1]]
# 
# fill_form1 <- set_values(ots_form1,
#                          webform_submission_value = "4")
# submit_form(ots_session, fill_form1)
# 
# comment4 <- jump_to(ots_session, "https://ourstoshape.virginia.edu/comments")
# comments <- comment4 %>% html_nodes(".comment") %>% html_text()
# names <- comment4 %>% html_nodes(".first-name") %>% html_text()
# role <- comment4 %>% html_nodes(".affiliations") %>% html_text()
# # Only gets first 10
# rm(ots_session, ots_source, ots_form1, fill_form1, comment4, comments, names, role)


