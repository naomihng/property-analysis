# This script scrapes data of property for sale on the website www.iproperty.com.sg. 
# There are about 350 pages with links to 20 property listings on each page. This script retrieves information like description, asking price, built up, location and others listed in the body of the property listing page.

library(data.table)
library(magrittr)
library(rvest)
# library(parallel) # for non-Windows users

start.time <- proc.time()

domain <- "https://www.iproperty.com.sg"
homePageNum = paste0(domain,"/sale/property/?pg=")

# Get a list of all property listing urls
pageNum = 1
url.list <- character()
switch <- 1
cat("Processing page")
# to avoid hard coding the page number of the last page to scrape, use tryCatch to stop loop when there are no more urls to retrieve
while (switch == 1) {
  tryCatch(
    {   cat(" ", pageNum)
        pageurl <- paste(homePageNum, pageNum, sep="")
        listingUrlPart <- read_html(pageurl) %>% html_nodes(".search-listing") %>% html_node("a") %>% html_attr("href")
        checkExistance <- listingUrlPart[[1]]
        url.list <- append(url.list, listingUrlPart)
        pageNum <- pageNum + 1
    },
    error = function(cond) {
      message("No results from this page: ", pageurl)
      message("Here's the original error message:")
      message(cond)
      switch <<- 0
    },
    warning=function(cond) {
      message(paste("URL caused a warning:", pageurl))
      message("Here's the original warning message:")
      message(cond)
      return(NULL)
    }
  )
}

cat("\nEnded at page", pageNum)
got.urls <- proc.time()

# Get data from each listing page
getFullDetails <- function(url) {
  # list of urls exclude domain, so add domain to load the page
  url <- paste(domain, url, sep="")
  cat("\nNow processing:", url)
  # Try retrieving details for the URL a few times if encountered error. If it failed all tries, return NULL and move on to the next URL. (lapply will skip the row if it returns NULL)
  finished <- FALSE
  tries <- 1
  while (finished == FALSE & tries <= 5) {
    tryCatch({
      page <- read_html(url)
      # Get heading and value of each data field using attributes
      headings <- page %>% html_nodes(" .description-left h3, .table-list span") %>% html_text()
      values <- page %>% html_nodes(" .description-left p, .table-list p") %>% html_text()
      # combine headings and values into a named list
      listing <- setNames(as.list(values), headings)
      return(listing)
      finished <<- TRUE
    },
    error = function(e) {
      cat("\nNumber of tries:", tries)
      message("Encountered error : ", e)
      # Wait for a random number of seconds before trying again
      Sys.sleep(sample(seq(1, 5, by=0.001), 1))
      tries <<- tries + 1
      NULL
    }
    )
  }
}
cat("\nGetting details for all URLs\n")

# apply function to each url in the list, then bind the results together into a data.table
dat <- rbindlist(lapply(url.list, getFullDetails), fill=TRUE, use.names = TRUE)
# If not using Windows: It is much faster to run using the package "Parallel" and mclapply
# dat <- rbindlist(mclapply(url.list, getFullDetails), fill=TRUE, use.names = TRUE)

# write to CSV
fwrite(dat, file="iproperty.csv")

get.url.time <- (got.urls - start.time)
get.details.time <- (proc.time() - got.urls)
