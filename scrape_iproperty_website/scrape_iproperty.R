library(data.table)
library(magrittr)
library(rvest)
# library(parallel) # for non-Window users

start.time <- proc.time()

homepage <- "https://www.iproperty.com.sg"
homePageNum = "https://www.iproperty.com.sg/sale/property/?pg="

pageNum = 1
url.list <- character()
switch <- 1
cat("Processing page")
while (switch == 1) {
  tryCatch(
    {   cat(pageNum)
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
cat("\nGetting full URL strings")

getUrlFromAttr <- function(x) {
  url <- paste(homepage, x, sep="")
}

url.list <- lapply(url.list, getUrlFromAttr)

getFullDetails <- function(url) {
  cat("\nNow processing:", url)
  finished <- FALSE
  tries <- 1
  while (finished == FALSE & tries <= 5) {
    
    tryCatch({
      page <- read_html(url)
      headings <- page %>% html_nodes(" .description-left h3, .table-list span") %>% html_text()
      values <- page %>% html_nodes(" .description-left p, .table-list p") %>% html_text()
      listing <- setNames(as.list(values), headings)
      return(listing)
      finished <<- TRUE
    },
    error = function(e) {
      cat("\nNumber of tries:", tries)
      message("Encountered error : ", e)
      Sys.sleep(sample(seq(1, 5, by=0.001), 1))
      tries <<- tries + 1
      NULL
    }
    )
  }
}
cat("\nGetting details for all URLs\n")

dat <- rbindlist(lapply(url.list, getFullDetails), fill=TRUE, use.names = TRUE)
# If not using Windows: It is much faster to run using the package "Parallel" and mclapply
# dat <- rbindlist(mclapply(url.list, getFullDetails), fill=TRUE, use.names = TRUE)

fwrite(dat, file="iproperty.csv")

get.url.time <- (got.urls - start.time)
get.details.time <- (proc.time() - got.urls)