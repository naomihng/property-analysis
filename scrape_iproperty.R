library(data.table)
library(magrittr)
library(rvest)
library(RCurl)
library(parallel)

start.time <- proc.time()

homepage <- "https://www.iproperty.com.sg"
homePageNum = "https://www.iproperty.com.sg/sale/property/?pg="

pageNum = 1
url.list <- character()
switch <- 1
while (switch == 1) {
  tryCatch(
    {   cat("Processing page")
        cat(" ", pageNum)
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

# mclapply is not available on Windows.
dat <- rbindlist(lapply(url.list, getFullDetails), fill=TRUE, use.names = TRUE)
# Note: result of rbindlist is a data.table

## fwrite(dat, file="iproperty.csv")

# set factor for selected columns
factor.cols <- c("Property Type", "Bedrooms", "Bathrooms", "Tenure", "Estate", "Unit Type", "Carpark", "Air Cond", "District")
for (col in factor.cols) set(dat, j = col, value = as.factor(dat[[col]]))

#remove colon and trailing spaces
names(dat) <- trimws(sub(" :$", "", names(dat)))

#combine two column name versions taken from different pages
combineTwoColumns <- function (dt, regex) {
  colnames <- dt[, names(.SD), .SDcols = names(dt) %like% regex] 
  dt[, (colnames[1]) := ifelse(is.na(get(colnames[1])) | get(colnames[1]) == "View to offer", get(colnames[2]), get(colnames[1]))]
  dt[, (colnames[2]) := NULL]
}

combineTwoColumns(dat, "^Asking.*(?i)psm")
combineTwoColumns(dat, "^Asking.*(?i)psf")

# set numeric for dollar values
dollar.cols <- dat[, names(.SD), .SDcols = names(dat) %like% "Asking|(?i)psf|(?i)psm"]
for (j in dollar.cols) set(dat, j=j, value=(as.numeric(gsub("SGD| |,","",dat[[j]]))))

return(dat)
elapsed.time <- (proc.time() - start.time)