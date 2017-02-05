# dat <- fread("iproperty.csv")

#remove colon and trailing spaces
names(dat) <- trimws(sub(" :$", "", names(dat)))

# set factor for selected columns
factor.cols <- c("Property Type", "Bedrooms", "Bathrooms", "Tenure", "Estate", "Unit Type", "Carpark", "Air Cond", "District")
for (col in factor.cols) set(dat, j = col, value = as.factor(dat[[col]]))

#combine two name versions taken from different pages
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

fwrite(dat, file="iproperty_cleaned.csv")