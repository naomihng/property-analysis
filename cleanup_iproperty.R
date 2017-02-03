# dollar.cols= c("Asking Price", "Asking (PSF)", "Asking Price  psf", "Asking Price  psm", "Land   psf", "Land   psm", "Asking (PSM)", "Asking (PSA)")

#your test DT
test3 <- dat.test
#remove colon and trailing spaces
names(test3) <- trimws(sub(" :$", "", names(test3)))

combineTwoColumns <- function (dt, regex) {
  colnames <- dt[, names(.SD), .SDcols = names(dt) %like% regex] 
  dt[, (colnames[1]) := ifelse(is.na(get(colnames[1])) | get(colnames[1]) == "View to offer", get(colnames[2]), get(colnames[1]))]
  dt[, (colnames[2]) := NULL]
}

combineTwoColumns(test3, "^Asking.*(?i)psm")
combineTwoColumns(test3, "^Asking.*(?i)psf")

dollar.cols <- test3[, names(.SD), .SDcols = names(test3) %like% "Asking|(?i)psf|(?i)psm"]
for (j in dollar.cols) set(test3, j=j, value=(as.numeric(gsub("SGD| |,","",test3[[j]]))))

# set factor columns
factor.cols <- c("Property Type", "Bedrooms", "Bathrooms", "Tenure", "Estate", "Unit Type", "Carpark", "Air Cond", "District")
for (col in factor.cols) set(test3, j = col, value = as.factor(test3[[col]]))



