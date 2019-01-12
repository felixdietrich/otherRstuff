# https://stackoverflow.com/questions/12028671/merging-a-large-list-of-xts-objects

library(xts)
l <- lapply(Sys.Date()-6000:1, function(x) { N=60*8;xts(rnorm(N),as.POSIXct(x)-seq(N*60,1,-60))})
GS <- do.call.rbind
JU <- function(x) Reduce(rbind, x)
Alex <- function(x) do.call(rbind, lapply(x, as.data.frame)) #returns data.frame, not xts

identical(GS(l), JU(l))

# https://stackoverflow.com/questions/7558181/compute-range-over-list-of-xts-objects

# https://stackoverflow.com/questions/14819362/m-operator-with-mapply
# https://stackoverflow.com/questions/46453736/rbind-in-mapply-results-different-output-in-r
# Map(c, first, second)
# list3 <- append(list1, list2)

# https://stackoverflow.com/questions/13016359/how-to-directly-select-the-same-column-from-all-nested-lists-within-a-list
myList <- list(`0` = c(`1` = 10, `2` = 20, `3` = 30, `4` = 72),
               `1` = c(`1` = 15, `2` = 9, `3` = 7))
myList
lapply(myList, '[', 1) # In the case of functions like +, %*%, the function name must be backquoted or quoted.
sapply(myList, `[[`, 1)


# https://stackoverflow.com/questions/18538977/combine-merge-lists-by-elements-names
Map(c, rangex, sdx)
mapply(c, rangex, sdx, SIMPLIFY = FALSE)
