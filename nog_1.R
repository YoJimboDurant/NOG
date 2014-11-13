# from http://stackoverflow.com/questions/3195522/is-there-a-simple-way-in-r-to-extract-only-the-text-elements-of-an-html-page
library(stringr)
library(plyr)


getTheName <- function(psalm){
u <- paste0("https://www.biblegateway.com/passage/?dc=1&utm_expid=13466113-14.7HxyD5XxTMyyfz37nhl-Gg.1&search=Psalm+",psalm,"&version=NOG") 
u
doc.raw <- readLines(u)

verse <- doc.raw[grepl("h1 class=\"passage-display", doc.raw)]

verse <- str_extract(verse, "<sup class=\"versenum\">.*") 

BigNames <- str_extract_all(verse, "<i>.*?</i>")
lx <- BigNames[[1]]


    lx <- gsub("<i>", "", lx)
    lx <- gsub("</i>", "", lx)
    lx <- factor(lx)
    dfx <- data.frame(name = levels(lx), freq = tabulate(as.integer(lx))) 
    return(dfx)

}
  
  
theList <- data.frame(number=1:150)

theNamesOf <- ddply(theList, .(number), function(x) getTheName(x))

lengths <- tapply(theNamesOf$name,theNamesOf$number, function(x) length(x) )

lengths[lengths==max(lengths)]

plot(lengths, xlab="Psalm No.", ylab="Number of Names for God")
