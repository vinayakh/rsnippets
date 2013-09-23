# Description : Wordcount comparison using tm package
# website : http://thebiobucket.blogspot.in/2013/08/text-mining-with-r-comparing-word.html

## a function that compares word counts in two texts
wordcount <- function(x, y, stem = F, minlen = 1, marg = F) {
  
  require(tm)
  
  x_clean <- unlist(strsplit(removePunctuation(x), "\\s+"))
  y_clean <- unlist(strsplit(removePunctuation(y), "\\s+"))
  
  x_clean <- tolower(x_clean[nchar(x_clean) >= minlen])
  y_clean <- tolower(y_clean[nchar(y_clean) >= minlen])
  
  if ( stem == T ) {
    
    x_stem <- stemDocument(x_clean)
    y_stem <- stemDocument(y_clean)
    x_tab <- table(x_stem)
    y_tab <- table(y_stem)   
    
    cnam <- sort(unique(c(names(x_tab), names(y_tab))))
    
    z <- matrix(rep(0, 3*(length(cnam)+1)), 3, length(cnam)+1, dimnames=list(c("x", "y", "rowsum"), c(cnam, "colsum")))
    z["x", names(x_tab)] <- x_tab
    z["y", names(y_tab)] <- y_tab
    z["rowsum",] <- colSums(z)
    z[,"colsum"] <- rowSums(z)
    ifelse(marg == T, return(t(z)), return(t(z[1:dim(z)[1]-1, 1:dim(z)[2]-1])))
    
  } else {
    
    x_tab <- table(x_clean)
    y_tab <- table(y_clean)   
    
    cnam <- sort(unique(c(names(x_tab), names(y_tab))))
    
    z <- matrix(rep(0, 3*(length(cnam)+1)), 3, length(cnam)+1, dimnames=list(c("x", "y", "rowsum"), c(cnam, "colsum")))
    z["x", names(x_tab)] <- x_tab
    z["y", names(y_tab)] <- y_tab
    z["rowsum",] <- colSums(z)
    z[,"colsum"] <- rowSums(z)
    ifelse(marg == T, return(t(z)), return(t(z[1:dim(z)[1]-1, 1:dim(z)[2]-1])))
  }
}

## example
x = "Hello new, new world, this is one of my nice text documents - I wrote it today"
y = "Good bye old, old world, this is a nicely and well written text document"

wordcount(x, y, stem = T, minlen = 3, marg = T)

# Alternate version

x = "Hello new, new world, this is one of my nice text documents"
y = "Good bye old, old world, this is a text document"
z = "Good bye old, old world, this is a text document with WORDS for STEMMING  - BTW, what is the stem of irregular verbs like write, wrote, written?"

# make a corpus with two or more documents (the cool thing here is that it could be endless (almost) numbers
# of documents to be cross tabulated with the used terms. And the control function enables you
# to do lots of tricks with it before it will be tabulated, see ?termFreq, i.e.)

xyz <- as.list(c(x,y,z))
xyz_corp <- Corpus(VectorSource(xyz))

cntr <- list(removePunctuation = T, stemming = T, wordLengths = c(3, Inf))

as.matrix(TermDocumentMatrix(xyz_corp, control = cntr))