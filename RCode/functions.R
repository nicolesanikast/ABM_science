extract.activity.duration <- function(file.name, name, last.year){
  #this function extracts the distribution of duration of activity, in years, in a given research field. 
  #It takes as an argument a file name for a references file obtained from the Web of Science (file type .ciw)
  #It then parse the file and extract for each author the first and last year of appearance. If the last year of appearance
  #is the latest year for which the references were obtained it marks the duration (last-first year) as censured data point.
  #it returns dataframe with each row represents an authors, with four columns: year of first appearance, year of last apperance,
  #duration in years and a column stating whether of not this data point is censored. The function also saves in the working directory the
  #duration distribution histogram. The "last.year" is an argument that says what is the last year for which the papers were searched for
  d.authors  <- data.frame(author = "", yearFirst = 0, yearLast = 0)
  col.size  <- c()
  author.found  <- FALSE
  wws.search  <- file(file.name, open="r")#open file connection
  while(length(l.line <- readLines(wws.search, n = 1, warn = FALSE)) > 0){
    if(grepl("^AU\\s+",l.line)){
      author.found  <- TRUE#a flag to mark we are in lines corresponding to authors
      #currentJournal  <- TRUE#a flag to mark we are in lines corresponding to a given journal}
      authors <- c()#a place holder to collect all authors from this current journal
      #if we got into a line were there are authors listed mark it and continue assuming that while author.found == TRUE the lines corresond to authors
      a <- gsub("^\\s+|\\{\\{|\\}\\}|\\,$","",unlist(strsplit(l.line,"AU "))[2])
      #print(a)
      authors  <- c(authors,tolower(a))#accumulate the authors from the current pubication in a list
      #print(authors)
      next
    }
    if(grepl("^TI\\s+", l.line)){
      author.found  <- FALSE#mark that the next lines don't correspond to authors anymore this is the next line tag after the authors' line tag
      #print(authors)
    }
    if(author.found){#if we are still in lines corresponding to authors
      authors <- c(authors, tolower(gsub("^\\s+|\\{\\{|\\}\\}|\\,$","",l.line)))#accumulate all authors (plus strip leadig and trailing spaces)
      #print(authors)
    }
    if(grepl("^DI\\s+",l.line)){#the next update the relevant DOI
      doi <- as.character(gsub("^\\s+|\\{\\{|\\}\\}|\\,$","",unlist(strsplit(l.line,split = "DI "))[2]))
      #print(doi)
    }
    if(grepl("^PY\\s+",l.line)){
      #so now we got to a line which correspond to a year of publication, get the year and store in the variable: "year"
      year <- as.numeric(gsub("^\\s+|\\{\\{|\\}\\}|\\,$","",unlist(strsplit(l.line,split = "PY "))[2]))
      #print(year)
      #now that we have both author list and the year of publication we can see of this is appears in the dataframe already and update accordingly
      col.size <- c(col.size, length(authors))
      for(a in authors){
        #print(a)
        if(a %in% d.authors$author){
          #if the author already exists in the database then update date of appearance and date of disappearance
          #print(d.authors$yearFirst[d.authors$author == a])
          if(year < d.authors$yearFirst[d.authors$author == a]){d.authors$yearFirst[d.authors$author == a]  <- year}#if the current publication year is earlier than the yearof first appearnce update the year of first appearance to be the current year of publication
          if(year > d.authors$yearLast[d.authors$author == a]){d.authors$yearLast[d.authors$author == a]  <- year}#if the current year is later than the last year of appearance then update the year of last apperance to the current year
        }
        if(!a %in% d.authors$author){
          #if the author is no present yet in the data base update his details with the current year of publication (of the current publicaitons)
          d.authors <- rbind(d.authors,data.frame(author = a, yearFirst = year, yearLast = year))
        }
      }
    }
  }
  d.authors$duration.years <- d.authors$yearLast - d.authors$yearFirst + 1
  d.authors$censored <- d.authors$yearLast == last.year#is the data point censored?
  pdf(paste(name, "authorsActivityDuration.pdf", sep = "_"))
  hist(d.authors$duration.years, main = "", xlab = "years", xlim = c(1, max(d.authors$duration)))
  dev.off()
  return(d.authors[-1, ])#remove the first line since it is a blank line
}
fit.survival.function(activity.duration){
 #for the activity duration of authors, the function fits a survival curve (weibull distribution). It returns the fitted model, which also contains the 
  #distribution parameters.
}
extract.coll.graph.prop <- function(coll.graph, name){
  #from the collaboration graph obtained from the VOS viewer the current function extract various properties such as degree distirbution,
  #assortitivity etc. It saves the figure files in the working directory using the argument "name" for identification.
}
