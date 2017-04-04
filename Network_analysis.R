library(igraph)
library(robustbase)
setwd("/home/nicole/Documents/Projects/version_control/ABM_DOM_PM_experiments/RCode/")

# Calibration parameters --------------------------------------------------
###Empirical parameters as appeared in the publication DOM-PM network:
load("~/Documents/Projects/version_control/DOM-PM-network/RCODE/dataBase.Rdata")

#########################################################################
#distribution of number of experiment per paper per year over the years since 1990:
#n.experiments  <- sapply(1990:2015, function(x) {n.papers  <- length(d.data.nom.type$DOI[d.data$year == x & d.data.nom.type$DOI != ""])
#n.exp  <- nrow(d.data.nom.type[d.data.nom.type$year == x,])
#if(n.papers == 0){return(0)}else{return(n.exp/n.papers)}})
#distribution of number of experiments per publication without segregation into years
n.experiments  <- sapply(d.data.nom.type$DOI[d.data.nom.type$DOI != ""], function(x) {ref  <- d.data.nom.type$reference[d.data.nom.type$DOI == x][1];
n.exp  <- length(d.data.nom.type$reference[d.data.nom.type$reference == ref])})                                            
#in order to avoid the influence of extremely large experiments which don't reflect the overall trend we'll use robust estimation of the mean value:
mean.exper  <- huberM(n.experiments)$mu #used to calibrate the 
pdf("../figures/nExperimentsEmpirical.pdf")
hist(n.experiments,main = "", xlab = "number of experiments per publication")
abline(v =mean.exper,col = "tomato")
legend("topright",legend = paste("robust mean = ",signif(mean.exper,digits = 2),sep = ""),bty="n")
dev.off()
#########################################################################
#Initialization of the diversity up to the year of simulation start:
#what was the state of diversity in up to 1990?
new.d  <- d.data.nom.type[d.data.nom.type$year <1990,]#data rows before 1990
comb.studied  <-  sapply(1:nrow(new.d), function(x) paste(new.d$ENP[x], new.d$NOM.type.detailed[x], sep = "-"))
comb.studied.unique  <- length(unique(comb.studied))
n.experiments  <- nrow(new.d)
comb.studied.unique/n.experiments#cummulative diversity index till 1990
#Into the model goes: there are 38 unique combinations studied in 51 experiments, their weights are not distrbuted evenly ,though, 
comb.studied.unique
n.experiments
table(comb.studied)#the distribution of weight for the 38 studied combinations till 1990 (excluding):
#the distribution of weigths for each studied combination:
unique((table(comb.studied)))#what are the different weights (repetition) till 1990? 1,2,7,4 different weights
sum(table(comb.studied) == 1)#32 combinations were studied once?
sum(table(comb.studied) == 2)#4 were studied twice
sum(table(comb.studied) == 4)#1 combination was studied 4 times
sum(table(comb.studied) == 7)#1 combination was studied 7 times
#over all this adds to 38 combinations and 51 experiments, and this along with the weight distribution should go into the model
#######
#so basicaly by 1990 there are already 48 unique combinations studied, out of those 45 were studeid once and 3 were studied 2. 
####

# Collaboration network analysis ------------------------------------------

#Analysis of empirical authors distribution: 
# #### Create a search query to for the web of science for the DOIs in the database:
#uncomment if you wish to research the DOIs that make the database:
  # #all authors and DOIs in the database:
  # wos.quesry <- ""
  # for(i in d.data.nom.type$DOI){
  #   if(i != ""){wos.quesry  <- paste(wos.quesry,i,sep = " OR ")}
  # }
#empirical collaboration network, its a weights undirected graph, where authors names are stored in the attribute id of the V(graph)
#and the links weigths are stored in E(graph)$weight
g.all.authors  <- read.graph("../data/All_authorsNetwork.net", format = "pajek")
vcount(g.all.authors)
ecount(g.all.authors)
assortativity_degree(g.all.authors)
length(clusters(g.all.authors)$csize)#number of individual clusters
barplot(table(clusters(g.all.authors)$csize))
hist(E(g.all.authors)$weight)
sum(E(g.all.authors)$weight)#overall pairwise collaborations among the 
empirical.g.char <- c(NA, length(clusters(g.all.authors)$csize), max(clusters(g.all.authors)$csize), 
                      ecount(g.all.authors), assortativity_degree(g.all.authors))

#Simulated collaboration network
#single instance comparison using a figure
g.auth.nomix.sim <- read.graph("../data/coll_networkSim_nomix.net", format = "pajek")
g.auth.yesmix.sim <- read.graph("../data/coll_networkSim_yesmix.net", format = "pajek")

# Coauthors figures -------------------------------------------------------
m  <- rbind(
  c(0, 1, 0.6, 1),#for the empirical collaboration network,
  c(0, 0.5, 0, 0.6),#for the no mixing simulated collboartion pattern
  c(0.5, 1, 0, 0.6))#for the mixing simulated collaboration pattern
pdf("../figures/collaboration_networks.pdf")#, width = 14, height = 7)#the figure will be saved under this name in the working directory
split.screen(m)
screen(1)
par(mar = rep(0.1, 4))
plot(g.all.authors,vertex.label = "",vertex.size = 3)#, edge.width = E(g.all.authors)$weight)
mtext("a", side = 3, line = -2, adj = 0.025, cex = 1, font = 2)#subfigure numbering
screen(2)
#legend for the PM
plot(g.auth.nomix.sim, vertex.label = "", vertex.size = 3)#, edge.width = E(g.all.authors)$weight)
mtext("b", side = 3, line = -2, adj = 0.025, cex = 1, font = 2)#subfigure numbering
screen(3)
plot(g.auth.yesmix.sim, vertex.label = "", vertex.size = 3)#, edge.width = E(g.all.authors)$weight)
mtext("c", side = 3, line = -2, adj = 0.025, cex = 1, font = 2)#subfigure numbering
close.screen(all.screens = TRUE)
dev.off()


# enssemble comparison the simulated collaboration pattern is far too simple to explain the empirical one
coauthor.nomix.sim <- read.csv("../data/coll_network_simul_nomix.csv", header = F)
#appending empirical data of the coauthorship network:
coauthor.nomix.sim <- rbind(coauthor.nomix.sim, empirical.g.char)
#coauthor.nomix.sim <- coauthor.nomix.sim[, c(-4)]
coauthor.nomix.sim.pc  <- princomp(coauthor.nomix.sim[ , -1], cor = T)
summary(coauthor.nomix.sim.pc, loadings = T)
#create colors based on the innovation threshold:
innov.val <- as.numeric(levels(as.factor(coauthor.nomix.sim[ , 1])))
colors.pall <- rainbow(length(innov.val))
col.innov <- sapply(1:(nrow(coauthor.nomix.sim)-1), function(x) return(colors.pall[coauthor.nomix.sim[x, 1] == innov.val]))
col.innov <- c(col.innov, "black") #add the color of the empirical network

plot(coauthor.nomix.sim.pc$scores[,1], coauthor.nomix.sim.pc$scores[,2], xlab = "pc1", ylab = "pc2", bty = "l")
points(coauthor.nomix.sim.pc$scores[nrow(coauthor.nomix.sim), 1], coauthor.nomix.sim.pc$scores[nrow(coauthor.nomix.sim), 2], 
       cex = 2, pch = 3)#mark the position of the empirical network

coauthor.yesmix.sim <- read.csv("../data/coll_network_simul_yesmix.csv", header = F)
#appending empirical data of the coauthorship network:
coauthor.yesmix.sim <- rbind(coauthor.yesmix.sim, empirical.g.char)
coauthor.yesmix.sim.pc  <- princomp(coauthor.yesmix.sim[ , -1], cor = T)

plot(coauthor.yesmix.sim.pc$scores[,1], coauthor.yesmix.sim.pc$scores[,2], xlab = "pc1", ylab = "pc2", bty = "l")
points(coauthor.yesmix.sim.pc$scores[nrow(coauthor.yesmix.sim), 1], coauthor.yesmix.sim.pc$scores[nrow(coauthor.yesmix.sim), 2], 
       cex = 2, pch = 3)#mark the position of the empirical network

coauthor.nomix.sim <- read.csv("../data/coll_network_simul_nomix.csv", header = F)
coauthor.yesmix.sim <- read.csv("../data/coll_network_simul_yesmix.csv", header = F)
#color based on mixing: the color balck is for the empirical data
collaboration.colors <- c(rep("skyblue", nrow(coauthor.nomix.sim)), rep("green", nrow(coauthor.yesmix.sim)), "black")
coauthor.all <- rbind(coauthor.nomix.sim, coauthor.yesmix.sim, empirical.g.char)
colnames(coauthor.all) <- c("innovation", "components", "giant component", "n.links", "assortativity")
coauthor.all.pc <- princomp(coauthor.all[, -1], cor = T)
summary(coauthor.all.pc, loadings = T)

pdf("../figures/collaboration_pca.pdf")
plot(coauthor.all.pc$scores[,1], coauthor.all.pc$scores[,2], xlab = "pc1", ylab = "pc2", bty = "l", col = collaboration.colors, 
     ylim = c(-2,5))
points(coauthor.all.pc$scores[nrow(coauthor.all), 1], coauthor.all.pc$scores[nrow(coauthor.all), 2], 
       cex = 2, pch = 3)#mark the position of the empirical network
legend("top", legend = c("mixing collaboration", "not mixing collaboration"), col = c("skyblue", "green"), pch = 1, bty = "n")
dev.off()

# Authors duration period -------------------------------------------------

#the next step is to add an attribute of diversity of tested materials to each author, and to extract the list of authors for each year using the output file from the web of science of all authors:
####
#a place holder for the authors information, where the year of first appearance is te year the author first appeared in the database and yera of last appearance is the year the
#last appeared in the database, also the node attribute  "dois" is a list of DOI associated with the author already appended to the authors
#network
d.authors  <- data.frame(author = "", yearFirst = 0, yearLast = 0)
V(g.all.authors)$dois = c()
col.size  <- c()
author.found  <- FALSE
wws.search  <- file("data/wosDoiSearchOut.ciw",open="r")#open file connection
while(length(l.line <- readLines(wws.search, n = 1, warn = FALSE)) > 0){
  #print(l.line)
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
        d.authors <- rbind(d.authors,data.frame(author = a,yearFirst = year,yearLast = year))
      }
      #print(sum(V(g.all.authors)$id == tolower(a)))
    V(g.all.authors)$dois[V(g.all.authors)$id == tolower(a)] <- list(c(unlist(V(g.all.authors)$dois[V(g.all.authors)$id == tolower(a)])
                                                                       , tolower(doi)))
    }
  }
  }


mean(col.size)#average collaboration group size
pdf("empiricalCollSize.pdf")
par(cex = 1.5)
barplot(table(col.size),ylab = "frequency",xlab = "number of authors in a single publication",las = 2)
abline(v = mean(col.size),col = "tomato")
legend("topright", legend = paste("mean = ",signif(mean(col.size),digits = 2),sep = ""),text.col = "tomato",bty = "n")
dev.off()

#What is the average "life expectency" of researchers?
hist(d.authors$yearLast - d.authors$yearFirst,main = "",xlab = "Life expectancy")#value of zero means people publish once and disappear
lifeExp <- table((d.authors$yearLast - d.authors$yearFirst)+1)/sum(table((d.authors$yearLast - d.authors$yearFirst)+1))#empirical probability of life expectency
pdf("lifeExp.pdf")
par(cex = 1.5)
barplot(lifeExp,ylab = "probability of leaving the field",xlab = "activity duration (years)")
dev.off()
#what is the birth rate?
pdf("birthRate.pdf")
par(cex = 1.5)
barplot(table(d.authors$yearFirst),ylab = "# of new researchers",main = "")
dev.off()
#death rate:
barplot(table(d.authors$yearLast))
#so apperently there are 3 phenomena that need to be accounted for:
#life expectency of about 1 year = 1 time step
#exponential birth rate (still accounting dying within 1 time step)
growth  <- table(d.authors$yearFirst)#number fo researchers entering the field each year, empirical data
#fit an exponential function to the growth of the number of researchers over the years:
#ommiting the entry of zero... or actually taking the data from 1990 onward:
y.growth <- growth[as.numeric(names(growth)) >= 1990]
d.growth <- data.frame(y = y.growth)#putting everything in a dataframe
#now, fitting a nonlinear regression to the data:
non.linear.birth.rate  <- nls(y.Freq~alpha^as.numeric(y.Var1),data = d.growth,start = list(alpha = 0.15),)
plot(d.growth$y.Freq~as.numeric(d.growth$y.Var1),pch = 1,type= "b")
lines(as.numeric(d.growth$y.Var1),predict(non.linear.birth.rate,newdata = data.frame(as.numeric(d.growth$y.Var1))),col = "tomato")

x1 <- 4
x <- c()
for(i in 1:26){
  x <- c(x,x1)
  x1 <- x1+0.15*x1
}
plot(x,type = "b")


# Experimental network analysis -------------------------------------------
exper.no.mix <- read.csv("../data/exp_network_simul_nomix.csv", header = F)
exper.no.mix <- rbind(exper.no.mix, c( NA,6, 227,535, -0.31, 0.563))#adding the properties of the empirical experimental network
exper.no.mix.pc  <- princomp(exper.no.mix[,-1], cor = T)
#create colors based on the innovation threshold:
innov.val <- as.numeric(levels(as.factor(exper.no.mix[, 1])))
colors.pall <- rainbow(length(innov.val))
col.innov <- sapply(1:(nrow(exper.no.mix)-1), function(x) return(colors.pall[t[x,1] == innov.val]))
col.innov <- c(col.innov, "black") #add the color of the empirical network

plot(exper.no.mix.pc$scores[,1], exper.no.mix.pc$scores[,2], col = col.innov, xlab = "pc1", ylab = "pc2", bty = "l")
points(t.pc$scores[nrow(t),1], t.pc$scores[nrow(t),2], cex = 2, pch = 3)#mark the position of the empirical network
legend("top", legend = innov.val, fill = colors.pall, x.intersp = 0.01, y.intersp = 0.9, bty = "n", border = NA)
#consider robust PCA
library(igraph)
t <-  read.graph("../coll_network.net", format = "pajek")
plot(t, vertex.label = "", vertex.size = 2)


p <- read.graph("../data/exp_networkSim_nomix.net", format = "pajek")
plot(p, vertex.label = "", vertex.size = 3)

#simulation output experimental network:  
g.data <- read.csv("empirical_network.csv",header = FALSE)
#give special annotations to ENP columns and DOM column:
g.data[,1] <- sapply(as.character(g.data[,1]),function(x) paste(x,"ENP",sep = "-"))
g.data[,2] <- sapply(as.character(g.data[,2]),function(x) paste(x,"DOM",sep = "-"))
#eliminate nodes that have no connection (third column is zero)
head(g.data)
nrow(g.data)
g.data <-  g.data[g.data[,3] >0 ,]
nrow(g.data)
g  <- graph.adjacency(get.adjacency(graph.data.frame(g.data[1:2],directed =FALSE),sparse=FALSE),mode ="undirected",weighted = TRUE)
V(g)$type = ifelse(V(g)$name %in% g.data[,1], TRUE, FALSE)
V(g)$shape = ifelse(V(g)$name %in% g.data[,1], "circle", "square")
V(g)$color = ifelse(V(g)$name %in% g.data[,1], "skyblue", "salmon")
E(g)$width  <- g.data[,3]
sum(E(g)$width)
#hist(degree(g))
ecount(g)
pdf("simulated_Innov05.pdf")
plot(g,vertex.label = "",vertex.size = sqrt(degree(g)),edge.width = sqrt(E(g)$width))
dev.off()
assortativity_degree(g)
hist(degree(g))
vcount(g)

  
  
  
  
  

# Old code ----------------------------------------------------------------

#Next is the calculation of other experiemntal attribute to each author:
doi.list <- d.data.nom.type[d.data.nom.type$DOI != "", c("reference", "DOI")]
for(author in V(g.all.authors)$id){
  doi <- unlist(V(g.all.authors)$dois[V(g.all.authors)$id == author])
  pub <- as.character(doi.list$reference[doi.list$DOI %in% doi])
  #if(sum(doi.list$DOI %in% tolower(doi)) == 0){
  #print(doi)
  #}
  expr <- d.data.nom.type[d.data.nom.type$reference %in% pub,c("ENP","NOM.type.detailed")]
  comb <- sapply(1:nrow(expr), function(x) paste(expr[x,1], expr[x,2], sep = "-"))
  if(nrow(expr) > 1){
    comb.div <- length(unique(comb))/nrow(expr)
  }else{comb.div = NA}
  V(g.all.authors)$n.comb[V(g.all.authors)$id == author] <- length(unique(comb))
  V(g.all.authors)$n.expr[V(g.all.authors)$id == author] <- nrow(expr)
  #print(comb.div)
  V(g.all.authors)$comb.div[V(g.all.authors)$id == author] <- comb.div
  #the colors of the nodes based on their number of experiments and diversity:
  V(g.all.authors)$n.expr.col[V(g.all.authors)$id == author] <- ifelse(nrow(expr) > 1, yes = "skyblue", no = "seagreen3")
  V(g.all.authors)$comb.div.col[V(g.all.authors)$id == author] <- ifelse(test = comb.div > 0.5,  yes = "skyblue", no = "seagreen3")
  V(g.all.authors)$comb.exp.div.col[V(g.all.authors)$id == author] <- ifelse(test = (comb.div > 0.5 & nrow(expr) > 1),  yes = "skyblue", no = "seagreen3") #this consideres both the number of experiments and the diversity, only those that
  #have n.expr above 1 and diversity above 0.5 will be blue
  if(tolower(author) %in% tolower(d.authors$author)){
    #print(author)
    author.years <- d.authors[tolower(d.authors$author) == tolower(author),2:3]
    age <- abs(as.numeric(author.years[1]) - as.numeric(author.years[2])) + 1
    #print(age)
    V(g.all.authors)$age[V(g.all.authors)$id == author] <- age#how long in years did this author stayed in the field
  }
}

#relationship between degree (number of different collaborators) and the diversity/ number of combinations:
cor(degree(g.all.authors), V(g.all.authors)$n.comb, use = "complete.obs")
plot(degree(g.all.authors), V(g.all.authors)$n.comb)
cor(degree(g.all.authors), V(g.all.authors)$comb.div, use = "complete.obs")
plot(degree(g.all.authors), V(g.all.authors)$comb.div/abs(diff(range(V(g.all.authors)$comb.div, na.rm = T))), col = V(g.all.authors)$n.expr.col)
cor(degree(g.all.authors), V(g.all.authors)$age, use = "complete.obs")
#relationship between duration of research and the collaboration
plot(V(g.all.authors)$age, degree(g.all.authors), col = V(g.all.authors)$n.expr.col)
#relationship between duration of research and the collaboration
cor(V(g.all.authors)$age, V(g.all.authors)$comb.div, use = "complete.obs")
plot(V(g.all.authors)$age, V(g.all.authors)$comb.div, col = V(g.all.authors)$n.expr.col)
#relationship between the number of experiemnts and an age:
plot(V(g.all.authors)$age, V(g.all.authors)$n.expr)
plot(V(g.all.authors)$age, V(g.all.authors)$n.comb)
#relatioship between the number of experiments and diversity:
pdf("emp_cor_eprDiver.pdf")
plot(V(g.all.authors)$n.expr, V(g.all.authors)$comb.div, ylab = "Combination diversity", xlab = "Number of experiments")
dev.off()
cor(V(g.all.authors)$n.expr, V(g.all.authors)$comb.div, use = "complete.obs")


#by the pairwise relationship there is not much to see, what about higher dim?
d.authors.prop <- data.frame(age = V(g.all.authors)$age, comb.div = V(g.all.authors)$comb.div, n.expr = V(g.all.authors)$n.expr, n.coll = degree(g.all.authors))

d.authors.prop <- d.authors.prop[complete.cases(d.authors.prop),]
pca.authors <- princomp(d.authors.prop, cor = T)
summary(pca.authors, loadings = T)
plot(pca.authors$scores[,1:2])#also by PCA there seems not to be strong relationships

plot(g.all.authors, vertex.color = V(g.all.authors)$comb.exp.div.col, vertex.label = "", vertex.size = 3)


