library(igraph)
library(robustbase)
library(SDMTools)
library(TeachingDemos)
library(survival)
setwd("/home/nicole/Documents/Projects/version_control/ABM_DOM_PM_experiments/RCode/")

# Calibration parameters --------------------------------------------------
###Empirical parameters as appeared in the publication DOM-PM network, the order at which they are presented here corresponds to their listing 
#order in the accompying python (jupyter) notebook:

#load the empirical data:
load("~/Documents/Projects/version_control/DOM-PM-network/RCODE/dataBase.Rdata")
#########################################################################
#Number of materials from each type (constituent 1 is particulate matter, PM, and constituent 2 is dissolved organic matter, DOM).
pm.types <- length(unique(d.data.nom.type$ENP))
dom.types <- length(unique(d.data.nom.type$NOM.type.detailed))
#########################################################################
#Number of experiment per publication since 1990:
n.experiments  <- sapply(d.data.nom.type$DOI[d.data.nom.type$DOI != "" & d.data.nom.type$year >= 1990], function(x) {ref  <- d.data.nom.type$reference[d.data.nom.type$DOI == x][1];
n.exp  <- length(d.data.nom.type$reference[d.data.nom.type$reference == ref])})                                            
#in order to avoid the influence of extremely large experiments which don't reflect the overall trend we'll use robust estimation of the mean value:
mean.exper  <- huberM(n.experiments)$mu #used to calibrate the 
mean.exper
pdf("../figures/empiricalnExperiments.pdf")
par(cex = 1.5)
hist(n.experiments,main = "", xlab = "number of experiments per publication")
abline(v =mean.exper,col = "tomato")
#legend("topright",legend = paste("robust mean = ",signif(mean.exper,digits = 2),sep = ""),bty="n")
legend("topright", legend = paste("robust mean = ", signif(mean.exper,digits = 2), sep = ""), text.col = "tomato", bty = "n")
dev.off()
#########################################################################
#Initialization of the diversity up to the year of simulation start:
#what was the state of diversity in up to 1990?
all.combinatons <- c()
for(pm in as.character(unique(d.data.nom.type$ENP))){
  for(dom in as.character((unique(d.data.nom.type$NOM.type.detailed)))){
    all.combinatons <- c(all.combinatons, paste(pm, dom, sep = "-"))
  }
}
d.data.1990 <- d.data.nom.type[d.data.nom.type$year < 1990, ]
comb.studied <- unlist(sapply(1:nrow(d.data.1990), function(x) paste(d.data.1990$ENP[x], d.data.1990$NOM.type.detailed[x], sep = "-")))
#weigths till 1990:
weights <- rep(0, length(all.combinatons))
for(i in comb.studied){
  weights[all.combinatons == i] <- weights[all.combinatons == i] + 1
}
#summery:
table(weights)
write.csv(x = weights, file = "../data/initial_weights.csv", row.names = F)
#########################################################################
comb <- unlist(sapply(1:nrow(d.data.nom.type), function(x) paste(d.data.nom.type$ENP[x],d.data.nom.type$NOM.type.detailed[x], sep = "-")))
diversity.empirical <- (sapply(1990:2015, function(x) length(unique(comb[d.data.nom.type$year <= x]))/length(comb[d.data.nom.type$year <=x])))
write.csv(x = diversity.empirical, file = "../data/diversity_empirical.csv", row.names = F)
#########################################################################

# Authors claibration information ------------------------------------------

#Analysis of empirical authors distribution, life expectency of authors, distibution of collaboration size and the empirical number
#of new researchers entering the field each year
# #### Create a search query to for the web of science for the DOIs in the database:
#uncomment if you wish to research the DOIs that make the database:
# #all authors and DOIs in the database:
wos.query <- ""
for(i in d.data.nom.type$DOI[d.data.nom.type$year >= 1990]){
  if(i != ""){wos.query  <- paste(wos.query,i,sep = " OR ")}
}
#Extenal processing required at this step:
# 1. Copy wos.query into http://apps.webofknowledge.com search box, make sure to omit the quatation marks and the first occurenc of "OR" in the search string
# 2. Save the search results obtained in 1. into local directory, by choosing the "save to endnote desktop" -> all records -> choose "Author, Title, Source" option. 
# 3. Use the downloaded file: "savedrecs.ciw" in the following analysis (it will be also used later on to generate the empirical collaboration network using VOsviewer software)
# 4. The file "../data/wosDoiSearchOut_23022017.ciw" is the corresponding file obtained from step 1-3.


# Authors duration period (survial analysis)-------------------------------------------------

#a place holder for the authors information, where the year of first appearance is te year the author first appeared in the database and yera of last appearance is the year the
#last appeared in the database, also the node attribute  "dois" is a list of DOI associated with the author already appended to the authors
#network
d.authors  <- data.frame(author = "", yearFirst = 0, yearLast = 0)
#V(g.all.authors)$dois = c()
col.size  <- c()#a lsit of the number of authors per publication
author.found  <- FALSE
wws.search  <- file("../data/wosDoiSearchOut_23022017.ciw",open="r")#open file connection
while(length(l.line <- readLines(wws.search, n = 1, warn = FALSE)) > 0){
  #print(l.line)
  if(grepl("^AU\\s+",l.line)){
    author.found  <- TRUE#a flag to mark we are in lines corresponding to authors
    #currentJournal  <- TRUE#a flag to mark we are in lines corresponding to a given journal}
    authors <- c()#a place holder to collect all authors from this current journal
    #if we got into a line were there are authors listed mark it and continue assuming that while author.found == TRUE the lines corresond to authors
    a <- gsub("^\\s+|\\{\\{|\\}\\}|\\,$","",unlist(strsplit(l.line,"AU "))[2])
    #print(a)
    authors  <- c(authors, tolower(a))#accumulate the authors from the current pubication in a list
    #print(authors)
    next
  }
  if(grepl("^AF\\s+", l.line)){
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
    }
  }
}
#remove first row:
d.authors <- d.authors[-1, ]
#add duration column:
d.authors$duration <- d.authors$yearLast - d.authors$yearFirst + 1#adding one since the

#add censured category. Since the search stops in 2015. Authors for whom the last year is 2015 it is unkwon if 2015 was really their last activity year
#in this field, therefore we mark them as censored individuals.
d.authors$censored <- ifelse(d.authors$yearLast == 2015, 0, 1)#0 is for censored data

#Survival curve
km.fit <- survfit(Surv(duration, censored) ~1, type = "kaplan-meier", data = d.authors)

pdf("../figures/empirical_survival.pdf")
plot(km.fit, xlab = "activity duration (years)s", ylab = "survival probability", bty = "n", col = "black", mark.time = T)
dev.off()
s <- summary(km.fit)#the survuvial is the probability that a person will survive up and during the current time step.
#since there are only values for the observed incience of "death" (leaving the field) we need to fill inthe information
#for onobserved duration periods (such as 13 years for example) remeber: Kaplen Meier is a step function that goes down
#only in observed events.

s.filled <- unlist(sapply(2:length(s$time), function(x) if(s$time[x] == s$time[x-1] + 1){return(s$surv[x])}else{return(c(rep(s$surv[x-1], 
                                                                                                                        s$time[x]-s$time[x-1]-1), s$surv[x]))}))
s.filled <- c(s$surv[1], s.filled)#adding the first term
write.csv(x = s.filled, file = "../data/empirical_survival_prop.csv", row.names = F)#export the survival probability to a csv file for use in the ABM (python notebook)

# Collaboration group size ------------------------------------------------
#obtained from the citation data obtained on 23.02.2017, with manuescripts from 1990--2015 (the same document used to analyze the 
#life expectency)
signif(huberM(col.size)$mu, digits = 2)#average collaboration group size
pdf("../figures/empiricalCollSize.pdf")
par(cex = 1.5)
barplot(table(col.size),ylab = "frequency",xlab = "collaboration group size",las = 2)
abline(v = signif(huberM(col.size)$mu, digits = 2) ,col = "tomato")
legend("topright", legend = paste("robust mean = ", signif(huberM(col.size)$mu, digits = 2), sep = ""), text.col = "tomato", bty = "n")
dev.off()


# Growth rate (number of researchers entering the field per year) ----------------------
growth  <- table(d.authors$yearFirst)#number fo researchers entering the field each year, empirical data
#make sure all years are present between 1990:2015
growth <- sapply(1990:2015, function(x) if(!x %in% names(growth)){return(0)}else{return(growth[names(growth) == x])})
names(growth) <- as.character(1990:2015)
write.csv(x = as.numeric(growth), file = "../data/newAents.csv", row.names = F)

pdf("../figures/empiricalNewResearchers.pdf")
par(cex = 1)
barplot(growth,ylab = "number of new researchers",main = "", las = 2)
dev.off()

# Model output analysis ---------------------------------------------------


##########################################################################
# COMPARISON BETWEEN THE EMPIRICAL AND SIMULATED DATA
##########################################################################

# Collaboration networks ---------------------------------------------------
g.all.authors  <- read.graph("../data/empirical_authorsNetwork_23022017.net", format = "pajek")#empirical collaboration graph generated using VOsviewer
#empirical collaboration network, its a weights undirected graph, where authors names are stored in the attribute id of the V(graph)
#and the links weigths are stored in E(graph)$weigh
#empirical.g.char <- c(NA, length(clusters(g.all.authors)$csize), max(clusters(g.all.authors)$csize), 
 #                     ecount(g.all.authors), assortativity_degree(g.all.authors), mean(clusters(g.all.authors)$csize))#basic properties of the empirical collaboration network
empirical.g.char <- c(NA, mean(clusters(g.all.authors)$csize), length(clusters(g.all.authors)$csize), max(clusters(g.all.authors)$csize), 
                      ecount(g.all.authors), sum(E(g.all.authors)$weight), assortativity_degree(g.all.authors))#basic properties of the empirical collaboration network


# Coauthors single instance comparison -------------------------------------------------------
#Simulated collaboration network
g.auth.nomix.sim <- read.graph("../data/coll_networkSim_nomix.net", format = "pajek")
g.auth.yesmix.sim <- read.graph("../data/coll_networkSim_yesmix.net", format = "pajek")

m  <- rbind(
  c(0, 1, 0.6, 1),#for the empirical collaboration network,
  c(0, 0.5, 0, 0.6),#for the no mixing simulated collboartion pattern
  c(0.5, 1, 0, 0.6))#for the mixing simulated collaboration pattern
pdf("../figures/collaboration_networks.pdf")#, width = 14, height = 7)#the figure will be saved under this name in the working directory
split.screen(m)
screen(1)
par(mar = rep(0.1, 4))
plot(g.all.authors, vertex.label = "",vertex.size = 3, layout=layout.fruchterman.reingold)#, edge.width = E(g.all.authors)$weight)
mtext("a", side = 3, line = -2, adj = 0.025, cex = 1, font = 2)#subfigure numbering
screen(2)
#legend for the PM
plot(g.auth.nomix.sim, vertex.label = "", vertex.size = 3, layout=layout.fruchterman.reingold)#, edge.width = E(g.all.authors)$weight)
mtext("b", side = 3, line = -2, adj = 0.025, cex = 1, font = 2)#subfigure numbering
screen(3)
plot(g.auth.yesmix.sim, vertex.label = "", vertex.size = 3, layout=layout.fruchterman.reingold)#, edge.width = E(g.all.authors)$weight)
mtext("c", side = 3, line = -2, adj = 0.025, cex = 1, font = 2)#subfigure numbering
close.screen(all.screens = TRUE)
dev.off()

# Collaboration network - ensemble  ---------------------------------------
# enssemble comparison the simulated collaboration pattern is far too simple to explain the empirical one

coauthor.nomix.sim <- read.csv("../data/coll_network_simul_nomix_coll4.csv", header = F)#properties of ensemble of simulated networks
coauthor.yesmix.sim <- read.csv("../data/coll_network_simul_yesmix_coll4.csv", header = F)#properties of ensemble of simulated networks

#a comparison between the simulated and empirical networks in terms of number of lnks and sum of link weights
d <- matrix(c(mean(coauthor.nomix.sim$V5), mean(coauthor.nomix.sim$V6), 
              ecount(g.all.authors), sum(E(g.all.authors)$weight),
              mean(coauthor.yesmix.sim$V5), mean(coauthor.yesmix.sim$V6)), nrow = 2)
#to build error bars:
d.error <- c(sd(coauthor.nomix.sim$V5), sd(coauthor.nomix.sim$V6), 0, 0, sd(coauthor.yesmix.sim$V5), sd(coauthor.yesmix.sim$V6))
d.means <- matrix(d, nrow = 1)
pdf("../figures/collaboration_nlinks.pdf")
b <- barplot(matrix(c(mean(coauthor.nomix.sim$V5), mean(coauthor.nomix.sim$V6), 
       ecount(g.all.authors), sum(E(g.all.authors)$weight),
       mean(coauthor.yesmix.sim$V5), mean(coauthor.yesmix.sim$V6)), nrow = 2), beside = T, names.arg = c("preferential collaboration",
      "empirical", "non-preferential collabotation"), col = c("grey47", "grey87"), border = c(NA, NA, "tomato", "tomato", NA, NA),
      ylim = c(0, 5000))
bar.position <-  matrix(b, nrow = 1)
segments(bar.position, d.means - d.error, bar.position, d.means + d.error, col = c("black", "black", NA, NA, "black", "black"))
legend("top", legend = c("number of unique\ncollaborations", "number of total\ncollaborations"), 
       fill = c("grey47", "grey87"), bty = "n", border = NA, y.intersp = 2)
dev.off()

collaboration.colors <- c(rep("skyblue", nrow(coauthor.nomix.sim)), rep("green", nrow(coauthor.yesmix.sim)), "black")#color based on mixing: the color balck is for the empirical data
coauthor.all <- rbind(coauthor.nomix.sim, coauthor.yesmix.sim, empirical.g.char)
colnames(coauthor.all) <- c("innovation", "mean.component.size", "n.components","size.giant.component", "n.links", "n.collaborations","degree.assortativity")
#coauthor.all$ratio.n.comp.n.links <- coauthor.all$n.components/coauthor.all$n.links
coauthor.all$ratio.giant.comp.n.links <- coauthor.all$size.giant.component/coauthor.all$n.links
coauthor.all$ratio.degree.assor.n.links <- coauthor.all$degree.assortativity/coauthor.all$n.links
mean(clusters(g.all.authors)$csize)
mean(clusters(g.auth.yesmix.sim)$csize)

#coauthor.all.pc <- princomp(coauthor.all[, -c(1, 4, 5, 6)], cor = T)#principle component analysis
coauthor.all.pc <- princomp(coauthor.all[ , c("mean.component.size", "n.components", "size.giant.component", "degree.assortativity")], cor = T)#principle component analysis
summary(coauthor.all.pc, loadings = T)

pdf("../figures/collaboration_pca.pdf")
plot(coauthor.all.pc$scores[,1], coauthor.all.pc$scores[,2], xlab = "pc1", ylab = "pc2", bty = "l", col = collaboration.colors)
points(coauthor.all.pc$scores[nrow(coauthor.all), 1], coauthor.all.pc$scores[nrow(coauthor.all), 2], 
       cex = 2, pch = 3)#mark the position of the empirical network
legend("top", legend = c("preferential collaboration", "nonpreferential collaboration", "empirical"), 
       col = c("skyblue", "green","black"), pch = c(1, 1, 3),  bty = "n")
dev.off()


# Experimental network analysis single instance ---------------------------
#single instance of simulated network:

# exp_network_sim <- read.graph("../data/exp_networkSim_nomix.net", format = "pajek")
exp_network_sim <- read.graph("../data/exp_networkSim_nomix_try.net", format = "pajek")
# V(exp_network_sim)$type <- bipartite_mapping(exp_network_sim)$type#split the network into a bipartite mapping, the first type is 
# #the PM based on the way the ABM was structured, in the bipartite mapping it'll become labeled as "FALSE".
# V(exp_network_sim)$shape = ifelse(V(exp_network_sim)$type, "square", "circle")
# V(exp_network_sim)$color = ifelse(V(exp_network_sim)$type, "salmon", "skyblue")
# E(exp_network_sim)$weight
# plot(exp_network_sim, vertex.label = "", vertex.size = sqrt(degree(exp_network_sim)), edge.width = sqrt(E(exp_network_sim)$weight))
# 
# 
# exp_network_sim <- read.graph("../data/exp_networkSim_nomix0.0.net", format = "pajek")
# V(exp_network_sim)$type <- bipartite_mapping(exp_network_sim)$type#split the network into a bipartite mapping, the first type is 
# #the PM based on the way the ABM was structured, in the bipartite mapping it'll become labeled as "FALSE".
# V(exp_network_sim)$shape = ifelse(V(exp_network_sim)$type, "square", "circle")
# V(exp_network_sim)$color = ifelse(V(exp_network_sim)$type, "salmon", "skyblue")
# E(exp_network_sim)$weight
# plot(exp_network_sim, vertex.label = "", vertex.size = sqrt(degree(exp_network_sim)), edge.width = sqrt(E(exp_network_sim)$weight))


adjust.simulated.network <- function(g){
  V(g)$type <- bipartite_mapping(g)$type#split the network into a bipartite mapping, the first type is 
  #the PM based on the way the ABM was structured, in the bipartite mapping it'll become labeled as "FALSE".
  V(g)$shape = ifelse(V(g)$type, "square", "circle")
  V(g)$color = ifelse(V(g)$type, "salmon", "skyblue")
  E(g)$weight
  plot(g, vertex.label = "", vertex.size = log(degree(g))+0.5, edge.width = sqrt(E(g)$weight)/2, layout=layout.fruchterman.reingold)
}

#the empirical network:
exp_network_empirical <- read.graph("../../DOM-PM-network/RCODE/gDataNOMtype.gml", format = "gml")
exp_netwrork_empirical_prop <- c( NA, average.path.length(exp_network_empirical), vcount(exp_network_empirical),
                                  ecount(exp_network_empirical), assortativity.degree(exp_network_empirical), 
                                  ecount(exp_network_empirical)/sum(E(exp_network_empirical)$weight))
#adjust.simulated.network(exp_network_sim)

m  <- rbind(
  c(0, 1, 0.6, 1),#for the empirical experimental network,
  c(0, 1, 0.1, 0.6))#for the no mixing simulated experimental network, innovation threshold 0.1
pdf("../figures/experimental_networks.pdf", width = 14, height = 7)#the figure will be saved under this name in the working directory
split.screen(m)
screen(1)
par(mar = rep(0., 4))
plot(exp_network_empirical, vertex.label = "", vertex.size = sqrt(degree(exp_network_empirical)), 
     edge.width = sqrt(E(exp_network_empirical)$weight), layout=layout.fruchterman.reingold)
mtext("a", side = 3, line = -2, adj = 0.025, cex = 1, font = 2)#subfigure numbering
screen(2)
par(mar = rep(0., 4))

plot(1:20, 1:20, bty = "l", pch = "", bty = "n", axes=FALSE, ylab = "", xlab = "")
axis(side = 1, labels = c(0.1, 0.2, 0.5, 0.6, 0.9), at = c(1, 4, 10, 13, 20), lwd = 2)
mtext("innovation threshold", side = 1, line = 2.2, adj = 0.5, cex = 1, font = 1)#subfigure numbering
par(fig = c(0.0, 0.15, 0.1, 0.4), new=T)
exp_network_sim <- read.graph("../data/exp_networkSim_nomix0.1.net", format = "pajek")

adjust.simulated.network(exp_network_sim)
mtext("b", side = 3, line = -1, adj = 0.25, cex = 1, font = 2)#subfigure numbering
#mtext("b", side = 3, line = -5, adj = 0.025, cex = 1, font = 2)#subfigure numbering

par(fig = c(0.15, 0.3, 0.1, 0.4), new=T)
exp_network_sim <- read.graph("../data/exp_networkSim_nomix0.2.net", format = "pajek")
adjust.simulated.network(exp_network_sim)
#mtext("c", side = 3, line = -5, adj = 0.025, cex = 1, font = 2)#subfigure numbering
#par(fig = c(0.1, 0., 0.1, 0.1), new=T)

par(fig = c(0.37, 0.52, 0.1, 0.4), new=T)
exp_network_sim <- read.graph("../data/exp_networkSim_nomix0.5.net", format = "pajek")
adjust.simulated.network(exp_network_sim)

par(fig = c(0.55, 0.7, 0.1, 0.4), new=T)
exp_network_sim <- read.graph("../data/exp_networkSim_nomix0.6.net", format = "pajek")
adjust.simulated.network(exp_network_sim)

par(fig = c(0.85, 1, 0.1, 0.4), new=T)
exp_network_sim <- read.graph("../data/exp_networkSim_nomix1.0.net", format = "pajek")
adjust.simulated.network(exp_network_sim)

close.screen(all.screens = TRUE)
dev.off()


# Experimenal network analysis ensemble -----------------------------------

#Enssemble analysis

#non mixing network
exper.no.mix <- read.csv("../data/exp_network_simul_nomix.csv", header = F)
exper.no.mix <- rbind(exper.no.mix, exp_netwrork_empirical_prop)#adding the properties of the empirical experimental network
#exper.no.mix[, 7] <- exper.no.mix[, 3]/exper.no.mix[, 4]
colnames(exper.no.mix) <- c("innovation", "average path", "n.vertices", "n.links", "degree assortativity", "diversity")
exper.no.mix.pc  <- princomp(exper.no.mix[, c(-1)], cor = T)
summary(exper.no.mix.pc, loadings = T)
#create colors based on the innovation threshold:
innov.val <- as.numeric(levels(as.factor(exper.no.mix[, 1])))
colors.pall <- rainbow(length(innov.val))
col.innov <- sapply(1:(nrow(exper.no.mix)-1), function(x) return(colors.pall[exper.no.mix[x, 1] == innov.val]))
col.innov <- c(col.innov, "black") #add the color of the empirical network
pdf("../figures/experimenal_networks_pca.pdf")
plot(exper.no.mix.pc$scores[,1], exper.no.mix.pc$scores[,2], col = col.innov, xlab = "pc1", ylab = "pc2", bty = "l")
points(exper.no.mix.pc$scores[nrow(exper.no.mix),1], exper.no.mix.pc$scores[nrow(exper.no.mix),2], cex = 2, pch = 3)#mark the position of the empirical network
legend(x = 3.5, y = 3, legend = c(innov.val, "empirical"), col = c(colors.pall, "black"), pch = c(rep(1, length(innov.val)), 3), 
       x.intersp = 0.5, y.intersp = 0.9, border = NA, title = "C", bg = "white", box.col = "white")
dev.off()


#yes mixing network
exper.yes.mix <- read.csv("../data/exp_network_simul_yesmix.csv", header = F)
exper.yes.mix <- rbind(exper.yes.mix, exp_netwrork_empirical_prop)#adding the properties of the empirical experimental network
exper.yes.mix.pc  <- princomp(exper.yes.mix[,-1], cor = T)
#create colors based on the innovation threshold:
innov.val <- as.numeric(levels(as.factor(exper.yes.mix[, 1])))
colors.pall <- rainbow(length(innov.val))
col.innov <- sapply(1:(nrow(exper.yes.mix)-1), function(x) return(colors.pall[exper.yes.mix[x, 1] == innov.val]))
col.innov <- c(col.innov, "black") #add the color of the empirical network
pdf("../figures/experimenal_networks_yesmix_pca.pdf")
plot(exper.yes.mix.pc$scores[,1], exper.yes.mix.pc$scores[,2], col = col.innov, xlab = "pc1", ylab = "pc2", bty = "l")
points(exper.yes.mix.pc$scores[nrow(exper.yes.mix),1], exper.yes.mix.pc$scores[nrow(exper.yes.mix),2], cex = 2, pch = 3)#mark the position of the empirical network
legend("right", legend = innov.val, fill = colors.pall, x.intersp = 0.01, y.intersp = 0.9, bty = "n", border = NA)
dev.off()

