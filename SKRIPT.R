library("igraph")

###Listen in R einfügen 

#Edgelist einfügen
el=read.csv("https://raw.githubusercontent.com/hdm-al107/Netzwerke-Gruppe-5/master/Edge.csv")

#Nodelist einfügen
nodes=read.csv("https://raw.githubusercontent.com/hdm-al107/Netzwerke-Gruppe-5/master/Node.csv")

# Umwnadlung der Listen in Matrixe
head(el)
head(nodes)

# addiert edges auf, wenn sie auf der gleichen Beziehung sind
hties <-as.matrix(el)
hochschulen <- graph_from_data_frame(d=hties, vertices=nodes,
                                     directed=F)

###Visualisierung des gesamten Netzwerks 

#Hochschulen sollen gelb angeigt werden 
V(hochschulen)$color = "yellow"

#Männer sollen blau angezeigt werden 
male <- V(hochschulen)[sex == "2"]
V(hochschulen)[male]$color = "deepskyblue"

#Frauen sollen pink angezeigt werden 
female <- V(hochschulen)[sex == "1"]
V(hochschulen)[female]$color = "hotpink"

#Organisationen sollen grün angezeigt werden 
org <- V(hochschulen)[type == "1"]
V(hochschulen)[org]$color = "lawngreen"

#Hochscnulen sollen die Form eines Kreises, Organisationen die Form eines Quadrats haben 
V(hochschulen)$shape = "circle"
organizations <- V(hochschulen)[type == "1"]
V(hochschulen)[organizations]$shape = "square"

#Die Kanten sollen geschwungen sein
E(hochschulen)$curved=.2


#Entzerren 
coords <- layout_with_kk(hochschulen)*0.2

#Gesamtes Netzwerk anzeigen 
plot(hochschulen,
     vertex.label=NA,
     layout=coords, 
     rescale=FALSE, 
     edge.color="grey80",
     vertex.frame.color="transparent", 
     edge.color="grey80",
     vertex.size=4,
     ylim=c(-4,3), xlim=c(-4,3)) 

###Zentralitätsmaße

edge_density(hochschulen, loops = FALSE)
diameter(hochschulen)
components(hochschulen)

############################################################################################################

###Power Player 

##Degrees 

#Auflistung aller Knoten mit Type 0 (Personen) und ihrer Degrees 
degree(hochschulen)
d <- degree(hochschulen)[V(hochschulen)$type=="0"]
d

#Anzeige des Knotens mit Type 0 (Person) mit der höchsten Anzahl an degrees 
V(hochschulen)$label[V(hochschulen)$type=="0"][degree(hochschulen)==max(degree(hochschulen))]

#Erstellung des Ego-Netzwerks unserer 6 Power Player 
WB <- make_ego_graph(hochschulen, order = 2, c("WB"),)
coords <- layout_with_kk(WB[[1]])*0.1
plot(WB[[1]],
     layout=coords, 
     edge.color="grey80", 
     vertex.frame.color="transparent", 
     edge.color="grey80",
     vertex.label.dist=0.2,
     vertex.label.cex=.55,
     vertex.size=9,
     vertex.label.color="black",
     main="Wilhelm Bauer",)

edge_density(WB[[1]], loops = FALSE)
diameter(WB[[1]])


RR <- make_ego_graph(hochschulen, order = 2, c("RR"))
plot(RR[[1]],
     edge.color="grey80", 
     vertex.frame.color="transparent", 
     edge.color="grey80",
     vertex.label.dist=0.2,
     vertex.label.cex=.55,
     vertex.size=9,
     vertex.label.color="black",
     main="Renate Rastaetter",)

edge_density(RR[[1]], loops = FALSE)
diameter(RR[[1]])

SF <- make_ego_graph(hochschulen, order = 2, c("SF"))
plot(SF[[1]],
     edge.color="grey80", 
     vertex.frame.color="transparent", 
     edge.color="grey80",
     vertex.label.dist=0.2,
     vertex.label.cex=.55,
     vertex.size=9,
     vertex.label.color="black",
     main="Stephan Frucht",)

edge_density(SF[[1]], loops = FALSE)
diameter(SF[[1]])

EE <- make_ego_graph(hochschulen, order = 2, c("EE"))
plot(EE[[1]],
     edge.color="grey80", 
     vertex.frame.color="transparent", 
     edge.color="grey80",
     vertex.label.dist=0.2,
     vertex.label.cex=.55,
     vertex.size=9,
     vertex.label.color="black",
     main="Eva Eckkrammer",)

edge_density(EE[[1]], loops = FALSE)
diameter(EE[[1]])

BEBN <- make_ego_graph(hochschulen, order = 2, c("BEBN"))
coords <- layout_with_kk(BEBN[[1]])*0.1
plot(BEBN[[1]],
     layout=coords,
     edge.color="grey80", 
     vertex.frame.color="transparent", 
     edge.color="grey80",
     vertex.label.dist=0.2,
     vertex.label.cex=.45,
     vertex.size=7,
     vertex.label.color="black",
     main="Barbara Ettinger-Brinckmann",)

edge_density(BEBN[[1]], loops = FALSE)
diameter(BEBN[[1]])


GWN <- make_ego_graph(hochschulen, order = 2, c("GWN"))
coords <- layout_with_kk(GWN[[1]])*0.1
plot(GWN[[1]],
     layout=coords,
     edge.color="grey80", 
     vertex.frame.color="transparent", 
     edge.color="grey80",
     vertex.label.dist=0.5,
     vertex.label.cex=.55,
     vertex.size=9,
     vertex.label.color="black",
     main="Gerd Weisskirchen",)

edge_density(GWN[[1]], loops = FALSE)
diameter(GWN[[1]])

GHB <- make_ego_graph(hochschulen, order = 2, c("GHB"))
plot(GHB[[1]],
     edge.color="grey80", 
     vertex.frame.color="transparent", 
     edge.color="grey80",
     vertex.label.dist=0.2,
     vertex.label.cex=.55,
     vertex.size=9,
     vertex.label.color="black",
     main="Gudrun Heute-Bluhm",)

edge_density(GHB[[1]], loops = FALSE)
diameter(GHB[[1]])

############################################################################################################

###Power Organisationen 

##Degrees 

#Auflistung aller Knoten mit Type 1 (Organisation) und ihrer Degrees 
degree(hochschulen)
d <- degree(hochschulen)[V(hochschulen)$type=="1"]
d

#Erstellung des Ego-Netzwerks unserer 4 Power Organisationen 
IH <- make_ego_graph(hochschulen, order = 3, c("IH"))
coords <- layout_with_kk(IH[[1]])*0.2
plot(IH[[1]],
     layout=coords,
     edge.color="grey80", 
     vertex.frame.color="transparent", 
     edge.color="grey80",
     vertex.label.dist=0.2,
     vertex.label.cex=.55,
     vertex.size=9,
     vertex.label.color="black",
     main="Industrie- und Handelskammer",)

edge_density(IH[[1]], loops = FALSE)
diameter(IH[[1]])
centr_betw(IH[[1]])
centralization.closeness(IH[[1]])
centr_eigen(IH[[1]])


DFG <- make_ego_graph(hochschulen, order = 3, c("DFG"))
coords <- layout_with_kk(DFG[[1]])*0.4
plot(DFG[[1]],
     layout=coords,
     edge.color="grey80", 
     vertex.frame.color="transparent", 
     edge.color="grey80",
     vertex.label.dist=0.4,
     vertex.label.cex=.55,
     vertex.size=9,
     vertex.label.color="black",
     main="Deutsche Forschungsgemeindschaft",)

edge_density(DFG[[1]], loops = FALSE)
diameter(DFG[[1]])
centr_betw(DFG[[1]])
centralization.closeness(DFG[[1]])
centr_eigen(DFG[[1]])

RC <- make_ego_graph(hochschulen, order = 3, c("RC"))
coords <- layout_with_kk(RC[[1]])*1
plot(RC[[1]],
     layout=coords,
     edge.color="grey80", 
     vertex.frame.color="transparent", 
     edge.color="grey80",
     vertex.label.dist=0.2,
     vertex.label.cex=.35,
     vertex.size=6,
     vertex.label.color="black",
     main="Rotary Club",)

edge_density(RC[[1]], loops = FALSE)
diameter(RC[[1]])
centr_betw(RC[[1]])
centralization.closeness(RC[[1]])
centr_eigen(RC[[1]])

SPD <- make_ego_graph(hochschulen, order = 3, c("SPD"))
coords <- layout_with_kk(SPD[[1]])*0.6
plot(SPD[[1]],
     layout=coords,
     edge.color="grey80", 
     vertex.frame.color="transparent", 
     edge.color="grey80",
     vertex.label.dist=0.4,
     vertex.label.cex=.55,
     vertex.size=9,
     vertex.label.color="black",
     main="SPD",)

edge_density(SPD[[1]], loops = FALSE)
diameter(SPD[[1]])
centr_betw(SPD[[1]])
centralization.closeness(SPD[[1]])
centr_eigen(SPD[[1]])

############################################################################################################


###Selektion nach Geschlecht

##Frauen
h_fem <- delete_vertices(hochschulen, V(hochschulen)[sex == "2", type == "1"])
h_fem
plot(h_fem,
     vertex.label=NA,
     edge.color="grey80", 
     vertex.frame.color="transparent", 
     edge.color="grey80",
     vertex.size=6,
     main="Frauen",)

##Männer
h_male <- delete_vertices(hochschulen, V(hochschulen)[sex == "1", type == "1"])
h_male
plot(h_male,
     vertex.label=NA,
     edge.color="grey80", 
     vertex.frame.color="transparent", 
     edge.color="grey80",
     vertex.size=6,
     main="Männer",)

############################################################################################################

###Verschiedene Hochschularten betrachten 

##Technik & Umwelt Hochschulen

#Listen in R einfügen

library("igraph")

el=read.csv("https://raw.githubusercontent.com/hdm-al107/Netzwerke-Gruppe-5/master/Edge_TechnischeHS.csv")
nodes=read.csv("https://raw.githubusercontent.com/hdm-al107/Netzwerke-Gruppe-5/master/Node_TechnischeHS.csv")

head(el)
head(nodes)

hties <-as.matrix(el)
TechnischeHS <- graph_from_data_frame(d=hties, vertices=nodes,
                                      directed=F)

#Visualisierung

V(TechnischeHS)$color = "yellow"
male <- V(TechnischeHS)[sex == "2"]
V(TechnischeHS)[male]$color = "deepskyblue"
female <- V(TechnischeHS)[sex == "1"]
V(TechnischeHS)[female]$color = "hotpink"
organizations <- V(TechnischeHS)[type == "1"]
V(TechnischeHS)[organizations]$color = "lawngreen"
V(TechnischeHS)$shape = "circle"
V(TechnischeHS)[organizations]$shape = "square"
coords <- layout_with_kk(TechnischeHS)*0.9
plot(TechnischeHS, 
     layout=coords, 
     rescale=FALSE, 
     edge.color="grey80",
     vertex.frame.color="transparent", 
     edge.color="grey80",
     vertex.label.dist=0.2,
     vertex.label.cex=.55,
     vertex.size=25,
     vertex.label.color="black",
     main="Technische Hochschulen",
     ylim=c(-9,3), xlim=c(-3,3))

edge_density(TechnischeHS, loops = FALSE)
diameter(TechnischeHS)
components(TechnischeHS)


##Pädagogische Hochschulen

#Listen in R einfügen

library("igraph")

el=read.csv("https://raw.githubusercontent.com/hdm-al107/Netzwerke-Gruppe-5/master/Edge_PaedagogischeHS.csv")
nodes=read.csv("https://raw.githubusercontent.com/hdm-al107/Netzwerke-Gruppe-5/master/Node_PaedagogischeHS.csv")

head(el)
head(nodes)

hties <-as.matrix(el)
PaedagogischeHS <- graph_from_data_frame(d=hties, vertices=nodes,
                                         directed=F)

#Visualisierung

V(PaedagogischeHS)$color = "yellow"
male <- V(PaedagogischeHS)[sex == "2"]
V(PaedagogischeHS)[male]$color = "deepskyblue"
female <- V(PaedagogischeHS)[sex == "1"]
V(PaedagogischeHS)[female]$color = "hotpink"
organizations <- V(PaedagogischeHS)[type == "1"]
V(PaedagogischeHS)[organizations]$color = "lawngreen"
V(PaedagogischeHS)$shape = "circle"
V(PaedagogischeHS)[organizations]$shape = "square"
coords <- layout_with_kk(PaedagogischeHS)*0.5
E(PaedagogischeHS)$curved=.2
plot(PaedagogischeHS, 
     layout=coords, 
     rescale=FALSE, 
     edge.color="grey80",
     vertex.frame.color="transparent", 
     edge.color="grey80",
     vertex.label.dist=0.2,
     vertex.label.cex=.35,
     vertex.size=18,
     vertex.label.color="black",
     main="Pädagogische Hochschulen",
     ylim=c(-4,3), xlim=c(-2,3))

edge_density(PaedagogischeHS, loops = FALSE)
diameter(PaedagogischeHS)
components(PaedagogischeHS)


##Kunst Hochschulen

#Listen in R einfügen

library("igraph")

el=read.csv("https://raw.githubusercontent.com/hdm-al107/Netzwerke-Gruppe-5/master/Edge_KunstHS.csv")
nodes=read.csv("https://raw.githubusercontent.com/hdm-al107/Netzwerke-Gruppe-5/master/Node_KunstHS.csv")

head(el)
head(nodes)

hties <-as.matrix(el)
KunstHS <- graph_from_data_frame(d=hties, vertices=nodes,
                                 directed=F)

#Visualisierung

V(KunstHS)$color = "yellow"
male <- V(KunstHS)[sex == "2"]
V(KunstHS)[male]$color = "deepskyblue"
female <- V(KunstHS)[sex == "1"]
V(KunstHS)[female]$color = "hotpink"
organizations <- V(KunstHS)[type == "1"]
V(KunstHS)[organizations]$color = "lawngreen"
V(KunstHS)$shape = "circle"
V(KunstHS)[organizations]$shape = "square"
coords <- layout_with_kk(KunstHS)*0.4
plot(KunstHS, 
     layout=coords, 
     rescale=FALSE, 
     edge.color="grey80",
     vertex.frame.color="transparent", 
     edge.color="grey80",
     vertex.label.dist=0.2,
     vertex.label.cex=.55,
     vertex.size=18,
     vertex.label.color="black",
     main="Kunst Hochschulen",
     ylim=c(-2,3), xlim=c(-5,3))

edge_density(KunstHS, loops = FALSE)
diameter(KunstHS)
components(KunstHS)
