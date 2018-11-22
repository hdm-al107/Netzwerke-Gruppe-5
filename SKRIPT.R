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
     layout=coords, 
     rescale=FALSE, 
     edge.color="grey80",
     vertex.frame.color="transparent", 
     edge.color="grey80",
     vertex.label.dist=0.2,
     vertex.label.cex=.55,
     vertex.size=9,
     vertex.label.color="black",
     ylim=c(-4,3), xlim=c(-4,3)) # Drei Netzwerke befinden sich in keinerlei Kontakt mit dem Rest, deshalb werden sie entfernt

### [Einpflegen des verkleinerten Skripts]

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

BEBN <- make_ego_graph(hochschulen, order = 2, c("BEBN"))
coords <- layout_with_kk(BEBN[[1]])*0.1
plot(BEBN[[1]],
     layout=coords,
     edge.color="grey80", 
     vertex.frame.color="transparent", 
     edge.color="grey80",
     vertex.label.dist=0.2,
     vertex.label.cex=.55,
     vertex.size=9,
     vertex.label.color="black",
     main="Barbara Ettinger-Brinckmann",)

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

RC <- make_ego_graph(hochschulen, order = 3, c("RC"))
coords <- layout_with_kk(RC[[1]])*0.3
plot(RC[[1]],
     layout=coords,
     edge.color="grey80", 
     vertex.frame.color="transparent", 
     edge.color="grey80",
     vertex.label.dist=0.2,
     vertex.label.cex=.55,
     vertex.size=9,
     vertex.label.color="black",
     main="Rotary Club",)

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
     vertex.size=9,
     main="Frauen",)

##Männer
h_male <- delete_vertices(hochschulen, V(hochschulen)[sex == "1", type == "1"])
h_male
plot(h_male,
     vertex.label=NA,
     edge.color="grey80", 
     vertex.frame.color="transparent", 
     edge.color="grey80",
     vertex.size=9,
     main="Männer",)

###Verschiedene Hochschularten betrachten 

##Allgemeine Hochschulen 

#Listen in R einfügen 

library("igraph")

el=read.csv("https://raw.githubusercontent.com/hdm-al107/Netzwerke-Gruppe-5/master/Edge_AllgemeineHS.csv")
nodes=read.csv("https://raw.githubusercontent.com/hdm-al107/Netzwerke-Gruppe-5/master/Node_AllgemeineHS.csv")

head(el)
head(nodes)

hties <-as.matrix(el)
AllgemeineHS <- graph_from_data_frame(d=hties, vertices=nodes,
                                     directed=F)

#Visualisierung 

V(AllgemeineHS )$color = "yellow"
male <- V(AllgemeineHS )[sex == "2"]
V(AllgemeineHS )[male]$color = "deepskyblue"
female <- V(AllgemeineHS )[sex == "1"]
V(AllgemeineHS )[female]$color = "hotpink"
org <- V(AllgemeineHS )[type == "1"]
V(AllgemeineHS )[org]$color = "lawngreen"
V(AllgemeineHS )$shape = "circle"
organizations <- V(AllgemeineHS )[type == "1"]
V(AllgemeineHS )[organizations]$shape = "square"
coords <- layout_with_kk(AllgemeineHS )*0.25
plot(AllgemeineHS , 
     layout=coords, 
     rescale=FALSE, 
     edge.color="grey80",
     vertex.frame.color="transparent", 
     edge.color="grey80",
     vertex.label.dist=0.2,
     vertex.label.cex=.55,
     vertex.size=8,
     vertex.label.color="black",
     ylim=c(-2,3), xlim=c(-3,3))

##Technische Hochschulen

#Listen in R einfügen

library("igraph")

el=read.csv("https://raw.githubusercontent.com/hdm-al107/Netzwerke-Gruppe-5/master/Edge_TechnischeHS.csv")
nodes=read.csv("https://raw.githubusercontent.com/hdm-al107/Netzwerke-Gruppe-5/master/Node_TechnischeHS.csv")

head(el)
head(nodes)

hties <-as.matrix(el)
TechnischeHS <- graph_from_data_frame(d=hties, vertices=nodes,
                                      directed=F)

###Visualisierung

V(TechnischeHS)$color = "yellow"
male <- V(TechnischeHS)[sex == "2"]
V(TechnischeHS)[male]$color = "deepskyblue"
female <- V(TechnischeHS)[sex == "1"]
V(TechnischeHS)[female]$color = "hotpink"
org <- V(TechnischeHS)[type == "1"]
V(TechnischeHS)[org]$color = "lawngreen"
V(TechnischeHS)$shape = "circle"
organizations <- V(TechnischeHS)[type == "1"]
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

###Visualisierung

V(PaedagogischeHS)$color = "yellow"
male <- V(PaedagogischeHS)[sex == "2"]
V(PaedagogischeHS)[male]$color = "deepskyblue"
female <- V(PaedagogischeHS)[sex == "1"]
V(PaedagogischeHS)[female]$color = "hotpink"
org <- V(PaedagogischeHS)[type == "1"]
V(PaedagogischeHS)[org]$color = "lawngreen"
V(PaedagogischeHS)$shape = "circle"
organizations <- V(PaedagogischeHS)[type == "1"]
V(PaedagogischeHS)[organizations]$shape = "square"
coords <- layout_with_kk(PaedagogischeHS)*0.4
plot(PaedagogischeHS, 
     layout=coords, 
     rescale=FALSE, 
     edge.color="grey80",
     vertex.frame.color="transparent", 
     edge.color="grey80",
     vertex.label.dist=0.2,
     vertex.label.cex=.55,
     vertex.size=18,
     vertex.label.color="black",
     main="Pädagogische Hochschulen",
     ylim=c(-3,3), xlim=c(-2,3))

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

###Visualisierung

V(KunstHS)$color = "yellow"
male <- V(KunstHS)[sex == "2"]
V(KunstHS)[male]$color = "deepskyblue"
female <- V(KunstHS)[sex == "1"]
V(KunstHS)[female]$color = "hotpink"
org <- V(KunstHS)[type == "1"]
V(KunstHS)[org]$color = "lawngreen"
V(KunstHS)$shape = "circle"
organizations <- V(KunstHS)[type == "1"]
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
     ylim=c(-3,3), xlim=c(-2,3))

