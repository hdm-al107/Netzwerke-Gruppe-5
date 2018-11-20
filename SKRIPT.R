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
plot(hochschulen)

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

#Anzeigen des Netzwerks 
plot(hochschulen, vertex.label=NA)

#Entzerren 
coords <- layout_with_kk(hochschulen)*0.2
plot(hochschulen, edge.arrow.size=.1, layout=coords, rescale=FALSE, edge.color="grey80", edge.arrow.size=.1, 
     vertex.frame.color="transparent", 
     edge.color="grey80",
     vertex.label.dist=0.2,
     vertex.label.cex=.55,
     vertex.size=9,
     vertex.label.color="black",
     ylim=c(-2,3), xlim=c(-2,3))

###Power Player 

##Degrees 

#Auflistung aller Knoten mit Type 0 (Personen) und ihrer Degrees 
degree(hochschulen)
d <- degree(hochschulen)[V(hochschulen)$type=="0"]
d

#Anzeige des Knotens mit Type 0 (Person) mit der höchsten Anzahl an degrees 
V(hochschulen)$label[V(hochschulen)$type=="0"][degree(hochschulen)==max(degree(hochschulen))]

#Erstellung des Ego-Netzwerks unserer 6 Power Player 
WB <- make_ego_graph(hochschulen, order = 2, c("WB"))
plot(WB[[1]], main="Wilhelm Bauer", edge.label.size=.1,)

m <- make_ego_graph(hochschulen, order = 2, c("RR"))
plot(m[[1]], edge.label.size=.1 )

m <- make_ego_graph(hochschulen, order = 2, c("SF"))
plot(m[[1]], edge.label.size=.1 )

m <- make_ego_graph(hochschulen, order = 2, c("EE"))
plot(m[[1]], edge.label.size=.1 )

m <- make_ego_graph(hochschulen, order = 2, c("BEBN"))
plot(m[[1]], edge.label.size=.1 )

m <- make_ego_graph(hochschulen, order = 2, c("GWN"))
plot(m[[1]], edge.label.size=.1 )

m <- make_ego_graph(hochschulen, order = 2, c("GHB"))
plot(m[[1]], edge.label.size=.1 )



###Power Organisationen 

##Degrees 

#Auflistung aller Knoten mit Type 1 (Organisation) und ihrer Degrees 
degree(hochschulen)
d <- degree(hochschulen)[V(hochschulen)$type=="1"]
d

#Anzeige des Knotens mit Type 1 (Organisation) mit der höchsten Anzahl an degrees 
V(hochschulen)$label[V(hochschulen)$type=="1"][degree(hochschulen)==max(degree(hochschulen))]

#Erstellung des Ego-Netzwerks unserer 4 Power Organisationen 
m <- make_ego_graph(hochschulen, order = 3, c("IH"))
plot(m[[1]], edge.label.size=.1 )

m <- make_ego_graph(hochschulen, order = 3, c("DFG"))
plot(m[[1]], edge.label.size=.1 )

m <- make_ego_graph(hochschulen, order = 3, c("RC"))
plot(m[[1]], edge.label.size=.1 )

m <- make_ego_graph(hochschulen, order = 3, c("SPD"))
plot(m[[1]], edge.label.size=.1 )

############################################################################################################

###Verschiedene Hochschularten betrachten 

##Teilnetzwerke erstellen

#Pädagogische Hochschulen 
vertex.attributes(hochschulen)
vertex.attributes(hochschulen)$art

PH <- subgraph.edges(hochschulen, V(hochschulen)[art == 3])
PH
vertex.attributes(PH)$art
plot(PH,  main="PHs")

#Kunsthochschulen
vertex.attributes(hochschulen)
vertex.attributes(hochschulen)$art

KH <- subgraph.edges(hochschulen, V(hochschulen)[art == 1])
KH
vertex.attributes(KH)$art
plot(KH,  main="KHs")

#Allgemeine Hochschulen 
vertex.attributes(hochschulen)
vertex.attributes(hochschulen)$art

AH <- subgraph.edges(hochschulen, V(hochschulen)[art == 1])
AH
vertex.attributes(AH)$art
plot(AH,  main="AHs")

#Technichsche Hochschulen
vertex.attributes(hochschulen)
vertex.attributes(hochschulen)$art

TH <- subgraph.edges(hochschulen, V(hochschulen)[art == 2])
TH
vertex.attributes(TH)$art
plot(TH,  main="THs")

##Teilnetzwerke vergeleichen 
par(mfrow=c(1,2), mar=c(0,0,2,0))
plot(AH, layout = layout_with_fr, edge.arrow.size=1, main="AH")
plot(TH, layout = layout_with_fr, edge.arrow.size=1, main="TH")
plot(KH, layout = layout_with_fr, edge.arrow.size=1, main="KH")
plot(PH, layout = layout_with_fr, edge.arrow.size=1, main="PH")

###Selektion nach Geschlecht

##Frauen
h_fem <- delete_vertices(hochschulen, V(hochschulen)[sex == "2"])
h_fem
plot(h_fem, layout = layout_with_kk, main="Frauen")

##Männer
h_male <- delete_vertices(help, V(help)[sex == "1"])
h_male
plot(h_male, layout = layout_with_kk, main="Männer")

