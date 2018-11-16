#Skript zum Einfügen in R
library("igraph")
el=read.csv("https://raw.githubusercontent.com/hdm-al107/Netzwerke-Gruppe-5/master/Hochschulr%C3%A4te_Edgelist_BEREINIGT_neu.csv")
el
#Nodelist einfügen
nodes=read.csv("https://raw.githubusercontent.com/hdm-al107/Netzwerke-Gruppe-5/master/Hochschulr%C3%A4te_Nodelist_BEREINIGT.csv")
nodes
# Umwnadlung der Listen in Matrixe
head(el)
head(nodes)
# addiert edges auf, wenn sie auf der gleichen Beziehung sind
hties <-as.matrix(el)
hochschulen <- graph_from_data_frame(d=hties, vertices=nodes,
                                     directed=T)
plot(hochschulen)
plot(hochschulen, edge.arrow.size=0.4, vertex.label=NA)
plot(hochschulen, edge.arrow.size=0.4,
     vertex.label=V(hochschulen)=id)

plot(hochschulen, edge.arrow.size=0.4, vertex.label=V(hochschulen)=id)

V(hochschulen)$color = "yellow"
male <- V(hochschulen)[sex == "2"]
V(hochschulen)[male]$color = "deepskyblue"

female <- V(hochschulen)[sex == "1"]
V(hochschulen)[female]$color = "hotpink"

org <- V(hochschulen)[type == "1"]
V(hochschulen)[org]$color = "lawngreen"

hs <- V(hochschulen)[type == "2"]
V(hochschulen)[hs]$color = "khaki1"

plot(hochschulen, vertex.label=NA)

dyad_census(hochschulen)
triad_census(hochschulen)

V(hochschulen)$shape = "circle"
organizations <- V(hochschulen)[type == "1"]
V(hochschulen)[organizations]$shape = "square"

E(hochschulen)$arrow.size <- .2
E(hochschulen)$curved=.2

degree(hochschulen)
#Alle Knoten vom Type 0 mit Anzahl ihrer degrees
d<-degree(hochschulen)[V(hochschulen)$type=="0"]
d
# Knoten mit Type 0 mit den meisten degrees
V(hochschulen)$label[V(hochschulen)$type=="0"][degree(hochschulen)==max(degree(hochschulen))]
#Visualisierung des Knoten Typ 0 mit den meisten degrees
m<-make_ego_graph(hochschulen,order=1,c(("SKN"))) #optional order=2
plot(m[[1]], edge.arrow.size=0.1, edge.label.size=1)
