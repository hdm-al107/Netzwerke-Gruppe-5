#Skript zum Einfügen in R
library("igraph")
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

plot(hochschulen, edge.arrow.size=0.4)

V(hochschulen)$color = "yellow"
plot(hochschulen)
male <- V(hochschulen)[sex == "2"]
V(hochschulen)[male]$color = "deepskyblue"

female <- V(hochschulen)[sex == "1"]
V(hochschulen)[female]$color = "hotpink"

org <- V(hochschulen)[type == "1"]
V(hochschulen)[org]$color = "lawngreen"


plot(hochschulen, vertex.label=NA)


V(hochschulen)$shape = "circle"
organizations <- V(hochschulen)[type == "1"]
V(hochschulen)[organizations]$shape = "square"

E(hochschulen)$arrow.size <- .2
E(hochschulen)$curved=.2



degree(hochschulen)
d <- degree(hochschulen)[V(hochschulen)$type=="0"]
d
V(hochschulen)$label[V(hochschulen)$type=="0"][degree(hochschulen)==max(degree(hochschulen))]
m <- make_ego_graph(hochschulen, order = 2, c("WB"))
plot(m[[1]], edge.arrow.size=0.1, edge.label.size=.1 )



degree(hochschulen)
d <- degree(hochschulen)[V(hochschulen)$type=="1"]
d
m <- make_ego_graph(hochschulen, order = 2, c("DB"))
plot(m[[1]], edge.arrow.size=0.1, edge.label.size=.1 )


d <- degree(hochschulen)[V(hochschulen)$type=="1"]
m <- make_ego_graph(hochschulen, order = 3, c("DFG"))
plot(m[[1]], edge.arrow.size=0.1, edge.label.size=.1 )

