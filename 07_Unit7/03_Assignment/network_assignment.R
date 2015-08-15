rm (list = ls())
edges = read.csv("edges.csv")
users = read.csv("users.csv")
#problem1
str(edges)
str(users)
2 * sum((table(edges$V1)))/ nrow(users)
table(users$locale,users$school )
table(users$gender,users$school )
#problem2
library(igraph)
?graph.data.frame
g = graph.data.frame(edges, FALSE, users)
plot(g, vertex.size=5, vertex.label=NA)
sum(degree(g) >= 10)
V(g)$size = degree(g)/2+2
plot(g, vertex.label=NA)
max(V(g)$size)
min(V(g)$size)
summary(V(g)$size)
#problem3
V(g)$color = "black"
V(g)$color[V(g)$gender == "A"] = "red"
V(g)$color[V(g)$gender == "B"] = "gray"
plot(g, vertex.label=NA)

V(g)$color = "black"
V(g)$color[V(g)$school == "A"] = "red"
V(g)$color[V(g)$school == "AB"] = "gray"
plot(g, vertex.label=NA)
V(g)$color = "black"
V(g)$color[V(g)$locale == "A"] = "red"
V(g)$color[V(g)$locale == "B"] = "gray"
plot(g, vertex.label=NA)

#problem4
?igraph.plotting
library(rgl)
rglplot(g, vertex.label=NA)
plot(g, edge.width=2, vertex.label=NA)
