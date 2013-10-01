# Description : Making pretty network graphs with sna and igraph
# Website :http://is-r.tumblr.com/post/38240018815/making-prettier-network-graphs-with-sna-and-igraph

doInstall <- TRUE
toInstall <- c("sna", "igraph")
if(doInstall){install.packages(toInstall, repos = "http://cran.us.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

adjacencyList <- read.csv("data/Twitter_network.R.csv")
head(adjacencyList)

adjacencyMatrix <- table(adjacencyList)
as.matrix(sort(rowSums(adjacencyMatrix))) # Out-degree
as.matrix(sort(colSums(adjacencyMatrix))) # In-degree

# To calculate PageRank, we need an igraph Object
graphObject <- graph.adjacency(adjacencyMatrix)
plot(graphObject) # This plot is not pretty.

pageRank <- page.rank(graphObject)$vector
as.matrix(sort(pageRank))
# isDotR has the highest pageRank, because this is basically its ego network
# Note thate pageRank is pretty similar to eigenvector centrality:
plot(pageRank, evcent(graphObject, directed = TRUE)$vector)

# Now, to make the prettiest graph we can:
png("Twitter_graph.png", h = 800, w = 800, type = "cairo-png")
par(mai = c(0, 0, 0, 0))
pageRankColor <- hsv(0, 1, (pageRank - min(pageRank)) /
                       (max(pageRank) - min(pageRank)))
pageRankScaler <- pageRank * 10 + 1/2
prettyPlot <- gplot(dat = adjacencyMatrix,
                    label = rownames(adjacencyMatrix),
                    mode = "kamadakawai",
                    pad = 0,
                    label.pad = 1,
                    boxed.labels = TRUE,
                    label.pos = 1, # Below vertex
                    label.bg = "#ffffff99",
                    vertex.sides = 100, # Basically circular
                    arrowhead.cex = pageRankScaler,
                    label.cex = pageRankScaler,
                    vertex.cex = pageRankScaler,
                    edge.col = "#00000011", # To make translucent bounding box
                    label.col = pageRankColor,
                    vertex.col = pageRankColor,
                    label.border = "#ffffff00", # To hide borders
                    vertex.border = "#ffffff00") # To hide borders
dev.off()