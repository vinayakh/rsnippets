# Description : Grouped Bar charts in R
# Website : http://rud.is/b/2013/10/27/alternative-to-grouped-bar-charts-in-r/

doInstall <- TRUE # Change to FALSE if you don't want packages installed.
toInstall <- c("ggplot2")
if(doInstall){install.packages(toInstall, repos = "http://cran.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

nosql.df <- read.csv("data/nosql.csv", header=TRUE)
nosql.df$Database <- factor(nosql.df$Database, levels=c("MongoDB","Cassandra","Redis","HBase","CouchDB", "Neo4j","Riak","MarkLogic","Couchbase","DynamoDB"))

gg <- ggplot(data=nosql.df, aes(x=Quarter, y=Index))
gg <- gg + geom_point(aes(color=Quarter), size=3)
gg <- gg + facet_grid(Database~.)
gg <- gg + coord_flip()
gg <- gg + theme_bw()
gg <- gg + labs(x="", title="NoSQL LinkedIn Skills Index\nSeptember 2013")
gg <- gg + theme(legend.position = "none")
gg <- gg + theme(strip.text.x = element_blank())
gg
