# Description : Getting data for Eugene Fama and plotting his career
# Website : http://timelyportfolio.github.io/research_scholar/

# Fama from Google Scholar
# http://scholar.google.com/citations?user=yP7euFUAAAAJ&hl=en yP7euFUAAAAJ
# French from Google Scholar does not exist
library(scholar)
famaId = "yP7euFUAAAAJ"
fama.df <- compare_scholar_careers(famaId)
plot(fama.df$cites ~ fama.df$year, type = "p")
abline(lm(fama.df$cites ~ fama.df$year), col = "green")
title(main = "Eugene Fama | Cites By Year", adj = 0.05, outer = TRUE, line = -2)

require(devtools)
install_github('rCharts', 'ramnathv')

library(rCharts)
fama.df$date <- paste0("#! new Date(", as.numeric(as.POSIXct(paste0(fama.df$year,
                                                                    "-12-31"))) * 1000, ")!#")
dy1 <- rCharts$new()
dy1$setLib(".")
dy1$templates$script = "chart.html"
dy1$set(data = fama.df, x = "date", y = "cites", chart = list(title = "Eugene Fama - Career in Citations from Google Scholar",
                                                              ylabel = "Citations", showRangeSelector = TRUE, labelsDivStyles = list(background = "none"),
                                                              strokeWidth = 1.5))
cat(noquote(dy1$html(chartId = "dygraphCite")))