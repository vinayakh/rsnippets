# Description : High Resolution graphics with R
# Website : http://lamages.blogspot.in/2013/10/high-resolution-graphics-with-r.html

x <- rnorm(100000)
plot(x, main="100,000 points", col=adjustcolor("black", alpha=0.2))

png("100kPoints72dpi.png", units = "px", width=400, height=400)
plot(x, main="100,000 points", col=adjustcolor("black", alpha=0.2))
dev.off()

png("100kHighRes150dpi.png", units="px", width=400, height=400, res=150)
plot(x, main="100,000 points", col=adjustcolor("black", alpha=0.2))
dev.off()

png("100kHighRes150dpi2.png", units="px", width=800, height=800, res=150)
plot(x, main="100,000 points", col=adjustcolor("black", alpha=0.2))
dev.off()

png("100kHighRes300dpi.png", units="px", width=1600, height=1600, res=300)
plot(x, main="100,000 points", col=adjustcolor("black", alpha=0.2))
dev.off()