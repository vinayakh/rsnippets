# Description : Generating own Latex table using tikzDevice
# Website : http://freakonometrics.hypotheses.org/9404

doInstall <- TRUE
toInstall <- c("xtable","tikzdevice")
if(doInstall){install.packages(toInstall, repos = "http://cran.us.r-project.org")}
lapply(toInstall, library, character.only = TRUE)

u=seq(0,3.09,by=0.01)
p=pnorm(u)
m=matrix(p,ncol=10,byrow=TRUE)
options(digits=4)
newm=xtable(m,digits=4)
print.xtable(newm, type="latex", file="nor1.tex")
options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}",
                                 "\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}", "\\usepackage{amssymb}"))
  tikz("normal-dist.tex", width = 8, height = 4, 
  standAlone = TRUE,
  packages = c("\\usepackage{tikz}",
  "\\usepackage[active,tightpage,psfixbb]{preview}",
  "\\PreviewEnvironment{pgfpicture}",
  "\\setlength\\PreviewBorder{0pt}",
  "\\usepackage{amssymb}"))
u=seq(-3,3,by=.01)
plot(u,dnorm(u),type="l",axes=FALSE,xlab="",ylab="",col="white")
axis(1)
I=which((u<=1))
polygon(c(u[I],rev(u[I])),c(dnorm(u)[I],rep(0,length(I))),col="red",border=NA)
lines(u,dnorm(u),lwd=2,col="blue")
text(-1.5, dnorm(-1.5)+.17, "$\\textcolor{blue}{X\\sim\\mathcal{N}(0,1)}$", cex = 1.5)
text(1.75, dnorm(1.75)+.25, 
       "$\\textcolor{red}{\\mathbb{P}(X\\leq x)=\\displaystyle{
       \\int_{-\\infty}^x \\varphi(t)dt}}$", cex = 1.5)
dev.off()