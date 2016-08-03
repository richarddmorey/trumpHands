# From http://mreed.umtri.umich.edu/mreed/downloads.html#ansur
ansur = read.delim("ansur_men.txt")

# We need to correct Trump's hand length for how the
# measurements were taken
# http://www.hollywoodreporter.com/news/how-small-are-trumps-hands-916593
trump.hand.correction = 1/.945
trump.hand = 187.325 * trump.hand.correction

# Trump's height in mm
# https://www.google.com/#q=donald+trump+height+in+mm
trump.stature = 1880

trump.data = data.frame(HAND_LNTH = trump.hand, STATURE = trump.stature)

lin.regr = lm(HAND_LNTH ~ STATURE, data = ansur)
resid.se = sqrt(deviance(lin.regr)/df.residual(lin.regr))

exp.hand.size = predict.lm(lin.regr, newdata = trump.data)#, interval = "confidence", level = .68)
trump.quantile = pnorm(trump.hand, exp.hand.size, resid.se)

summary(ansur$STATURE)
summary(ansur$HAND_LNTH)
mean(trump.hand > ansur$HAND_LNTH)
mean(trump.stature > ansur$STATURE)


cols1 = c("#edd9c0",
          "#c9d8c5",
          "#a8b6bf",
          "#7d4627",
          "#506C61",
          "#E06C50",
          "#004073")

library(extrafont)
loadfonts()

par.list = list(bg = cols1[7], col = cols1[2], col.axis = cols1[2],
                col.lab = cols1[2], col.main  = cols1[2], col.sub = cols1[2],
                las = 1,
                #lwd = 2,
                cex = 1.3,
                cex.axis = 1.3,
                cex.lab = 1.3,
                yaxs="i",mgp = c(2.5,.5,0), tcl = -0.25,
                mar=c(4.5,4.5,1,1))

par.list2 = par.list
par.list2[['mar']] = c(4.5,1,1,1)

pdf("height.pdf", family="Abel", width=6, height=3, bg = cols1[7], fg = cols1[2], pointsize = 10)

par( par.list2 )

dens = density(ansur$STATURE)
idx = which(diff(sign(dens$x - trump.stature))>0)
y.trump = dens$y[idx]

plot(dens, ty='n',axes=FALSE,main="",ylab="",xlab="Height in ANSUR sample (mm)")
axis(1)
polygon(c(dens$x[1:idx],rev(dens$x[1:idx])),
        c(dens$y[1:idx],rep(0,idx)),
        border=NA,col = cols1[3])
polygon(c(dens$x[-(1:idx)],rev(dens$x[-(1:idx)])),
        c(dens$y[-(1:idx)],rep(0,length(dens$x)-idx)),
        border=NA,col = cols1[6])
abline(v = trump.stature,lwd=2, col = cols1[2])
text(trump.stature, par()$usr[4], "Trump's height", adj=c(1.1,1.2), srt=90,col= cols1[2])


dev.off()


pdf("hand.pdf", family="Abel", width=6, height=3, bg = cols1[7], fg = cols1[2], pointsize = 10)

par( par.list2 )

dens = density(ansur$HAND_LNTH)
idx = which(diff(sign(dens$x - trump.hand))>0)
y.trump = dens$y[idx]

plot(dens, ty='n',axes=FALSE,main="",ylab="",xlab="Hand length in ANSUR sample (mm)")
axis(1)
polygon(c(dens$x[1:idx],rev(dens$x[1:idx])),
        c(dens$y[1:idx],rep(0,idx)),
        border=NA,col = cols1[3])
polygon(c(dens$x[-(1:idx)],rev(dens$x[-(1:idx)])),
        c(dens$y[-(1:idx)],rep(0,length(dens$x)-idx)),
        border=NA,col = cols1[6])
abline(v = trump.hand,lwd=2, col = cols1[2])
text(trump.hand, par()$usr[4], "Trump's hand", adj=c(1.1,1.2), srt=90,col= cols1[2])


dev.off()


pdf("scatter.pdf", family="Abel", width=6, height=3, bg = cols1[7], fg = cols1[2], pointsize = 10)

par( par.list )


plot(ansur$STATURE, ansur$HAND_LNTH, ylab="Hand length (mm)", xlab="Height (mm)", pch=21, col=NA,
     bg = paste0(cols1[3],"66"),cex=.7)

abline(v = trump.stature,lwd=2, col = cols1[1])
text(trump.stature, par()$usr[3], "Trump", adj=c(-.05,1.2), srt=90,col= cols1[1])

abline(h = trump.hand,lwd=2, col = cols1[1])
text(par()$usr[1], trump.hand, "Trump", adj=c(-.05,1.2),col= cols1[1])

points(trump.stature, trump.hand, pch=21, col="black",
       bg = paste0(cols1[6]))

dev.off()


pdf("scatter2.pdf", family="Abel", width=6, height=3, bg = cols1[7], fg = cols1[2], pointsize = 10)

par( par.list )


plot(ansur$STATURE, ansur$HAND_LNTH, ylab="Hand length (mm)", xlab="Height (mm)", pch=21, col=NA,
     bg = paste0(cols1[3],"66"),cex=.7)

abline(lin.regr, col = cols1[1], lty=2, lwd=2)
points(trump.stature, trump.hand, pch=21, col="black",
       bg = paste0(cols1[6]))

dev.off()

pdf("resid1.pdf", family="Abel", width=6, height=3, bg = cols1[7], fg = cols1[2], pointsize = 10)

par( par.list )


plot(ansur$STATURE, lin.regr$residuals, ylab="Deviation (mm)", xlab="Height (mm)", pch=21, col=NA,
     bg = paste0(cols1[3],"66"),cex=.7)

abline(h=0, col = cols1[1], lty=2, lwd=2)

abline(h=c(-1,1)*resid.se, col=cols1[1], lty=3, lwd=2)

abline(v = trump.stature,lwd=2, col = cols1[1])
text(trump.stature, par()$usr[4], "Trump", adj=c(1.1,1.2), srt=90,col= cols1[1])


points(trump.stature, trump.hand - exp.hand.size, pch=21, col="black",
       bg = paste0(cols1[6]))


dev.off()


pdf("predict.pdf", family="Abel", width=6, height=3, bg = cols1[7], fg = cols1[2], pointsize = 10)

par( par.list2 )


plot(0, ty='n',axes=FALSE,main="",ylab="",xlab="Hand length (mm)", 
     xlim = exp.hand.size + 4 * c(-1,1) * resid.se,
     ylim = c(0, dnorm(0,0,resid.se)))
axis(1)

xx = seq(exp.hand.size - 4 * resid.se,
         trump.hand,
         len=400)
polygon(c(xx,rev(xx)),
        c(dnorm(xx,exp.hand.size,resid.se),0*xx),
        border=NA,col = cols1[6])

xx = seq(trump.hand, 
         exp.hand.size + 4 * resid.se,
         len=400)

polygon(c(xx,rev(xx)),
        c(dnorm(xx,exp.hand.size,resid.se),0*xx),
        border=NA,col = cols1[3])

abline(v = trump.hand,lwd=2, col = cols1[2])
text(trump.hand, par()$usr[4], "Trump's hand", adj=c(1.1,-.2), srt=90,col= cols1[2])

text(exp.hand.size,par()$usr[4]/2,paste0(round((1-trump.quantile)*100),"%"),cex=2.5)

dev.off()





