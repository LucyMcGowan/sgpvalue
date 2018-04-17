#' Fig 3. Display of 95% confidence intervals for gene specific fold-changes
#' (AML vs. ALL) in the gene expression levels of patients from the Leukemia 
#' Microarray study [23].
#' All 7128 genes are sorted on the x-axis by classical p-value rank. Interval null
#' hypothesis (blue-grey zone) shows all absolute fold changes between ½ and 2. 
#' Red genes have a second-generation p-value of 0, blue genes do not. Vertical 
#' dashed lines show various traditional p-value cutoffs at the 0.05 level 
#' (Bonferroni, Benjamini-Hochberg false discovery rate, and unadjusted).

#' Authors: JDB and LDM
library(sgpvalue)
rep <- dim(leukdata)[[1]]
gene <- seq(1, rep, 1)

## T-statistics
t.list <- apply(leukdata, 1, 
                function(x) {
                  t.test(x[48:72], x[1:47], var.equal = TRUE)$statistic
                }
)
t.pv <- apply(leukdata, 1,
              function(x) {
                t.test(x[48:72], x[1:47], var.equal = TRUE)$p.value
              }
)  
t.cil <- apply(leukdata, 1, 
               function(x) {
                 t.test(x[48:72], x[1:47], var.equal = TRUE)$conf.int[1]
               }
) 
t.ciu <- apply(leukdata, 1,
               function(x) {
                 t.test(x[48:72], x[1:47], var.equal = TRUE)$conf.int[2]
               }
)
m.diff <- rowMeans(leukdata[, 48:72]) - rowMeans(leukdata[, 1:47])


## Tail Area probabilities
pv <- sort(2 * (1 - pt(abs(t.list), df = 70)))
ord <- order(2 * (1 - pt(abs(t.list), df = 70)))
pv.bon <- pmin(length(pv) * pv, 1)
pv.bh <- length(pv) * pv / seq(1:length(pv))

zone.l <- -.3
zone.u <- .3
upper <- 7128
idz.col <- rgb(208, 216, 232, max = 255)
gene <- rank(2 * (1 - pt(abs(t.list), df = 70)), ties.method = "random")

gene <- seq(1, rep, 1)
gene <- rank(2 * (1 - pt(abs(t.list), df = 70)), ties.method = "random")

plot(gene, 
     t.ciu,
     ylim = c(-3, 3),
     type = "n",
     ylab = expression("Fold-change (AML vs. ALL)", sep = ""),
     xlim = c(0, upper), 
     xlab = expression("Genes, order by classical " * italic(p) * "-value", sep = ""),
     yaxt="n"
) ## FC is AML to ALL 

axis(side = 2,
     at = round(log(c(1/1000, 1/100, 1/10, 1/2, 1, 2, 10, 100, 1000), base = 10), 2),
     labels = c("1/1000", "1/100", "1/10", "1/2", 1, 2, 10, 100, 1000), las = 2)

title(main = ("Leukemia Gene Expressions"))
rect(1, log(1/2, base = 10), upper, log(2, base = 10), col = idz.col, border = NA)
points(gene, t.cil, cex = 0.6, pch = 16, col = "cornflowerblue")
points(gene, t.ciu, cex = 0.6, pch = 16, col = "cornflowerblue")
segments(gene, t.cil, gene, t.ciu, lty = 1, col = "cornflowerblue")
abline(h = 0, lty = 2)

gene.list <- 0
ct <- 0

if (zone.l * zone.u != 0) {
  segments(gene, zone.u, gene, zone.u, col = c("white"), lty = 1, lwd = 1.5)  # used white instead of idz.col
  segments(gene, zone.l, gene, zone.l, col = c("white"), lty = 1, lwd = 1.5)  # used white instead of idz.col
}


for (i in 1:rep) {
  
  if (t.ciu[i] < zone.l | t.cil[i] > zone.u) {
    segments(gene[i], t.ciu[i], gene[i], t.cil[i], lty = 1, col = "firebrick3", lwd = 2) 
    points(gene[i], t.ciu[i], cex = 0.6, pch = 16, col = "firebrick3")
    points(gene[i], t.cil[i], cex = 0.6, pch = 16, col = "firebrick3")
    ct <- ct + 1
    gene.list <- c(gene.list, i)
  }
}

raw.head <- max(which(pv <= 0.05))
bone.head <- max(which(pv.bon <= 0.05))
fdr.head <- max(which(pv.bh <= 0.05))

abline(v = bone.head, lty = 2, col = 'black')
abline(v = fdr.head, lty = 2, col = 'black')
abline(v = raw.head, lty = 2, col = 'black')

#arrows(1475,log(1/1100,base=10),1275,length=0.1,code=2)
text(1350, log(1/1100, base = 10), "f", cex = 1)

#arrows(500,log(1/1100,base=10),300,length=0.1,code=2)
text(400, log(1/1100, base = 10), "b", cex = 1)

#arrows(2305,log(1/1100,base=10),2080,length=0.1,code=2)
text(2150, log(1/1100, base = 10), "u", cex = 1)

legend("topright", 
       c("Interval Null", expression("p"[delta]*" = 0"), expression("p"[delta]*" > 0")),
       lty = 1, 
       col = c(idz.col, "firebrick3", "cornflowerblue"),
       lwd = c(8, 1.5, 1), 
       bty ="n"
)

legend("bottomright",
       c("Bonferroni", "FDR", "Unadjusted"),
       pch = c("b", "f", "u"),
       col = 1,
       bty = "n"
)