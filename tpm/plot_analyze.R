"

Toward More Precise Numerical Statements

"

# Read in data
# Read in APSR data
apsr <- read.csv("apsr_small.csv")

# Read in AER
aer_all <- read.csv("aer_small.csv")

# Only AER (without papers and proceedings)
aer <- aer_all[aer_all$pnp==0,]

# APSR Brief Analysis
# Issues and volumes covered in the data
table(apsr$issue.vol)

# proportion empirical articles carrying precise empirical statements
nemperical_apsr <- sum(apsr$type=="empirical")
apsr_precise <- sum(apsr$numberOrnot[apsr$type=="empirical"])/nemperical_apsr

# Brief Analysis of AER
# proportion empirical articles carrying precise empirical statements
nemperical_aer <- sum(aer$type=="empirical")
aer_precise <- sum(aer$numberOrnot[aer$type=="empirical"])/nemperical_aer

# AER Papers and proceedings
aerp <- aer_all[aer_all$pnp==1,]

# proportion empirical articles carrying precise empirical statements
nemperical_aerp <- sum(aerp$type=="empirical")
aerp_precise <- sum(aerp$numberOrnot[aerp$type=="empirical"])/nemperical_aerp

# Plot the proportions
library(ggplot2)
library(grid)
library(goji) #devtools::install_github("soodoku/goji")

res <- data.frame(means=c(aer_precise, aerp_precise, apsr_precise), labels=c("AER", "AER P & P", "APSR"))
res$labels <- factor(res$labels, levels=c("AER", "AER P & P", "APSR"))

ggplot(data=res, aes(y=res$means, x=res$labels)) + 
	geom_bar(stat = "identity", fill="#42C4C7") + 
  geom_text(size=3, aes(y=res$means+.035, x=c(1,2,3), label=unlist(strsplit(paste0(nolead0s(round(res$means,2)), "\n", c("(n=66)", "(n=68)", "(n=81)")), " ")))) + 
	theme_minimal() + 
	xlab("") +
  scale_y_continuous(breaks=seq(0,.7,.1), labels=nolead0s(seq(0,.7,.1)), limits=c(0,.7), name="Proportion of Abstracts with Precise Numerical Statements") +
	theme(panel.grid.major.y = element_line(colour = "#e3e3e3", linetype = "dotted"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_line(colour = "#f7f7f7", linetype = "solid"),
        panel.border       = element_blank(),
        legend.position  = "none",
        legend.key       = element_blank(),
        legend.key.width = unit(1,"cm"),
        axis.title   = element_text(size=10),
        axis.text    = element_text(size=8),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(vjust=-1),
        axis.title.y = element_text(vjust= 1),
        plot.margin = unit(c(0,.5,.5,.5), "cm"))

ggsave("figs/apsr_aer.pdf")
ggsave("figs/apsr_aer.png")

