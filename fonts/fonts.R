#Fonts

library(remotes)
remotes::install_version("Rttf2pt1", version = "1.3.8")
#install.packages("extrafont",dependencies = T)
library(extrafont)
font_import()
loadfonts(device = "win")
fonts()

fonttable()


library(ggplot2)
library(ggthemes)

fontTable = fonttable()

fontTable$Face = with(fontTable, ifelse(Bold & Italic, "bold.italic", 
                                        ifelse(Bold, "bold",
                                               ifelse(Italic, "italic", "plain"))))
fontTable$Face = factor(fontTable$Face, levels = c("plain","bold","italic","bold.italic"), ordered = TRUE)
fontTable$FamilyName = factor(fontTable$FamilyName, levels = rev(sort(unique(fontTable$FamilyName))), ordered = TRUE)

p = ggplot(fontTable) +
  geom_text(aes(x=Face, y=FamilyName, label=FullName, family=FamilyName, fontface=Face)) +
  labs(title="Windows Fonts in R", x=NULL, y=NULL) +
  theme_tufte() +
  theme(axis.ticks = element_blank(),
        axis.text=element_text(size=12, colour="gray40", family='Arial'),
        axis.text.x=element_text(face=c("plain","bold","italic","bold.italic")),
        plot.title=element_text(size=16, family='Arial'))

ggsave("font_ggplot_map.png", width = 10, height = 33, dpi=300)
