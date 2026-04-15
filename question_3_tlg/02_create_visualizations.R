library(ggplot2)
library(pharmaverseadam)
install.packages("showtext")
library(showtext)
showtext_auto()

adae<-pharmaverseadam::adae

png("teae_severity.png", width = 800, height = 600, res = 100)
ggplot(adae, aes(x = ACTARM,fill = AESEV)) +
  geom_bar(position = "stack") +
  labs(title = "Treatment-Emergent AE Severity by Arm", x = "Treatment Arm", y = "Countof AEs") +
  theme_bw() +  
  theme(legend.position = "right")
dev.off() 
