library(tidyverse)
library(circlize)
######

dat = read_csv('foe.csv')
# data fram
df = dat %>% gather(key = 'vet',value= 'value',2:13)
chordDiagram(df)
## edu to commerce
df_1 = df %>% filter(vet=='Commerce') %>% filter(value >100) %>% 
  filter(!edu %in% c('Commerce','Non-award'))

chordDiagram(df_1)

# colors
sector.col = c( Physics = "red", Engineering = "green",Health = "blue",
                Education = "purple", Society = "#FF5733", Arts = "#FFC300",
                Commerce = '#581845')
link.col = sector.col[-length(sector.col)]
# gap
gaps = c(rep(5, length(unique(mdf1[[1]]))-1), 30, 
         rep(5, length(unique(mdf1[[2]]))-1), 30)

gaps[c(1,2,6)] = c(15,8,35)  # adjust for engineering



#start_degree = 0 - (180 - row_sector_degree)/2
circos.clear()
circos.par(gap.after = gaps,start.degree =0)

df_1 %>% chordDiagram(.,directional = 1, direction.type = c("diffHeight", "arrows"),
                      col = link.col,annotationTrack = c("name", "grid"),grid.col = sector.col,
                      annotationTrackHeight = c(0.07, 0.11)) %>%
  abline(h = 0.1, lty = 2, col = "#00000080")
