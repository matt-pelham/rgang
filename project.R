
rm(list = ls())
#Source the file containing the functions that load and parse the data
source("dataload.R")
source("defaultanalysis.R")
source("schooltypeanalysis.R")
source("geographicanalysis.R")

full_df <- load.data()

unflattened_df <- unflatten.data(full_df)

p1 <- densityplot.default.over.tuition(unflattened_df)
print(p1)

p2 <- densityplot.defaultrate.over.tuition(unflattened_df)
print(p2)

p3 <- plot.combined.default.over.tuition(unflattened_df)
print(p3)
ggsave(filename = "defaultovertuition.png", plot = p3, width = 6, height = 4, dpi = 600)

p4 <- plot.default.rate.over.degree.and.tuition(unflattened_df)
print(p4)
ggsave(filename = "defaultrate-degree-tuition.png", plot = p4, width = 6, height = 4, dpi = 600)

pboxplot <- boxplot.default.rates.v.program.length(unflattened_df) 
ggsave(filename = "ProgLen.png", plot = pboxplot, width = 7, height = 4, dpi = 600)

generate.summary.tables(unflattened_df)

p5 <- plot.default.over.tuition(unflattened_df)
ggsave(filename = "Ben1.png", plot = p5, width = 6, height = 4, dpi = 600)

p6 <- plot.default.over.tuition.typefacet(unflattened_df)
ggsave(filename = "Ben2.png", plot = p6, width = 7, height = 4, dpi = 600)

map <- generate.choropleth.maps(unflattened_df)
print(map)