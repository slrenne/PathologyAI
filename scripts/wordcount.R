library(wordcloud)
text <- read.csv("input/tasks.csv")



png("figures/wordcount.png", width = 8000, height = 8000,  res = 1200)
wordcloud(words = text$term, freq = text$fr, colors=brewer.pal(8, "Dark2"))
dev.off()
