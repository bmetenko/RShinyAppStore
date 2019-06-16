df$id
df %>% group_by() %>% tally() 


priceTally <- df %>% group_by(price) %>% tally() 

pie(x = priceTally$n, labels = priceTally$price)

blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )


bp<- ggplot(priceTally, aes(x="", y=n, fill=as.factor(price)))+
  geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) + scale_fill_grey() + blank_theme
bp

df[, sapply(df, class) == "numeric"] %>% colnames

