BMI = read.table("c:/Users/a/BMI_IRAN.txt", header = TRUE, sep = " ") 
library(ggplot2)
data(mpg, package="ggplot2") 
theme_set(theme_bw())  
g <- ggplot(mpg, aes(cty, hwy))
g + geom_point() + 
   geom_smooth(method="lm", se=F) +
   labs(subtitle="mpg: city vs highway mileage", 
       y="hwy", 
       x="cty", 
       title="Scatterplot with overlapping points", 
       caption="Source: midwest")
#
library(ggplot2)
data(mpg, package="ggplot2")
theme_set(theme_bw())  
g <- ggplot(mpg, aes(cty, hwy))
g + geom_jitter(width = .5, size=1) +
  labs(subtitle="mpg: city vs highway mileage", 
       y="hwy", 
       x="cty", 
       title="Jittered Points")
#
library(ggplot2)
library(ggExtra)
data(mpg, package="ggplot2")
theme_set(theme_bw())  
g <- ggplot(mpg, aes(cty, hwy)) + 
  geom_count() + 
  geom_smooth(method="lm", se=F)
ggMarginal(g, type = "histogram", fill="transparent")
# ggMarginal(g, type = "density", fill="transparent")
#ggMarginal(g, type = "boxplot", fill="transparent")
#
library(ggplot2)
library(ggcorrplot)
BMI = read.table("c:/Users/a/BMI_IRAN.txt", header = TRUE, sep = " ")
corr <- round(cor(BMI), 1)
ggcorrplot(corr, hc.order = TRUE, 
            type = "lower", 
            lab = TRUE, 
            lab_size = 3, 
            method="circle", 
            colors = c("tomato2", "white", "springgreen3"), 
            title="Correlogram of BMI", 
            ggtheme=theme_bw)
#
library(ggplot2)
theme_set(theme_bw())  
data("mtcars")  
mtcars$`car name` <- rownames(mtcars)  
mtcars$mpg_z <- round((mtcars$mpg - mean(mtcars$mpg))/sd(mtcars$mpg), 2)  
mtcars$mpg_type <- ifelse(mtcars$mpg_z < 0, "below", "above")  
mtcars <- mtcars[order(mtcars$mpg_z), ]  
mtcars$`car name` <- factor(mtcars$`car name`, levels = mtcars$`car name`) 
ggplot(mtcars, aes(x=`car name`, y=mpg_z, label=mpg_z)) + 
   geom_bar(stat='identity', aes(fill=mpg_type), width=.5)  +
   scale_fill_manual(name="Mileage", 
          labels = c("Above Average", "Below Average"), 
          values = c("above"="#00ba38", "below"="#f8766d")) + 
   labs(subtitle="Normalised mileage from 'mtcars'", 
        title= "Diverging Bars") + 
   coord_flip()
#
cty_mpg <- aggregate(mpg$cty, by=list(mpg$manufacturer), FUN=mean)  
colnames(cty_mpg) <- c("make", "mileage")  
cty_mpg <- cty_mpg[order(cty_mpg$mileage), ]  
cty_mpg$make <- factor(cty_mpg$make, levels = cty_mpg$make) 
head(cty_mpg, 4)
         make  mileage
9     lincoln 11.33333
8  land rover 11.50000
3       dodge 13.13514
10    mercury 13.25000
#
library(ggplot2)
theme_set(theme_bw())
ggplot(cty_mpg, aes(x=make, y=mileage)) + 
   geom_bar(stat="identity", width=.5, fill="tomato3") + 
   labs(title="Ordered Bar Chart", 
        subtitle="Make Vs Avg. Mileage", 
        caption="source: mpg") + 
   theme(axis.text.x = element_text(angle=65, vjust=0.6))
#
library(ggplot2)
theme_set(theme_classic())
g <- ggplot(iris, aes(Sepal.Length)) + scale_fill_brewer(palette = "Spectral")
g + geom_histogram(aes(fill=Species), 
                    binwidth = .1, 
                    col="black", 
                    size=.1) +  
   labs(title="Histogram with Auto Binning", 
        subtitle="Size of sepals length according to species")
#
library(ggplot2)
theme_set(theme_classic())
g <- ggplot(mpg, aes(class, cty))
g + geom_boxplot(varwidth=TRUE, fill="plum") + 
     labs(title="Box plot", 
          subtitle="City Mileage grouped by Class of vehicle",
          caption="Source: mpg",
          x="Class of Vehicle",
          y="City Mileage")
#
library(ggplot2)
theme_set(theme_bw())
g <- ggplot(mpg, aes(class, cty))
g + geom_violin() + 
   labs(title="Violin plot", 
        subtitle="City Mileage vs Class of vehicle",
        caption="Source: mpg",
        x="Class of Vehicle",
        y="City Mileage")
#
library(ggplot2)                                                                
pop <- data.frame(                                                              
   age    = c("00 a 04", "05 a 09", "10 a 14", "15 a 19", "20 a 24", "25 a 29",       
                "30 a 34", "35 a 39", "40 a 44", "45 a 49", "50 a 54", "55 a 59", "60 a 64", 
                "65 a 69", "70 a 74", "75 a 79", "80 a 84", "85 a 89", "90 a 94", "95 a 99", 
                "Mais de 100"),                                                     
   mens   = c(3406, 3630, 4431, 4531, 4957, 5323, 4718, 4303, 4201, 4297, 3816, 3081, 2252, 1502, 1114, 734,  422, 165, 52,  5,  3),
   womans = c(3126, 3610, 4374, 4537, 4998, 5486, 4748, 4404, 4640, 4522, 4170, 3506, 2690, 2080, 1630, 1332, 896, 462, 177, 38, 5)
 )                                                                               
ggplot(pop) +                                                                   
   geom_bar(aes(x=age, y=-mens   ), fill = "#009999", stat="identity") +      
   geom_bar(aes(x=age, y=womans), fill = "#FF9933", stat="identity") +     
   coord_flip() +                                                                
   theme_bw() +                                                                  
   ylab("Quantidade") +                                                          
   scale_y_continuous(labels=abs, breaks=c(-3000, 0, 3000)) 
#
var <- mpg$class  
nrows <- 10
df <- expand.grid(y = 1:nrows, x = 1:nrows)
categ_table <- round(table(var) * ((nrows*nrows)/(length(var))))
categ_table
df$category <- factor(rep(names(categ_table), categ_table))  
ggplot(df, aes(x = x, y = y, fill = category)) + 
         geom_tile(color = "black", size = 0.5) +
         scale_x_continuous(expand = c(0, 0)) +
         scale_y_continuous(expand = c(0, 0), trans = 'reverse') +
         scale_fill_brewer(palette = "Set3") +
         labs(title="Waffle Chart", subtitle="'Class' of vehicles",
              caption="Source: mpg") + 
         theme(panel.border = element_rect(size = 2),
               plot.title = element_text(size = rel(1.2)),
               axis.text = element_blank(),
               axis.title = element_blank(),
               axis.ticks = element_blank(),
               legend.title = element_blank(),
               legend.position = "right")
#
library(treemap)
group <- c(rep("group-1",4),rep("group-2",2),rep("group-3",3))
subgroup <- paste("subgroup" , c(1,2,3,4,1,2,1,2,3), sep="-")
value <- c(13,5,22,12,11,8,3,1,23)
data <- data.frame(group,subgroup,value)
treemap(data, index=c("group","subgroup"), vSize="value", type="index", palette = "Set2",
     fontsize.labels=c(15,12),                
     fontcolor.labels=c("red","white"),    
     fontface.labels=c(2,1),                  
     bg.labels=c("transparent"),              
     align.labels=list(
         c("center", "center"), 
         c("right", "bottom")
         ),                                   
     overlap.labels=0.5,                     
     inflate.labels=F,                        
 )
#
ggplot(economics, aes(x=date)) + 
   geom_line(aes(y = psavert), color = "blue") + 
   geom_line(aes(y = uempmed), color="red", lty = 1) 
#
library(ggplot2)
library(lubridate)
theme_set(theme_bw())
df <- economics[, c("date", "psavert", "uempmed")]
df <- df[lubridate::year(df$date) %in% c(1967:1981), ]
brks <- df$date[seq(1, length(df$date), 12)]
lbls <- lubridate::year(brks)
ggplot(df, aes(x=date)) + 
   geom_area(aes(y=psavert+uempmed, fill="psavert")) + 
   geom_area(aes(y=uempmed, fill="uempmed")) + 
   labs(title="Area Chart of Returns Percentage", 
        subtitle="From Wide Data format", 
        caption="Source: Economics", 
        y="Returns %") + 
   scale_x_date(labels = lbls, breaks = brks) +  
   scale_fill_manual(name="", 
                     values = c("psavert"="#00ba38", "uempmed"="#f8766d"))
#
ggsave("my_plot.png")