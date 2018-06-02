library(ggplot2)

setwd("D:\\Dropbox\\My projects\\Courses\\ST_R\\website\\meetings\\m10\\data")

figure1 <- read.csv("figure1.csv", stringsAsFactors = FALSE)

ggplot(figure1, aes(x=lrgdpch, y=fhpolrigaug))+
  geom_text(aes(label=code), size=2)+
  geom_smooth(method="lm")+
  labs(x="Log GDP per Capita (Penn World Tables)",
       y="Freedom House Measure of Democracy",
       title="Figure 1",
       subtitle="Democracy and Income, 1990s")

acem1 <- lm(fhpolrigaug~lrgdpch, data=figure1)

summary(acem1)

acem1$r.squared

library(lmtest)

bptest(acem1)

par(mfrow=c(2,2)) # 

plot(acem1) #

figure2 <- read.csv("figure2.csv", 
                    stringsAsFactors = FALSE, sep="\t")

ggplot(figure2, aes(x=growth, y=democ))+
  geom_text(aes(label=code), size=2)+
  geom_smooth(method="lm")+
  labs(x="Change in Log GDP per Capita",
       y="Change in Democracy",
       title="Figure 5",
       subtitle="Change in Democracy and Change in Income, 1500-2000")
acem2 <- lm(democ~growth, data=figure2)

summary(acem2)

figure3 <- read.csv("figure3.csv", stringsAsFactors = FALSE, sep="\t")

ggplot(figure3, aes(x=growth, y=democ))+
  geom_text(aes(label=code), size=2)+
  geom_smooth(method="lm")+
  labs(x="Change in Log GDP per Capita",
       y="Change in Democracy",
       title="Figure 5",
       subtitle="Change in Democracy and Change in Income, 1500-2000")

democracy <- lm(democ~consfirstaug+indcent+
              rel_catho80+rel_muslim80+rel_protmg80, 
              data=figure3)
growth <- lm(growth~consfirstaug+indcent+
               rel_catho80+rel_muslim80+rel_protmg80, 
             data=figure3)
			 
democracy <- lm(growth~ democ+consfirstaug+indcent+
                  rel_catho80+rel_muslim80+rel_protmg80, 
                data=figure3)

summary(democracy)

democracy$residuals <- as.numeric(democracy$residuals)
growth$residuals <- as.numeric(growth$residuals)

residuals <- as.data.frame(cbind(democracy$residuals, growth$residuals))

names(residuals) <- c("democracy", "growth")

mod3 <- lm(growth~democracy, data=residuals)
summary(mod3)

cor(residuals$democracy, residuals$growth, use="complete.obs")
  
ggplot(residuals, aes(x=growth, y=democracy))+
  geom_point()+
  geom_smooth(method="lm")+
  labs(x="Change in Log GDP per Capita, Independent of Historical Factors",
       y="Change in Democracy Independent of Historical Factors",
       title="Change in Democracy and Change in Income, 1500-2000",
       subtitle="Conditional on Historical Factors")
