## Tables of the most overepresented names by party 

all <- rbind(pri,pan,prd)

total <- all %>%
  group_by(nombre) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

total.parties <- all %>%
  group_by(nombre, partido) %>%
  summarise(count.party = n()) %>%
  arrange(desc(count.party), partido)

total.all  <- all %>%
  group_by(partido) %>%
  summarise(per.all = n() / nrow(all) )

total %<>% inner_join(total.parties) %>% 
  inner_join(total.all) %>%
  mutate(per = count.party / count) %>%
  mutate(diff =  per.all / per) %>%
  arrange(partido, desc(diff)) %>% 
  filter(count > 100 & count.party > 30)

print.data.frame(head(filter(total, partido == "PAN"), 30))
print.data.frame(head(filter(total, partido == "PRI"), 30))
print.data.frame(head(filter(total, partido == "PRD"), 30))
filter(total, nombre == "DIEGO")

## Does name length predict PANismo

last_names <- unique(c(as.character(pan$paterno), as.character(pan$materno)))
freq <- prd %>%
  group_by(paterno, materno) %>%
  summarise(weight = n()) %>%
  arrange(-weight)

freq[order(desc(freq$weight)),]


prird <- rbind(prd,
               pan)
prird <- rbind(filter(prd, entidad == "SONORA"), 
               filter(pri, entidad == "SONORA"))

nrow(filter(prd, entidad == "YUCATAN"))

freq <- prird %>%
  group_by(nombre) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  filter(count > 50)
# 
# b=prird %>%
#   group_by(nombre, partido) %>%
#   summarise(count = n()) %>%
#   filter(count > 10) %>%
#   arrange(desc(count))

prird <- semi_join(prird, freq)


prird$nombre <- as.factor(prird$nombre)
prird$partido <- as.factor(prird$partido)
prird$length <- str_length(prird$nombre) + str_length(prird$paterno) + str_length(prird$materno)
prird %>%
  group_by(partido) %>%
  summarise(quantile(length, probs = .05))


prird$partido <- relevel(prird$partido, "PRD")
fit.len <- glm(partido ~ length, family = binomial(), data = prird)
summary(fit.len)
exp(coef(fit.len))
visreg(fit.len,  scale="response")

# glmmod<-glmnet(x = model.matrix(~ nombre, prird), y = prird$partido, 
#                alpha=1, family='binomial')

make.data <- function(data, chunksize, ...){
  pos <- 1
  function(reset=FALSE){
    if(reset){
      pos  <<- 1
    } else {
      if((pos + chunksize - 1) < nrow(data)) {
        rval <- data[pos:(pos + chunksize -1),]
      }
      else if(pos < nrow(data)) {
        rval <- data[pos:nrow(data),]
      }
      else {
        rval<-NULL
      }
      pos  <<-  pos + chunksize
      return(rval)
    }
  }
}

data.fun <- make.data(prird[1:202,c("partido", "nombre")], chunksize = 100)
formula <- partido ~ nombre
fit <- bigglm(formula, family  = binomial(), 
              data = prird,
              chunksize = 50, sandwich = TRUE)

library("ffbase")
m <- as.ff(prird)
ffmdf.out <- bigglm(fg, data = ffmdf, chunksize = 10, sandwich = TRUE)


prird$nombre <- relevel(prird$nombre, "ANA LOURDES")
prird$partido <- relevel(prird$partido, "PRI")
fit <- glm(partido ~ nombre, family  = binomial(), 
           data = prird[,c("partido", "nombre")])

glm.fitted <- fitted(fit)
fitted <- unique(data_frame(probs = as.vector(glm.fitted),
                            name = prird$nombre))

#e <- exp(confint(fit))
tmp <- data.frame(cbind(exp(coef(fit))))
odds<-data.frame(OR = tmp[-1,])
odds$name<-row.names(tmp)[-1]

odds$p.value <- coef(summary(fit))[,4][-1]
odds$name <- str_replace(odds$name, "nombre", "")

odds <- odds[order(odds$OR),]
odds <- filter(odds, p.value < .05)
top <- odds[-c(20:(nrow(odds)-20)),]

odds <- inner_join(odds, fitted)

ggplot(top, aes(reorder(name, OR), OR)) +
  geom_point() +
  scale_y_log10() +
  geom_hline(yintercept = 1, linetype=2) +
  coord_flip() +
  labs(title = 'title', x = 'Variables', y = 'OR') +
  theme_bw()



#plot variable coefficients vs. shrinkage parameter lambda.
plot(glmmod,xvar="lambda")