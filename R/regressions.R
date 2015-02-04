mostXNames <- function(total, party, x, y, color) {
  df <- filter(total, partido == party)[1:30,]
  df <- droplevels(df)
  df$nombre <- reorder(df$nombre, df$per, mean)
  ggplot(df, aes(per, nombre)) +
    geom_point(aes(size = count.party), color = color) +
    geom_vline(xintercept = df$per.all[1], color = gray,
               linetype = 2) +
    annotate("text", y = y, x =x, 
             label = str_c("Average ", party, "\nname"),
             hjust = 0, size = 4) +
    ggtitle(str_c("These are the most overrepresented names in the ", party, ",\ngiven that someone belongs to a political party\n")) +
    scale_x_continuous(labels = percent) +
    scale_size_area("name\ncount") +
    xlab(str_c("percentage bearing the name that are members of the ", party)) +
    ylab("name") +
    theme_bw()
}

## Tables of the most overepresented names by party 



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
  mutate(diff =  per - per.all ) %>%
  arrange(partido, desc(diff)) %>% 
  filter(count > 100 & count.party > 30)


mostXNames(total, "PAN", .029, "MARIA FERNANDA", PAN.COLOR)
ggsave(file.path("charts", "pan_names.svg"), dpi = 100, width = 7, height = 9)
mostXNames(total, "PRD", .34, "ANALLELI", PRD.COLOR)
ggsave(file.path("charts", "prd_names.svg"), dpi = 100, width = 7, height = 9)
mostXNames(total, "PRI", .65, "NORMA ARACELY", PRI.COLOR)
ggsave(file.path("charts", "pri_names.svg"), dpi = 100, width = 7, height = 9)

print.data.frame(head(filter(total, partido == "PAN"), 30))
print.data.frame(head(filter(total, partido == "PRI"), 30))
print.data.frame(head(filter(total, partido == "PRD"), 100))
filter(total, nombre == "DIEGO")
print.data.frame(total[which(total$nombre %in% c("NAYELI", "MAYELI", "NAYELLI",
                          "ANA YELI", "ANALLELI", "ANAYELI",
                          "ANALLELY", "NALLELY", "YANELI",
                          "NAYELY")),])



## Percentage unique by party
uniq.parties <- all %>%
  group_by(nombre, partido) %>%
  summarise(a = n())%>%
  group_by(partido) %>%
  summarise(unique = n()) %>%
  inner_join(all %>% group_by(partido) %>% summarise(count = n())) %>%
  mutate(per = unique / count)

uniq.parties$names <- NA
vec<-unlist(str_split(filter(all, partido == "PAN")$nombre, " "))
uniq.parties$names[1] <- length(unique(vec))
vec<-unlist(str_split(filter(all, partido == "PRD")$nombre, " "))
uniq.parties$names[2] <- length(unique(vec))
vec<-unlist(str_split(filter(all, partido == "PRI")$nombre, " "))
uniq.parties$names[3] <- length(unique(vec))

uniq.parties$per.names <- uniq.parties$names / uniq.parties$count

ggplot(uniq.parties, aes(partido, per.names, fill = partido)) +
  geom_bar(stat = "identity") +
  ggtitle("Percentage of unique names by party") +
  xlab("percent unique") +
  scale_fill_manual("party",
                     breaks = c("PRI", "PRD", "PAN"), 
                     values = c("#2b8cbe", "#fed300", "#de2d26")) +
  theme_bw() +
  coord_flip()
ggsave(file.path("charts", "unique_name.svg"), 
       dpi = 100, width = 7.60, height = 5)
# uniq.states <- all %>%
#   group_by(state_code, partido) %>%
#   summarise(unique.names = length(unique(unlist(str_split(nombre, " ")))),
#             count = n())
# uniq.states$per.names <- uniq.states$unique.names / uniq.states$count
# 
# ggplot(left_join(states.uniq, uniq.states), aes(lat, lon, fill = per.names)) +
#   geom_polygon(aes(long, lat, group=group),
#                color = "black", size = .1) +
#   coord_map("albers", lat0 = bb[ 2 , 1 ] , lat1 = bb[ 2 , 2 ] ) +
#   theme_bw() + 
#   ggtitle("title") +
#   theme_bare +
#   facet_wrap(~ partido, ncol = 2)
# 
# uniq.states <- all %>%
#   group_by(nombre, state_code) %>%
#   summarise(a = n())%>%
#   group_by(state_code) %>%
#   summarise(unique = n()) %>%
#   inner_join(all %>% group_by(state_code) %>% summarise(count = n())) %>%
#   mutate(per = unique / count)
# 
# 
# ggplot(left_join(states.ff, uniq.states), aes(lat, lon, fill = per)) +
#   geom_polygon(aes(long, lat, group=group),
#                color = "black", size = .1) +
#   coord_map("albers", lat0 = bb[ 2 , 1 ] , lat1 = bb[ 2 , 2 ] ) +
#   theme_bw() + 
#   ggtitle("title") +
#   theme_bare

## Does name length predict PANismo

all$length <- str_length(all$nombre) + str_length(all$paterno) + str_length(all$materno)

ggplot(all,
  aes(x = length,group = partido, color = partido)) +
    geom_density(adjust = 3, size = 1.2)+
  ggtitle("Distribution of name length for all major political parties (PAN, PRI, PRD)") +
  xlab("number of characters") +
  scale_color_manual("party",
                    breaks = c("PAN", "PRI", "PRD"), 
                    values = c("#2b8cbe", "#fed300", "#de2d26")) +
  theme_bw()
ggsave(file.path("charts", "length_distribution_parties.svg"), 
       dpi = 100, width = 9.60, height = 7)

dist_s <- condense(bin(all$length, 1))
autoplot(dist_s) +
  ggtitle("Distribution of name length for all major political parties (PAN, PRI, PRD)") +
  xlab("number of characters") +
  theme_bw()
ggsave(file.path("charts", "length_distribution.svg"), dpi = 100, width = 9.60, height = 7)

all %>%
  group_by(partido) %>%
  summarise(quantile(length, probs = .02))

all[which.max(all$length),]
all[which(all$length == 53),]

ggplot(all, aes(partido, length)) +
  geom_boxplot()

pran <- filter(all, partido %in% c("PAN", "PRD"))

pran$partido <- as.factor(pran$partido)
pran$partido <- relevel(pran$partido, "PRD")




fit.len <- glm(partido ~ length, family = binomial(), 
               data = pran)
plot(fit.len, which = 1)
# qplot(.fitted, .resid, data = fit.len) +
#   geom_hline(yintercept = 0) +
#   geom_smooth(se = FALSE)
# qplot(.fitted, .stdresid, data = mod) +
#   geom_hline(yintercept = 0) +
#   geom_smooth(se = FALSE)
# qplot(.fitted, sqrt(abs(.stdresid)), data = mod) + geom_smooth(se = FALSE)
# qplot(seq_along(.cooksd), .cooksd, data = mod, geom = "bar",
#       stat="identity")
# test <- multinom(partido ~ length, data = all)
summary(fit.len)
exp(coef(fit.len))
visreg(fit.len,  scale="response")

pred.df <-  data.frame(partido = as.factor(rep(c("PAN", "PRD"), each = 37)), 
                       length = rep(14:50, 2))
pred.df$response <- predict(fit.len, newdata = pred.df, "response")
pred.df$response[38:74] <- 1 - pred.df$response[38:74]

length.pran <- pran %>%
  group_by(partido) %>%
  summarise(n = n() / nrow(pran) )

ggplot(pred.df, aes(length, response, group = partido, color = partido)) +
  geom_line(size = 1.2) +
  ylab("predicted probability") +
  xlab("length of name") +
  scale_color_manual("party", breaks =c("PRD", "PAN"),
                     values = c(PAN.COLOR, PRD.COLOR))+ 
  geom_hline(data = length.pran, aes(yintercept = n), linetype = 2, color = "gray") +
  geom_vline(xintercept = median(pran$length), color = "gray") +
  scale_y_continuous(limits = c(0, 1), breaks = c(.08, .25, .5, .75, .92)) +
  annotate("text", x = 26, y = .55, size = 4, color = "#333333",
           label = "Median name length") +
  annotate("text", x = 42, y = .94, size = 4, color = "#333333",
           label = "Proportion of names affiliated with the PRD (left wing)") +
  annotate("text", x = 42, y = .06, size = 4, color = "#333333",
           label = "Proportion of names affiliated with the PAN (right wing)") +
  ggtitle("Probability of belonging to the PRD or PAN (given that you already are a member of a party)\nbased on a logistic regression on name length") +
  theme_bw()
ggsave(file.path("charts", "length_reg.svg"), dpi = 100, width = 9.60, height = 7)
##
# 
# last_names <- unique(c(as.character(pan$paterno), as.character(pan$materno)))
# freq <- prd %>%
#   group_by(paterno, materno) %>%
#   summarise(weight = n()) %>%
#   arrange(-weight)
# 
# freq[order(desc(freq$weight)),]
# 
# 
# prird <- rbind(prd,
#                pan)
# prird <- rbind(filter(prd, entidad == "SONORA"), 
#                filter(pri, entidad == "SONORA"))
# 
# nrow(filter(prd, entidad == "YUCATAN"))
# 
# freq <- prird %>%
#   group_by(nombre) %>%
#   summarise(count = n()) %>%
#   arrange(desc(count)) %>%
#   filter(count > 50)
# # 
# # b=prird %>%
# #   group_by(nombre, partido) %>%
# #   summarise(count = n()) %>%
# #   filter(count > 10) %>%
# #   arrange(desc(count))
# 
# prird <- semi_join(prird, freq)
# 
# 
# prird$nombre <- as.factor(prird$nombre)
# prird$partido <- as.factor(prird$partido)
# prird$length <- str_length(prird$nombre) + str_length(prird$paterno) + str_length(prird$materno)
# prird %>%
#   group_by(partido) %>%
#   summarise(quantile(length, probs = .05))
# 
# 
# prird$partido <- relevel(prird$partido, "PRD")
# fit.len <- glm(partido ~ length, family = binomial(), data = prird)
# summary(fit.len)
# exp(coef(fit.len))
# visreg(fit.len,  scale="response")
# 
# # glmmod<-glmnet(x = model.matrix(~ nombre, prird), y = prird$partido, 
# #                alpha=1, family='binomial')
# 
# make.data <- function(data, chunksize, ...){
#   pos <- 1
#   function(reset=FALSE){
#     if(reset){
#       pos  <<- 1
#     } else {
#       if((pos + chunksize - 1) < nrow(data)) {
#         rval <- data[pos:(pos + chunksize -1),]
#       }
#       else if(pos < nrow(data)) {
#         rval <- data[pos:nrow(data),]
#       }
#       else {
#         rval<-NULL
#       }
#       pos  <<-  pos + chunksize
#       return(rval)
#     }
#   }
# }
# 
# data.fun <- make.data(prird[1:202,c("partido", "nombre")], chunksize = 100)
# formula <- partido ~ nombre
# fit <- bigglm(formula, family  = binomial(), 
#               data = prird,
#               chunksize = 50, sandwich = TRUE)
# 
# library("ffbase")
# m <- as.ff(prird)
# ffmdf.out <- bigglm(fg, data = ffmdf, chunksize = 10, sandwich = TRUE)
# 
# 
# prird$nombre <- relevel(prird$nombre, "ANA LOURDES")
# prird$partido <- relevel(prird$partido, "PRI")
# fit <- glm(partido ~ nombre, family  = binomial(), 
#            data = prird[,c("partido", "nombre")])
# 
# glm.fitted <- fitted(fit)
# fitted <- unique(data_frame(probs = as.vector(glm.fitted),
#                             name = prird$nombre))
# 
# #e <- exp(confint(fit))
# tmp <- data.frame(cbind(exp(coef(fit))))
# odds<-data.frame(OR = tmp[-1,])
# odds$name<-row.names(tmp)[-1]
# 
# odds$p.value <- coef(summary(fit))[,4][-1]
# odds$name <- str_replace(odds$name, "nombre", "")
# 
# odds <- odds[order(odds$OR),]
# odds <- filter(odds, p.value < .05)
# top <- odds[-c(20:(nrow(odds)-20)),]
# 
# odds <- inner_join(odds, fitted)
# 
# ggplot(top, aes(reorder(name, OR), OR)) +
#   geom_point() +
#   scale_y_log10() +
#   geom_hline(yintercept = 1, linetype=2) +
#   coord_flip() +
#   labs(title = 'title', x = 'Variables', y = 'OR') +
#   theme_bw()
# 
# 
# 
# #plot variable coefficients vs. shrinkage parameter lambda.
# plot(glmmod,xvar="lambda")