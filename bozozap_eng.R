
library(dplyr)
bozozap_classe <- bozozap %>% 
  select(memes, persuasivo, acao_popular, discussao_publica, duvida) %>% 
  filter(persuasivo == 1 | acao_popular == 1 | discussao_publica == 1 | duvida == 1) %>% 
  count(persuasivo == 1)

bozozap %>%
  count(racismo, misoginia, sort = TRUE)

data %>%
  select(semana) %>% 
  count(semana)

bozozap_ext <- bozozap %>% 
  select(-data, -classe, -representacao, -memes)

res <- cor(corr_total) # Corr matrix
matriz <- round(res, 2)

library(corrplot)
corrplot(matriz, method = "circle")

library(ca)
table(corr_rep_nac$nacionalismo, corr_rep_nac$representacao)
plot(ca(table(corr_rep_nac$nacionalismo, corr_rep_nac$representacao)))

library(ggpubr)
# Add titles andd footnote
# :::::::::::::::::::::::::::::::::::::::::::::::::::
# Add titles and footnote
# Wrap subtitle into multiple lines using strwrap()
main.title <- paste0("Table 1: Functions of analyzed memes \n")

tab <- ggtexttable(tabela1, theme = ttheme("light"))
tab %>%
  #tab_add_title(text = subtitle, face = "plain", size = 10) %>%
  tab_add_title(text = main.title, face = "bold", padding = unit(0.1, "line")) %>%
  tab_add_footnote(text = "Source: coLAB/UFF", size = 10, face = "italic")


### TABELA 2

main.title <- paste0("Table 2: Archetypal representations \n")

tab <- ggtexttable(tabela2, theme = ttheme("light"))
tab %>%
  #tab_add_title(text = subtitle, face = "plain", size = 10) %>%
  tab_add_title(text = main.title, face = "bold", padding = unit(0.1, "line")) %>%
  tab_add_footnote(text = "Source: coLAB/UFF", size = 10, face = "italic")

### TABELA 3

main.title <- paste0("Table 3: Nationalist motives \n")

tab <- ggtexttable(tabela3, theme = ttheme("light"))
tab %>%
  #tab_add_title(text = subtitle, face = "plain", size = 10) %>%
  tab_add_title(text = main.title, face = "bold", padding = unit(0.1, "line")) %>%
  tab_add_footnote(text = "Source: coLAB/UFF", size = 10, face = "italic")


### TABELA 4

main.title <- paste0("Table 4: Antidemocratic values \n")

tab <- ggtexttable(tabela4, theme = ttheme("light"))
tab %>%
  #tab_add_title(text = subtitle, face = "plain", size = 10) %>%
  tab_add_title(text = main.title, face = "bold", padding = unit(0.1, "line")) %>%
  tab_add_footnote(text = "Source: coLAB/UFF", size = 10, face = "italic")

## CA 1

library(ca)

#Criar tabela cruzada
table(corr_eng$class, corr_eng$representation)
#Ou verificar o script colab_cross_table.R

#Executar análise de correspondência canônica
ca(table(corr_eng$class, corr_eng$representation))

#Plotar gráfico de análise de correspondência canônica
plot(ca(table(corr_eng$class, corr_eng$representation)))

#Customizar gráfico
#O parâmetro main indica o título do gráfico
#Os parâmetros ylab e xlab indicam os nomes dos eixos
#O parâmetro col indica as cores em vetores RGB
#O parâmetro col.lab indica as cores dos labels em vetores RGB
plot(ca(table(corr_eng$class, corr_eng$representation)),
     main="Chart 1: Canonical correspondence between functions of memes and Bolsonaro's representations", 
     ylab="Dimension 2 (34%)", xlab="Dimension 1 (48.2%)",
     col = c("#433f62","#FF914B"), col.lab = c("#433f62","#FF914B"),
     sub = "Source: coLAB/UFF")

remotes::install_github("pbiecek/ggplotit")
library(ggplotit)
library(ggplot2)
library(hrbrthemes)
library(ggthemes)
dataca <- ca(table(corr_eng$class, corr_eng$representation))
ggplotit(dataca) +
  theme_ipsum() + 
  #theme(axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  #      axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid')) +
  geom_vline(aes(xintercept = 0)) +
  geom_hline(aes(yintercept = 0)) +
  labs(title = "Chart 1: Canonical correspondence between functions of memes and Bolsonaro's representations",
       subtitle = "Canonical correspondence analysis",
       x = "Dimension 1 (48.2%)",
       y = "Dimension 2 (34%)",
       caption = "Source: coLAB/UFF")

### PLOT CA 2

plot(ca(table(corr_eng$representation, corr_eng$nacionalism)),
     main="Chart 2: Functions and Representations", 
     col = c("#433f62","#FF914B"), col.lab = c("#433f62","#FF914B"),
     sub = "Source: coLAB/UFF")

dataca2 <- ca(table(corr_eng$representation, corr_eng$nacionalism))
ggplotit(dataca2) +
  theme_ipsum() + 
  #theme(axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  #      axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid')) +
  geom_vline(aes(xintercept = 0)) +
  geom_hline(aes(yintercept = 0)) +
  labs(title = "Chart 2: Canonical correspondence between Bolsonaro's representations and nationalist motives",
       subtitle = "Canonical correspondence analysis",
       x = "Dimension 1 (43.5%)",
       y = "Dimension 2 (22.7%)",
       caption = "Source: coLAB/UFF")


### DODGE 1
dodge1 <- as.data.frame(table(corr_eng$representation, corr_eng$nacionalism))

ggplot(dodge1, aes(fill=Var2, y=Freq, x=Var1)) + 
  geom_bar(position="dodge", stat = "identity", alpha=.8, width=.9) +
  #coord_flip() +
  scale_fill_brewer(palette = "Spectral") +
  theme(axis.text.x=element_text(angle = 90, vjust = 0.5)) +
  theme_ipsum() + 
  labs(title = "Chart 3: Bolsonaro's representations and nationalist motives", 
       subtitle = "Simple frequencies (Absolute values)",
       x = "Representations",
       y = "Memes",
       fill = "Nationalist motives",
       caption = "Source: coLAB/UFF")

### DODGE 2
dodge2 <- as.data.frame(table(corr2_eng$week, corr2_eng$representation))

dodge2 <- dodge2 %>% 
mutate(
  Var1 = case_when(
    grepl("semana 01", Var1) ~ "01",
    grepl("semana 02", Var1) ~ "02",
    grepl("semana 03", Var1) ~ "03",
    grepl("semana 04", Var1) ~ "04",
    grepl("semana 05", Var1) ~ "05",
    grepl("semana 06", Var1) ~ "06",
    grepl("semana 07", Var1) ~ "07",
    grepl("semana 08", Var1) ~ "08",
    grepl("semana 09", Var1) ~ "09",
    grepl("semana 10", Var1) ~ "10",
    grepl("semana 11", Var1) ~ "11",
    grepl("semana 12", Var1) ~ "12",
    grepl("semana 13", Var1) ~ "13",
    grepl("semana 14", Var1) ~ "14",
    grepl("semana 15", Var1) ~ "15",
    grepl("semana 16", Var1) ~ "16",
    grepl("semana 17", Var1) ~ "17",
    grepl("semana 18", Var1) ~ "18",
    grepl("semana 19", Var1) ~ "19",
    grepl("semana 20", Var1) ~ "20",
    grepl("semana 21", Var1) ~ "21",
    grepl("semana 22", Var1) ~ "22",
    grepl("semana 23", Var1) ~ "23",
    grepl("semana 24", Var1) ~ "24",
    grepl("semana 25", Var1) ~ "25",
    grepl("semana 26", Var1) ~ "26",
    grepl("semana 27", Var1) ~ "27"))

ggplot(dodge2, aes(fill=Var2, y=Freq, x=Var1)) + 
  geom_bar(position="dodge", stat = "identity", alpha=.8, width=.9) +
  #coord_flip() +
  scale_fill_brewer(palette = "Spectral") +
  theme_ipsum() + 
  labs(title = "Chart 5: Bolsonaro's representations per week", 
       subtitle = "Simple frequencies (Absolute values)",
       x = "Week",
       y = "Memes",
       fill = "Representations",
       caption = "Source: coLAB/UFF")

### DODGE 3

dodge3 <- corr3_eng

ggplot(dodge3, aes(fill=democracy, y=freq, x=week)) + 
  geom_bar(position="dodge", stat = "identity", alpha=.8, width=.9) +
  #coord_flip() +
  scale_fill_brewer(palette = "Spectral") +
  theme(axis.text.x = element_text(angle = 45)) +
  theme_ipsum() + 
  labs(title = "Chart 6: Antidemocratic values per week", 
       subtitle = "Simple frequencies (Absolute values)",
       x = "Week",
       y = "Memes",
       fill = "Antidemocratic values",
       caption = "Source: coLAB/UFF")


### LINE

library(RColorBrewer)
#display.brewer.all()

corr_line_eng <- corr_line %>% 
  mutate(
    week = case_when(
      grepl("semana 01", week) ~ "01",
      grepl("semana 02", week) ~ "02",
      grepl("semana 03", week) ~ "03",
      grepl("semana 04", week) ~ "04",
      grepl("semana 05", week) ~ "05",
      grepl("semana 06", week) ~ "06",
      grepl("semana 07", week) ~ "07",
      grepl("semana 08", week) ~ "08",
      grepl("semana 09", week) ~ "09",
      grepl("semana 10", week) ~ "10",
      grepl("semana 11", week) ~ "11",
      grepl("semana 12", week) ~ "12",
      grepl("semana 13", week) ~ "13",
      grepl("semana 14", week) ~ "14",
      grepl("semana 15", week) ~ "15",
      grepl("semana 16", week) ~ "16",
      grepl("semana 17", week) ~ "17",
      grepl("semana 18", week) ~ "18",
      grepl("semana 19", week) ~ "19",
      grepl("semana 20", week) ~ "20",
      grepl("semana 21", week) ~ "21",
      grepl("semana 22", week) ~ "22",
      grepl("semana 23", week) ~ "23",
      grepl("semana 24", week) ~ "24",
      grepl("semana 25", week) ~ "25",
      grepl("semana 26", week) ~ "26",
      grepl("semana 27", week) ~ "27"))

ggplot(corr_line_eng, aes(x=week, y=percent, group=line, color=line)) +
  geom_line(stat = "identity", alpha=.8, size = .9) +
  scale_color_brewer(palette="Dark2") +
    theme_ipsum() +
  #theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  theme(axis.text.x = element_text(angle = 0), legend.position = "none") +
  labs(title = "Chart 4: Historical series",
       subtitle = "Bolsonaro memes vs. Bolsonaro's statements",
       x = "Week",
       y = "Frequency",
       caption = "Source: coLAB/UFF")

### IMAGENS 1 A 3

library(cowplot)

imagem1 <- ggdraw() + 
  draw_image("/Volumes/HD VIKTOR/meus datasets/DADOS BolsoGrupos/memes_bozozap/IMG-20200520-WA0006.jpg", scale = 0.9)
imagem2 <- ggdraw() + 
  draw_image("/Volumes/HD VIKTOR/meus datasets/DADOS BolsoGrupos/memes_bozozap/IMG-20200505-WA0126.jpg", scale = 0.9)
imagem3 <- ggdraw() + 
  draw_image("/Volumes/HD VIKTOR/meus datasets/DADOS BolsoGrupos/memes_bozozap/IMG-20200425-WA0783.jpg", scale = 0.9)

# Criar lista para imagens a serem plotadas
plot_row <- plot_grid(imagem1, imagem2, imagem3, ncol =3)

# Adicionar título
title <- ggdraw() +
  draw_label(
    "Figures 1 to 3: Humor and far-right memes",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme_ipsum()

# Adicionar legenda
caption <- ggdraw() +  
  theme_ipsum() +
  labs(caption = "Source: coLAB/UFF")

# Plotar imagens com título e legenda
plot_grid(
  title, plot_row, caption,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)

### IMAGENS 4 A 6

imagem4 <- ggdraw() + 
  draw_image("/Volumes/HD VIKTOR/meus datasets/DADOS BolsoGrupos/memes_bozozap/IMG-20200523-WA0017.jpg", scale = 0.9)
imagem5 <- ggdraw() + 
  draw_image("/Volumes/HD VIKTOR/meus datasets/DADOS BolsoGrupos/memes_bozozap/IMG-20200505-WA0072.jpg", scale = 0.9)
imagem6 <- ggdraw() + 
  draw_image("/Volumes/HD VIKTOR/meus datasets/DADOS BolsoGrupos/memes_bozozap/IMG-20200523-WA0165.jpg", scale = 0.9)

# Criar lista para imagens a serem plotadas
plot_row <- plot_grid(imagem4, imagem5, imagem6, ncol =3)

# Adicionar título
title <- ggdraw() +
  draw_label(
    "Figures 4 to 6: References to pop culture in Bolsonaro memes",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme_ipsum()

# Adicionar legenda
caption <- ggdraw() +  
  theme_ipsum() +
  labs(caption = "Source: coLAB/UFF")

# Plotar imagens com título e legenda
plot_grid(
  title, plot_row, caption,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)

### IMAGENS 7 A 9

imagem7 <- ggdraw() + 
  draw_image("/Volumes/HD VIKTOR/meus datasets/DADOS BolsoGrupos/memes_bozozap/IMG-20200101-WA0166.jpg", scale = 0.9)
imagem8 <- ggdraw() + 
  draw_image("/Volumes/HD VIKTOR/meus datasets/DADOS BolsoGrupos/memes_bozozap/IMG-20200531-WA0335.jpg", scale = 0.9)
imagem9 <- ggdraw() + 
  draw_image("/Volumes/HD VIKTOR/meus datasets/DADOS BolsoGrupos/memes_bozozap/IMG-20200503-WA0135.jpg", scale = 0.9)

# Criar lista para imagens a serem plotadas
plot_row <- plot_grid(imagem7, imagem8, imagem9, ncol =3)

# Adicionar título
title <- ggdraw() +
  draw_label(
    "Figures 7 to 9: Historical and religious reinterpretations",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme_ipsum()

# Adicionar legenda
caption <- ggdraw() +  
  theme_ipsum() +
  labs(caption = "Source: coLAB/UFF")

# Plotar imagens com título e legenda
plot_grid(
  title, plot_row, caption,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)





