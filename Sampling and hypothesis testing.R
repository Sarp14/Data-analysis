# Ucitavanje neophodnih paketa
library(rjson)
library(corrplot)
library(e1071)
library(plotly)
library(tidyr)
library(ggplot2)
library(ggfortify)
library(lmtest)
library(dplyr)
library(gplots)
library(naniar)
library(ggcorrplot)
library(car)
library(whitestrap)
library(olsrr)
library(viridis)
library(TeachingSampling)

# Podaci su prikupljeni sa sajta Kaggle: https://www.kaggle.com/datasets/undp/human-development
# Za potrebe analize koriscena su dva csv fajla: human_development.csv koji sadrzi podatke o osnovim varijablama koriscenjim za merenje HDI indeksa i 
# inequality_adjusted.csv koji sadrzi podatke o varijablama vezanim za ekonomsku nejednakost.
# Iskoriscen je i fajl: gdp_per_capita.csv u kome se nalaze podaci za BDP po zemaljama za dugi niz godina. 
# Ovde je koriscena samo kolona za 2015. godinu, kako bi se uklopila sa podacima iz prethodna dva csv fajla, gde svi podaci odgovaraju datoj godini. 
# GDP fajl je takodje skinut sa Kaggle-a: https://www.kaggle.com/datasets/zgrcemta/world-gdpgdp-gdp-per-capita-and-annual-growths


# Ucitavanje samih podataka, za human development skup podataka
hdi <- read.csv("/Users/ndamljanovic/Downloads/archive/human_development.csv")

# Pregled tabele i kolona 
View(hdi)
head(hdi)
str(hdi)

# Konvertovanje GNI per capita kolone u numericku 
hdi$Gross.National.Income..GNI..per.Capita <- as.numeric(gsub(",", ".", gsub("\\.", "", 
                                                 hdi$Gross.National.Income..GNI..per.Capita)))

str(hdi$Gross.National.Income..GNI..per.Capita)

# Evidencija i uklanjanje nedostajucih vrednosti za HDI skup podataka
sum(is.na(hdi))
hdi_new <- na.omit(hdi)

# Skup podataka za BDP po glavi stanovnika, izdvojicemo kolonu koja se odnosi na 2015. godinu 
gdp <- read.csv("/Users/ndamljanovic/Downloads/archive (9)/gdp_per_capita.csv")
str(gdp)

gdp_2015 <- gdp %>%
  select(Country.Name, X2015) %>%
  rename(Country = Country.Name, `GDP_per_cap` = X2015)

View(gdp_2015)

# Uklonicemo nedostajuce vrednosti
gdp_2015 <- na.omit(gdp_2015)

# Ucitavanje samih podataka, za inequality skup podataka
ineq <- read.csv("/Users/ndamljanovic/Downloads/archive/inequality_adjusted.csv")

# Pregled tabele i kolona
View(ineq)
head(ineq)
str(ineq)

# Evidencija i uklanjanje nedostajucih vrednosti za inequality skup podataka
sum(is.na(ineq))
ineq_new <- na.omit(ineq)
View(ineq_new)

# Dodacemo kolonu za BDP po glavni stanovnika za 2015 godinu HDI skupu podataka po koloni koja sadrzi imena drzava
hdi_new <- hdi_new %>%
  left_join(gdp_2015 %>% select(Country, GDP_per_cap), by = "Country")

View(hdi_new)

# Spajanje HDI sa novom GDP kolonom i Inequality skupova podataka po koloni koja sadrzi imena drzava
merged_data <- hdi_new %>%
  left_join(ineq_new %>% select(Country, Income.Inequality..Quintile.Ratio., 
                                Income.Inequality..Palma.Rati., Income.Inequality..Gini.Coefficient.), by = "Country")
View(merged_data)

# Dodelicmo svim kolonama drugacija imena radi bolje citljivosti 
new_column_names <- c("HDI_rank", "Country", "HDI", "Life_expectancy", "Expected_years_of_educ", "Mean_educ", "GNI_per_capita",
                      "GNI_per_cap_minus_HDI_rank", "GDP_per_capita" , "Inequality_quintile_rank", "Inequality_palma_ratio", "Gini_coef")  


colnames(merged_data) <- new_column_names
View(merged_data)
str(merged_data)


# Vidimo na novi skup podataka sadrzi 26 nedostajucih vrednosti 
sum(is.na(merged_data))


# Vizualizacija nedostajucih vrednosti u novom skupu podataka
gg_miss_var(merged_data) +
  labs(title = "Prikaz nedostajucih vrednosti", 
       x = "Varijable", y = "Broj nedostajucih vrednosti") +
  theme_minimal() +
  theme(axis.text = element_text(size = 8)) +
  theme(plot.title = element_text(hjust = 0.5))

# Mozemo videti da su svih 26 nedostajucih vrednosti prisutne u GDP_per_capita koloni 
# Kada pogledamo skup podataka preko View() funkcije mozemo videti da zapravo tri kolone vezane za nejednakost
# sadrze nepostoje vrednosti, tacnije .. sto R iz nekog razloga ne prepoznaje kao NA, tj nedostajuce vrednosti.

# Zamenicemo sve celije koje sadrze .. sa NA kako bismo ponovili vizualizaciju i stekli uvid o pravom broju NA vrednosti
merged_data[merged_data == ".."] <- NA
gg_miss_var(merged_data) +
  labs(title = "Prikaz nedostajucih vrednosti", 
       x = "Varijable", y = "Broj nedostajucih vrednosti") +
  theme_minimal() +
  theme(axis.text = element_text(size = 8)) +
  theme(plot.title = element_text(hjust = 0.5)) 

# Nakon izvrsene zamene vidimo da zapravo te tri kolone prednjace sto se NA vrednosti tice, dok je GDP_per_capita na 4. mestu
# Zamenicemo NA vrednosti u datim kolonama sa medijalnim vrednostima tih kolona, kako ne bismo izgubili znacajan deo populacije
# sa obzirom da u tim kolonama .. tj NA vrednosti idu i preko 40

# Mozemo uociti da su poslednje tri varijable, vezane za ekonomsku nejednakost u char tipu, zbog toga ih prebacujemo u numericki
# tip kako bismo mogli da unesemo medijalne vrednosti

str(merged_data)

merged_data <- merged_data %>%
  mutate(Inequality_quintile_rank = as.numeric(Inequality_quintile_rank),
         Inequality_palma_ratio = as.numeric(Inequality_palma_ratio),
         Gini_coef = as.numeric(Gini_coef))

# Unosimo medijalne vrednosti za date 4 kolone umesto nedostajucih vrednosti
merged_data <- merged_data %>%
  mutate_at(vars(GDP_per_capita, Inequality_quintile_rank, Inequality_palma_ratio, Gini_coef),
            ~ ifelse(is.na(.), median(., na.rm = TRUE), .))

# Vidimo da vise nemamo nedostajucih vrednosti u datom skupu podataka
sum(is.na(merged_data))
View(merged_data)

# Vizualizacija celokupnog skupa podataka, nakon spajanja vise tabela i kolona zajedno. 
# Prikazana je choroplet mapa uz pomoc Ploty paketa za interaktivnu vizualizaciju,na mapi su prikazani neki od kljucnih indikatora ljudskog razvoja,
# sam HDI index, BNP po glavi stanovnika, Gini koeficijent i ocekivani zivotni vek. Raspon boja reflektuje razlicite HDI nivoe razlicitih zeamalja. 
# Mozemo uociti jasnu razliku u nivou HDI indeksa medju razlicitim zemljama i grupama zemalja, koja korespondira i sa ostalim indikatorima. 
map <- plot_ly(data = merged_data, type = "choropleth", locations = ~Country,
               locationmode = "country names", colorscale = "Cividis",
               z = ~HDI,
               text = ~paste("Country: ", Country, "<br>GNI per Capita: ", GNI_per_capita, 
                             "<br>Life Expectancy :", Life_expectancy,
                             "<br>Gini Coef: ", Gini_coef, "<br>HDI: ", HDI),
               hoverinfo = "text",
               colorbar = list(title = "HDI"))

map <- map %>% layout(title = "Prikaz kljucnih HDI indikatora za ceo svet", margin = list(t = 130))

# Prikaz mape
map

# Uzorokovanje - koristimo varijablu bruto domaci proizvod po glavi stanovnika (GDP_per_capita)
n <- 50
set.seed(321)

# Odabir verovatnoce svake jedinice proporcionalne varijabli BDP po glavi stanovnika
res <- S.piPS(n, merged_data$Gini_coef)
# Selektovani uzorak
sam <- res[,1]
# Verovatnoce ukljucivanja svake jedinice u uzorku
Pik.s <- res[,2]
# Kreira se novi data frame uzorak_hdi koji sadrzi jedinice dobijene PPS metodom bez ponavljanja
uzorak_hdi <- merged_data[sam,]

# Pregled uzorka od 50 zemalja 
View(uzorak_hdi)

# Deskriptivna analiza uzorka 

# Vidimo da novi dataframe koji smo dobili uzorkovanjem sadrzi 50 redova i 12 kolona
# Nakon izmena koje smo izvrsili sve kolone su kvantitativnog tipa sem kolone Drzava
str(uzorak_hdi)

# Pregled sumarnih statistika na nivou celokupnog uzorka
summary(uzorak_hdi[, !(names(uzorak_hdi) %in% c("HDI_rank", "Country"))])

# Raspodela osnovnih deskriptivnih statistika za par najvaznijih varijabli 
ggplot(uzorak_hdi, aes(x = "", y = GDP_per_capita)) +
  geom_boxplot(fill = "red", alpha = 0.6, width = 0.6) +
  theme_minimal() +
  ggtitle("BDP po glavi stanovnika") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("") +
  ylab("BDP")


ggplot(uzorak_hdi, aes(x = "", y = Life_expectancy)) +
  geom_boxplot(fill = "darkgreen", alpha = 0.6, width = 0.6) +
  theme_minimal() +
  ggtitle("Ocekivani zivotni vek") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("") +
  ylab("Zivotni vek")

ggplot(uzorak_hdi, aes(x = "", y = Mean_educ)) +
  geom_boxplot(fill = "orange", alpha = 0.6, width = 0.6) +
  theme_minimal() +
  ggtitle("Prosecne godine skolovanja") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("") +
  ylab("Godine skolovanja")


# Mozemo uociti veliki raspon u vrednostima medju odredjenim varijablama. 
# Pogotovo kod GNI i GDP per capita, Gini coef, Mean_educ i HDI

# Bar chart koji prikazuje zemlje po njihovom Dzini koeficijentu
uzorak_hdi %>%
  arrange(Gini_coef) %>%
  plot_ly(x = ~Gini_coef, y = ~seq_along(Country),
          type = "bar", orientation = 'h', marker = list(color = "skyblue"),
          text = ~Country) %>%
  layout(title = "Drzave po Dzini koeficijentu",
         xaxis = list(title = "Dzini koeficijent"),
         yaxis = list(title = ""))


# Dijagram rasturanja za zemlje po BDP-u po glavi stanovika 
plot <- plot_ly(
  data = uzorak_hdi,
  x = ~Country,
  y = ~GDP_per_capita,
  text = ~paste("Country: ", Country, "<br>GDP Per Capita: $", GDP_per_capita),
  type = "scatter",
  mode = "markers",
  marker = list(size = 12, color = ~GDP_per_capita, colorscale = "Viridis")
) %>% layout(
  title = list(text = "BDP po glavi stanovnika za drzave u uzorku", y = 0.9),  
  xaxis = list(title = "Drzave", 
               tickangle = 45),
  yaxis = list(title = "BDP"),
  margin = list(t = 80)  # Increase top margin
)

# Prikazivanje grafika
plot


# Korelaciona matrica numerickih varijabli 
kvant_uzorak_hdi <- uzorak_hdi %>%
  select(-Country, -GNI_per_cap_minus_HDI_rank ,-HDI_rank)


cor_matrix_hdi <- cor(kvant_uzorak_hdi)

ggcorrplot(cor_matrix_hdi, 
           title = "Korelaciona matrica",
           ggtheme = theme_minimal(),  
           colors = c("#6D9EC1", "white", "#E46726"),
           hc.order = TRUE,
           lab = TRUE,
           method = "square") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, margin = margin(b = 20)),  
    axis.text.x = element_text(size = 10),  
    axis.text.y = element_text(size = 10),  
    axis.title = element_text(size = 14)  
  )


# Dvostruki linearni model, koristimo Bruto domaci proizvod po glavi stanovnika i ocekivani zivotni vek, 
# kao nezavisne i HDI indeks kao zavisnu promenljivu
set.seed(123)
model <- lm(HDI ~ GDP_per_capita + Life_expectancy, uzorak_hdi)
summary(model)

# Primecujemo da je model statisticki znacajan. Obe zavisne varijable ispoljavaju statisticku znacajnost: 
# GDP_per_capita   7.837e-07  2.382e-07    3.29   0.0018 ** 
# Life_expectancy  1.386e-02  1.126e-03   12.30   <2e-16 ***

# Statisticki je znacajna i cela regresija sto vidimo po F statistici i njenoj p vrednosti: 
# F-statistic:  137.1 on 2 and 52 DF,  p-value: < 2.2e-16


# Vrednosti koeficijenta determinacije i prilagodjenog koeficijenta determinacije su: 
# Multiple R-squared:  0.8406,	Adjusted R-squared:  0.8345  
# Pri cemu zakljucujemo da vecinu  varijabiliteta (83%) zavisne promenljive mozemo objasniti datim modelom. 

# Testiracemo kljucne predpostavke linearnog modela
# Da li reziduali modela imaju normalnu raspodelu

# Vizuelni prikaz raspodele reziduala, vecina reziduala se nalazi na samoj krivi
plot(model, 2, main = "Vizuelni prikaz raspodele reziduala")

# Formalno testiranje uz pomoc Shapiro Wilk testa ukazuje da reziduali imaju normalnu raspodelu
# p vrednost u ovom slucaju iznosi 0.97476, tacnije veca je od 0.05 (0.2982) zbog cega mozemo zakljuciti da su 
# rezidualni normalno raspodeljeni
set.seed(123)
shapiro.test(model$residuals)

# Korelacija dve nezavisne varijable
cor(uzorak_hdi$GDP_per_capita, uzorak_hdi$Life_expectancy)

# Koriscenjem Faktora rasta varijanse (Variance Inflation Factor) uocavamo da ne postoji izrazena multikolinearnost izmedju objasnjavajucih promenljivih
# Vrednost FRV je izrazenija za vecu multikolinearnost, visoka za vrednost preko 10, dok je u datom modelu  1.372373
set.seed(123)
vif(model)

# Testiranje homoskedasticnosti, tacnije da li je varijansa za sve slucajne greske konstantna. Koriscen je Vajtov test
set.seed(123)
white_test(model)

# Na osnovu rezultata Vajtovog testa: Null hypothesis: Homoskedasticity of the residuals
# Alternative hypothesis: Heteroskedasticity of the residuals
# Test Statistic: 5.47
# P-value: 0.064886
# Zakljucujemo da u modelu nema prisustva heteroskedasticnosti

# Testiranje autokorelacije putem Durbin - Watson testa
set.seed(123)
durbinWatsonTest(model)

# DW statistika je blizu 2 (1.749609) dok je p vrednost 0.29 veca od 0.05
# Na osnovu cega zakljucujemo da ne postoji autokorelacijea prvog reda u datom regresionom modelu 
