#ilk önce verileriçağırmak için heven kutupanesini insert ediyoruz
library(haven)
library(tidyr)
library(xgboost)
library(ggplot2)
library(plotly)
library(dplyr)
library(knitr)
#burda cağıracağımız csv uzantılı data seti a diye bir değişkene tanımlıyoruz
#data<-read.csv("C:/Users/Eugeo/Desktop/analiz vize/train.csv")

#burda Na gözlemleri siliyoruz
#data <- data[complete.cases(data), ]

#bu kodda sample diye bir değişken veri setimizden rastgele 2000 gözlem atıyoruz
#sample <- data[sample(1:nrow(data), 2000), ]

#veriyi kaydediyoruz
#write.table(sample,file = "sample.txt",sep="\t")

#veriyi ekliyoruz
sample <- read.delim("C:/Users/Eugeo/Desktop/analiz vize/sample.txt")

#burda görme derecesini birleştirdim
a <- sample %>% select(eyesight.left.,eyesight.right.)%>% gather("goz", "gorme_keskinligi", eyesight.left., eyesight.right.)
head(a)
#burda duyma derecesini birleştirdim
b<- sample %>% select(hearing.left.,hearing.right.)%>% gather("kulak", "işitme_keskinliği", hearing.left., hearing.right.)
head(b)
#burda kan değerleri birleştirdim
c<- sample %>% select(systolic,relaxation,hemoglobin)%>%gather("kan_basıncı", "değer", systolic, relaxation) %>% gather("kan_yapısı", "değer", hemoglobin)
head(c)
#burda kolostrol değerşerini birleştirdim
d <-sample %>% select(HDL,LDL)%>%gather("cholesterol", "değer", HDL, LDL)
head(d)

# Veriyi train ve test setlere bölme
set.seed(123)
index <- sample(1:nrow(sample), 0.7 * nrow(sample))
train_data <- sample[index, ]
test_data <- sample[-index, ]

# xgboost modeli oluşturma
model <- xgboost(data = as.matrix(train_data[, c("id", "systolic")]), 
                 label = train_data$systolic, 
                 nrounds = 100, 
                 objective = "reg:squarederror")

# Test seti üzerinde tahmin yapma
predictions <- predict(model, as.matrix(test_data[, c("id", "systolic")]))

# Tahmin başarı değerlendirmesi (örneğin, RMSE hesaplamak için)
rmse <- sqrt(mean((predictions - test_data$systolic)^2))
print(paste("RMSE:", rmse))


## 9.2 Calculating Relative Frequencies

#toplam gözlem sayısının buluyıruz
n=nrow(sample)
#burda frekansını buluyoruz
age_freq <- table(sample$age)/n
#bulduğumuz freksan fazla küsratlı olmassın diye 0 dan sonra 5 basamağını alıyoruz
age_freq_rounded <- round(age_freq, digits = 5)
print(age_freq_rounded)

#yaşları gurplar haline getiriyorum
yas_gurupları <- cut(sample$age, breaks = seq(10, 100, 10))
yas_gurupları_sayısı <- as.data.frame( table(yas_gurupları))
yas_gurupları_sayısı

#gruplarhaline gelen yaşları pasta grafiğinde görteriyorum
fig <- plot_ly(yas_gurupları_sayısı, labels = ~paste(yas_gurupları,"Yaşında",sep = " "), values = ~Freq, type = 'pie')
fig <- fig %>% layout(title = 'Yaş sıklığ',
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
fig
### Yorumverinin yüzde:yaşı 20 olanlar oranı 2.5,yaşı 25 olanları oranı 4.9 ,yaşı 30 olanları oranı 7.6,yaşı 35 olanları oranı 8.15,yaşı 40 olanları oranı 29.15,yaşı 45 olanları oranı 11.6,
###yaşı 50 olanları oranı 10,5, yaşı 55 olanları 8.55, yaşı 60 olanları oranı 12.3,yaşı 65 olanları oranı 1.6, yaşı 70 olanları oranı 1.6, yaşı 75 olanları oranı 1.2, yaşı 80 olanları oranı 0.035
#-------------------------------------------------------------------------------------------------
height_frequencies <- table(sample$height.cm.)/n
height_frequencies_rounded <- round(height_frequencies, digits = 5)
print(height_frequencies_rounded)

fig <- plot_ly(sample,
               y = ~height.cm.,
               type = 'violin',
               box = list(
                 visible = TRUE
               ),
               meanline = list(
                 visible = TRUE
               ),
               x0 = 'boy'
)

fig <- fig %>%
  layout(
    yaxis = list(
      title = "",
      zeroline = FALSE
    )
  )

fig
### Yorumverinin yüzde:boyu 140 cm olanları oranı 0.2 dir , boyu 145 cm olanları oranı 1.6 dir, boyu 150 cm olanları oranı 6.7 dir, boyu 155 cm olanları oranı 15.65, boyu 160 cm olanları oranı 14.45,
###boyu 165 cm olanları oranı 18.95, boy 170 cm olanları oranı 22.95, boyu 175 cm olanları oranı 16.3,boy 180 cm olanları oranı 4.95, boyu 185 cm olanları oranı 0.125
#--------------------------------------------------------------------------------------------------
weight_frequencies <- table(sample$weight.kg.)/n
weight_frequencies_rounded <- round(weight_frequencies, digits = 5)
print(weight_frequencies_rounded)

fig <- plot_ly(sample,
               x=~age,
               y = ~weight_frequencies_rounded,
               name = "kilo ",
               type = "bar"
)

fig

#burda yaşları ağırlık ortalamasına göre grupluyoruz
grouped_data <- sample %>%
  group_by(age) %>%
  summarize(mean_weight = mean(weight.kg.))

fig <- plot_ly(data = sample,type = "bar" ,x = ~grouped_data$age, y = ~grouped_data$mean_weight,marker = list(size = 10,color = 'rgba(255, 182, 193, .9)',line = list(color = 'rgba(152, 0, 0, .8)',width = 2)))
fig <- fig %>% layout(title = 'yaş kilo',yaxis = list(zeroline = FALSE),xaxis = list(zeroline = FALSE))
fig

### Yorumverinin yüzde:bel çevresi 30 cm olanları oranı 0.05 dir , bel çevresi 40 cm olanları oranı 0.3 dir, bel çevresi 45 cm olanları oranı 2.75 dir, bel çevresi 50 cm olanları oranı 9.75, bel çevresi 55 cm olanları oranı 11,
###bel çevresi 60 cm olanları oranı 12.85 bel çevresi 65 cm olanları oranı 15.65, bel çevresi 70 cm olanları oranı 15.0,bel çevresi 75 cm olanları oranı 12.35, bel çevresi 80 cm olanları oranı 9.15, bel ceversi 90 olanlarını oranı 2.5,  bel ceversi 95 olanlarını oranı 1.75, bel ceversi 100 olanlarını oranı 0.065, bel ceversi 105 olanlarını oranı 0.2, bel ceversi 110 olanlarını oranı 0.2, bel ceversi 115 olanlarını oranı 0.05,
#----------------------------------------------------------------------------------------------------
hearing_left_frequencies <- table(sample$hearing.left.)/n
hearing_left_frequencies_rounded <- round(hearing_left_frequencies, digits = 5)
print(hearing_left_frequencies_rounded)


fig <- plot_ly(sample,
  x=c(1,2),
  y = ~hearing_left_frequencies_rounded,
  name = "sol kulağı duyan",
  type = "bar"
)

fig
###  Yorumverinin yüzde:sol kulağının duyanların oranı yüzde 97.4 duymayanlarını oranı 2.6 dır.
#----------------------------------------------------------------------------------------------------
hearing_right_frequencies <- table(sample$hearing.right.)/n
hearing_right_frequencies_rounded <- round(hearing_right_frequencies, digits = 5)
print(hearing_right_frequencies_rounded)


fig <- plot_ly(sample,
               x=c(1,2),
               y = ~hearing_right_frequencies_rounded,
               name = "sağ duyan",
               type = "bar"
)

fig

###  Yorumverinin yüzde:sağ kulağının duyanların oranı yüzde 97.45 duymayanlarını oranı 2.55 dır.
#----------------------------------------------------------------------------------------------------
Urine_protein_frequencies <- table(sample$Urine.protein)/n
Urine_protein_frequencies_rounded <- round(Urine_protein_frequencies, digits = 5)
print(Urine_protein_frequencies_rounded)

fig <- plot_ly(sample, x = c(1,2,3,4,5), y = ~Urine_protein_frequencies_rounded, type = 'scatter', mode = 'lines')

fig

###  Yorumverinin yüzde:Urin protein 1 olanlarını oranı 94.3,  Urin protein 2 olanlarını oranı 3.5 ,  Urin protein 3 olanlarını oranı 1.7,  Urin protein 1 olanlarını oranı 0.5
#---------------------------------------------------------------------------------------------------
dental_caries_frequencies <- table(sample$dental.caries)/n
dental_caries_frequencies_rounded <- round(dental_caries_frequencies, digits = 5)
print(dental_caries_frequencies_rounded)

fig <- plot_ly(sample,
               x=c(0,1),
               y = ~dental_caries_frequencies_rounded,
               name = "diş eti hastalığı",
               type = "bar"
)

fig

###  Yorumverinin yüzde:diş eti hastalığ olmayanlarını oranı 85.85 , diş eti hastalığı olanlarını oranı 18.15
#-----------------------------------------------------------------------------------------------------
smoking_frequencies <- table(sample$smoking)/n
smoking_frequencies_rounded <- round(smoking_frequencies, digits = 5)
print(smoking_frequencies_rounded)


fig <- plot_ly(sample,
               x=c(0,1),
               y = ~smoking_frequencies_rounded,
               name = "sigar içip içmeme",
               type = "bar"
)

fig
###  Yorumverinin yüzde:sigara kulananlarınmayanlarının  olanların oranı 55.65 , sigara kulananlarını olmayanlarını oranı 44.35

## 9.6  Inverting a Quantile

#burda ben sayı vermektense rastgele max ve min değrleri arasında bir sayı üretiyorum
random_age <- runif(1, min(sample$age), max(sample$age))
#ürettiğim sayının ortalamadan nekadar büyük olduğunu buluyorum
percentage_less_age <- mean(sample$age < random_age)
print(paste('üretile sayı', random_age, ' verinin yüzde:' , percentage_less_age ))

values <- c(1-percentage_less_age, percentage_less_age)
labels <- c("üretilen sayının karşılamadığı yüzdeliği", "üretilen yaşın karşıladığı yüzdeliği")

# Pasta grafiğini oluştur
fig <- plot_ly(
  values = values,
  labels = labels,
  type = "pie",
  title = "Yaşın yüzdeliği"
)

# Grafikte açıklama ekle
fig <- fig %>% layout(
  annotations = list(
    text = paste0("Yaşın karşıladığı yüzdelikverinin yüzde:", round(values[2] * 100, 2), ""),
    x = 0.5,
    y = 0.8,
    showarrow = FALSE,
    font = list(size = 14)
  )
)

# Grafiki göster
fig


random_height.cm. <- runif(1, min(sample$height.cm.), max(sample$height.cm.))
percentage_less_height.cm. <- mean(sample$height.cm. < random_height.cm.)
print(paste('üretile sayı', random_height.cm., ' verinin yüzde:' , percentage_less_height.cm.*100,"'sını açıklar" ))

random_weight.kg. <- runif(1, min(sample$weight.kg.), max(sample$weight.kg.))
percentage_less_weight.kg. <- mean(sample$weight.kg. < random_weight.kg.)
print(paste('üretile sayı', random_weight.kg., 'verinin yüzde:' , percentage_less_weight.kg.*100,"'sını açıklar" ))

random_waist.cm. <- runif(1, min(sample$waist.cm.), max(sample$waist.cm.))
percentage_less_waist.cm. <- mean(sample$waist.cm. < random_waist.cm.)
print(paste('üretile sayı', random_waist.cm., 'verinin yüzde şukadarını açıklarverinin yüzde:', percentage_less_waist.cm.*100,"'sını açıklar" ))

random_systolic <- runif(1, min(sample$systolic), max(sample$systolic))
percentage_less_systolic <- mean(sample$systolic < random_systolic)
print(paste('üretile sayı', random_systolic, 'verinin yüzde:', percentage_less_systolic *100,"'sını açıklar"))

random_relaxation <- runif(1, min(sample$relaxation), max(sample$relaxation))
percentage_less_relaxation <- mean(sample$relaxation < random_relaxation)
print(paste('üretile sayı', random_relaxation, 'verinin yüzde:', percentage_less_relaxation*100,"'sını açıklar" ))

random_fasting.blood.sugar<- runif(1, min(sample$fasting.blood.sugar), max(sample$fasting.blood.sugar))
percentage_less_fasting.blood.sugar <- mean(sample$fasting.blood.sugar < random_fasting.blood.sugar)
print(paste('üretile sayı', random_fasting.blood.sugar, 'verinin yüzde:', percentage_less_fasting.blood.sugar*100,"'sını açıklar" ))

random_Cholesterol <- runif(1, min(sample$Cholesterol), max(sample$Cholesterol))
percentage_less_Cholesterol <- mean(sample$Cholesterol < random_Cholesterol)
print(paste('üretile sayı', random_Cholesterol, 'verinin yüzde:', percentage_less_Cholesterol*100,"'sını açıklar" ))

random_triglyceride <- runif(1, min(sample$triglyceride), max(sample$triglyceride))
percentage_less_triglyceride <- mean(sample$triglyceride < random_triglyceride)
print(paste('üretile sayı', random_triglyceride, 'verinin yüzde:', percentage_less_triglyceride*100,"'sını açıklar"))

random_HDL <- runif(1, min(sample$HDL), max(sample$HDL))
percentage_less_HDL <- mean(sample$HDL < random_HDL)
print(paste('üretile sayı', random_HDL, 'verinin yüzde:', percentage_less_HDL*100,"'sını açıklar" ))

random_LDL <- runif(1, min(sample$LDL), max(sample$LDL))
percentage_less_LDL <- mean(sample$LDL < random_LDL)
print(paste('üretile sayı', random_LDL, 'verinin yüzde:', percentage_less_LDL*100,"'sını açıklar" ))

random_hemoglobin <- runif(1, min(sample$hemoglobin), max(sample$hemoglobin))
percentage_less_hemoglobin <- mean(sample$hemoglobin < random_hemoglobin)
print(paste('üretile sayı', random_hemoglobin, 'verinin yüzde',percentage_less_hemoglobin*100,"'sını açıklar"))

random_Urine.protein <- runif(1, min(sample$Urine.protein), max(sample$Urine.protein))
percentage_less_Urine.protein <- mean(sample$Urine.protein < random_Urine.protein)
print(paste('üretile sayı', random_Urine.protein, 'verinin yüzde:', percentage_less_Urine.protein*100,"'sını açıklar" ))

random_AST <- runif(1, min(sample$AST), max(sample$AST))
percentage_less_AST <- mean(sample$AST < random_AST)
print(paste('üretile sayı', random_AST, 'verinin yüzde:', percentage_less_AST*100,"'sını açıklar" ))

random_ALT<- runif(1, min(sample$ALT), max(sample$ALT))
percentage_less_ALT <- mean(sample$ALT < random_ALT)
print(paste('üretile sayı', random_ALT, 'verinin yüzde:', percentage_less_ALT*100,"'sını açıklar" ))

random_Gtp <- runif(1, min(sample$Gtp), max(sample$Gtp))
percentage_less_Gtp <- mean(sample$Gtp < random_Gtp)
print(paste('üretile sayı', random_Gtp, 'verinin yüzde:', percentage_less_Gtp*100,"'sını açıklar" ))

random_serum.creatinine <- runif(1, min(sample$serum.creatinine), max(sample$serum.creatinine))
percentage_less_serum.creatinine <- mean(sample$serum.creatinine < random_serum.creatinine)
print(paste('üretile sayı', random_serum.creatinine, 'verinin yüzde:', percentage_less_serum.creatinine*100,"'sını açıklar" ))


## 9.10 Forming a Confidence Interval for a Median

wilcox.test(sample$fasting.blood.sugar, conf.int = TRUE)



fig <- plot_ly(sample,
               x = sample$fasting.blood.sugar,
               type = "histogram"
)

fig
### Açlık kan şekeri dağılımının gerçek medyanı 0'dan farklıdır. %95 güven aralığı 96.00002 ile 97.00007 arasında ve tahmini medyan 96.50006'dır.
#Bu, açlık kan şekeri dağılımının muhtemelen sağa doğru çarpık olduğunu gösteriri

wilcox.test(sample$age, conf.int = TRUE)
wilcox.test(sample$height.cm., conf.int = TRUE)

wilcox.test(sample$weight.kg., conf.int = TRUE)

wilcox.test(sample$waist.cm., conf.int = TRUE)

wilcox.test(sample$eyesight.left., conf.int = TRUE)

wilcox.test(sample$eyesight.right., conf.int = TRUE)

wilcox.test(sample$systolic, conf.int = TRUE)

wilcox.test(sample$relaxation, conf.int = TRUE)

wilcox.test(sample$fasting.blood.sugar, conf.int = TRUE)

wilcox.test(sample$Cholesterol, conf.int = TRUE)

wilcox.test(sample$triglyceride, conf.int = TRUE)

wilcox.test(sample$HDL, conf.int = TRUE)

wilcox.test(sample$LDL, conf.int = TRUE)

wilcox.test(sample$hemoglobin, conf.int = TRUE)

wilcox.test(sample$serum.creatinine, conf.int = TRUE)

wilcox.test(sample$AST, conf.int = TRUE)

wilcox.test(sample$ALT, conf.int = TRUE)

wilcox.test(sample$Gtp, conf.int = TRUE)

### 9.13  Testing for Normality

p <- shapiro.test(sample$triglyceride)$p.value
shapiro.test(sample$triglyceride)
p
if (p[1] < 0.05) {
  print("Veri kümesi normal dağılım gösteriyor.")
} else {
  print("Veri kümesi normal dağılım göstermiyor.")
}
fig <- plot_ly(sample, x = ~triglyceride, type = "histogram")
fig



p <- shapiro.test(sample$age)$p.value
p
shapiro.test(sample$age)
if (p[1] <  0.05) {
  print('Veri kümesi normal dağılım gösteriyor.')
} else {
  print('Veri kümesi normal dağılım göstermiyor.')
}
fig <- plot_ly(sample, x = ~age, type = 'histogram')
fig

p <- shapiro.test(sample$height.cm.)$p.value
p
shapiro.test(sample$height.cm.)
if (p[1] < 0.05) {
  print('Veri kümesi normal dağılım gösteriyor.')
} else {
  print('Veri kümesi normal dağılım göstermiyor.')
}
fig <- plot_ly(sample, x = ~height.cm., type = 'histogram')
fig

p <- shapiro.test(sample$weight.kg.)$p.value
p
shapiro.test(sample$weight.kg.)
if (p[1] < 0.05) {
  print('Veri kümesi normal dağılım gösteriyor.')
} else {
  print('Veri kümesi normal dağılım göstermiyor.')
}
fig <- plot_ly(sample, x = ~weight.kg., type = 'histogram')
fig

p <- shapiro.test(sample$waist.cm.)$p.value
p
shapiro.test(sample$waist.cm.)
if (p[1] < 0.05) {
  print('Veri kümesi normal dağılım gösteriyor.')
} else {
  print('Veri kümesi normal dağılım göstermiyor.')
}
fig <- plot_ly(sample, x = ~waist.cm., type = 'histogram')
fig

p <- shapiro.test(sample$waist.cm.)$p.value
p
shapiro.test(sample$waist.cm.)
if (p[1] < 0.05) {
  print('Veri kümesi normal dağılım gösteriyor.')
} else {
  print('Veri kümesi normal dağılım göstermiyor.')
}
fig <- plot_ly(sample, x = ~waist.cm., type = 'histogram')
fig

p <- shapiro.test(sample$eyesight.left.)$p.value
p
shapiro.test(sample$eyesight.left.)
if (p[1] < 0.05) {
  print('Veri kümesi normal dağılım gösteriyor.')
} else {
  print('Veri kümesi normal dağılım göstermiyor.')
}
fig <- plot_ly(sample, x = ~eyesight.left., type = 'histogram')
fig

p <- shapiro.test(sample$eyesight.right.)$p.value
p
shapiro.test(sample$eyesight.right.)
if (p[1] < 0.05) {
  print('Veri kümesi normal dağılım gösteriyor.')
} else {
  print('Veri kümesi normal dağılım göstermiyor.')
}
fig <- plot_ly(sample, x = ~eyesight.right., type = 'histogram')
fig

p <- shapiro.test(sample$systolic)$p.value
p
shapiro.test(sample$systolic)
if (p[1] < 0.05) {
  print('Veri kümesi normal dağılım gösteriyor.')
} else {
  print('Veri kümesi normal dağılım göstermiyor.')
}
fig <- plot_ly(sample, x = ~systolic, type = 'histogram')
fig

p <- shapiro.test(sample$relaxation)$p.value
p
shapiro.test(sample$relaxation)
if (p[1] < 0.05) {
  print('Veri kümesi normal dağılım gösteriyor.')
} else {
  print('Veri kümesi normal dağılım göstermiyor.')
}
fig <- plot_ly(sample, x = ~relaxation, type = 'histogram')
fig

p <- shapiro.test(sample$fasting.blood.sugar)$p.value
p
shapiro.test(sample$fasting.blood.sugar)
if (p[1] < 0.05) {
  print('Veri kümesi normal dağılım gösteriyor.')
} else {
  print('Veri kümesi normal dağılım göstermiyor.')
}
fig <- plot_ly(sample, x = ~fasting.blood.sugar, type = 'histogram')
fig

p <- shapiro.test(sample$Cholesterol)$p.value
p
shapiro.test(sample$Cholesterol)
if (p[1] >0.05) {
  print('Veri kümesi normal dağılım gösteriyor.')
} else {
  print('Veri kümesi normal dağılım göstermiyor.')
}
fig <- plot_ly(sample, x = ~Cholesterol, type = 'histogram')
fig

p <- shapiro.test(sample$HDL)$p.value
p
shapiro.test(sample$HDL)
if (p[1] < 0.05) {
  print('Veri kümesi normal dağılım gösteriyor.')
} else {
  print('Veri kümesi normal dağılım göstermiyor.')
}
fig <- plot_ly(sample, x = ~HDL, type = 'histogram')
fig

p <- shapiro.test(sample$hemoglobin)$p.value
p
shapiro.test(sample$hemoglobin)
if (p[1] < 0.05) {
  print('Veri kümesi normal dağılım gösteriyor.')
} else {
  print('Veri kümesi normal dağılım göstermiyor.')
}

fig <- plot_ly(sample, x = ~hemoglobin, type = 'histogram')
fig

p <- shapiro.test(sample$serum.creatinine)$p.value
p
shapiro.test(sample$serum.creatinine)

if (p[1] < 0.05) {
  print('Veri kümesi normal dağılım gösteriyor.')
} else {
  print('Veri kümesi normal dağılım göstermiyor.')
}
fig <- plot_ly(sample, x = ~serum.creatinine, type = 'histogram')
fig

p <- shapiro.test(sample$AST)$p.value
p
shapiro.test(sample$AST)
if (p[1] < 0.05) {
  print('Veri kümesi normal dağılım gösteriyor.')
} else {
  print('Veri kümesi normal dağılım göstermiyor.')
}
fig <- plot_ly(sample, x = ~AST, type = 'histogram')
fig

p <- shapiro.test(sample$ALT)$p.value
p
shapiro.test(sample$ALT)
if (p[1] < 0.05) {
  print('Veri kümesi normal dağılım gösteriyor.')
} else {
  print('Veri kümesi normal dağılım göstermiyor.')
}
fig <- plot_ly(sample, x = ~ALT, type = 'histogram')
fig

p <- shapiro.test(sample$Gtp)$p.value
p
shapiro.test(sample$Gtp)
if (p[1] < 0.05) {
  print('Veri kümesi normal dağılım gösteriyor.')
} else {
  print('Veri kümesi normal dağılım göstermiyor.')
}
fig <- plot_ly(sample, x = ~Gtp, type = 'histogram')
fig





# 9.17 Testing a Correlation for Significance

cor.test(sample$systolic,sample$relaxation,method = "pearson",alternative = "less")
### Yorum : Bu analiz, sistol ve diyastolik kan basıncı arasında anlamlı ve % 73'lül pozitif bir ilişki olduğunu %95 güven düzeyinde olduğu söylenebilir göstermektedir. 

fig <- plot_ly(data = sample, x = ~systolic, y = ~relaxation)
fig <- fig %>% add_markers()
fig <- fig %>% layout(title = "Sistolik ve Gevşeme Kan Basınçları Arasındaki Korelasyon", xaxis = list(title = "Sistolik Kan Basıncı (mmHg)"), yaxis = list(title = "Gevşeme Kan Basıncı (mmHg)"))
fig


cor.test(sample$LDL,sample$HDL, method = 'spearman',alternative = 'two.sided')

fig <- plot_ly(data = sample, x = ~systolic, y = ~relaxation)
fig <- fig %>% add_markers()
fig <- fig %>% layout(title = 'Sistolik ve Gevşeme Kan Basınçları Arasındaki Korelasyon', xaxis = list(title = 'Sistolik Kan Basıncı (mmHg)'), yaxis = list(title = 'Gevşeme Kan Basıncı (mmHg)'))
fig

cor.test(sample$relaxation,sample$fasting.blood.sugar,method = 'kendall',alternative = 'greater')
fig <- plot_ly(data = sample, x = ~systolic, y = ~relaxation)
fig <- fig %>% add_markers()
fig <- fig %>% layout(title = 'Sistolik ve Gevşeme Kan Basınçları Arasındaki Korelasyon', xaxis = list(title = 'Sistolik Kan Basıncı (mmHg)'), yaxis = list(title = 'Gevşeme Kan Basıncı (mmHg)'))
fig

