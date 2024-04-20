
####   GIZEM CICEKLI  ####

##### VERI MADENCILIGI Hafta - 6: 04.04.2024 / Kesifsel Veri Analizi, RStudio Uygulama

####### 401. SATIR SONRASI VERI URETEREK KESIFSEL VERI ANALIZI UYGULAMASI 


         #################################################
######## 1- VERI CEKEREK KESIFSEL VERI ANALIZI UYGULAMASI #########
         #################################################

#Calısma izini tespiti 
getwd()

# Excel'den veri setini aktarma

install.packages("readxl")
library(readxl)

vr<- read_excel("C:/Users/HP/OneDrive/veri.xlsx") #EXCEL DOSYASINI GitHub a yükledim.

#ADIM1: veri Kayna????
#Veriyi lisans d??neminde ald??????m veri analizi dersinde kulland??????m??z bir ??rnek veri setinden ald??m.
#Bu veri seti, bir k??yde ya??ayan bireylerin baz?? demografik bilgilerini ve sa??l??k verilerini
#i??ermektedir. 


# Veri setinin ilk birka?? g??zlemini g??zden ge??irme
head(vr)

# Veri setinin boyutunu kontrol etme
dim(vr)
# Veri setinin s??tun isimlerini ve veri tiplerin, yap??s??n?? kontrol etme
str(vr)

# Veri setinin s??tun isimlerini de??i??tirme

install.packages("dplyr")
library(dplyr) 

vr1<-rename(vr, cinsiyet=cins, kolestrol=kol, enerji=enrj)

# De??i??iklikten sonra veri setinin s??tun isimlerini kontrol etme
names(vr1)

#ADIM:2 Aktar??m ve Gerekli ise D??n??????m

# k??y, cinsiyet,??grdurum,sikayet,sigara de??i??kenini fakt??rel de??i??kene d??n????t??rme
vr1$koy <- as.factor(vr1$koy)
vr1$cinsiyet <- as.factor(vr1$cinsiyet)
vr1$ogrdurum <- as.factor(vr1$ogrdurum)
vr1$sikayet <- as.factor(vr1$sikayet)
vr1$sigara <- as.factor(vr1$sigara)
# yas, boy, kilo,kolestrol,hdl,ldl,enerji,prot,ilacsonrasikol de??i??kenleri zaten say??sal de??iken olarak kodlanm???? d??n??????me gerek yok 
# gerek olsayd??;
#vr1$yas <- as.numeric(vr1$yas) olarak yap??l??rd??.


str(vr1)

#Temel istatistikleri g??r??nt??leme---- eksik veriler de g??r??n??yor. 

summary(vr1)
#koyde yasayan kisiilerin yaslarinin toplami
sum(vr1$yas)
#veri setini bir data.frame veri cercevesine donusturme
vr_dataframe=data.frame(vr1)
str(vr_dataframe)


#ADIM:3 VERI KALITESI-Eksik-aykırı veri tespiti


# Eksik degerleri tespit etme
eksik_degerler <- is.na(vr1)

# Eksik degerlerin ozeti

summary(eksik_degerler)

#koy,yas , boy degiskenlerinde eksik deger yok.
#cinsiyet, sikayet,kilo,prot degiskenlerinde 2 eksik deger bulunmaktadir.
#kolestrol, enerji,ilacsonrasikol degiskenlerinde 3 eksik deger bulunmaktadir.
#ogrdurum,sigara, hdl, ldl ,degiskenlerindee 1 eksik degerbulunmaktadir.

# Eksik verileri ortalama ile doldurma

# Eksik degerleri teker teker tespit etme true donernerse eksik dger var.

is_na_cinsiyet <- is.na(vr1$cinsiyet)
is_na_sikayet <- is.na(vr1$sikayet)
is_na_kilo <- is.na(vr1$kilo)
is_na_prot <- is.na(vr1$prot)
is_na_kolestrol <- is.na(vr1$kolestrol)
is_na_enerji <- is.na(vr1$enerji)
is_na_ilacsonrasikol <- is.na(vr1$ilacsonrasikol)
is_na_ogrdurum <- is.na(vr1$ogrdurum)
is_na_sigara <- is.na(vr1$sigara)
is_na_hdl <- is.na(vr1$hdl)
is_na_ldl <- is.na(vr1$ldl)

# # Eksik degerlerin sayisini ve oranini bulma
missing_count_kolestrol <- sum(is_na_kolestrol)
missing_count_cinsiyet <- sum(is_na_cinsiyet)


# Eksik degerleri  ortalama doldurma



# Fonksiyon  tanımlayarak doldurma: 
ortalama_ile_doldur <- function(data) {
  # Numeric degiskenleri bulma
  Numeric_degisken <- sapply(data, is.numeric)
  
  # Numeric degisken ortalamasını hesaplama
  ortalama <- colMeans(data[, Numeric_degisken], na.rm = TRUE)
  
  # Numeric degisken için eksik değerleri ortalama ile doldurma
  data[, Numeric_degisken] <- lapply(data[, Numeric_degisken], function(x) {
    ifelse(is.na(x), ortalama, x)
  })
  
  # Doldurulmuş veriyi döndürme
  return(data)
}

# Fonksiyonu kullanarak eksik değerleri ortalama ile doldurma
vr_1_doldurulmus <- ortalama_ile_doldur(vr1)

# Doldurulmuş veriyi kontrol et
print(vr_1_doldurulmus)



### sadece numerik değişkenlerin ortalaması alınabilir bu sebeple kategorik değişkenleri ortalama ile dolduramadık

# Kategorik değişkenleri doldurma: 
# doldurma Fonksiyonu tanımlayarak doldurma
mod_ile_doldur <- function(data) {
  # Faktör degisken indekslerini bul
  faktor_degisken<- sapply(data, is.factor)
  
  # Her bir faktör sütunu için modu bul ve eksik değerleri doldur
  for (i in which(faktor_degisken)) {
    mod <- names(sort(table(data[, i]), decreasing = TRUE))[1]  # Mod değeri bul
    data[is.na(data[, i]), i] <- mod  # Eksik değerleri mod ile doldur
  }
  
  # Doldurulmuş veriyi döndür
  return(data)
}

# Fonksiyonu kullanarak faktör değişkenleri mod ile doldur
vr2_dolu <- mod_ile_doldur(vr_1_doldurulmus)

# Doldurulmuş veriyi kontrol et
print(vr2_dolu)

##### Aykırı değer tespiti

str(vr2_dolu)

boxplot(vr2_dolu$yas, main = "Yas Dagilimi", ylab = "Yas") # aykırı değer gözlemlendi.Aykırı değer üst sapın üstünde nokta halinde görülüyor   
boxplot(vr2_dolu$boy, main = "Boy Dagilimi", ylab = "boy") # aykırı değer yok
boxplot(vr2_dolu$kilo, main = "kilo Dagilimi", ylab = "kilo") # aykırı değer yok
boxplot(vr2_dolu$kilo, main = "kilo Dagilimi", ylab = "kilo") # aykırı değer yok
boxplot(vr2_dolu$kolestrol, main = "kolest Dagilimi", ylab = "kolestrol") # aykırı değer yok
boxplot(vr2_dolu$hdl, main = "hdl Dagilimi", ylab = "hdl")# aykırı değer gözlemlendi.
boxplot(vr2_dolu$ldl, main = "ldl Dagilimi", ylab = "ldl")# aykırı değer yok
boxplot(vr2_dolu$enerji, main = "enerji Dagilimi", ylab = "enerji")# aykırı değer gözlemlendi.
boxplot(vr2_dolu$prot, main = "prot Dagilimi", ylab = "prot")# aykırı değer gözlemlendi.
boxplot(vr2_dolu$ilacsonrasikol, main = "isk Dagilimi", ylab = "isk")# aykırı değer yok


# Aykırı değerleri Z-puanı hesaplayarak tespit etme fonksiyonu tanımlama
z_score_outlier_detection <- function(data, threshold = 3) {
  # Sayısal sütunların indekslerini bulma
  sayisal_sutunlar <- sapply(data, is.numeric)
  
  # Sayısal sütunları kullanarak Z-puanlarını hesapla
  z_scores <- scale(data[, sayisal_sutunlar])
  
  # Her bir değişken için aykırı değerlerin indekslerini bul
  outliers <- apply(abs(z_scores) > threshold, 2, any)
  
  # Aykırı değerlerin indekslerini döndür
  return(outliers)
}

# Aykırı değerleri Z-puanı hesaplayarak tespit etme fonksiyonunu kullanma
outliers2 <- z_score_outlier_detection(vr2_dolu)

# Aykırı değerlerin gösterilmesi
print(outliers2) #yas , hdl, prottrue döndü yani aykırı değer var. 


#####

# Alt ve üst çeyreklikleri hesaplama
Q1_vr2d <- quantile(vr2_dolu$yas, 0.25)
Q3_vr2d <- quantile(vr2_dolu$yas, 0.75)

# Alt ve üst sınırı hesaplama
IQR_vr2d <- Q3_vr2d  - Q1_vr2d
lower_bound_vr2d <- Q1_vr2d - 1.5 * IQR_vr2d
upper_bound_vr2d<- Q3_vr2d+ 1.5 * IQR_vr2d

# Aykırı değerleri belirleme
outliers_vr2d<- vr2_dolu$yas < lower_bound_vr2d| vr2_dolu$yas > upper_bound_vr2d
outliers_vr2d
#"yas" değişkeni için alt ve üst çeyreklikleri hesaplar, sonra alt ve üst sınırları hesaplar ve en 
#sonunda aykırı değerleri belirler. Aykırı değerler vektöründe TRUE olarak gösterilecektir. 
#Aykırı değer yoksa, tüm değerler FALSE olarak dönecektir. diğer değişkenler için de yapılabilir. 

# Alt ve üst çeyreklikleri hesaplama
Q1_vr2d <- quantile(vr2_dolu$enerji, 0.25)
Q3_vr2d <- quantile(vr2_dolu$enerji, 0.75)

# Alt ve üst sınırı hesaplama
IQR_vr2d <- Q3_vr2d  - Q1_vr2d
lower_bound_vr2d <- Q1_vr2d - 1.5 * IQR_vr2d
upper_bound_vr2d<- Q3_vr2d+ 1.5 * IQR_vr2d

# Aykırı değerleri belirleme
outliers_vr2d<- vr2_dolu$enerji < lower_bound_vr2d| vr2_dolu$yas > upper_bound_vr2d
outliers_vr2d

###### enerji değişkeni için aykırı deşğer tespitinde boxplot grafiğinde 
######ve çeyrekliklerle hesaplaninca gözlemlenyor fakat z puanı ile false dönüyor neden anlamadım?###########



#..................

#---
##ADIM:4 DAGILIMLARI KESIF##
# Histogram oluşturma
# değişkenlerin histogramlarını oluşturma
par(mfrow=c(3, 3)) # 3x3 bir düzenleme için

hist(vr2_dolu$yas, main="Yaş", xlab="Yaş", col="lightblue")
hist(vr2_dolu$boy, main="Boy", xlab="Boy (cm)", col="lightgreen")
hist(vr2_dolu$kilo, main="Kilo", xlab="Kilo (kg)", col="lightpink")
hist(vr2_dolu$kolestrol, main="Kolestrol", xlab="Kolestrol", col="lightyellow")
hist(vr2_dolu$hdl, main="HDL", xlab="HDL", col="lightcyan")
hist(vr2_dolu$hdl, main="LDL", xlab="LDL", col="lightcoral")
hist(vr2_dolu$enerji, main="Enerji", xlab="Enerji", col="lightgray")
hist(vr2_dolu$prot, main="Protein", xlab="Protein", col="lightgoldenrod")
hist(vr2_dolu$ilacsonrasikol, main="İlaç Sonrası Kolestrol", xlab="İlaç Sonrası Kolestrol", col="lightseagreen")


# Cinsiyet dağılımını görselleştirme
barplot(table(vr2_dolu$cinsiyet), main="Cinsiyet Dağılımı", xlab="Cinsiyet", ylab="Frekans", col="skyblue")#cinsiyet te de aykırı değer var ama sanırım derse daha görmedik öyle bir durum kategorik keşif adımında olabilir<


#  boxplot oluşturma
par(mfrow=c(1, 2)) # 1x2 bir düzenleme için

boxplot(vr2_dolu$boy, main="Boy", col="lightblue")
boxplot(vr2_dolu$kilo, main="Kilo", col="lightgreen")


# Q-Q plot oluşturma

qqnorm(vr2_dolu$kilo, main = "kilo Q-Q Plot")
qqline(vr2_dolu$kilo,main = "kilo Q-Q Plot") # Normal dağılıma en iyi uyan doğruyu çizme

#noktalar, çizilen doğruya yakın bir şekilde düzgün bir dağılım gösteriyorsa, bu durum "Kilo" 
#değişkeninin normal dağılıma oldukça yakın olduğunu gösterir.

qqnorm(vr2_dolu$yas, main = "yas Q-Q Plot")
qqline(vr2_dolu$yas,main = "yas Q-Q Plot")

#vr2dolu özet istatistikleri
summary(vr2_dolu )  


# Bağımlı ve bağımsız değişkenleri belirleme
b_li_deg<- vr2_dolu$kolestrol # Bağımlı değişken kolestrol olsun
b_siz_deg <- vr2_dolu$kilo # Bağımsız değişken kilo olsun

b_li_deg1<-vr2_dolu$ldl
b_siz_deg1 <- vr2_dolu$hdl

b_li_deg2<-vr2_dolu$yas


# "kilo" ve "kolestrol" değişkenleri arasındaki korelasyon katsayısını hesaplama
korelasyon_vr2.1 <- cor(vr2_dolu$kilo, vr2_dolu$kolestrol)

print(korelasyon_vr2.1) #  "kilo" ve "kolestrol" değişkenleri arasındaki -0.06234963 korelasyon katsayısı -0.06234963 olarak hesaplanmış. Bu değer, neredeyse sıfıra
                          # yakın olduğundan, "kilo" ile "kolestrol" arasında zayıf korelasyon olduğunu gösterir. 
                         # Yani, bu iki değişken arasında lineer bir ilişki yoktur veya bu ilişki çok zayıftır.


# Scatter plot (dağılım grafiği) çizme.... arasında ilişki olmasını beklediğim için inanamadım bir de grafikte görmek istedim:)
plot(vr2_dolu$kilo, vr2_dolu$kolestrol,
     xlab = "Kilo", ylab = "Kolestrol",
     main = "Kilo ve Kolestrol Dağılımı")
#scatter plot üzerindeki veri noktaları rastgele bir şekilde dağılmış yani ilişki yok.

# "ldl" ve "hdl" değişkenleri arasındaki korelasyon katsayısını hesaplama
korelasyon_ldl_hdl <- cor(vr2_dolu$ldl, vr2_dolu$hdl)

print(korelasyon_ldl_hdl)
# 
#korelasyon_x <- cor(vr2_dolu$yas, vr2_dolu$kolestrol)
#print(korelasyon_x) #  0.1526193 pozitif yönlü bir korelasyonu gösterir, ancak oldukça düşüktür. 


# Çoklu doğrusal regresyon modelini oluşturma


#bağımlı değişken olarak Kolestrol'ü ve
#bağımsız değişkenler olarak Yas, Boy, Kilo, HDL, LDL, Enerji ve Prot'ü kullanacam


# Çoklu doğrusal regresyon modeli oluşturma
model1 <- lm(kolestrol ~ yas + boy + kilo + hdl + ldl + enerji + prot, data = vr2_dolu)

# Model özetini görüntüleme
summary(model1)

#Modelin sonuçlarına dayanarak, kolesterol seviyelerini 
#etkileyen önemli faktörlerin ldl ve prot olduğu görülmekte.



# Bağımsız değişkenler arasındaki korelasyonu hesaplama

correlation_matrix1 <- cor(vr2_dolu[, c("yas", "boy", "kilo", "hdl", "ldl", "enerji", "prot")])

# Korelasyon matrisini görüntüleme
print(correlation_matrix1)

# Korelasyon matrisini görselleştirme
library(corrplot)
corrplot(correlation_matrix1, method = "color")

#Bu korelasyonlar, bağımsız değişkenler arasındaki ilişkileri anlamaya yardımcı olur. örn: Enerji ile Prot arasında 0.50893 pozitif yönlü orta derecede bir korelasyon bulunmaktadır.

# Bağımsız değişkenlerin grafiğini çizme
par(mfrow=c(2,4)) # Grafiklerin yan yana yerleştirilmesi için ayar

# Yas vs. Boy
plot(vr2_dolu$yas, vr2_dolu$boy, main = "Yaş vs. Boy", xlab = "Yaş", ylab = "Boy", col = "blue", pch = 16)

# Yas vs. Kilo
plot(vr2_dolu$yas,  vr2_dolu$kilo, main = "Yaş vs. Kilo", xlab = "Yaş", ylab = "Kilo", col = "red", pch = 16)

# Yas vs. HDL
plot(vr2_dolu$yas,  vr2_dolu$hdl, main = "Yaş vs. HDL", xlab = "Yaş", ylab = "HDL", col = "green", pch = 16)

# Yas vs. LDL
plot(vr2_dolu$yas,  vr2_dolu$ldl, main = "Yaş vs. LDL", xlab = "Yaş", ylab = "LDL", col = "purple", pch = 16)

# Yas vs. Enerji
plot(vr2_dolu$yas, vr2_dolu$enerji, main = "Yaş vs. Enerji", xlab = "Yaş", ylab = "Enerji", col = "orange", pch = 16)

# Yas vs. Prot
plot(vr2_dolu$yas,  vr2_dolu$prot, main = "Yaş vs. Prot", xlab = "Yaş", ylab = "Prot", col = "brown", pch = 16)


# Veri setini standartlaştırma
standart_vr2 <- as.data.frame(scale(vr2_dolu[, -c(1,2,4:6)]))  # Kategorik sütunları hariç tutuyoruz

# İlk 6 satırı gösterme
head(standart_vr2)
# özet istatistikler
summary(standart_vr2)

#ADIM5#kategorik keşif
#---

#---


#ADIM6-son değişiklik
  # caTools paketini yükleme
install.packages("caTools")
library(caTools)


# Veriyi test ve eğitim alt kümelerine böleme
split1 <- sample.split(vr2_dolu$yas, SplitRatio = 0.7) # 70% eğitim, 30% test
train_data1 <- subset(vr2_dolu, split1 == TRUE)
test_data1 <- subset(vr2_dolu, split1 == FALSE)



# Eğitim alt kümenin boyutunu kontrol etme
dim(train_data1)

# Test alt kümenin boyutunu kontrol etme
dim(test_data1)

        ####################################################

########  2- VERI URETEREK KESIFSEL VERI ANALIZI UYGULAMASI #######
       
        ####################################################



# Rastgele veri oluşturma için kütüphane
install.packages("MASS")
library(MASS)

#ADIM1: veri Kaynağı
# Veri setini oluşturma
set.seed(123) # Tekrarlanabilirlik için seed belirleme
n <- 100 # Öğrenci sayısı
math_score <- round(rnorm(n, mean = 70, sd = 15)) # Matematik sınavı notları
english_score <- round(rnorm(n, mean = 75, sd = 10)) # İngilizce sınavı notları
science_score <- round(rnorm(n, mean = 65, sd = 12)) # Fen sınavı notları
student_id <- 1:n # Öğrenci kimlik numaraları
gender <- sample(c("Erkek", "Kadın"), n, replace = TRUE) # Rastgele cinsiyet atama

# Eksik değerler oluşturma
missing_index <- sample(1:n, size = round(0.1 * n)) # %10 eksik değer oluşturma
math_score[missing_index] <- NA
english_score[missing_index] <- NA
science_score[missing_index] <- NA

# Oluşturulan veri setini bir veri çerçevesine dönüştürme
student_data <- data.frame(Student_ID = student_id, Gender = gender, Math_Score = math_score, English_Score = english_score, Science_Score = science_score)

# Oluşturulan veri setini gösterme
head(student_data)
# Veri setinin boyutunu kontrol etme
dim(student_data)
# Veri setinin sütun isimlerini ve veri tiplerini kontrol etme
str(student_data)



# student_data veri setindeki sütun isimlerini değiştirme
install.packages("dplyr")
library(dplyr)
veri <- rename(student_data,
               mat_puan = Math_Score,
               ing_puan = English_Score,
               fen_puan = Science_Score,
               ogrenci_no = Student_ID,
               cinsiyet = Gender)

# Değişiklikten sonra veri setinin sütun isimlerini kontrol etme
names(veri)


#ADIM:2 Aktarım ve Gerekli ise Dönüşüm
# "ogrenci_no" ve "cinsiyet" değişkenlerini faktörel değişkenlere dönüştürme
veri$ogrenci_no <- as.factor(veri$ogrenci_no)
veri$cinsiyet <- as.factor(veri$cinsiyet)
# "mat_puan", "ing_puan" ve "fen_puan" değişkenlerini sayısal değişkenlere dönüştürme 
veri$mat_puan <- as.numeric(veri$mat_puan)
veri$ing_puan <- as.numeric(veri$ing_puan)
veri$fen_puan <- as.numeric(veri$fen_puan)

str(veri)
# 
# .......
# Eksik veri tespiti
is_na_mat_puan <- is.na(veri$mat_puan)
is_na_ing_puan <- is.na(veri$ing_puan)
is_na_fen_puan <- is.na(veri$fen_puan)
is_na_cinsiyet <- is.na(veri$cinsiyet)
is_na_ogrenci_no <- is.na(veri$ogrenci_no)
# Eksik değerlerin sayısını ve oranını bulma
missing_count_mat_puan <- sum(is_na_mat_puan)
missing_count_ing_puan <- sum(is_na_ing_puan)
missing_count_fen_puan <- sum(is_na_fen_puan)



#.......

# Matematik puanı için eksik değerleri ortalama ile doldurma
mean_mat_puan <- mean(veri$mat_puan, na.rm = TRUE)
veri$mat_puan <- ifelse(is.na(veri$mat_puan), mean_mat_puan, veri$mat_puan)

# İngilizce puanı için eksik değerleri ortalama ile doldurma
mean_ing_puan <- mean(veri$ing_puan, na.rm = TRUE)
veri$ing_puan <- ifelse(is.na(veri$ing_puan), mean_ing_puan, veri$ing_puan)

# Fen puanı için eksik değerleri ortalama ile doldurma
mean_fen_puan <- mean(veri$fen_puan, na.rm = TRUE)
veri$fen_puan <- ifelse(is.na(veri$fen_puan), mean_fen_puan, veri$fen_puan)


#.......  eksik değerlerini LOCF yöntemiyle doldurma

# Gerekli kütüphanenin yüklenmesi
install.packages("zoo")
library(zoo)

# Eksik değerleri en yakın değerle doldurma
veri_dolu <- na.locf(veri, na.rm = FALSE)

# Eksik değerleri doldurulmuş veri setini kontrol etme
head(veri_dolu)


#.......  
veri_a<-student_data
veri_a <- rename(student_data,
                 mat_puan = Math_Score,
                 ing_puan = English_Score,
                 fen_puan = Science_Score,
                 ogrenci_no = Student_ID,
                 cinsiyet = Gender)
veri_b<-veri_a
veri_c<-veri_b

# Eksik verileri medyan ile doldurma
median_mat_puan <- median(veri_a$mat_puan, na.rm = TRUE)
veri_a$mat_puan<- ifelse(is.na(veri_a$mat_puan), median_mat_puan, veri_a$mat_puan)

median_ing_puan <- median(veri_a$ing_puan, na.rm = TRUE)
veri_a$ing_puan <- ifelse(is.na(veri_a$ing_puan), median_ing_puan, veri_a$ing_puan)

median_fen_puan <- median(veri_a$fen_puan, na.rm = TRUE)
veri_a$fen_puan <- ifelse(is.na(veri_a$fen_puan), median_fen_puan, veri_a$fen_puan)

# Doldurulmuş veri setini kontrol etme
head(veri_a)

#....... 
#veri_b için aykırı değer tespiti

# Kutu grafiği oluşturma

boxplot(veri_b$mat_puan, main = "Matematik Puanı", ylab = "Puan")

par(mfrow = c(1, 3)) # Grafiği yan yana göstermek için par() fonksiyonunu kullanma
boxplot(veri_b$ing_puan, main = "İngilizce Puanı", ylab = "Puan")
boxplot(veri_b$fen_puan, main = "Fen Puanı", ylab = "Puan")
boxplot(veri_b$mat_puan, main = "Matematik Puanı", ylab = "Puan")

# herhangi bir kutunun dışında veya bıyıkların ötesinde yer alan noktalar gözlemlenmiyo yani aykırı değer yok 


# Z-puanı hesaplama ile aykırı değer tespiti
z_scores_veri_b <- scale(veri_b[, c("mat_puan", "ing_puan", "fen_puan")])

# Aykırı değerleri belirleme
outliers_veri_b <- abs(z_scores_veri_b) > 3 # hepsi FALSE döndürdü yani aykırı değer yokk
#..................

# Alt ve üst çeyreklikleri hesaplama veri_a için
Q1_veri_a <- quantile(veri_a$mat_puan, 0.25)
Q3_veri_a <- quantile(veri_a$mat_puan, 0.75)

# Alt ve üst sınırı hesaplama
IQR_veri_a <- Q3_veri_a - Q1_veri_a
lower_bound_veri_a <- Q1_veri_a - 1.5 * IQR_veri_a
upper_bound_veri_a <- Q3_veri_a + 1.5 * IQR_veri_a

# Aykırı değerleri belirleme
outliers_veri_a <- veri_a$mat_puan < lower_bound_veri_a | veri_a$mat_puan > upper_bound_veri_a
outliers_veri_a # hepsi FALSE döndürdü yani aykırı değer yokk


#..................

#---
#dağılımları keşif

# Histogram oluşturma
par(mfrow = c(2, 2))  # 2x2 düzenleme
hist(veri_a$mat_puan, main = "Matematik Puanı", xlab = "Puan", ylab = "Frekans")
hist(veri_a$ing_puan, main = "İngilizce Puanı", xlab = "Puan", ylab = "Frekans")
hist(veri_a$fen_puan, main = "Fen Puanı", xlab = "Puan", ylab = "Frekans")

# Kutu grafiği oluşturma
boxplot(veri_a$mat_puan, main = "Matematik Puanı", ylab = "Puan")
boxplot(veri_a$ing_puan, main = "İngilizce Puanı", ylab = "Puan")
boxplot(veri_a$fen_puan, main = "Fen Puanı", ylab = "Puan")

# Q-Q plot oluşturma
qqnorm(veri_a$mat_puan, main = "Matematik Puanı Q-Q Plot")
qqline(veri_a$mat_puan)
qqnorm(veri_a$ing_puan, main = "İngilizce Puanı Q-Q Plot")
qqline(veri_a$ing_puan)
qqnorm(veri_a$fen_puan, main = "Fen Puanı Q-Q Plot")
qqline(veri_a$fen_puan)

#veri_a  özet istatistikleri
summary(veri_a)    
#..................
# Bağımlı ve bağımsız değişkenleri belirleme
bagimli_degisken <- veri_a$mat_puan
bagimsiz_degisken <- veri_a$ing_puan

# Korelasyon katsayısını hesaplama  , matematik puanı ile İngilizce puanı arasındaki ilişkiyi ifade eder.
correlation <- cor(bagimli_degisken, bagimsiz_degisken)

# Korelasyon katsayısını ekrana yazdırma
print(correlation)     
#..................



# Yeni yaş verisini oluşturma            
yas <- round(rnorm(n, mean = 30, sd = 5))

# veri_a setine yaş sütununu ekleyerek yeni bir veri çerçevesi oluşturma
veri_y <- cbind(veri_a, Yas = yas)

# Çoklu doğrusal regresyon modelini oluşturma
model <- lm(mat_puan + ing_puan + fen_puan ~ Yas, data = veri_y)

# Model özetini alma
summary(model)         # yaş değişkeninin matematik, İngilizce ve fen puanları üzerinde anlamlı bir etkisinin olmadığını gösteriyor.



# Bağımsız değişkenler arasındaki korelasyonu hesaplama
correlation_matrix <- cor(veri_y[, c("mat_puan", "ing_puan", "fen_puan")])

# Korelasyon matrisini görselleştirme
library(corrplot)
corrplot(correlation_matrix, method = "color")
#(mat_puan) ile  (ing_puan) arasında güçlü bir pozitif ilişki var(fen_puan) ile diğer değişkenler arasındaki ilişkiler ise daha zayıf g.

# Bağımsız değişkenlerin grafiğini çizme
par(mfrow=c(1,3)) # Grafiklerin yan yana yerleştirilmesi için ayar
plot(veri_y$mat_puan, veri_y$yas, main = "Matematik Puanı vs. Yaş", xlab = "Matematik Puanı", ylab = "Yaş", col = "blue", pch = 16)
plot(veri_y$ing_puan, veri_y$yas, main = "İngilizce Puanı vs. Yaş", xlab = "İngilizce Puanı", ylab = "Yaş", col = "red", pch = 16)
plot(veri_y$fen_puan, veri_y$yas, main = "Fen Puanı vs. Yaş", xlab = "Fen Puanı", ylab = "Yaş", col = "green", pch = 16)

# Veri setini standartlaştırma
veri_y_std <- veri_y # Kopya oluşturma

# Bağımsız değişkenlerin standartlaştırılması
veri_y_std$mat_puan <- scale(veri_y_std$mat_puan)
veri_y_std$ing_puan <- scale(veri_y_std$ing_puan)
veri_y_std$fen_puan <- scale(veri_y_std$fen_puan)

# Yaşın standartlaştırılması
veri_y_std$Yas <- scale(veri_y_std$Yas)

# Standartlaştırılmış veri setini izleme
head(veri_y_std)
summary(veri_y_std)

#..................

#..................

#son değişiklik
# caTools paketini yükleme
install.packages("caTools")
library(caTools)



# Veri setini eğitim ve test kümelerine bölme oranını belirle
split_ratio <- 0.7 # Örneğin, verinin %70'i eğitim, %30'u test için ayrılacak

# Veri setini eğitim ve test kümelerine bölme
split <- sample.split(veri_y$Yas, SplitRatio = split_ratio)

# Eğitim ve test kümelerini ayırma
veri_y_egitim <- subset(veri_y, split == TRUE)
veri_y_test <- subset(veri_y, split == FALSE)

# Eğitim ve test kümelerinin boyutlarını kontrol etme
nrow(veri_y_egitim) # Eğitim kümesinin satır sayısı
nrow(veri_y_test)   # Test kümesinin satır sayısı

# Verinin boyutlarını kontrol etme
dim(veri_y_egitim )
dim(veri_y_test )


