library(e1071)
library(ROCR)

# W celu zapewnienia powtarzalnosci wyniku zwiazanego z losowo (funkcja sample)
# dobieranymi obserwacjami do zbioru uczacego nalezy ustalic stale ziarno dla
# funkcji losowej
#seed.set(100)

# Wczytanie danych, nazwanie zmiennych
data <- read.csv("tic-tac-toe.data", header = F, strip.white = T)
colnames(data) <- c(1:9, "win_x")
n = nrow(data)

# Pomieszanie danych, oryginalna kolejnosc posiada widoczna tendencje obserwacji
data <- data[sample(n),]

# Podzielenie danych na zbior uczacy i testowy w proporcji 9:1
train <- data[1:round(0.9*n),]
test <- data[(round(0.9*n)+1):n,]

# Wyternowanie modelu maszyny wektorow nosnych
svm_fit <- svm(win_x ~ ., data=train)

# Predykcja obserwacji testowych
pred <- predict(svm_fit, test)

# Tablica pomylek
conf <- table(test$win_x, pred)

# Sprawdzenie trafnosci modelu
predROCR <- prediction(as.numeric(pred), as.numeric(test$win_x))
ROC <- performance(predROCR, "tpr", "fpr")

# Obliczenie obszaru pod wykresem ROC
auc <- performance(predROCR, "auc")

# Wykres
plot(ROC, main=sprintf("Receiver Operating Characteristic AUC=%f",as.numeric(auc@y.values)), col="red")
abline(a=0, b= 1, lty=3)
print(svm_fit)
print(conf)

# KOMENTARZ
# Obszar pod wykresem ROC modelu to 0.97 zatem powyzsza implementacja modelu SVM
# daje bardzo duza trafnosc. Tabela pomylek pokazuje, ze 30 obserwacji zostalo 
# slusznie zdyskryminowanych oraz 64 slusznie sklasyfikowane pozytywnie. Dwie obserwacje
# zostaly nieslusznie sklasyfikwane jako prawdziwe. Alogorytm SVM wybral jadro radialne
# oraz parametr gamma=0.05263158. Liczba wektorow wspierajacych to: 465.
# Do treningu modelu wykorzystano 862 obserwacje wybrane losowo. Test modelu zostal
# wykonany na 96 obserwacjach dobranych losowo i nie zawierajacych sie w modelu 
# uczacym.


