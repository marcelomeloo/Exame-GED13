## Aluno: Marcelo Melo de Oliveira 
Link do Repo no Github: https://github.com/marcelomeloo/Exame-GED13
OBS: O seguinte arquivo foi feito em Rmarkdown no editor de texto VSCode.
Por esse motivo, o pdf é ligeiramente diferente das versões Rmarkdown feitas
no RStudio.

# Questão 1
### a)
### - Inicializacao

```r
  library(nortest)
  amostra1990 <- c(281, 359, 247, 470, 432, 194, 306, 210,  
    305, 430, 200, 223, 388, 480, 291, 190, 300, 235, 241, 380)
  amostra2000 <- c(140, 160, 22, 20, 223, 60, 30, 95, 360,  
    70, 218, 300, 217, 58, 235, 280, 200, 175, 85, 65)

  z.amostra1990 <- scale(amostra1990)
  z.amostra2000 <- scale(amostra2000)
```
### - Testes da Amostra de 1990

```r
  ks.test(z.amostra1990, "pnorm", 0, 1)
```

```
## 
## 	Exact one-sample Kolmogorov-Smirnov test
## 
## data:  z.amostra1990
## D = 0.15885, p-value = 0.6373
## alternative hypothesis: two-sided
```

```r
  shapiro.test(amostra1990)
```

```
## 
## 	Shapiro-Wilk normality test
## 
## data:  amostra1990
## W = 0.91948, p-value = 0.09683
```

```r
  ad.test(amostra1990)
```

```
## 
## 	Anderson-Darling normality test
## 
## data:  amostra1990
## A = 0.53266, p-value = 0.1517
```

```r
  lillie.test(amostra1990)
```

```
## 
## 	Lilliefors (Kolmogorov-Smirnov) normality test
## 
## data:  amostra1990
## D = 0.15885, p-value = 0.206
```
Como em todos os testes obteve-se um valor de ```p-value``` maior do que 0.05, 
não foi negada a hipotese nula dos testes de normalidade. Isto é, é pertinente
considerar que a amostra dos modelos de 1990 segue uma distribuição normal.  

### - Testes da Amostra de 2000

```r
  ks.test(z.amostra2000, "pnorm", 0, 1)
```

```
## 
## 	Exact one-sample Kolmogorov-Smirnov test
## 
## data:  z.amostra2000
## D = 0.15928, p-value = 0.634
## alternative hypothesis: two-sided
```

```r
  shapiro.test(amostra2000)
```

```
## 
## 	Shapiro-Wilk normality test
## 
## data:  amostra2000
## W = 0.93683, p-value = 0.2087
```

```r
  ad.test(amostra2000)
```

```
## 
## 	Anderson-Darling normality test
## 
## data:  amostra2000
## A = 0.45894, p-value = 0.2353
```

```r
  lillie.test(amostra2000)
```

```
## 
## 	Lilliefors (Kolmogorov-Smirnov) normality test
## 
## data:  amostra2000
## D = 0.15928, p-value = 0.2027
```
Como em todos os testes obteve-se um valor de ```p-value``` maior do que 0.05, 
não foi negada a hipotese nula dos testes de normalidade. Isto é, é pertinente
considerar que a amostra dos modelos de 2000 também segue uma distribuição normal.

### b)

```r
  var1 <- var(amostra1990)
  var2 <- var(amostra2000)
  graus <- length(amostra1990) - 1
  var1
```

```
## [1] 8964.2
```

```r
  var2
```

```
## [1] 10189.82
```
Como ```var1``` menor que ```var2``` e assumindo um intervalo de confiança de 95%, temos:

```r
  alpha <- 1 - 0.95
  estatistica.teste <- graus*var2/var1
  qui.critico <- qchisq(alpha/2, df=graus)

  if (estatistica.teste < qui.critico) {
    print("Hipotese nula rejeitada")
  } else {
    print("Hipotese nula nao rejeitada")
  }
```

```
## [1] "Hipotese nula nao rejeitada"
```
Portanto, não rejeitamos a hipótese de que as variâncias
são iguais.

### c)

```r
  var.test(amostra1990, amostra2000, alternative="two.sided")
```

```
## 
## 	F test to compare two variances
## 
## data:  amostra1990 and amostra2000
## F = 0.87972, num df = 19, denom df = 19, p-value = 0.7829
## alternative hypothesis: true ratio of variances is not equal to 1
## 95 percent confidence interval:
##  0.3482044 2.2225726
## sample estimates:
## ratio of variances 
##          0.8797213
```
Considerando um nível de significância de 0.05, temos que o ```p-value``` é
maior que 0.05. Dessa forma, não rejeitamos a hipótese de que as variâncias
são iguais.

# Questão 2
### a)
Pelo método dos momentos temos:  
$$ \mu_1 = \ \int_{0}^{\infty} \frac{x*x^6*e^\frac{-x}{\beta}}{\Gamma(7)\beta^7} \,dy \ = m_1 = E[X] \quad\therefore\quad  
E[X] = 7\beta $$
$$ \mu = E[X] = 7\beta \quad\therefore\quad \beta = \mu/7 $$

### b)
Pelo método da máxima verossimilhança, temos:
$$ L(\beta) = f(x_1)*f(x_2)\cdots*f(x_n) = x^6_1*x^6_2\cdots*x^6_n * \frac{e^\frac{-n*<X>}{\beta}}{\beta^{7n}} $$
$$ l(\beta) = ln(L(\beta)) = ln(x^6_1)+ln(x^6_2)+\cdots+ln(x^6_n)-\frac{n*<X>}{\beta}-7nln(\beta) $$
$$ \frac{dl(\beta)}{d\beta} = 0 \quad\therefore\quad \frac{n*<X>}{\beta} = 7n \quad\therefore\quad \beta_{MV} = \frac{\mu}{7} $$


# Questão 4
### a)

```r
  library(nortest)
  ipca <- c(6.41, 10.67, 6.29, 2.95, 3.75, 4.31, 4.52)
```

```r
  shapiro.test(ipca)
```

```
## 
## 	Shapiro-Wilk normality test
## 
## data:  ipca
## W = 0.86686, p-value = 0.1742
```

```r
  lillie.test(ipca)
```

```
## 
## 	Lilliefors (Kolmogorov-Smirnov) normality test
## 
## data:  ipca
## D = 0.22788, p-value = 0.3315
```
Como em todos os testes obteve-se um valor de ```p-value``` maior do que 0.05, 
não foi negada a hipotese nula dos testes de normalidade. Isto é, é pertinente
considerar que a amostra do IPCA segue uma distribuição normal.

### b)

```r
  mi <- mean(ipca)
  s <- sqrt(var(ipca))
  graus <- length(ipca) - 1
  ic <- 99
  alpha <- 1 - ic/100

  z <- (-1)*qt(alpha/2, df=graus, lower.tail = TRUE)
  limite.sup <- mi + z*s/sqrt(graus+1)
  limite.inf <- mi - z*s/sqrt(graus+1)
  limite.sup
```

```
## [1] 9.17973
```

```r
  limite.inf
```

```
## [1] 1.934555
```
Assim, obteve-se: 1.9345554 < Média < 9.1797303

### c)

```r
  distancia <- 3
  t <- distancia*sqrt(graus+1)/(2*s)
  alpha <- 2*pt((-1)*t, df=graus)
  ic <- 100*(1 - alpha)
  ic
```

```
## [1] 82.43458
```
Portanto, o nível de confiança foi de: 82.4345791

### d)

```r
  ic <- 0.9
  alpha <- 1 - ic
  qui.quadrado.inf <- qchisq(1-alpha/2, graus)
  qui.quadrado.sup <- qchisq(alpha/2, graus)
  limite.inf <- sqrt(graus/qui.quadrado.inf)*s
  limite.sup <- sqrt(graus/qui.quadrado.sup)*s
```
Assim, obteve-se: 1.7845575 < Desvio padrão < 4.9517769

