# Econometria I - 2026-2 - FGV EESP
# O efeito flypaper nos municípios brasileiros (2023): MQO x MQ2E
#
# Nomes: André Solovjovas Leite e Enzo Cascaldi da Paz
#
# Modelo: ln(G) = b0 + b1 ln(FPM) + b2 ln(PIB) + controles + EF de UF + e, tudo per capita.
# Instrumento: FPM teórico, pela regra de faixas do DL 1.881/1981 aplicada ao Censo 2022.
#
# Para rodar, basta ter a pasta "bases" ao lado deste script, com rec_orc, desp_orc e desp_por-func
# (em .csv ou compactados em .zip, como estão no GitHub) e o ibge.csv. Os resultados saem no
# console e as figuras vão para a pasta "figuras".


# 0. Pacotes e pastas ---------------------------------------------------------

# Instala só os pacotes que faltarem, pra o script rodar direto em qualquer computador. O AER traz
# o ivreg e já carrega o lmtest (coeftest, bptest, resettest) e o sandwich (vcovHC). O gmm é só
# para o J de Hansen, e o ggplot2 e o patchwork só para as figuras
pacotes <- c("AER", "lmtest", "sandwich", "gmm", "ggplot2", "patchwork")
faltam <- setdiff(pacotes, rownames(installed.packages()))
if (length(faltam)) install.packages(faltam)
library(AER)
library(lmtest)
library(sandwich)
library(gmm)
library(ggplot2)
library(patchwork)

# Aqui uso um par tryCatch para achar a pasta do script nos três jeitos usuais de rodar
# (Rscript, source() e RStudio). Se a pasta "bases" não estiver no diretório de trabalho,
# mudo para a pasta do script, que é onde ela deveria estar
if (!dir.exists("bases")) {
  arq <- c(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)),
           tryCatch(sys.frame(1)$ofile, error = function(e) NULL),
           tryCatch(rstudioapi::getSourceEditorContext()$path, error = function(e) NULL))
  arq <- arq[nzchar(arq)]
  if (length(arq)) setwd(dirname(arq[1]))
}

# Condição pro "failstate" em que a pasta não existe nem ao lado do script
if (!dir.exists("bases")) stop("Pasta 'bases' não encontrada: coloque-a ao lado deste script.")
dir.create("figuras", showWarnings = FALSE)


# 1. FINBRA ------------------------------------------------------------------

# Defini aqui uma função só para ler os três CSV do FINBRA do mesmo jeito, já que todos vêm com
# três linhas de cabeçalho, separador ";", vírgula decimal e acentuação em Latin-1. O código
# IBGE é lido como texto para não perder zeros nem virar número
ler <- function(arquivo) {

  # Código para lidar com o caso em que só a versão compactada está na pasta (como no GitHub, que
  # não aceita arquivos desse tamanho): extrai o CSV do .zip de mesmo nome na primeira vez
  zipado <- file.path("bases", sub("\\.csv$", ".zip", arquivo))
  if (!file.exists(file.path("bases", arquivo)) && file.exists(zipado)) unzip(zipado, exdir = "bases")

  classes <- c("character", "character", "character", "numeric",
               "character", "character", "character", "numeric")
  dados <- read.csv2(file.path("bases", arquivo), skip = 3, fileEncoding = "latin1", colClasses = classes)
  names(dados) <- c("inst", "cod", "uf", "pop_finbra", "coluna", "conta", "id", "valor")
  dados
}
rec  <- ler("rec_orc.csv")
desp <- ler("desp_orc.csv")
func <- ler("desp_por-func.csv")

# FPM bruto realizado. A conta 1.7.1.1.51 junta a cota mensal e as cotas extras de 1%
linhas <- rec[rec$coluna == "Receitas Brutas Realizadas" & rec$id == "siconfi-cor_RO1.7.1.1.51.0.0", ]
fpm <- aggregate(valor ~ cod, data = linhas, FUN = sum)
names(fpm) <- c("cod", "fpm")

# Despesa total liquidada, já sem as operações intraorçamentárias
linhas <- func[func$coluna == "Despesas Liquidadas" & grepl("^Despesas Exceto Intra", func$conta), ]
total <- aggregate(valor ~ cod, data = linhas, FUN = sum)
names(total) <- c("cod", "total")

# No Anexo I-D as intraorçamentárias (modalidade 91) vêm misturadas, então a ideia é marcar com
# sinal positivo a linha do total de cada categoria e com sinal negativo as linhas de modalidade
# 91 correspondentes. Somando por município, sobra só a despesa sem as intraorçamentárias
linhas <- desp[desp$coluna == "Despesas Liquidadas", ]
intra_corr <- grepl("^siconfi-cor_DI3\\.[1-3]\\.91\\.00\\.00\\.00$", linhas$id)
linhas$corrente <- ifelse(linhas$id == "siconfi-cor_DO3.0.00.00.00.00", linhas$valor, 0) -
                   ifelse(intra_corr, linhas$valor, 0)
linhas$invest <- ifelse(linhas$id == "siconfi-cor_DO4.4.00.00.00.00", linhas$valor, 0) -
                 ifelse(linhas$id == "siconfi-cor_DI4.4.91.00.00.00", linhas$valor, 0)
cat_econ <- aggregate(cbind(corrente, invest) ~ cod, data = linhas, FUN = sum)


# 2. IBGE --------------------------------------------------------------------

# Só baixa os dados do SIDRA se ainda não houver o ibge.csv, pra não depender de internet toda vez
if (!file.exists(file.path("bases", "ibge.csv"))) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) install.packages("jsonlite")

  # Como diz o nome, função que baixa uma tabela da API do SIDRA. A primeira linha que a API
  # devolve é só o cabeçalho, e o "-" aparece no lugar de zero, então trato as duas coisas aqui
  sidra <- function(api) {
    x <- jsonlite::fromJSON(paste0("https://apisidra.ibge.gov.br/values", api))[-1, ]
    x$V[x$V == "-"] <- "0"
    x$V <- as.numeric(x$V)
    x
  }

  # PIB de 2023 vem em R$ mil, por isso multiplico por 1000. População é a do Censo 2022
  x <- sidra("/t/5938/n6/all/v/37/p/2023")
  pib <- data.frame(cod = x$D1C, pib = x$V * 1000)
  x <- sidra("/t/4709/n6/all/v/93/p/2022")
  pop <- data.frame(cod = x$D1C, pop = x$V)

  # Códigos das faixas de idade da tabela 9514: as três primeiras somam 0 a 14 anos e as
  # demais somam 65 anos ou mais. O 100362 é o total, usado para virar proporção
  jov <- c("93070", "93084", "93085")
  ido <- c("93096", "93097", "93098", "49108", "49109", "60040", "60041", "6653")
  x <- sidra(paste0("/t/9514/n6/all/v/93/p/2022/c2/6794/c287/100362,",
                    paste(c(jov, ido), collapse = ","), "/c286/113635"))
  idade <- data.frame(cod = x$D1C[x$D5C == "100362"], tot = x$V[x$D5C == "100362"])
  soma_jov <- aggregate(V ~ D1C, data = x[x$D5C %in% jov, ], FUN = sum)
  soma_ido <- aggregate(V ~ D1C, data = x[x$D5C %in% ido, ], FUN = sum)
  idade$jovens <- soma_jov$V[match(idade$cod, soma_jov$D1C)] / idade$tot
  idade$idosos <- soma_ido$V[match(idade$cod, soma_ido$D1C)] / idade$tot

  # Taxa de urbanização: população urbana (1) sobre a total (6795)
  x <- sidra("/t/9923/n6/all/v/93/p/2022/c1/6795,1")
  urb <- data.frame(cod = x$D1C[x$D4C == "6795"], tot = x$V[x$D4C == "6795"])
  urb$urb <- x$V[x$D4C == "1"][match(urb$cod, x$D1C[x$D4C == "1"])] / urb$tot

  ibge <- merge(pib, pop, by = "cod")
  ibge <- merge(ibge, idade[, c("cod", "jovens", "idosos")], by = "cod")
  ibge <- merge(ibge, urb[, c("cod", "urb")], by = "cod")
  write.csv(ibge, file.path("bases", "ibge.csv"), row.names = FALSE)
}
ibge <- read.csv(file.path("bases", "ibge.csv"), colClasses = c(cod = "character"))


# 3. Base e instrumento (seções 3.3 e 4.2) -----------------------------------

# As capitais recebem o FPM-Capital, que segue outra regra, então ficam de fora
capitais <- c("1100205", "1200401", "1302603", "1400100", "1501402", "1600303", "1721000",
              "2111300", "2211001", "2304400", "2408102", "2507507", "2611606", "2704302",
              "2800308", "2927408", "3106200", "3205309", "3304557", "3550308", "4106902",
              "4205407", "4314902", "5002704", "5103403", "5208707")

d <- merge(unique(rec[, c("cod", "uf", "pop_finbra")]), fpm, by = "cod")
d <- merge(d, total, by = "cod")
d <- merge(d, cat_econ, by = "cod")
d <- merge(d, ibge, by = "cod")
d <- d[!(d$cod %in% capitais), ]

# Coeficiente legal do FPM-Interior: começa em 0,6 (até 10.188 hab.), sobe 0,2 a cada corte e
# para em 4,0 (acima de 156.216 hab.). O findInterval diz em qual faixa cada população cai
cortes <- c(10188, 13584, 16980, 23772, 30564, 37356, 44148, 50940, 61128,
            71316, 81504, 91692, 101880, 115464, 129048, 142632, 156216)
faixa <- function(p) seq(0.6, 4, by = 0.2)[findInterval(p, cortes, left.open = TRUE) + 1]

# coef é a regra estrita aplicada ao Censo 2022 (o instrumento). coef21 é a mesma regra aplicada
# à estimativa de 2021, que foi a base dos coeficientes efetivamente usados em 2023
d$coef   <- faixa(d$pop)
d$coef21 <- faixa(d$pop_finbra)

# A fatia de cada UF no FPM-Interior é fixa por lei e repartida proporcionalmente ao coeficiente,
# então o FPM teórico é o total da UF vezes a participação do município nos coeficientes (o ave
# faz a soma dentro de cada UF). O coef_ef faz a conta inversa (quantos "pontos" de coeficiente
# o FPM recebido vale) e só serve para a Figura 4
d$fpm_teo <- ave(d$fpm, d$uf, FUN = sum) * d$coef / ave(d$coef, d$uf, FUN = sum)
d$coef_ef <- d$fpm / (ave(d$fpm, d$uf, FUN = sum) / ave(d$coef, d$uf, FUN = sum))

# Variáveis per capita e em log
d$g_pc     <- d$total / d$pop
d$fpm_pc   <- d$fpm / d$pop
d$pib_pc   <- d$pib / d$pop
d$ln_g     <- log(d$total / d$pop)
d$ln_gcorr <- log(d$corrente / d$pop)
d$ln_ginv  <- log(d$invest / d$pop)
d$ln_fpm   <- log(d$fpm / d$pop)
d$ln_pib   <- log(d$pib / d$pop)
d$ln_z     <- log(d$fpm_teo / d$pop)
d$ln_pop   <- log(d$pop)

# Limpeza da base. Despesa menor que o próprio FPM é sinal de relatório incompleto, e os
# outliers são os valores a mais de 3 amplitudes interquartis dos quartis (em log)
fora <- function(x) {
  q <- quantile(x, c(.25, .75))
  x < q[1] - 3 * diff(q) | x > q[2] + 3 * diff(q)
}
n0 <- nrow(d)
d <- d[d$total > d$fpm & d$invest > 0 & complete.cases(d[, c("pib", "urb", "jovens", "idosos")]), ]
d <- d[!fora(d$ln_fpm) & !fora(d$ln_g), ]

# Contagem da seção 4.2: municípios no FINBRA, sem FPM discriminado, capitais, excluídos na
# limpeza e amostra final
cat("\n== Amostra (seção 4.2)\n")
n_finbra <- length(unique(rec$cod))
print(c(finbra = n_finbra, sem_fpm = n_finbra - nrow(fpm), capitais = length(capitais),
        limpeza = n0 - nrow(d), final = nrow(d)))


# 4. Análise descritiva (seção 4.3) -------------------------------------------

# Tabela 1: para cada variável, média, desvio-padrão, mínimo, mediana e máximo
desc <- data.frame(g = d$g_pc, gcorr = d$corrente / d$pop, ginv = d$invest / d$pop, fpm = d$fpm_pc,
                   pib = d$pib_pc, pop = d$pop, urb = d$urb, jovens = d$jovens, idosos = d$idosos,
                   coef = d$coef, fpm_desp = d$fpm / d$total)
cat("\n== Tabela 1\n")
print(round(t(sapply(desc, function(x) c(media = mean(x), dp = sd(x), min = min(x),
                                         mediana = median(x), max = max(x)))), 3))

# Números citados no texto: peso mediano do FPM na despesa, parcela na faixa 0,6, razão entre
# FPM e PIB médios (a razão b1/b2 compatível com a equivalência) e correlação com a população
cat("\n== Números do texto (seção 4.3)\n")
print(c(fpm_desp_mediana = median(d$fpm / d$total), faixa_06 = mean(d$coef == 0.6),
        fpm_pib_medias = mean(d$fpm_pc) / mean(d$pib_pc), cor_fpm_pop = cor(d$ln_fpm, d$ln_pop)))

# Dados da Figura 2: médias por porte populacional
d$classe <- cut(d$pop, c(0, 5e3, 1e4, 2e4, 5e4, 1e5, Inf), dig.lab = 7)
d$dep <- d$fpm / d$total
porte <- aggregate(cbind(g_pc, fpm_pc) ~ classe, data = d, FUN = mean)
porte$dep <- aggregate(dep ~ classe, data = d, FUN = median)$dep
porte$n <- as.vector(table(d$classe))
cat("\n== Figura 2: médias por porte\n")
print(porte)

# Inclinações das retas da Figura 3 (regressões simples em log)
cat("\n== Figura 3: inclinações brutas\n")
print(c(fpm = coef(lm(ln_g ~ ln_fpm, data = d))[[2]], pib = coef(lm(ln_g ~ ln_pib, data = d))[[2]]))

# Figura 4: comparo a faixa pelo Censo 2022 com a faixa pela estimativa de 2021 para ver quem
# cairia ou subiria de faixa, e quanto o FPM recebido se afasta do teórico em cada grupo
d$mov <- "mantem"
d$mov[d$coef < d$coef21] <- "cai"
d$mov[d$coef > d$coef21] <- "sobe"
cat("\n== Figura 4: mudança de faixa com o Censo 2022\n")
print(table(d$mov))
print(aggregate(I(ln_fpm - ln_z) ~ mov, data = d, FUN = mean))


# 5. Estimação (Tabela 2) ----------------------------------------------------

# Erros-padrão robustos à heterocedasticidade (HC1), usados em todas as estimações. Defini como
# função porque o summary do ivreg precisa recalcular a matriz também no primeiro estágio
vc <- function(m) vcovHC(m, type = "HC1")

# Defini aqui uma função só para não repetir a fórmula do MQ2E em cada variável dependente.
# O que vem depois da "|" são os instrumentos (ln_z ou as dummies de faixa) mais os controles
controles <- "ln_pib + ln_pop + urb + jovens + idosos + factor(uf)"
mq2e <- function(y, inst = "ln_z") {
  ivreg(as.formula(paste(y, "~ ln_fpm +", controles, "|", inst, "+", controles)), data = d)
}

ols <- lm(ln_g ~ ln_fpm + ln_pib + ln_pop + urb + jovens + idosos + factor(uf), data = d)
fs  <- lm(ln_fpm ~ ln_z + ln_pib + ln_pop + urb + jovens + idosos + factor(uf), data = d)
iv  <- mq2e("ln_g")

# As linhas 1 a 7 são o intercepto, as variáveis de interesse e os controles (sem as UF)
cat("\n== Tabela 2: (1) MQO, (2) 1º estágio, (3) MQ2E\n")
print(coeftest(ols, vcov. = vc)[1:7, ])
print(coeftest(fs,  vcov. = vc)[1:7, ])
print(coeftest(iv,  vcov. = vc)[1:7, ])
print(c(r2_mqo = summary(ols)$r.squared, r2_1e = summary(fs)$r.squared))

# F do 1º estágio (instrumento fraco se < 10) e Wu-Hausman (H0: FPM exógeno)
print(summary(iv, vcov. = vc, diagnostics = TRUE)$diagnostics)


# 6. Testes do efeito flypaper (Tabela 3) --------------------------------------

# A ideia aqui é converter as elasticidades em R$ por R$ nas médias, já que dG/dFPM = b1*G/FPM e
# dG/dPIB = b2*G/PIB. O kf e o ky são essas razões entre médias, e é em reais que a equivalência
# deve ser testada
kf <- mean(d$g_pc) / mean(d$fpm_pc)
ky <- mean(d$g_pc) / mean(d$pib_pc)

# Como diz o nome, função para o teste unilateral de a*b1 - c*b2 > 0. Como a combinação é linear,
# a variância sai direto da matriz de covariâncias: a²V11 + c²V22 - 2acV12. Com a = c = 1 é o
# teste em elasticidades, e com a = kf e c = ky é o teste em reais
unilateral <- function(m, a = 1, c = 1) {
  b <- coef(m)
  V <- vc(m)
  est <- a * b["ln_fpm"] - c * b["ln_pib"]
  ep <- sqrt(a^2 * V["ln_fpm", "ln_fpm"] + c^2 * V["ln_pib", "ln_pib"] - 2 * a * c * V["ln_fpm", "ln_pib"])
  z <- unname(est / ep)
  c(estimativa = unname(est), ep = ep, z = z, p = pnorm(z, lower.tail = FALSE))
}

cat("\n== Tabela 3: testes do efeito flypaper\n")
print(c(kf = kf, ky = ky))

# Propensões marginais a gastar (R$ por R$) e seus erros-padrão
b <- coef(iv)
V <- vc(iv)
print(c(pmg_fpm = kf * b[["ln_fpm"]], ep_fpm = kf * sqrt(V["ln_fpm", "ln_fpm"]),
        pmg_pib = ky * b[["ln_pib"]], ep_pib = ky * sqrt(V["ln_pib", "ln_pib"])))

# Teste em reais, que é o teste exato da equivalência
print(unilateral(iv, a = kf, c = ky))

# Testes em elasticidades, que são o critério mais conservador (sob equivalência b1/b2 = 0,06).
# Com uma única restrição, o Wald de b1 = b2 é o quadrado do z, com distribuição qui-quadrado(1)
el <- unilateral(iv)
print(el)
print(c(wald = el[["z"]]^2, p_wald = pchisq(el[["z"]]^2, df = 1, lower.tail = FALSE)))


# 7. Diagnóstico do modelo (seção 5.3) ----------------------------------------

# Breusch-Pagan para heterocedasticidade e RESET para forma funcional, os dois sobre o MQO
cat("\n== Diagnóstico (seção 5.3)\n")
print(bptest(ols, studentize = TRUE))
print(resettest(ols))


# 8. Especificações alternativas (Tabela 4 e seção 5.5) -----------------------

# Defini aqui uma função só para imprimir o mesmo bloco de resultados em cada especificação:
# coeficientes de interesse, teste unilateral, diagnósticos e número de observações
alternativa <- function(m) {
  print(coeftest(m, vcov. = vc)[2:3, ])
  print(unilateral(m))
  print(summary(m, vcov. = vc, diagnostics = TRUE)$diagnostics)
  cat("N =", nobs(m), "\n")
}

# Colunas (1) e (2): despesa corrente e investimentos, como em Sakurai (2013), com o intervalo
# de confiança de 95% de b1 para comparar as duas categorias
for (y in c("ln_gcorr", "ln_ginv")) {
  cat("\n== Tabela 4,", y, "\n")
  m <- mq2e(y)
  alternativa(m)
  ep <- sqrt(vc(m)["ln_fpm", "ln_fpm"])
  print(coef(m)[["ln_fpm"]] + c(-1, 1) * qnorm(0.975) * ep)
}

# Coluna (3): troco o ln_z pelas 17 dummies de faixa, o que deixa o modelo sobreidentificado.
# O Sargan sai no próprio ivreg, mas supõe homocedasticidade, então o J de Hansen vem do GMM
# eficiente em dois passos com vcov = "MDS", que é a versão robusta
iv_faixas <- mq2e("ln_g", inst = "factor(coef)")
cat("\n== Tabela 4, dummies de faixa\n")
alternativa(iv_faixas)
gmm_faixas <- gmm(formula(ols), as.formula(paste("~ factor(coef) +", controles)),
                  data = d, type = "twoStep", vcov = "MDS")
print(specTest(gmm_faixas))

# Coluna (4): sem os 175 municípios da Reserva do FPM (pop. > 142.632), para os quais o FPM
# teórico é só uma aproximação. Isso tira também todos os que estão no teto do coeficiente
cat("\n== Tabela 4, sem a Reserva do FPM\n")
alternativa(ivreg(formula(iv), data = d[d$pop <= 142632, ]))

# Seção 5.5: sem o 1% de maior PIB per capita, onde o PIB mais se afasta da renda dos moradores
# (hidrelétricas, mineração, royalties)
cat("\n== Seção 5.5, sem o 1% de maior PIB per capita\n")
alternativa(ivreg(formula(iv), data = d[d$pib_pc <= quantile(d$pib_pc, 0.99), ]))

# Seção 5.5: controles mais flexíveis de população, por causa da curvatura que o RESET aponta.
# A lógica desse passo é simples, para cada forma de controlar a população refazemos o RESET,
# o MQ2E, o teste em reais e o J de Hansen. A população em nível vai em 100 mil habitantes
# pra as potências não ficarem gigantes
d$p <- d$pop / 1e5
flex <- c("ln_pop + p + I(p^2) + I(p^3) + I(p^4)",
          "ln_pop + I(ln_pop^2) + I(ln_pop^3)")
for (f in flex) {
  x <- paste("ln_pib +", f, "+ urb + jovens + idosos + factor(uf)")
  cat("\n== Seção 5.5, população:", f, "\n")
  ols_f <- lm(as.formula(paste("ln_g ~ ln_fpm +", x)), data = d)
  print(resettest(ols_f))
  m <- ivreg(as.formula(paste("ln_g ~ ln_fpm +", x, "| ln_z +", x)), data = d)
  alternativa(m)
  print(unilateral(m, a = kf, c = ky))
  print(specTest(gmm(formula(ols_f), as.formula(paste("~ factor(coef) +", x)), data = d,
                     type = "twoStep", vcov = "MDS")))
}


# 9. Figuras -----------------------------------------------------------------

# Cores, formato brasileiro dos números (ponto no milhar, vírgula decimal) e um tema comum,
# pra as quatro figuras ficarem com a mesma cara
AZUL <- "#2a78d6"; LARANJA <- "#eb6834"; CINZA <- "#9a9890"; TINTA <- "#0b0b0b"; TINTA2 <- "#52514e"
br <- function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE, trim = TRUE)
tema <- theme_minimal(base_family = "serif", base_size = 9) +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(color = "#e6e5e0", linewidth = 0.3),
        axis.text = element_text(color = TINTA2), axis.title = element_text(color = TINTA2, size = 8.5),
        legend.position = "bottom", legend.text = element_text(size = 8, color = TINTA2),
        legend.title = element_blank(), plot.margin = margin(4, 6, 2, 4))
salva <- function(p, nome, h) ggsave(file.path("figuras", nome), p, width = 16, height = h,
                                     units = "cm", dpi = 300, bg = "white")

# Figura 1: histogramas em escala log, com a mediana marcada em cada painel
f1 <- rbind(data.frame(v = d$fpm_pc, var = "FPM per capita"),
            data.frame(v = d$g_pc, var = "Despesa total per capita"),
            data.frame(v = d$pib_pc, var = "PIB per capita"))
f1$var <- factor(f1$var, levels = unique(f1$var))
med <- aggregate(v ~ var, data = f1, FUN = median)
names(med) <- c("var", "m")
p1 <- ggplot(f1, aes(v)) + geom_histogram(bins = 40, fill = AZUL, color = "white", linewidth = 0.15) +
  geom_vline(data = med, aes(xintercept = m), linetype = "22", color = TINTA, linewidth = 0.35) +
  geom_text(data = med, aes(x = m, y = Inf, label = paste0("mediana\nR$ ", br(round(m, -1)))), hjust = -0.08,
            vjust = 1.3, family = "serif", size = 2.6, color = TINTA, lineheight = 0.9) +
  scale_x_log10(labels = br) + facet_wrap(~var, scales = "free_x") +
  labs(x = "R$ correntes de 2023 por habitante (escala logarítmica)", y = "Municípios") + tema +
  theme(strip.text = element_text(face = "bold", size = 8.5, color = TINTA))
salva(p1, "fig1_distribuicoes.png", 6)

# Figura 2: barras de despesa e FPM médios por porte, com o peso mediano do FPM em cima
faixas_pop <- c("Até 5 mil", "5 a 10 mil", "10 a 20 mil", "20 a 50 mil", "50 a 100 mil", "Mais de 100 mil")
porte$rot <- factor(faixas_pop, levels = faixas_pop)
f2 <- rbind(data.frame(rot = porte$rot, variable = "Despesa total per capita", value = porte$g_pc),
            data.frame(rot = porte$rot, variable = "FPM per capita", value = porte$fpm_pc))
p2 <- ggplot(f2, aes(rot, value, fill = variable)) +
  geom_col(position = position_dodge(width = 0.78), width = 0.72, color = "white", linewidth = 0.5) +
  geom_text(data = porte, aes(x = rot, y = g_pc, label = paste0("FPM/despesa:\n", round(100 * dep), "%"), fill = NULL),
            vjust = -0.35, size = 2.5, family = "serif", color = TINTA2, lineheight = 0.9) +
  scale_fill_manual(values = c(AZUL, LARANJA)) + scale_y_continuous(labels = br, expand = expansion(mult = c(0, 0.18))) +
  labs(x = "Faixa de população (Censo 2022)", y = "R$ por habitante (média)") + tema +
  theme(panel.grid.major.x = element_blank())
salva(p2, "fig2_porte.png", 6.5)

# Figura 3: defini aqui uma função só para montar os dois painéis do mesmo jeito. Cada painel
# tem a nuvem de pontos, a média por vintil do eixo horizontal e a reta de MQO em log
rel <- function(x, xl) {
  dd <- data.frame(x = x, y = d$g_pc)
  dd$bin <- cut(dd$x, quantile(dd$x, 0:20 / 20), include.lowest = TRUE)
  bm <- aggregate(cbind(x, y) ~ bin, data = dd, FUN = function(v) exp(mean(log(v))))
  sl <- coef(lm(log(y) ~ log(x), dd))[2]
  ggplot(dd, aes(x, y)) + geom_point(color = CINZA, alpha = 0.18, size = 0.35, stroke = 0) +
    geom_smooth(aes(linetype = "Reta de MQO (log-log)"), method = "lm", formula = y ~ x, se = FALSE,
                color = LARANJA, linewidth = 0.6) +
    geom_point(data = bm, aes(shape = "Média por vintil"), color = AZUL, size = 1.6) +
    annotate("text", x = Inf, y = Inf, hjust = 1.05, vjust = 1.5, size = 2.7, family = "serif", color = TINTA,
             label = paste0("inclinação = ", format(round(sl, 2), decimal.mark = ","))) +
    scale_x_log10(labels = br) + scale_y_log10(labels = br) +
    scale_shape_manual(values = 16) + scale_linetype_manual(values = 1) +
    labs(x = xl, y = "Despesa total per capita (R$)") + tema
}
p3 <- (rel(d$fpm_pc, "FPM per capita (R$, escala log)") | rel(d$pib_pc, "PIB per capita (R$, escala log)")) +
  plot_layout(guides = "collect") & theme(legend.position = "bottom")
salva(p3, "fig3_relacoes.png", 7)

# Figura 4: coeficiente efetivo de cada município contra a escada da regra estrita. Ordeno pelo
# grupo pra os pontos de quem mudaria de faixa ficarem por cima dos demais
f4 <- d[d$pop <= 200000, ]
grupos <- c("mantem" = "Mesma faixa", "cai" = "Cairia de faixa com o Censo 2022",
            "sobe" = "Subiria de faixa com o Censo 2022")
f4$grupo <- factor(grupos[f4$mov], levels = grupos)
f4 <- f4[order(f4$grupo), ]
degraus <- data.frame(x0 = c(800, cortes), x1 = c(cortes, 200000), y = seq(0.6, 4, by = 0.2))
p4 <- ggplot(f4, aes(pop, coef_ef)) +
  geom_point(aes(color = grupo), size = 0.45, alpha = 0.75, stroke = 0) +
  geom_segment(data = degraus, aes(x = x0, xend = x1, y = y, yend = y,
                                   linetype = "Coeficiente pela regra estrita (DL 1.881/1981)"),
               inherit.aes = FALSE, color = TINTA, linewidth = 0.55) +
  scale_color_manual(values = c(CINZA, AZUL, LARANJA)) + scale_linetype_manual(values = 1) +
  scale_x_log10(labels = br, breaks = c(1000, 3000, 10000, 30000, 100000)) + coord_cartesian(ylim = c(0, 4.6)) +
  guides(color = guide_legend(override.aes = list(size = 2, alpha = 1), order = 1), linetype = guide_legend(order = 2)) +
  labs(x = "População residente, Censo 2022 (escala log)", y = "Coeficiente efetivo implícito") + tema +
  theme(legend.box = "vertical", legend.spacing.y = unit(0, "pt"), legend.margin = margin(0, 0, 0, 0))
salva(p4, "fig4_regra.png", 7.5)
cat("\nFiguras salvas em", normalizePath("figuras"), "\n")
