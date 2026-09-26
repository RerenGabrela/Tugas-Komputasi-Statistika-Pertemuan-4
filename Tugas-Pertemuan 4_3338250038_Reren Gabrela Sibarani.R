# Nama = Reren Gabrela Sibarani
# NIM = 3338250038
# Kelas = 3B

# 1. Rata-rata pelanggan yang datang ke toko adalah 3 orang/jam.
#    Modelkan dengan Poisson dan hitung P(X >= 5).

# Rata-rata pelanggan yang datang per jam
lambda <- 3

# Model Poisson
# X ~ Poisson(lambda = 3)

# Menghitung peluang X >= 5
P <- 1 - ppois(4, lambda)

# Menampilkan hasil peluang
P

# 2. Dari 100 bola, terdapat 20 bola merah.
#    Diambil 10 bola tanpa pengembalian.
#    Modelkan banyak bola merah yang terambil.

# Ukuran populasi
N <- 100

# Jumlah bola merah
K <- 20

# Ukuran sampel
n <- 10

# Model Hypergeometric
# X ~ Hypergeometric(N = 100, K = 20, n = 10)

# Menentukan nilai k yang mungkin
k <- seq(from = max(0, n + K - N), to = min(n, K))

# Menghitung PMF Hypergeometric
pmf <- dhyper(k, m = K, n = N - K, k = n)

# Menampilkan nilai k dan peluangnya
data.frame(k = k, P = pmf)

# 3. Simulasikan 1000 data Binomial dengan n = 15 dan p = 0.4.
#    Bandingkan histogram simulasi dengan PMF teoretis.

# Banyak percobaan
n <- 15

# Peluang sukses
p <- 0.4

# Banyak simulasi
m <- 1000

# Menentukan seed agar hasil simulasi tetap sama
set.seed(2025)

# Simulasi 1000 data Binomial
simulasi <- rbinom(m, size = n, prob = p)

# Menentukan nilai X
x <- 0:n

# Menghitung PMF Binomial teoretis
pmf <- dbinom(x, size = n, prob = p)

# Membuat histogram hasil simulasi
hist(simulasi,
     breaks = seq(-0.5, 15.5, by = 1),
     probability = TRUE,
     main = "Histogram Simulasi Binomial",
     xlab = "k",
     ylab = "Probability")

# Menambahkan PMF teoretis
points(x, pmf, type = "h", lwd = 3)
points(x, pmf, pch = 16)