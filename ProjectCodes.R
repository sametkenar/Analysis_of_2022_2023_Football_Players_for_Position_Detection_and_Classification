Sys.setlocale("LC_CTYPE", "en_US.UTF-8")
options(scipen = 999)
library(naniar)
library(missMethods)
library(mice)
library(readxl)
library(skimr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(GGally)
library(corrplot)
library(forcats)
library(viridis)
library(scales)
library(RColorBrewer)
library(ggExtra)
library(ggpubr)
library(tidyr)
library(ggcorrplot)
library(caret)
library(glmnet)
library(MASS)
library(broom)
library(factoextra)
library(car)
library(psych)
library(writexl)
library(janitor)
library(FSA)
library(PMCMRplus)
library(MVN)
library(vegan)
library(VIM)
library(car); library(lmtest); library(sandwich)
library(nnet)
library(visdat)
library(Amelia)
library(MVN)
set.seed(412)
# READING DATA
data <- read_excel(
  path = "maindata.xlsx",
  col_names=TRUE
)


# FEATURE ENGINEERING

# 1) PLAYING TIME AND PARTICIPATION INTO GAME
playing_vars <- c("mp","starts","min","x90s")
playing_data <- data[,playing_vars]
# PCA ASSUMPTIONS
# Needs to be scaled
playing_scaled <- scale(playing_data)
# Değişkenler arası korelasyon
cor_matrix <- cor(playing_scaled)
print(cor_matrix)
corrplot(cor_matrix, method="circle")
# KMO test
KMO(playing_scaled)
# Barlett's test
cortest.bartlett(cor(playing_scaled), n=nrow(playing_scaled))

pca_playing <- prcomp(playing_scaled,center=FALSE,scale. = FALSE)
summary(pca_playing)
pca_playing$rotation # değişkenlerin bileşene katkıları

fviz_eig(pca_playing, addlabels = TRUE, ylim = c(0, 100))

# PCA sonuçlarına göre PC1 tek başına 92.04% varyans açıklıyor. PC2 eklenince varyans açıklama oranı
# 96.8% oluyor. PC1 kullabnmak yeterli.
# PCA loadingse baktığımızda tüm değişkenler PC1'e benzer vev yüksek negatif ağırlıklarla katkı yapmış.
# Bu da, PC1'in temelde oyuncunun oyunda geçirdiği süre ve katılım seviyesini özetlediğini gösteriyor.
# Tüm yükler negatif bu şu demek:, PC1 skoru yüksek oyuncular daha az süre alan oyuncular.
# PC1 skoru düşük olanlarlar daha çok oynayan daha fazla başlayan dakikası fazla olan oyuncular.
# Ters anlamlandırmak istediğimiz için (yüksek değer daha çok süre alsın) bileşeni - ile çarpıcaz. 
# Veriye ekleyelim.
data$PlayingContribution <- -pca_playing$x[,1]
# 2) SHOTS AND GOALS
shots_vars <- c("goals", "shots","so_t", "so_t_percent","g_sh", "g_so_t","sho_dist", "sho_fk", "sho_pk", "p_katt")
shots_data <- data[,shots_vars]
# PCA Assumptions
# Needs to be scaled
shots_scaled <- scale(shots_data)
# Değişkenler arası korelasyon
cor_matrix <- cor(shots_scaled)
print(cor_matrix)
corrplot(cor_matrix, method="circle")
# KMO test
KMO(shots_scaled)
# Barlett's test
cortest.bartlett(cor(shots_scaled), n=nrow(shots_scaled))

pca_shots <- prcomp(shots_scaled, center=FALSE, scale. = FALSE)
summary(pca_shots)

fviz_eig(pca_shots, addlabels = TRUE, ylim = c(0, 100))

pca_shots$rotation # Loadings: hangi değişken hangi bileşene nasıl etki ediyor
# Buradaki PC1 şutun kalitesi ve gol verimliliği ile ilgilidir. Shot Efficiency Index
# Gol üretme verimliliği, şut isabeti ve genel şut kalitesi. 
# Yüksek PC1 Skoru, oyuncu çok fazla şut çekiyor. Bu şutalr genelde isabetli. Gol/şut oranı yüksek.
# Büyük ihtimalle "bitirici" pozisyonda
# PC2 Penalty & Set Piece Impact oyuncunun penaltı katkısını temsil ediyor. Bu skor yüksek olan oyuncular
# genellikle penaltı atanlardan. Penalty Impact Score
# Oyuncu sık penaltı kullanıyor. Gol katkısının büyük kısmı penaltılardan geliyor olabilir.
# Düşükse gol ve şutları açık oyundan geliyor olabilir.
# PC3 Distance vs Close-Range Shooting Tradeoff, Bu bileşen iki farklı şut tarzını karşılaştırıyor.
# Yüksek gol verimliliği (yakın mesafe etkili şutlar) vs uzaktan duran toplardan gelen şutlar. Close-Range Finisher vs Long-Range Shooter,
# Oyuncu genellikle yakın mesafe veya net pozisyonlardan şut çekiyor.
# Gol/SoT oranı yüksek olabilir. Düşükse uzaktan şut atma eğilimi
# PC4 Free Kick Specialist /Distance Taker, bu bileşen, uzaktan ve serbest vuruş şutu çeken oyuncuları
# tanımlıyor. Free Kick Profile
# Yüksekse Oyuncu sık sık serbest vuruş kullanıyor. genellikle uzak mesafeden şut çekiyor.
data$ShotEfficiencyIndex <- pca_shots$x[,1]
data$PenaltyImpactScore <- pca_shots$x[,2]
data$ShotTypePreference <- pca_shots$x[,3]
data$FreeKickProfile <- pca_shots$x[,4]

# 3) BASIC PASSESS AND DISTANCE BASED PASSESS
basepass_vars <- c("pas_tot_cmp","pas_tot_att","pas_tot_cmp_percent", "pas_tot_dist",
                   "pas_tot_prg_dist")
shotpass_vars <- c("pas_sho_cmp", "pas_sho_att", "pas_sho_cmp_percent")
medpass_vars <- c("pas_med_cmp","pas_med_att", "pas_med_cmp_percent")
longpass_vars <- c("pas_lon_cmp", "pas_lon_att", "pas_lon_cmp_percent")

passing_vars <- c("pas_tot_cmp","pas_tot_att","pas_tot_cmp_percent", "pas_tot_dist",
                 "pas_tot_prg_dist","pas_sho_cmp", "pas_sho_att", "pas_sho_cmp_percent",
                 "pas_med_cmp","pas_med_att", "pas_med_cmp_percent",
                 "pas_lon_cmp", "pas_lon_att", "pas_lon_cmp_percent")
passing_data <- data[,passing_vars]

# PCA Assumptions
# Needs to be scaled
passing_scaled <- scale(passing_data)
# Değişkenler arası korelasyon
cor_matrix <- cor(passing_scaled)
print(cor_matrix)
corrplot(cor_matrix, method="circle")
# KMO test
KMO(passing_scaled)
# Barlett's test
cortest.bartlett(cor(passing_scaled), n=nrow(passing_scaled))

pca_passing <- prcomp(passing_scaled, center=FALSE, scale. = FALSE)
summary(pca_passing)

fviz_eig(pca_passing, addlabels = TRUE, ylim = c(0, 100))

pca_passing$rotation # değişkenlerin bileşen katkısı

# PC1: Total Passing Volume Index
# Buna Yüksek katkı yapan değişkenler pas türlerinin tamamlanma ve deneme sayılarıyla ilgili
# PasTotCmp, PasTotAtt, PasTotDist, PasMedCmp, PasShoCmp, PasLonCmp vb.
# bu bileşen oyuncunun genel pas hacmini ve toplam pas alışlkanlığını temsil ediyor.
# Yüksek skor: pas yapan oyunu yönlendiren oyuncu. 
# Düşük skor: Daha az pas yapan, defansif ya da oyun dışında kalan

# PC2: Short Passing Emphasis
# Buna yüksek katkı yapan değişkenler, Pas ShoCmp, PasShoAtt pozitif.
# PasLonAtt, PasLonCmp, PasTotPrgDist da negatif katkılı,
# Bu bileşen oyuncunun kısa paslara mı, yoksa uzun paslara mı yöneldiğini yansıtıyor.
# Short vs Long Pass Preference diyebilirim.
# Yüksek skor: kısa pas ağırlıklı, topa sahip olma oyuncusu
# Düşük skor: uzun pasla oyun kuran oyuncu.

# PC3: Pass Accuracy Index 
# Buna Yüksek katkı yapanlar: PasTotCmp%, PasShoCmp%, PasMedCmp%, PasLonCmp%
# Oyuncunun pas isabet oranlarını temsil eden bir bileşen. Sayıdan çok kalite öne çıkıyor.
# Passing Accuracy Index diyebilirim.
# Yüksek Skor: İsabetli Pasör
# Düşük Skor: Pas kalitesi düşük, belki baskı altında top kaybeden.

# PC4 Mixed Pass Profile: 
# Bunda etkili değişkenler: PasMedCmp, PasMedAtt negatif, PasShoCmp% ve PasLonAtt pozitif
# Orta mesafe yerine kısa veya uzun paps tercih edenler.
# Pass Profile Variation Diyebilirim.

data$TotalPassingVolume <- pca_passing$x[,1]
data$ShortvsLongPassProfile <- pca_passing$x[,2]
data$PassingAccuracy <- pca_passing$x[,3]
data$PassProfileVariation <- pca_passing$x[,4]


# 4) LUCK AND CREATIVITY 
creativity_vars <- c("assists", "pas_ass", "sca_pass_live", "sca_pass_dead",
                     "sca_drib", "sca_sh", "sca_fld", "sca_def", "gca_pass_live",
                     "gca_pass_dead", "gca_drib", "gca_sh", "gca_fld", "gca_def")
# SCA (Shot Creating Actions) <-  şutla sonuçlanan aksiyonlar
# GCA (Goal Creating Actions) <- golle sonuçlanan aksiyonlar
# pas, dribbling, foul, savunma gibi yollarla bu katkılar sağlanmış mı?

creativity_data <- data[,creativity_vars]
# PCA Assumptions
# Needs to be scaled
creativity_scaled <- scale(creativity_data)
# Değişkenler arası korelasyon
cor_matrix <- cor(creativity_scaled)
print(cor_matrix)
corrplot(cor_matrix, method="circle")
# KMO test
KMO(creativity_scaled)
# Barlett's test
cortest.bartlett(cor(creativity_scaled), n=nrow(creativity_scaled))

pca_creativity <- prcomp(creativity_scaled, center=FALSE, scale. = FALSE)
summary(pca_creativity)
fviz_eig(pca_creativity, addlabels = TRUE, ylim = c(0, 100))
pca_creativity$rotation

# PC1: General Creativity & Involvement Index
# En güçlü katkı yapan değişkenler: PasAss, ScaPassLive, GcaPassLive,Assists
# Hepsi pozitif yüksek yüklere sahip. Bu bileşen oyuncunun şut ve gol öncesi pas katkısı ve 
# genel yaratıcı etkinliğini temsil ediyor. 
# Yüksek skor: hücumun pas odaklı yaratıcısı oyuncular. Düşük skor: Kreatif etkisi düşük, yaratıcı oyun dışında kalan
# oyuncular. 
# Creative Involvement Index ya da Offensive Playmaker Score diyebilirim.

# PC2: Shot-Based Chain Contributor
# En güçlü katkı yapan değişkenler ScaSh, GcaSh, ScaFld, GcaDrib. Bunlar negatif katkı yapıyorlar.
# Bu bileşen, oyuncunun şut aksiyonları zincirine olan katksını gösteriyor.
# Yüksek skor: hücum zincirlerinde yok (şutla bağlantısı zayıf). Düşük Skor: şutla doğrudan bağlantılı aksiyonlarda aktif
# Shot Chain Impact ya da Indirect Offensive Involvement diyebilirim. - ile çarptım

# PC3: Open Play vs Set Piece Creator
# Katkı yapan değişkenler: GcaPassDead, ScaPassDead (negatif) ve Assists, GcaPasslive, ScaPassLive (pozitif)
# Bu bileşen oyuncunun açık oyunda mı yoksa duran topa mı yaratıcı olduğunu gösteriyor.
# Yüksek Skor: Açık oyunda etkili pasör. Düşük Skor: Set Piece(Korner, serbest vuruş) üzerinden katkı sağlayan oyuncu.
# OpenPlayvsSetPieceCreativity diyebilirim.

# PC4: Dribbling & Drawn Fouls
# Katkı yapan değişkenler: GcaFld, ScaFld, GcaDrib bunlar negatif. GcaSh pozitif.
# Oyuncunun dribbling ve foul kazandırma yoluyla pozisyon üretip üretmediğini gösteriyor.
# Düşük Skor: Foul ve Dribbling ile yaratıcı pozisyon yaratan oyuncular
# Yüksek Skor: Bunlara az katılan.
# Dribble & Foul Creation diyebilirim. - ile çarptım

# PC5: Individual Playmaker Index
# Katkı yapan değişkenler: ScaDrib, GcaDrib pozitif. GcaDef, GcaFld negatif.
# Bu bileşen oyuncunun bireysel yaratıcılığı, özellikle dribbling ile katkı sağladığını gösteriyor.
# Individual Creativity/1v1 Impact diyebilirim

# PC6: Defensive Creation (Negative Contributor)
# Bu bileşene katkı yapan değişkenler ScaDef, GcaDef (negatif). ScaFld, GcaFld pozitif.
# Bu bileşen, savunmadan başlayan hücum katkısı ile ilgilidir. 
# Yüksek PC6 Skoru savunma katkısı düşük, foul alarak pozisyon kazandırma durumu.
# Düşük Skor, oyuncu savunmadan başlatılan hücumlarda etkili, takımın ileri çıkışına savunmadan katkı veriyor.
# CounterAttack Initiation Score diyebilirim. - ile çarptım

data$OffensivePlaymakerScore <- pca_creativity$x[,1]
data$ChainImpact <- -pca_creativity$x[,2]
data$OpenPlayvsPieceCreativity <- pca_creativity$x[,3]
data$DribbleandFoulCreation <- -pca_creativity$x[,4]
data$IndividualCreativity <- pca_creativity$x[,5]
data$CounterAttackInitiationScore <- -pca_creativity$x[,6]

# 5) DRIBLING & TAKE ON
dribble_vars <- c("to_att", "to_suc", "to_suc_percent", "to_tkl", "to_tkl_percent")
# ToAtt: denenen başarılı dribbling sayısı, ToSuc: Başarılı dribbling, ToSuc%:başarı yüzdesi
# ToTkl:rakip tarafından durdurulma sayısı, ToTkl%: ne sıklıkla rakip tarafından durdurulmuş.

dribble_data <- data[,dribble_vars]

# PCA Assumptions
# Needs to be scaled
dribble_scaled <- scale(dribble_data)
# Değişkenler arası korelasyon
cor_matrix <- cor(dribble_scaled)
print(cor_matrix)
corrplot(cor_matrix, method="circle")
# KMO test
KMO(dribble_scaled)
# Barlett's test
cortest.bartlett(cor(dribble_scaled), n=nrow(dribble_scaled))

pca_dribble <- prcomp(dribble_scaled, center=FALSE, scale. = FALSE)
summary(pca_dribble)
fviz_eig(pca_dribble, addlabels = TRUE, ylim = c(0, 100))
pca_dribble$rotation

# PC1: Dribbling Activity Index
# Katkıya sahip olan değişkenler: ToAtt, ToTkl, ToSuc, ToTkl% hepsi negatif
# Bu bileşen oyuncunun ne kadar çok dribbling denediği ve ne sıklıkla durdurulduğu ile ilgili
# Başarı yüzdesinden çok hacim ve toplam aksiyon ön planda.
# Yüksek skor: Çok Dribbling deneyen ve sık durdurulan
# Düşük Skor: Daha Az Dribbling yapan veya pasif
# Dribble Activity Index. - ile çarpabilirim.

# PC2: Dribble Success Efficiency
# Katkıya sahip olan değişkenler: ToSuc%, ToTkl%,ToSuc
# Bu bileşen dribbling başarı yüzdesi odaklı.
# ToSuc% ne kadar yüksekse PC2 skoru o kadar düşük.
# Yüksek Skor düşük başarı oranı. daha sık durdurulan. 
# Düşük skor daha verimli daha isabetli dribbler
# Dribbling success score diyebilirim. - ile çarpabilirim
data$DribbleActivityIndex <- -pca_dribble$x[,1]
data$DribbleSuccessScore <- -pca_dribble$x[,2]
# 6) BALL CARRYING
carrying_vars <- c("carries", "car_tot_dist", "car_prg_dist", "car_prog", "car3rd", 
                   "cpa", "car_mis", "car_dis")
# Carries: taşıma sayısı, cartotdist: taşıma sırasında toplam mesafe, carprgdist:ileriye doğru taşıma mesafesi
# carprog: ileriye doğru yapılan taşıma sayısı, car3rd: final üçüncü bölgeye taşıma, cpa: ceza sahasına taşıma
# carmis: taşıma sırasında top kayıp, cardis: taşıma sırasında top kaptırma
carrying_data <- data[,carrying_vars]
# PCA Assumptions
# Needs to be scaled
carrying_scaled <- scale(carrying_data)
# Değişkenler arası korelasyon
cor_matrix <- cor(carrying_scaled)
print(cor_matrix)
corrplot(cor_matrix, method="circle")
# KMO test
KMO(carrying_scaled)
# Barlett's test
cortest.bartlett(cor(carrying_scaled), n=nrow(carrying_scaled))

pca_carrying <- prcomp(carrying_scaled, center=FALSE, scale. = FALSE)
summary(pca_carrying)
fviz_eig(pca_carrying, addlabels = TRUE, ylim = c(0, 100))

pca_carrying$rotation

# PC1: Ball Progression Volume
# Katkı yapan değişkenler: Cartotdist, carprgdist, carries, carprog, car3rd
# Bu bileşen oyuncunun topla ne kadar ilerlediğini, ne kadar taşıma yaptığını temsil eder.
# Toplam ve ileri mesafeli taşıma odakta.
# Yüksek Skor. Topla oyunu ileri taşıyan sürükleyici oyuncu
# Düşük skor: taşıma katkısı az
# Ball ProgressionIndex diyebilirim

# PC2: Risk&FinalThirdMovement
# Katkı yapan değişkenler: Carmis, cardis, cpa, pozitif, carries, cartotdist negatif.
# Bu bileşen oyuncunnu riskli taşıma aksiyonları ile ilgilidir.
# Ceza sahasına taşıyor ama aynı zamanda top kayıpları da yüksek.
# Yüksek Skor: Riskli taşıma yapan, ceza sahasına girmeye çalışan ama topda kaybeden
# Düşük Skor: daha kontrollü, kayıp riski düşük taşıyan.
# Risk Carrying Profile

# PC3: Carry Success Efficiency
# Katkı yapan değişkenler: cardis, carmis negatif, carprog, cpa pozitif.
# Oyuncu daha az kayıpla daha etkili taşıyorsa bu bileşende yüksek skor alır.
# CarryEfficiencyScore diyebiliriz.

data$BallProgressionIndex <- pca_carrying$x[,1]
data$RiskCarryingProfile <- pca_carrying$x[,2]
data$CarryEfficiencyScore <- pca_carrying$x[,3]
# 7) CONTACT AND TOUCH WITH BALL
touch_vars <- c("touches", "tou_def_pen", "tou_def3rd", "tou_mid3rd", "tou_att3rd",
                "tou_att_pen", "tou_live")
touch_data <- data[,touch_vars]
  
# PCA Assumptions
# Needs to be scaled
touch_scaled <- scale(touch_data)
# Değişkenler arası korelasyon
cor_matrix <- cor(touch_scaled)
print(cor_matrix)
corrplot(cor_matrix, method="circle")
# KMO test
KMO(touch_scaled)
# Barlett's test
cortest.bartlett(cor(touch_scaled), n=nrow(touch_scaled))

pca_touch <- prcomp(touch_scaled, center=FALSE, scale. = FALSE)
summary(pca_touch)

fviz_eig(pca_touch, addlabels = TRUE, ylim = c(0, 100))
pca_touch$rotation

# PC1: Total Touch & Central Involvement
# Katkı Sağlayan değişkenler: Touches, toulive, toumid3rd, toudef3rd
# Bu bileşen, oyuncunun genel temas sayısı ve merkez bölgelerdeki top hakimiyetini temsil ediyor.
# Topla oynayan, aktif oyuncular burada yüksek skor alır.
# OverallTouchActivity diyebilirim.
# Yüksek Skor: Topa daha fazla temas, oyun içinde daha aktif.

# PC2: Field Zone Touch Profile (Att vs Def)
# Katkı sağlayan değişkenler:TouDefPen, TouDef3rd negative. TouAtt3rd, TouAttPen positive.
# Bu bileşen oyuncunun dokunuşlarının sahadaki bölgesini ayırıyor.
# Negatif skor, savunma bölgesi. Pozitif Skor hücum bölgesi
# Touch Location Index.
# Düşük Skor: Defansif Oyuncu. Yüksek Skor: Hücumcu

# PC3:Final Third Specialist
# Katkı sağlayan değişkenler: TouAttPen, TouAtt3rd pozitif. TouMid3rd, TouDefPen negatif.
# Oyuncunun topa dokunma profilinde ne kadar ileri uçta pozisyon aldığını gösteriyor.
# Yüksek Skor: Ceza Sahasında ve İleri Bölgede Aktif
# FinalThirdPresence

data$OverallTouchActivity <- pca_touch$x[,1]
data$TouchLocationIndex <- pca_touch$x[,1]
data$FinalThirdPresence <- pca_touch$x[,1]

# 8) DEFENSE ACTIVITIES
defense_vars <- c("tkl", "tkl_won", "tkl_def3rd", "tkl_mid3rd", "tkl_att3rd", "tkl_dri",
                  "tkl_dri_att", "tkl_dri_percent", "tkl_dri_past", "blocks", "blk_sh",
                  "blk_pass", "int", "tkl_int", "clr", "err", "recov")

defense_data <- data[,defense_vars]
# PCA Assumptions
# Needs to be scaled
defense_scaled <- scale(defense_data)
# Değişkenler arası korelasyon
cor_matrix <- cor(defense_scaled)
print(cor_matrix)
corrplot(cor_matrix, method="circle")
# KMO test
KMO(defense_scaled)
# Barlett's test
cortest.bartlett(cor(defense_scaled), n=nrow(defense_scaled))

pca_defense <- prcomp(defense_scaled, center = FALSE, scale. = FALSE)
summary(pca_defense)
fviz_eig(pca_defense, addlabels = TRUE, ylim = c(0, 100))
pca_defense$rotation

# PC1: Overall Defensive Intensity
# Yüksek Katkı yapan değişkenler: Tkl, TklWon, TklDri, TklDriAtt, Tkl+Int, TklDef3rd, TklMid3rd
# Bu bileşen savunmada aktif olan sürekli top kapmaya çalışan müdaheleci oyuncuları temsil ediyor.
# Genel savunma katkısının hacmini gösteriyor.
# Defensive Activity Index
# Yüksek Skor: Savunma aksiyonu yoğun

# PC2: Defensive Zonal Profile
# Katkı yapan değişkenler: BlkSh, Clr, Int, TklDri%, Err pozitif, TklAtt3rd, TklDriPast
# Bu bileşen oyuncunun savunmayı nerede yaptığına ve müdahalenin kalitesine odaklanıyor.
# Yüksek Skor: daha derinde oynayan, blok yapan, top çıkaran
# Düşük Skor: Hücumda savunma yapan dribblingde geçilen oyuncular
# Defensive Zone & Recovery Profile

# PC3: Blocking/ Interception Specialist
# Katkı yapan değişkenler: Blocks, BlkPass pozitif, TklDri%, TklDri negatif.
# Oyuncunun pas/şut engelleme odaklı savunma profiline işaret eder.
# Yüksek Skor savunmayı blokla yapan oyuncular
# Defensive Blocking Profile

# PC4: Tackled While Defending
# Katkı yapan değişkenler: TklDriPast, TklDriAtt pozitif. TklMid3rd, Tkl+Int negatif.
# Oyuncunun savunmada ne kadar başarılı/başarısız olduğunu ölçüyor.
# Yüksek Skor: Dribblingde geçilen birebirde zayıf
# 1v1 Defensive Vulnerability
# - ile çarpabilirim.

# PC5: High Pressing & Attacking Third Defensive Profile
# Yüksek Katkı Yapan değişkenler: TklAtt3rd, Int, Recov, TklDriPast
# Bu bileşen, oyuncunun ileri bölgede savunma yapıp top kazandığı profili temsil ediyor.
# Hücum Presi, topu rakip sahada kapma gibi katkılar burada öne çıkıyor.
# Yüksek Skor: Agresif pressing yapan, Önde top kazanan oyuncu
# Pressing DefenderIndex

# PC6: Error Prone or Risky Defender
# Katkı Yapan Değişkenler: Err, Recov
# Bu bileşen büyük ölçüde hatalar üzerinden tanımlanmış.
# Err değişkeni yönü ters olduğu için skor düştükçe hata oranı yükseliyor.
# Yüksek Skor: Daha az hata yapan, güvenli oyuncu.
# Düşük Skor: hata yapan, riskli savunmacı
# DefensiveReliabilityIndex
data$DefensiveActivityIndex <- pca_defense$x[,1]
data$DefensiveZoneRecoveryProfile <- pca_defense$x[,2]
data$DefensiveBlockingProfile <- pca_defense$x[,3]
data$onetooneDefensiveVulnerability <- -pca_defense$x[,4]
data$PressingDefenderIndex <- pca_defense$x[,5]
data$DefensiveReliabilityIndex <- pca_defense$x[,6]

# 9) DISCIPLINE AND FAUL ACTIVITIES
discipline_vars <- c("fls","fld","crd_y","crd_r","x2crd_y","off","p_kcon","p_kwon","og")
# Fls: yapılan fauller, Fld:yapılan faullerden kazanılan faul, crdy:sarı kart, crdR:direkt kırmızı
# 2crdy:2.sarıdan kırmızı, off: ofsayt, PKCon: verilen penaltı, PKwon: kazanılan penaltı, og: kendi kalesine gol
discipline_data <- data[,discipline_vars]

# PCA Assumptions
# Needs to be scaled
discipline_scaled <- scale(discipline_data)
# Değişkenler arası korelasyon
cor_matrix <- cor(discipline_scaled)
print(cor_matrix)
corrplot(cor_matrix, method="circle")
# KMO test
KMO(discipline_scaled)
# Barlett's test
cortest.bartlett(cor(discipline_scaled), n=nrow(discipline_scaled))

pca_discipline <- prcomp(discipline_scaled, center=FALSE, scale. = FALSE)
summary(pca_discipline)
fviz_eig(pca_discipline, addlabels = TRUE, ylim = c(0, 100))
pca_discipline$rotation

# PC1: Aggressive / Undisciplined Profile
# Yüksek katkı yapan değişkenler: CrdY, CrdR, 2CrdY, Fls
# Bu bileşen, oyuncunun yaptığı fauller ve kartlar üzerinden tanımlanmış.
# Genel olarak agresif, kart görmeye yatkın profili temsil ediyor.
# Yüksek skor, faul yapan kart gören oyuncu.
# Disciplinary Agression Index

# PC2: Victimized / Fouled Profile
# Yüksek katkı yapan değişkenler: Fld, Off, PkWon, bunlar negatif etki yapıyor.
# Bu bileşen oyuncunun faul yapılan taraf olduğunu ve ofsayta düşme / penaltı kazanma gibi
# aksiyonlara karıştığını temsil ediyor.
# Düşük Skor daha çok foul yapılan ofsayta düşen penaltı kazanan oyuncu.
# Fouled & Offside Tendency
# - ile çarpabilirim.

# PC3: Penalty Conceder
# Yüksek katkı yapan değişkenler: PkCon, OG pozitif, CrdR, 2CrdY negatif.
# Bu bileşen, penaltı yaptıran veya olumsuz savunma aksiyonu içeren oyuncuları temsil ediyor.
# Yüksek skor: penaltı yaptıran, og atan oyuncu
# Penalty Liability Index
# - ile çarpabilirim.

# PC4: Attacking Offenses vs Penalty Gains
# Yüksek katkı yapan değişkenler: Off pozitif, Pkwon, PkCon negatif.
# Bu bileşen hücumda ihlal yapan (ofsayt) ama penaltı kazanmayan oyuncuları tanımlar.
# Yüksek skoru olan oyuncu daha akıllı hücumcu.
# Offside vs Penalty Outcome
# - ile çarpabilirim.

# PC5: Own Goal Indicator
# Yüksek katkı yapan değişkenler: OG
# Bu bileşen açıkça kendi kalesine gol atan oyuncular üzerinden tanımlanmış.
# Yüksek Skor daha güvenli oyuncu.
# Own Goal Propensity
# - ile çarpabilirim.

# PC6: Discipline Inverse / Foul Drawn
# Yüksek katkı yapan değişkenler: fld, pozitif, pkwon, fls negatif
# Bu bileşen oyuncunun faul kazanma becerisi ile faul yapma eğiliminin ters yönlü etkisini yansıtır.
# Yüksek skor faul alan ama yapmayan oyuncu. (Pozitif disiplin)
# Fair Play Profile
data$DisciplinaryAgressionIndex <- pca_discipline$x[,1]
data$FouledandOffsideTendency <- -pca_discipline$x[,2]
data$PenaltyLiabilityIndex <- -pca_discipline$x[,3]
data$OffsidevsPenaltyOutcome <- -pca_discipline$x[,4]
data$OwnGoalPropensity <- -pca_discipline$x[,5]
data$FairPlayProfile <- pca_discipline$x[,6]

# 10) AIR BALL ACTIVITIES
airball_vars <- c("aer_won","aer_lost", "aer_won_percent")
airball_data <- data[,airball_vars]
# PCA Assumptions
# Needs to be scaled
airball_scaled <- scale(airball_data)
# Değişkenler arası korelasyon
cor_matrix <- cor(airball_scaled)
print(cor_matrix)
corrplot(cor_matrix, method="circle")
# KMO test
KMO(airball_scaled)
# Barlett's test
cortest.bartlett(cor(airball_scaled), n=nrow(airball_scaled))

pca_airball <- prcomp(airball_scaled, center=FALSE, scale. = FALSE)
summary(pca_airball)
fviz_eig(pca_airball, addlabels = TRUE, ylim = c(0, 100))
pca_airball$rotation

# PC1: Aerial Involvement Index
# Yüksek katkı sağlayan değişkenler: Aerwon, Aerlost, AerWon%
# Bu bileşen oyuncunun hava topu mücadelelerine ne kadar sık katıldığını gösteriyor.
# Oran değil, toplam sayı üzerinden tanımlı
# Yüksek Skor: çok sayıda hava topuna giren oyuncular.
# Düşük Skor: havaya fazla çıkmayanlar.
# Aerial Duel Activity
# - ile çarpılabilir.

# PC2: Aerial Effectiveness Score
# Katkıda bulunan değişkenler: AerLost, AerWon%
# Bu bileşen başarı oranını temsil ediyor.
# Yüksek skor çok kaybeden, düşük oran başarılı hava topçusu
# Aerial Success Index

data$AerialDuelActivity <- pca_airball$x[,1]
data$AerialSuccessIndex <- pca_airball$x[,2]

main_vars <- c("rk","player", "nation", "pos", "squad", "comp", "age", "born", 
               "PlayingContribution", "ShotEfficiencyIndex", "PenaltyImpactScore",
               "ShotTypePreference", "FreeKickProfile", "TotalPassingVolume", "ShortvsLongPassProfile",
               "PassingAccuracy", "PassProfileVariation", "OffensivePlaymakerScore", "ChainImpact", "OpenPlayvsPieceCreativity",
               "DribbleandFoulCreation", "IndividualCreativity", "CounterAttackInitiationScore", "DribbleActivityIndex", "DribbleSuccessScore",
               "BallProgressionIndex", "RiskCarryingProfile", "CarryEfficiencyScore", "OverallTouchActivity", "TouchLocationIndex",
               "FinalThirdPresence", "DefensiveActivityIndex", "DefensiveZoneRecoveryProfile", "DefensiveBlockingProfile",
               "onetooneDefensiveVulnerability", "PressingDefenderIndex", "DefensiveReliabilityIndex", "DisciplinaryAgressionIndex",
               "FouledandOffsideTendency", "PenaltyLiabilityIndex", "OffsidevsPenaltyOutcome", "OwnGoalPropensity", "FairPlayProfile","AerialDuelActivity",
               "AerialSuccessIndex")
x <- data[,main_vars]
write_xlsx(x,"reduceddata.xlsx")


# MISSINGNESS MECHANISM OLUŞTURALIM
data <- read_xlsx("reduceddata.xlsx")
data$rk <- NULL
data <- data %>%
  mutate(
    pos4 = case_when(
      pos %in% c("DF",   "DFFW", "DFMF")  ~ "DF",
      pos %in% c("MF",   "MFDF", "MFFW")  ~ "MF",
      pos %in% c("FW",   "FWDF", "FWMF")  ~ "FW",
      pos ==   "GK"                     ~ "GK",
      TRUE                               ~ NA_character_
    ),
    pos4 = factor(pos4, levels = c("DF","MF","FW","GK"))
  )
data <- data %>% 
  relocate(pos4, .before=comp)
# Eşleme sonucunu görmek için:
table(data$pos, data$pos4)
# Veride NA varmı kontrol edelim.
na_locs <- which(is.na(data), arr.ind = TRUE)
na_locs
# Veride 1 tane NA var. Missing mechanismi daha düzgün oluşturmak.
colname <- colnames(data)[ na_locs[,"col"] ]
rownum  <- na_locs[,"row"]
sprintf("NA olan hücre: satır %d, sütun %d (%s)", rownum, na_locs[,"col"], colname)

# Bu NA'i geçici olarak dolduralım.
for(i in seq_len(nrow(na_locs))) {
  row_i <- na_locs[i, "row"]
  col_i <- na_locs[i, "col"]
  data[row_i, col_i] <- "Doldurulcak"
}

# For simplicity, we will take a sample of 5000 this time.
samp = sample(1:nrow(data), replace = FALSE)
data = data[samp,]

cat_vars <- c("player", "nation", "pos", "squad", "comp",
              "born")
data[cat_vars] <- lapply(data[cat_vars], as.factor)

# Load the required library.
library(missMethods)

# Generating missing values in for numeric variables
num_vars <- names(data)[ sapply(data, is.numeric) ]

data_num     <- data[ , num_vars, drop = FALSE ]
# To generate 10% missing completely at random data for each column
data_num_mcar <- delete_MCAR(ds = data_num, p = 0.1)
data_mcar <- data
data_mcar[ , num_vars ] <- data_num_mcar

colSums(is.na(data_mcar)) # Every variable has 10% missing at random. 
# The missing value rows are selected separately so unless there is a coincidence, there shouldn't be a completely empty row.
data_mcar$pos <- NULL
write_xlsx(data_mcar,"data_mcar.xlsx")

# SUMMARY STATISTICS
summary(data_mcar)

# Test
mcar_test(data_mcar)

# 1) Genel eksiklik haritası
vis_miss(data_mcar) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x = element_text(
      angle   = 45,    # dikey
      vjust   = 1,
      hjust   = 0,
      size    = 8      # daha küçük font
    ),
    axis.text.y = element_text(size = 7),
    plot.margin = margin(t = 10, r = 5, b = 5, l = 5)
  )
# MISSING VALUE IMPUTATION
# 1) Değişkenleri ayırt et
vars     <- names(data_mcar)
num_vars <- vars[ sapply(data_mcar, is.numeric) ]
fac_vars <- vars[ sapply(data_mcar, is.factor) ]

# 2) Yalnızca numerikleri imputelamak için method vektörü
meth <- rep("", length(vars))
names(meth) <- vars
meth[ num_vars ] <- "pmm"       # sayısallara pmm
# faktörler zaten "" (skip) kalacak

# 3) predictorMatrix: faktörleri hem predictor hem target'tan çıkar
ini  <- mice(data_mcar, m = 1, maxit = 0, printFlag = FALSE)
pred <- ini$predictorMatrix
pred[ fac_vars, ] <- 0           # faktörler hedef olmasın
pred[ , fac_vars] <- 0           # faktörler predictor olarak da kullanılmasın

# 4) Imputation’u çalıştır (yalnızca numerikler pmm’lenir)
imp <- mice(
  data            = data_mcar,
  method          = meth,
  predictorMatrix = pred,
  m               = 5,
  maxit           = 10,
  printFlag       = FALSE
)

# 5) Tamamlanmış veriyi al ve kontrol et
completed <- complete(imp, 1)
colSums(is.na(completed))

imputed_data <- completed
write_xlsx(imputed_data, "imputed_data.xlsx")

# EXPLORATORY DATA ANALYSIS
# 1) Does the “Playing Contribution” score differ significantly by player position?
# EDA: Boxplot.
# Confirmatory: One-way Anova veya Kruskal-Wallis testi
# 1) Playing Time and Participation into Game

plot1 <- ggplot(data_mcar, aes(x = pos4, y = PlayingContribution, fill = pos4)) +
  geom_boxplot() +
  labs(title = "Box Plot of Playing Contribution Score by Position",
       x = "Position", y = "Playing Contribution Score") +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(
    plot.title      = element_text(size = 18, face = "bold", hjust = 0.5),  # başlığı 18 pt ve kalın, ortalı
    axis.title      = element_text(size = 14),                             # eksen başlıkları 14 pt
    axis.text       = element_text(size = 12),                             # eksen üzeri yazılar 12 pt
    legend.position = "none"
  )
plot1

by(imputed_data$PlayingContribution, imputed_data$pos4, shapiro.test)


leveneTest(PlayingContribution ~ pos4, data = imputed_data)

kruskal.test(PlayingContribution ~ pos4, data = imputed_data)
library(PMCMRplus)

# POST HOC

pairwise.wilcox.test(x = imputed_data$PlayingContribution,
                     g = imputed_data$pos4,
                     p.adjust.method = "holm")
# 2) Is there a difference in the average Shot Efficiency Index and Passing Accuracy between players in different leagues?
# Facet-Histogramlar,  pairwisecorrelation matrix
# MANOVA, çoklu t testi
# 2) Shots and Goals ve 3) Basic Passess

# Shot Efficiency Index
plot2 <- ggplot(data_mcar, aes(x = ShotEfficiencyIndex, fill = comp)) +
  geom_histogram(alpha = 0.7, bins = 30) +
  facet_wrap(~comp) +
  labs(
    title = "Shot Efficiency Index Distributions by Leagues",
    x = "Shot Efficiency Index",
    y = "Count"
  ) +
  theme_minimal(base_size = 14) +  # Tüm metinler için taban boyutu
  theme(
    plot.title    = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title    = element_text(size = 14),
    axis.text     = element_text(size = 12),
    strip.text    = element_text(size = 14, face = "bold"),  # facet başlıkları
    legend.position = "none"
  )
plot2
# Passing Accuracy
plot3 <- ggplot(data_mcar, aes(x = PassingAccuracy, fill = comp)) +
  geom_histogram(alpha = 0.7, bins = 30) +
  facet_wrap(~comp) +
  labs(
    title = "Passing Accuracy Distributions by League",
    x = "Passing Accuracy",
    y = "Count"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title    = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title    = element_text(size = 14),
    axis.text     = element_text(size = 12),
    strip.text    = element_text(size = 14, face = "bold"),
    legend.position = "none"
  )
plot3

premier_data <- imputed_data[imputed_data$comp == "Premier League", c("ShotEfficiencyIndex", "PassingAccuracy")]
mvn(premier_data,mvn_test = "hz")


# Kruskal Wallis
kruskal.test(ShotEfficiencyIndex ~ comp, data = imputed_data)

kruskal.test(PassingAccuracy ~ comp, data = imputed_data)

pairwise.wilcox.test(imputed_data$PassingAccuracy, imputed_data$comp, p.adjust.method = "bonferroni")

# 3) Do individual skill indicex – such as Dribble Success Score and Carry Efficiency Score – decrease as age increases?
# ScatterPlot, yaş gruplarına göre çizgi grafik
# Regresyon analizi
# 5) Dribbling & Take on ve 6) Ball Carrying

shapiro.test(imputed_data$DribbleSuccessScore)
shapiro.test(imputed_data$CarryEfficiencyScore)
shapiro.test(imputed_data$age)

# Yaş grupları oluşturalım.
data_mcar <- data_mcar %>%
  mutate(age_group = case_when(
    is.na(age) ~ "Missing",
    age >= 15 & age <= 20 ~ "15-20",
    age >= 21 & age <= 25 ~ "21-25",
    age >= 26 & age <= 30 ~ "26-30",
    age >= 31 & age <= 35 ~ "31-35",
    age >= 36 & age <= 40 ~ "36-40",
    age > 40 ~ "40+"
  ))


# Veriyi long formata çevir
data_long <- data_mcar %>%
  dplyr::select(age, DribbleSuccessScore, CarryEfficiencyScore) %>%
  pivot_longer(
    cols = c(DribbleSuccessScore, CarryEfficiencyScore),
    names_to  = "metric",
    values_to = "score"
  )

# Tek grafikte iki metriği karşılaştır

ggplot(data_long, aes(x = age, y = score, color = metric)) +
  geom_jitter(width = 0.6, height = 0.2, alpha = 0.6, size = 2) +
  labs(
    title    = "Individual Skill Scores vs Age",
    subtitle = "Dribble Success and Carry Efficiency",
    x        = "Age",
    y        = "Score",
    color    = "Metric"
  ) +
  theme_minimal(base_size = 16) +   # Taban font boyutu 16
  theme(
    plot.title    = element_text(size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 16, hjust = 0.5),
    axis.title    = element_text(size = 16, face = "bold"),
    axis.text     = element_text(size = 14),
    legend.position  = "right",      # Legend sağda
    legend.title     = element_text(size = 15, face = "bold"),
    legend.text      = element_text(size = 14),
    legend.key.size  = unit(1.2, "lines"),  # legend kutucuğu boyutu
    legend.background = element_rect(fill = "transparent")  # istersen şeffaf yap
  )

# 1) Age vs Dribble Success Score
plot_dribble <- ggplot(data_mcar, aes(x = age, y = DribbleSuccessScore)) +
  geom_jitter(width = 0.6, height = 0.2, alpha = 0.6, size = 2.5, color = "steelblue") +
  labs(
    title = "Scatterplot of Age and Dribble Success Score",
    x     = "Age",
    y     = "Dribble Success Score"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title      = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title      = element_text(size = 16, face = "bold"),
    axis.text       = element_text(size = 14)
  )

# 2) Age vs Carry Efficiency Score
plot_carry <- ggplot(data_mcar, aes(x = age, y = CarryEfficiencyScore)) +
  geom_jitter(width = 0.6, height = 0.2, alpha = 0.6, size = 2.5, color = "forestgreen") +
  labs(
    title = "Scatter Plot of Age and Carry Efficiency Score",
    x     = "Age",
    y     = "Carry Efficiency Score"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title      = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title      = element_text(size = 16, face = "bold"),
    axis.text       = element_text(size = 14)
  )

# Grafikleri yan yana ya da alt alta görmek için:
library(gridExtra)
grid.arrange(plot_dribble, plot_carry, ncol = 2)

shapiro.test(imputed_data$DribbleSuccessScore)
shapiro.test(imputed_data$CarryEfficiencyScore)
shapiro.test(imputed_data$age)


# Dribble Success Score ve Age arası
cor.test(imputed_data$DribbleSuccessScore, imputed_data$age, method = "spearman")

# Carry Efficiency Score ve Age arası
cor.test(imputed_data$CarryEfficiencyScore, imputed_data$age, method = "spearman")


# 4) How effective are players offensively, as measured by Shot Efficiency Index, 
# Offensive Playmaker Score and Ball Progression Index? 
# Boxplot veya violin plot
# MANOVA, ANOVA, post-hoc

# 1) Ham veriyi long formata çevir
eda_raw <- data_mcar %>%
  dplyr::select(pos4, ShotEfficiencyIndex, OffensivePlaymakerScore, BallProgressionIndex) %>%
  pivot_longer(cols = -pos4, names_to = "metric", values_to = "value")

# 2) Metrik başına kırpılmış (truncate) bir "value_tr" sütunu oluştur
eda_trunc <- eda_raw %>%
  mutate(
    value_tr = case_when(
      metric == "BallProgressionIndex"    ~ pmin(pmax(value, 0), 3),   # örn. 0–12 arası
      metric == "OffensivePlaymakerScore" ~ pmin(pmax(value, -3), 3),  # örn. -5–7 arası
      metric == "ShotEfficiencyIndex"     ~ pmin(pmax(value, -3),  5)  # örn. -2–8 arası
    )
  )

# 3) Ridgeline + free_x kullanarak ayrı aralıklarla çiz
ggplot(eda_trunc, aes(
  x = value_tr, 
  y = factor(pos4, levels = c("GK","DF","MF","FW")), 
  fill = pos4
)) +
  geom_density_ridges(alpha = 0.8, scale = 1) +
  facet_wrap(~ metric, scales = "free_x", nrow = 1) +
  scale_fill_brewer(palette = "Set2", name = "Position") +
  labs(
    title = "Distributions by Positions ",
    x     = "Value",
    y     = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text      = element_text(size = 14, face = "bold"),
    plot.title      = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.text.y     = element_text(face = "bold"),
    legend.position = "bottom",
    panel.spacing   = unit(0.5, "lines")
  )
shapiro.test(imputed_data$OffensivePlaymakerScore)
shapiro.test(imputed_data$BallProgressionIndex)
shapiro.test(imputed_data$ShotEfficiencyIndex)


kruskal.test(ShotEfficiencyIndex ~ pos4, data = imputed_data)
kruskal.test(BallProgressionIndex ~ pos4, data = imputed_data)
kruskal.test(OffensivePlaymakerScore ~ pos4, data = imputed_data)


pairwise.wilcox.test(imputed_data$ShotEfficiencyIndex, imputed_data$pos4, p.adjust.method = "bonferroni")
pairwise.wilcox.test(imputed_data$BallProgressionIndex, imputed_data$pos4, p.adjust.method = "bonferroni")
pairwise.wilcox.test(imputed_data$OffensivePlaymakerScore, imputed_data$pos4, p.adjust.method = "bonferroni")

# 5) Oyuncuların pozisyonlarına göre performans metrikleri üzerinden sınıflandırılması.
# multinomial regresyon

eda_vars_5 <- data_mcar %>%
  dplyr::select(IndividualCreativity,
                DribbleActivityIndex,
                DribbleSuccessScore,
                OffensivePlaymakerScore,
                CarryEfficiencyScore,
                FairPlayProfile,
                BallProgressionIndex,
                TouchLocationIndex,
                ShotTypePreference,
                OpenPlayvsPieceCreativity,
                ChainImpact)

# Korelasyon matrisi (NA'leri otomatik dışlar)
corr_matrix <- cor(eda_vars_5, use = "pairwise.complete.obs")

# Görselleştir
corrplot::corrplot(corr_matrix, method = "color", type = "upper",
                   tl.col = "black", tl.cex = 0.8, number.cex = 0.7,
                   addCoef.col = "black")

# Tam model: tüm predictor’lar
model_df <- imputed_data %>%
  dplyr::select(
    pos4,
    ShotEfficiencyIndex,
    TotalPassingVolume,
    OffensivePlaymakerScore,
    DefensiveActivityIndex,
    DribbleSuccessScore,
    BallProgressionIndex,
    PassingAccuracy,
    AerialSuccessIndex,
    PlayingContribution,
    ShotTypePreference,
    PenaltyImpactScore,
    FreeKickProfile,
    ShortvsLongPassProfile,
    PassProfileVariation,
    ChainImpact,
    OpenPlayvsPieceCreativity,
    DribbleandFoulCreation,
    IndividualCreativity,
    CounterAttackInitiationScore,
    RiskCarryingProfile,
    CarryEfficiencyScore,
    OverallTouchActivity,
    TouchLocationIndex,
    DisciplinaryAgressionIndex,
    FouledandOffsideTendency,
    AerialDuelActivity
  ) %>%
  mutate(pos4 = factor(pos4))

train_idx <- createDataPartition(model_df$pos4, p = 0.7, list = FALSE)
train <- model_df[train_idx, ]
test  <- model_df[-train_idx, ]

# 5) Özellik ölçekleme (center & scale)
preproc <- preProcess(train[-1], method = c("center","scale"))
train_scaled <- predict(preproc, train[-1])
train_scaled$pos4 <- train$pos4
test_scaled  <- predict(preproc, test[-1])
test_scaled$pos4  <- test$pos4

# 6) Multinomial lojistik regresyon modeli
#    - nnet paketinden multinom()
multi_mod <- multinom(pos4 ~ ., data = train_scaled)

# 7) Model özetine bakış
summary(multi_mod)

# 8) Test seti üzerinde tahmin ve performans
pred <- predict(multi_mod, newdata = test_scaled)
conf_mat <- confusionMatrix(pred, test_scaled$pos4)
print(conf_mat)

# 9) Önemli değişkenler
#    - İlk katsayıların mutlak değerine bakarak hangi metriklerin daha belirleyici olduğunu görebilirsiniz
coef_df <- as.data.frame(coef(multi_mod))
round(coef_df, 3)

# 6) How does the relationship between the Disciplinary Agression Index and 
# the Fair Play Profile vary across player positions, and what role does 
# touch location play?
# Age gruplarına göre boxplot 
# Scatter plot olabilir

ggplot(data_mcar, aes(
  x = FairPlayProfile,
  y = DisciplinaryAgressionIndex,
  size = TouchLocationIndex,
  color = pos
)) +
  geom_point(alpha = 0.5) +
  scale_size_continuous(range = c(1, 10)) +
  labs(
    title = "Pozisyona Göre Fair Play ve Agresyon İlişkisi (Balon: Touch Location)",
    x = "Fair Play Profile",
    y = "Disciplinary Agression Index",
    size = "Touch Location Index"
  ) +
  theme_minimal()


plot_data <- data_mcar %>%
  mutate(
    pos4 = factor(pos4, levels = c("DF","MF","FW","GK")),
  )

# 2) Create the plot
ggplot(data_mcar, aes(
  x     = FairPlayProfile,
  y     = DisciplinaryAgressionIndex,
  color = pos4,
  size  = TouchLocationIndex
)) +
  geom_jitter(
    width  = 3,    # daha fazla yatay jitter
    height = 1,    # dikey jitter
    alpha  = 0.7
  ) +
  scale_size_continuous(range = c(1, 8), name = "Touch Location\nIndex") +
  scale_color_brewer(palette = "Set2", name = "Position") +
  coord_cartesian(xlim = c(-5, 5), ylim = c(-3,5)) +  # zoom bölgesi
  labs(
    title    = "Fair Play vs. Disciplinary Aggression",
    subtitle = "Bubble size = Touch Location Index",
    x        = "Fair Play Profile",
    y        = "Disciplinary Aggression Index"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title      = element_text(size = 20, face = "bold", hjust = 0.5),
    plot.subtitle   = element_text(size = 16,        hjust = 0.5),
    axis.title      = element_text(size = 16, face = "bold"),
    axis.text       = element_text(size = 14),
    legend.title    = element_text(size = 14, face = "bold"),
    legend.text     = element_text(size = 12),
    legend.position = "right"
  )

robust_model <- rlm(FairPlayProfile ~ DisciplinaryAggressionIndex * Position * TouchLocation, data = data)
summary(robust_model)






