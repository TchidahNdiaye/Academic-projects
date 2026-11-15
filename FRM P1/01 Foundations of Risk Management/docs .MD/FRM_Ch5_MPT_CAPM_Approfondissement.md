---
title: "FRM Part I · Foundations — Approfondissement du Chapitre 5"
subtitle: "Modern Portfolio Theory \\& CAPM — démonstrations, exemples chiffrés, études de cas \\& 100 QCM"
---

> **Objet.** Approfondissement **quantitatif** du chapitre 5 de la synthèse Foundations. Démonstrations rigoureuses, exemples chiffrés pas-à-pas, études de cas et **plus de 100 QCM** corrigés en français. Les **formules sont en notation GARP** ; les **équations sont composées en LaTeX**. Concepts expliqués en français. *Pour les nombres et les formules, on utilise le point décimal (convention quantitative internationale).*

**Plan.** (1) Fondations statistiques. (2) Diversification — démonstration. (3) Frontière efficiente — démonstration. (4) Actif sans risque & CML. (5) Le CAPM — démonstration. (6) Hypothèses, critique de Roll, anomalies. (7) Mesures de performance — démonstrations & exemples. (8) Études de cas. (9) Formulaire. (10) 100+ QCM + corrigé commenté.

---

# 1. Fondations statistiques : rendement et risque

## 1.1 Rendement et risque d'un actif

Le **rendement espéré** d'un actif est l'espérance de sa distribution de rendements. Sur des états du monde $s$ de probabilité $p_s$ :

$$E(R) = \sum_{s} p_s\, R_s .$$

Le **risque** se mesure par la **variance** (ou l'écart-type), dispersion autour de l'espérance :

$$\sigma^2 = E\!\left[(R - E(R))^2\right], \qquad \sigma = \sqrt{\sigma^2}.$$

Sur un échantillon de $T$ observations, on estime $\hat{\mu}=\frac1T\sum_t R_t$ et $\hat{\sigma}^2=\frac{1}{T-1}\sum_t (R_t-\hat\mu)^2$ (diviseur $T-1$ : estimateur sans biais).

## 1.2 Covariance et corrélation

La **covariance** mesure le co-mouvement de deux actifs ; la **corrélation** la normalise dans $[-1,1]$ :

$$\operatorname{Cov}(R_1,R_2)=E\!\left[(R_1-E(R_1))(R_2-E(R_2))\right]=\rho_{12}\,\sigma_1\sigma_2,
\qquad \rho_{12}=\frac{\operatorname{Cov}(R_1,R_2)}{\sigma_1\sigma_2}.$$

## 1.3 Rendement et variance d'un portefeuille

Pour un portefeuille de poids $w_i$ (avec $\sum_i w_i = 1$), le rendement espéré est **linéaire** :

$$E(R_P)=\sum_{i=1}^{N} w_i\,E(R_i)=\mathbf{w}^{\top}\boldsymbol{\mu}.$$

La **variance** dépend des covariances et s'écrit, en notation matricielle avec $\Sigma$ la matrice de variance-covariance :

$$\boxed{\;\sigma_P^2=\sum_{i=1}^{N}\sum_{j=1}^{N} w_i w_j\,\sigma_{ij}=\mathbf{w}^{\top}\Sigma\,\mathbf{w}\;}$$

**Cas à deux actifs** (à connaître par cœur) :

$$\sigma_P^2 = w_1^2\sigma_1^2 + w_2^2\sigma_2^2 + 2\,w_1 w_2\,\rho_{12}\,\sigma_1\sigma_2 .$$

> **Idée maîtresse.** Le rendement d'un portefeuille est la **moyenne pondérée** des rendements, mais son risque est **inférieur** à la moyenne pondérée des risques dès que $\rho_{12}<1$ : c'est le **bénéfice de diversification**.

## 1.4 Exemple chiffré — portefeuille à deux actifs

Soient $w_1=0.60$, $w_2=0.40$, $E(R_1)=10\%$, $E(R_2)=5\%$, $\sigma_1=20\%$, $\sigma_2=10\%$, $\rho_{12}=0.30$.

$$E(R_P)=0.60(10\%)+0.40(5\%)=8.0\%.$$
$$\sigma_P^2 = 0.6^2(0.20)^2+0.4^2(0.10)^2+2(0.6)(0.4)(0.30)(0.20)(0.10)$$
$$=0.0144+0.0016+0.00288=0.01888 \;\Rightarrow\; \sigma_P=\sqrt{0.01888}=13.7\%.$$

La moyenne pondérée des volatilités serait $0.6(20\%)+0.4(10\%)=16\%$ : la diversification a **réduit le risque de $\approx 2.3$ points** sans rien sacrifier au rendement.

---

# 2. La diversification — démonstration

## 2.1 Décomposition de la variance d'un portefeuille équipondéré

Pour un portefeuille **équipondéré** ($w_i=1/N$), on sépare les $N$ termes de variance des $N(N-1)$ termes de covariance :

$$\sigma_P^2=\sum_{i=1}^N \frac{1}{N^2}\sigma_i^2 + \sum_{i\neq j}\frac{1}{N^2}\sigma_{ij}
=\frac{1}{N}\,\overline{\sigma^2}+\frac{N-1}{N}\,\overline{\operatorname{cov}},$$

où $\overline{\sigma^2}=\frac1N\sum_i\sigma_i^2$ est la **variance moyenne** et $\overline{\operatorname{cov}}=\frac{1}{N(N-1)}\sum_{i\neq j}\sigma_{ij}$ la **covariance moyenne**. D'où la **formule fondamentale** :

$$\boxed{\;\sigma_P^2=\frac{1}{N}\,\overline{\sigma^2}+\left(1-\frac{1}{N}\right)\overline{\operatorname{cov}}\;}$$

## 2.2 Le plancher systématique : passage à la limite

Quand $N\to\infty$, le premier terme s'annule et il reste **la covariance moyenne** :

$$\lim_{N\to\infty}\sigma_P^2=\overline{\operatorname{cov}}.$$

**Conclusion décisive.** La diversification élimine le risque **idiosyncratique** (terme en $1/N$), mais **jamais** le risque **systématique** (porté par $\overline{\operatorname{cov}}$). C'est le fondement du CAPM : **seul le risque non diversifiable est rémunéré**.

## 2.3 Exemple chiffré — la diversification à l'œuvre

Variance moyenne $\overline{\sigma^2}=0.04$ ($\sigma=20\%$ par actif), corrélation moyenne $\bar\rho=0.30$, donc $\overline{\operatorname{cov}}=0.30\times0.04=0.012$ :

| $N$ | $\sigma_P^2$ | $\sigma_P$ |
|---|---|---|
| 1 | 0.04000 | 20.0% |
| 2 | 0.02600 | 16.1% |
| 5 | 0.01760 | 13.3% |
| 10 | 0.01480 | 12.2% |
| 20 | 0.01340 | 11.6% |
| 50 | 0.01256 | 11.2% |
| $\infty$ | 0.01200 | 11.0% |

**Lecture.** L'essentiel du bénéfice est capté **vite** ($\approx 20$–$30$ titres) ; au-delà, on s'approche du **plancher systématique** $\sigma_\infty=\sqrt{0.012}=11.0\%$, **incompressible**.

## 2.4 Risque systématique vs idiosyncratique

Au niveau d'un **actif**, via le modèle de marché (§5.4) :

$$\sigma_i^2=\underbrace{\beta_i^2\,\sigma_M^2}_{\text{systématique}}+\underbrace{\sigma_{\varepsilon,i}^2}_{\text{idiosyncratique}}.$$

Le risque **systématique** est commun et **non diversifiable** ; le risque **idiosyncratique** **s'annule** dans un grand portefeuille. **Seul le premier est rémunéré.**

---

# 3. La frontière efficiente — démonstration

## 3.1 Ensemble des portefeuilles et frontière efficiente

En faisant varier les poids, deux actifs engendrent une **courbe** dans le plan $(\sigma,\,E(R))$. L'ensemble des portefeuilles atteignant, pour chaque niveau de risque, le **rendement maximal** (et, pour chaque rendement, le **risque minimal**) constitue la **frontière à variance minimale**. Sa **partie supérieure**, au-dessus du **portefeuille de variance minimale (MVP)**, est la **frontière efficiente** : aucun investisseur rationnel ne détient un portefeuille situé en dessous.

## 3.2 Démonstration — poids du portefeuille de variance minimale (MVP)

On minimise $\sigma_P^2$ à deux actifs avec $w_2=1-w_1$ :

$$\sigma_P^2(w_1)=w_1^2\sigma_1^2+(1-w_1)^2\sigma_2^2+2w_1(1-w_1)\sigma_{12}.$$

Condition du premier ordre $\dfrac{d\sigma_P^2}{dw_1}=0$ :

$$2w_1\sigma_1^2-2(1-w_1)\sigma_2^2+2(1-2w_1)\sigma_{12}=0
\;\Longrightarrow\; w_1\big(\sigma_1^2+\sigma_2^2-2\sigma_{12}\big)=\sigma_2^2-\sigma_{12}.$$

D'où les **poids du MVP** :

$$\boxed{\;w_1^{\text{MVP}}=\frac{\sigma_2^2-\sigma_{12}}{\sigma_1^2+\sigma_2^2-2\sigma_{12}},\qquad w_2^{\text{MVP}}=1-w_1^{\text{MVP}}\;}$$

## 3.3 Exemple chiffré — MVP

Soient $\sigma_1=20\%$, $\sigma_2=30\%$, $\rho_{12}=0.20$, donc $\sigma_{12}=0.20(0.20)(0.30)=0.012$.

$$w_1^{\text{MVP}}=\frac{0.09-0.012}{0.04+0.09-2(0.012)}=\frac{0.078}{0.106}=0.736,\qquad w_2^{\text{MVP}}=0.264.$$

$$\sigma_{\text{MVP}}^2=0.736^2(0.04)+0.264^2(0.09)+2(0.736)(0.264)(0.012)=0.0326\;\Rightarrow\;\sigma_{\text{MVP}}=18.1\%.$$

Le MVP a un risque ($18.1\%$) **inférieur à celui de l'actif le moins risqué** ($20\%$) : c'est possible car $\rho_{12}=0.20<\sigma_1/\sigma_2=0.667$.

## 3.4 Démonstration — le cas $\rho=-1$ : un portefeuille sans risque

Si $\rho_{12}=-1$, alors $\sigma_P=\big|\,w_1\sigma_1-w_2\sigma_2\,\big|$. On peut **annuler** le risque en posant $w_1\sigma_1=w_2\sigma_2$, soit avec $w_2=1-w_1$ :

$$w_1\sigma_1=(1-w_1)\sigma_2 \;\Longrightarrow\; \boxed{\,w_1=\frac{\sigma_2}{\sigma_1+\sigma_2}\,}.$$

**Exemple** : $\sigma_1=20\%$, $\sigma_2=30\%$ $\Rightarrow$ $w_1=30/50=0.60$, $w_2=0.40$, et $\sigma_P=0$. Une **couverture parfaite** : deux actifs risqués combinés deviennent sans risque.

## 3.5 L'effet de la corrélation sur la frontière

Plus la corrélation est **faible**, plus la frontière se **« courbe »** vers la gauche (plus de diversification), et plus l'ensemble des opportunités est **favorable** :

- $\rho=+1$ : la frontière est un **segment de droite** (aucun bénéfice de diversification) ;
- $-1<\rho<+1$ : la frontière est une **courbe** d'autant plus bombée que $\rho$ est faible ;
- $\rho=-1$ : la frontière « touche » l'**axe vertical** ($\sigma=0$ atteignable).

> **Point d'examen.** Le MVP et la forme de la frontière dépendent de $\rho$ ; une baisse de $\rho$ **améliore** strictement le compromis rendement-risque accessible.

---

# 4. L'actif sans risque, la CAL et la CML

## 4.1 La Capital Allocation Line (CAL)

En combinant l'**actif sans risque** (rendement $R_f$, $\sigma=0$) avec un portefeuille risqué $P$, on obtient une **droite** dans le plan $(\sigma,E(R))$ :

$$E(R_C)=R_f+\underbrace{\frac{E(R_P)-R_f}{\sigma_P}}_{\text{ratio de Sharpe de }P}\,\sigma_C.$$

La **pente** de la CAL est le **ratio de Sharpe** du portefeuille risqué : plus il est élevé, meilleure est la droite.

## 4.2 Portefeuille de tangence et théorème de séparation

Parmi tous les portefeuilles risqués, celui qui maximise le ratio de Sharpe — la CAL la **plus pentue**, **tangente** à la frontière efficiente — est le **portefeuille de tangence** $T$. **À l'équilibre de marché, $T$ est le portefeuille de marché $M$.** Conséquence (**théorème de séparation en deux fonds**) : **tous** les investisseurs détiennent la **même** combinaison risquée ($M$) ; seule la **proportion** investie en $R_f$ vs $M$ varie selon l'aversion au risque.

**Poids du portefeuille de tangence** (deux actifs risqués + actif sans risque), avec les excès de rendement $z_i=E(R_i)-R_f$ :

$$w_1^{T}=\frac{z_1\sigma_2^2-z_2\sigma_{12}}{z_1\sigma_2^2+z_2\sigma_1^2-(z_1+z_2)\sigma_{12}},\qquad w_2^{T}=1-w_1^{T}.$$

## 4.3 Exemple chiffré — portefeuille de tangence

$E(R_1)=10\%$, $E(R_2)=8\%$, $R_f=3\%$, $\sigma_1=20\%$, $\sigma_2=15\%$, $\rho=0.25$ $\Rightarrow$ $\sigma_{12}=0.0075$, $z_1=0.07$, $z_2=0.05$.

$$w_1^{T}=\frac{0.07(0.0225)-0.05(0.0075)}{0.07(0.0225)+0.05(0.04)-0.12(0.0075)}=\frac{0.0012}{0.002675}=0.449,\quad w_2^{T}=0.551.$$

Le portefeuille de tangence : $E(R_T)=0.449(10\%)+0.551(8\%)=8.90\%$ ; $\sigma_T=13.6\%$ ; **Sharpe** $=\dfrac{8.90\%-3\%}{13.6\%}=0.43$ — supérieur au Sharpe de chaque actif pris isolément.

## 4.4 La Capital Market Line (CML)

Quand le portefeuille de tangence est le **marché** $M$, la CAL devient la **Capital Market Line** :

$$\boxed{\;E(R_C)=R_f+\frac{E(R_M)-R_f}{\sigma_M}\,\sigma_C\;}$$

Sa pente $\dfrac{E(R_M)-R_f}{\sigma_M}$ est le **prix de marché du risque** (le **ratio de Sharpe du marché**). La CML relie rendement et **risque total $\sigma$**, et **ne concerne que les portefeuilles efficients** (combinaisons de $R_f$ et de $M$).

**Exemple** : $R_f=2\%$, $E(R_M)=8\%$, $\sigma_M=15\%$. Un portefeuille efficient de risque $\sigma_C=10\%$ a pour rendement $E(R_C)=2\%+\dfrac{6\%}{15\%}(10\%)=6.0\%$, obtenu en investissant $\sigma_C/\sigma_M=66.7\%$ dans le marché et $33.3\%$ au taux sans risque.

---

# 5. Le CAPM — démonstration

## 5.1 Démonstration — la contribution marginale au risque

À l'équilibre, **tous** les investisseurs détiennent le portefeuille de marché $M$. Ajoutons une fraction infinitésimale de l'actif $i$ à $M$ : sa **contribution marginale au risque** du marché est proportionnelle à $\operatorname{Cov}(R_i,R_M)$ (car $\frac{\partial \sigma_M^2}{\partial w_i}\propto \operatorname{Cov}(R_i,R_M)$). À l'optimum, le **rapport rendement excédentaire / risque marginal** doit être **identique** pour tous les actifs — sinon on réalloue. En l'égalisant entre l'actif $i$ et le marché lui-même :

$$\frac{E(R_i)-R_f}{\operatorname{Cov}(R_i,R_M)}=\frac{E(R_M)-R_f}{\sigma_M^2}.$$

En isolant $E(R_i)$ et en posant $\beta_i=\dfrac{\operatorname{Cov}(R_i,R_M)}{\sigma_M^2}$, on obtient le **CAPM** :

$$\boxed{\;E(R_i)=R_f+\beta_i\big[E(R_M)-R_f\big]\;}\qquad
\beta_i=\frac{\operatorname{Cov}(R_i,R_M)}{\sigma_M^2}=\rho_{i,M}\,\frac{\sigma_i}{\sigma_M}.$$

La représentation $E(R_i)$ en fonction de $\beta_i$ est la **Security Market Line (SML)**. Le bêta d'un portefeuille est la **moyenne pondérée** des bêtas : $\beta_P=\sum_i w_i\beta_i$.

## 5.2 Démonstration — pourquoi la CML est un cas particulier de la SML

Pour un portefeuille **efficient** $P$ (combinaison de $R_f$ et de $M$), on a $\rho_{P,M}=1$, donc :

$$\beta_P=\rho_{P,M}\frac{\sigma_P}{\sigma_M}=\frac{\sigma_P}{\sigma_M}.$$

En l'injectant dans la SML :

$$E(R_P)=R_f+\frac{\sigma_P}{\sigma_M}\big[E(R_M)-R_f\big]=R_f+\frac{E(R_M)-R_f}{\sigma_M}\,\sigma_P,$$

c'est-à-dire **exactement la CML**. **Conclusion** : la **CML est la SML restreinte aux portefeuilles efficients**. Un actif **inefficient** (qui possède du risque idiosyncratique, $\rho_{i,M}<1$) vérifie la **SML** mais **se situe sous la CML** (son risque total n'est pas entièrement rémunéré).

> **Point d'examen (le piège classique).** **CML** : axe = risque **total $\sigma$**, **portefeuilles efficients** seulement. **SML** : axe = risque **systématique $\beta$**, **tout actif**. Un actif **au-dessus de la SML** est **sous-évalué** ($\alpha>0$) ; **en dessous**, **surévalué**.

## 5.3 Le modèle de marché et la décomposition du risque

Empiriquement, on estime le bêta par la **régression du modèle de marché** (la *characteristic line*) :

$$R_i=\alpha_i+\beta_i R_M+\varepsilon_i,\qquad \hat\beta_i=\frac{\operatorname{Cov}(R_i,R_M)}{\sigma_M^2}.$$

La variance de l'actif se **décompose** alors en part systématique et idiosyncratique, et le **$R^2$** mesure la fraction systématique :

$$\sigma_i^2=\beta_i^2\sigma_M^2+\sigma_{\varepsilon,i}^2,\qquad R^2=\frac{\beta_i^2\sigma_M^2}{\sigma_i^2}=\rho_{i,M}^2.$$

## 5.4 Exemple chiffré — bêta, décomposition et rendement exigé

Une régression donne $\beta_i=1.2$ ; on observe $\sigma_M=15\%$ et un risque résiduel $\sigma_{\varepsilon,i}=10\%$.

$$\sigma_i^2=1.2^2(0.15)^2+(0.10)^2=0.0324+0.0100=0.0424\;\Rightarrow\;\sigma_i=20.6\%.$$
$$R^2=\frac{0.0324}{0.0424}=0.764\;(76.4\%\ \text{systématique}),\qquad \rho_{i,M}=\sqrt{0.764}=0.874.$$

Rendement **exigé** par le CAPM, avec $R_f=3\%$ et $E(R_M)=9\%$ :

$$E(R_i)=3\%+1.2\,(9\%-3\%)=10.2\%.$$

Si un analyste **prévoit** un rendement de $12\%$ pour cet actif ($>10.2\%$), l'actif est **sous-évalué** ($\alpha=+1.8\%$, il est **au-dessus de la SML**).

---

# 6. Hypothèses, critique de Roll et anomalies

## 6.1 Les hypothèses du CAPM

Le CAPM repose sur un monde idéalisé : investisseurs **rationnels et averses au risque** optimisant en **moyenne-variance** sur **une seule période** ; **anticipations homogènes** ; prêt et emprunt **illimités au même taux sans risque** ; marchés **sans frictions** (ni impôts ni coûts de transaction), actifs **parfaitement divisibles et liquides** ; information **gratuite et simultanée** ; aucun agent ne peut **influencer les prix** (preneurs de prix).

## 6.2 La critique de Roll

**Roll (1977)** : le **vrai** portefeuille de marché (qui devrait inclure **tous** les actifs — actions, obligations, immobilier, capital humain…) est **inobservable**. On le remplace par un **proxy** (ex. un indice actions). Conséquence : tout test du CAPM est un **test joint** du CAPM **et** de l'efficience du proxy ; rejeter le CAPM peut simplement signifier que le proxy est inefficient. **Le CAPM n'est donc pas réellement testable.**

## 6.3 Les anomalies empiriques

De nombreuses régularités **contredisent** le CAPM (un seul facteur ne suffit pas) :

- **Effet taille** (*size*) : les petites capitalisations sur-performent (facteur **SMB**).
- **Effet valeur** (*value*) : les titres à fort *book-to-market* sur-performent les *growth* (facteur **HML**).
- **Momentum** : les gagnants récents continuent de sur-performer (facteur **WML/UMD**).
- **Anomalie de faible volatilité / faible bêta** (*low-beta / Betting-Against-Beta*) : les titres à **faible** $\beta$ offrent un rendement **ajusté du risque supérieur** à ce que prédit le CAPM. Empiriquement, la **SML est « trop plate »** : les hauts $\beta$ rapportent **moins**, et les bas $\beta$ **plus**, que la théorie.
- **Profitabilité** et **investissement** (facteurs ajoutés par Fama-French 5 facteurs).

Ces anomalies motivent les **modèles multifactoriels** et l'**APT** (chapitre 6). Autres limites : **bêtas instables** dans le temps, hypothèses irréalistes, biais **comportementaux** des investisseurs.

> **Point d'examen.** Sache (1) lister les hypothèses, (2) énoncer la **critique de Roll** (marché inobservable $\Rightarrow$ test joint), (3) citer les anomalies (**taille, valeur, momentum, faible bêta**) et le fait que la **SML empirique est plus plate** que la théorie.

---

# 7. Mesures de performance ajustées du risque

## 7.1 Les six mesures et leur dénominateur

| Mesure | Formule | Risque utilisé | Contexte |
|---|---|---|---|
| Sharpe | $S=\dfrac{E(R_P)-R_f}{\sigma_P}$ | total $\sigma$ | portefeuille autonome |
| Treynor | $T=\dfrac{E(R_P)-R_f}{\beta_P}$ | systématique $\beta$ | composante d'un ensemble diversifié |
| Alpha de Jensen | $\alpha_P=E(R_P)-\big[R_f+\beta_P(E(R_M)-R_f)\big]$ | systématique $\beta$ | valeur ajoutée du gérant |
| Information Ratio | $\mathrm{IR}=\dfrac{E(R_P)-E(R_B)}{\sigma(R_P-R_B)}$ | tracking error | gestion active vs benchmark |
| Sortino | $\mathrm{SOR}=\dfrac{E(R_P)-R_{\min}}{\sigma_{\text{downside}}}$ | volatilité à la baisse | asymétrie / pertes |
| $M^2$ | $M^2=R_f+S_P\,\sigma_M$ | total $\sigma$ | comparaison au marché |

## 7.2 Exemples chiffrés

Soit $R_P=12\%$, $\sigma_P=18\%$, $\beta_P=1.1$ ; marché $R_M=9\%$, $\sigma_M=15\%$ ; $R_f=2\%$ ; benchmark $R_B=9.5\%$, tracking error $=4\%$ ; downside deviation $=12\%$, $R_{\min}=2\%$.

$$S=\frac{12-2}{18}=0.556;\quad T=\frac{12-2}{1.1}=9.09\%;\quad \alpha=12-[2+1.1(7)]=2.3\%;$$
$$\mathrm{IR}=\frac{12-9.5}{4}=0.625;\quad \mathrm{SOR}=\frac{12-2}{12}=0.833.$$

## 7.3 Démonstration — la mesure $M^2$

On construit un portefeuille $P^{*}$ en mélangeant $P$ et l'actif sans risque pour **égaler la volatilité du marché** : on investit la fraction $a=\sigma_M/\sigma_P$ dans $P$. Son rendement est :

$$R_{P^{*}}=R_f+a\,(R_P-R_f)=R_f+\frac{\sigma_M}{\sigma_P}(R_P-R_f)=R_f+S_P\,\sigma_M=M^2.$$

À volatilité **identique** ($\sigma_M$), comparer $M^2$ à $E(R_M)$ est légitime, et :

$$M^2-E(R_M)=(S_P-S_M)\,\sigma_M.$$

Le portefeuille **bat le marché** si et seulement si son **Sharpe dépasse celui du marché**. **Exemple** : $S_P=0.556$, $S_M=\frac{9-2}{15}=0.467$ $\Rightarrow$ $M^2=2\%+0.556(15\%)=10.33\%$ vs $R_M=9\%$ : sur-performance de $(0.556-0.467)(15\%)=+1.33\%$.

## 7.4 Démonstration — quand Sharpe et Treynor divergent

Deux portefeuilles de **même** excès de rendement ($8\%$) et **même** $\beta$ ($1.0$), mais l'un (B) plus **concentré** (risque idiosyncratique plus élevé) : $\sigma_A=16\%$, $\sigma_B=24\%$.

$$T_A=\frac{8}{1.0}=8;\quad T_B=\frac{8}{1.0}=8 \quad(\textbf{égaux}).$$
$$S_A=\frac{8}{16}=0.50;\quad S_B=\frac{8}{24}=0.333 \quad(\textbf{A meilleur}).$$

**Interprétation.** **Treynor** les juge **équivalents** (même $\beta$) : si l'on **ajoute** ces portefeuilles à un ensemble déjà diversifié, le risque idiosyncratique de B **disparaît** et seul le $\beta$ compte. **Sharpe** préfère A : si le portefeuille est détenu **isolément**, le risque idiosyncratique de B **pénalise** réellement. **La « bonne » mesure dépend du contexte** — c'est le piège d'examen classique. (Sharpe et Treynor donnent le **même classement** pour des portefeuilles **parfaitement diversifiés**.)

## 7.5 Le ratio d'appréciation (Treynor-Black)

Pour dimensionner un **pari actif**, on rapporte l'alpha au **risque idiosyncratique** assumé :

$$\mathrm{AR}=\frac{\alpha_P}{\sigma_{\varepsilon,P}}.$$

Plus ce ratio est élevé, plus le pari actif est efficient par unité de risque spécifique pris.

---

# 8. Études de cas pratiques

## 8.1 LTCM (1998) — quand la diversification s'évapore

**Les faits.** *Long-Term Capital Management*, dirigé par des prix Nobel, menait des paris de **convergence** (*relative value*) avec un **levier colossal** : fonds propres $\approx 4.7$ Md$, bilan $\approx 125$ Md$ (**levier $\approx 25{:}1$**) et un notionnel de dérivés de l'ordre de $1\,250$ Md$. La stratégie **supposait** une faible corrélation entre des dizaines de positions « diversifiées ».

**Le mécanisme.** Le **défaut russe** (août 1998) a déclenché une **fuite vers la qualité** : **tous** les spreads se sont écartés **ensemble** — la corrélation a **convergé vers 1**. Le portefeuille « diversifié » a perdu **simultanément** sur toutes ses jambes ; les **appels de marge** ont forcé la liquidation au pire moment ; pertes $\approx 4.6$ Md$ en quelques semaines, puis recapitalisation de $3.6$ Md$ orchestrée par la Fed avec 14 banques.

**La leçon (directement liée au §2).** La diversification de Markowitz **suppose des corrélations stables** ; en **crise**, les corrélations **bondissent vers 1** et le bénéfice de diversification **disparaît précisément quand on en a besoin**. Conjuguée au **levier** et au **risque de liquidité**, cette illusion de diversification est mortelle.

## 8.2 L'anomalie « faible bêta » (*Betting-Against-Beta*)

**Le constat empirique** (Black 1972 ; Frazzini-Pedersen 2014) : la **SML réalisée est trop plate**. Les titres à **faible** $\beta$ délivrent un rendement **ajusté du risque supérieur**, les **hauts** $\beta$ inférieur, à ce que prédit le CAPM. Une stratégie ***Betting-Against-Beta*** (acheter, avec levier, les bas $\beta$ ; vendre les hauts $\beta$) dégage un **alpha positif**. **Explication** : les investisseurs **contraints en levier** (qui ne peuvent emprunter) **surpondèrent** les titres à haut $\beta$ pour « doper » leur rendement, ce qui **renchérit** ces titres et **déprime** leur rendement futur — une violation directe du CAPM.

## 8.3 Les primes de taille et de valeur (Fama-French)

**Fama & French (1992)** montrent que, sur longue période, les **petites** capitalisations et les titres **value** (fort *book-to-market*) ont rapporté **davantage** que ne le justifie leur $\beta$ CAPM. Le CAPM **mésévalue** donc la coupe transversale des rendements ; **taille (SMB)** et **valeur (HML)** sont des facteurs de risque rémunérés **distincts** du marché. C'est le socle empirique de l'**investissement factoriel** et des modèles du chapitre 6.

## 8.4 2008 — l'échec de la diversification, à nouveau

Pendant la crise, les corrélations **inter-actifs** (actions, crédit, matières premières, immobilier) ont **convergé vers 1** : des portefeuilles « bien diversifiés » ont **chuté ensemble**. Même leçon que LTCM, à l'échelle systémique : les **corrélations ne sont pas stables**, surtout **dans les queues** — un avertissement central à toute application naïve de la MPT.

## 8.5 Étude de cas intégrée — évaluation d'un gérant

Un gérant affiche $R_P=14\%$, $\sigma_P=22\%$, $\beta_P=1.3$ ; le marché/benchmark $R_M=10\%$, $\sigma_M=16\%$ ; $R_f=3\%$ ; tracking error $=7\%$. Faut-il le retenir ?

$$S_P=\frac{14-3}{22}=0.50\;(>S_M=\tfrac{10-3}{16}=0.44);\quad T_P=\frac{14-3}{1.3}=8.46\%\;(>T_M=7.0\%);$$
$$\alpha_P=14-[3+1.3(7)]=+1.9\%;\quad \mathrm{IR}=\frac{14-10}{7}=0.571;\quad M^2=3+0.50(16)=11\%\;(>R_M=10\%).$$

**Conclusion.** Le gérant **crée de la valeur sur toutes les mesures** : Sharpe et Treynor supérieurs au marché, **alpha de $+1.9\%$**, $M^2$ supérieur de $+1$ point, et un **IR de $0.57$** (gestion active de bonne qualité, $\mathrm{IR}>0.5$). Décision cohérente : **retenir**, sous réserve de la robustesse statistique de l'historique.

## 8.6 Application — couverture du bêta (*beta hedging*)

Le $\beta$ n'est pas qu'un concept : pour **neutraliser le risque de marché** d'un portefeuille actions de valeur $V$ et de bêta $\beta_P$, on vend $N=\dfrac{\beta_P\,V}{\text{valeur d'un contrat}}$ contrats à terme sur indice, ramenant le bêta du portefeuille couvert à $\approx 0$. Le $\beta$ pilote directement le **ratio de couverture** (revu dans le book *Financial Markets and Products*).

---

# 9. Formulaire (notation GARP)

**Rendement / risque / portefeuille**
$$E(R_P)=\sum_i w_i E(R_i)=\mathbf{w}^\top\boldsymbol\mu,\qquad \sigma_P^2=\mathbf{w}^\top\Sigma\,\mathbf{w}$$
$$\sigma_P^2=w_1^2\sigma_1^2+w_2^2\sigma_2^2+2w_1w_2\rho_{12}\sigma_1\sigma_2,\qquad \operatorname{Cov}(R_1,R_2)=\rho_{12}\sigma_1\sigma_2$$

**Diversification / décomposition**
$$\sigma_P^2=\tfrac{1}{N}\overline{\sigma^2}+\big(1-\tfrac1N\big)\overline{\operatorname{cov}}\ \xrightarrow[N\to\infty]{}\ \overline{\operatorname{cov}},\qquad \sigma_i^2=\beta_i^2\sigma_M^2+\sigma_{\varepsilon,i}^2$$

**Frontière / MVP / tangence**
$$w_1^{\text{MVP}}=\frac{\sigma_2^2-\sigma_{12}}{\sigma_1^2+\sigma_2^2-2\sigma_{12}},\qquad w_1^{\rho=-1}=\frac{\sigma_2}{\sigma_1+\sigma_2},\qquad w_1^{T}=\frac{z_1\sigma_2^2-z_2\sigma_{12}}{z_1\sigma_2^2+z_2\sigma_1^2-(z_1+z_2)\sigma_{12}}\ \ (z_i=E(R_i)-R_f)$$

**Lignes de marché / CAPM**
$$\text{CML:}\ E(R_C)=R_f+\frac{E(R_M)-R_f}{\sigma_M}\sigma_C,\qquad \text{SML/CAPM:}\ E(R_i)=R_f+\beta_i[E(R_M)-R_f]$$
$$\beta_i=\frac{\operatorname{Cov}(R_i,R_M)}{\sigma_M^2}=\rho_{i,M}\frac{\sigma_i}{\sigma_M},\qquad \beta_P=\sum_i w_i\beta_i,\qquad R^2=\rho_{i,M}^2$$

**Mesures de performance**
$$S=\frac{E(R_P)-R_f}{\sigma_P},\quad T=\frac{E(R_P)-R_f}{\beta_P},\quad \alpha_P=E(R_P)-[R_f+\beta_P(E(R_M)-R_f)],$$
$$\mathrm{IR}=\frac{E(R_P)-E(R_B)}{\sigma(R_P-R_B)},\quad \mathrm{SOR}=\frac{E(R_P)-R_{\min}}{\sigma_{\text{down}}},\quad M^2=R_f+S_P\sigma_M,\quad \mathrm{AR}=\frac{\alpha_P}{\sigma_{\varepsilon,P}}$$

**À retenir** : seul le risque **systématique** est rémunéré ; **CML** = risque total (efficients), **SML** = $\beta$ (tout actif) ; **Sharpe/Sortino/$M^2$** = $\sigma$, **Treynor/Jensen** = $\beta$, **IR** = tracking error.

---

# 10. QCM (en anglais) — 104 questions

> Style examen FRM. Une seule réponse par question. **Corrigé commenté en français à la fin.** Pas de pénalité : répondez à tout.

## Bloc A — Returns, variance, covariance, portfolios

**Q1.** A portfolio holds 70% in asset 1 ($E(R)=12\%$) and 30% in asset 2 ($E(R)=6\%$). The portfolio expected return is — **(A)** 9.0% **(B)** 9.6% **(C)** 10.2% **(D)** 11.4%

**Q2.** Two assets each with $\sigma=20\%$, equally weighted, $\rho=0.5$. The portfolio standard deviation is — **(A)** 14.1% **(B)** 17.3% **(C)** 20.0% **(D)** 24.5%

**Q3.** With $\rho=0.4$, $\sigma_1=25\%$, $\sigma_2=15\%$, the covariance is — **(A)** 0.0150 **(B)** 0.0600 **(C)** 0.0094 **(D)** 0.1500

**Q4.** With $\operatorname{Cov}=0.018$, $\sigma_1=30\%$, $\sigma_2=20\%$, the correlation is — **(A)** 0.18 **(B)** 0.30 **(C)** 0.60 **(D)** 0.90

**Q5.** Holding $\sigma$'s and weights constant, which correlation gives the lowest portfolio variance? **(A)** $\rho=1$ **(B)** $\rho=0.5$ **(C)** $\rho=0$ **(D)** $\rho=-0.5$

**Q6.** For two perfectly correlated assets ($\rho=1$), the portfolio standard deviation equals — **(A)** less than the weighted average of $\sigma$'s **(B)** the weighted average of $\sigma$'s **(C)** zero **(D)** greater than the weighted average

**Q7.** An asset has variance $\sigma^2=0.0625$. Its standard deviation is — **(A)** 6.25% **(B)** 12.5% **(C)** 25% **(D)** 62.5%

**Q8.** The unbiased sample variance uses a divisor of — **(A)** $T$ **(B)** $T-1$ **(C)** $T+1$ **(D)** $\sqrt{T}$

**Q9.** A portfolio holds 50%/30%/20% in assets with $E(R)=8\%/12\%/20\%$. Its expected return is — **(A)** 10.0% **(B)** 11.6% **(C)** 13.3% **(D)** 40.0%

**Q10.** In matrix form the portfolio variance is — **(A)** $\mathbf{w}^\top\boldsymbol\mu$ **(B)** $\mathbf{w}^\top\Sigma\mathbf{w}$ **(C)** $\boldsymbol\mu^\top\Sigma\boldsymbol\mu$ **(D)** $\Sigma\mathbf{w}$

**Q11.** A risk-free combination of two risky assets is achievable — **(A)** always, for any weights **(B)** only when $\rho=-1$, with $w_1=\sigma_2/(\sigma_1+\sigma_2)$ **(C)** never **(D)** only when $\rho=+1$

**Q12.** With a short position $w_1=1.3$, $w_2=-0.3$ and $E(R)=10\%/4\%$, the portfolio expected return is — **(A)** 8.2% **(B)** 10.0% **(C)** 11.8% **(D)** 13.0%

**Q13.** If two assets have $\rho=+1$, the diversification benefit is — **(A)** maximal **(B)** zero **(C)** negative **(D)** infinite

**Q14.** Adding a volatile asset with low/negative correlation to a portfolio — **(A)** always increases risk **(B)** can reduce portfolio risk despite the asset's volatility **(C)** has no effect **(D)** helps only if the asset has low volatility

## Bloc B — Diversification

**Q15.** The equal-weighted portfolio variance equals — **(A)** $\frac1N\overline{\sigma^2}+(1-\frac1N)\overline{\operatorname{cov}}$ **(B)** $\overline{\sigma^2}+\overline{\operatorname{cov}}$ **(C)** $\frac1N\overline{\operatorname{cov}}$ **(D)** $N\,\overline{\sigma^2}$

**Q16.** As $N\to\infty$, the portfolio variance approaches — **(A)** 0 **(B)** the average variance **(C)** the average covariance **(D)** the largest variance

**Q17.** Diversification eliminates which type of risk? **(A)** systematic **(B)** idiosyncratic **(C)** market **(D)** all risk

**Q18.** With average variance 0.0441 ($\sigma=21\%$) and average covariance 0.009, the limiting $\sigma$ as $N\to\infty$ is — **(A)** 0% **(B)** 9.5% **(C)** 21% **(D)** 30%

**Q19.** Which risk is rewarded with a premium? **(A)** idiosyncratic **(B)** diversifiable **(C)** systematic **(D)** total

**Q20.** Equal-weighted, $N=4$, average variance 0.04, average covariance 0.01. The portfolio variance is — **(A)** 0.0175 **(B)** 0.0250 **(C)** 0.0400 **(D)** 0.0100

**Q21.** Most of the diversification benefit is typically achieved with roughly — **(A)** 2–3 securities **(B)** 20–30 securities **(C)** 500+ securities **(D)** it never plateaus

**Q22.** The undiversifiable risk floor equals — **(A)** the average variance **(B)** the average covariance **(C)** zero **(D)** the risk-free rate

## Bloc C — Efficient frontier & MVP

**Q23.** The efficient frontier is — **(A)** all possible portfolios **(B)** the set offering maximum return per risk level, above the MVP **(C)** the part below the MVP **(D)** the CML

**Q24.** The two-asset MVP weight is — **(A)** $\frac{\sigma_2^2-\sigma_{12}}{\sigma_1^2+\sigma_2^2-2\sigma_{12}}$ **(B)** $\frac{\sigma_1^2}{\sigma_1^2+\sigma_2^2}$ **(C)** $\frac{E(R_1)-R_f}{\sigma_1}$ **(D)** $\frac{\sigma_2}{\sigma_1+\sigma_2}$

**Q25.** MVP with $\sigma_1=10\%$, $\sigma_2=20\%$, $\rho=0$: $w_1$ is — **(A)** 0.20 **(B)** 0.50 **(C)** 0.80 **(D)** 0.67

**Q26.** With $\rho=-1$, $\sigma_1=10\%$, $\sigma_2=15\%$, the riskless weight $w_1$ is — **(A)** 0.40 **(B)** 0.50 **(C)** 0.60 **(D)** 0.67

**Q27.** Lower correlation between two assets makes the frontier — **(A)** a straight line **(B)** more bowed (better opportunities) **(C)** worse **(D)** unchanged

**Q28.** When $\rho=+1$, the two-asset frontier is — **(A)** a curve **(B)** a straight line **(C)** touching $\sigma=0$ **(D)** a single point

**Q29.** The MVP can have lower risk than the least-risky asset when — **(A)** $\rho$ is high **(B)** $\rho<\sigma_{\min}/\sigma_{\max}$ **(C)** never **(D)** always

**Q30.** A portfolio plotting below the efficient frontier is — **(A)** optimal **(B)** dominated (inefficient) **(C)** the MVP **(D)** the market portfolio

**Q31.** MVP with $\sigma_1=20\%$, $\sigma_2=30\%$, $\rho=0.5$: $w_1$ is — **(A)** 0.50 **(B)** 0.71 **(C)** 0.86 **(D)** 0.93

**Q32.** The lower half of the minimum-variance frontier is — **(A)** efficient **(B)** inefficient (dominated) **(C)** the CML **(D)** the SML

## Bloc D — Risk-free asset, CAL, CML, tangency

**Q33.** The slope of the Capital Allocation Line equals — **(A)** beta **(B)** the Sharpe ratio of the risky portfolio **(C)** the risk-free rate **(D)** the market variance

**Q34.** The two-fund separation theorem implies investors hold — **(A)** different risky portfolios **(B)** the same risky (market) portfolio combined with the risk-free asset in varying proportions **(C)** only the risk-free asset **(D)** only equities

**Q35.** The slope of the CML is — **(A)** the market beta **(B)** the market Sharpe ratio **(C)** the Treynor ratio **(D)** zero

**Q36.** CML with $R_f=2\%$, $E(R_M)=10\%$, $\sigma_M=20\%$: a portfolio with $\sigma=15\%$ has expected return — **(A)** 6% **(B)** 7% **(C)** 8% **(D)** 9%

**Q37.** To achieve $\sigma=15\%$ on the CML ($\sigma_M=20\%$), the weight in the market portfolio is — **(A)** 50% **(B)** 60% **(C)** 75% **(D)** 100%

**Q38.** The tangency portfolio is the risky portfolio that — **(A)** has minimum variance **(B)** maximizes the Sharpe ratio **(C)** has zero beta **(D)** lies only on the SML

**Q39.** The best CAL is tangent to the efficient frontier at the — **(A)** MVP **(B)** tangency (market) portfolio **(C)** risk-free asset **(D)** maximum-return asset

**Q40.** The CML applies to — **(A)** any individual security **(B)** efficient portfolios only **(C)** inefficient portfolios **(D)** zero-beta assets

**Q41.** On the CML with 130% in the market (borrowing at $R_f$), $R_f=3\%$, $E(R_M)=9\%$: the expected return is — **(A)** 9.0% **(B)** 10.2% **(C)** 10.8% **(D)** 11.7%

**Q42.** The "market price of risk" is — **(A)** beta **(B)** $\frac{E(R_M)-R_f}{\sigma_M}$ **(C)** the risk-free rate **(D)** alpha

**Q43.** Lending portfolios (positive weight in $R_f$) lie on the CML — **(A)** above $M$ **(B)** between $R_f$ and $M$ **(C)** beyond $M$ **(D)** below $R_f$

**Q44.** If a risky portfolio's Sharpe ratio is below the market's, its CAL is — **(A)** steeper than the CML **(B)** flatter than the CML **(C)** identical to the CML **(D)** vertical

## Bloc E — CAPM, beta, SML

**Q45.** Beta is defined as — **(A)** $\operatorname{Cov}(R_i,R_M)/\sigma_M^2$ **(B)** $\operatorname{Cov}(R_i,R_M)/\sigma_i^2$ **(C)** $\rho\,\sigma_M/\sigma_i$ **(D)** $\sigma_i^2/\sigma_M^2$

**Q46.** CAPM required return with $R_f=3\%$, $E(R_M)=9\%$, $\beta=1.4$ is — **(A)** 9.0% **(B)** 10.2% **(C)** 11.4% **(D)** 12.6%

**Q47.** Beta with $\operatorname{Cov}=0.030$, $\sigma_M^2=0.025$ is — **(A)** 0.83 **(B)** 1.20 **(C)** 1.25 **(D)** 0.75

**Q48.** Beta with $\rho=0.6$, $\sigma_i=25\%$, $\sigma_M=15\%$ is — **(A)** 0.36 **(B)** 0.60 **(C)** 1.00 **(D)** 1.67

**Q49.** A stock with $\beta=0$ has an expected return of — **(A)** 0 **(B)** $R_f$ **(C)** $E(R_M)$ **(D)** a negative value

**Q50.** A defensive stock has — **(A)** $\beta>1$ **(B)** $\beta<1$ **(C)** $\beta=1$ **(D)** $\beta<0$

**Q51.** Portfolio beta with weights 0.4/0.4/0.2 and betas 0.8/1.2/1.6 is — **(A)** 1.00 **(B)** 1.12 **(C)** 1.20 **(D)** 1.33

**Q52.** A stock plotting above the SML is — **(A)** fairly valued **(B)** undervalued (positive alpha) **(C)** overvalued **(D)** risk-free

**Q53.** Implied market risk premium when CAPM $E(R)=12\%$, $\beta=1.5$, $R_f=3\%$ is — **(A)** 4% **(B)** 5% **(C)** 6% **(D)** 9%

**Q54.** The market portfolio has a beta of — **(A)** 0 **(B)** 0.5 **(C)** 1 **(D)** it depends

**Q55.** The CAPM prices which risk? **(A)** total **(B)** idiosyncratic **(C)** systematic ($\beta$) **(D)** downside

**Q56.** Jensen's alpha is the vertical distance of an asset from the — **(A)** CML **(B)** SML **(C)** efficient frontier **(D)** CAL

**Q57.** A stock has $\beta=1.5$; if the market rises 10% (negligible $R_f$), the expected stock move is approximately — **(A)** 10% **(B)** 15% **(C)** 5% **(D)** 1.5%

**Q58.** A negative-beta asset — **(A)** is impossible **(B)** acts as a hedge, with expected return below $R_f$ **(C)** always loses money **(D)** has zero risk

**Q59.** Required return with $\beta=0.8$, $R_f=2\%$, $E(R_M)=8\%$ is — **(A)** 5.6% **(B)** 6.8% **(C)** 7.4% **(D)** 8.0%

**Q60.** A stock has $\beta=1.2$, $R_f=3\%$, $E(R_M)=9\%$; a forecast return of 12% implies — **(A)** overvalued **(B)** undervalued, $\alpha=+1.8\%$ **(C)** fairly valued **(D)** undervalued, $\alpha=+3\%$

**Q61.** The Security Market Line plots expected return against — **(A)** $\sigma$ **(B)** $\beta$ **(C)** variance **(D)** tracking error

**Q62.** A portfolio beta of 1.3 means it is — **(A)** defensive **(B)** aggressive (more volatile than the market) **(C)** market-neutral **(D)** risk-free

**Q63.** Two stocks with the same total risk $\sigma$ but different betas have, per the CAPM — **(A)** the same required return **(B)** different required returns (higher for higher beta) **(C)** zero required return **(D)** equal alpha

**Q64.** The beta of a portfolio that is 50% market and 50% risk-free is — **(A)** 0 **(B)** 0.5 **(C)** 1.0 **(D)** 1.5

**Q65.** The market risk premium is — **(A)** $E(R_M)$ **(B)** $E(R_M)-R_f$ **(C)** $R_f$ **(D)** $\beta(E(R_M)-R_f)$

**Q66.** A portfolio whose CAPM-required return equals $R_f$ has a beta of — **(A)** 0 **(B)** 0.5 **(C)** 1 **(D)** −1

## Bloc F — CML vs SML, market model, R²

**Q67.** The key difference between the CML and the SML is — **(A)** both use $\sigma$ **(B)** the CML uses $\sigma$ (efficient portfolios), the SML uses $\beta$ (any asset) **(C)** the SML uses $\sigma$ **(D)** the CML uses $\beta$

**Q68.** For an efficient portfolio, beta equals — **(A)** 1 **(B)** $\sigma_P/\sigma_M$ **(C)** $\sigma_M/\sigma_P$ **(D)** 0

**Q69.** The market-model regression $R_i=\alpha+\beta R_M+\varepsilon$ estimates beta as — **(A)** $\operatorname{Cov}(R_i,R_M)/\sigma_M^2$ **(B)** $\alpha/\sigma_\varepsilon$ **(C)** $\sigma_i/\sigma_M$ **(D)** $\rho$

**Q70.** In the market model, $R^2$ equals — **(A)** $\beta$ **(B)** $\rho_{i,M}^2$ **(C)** $\alpha$ **(D)** $\sigma_\varepsilon^2$

**Q71.** Variance decomposition with $\beta=1.0$, $\sigma_M=20\%$, $\sigma_\varepsilon=15\%$ gives $\sigma_i$ of — **(A)** 20% **(B)** 25% **(C)** 35% **(D)** 5%

**Q72.** The systematic fraction ($R^2$) for the asset in Q71 is — **(A)** 36% **(B)** 64% **(C)** 80% **(D)** 100%

**Q73.** An inefficient asset ($\rho_{i,M}<1$) plots, relative to the CML — **(A)** on it **(B)** below it **(C)** above it **(D)** cannot be determined

**Q74.** The characteristic line is the regression of — **(A)** the asset's return on the market return **(B)** the market on the asset **(C)** $\sigma$ on $\beta$ **(D)** return on time

**Q75.** If $R^2=1$ for an asset versus the market, the asset has — **(A)** only idiosyncratic risk **(B)** no idiosyncratic risk (perfectly correlated with the market) **(C)** no risk **(D)** zero correlation

**Q76.** Higher idiosyncratic risk ($\sigma_\varepsilon$), holding $\beta$ and $\sigma_M$ fixed — **(A)** raises $R^2$ **(B)** lowers $R^2$ **(C)** has no effect **(D)** raises $\beta$

## Bloc G — Performance measures

**Q77.** Sharpe ratio with $R_P=11\%$, $R_f=3\%$, $\sigma=16\%$ is — **(A)** 0.30 **(B)** 0.50 **(C)** 0.69 **(D)** 0.80

**Q78.** Treynor ratio with $R_P=11\%$, $R_f=3\%$, $\beta=1.0$ is — **(A)** 0.05 **(B)** 0.08 **(C)** 0.069 **(D)** 0.11

**Q79.** Jensen's alpha with $R_P=14\%$, $\beta=1.2$, $R_f=3\%$, $R_M=9\%$ is — **(A)** 1.8% **(B)** 2.8% **(C)** 3.8% **(D)** 5.0%

**Q80.** Information ratio with $R_P=12\%$, $R_B=9\%$, tracking error 5% is — **(A)** 0.40 **(B)** 0.60 **(C)** 0.75 **(D)** 1.20

**Q81.** Which measure uses total risk ($\sigma$) in the denominator? **(A)** Treynor **(B)** Jensen **(C)** Sharpe **(D)** all use $\beta$

**Q82.** Which measure uses systematic risk ($\beta$)? **(A)** Sharpe **(B)** Treynor **(C)** Sortino **(D)** $M^2$

**Q83.** The Sortino ratio differs from the Sharpe ratio by using — **(A)** beta **(B)** downside deviation instead of total $\sigma$ **(C)** tracking error **(D)** the market return

**Q84.** The $M^2$ measure equals — **(A)** $R_f+S_P\sigma_M$ **(B)** $S_P\sigma_P$ **(C)** $T_P\beta$ **(D)** $\alpha/\sigma_\varepsilon$

**Q85.** $M^2$ with $S_P=0.5$, $\sigma_M=18\%$, $R_f=2\%$ is — **(A)** 9% **(B)** 10% **(C)** 11% **(D)** 12%

**Q86.** The information ratio's denominator is — **(A)** $\sigma_P$ **(B)** $\beta_P$ **(C)** tracking error **(D)** downside deviation

**Q87.** Sharpe and Treynor give the same ranking when portfolios are — **(A)** poorly diversified **(B)** perfectly diversified **(C)** levered **(D)** short

**Q88.** Sharpe and Treynor can disagree when portfolios differ in — **(A)** nothing (never disagree) **(B)** idiosyncratic (diversifiable) risk **(C)** only with negative returns **(D)** only for the market

**Q89.** A portfolio's $M^2$ exceeds the market return if and only if — **(A)** its beta $>1$ **(B)** its Sharpe ratio exceeds the market's **(C)** its alpha is negative **(D)** its tracking error is zero

**Q90.** The appraisal (Treynor-Black) ratio is — **(A)** $\alpha/\sigma_\varepsilon$ **(B)** $(R_P-R_f)/\sigma_P$ **(C)** $\alpha/\beta$ **(D)** $R^2/\beta$

**Q91.** The most appropriate measure for a single sleeve within a diversified fund is — **(A)** Sharpe **(B)** Treynor **(C)** Sortino **(D)** $M^2$

**Q92.** Treynor ratio with $R_P=10\%$, $R_f=2\%$, $\beta=1.6$ is — **(A)** 0.040 **(B)** 0.050 **(C)** 0.063 **(D)** 0.080

**Q93.** Jensen's alpha measures performance relative to — **(A)** the benchmark return **(B)** the CAPM-required return **(C)** the risk-free rate **(D)** total risk

**Q94.** A negative Jensen's alpha means the portfolio — **(A)** beat its CAPM benchmark **(B)** underperformed its CAPM-required return **(C)** had zero beta **(D)** is risk-free

## Bloc H — Assumptions, Roll, anomalies, APT

**Q95.** Which is NOT a CAPM assumption? **(A)** homogeneous expectations **(B)** frictionless markets **(C)** investors can influence prices **(D)** a single-period horizon

**Q96.** Roll's critique states that — **(A)** beta is stable **(B)** the true market portfolio is unobservable, so the CAPM is not truly testable **(C)** the CAPM has too many factors **(D)** idiosyncratic risk is priced

**Q97.** The value premium is captured by which Fama-French factor? **(A)** SMB **(B)** HML **(C)** WML **(D)** market

**Q98.** The size premium is captured by — **(A)** SMB **(B)** HML **(C)** momentum **(D)** market

**Q99.** The Betting-Against-Beta (low-beta) anomaly implies the empirical SML is — **(A)** steeper than the CAPM predicts **(B)** flatter than the CAPM predicts **(C)** vertical **(D)** exactly as predicted

**Q100.** The Carhart model adds which factor to Fama-French? **(A)** liquidity **(B)** momentum (WML/UMD) **(C)** volatility **(D)** credit

**Q101.** Compared with the CAPM, the APT — **(A)** requires the market portfolio **(B)** rests on no-arbitrage and does not require the market portfolio **(C)** assumes a single factor **(D)** requires mean-variance preferences

**Q102.** A practical weakness of the APT is that it — **(A)** needs the risk-free rate **(B)** does not specify the factors **(C)** cannot exceed two factors **(D)** assumes homogeneous expectations

**Q103.** Empirically, high-beta stocks tend to deliver returns that are — **(A)** higher than the CAPM predicts **(B)** lower than the CAPM predicts **(C)** exactly as predicted **(D)** zero

**Q104.** The CAPM is a special case of the APT with — **(A)** three factors **(B)** a single factor (the market) **(C)** no factors **(D)** only idiosyncratic risk

---

# Corrigé commenté (en français)

> Réponse en **gras**, avec calcul lorsqu'il est utile.

## Bloc A

**Q1 — C.** $0.7(12\%)+0.3(6\%)=10.2\%$.
**Q2 — B.** $\sigma_P^2=0.25(0.04)+0.25(0.04)+2(0.5)(0.5)(0.5)(0.2)(0.2)=0.03$, $\sigma_P=17.3\%$.
**Q3 — A.** $\operatorname{Cov}=0.4(0.25)(0.15)=0.0150$.
**Q4 — B.** $\rho=0.018/(0.30\cdot0.20)=0.30$.
**Q5 — D.** La variance est croissante en $\rho$ ; la plus faible corrélation ($\rho=-0.5$) minimise le risque.
**Q6 — B.** Si $\rho=1$, $\sigma_P$ = moyenne pondérée des $\sigma$ (aucun bénéfice de diversification).
**Q7 — C.** $\sigma=\sqrt{0.0625}=0.25=25\%$.
**Q8 — B.** Estimateur sans biais : diviseur $T-1$.
**Q9 — B.** $0.5(8)+0.3(12)+0.2(20)=11.6\%$.
**Q10 — B.** $\sigma_P^2=\mathbf{w}^\top\Sigma\mathbf{w}$ ; $\mathbf{w}^\top\boldsymbol\mu$ est le rendement.
**Q11 — B.** Risque nul atteignable uniquement si $\rho=-1$, avec $w_1=\sigma_2/(\sigma_1+\sigma_2)$.
**Q12 — C.** $1.3(10\%)+(-0.3)(4\%)=13\%-1.2\%=11.8\%$.
**Q13 — B.** $\rho=+1$ : bénéfice de diversification nul.
**Q14 — B.** Un actif volatil mais peu/négativement corrélé peut **réduire** le risque global (c'est la covariance, pas la volatilité seule, qui compte).

## Bloc B

**Q15 — A.** $\sigma_P^2=\frac1N\overline{\sigma^2}+(1-\frac1N)\overline{\operatorname{cov}}$.
**Q16 — C.** À la limite, il reste la **covariance moyenne** (risque systématique).
**Q17 — B.** La diversification supprime le risque **idiosyncratique** seulement.
**Q18 — B.** $\sigma_\infty=\sqrt{0.009}=0.0949\approx 9.5\%$.
**Q19 — C.** Seul le risque **systématique** est rémunéré.
**Q20 — A.** $0.25(0.04)+0.75(0.01)=0.0175$.
**Q21 — B.** L'essentiel du bénéfice est atteint vers **20–30** titres.
**Q22 — B.** Le plancher incompressible = **covariance moyenne**.

## Bloc C

**Q23 — B.** Frontière efficiente = rendement max par niveau de risque, **au-dessus du MVP**.
**Q24 — A.** $w_1^{\text{MVP}}=\frac{\sigma_2^2-\sigma_{12}}{\sigma_1^2+\sigma_2^2-2\sigma_{12}}$.
**Q25 — C.** $\sigma_{12}=0$ : $w_1=0.04/(0.01+0.04)=0.80$.
**Q26 — C.** $w_1=\sigma_2/(\sigma_1+\sigma_2)=15/25=0.60$.
**Q27 — B.** Corrélation plus faible $\Rightarrow$ frontière plus **bombée** (meilleures opportunités).
**Q28 — B.** $\rho=+1$ $\Rightarrow$ frontière = **droite**.
**Q29 — B.** Le MVP bat l'actif le moins risqué si $\rho<\sigma_{\min}/\sigma_{\max}$.
**Q30 — B.** Sous la frontière $\Rightarrow$ portefeuille **dominé (inefficient)**.
**Q31 — C.** $\sigma_{12}=0.03$ : $w_1=(0.09-0.03)/(0.04+0.09-0.06)=0.06/0.07=0.857\approx 0.86$.
**Q32 — B.** La moitié inférieure de la frontière est **inefficiente**.

## Bloc D

**Q33 — B.** Pente de la CAL = **ratio de Sharpe** du portefeuille risqué.
**Q34 — B.** Séparation en deux fonds : tous détiennent $M$ + $R_f$ en proportions variables.
**Q35 — B.** Pente de la CML = **ratio de Sharpe du marché**.
**Q36 — C.** $E=2\%+\frac{8\%}{20\%}(15\%)=2\%+6\%=8\%$.
**Q37 — C.** $w_M=\sigma/\sigma_M=15/20=75\%$.
**Q38 — B.** Le portefeuille de tangence **maximise le Sharpe**.
**Q39 — B.** La meilleure CAL est tangente au **portefeuille de tangence (= marché)**.
**Q40 — B.** La CML ne concerne que les **portefeuilles efficients**.
**Q41 — C.** $E=3\%+1.3(9\%-3\%)=3\%+7.8\%=10.8\%$.
**Q42 — B.** Prix de marché du risque $=\frac{E(R_M)-R_f}{\sigma_M}$.
**Q43 — B.** Les portefeuilles « prêteurs » sont **entre $R_f$ et $M$**.
**Q44 — B.** Un Sharpe inférieur au marché $\Rightarrow$ CAL **plus plate** que la CML.

## Bloc E

**Q45 — A.** $\beta_i=\operatorname{Cov}(R_i,R_M)/\sigma_M^2$.
**Q46 — C.** $3\%+1.4(6\%)=11.4\%$.
**Q47 — B.** $0.030/0.025=1.20$.
**Q48 — C.** $\beta=0.6(25/15)=1.00$.
**Q49 — B.** $\beta=0$ $\Rightarrow$ rendement exigé $=R_f$.
**Q50 — B.** Titre défensif : $\beta<1$.
**Q51 — B.** $0.4(0.8)+0.4(1.2)+0.2(1.6)=1.12$.
**Q52 — B.** Au-dessus de la SML $\Rightarrow$ **sous-évalué** ($\alpha>0$).

**Q53 — C.** $12\%=3\%+1.5\,\mathrm{MRP}\Rightarrow \mathrm{MRP}=6\%$.
**Q54 — C.** Le marché a un bêta de **1**.
**Q55 — C.** Le CAPM rémunère le risque **systématique**.
**Q56 — B.** L'alpha de Jensen = distance verticale à la **SML**.
**Q57 — B.** $\approx\beta\times$ mouvement du marché $=1.5\times10\%=15\%$ (approximation).
**Q58 — B.** Bêta négatif : actif de **couverture**, rendement exigé **sous $R_f$**.
**Q59 — B.** $2\%+0.8(6\%)=6.8\%$.
**Q60 — B.** Exigé $=3\%+1.2(6\%)=10.2\%$ ; prévu $12\%>10.2\%$ $\Rightarrow$ **sous-évalué**, $\alpha=+1.8\%$.
**Q61 — B.** La SML porte le rendement contre **$\beta$**.
**Q62 — B.** $\beta=1.3$ $\Rightarrow$ **agressif** (plus volatil que le marché).
**Q63 — B.** À $\sigma$ égal mais $\beta$ différents, le CAPM exige des rendements **différents** (plus élevé pour $\beta$ plus élevé) : seul $\beta$ compte.
**Q64 — B.** $0.5(1)+0.5(0)=0.5$.
**Q65 — B.** Prime de risque de marché $=E(R_M)-R_f$.
**Q66 — A.** Rendement exigé $=R_f$ $\Rightarrow$ $\beta=0$.

## Bloc F

**Q67 — B.** CML : $\sigma$ (efficients) ; SML : $\beta$ (tout actif).
**Q68 — B.** Pour un portefeuille efficient, $\rho_{P,M}=1$ $\Rightarrow$ $\beta=\sigma_P/\sigma_M$.
**Q69 — A.** $\hat\beta=\operatorname{Cov}(R_i,R_M)/\sigma_M^2$.
**Q70 — B.** $R^2=\rho_{i,M}^2$.
**Q71 — B.** $\sigma_i^2=1.0(0.04)+0.0225=0.0625$ $\Rightarrow$ $\sigma_i=25\%$.
**Q72 — B.** $R^2=0.04/0.0625=0.64$.
**Q73 — B.** Un actif inefficient se situe **sous la CML**.
**Q74 — A.** La *characteristic line* = régression du rendement de l'**actif sur le marché**.
**Q75 — B.** $R^2=1$ $\Rightarrow$ **aucun risque idiosyncratique** (parfaitement corrélé au marché).
**Q76 — B.** Plus de risque idiosyncratique $\Rightarrow$ **$R^2$ plus faible**.

## Bloc G

**Q77 — B.** $(11\%-3\%)/16\%=0.50$.
**Q78 — B.** $(11\%-3\%)/1.0=8\%$.
**Q79 — C.** $14\%-[3\%+1.2(6\%)]=14\%-10.2\%=3.8\%$.
**Q80 — B.** $(12\%-9\%)/5\%=0.60$.
**Q81 — C.** Le **Sharpe** utilise le risque total $\sigma$.
**Q82 — B.** Le **Treynor** utilise le risque systématique $\beta$.
**Q83 — B.** Sortino : **downside deviation** au lieu de $\sigma$ total.
**Q84 — A.** $M^2=R_f+S_P\sigma_M$.
**Q85 — C.** $2\%+0.5(18\%)=11\%$.
**Q86 — C.** Dénominateur de l'IR = **tracking error**.
**Q87 — B.** Même classement Sharpe/Treynor pour des portefeuilles **parfaitement diversifiés**.
**Q88 — B.** Ils divergent quand les portefeuilles diffèrent par leur risque **idiosyncratique**.
**Q89 — B.** $M^2>R_M$ ssi le **Sharpe** du portefeuille dépasse celui du marché.
**Q90 — A.** Ratio d'appréciation $=\alpha/\sigma_\varepsilon$.
**Q91 — B.** Pour une poche d'un fonds diversifié : **Treynor** (seul $\beta$ compte).
**Q92 — B.** $(10\%-2\%)/1.6=5\%$.
**Q93 — B.** L'alpha de Jensen se mesure vs le **rendement exigé par le CAPM**.
**Q94 — B.** Alpha négatif $\Rightarrow$ **sous-performance** du rendement exigé.

## Bloc H

**Q95 — C.** Les agents sont **preneurs de prix** : « pouvoir influencer les prix » n'est **pas** une hypothèse.
**Q96 — B.** Roll : marché **inobservable** $\Rightarrow$ CAPM non réellement testable.
**Q97 — B.** Prime de **valeur** = **HML**.
**Q98 — A.** Prime de **taille** = **SMB**.
**Q99 — B.** L'anomalie *low-beta* $\Rightarrow$ SML empirique **plus plate**.
**Q100 — B.** Carhart ajoute le **momentum** (WML/UMD).
**Q101 — B.** L'APT repose sur l'**absence d'arbitrage** et **n'exige pas** le portefeuille de marché.
**Q102 — B.** Faiblesse de l'APT : elle **ne spécifie pas les facteurs**.
**Q103 — B.** Empiriquement, les hauts $\beta$ rapportent **moins** que prévu.
**Q104 — B.** Le CAPM = APT à **un seul facteur** (le marché).

---

## Note finale

Cet approfondissement complète le chapitre 5 de la synthèse Foundations. Pour la maîtrise « 360° » : (1) sache **refaire les démonstrations** (variance de portefeuille et plancher de diversification, poids du MVP, relation **CML↔SML**, $M^2$, divergence Sharpe/Treynor) **sans les notes** ; (2) **automatise** les calculs (bêta, CAPM, ratios) — c'est la zone la plus rentable de l'examen ; (3) garde en tête les **pièges** (CML = $\sigma$/efficients vs SML = $\beta$/tout actif ; au-dessus de la SML = sous-évalué ; Treynor pour une poche, Sharpe en autonome) ; (4) relie les **études de cas** au cours (LTCM & 2008 $\Rightarrow$ la diversification s'effondre quand $\rho\to1$ ; *low-beta* & primes Fama-French $\Rightarrow$ le CAPM est empiriquement incomplet, d'où l'**APT** du chapitre 6). Les outils de ce chapitre (bêta, frontière, mesures de performance) resurgissent dans le book *Investment Management* de la **Partie II** et au **CFA** : l'avance prise ici paiera plusieurs fois.

*Approfondissement pédagogique original — FRM, Foundations of Risk Management, Chapitre 5. Formules en notation GARP, équations composées en LaTeX.*
