---
title: "FRM Part I · Foundations — Approfondissement du Chapitre 6"
subtitle: 'Arbitrage Pricing Theory & Multifactor Models — concepts, démonstrations, une cinquantaine de cas et environ 150 QCM corrigés'
---

> **Objet.** Approfondissement « 360° » du chapitre 6 : **concepts détaillés**, **démonstrations** complètes (équations en **LaTeX**), une **cinquantaine d'études de cas** (théoriques, empiriques et chiffrées), un **formulaire**, et une banque de **$\approx\!150$ QCM** au format examen (très aérés) avec **corrigé commenté en français** renvoyant aux sections (§). *Document pédagogique original ; les chiffres des cas sont des ordres de grandeur communément cités, à visée illustrative. QCM originaux : le GARP ne publiant pas ses annales, aucune question n'est attribuée à une session passée.*

### Plan

**Partie A — Concepts & démonstrations.** (1) Du CAPM aux modèles multifactoriels — (2) L'APT : hypothèses & processus générateur — (3) Démonstration : la relation de prix par absence d'arbitrage — (4) APT vs CAPM — (5) Les trois familles de facteurs — (6) Modèles empiriques (Fama-French, Carhart, 5 facteurs, q-factor) — (7) Estimation (séries temporelles, coupe transversale, Fama-MacBeth) — (8) Décomposition du risque — (9) *Smart beta* & investissement factoriel — (10) Anomalies, *factor zoo* & critiques — (11) Formulaire.
**Partie B — $\approx\!50$ études de cas** (Chen-Roll-Ross, Fama-French, Carhart, *quant quake* 2007, krachs de momentum, décennie perdue de la *value*, AQR/DFA/BARRA, *betting-against-beta*, cas chiffrés…).
**Partie C — $\approx\!150$ QCM** (format examen, très aérés).
**Partie D — Corrigé commenté** (français, renvois §).

---

# Partie A — Concepts & démonstrations

## 1. Du CAPM aux modèles multifactoriels

Le **CAPM** explique le rendement attendu d'un actif par **un seul** facteur de risque systématique — le **portefeuille de marché** :
$$E(R_i) = R_f + \beta_i\,[\,E(R_m) - R_f\,], \qquad \beta_i = \frac{\operatorname{Cov}(R_i,R_m)}{\sigma_m^2}.$$

**Limites empiriques et théoriques.** (i) Le **portefeuille de marché** est **inobservable** (critique de **Roll** : on ne teste jamais le vrai marché). (ii) Des **anomalies** persistantes (effet **taille**, effet **valeur**, **momentum**, **faible volatilité**) montrent que le seul $\beta$ marché **ne suffit pas** à expliquer la coupe transversale des rendements. (iii) Plusieurs **sources de risque systématique** (taux, inflation, croissance, liquidité…) peuvent être rémunérées simultanément.

D'où l'idée d'un **modèle multifactoriel** : le rendement dépend de **plusieurs** facteurs systématiques $F_1,\dots,F_K$,
$$R_i = E(R_i) + \sum_{k=1}^{K} \beta_{ik}\,F_k + \varepsilon_i,$$
où $\beta_{ik}$ est la **sensibilité** (*factor loading*) de l'actif $i$ au facteur $k$, $F_k$ est la **surprise** du facteur $k$ ($E(F_k)=0$), et $\varepsilon_i$ est le **risque idiosyncratique** ($E(\varepsilon_i)=0$, indépendant des facteurs et **diversifiable**). L'**APT** fournit le cadre théorique — fondé sur l'**absence d'arbitrage** — qui **price** ces facteurs.

## 2. L'APT : hypothèses & processus générateur

L'**Arbitrage Pricing Theory** (Ross, 1976) repose sur trois piliers :

1. Les rendements suivent un **processus générateur linéaire à facteurs** (ci-dessus).
2. Il existe **suffisamment d'actifs** pour **diversifier** le risque idiosyncratique (former des portefeuilles « bien diversifiés »).
3. **Absence d'opportunité d'arbitrage** : pas de gain certain sans mise de fonds ni risque.

**Ce que l'APT n'exige PAS** (vs CAPM) : ni **portefeuille de marché**, ni hypothèse de **distribution normale** des rendements, ni **fonction d'utilité** quadratique, ni que **tous** les investisseurs détiennent le même portefeuille. Ses hypothèses sont donc **plus faibles et plus robustes**. **En contrepartie**, l'APT **ne dit pas** quels sont les facteurs, ni combien — c'est sa **faiblesse pratique** (à résoudre empiriquement, §5-§6).

**Résultat central (relation de prix APT).** En l'absence d'arbitrage, le rendement **attendu** d'un actif est **linéaire** en ses sensibilités :
$$\boxed{\,E(R_i) = R_f + \sum_{k=1}^{K} \beta_{ik}\,\lambda_k\,}$$
où $\lambda_k$ est la **prime de risque du facteur** $k$ (le rendement excédentaire attendu par **unité d'exposition** à $F_k$ ; pour un facteur exprimé comme un portefeuille, $\lambda_k = E(F_k) - R_f$).

## 3. Démonstration — la relation de prix par absence d'arbitrage

**Idée.** Construire un portefeuille **sans coût** (autofinancé) et **sans risque** (ni factoriel, ni idiosyncratique) : par absence d'arbitrage, son rendement attendu doit être **nul**, ce qui **contraint** les rendements attendus à être linéaires en les $\beta$.

**Étape 1 — diversifier l'idiosyncratique.** Pour un portefeuille de poids $w_i$ ($\sum_i w_i = 1$),
$$R_P = \sum_i w_i R_i = \sum_i w_i E(R_i) + \sum_{k} \Big(\underbrace{\sum_i w_i \beta_{ik}}_{\beta_{Pk}}\Big) F_k + \underbrace{\sum_i w_i \varepsilon_i}_{\to\,0}.$$
En répartissant sur $n$ actifs ($w_i \sim 1/n$), la variance idiosyncratique $\sum_i w_i^2 \sigma_{\varepsilon_i}^2 \sim 1/n \to 0$ : le portefeuille **bien diversifié** n'a plus que du **risque factoriel** : $R_P \approx E(R_P) + \sum_k \beta_{Pk} F_k$.

**Étape 2 — portefeuille d'arbitrage.** On choisit des poids $w_i$ tels que :
$$\sum_i w_i = 0 \ (\text{coût nul}), \qquad \sum_i w_i \beta_{ik} = 0 \ \forall k \ (\text{zéro exposition à chaque facteur}).$$
Un tel portefeuille est **sans mise de fonds** et **sans risque** (idiosyncratique diversifié + factoriel annulé). Son rendement est **certain** $= \sum_i w_i E(R_i)$.

**Étape 3 — conclusion.** Absence d'arbitrage $\Rightarrow$ ce rendement certain doit être **nul** :
$$\sum_i w_i = 0,\ \ \sum_i w_i \beta_{ik}=0\ \forall k \;\Longrightarrow\; \sum_i w_i E(R_i) = 0.$$
En algèbre linéaire, le vecteur $E(R_i)$ est **orthogonal** à tout vecteur $w$ orthogonal à $\mathbf{1}$ et aux $\beta_{\cdot k}$. Donc $E(R_i)$ est une **combinaison linéaire** de $\mathbf{1}$ et des $\beta_{\cdot k}$ : il existe des constantes $\lambda_0,\lambda_1,\dots,\lambda_K$ telles que
$$E(R_i) = \lambda_0 + \sum_k \beta_{ik}\,\lambda_k.$$
En prenant un actif **sans risque** ($\beta_{ik}=0\ \forall k$), on identifie $\lambda_0 = R_f$. **CQFD** :
$$E(R_i) = R_f + \sum_k \beta_{ik}\,\lambda_k.$$

**Interprétation de $\lambda_k$.** Un **portefeuille pur** de facteur $k$ (exposition 1 au facteur $k$, 0 aux autres) a un rendement attendu $R_f + \lambda_k$ ; donc $\lambda_k$ est la **prime** rémunérant une unité du risque systématique $k$. Si $F_k$ est lui-même un portefeuille (ex. SMB), $\lambda_k = E(F_k)-R_f$ (ou $E(F_k)$ si le facteur est déjà un *spread* « long-short » autofinancé).

## 4. APT vs CAPM

| | **CAPM** | **APT** |
|---|---|---|
| Facteurs | **1** (le marché) | **$K$** (non spécifiés par la théorie) |
| Fondement | optimisation moyenne-variance, équilibre | **absence d'arbitrage** |
| Hypothèses | marché observable, normalité/utilité quadratique, homogénéité | **plus faibles** (ni marché, ni normalité, ni utilité) |
| Portefeuille de marché | **requis** (critique de Roll) | **non requis** |
| Forme du prix | $E(R_i)=R_f+\beta_i\,\text{MRP}$ | $E(R_i)=R_f+\sum_k\beta_{ik}\lambda_k$ |
| Faiblesse | hypothèses fortes, marché inobservable | **n'identifie pas** les facteurs |

**Lien.** Le **CAPM est un cas particulier de l'APT** à **un seul facteur** (le marché). Inversement, l'APT **généralise** le CAPM en autorisant **plusieurs** sources de risque rémunérées. *Smart beta* et investissement factoriel (§9) sont les applications industrielles directes de l'APT.

## 5. Les trois familles de facteurs

L'APT ne dit pas quels facteurs utiliser ; la pratique en distingue **trois familles** :

- **Facteurs macroéconomiques** : variables économiques **observables** dont les **surprises** meuvent les rendements — croissance du **PIB / production industrielle**, **inflation** (surprise), **spreads de crédit** (défaut), **pente de la courbe des taux**, **prix du pétrole**, **change**. *Modèle canonique* : **Chen, Roll & Ross (1986)**.
- **Facteurs fondamentaux / caractéristiques** : construits à partir d'**attributs d'entreprise** — **taille** (capitalisation), **valeur** (*book-to-market*, PER), **momentum**, **rentabilité**, **investissement**, **levier**, **bêta**, **volatilité**. Les facteurs sont des **portefeuilles long-short** triés sur ces attributs (ex. SMB, HML). *Modèles* : **Fama-French**, **Carhart**, modèles de risque **BARRA / Axioma**.
- **Facteurs statistiques** : **extraits des données** par **analyse en composantes principales (ACP/PCA)** ou **analyse factorielle**, **sans interprétation économique a priori**. Avantage : capturent la structure de covariance « optimale » ; inconvénient : facteurs **difficiles à nommer** et **instables**.

## 6. Modèles empiriques (Fama-French, Carhart, 5 facteurs)

**Fama-French à 3 facteurs (1993)** — régression de série temporelle des **rendements excédentaires** :
$$R_i - R_f = \alpha_i + \beta_i^{\text{MKT}}(R_m - R_f) + \beta_i^{\text{SMB}}\,\text{SMB} + \beta_i^{\text{HML}}\,\text{HML} + \varepsilon_i,$$
avec **SMB** (*Small Minus Big*, prime de **taille**) et **HML** (*High Minus Low* *book-to-market*, prime de **valeur**). Un **$\alpha_i$** significativement non nul signale une **performance anormale** (sur/sous-évaluation) **après** contrôle des trois facteurs.

**Carhart à 4 facteurs (1997)** : ajoute **WML** (*Winners Minus Losers*, aussi **UMD**), la prime de **momentum** :
$$R_i - R_f = \alpha_i + \beta^{\text{MKT}}(R_m-R_f) + \beta^{\text{SMB}}\text{SMB} + \beta^{\text{HML}}\text{HML} + \beta^{\text{WML}}\text{WML} + \varepsilon_i.$$

**Fama-French à 5 facteurs (2015)** : ajoute **RMW** (*Robust Minus Weak*, **rentabilité**) et **CMA** (*Conservative Minus Aggressive*, **investissement**). Le **q-factor model** de **Hou-Xue-Zhang (2015)** propose une alternative (marché, taille, investissement, rentabilité) issue de la théorie de l'investissement.

**Lecture des *loadings*.** Un fort $\beta^{\text{HML}}$ $\Rightarrow$ comportement de ***value*** ; un $\beta^{\text{HML}}$ négatif $\Rightarrow$ *growth*. Un fort $\beta^{\text{SMB}}$ $\Rightarrow$ biais **petites capitalisations**.

## 7. Estimation : séries temporelles, coupe transversale, Fama-MacBeth

- **Régression de série temporelle** (facteurs = portefeuilles, ex. FF) : pour chaque actif, on régresse $R_i-R_f$ sur les facteurs $\Rightarrow$ **bêtas** et **$\alpha$**.
- **Régression de coupe transversale** (facteurs = macro) : on estime d'abord les **bêtas** (séries temporelles), puis on régresse les **rendements moyens** des actifs sur leurs bêtas $\Rightarrow$ **primes $\lambda_k$**.
- **Procédure de Fama-MacBeth (1973)** — deux passes : (1) **bêtas** par série temporelle ; (2) à **chaque période** $t$, **régression en coupe** des rendements sur les bêtas $\Rightarrow$ une série de $\hat\lambda_{k,t}$ ; la **prime estimée** est la **moyenne** $\bar\lambda_k = \frac1T\sum_t \hat\lambda_{k,t}$, avec une statistique $t$ fondée sur la **variabilité temporelle** des $\hat\lambda_{k,t}$. Avantage : gère la **corrélation transversale** des résidus.

## 8. Décomposition du risque — systématique vs idiosyncratique

Avec un modèle à facteurs, la variance d'un actif se **décompose** :
$$\sigma_i^2 = \underbrace{\boldsymbol\beta_i^{\top}\,\Omega_F\,\boldsymbol\beta_i}_{\text{systématique}} + \underbrace{\sigma_{\varepsilon_i}^2}_{\text{idiosyncratique}},$$
où $\boldsymbol\beta_i$ est le vecteur des sensibilités et $\Omega_F$ la **matrice de covariance des facteurs**. **Cas à un facteur** : $\sigma_i^2 = \beta_i^2 \sigma_F^2 + \sigma_{\varepsilon_i}^2$, et le **$R^2$** de la régression $= \beta_i^2\sigma_F^2/\sigma_i^2$ mesure la **part systématique**.

**Pour un portefeuille** : $\boldsymbol\beta_P = \sum_i w_i \boldsymbol\beta_i$ et
$$\sigma_P^2 = \boldsymbol\beta_P^{\top}\,\Omega_F\,\boldsymbol\beta_P + \sum_i w_i^2 \sigma_{\varepsilon_i}^2.$$
Le **deuxième terme** $\to 0$ quand le nombre de positions augmente (diversification) : un grand portefeuille n'est exposé qu'au **risque factoriel**. C'est la base des **modèles de risque** (BARRA/Axioma) : estimer $\Omega_F$ et les bêtas plutôt qu'une énorme matrice de covariance $N\times N$ des titres.

## 9. *Smart beta* & investissement factoriel

L'**investissement factoriel** met l'APT en pratique : au lieu de viser le seul $\beta$ marché, on **s'expose délibérément** à des facteurs **rémunérés** (value, momentum, quality, low-vol, size). Le ***smart beta*** désigne des indices/ETF **règles** capturant ces primes à **faible coût**. Enjeux : **construction** (long-only vs long-short, pondération), **coûts de transaction** et **capacité**, **timing factoriel** (controversé : la value peut sous-performer une décennie), **chevauchement** et **interaction** des facteurs, **encombrement** (*crowding*) qui peut éroder les primes.

## 10. Anomalies, *factor zoo* & critiques

- **Anomalies** : taille, valeur, momentum, **faible volatilité** (les titres peu volatils sur-performent — paradoxe vs CAPM), **qualité/rentabilité**, ***betting-against-beta*** (Frazzini-Pedersen : les titres à faible $\beta$ ont un meilleur rendement ajusté du risque).
- **Risque vs comportement** : une prime peut rémunérer un **risque** (explication rationnelle) **ou** refléter un **biais comportemental** / des **frictions** (sous-réaction, contraintes de levier) — débat non tranché.
- ***Factor zoo*** : des **centaines** de facteurs « découverts » dans la littérature. **Harvey, Liu & Zhu (2016)** alertent sur le ***data mining*** : avec tant de tests, beaucoup de facteurs sont des **faux positifs**. D'où l'exigence de **seuils de significativité plus élevés**, de **réplication hors échantillon** et de **fondement économique**.
- **Limite « à la Roll »** : comme le portefeuille de marché du CAPM, le **choix des facteurs** de l'APT est en partie **arbitraire** ; un modèle peut « marcher » en échantillon par construction.

## 11. Formulaire (notation GARP)

$$\text{CAPM}:\ E(R_i)=R_f+\beta_i[E(R_m)-R_f],\quad \beta_i=\frac{\operatorname{Cov}(R_i,R_m)}{\sigma_m^2}$$
$$\text{Processus}:\ R_i=E(R_i)+\sum_k \beta_{ik}F_k+\varepsilon_i,\quad E(F_k)=E(\varepsilon_i)=0$$
$$\text{Prix APT}:\ E(R_i)=R_f+\sum_k \beta_{ik}\lambda_k,\qquad \lambda_k=\text{prime du facteur }k$$
$$\text{Fama-French 3F}:\ R_i-R_f=\alpha_i+\beta^{\text{MKT}}(R_m-R_f)+\beta^{\text{SMB}}\text{SMB}+\beta^{\text{HML}}\text{HML}+\varepsilon_i$$
$$\text{Risque}:\ \sigma_i^2=\boldsymbol\beta_i^{\top}\Omega_F\boldsymbol\beta_i+\sigma_{\varepsilon_i}^2,\qquad R^2=\frac{\boldsymbol\beta_i^{\top}\Omega_F\boldsymbol\beta_i}{\sigma_i^2}$$
$$\text{Portefeuille}:\ \boldsymbol\beta_P=\sum_i w_i\boldsymbol\beta_i,\qquad \sigma_P^2=\boldsymbol\beta_P^{\top}\Omega_F\boldsymbol\beta_P+\sum_i w_i^2\sigma_{\varepsilon_i}^2$$
$$\text{Fama-MacBeth}:\ \bar\lambda_k=\frac1T\sum_{t=1}^{T}\hat\lambda_{k,t}$$

**À retenir** : l'APT **price plusieurs facteurs** par **absence d'arbitrage**, **sans** portefeuille de marché ni normalité ; sa **faiblesse** est qu'elle **n'identifie pas** les facteurs ; le **CAPM** en est le **cas à un facteur**.

---

# Partie B — $\approx\!50$ études de cas

> Chaque cas relie une **idée**, un **épisode** ou un **calcul** à son **mécanisme** et à sa **leçon**. Les cas chiffrés (40–50) appliquent les démonstrations.

### Cas 1 — Ross (1976) : la naissance de l'APT
**Idée.** Stephen Ross dérive une relation de prix **multifactorielle** à partir de la **seule** absence d'arbitrage, sans portefeuille de marché ni hypothèse de normalité. **Leçon.** L'APT offre un cadre **plus général et plus robuste** que le CAPM, au prix de **ne pas spécifier** les facteurs.

### Cas 2 — Chen, Roll & Ross (1986) : les facteurs macro sont rémunérés
**Faits.** CRR testent des **variables macroéconomiques** et trouvent que la **production industrielle**, les **surprises d'inflation**, le **spread de défaut** et la **pente des taux** sont **prisés** dans la coupe transversale des actions. **Leçon.** Premier modèle **macro** convaincant : plusieurs **risques systématiques observables** portent une prime — illustration empirique directe de l'APT.

### Cas 3 — Banz (1981) : l'effet taille
**Faits.** Les **petites capitalisations** affichent des rendements ajustés du risque supérieurs à ce que prédit le CAPM. **Leçon.** Première **anomalie** majeure ; germe du facteur **SMB**.

### Cas 4 — Fama & French (1992) : la taille et la valeur battent le bêta
**Faits.** Sur 1963-1990, la **capitalisation** et le ***book-to-market*** expliquent la coupe transversale **mieux** que le $\beta$ marché — dont le pouvoir explicatif s'effondre une fois ces variables contrôlées. **Leçon.** Choc pour le CAPM : « **beta is dead** » ; il faut **plusieurs** facteurs.

### Cas 5 — Fama & French (1993) : le modèle à 3 facteurs
**Faits.** FF formalisent **MKT + SMB + HML**, qui devient le **standard** de la recherche et de l'attribution de performance. **Leçon.** Opérationnalise l'APT avec des **facteurs-portefeuilles** ; le $\alpha$ de cette régression devient la mesure de **vraie** surperformance.

### Cas 6 — Jegadeesh & Titman (1993) : le momentum
**Faits.** Acheter les **gagnants** récents (3-12 mois) et vendre les **perdants** génère un rendement **persistant** — difficile à expliquer par le risque. **Leçon.** Anomalie **robuste** dans le monde entier ; base du facteur **WML/UMD**, souvent **comportemental** (sous-réaction).

### Cas 7 — Carhart (1997) : 4 facteurs et la (non-)persistance des fonds
**Faits.** En ajoutant le **momentum**, Carhart montre que la **performance des fonds** mutuels s'explique largement par les **facteurs** et les **frais** — peu de vraie compétence persistante. **Leçon.** Le modèle à **4 facteurs** est l'outil d'**attribution** de référence ; la « surperformance » est souvent du **bêta factoriel** déguisé en alpha.

### Cas 8 — Fama & French (2015) : le modèle à 5 facteurs
**Faits.** Ajout de **RMW** (rentabilité) et **CMA** (investissement) ; HML devient parfois **redondant**. **Leçon.** Les facteurs **qualité** (rentabilité, investissement prudent) capturent une part du rendement — extension naturelle, mais débat sur le **bon** jeu de facteurs.

### Cas 9 — Hou, Xue & Zhang (2015) : le q-factor model
**Faits.** Modèle alternatif (**marché, taille, investissement, rentabilité**) dérivé de la **théorie q de l'investissement**. **Leçon.** Plusieurs modèles **concurrents** « expliquent » les mêmes anomalies : signe que le **choix des facteurs** reste ouvert (limite « à la Roll »).

### Cas 10 — Frazzini & Pedersen (2014) : *betting against beta*
**Faits.** Les actifs à **faible $\beta$** offrent un **meilleur rendement ajusté du risque** que les actifs à fort $\beta$ ; une stratégie **BAB** (long faible-bêta à levier, short fort-bêta) est rentable. **Leçon.** Les **contraintes de levier** des investisseurs déforment la SML — anomalie **rationnelle** (frictions) plutôt que pur biais.

### Cas 11 — L'anomalie de faible volatilité
**Faits.** Haugen-Baker puis Ang, Hodrick, Xing & Zhang (2006) : les titres **peu volatils** sur-performent, contredisant l'intuition risque-rendement. **Leçon.** Facteur ***low-vol*** populaire en *smart beta* ; explication mêlant **contraintes de levier**, **préférence pour la loterie** et **mandats benchmarkés**.

### Cas 12 — Novy-Marx (2013) : la rentabilité brute
**Faits.** La **rentabilité brute** (*gross profitability*) prédit les rendements **au-delà** de la value. **Leçon.** Combinée à la value, elle améliore nettement le portefeuille — fondement du facteur **qualité/RMW**.

### Cas 13 — Asness : *Quality Minus Junk* (QMJ)
**Faits.** AQR construit un facteur **qualité** (rentabilité, croissance, sécurité, *payout*) : les entreprises de **qualité** sont **sous-évaluées** relativement à leur mérite. **Leçon.** La **qualité** est une dimension factorielle robuste, complémentaire de value et momentum.

### Cas 14 — Harvey, Liu & Zhu (2016) : « …and the cross-section of expected returns »
**Faits.** Recension de **plus de 300 facteurs** publiés ; avec autant de tests, beaucoup sont des **faux positifs**. **Leçon.** Exiger des **seuils $t$ plus élevés** (≈ 3 plutôt que 2), de la **réplication** et un **fondement économique** — antidote au ***data mining***.

### Cas 15 — Cochrane (2011) : le « *factor zoo* »
**Faits.** Dans son adresse présidentielle, John Cochrane qualifie de **« zoo de facteurs »** la prolifération des facteurs et appelle à **discipliner** le champ. **Leçon.** Question centrale : **lesquels** des facteurs sont **indépendants** et **réellement** rémunérés ?

### Cas 16 — McLean & Pontiff (2016) : les anomalies s'érodent après publication
**Faits.** En moyenne, le rendement d'une anomalie **chute** (~−30 % à −60 %) **après** sa publication académique. **Leçon.** L'**arbitrage** et le ***crowding*** **érodent** les primes : une stratégie largement connue rapporte **moins** — argument clé contre le *smart beta* naïf.

### Cas 17 — BARRA : le premier modèle de risque fondamental
**Faits.** Barr Rosenberg commercialise dès les années 1970 un **modèle de risque multifactoriel** (facteurs sectoriels et de style) — ancêtre des modèles **MSCI BARRA / Axioma**. **Leçon.** Estimer $\Omega_F$ et des **bêtas de style** est **bien plus parcimonieux** qu'une matrice $N\times N$ — l'APT au service de la **gestion du risque** et de l'**attribution**.

### Cas 18 — MSCI / Axioma : l'industrialisation des modèles de risque
**Faits.** **MSCI BARRA** et **Axioma** fournissent aux gérants des **modèles de risque factoriels** (style, secteur, pays, devise) pour mesurer l'**exposition**, le **risque actif** et la ***tracking error***. **Leçon.** L'APT structure la **gestion du risque** institutionnelle : on **décompose** le risque par facteur plutôt que titre par titre.

### Cas 19 — Dimensional (DFA) : la recherche transformée en fonds
**Faits.** Fondée avec l'appui de Fama et French, **DFA** construit des fonds **systématiques** inclinés vers **taille**, **value** et **rentabilité**. **Leçon.** Premier pont à grande échelle entre **académie** et **industrie** ; l'investissement factoriel **avant la lettre**.

### Cas 20 — AQR : « *Value and Momentum Everywhere* »
**Faits.** AQR (Asness et al.) montre que **value** et **momentum** sont rémunérés **dans toutes les classes d'actifs** (actions, obligations, devises, matières premières) et que leur **combinaison** (corrélation négative) est puissante. **Leçon.** Les primes factorielles sont **universelles** et se **diversifient** entre elles — argument fort pour le multifactoriel.

### Cas 21 — Research Affiliates / Arnott : l'indexation fondamentale (RAFI)
**Faits.** Rob Arnott propose de pondérer les indices par des **mesures fondamentales** (ventes, *book*, dividendes) plutôt que par la **capitalisation** — capturant implicitement une prime de **value**. **Leçon.** Une voie alternative au *smart beta* ; débat sur la part de **vraie** valeur ajoutée vs simple **tilt value**.

### Cas 22 — Le boom des ETF *smart beta*
**Faits.** Dans les années 2010, les **ETF factoriels / smart beta** (iShares, Vanguard, Invesco) explosent, démocratisant l'accès aux primes value/momentum/quality/low-vol à **faible coût**. **Leçon.** L'APT devient un **produit grand public** — mais le succès même nourrit le risque d'**encombrement** (cf. Cas 16, 34).

### Cas 23 — MSCI Factor Indexes : la standardisation
**Faits.** MSCI publie des **indices factoriels** officiels (Value, Momentum, Quality, Minimum Volatility, Size) servant de **benchmarks** aux ETF. **Leçon.** La **standardisation** rend les facteurs **investissables** et **comparables**, mais **fige** des définitions parfois discutables.

### Cas 24 — Minimum Volatility : le low-vol en portefeuille
**Faits.** Les stratégies **minimum variance / min-vol** (MSCI Min Vol, iShares USMV) cherchent un risque **plus faible** à rendement comparable. **Leçon.** Exploite l'anomalie low-vol (Cas 11) ; performance dépendante du **régime** et sensible aux **taux** (biais vers les titres « bond-like »).

### Cas 25 — Le « quant quake » d'août 2007
**Faits.** En quelques jours (6-9 août 2007), de nombreux **fonds quantitatifs** subissent des pertes brutales et **simultanées** : un grand acteur **déleverage** des positions factorielles très **populaires**, déclenchant une **liquidation en chaîne** des mêmes paris, puis un **rebond** rapide. **Leçon.** Le ***crowding*** factoriel crée un **risque de liquidité endogène** invisible des modèles : quand tout le monde détient les mêmes facteurs à levier, la sortie est **corrélée**.

### Cas 26 — Le krach de momentum de 2009
**Faits.** Au **rebond** de mars-août 2009, les **perdants** (titres massacrés en 2008) **explosent** à la hausse : la stratégie momentum (short perdants) subit une **perte historique**. **Leçon.** Le momentum a une **asymétrie** (***momentum crashes***) : il s'effondre lors des **retournements violents** après un marché baissier — un risque de **queue** propre au facteur.

### Cas 27 — Daniel & Moskowitz : l'anatomie des krachs de momentum
**Faits.** La recherche montre que les krachs de momentum surviennent en marchés **stressés et volatils**, lors de **rebonds**, et peuvent être **atténués** par un momentum **ajusté de la volatilité**. **Leçon.** Comprendre la **dynamique conditionnelle** d'un facteur permet de **gérer** son risque de queue.

### Cas 28 — La « décennie perdue » de la *value* (2010s)
**Faits.** Après des décennies de prime positive, la **value sous-performe** la *growth* pendant la majeure partie des années 2010. **Leçon.** Une prime factorielle peut **disparaître très longtemps** : le ***timing factoriel*** est tentant mais dangereux ; remise en cause de la **robustesse** de HML.

### Cas 29 — Le drawdown record de la value (2018-2020)
**Faits.** La value connaît son **pire drawdown** historique, culminant lors du choc **COVID** de 2020. **Leçon.** Débat intense : la value est-elle **cassée** (changement structurel, immatériel mal mesuré) ou simplement **bon marché** et prête à rebondir ?

### Cas 30 — Le grand retournement value (nov. 2020 – 2021)
**Faits.** À l'annonce des **vaccins** (9 nov. 2020), la value **rebondit** violemment contre la growth/momentum. **Leçon.** Les primes peuvent **revenir brutalement** : sortir d'un facteur après une longue sous-performance peut faire **rater** le redressement (cf. débat Cas 35).

### Cas 31 — COVID, mars 2020 : le comportement des facteurs
**Faits.** Dans le krach, **momentum**, **quality** et **low-vol** résistent mieux ; **value** et **size** souffrent. **Leçon.** Les facteurs ont des **profils défensifs/cycliques** distincts : un portefeuille **multifactoriel** diversifie ces comportements.

### Cas 32 — *Volmageddon* (févr. 2018) et le low-vol
**Faits.** L'implosion des produits **short-VIX** secoue les stratégies sensibles à la **volatilité**. **Leçon.** Les facteurs « calmes » (low-vol) peuvent cacher une **exposition au régime de volatilité** ; attention aux **risques cachés** d'un tilt factoriel.

### Cas 33 — Arnott : « *How Can Smart Beta Go Horribly Wrong?* »
**Faits.** Arnott et al. avertissent qu'une partie de la performance *smart beta* vient d'un **renchérissement** des facteurs (revalorisation) **non répétable**, et que le ***crowding*** peut provoquer des **retournements**. **Leçon.** Distinguer la prime **structurelle** d'un facteur de sa **revalorisation** conjoncturelle — sous peine d'acheter au plus haut.

### Cas 34 — Asness vs Arnott : le débat du *factor timing*
**Faits.** Asness (AQR) défend la **diversification factorielle constante** ; Arnott plaide pour un **timing** fondé sur la **valorisation** des facteurs. **Leçon.** Le débat structurant de l'investissement factoriel : **timing** (potentiellement rémunérateur mais risqué) vs **discipline** (exposition stable, plus robuste).

### Cas 35 — *Meme stocks* (2021) : le facteur perturbé
**Faits.** Le *short squeeze* sur **GameStop** et d'autres titres très **shortés** inflige des pertes aux stratégies exposées au **court** (dont certains facteurs value/quality short des titres « pourris »). **Leçon.** Des **flux non fondamentaux** (coordination de particuliers) peuvent **briser temporairement** les relations factorielles — risque de **crowding** côté short.

### Cas 36 — 2022 : le retour de la value avec les taux
**Faits.** La **remontée des taux** en 2022 pénalise la *growth*/tech à longue duration et **favorise la value** ; momentum et low-vol défensifs surperforment. **Leçon.** Les facteurs ont une **sensibilité aux taux et au régime macro** : la value se comporte comme un actif de **duration courte** relative.

### Cas 37 — Le CAPM comme APT à un facteur (conceptuel)
**Idée.** Poser $K=1$ avec le **marché** comme unique facteur réduit l'APT à $E(R_i)=R_f+\beta_i\lambda_{\text{MKT}}$, soit le **CAPM**. **Leçon.** Les deux modèles ne s'opposent pas : le **CAPM est un cas particulier** ; l'APT **emboîte** et **généralise**.

### Cas 38 — La critique de Roll appliquée au choix des facteurs
**Idée.** De même que le « vrai » marché du CAPM est inobservable, le **bon** jeu de facteurs de l'APT est **indéterminé** : on peut toujours **ajuster** un modèle en échantillon. **Leçon.** Exiger **parcimonie**, **fondement économique** et **validation hors échantillon** ; se méfier des modèles « parfaits » a posteriori.

### Cas 39 — Allocation d'un fonds de pension par facteurs macro
**Application.** Un fonds décompose ses actifs par exposition à **croissance**, **inflation**, **taux réels** et **crédit**, puis **équilibre** ces expositions plutôt que par classes d'actifs comptables. **Leçon.** Le ***risk factor investing*** (approche « *risk parity* factorielle ») révèle des **concentrations cachées** (ex. actions et crédit partagent le facteur croissance).

### Cas 40 — Attribution de performance avec Carhart
**Application.** On régresse les rendements d'un fonds sur **MKT/SMB/HML/WML** : les **bêtas** disent **d'où** vient le rendement (tilts), l'**$\alpha$** mesure la valeur ajoutée **résiduelle**. **Leçon.** Beaucoup d'« alpha » se révèle être du **bêta factoriel** rémunéré — distinction cruciale pour juger un gérant et fixer ses frais.

### Cas 41 — Chiffré : $E(R)$ sous APT à 2 facteurs
**Données.** $R_f=3\%$ ; facteur croissance $\lambda_1=5\%$, $\beta_1=1{,}0$ ; facteur spread de crédit $\lambda_2=2\%$, $\beta_2=0{,}8$.
$$E(R)=3\%+1{,}0(5\%)+0{,}8(2\%)=3+5+1{,}6=9{,}6\%.$$
**Leçon.** Le rendement attendu **s'additionne** sur les expositions factorielles pondérées par leurs primes.

### Cas 42 — Chiffré : $E(R)$ sous APT à 3 facteurs (avec *loading* négatif)
**Données.** $R_f=3\%$ ; MKT $\lambda=6\%$, $\beta=1{,}1$ ; SMB $\lambda=2\%$, $\beta=0{,}7$ ; HML $\lambda=3\%$, $\beta=-0{,}5$ (titre de *growth*).
$$E(R)=3\%+1{,}1(6\%)+0{,}7(2\%)-0{,}5(3\%)=3+6{,}6+1{,}4-1{,}5=9{,}5\%.$$
**Leçon.** Un ***loading*** **négatif** (ici sur la value) **réduit** le rendement attendu : le titre « paie » son biais *growth*.

### Cas 43 — Chiffré : détecter un arbitrage (mêmes bêtas, rendements différents)
**Données.** Deux portefeuilles **bien diversifiés**, $\beta=1{,}0$ au même facteur unique : $E(R_A)=12\%$, $E(R_B)=10\%$.
**Arbitrage.** Même risque, rendement différent $\Rightarrow$ **acheter A**, **vendre B** (coût nul, bêta net 0) : gain **certain** $\approx 12\%-10\%=2\%$. **Leçon.** L'APT **interdit** deux expositions identiques à prix différents : l'arbitrage **force** l'alignement.

### Cas 44 — Chiffré : portefeuille de réplication et tranche mal évaluée
**Données.** $R_f=4\%$ ; portefeuilles purs : $P_1(\beta_1=1,\beta_2=0)$ avec $E=10\%\Rightarrow\lambda_1=6\%$ ; $P_2(\beta_1=0,\beta_2=1)$ avec $E=7\%\Rightarrow\lambda_2=3\%$. Un portefeuille $Q$ a $\beta_1=0{,}5$, $\beta_2=0{,}5$.
$$E(R_Q)^{\text{juste}}=4\%+0{,}5(6\%)+0{,}5(3\%)=8{,}5\%.$$
Si $Q$ se traite à $E=10\%$ ($\alpha=+1{,}5\%$) : **acheter $Q$**, **vendre** $0{,}5\,P_1+0{,}5\,P_2$ (mêmes bêtas) $\Rightarrow$ profit **sans risque** $\approx 1{,}5\%$. **Leçon.** On **réplique** les bêtas avec des portefeuilles-facteurs et on **capte** l'écart de prix.

### Cas 45 — Chiffré : décomposition systématique / idiosyncratique
**Données.** Modèle à un facteur : $\beta=1{,}3$, $\sigma_F=18\%$, $\sigma_\varepsilon=22\%$.
$$\sigma_{\text{sys}}^2=\beta^2\sigma_F^2=1{,}3^2(0{,}18)^2=0{,}0548,\quad \sigma_\varepsilon^2=0{,}0484,\quad \sigma^2=0{,}1032\Rightarrow\sigma=32{,}1\%.$$
$$R^2=\frac{0{,}0548}{0{,}1032}=53{,}1\%.$$
**Leçon.** Ici **53 %** du risque est **systématique** (factoriel), **47 %** idiosyncratique (diversifiable).

### Cas 46 — Chiffré : bêtas et risque d'un portefeuille
**Données.** Poids $0{,}40/0{,}35/0{,}25$ ; bêtas marché $1{,}2/0{,}9/1{,}4$.
$$\beta_P=0{,}40(1{,}2)+0{,}35(0{,}9)+0{,}25(1{,}4)=0{,}48+0{,}315+0{,}35=1{,}145.$$
**Leçon.** Le **bêta de portefeuille** est la **moyenne pondérée** des bêtas ; idem pour chaque facteur, ce qui pilote l'exposition factorielle d'ensemble.

### Cas 47 — Chiffré : lire une régression Fama-French
**Données.** Pour un fonds : $\hat\alpha=0{,}1\%/\text{mois}$ (non significatif), $\beta^{\text{MKT}}=1{,}0$, $\beta^{\text{SMB}}=+0{,}6$, $\beta^{\text{HML}}=-0{,}4$.
**Lecture.** Fonds **petites capitalisations** ($SMB>0$) à biais ***growth*** ($HML<0$), de risque marché « normal », **sans alpha** après facteurs. **Leçon.** Les ***loadings*** **décrivent le style** ; l'$\alpha$ non significatif dit que la performance vient des **facteurs**, pas du talent.

### Cas 48 — Chiffré : prime estimée par Fama-MacBeth
**Données.** Estimations de coupe transversale de $\lambda_{\text{HML}}$ sur 4 mois : $+0{,}4\%,\ -0{,}2\%,\ +0{,}6\%,\ 0{,}0\%$.
$$\bar\lambda_{\text{HML}}=\tfrac14(0{,}4-0{,}2+0{,}6+0{,}0)=+0{,}2\%/\text{mois}\ (\approx 2{,}4\%/\text{an}).$$
**Leçon.** La prime est la **moyenne** des primes périodiques ; sa **significativité** vient de la **stabilité** temporelle des $\hat\lambda_t$.

### Cas 49 — Chiffré : ratio d'information d'une stratégie factorielle
**Données.** Rendement actif $3\%/\text{an}$, ***tracking error*** $6\%$.
$$\mathrm{IR}=\frac{3\%}{6\%}=0{,}50.$$
**Leçon.** Un $\mathrm{IR}=0{,}5$ est **honorable** pour une prime factorielle ; il mesure la **qualité** (rendement actif par unité de risque actif), pas seulement l'ampleur.

### Cas 50 — Chiffré : l'ampleur d'un krach de momentum
**Données.** Lors du rebond de **2009**, le facteur **WML** chute d'environ **$-70\%$** de son pic en quelques mois (les perdants rebondissant le plus fort).
**Leçon.** Le momentum porte un **risque de queue** asymétrique : son rendement moyen élevé est la **compensation** de pertes **rares mais violentes** lors des retournements — à **couvrir** (ex. momentum ajusté de la volatilité, Cas 27).

---

# Partie C — Banque de $\approx\!150$ QCM (format examen)

> QCM en anglais, **une seule bonne réponse**, présentation **aérée**. **Corrigé commenté en français** en **Partie D**, avec renvois aux sections (§) et aux cas. *Questions originales au format examen ; le GARP ne publie pas ses annales.*

## C.1 — Du CAPM aux modèles multifactoriels

**Q1.** The CAPM explains expected returns using how many systematic factors?

(A) zero

(B) one (the market)

(C) three

(D) five

\medskip

**Q2.** Roll's critique observes that the CAPM is hard to test because:

(A) betas are unstable

(B) returns are normal

(C) the true market portfolio is unobservable

(D) there are too many factors

\medskip

**Q3.** The size effect (Banz) is that:

(A) large caps outperform on a risk-adjusted basis

(B) small caps outperform the CAPM prediction

(C) size is irrelevant

(D) size equals beta

\medskip

**Q4.** The value effect is that:

(A) low book-to-market (growth) stocks outperform

(B) high book-to-market (value) stocks outperform on average

(C) value equals momentum

(D) value is the market factor

\medskip

**Q5.** The momentum effect is that:

(A) past losers outperform

(B) returns mean-revert immediately

(C) recent winners tend to keep outperforming

(D) there is no pattern

\medskip

**Q6.** The low-volatility anomaly is that:

(A) high-volatility stocks always win

(B) low-volatility stocks deliver better risk-adjusted returns than the CAPM predicts

(C) volatility is unpriced

(D) beta fully explains returns

\medskip

**Q7.** A multifactor return-generating process is written:

(A) $R_i = R_f + \beta_i(R_m - R_f)$

(B) $R_i = \alpha_i + \varepsilon_i$

(C) $R_i = E(R_i) + \sum_k \beta_{ik}F_k + \varepsilon_i$

(D) $R_i = \sum_k F_k$

\medskip

**Q8.** In that process, the term $\varepsilon_i$ is:

(A) a systematic factor

(B) the market premium

(C) idiosyncratic, diversifiable risk

(D) the risk-free rate

\medskip

**Q9.** In that process, each factor surprise $F_k$ has an expectation of:

(A) $R_f$

(B) 0

(C) $\lambda_k$

(D) 1

\medskip

**Q10.** The factor loading $\beta_{ik}$ measures:

(A) the risk-free rate

(B) the factor premium

(C) the sensitivity of asset $i$ to factor $k$

(D) idiosyncratic variance

\medskip

**Q11.** Idiosyncratic risk in a multifactor model is:

(A) priced

(B) diversified away in large portfolios

(C) the only risk

(D) equal to beta

\medskip

**Q12.** The motivation for multifactor models includes:

(A) only that the market is unobservable

(B) only the size effect

(C) only data mining

(D) multiple priced systematic risks and persistent CAPM anomalies

\medskip

**Q13.** "Beta is dead" is associated with:

(A) Ross (1976)

(B) Fama-French (1992), where size and value dominate beta

(C) Markowitz (1952)

(D) Sharpe (1964)

\medskip

**Q14.** A factor portfolio such as SMB is:

(A) a single stock

(B) the risk-free asset

(C) a long-short portfolio sorted on a characteristic

(D) the market index

\medskip

**Q15.** The first commercial fundamental risk model is associated with:

(A) Vanguard

(B) BARRA (Rosenberg)

(C) the CAPM

(D) the Fed

\medskip

## C.2 — L'APT : modèle & hypothèses

**Q16.** The Arbitrage Pricing Theory was developed by:

(A) Markowitz

(B) Ross (1976)

(C) Sharpe

(D) Black-Scholes

\medskip

**Q17.** The APT rests primarily on:

(A) the market portfolio

(B) mean-variance optimization

(C) the absence of arbitrage

(D) normally distributed returns

\medskip

**Q18.** Unlike the CAPM, the APT does NOT require:

(A) a risk-free rate

(B) the (unobservable) market portfolio

(C) any factors at all

(D) positive betas

\medskip

**Q19.** The APT does NOT require:

(A) the risk-free rate

(B) any betas

(C) normally distributed returns or quadratic utility

(D) at least one factor

\medskip

**Q20.** The main practical weakness of the APT is that it:

(A) needs the risk-free rate

(B) allows only one factor

(C) does not specify which or how many factors drive returns

(D) assumes homogeneous expectations

\medskip

**Q21.** The APT pricing relation is:

(A) $E(R_i)=\beta_i$

(B) $E(R_i)=R_f$

(C) $E(R_i)=\sum_k F_k$

(D) $E(R_i)=R_f+\sum_k \beta_{ik}\lambda_k$

\medskip

**Q22.** In the APT, $\lambda_k$ represents:

(A) the risk-free rate

(B) the risk premium per unit of exposure to factor $k$

(C) the factor loading

(D) idiosyncratic risk

\medskip

**Q23.** When factor $k$ is expressed as a tradable portfolio, its premium $\lambda_k$ equals:

(A) $R_f$

(B) $\beta_{ik}$

(C) $E(F_k) - R_f$

(D) zero

\medskip

**Q24.** A "well-diversified" portfolio is one whose:

(A) factor exposures are zero

(B) idiosyncratic risk is negligible

(C) beta to the market is one

(D) return equals the risk-free rate

\medskip

**Q25.** The APT allows:

(A) only one priced factor

(B) only idiosyncratic risk

(C) multiple priced systematic factors

(D) no risk premia

\medskip

**Q26.** The number of factors in the APT is:

(A) exactly one

(B) exactly three

(C) not specified by the theory

(D) always five

\medskip

**Q27.** In the APT derivation, an arbitrage portfolio is constructed to have:

(A) maximum factor exposure

(B) a positive cost

(C) zero net cost and zero exposure to every factor

(D) only idiosyncratic risk

\medskip

**Q28.** Absence of arbitrage forces expected returns to be:

(A) equal across assets

(B) linear in the factor betas

(C) zero

(D) normally distributed

\medskip

**Q29.** In $E(R_i)=\lambda_0+\sum_k\beta_{ik}\lambda_k$, the intercept $\lambda_0$ equals:

(A) the market return

(B) the risk-free rate

(C) zero

(D) the average beta

\medskip

**Q30.** A "pure factor portfolio" with unit exposure to factor $k$ and zero to all others earns an expected return of:

(A) $\lambda_k$

(B) $R_f$

(C) $R_f - \lambda_k$

(D) $R_f + \lambda_k$

\medskip

## C.3 — Prix APT & arbitrage

**Q31.** A two-factor APT model: $R_f=4\%$, $\beta_1=1.0$ ($\lambda_1=5\%$), $\beta_2=0.5$ ($\lambda_2=4\%$). The expected return is:

(A) 9%

(B) 10%

(C) 11%

(D) 12%

\medskip

**Q32.** A two-factor APT model: $R_f=3\%$, $\beta_1=1.2$ ($\lambda_1=6\%$), $\beta_2=0.8$ ($\lambda_2=2\%$). The expected return is:

(A) 10.4%

(B) 11.0%

(C) 11.8%

(D) 12.6%

\medskip

**Q33.** A three-factor model: $R_f=2\%$, MKT $\beta=1.0$ ($\lambda=5\%$), SMB $\beta=0.5$ ($\lambda=3\%$), HML $\beta=-0.4$ ($\lambda=4\%$). The expected return is:

(A) 5.9%

(B) 6.9%

(C) 8.1%

(D) 10.1%

\medskip

**Q34.** Two well-diversified portfolios both have $\beta=1.0$ to the single factor, with $E(R_A)=13\%$ and $E(R_B)=11\%$. The arbitrage is to:

(A) sell A and buy B

(B) buy A and sell B, earning roughly 2% risk-free

(C) do nothing (no arbitrage)

(D) buy both

\medskip

**Q35.** Pure factor portfolios imply $\lambda_1$ and $\lambda_2$. A portfolio with those betas that trades ABOVE its APT-fair value is:

(A) fairly priced

(B) overpriced — sell it

(C) underpriced — buy it

(D) risk-free

\medskip

**Q36.** The APT arbitrage argument constructs a portfolio that is:

(A) maximally risky

(B) costly to set up

(C) zero-cost and riskless, hence must earn a zero expected return

(D) market-neutral only

\medskip

**Q37.** If a zero-cost, zero-factor-exposure portfolio had a positive expected return, then:

(A) the market is efficient

(B) an arbitrage opportunity exists

(C) all $\lambda_k=0$

(D) $R_f$ is negative

\medskip

**Q38.** The APT pricing relation holds exactly for:

(A) every individual stock

(B) well-diversified portfolios (idiosyncratic risk $\approx 0$); only approximately for single assets

(C) only the market portfolio

(D) only the risk-free asset

\medskip

**Q39.** A pure factor portfolio $P_1$ ($\beta_1=1$, others 0) has $E(R)=10\%$ with $R_f=4\%$. The factor premium $\lambda_1$ is:

(A) 4%

(B) 5%

(C) 6%

(D) 10%

\medskip

**Q40.** $R_f=3\%$; $P_1$ ($\beta_1=1,\beta_2=0$) has $E=9\%$; $P_2$ ($\beta_1=0,\beta_2=1$) has $E=11\%$. A portfolio with $\beta_1=\beta_2=1$ has a fair expected return of:

(A) 11%

(B) 14%

(C) 16%

(D) 17%

\medskip

**Q41.** Using the premia from Q40, a portfolio $Q$ with $\beta_1=\beta_2=0.5$ has a fair return of 10% but trades at 12%. You should:

(A) sell it

(B) buy it (underpriced, $\alpha=+2\%$)

(C) ignore it

(D) short the factors

\medskip

**Q42.** An asset whose actual expected return exceeds its APT-fair return is:

(A) overpriced

(B) underpriced (a buy)

(C) fairly priced

(D) risk-free

\medskip

**Q43.** A replicating portfolio used to arbitrage a mispriced portfolio must have:

(A) zero beta

(B) a higher return

(C) the same factor betas as the mispriced portfolio

(D) only idiosyncratic risk

\medskip

**Q44.** Idiosyncratic risk earns no premium because it is:

(A) systematic

(B) diversifiable

(C) the market factor

(D) priced

\medskip

**Q45.** A mispriced well-diversified portfolio enables near-riskless arbitrage, whereas a mispriced individual stock:

(A) gives the identical arbitrage

(B) is risk-free

(C) may not, because idiosyncratic risk remains

(D) cannot be mispriced

\medskip

## C.4 — APT vs CAPM

**Q46.** The CAPM is the APT with:

(A) three factors

(B) a single factor (the market)

(C) no factors

(D) PCA factors

\medskip

**Q47.** Which does the CAPM require that the APT does NOT?

(A) a risk-free rate

(B) the market portfolio

(C) betas

(D) factors

\medskip

**Q48.** Which assumption does the APT relax relative to the CAPM?

(A) the existence of a risk-free rate

(B) the existence of betas

(C) normally distributed returns / quadratic utility

(D) positive returns

\medskip

**Q49.** Both the CAPM and the APT feature:

(A) PCA

(B) a linear expected-return relation with an $R_f$ intercept

(C) exactly one factor

(D) no risk premia

\medskip

**Q50.** The CAPM SML uses a single market beta; the APT uses:

(A) no betas

(B) multiple factor betas

(C) only alpha

(D) the risk-free rate

\medskip

**Q51.** A stock with a positive APT alpha is:

(A) overpriced

(B) mispriced and underpriced

(C) fairly priced

(D) risk-free

\medskip

**Q52.** The APT is more general than the CAPM because it:

(A) requires the market portfolio

(B) has exactly one factor

(C) allows multiple factors under weaker assumptions

(D) assumes normality

\medskip

**Q53.** The empirical advantage of multifactor models is that they:

(A) ignore risk

(B) capture size, value, and momentum that the CAPM misses

(C) require the market portfolio

(D) use a single factor

\medskip

**Q54.** The disadvantage of the APT relative to the CAPM is that:

(A) it needs the market portfolio

(B) it has one factor

(C) its factors are not specified by theory (factor identification is ambiguous)

(D) it assumes normality

\medskip

**Q55.** If only the market factor is priced, the APT reduces to:

(A) Fama-French

(B) the CAPM

(C) Carhart

(D) a PCA model

\medskip

**Q56.** A Roll-type critique of the APT concerns:

(A) stable betas

(B) normality

(C) the arbitrary choice of factors

(D) having only one factor

\medskip

**Q57.** The market risk premium of the CAPM corresponds in the APT to:

(A) the risk-free rate

(B) the premium $\lambda$ on the market factor

(C) idiosyncratic risk

(D) alpha

\medskip

## C.5 — Familles de facteurs

**Q58.** Macroeconomic factors include:

(A) book-to-market

(B) momentum

(C) GDP growth, inflation surprises, and credit spreads

(D) firm size

\medskip

**Q59.** Statistical factors are:

(A) macro variables

(B) extracted by PCA with no a priori economic meaning

(C) firm characteristics

(D) the risk-free rate

\medskip

**Q60.** Fundamental (characteristic) factors are built from:

(A) PCA components

(B) macro surprises

(C) firm characteristics such as size, value, and momentum

(D) random numbers

\medskip

**Q61.** An advantage of statistical (PCA) factors is capturing the covariance structure well; a drawback is that they are:

(A) economically obvious

(B) always stable

(C) hard to interpret and can be unstable

(D) risk-free

\medskip

**Q62.** Chen, Roll & Ross (1986) used:

(A) PCA factors

(B) macroeconomic factors

(C) only the market

(D) momentum

\medskip

**Q63.** A commercial risk model (e.g., BARRA) models covariance using:

(A) a single factor

(B) every pairwise stock covariance

(C) style and industry factors, parsimoniously

(D) the risk-free rate

\medskip

**Q64.** SMB is best classified as a:

(A) macro factor

(B) statistical factor

(C) fundamental (characteristic) factor

(D) liquidity factor

\medskip

**Q65.** The main drawback of PCA factors is:

(A) too few of them

(B) no economic meaning and possible instability

(C) they need the market portfolio

(D) they are perfectly observable

\medskip

**Q66.** Macroeconomic factor models price:

(A) the levels of macro variables

(B) the surprises (unexpected changes) in macro variables

(C) firm characteristics

(D) nothing

\medskip

**Q67.** Which is NOT a typical fundamental factor?

(A) value

(B) momentum

(C) profitability

(D) an inflation surprise

\medskip

**Q68.** A multifactor risk model reduces dimensionality from:

(A) one number to many

(B) betas to alphas

(C) an $N\times N$ stock covariance matrix to a small factor covariance plus betas

(D) factors to the market

\medskip

**Q69.** The "factor zoo" problem refers to:

(A) too few factors

(B) the market portfolio

(C) the proliferation of published factors and the risk of data mining

(D) PCA

\medskip

**Q70.** Liquidity can be:

(A) only idiosyncratic

(B) a priced systematic factor

(C) the risk-free rate

(D) never relevant

\medskip

**Q71.** Oil prices and exchange rates are examples of:

(A) fundamental factors

(B) statistical factors

(C) macroeconomic factors

(D) idiosyncratic noise

\medskip

**Q72.** Selecting factors should be guided by:

(A) maximizing in-sample fit only

(B) the sheer number of factors

(C) economic rationale and out-of-sample robustness

(D) randomness

\medskip

## C.6 — Fama-French, Carhart & 5 facteurs

**Q73.** The Fama-French three factors are:

(A) MKT, SMB, WML

(B) MKT, SMB, HML

(C) SMB, HML, WML

(D) the market only

\medskip

**Q74.** SMB captures the premium for:

(A) value

(B) size (small minus big)

(C) momentum

(D) the market

\medskip

**Q75.** HML captures the premium for:

(A) size

(B) momentum

(C) value (high minus low book-to-market)

(D) liquidity

\medskip

**Q76.** The Carhart four-factor model adds:

(A) a liquidity factor

(B) momentum (WML/UMD)

(C) a volatility factor

(D) a credit factor

\medskip

**Q77.** The Fama-French five-factor model adds:

(A) momentum and liquidity

(B) size and value

(C) RMW (profitability) and CMA (investment)

(D) credit and rates

\medskip

**Q78.** The dependent variable in a Fama-French regression is:

(A) the raw return

(B) the factor premium

(C) the excess return $R_i - R_f$

(D) alpha

\medskip

**Q79.** A statistically significant $\alpha$ in a Fama-French regression indicates:

(A) a high beta

(B) abnormal performance after controlling for the factors

(C) high volatility

(D) a small-cap tilt

\medskip

**Q80.** A high $\beta^{\text{HML}}$ indicates a:

(A) growth tilt

(B) value tilt

(C) momentum tilt

(D) risk-free position

\medskip

**Q81.** A negative $\beta^{\text{HML}}$ indicates a:

(A) value tilt

(B) large-cap tilt

(C) growth tilt

(D) momentum tilt

\medskip

**Q82.** A high $\beta^{\text{SMB}}$ indicates a:

(A) large-cap tilt

(B) small-cap tilt

(C) value tilt

(D) growth tilt

\medskip

**Q83.** Carhart (1997) found that mutual-fund performance is:

(A) pure skill

(B) entirely random

(C) largely explained by factor exposures and fees, with little persistent skill

(D) all alpha

\medskip

**Q84.** The q-factor model (Hou-Xue-Zhang) uses:

(A) MKT, SMB, HML, WML

(B) only the market

(C) market, size, investment, and profitability

(D) PCA factors

\medskip

**Q85.** RMW captures:

(A) value

(B) profitability (robust minus weak)

(C) momentum

(D) size

\medskip

**Q86.** CMA captures:

(A) size

(B) value

(C) investment (conservative minus aggressive)

(D) momentum

\medskip

**Q87.** WML is also called:

(A) SMB

(B) HML

(C) UMD (up minus down)

(D) RMW

\medskip

**Q88.** The empirical value and size premia are evidence:

(A) for the single-factor CAPM

(B) against the single-factor CAPM

(C) of arbitrage

(D) of normality

\medskip

**Q89.** In the five-factor model, after adding RMW and CMA, HML can become:

(A) the only factor

(B) the market

(C) redundant in some specifications

(D) negative by construction

\medskip

**Q90.** Fund "alpha" that disappears in a Carhart regression was actually:

(A) genuine skill

(B) luck only

(C) factor exposure (beta), not skill

(D) idiosyncratic noise

\medskip

## C.7 — Estimation

**Q91.** A time-series regression on factor-portfolios (e.g., Fama-French) estimates, for each asset:

(A) only the risk-free rate

(B) the factor premia

(C) the betas and the alpha

(D) the market portfolio

\medskip

**Q92.** A cross-sectional regression of average returns on betas estimates:

(A) the betas

(B) idiosyncratic risk

(C) the factor risk premia $\lambda_k$

(D) the alphas

\medskip

**Q93.** Step 1 of the Fama-MacBeth procedure is to:

(A) average returns

(B) estimate the betas (time-series)

(C) run one big regression

(D) compute the Sharpe ratio

\medskip

**Q94.** Step 2 of Fama-MacBeth is to:

(A) re-estimate betas

(B) compute alpha

(C) run a cross-sectional regression each period, yielding $\hat\lambda_{k,t}$

(D) average the betas

\medskip

**Q95.** The Fama-MacBeth premium estimate is:

(A) the last period's $\hat\lambda$

(B) the average of the period-by-period $\hat\lambda_{k,t}$

(C) the maximum $\hat\lambda$

(D) zero

\medskip

**Q96.** The t-statistic in Fama-MacBeth is based on:

(A) the number of stocks

(B) the betas

(C) the time-series variability of the $\hat\lambda_{k,t}$

(D) the alphas

\medskip

**Q97.** A key advantage of Fama-MacBeth is that it handles:

(A) heteroskedasticity only

(B) missing data

(C) the cross-sectional correlation of residuals

(D) nonlinearity

\medskip

**Q98.** Cross-sectional estimates of $\lambda_{\text{HML}}$ over four months are $0.5\%, 0.3\%, -0.1\%, 0.3\%$. The Fama-MacBeth premium is:

(A) 0.10%

(B) 0.25%

(C) 0.40%

(D) 1.00%

\medskip

**Q99.** When a factor is a tradable portfolio, its premium can be estimated directly as:

(A) its beta

(B) the risk-free rate

(C) its average excess return

(D) its variance

\medskip

**Q100.** A factor strategy has an active return of 4% and a tracking error of 8%. Its information ratio is:

(A) 0.25

(B) 0.50

(C) 0.75

(D) 2.00

\medskip

**Q101.** To estimate a stock's beta to a factor, you regress:

(A) the factor on the stock

(B) the stock's price on time

(C) the stock's excess return on the factor

(D) alpha on beta

\medskip

## C.8 — Décomposition du risque

**Q102.** In a single-factor model, total variance decomposes as:

(A) $\sigma_i^2 = \beta_i \sigma_F$

(B) $\sigma_i^2 = \beta_i^2 \sigma_F^2 + \sigma_{\varepsilon_i}^2$

(C) $\sigma_i^2 = \sigma_F^2 - \sigma_{\varepsilon_i}^2$

(D) $\sigma_i^2 = \beta_i^2$

\medskip

**Q103.** With $\beta=1.2$ and factor volatility $\sigma_F=15\%$, the systematic volatility is:

(A) 12%

(B) 15%

(C) 18%

(D) 22%

\medskip

**Q104.** Continuing Q103, with idiosyncratic volatility $\sigma_\varepsilon=20\%$, the total volatility is closest to:

(A) 20%

(B) 24%

(C) 26.9%

(D) 38%

\medskip

**Q105.** Continuing Q104, the $R^2$ (systematic share of variance) is closest to:

(A) 30%

(B) 44.8%

(C) 60%

(D) 100%

\medskip

**Q106.** The idiosyncratic share of variance equals:

(A) $R^2$

(B) $\beta^2$

(C) $1 - R^2$

(D) $\sigma_F^2$

\medskip

**Q107.** A portfolio holds 50%/50% of two stocks with betas 1.0 and 1.4. Its beta is:

(A) 1.0

(B) 1.4

(C) 1.2

(D) 2.4

\medskip

**Q108.** As the number of holdings grows, a portfolio's idiosyncratic variance:

(A) grows

(B) tends to zero (diversification)

(C) stays constant

(D) equals the factor variance

\medskip

**Q109.** A well-diversified portfolio's risk is:

(A) all idiosyncratic

(B) zero

(C) almost entirely systematic (factor) risk

(D) equal to the risk-free rate

\medskip

**Q110.** The factor covariance matrix $\Omega_F$ is used to compute:

(A) idiosyncratic risk

(B) the alpha

(C) systematic portfolio variance $\boldsymbol\beta_P^{\top}\Omega_F\boldsymbol\beta_P$

(D) the risk-free rate

\medskip

**Q111.** For two uncorrelated factors, the systematic variance is:

(A) $\beta_1\beta_2\sigma_1\sigma_2$

(B) $\beta_1^2\sigma_1^2 - \beta_2^2\sigma_2^2$

(C) $\beta_1^2\sigma_1^2 + \beta_2^2\sigma_2^2$

(D) $\sigma_1^2 + \sigma_2^2$

\medskip

**Q112.** If factors are uncorrelated, cross-terms in the systematic variance:

(A) dominate

(B) are negative

(C) vanish

(D) equal one

\medskip

**Q113.** Two uncorrelated factors: $\beta_1=1.0,\sigma_1=10\%$; $\beta_2=0.5,\sigma_2=20\%$. The systematic volatility is:

(A) 10%

(B) 12%

(C) 14.1%

(D) 20%

\medskip

**Q114.** A higher $R^2$ in a factor regression means:

(A) more idiosyncratic risk

(B) lower beta

(C) more of the risk is explained by the factors (systematic)

(D) a higher alpha

\medskip

**Q115.** Idiosyncratic risk is:

(A) perfectly correlated across assets

(B) uncorrelated across assets and diversifiable

(C) systematic

(D) priced

\medskip

**Q116.** A portfolio's active risk (tracking error) is driven by:

(A) the risk-free rate

(B) only the market

(C) factor tilts and stock-specific bets relative to the benchmark

(D) alpha only

\medskip

**Q117.** A key benefit of a factor risk model is:

(A) more parameters

(B) ignoring correlations

(C) parsimony and an interpretable risk decomposition

(D) higher returns

\medskip

**Q118.** Marginal contribution to risk is used to:

(A) compute alpha

(B) set the risk-free rate

(C) manage and budget factor exposures

(D) eliminate the market

\medskip

**Q119.** Systematic (factor) risk cannot be:

(A) measured

(B) priced

(C) diversified away

(D) hedged

\medskip

**Q120.** A portfolio's exposure to a factor equals:

(A) the alpha

(B) the risk-free rate

(C) the weighted sum of its constituents' betas to that factor

(D) the idiosyncratic variance

\medskip

## C.9 — *Smart beta* & investissement factoriel

**Q121.** Factor investing means:

(A) buying the index

(B) avoiding all risk

(C) deliberately tilting toward rewarded factors

(D) holding only cash

\medskip

**Q122.** Smart beta is:

(A) active stock picking

(B) rules-based, low-cost factor exposure

(C) a hedge fund strategy

(D) the risk-free asset

\medskip

**Q123.** A risk of factor crowding is that it:

(A) raises the premium

(B) has no effect

(C) erodes the premium and creates correlated exits

(D) eliminates risk

\medskip

**Q124.** The "quant quake" of August 2007 illustrates:

(A) interest rate risk

(B) a sovereign default

(C) credit risk

(D) crowding and endogenous liquidity risk in factor strategies

\medskip

**Q125.** Factor timing is:

(A) always profitable

(B) controversial — premia can underperform for long periods

(C) impossible to attempt

(D) risk-free

\medskip

**Q126.** The value "lost decade" (2010s) warns that:

(A) value never works

(B) momentum is dead

(C) a premium can vanish for many years

(D) the CAPM holds

\medskip

**Q127.** AQR's "value and momentum everywhere" shows that:

(A) factors are US-only

(B) momentum is fake

(C) value beats momentum

(D) factors are universal across asset classes and diversify each other

\medskip

**Q128.** Fundamental indexing (RAFI) weights stocks by:

(A) market capitalization

(B) momentum

(C) fundamentals, capturing a value tilt

(D) volatility

\medskip

**Q129.** Momentum crashes typically occur:

(A) in calm markets

(B) in violent rebounds after bear markets

(C) never

(D) when rates are zero

\medskip

**Q130.** Combining value and momentum is attractive because they are:

(A) identical

(B) perfectly correlated

(C) negatively correlated, diversifying each other

(D) risk-free

\medskip

**Q131.** A long-only smart-beta product:

(A) shorts heavily

(B) is the risk-free asset

(C) captures factor exposure under no-shorting constraints

(D) has no factor exposure

\medskip

**Q132.** Low-volatility / minimum-volatility strategies exploit:

(A) momentum

(B) the low-volatility anomaly

(C) the size effect

(D) the risk-free rate

\medskip

**Q133.** McLean & Pontiff find that anomalies:

(A) strengthen after publication

(B) stay constant

(C) become risk-free

(D) decay after publication as arbitrageurs act

\medskip

## C.10 — Anomalies, *factor zoo* & critiques

**Q134.** Betting-against-beta (Frazzini-Pedersen) finds that:

(A) high-beta assets win

(B) low-beta assets have better risk-adjusted returns

(C) beta is irrelevant

(D) the CAPM holds exactly

\medskip

**Q135.** The BAB premium is explained by:

(A) normality

(B) momentum

(C) leverage constraints distorting the SML

(D) the risk-free rate

\medskip

**Q136.** The profitability/quality factor finds that:

(A) unprofitable firms win

(B) robustly profitable firms outperform

(C) profitability is irrelevant

(D) only size matters

\medskip

**Q137.** A factor premium may reflect:

(A) only luck

(B) only the market

(C) only the risk-free rate

(D) compensation for risk OR a behavioral bias/friction (debated)

\medskip

**Q138.** Harvey, Liu & Zhu (the "factor zoo") imply that:

(A) more factors are always better

(B) the market is the only factor

(C) many published factors are false positives, so t-thresholds should be raised

(D) PCA solves everything

\medskip

**Q139.** Data-mining risk is mitigated by:

(A) maximizing in-sample fit

(B) adding more factors

(C) out-of-sample testing and economic rationale

(D) ignoring statistics

\medskip

**Q140.** A Roll-type critique of the APT is that:

(A) betas are stable

(B) the choice of factors is arbitrary

(C) returns are normal

(D) it has one factor

\medskip

**Q141.** The momentum effect is often explained by:

(A) leverage

(B) the risk-free rate

(C) behavioral underreaction to news

(D) normality

\medskip

**Q142.** The low-volatility anomaly contradicts:

(A) the APT

(B) the CAPM's positive risk-return relation

(C) diversification

(D) Fama-French

\medskip

**Q143.** Crowding reduces future premia because:

(A) it raises them

(B) it has no effect

(C) once a strategy is widely known, arbitrage competes the premium away

(D) of normality

\medskip

**Q144.** A higher t-statistic threshold (~3) is advocated to:

(A) find more factors

(B) guard against false discoveries

(C) raise returns

(D) reduce betas

\medskip

**Q145.** Smart beta can "go horribly wrong" (Arnott) when:

(A) factors are cheap

(B) there is no crowding

(C) factor valuations are rich, so past returns came from revaluation, not the premium

(D) the market portfolio is used

\medskip

**Q146.** The Asness-vs-Arnott debate is about:

(A) the risk-free rate

(B) normality

(C) factor timing vs constant diversification

(D) PCA

\medskip

**Q147.** Multifactor models aid performance attribution by:

(A) ignoring factors

(B) separating factor beta from true alpha

(C) using one factor

(D) maximizing fees

\medskip

**Q148.** The COVID-2020 episode shows that factors:

(A) are identical

(B) are risk-free

(C) have distinct defensive/cyclical profiles

(D) never move

\medskip

**Q149.** The key takeaway of this chapter is that the APT:

(A) requires the market portfolio

(B) has one factor

(C) assumes normality

(D) prices multiple factors by no-arbitrage, without the market portfolio

\medskip

**Q150.** The enduring weakness of the APT is that it:

(A) needs the risk-free rate

(B) is too simple

(C) does not identify the factors

(D) assumes quadratic utility

\medskip

---

# Partie D — Corrigé commenté & renvois aux sections

> Réponse en **gras**, **renvoi à la section** (§) / au **cas**, et explication en français. Calcul détaillé pour les questions chiffrées.

## D.1 — Du CAPM aux modèles multifactoriels

**Q1 — B** *(§1)*. Le CAPM n'a **qu'un** facteur systématique : le **marché**.
**Q2 — C** *(§1)*. Roll : le **vrai portefeuille de marché** est **inobservable** $\Rightarrow$ CAPM non testable.
**Q3 — B** *(§1 ; Cas 3)*. **Effet taille** : les **petites capitalisations** sur-performent la prédiction du CAPM.
**Q4 — B** *(§1 ; Cas 4)*. **Effet valeur** : fort *book-to-market* (**value**) sur-performe en moyenne.
**Q5 — C** *(§1 ; Cas 6)*. **Momentum** : les **gagnants récents** continuent de sur-performer.
**Q6 — B** *(§10 ; Cas 11)*. **Anomalie low-vol** : meilleur rendement **ajusté du risque** que prévu.
**Q7 — C** *(§1)*. $R_i=E(R_i)+\sum_k\beta_{ik}F_k+\varepsilon_i$.
**Q8 — C** *(§1)*. $\varepsilon_i$ = risque **idiosyncratique** (diversifiable).
**Q9 — B** *(§1)*. $F_k$ est une **surprise** : $E(F_k)=0$.
**Q10 — C** *(§1)*. $\beta_{ik}$ = **sensibilité** de l'actif $i$ au facteur $k$.
**Q11 — B** *(§1)*. Le risque **idiosyncratique** se **diversifie**.
**Q12 — D** *(§1)*. Motivation : **plusieurs risques systématiques** prisés + **anomalies** du CAPM.
**Q13 — B** *(§1 ; Cas 4)*. « *Beta is dead* » = Fama-French (1992) : taille/valeur dominent le $\beta$.
**Q14 — C** *(§5, §6)*. SMB = portefeuille **long-short** trié sur une **caractéristique**.
**Q15 — B** *(§5 ; Cas 17)*. **BARRA** (Rosenberg) : premier modèle de risque fondamental.

## D.2 — L'APT : modèle & hypothèses

**Q16 — B** *(§2 ; Cas 1)*. APT = **Ross (1976)**.
**Q17 — C** *(§2)*. Fondement : **absence d'arbitrage**.
**Q18 — B** *(§2)*. L'APT **n'exige pas** le **portefeuille de marché**.
**Q19 — C** *(§2)*. Ni **normalité** ni **utilité quadratique** requises.
**Q20 — C** *(§2)*. Faiblesse : l'APT **ne spécifie pas** les facteurs (ni leur nombre).
**Q21 — D** *(§2)*. $E(R_i)=R_f+\sum_k\beta_{ik}\lambda_k$.
**Q22 — B** *(§2)*. $\lambda_k$ = **prime** par unité d'exposition au facteur $k$.
**Q23 — C** *(§2)*. Facteur-portefeuille : $\lambda_k=E(F_k)-R_f$.
**Q24 — B** *(§2, §3)*. Portefeuille **bien diversifié** : risque **idiosyncratique négligeable**.
**Q25 — C** *(§2)*. L'APT admet **plusieurs** facteurs systématiques rémunérés.
**Q26 — C** *(§2)*. Le **nombre de facteurs** n'est **pas spécifié**.
**Q27 — C** *(§3)*. Portefeuille d'arbitrage : **coût nul** et **exposition nulle** à chaque facteur.
**Q28 — B** *(§3)*. L'absence d'arbitrage **impose** la **linéarité** en les bêtas.
**Q29 — B** *(§3)*. $\lambda_0=R_f$ (actif sans risque, bêtas nuls).
**Q30 — D** *(§3)*. Portefeuille **pur** de facteur $k$ : $R_f+\lambda_k$.

## D.3 — Prix APT & arbitrage

**Q31 — C** *(§2)*. $E(R)=4\%+1.0(5\%)+0.5(4\%)=11\%$.
**Q32 — C** *(§2)*. $E(R)=3\%+1.2(6\%)+0.8(2\%)=3+7.2+1.6=11.8\%$.
**Q33 — B** *(§2)*. $E(R)=2\%+1.0(5\%)+0.5(3\%)-0.4(4\%)=2+5+1.5-1.6=6.9\%$.
**Q34 — B** *(§3 ; Cas 43)*. Mêmes bêtas, rendements différents $\Rightarrow$ **acheter A, vendre B** : $\approx 2\%$ sans risque.
**Q35 — C** *(§3 ; Cas 44)*. Au-dessus du juste prix $\Rightarrow$ **sous-évalué $\Rightarrow$ acheter**.
**Q36 — C** *(§3)*. Portefeuille **sans coût et sans risque** $\Rightarrow$ rendement attendu **nul**.
**Q37 — B** *(§3)*. Un portefeuille coût/bêta nuls à rendement positif = **arbitrage**.
**Q38 — B** *(§3)*. Exact pour les **portefeuilles bien diversifiés** ; approximatif pour un titre isolé.
**Q39 — C** *(§3)*. $\lambda_1=E(P_1)-R_f=10\%-4\%=6\%$.
**Q40 — D** *(§3)*. $\lambda_1=6\%$, $\lambda_2=8\%$ ; $E=3\%+6\%+8\%=17\%$.
**Q41 — B** *(§3 ; Cas 44)*. Juste $=3+0.5(6)+0.5(8)=10\%$ ; à 12 % $\Rightarrow$ **sous-évalué, acheter** ($\alpha=+2\%$).
**Q42 — B** *(§3)*. Rendement réel $>$ juste $\Rightarrow$ **sous-évalué (achat)**.
**Q43 — C** *(§3)*. La réplication doit avoir les **mêmes bêtas** que l'actif mal évalué.
**Q44 — B** *(§1, §3)*. L'idiosyncratique n'est pas rémunéré car **diversifiable**.
**Q45 — C** *(§3)*. Un titre isolé mal évalué garde du risque **idiosyncratique** : pas d'arbitrage sans risque.

## D.4 — APT vs CAPM

**Q46 — B** *(§4 ; Cas 37)*. CAPM = APT à **un facteur** (le marché).
**Q47 — B** *(§4)*. Le CAPM exige le **portefeuille de marché** ; pas l'APT.
**Q48 — C** *(§4)*. L'APT **relâche** la **normalité / utilité quadratique**.
**Q49 — B** *(§4)*. Les deux : relation **linéaire** avec intercept $R_f$.
**Q50 — B** *(§4)*. L'APT utilise **plusieurs bêtas** factoriels.
**Q51 — B** *(§4)*. $\alpha$ APT positif $\Rightarrow$ titre **sous-évalué**.
**Q52 — C** *(§4)*. Plus général : **plusieurs facteurs**, hypothèses **plus faibles**.
**Q53 — B** *(§4)*. Capture **taille/valeur/momentum** que le CAPM rate.
**Q54 — C** *(§4 ; Cas 38)*. Inconvénient : **facteurs non spécifiés** par la théorie.
**Q55 — B** *(§4)*. Un seul facteur (marché) $\Rightarrow$ **CAPM**.
**Q56 — C** *(§4 ; Cas 38)*. Critique « à la Roll » : **choix arbitraire** des facteurs.
**Q57 — B** *(§4)*. La MRP du CAPM = **prime $\lambda$** du facteur marché.

## D.5 — Familles de facteurs

**Q58 — C** *(§5)*. Facteurs **macro** : PIB, surprises d'inflation, spreads de crédit.
**Q59 — B** *(§5)*. Facteurs **statistiques** : **ACP/PCA**, sans sens économique a priori.
**Q60 — C** *(§5)*. Facteurs **fondamentaux** : **caractéristiques** d'entreprise (taille, valeur, momentum).

(suite §5)
**Q61 — C** *(§5)*. Facteurs PCA : bonne capture de la covariance mais **difficiles à interpréter / instables**.
**Q62 — B** *(§5 ; Cas 2)*. CRR : facteurs **macroéconomiques**.
**Q63 — C** *(§5, §8 ; Cas 17)*. BARRA modélise via **facteurs de style et sectoriels** (parcimonie).
**Q64 — C** *(§5)*. SMB = facteur **fondamental** (taille).
**Q65 — B** *(§5)*. PCA : **pas de sens économique** et **instabilité**.
**Q66 — B** *(§5)*. Les modèles macro prisent les **surprises**, pas les niveaux.
**Q67 — D** *(§5)*. Une **surprise d'inflation** est **macro**, pas fondamentale.
**Q68 — C** *(§8)*. On passe d'une covariance **$N\times N$** à **covariance des facteurs + bêtas**.
**Q69 — C** *(§10 ; Cas 14, 15)*. *Factor zoo* = **prolifération** des facteurs et **data mining**.
**Q70 — B** *(§5)*. La **liquidité** peut être un facteur **systématique** rémunéré.
**Q71 — C** *(§5)*. Pétrole/change = facteurs **macro**.
**Q72 — C** *(§5, §10 ; Cas 38)*. Guidé par **fondement économique** et **robustesse hors échantillon**.

## D.6 — Fama-French, Carhart & 5 facteurs

**Q73 — B** *(§6)*. FF3 : **MKT, SMB, HML**.
**Q74 — B** *(§6)*. SMB = prime de **taille**.
**Q75 — C** *(§6)*. HML = prime de **valeur**.
**Q76 — B** *(§6 ; Cas 7)*. Carhart ajoute le **momentum** (WML/UMD).
**Q77 — C** *(§6 ; Cas 8)*. FF5 ajoute **RMW** (rentabilité) et **CMA** (investissement).
**Q78 — C** *(§6)*. Variable expliquée : **rendement excédentaire** $R_i-R_f$.
**Q79 — B** *(§6)*. $\alpha$ significatif = **performance anormale** après contrôle des facteurs.
**Q80 — B** *(§6)*. Fort $\beta^{\text{HML}}$ $\Rightarrow$ biais **value**.
**Q81 — C** *(§6)*. $\beta^{\text{HML}}$ négatif $\Rightarrow$ biais ***growth***.
**Q82 — B** *(§6)*. Fort $\beta^{\text{SMB}}$ $\Rightarrow$ biais **petites capitalisations**.
**Q83 — C** *(§6 ; Cas 7)*. Performance des fonds = **facteurs + frais**, peu de **talent persistant**.
**Q84 — C** *(§6 ; Cas 9)*. q-factor : **marché, taille, investissement, rentabilité**.
**Q85 — B** *(§6)*. RMW = **rentabilité** (robust minus weak).
**Q86 — C** *(§6)*. CMA = **investissement** (conservative minus aggressive).
**Q87 — C** *(§6)*. WML = **UMD**.
**Q88 — B** *(§10)*. Primes taille/valeur = preuves **contre** le CAPM à un facteur.
**Q89 — C** *(§6 ; Cas 8)*. Avec RMW/CMA, HML peut devenir **redondant**.
**Q90 — C** *(§6 ; Cas 40)*. Cet « alpha » était du **bêta factoriel**, pas du talent.

## D.7 — Estimation

**Q91 — C** *(§7)*. Série temporelle (facteurs-portefeuilles) $\Rightarrow$ **bêtas et alpha**.
**Q92 — C** *(§7)*. Coupe transversale $\Rightarrow$ **primes $\lambda_k$**.
**Q93 — B** *(§7)*. Fama-MacBeth, étape 1 : **bêtas** (série temporelle).
**Q94 — C** *(§7)*. Étape 2 : **régression en coupe** à chaque période $\Rightarrow \hat\lambda_{k,t}$.
**Q95 — B** *(§7)*. Prime = **moyenne** des $\hat\lambda_{k,t}$.
**Q96 — C** *(§7)*. La stat $t$ vient de la **variabilité temporelle** des $\hat\lambda_t$.
**Q97 — C** *(§7)*. Fama-MacBeth gère la **corrélation transversale** des résidus.
**Q98 — B** *(§7)*. $\bar\lambda=\tfrac14(0{,}5+0{,}3-0{,}1+0{,}3)=0{,}25\%$.
**Q99 — C** *(§7)*. Facteur-portefeuille : $\lambda$ = **rendement excédentaire moyen**.
**Q100 — B** *(§7)*. $\mathrm{IR}=4\%/8\%=0{,}50$.
**Q101 — C** *(§7)*. On régresse le **rendement excédentaire** du titre **sur le facteur**.

## D.8 — Décomposition du risque

**Q102 — B** *(§8)*. $\sigma_i^2=\beta_i^2\sigma_F^2+\sigma_{\varepsilon_i}^2$.
**Q103 — C** *(§8)*. $\sqrt{1{,}2^2\times0{,}15^2}=1{,}2\times0{,}15=18\%$.
**Q104 — C** *(§8)*. $\sigma^2=0{,}0324+0{,}04=0{,}0724\Rightarrow\sigma=26{,}9\%$.
**Q105 — B** *(§8)*. $R^2=0{,}0324/0{,}0724=44{,}8\%$.
**Q106 — C** *(§8)*. Part idiosyncratique $=1-R^2$.
**Q107 — C** *(§8 ; Cas 46)*. $\beta_P=0{,}5(1{,}0)+0{,}5(1{,}4)=1{,}2$.
**Q108 — B** *(§8)*. La variance **idiosyncratique** $\to 0$ (diversification).
**Q109 — C** *(§8)*. Un portefeuille diversifié n'a presque que du risque **systématique**.
**Q110 — C** *(§8)*. $\Omega_F$ sert à $\boldsymbol\beta_P^{\top}\Omega_F\boldsymbol\beta_P$ (variance systématique).
**Q111 — C** *(§8)*. Facteurs **non corrélés** : $\beta_1^2\sigma_1^2+\beta_2^2\sigma_2^2$.
**Q112 — C** *(§8)*. Facteurs non corrélés $\Rightarrow$ **termes croisés nuls**.
**Q113 — C** *(§8 ; Cas 45)*. $\sqrt{1\cdot0{,}10^2+0{,}25\cdot0{,}20^2}=\sqrt{0{,}02}=14{,}1\%$.
**Q114 — C** *(§8)*. $R^2$ élevé $\Rightarrow$ risque davantage **expliqué par les facteurs**.
**Q115 — B** *(§8)*. Idiosyncratique : **non corrélé** entre titres, **diversifiable**.
**Q116 — C** *(§8)*. *Tracking error* = **tilts factoriels + paris spécifiques** vs benchmark.
**Q117 — C** *(§8)*. Modèle factoriel : **parcimonie** et **décomposition interprétable**.
**Q118 — C** *(§8)*. La contribution marginale au risque sert à **piloter les expositions** factorielles.
**Q119 — C** *(§8)*. Le risque **systématique** ne se **diversifie pas**.
**Q120 — C** *(§8)*. Exposition = **somme pondérée** des bêtas des composants.

## D.9 — *Smart beta* & investissement factoriel

**Q121 — C** *(§9)*. Investissement factoriel = **s'incliner délibérément** vers les facteurs rémunérés.
**Q122 — B** *(§9 ; Cas 22)*. *Smart beta* = exposition factorielle **règles, faible coût**.
**Q123 — C** *(§9 ; Cas 16)*. Le ***crowding*** **érode** la prime et crée des **sorties corrélées**.
**Q124 — D** *(§9 ; Cas 25)*. *Quant quake* 2007 = **crowding** et **liquidité endogène**.
**Q125 — B** *(§9 ; Cas 34)*. Le *timing* factoriel est **controversé** (sous-performances longues).
**Q126 — C** *(§9 ; Cas 28)*. La « décennie perdue » montre qu'une prime peut **disparaître des années**.
**Q127 — D** *(§9 ; Cas 20)*. Value et momentum sont **universels** et se **diversifient**.
**Q128 — C** *(§9 ; Cas 21)*. RAFI pondère par les **fondamentaux** (biais value).
**Q129 — B** *(§9 ; Cas 26, 27)*. Les krachs de momentum surviennent dans les **rebonds violents**.
**Q130 — C** *(§9 ; Cas 20)*. Value et momentum sont **négativement corrélés** (diversification).
**Q131 — C** *(§9)*. *Long-only* : exposition factorielle **sous contrainte de non-vente à découvert**.
**Q132 — B** *(§9, §10 ; Cas 11, 24)*. Min-vol exploite l'**anomalie low-vol**.
**Q133 — D** *(§10 ; Cas 16)*. McLean-Pontiff : les anomalies **s'érodent après publication**.

## D.10 — Anomalies, *factor zoo* & critiques

**Q134 — B** *(§10 ; Cas 10)*. BAB : les actifs à **faible bêta** ont un meilleur rendement ajusté du risque.
**Q135 — C** *(§10 ; Cas 10)*. Explication : **contraintes de levier** déformant la SML.
**Q136 — B** *(§10 ; Cas 12, 13)*. Les entreprises **rentables/de qualité** sur-performent.
**Q137 — D** *(§10)*. Une prime peut rémunérer un **risque** OU un **biais/friction** (débat).
**Q138 — C** *(§10 ; Cas 14)*. *Factor zoo* : beaucoup de **faux positifs** $\Rightarrow$ **seuils $t$ plus élevés**.
**Q139 — C** *(§10)*. Antidote au data mining : **hors échantillon** + **fondement économique**.
**Q140 — B** *(§10 ; Cas 38)*. Critique « à la Roll » : **choix des facteurs arbitraire**.
**Q141 — C** *(§10 ; Cas 6)*. Le momentum s'explique souvent par la **sous-réaction** comportementale.
**Q142 — B** *(§10 ; Cas 11)*. Le low-vol **contredit** la relation risque-rendement **positive** du CAPM.
**Q143 — C** *(§10 ; Cas 16)*. Une fois connue, la stratégie voit sa prime **arbitrée**.
**Q144 — B** *(§10 ; Cas 14)*. Seuil $t\approx3$ : se prémunir contre les **fausses découvertes**.
**Q145 — C** *(§9, §10 ; Cas 33)*. *Smart beta* déraille si les **valorisations factorielles** sont chères (revalorisation, pas prime).
**Q146 — C** *(§9 ; Cas 34)*. Débat : **timing factoriel** vs **diversification constante**.
**Q147 — B** *(§8, §6 ; Cas 40)*. L'attribution **sépare le bêta factoriel** du **vrai alpha**.
**Q148 — C** *(§10 ; Cas 31)*. COVID 2020 : les facteurs ont des **profils défensifs/cycliques** distincts.
**Q149 — D** *(§2, §3)*. À retenir : l'APT **price plusieurs facteurs** par **absence d'arbitrage**, **sans** portefeuille de marché.
**Q150 — C** *(§2)*. Faiblesse durable : l'APT **n'identifie pas** les facteurs.

---

## Note finale

Cet approfondissement couvre le chapitre 6 « 360° » : **concepts détaillés**, **démonstrations** (toutes en LaTeX), **50 études de cas** (théoriques, empiriques et chiffrées), un **formulaire**, et **150 QCM** au format examen avec **corrigé commenté**. Pour viser la maîtrise parfaite :

1. **Refais les démonstrations sans les notes** : l'**argument d'arbitrage** (portefeuille à coût nul et exposition factorielle nulle $\Rightarrow$ rendement nul $\Rightarrow$ linéarité $E(R_i)=R_f+\sum_k\beta_{ik}\lambda_k$), la **décomposition du risque** $\sigma_i^2=\boldsymbol\beta_i^{\top}\Omega_F\boldsymbol\beta_i+\sigma_{\varepsilon_i}^2$, et la procédure de **Fama-MacBeth**.
2. **Mémorise les distinctions** : APT (multifactoriel, **absence d'arbitrage**, **sans** marché ni normalité) vs CAPM (**cas à un facteur**) ; **trois familles** de facteurs (macro / fondamentaux / statistiques) ; **FF3 $\to$ Carhart $\to$ FF5** ; risque **systématique** (rémunéré) vs **idiosyncratique** (diversifiable, non rémunéré).
3. **Associe chaque cas à son idée** : Chen-Roll-Ross $\to$ facteurs macro prisés ; Fama-French/Carhart $\to$ taille/valeur/momentum ; *quant quake* 2007 & krachs de momentum $\to$ risque de **crowding** et de **queue** ; décennie perdue de la value $\to$ une prime peut disparaître ; *factor zoo* $\to$ data mining et discipline statistique.
4. **Le pont vers la pratique** : modèles de risque (BARRA/Axioma), *smart beta*, attribution de performance et *risk factor investing* sont des **applications directes** de l'APT — et reviennent en **gestion de portefeuille** et **risque de marché** (Partie II).

*Approfondissement pédagogique original — FRM, Foundations of Risk Management, Chapitre 6. Formules en notation GARP. À utiliser en complément des manuels officiels.*
