---
title: "FRM Part I · Foundations — Approfondissement du Chapitre 4"
subtitle: 'Credit Risk Transfer Mechanisms — concepts, démonstrations, une cinquantaine d''études de cas et environ 150 QCM corrigés'
---

> **Objet.** Approfondissement « 360° » du chapitre 4 : **concepts détaillés**, **démonstrations** complètes (équations en **LaTeX**, notation GARP — $N(\cdot)$ = fonction de répartition de la loi normale centrée réduite, $N^{-1}(\cdot)$ sa réciproque), **$\approx\!50$ études de cas** (historiques et chiffrées), un **formulaire**, et une **banque de $\approx\!150$ QCM** au format examen (très aérés) avec **corrigé commenté en français**. *Document pédagogique original ; les chiffres des cas sont des ordres de grandeur communément cités, à visée illustrative. QCM originaux : le GARP ne publiant pas ses annales, aucune question n'est attribuée à une session passée.*

### Plan

**Partie A — Concepts & démonstrations.** (1) Briques quantitatives — (2) CDS en profondeur — (3) Autres dérivés de crédit & indices — (4) Titrisation & *tranching* — (5) Familles de CDO — (6) Corrélation & modèle à un facteur (Vasicek) — (7) *Originate-to-distribute* & crise — (8) Réglementation post-crise — (9) Formulaire.
**Partie B — $\approx\!50$ études de cas** (AIG, monolines, ABACUS, London Whale, Archegos, Lehman/auctions, GM-Ford 2005, Grèce, Argentine, Northern Rock, CLO, cas chiffrés…).
**Partie C — $\approx\!150$ QCM** (format examen, très aérés).
**Partie D — Corrigé commenté** (français, renvois aux sections §).

---

# Partie A — Concepts & démonstrations

## 1. Briques quantitatives indispensables

Avant de **transférer** un risque de crédit, il faut le **mesurer**. Quatre briques structurent tout le chapitre.

**Perte attendue (EL) et inattendue (UL).** La perte attendue d'une exposition de crédit est
$$EL = PD \times LGD \times EAD, \qquad LGD = 1 - R,$$
où $PD$ = probabilité de défaut sur l'horizon, $LGD$ = perte en cas de défaut, $EAD$ = exposition au défaut, $R$ = taux de recouvrement. La **perte inattendue** $UL$ est la **dispersion** autour de l'EL ; c'est elle que couvre le **capital économique**. Point décisif pour la suite : **l'EL d'un portefeuille est insensible à la corrélation** (linéarité de l'espérance), alors que **l'UL et la queue de distribution en dépendent fortement**.

**Du taux de défaut au taux de hasard.** Pour modéliser le défaut dans le temps, on introduit l'**intensité de défaut** (ou **taux de hasard**, *hazard rate*) $\lambda$ : la probabilité instantanée de défaut **conditionnelle à la survie**. Sous hasard constant, la **probabilité de survie** jusqu'à $t$ et la **PD cumulée** valent
$$Q(t) = e^{-\lambda t}, \qquad PD(0,t) = 1 - e^{-\lambda t}.$$
Pour plusieurs périodes, la survie cumulée est le produit des survies périodiques : $Q(t) = \prod_i (1 - PD_i)$.

**PD réelle vs PD risque-neutre.** Les modèles de valorisation (CDS, obligations) utilisent des probabilités **risque-neutres** (extraites des prix), généralement **supérieures** aux probabilités **historiques/réelles** (elles intègrent une prime de risque). Règle d'or : on **price** avec la PD risque-neutre, on **gère le risque/capital** avec la PD réelle ; on ne les mélange jamais.

> **Boîte à outils — loi normale.** $N^{-1}(0{,}02) = -2{,}054$ ; $N^{-1}(0{,}05) = -1{,}645$ ; $N^{-1}(0{,}10) = -1{,}282$ ; $N^{-1}(0{,}12) = -1{,}175$ ; $N^{-1}(0{,}15) = -1{,}036$ ; $N^{-1}(0{,}999) = 3{,}090$. Et $N(1{,}28)\approx0{,}90$, $N(2{,}33)\approx0{,}99$, $N(3{,}09)\approx0{,}999$.

## 2. Le Credit Default Swap (CDS) en profondeur

### 2.1 Mécanique détaillée

Un **CDS** est un contrat bilatéral par lequel l'**acheteur de protection** transfère le risque de crédit d'une **entité de référence** au **vendeur de protection**, **sans céder l'actif sous-jacent**. Deux jambes :

- **Jambe de prime (*premium leg*)** : l'acheteur paie un *spread* périodique $s$ (souvent trimestriel ; coupons standardisés à 100 ou 500 bps depuis le *Big Bang* ISDA 2009, le solde réglé en *upfront*), calculé sur le notionnel, **tant qu'aucun événement de crédit ne survient**.
- **Jambe de protection (*protection leg*)** : si un **événement de crédit** frappe l'entité, le vendeur **indemnise** la perte $(1-R)\times$ notionnel.

Les **événements de crédit** (ISDA) incluent : **faillite (*bankruptcy*)**, **défaut de paiement (*failure to pay*)** et **restructuration (*restructuring*)**. Le **règlement** se fait en **physique** (l'acheteur livre l'obligation contre le pair) ou en **cash** (le vendeur paie $(1-R)\times$ notionnel, $R$ étant fixé par **enchère ISDA** post-événement).

> **Point d'examen.** Le CDS transfère le **risque de crédit**, pas le titre. Un CDS ***naked*** (protection sans détenir la dette) est **spéculatif** ou de couverture macro. L'acheteur de protection est **« short crédit »** (il gagne si la qualité se dégrade) ; le vendeur est **« long crédit »**.

### 2.2 Démonstration — le *credit triangle*

Cherchons le ***spread* équitable** sur une petite période $\Delta t$, sans actualisation (pour l'intuition).

- Coût attendu de la **protection** sur $\Delta t$ : $(\lambda\,\Delta t)\times(1-R)$.
- Coût de la **prime** sur $\Delta t$ : $s\,\Delta t$.

À l'équilibre (absence d'arbitrage), la prime payée égale la valeur de la protection :
$$s\,\Delta t \approx (\lambda\,\Delta t)(1-R) \;\Longrightarrow\; \boxed{\,s \approx \lambda(1-R)\,} \;\Longleftrightarrow\; \lambda \approx \frac{s}{1-R}.$$

**Interprétation.** Le *spread* CDS $\approx$ intensité de défaut $\times$ perte en cas de défaut. Ce « triangle » à trois variables $(s,\lambda,R)$ permet de passer de l'une à l'autre.

**Exemple chiffré.** CDS 5 ans, $s=200$ bps, $R=40\%$ :
$$\lambda \approx \frac{0{,}0200}{0{,}60} = 0{,}0333\ (3{,}33\%/\text{an}), \qquad PD(0,5) = 1 - e^{-0{,}0333\times5} = 1 - 0{,}8465 = 15{,}35\%.$$

### 2.3 Démonstration — valorisation exacte et *spread* par

En tenant compte de l'actualisation $DF(t_i)$ et des survies $Q(t_i)$ :
$$PV_{\text{prem}} = s\sum_i \Delta t_i\,DF(t_i)\,Q(t_i) = s\cdot RPV01, \qquad PV_{\text{prot}} = (1-R)\sum_i DF(t_i)\big[Q(t_{i-1})-Q(t_i)\big].$$
La quantité $RPV01 = \sum_i \Delta t_i\,DF(t_i)\,Q(t_i)$ est l'**annuité risquée** (valeur d'1 bp de prime). Le ***spread* par** (équitable à la mise en place) annule la valeur du contrat :
$$s^{*} = \frac{PV_{\text{prot}}}{RPV01}.$$
C'est la version « complète » du *credit triangle*.

### 2.4 Démonstration — *bootstrap* d'une PD à partir du *spread*

On **infère** la courbe de défaut implicite des prix CDS. Avec un seul *spread* $s$ et un recouvrement $R$ :
$$\lambda \approx \frac{s}{1-R}, \qquad Q(t) = e^{-\lambda t}.$$

**Exemple.** CDS 1 an à 80 bps, $R=40\%$ : $\lambda \approx 0{,}008/0{,}6 = 1{,}33\%$ ; $Q(1)=e^{-0{,}0133}=0{,}9868$, donc **PD 1 an $\approx 1{,}32\%$**. Avec une courbe ($1,3,5,10$ ans), on *bootstrappe* successivement les intensités *forward* $\lambda_1,\lambda_2,\dots$, comme une courbe de taux zéro.

### 2.5 Démonstration — mark-to-market d'un CDS existant

Un CDS conclu à $s_0$ prend de la valeur quand le *spread* de marché devient $s_1$. Du point de vue de l'**acheteur de protection** :
$$\mathrm{MtM}_{\text{ach}} \approx (s_1 - s_0)\times RPV01 \times \text{Notionnel}.$$

**Exemple.** Entré à $s_0=100$ bps ; marché $s_1=250$ bps ; $RPV01=4{,}3$ ; notionnel $10$ M€ :
$$\mathrm{MtM} \approx (0{,}0250-0{,}0100)\times4{,}3\times10\,000\,000 = 645\,000\ \text{€}.$$
L'acheteur a un **gain latent $\approx 645\,000$ €** (le vendeur, une perte symétrique).

### 2.6 Démonstration — la base CDS-obligation

La **base** mesure l'écart dérivés/comptant :
$$\text{Basis} = s_{\text{CDS}} - s_{\text{obligation}} \quad (s_{\text{obligation}} \approx \text{Z-spread / asset-swap spread}).$$

- **Base négative** ($\text{CDS}<\text{obligation}$) : protection « bon marché ». **Trade de base négative** : *acheter l'obligation + acheter la protection* $\Rightarrow$ crédit quasi neutralisé, **portage positif** $\approx -\text{Basis}$.
- **Base positive** : trade inverse.

**Exemple.** Obligation Z-spread $=180$ bps, CDS $=150$ bps $\Rightarrow \text{Basis}=-30$ bps ; le trade capture $\approx 30$ bps « sans risque » (avant frictions). **Drivers** de la base : risque de **contrepartie** sur le vendeur, **coûts de financement** (repo/funding de l'obligation), **option de livraison** du *cheapest-to-deliver*, **liquidité** relative, contraintes réglementaires. En 2008, les bases sont devenues **très négatives** (impossible de financer les obligations).

### 2.7 Règlement, enchères ISDA et squeeze de livraison

Après un événement de crédit, une **enchère ISDA** fixe le **prix de récupération** $R$ servant au règlement cash, ce qui **standardise** le dénouement de milliers de contrats. Historiquement, le **règlement physique** posait un **problème de rareté** : le notionnel de CDS pouvant **excéder** la dette livrable, les acheteurs de protection se **disputaient** les obligations (squeeze, ex. **Delphi 2005**), d'où le passage généralisé au **règlement par enchère**.

### 2.8 Risque de contrepartie et *wrong-way risk*

Le CDS **crée** un risque de contrepartie : l'acheteur n'est protégé que si le **vendeur** est **solvable** au moment du défaut. Le ***wrong-way risk*** est le cas pernicieux où l'**exposition** et la **qualité de la contrepartie** se **dégradent ensemble** (ex. acheter une protection sur une banque espagnole… auprès d'une autre banque espagnole). C'est ce qui a piégé les acheteurs auprès d'**AIG** et des **monolines** (cf. Partie B). Mitigation : **collatéral (CSA)**, **netting**, **compensation centrale (CCP)** — généralisée après 2008.

## 3. Autres dérivés de crédit et indices

- **Total Return Swap (TRS)** : échange l'**intégralité de la performance** d'un actif (intérêts + plus/moins-values) contre un **taux variable**. Le **payeur du *total return*** transfère **risque de crédit ET de marché** ; c'est un **financement synthétique avec levier** (l'acheteur de risque n'immobilise pas le notionnel). Cœur du dossier **Archegos** (Partie B).
- **Credit-Linked Note (CLN)** : obligation dont le remboursement dépend d'un événement de crédit ; le vendeur de protection **finance d'avance** (le SPV détient du collatéral), donc **pas de risque de contrepartie** sur lui — contrairement au CDS.
- **Indices CDS — CDX (Amérique du Nord) et iTraxx (Europe/Asie)** : paniers **standardisés** (ex. **CDX IG** = 125 *investment grade* ; **iTraxx Main** = 125 IG européens ; versions *High Yield* / *Crossover*). Très **liquides**, ils se renouvellent (***roll***) tous les 6 mois (nouvelle série « *on-the-run* »). Acheter l'indice = vendre/acheter la protection sur **tout le panier** d'un coup.
- **Tranches d'indices standardisées** : sur iTraxx Main, tranches **0–3 % (equity), 3–6 %, 6–9 %, 9–12 %, 12–22 %** ; sur CDX IG, **0–3, 3–7, 7–10, 10–15, 15–30**. Ce sont les instruments où se **négocie la corrélation** (§6) — théâtre de la **crise de corrélation de mai 2005** et de la **« Baleine de Londres »** (Partie B).
- **Panier *n-th-to-default*** : protection déclenchée au **$n$-ième** défaut du panier. Le ***first-to-default* (FTD)** est **long corrélation** côté vendeur : plus les noms sont corrélés, **moins** probable qu'au moins un (puis le premier) fasse défaut isolément — le *spread* FTD est **inférieur** à la somme des *spreads* mais **supérieur** au max individuel.
- **Options sur CDS / indices** (*credit default swaptions*, *payer/receiver*) : droit d'entrer dans un CDS à un *spread* d'exercice ; servent à se positionner sur la **volatilité des spreads**.

## 4. Titrisation & *tranching* en profondeur

### 4.1 La mécanique de l'opération

La **titrisation** transforme des créances **illiquides** (hypothèques, auto, cartes, prêts d'entreprise) en **titres négociables** :

1. Un **initiateur (*originator*)** cède un portefeuille à une **entité dédiée (*Special Purpose Vehicle*, SPV)**.
2. La cession est une ***true sale*** : les actifs **sortent du bilan** ; le SPV est ***bankruptcy remote*** (la faillite de l'initiateur n'atteint pas les actifs).
3. Le SPV **finance** l'achat en émettant des **titres adossés** (ABS ; **MBS** pour l'hypothécaire), vendus aux investisseurs.
4. Les **flux** des créances **remontent** et rémunèrent les titres selon une **cascade (*waterfall*)**.

### 4.2 Cascade et rehaussement de crédit (*credit enhancement*)

Pertes et flux sont alloués par **séniorité**. Le **rehaussement** protège les tranches hautes et permet des notations élevées :

- **Subordination (*tranching*)** : les juniors absorbent les premières pertes — rehaussement **principal**.
- **Surdimensionnement (*overcollateralization*, OC)** : valeur des actifs $>$ valeur des titres émis (coussin).
- **Excès de marge (*excess spread*)** : intérêts perçus $>$ intérêts versés $+$ frais ; **première ligne de défense** (absorbe les pertes courantes **avant** l'equity).
- **Compte de réserve** et **garanties externes** (*wrap* monoline, lettre de crédit).

On distingue la **cascade des intérêts** (senior, puis mezzanine, puis résiduel à l'equity) et la **cascade du principal** (**séquentielle** — senior remboursé d'abord — ou **pro-rata**). Des **déclencheurs (*OC/IC tests*)** **détournent** les flux vers le senior si la qualité se dégrade.

### 4.3 Démonstration — la fonction de perte d'une tranche

Soit une tranche d'**attachement** $A$ et de **détachement** $D$ (en % du notionnel du portefeuille). Son notionnel est $(D-A)\times$ Notionnel. Pour une **perte de portefeuille** $L$ (en %), la **perte de la tranche** (en % du portefeuille) est
$$\text{Loss}_{\text{tr}}(L) = \big[\min(L,D)-A\big]^{+}, \qquad [x]^{+}=\max(x,0),$$
et la **fraction du notionnel de tranche** détruite vaut
$$f(L) = \frac{\big[\min(L,D)-A\big]^{+}}{D-A} \in [0,1].$$

**Lecture** : tranche **intacte** tant que $L\le A$ ; **partielle** pour $A<L<D$ ; **détruite** dès $L\ge D$. Une tranche est donc un **spread d'options** sur la perte du portefeuille (positions sur des « calls sur la perte » aux strikes $A$ et $D$).

### 4.4 Exemple chiffré — allocation des pertes dans une cascade

Portefeuille **100 M€** : **Equity 0–3 %** (3 M€), **Mezzanine 3–7 %** (4 M€), **Senior 7–100 %** (93 M€).

| Perte portefeuille $L$ | Equity (0–3 %) | Mezzanine (3–7 %) | Senior (7–100 %) |
|---|---|---|---|
| 1 % (1,0 M€) | 1,0 M€ $\to$ **33,3 %** | 0 | 0 |
| 3 % (3,0 M€) | 3,0 M€ $\to$ **100 %** | 0 | 0 |
| 5 % (5,0 M€) | 3,0 M€ $\to$ **100 %** | 2,0 M€ $\to$ **50,0 %** | 0 |
| 7 % (7,0 M€) | 3,0 M€ $\to$ **100 %** | 4,0 M€ $\to$ **100 %** | 0 |
| 10 % (10,0 M€) | 3,0 M€ $\to$ **100 %** | 4,0 M€ $\to$ **100 %** | 3,0 M€ $\to$ **3,23 %** |
| 20 % (20,0 M€) | 3,0 M€ $\to$ **100 %** | 4,0 M€ $\to$ **100 %** | 13,0 M€ $\to$ **13,98 %** |

**Vérification** ($L=5\%$) : equity $[\min(5,3)]^{+}=3$ ; mezz $[\min(5,7)-3]^{+}=2$ ; senior $[\min(5,100)-7]^{+}=0$. Somme $=5\%$. ✓

**Enseignements.** (i) L'**equity** subit **chaque** euro de perte initiale : très risquée, très rémunérée (capte l'excès de marge tant qu'elle survit). (ii) Le **senior** ne souffre qu'après **épuisement** des juniors (ici $>7\%$) : ce qui justifiait des notes **AAA**… **sous réserve** que ces pertes restent improbables — hypothèse **entièrement dépendante de la corrélation** (§6). (iii) Une **tranche mince** (mezz 3–7 %) a un **levier** élevé : 0 % $\to$ 100 % sur 4 points de perte.

## 5. Les familles de CDO

- **CDO *cash*** : détient de **vrais** actifs. Sous-types : **CLO** (*Collateralized Loan Obligation*, prêts à effet de levier — segment **résilient** en 2008 **et** 2020), **ABS CDO** (titrise des tranches d'ABS/RMBS — au cœur de la crise), **CBO** (obligations).
- **CDO *synthétique*** : prend l'exposition via des **CDS** (le SPV **vend** la protection) — **pas de prêts réels**, donc **offre quasi illimitée** référençant le même panier (amplification).
- **CDO$^2$** : titrise des **tranches de CDO**. Opacité maximale.
- **Bilan vs arbitrage** : *balance-sheet CDO* (la banque transfère **ses** actifs pour libérer du capital) vs *arbitrage CDO* (on achète des actifs pour capter l'écart entre leur rendement et le coût des tranches).
- **CDO à tranche unique (*single-tranche / bespoke*)** : on vend **une seule** tranche « sur mesure » à un investisseur ; le vendeur **delta-hedge** son exposition à la corrélation sur l'indice — d'où l'importance des modèles de §6.

## 6. Le rôle de la corrélation — le modèle à un facteur

C'est **le** point conceptuel décisif et la cause profonde de la mésévaluation de 2007-2008. On le démontre avec le **modèle à un facteur à copule gaussienne** (modèle de **Vasicek**, socle de la formule IRB de Bâle).

### 6.1 Démonstration — PD conditionnelle

On modélise la « valeur d'actif » normalisée de l'emprunteur $i$ comme un **facteur systématique commun** $M$ plus un **bruit idiosyncratique** $\varepsilon_i$, tous deux $N(0,1)$ indépendants :
$$A_i = \sqrt{\rho}\,M + \sqrt{1-\rho}\,\varepsilon_i.$$
$\rho$ est la **corrélation d'actif** commune. L'emprunteur **fait défaut** si $A_i$ passe sous un seuil $c$ calibré sur la PD inconditionnelle :
$$P(A_i < c) = N(c) = PD \;\Longrightarrow\; c = N^{-1}(PD).$$
**Conditionnellement** à $M=m$, les défauts deviennent **indépendants**, et la **PD conditionnelle** est
$$p(m) = P\!\left(\varepsilon_i < \frac{c-\sqrt{\rho}\,m}{\sqrt{1-\rho}}\right) = N\!\left[\frac{N^{-1}(PD) - \sqrt{\rho}\,m}{\sqrt{1-\rho}}\right].$$

**Interprétation cruciale.** $p(m)$ est **décroissante** en $m$ : quand l'économie va mal ($m$ très négatif), $p(m)$ **explose** — tous défaillent **en même temps**. C'est le mécanisme des **défauts corrélés**.

### 6.2 Démonstration — distribution des pertes (Vasicek) et quantile

Pour un **grand portefeuille homogène** (loi des grands nombres conditionnelle), la perte est $L = LGD\cdot p(M)$. Avec $LGD=1$, sa fonction de répartition (loi de **Vasicek**) est
$$P(L \le \ell) = N\!\left[\frac{\sqrt{1-\rho}\,N^{-1}(\ell) - N^{-1}(PD)}{\sqrt{\rho}}\right].$$
Le **taux de défaut au pire cas** (*Worst-Case Default Rate*) au niveau $1-\alpha$ — base du **capital IRB de Bâle** — place $M$ dans sa mauvaise queue :
$$WCDR = N\!\left[\frac{N^{-1}(PD) + \sqrt{\rho}\,N^{-1}(1-\alpha)}{\sqrt{1-\rho}}\right].$$

**Exemple.** $PD=2\%$, $\rho=0{,}20$, confiance $99{,}9\%$ :
$$WCDR = N\!\left[\frac{-2{,}054 + \sqrt{0{,}20}\,(3{,}090)}{\sqrt{0{,}80}}\right] = N\!\left[\frac{-2{,}054+1{,}381}{0{,}894}\right] = N[-0{,}753] \approx 22{,}6\%.$$
Avec une PD moyenne de **2 %**, le taux de défaut « 1 fois sur 1000 » atteint **$\approx 22{,}6\%$** : voilà ce que le capital doit couvrir, et que la corrélation amplifie.

### 6.3 Démonstration — pourquoi la corrélation déplace le risque vers le senior

Clé : **l'EL du portefeuille ne dépend pas de $\rho$** (elle vaut toujours $PD\times LGD$). La corrélation **déplace seulement la masse vers les deux queues**. Avec $PD=5\%$, $LGD=100\%$, on calcule $P(L>\text{seuil})$ pour une **faible** ($\rho=0{,}05$) et une **forte** ($\rho=0{,}30$) corrélation :

| Seuil de perte | $P(L>\text{seuil})$, $\rho=0{,}05$ | $P(L>\text{seuil})$, $\rho=0{,}30$ |
|---|---|---|
| 3 % | 80,0 % | 44,8 % |
| 7 % | 17,9 % | 22,7 % |
| 15 % | 0,23 % | 7,8 % |
| 22 % | 0,003 % | 3,4 % |

**Détail** (seuil 15 %, $\rho=0{,}30$) : $F(0{,}15)=N\!\big[(\sqrt{0{,}70}\,(-1{,}036)+1{,}645)/\sqrt{0{,}30}\big]=N[1{,}421]=0{,}922$, donc $P(L>15\%)=7{,}8\%$.

**Lecture pour les tranches** (equity 0–3, mezz 3–7, senior 7+) :

- $\rho\uparrow \Rightarrow P(L>3\%)$ **chute** (80 % $\to$ 45 %) : le portefeuille subit **plus souvent** de **petites** pertes $\Rightarrow$ l'**equity survit plus souvent** $\Rightarrow$ son **EL diminue**. **L'equity est « longue corrélation »**.
- Simultanément $P(L>15\%)$ **bondit** (0,23 % $\to$ 7,8 %) : les pertes extrêmes deviennent **bien plus probables** $\Rightarrow$ le **senior** est touché **plus souvent** $\Rightarrow$ son **EL augmente**. **Le senior est « court corrélation ».**

> **Résultat central (à retenir absolument).** À EL de portefeuille **constante**, une **hausse de $\rho$** **augmente** l'EL (et le *spread* équitable) de la tranche **senior** et **diminue** celle de la tranche **equity**. La **sous-estimation de $\rho$** (défauts subprime supposés peu corrélés, prix immobiliers ne baissant pas partout ensemble) a fait noter **AAA** des tranches en réalité très exposées.

### 6.4 Corrélation implicite, *base correlation* et limites

On **inverse** le modèle : le prix d'une tranche donne sa **corrélation implicite**. Comme la volatilité implicite des options, on observe un ***correlation skew/smile*** — signe que le modèle gaussien à un facteur est **trop simple**. Pour la cohérence inter-tranches, le marché utilise la ***base correlation*** (corrélation des tranches equity « empilées » $0$–$D$). **Limite fondamentale** : la copule gaussienne **sous-estime la dépendance de queue** (défauts conjoints extrêmes) — défaut technique au cœur de la crise.

## 7. *Originate-to-distribute*, aléa moral et la chaîne subprime

### 7.1 Le changement de modèle et l'incitation perverse

En ***originate-to-hold***, la banque **conserve** ses prêts : elle a tout intérêt à **bien sélectionner** et **suivre** (son capital est en jeu). Le transfert du risque fait basculer vers l'***originate-to-distribute*** : on **octroie puis revend**. L'incitation à la **sélection (*screening*)** et au **suivi (*monitoring*)** s'**effondre**. C'est un **aléa moral** classique, doublé d'**antisélection** (marché des *lemons* d'Akerlof : l'initiateur connaît mieux la qualité que l'acheteur). Réponse réglementaire : le ***skin in the game*** (rétention $\approx 5\%$) pour réaligner les intérêts.

### 7.2 La chaîne de transformation et son opacité

1. **Prêts subprime** sur l'hypothèse de **hausse perpétuelle** des prix (permettant le refinancement).
2. **RMBS** : titrisation de ces prêts.
3. **CDO d'ABS** : retitrisation des **tranches mezzanine** de RMBS en un nouveau CDO, dont on extrait encore des tranches **« AAA »**.
4. **CDO$^2$** : retitrisation de tranches de CDO. **Plus personne ne sait** quels prêts sous-tendent un titre.

Transformer une majorité de BBB en AAA n'est possible **que** sous **faible corrélation** (§6). Quand $\rho$ a bondi, **toute la chaîne** s'est effondrée **simultanément**.

### 7.3 Agences de notation et « la formule qui a tué Wall Street »

- **Conflit d'intérêts** : modèle ***issuer-pays*** (payées par l'émetteur), conseillant parfois la structuration qu'elles notaient.
- **Modèles défaillants** : valorisation par **copule gaussienne** (popularisée par **David X. Li**, 2000), **corrélations historiques faibles**, **sans dépendance de queue**. Elle a **systématiquement sous-estimé** les défauts conjoints extrêmes.

> **Point d'examen.** Le transfert **ne fait pas disparaître** le risque agrégé : il le **disperse, l'opacifie**, peut **dégrader sa qualité** (aléa moral) et le **concentrer** chez des acteurs mal capitalisés. Leçon : *skin in the game*, transparence, défiance envers les notations adossées à des modèles de corrélation.

## 8. Réglementation post-crise

- **Rétention du risque (*skin in the game*)** : initiateur obligé de **garder $\approx 5\%$** du risque titrisé (Dodd-Frank, CRR européen).
- **Cadre de titrisation de Bâle** (révisé) : pondérations plus élevées et plus sensibles au risque ; label **STS** (*Simple, Transparent, Standardised*) en Europe pour les titrisations de qualité.
- **CVA** (*Credit Valuation Adjustment*) : charge de capital **Bâle III** pour le **risque de contrepartie** des dérivés (dont les CDS) — réponse directe aux pertes de CVA monolines.
- **Compensation centrale (CCP)** des dérivés standardisés (indices CDS), **marges** obligatoires, **transparence** des transactions (*trade repositories*) : réduisent le **risque de contrepartie** et l'**opacité**.
- **Notations** : encadrement des agences, réduction des **références réglementaires automatiques** aux notations.

## 9. Formulaire (notation GARP)

$$EL = PD\times LGD\times EAD,\quad LGD=1-R,\quad Q(t)=e^{-\lambda t},\quad PD(0,t)=1-e^{-\lambda t}$$
$$s\approx\lambda(1-R)\ \Leftrightarrow\ \lambda\approx\frac{s}{1-R},\qquad PV_{\text{prem}}=s\cdot RPV01,\qquad PV_{\text{prot}}=(1-R)\sum_i DF(t_i)\big[Q(t_{i-1})-Q(t_i)\big]$$
$$s^{*}=\frac{PV_{\text{prot}}}{RPV01},\qquad \mathrm{MtM}_{\text{ach}}\approx(s_1-s_0)\,RPV01\,\text{Notionnel},\qquad \text{Basis}=s_{\text{CDS}}-s_{\text{obligation}}$$
$$\text{Tranche }[A,D]:\ \text{Loss}=[\min(L,D)-A]^{+},\quad f=\frac{[\min(L,D)-A]^{+}}{D-A}$$
$$A_i=\sqrt{\rho}\,M+\sqrt{1-\rho}\,\varepsilon_i,\quad p(m)=N\!\left[\frac{N^{-1}(PD)-\sqrt{\rho}\,m}{\sqrt{1-\rho}}\right]$$
$$P(L\le\ell)=N\!\left[\frac{\sqrt{1-\rho}\,N^{-1}(\ell)-N^{-1}(PD)}{\sqrt{\rho}}\right],\qquad WCDR=N\!\left[\frac{N^{-1}(PD)+\sqrt{\rho}\,N^{-1}(1-\alpha)}{\sqrt{1-\rho}}\right]$$

**À retenir** : EL de portefeuille **indépendante de $\rho$** ; $\rho\uparrow \Rightarrow EL_{\text{senior}}\uparrow$ et $EL_{\text{equity}}\downarrow$ ; la copule gaussienne **sous-estime la dépendance de queue**.

---

# Partie B — $\approx\!50$ études de cas

> Chaque cas relie un **épisode** à son **mécanisme** et à sa **leçon**. Les cas chiffrés (25–30) refont les démonstrations sur des situations concrètes.

### Cas 1 — JPMorgan & BISTRO (1997) : la naissance du transfert synthétique
**Faits.** JPMorgan structure **BISTRO** (*Broad Index Secured Trust Offering*), considéré comme le **premier CDO synthétique** : la banque achète une protection de crédit sur un large portefeuille de prêts d'entreprise via un SPV, transférant le risque aux marchés **sans céder les prêts**. **Mécanisme.** Libère du **capital réglementaire** tout en gardant la relation client. **Leçon.** Acte de naissance du transfert de crédit moderne — formidable outil de gestion du capital… qui, mal encadré (§7), nourrira les excès de 2007.

### Cas 2 — Exxon / EBRD (1994) : le premier CDS moderne
**Faits.** Pour couvrir une ligne de crédit accordée à **Exxon** (risque lié à la marée noire de l'*Exxon Valdez*), JPMorgan transfère le risque de défaut à la **BERD** contre une prime : l'un des **premiers CDS**. **Leçon.** Le CDS est né comme **outil de couverture** et de gestion du capital bancaire, bien avant son usage spéculatif.

### Cas 3 — Enron (2001) : le transfert qui a (un peu) protégé
**Faits.** À la faillite d'Enron, les pertes de crédit étaient **dispersées** via dérivés et titrisations chez de nombreux acteurs. **Leçon nuancée.** Le transfert a **réparti** le choc (aucun prêteur unique terrassé) — l'argument « positif » du CRT — mais a aussi montré l'**opacité** : nul ne savait **qui** portait quoi. Préfiguration des deux visages de 2008.

### Cas 4 — AIG : vendre la protection de trop (2008)
**Faits.** **AIG Financial Products** a vendu des protections (CDS et garanties) sur $\approx 500$ Md\$, dont $\approx 78$ Md\$ sur des tranches **super-senior** de CDO subprime, fort de la note **AAA** d'AIG. **Mécanisme de la catastrophe.** Des **déclencheurs de collatéral (CSA)** imposaient de **poster du cash** si (a) les **marks** des CDO baissaient et/ou (b) la **note d'AIG** était dégradée. En septembre 2008, **les deux** surviennent : appels de collatéral de **dizaines de milliards** qu'AIG, illiquide, ne peut honorer. La Fed ouvre $\approx 85$ Md\$ (soutien total $\approx 182$ Md\$) ; **Maiden Lane III** rachète $\approx 62$ Md\$ de CDO. **Leçons.** (1) ***wrong-way risk*** ; (2) **liquidité** née des déclencheurs de collatéral ; (3) **concentration** systémique chez un seul acteur ; (4) **faux sentiment de sécurité** (AAA + super-senior).

### Cas 5 — Les *monolines* (MBIA, Ambac, FGIC) : la garantie qui se retourne
**Faits.** Assureurs de **municipal bonds**, les monolines ont étendu leurs ***wraps*** au crédit structuré. La détérioration du structuré a gonflé leurs engagements **au-delà de leur capital**. **Mécanisme.** Dégradés de **AAA**, ils ont (a) fait **cascader** la dégradation sur tout ce qu'ils garantissaient (y compris des munis sains) et (b) infligé aux banques d'importantes **pertes de CVA** (la couverture ne valait plus rien). **Leçon.** *Wrong-way risk* + garantir un risque de **queue corrélé** avec un capital **mince** $\Rightarrow$ origine de la charge **CVA** de Bâle III.

### Cas 6 — ABACUS / Goldman–Paulson (2010) : le conflit de sélection
**Faits.** Dans un **CDO synthétique** référençant des RMBS, le hedge fund **Paulson & Co.** aurait contribué à **sélectionner** le panier **tout en le shortant**, les investisseurs (IKB, ACA) étant à l'achat. Règlement SEC $\approx 550$ M\$. **Leçon.** **Conflit d'intérêts** dans la sélection du collatéral ; le transfert synthétique est un **jeu à somme nulle** où la partie **informée** peut faire face à l'investisseur.

### Cas 7 — Les hedge funds de Bear Stearns (2007) : le levier sur CDO
**Faits.** Deux fonds de Bear Stearns, fortement **endettés** sur des **tranches de CDO subprime**, s'effondrent à l'été 2007 ; pertes quasi totales pour les investisseurs. **Mécanisme.** **Levier** $\times$ **illiquidité** des tranches $\times$ **appels de marge** lorsque les prix chutent. **Leçon.** Premier **signal d'alarme** de la crise ; le risque transféré était **reconcentré** chez des acteurs à fort levier.

### Cas 8 — Merrill Lynch et le *super-senior* conservé (2007–08)
**Faits.** Pour fabriquer des CDO, les banques peinaient à **placer** les énormes tranches **super-senior** (peu rémunérées) : beaucoup les ont **conservées** au bilan (ou « assurées » auprès de monolines). Merrill a passé des **dépréciations massives** dessus. **Leçon.** Le modèle *originate-to-distribute* a échoué à **distribuer le haut de la structure** : le risque « AAA » est **revenu** au bilan des arrangeurs.

### Cas 9 — Northern Rock (2007) : la titrisation comme financement
**Faits.** Northern Rock finançait ses prêts via la **titrisation** (véhicule **Granite**) et le **gros court terme**, peu par dépôts. **Mécanisme.** Au **gel** du marché de titrisation (été 2007), plus moyen de **placer** les titres ni de **renouveler** le financement $\to$ **crise de liquidité** $\to$ **première ruée** bancaire britannique depuis plus d'un siècle $\to$ **nationalisation**. **Leçon.** La titrisation **comme source de financement** crée un **risque de liquidité de financement** aigu quand le marché se referme.

### Cas 10 — L'enchère CDS de Lehman (2008) : le règlement à grande échelle
**Faits.** À la faillite de Lehman, l'**enchère ISDA** fixe un prix de récupération **très bas** ($\approx 8{,}6\%$), soit une indemnisation $\approx 91{,}4\%$ du notionnel. Malgré un notionnel **brut** colossal, le règlement **net** (après *netting*) s'est élevé à $\approx 5{,}2$ Md\$. **Leçon.** (1) L'**enchère** permet de dénouer des milliers de contrats sans squeeze ; (2) le notionnel **brut** surestime largement le risque réel : c'est le **net** qui compte (argument clé du *netting* et des CCP).

### Cas 11 — Delphi (2005) : le squeeze de livraison physique
**Faits.** À la faillite de l'équipementier **Delphi**, le **notionnel de CDS** dépassait de loin la **dette livrable** : les acheteurs de protection (règlement **physique**) se sont **disputés** les obligations, faisant **grimper** leur prix au-dessus de la valeur de recouvrement réelle. **Leçon.** Déclencheur du passage généralisé au **règlement par enchère cash**, qui découple l'indemnisation de la rareté du titre.

### Cas 12 — Le CDS souverain grec (2012) : qu'est-ce qu'un événement de crédit ?
**Faits.** Lors de la **restructuration** grecque (PSI, décote $\approx 53{,}5\%$), l'usage de **clauses d'action collective** rendant l'échange **contraignant** a conduit l'**ISDA** à déclarer un **événement de crédit** ; l'enchère a fixé un recouvrement $\approx 21{,}5\%$. **Leçon.** Un CDS ne paie que si l'événement coche les **définitions ISDA** : une restructuration « volontaire » initiale **ne déclenche pas**, mais une **CAC** forçant les récalcitrants, **oui**. La **définition contractuelle** prime — risque de **base** entre couverture et perte économique.

### Cas 13 — Argentine (2014) : un défaut « technique » et le *pari passu*
**Faits.** Bloquée par une décision de justice américaine (clause ***pari passu*** au profit des *holdouts*), l'Argentine, pourtant **disposée à payer** ses créanciers restructurés, ne peut le faire : l'ISDA déclare un **événement de crédit** et les CDS paient. **Leçon.** Un événement de crédit peut survenir sans **insolvabilité** classique : le **défaut de paiement** (même « contraint ») suffit. Le CDS suit la **lettre** du contrat, pas l'**intention** économique.

### Cas 14 — GM & Ford (mai 2005) : la première crise de corrélation
**Faits.** La **dégradation** surprise de **GM** et **Ford** en catégorie spéculative provoque un **choc idiosyncratique** : les *spreads* de ces noms explosent **sans** mouvement d'ensemble du marché. Les *desks* de **corrélation** (vendeurs de tranches **equity**, couverts en **mezzanine**) subissent des pertes : la corrélation implicite **chute**, l'equity perd, la couverture mezzanine ne protège pas. **Leçon.** Les **tranches** sont des paris sur la **corrélation** ; un choc **idiosyncratique** (peu corrélé) est le pire scénario pour un vendeur d'equity couvert — répétition générale du risque de modèle de 2008.

### Cas 15 — JPMorgan « Baleine de Londres » (2012) : le crédit synthétique qui dérape
**Faits.** L'unité **CIO** de JPMorgan accumule d'énormes positions sur l'**indice CDX IG9** (crédit synthétique), censées « couvrir », en réalité directionnelles et **trop grandes pour le marché**. Pertes $\approx 6{,}2$ Md\$. **Mécanisme.** **Taille** illiquide $+$ **VaR** recalibrée qui **sous-estime** le risque $+$ contrôles faibles. **Leçon.** Les **indices CDS** sont des outils puissants mais une position démesurée devient **impossible à dénouer** ; risque de **modèle** et de **gouvernance** (cf. chapitre 9).

### Cas 16 — Archegos (2021) : le levier caché par *total return swaps*
**Faits.** Le *family office* **Archegos** prend, via des **TRS** auprès de plusieurs *prime brokers*, une exposition actions **massive et à fort levier** — invisible car détenue **en swap** (pas en titres). À la baisse des sous-jacents, les **appels de marge** déclenchent des **ventes forcées** ; pertes des banques $> 10$ Md\$ (dont $\approx 5{,}5$ Md\$ pour **Credit Suisse**). **Leçon.** Le **TRS** transfère le risque **et** procure un **levier** **opaque** ; chaque courtier ignorait l'exposition **agrégée** $\Rightarrow$ *wrong-way* et concentration. Pont direct avec le **risque de contrepartie** (§8) et le book *Liquidity* (Partie II).

### Cas 17 — Credit Suisse / Greensill (2021) : le financement de chaîne d'appro titrisé
**Faits.** Les fonds **supply-chain finance** de Credit Suisse, adossés à des créances originées par **Greensill**, se sont révélés exposés à des créances **concentrées, illiquides et mal garanties** ; gel et pertes pour les investisseurs. **Leçon.** Reconditionner des créances en produits « sûrs » **ne supprime pas** le risque sous-jacent ; **concentration**, **qualité de l'actif** et **assurance-crédit** défaillante refont surface — écho moderne de 2008.

### Cas 18 — Washington Mutual (2008) : faillite bancaire et CDS
**Faits.** Plus grande faillite de caisse d'épargne américaine ; l'enchère CDS fixe un recouvrement bas. **Leçon.** Même la **dette senior bancaire**, jugée sûre, peut voir un recouvrement **faible** : le CDS reflète la **structure de capital** réelle, pas la réputation.

### Cas 19 — Magnetar (2006–07) : parrainer un CDO pour le shorter
**Faits.** Le hedge fund **Magnetar** aurait **financé la tranche equity** de CDO (la plus risquée) **tout en shortant** les tranches mezzanine — pari net **gagnant si tout s'effondre**, en aidant à **créer** l'offre de CDO. **Leçon.** Quand l'**investisseur de la tranche la plus risquée** souhaite l'**échec** de la structure, les incitations sont **inversées** — illustration extrême de l'**aléa moral** et du **conflit d'intérêts** du CRT synthétique.

### Cas 20 — L'indice ABX (2007) : le signal de marché
**Faits.** L'**ABX.HE** (indice de CDS sur RMBS subprime) **chute** dès le début 2007, **bien avant** que les agences ne dégradent massivement les titres. **Leçon.** Les **dérivés de crédit** (et leurs indices) offrent un **signal de prix** **plus rapide** que les notations — argument pour la transparence, mais aussi pour ne pas s'en remettre aux seules agences.

### Cas 21 — Interdiction des CDS souverains *naked* en UE (2012)
**Faits.** Après la crise de la dette de la zone euro, l'UE **interdit** l'achat de **CDS souverains à nu** (*naked*, sans détenir la dette correspondante), accusé d'**amplifier** la spéculation contre les États. **Leçon.** Frontière réglementaire entre **couverture légitime** et **spéculation** ; débat persistant sur l'effet d'un tel CDS sur les *spreads* souverains.

### Cas 22 — La résilience des CLO (2008 et 2020)
**Faits.** Contrairement aux **ABS CDO** subprime, les **CLO** (prêts d'entreprise à levier) ont **largement tenu** en 2008 **et** lors du choc **COVID 2020** : très peu de défauts de tranches AAA. **Mécanisme.** **Granularité**, **gestion active**, **rehaussement** robuste et **collatéral mieux compris** (prêts seniors garantis). **Leçon.** La titrisation **n'est pas mauvaise en soi** : tout dépend de la **qualité du collatéral**, de la **structure** et de la **transparence**. Nuance essentielle pour ne pas diaboliser tout le CRT.

### Cas 23 — Les CDPC : des vendeurs de protection trop minces
**Faits.** Les ***Credit Derivative Product Companies*** étaient des véhicules **faiblement capitalisés** vendant en masse de la protection super-senior, censés rester **AAA** par leur structure. La crise a révélé leur **sous-capitalisation** face à un risque **corrélé**. **Leçon.** Variante des monolines : **vendre de l'assurance de queue** sans capital suffisant est intenable quand la **corrélation** se matérialise.

### Cas 24 — Citigroup et les *liquidity puts* (2007)
**Faits.** Citi avait vendu des **options de liquidité** obligeant la banque à **racheter** des CDO si leur financement de marché disparaissait. Au gel du marché, ces puts ont **ramené des dizaines de milliards** de CDO **au bilan**. **Leçon.** Une **exposition contingente** (hors-bilan) peut **revenir** brutalement : le risque transféré « sur le papier » reste **attaché** par des clauses cachées — leçon de **risque de liquidité** et de transparence.

### Cas 25 — Chiffré : *bootstrap* d'une courbe CDS à deux points
**Données.** CDS 1 an à **100 bps**, CDS 2 ans à **150 bps**, $R=40\%$.
$$\lambda(0,1)\approx\frac{0{,}0100}{0{,}60}=1{,}667\%,\quad Q(1)=e^{-0{,}01667}=0{,}9835,\quad PD(0,1)=1{,}65\%.$$
$$\lambda(0,2)\approx\frac{0{,}0150}{0{,}60}=2{,}50\%,\quad Q(2)=e^{-0{,}025\times2}=0{,}9512,\quad PD(0,2)=4{,}88\%.$$
**Intensité *forward* de l'année 2** : $\lambda_{2}=-\ln\!\big(Q(2)/Q(1)\big)=-\ln(0{,}9672)=3{,}34\%$ (ou $2\times2{,}50\%-1{,}667\%$). **Leçon.** Une courbe **pentue** se *bootstrappe* en intensités *forward* **croissantes** — le marché anticipe un risque **plus élevé à terme**.

### Cas 26 — Chiffré : EL des tranches selon la corrélation (equity vs senior)
**Données.** $PD=5\%$, $LGD=100\%$ ; equity **0–3 %**, senior **15 %+**. On compare $\rho=0{,}10$ et $\rho=0{,}30$ via la loi de Vasicek.

| Probabilité | $\rho=0{,}10$ | $\rho=0{,}30$ | Effet |
|---|---|---|---|
| $P(L>3\%)$ — equity **anéantie** | 67,1 % | 44,8 % | $\downarrow$ : equity **survit plus** $\Rightarrow EL_{eq}\downarrow$ |
| $P(L>15\%)$ — senior **entamé** | 1,8 % | 7,8 % | $\uparrow$ : senior touché **plus** $\Rightarrow EL_{sen}\uparrow$ |

**Détail** ($P(L>3\%)$, $\rho=0{,}10$) : $F(0{,}03)=N\!\big[(\sqrt{0{,}9}\,(-1{,}881)+1{,}645)/\sqrt{0{,}1}\big]=N[-0{,}443]=0{,}329$, donc $P(L>3\%)=67{,}1\%$. **Leçon chiffrée du résultat central** : à EL de portefeuille **inchangée** ($=5\%$), $\rho\uparrow$ **réduit** l'EL de l'equity (longue corrélation) et **augmente** celle du senior (courte corrélation).

### Cas 27 — Chiffré : CVA sur une contrepartie CDS
**Données.** Une banque **achète** une protection (notionnel 10 M€). Exposition positive attendue sur la vie du contrat $EE\approx 1{,}0$ M€ ; PD de la **contrepartie** sur l'horizon $=3\%$ ; $LGD_{\text{cpty}}=60\%$.
$$CVA \approx EE\times PD_{\text{cpty}}\times LGD_{\text{cpty}} = 1\,000\,000\times0{,}03\times0{,}60 = 18\,000\ \text{€}.$$
La protection « vaut » donc **18 000 € de moins** que sa valeur sans risque de contrepartie. **Leçon.** En ***wrong-way risk*** (cas monolines/AIG), $EE$ **et** $PD_{\text{cpty}}$ **montent ensemble** $\Rightarrow$ CVA bien plus élevé — d'où la charge **CVA** de Bâle III.

### Cas 28 — Chiffré : le trade de base négative et son piège de financement
**Données.** Obligation Z-spread $=180$ bps, CDS $=150$ bps $\Rightarrow$ **base $=-30$ bps**. On *achète l'obligation + achète la protection* : crédit neutralisé, **portage $\approx +30$ bps** ($30\,000$ €/an sur 10 M€). **Le piège (2008).** Porter l'obligation exige du **financement (repo)** ; quand les **décotes de repo explosent** et que le financement se **tarit**, on doit **déboucler** alors que la base devient **très négative** (ex. $-250$ bps). MtM $\approx (0{,}0250-0{,}0030)\times RPV01(4)\times10\,\text{M} = 880\,000$ € de **perte**. **Leçon.** L'« arbitrage » sans risque de **crédit** portait un **risque de liquidité/financement** — concrétisé en 2008.

### Cas 29 — Chiffré : capital IRB (Bâle) via la WCDR
**Données.** Portefeuille $PD=1\%$, $\rho=0{,}15$, $LGD=45\%$, confiance $99{,}9\%$.
$$WCDR=N\!\left[\frac{N^{-1}(0{,}01)+\sqrt{0{,}15}\,N^{-1}(0{,}999)}{\sqrt{0{,}85}}\right]=N\!\left[\frac{-2{,}326+1{,}196}{0{,}922}\right]=N[-1{,}226]\approx11{,}0\%.$$
$$\text{Capital} \approx LGD\times(WCDR-PD)=0{,}45\times(0{,}110-0{,}010)=4{,}5\%\ \text{de l'EAD}.$$
**Leçon.** C'est l'**âme de l'IRB** : le capital couvre l'**UL** $=LGD\times(WCDR-PD)$, l'**EL** étant provisionnée. La même mécanique de Vasicek sert à valoriser les tranches **et** à fixer le capital réglementaire.

### Cas 30 — Chiffré : anatomie d'une tranche « AAA » qui saute
**Tranche senior attachant à $A=15\%$** (protégée tant que $L<15\%$).
**À la notation** (hypothèses bénignes, $PD=2\%$, $\rho=0{,}10$) :
$$P(L>15\%)=1-N\!\left[\frac{\sqrt{0{,}90}\,(-1{,}036)+2{,}054}{\sqrt{0{,}10}}\right]=1-N[3{,}389]=0{,}035\%\ \Rightarrow\ \text{compatible AAA}.$$
**En crise** ($PD=12\%$, $\rho=0{,}35$) :
$$P(L>15\%)=1-N\!\left[\frac{\sqrt{0{,}65}\,(-1{,}036)+1{,}175}{\sqrt{0{,}35}}\right]=1-N[0{,}575]=28{,}3\%.$$
**Leçon.** La **même** tranche « sans risque » avait en réalité **$\approx 28\%$** de probabilité d'être entamée : **deux** paramètres ont dérapé **ensemble** ($PD$ : 2 %$\to$12 % ; $\rho$ : 0,10$\to$0,35). C'est, en une démonstration, **l'histoire de 2007-2008**.

---

### Cas 31 — Panier *first-to-default* (FTD) et corrélation
**Mécanique.** Un **FTD** sur 5 noms paie sur le **premier** défaut du panier, puis s'éteint. **Effet corrélation.** À faible corrélation, **n'importe quel** nom peut déclencher $\Rightarrow$ le spread FTD s'approche de la **somme** des spreads ; à forte corrélation, les défauts sont groupés $\Rightarrow$ le FTD se comporte comme un **seul** nom (spread plus bas). **Leçon.** Le vendeur de FTD est **court corrélation** ; le produit **concentre** le risque de défaut du panier.

### Cas 32 — Le *trade* de corrélation (equity vs mezzanine)
**Mécanique.** Un trader **vend** la tranche **equity** (long corrélation) et **achète** la protection **mezzanine** (short corrélation) pour parier sur la **corrélation implicite**, en **delta-hedgeant** l'exposition directionnelle au spread d'indice. **Leçon.** Les tranches synthétiques créent un **marché de la corrélation** ; la **base correlation** (§6.4) sert à coter ces trades de façon cohérente.

### Cas 33 — La dislocation de corrélation de mai 2005
**Faits.** La dégradation **idiosyncratique** de **GM et Ford** fait **bondir** la tranche equity (pertes attendues des juniors) tout en **resserrant** les tranches senior : la **corrélation implicite** s'effondre, piégeant les *correlation desks* **longs equity / shorts mezz**. **Leçon.** Un choc **spécifique** peut **casser** un modèle de corrélation : le risque de **modèle** et de **base** est réel, distinct du risque de défaut.

### Cas 34 — L'indice ABX et le « *big short* »
**Faits.** L'indice **ABX.HE** rend le subprime **shortable** : des investisseurs **achètent la protection** sur des tranches RMBS/CDO surévaluées avant l'effondrement de 2007. **Leçon.** Les indices de crédit **révèlent les prix** et permettent d'**exprimer une vue baissière** — la titrisation devient un **champ de bataille** haussiers/baissiers.

### Cas 35 — Le CPDO : un AAA qui implose
**Faits.** Les **Constant Proportion Debt Obligations** vendaient de la protection d'indice avec **fort levier**, notés **AAA** ; la volatilité de 2007-2008 les a **anéantis**. **Leçon.** Une **note AAA** apposée sur une stratégie **à levier vendeuse de risque systématique** est **trompeuse** : la note ne capture pas le risque de **queue**.

### Cas 36 — Les SIV et la ruée du *shadow banking* (2007)
**Faits.** Les **Structured Investment Vehicles** finançaient des actifs longs (ABS) par du **papier commercial** court (ABCP) ; le gel de l'ABCP en 2007 a forcé des **liquidations** et le rapatriement au bilan des banques. **Leçon.** Titrisation + **transformation d'échéances** hors bilan $=$ **risque de liquidité systémique** — même mécanisme que Northern Rock, à l'échelle du *shadow banking*.

### Cas 37 — Les TruPS CDO et le capital bancaire
**Faits.** Des CDO de **titres hybrides** (*trust-preferred*) de petites banques ont concentré un risque **fortement corrélé** au secteur bancaire ; la crise de 2008-2009 les a dévastés. **Leçon.** Titriser des expositions **toutes exposées au même facteur** (santé des banques) **n'élimine pas** le risque systématique — il le **déguise**.

### Cas 38 — Le « *Big Bang* » CDS de 2009
**Faits.** La réforme **ISDA 2009** standardise les CDS : **coupons fixes** (100/500 bps), **paiement *upfront***, **comités de détermination** des événements de crédit, **enchères** de règlement. **Leçon.** La **standardisation** rend les CDS **fongibles** et **compensables** — préalable à la **compensation centrale** et à la réduction du risque de contrepartie.

### Cas 39 — La compensation centrale des indices CDS
**Faits.** Après 2008, les **indices CDS** (CDX, iTraxx) passent par des **chambres de compensation** (ex. ICE Clear Credit) : **novation**, **marges** initiales et de variation, **mutualisation**. **Leçon.** La **CCP** réduit le risque de **contrepartie** bilatéral et l'**opacité**, mais **concentre** le risque dans la chambre (qui devient **systémique**).

### Cas 40 — La rétention au risque (*Dodd-Frank* 941 / UE)
**Faits.** La réglementation post-crise impose à l'initiateur de **conserver $\approx 5\%$** de l'exposition titrisée (*skin in the game*). **Leçon.** Réaligner les **incitations** de l'*originate-to-distribute* (§7.1) : celui qui **structure** doit **garder une part** du risque qu'il crée.

### Cas 41 — La règle Volcker et la niche CLO
**Faits.** La **règle Volcker** limite les positions des banques ; son application aux **CLO** (selon qu'ils détiennent des obligations) a nécessité des **ajustements** de structure. **Leçon.** La **frontière réglementaire** entre prêt titrisé « sain » et « pari » spéculatif est **mouvante** et façonne l'**ingénierie** des produits.

### Cas 42 — Le déclin de liquidité des CDS mononom
**Faits.** Après 2008, le marché des **CDS mononom** (hors indices) s'est **contracté** : moins de teneurs de marché, coûts de capital accrus. **Leçon.** Un instrument de **couverture** peut **perdre en liquidité** ; le risque de **liquidité de marché** doit être intégré à toute stratégie de transfert.

### Cas 43 — Base CDS-obligation sur souverains
**Faits.** Sur les **souverains**, la **base** (CDS − obligation) peut diverger fortement (contraintes de *repo*, interdiction des CDS **à nu**, segmentation). **Leçon.** La **base** n'est pas un pur arbitrage : elle rémunère des **frictions** (financement, réglementation) — cf. l'interdiction UE 2012 des CDS souverains nus.

### Cas 44 — *Wrong-way risk* : vendre des CDS sur ses banques domestiques
**Mécanique.** Une entité qui **vend** de la protection sur les **banques de son propre pays** cumule une exposition qui **augmente** précisément quand sa contrepartie (souvent une banque locale) **s'affaiblit** — corrélation défavorable extrême. **Leçon.** Le ***wrong-way risk*** (§2.5) peut être **structurel** et **systémique**, pas seulement contractuel.

### Cas 45 — Chiffré : spread d'un FTD selon la corrélation
**Données.** Panier de 5 noms, spread individuel $\approx 120$ bps chacun.
$$\text{Borne haute (}\rho\to 0\text{)} \approx 5\times120=600\ \text{bps};\qquad \text{Borne basse (}\rho\to 1\text{)} \approx 120\ \text{bps}.$$
Pour une corrélation **modérée**, le FTD cote **entre** les deux (ex. $\approx 450$ bps). **Leçon.** Le spread FTD **décroît** avec la corrélation : vendre un FTD, c'est être **court corrélation** et concentrer le défaut.

### Cas 46 — Chiffré : *upfront* d'un CDS indiciel (convention *Big Bang*)
**Données.** Coupon fixe **100 bps**, spread de marché **150 bps**, $RPV01=4{,}5$, notionnel **10 M€**.
$$\text{Upfront} \approx (s_{\text{mkt}}-c)\times RPV01\times N = 0{,}0050\times4{,}5\times10{,}000{,}000 = 225\,000\ \text{€}.$$
**Leçon.** Le marché $>$ coupon $\Rightarrow$ l'**acheteur** de protection paie un ***upfront*** ; la standardisation **fongibilise** les positions.

### Cas 47 — Chiffré : *trade* de base négative et son portage
**Données.** Obligation **Z-spread** 180 bps ; **CDS** 150 bps $\Rightarrow$ **base** $=-30$ bps. Notionnel **10 M€**.
$$\text{Portage} \approx 30\ \text{bps} \times 10\,\text{M€} = 30\,000\ \text{€/an (avant frictions)}.$$
**Leçon.** On **achète l'obligation + la protection** : crédit quasi neutralisé, on **capte la base**, sous réserve du **financement** et du risque de **contrepartie** sur le vendeur de CDS.

### Cas 48 — Chiffré : sensibilité au taux de recouvrement
**Données.** *Credit triangle* $s\approx\lambda(1-R)$. À $\lambda$ implicite constant, $R$ passe de **40 %** à **60 %**.
$$\frac{s_{\text{new}}}{s_{\text{old}}}=\frac{1-0{,}60}{1-0{,}40}=\frac{0{,}40}{0{,}60}=0{,}667\ \Rightarrow\ 150\ \text{bps}\to 100\ \text{bps}.$$
**Leçon.** Un **recouvrement plus élevé** réduit le spread : l'hypothèse de **$R$** est **cruciale** pour valoriser et couvrir un CDS.

### Cas 49 — Chiffré : couvrir une obligation par CDS (ratio de couverture)
**Données.** Obligation **10 M€** au **pair**, duration de spread $\approx$ celle du CDS. Couverture : **acheter 10 M€** de protection.
$$\text{Ratio}\approx 1\ (\text{pour un pair}) ;\quad \text{résiduel} = \text{risque de base} + \text{recouvrement/CTD}.$$
**Leçon.** La couverture par CDS **neutralise l'essentiel** du risque de crédit mais **laisse** une **base** et un risque de **livraison** (*cheapest-to-deliver*).

### Cas 50 — Chiffré : delta de la tranche equity à la corrélation
**Données.** Tranche **equity** synthétique. Quand la corrélation implicite **augmente** de $\rho_1$ à $\rho_2$, l'**EL de l'equity diminue** $\Rightarrow$ sa **valeur** (côté vendeur) **augmente**.
$$\text{Signe} : \frac{\partial\,EL_{\text{equity}}}{\partial\rho}<0 \quad\Rightarrow\quad \text{vendeur equity} = \textbf{long corrélation}.$$
**Leçon.** Le **delta-corrélation** de l'equity est **positif** pour le vendeur ; c'est la brique des *trades* de corrélation (Cas 32) et le résultat central du §6.3.

# Partie C — Banque de $\approx\!150$ QCM (format examen)

> QCM en anglais, **une seule bonne réponse**. Présentation **aérée** (chaque option sur sa propre ligne). **Corrigé commenté en français** en **Partie D**, avec renvois aux sections (§). *Questions originales au format examen ; le GARP ne publie pas ses annales.*

## C.1 — Briques quantitatives & EL

**Q1.** The credit expected-loss identity is:

(A) $PD + LGD + EAD$

(B) $PD \times EAD - LGD$

(C) $PD \times LGD \times EAD$

(D) $PD / LGD$

\medskip

**Q2.** With a recovery rate of 35%, the LGD is:

(A) 35%

(B) 65%

(C) 100%

(D) 20%

\medskip

**Q3.** With $PD = 3\%$, $LGD = 60\%$, $EAD = \$5\text{m}$, the expected loss is:

(A) \$30,000

(B) \$90,000

(C) \$180,000

(D) \$300,000

\medskip

**Q4.** With a constant hazard rate $\lambda = 2\%$, the cumulative PD over 3 years $1-e^{-\lambda t}$ is closest to:

(A) 2.0%

(B) 5.8%

(C) 6.0%

(D) 18.0%

\medskip

**Q5.** Which statement about a portfolio's expected loss is correct?

(A) It increases with default correlation

(B) It decreases with default correlation

(C) It is invariant to default correlation; only the loss distribution's shape changes

(D) It equals the senior tranche loss

\medskip

**Q6.** Unexpected loss (the dispersion around EL) is primarily covered by:

(A) provisions

(B) excess spread

(C) the coupon

(D) economic capital

\medskip

**Q7.** Compared with real-world (historical) PDs, risk-neutral PDs extracted from market prices are generally:

(A) lower

(B) higher (they embed a risk premium)

(C) identical

(D) always zero

\medskip

**Q8.** The hazard rate (default intensity) $\lambda$ is:

(A) the recovery rate

(B) the bond's coupon

(C) the instantaneous probability of default conditional on survival

(D) the par spread

\medskip

## C.2 — CDS : mécanique & *credit triangle*

**Q9.** In a single-name CDS, the protection buyer:

(A) receives a periodic premium

(B) pays a periodic premium and is compensated upon a credit event

(C) must always own the reference obligation

(D) bears no risk at all

\medskip

**Q10.** The CDS protection seller:

(A) pays the premium

(B) is short credit risk

(C) receives the premium and pays out on default (effectively long credit risk)

(D) is fully hedged by construction

\medskip

**Q11.** The credit triangle relates the CDS spread to:

(A) $\lambda + R$

(B) $\lambda(1-R)$

(C) $\lambda / R$

(D) $R - \lambda$

\medskip

**Q12.** With a CDS spread $s = 240$ bps and recovery $R = 40\%$, the implied hazard rate is approximately:

(A) 1.4%

(B) 2.4%

(C) 4.0%

(D) 6.0%

\medskip

**Q13.** With hazard rate $\lambda = 2\%$ and recovery $R = 30\%$, the approximate CDS spread is:

(A) 60 bps

(B) 200 bps

(C) 140 bps

(D) 290 bps

\medskip

**Q14.** A "naked" CDS position refers to:

(A) a fully collateralized swap

(B) buying protection without owning the reference obligation

(C) selling protection on one's own debt

(D) a CDS with no premium leg

\medskip

**Q15.** Which is NOT typically an ISDA credit event?

(A) bankruptcy

(B) failure to pay

(C) restructuring

(D) a one-notch rating downgrade

\medskip

**Q16.** The protection buyer's credit position is best described as:

(A) long credit

(B) short credit (gains if credit quality deteriorates)

(C) neutral to credit

(D) long the risk-free rate

\medskip

**Q17.** With $s = 300$ bps and $R = 25\%$, the implied hazard rate is:

(A) 2.25%

(B) 3.00%

(C) 4.00%

(D) 7.50%

\medskip

**Q18.** A 5-year CDS at $s = 200$ bps with $R = 40\%$ implies a cumulative 5-year PD of approximately:

(A) 3.3%

(B) 10.0%

(C) 15.4%

(D) 20.0%

\medskip

**Q19.** In cash settlement, the recovery rate $R$ used to compute the payout is determined by:

(A) the protection buyer

(B) an ISDA auction held after the credit event

(C) the reference entity

(D) the coupon schedule

\medskip

**Q20.** Auctions largely replaced physical settlement because:

(A) auctions are cheaper to run

(B) physical settlement is illegal

(C) the CDS notional can exceed deliverable debt, causing a delivery squeeze

(D) auctions remove all counterparty risk

\medskip

**Q21.** The RPV01 of a CDS is:

(A) the recovery rate

(B) the risky annuity (the value of 1 bp of premium)

(C) the notional

(D) the hazard rate

\medskip

**Q22.** The par CDS spread $s^{*}$ is:

(A) $RPV01 / PV_{\text{prot}}$

(B) $\lambda + R$

(C) $PV_{\text{prot}} / RPV01$

(D) the coupon minus the recovery

\medskip

**Q23.** When the market CDS spread widens after a buyer entered the trade, the protection buyer's mark-to-market:

(A) falls (a loss)

(B) rises (a gain)

(C) is unchanged

(D) becomes the recovery rate

\medskip

**Q24.** A buyer entered at $s_0 = 100$ bps; the market spread is now $s_1 = 250$ bps; $RPV01 = 4.3$, notional €10m. The mark-to-market is approximately:

(A) −€645,000

(B) +€645,000

(C) +€150,000

(D) −€150,000

\medskip

**Q25.** "Wrong-way risk" in a CDS is:

(A) buying protection on the wrong name

(B) exposure increasing exactly as the counterparty's creditworthiness deteriorates

(C) zero exposure

(D) a settlement error

\medskip

## C.3 — CDS : valorisation, base & *bootstrap*

**Q26.** A protection buyer entered at 120 bps; the market spread has since tightened to 80 bps; $RPV01 = 4.0$, notional €20m. The mark-to-market to the buyer is approximately:

(A) +€320,000

(B) −€320,000

(C) +€640,000

(D) −€640,000

\medskip

**Q27.** A negative CDS-bond basis trade consists of:

(A) selling the bond and selling protection

(B) buying the bond and buying protection (earning roughly the negative basis as carry)

(C) buying the bond and selling protection

(D) shorting both

\medskip

**Q28.** A negative CDS-bond basis means:

(A) the CDS spread is below the bond credit spread

(B) the CDS spread is above the bond spread

(C) the two spreads are equal

(D) recovery is zero

\medskip

**Q29.** Which contributes to a non-zero CDS-bond basis?

(A) counterparty risk on the protection seller

(B) the cost of funding the cash bond

(C) the cheapest-to-deliver option

(D) all of the above

\medskip

**Q30.** A 1-year CDS trades at 120 bps with $R = 40\%$. The implied 1-year PD is closest to:

(A) 1.2%

(B) 2.0%

(C) 3.0%

(D) 6.0%

\medskip

**Q31.** Bootstrapping an upward-sloping CDS curve produces forward default intensities that:

(A) are constant

(B) rise with maturity

(C) fall with maturity

(D) are always negative

\medskip

**Q32.** All else equal, the risky annuity $RPV01$ is lower when:

(A) the spread is lower

(B) the hazard rate is higher, reducing survival probabilities

(C) maturity is longer

(D) interest rates are zero

\medskip

**Q33.** The par spread is the spread that:

(A) maximizes the premium leg

(B) makes the contract's value zero at inception

(C) equals the recovery rate

(D) eliminates counterparty risk

\medskip

**Q34.** A protection buyer who also owns the underlying bond holds:

(A) a doubly long credit position

(B) pure speculation

(C) a hedged (offsetting) credit position

(D) an equity position

\medskip

**Q35.** Index CDS such as CDX and iTraxx are:

(A) bespoke and illiquid

(B) standardized baskets, highly liquid, rolling every 6 months

(C) single-name only

(D) equity indices

\medskip

**Q36.** iTraxx Europe Main references approximately:

(A) 30 high-yield names

(B) 500 names

(C) 125 European investment-grade names

(D) 25 sovereigns

\medskip

**Q37.** The "on-the-run" index series is:

(A) the oldest series

(B) the most recent, most liquid series

(C) a single-name CDS

(D) a defaulted name

\medskip

**Q38.** The CDS-bond basis turned strongly negative in 2008 mainly because:

(A) CDS were banned

(B) recoveries rose

(C) cash bonds became impossible to fund, pushing investors to CDS

(D) spreads fell to zero

\medskip

## C.4 — Autres dérivés de crédit & indices

**Q39.** A Total Return Swap transfers:

(A) only interest rate risk

(B) only operational risk

(C) the total performance — both credit and market risk — of the asset

(D) no risk

\medskip

**Q40.** A Credit-Linked Note (CLN) is:

(A) unfunded, with counterparty risk on the seller

(B) an equity share

(C) prefunded, so there is no counterparty risk on the protection seller

(D) a government bill

\medskip

**Q41.** Compared with a CDS, in a CLN the protection seller:

(A) takes no credit risk

(B) funds the position upfront (collateral is held in the SPV)

(C) pays a premium

(D) cannot lose money

\medskip

**Q42.** The seller of protection on a first-to-default (FTD) basket is:

(A) short correlation

(B) long correlation (higher correlation lowers the chance any one name defaults first)

(C) insensitive to correlation

(D) short the risk-free rate

\medskip

**Q43.** The FTD basket spread is generally:

(A) below the maximum single-name spread

(B) above the sum of the single-name spreads

(C) below the sum of single-name spreads but above the largest individual spread

(D) exactly zero

\medskip

**Q44.** Standardized index tranches (e.g., 0–3%, 3–7%) are primarily used to:

(A) trade interest rates

(B) trade correlation

(C) avoid all risk

(D) replace equities

\medskip

**Q45.** A synthetic CDO differs from a cash CDO mainly because it:

(A) contains actual loans

(B) gains credit exposure through CDS, allowing near-unlimited supply referencing the same names

(C) has no tranches

(D) carries no credit risk

\medskip

**Q46.** A CDO-squared ($\text{CDO}^2$):

(A) contains only cash loans

(B) has a single tranche

(C) securitizes tranches of other CDOs (maximal opacity)

(D) is risk-free

\medskip

**Q47.** A CLO is collateralized primarily by:

(A) residential mortgages

(B) credit card receivables

(C) leveraged corporate loans

(D) sovereign bonds

\medskip

**Q48.** CLOs largely survived both 2008 and 2020 because of:

(A) government guarantees

(B) zero leverage

(C) the absence of tranches

(D) granularity, active management, robust enhancement, and senior secured collateral

\medskip

**Q49.** In a single-tranche (bespoke) CDO, the dealer typically:

(A) sells the whole structure

(B) delta-hedges its correlation exposure on the index

(C) takes no risk

(D) guarantees the tranche

\medskip

**Q50.** The core problem with Credit Derivative Product Companies (CDPCs) was that they were:

(A) over-capitalized

(B) thinly capitalized sellers of correlated tail risk

(C) protection buyers

(D) government agencies

\medskip

## C.5 — Titrisation & *tranching*

**Q51.** A "true sale" in a securitization means:

(A) the originator keeps the assets on its balance sheet

(B) the SPV is consolidated with the originator

(C) the assets leave the originator's balance sheet and the SPV is bankruptcy-remote

(D) the investors guarantee the loans

\medskip

**Q52.** The SPV is structured to be "bankruptcy remote" so that:

(A) it can default freely

(B) the originator's bankruptcy cannot reach the transferred assets

(C) it pays no coupons

(D) it avoids taxes only

\medskip

**Q53.** The loss borne by a tranche $[A,D]$ for a portfolio loss $L$ is:

(A) $L \times (D-A)$

(B) $L - A$

(C) $[\min(L,D)-A]^{+}$

(D) $\max(L,D)$

\medskip

**Q54.** Equity tranche 0–3%; portfolio loss is 2%. The equity loss, as a % of its own notional, is:

(A) 2%

(B) 33%

(C) 66.7%

(D) 100%

\medskip

**Q55.** Mezzanine tranche 3–7%; portfolio loss is 5%. The mezzanine loss, as a % of its own notional, is:

(A) 20%

(B) 50%

(C) 75%

(D) 100%

\medskip

**Q56.** Senior tranche 7–100%; portfolio loss is 10%. The senior loss, as a % of its own notional, is:

(A) 1.0%

(B) 3.23%

(C) 7.0%

(D) 10.0%

\medskip

**Q57.** A €200m portfolio is tranched equity 0–4%, mezzanine 4–9%, senior 9–100%. If the portfolio loss is 6%, the mezzanine loss as a % of its own notional is:

(A) 20%

(B) 40%

(C) 50%

(D) 100%

\medskip

**Q58.** In a securitization, "excess spread" functions as:

(A) the senior coupon

(B) a regulatory charge

(C) a first line of defense that absorbs losses before the equity tranche

(D) the last loss-absorbing layer

\medskip

**Q59.** Overcollateralization (OC) means:

(A) the notes exceed the assets

(B) the asset value exceeds the issued notes (a cushion)

(C) there is no equity tranche

(D) the SPV is unfunded

\medskip

**Q60.** Which is an EXTERNAL credit enhancement?

(A) subordination

(B) overcollateralization

(C) excess spread

(D) a monoline wrap (third-party guarantee)

\medskip

**Q61.** Under sequential principal repayment:

(A) the equity is repaid first

(B) all tranches amortize pro-rata

(C) the senior tranche is repaid first

(D) nothing is repaid until maturity

\medskip

**Q62.** OC/IC coverage tests, when breached, typically:

(A) pay more to the equity

(B) divert cash flows toward the senior tranche

(C) increase leverage

(D) cancel the deal

\medskip

**Q63.** The equity (first-loss) tranche is best described as:

(A) the safest, lowest-yielding tranche

(B) risk-free

(C) the highest-risk, highest-return, first-loss tranche

(D) a senior claim

\medskip

**Q64.** A thin mezzanine tranche is characterized by:

(A) no risk

(B) high leverage (0% to 100% loss over a small portfolio-loss range)

(C) a fixed 1% loss

(D) immunity to correlation

\medskip

**Q65.** A senior tranche's AAA rating depends most critically on the assumption of:

(A) high recovery only

(B) zero interest rates

(C) high default correlation

(D) low default correlation

\medskip

## C.6 — Corrélation & modèle à un facteur (Vasicek)

**Q66.** In the one-factor Gaussian copula, the normalized asset value of borrower $i$ is:

(A) $\sqrt{\rho}\,\varepsilon_i + M$

(B) $\sqrt{\rho}\,M + \sqrt{1-\rho}\,\varepsilon_i$

(C) $\rho M + (1-\rho)\varepsilon_i$

(D) $M \times \varepsilon_i$

\medskip

**Q67.** The default threshold $c$ is calibrated as:

(A) $PD$

(B) $1-PD$

(C) $N^{-1}(PD)$

(D) $N(PD)$

\medskip

**Q68.** The PD conditional on the systematic factor $M=m$ is:

(A) $N[N^{-1}(PD)+\sqrt{\rho}\,m]$

(B) $N^{-1}\!\left[\frac{PD-\sqrt{\rho}\,m}{\sqrt{1-\rho}}\right]$

(C) $N\!\left[\frac{N^{-1}(PD)-\sqrt{\rho}\,m}{\sqrt{1-\rho}}\right]$

(D) $PD \times m$

\medskip

**Q69.** As $m$ becomes more negative (a bad economy), the conditional PD $p(m)$:

(A) decreases toward zero

(B) increases (more simultaneous defaults)

(C) stays equal to the unconditional PD

(D) becomes negative

\medskip

**Q70.** With $PD=5\%$ and $\rho=0.20$, the conditional PD at a "two-sigma bad" factor ($m=-2$) is closest to:

(A) 5%

(B) 10%

(C) 20%

(D) 35%

\medskip

**Q71.** The Vasicek large-portfolio loss distribution ($LGD=1$) is:

(A) $N[\sqrt{\rho}\,N^{-1}(\ell)]$

(B) $N\!\left[\frac{\sqrt{1-\rho}\,N^{-1}(\ell)-N^{-1}(PD)}{\sqrt{\rho}}\right]$

(C) $N[N^{-1}(\ell)+PD]$

(D) $1-e^{-\rho\ell}$

\medskip

**Q72.** The worst-case default rate (WCDR) at confidence $1-\alpha$ is:

(A) $PD \times \sqrt{\rho}$

(B) $N[N^{-1}(PD)]$

(C) $PD + \alpha$

(D) $N\!\left[\frac{N^{-1}(PD)+\sqrt{\rho}\,N^{-1}(1-\alpha)}{\sqrt{1-\rho}}\right]$

\medskip

**Q73.** With $PD=2\%$, $\rho=0.20$, the 99.9% WCDR is closest to:

(A) 2%

(B) 10%

(C) 22.6%

(D) 40%

\medskip

**Q74.** With $PD=1\%$, $\rho=0.15$, the 99.9% WCDR is closest to:

(A) 1%

(B) 6%

(C) 11%

(D) 22%

\medskip

**Q75.** Holding the portfolio's expected loss constant, an increase in default correlation will:

(A) increase the senior tranche's EL and decrease the equity tranche's EL

(B) decrease the senior tranche's EL and increase the equity tranche's EL

(C) increase all tranches' EL equally

(D) leave all tranche ELs unchanged

\medskip

**Q76.** The seller of protection on an equity (first-loss) tranche is:

(A) short correlation

(B) long correlation (benefits when correlation rises)

(C) insensitive to correlation

(D) short the risk-free rate

\medskip

**Q77.** The seller of protection on a senior tranche is best described as:

(A) short correlation

(B) long correlation

(C) insensitive to correlation

(D) long the risk-free rate

\medskip

**Q78.** Under the IRB approach, the capital requirement (per unit EAD) is approximately:

(A) $PD \times LGD$

(B) $WCDR$

(C) $LGD \times (WCDR - PD)$

(D) $LGD \times WCDR \times PD$

\medskip

**Q79.** Base correlation is used because:

(A) the Gaussian copula needs no correlation

(B) all tranches share one correlation

(C) market prices imply different correlations across tranches (a skew), and a consistent, arbitrage-free measure is needed

(D) recovery is unknown

\medskip

**Q80.** The key technical flaw of the Gaussian copula in pricing CDOs was that it:

(A) overstated default correlations

(B) underestimated tail dependence (the probability of joint extreme defaults)

(C) could not handle one tranche

(D) ignored recovery entirely

\medskip

## C.7 — *Originate-to-distribute*, crise & agences

**Q81.** The originate-to-distribute model weakens:

(A) interest rates

(B) lenders' incentives to screen and monitor borrowers

(C) recovery rates

(D) the risk-free curve

\medskip

**Q82.** This weakening of incentives is a form of:

(A) market risk

(B) liquidity risk

(C) moral hazard

(D) operational risk

\medskip

**Q83.** Adverse selection (the "lemons" problem) here means:

(A) buyers know more than the originator

(B) the originator knows the true asset quality better than the buyers

(C) all parties are equally informed

(D) there is no information at all

\medskip

**Q84.** The post-crisis "skin in the game" rule requires the originator to:

(A) sell all the risk

(B) increase leverage

(C) retain roughly 5% of the securitized risk

(D) avoid lending

\medskip

**Q85.** The subprime securitization chain, in order, was:

(A) CDO$^{2}$ $\to$ CDO $\to$ RMBS $\to$ mortgages

(B) mortgages $\to$ RMBS $\to$ ABS CDO $\to$ CDO$^{2}$

(C) equities $\to$ bonds $\to$ loans

(D) deposits $\to$ reserves $\to$ capital

\medskip

**Q86.** Turning a pool of mostly-BBB assets into AAA tranches is possible only if defaults are:

(A) perfectly correlated

(B) certain

(C) weakly correlated

(D) all senior

\medskip

**Q87.** The rating agencies' central conflict of interest was the:

(A) investor-pays model

(B) issuer-pays model

(C) government-pays model

(D) absence of fees

\medskip

**Q88.** "The formula that killed Wall Street" refers to:

(A) the Black-Scholes formula

(B) the CAPM

(C) the Gaussian copula (popularized by David X. Li)

(D) the Vasicek bond formula

\medskip

**Q89.** AIG's near-collapse in 2008 was driven primarily by:

(A) a duration mismatch on mortgages

(B) collateral-posting triggers on sold CDS protection plus a downgrade, creating a liquidity crisis

(C) a rogue trader

(D) a cyber attack

\medskip

**Q90.** The deep lesson of credit risk transfer is that aggregate risk is:

(A) eliminated

(B) always reduced

(C) converted into equity

(D) dispersed and opacified — and can be degraded (moral hazard) and concentrated — not eliminated

\medskip

## C.8 — Réglementation & cas

**Q91.** The "skin in the game" retention requirement is designed to address:

(A) market liquidity risk

(B) the moral hazard of the originate-to-distribute model

(C) interest rate risk

(D) operational risk

\medskip

**Q92.** The CVA capital charge under Basel III addresses:

(A) market risk only

(B) liquidity risk

(C) counterparty credit risk on derivatives

(D) operational risk

\medskip

**Q93.** Central clearing of standardized derivatives through a CCP primarily reduces:

(A) interest rate risk

(B) counterparty risk and opacity

(C) equity risk

(D) inflation

\medskip

**Q94.** The Lehman CDS settlement (2008) showed that:

(A) gross notional equals net risk

(B) CDS never pay out

(C) the net settled exposure was far below the gross notional

(D) recovery was 100%

\medskip

**Q95.** The Greek CDS episode (2012) showed that a credit event:

(A) can never occur on a sovereign

(B) was triggered when collective action clauses forced a restructuring

(C) requires bankruptcy only

(D) was avoided entirely

\medskip

**Q96.** The Archegos episode (2021) illustrates:

(A) interest rate risk

(B) a duration mismatch

(C) a sovereign default

(D) hidden leverage via total return swaps, opaque to each prime broker

\medskip

**Q97.** Northern Rock (2007) illustrates:

(A) a rogue trader

(B) securitization-as-funding producing funding liquidity risk when the market froze

(C) a cyber attack

(D) a Ponzi scheme

\medskip

**Q98.** The JPMorgan "London Whale" (2012) illustrates:

(A) a sovereign default

(B) a bank run

(C) an oversized synthetic-credit index position with weak controls

(D) a cyber attack

\medskip

**Q99.** The performance of CLOs in 2008 and 2020 illustrates that:

(A) all securitization is doomed

(B) leverage is irrelevant

(C) tranches do not work

(D) securitization can be resilient when collateral and structure are sound

\medskip

**Q100.** The EU's 2012 ban on "naked" sovereign CDS targeted:

(A) hedging by bondholders

(B) speculative protection-buying without owning the underlying debt

(C) central clearing

(D) index rolls

\medskip

---

## C.9 — Indices, FTD & *trading* de corrélation

**Q101.** A first-to-default (FTD) basket pays out on:

(A) every default in the basket

(B) the last default only

(C) the first default, then terminates

(D) no defaults

\medskip

**Q102.** The seller of protection on an FTD basket is:

(A) long correlation

(B) short correlation

(C) insensitive to correlation

(D) long the risk-free rate

\medskip

**Q103.** As default correlation rises, the FTD spread:

(A) rises toward the sum of spreads

(B) falls toward a single name's spread

(C) is unchanged

(D) becomes negative

\medskip

**Q104.** At very low correlation, the FTD spread approaches:

(A) zero

(B) a single name's spread

(C) the sum of the names' spreads (upper bound)

(D) the risk-free rate

\medskip

**Q105.** A CDS index (CDX / iTraxx) is:

(A) a single-name CDS

(B) a portfolio of single-name CDS

(C) a government bond

(D) an equity index

\medskip

**Q106.** Correlation trading involves:

(A) buying only the senior tranche

(B) holding cash

(C) buying/selling tranches to bet on implied correlation

(D) shorting the risk-free rate

\medskip

**Q107.** The seller of the equity tranche is:

(A) short correlation

(B) long correlation

(C) insensitive to correlation

(D) short the index

\medskip

**Q108.** Base correlation is used to:

(A) remove all risk

(B) price single-name CDS

(C) price tranches consistently and avoid arbitrage across the capital structure

(D) set the risk-free rate

\medskip

**Q109.** The May 2005 correlation dislocation was triggered by:

(A) a rate hike

(B) idiosyncratic GM/Ford downgrades that broke implied correlation

(C) a sovereign default

(D) a cyber attack

\medskip

**Q110.** The ABX index allowed investors to:

(A) buy Treasuries

(B) hedge equities

(C) short subprime RMBS/CDO exposure

(D) avoid all risk

\medskip

**Q111.** An FTD basket of five names at ~100 bps each has a low-correlation upper-bound spread of about:

(A) 100 bps

(B) 300 bps

(C) 500 bps

(D) 1000 bps

\medskip

**Q112.** A synthetic CDO transfers risk via:

(A) a true sale of loans

(B) credit default swaps (unfunded)

(C) equity issuance

(D) deposits

\medskip

**Q113.** Delta-hedging a correlation trade neutralizes:

(A) idiosyncratic risk

(B) directional index-spread exposure

(C) the risk-free rate

(D) recovery risk

\medskip

**Q114.** A standard CDX Investment Grade index contains:

(A) 5 names

(B) 50 names

(C) 125 names

(D) 500 names

\medskip

**Q115.** An index "roll" refers to:

(A) a default

(B) a coupon payment

(C) moving to the new on-the-run series

(D) an auction

\medskip

## C.10 — *Shadow banking*, SIV, CPDO, TruPS

**Q116.** A SIV funded long-dated assets with:

(A) equity only

(B) short-term ABCP (maturity transformation)

(C) government grants

(D) deposits insured by the state

\medskip

**Q117.** The 2007 ABCP freeze forced SIVs to:

(A) expand

(B) pay dividends

(C) liquidate assets or return to bank balance sheets

(D) issue equity

\medskip

**Q118.** A CPDO was:

(A) a government bond

(B) a highly leveraged, AAA-rated seller of index protection

(C) a deposit account

(D) an equity fund

\medskip

**Q119.** CPDOs collapsed because:

(A) rates fell

(B) they had no leverage

(C) leverage plus volatility overwhelmed them, and the AAA rating ignored tail risk

(D) of a cyber attack

\medskip

**Q120.** TruPS CDOs concentrated risk in:

(A) sovereigns

(B) the banking sector (highly correlated)

(C) commodities

(D) equities

\medskip

**Q121.** The deep lesson of SIVs and TruPS CDOs is that securitizing same-factor exposures:

(A) removes systematic risk

(B) creates alpha

(C) hides but does not remove systematic risk

(D) is risk-free

\medskip

**Q122.** Shadow banking's core fragility is:

(A) deposit insurance

(B) maturity/liquidity transformation without a safety net

(C) too much equity

(D) low leverage

\medskip

**Q123.** An AAA rating on a leveraged risk-seller is:

(A) fully reliable

(B) conservative

(C) misleading, because it ignores tail risk

(D) irrelevant

\medskip

**Q124.** Northern Rock and SIVs share:

(A) a rogue trader

(B) funding liquidity risk from securitization-as-funding

(C) a sovereign default

(D) zero leverage

\medskip

**Q125.** The originate-to-distribute incentive problem is:

(A) too much screening

(B) weakened screening and monitoring (moral hazard)

(C) excess capital

(D) high recovery

\medskip

## C.11 — Structure de marché post-crise

**Q126.** The 2009 CDS "Big Bang" introduced:

(A) floating coupons only

(B) fixed coupons, upfront payments, and determination committees

(C) the abolition of CDS

(D) physical settlement only

\medskip

**Q127.** Standardization of CDS enabled:

(A) more opacity

(B) higher counterparty risk

(C) central clearing and fungibility

(D) the end of indices

\medskip

**Q128.** A central counterparty (CCP) reduces:

(A) market risk

(B) bilateral counterparty risk and opacity

(C) inflation

(D) equity risk

\medskip

**Q129.** A CCP also concentrates risk in:

(A) the government

(B) each bank separately

(C) the clearinghouse (a systemic node)

(D) retail investors

\medskip

**Q130.** Risk retention ("skin in the game") requires the originator to keep about:

(A) 0%

(B) 1%

(C) 5%

(D) 50%

\medskip

**Q131.** Risk retention primarily addresses:

(A) interest rate risk

(B) the moral hazard of originate-to-distribute

(C) liquidity risk

(D) operational risk

\medskip

**Q132.** The Volcker Rule affected:

(A) government bonds

(B) deposits

(C) banks' securitization/CLO positions

(D) equity indices

\medskip

**Q133.** Single-name CDS liquidity after the crisis:

(A) increased sharply

(B) declined

(C) stayed identical

(D) became risk-free

\medskip

**Q134.** Determination committees decide:

(A) the risk-free rate

(B) coupon levels

(C) whether a credit event has occurred

(D) the recovery rate only

\medskip

**Q135.** CDS settlement is now typically achieved via:

(A) physical delivery only

(B) an auction (cash settlement)

(C) no settlement

(D) equity swap

\medskip

**Q136.** The EU 2012 ban on "naked" sovereign CDS targeted:

(A) hedging by bondholders

(B) central clearing

(C) buying protection without owning the underlying debt

(D) index rolls

\medskip

**Q137.** Central clearing requires:

(A) no collateral

(B) initial and variation margin

(C) equity only

(D) a sovereign guarantee

\medskip

**Q138.** The CVA capital charge (Basel III) covers:

(A) market risk only

(B) liquidity risk

(C) counterparty credit risk on derivatives

(D) operational risk

\medskip

## C.12 — Cas chiffrés & synthèse

**Q139.** Index CDS upfront: coupon 100 bps, market spread 160 bps, $RPV01=4$, notional €20m. The upfront is closest to:

(A) €120,000

(B) €240,000

(C) €480,000

(D) €600,000

\medskip

**Q140.** Using $s\approx\lambda(1-R)$ with $\lambda$ fixed, if recovery rises from 40% to 50%, a 150 bps spread becomes:

(A) 100 bps

(B) 125 bps

(C) 150 bps

(D) 180 bps

\medskip

**Q141.** Negative-basis carry: bond Z-spread 200 bps, CDS 160 bps, notional €25m. The annual carry (before frictions) is about:

(A) €40,000

(B) €100,000

(C) €160,000

(D) €250,000

\medskip

**Q142.** The FTD basket spread lies:

(A) below the smallest single-name spread

(B) above the sum of spreads

(C) between the largest single-name spread and the sum of spreads

(D) exactly at zero

\medskip

**Q143.** The hedge ratio to hedge a par bond's credit risk with a CDS is approximately:

(A) 0

(B) 1

(C) 5

(D) 10

\medskip

**Q144.** The correlation delta of the equity tranche, for its seller, is:

(A) negative

(B) zero

(C) positive (long correlation)

(D) undefined

\medskip

**Q145.** Vasicek WCDR: $PD=3\%$, $\rho=0.20$, 99.9% confidence. The WCDR is closest to:

(A) 3%

(B) 15%

(C) 28.8%

(D) 45%

\medskip

**Q146.** Continuing Q145, with $LGD=45\%$, the IRB capital (per unit EAD, $\approx LGD(WCDR-PD)$) is closest to:

(A) 5.0%

(B) 11.6%

(C) 13.0%

(D) 29.0%

\medskip

**Q147.** A mezzanine tranche 3–7% faces a portfolio loss of 8%. Its loss, as a % of its own notional, is:

(A) 0%

(B) 50%

(C) 100%

(D) 25%

\medskip

**Q148.** Excess spread in a securitization absorbs losses:

(A) after the senior tranche

(B) before the equity tranche

(C) never

(D) only at maturity

\medskip

**Q149.** The core lesson of credit risk transfer is that aggregate risk is:

(A) eliminated

(B) always reduced

(C) converted into equity

(D) dispersed and opacified — and can be degraded and concentrated — not eliminated

\medskip

**Q150.** The Vasicek / WCDR framework reappears directly in:

(A) equity valuation

(B) FX pricing

(C) the Basel IRB capital formula (Part II)

(D) option Greeks

\medskip

# Partie D — Corrigé commenté & renvois aux sections

> Réponse en **gras**, **renvoi à la section** (§) et explication en français. Calcul détaillé pour les questions chiffrées.

## D.1 — Briques quantitatives & EL

**Q1 — C** *(§1)*. $EL = PD \times LGD \times EAD$ (produit).
**Q2 — B** *(§1)*. $LGD = 1 - R = 1 - 0{,}35 = 65\%$.
**Q3 — B** *(§1)*. $EL = 0{,}03 \times 0{,}60 \times \$5\text{m} = \$90\,000$.
**Q4 — B** *(§1)*. $PD(0,3) = 1 - e^{-0{,}02\times3} = 1 - e^{-0{,}06} = 5{,}82\%$.
**Q5 — C** *(§1)*. L'EL de portefeuille $= PD\times LGD$ est **invariante** à $\rho$ (linéarité de l'espérance) ; seule la **forme** (queues) change.
**Q6 — D** *(§1)*. L'**UL** est couverte par le **capital économique** (l'EL, elle, est provisionnée).
**Q7 — B** *(§1)*. Les PD **risque-neutres** sont généralement **plus élevées** (prime de risque incluse).
**Q8 — C** *(§1)*. $\lambda$ = probabilité **instantanée** de défaut **conditionnelle à la survie**.

## D.2 — CDS : mécanique & *credit triangle*

**Q9 — B** *(§2.1)*. L'acheteur **paie la prime** et est **indemnisé** lors d'un événement de crédit.
**Q10 — C** *(§2.1)*. Le vendeur **reçoit la prime** et **paie au défaut** ($\approx$ long crédit).
**Q11 — B** *(§2.2)*. *Credit triangle* : $s \approx \lambda(1-R)$.
**Q12 — C** *(§2.2)*. $\lambda \approx s/(1-R) = 0{,}024/0{,}60 = 4{,}0\%$.
**Q13 — C** *(§2.2)*. $s \approx \lambda(1-R) = 0{,}02\times0{,}70 = 1{,}4\% = 140$ bps.
**Q14 — B** *(§2.1)*. CDS ***naked*** = protection **sans détenir** la dette (spéculatif/macro).
**Q15 — D** *(§2.1)*. Un simple ***downgrade*** n'est **pas** un événement de crédit ISDA (faillite, défaut de paiement, restructuration le sont).
**Q16 — B** *(§2.1)*. L'acheteur de protection est **« short crédit »** (gagne si la qualité se dégrade).
**Q17 — C** *(§2.2)*. $\lambda \approx 0{,}03/0{,}75 = 4{,}0\%$.
**Q18 — C** *(§2.2)*. $\lambda \approx 0{,}02/0{,}6 = 3{,}33\%$ ; $PD(0,5) = 1 - e^{-0{,}1667} = 15{,}4\%$.
**Q19 — B** *(§2.7)*. $R$ est fixé par une **enchère ISDA** post-événement.
**Q20 — C** *(§2.7)*. Le notionnel de CDS pouvait **dépasser** la dette livrable $\Rightarrow$ **squeeze** $\Rightarrow$ passage à l'enchère cash.
**Q21 — B** *(§2.3)*. $RPV01$ = **annuité risquée** (valeur d'1 bp de prime).
**Q22 — C** *(§2.3)*. $s^{*} = PV_{\text{prot}} / RPV01$.
**Q23 — B** *(§2.5)*. *Spread* qui s'écarte $\Rightarrow$ l'acheteur de protection **gagne** (MtM positif).
**Q24 — B** *(§2.5)*. $\mathrm{MtM} \approx (0{,}0250-0{,}0100)\times4{,}3\times10\,\text{M} = +645\,000$ €.
**Q25 — B** *(§2.8)*. *Wrong-way risk* : l'exposition **monte** au moment où la **contrepartie se dégrade**.

## D.3 — CDS : valorisation, base & *bootstrap*

**Q26 — B** *(§2.5)*. $\mathrm{MtM} \approx (0{,}008-0{,}012)\times4{,}0\times20\,\text{M} = -320\,000$ € : le *spread* s'est **resserré**, l'acheteur de protection **perd**.
**Q27 — B** *(§2.6)*. Base négative $\Rightarrow$ **acheter l'obligation + acheter la protection** (portage $\approx -\text{Basis}$).
**Q28 — A** *(§2.6)*. Base négative : $s_{\text{CDS}} < s_{\text{obligation}}$.
**Q29 — D** *(§2.6)*. Contrepartie, **financement** de l'obligation, **cheapest-to-deliver**, liquidité : **tous** écartent la base de zéro.
**Q30 — B** *(§2.4)*. $\lambda \approx 0{,}012/0{,}6 = 2\%$ ; $PD(0,1) = 1 - e^{-0{,}02} = 1{,}98\% \approx 2\%$.
**Q31 — B** *(§2.4)*. Une courbe **pentue** $\Rightarrow$ intensités *forward* **croissantes**.
**Q32 — B** *(§2.3)*. Un **hasard plus élevé** réduit les survies $Q(t_i)$, donc le $RPV01$.
**Q33 — B** *(§2.3)*. Le *spread* **par** **annule** la valeur du contrat à la mise en place.
**Q34 — C** *(§2.1)*. Obligation + protection = position de crédit **couverte**.
**Q35 — B** *(§3)*. CDX/iTraxx = paniers **standardisés**, **liquides**, **roll** semestriel.
**Q36 — C** *(§3)*. iTraxx Main $\approx$ **125** noms **IG** européens.
**Q37 — B** *(§3)*. *On-the-run* = série la **plus récente** et **liquide**.
**Q38 — C** *(§2.6)*. En 2008, impossible de **financer** les obligations $\Rightarrow$ fuite vers les CDS $\Rightarrow$ base **très négative**.

## D.4 — Autres dérivés de crédit & indices

**Q39 — C** *(§3)*. Le **TRS** transfère la **performance totale** (crédit **et** marché) — financement synthétique à levier.
**Q40 — C** *(§3)*. La **CLN** est **préfinancée** : **pas** de risque de contrepartie sur le vendeur.
**Q41 — B** *(§3)*. Dans une CLN, le vendeur **finance d'avance** (collatéral dans le SPV).
**Q42 — B** *(§3)*. Le vendeur **FTD** est **long corrélation** (plus de $\rho$ $\Rightarrow$ moins de chance qu'un seul nom fasse défaut isolément).
**Q43 — C** *(§3)*. Le *spread* FTD $<$ **somme** des *spreads* mais $>$ **max** individuel.
**Q44 — B** *(§3)*. Les **tranches d'indices** servent à **négocier la corrélation**.
**Q45 — B** *(§5)*. Le CDO **synthétique** prend l'exposition via **CDS** : **offre quasi illimitée** sur les mêmes noms (amplification).
**Q46 — C** *(§5, §7.2)*. Un $\text{CDO}^2$ **titrise des tranches de CDO** (opacité maximale).
**Q47 — C** *(§5)*. Un **CLO** est adossé à des **prêts d'entreprise à levier**.
**Q48 — D** *(§5 ; Cas 22)*. Granularité, gestion active, rehaussement robuste, **prêts seniors garantis**.
**Q49 — B** *(§5)*. En *single-tranche*, le *dealer* **delta-couvre** sa **corrélation** sur l'indice.
**Q50 — B** *(§8 ; Cas 23)*. Les **CDPC** étaient des vendeurs **sous-capitalisés** de **risque de queue corrélé**.

## D.5 — Titrisation & *tranching*

**Q51 — C** *(§4.1)*. ***True sale*** : les actifs **sortent du bilan** ; le SPV est ***bankruptcy-remote***.
**Q52 — B** *(§4.1)*. *Bankruptcy remote* : la faillite de l'initiateur **n'atteint pas** les actifs cédés.
**Q53 — C** *(§4.3)*. Perte de tranche $= [\min(L,D)-A]^{+}$.
**Q54 — C** *(§4.3)*. $[\min(2,3)-0]^{+}=2\%$ du portefeuille ; notionnel equity $=3\%$ ; fraction $=2/3=66{,}7\%$.
**Q55 — B** *(§4.3)*. $[\min(5,7)-3]^{+}=2\%$ ; notionnel mezz $=4\%$ ; $2/4=50\%$.
**Q56 — B** *(§4.3)*. $[\min(10,100)-7]^{+}=3\%$ ; notionnel senior $=93\%$ ; $3/93=3{,}23\%$.
**Q57 — B** *(§4.3)*. Perte $=6\%$ de 200 M $=$ 12 M ; mezz $[\min(6,9)-4]^{+}=2\%=$ 4 M ; notionnel $=5\%\times200=$ 10 M ; $4/10=40\%$.
**Q58 — C** *(§4.2)*. L'**excès de marge** absorbe les pertes courantes **avant** l'equity (première ligne de défense).
**Q59 — B** *(§4.2)*. **OC** : valeur des actifs $>$ titres émis (coussin).
**Q60 — D** *(§4.2)*. Le ***wrap* monoline** est un rehaussement **externe** (les autres sont internes).
**Q61 — C** *(§4.2)*. Cascade **séquentielle** : le **senior** est remboursé en premier.
**Q62 — B** *(§4.2)*. Les **déclencheurs OC/IC** **détournent** les flux vers le **senior**.
**Q63 — C** *(§4.4)*. L'**equity** = tranche de **première perte**, plus **risquée** et plus **rémunérée**.
**Q64 — B** *(§4.4)*. Une **tranche mince** a un **levier** élevé (0 %$\to$100 % sur une faible plage).
**Q65 — D** *(§6.3)*. Le AAA du **senior** suppose une **faible corrélation** des défauts.

## D.6 — Corrélation & modèle à un facteur (Vasicek)

**Q66 — B** *(§6.1)*. $A_i=\sqrt{\rho}\,M+\sqrt{1-\rho}\,\varepsilon_i$ (facteur commun + bruit idiosyncratique).
**Q67 — C** *(§6.1)*. $P(A_i<c)=N(c)=PD \Rightarrow c=N^{-1}(PD)$.
**Q68 — C** *(§6.1)*. $p(m)=N\!\big[(N^{-1}(PD)-\sqrt{\rho}\,m)/\sqrt{1-\rho}\big]$.
**Q69 — B** *(§6.1)*. $p(m)$ **décroît** en $m$ : $m$ très négatif $\Rightarrow$ défauts **simultanés**.
**Q70 — C** *(§6.1)*. $p(-2)=N\!\big[(-1{,}645-0{,}447(-2))/\sqrt{0{,}8}\big]=N[-0{,}840]=20\%$.
**Q71 — B** *(§6.2)*. Loi de Vasicek : $N\!\big[(\sqrt{1-\rho}\,N^{-1}(\ell)-N^{-1}(PD))/\sqrt{\rho}\big]$.
**Q72 — D** *(§6.2)*. $WCDR=N\!\big[(N^{-1}(PD)+\sqrt{\rho}\,N^{-1}(1-\alpha))/\sqrt{1-\rho}\big]$.
**Q73 — C** *(§6.2)*. $N\!\big[(-2{,}054+0{,}447\times3{,}090)/\sqrt{0{,}8}\big]=N[-0{,}753]\approx22{,}6\%$.
**Q74 — C** *(§6.2 ; Cas 29)*. $N\!\big[(-2{,}326+0{,}387\times3{,}090)/\sqrt{0{,}85}\big]=N[-1{,}226]\approx11\%$.
**Q75 — A** *(§6.3)*. À EL constante, $\rho\uparrow \Rightarrow EL_{\text{senior}}\uparrow$ et $EL_{\text{equity}}\downarrow$ (résultat central).
**Q76 — B** *(§6.3)*. Le vendeur **equity** est **long corrélation** ($\rho\uparrow$ réduit l'EL de l'equity).
**Q77 — A** *(§6.3)*. Le vendeur **senior** est **court corrélation** ($\rho\uparrow$ augmente l'EL du senior).
**Q78 — C** *(§6.2 ; Cas 29)*. Capital IRB $\approx LGD\times(WCDR-PD)$ (couvre l'**UL**).
**Q79 — C** *(§6.4)*. Les prix impliquent des **corrélations différentes** par tranche (skew) $\Rightarrow$ besoin d'une mesure **cohérente** (*base correlation*).
**Q80 — B** *(§6.4)*. La copule gaussienne **sous-estime la dépendance de queue** (défauts conjoints extrêmes).

## D.7 — *Originate-to-distribute*, crise & agences

**Q81 — B** *(§7.1)*. L'OtD **affaiblit** la **sélection/suivi** (on revend le risque).
**Q82 — C** *(§7.1)*. C'est un **aléa moral**.
**Q83 — B** *(§7.1)*. **Antisélection** : l'initiateur connaît mieux la **qualité réelle** que l'acheteur.
**Q84 — C** *(§7.1)*. *Skin in the game* : **rétention $\approx 5\%$**.
**Q85 — B** *(§7.2)*. Chaîne : **prêts $\to$ RMBS $\to$ ABS CDO $\to$ CDO$^2$**.
**Q86 — C** *(§7.2)*. Transformer du BBB en AAA suppose des défauts **peu corrélés**.
**Q87 — B** *(§7.3)*. Conflit central : modèle ***issuer-pays***.
**Q88 — C** *(§7.3)*. La **copule gaussienne** (Li, 2000).
**Q89 — B** *(§8 ; Cas 4)*. **Déclencheurs de collatéral** sur CDS vendus **+** dégradation $\Rightarrow$ **crise de liquidité**.
**Q90 — D** *(§7.3)*. Le risque agrégé est **dispersé/opacifié**, possiblement **dégradé** et **concentré** — **pas** éliminé.

## D.8 — Réglementation & cas

**Q91 — B** *(§8)*. Le ***skin in the game*** vise l'**aléa moral** de l'OtD.
**Q92 — C** *(§8)*. Le **CVA** couvre le **risque de contrepartie** des dérivés.
**Q93 — B** *(§8)*. La **CCP** réduit le **risque de contrepartie** et l'**opacité**.
**Q94 — C** *(§2.7 ; Cas 10)*. L'exposition **nette** réglée était **bien inférieure** au notionnel **brut**.
**Q95 — B** *(Cas 12)*. Les **CAC** rendant l'échange contraignant ont **déclenché** l'événement de crédit.
**Q96 — D** *(§3 ; Cas 16)*. **Levier caché** via **TRS**, **opaque** pour chaque courtier.
**Q97 — B** *(Cas 9)*. **Titrisation-financement** $\Rightarrow$ **risque de liquidité** au gel du marché.
**Q98 — C** *(§3 ; Cas 15)*. Position **démesurée** sur indice de crédit synthétique + contrôles faibles.
**Q99 — D** *(§5 ; Cas 22)*. La titrisation peut être **résiliente** si **collatéral et structure** sont sains.
**Q100 — B** *(§3, §8 ; Cas 21)*. Cible : l'**achat spéculatif** de protection souveraine **sans détenir** la dette.

---

## D.9 — Indices, FTD & *trading* de corrélation

**Q101 — C** *(§3 ; Cas 31)*. Le **FTD** paie sur le **premier** défaut puis s'éteint.
**Q102 — B** *(§3, §6.3 ; Cas 31)*. Le vendeur de FTD est **court corrélation**.
**Q103 — B** *(§6.3 ; Cas 31, 45)*. $\rho\uparrow \Rightarrow$ spread FTD **baisse** (vers un seul nom).
**Q104 — C** *(§6.3 ; Cas 45)*. $\rho\to0 \Rightarrow$ FTD $\to$ **somme** des spreads (borne haute).
**Q105 — B** *(§3)*. Un indice CDS = **portefeuille** de CDS mononom.
**Q106 — C** *(§6.4 ; Cas 32)*. *Correlation trading* = acheter/vendre des **tranches** sur la corrélation implicite.
**Q107 — B** *(§6.3 ; Cas 50)*. Le vendeur **equity** est **long corrélation**.
**Q108 — C** *(§6.4)*. La *base correlation* price les tranches de façon **cohérente** (sans arbitrage).
**Q109 — B** *(§6.4 ; Cas 33)*. Dislocation mai 2005 : dégradations **idiosyncratiques** GM/Ford cassant la corrélation implicite.
**Q110 — C** *(§3 ; Cas 34)*. L'**ABX** permet de **shorter** le subprime.
**Q111 — C** *(§6.3 ; Cas 45)*. Borne haute $\approx 5\times100=500$ bps.
**Q112 — B** *(§5 ; Cas 32)*. CDO **synthétique** : transfert via **CDS** (non financé).
**Q113 — B** *(§6.4 ; Cas 32)*. Le *delta-hedge* neutralise l'**exposition directionnelle** au spread d'indice.
**Q114 — C** *(§3)*. Le **CDX IG** compte **125** noms.
**Q115 — C** *(§3 ; Cas 38)*. Le *roll* = passage à la **nouvelle série** *on-the-run*.

## D.10 — *Shadow banking*, SIV, CPDO, TruPS

**Q116 — B** *(§7 ; Cas 36)*. SIV : actifs longs financés par **ABCP** court (transformation d'échéances).
**Q117 — C** *(§7 ; Cas 36)*. Gel de l'ABCP 2007 $\Rightarrow$ **liquidation** / retour au bilan.
**Q118 — B** *(§5 ; Cas 35)*. CPDO : vendeur de protection d'indice **à fort levier**, noté **AAA**.
**Q119 — C** *(§5 ; Cas 35)*. Levier + volatilité ; le **AAA** ignorait le risque de **queue**.
**Q120 — B** *(§5 ; Cas 37)*. TruPS CDO : risque **concentré** sur le **secteur bancaire**.
**Q121 — C** *(§7 ; Cas 37)*. Titriser des expositions **au même facteur** **cache** mais **n'ôte pas** le risque systématique.
**Q122 — B** *(§7 ; Cas 36)*. Fragilité du *shadow banking* : **transformation** sans **filet** (assurance des dépôts).
**Q123 — C** *(§5 ; Cas 35)*. Un AAA sur un **vendeur de risque à levier** est **trompeur** (ignore la queue).
**Q124 — B** *(§7 ; Cas 9, 36)*. Northern Rock et SIV : **risque de liquidité** de la titrisation-financement.
**Q125 — C** *(§7.1)*. OtD : **affaiblit** sélection/suivi (**aléa moral**).

## D.11 — Structure de marché post-crise

**Q126 — B** *(§8 ; Cas 38)*. *Big Bang* 2009 : **coupons fixes**, ***upfront***, **comités de détermination**.
**Q127 — C** *(§8 ; Cas 38, 39)*. Standardisation $\Rightarrow$ **compensation centrale** et **fongibilité**.
**Q128 — B** *(§8 ; Cas 39)*. La **CCP** réduit le risque de **contrepartie** bilatéral et l'**opacité**.
**Q129 — C** *(§8 ; Cas 39)*. La CCP **concentre** le risque dans la **chambre** (nœud systémique).
**Q130 — C** *(§8 ; Cas 40)*. Rétention $\approx 5\%$.
**Q131 — B** *(§8 ; Cas 40)*. La rétention vise l'**aléa moral** de l'OtD.
**Q132 — C** *(§8 ; Cas 41)*. La règle **Volcker** a touché les positions **titrisation/CLO** des banques.
**Q133 — B** *(§2 ; Cas 42)*. La liquidité des **CDS mononom** a **décliné** après la crise.
**Q134 — C** *(§8 ; Cas 38)*. Les **comités** décident de l'**événement de crédit**.
**Q135 — B** *(§2.7 ; Cas 10, 38)*. Règlement par **enchère** (*cash settlement*).
**Q136 — C** *(§3, §8 ; Cas 43)*. Interdiction UE 2012 : **protection sans détenir** la dette.
**Q137 — B** *(§8 ; Cas 39)*. La compensation centrale exige **marges initiale et de variation**.
**Q138 — C** *(§8)*. Le **CVA** couvre le **risque de contrepartie** des dérivés.

## D.12 — Cas chiffrés & synthèse

**Q139 — C** *(§3 ; Cas 46)*. Upfront $=(0{,}0160-0{,}0100)\times4\times20\,\text{M}=0{,}0060\times4\times20\,\text{M}=480\,000$ €.
**Q140 — B** *(§2.2 ; Cas 48)*. $150\times\dfrac{1-0{,}50}{1-0{,}40}=150\times0{,}833=125$ bps.
**Q141 — B** *(§2.6 ; Cas 47)*. Base $=200-160=40$ bps ; $0{,}0040\times25\,\text{M}=100\,000$ €/an.
**Q142 — C** *(§6.3 ; Cas 45)*. Le spread FTD est **entre** le plus grand nom **et** la somme.
**Q143 — B** *(§2 ; Cas 49)*. Pour un **pair**, ratio de couverture $\approx 1$.
**Q144 — C** *(§6.3 ; Cas 50)*. $\partial EL_{\text{equity}}/\partial\rho<0 \Rightarrow$ vendeur **long corrélation** (delta **positif**).
**Q145 — C** *(§6.2 ; Cas 29)*. $N\!\big[(-1{,}881+0{,}447\times3{,}090)/\sqrt{0{,}8}\big]=N[-0{,}558]\approx28{,}8\%$.
**Q146 — B** *(§6.2 ; Cas 29)*. $0{,}45\times(0{,}288-0{,}03)=0{,}45\times0{,}258=11{,}6\%$.
**Q147 — C** *(§4.3 ; Cas 26)*. $[\min(8,7)-3]^{+}=4\%=$ **toute** la tranche (largeur 4 %) $\Rightarrow$ **100 %**.
**Q148 — B** *(§4.2)*. L'**excès de marge** absorbe **avant** l'equity.
**Q149 — D** *(§7.3)*. Risque **dispersé/opacifié**, possiblement **dégradé** et **concentré** — **pas** éliminé.
**Q150 — C** *(§6.2)*. Le cadre **Vasicek/WCDR** revient dans le **capital IRB de Bâle** (Partie II).

## Note finale

Cet approfondissement couvre le chapitre 4 « 360° » : **concepts détaillés**, **démonstrations** (toutes en LaTeX), **30 études de cas** (historiques et chiffrées), un **formulaire**, et **100 QCM** au format examen avec **corrigé commenté**. Pour viser la maîtrise parfaite :

1. **Refais les démonstrations sans les notes** : *credit triangle* $s\approx\lambda(1-R)$, valorisation/par spread, *bootstrap*, MtM, base CDS-obligation, fonction de perte des tranches $[\min(L,D)-A]^{+}$, PD conditionnelle $p(m)$, loi de Vasicek et $WCDR$.
2. **Mémorise le résultat central** : à EL de portefeuille **constante**, $\rho\uparrow \Rightarrow EL_{\text{senior}}\uparrow$ (senior **court corrélation**) et $EL_{\text{equity}}\downarrow$ (equity **longue corrélation**).
3. **Associe chaque cas à son mécanisme** : AIG $\to$ collatéral/*wrong-way*/liquidité ; monolines/CDPC $\to$ garantie de risque corrélé sous-capitalisée ; ABACUS/Magnetar $\to$ conflit de sélection ; London Whale $\to$ taille/indice/contrôles ; Archegos $\to$ TRS/levier opaque ; Lehman/Delphi/Grèce/Argentine $\to$ règlement et définition de l'événement de crédit ; Northern Rock $\to$ titrisation-financement ; CLO 2008/2020 $\to$ résilience d'une titrisation saine.
4. **Le pont vers la Partie II** : le modèle de Vasicek et la $WCDR$ **reviennent** en *Credit Risk* et dans le **capital IRB de Bâle** — l'avance prise ici paiera deux fois.

*Approfondissement pédagogique original — FRM, Foundations of Risk Management, Chapitre 4. Formules en notation GARP. À utiliser en complément des manuels officiels.*
