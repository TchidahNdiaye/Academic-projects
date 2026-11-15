---
title: "FRM 2026 · Part I — Foundations of Risk Management"
subtitle: "Synthèse française intégrale — concepts, équations LaTeX \\& QCM corrigés par chapitre"
---

> **Objet.** Synthèse française **complète** du book *Foundations of Risk Management* (Partie I de l'examen FRM, pondération **20 %**), couvrant **l'intégralité des concepts clés du programme**. Concepts expliqués en français de façon claire et précise ; **équations composées en LaTeX** (notation GARP : $N(\cdot)$ = loi normale centrée réduite). une **banque de QCM en anglais** (format examen), **organisée par chapitre**, suit la synthèse ; le **corrigé commenté en français**, avec **renvois aux sections du cours** (§), figure **en fin de document**.
>
> *Synthèse pédagogique originale rédigée à partir des objectifs d'apprentissage officiels ; elle ne reproduit pas le texte des manuels GARP. À confronter aux* Learning Objectives *sur garp.org.*

**Sommaire des chapitres.** (1) Building Blocks — (2) Managing Risk — (3) Governance — (4) Credit Risk Transfer — (5) MPT & CAPM — (6) APT & Multifactor — (7) Data Aggregation (BCBS 239) — (8) ERM — (9) Financial Disasters — (10) The Great Financial Crisis — (11) GARP Code of Conduct. **Corrigé des QCM** en annexe.

---

# Chapitre 1 — The Building Blocks of Risk Management
### Les briques fondamentales de la gestion des risques

## 1.1 Risque et incertitude

Le **risque** est l'exposition à une **incertitude** dont la réalisation affecte les objectifs d'un agent : concrètement, la **variabilité des résultats futurs** autour d'une valeur anticipée. Le risque n'est pas seulement la possibilité de **perdre**, mais la **dispersion** de l'ensemble des résultats possibles. On distingue :

- **Risque** (*risk*) : les états possibles et leurs **probabilités** sont **connus ou estimables** ; le risque est en principe **mesurable** (on lui attache une distribution).
- **Incertitude de Knight** (*Knightian uncertainty*) : les probabilités — voire les états eux-mêmes — sont **inconnues** ; elle n'est **pas probabilisable** et échappe aux modèles.

On classe aussi les risques par degré de connaissance : **connus** (identifiés et mesurés), **connaissables** (identifiables avec plus de données), et **inconnaissables** (*unknown unknowns*, ruptures sans précédent).

## 1.2 Risque et récompense

Principe fondateur : **aucun rendement excédentaire sans prise de risque**. Les agents exigent une **prime de risque** pour détenir des actifs risqués. La gestion des risques **ne vise pas à éliminer** le risque (ce serait éliminer le rendement) mais à l'**identifier**, le **mesurer**, le **tarifer**, **conserver** ceux pour lesquels la firme a un avantage et **transférer/réduire** les autres, en restant aligné sur l'**appétit** et la **capacité** d'absorption.

## 1.3 Perte attendue et perte inattendue

Distinction réutilisée dans tout le FRM :

- **Perte attendue** (*Expected Loss*, EL) : la perte **moyenne anticipée**. Prévisible, elle est traitée comme un **coût** : on la **tarifie** et on la **provisionne**.
- **Perte inattendue** (*Unexpected Loss*, UL) : la **variabilité** des pertes autour de l'EL ; elle menace la solvabilité et se couvre par du **capital économique**.

Pour le risque de crédit :

$$EL = PD \times LGD \times EAD,\qquad LGD = 1 - R,$$

avec $PD$ la probabilité de défaut, $LGD$ la perte en cas de défaut, $EAD$ l'exposition au défaut, $R$ le taux de recouvrement. La mesure de base du risque est l'écart-type : $\sigma=\sqrt{E[(X-E[X])^2]}$.

> **Point d'examen.** Mémorise « **EL $\to$ provisions/tarification ; UL $\to$ capital** ». Une question classique demande quelle composante couvre le capital économique (réponse : l'**UL**).

## 1.4 La typologie des risques

- **Risque de marché** : pertes dues aux variations des prix/taux (actions, taux, change, matières premières, spreads) ; composantes **directionnelle** et **non directionnelle** (volatilité, corrélation, convexité).
- **Risque de crédit** : pertes liées au **défaut** ou à la **dégradation** (migration) d'une contrepartie ; inclut le **risque de contrepartie**, de **concentration**, **souverain**.
- **Risque de liquidité** : *funding liquidity* (incapacité à **se financer**/refinancer) et *market liquidity* (incapacité à **vendre** sans déprimer le prix). À ne jamais confondre.
- **Risque opérationnel** : pertes dues à des **processus, personnes, systèmes** défaillants ou à des **événements externes** (fraude, erreurs, cyber, juridique). Exclut le risque stratégique et réputationnel.
- **Risque business / stratégique** : lié au **modèle d'affaires** (volumes, marges, décisions).
- **Risque de réputation** : atteinte à l'**image**, souvent **conséquence** d'autres risques.
- **Risque légal / réglementaire / de conformité** : litiges, sanctions, changements de règles.
- **Risque systémique** : propagation d'une défaillance à **tout le système** par contagion.

> **Point d'examen.** GARP teste les **frontières** : un *rogue trader* = risque **opérationnel** (même si la perte se voit sur les marchés) ; un *downgrade* = risque de crédit (**migration**) ; l'impossibilité de refinancer = *funding liquidity*.

## 1.5 Le processus de gestion des risques

Cycle en quatre temps, soutenu par la gouvernance :

1. **Identification** (y compris risques émergents et non quantifiables) ;
2. **Évaluation / mesure** (VaR, ES, sensibilités, EL/UL — et appréciation qualitative) ;
3. **Gestion / décision** — les **« 4 T »** : **Take/accept**, **Treat/mitigate**, **Transfer**, **Terminate/avoid** ;
4. **Suivi & reporting** (comparer aux **limites**, alerter, boucler).

## 1.6 Appétit, tolérance, capacité, limites

Hiérarchie à connaître :

$$\text{limits} \;\le\; \text{tolerance} \;\le\; \text{appetite} \;\le\; \text{capacity}.$$

- **Risk capacity** : maximum **absolu** supportable avant de menacer la solvabilité.
- **Risk appetite** : niveau de risque **choisi** par le conseil pour atteindre ses objectifs.
- **Risk tolerance** : déclinaison de l'appétit par type de risque/activité.
- **Risk limits** : **bornes opérationnelles** concrètes (VaR, notionnel, concentration, stop-loss).

## 1.7 Comment la gestion des risques crée — ou détruit — de la valeur

**Création** : meilleure **allocation du capital**, réduction des **coûts de détresse**, lissage des résultats, sécurisation de l'investissement, **tarification** correcte, confiance des parties prenantes. **Destruction** : **faux sentiment de sécurité**, **risque de modèle**, coûts excessifs, incitations perverses, sur-confiance dans des mesures statistiques. Les **modèles sont des outils, pas la vérité** : à compléter par jugement, scénarios et stress tests.

---

# Chapitre 2 — How Do Firms and Individuals Manage Risk?
### Comment firmes et individus gèrent-ils le risque ?

## 2.1 Le paradoxe de Modigliani-Miller

En **marché parfait** (pas d'impôts, ni coûts de faillite, ni asymétrie d'information ; marchés complets), le théorème de **Modigliani-Miller (MM)** implique que la **couverture est sans effet sur la valeur** : un actionnaire peut **répliquer** lui-même n'importe quelle couverture. La couverture **crée de la valeur uniquement** grâce aux **frictions réelles** :

- **Coûts de détresse financière et de faillite** : la couverture réduit la probabilité d'états coûteux (faillite, ventes forcées, perte de clients/fournisseurs) — **l'argument le plus fort**.
- **Fiscalité** : avec un barème d'impôt **convexe**, lisser le résultat avant impôt **réduit l'impôt espéré**.
- **Coûts d'agence et sous-investissement** (*debt overhang*) : préserver la capacité à investir et aligner actionnaires/créanciers.
- **Asymétrie d'information** : le financement externe coûte plus cher ; sécuriser les cash-flows évite de lever des fonds au pire moment.
- **Aversion des dirigeants** : peu diversifiés, ils valorisent la stabilité.

> **Point d'examen.** *« En cadre MM, la couverture crée-t-elle de la valeur ? »* $\to$ **Non en marché parfait** ; **oui grâce aux frictions** (détresse, impôts convexes, agence, asymétrie).

## 2.2 La décision de couverture : bénéfices et coûts

**Bénéfices** : baisse de la volatilité des cash-flows, réduction du coût de détresse, meilleure planification, accès au financement. **Coûts** : transaction, **coût d'opportunité** (renoncer à l'*upside* si couverture symétrique), **risque de base** (la couverture n'épouse pas l'exposition), complexité, **risque de modèle**, **risque de liquidité** (appels de marge), risque d'être perçue comme spéculative. **Principe directeur** : **conserver** les risques « cœur de métier » (compris et rémunérés), **transférer/réduire** les risques périphériques sans avantage comparatif.

## 2.3 Les mécanismes de réduction et de transfert

1. **Diversification** : combiner des expositions imparfaitement corrélées réduit le risque **idiosyncratique** sans coût explicite, mais **ne supprime pas** le risque systématique et suppose des corrélations **stables**.
2. **Assurance** : transfert (souvent **asymétrique**) d'un risque à un assureur contre une **prime** ; soumise à l'**aléa moral** et à l'**antisélection**.
3. **Dérivés** : *forwards, futures, swaps* couvrent de façon **symétrique** (on fige le résultat, on perd l'*upside*, sans prime initiale) ; les **options** couvrent de façon **asymétrique** (on garde l'*upside* contre une **prime**).

## 2.4 Cartographie des risques et rôle des transferts

La **cartographie des facteurs de risque** (*risk factor mapping*) décompose les positions en un petit nombre de **facteurs** observables (taux, change, spreads…), base de l'**agrégation** et de la **couverture** (on couvre le facteur, pas chaque position) et du calcul de VaR. En transférant le risque vers ceux qui sont le mieux à même de le porter, les marchés **améliorent l'efficience** ; mais un transfert mal maîtrisé peut **disperser et opacifier** le risque, créer de l'**aléa moral** (*originate-to-distribute*) et **concentrer** in fine le risque chez des acteurs mal identifiés. Le transfert **ne fait pas disparaître** le risque agrégé du système.

---

# Chapitre 3 — The Governance of Risk Management
### La gouvernance de la gestion des risques

## 3.1 Gouvernance d'entreprise et du risque

La **gouvernance d'entreprise** organise la direction et le contrôle de la firme dans l'intérêt des **parties prenantes** (actionnaires, créanciers, déposants, régulateurs, salariés, clients). La **gouvernance du risque** en est la composante : elle définit **qui fixe l'appétit, qui surveille, et comment l'information circule**. Leçon post-2008 : beaucoup de faillites tiennent à une **gouvernance défaillante** (conseils passifs, fonction risque sans pouvoir, incitations court-termistes, information ne remontant pas).

## 3.2 Les acteurs et leurs responsabilités

- **Conseil d'administration** : responsable **en dernier ressort** ; **fixe et approuve l'appétit pour le risque**, valide la stratégie et le cadre, assure les ressources et l'indépendance de la fonction risque, **challenge** la direction.
- **Comité des risques** (du conseil) : examine en détail le profil de risque, les politiques et les dépassements.
- **Directeur des risques (CRO)** : dirige la fonction risque, **indépendant** du *front office*, avec **accès direct au conseil/comité des risques** ; porte la vue **transversale et agrégée**.
- **Comité d'audit** : supervise l'**information financière**, le **contrôle interne** et l'**audit** ($\ne$ comité des risques).
- **Direction générale** : met en œuvre la stratégie dans les limites de l'appétit ; garantit une **culture du risque** saine.

> **Point d'examen.** Le CRO **rapporte au conseil/comité des risques** et est **indépendant** du *front office*. Comité d'**audit** (contrôle/reporting) $\ne$ comité des **risques** (profil de risque).

## 3.3 Les trois lignes de défense

- **1\textsuperscript{re} ligne — les métiers** : **prennent et possèdent** les risques ; contrôles de premier niveau.
- **2\textsuperscript{e} ligne — risque & conformité** : **indépendante**, définit le cadre/limites, **mesure et surveille**, **challenge** la 1\textsuperscript{re} ligne.
- **3\textsuperscript{e} ligne — audit interne** : **indépendante** des deux, fournit une **assurance objective** au conseil sur l'efficacité du dispositif.

> **Point d'examen.** **Qui possède (1\textsuperscript{re}) $\ne$ qui surveille (2\textsuperscript{e}) $\ne$ qui audite (3\textsuperscript{e})**, chaque ligne **indépendante**. L'audit interne ne « gère » pas le risque ; il en évalue le contrôle.

## 3.4 Incitations, rémunération et culture

Bonnes pratiques (largement reprises par la réglementation) : **différer** une part des bonus, les indexer sur la performance **ajustée du risque** et **pluriannuelle**, mécanismes de ***malus* et de *clawback*** (récupération en cas de pertes/faute), éviter de rémunérer le **revenu brut**. Une **culture du risque** saine (*tone from the top*) permet de **dire non** et de **remonter les alertes**.

## 3.5 Parties prenantes et conflits

Conflit central **actionnaires/créanciers** : les actionnaires (responsabilité limitée) peuvent préférer **plus de risque** (ils captent l'*upside*, les créanciers subissent une partie du *downside*) — c'est le ***risk shifting / asset substitution***. Une bonne gouvernance empêche qu'un groupe impose une prise de risque excessive aux autres et au système.

---

# Chapitre 4 — Credit Risk Transfer Mechanisms
### Les mécanismes de transfert du risque de crédit

## 4.1 Pourquoi et comment transférer le risque de crédit

Transférer le risque de crédit permet de **libérer du capital**, **gérer la concentration**, **diversifier** et **améliorer la liquidité**. Le revers est l'**aléa moral** du modèle ***originate-to-distribute*** : en revendant le risque, le prêteur **relâche** la sélection et le suivi des emprunteurs.

## 4.2 Le Credit Default Swap (CDS)

L'**acheteur de protection** paie un *spread* périodique $s$ au **vendeur de protection** ; en cas d'**événement de crédit** (faillite, défaut de paiement, restructuration), le vendeur **indemnise** la perte $(1-R)$, **sans cession de l'actif**. Un CDS ***naked*** (sans détenir la dette) est **spéculatif**. L'acheteur est **« short crédit »**, le vendeur **« long crédit »**. Le **credit triangle** relie *spread*, intensité de défaut $\lambda$ et recouvrement $R$ :

$$s \approx \lambda\,(1-R)\quad\Longleftrightarrow\quad \lambda \approx \frac{s}{1-R}.$$

Le CDS **crée un risque de contrepartie** (sur le vendeur) et un ***wrong-way risk*** lorsque exposition et qualité du vendeur se dégradent ensemble (cas **AIG**, **monolines**).

## 4.3 Titrisation et *tranching*

Un portefeuille de créances est cédé à un **SPV** (*true sale*, *bankruptcy remote*) qui émet des **titres** (ABS/MBS) découpés en **tranches** par séniorité, organisées en **cascade (*waterfall*)** : l'**equity** absorbe les **premières** pertes, puis la **mezzanine**, enfin le **senior**. Le **rehaussement** (subordination, surdimensionnement, excès de marge, réserve) protège les tranches hautes. Pour une tranche définie par un point d'attachement $A$ et de détachement $D$, la **fraction détruite** par une perte de portefeuille $L$ est :

$$\text{Loss fraction} = \frac{[\min(L,D)-A]^{+}}{D-A},\qquad [x]^{+}=\max(x,0).$$

## 4.4 Le rôle de la corrélation

L'**EL du portefeuille est invariante à la corrélation** ; celle-ci ne change **que la forme** de la distribution. Dans le **modèle à un facteur** (copule gaussienne), $A_i=\sqrt{\rho}\,M+\sqrt{1-\rho}\,\varepsilon_i$, la **PD conditionnelle** au facteur $M=m$ est :

$$p(m)=N\!\left[\frac{N^{-1}(PD)-\sqrt{\rho}\,m}{\sqrt{1-\rho}}\right].$$

Une **hausse de corrélation** déplace la masse vers les queues : elle **augmente** l'EL de la tranche **senior** et **diminue** celle de l'**equity**. La **sous-estimation de $\rho$** (les défauts subprime supposés peu corrélés) a fait noter **AAA** des tranches en réalité très exposées.

## 4.5 *Originate-to-distribute* et la crise

La chaîne **subprime $\to$ RMBS $\to$ CDO $\to$ CDO$^{2}$** a **dispersé et opacifié** le risque, l'a **concentré** chez quelques acteurs (AIG), tout en **dégradant** sa qualité (aléa moral). Les **agences de notation** (en **conflit d'intérêts**, *issuer-pays*) et la **copule de Li** (sans dépendance de queue) ont **sur-noté** des produits opaques. La réponse réglementaire : ***skin in the game*** (rétention), transparence, défiance envers les notations.

> **Point d'examen.** (1) Le CDS transfère le risque **sans vendre l'actif** ; (2) le **tranchage** alloue les pertes par séniorité, l'**equity** en premier ; (3) la **corrélation** est le paramètre clé — **$\rho\uparrow$ pénalise le senior, favorise l'equity** ; l'**EL de portefeuille** n'en dépend **pas**.

---

# Chapitre 5 — Modern Portfolio Theory & the CAPM
### Théorie moderne du portefeuille et CAPM

## 5.1 Cadre moyenne-variance

Un investisseur rationnel averse au risque juge un portefeuille par son **rendement espéré** et sa **variance**. Pour des poids $w_i$ ($\sum_i w_i=1$) :

$$E(R_P)=\sum_i w_i E(R_i),\qquad \sigma_P^2=\sum_i\sum_j w_iw_j\,\rho_{ij}\sigma_i\sigma_j=\mathbf{w}^{\top}\Sigma\mathbf{w}.$$

À deux actifs : $\sigma_P^2=w_1^2\sigma_1^2+w_2^2\sigma_2^2+2w_1w_2\rho_{12}\sigma_1\sigma_2$, avec $\operatorname{Cov}(R_1,R_2)=\rho_{12}\sigma_1\sigma_2$. Dès que $\rho_{12}<1$, le risque est **inférieur** à la moyenne pondérée des risques.

## 5.2 Diversification

Pour un portefeuille équipondéré : $\sigma_P^2=\frac1N\overline{\sigma^2}+\big(1-\frac1N\big)\overline{\operatorname{cov}}\ \xrightarrow[N\to\infty]{}\ \overline{\operatorname{cov}}$. La diversification supprime le risque **idiosyncratique** mais **jamais** le risque **systématique** (la covariance moyenne). Au niveau d'un actif : $\sigma_i^2=\beta_i^2\sigma_M^2+\sigma_{\varepsilon,i}^2$. **Seul le risque systématique est rémunéré.**

## 5.3 Frontière efficiente, MVP, CAL et CML

La **frontière efficiente** offre le rendement maximal par niveau de risque (au-dessus du **MVP**). Poids du MVP à deux actifs :

$$w_1^{\text{MVP}}=\frac{\sigma_2^2-\sigma_{12}}{\sigma_1^2+\sigma_2^2-2\sigma_{12}}.$$

En ajoutant l'actif sans risque, la **CAL** a pour pente le **ratio de Sharpe** ; la meilleure CAL est tangente à la frontière au **portefeuille de tangence = marché** (**séparation en deux fonds**). La **Capital Market Line** :

$$E(R_C)=R_f+\frac{E(R_M)-R_f}{\sigma_M}\,\sigma_C\quad(\text{pente}=\text{Sharpe du marché}).$$

## 5.4 Le CAPM, la SML et le modèle de marché

$$E(R_i)=R_f+\beta_i\big[E(R_M)-R_f\big],\qquad \beta_i=\frac{\operatorname{Cov}(R_i,R_M)}{\sigma_M^2}=\rho_{i,M}\frac{\sigma_i}{\sigma_M},\qquad \beta_P=\sum_i w_i\beta_i.$$

La **SML** porte le rendement contre $\beta$ ; un actif **au-dessus** est **sous-évalué** ($\alpha>0$). Pour un portefeuille **efficient** ($\rho_{P,M}=1$), $\beta_P=\sigma_P/\sigma_M$ et la SML **se réduit à la CML** : **la CML est la SML restreinte aux efficients**. Le **modèle de marché** $R_i=\alpha_i+\beta_iR_M+\varepsilon_i$ donne $R^2=\rho_{i,M}^2$.

> **Point d'examen.** **CML** = risque **total $\sigma$** (efficients) ; **SML** = **$\beta$** (tout actif). Au-dessus de la SML $\Rightarrow$ sous-évalué.

## 5.5 Hypothèses, critique de Roll, anomalies

**Hypothèses** : agents rationnels averses au risque, **mono-période**, **anticipations homogènes**, prêt/emprunt illimité au **même $R_f$**, marchés **sans frictions**, preneurs de prix. **Critique de Roll** : le **vrai portefeuille de marché est inobservable** $\Rightarrow$ le CAPM n'est pas réellement testable. **Anomalies** : **taille (SMB)**, **valeur (HML)**, **momentum (WML)**, **faible bêta** (la **SML empirique est « trop plate »**) — d'où les modèles multifactoriels (chapitre 6).

## 5.6 Mesures de performance ajustées du risque

$$S=\frac{E(R_P)-R_f}{\sigma_P},\quad T=\frac{E(R_P)-R_f}{\beta_P},\quad \alpha_P=E(R_P)-[R_f+\beta_P(E(R_M)-R_f)],$$
$$\mathrm{IR}=\frac{E(R_P)-E(R_B)}{\sigma(R_P-R_B)},\quad \mathrm{SOR}=\frac{E(R_P)-R_{\min}}{\sigma_{\text{down}}},\quad M^2=R_f+S_P\sigma_M.$$

> **Point d'examen.** **Sharpe/Sortino/$M^2$ $\to$ $\sigma$** ; **Treynor/Jensen $\to$ $\beta$** ; **IR $\to$ tracking error**. Sharpe et Treynor coïncident pour des portefeuilles **parfaitement diversifiés**.

---

# Chapitre 6 — Arbitrage Pricing Theory & Multifactor Models
### Théorie de l'arbitrage et modèles multifactoriels

## 6.1 De l'APT aux modèles multifactoriels

Le CAPM est un modèle **à facteur unique** (le marché). L'**APT** de Ross repose sur l'**absence d'arbitrage** : deux portefeuilles aux **mêmes expositions aux facteurs** doivent offrir le **même rendement espéré**. Le rendement exigé est **linéaire** dans les **bêtas factoriels** :

$$E(R_i)=R_f+\beta_{i,1}RP_1+\beta_{i,2}RP_2+\dots+\beta_{i,k}RP_k,$$

avec $RP_j$ la **prime de risque** du facteur $j$. Le **processus générateur** s'écrit $R_i=E(R_i)+\sum_j\beta_{i,j}F_j+\varepsilon_i$. **APT vs CAPM** : l'APT **n'exige pas** le portefeuille de marché ni les hypothèses fortes du CAPM, mais elle **ne spécifie pas** quels facteurs (sa faiblesse). Le **CAPM est un cas particulier** de l'APT à un facteur.

## 6.2 Types de facteurs et modèles empiriques

Facteurs **macroéconomiques** (PIB, inflation, spreads…), **fondamentaux** (taille, *book-to-market*, momentum…), ou **statistiques** (extraits par ACP, sans interprétation économique a priori).

- **Fama-French (3 facteurs)** : $E(R_i)-R_f=\beta_{mkt}(E(R_M)-R_f)+\beta_{SMB}E(SMB)+\beta_{HML}E(HML)$, avec **SMB** = prime de **taille**, **HML** = prime de **valeur**.
- **Carhart (4 facteurs)** : ajoute le **momentum** **WML** (*Winners Minus Losers*, ou **UMD**).
- **Fama-French (5 facteurs)** : ajoute **rentabilité** et **investissement**. Socle du ***smart beta***.

> **Point d'examen.** APT = **absence d'arbitrage**, **sans** portefeuille de marché ; faiblesse = **facteurs non spécifiés** ; **SMB = taille, HML = valeur, WML/UMD = momentum**.

---

# Chapitre 7 — Data Aggregation and Risk Reporting (BCBS 239)
### Agrégation des données de risque et reporting

## 7.1 Contexte et objectif

Pendant la crise de 2007-2009, beaucoup de banques étaient **incapables d'agréger rapidement** leurs expositions (par contrepartie, produit, groupe). En réponse, le **Comité de Bâle** a publié **BCBS 239** (2013), d'abord pour les **banques d'importance systémique (G-SIB)**. Idée : on ne **gère** que ce qu'on peut **voir** — la **qualité, l'exhaustivité et la rapidité** de l'information de risque conditionnent la solidité.

## 7.2 Les 14 principes en 4 thèmes

- **I. Gouvernance & infrastructure** (P1-2) : responsabilité du **conseil/direction** ; **architecture IT** soutenant l'agrégation, **y compris en crise**.
- **II. Capacités d'agrégation** (P3-6) : **exactitude (*accuracy*)**, **exhaustivité (*completeness*)**, **rapidité (*timeliness*)**, **adaptabilité (*adaptability*)** (requêtes *ad hoc*, scénarios de stress).
- **III. Pratiques de reporting** (P7-11) : exactitude, couverture, **clarté/utilité**, fréquence, **distribution** appropriée.
- **IV. Surveillance prudentielle** (P12-14) : revue, mesures correctives, coopération *home/host*.

> **Point d'examen.** Connaître les **4 thèmes** et les **qualités des données** : **accuracy, completeness, timeliness, adaptability** — et que ces capacités doivent **tenir en période de stress**, pas seulement en temps normal.

---

# Chapitre 8 — Enterprise Risk Management (ERM)
### Gestion des risques d'entreprise

## 8.1 De la gestion en silos à la gestion intégrée

L'**ERM** considère le **portefeuille global** de risques de la firme, géré de façon **transversale** et alignée sur la **stratégie** et l'**appétit** du conseil. **Définition (esprit COSO)** : identifier les événements potentiels, gérer le risque dans les limites de l'appétit, fournir une **assurance raisonnable** quant aux objectifs.

## 8.2 Valeur de l'ERM, capital économique et RAROC

L'ERM révèle **concentrations et corrélations** invisibles en silos et reconnaît que **tous les risques ne se réalisent pas ensemble** $\to$ besoin de capital **inférieur** à la somme des silos (**diversification**). Le **capital économique** est l'estimation **interne** du capital couvrant les **pertes inattendues** à un niveau de confiance donné ($\ne$ **capital réglementaire**). Le **RAROC** mesure la rentabilité ajustée du risque :

$$\mathrm{RAROC}=\frac{\text{Revenus}-\text{Coûts}-EL+\text{Rendement du capital}}{\text{Capital économique}}.$$

Une activité **crée de la valeur** si son **RAROC dépasse le coût des fonds propres** (*hurdle rate*).

## 8.3 Cadres et risques émergents

Référentiels : **COSO ERM** et **ISO 31000**. Le **cadre d'appétit pour le risque** décline l'appétit du conseil en **tolérances** et **limites**. Risques émergents : **climat** (physique/transition), **cyber**, **IA/ML** (nouveaux risques de modèle), tiers, géopolitique.

> **Point d'examen.** **Capital économique (interne, UL)** $\ne$ **réglementaire** ; l'ERM réduit le capital **grâce à la diversification** ; **RAROC > coût du capital** $\Rightarrow$ création de valeur.

---

# Chapitre 9 — Learning from Financial Disasters
### Les leçons des grandes catastrophes financières

GARP attend, pour chaque cas, **le type de risque illustré** et **la leçon**. Organisation **par type de risque** :

- **Risque de taux — *Savings & Loan* (années 1980)** : prêts hypothécaires à **taux fixe long** financés par des **dépôts courts** ; la hausse des taux courts a rendu la marge **négative**. *Mismatch de duration*.
- **Liquidité de financement — Continental Illinois (1984), Northern Rock (2007), Lehman (2008)** : dépendance à un **financement de gros court** pour des actifs **longs/illiquides** ; gel du marché $\to$ **ruée**, incapacité à se refinancer.
- **Mise en œuvre d'une couverture — Metallgesellschaft (1993)** : couverture *stack-and-roll* de contrats longs par des futures courts ; **risque de base/rollover** + **risque de liquidité** (appels de marge en cash sur gains latents futurs).
- **Risque de modèle — LTCM (1998), London Whale (2012)** : modèles supposant des corrélations « normales » ; en crise, **corrélations $\to$ 1**, **levier** et **liquidité** font le reste. La VaR peut aussi être **manipulée** (London Whale).
- **Rogue trading / opérationnel — Barings (Leeson, 1995), Société Générale (Kerviel, 2008), AIB (Rusnak), UBS (Adoboli)** : défaut de **séparation front/back office**, contrôles internes insuffisants. La perte se voit sur les marchés, la **cause est opérationnelle**.
- **Ingénierie & *suitability* — Bankers Trust / P&G (1994), Orange County (1994)** : produits **complexes et à fort levier** vendus à des clients qui ne les comprenaient pas ; **risque de pertinence** et **réputationnel**.
- **Réputation — Volkswagen (2015)** : fraude aux tests d'émissions $\to$ crise **réputationnelle** née d'un manquement éthique.
- **Cyber — Banque centrale du Bangladesh / SWIFT (2016)** : faux ordres SWIFT, détournement de fonds.
- **Fraude — Madoff (2008)** : **schéma de Ponzi** masqué par l'**opacité** et l'échec de la **due diligence**.

> **Point d'examen.** Mémorise le couple **« incident $\to$ type de risque »** : S&L = **taux** ; Lehman/Northern Rock = **liquidité de financement** ; Metallgesellschaft = **couverture/base + liquidité** ; LTCM/London Whale = **modèle** ; Barings/SocGen = **opérationnel** ; P&G/Orange County = **suitability** ; VW = **réputation** ; Bangladesh = **cyber** ; Madoff = **fraude**.

---

# Chapitre 10 — Anatomy of the Great Financial Crisis (2007-2009)
### Anatomie de la grande crise financière

## 10.1 Causes et moteur

**Taux bas**, afflux d'épargne et **course au rendement** ont nourri une **bulle immobilière** et des prêts **subprime**, sur l'hypothèse (fausse) que **les prix de l'immobilier ne baisseraient pas tous ensemble**. Le **modèle *originate-to-distribute*** a **relâché la sélection** ; la **titrisation** (RMBS $\to$ CDO $\to$ CDO$^{2}$) a **opacifié** le risque ; les **agences de notation** (conflit d'intérêts) l'ont **sur-noté** ; le **système bancaire parallèle** à fort **levier**, financé par **repo** court terme, et la **concentration** chez **AIG** (CDS) ont préparé le choc.

## 10.2 La mécanique et la chronologie

Spirale : **pertes** sur les structurés $\to$ **gel du financement** (*funding liquidity*) $\to$ **ventes forcées (*fire sales*)** $\to$ nouvelles baisses $\to$ **contagion** par interconnexions $\to$ **risque systémique**. Chronologie : 2007 effondrement des fonds subprime ; **mars 2008 Bear Stearns** ; **sept. 2008** mise sous tutelle de **Fannie/Freddie**, **faillite de Lehman (point de bascule)**, sauvetage d'**AIG**, un *money market fund* **« casse la barre du dollar »** ; **oct. 2008** plan **TARP**.

## 10.3 Réformes

**Bâle III** (plus de capital de meilleure qualité, **ratio de levier**, coussins, **LCR/NSFR**), **Dodd-Frank** (FSOC, **stress tests CCAR/DFAST**, règle **Volcker**, **compensation centrale**), approche **macroprudentielle**, encadrement de la **titrisation** et des **agences**.

> **Point d'examen.** Sache **raconter la spirale** (pertes $\to$ gel du financement $\to$ ventes forcées $\to$ contagion $\to$ systémique) et **relier cause $\to$ réforme** (manque de capital $\to$ Bâle III ; opacité des dérivés $\to$ compensation centrale). **Lehman** = point de bascule.

---

# Chapitre 11 — GARP Code of Conduct
### Le Code de conduite du GARP

## 11.1 Portée et principes

Le **Code de conduite du GARP** s'impose à **tous les membres et candidats**, partout. Sa violation peut entraîner des **sanctions** jusqu'au **retrait de la certification**. **Principes fondamentaux** : (1) **Intégrité professionnelle et conduite éthique** (honnêteté, diligence, compétence ; pas de fraude ni de **dénaturation** ; **jugement indépendant**) ; (2) **Conflits d'intérêts** (les **identifier, éviter ou pleinement divulguer**) ; (3) **Confidentialité** (protéger l'information des clients/employeur). **Standards professionnels** : **responsabilités fondamentales** (compétence, **respect des lois**) et **bonnes pratiques** (méthodes reconnues, distinguer **faits et opinions**).

> **Point d'examen.** Mises en situation : **divulguer** les conflits ; **ne jamais utiliser/révéler** d'information confidentielle ; **ne pas dénaturer** ses résultats/compétences ; **respecter la loi** ; **faire primer** l'intégrité et l'intérêt des clients/employeurs/du public. En cas de doute, l'option **la plus transparente et prudente** est la bonne.

---

---

# Banque de QCM par chapitre (format examen)

> **Mode d'emploi.** QCM en anglais, **format examen FRM** (une seule bonne réponse). Numérotation **par chapitre** (Q*chapitre.numéro*). Le **corrigé commenté en français**, avec **renvoi à la section** (§) de la synthèse, suit la banque. Compte $\approx$ 1 min 30 par question ; reprends chaque erreur via son renvoi. *Remarque : les véritables questions d'examen du GARP ne sont pas publiées ; ces QCM sont des questions **originales au format examen**, sans attribution à une session passée (qui serait fictive).*

## QCM — Chapitre 1 · The Building Blocks of Risk Management

**Q1.1** Which best distinguishes *risk* from *Knightian uncertainty*? — **(A)** Risk always implies a loss **(B)** Risk has quantifiable probabilities; Knightian uncertainty does not **(C)** Risk is qualitative **(D)** They are identical

**Q1.2** The average anticipated loss a bank prices and provisions for is — **(A)** unexpected loss **(B)** expected loss **(C)** stress loss **(D)** economic capital

**Q1.3** Economic capital is held primarily to absorb — **(A)** expected losses **(B)** unexpected losses **(C)** operating costs **(D)** taxes

**Q1.4** A rogue trader's hidden losses, realized in the equity market, are classified as — **(A)** market risk **(B)** credit risk **(C)** operational risk **(D)** strategic risk

**Q1.5** Inability to refinance short-term funding for unimpaired long-term assets is — **(A)** market liquidity risk **(B)** funding liquidity risk **(C)** credit migration risk **(D)** reputational risk

**Q1.6** The correct ordering (broadest to most operational) is — **(A)** limits $\ge$ tolerance $\ge$ appetite $\ge$ capacity **(B)** capacity $\ge$ appetite $\ge$ tolerance $\ge$ limits **(C)** appetite $\ge$ capacity $\ge$ limits $\ge$ tolerance **(D)** tolerance $\ge$ limits $\ge$ capacity $\ge$ appetite

**Q1.7** The maximum risk a firm can bear before threatening solvency is its — **(A)** appetite **(B)** tolerance **(C)** capacity **(D)** limit

**Q1.8** Which is NOT one of the four basic risk responses? — **(A)** accept **(B)** mitigate **(C)** transfer **(D)** securitize regardless of cost

**Q1.9** Using $EL=PD\times LGD\times EAD$ with $PD=2\%$, $LGD=40\%$, $EAD=\$10\text{m}$, the expected loss is — **(A)** \$40,000 **(B)** \$80,000 **(C)** \$200,000 **(D)** \$800,000

**Q1.10** A bond downgrade (no default) is an example of — **(A)** market risk only **(B)** credit (migration) risk **(C)** operational risk **(D)** liquidity risk

**Q1.11** Which statement about risk management is most accurate? — **(A)** It eliminates risk and guarantees higher returns **(B)** It can destroy value (e.g., model risk, false security) **(C)** It is purely compliance **(D)** Diversification removes systematic risk

**Q1.12** With a recovery rate of 30%, the LGD is — **(A)** 30% **(B)** 50% **(C)** 70% **(D)** 100%

**Q1.13** Risk is best defined as — **(A)** the certainty of a loss **(B)** the variability/dispersion of future outcomes around an expectation **(C)** only the downside **(D)** the absence of return

**Q1.14** "Unknown unknowns" refers to — **(A)** known and measured risks **(B)** unknowable risks / unprecedented events **(C)** priced risks **(D)** hedged risks

**Q1.15** The risk premium is — **(A)** the risk-free rate **(B)** the additional expected return demanded for bearing risk **(C)** a guaranteed return **(D)** a regulatory charge

**Q1.16** The credit expected-loss identity is — **(A)** $PD+LGD+EAD$ **(B)** $PD\times LGD\times EAD$ **(C)** $PD\times EAD-LGD$ **(D)** $PD/LGD$

**Q1.17** EAD stands for — **(A)** Exposure at Default **(B)** Expected Average Default **(C)** Equity Asset Duration **(D)** Exposure After Discount

**Q1.18** Market liquidity risk is — **(A)** the inability to obtain funding **(B)** the inability to sell an asset without moving its price **(C)** the risk of default **(D)** an operational failure

**Q1.19** A trader who cannot sell a large bond position without a steep price concession faces — **(A)** funding liquidity risk **(B)** market liquidity risk **(C)** credit risk **(D)** reputational risk

**Q1.20** Systemic risk is — **(A)** firm-specific **(B)** the risk that one failure propagates across the whole system **(C)** purely operational **(D)** fully diversifiable

**Q1.21** Reputational risk is most often — **(A)** a primary standalone risk **(B)** a consequence of other risk events **(C)** a form of market risk **(D)** unmeasurable by definition

**Q1.22** The four steps of the risk-management process are — **(A)** identify, measure, manage, monitor **(B)** buy, hold, sell, hedge **(C)** price, fund, lend, report **(D)** model, validate, audit, disclose

**Q1.23** In the "four T's", *treat/mitigate* means — **(A)** accept as is **(B)** reduce the risk (controls, hedging) **(C)** transfer it to a third party **(D)** exit the activity

**Q1.24** Risk appetite is — **(A)** the absolute maximum bearable **(B)** the level of risk the board chooses to take to meet objectives **(C)** an operational VaR limit **(D)** a regulatory minimum

**Q1.25** Risk limits are — **(A)** the board's strategic appetite **(B)** concrete operational bounds (VaR, notional, stop-loss) **(C)** the firm's capacity **(D)** tolerance by risk type

**Q1.26** Operational risk explicitly EXCLUDES — **(A)** internal fraud **(B)** system failures **(C)** strategic and reputational risk **(D)** external events

**Q1.27** $EL$ with $PD=1\%$, $LGD=50\%$, $EAD=\$20\text{m}$ is — **(A)** \$50,000 **(B)** \$100,000 **(C)** \$200,000 **(D)** \$1,000,000

**Q1.28** If the recovery rate is 60%, the LGD is — **(A)** 40% **(B)** 60% **(C)** 100% **(D)** 24%

**Q1.29** The basic statistical measure of risk (dispersion) is — **(A)** the mean **(B)** the standard deviation **(C)** the mode **(D)** the median

**Q1.30** Which is a source of value DESTRUCTION by risk management? — **(A)** better capital allocation **(B)** a false sense of security / model risk **(C)** lower distress costs **(D)** correct risk pricing

**Q1.31** A sovereign defaulting on its debt is primarily — **(A)** market risk **(B)** credit (sovereign) risk **(C)** operational risk **(D)** liquidity risk

**Q1.32** Unexpected loss is best described as — **(A)** the mean loss **(B)** the variability of losses around the expected loss **(C)** a financing cost **(D)** always zero

## QCM — Chapitre 2 · How Firms and Individuals Manage Risk

**Q2.1** Under Modigliani-Miller (perfect markets), corporate hedging is — **(A)** always value-increasing **(B)** required by regulators **(C)** irrelevant, since shareholders can replicate it themselves **(D)** value-destroying

**Q2.2** The Modigliani-Miller irrelevance result assumes — **(A)** no taxes, no transaction or distress costs, symmetric information **(B)** costly bankruptcy **(C)** progressive taxes **(D)** managerial risk aversion

**Q2.3** In the real world, the strongest rationale for corporate hedging is — **(A)** to eliminate systematic risk **(B)** to increase expected cash flows directly **(C)** to avoid all taxes **(D)** to reduce the probability and cost of financial distress

**Q2.4** Hedging can reduce a firm's expected tax bill when the tax schedule is — **(A)** flat **(B)** convex (progressive) **(C)** regressive **(D)** zero

**Q2.5** The underinvestment problem that hedging mitigates arises when — **(A)** the firm holds excess cash **(B)** leverage is zero **(C)** cash shortfalls force the firm to forgo positive-NPV projects **(D)** dividends are too high

**Q2.6** Managers and employees often favor hedging because their personal wealth/human capital is — **(A)** concentrated and undiversified in the firm **(B)** fully diversified **(C)** irrelevant to decisions **(D)** hedged by the market

**Q2.7** Reducing cash-flow volatility can lower a firm's cost of capital mainly by — **(A)** raising systematic risk **(B)** increasing taxes **(C)** reducing the probability and cost of financial distress **(D)** removing idiosyncratic risk

**Q2.8** A genuine drawback of hedging is — **(A)** lower distress costs **(B)** transaction costs, basis risk, and forgone upside **(C)** smoother earnings **(D)** improved planning

**Q2.9** A forward contract — **(A)** gives the right but not the obligation **(B)** requires an upfront premium **(C)** locks in a price with no upfront premium and obligates both parties **(D)** has an asymmetric payoff

**Q2.10** Compared with a forward, an option — **(A)** obligates exercise **(B)** is always cheaper **(C)** cannot be exchange-traded **(D)** offers an asymmetric payoff (keeps upside) for a premium

**Q2.11** Relative to a forward, an exchange-traded future features — **(A)** no standardization **(B)** daily marking-to-market and margin **(C)** higher counterparty risk **(D)** an upfront premium

**Q2.12** An interest-rate swap is — **(A)** an option on rates **(B)** a loan **(C)** an exchange of fixed- for floating-rate cash flows **(D)** an insurance policy

**Q2.13** Purchasing insurance is an example of risk — **(A)** retention **(B)** transfer **(C)** avoidance **(D)** diversification

**Q2.14** Diversification primarily reduces — **(A)** systematic risk **(B)** basis risk **(C)** idiosyncratic risk, at little cost **(D)** all risk to zero

**Q2.15** Basis risk arises when — **(A)** the hedge perfectly matches the exposure **(B)** the hedging instrument and the exposure are imperfectly correlated **(C)** there is no hedge in place **(D)** an option expires worthless

**Q2.16** A natural hedge is — **(A)** a derivative overlay **(B)** an insurance contract **(C)** an offsetting exposure arising from operations (e.g., revenues and costs in the same currency) **(D)** a credit default swap

**Q2.17** "Bear the risks in which you have a comparative advantage" implies — **(A)** retain core risks and transfer peripheral ones **(B)** hedge everything **(C)** transfer core risks **(D)** avoid all risk

**Q2.18** Risk mapping (decomposition) is used to — **(A)** conceal exposures **(B)** express positions in terms of common risk factors to aggregate and hedge them **(C)** increase leverage **(D)** avoid reporting

**Q2.19** Hedging differs from speculation in that hedging — **(A)** seeks profit from price moves **(B)** uses no derivatives **(C)** reduces an existing exposure, whereas speculation takes on new risk for expected profit **(D)** is always costless

**Q2.20** A poorly constructed hedge can — **(A)** never lose money **(B)** create basis losses or even a speculative position **(C)** eliminate all risk **(D)** always reduce costs

**Q2.21** The originate-to-distribute model weakens lenders' incentive to — **(A)** maximize fee income **(B)** sell loans **(C)** screen and monitor borrowers carefully **(D)** diversify

**Q2.22** A recognized benefit of corporate hedging is — **(A)** higher systematic risk **(B)** more stable earnings and cash flows, aiding planning and investment **(C)** guaranteed higher profit **(D)** elimination of basis risk

**Q2.23** The 2008 experience showed that transferring risk can — **(A)** always remove it from the system **(B)** reduce systemic risk **(C)** opacify and concentrate it (e.g., in AIG) **(D)** eliminate counterparty risk

**Q2.24** Keeping an exposure on the balance sheet without offsetting it is — **(A)** risk transfer **(B)** risk retention **(C)** risk avoidance **(D)** diversification

**Q2.25** Individuals buy insurance mainly because they are — **(A)** risk-seeking **(B)** risk-averse and undiversified against large personal losses **(C)** tax-exempt **(D)** fully diversified

**Q2.26** A firm should hedge a given exposure when — **(A)** hedging is free **(B)** regulators require it **(C)** the expected benefit (lower distress/taxes) exceeds the cost (transaction costs, basis risk) **(D)** never

## QCM — Chapitre 3 · The Governance of Risk Management

**Q3.1** The board's primary risk-governance role is to — **(A)** execute daily trades **(B)** run model validation **(C)** set/approve the risk appetite and oversee the framework **(D)** prepare the financial statements

**Q3.2** To preserve independence, the Chief Risk Officer should report mainly to — **(A)** the head of trading **(B)** the board (risk committee) **(C)** the front office **(D)** external auditors

**Q3.3** Under the "three lines of defense," the FIRST line is — **(A)** the business units that own and manage their risks **(B)** the compliance department **(C)** internal audit **(D)** the external auditor

**Q3.4** The SECOND line of defense is — **(A)** the business units **(B)** the external auditor **(C)** risk management and compliance **(D)** internal audit

**Q3.5** The THIRD line of defense is — **(A)** the trading desk **(B)** risk management **(C)** compliance **(D)** internal audit (independent assurance)

**Q3.6** Internal audit's role regarding risk is to — **(A)** set risk limits **(B)** provide independent assurance on controls, not to manage risk **(C)** take trading positions **(D)** approve the risk appetite

**Q3.7** The audit committee of the board primarily oversees — **(A)** marketing **(B)** product pricing **(C)** financial reporting and internal control **(D)** trading strategy

**Q3.8** Sound risk-aligned compensation is typically — **(A)** all cash, paid immediately **(B)** fixed salary only **(C)** unrelated to performance **(D)** deferred, risk-adjusted, multi-year, with clawback

**Q3.9** A clawback provision allows a firm to — **(A)** increase bonuses automatically **(B)** recover compensation already paid after losses or misconduct **(C)** defer taxes **(D)** waive regulation

**Q3.10** "Risk shifting" (asset substitution) describes the tendency of — **(A)** creditors to reduce risk **(B)** regulators to raise capital **(C)** shareholders, especially near distress, to take excessive risk at creditors' expense **(D)** auditors to overstate income

**Q3.11** A strong risk culture is characterized by — **(A)** suppressing bad news **(B)** tone from the top, accountability, escalation, and constructive challenge **(C)** rewarding only revenue **(D)** ignoring limits

**Q3.12** Corporate governance refers to — **(A)** the system of rules and relationships directing and controlling a firm **(B)** the marketing plan **(C)** the trading book **(D)** the tax code

**Q3.13** The board's risk committee is responsible for — **(A)** executing hedges **(B)** auditing accounts **(C)** overseeing the risk framework and appetite **(D)** setting product prices

**Q3.14** CRO independence matters because it — **(A)** raises trading revenue **(B)** prevents revenue-generating units from overriding risk controls **(C)** lowers taxes **(D)** removes the need for audit

**Q3.15** Which is a SECOND-line function? — **(A)** proprietary trading **(B)** independent audit **(C)** compliance **(D)** loan origination

**Q3.16** A FIRST-line responsibility is — **(A)** day-to-day risk-taking and embedded controls within the business **(B)** independent assurance **(C)** external audit **(D)** setting group appetite

**Q3.17** A classic agency conflict in banks is between — **(A)** customers and suppliers **(B)** shareholders and debtholders/depositors **(C)** auditors and regulators **(D)** traders and clients only

**Q3.18** Excessive short-term cash bonuses can — **(A)** reduce risk-taking **(B)** improve long-termism **(C)** encourage excessive risk-taking and short-termism **(D)** lower turnover

**Q3.19** "Tone at the top" refers to — **(A)** the noise on the trading floor **(B)** leadership setting ethical and risk standards **(C)** the audit schedule **(D)** the dividend policy

**Q3.20** Separating the roles of CEO and board chair tends to improve — **(A)** trading profits **(B)** marketing reach **(C)** board independence and oversight **(D)** tax efficiency

**Q3.21** Stakeholders in risk governance include — **(A)** only shareholders **(B)** only regulators **(C)** only management **(D)** shareholders, creditors, regulators, employees, and customers

**Q3.22** Internal audit must be independent of — **(A)** nothing **(B)** the activities it audits, reporting to the audit committee **(C)** the regulator **(D)** the shareholders only

**Q3.23** A risk appetite statement should be — **(A)** drafted by traders **(B)** kept secret **(C)** approved by the board and cascaded into concrete limits **(D)** ignored in practice

**Q3.24** A board that merely rubber-stamps management decisions exhibits — **(A)** strong governance **(B)** weak governance and oversight **(C)** good independence **(D)** effective challenge

**Q3.25** Deferring and risk-adjusting bonuses aligns pay with — **(A)** short-term revenue **(B)** headcount **(C)** long-term, risk-adjusted performance **(D)** market share

**Q3.26** The three-lines-of-defense model aims to — **(A)** increase leverage **(B)** clarify roles and avoid gaps/overlaps in risk control **(C)** centralize all trading **(D)** remove the board

**Q3.27** Conflicts between management and shareholders are classic — **(A)** liquidity problems **(B)** market-risk events **(C)** principal-agent (agency) problems **(D)** operational losses

**Q3.28** Regulators expect ultimate responsibility for risk to rest with — **(A)** junior staff **(B)** the external auditor **(C)** the regulator itself **(D)** the board and senior management

## QCM — Chapitre 4 · Credit Risk Transfer Mechanisms

**Q4.1** A primary motivation for transferring credit risk is to — **(A)** increase default rates **(B)** free up capital and manage concentration **(C)** raise funding costs **(D)** avoid all reporting

**Q4.2** In a CDS, the protection buyer — **(A)** pays a periodic premium and is compensated on a credit event **(B)** receives the premium **(C)** must own the bond **(D)** has no obligations

**Q4.3** The CDS protection seller — **(A)** is short credit risk **(B)** pays the premium **(C)** receives the premium and pays out on default (effectively long credit risk) **(D)** is fully hedged

**Q4.4** The "credit triangle" links the CDS spread to — **(A)** $s\approx\lambda+R$ **(B)** $s\approx\lambda(1-R)$ **(C)** $s\approx\lambda/R$ **(D)** $s\approx R-\lambda$

**Q4.5** With a CDS spread $s=240$ bps and recovery $R=40\%$, the implied hazard rate is — **(A)** 1.4% **(B)** 2.4% **(C)** 4.0% **(D)** 6.0%

**Q4.6** A "naked" CDS is — **(A)** a fully collateralized swap **(B)** buying protection without owning the underlying debt (speculative) **(C)** a risk-free position **(D)** an insurance contract on one's own assets

**Q4.7** With hazard rate $\lambda=2\%$ and recovery $R=30\%$, the approximate CDS spread is — **(A)** 60 bps **(B)** 200 bps **(C)** 140 bps **(D)** 290 bps

**Q4.8** Securitization pools assets and issues — **(A)** a single bond **(B)** equity shares only **(C)** insurance policies **(D)** tranches of differing seniority through an SPV

**Q4.9** The equity (first-loss) tranche — **(A)** absorbs the first losses and carries the highest risk/yield **(B)** is paid first **(C)** is risk-free **(D)** loses last

**Q4.10** The senior tranche — **(A)** absorbs first losses **(B)** has the highest coupon **(C)** is paid first / loses last, with the lowest risk **(D)** is unrated

**Q4.11** For a tranche with attachment $A$ and detachment $D$, the fraction of tranche notional lost given portfolio loss $L$ is — **(A)** $L/(D-A)$ **(B)** $[\min(L,D)-A]^{+}/(D-A)$ **(C)** $(L-D)/A$ **(D)** $L\times(D-A)$

**Q4.12** A tranche with $A=3\%$, $D=7\%$ facing a portfolio loss $L=5\%$ loses — **(A)** 0% **(B)** 25% **(C)** 50% **(D)** 100%

**Q4.13** The same tranche ($A=3\%$, $D=7\%$) with $L=10\%$ loses — **(A)** 0% **(B)** 50% **(C)** 75% **(D)** 100%

**Q4.14** The same tranche ($A=3\%$, $D=7\%$) with $L=2\%$ loses — **(A)** 0% **(B)** 25% **(C)** 50% **(D)** 100%

**Q4.15** An increase in default correlation $\rho$ — **(A)** helps the senior tranche **(B)** hurts the senior tranche and benefits the equity tranche **(C)** has no effect **(D)** benefits all tranches

**Q4.16** The portfolio's total expected loss is — **(A)** increasing in $\rho$ **(B)** decreasing in $\rho$ **(C)** invariant to $\rho$ (only the loss-distribution shape changes) **(D)** zero

**Q4.17** Higher default correlation makes the loss distribution — **(A)** thinner-tailed **(B)** more bimodal / fatter-tailed (more extreme mass) **(C)** exactly normal **(D)** degenerate

**Q4.18** The Gaussian copula model was famously criticized for — **(A)** overstating recoveries **(B)** ignoring spreads **(C)** underestimating tail dependence (joint extreme defaults) **(D)** being too conservative

**Q4.19** AIG's near-collapse stemmed from — **(A)** holding too much cash **(B)** selling massive CDS protection with collateral/downgrade triggers (wrong-way and liquidity risk) **(C)** buying protection **(D)** shorting equities

**Q4.20** The originate-to-distribute model — **(A)** weakened lenders' incentive to screen borrowers **(B)** improved underwriting **(C)** reduced systemic risk **(D)** eliminated default risk

**Q4.21** A synthetic CDO is constructed using — **(A)** cash bonds only **(B)** equities **(C)** CDS (credit derivatives) rather than cash bonds **(D)** government bills

**Q4.22** A first-to-default basket pays out on — **(A)** the last name to default **(B)** the first name in the basket to default **(C)** all names simultaneously **(D)** no default

**Q4.23** A credit-linked note (CLN) — **(A)** is a risk-free deposit **(B)** removes all credit risk **(C)** embeds a CDS so the investor bears credit risk for extra yield **(D)** is an equity share

**Q4.24** "Tranching" creates — **(A)** securities of differing credit quality from the same asset pool **(B)** a single risk class **(C)** only equity **(D)** insurance contracts

**Q4.25** The CDS-bond basis is — **(A)** the bond's coupon **(B)** the difference between the CDS spread and the bond's credit spread **(C)** the recovery rate **(D)** the hazard rate

**Q4.26** With zero recovery ($R=0$), the credit triangle gives — **(A)** $s\approx 0$ **(B)** $s\approx R$ **(C)** $s\approx\lambda$ **(D)** $s\approx 1/\lambda$

**Q4.27** Monoline insurers ran into trouble by — **(A)** holding only cash **(B)** guaranteeing structured credit and then being downgraded **(C)** avoiding credit risk **(D)** issuing equity

**Q4.28** A protection buyer who also owns the underlying bond holds — **(A)** a doubly long credit position **(B)** pure speculation **(C)** a hedged (offsetting) credit position **(D)** an equity position

**Q4.29** Wrong-way risk in a CDS is — **(A)** exposure falling as the counterparty weakens **(B)** exposure rising as the counterparty's own credit deteriorates (correlated) **(C)** zero exposure **(D)** market risk only

**Q4.30** Relative to equity and senior tranches, the mezzanine tranche — **(A)** absorbs the very first losses **(B)** is always AAA **(C)** sits between equity and senior in loss priority **(D)** is risk-free

**Q4.31** The default intensity (hazard rate) $\lambda$ measures — **(A)** the recovery rate **(B)** the conditional instantaneous probability of default per unit time **(C)** the coupon **(D)** the bond price

**Q4.32** Securitization can benefit the originator by — **(A)** raising default rates **(B)** increasing concentration **(C)** worsening capital ratios **(D)** transferring risk and freeing regulatory capital

**Q2.27** The "only free lunch in finance" usually refers to — **(A)** leverage **(B)** arbitrage **(C)** diversification **(D)** insider trading

**Q2.28** Adverse selection in insurance means — **(A)** insurers refuse all clients **(B)** higher-risk parties are more likely to seek insurance **(C)** the insured take more risk once covered **(D)** premiums are always fair

**Q2.29** Moral hazard in insurance means — **(A)** low-risk parties buy more cover **(B)** premiums fall over time **(C)** the insured take on more risk once they are covered **(D)** claims are always denied

**Q2.30** An oil producer that sells crude-oil futures to fix its selling price has put on — **(A)** an asymmetric hedge **(B)** a symmetric hedge that locks in the price and forgoes upside **(C)** a speculative long **(D)** an insurance policy

**Q2.31** Margin calls on a derivative hedge expose the firm primarily to — **(A)** credit migration risk **(B)** reputational risk **(C)** liquidity (cash-flow timing) risk **(D)** equity risk

**Q2.32** When a bank securitizes and sells its loans, the aggregate risk in the financial system is — **(A)** eliminated **(B)** doubled **(C)** not eliminated, only redistributed (and possibly concentrated) **(D)** converted into equity

**Q3.29** The risk committee differs from the audit committee in that it — **(A)** audits past transactions **(B)** prepares the accounts **(C)** oversees the forward-looking risk profile, whereas audit checks controls after the fact **(D)** sets dividends

**Q3.30** A "malus" adjustment — **(A)** increases guaranteed pay **(B)** reduces unvested deferred compensation before payout when risks crystallize **(C)** is a tax rebate **(D)** raises leverage

**Q3.31** Effective governance requires the board to be — **(A)** dominated by management **(B)** purely ceremonial **(C)** sufficiently independent and competent to challenge management **(D)** focused only on revenue

**Q3.32** The ultimate purpose of risk governance is to — **(A)** maximize trading volume **(B)** remove all risk **(C)** ensure risk-taking stays aligned with the approved appetite and stakeholders' interests **(D)** replace the regulator

## QCM — Chapitre 5 · Modern Portfolio Theory & the CAPM

**Q5.1** A portfolio invests 60% in asset 1 ($E(R)=10\%$) and 40% in asset 2 ($E(R)=5\%$). Its expected return is — **(A)** 6.5% **(B)** 7.0% **(C)** 7.5% **(D)** 8.0%

**Q5.2** A portfolio invests 40% in asset 1 ($\sigma=30\%$) and 60% in asset 2 ($\sigma=20\%$), $\rho=0.5$. The portfolio standard deviation is closest to — **(A)** 18.0% **(B)** 20.8% **(C)** 24.0% **(D)** 25.0%

**Q5.3** With $\rho=0.4$, $\sigma_1=25\%$, $\sigma_2=15\%$, the covariance is — **(A)** 0.0150 **(B)** 0.0600 **(C)** 0.0094 **(D)** 0.1500

**Q5.4** With $\operatorname{Cov}=0.018$, $\sigma_1=30\%$, $\sigma_2=20\%$, the correlation is — **(A)** 0.18 **(B)** 0.30 **(C)** 0.60 **(D)** 0.90

**Q5.5** In matrix form, portfolio variance equals — **(A)** $\mathbf{w}^\top\boldsymbol\mu$ **(B)** $\mathbf{w}^\top\Sigma\mathbf{w}$ **(C)** $\boldsymbol\mu^\top\Sigma\boldsymbol\mu$ **(D)** $\Sigma\mathbf{w}$

**Q5.6** Two assets each with $\sigma=20\%$, equally weighted, $\rho=-1$: portfolio $\sigma$ is — **(A)** 0% **(B)** 10% **(C)** 14.1% **(D)** 20%

**Q5.7** Two assets each with $\sigma=20\%$, equally weighted, $\rho=0$: portfolio $\sigma$ is — **(A)** 0% **(B)** 10% **(C)** 14.1% **(D)** 20%

**Q5.8** A portfolio holds 50%/30%/20% with $E(R)=8\%/12\%/20\%$. Its expected return is — **(A)** 10.0% **(B)** 11.6% **(C)** 13.3% **(D)** 40.0%

**Q5.9** Diversification eliminates which type of risk? — **(A)** systematic **(B)** idiosyncratic **(C)** market **(D)** all risk

**Q5.10** For an equally weighted portfolio, as $N\to\infty$ the variance approaches — **(A)** 0 **(B)** the average variance **(C)** the average covariance **(D)** the largest variance

**Q5.11** Which risk is rewarded with a premium? — **(A)** idiosyncratic **(B)** diversifiable **(C)** systematic **(D)** total

**Q5.12** In $\sigma_i^2=\beta_i^2\sigma_m^2+\sigma_\varepsilon^2$, the first term represents — **(A)** idiosyncratic risk **(B)** systematic risk **(C)** total risk **(D)** tracking error

**Q5.13** The efficient frontier is — **(A)** all possible portfolios **(B)** the set offering maximum return per risk level, above the MVP **(C)** the part below the MVP **(D)** the CML

**Q5.14** The two-asset minimum-variance-portfolio weight is — **(A)** $\frac{\sigma_2^2-\sigma_{12}}{\sigma_1^2+\sigma_2^2-2\sigma_{12}}$ **(B)** $\frac{\sigma_1^2}{\sigma_1^2+\sigma_2^2}$ **(C)** $\frac{\mu_1-R_f}{\sigma_1}$ **(D)** $\frac{\sigma_2}{\sigma_1+\sigma_2}$

**Q5.15** MVP with $\sigma_1=10\%$, $\sigma_2=20\%$, $\rho=0$: $w_1$ is — **(A)** 0.20 **(B)** 0.50 **(C)** 0.80 **(D)** 0.67

**Q5.16** With $\rho=-1$, $\sigma_1=10\%$, $\sigma_2=15\%$, the riskless weight $w_1$ is — **(A)** 0.40 **(B)** 0.50 **(C)** 0.60 **(D)** 0.67

**Q5.17** The slope of the Capital Market Line equals — **(A)** the market beta **(B)** the market Sharpe ratio **(C)** the Treynor ratio **(D)** zero

**Q5.18** CML with $R_f=2\%$, $E(R_m)=10\%$, $\sigma_m=20\%$: a portfolio with $\sigma=15\%$ has expected return — **(A)** 6% **(B)** 7% **(C)** 8% **(D)** 9%

**Q5.19** To achieve $\sigma=15\%$ on the CML ($\sigma_m=20\%$), the weight in the market portfolio is — **(A)** 50% **(B)** 60% **(C)** 75% **(D)** 100%

**Q5.20** The two-fund separation theorem implies investors hold — **(A)** different risky portfolios **(B)** the same risky (market) portfolio plus the risk-free asset in varying proportions **(C)** only the risk-free asset **(D)** only equities

**Q5.21** Beta with $\operatorname{Cov}(R_i,R_m)=0.024$ and $\sigma_m^2=0.016$ is — **(A)** 0.67 **(B)** 1.20 **(C)** 1.50 **(D)** 2.40

**Q5.22** CAPM required return with $R_f=2\%$, $E(R_m)=8\%$, $\beta=1.5$ is — **(A)** 9% **(B)** 10% **(C)** 11% **(D)** 12%

**Q5.23** Beta with $\rho=0.6$, $\sigma_i=25\%$, $\sigma_m=15\%$ is — **(A)** 0.36 **(B)** 0.60 **(C)** 1.00 **(D)** 1.67

**Q5.24** A stock with $\beta=0$ has an expected return of — **(A)** 0 **(B)** $R_f$ **(C)** $E(R_m)$ **(D)** a negative value

**Q5.25** A defensive stock has — **(A)** $\beta>1$ **(B)** $\beta<1$ **(C)** $\beta=1$ **(D)** $\beta<0$

**Q5.26** Portfolio beta with weights 0.5/0.3/0.2 and betas 1.2/0.8/1.5 is — **(A)** 1.00 **(B)** 1.14 **(C)** 1.20 **(D)** 1.30

**Q5.27** A stock plotting above the SML is — **(A)** fairly valued **(B)** undervalued ($\alpha>0$) **(C)** overvalued **(D)** risk-free

**Q5.28** Implied market risk premium when CAPM $E(R)=11\%$, $\beta=1.5$, $R_f=2\%$ is — **(A)** 4% **(B)** 5% **(C)** 6% **(D)** 9%

**Q5.29** The Security Market Line plots expected return against — **(A)** $\sigma$ **(B)** $\beta$ **(C)** variance **(D)** tracking error

**Q5.30** A stock has $\beta=1.2$, $R_f=3\%$, $E(R_m)=9\%$; a forecast return of 12% implies — **(A)** overvalued **(B)** undervalued, $\alpha=+1.8\%$ **(C)** fairly valued **(D)** undervalued, $\alpha=+3\%$

**Q5.31** The difference between the CML and the SML is — **(A)** both use $\sigma$ **(B)** the CML uses $\sigma$ (efficient portfolios), the SML uses $\beta$ (any asset) **(C)** the SML uses $\sigma$ **(D)** the CML uses $\beta$

**Q5.32** For an efficient portfolio, beta equals — **(A)** 1 **(B)** $\sigma_P/\sigma_m$ **(C)** $\sigma_m/\sigma_P$ **(D)** 0

**Q5.33** In the market model, $R^2$ equals — **(A)** $\beta$ **(B)** $\rho_{i,m}^2$ **(C)** $\alpha$ **(D)** $\sigma_\varepsilon^2$

**Q5.34** Roll's critique states that — **(A)** beta is stable **(B)** the true market portfolio is unobservable, so the CAPM is not truly testable **(C)** the CAPM has too many factors **(D)** idiosyncratic risk is priced

**Q5.35** Sharpe ratio with $R_P=10\%$, $R_f=2\%$, $\sigma=20\%$ is — **(A)** 0.30 **(B)** 0.40 **(C)** 0.50 **(D)** 0.80

**Q5.36** Treynor ratio with $R_P=10\%$, $R_f=2\%$, $\beta=1.6$ is — **(A)** 0.040 **(B)** 0.050 **(C)** 0.063 **(D)** 0.080

**Q5.37** Jensen's alpha with $R_P=13\%$, $R_f=3\%$, $R_m=9\%$, $\beta=1.2$ is — **(A)** 1.0% **(B)** 1.8% **(C)** 2.8% **(D)** 4.0%

**Q5.38** Information ratio with $R_P=11\%$, $R_B=8\%$, tracking error 5% is — **(A)** 0.30 **(B)** 0.60 **(C)** 0.75 **(D)** 1.50

**Q5.39** Which performance measure uses systematic risk ($\beta$) in the denominator? — **(A)** Sharpe **(B)** Treynor **(C)** Sortino **(D)** $M^2$

**Q5.40** A portfolio's $M^2$ exceeds the market return if and only if — **(A)** its beta $>1$ **(B)** its Sharpe ratio exceeds the market's **(C)** its alpha is negative **(D)** its tracking error is zero

## QCM — Chapitre 6 · Arbitrage Pricing Theory & Multifactor Models

**Q6.1** The CAPM is best described as a — **(A)** two-factor model **(B)** single-factor model (the market) **(C)** no-factor model **(D)** pure arbitrage model

**Q6.2** The Arbitrage Pricing Theory is built primarily on — **(A)** the market portfolio **(B)** mean-variance optimization **(C)** the absence of arbitrage **(D)** normally distributed returns

**Q6.3** Unlike the CAPM, the APT does NOT require — **(A)** a risk-free rate **(B)** the (unobservable) market portfolio **(C)** any factors at all **(D)** positive betas

**Q6.4** The main practical weakness of the APT is that it — **(A)** needs the risk-free rate **(B)** allows only one factor **(C)** does not specify which or how many factors drive returns **(D)** assumes homogeneous expectations

**Q6.5** The Fama-French SMB factor captures the premium for — **(A)** value **(B)** firm size (small minus big) **(C)** momentum **(D)** the market

**Q6.6** The Fama-French HML factor captures the premium for — **(A)** size **(B)** momentum **(C)** value (high minus low book-to-market) **(D)** liquidity

**Q6.7** The Carhart four-factor model adds, to Fama-French, a — **(A)** liquidity factor **(B)** momentum factor (WML/UMD) **(C)** volatility factor **(D)** credit factor

**Q6.8** The CAPM can be viewed as the APT with — **(A)** three factors **(B)** two factors **(C)** no factors **(D)** a single factor (the market)

**Q6.9** A two-factor APT model: $R_f=3\%$, betas 1.0 (premium 4%) and 0.5 (premium 2%). The expected return is — **(A)** 6% **(B)** 7% **(C)** 8% **(D)** 9%

**Q6.10** Statistical factors are — **(A)** macro variables **(B)** firm characteristics **(C)** factors extracted by statistical methods (e.g., PCA) with no a priori economic meaning **(D)** the risk-free rate

**Q6.11** Macroeconomic factors include — **(A)** book-to-market **(B)** momentum **(C)** GDP growth, inflation, and credit spreads **(D)** firm size

**Q6.12** Fundamental factors include — **(A)** GDP growth **(B)** inflation surprises **(C)** size, value, and momentum **(D)** oil-price shocks

**Q6.13** In the APT process $R_i=E(R_i)+\sum\beta F+\varepsilon$, the term $\varepsilon$ is — **(A)** systematic **(B)** the market premium **(C)** a factor surprise **(D)** idiosyncratic (diversifiable) noise

**Q6.14** The value premium (HML) reflects the outperformance of — **(A)** growth over value **(B)** value over growth **(C)** large over small **(D)** losers over winners

**Q6.15** The momentum effect (WML) means — **(A)** losers outperform **(B)** mean reversion dominates **(C)** recent winners tend to keep outperforming **(D)** there is no pattern

**Q6.16** The Fama-French three factors are — **(A)** market, SMB, WML **(B)** market, SMB, HML **(C)** SMB, HML, WML **(D)** market only

**Q6.17** The Fama-French five-factor model adds — **(A)** momentum and liquidity **(B)** profitability and investment **(C)** size and value **(D)** credit and rates

**Q6.18** If two well-diversified portfolios have identical factor exposures but different expected returns, then — **(A)** no arbitrage exists **(B)** an arbitrage opportunity exists **(C)** the market portfolio is needed **(D)** returns must be normal

**Q6.19** The APT allows — **(A)** only one priced factor **(B)** only idiosyncratic risk **(C)** multiple priced systematic factors **(D)** no risk premia

**Q6.20** In the APT, idiosyncratic risk is — **(A)** priced **(B)** diversified away in large portfolios **(C)** the only risk **(D)** equal to beta

**Q6.21** Smart-beta and factor investing are built on — **(A)** the single-factor CAPM **(B)** multifactor models **(C)** random selection **(D)** the risk-free asset

**Q6.22** An APT model with the market as its only factor is equivalent to — **(A)** Fama-French **(B)** the CAPM **(C)** Carhart **(D)** a statistical-factor model

**Q6.23** A three-factor APT: $R_f=2\%$ with exposures/premia $1.0\times5\%$, $0.5\times3\%$, $-0.5\times2\%$. The expected return is — **(A)** 6.5% **(B)** 7.0% **(C)** 7.5% **(D)** 8.5%

**Q6.24** In the APT, $RP_j$ is — **(A)** the risk-free rate **(B)** the expected return premium per unit of exposure to factor $j$ **(C)** the factor beta **(D)** the idiosyncratic term

**Q6.25** A key advantage of the APT over the CAPM is that it — **(A)** specifies the factors precisely **(B)** rests on weaker, more robust assumptions (no-arbitrage) **(C)** requires the market portfolio **(D)** is limited to one factor

**Q6.26** "UMD" (Up Minus Down) is another name for the — **(A)** size factor **(B)** value factor **(C)** momentum factor **(D)** liquidity factor

**Q6.27** The empirical size and value premia are evidence — **(A)** supporting the single-factor CAPM **(B)** against the single-factor CAPM **(C)** of risk-free arbitrage **(D)** of normal returns

**Q6.28** A macroeconomic multifactor model uses factors that are — **(A)** PCA components **(B)** firm characteristics **(C)** observable economic variables **(D)** purely random

**Q6.29** In the Fama-French model, a stock with a high $\beta_{HML}$ behaves like a — **(A)** growth stock **(B)** value stock **(C)** momentum stock **(D)** risk-free asset

**Q6.30** The number of factors in the APT is — **(A)** exactly one **(B)** exactly three **(C)** not specified by the theory **(D)** always five

## QCM — Chapitre 7 · Data Aggregation & Risk Reporting (BCBS 239)

**Q7.1** BCBS 239 was issued primarily in response to — **(A)** banks holding too much capital **(B)** banks' inability to aggregate risk exposures quickly during the crisis **(C)** over-diversification **(D)** over-reliance on internal audit

**Q7.2** BCBS 239 applies first and foremost to — **(A)** all small banks **(B)** insurers **(C)** global systemically important banks (G-SIBs) **(D)** hedge funds

**Q7.3** The four risk-data-aggregation qualities required by BCBS 239 are — **(A)** accuracy, completeness, timeliness, adaptability **(B)** profitability, growth, share, size **(C)** secrecy, exclusivity, brevity, scale **(D)** speed, cost, simplicity, scale

**Q7.4** Risk-data-aggregation systems must remain effective — **(A)** only in normal conditions **(B)** only for regulatory reporting **(C)** only at year-end **(D)** particularly during times of stress or crisis

**Q7.5** Ultimate responsibility for risk data aggregation under BCBS 239 lies with — **(A)** the IT department alone **(B)** the board and senior management **(C)** the external auditor **(D)** the trading desks

**Q7.6** "Adaptability" under BCBS 239 is the ability to — **(A)** cut the number of reports **(B)** outsource data management **(C)** generate ad hoc requests and stress-scenario aggregations on demand **(D)** avoid supervisory review

**Q7.7** A reporting principle (rather than an aggregation principle) is — **(A)** accuracy of data **(B)** completeness of data **(C)** IT infrastructure design **(D)** clarity, usefulness, and appropriate distribution of reports

**Q7.8** The 14 BCBS 239 principles are grouped into — **(A)** 2 themes **(B)** 4 themes **(C)** 11 themes **(D)** no grouping

**Q7.9** The four themes are — **(A)** governance/infrastructure, aggregation capabilities, reporting, supervisory review **(B)** market, credit, liquidity, operational **(C)** capital, leverage, liquidity, funding **(D)** identify, measure, manage, monitor

**Q7.10** "Accuracy and integrity" of data requires — **(A)** secrecy **(B)** more manual processing **(C)** reliable data, ideally automated to limit errors **(D)** fewer controls

**Q7.11** "Completeness" requires capturing — **(A)** only the largest exposure **(B)** all material exposures across lines, entities, and risk types **(C)** only credit risk **(D)** only year-end data

**Q7.12** "Timeliness" requires — **(A)** annual reporting only **(B)** ignoring deadlines **(C)** producing information on time, especially during crises or urgent requests **(D)** delaying reports for accuracy

**Q7.13** Principle 1 (governance) places data aggregation under — **(A)** the trading desk **(B)** the board, senior management, and the governance framework **(C)** the external auditor **(D)** the regulator alone

**Q7.14** Principle 2 (IT architecture) requires systems that — **(A)** work only in calm markets **(B)** are fully manual **(C)** support aggregation, including in periods of stress **(D)** are outsourced entirely

**Q7.15** BCBS 239 is a standard issued by — **(A)** the SEC **(B)** the Basel Committee on Banking Supervision **(C)** the IMF **(D)** the FASB

**Q7.16** The last three of the 14 principles concern — **(A)** the trading book **(B)** capital ratios **(C)** liquidity ratios **(D)** supervisory review, remedial actions, and home/host cooperation

**Q7.17** "Comprehensiveness" of risk reports means — **(A)** covering only market risk **(B)** covering all material risk areas **(C)** maximal brevity **(D)** annual frequency only

**Q7.18** "Distribution" of reports requires that they — **(A)** be made fully public **(B)** be withheld from the board **(C)** reach the right recipients while preserving confidentiality **(D)** be sent only to IT

**Q7.19** The principle "a bank can only manage what it can see" underlies BCBS 239's focus on — **(A)** capital **(B)** data quality and reporting **(C)** leverage **(D)** dividends

**Q7.20** Report frequency under BCBS 239 should — **(A)** decrease in stress **(B)** be annual only **(C)** increase during periods of stress **(D)** be fixed forever

**Q7.21** "BCBS 239" refers to — **(A)** a capital rule **(B)** Basel Committee document no. 239 on risk data aggregation and reporting **(C)** a liquidity ratio **(D)** an accounting standard

**Q7.22** Fragmented, manual data systems are — **(A)** encouraged by BCBS 239 **(B)** irrelevant to it **(C)** precisely what BCBS 239 seeks to remediate **(D)** required by it

**Q7.23** Which is NOT one of the four aggregation qualities? — **(A)** accuracy **(B)** completeness **(C)** clarity of reports **(D)** timeliness

**Q7.24** Supervisory review (theme IV) involves supervisors — **(A)** trading on behalf of banks **(B)** reviewing compliance and requiring remediation **(C)** setting dividends **(D)** auditing clients

**Q7.25** "Home/host" cooperation refers to — **(A)** employee housing **(B)** data centers **(C)** cross-border coordination between supervisors **(D)** outsourcing arrangements

**Q7.26** A core benefit of strong risk-data aggregation is — **(A)** higher leverage **(B)** lower capital **(C)** faster, better risk decisions in normal and stressed times **(D)** fewer audits

**Q7.27** A bank unable to produce group-wide counterparty exposure quickly violates — **(A)** confidentiality **(B)** timeliness and completeness **(C)** the leverage ratio **(D)** the Volcker rule

**Q7.28** Automated data lineage primarily supports — **(A)** marketing **(B)** tax planning **(C)** accuracy and integrity of risk data **(D)** dividend policy

**Q7.29** Compliance with BCBS 239 is assessed by — **(A)** shareholders **(B)** supervisors **(C)** clients **(D)** rating agencies

**Q7.30** The overarching goal of BCBS 239 is to — **(A)** raise capital ratios **(B)** cut costs **(C)** standardize accounting **(D)** strengthen banks' risk data aggregation and reporting

## QCM — Chapitre 8 · Enterprise Risk Management

**Q8.1** ERM is best described as — **(A)** silo-by-silo risk management **(B)** a trading strategy **(C)** integrated, firm-wide risk management aligned with strategy and appetite **(D)** an audit function

**Q8.2** A key advantage of ERM over silos is that it — **(A)** eliminates systematic risk **(B)** captures diversification and concentration effects across the whole firm **(C)** removes the need for capital **(D)** guarantees regulatory approval

**Q8.3** Economic capital is best described as — **(A)** the regulatory minimum **(B)** an internal estimate of capital needed to absorb unexpected losses at a chosen confidence level **(C)** capital for expected losses **(D)** the firm's market capitalization

**Q8.4** Because not all risks materialize at once, an ERM (portfolio) view of capital is generally — **(A)** larger than the sum of silos **(B)** equal to the sum of silos **(C)** smaller than the sum of silos, due to diversification **(D)** independent of correlations

**Q8.5** RAROC is computed as risk-adjusted return divided by — **(A)** total assets **(B)** economic capital **(C)** revenue **(D)** market capitalization

**Q8.6** A business line creates shareholder value when its RAROC — **(A)** is positive **(B)** equals the risk-free rate **(C)** exceeds the firm's cost of equity (hurdle rate) **(D)** is below its expected loss

**Q8.7** Which pair are recognized ERM frameworks? — **(A)** COSO ERM and ISO 31000 **(B)** Basel III and IFRS 9 **(C)** CAPM and APT **(D)** LCR and NSFR

**Q8.8** An emerging risk emphasized in recent ERM discussions is — **(A)** interest rate risk only **(B)** climate risk and cyber risk **(C)** FX risk only **(D)** equity risk only

**Q8.9** A risk appetite framework primarily serves to — **(A)** maximize short-term revenue **(B)** replace internal audit **(C)** translate the board's risk appetite into tolerances and operational limits **(D)** eliminate the CRO

**Q8.10** Economic capital differs from regulatory capital in that it is — **(A)** imposed by the supervisor **(B)** an internal estimate **(C)** always larger **(D)** held only for expected losses

**Q8.11** ERM replaces — **(A)** integrated management with silos **(B)** the board **(C)** economic capital **(D)** siloed risk management with an integrated, firm-wide view

**Q8.12** The "common currency" translating all risks into one unit for comparison is — **(A)** revenue **(B)** market cap **(C)** economic capital **(D)** the risk-free rate

**Q8.13** COSO ERM is oriented mainly toward — **(A)** trading **(B)** governance and enterprise objectives **(C)** liquidity ratios **(D)** tax planning

**Q8.14** Climate risk comprises — **(A)** only market risk **(B)** only credit risk **(C)** physical risk and transition risk **(D)** only operational risk

**Q8.15** AI/ML in finance brings — **(A)** no new risks **(B)** new model, explainability, data, and bias risks **(C)** only cost savings **(D)** guaranteed accuracy

**Q8.16** Two individually small exposures can, once aggregated, be — **(A)** always negligible **(B)** negative **(C)** large (a hidden concentration) **(D)** risk-free

**Q8.17** RAROC enables a firm to — **(A)** ignore risk **(B)** compare activities of different risk profiles and allocate capital to value creators **(C)** raise leverage **(D)** avoid capital

**Q8.18** The hurdle rate in RAROC is — **(A)** the risk-free rate **(B)** the expected loss **(C)** the cost of equity capital **(D)** zero

**Q8.19** ERM aligns risk-taking with — **(A)** the trading desk's targets **(B)** the board's strategy and risk appetite **(C)** the auditor's plan **(D)** the regulator's model

**Q8.20** ISO 31000 is — **(A)** a capital rule **(B)** an accounting standard **(C)** a generic risk-management standard **(D)** a liquidity ratio

**Q8.21** The diversification benefit captured by ERM arises because — **(A)** all risks occur together **(B)** not all risks materialize simultaneously **(C)** correlations are always 1 **(D)** capital is unlimited

**Q8.22** Economic capital is held against — **(A)** operating costs **(B)** taxes **(C)** unexpected losses (UL) **(D)** expected losses

**Q8.23** A unified, firm-wide risk report is — **(A)** discouraged by ERM **(B)** a hallmark of ERM **(C)** a regulatory ban **(D)** impossible

**Q8.24** Operational resilience relates closely to — **(A)** dividend policy **(B)** interest-rate risk **(C)** cyber risk and third-party dependence **(D)** FX risk

**Q8.25** A value-destroying activity has a RAROC — **(A)** above the hurdle rate **(B)** below its cost of capital **(C)** equal to the market return **(D)** of exactly 1

**Q8.26** Regulatory capital is — **(A)** an internal estimate **(B)** optional **(C)** imposed by the supervisor **(D)** the same as market cap

**Q8.27** The ERM "portfolio view" of risk explicitly recognizes — **(A)** only standalone risks **(B)** correlations across risk types **(C)** no correlations **(D)** only credit risk

**Q8.28** Emerging risks also include — **(A)** none **(B)** only equity risk **(C)** only FX risk **(D)** geopolitical, third-party/supply-chain, and digital-asset risks

**Q8.29** The main reason ERM can lower total required capital is — **(A)** higher leverage **(B)** diversification across risk types **(C)** ignoring risk **(D)** lower revenue

**Q8.30** The COSO-spirit definition of ERM emphasizes — **(A)** maximizing revenue **(B)** eliminating all risk **(C)** managing risk within appetite to give reasonable assurance on objectives **(D)** replacing the board

## QCM — Chapitre 9 · Learning from Financial Disasters

**Q9.1** The U.S. Savings & Loan crisis primarily illustrates — **(A)** fraud **(B)** interest rate risk from an asset-liability duration mismatch **(C)** cyber risk **(D)** reputational risk

**Q9.2** LTCM (1998) is best characterized as — **(A)** pure operational risk **(B)** currency risk only **(C)** model risk, excessive leverage, and a correlation breakdown **(D)** reputational risk only

**Q9.3** The collapse of Barings Bank (1995) primarily illustrates — **(A)** uncontrollable market risk **(B)** operational risk from a lack of front/back office segregation **(C)** credit migration risk **(D)** funding liquidity risk

**Q9.4** Metallgesellschaft's hedging losses resulted mainly from — **(A)** a directional bet with no hedge **(B)** a sovereign default **(C)** basis/rollover risk and a liquidity mismatch from margin calls on a stack-and-roll hedge **(D)** fraud

**Q9.5** The Société Générale loss (Kerviel, 2008) is best classified as — **(A)** inherent market risk **(B)** operational risk (unauthorized trading concealed by fictitious hedges) **(C)** credit risk **(D)** liquidity risk

**Q9.6** Bernard Madoff's scheme is an example of — **(A)** a Ponzi fraud enabled by opacity and weak due diligence **(B)** model risk **(C)** funding liquidity risk **(D)** an interest rate mismatch

**Q9.7** Northern Rock (2007) failed primarily because of — **(A)** credit losses on its loan book **(B)** reliance on short-term wholesale funding for long-term assets **(C)** a rogue trader **(D)** a cyber attack

**Q9.8** The Bankers Trust / Procter & Gamble episode illustrates — **(A)** product suitability/complexity and reputational/legal risk for the seller **(B)** sovereign risk **(C)** banking-book interest rate risk **(D)** cyber risk

**Q9.9** The Orange County (1994) bankruptcy resulted from — **(A)** a Ponzi scheme **(B)** a cyber attack **(C)** a highly leveraged, repo-financed bet on interest rates **(D)** a rogue trader

**Q9.10** The 2016 Bangladesh Bank SWIFT incident is primarily — **(A)** market risk **(B)** cyber/operational risk **(C)** credit risk **(D)** sovereign risk

**Q9.11** The Volkswagen "dieselgate" episode primarily illustrates — **(A)** funding liquidity risk **(B)** reputational risk from an ethical/operational failing **(C)** interest rate risk **(D)** model risk

**Q9.12** A common lesson across rogue-trading disasters is the importance of — **(A)** higher leverage **(B)** eliminating all market risk **(C)** reducing capital **(D)** segregation of duties and strong internal controls

**Q9.13** The JPMorgan "London Whale" loss was aggravated by — **(A)** a sovereign default **(B)** a modified VaR model that understated risk and weak controls **(C)** a cyber attack **(D)** a bank run

**Q9.14** Continental Illinois (1984) is most associated with — **(A)** a mortgage duration mismatch **(B)** a Ponzi scheme **(C)** a run driven by over-reliance on volatile wholesale funding **(D)** a rogue trader

**Q9.15** Lehman Brothers (2008) failed due to — **(A)** a cyber attack **(B)** extreme leverage and illiquid assets funded very short-term (repo), with collapsing confidence **(C)** a Ponzi scheme **(D)** a duration-matched book

**Q9.16** The S&L lesson concerns — **(A)** cyber risk **(B)** fraud **(C)** ALM duration mismatch (banking-book rate risk) **(D)** reputational risk

**Q9.17** LTCM's enormous leverage made — **(A)** profits guaranteed **(B)** relatively small market moves catastrophic **(C)** the fund risk-free **(D)** correlations irrelevant

**Q9.18** In a flight to quality, correlations tend to — **(A)** fall to zero **(B)** stay constant **(C)** converge toward 1, defeating diversification **(D)** become strongly negative

**Q9.19** Nick Leeson concealed his losses in — **(A)** a client account **(B)** an error account (the "88888") **(C)** a tax shelter **(D)** a pension fund

**Q9.20** The "stack-and-roll" hedge failed because of — **(A)** too little leverage **(B)** a sovereign default **(C)** rollover/basis risk and cash-flow timing (margin) problems **(D)** fraud

**Q9.21** Kerviel hid positions by exploiting his knowledge of — **(A)** the trading algorithms **(B)** the back office and controls **(C)** the risk model **(D)** the tax code

**Q9.22** Madoff's reported returns were suspicious mainly because they were — **(A)** very volatile **(B)** negative **(C)** implausibly smooth and consistent **(D)** tax-free

**Q9.23** Northern Rock experienced — **(A)** a cyber breach **(B)** the first UK bank run in over a century **(C)** a rogue trader **(D)** a sovereign default

**Q9.24** Procter & Gamble's losses arose from — **(A)** FX hedging **(B)** equity investing **(C)** complex leveraged swaps it did not fully understand **(D)** a cyber attack

**Q9.25** Orange County's treasurer effectively bet that interest rates would — **(A)** be irrelevant **(B)** stay low (but they rose) **(C)** be fully hedged **(D)** default

**Q9.26** The Bangladesh heist was executed using — **(A)** insider trading **(B)** a margin call **(C)** fraudulent SWIFT payment messages **(D)** a short squeeze

**Q9.27** Volkswagen's crisis originated in — **(A)** a funding shortfall **(B)** emissions-test cheating **(C)** a rogue trader **(D)** a model error

**Q9.28** A unifying theme of Barings, SocGen, UBS, and AIB is — **(A)** sovereign default **(B)** a cyber attack **(C)** interest rate risk **(D)** failure of internal controls and segregation of duties

**Q9.29** The "London Whale" loss occurred at — **(A)** Barings **(B)** JPMorgan (the CIO unit) **(C)** Lehman **(D)** AIG

**Q9.30** Model risk refers to — **(A)** market volatility **(B)** credit default **(C)** losses from wrong or miscalibrated models, or assumptions that break in stress **(D)** operational fraud only

**Q9.31** In Metallgesellschaft, the gains on the long supply contracts were — **(A)** immediate cash **(B)** latent and future, while the futures losses were immediate cash **(C)** fraudulent **(D)** perfectly hedged

**Q9.32** Funding liquidity risk is best mitigated by — **(A)** more leverage **(B)** shorter funding **(C)** diversified, stable funding and liquidity buffers **(D)** ignoring it

**Q9.33** A rogue trader's loss appears in the markets, but its root cause is — **(A)** market risk **(B)** credit risk **(C)** liquidity risk **(D)** an operational/governance failure

**Q9.34** The broad lesson of these disasters is that — **(A)** models are infallible **(B)** leverage is harmless **(C)** sound controls, governance, and humility about models matter as much as market views **(D)** diversification always works

## QCM — Chapitre 10 · Anatomy of the Great Financial Crisis (2007-2009)

**Q10.1** Which flawed assumption was central to the mispricing of subprime-backed structured products? — **(A)** that interest rates would never change **(B)** that nationwide house prices would not fall significantly and defaults were largely uncorrelated **(C)** that equities would always rise **(D)** that sovereigns could not default

**Q10.2** The originate-to-distribute model contributed to the crisis by — **(A)** forcing banks to hold more capital **(B)** weakening lenders' incentives to screen borrowers because loans were sold off **(C)** eliminating securitization **(D)** reducing leverage

**Q10.3** Rating agencies contributed to the crisis primarily because — **(A)** they held too much capital **(B)** they refused to rate structured products **(C)** they faced issuer-paid conflicts of interest and used flawed models, over-rating products **(D)** they were government agencies

**Q10.4** The "liquidity spiral" describes — **(A)** a one-time loss with no feedback **(B)** a feedback loop where falling prices and funding withdrawals force fire sales, depressing prices further **(C)** an increase in central bank reserves **(D)** a rise in house prices

**Q10.5** The event widely regarded as the tipping point into global panic was — **(A)** the rescue of Bear Stearns **(B)** the TARP programme **(C)** the bankruptcy of Lehman Brothers (September 2008) **(D)** the downgrade of Greece

**Q10.6** AIG's role in the crisis primarily reflected — **(A)** a duration mismatch **(B)** a large, under-capitalized concentration of credit risk through CDS protection it had sold **(C)** a rogue trader **(D)** a cyber attack

**Q10.7** Which reform introduced the liquidity standards LCR and NSFR? — **(A)** Dodd-Frank only **(B)** Basel III **(C)** Sarbanes-Oxley **(D)** MiFID I

**Q10.8** A money market fund "breaking the buck" in 2008 refers to — **(A)** its NAV falling below \$1 per share, triggering a run on such funds **(B)** failing a stress test **(C)** a sovereign default **(D)** a rogue trading loss

**Q10.9** Shadow banking amplified the crisis through — **(A)** high capital buffers **(B)** long-term deposits **(C)** high leverage funded by short-term repo and commercial paper **(D)** central clearing

**Q10.10** Dodd-Frank introduced — **(A)** only liquidity ratios **(B)** the gold standard **(C)** deposit insurance **(D)** systemic oversight (FSOC), stress tests, the Volcker rule, and central clearing of derivatives

**Q10.11** "Teaser rate" subprime loans assumed borrowers could — **(A)** never default **(B)** refinance as house prices kept rising **(C)** pay cash **(D)** avoid all interest

**Q10.12** The "global savings glut" contributed to — **(A)** high interest rates **(B)** a credit crunch in 2003 **(C)** low interest rates and a reach for yield **(D)** deflation

**Q10.13** The securitization chain was, in order — **(A)** CDO $\to$ MBS $\to$ mortgages **(B)** mortgages $\to$ MBS $\to$ CDO ($\to$ CDO$^{2}$) **(C)** equities $\to$ bonds $\to$ loans **(D)** deposits $\to$ reserves $\to$ capital

**Q10.14** Senior tranches backed by subprime mortgages were frequently rated — **(A)** junk **(B)** unrated **(C)** AAA (wrongly) **(D)** BBB

**Q10.15** Bear Stearns (March 2008) was — **(A)** allowed to fail **(B)** rescued/acquired by JPMorgan with Fed support **(C)** nationalized **(D)** unaffected

**Q10.16** Fannie Mae and Freddie Mac were, in September 2008 — **(A)** liquidated **(B)** merged with Lehman **(C)** placed into government conservatorship **(D)** sold to AIG

**Q10.17** The crisis revealed failures of — **(A)** only capital **(B)** only liquidity **(C)** only governance **(D)** capital, liquidity, leverage, governance, incentives, and transparency

**Q10.18** Basel III's leverage ratio is — **(A)** risk-weighted **(B)** non-risk-weighted **(C)** a liquidity ratio **(D)** an accounting rule

**Q10.19** The Volcker rule restricts — **(A)** deposit-taking **(B)** mortgage lending **(C)** proprietary trading by banks **(D)** dividends

**Q10.20** Central clearing of derivatives through CCPs addresses — **(A)** interest rate risk **(B)** counterparty risk and opacity **(C)** equity risk **(D)** inflation

**Q10.21** A macroprudential approach focuses on — **(A)** a single bank only **(B)** marketing **(C)** the system as a whole and procyclicality **(D)** taxation

**Q10.22** Fire sales occur when — **(A)** prices rise **(B)** firms sell assets to deleverage, depressing prices further **(C)** central banks buy bonds **(D)** funding is abundant

**Q10.23** Contagion in 2008 spread mainly through — **(A)** consumer spending **(B)** advertising **(C)** interconnections (each party a counterparty of others) **(D)** exchange rates only

**Q10.24** The TARP programme (October 2008) provided — **(A)** a new currency **(B)** capital injections and guarantees to stabilize banks **(C)** a tax cut **(D)** deposit rationing

**Q10.25** "Skin in the game" rules require originators to — **(A)** sell all risk **(B)** increase leverage **(C)** retain a portion of securitized risk **(D)** avoid lending

**Q10.26** Repo and commercial paper are — **(A)** long-term equity **(B)** short-term wholesale funding sources **(C)** insurance contracts **(D)** central bank reserves

**Q10.27** The crisis became a systemic event because — **(A)** it affected only one bank **(B)** equities rose **(C)** failures threatened the whole financial system **(D)** house prices rose

**Q10.28** CVA capital under Basel III addresses — **(A)** market risk only **(B)** counterparty credit risk on derivatives **(C)** liquidity risk **(D)** operational risk

**Q10.29** After one fund "broke the buck," money market funds suffered — **(A)** higher returns **(B)** no effect **(C)** a run **(D)** a tax rebate

**Q10.30** The correct crisis sequence is — **(A)** contagion $\to$ losses $\to$ fire sales $\to$ funding freeze **(B)** fire sales $\to$ contagion $\to$ losses $\to$ funding freeze **(C)** funding freeze $\to$ contagion $\to$ fire sales $\to$ losses **(D)** losses $\to$ funding freeze $\to$ fire sales $\to$ contagion $\to$ systemic risk

**Q10.31** A central cause of the crisis was — **(A)** too little lending **(B)** excessive leverage with too little capital **(C)** excessive diversification **(D)** high interest rates

**Q10.32** The single event to remember as the tipping point is — **(A)** the Bear Stearns rescue **(B)** TARP **(C)** Lehman's bankruptcy **(D)** the Greek downgrade

## QCM — Chapitre 11 · GARP Code of Conduct

**Q11.1** A risk analyst's honest, unfavorable assessment will displease a major client. Under the GARP Code, the analyst should — **(A)** soften it to keep the client happy **(B)** provide an honest, well-supported assessment with independent judgment **(C)** withhold it entirely **(D)** delegate the decision to the client

**Q11.2** A member with a personal financial interest that could affect a recommendation should — **(A)** ignore it if small **(B)** disclose it only if asked **(C)** fully disclose the conflict of interest to clients and employers **(D)** resign immediately

**Q11.3** With material non-public (confidential) information, a member must — **(A)** use it to trade for personal gain **(B)** protect it and neither disclose nor misuse it, except where legally required **(C)** share it with colleagues **(D)** publish it for transparency

**Q11.4** Overstating one's qualifications or a model's expected accuracy violates the requirement to — **(A)** disclose conflicts of interest **(B)** maintain confidentiality **(C)** not misrepresent competence, analysis, or results **(D)** cooperate with supervisors

**Q11.5** Regarding laws and regulations, the Code requires members to — **(A)** comply only when convenient **(B)** comply with applicable laws and regulations **(C)** follow only home-country law **(D)** ignore conflicting regulations

**Q11.6** When the interests of the member, employer, and public conflict, the member should — **(A)** prioritize personal interest **(B)** defer to the highest payer **(C)** prioritize the most profitable outcome **(D)** act with integrity, placing the legitimate interests of clients, employers, and the public above personal gain

**Q11.7** A violation of the GARP Code can result in — **(A)** no consequences **(B)** disciplinary action up to and including removal of the FRM designation **(C)** a mandatory fine only **(D)** automatic criminal prosecution

**Q11.8** The GARP Code of Conduct applies to — **(A)** only certified FRMs employed by banks **(B)** only US-based members **(C)** all GARP members and candidates, worldwide **(D)** only those who have passed Part II

**Q11.9** Maintaining and improving one's professional competence is part of the Code's — **(A)** confidentiality principle **(B)** fundamental responsibilities (professional competence) **(C)** conflict-of-interest rules **(D)** distribution rules

**Q11.10** In a risk report, the distinction between facts and opinions — **(A)** is unnecessary **(B)** applies only to internal reports **(C)** must be made clearly, with conclusions based on diligent analysis **(D)** is left to the reader

**Q11.11** A member should exercise — **(A)** the client's judgment **(B)** the employer's judgment only **(C)** independent professional judgment **(D)** no judgment at all

**Q11.12** Helping a client carry out a fraudulent transaction is — **(A)** acceptable if profitable **(B)** prohibited under professional integrity **(C)** acceptable if disclosed **(D)** required by the employer

**Q11.13** Using insider (material non-public) information to trade is — **(A)** encouraged **(B)** acceptable if small **(C)** prohibited (confidentiality, integrity, and law) **(D)** mandatory

**Q11.14** When unsure of the ethical course of action, a member should generally choose — **(A)** the cheapest option **(B)** the most profitable option **(C)** the employer's preference **(D)** the most transparent and prudent option

**Q11.15** A conflict of interest that cannot be avoided must be — **(A)** concealed **(B)** fully disclosed **(C)** ignored **(D)** resolved only by resigning

**Q11.16** The purpose of the GARP Code is to — **(A)** increase fees **(B)** limit competition **(C)** protect the integrity of the profession and public trust **(D)** replace regulation

**Q11.17** Misrepresenting a model's expected accuracy is — **(A)** acceptable marketing **(B)** a form of misrepresentation (prohibited) **(C)** a required disclosure **(D)** a confidentiality breach

**Q11.18** A member must keep client information — **(A)** public **(B)** shared internally without limit **(C)** confidential (subject to legal exceptions) **(D)** posted on social media

**Q11.19** Professional competence requires — **(A)** ignoring new developments **(B)** acting with reasonable care and maintaining knowledge **(C)** delegating all analysis **(D)** avoiding clients

**Q11.20** Disciplinary procedures under the Code may include — **(A)** bonuses **(B)** promotions **(C)** tax audits **(D)** investigation and sanctions (warning, suspension, removal)

**Q11.21** Best practices require a member to — **(A)** guess **(B)** use recognized, rigorous methods and diligent analysis **(C)** copy competitors **(D)** avoid documentation

**Q11.22** Placing personal gain above clients' legitimate interests is — **(A)** encouraged **(B)** neutral **(C)** a Code violation **(D)** required

**Q11.23** Compliance with the Code is — **(A)** optional after certification **(B)** a continuing condition of certification **(C)** required only during the exam **(D)** waived for senior members

**Q11.24** A member who discovers misconduct should — **(A)** conceal it **(B)** profit from it **(C)** act with integrity and not conceal it **(D)** ignore it

**Q11.25** The confidentiality principle has an exception for — **(A)** profitable trades **(B)** legally required disclosure **(C)** social media posts **(D)** marketing material

**Q11.26** Fabricating credentials on a résumé is — **(A)** acceptable if minor **(B)** a confidentiality breach **(C)** misrepresentation (prohibited) **(D)** required

**Q11.27** The Code requires members to clearly distinguish — **(A)** clients from employers **(B)** facts from opinions **(C)** profits from losses **(D)** assets from liabilities

**Q11.28** An apparent (not just actual) conflict of interest should be — **(A)** ignored **(B)** hidden **(C)** exploited **(D)** disclosed

**Q11.29** The Code binds — **(A)** only certified FRMs **(B)** candidates as well as certified FRMs **(C)** only employers **(D)** only regulators

**Q11.30** Exam questions on the Code are typically — **(A)** pure calculations **(B)** historical dates **(C)** situational (identify the correct conduct) **(D)** essay questions

---

# Corrigé commenté & renvois au cours

> Réponse en **gras**, suivie du **renvoi à la section** (§) de la synthèse et d'une explication concise en français. Calcul détaillé pour les questions chiffrées.

## Chapitre 1 · Building Blocks

**Q1.1 — B** *(§1.1)*. Le *risque* a des probabilités **quantifiables** ; l'incertitude knightienne, non. Le risque n'implique pas forcément une perte.
**Q1.2 — B** *(§1.3)*. La perte **moyenne anticipée** = **EL**, tarifée et provisionnée (un coût).
**Q1.3 — B** *(§1.3)*. Le **capital économique** absorbe les **pertes inattendues (UL)**.
**Q1.4 — C** *(§1.4)*. *Rogue trading* = **risque opérationnel** (défaut de contrôle), même si la perte se voit sur les marchés.
**Q1.5 — B** *(§1.4)*. Incapacité à **se refinancer** pour des actifs sains = ***funding liquidity risk***.
**Q1.6 — B** *(§1.6)*. **capacity $\ge$ appetite $\ge$ tolerance $\ge$ limits**.
**Q1.7 — C** *(§1.6)*. Maximum supportable avant insolvabilité = **risk capacity**.
**Q1.8 — D** *(§1.5)*. Les 4 réponses : accept, treat, transfer, terminate. « Titriser à tout prix » n'en est pas une.
**Q1.9 — B** *(§1.3)*. $EL=0.02\times0.40\times\$10\text{m}=\$80{,}000$.
**Q1.10 — B** *(§1.4)*. Un *downgrade* = risque de crédit (**migration**).
**Q1.11 — B** *(§1.7)*. La gestion des risques peut **détruire** de la valeur (risque de modèle, faux sentiment de sécurité).
**Q1.12 — C** *(§1.3)*. $LGD=1-R=1-0.30=70\%$.
**Q1.13 — B** *(§1.1)*. Le risque = **dispersion** des résultats futurs autour de l'anticipation (pas seulement la perte).
**Q1.14 — B** *(§1.1)*. *Unknown unknowns* = risques **inconnaissables**, ruptures sans précédent.
**Q1.15 — B** *(§1.2)*. Prime de risque = **surcroît de rendement** exigé pour porter le risque.
**Q1.16 — B** *(§1.3)*. $EL=PD\times LGD\times EAD$ (produit).
**Q1.17 — A** *(§1.3)*. **EAD** = *Exposure at Default* (exposition au défaut).
**Q1.18 — B** *(§1.4)*. *Market liquidity* = impossibilité de **vendre** sans déprimer le prix.
**Q1.19 — B** *(§1.4)*. Concession de prix à la vente = ***market liquidity risk***.
**Q1.20 — B** *(§1.4)*. Risque **systémique** = propagation d'une défaillance à **tout le système**.
**Q1.21 — B** *(§1.4)*. La réputation est le plus souvent une **conséquence** d'autres risques.
**Q1.22 — A** *(§1.5)*. **Identifier, mesurer, gérer, suivre**.
**Q1.23 — B** *(§1.5)*. *Treat/mitigate* = **réduire** le risque (contrôles, couverture).
**Q1.24 — B** *(§1.6)*. *Appetite* = niveau de risque **choisi** par le conseil.
**Q1.25 — B** *(§1.6)*. *Limits* = **bornes opérationnelles** concrètes.
**Q1.26 — C** *(§1.4)*. Le risque opérationnel **exclut** le stratégique et le réputationnel.
**Q1.27 — B** *(§1.3)*. $EL=0.01\times0.50\times\$20\text{m}=\$100{,}000$.
**Q1.28 — A** *(§1.3)*. $LGD=1-0.60=40\%$.
**Q1.29 — B** *(§1.3)*. La mesure de base de la dispersion = **écart-type**.
**Q1.30 — B** *(§1.7)*. **Faux sentiment de sécurité / risque de modèle** détruisent de la valeur.
**Q1.31 — B** *(§1.4)*. Défaut souverain = **risque de crédit (souverain)**.
**Q1.32 — B** *(§1.3)*. UL = **variabilité** des pertes autour de l'EL.

## Chapitre 2 · Managing Risk

**Q2.1 — C** *(§2.1)*. En **marché parfait (MM)**, la couverture est **sans effet** (l'actionnaire la réplique).
**Q2.2 — A** *(§2.1)*. MM suppose **ni impôts, ni coûts de transaction/détresse, information symétrique**.
**Q2.3 — D** *(§2.2)*. Argument le plus fort : **réduction des coûts de détresse financière**.
**Q2.4 — B** *(§2.2)*. Barème **convexe** : lisser le résultat **réduit l'impôt espéré**.
**Q2.5 — C** *(§2.2)*. Sous-investissement : un **manque de trésorerie** force à renoncer à des projets **VAN > 0**.
**Q2.6 — A** *(§2.2)*. Dirigeants/salariés : **capital humain concentré et non diversifié** dans la firme.
**Q2.7 — C** *(§2.2)*. Moins de volatilité $\to$ **moins de détresse** $\to$ coût du capital plus faible.
**Q2.8 — B** *(§2.2)*. Coûts de la couverture : **transaction, base, perte de l'*upside***.
**Q2.9 — C** *(§2.3)*. *Forward* : **fige** un prix, **sans prime**, **engage les deux** parties.
**Q2.10 — D** *(§2.3)*. L'**option** = payoff **asymétrique** (garde l'*upside*) contre **prime**.
**Q2.11 — B** *(§2.3)*. *Future* : **standardisé, appels de marge quotidiens** (chambre de compensation).
**Q2.12 — C** *(§2.3)*. Swap de taux = **échange fixe contre variable**.
**Q2.13 — B** *(§2.3)*. L'assurance = **transfert** de risque.
**Q2.14 — C** *(§2.3)*. Diversification : réduit l'**idiosyncratique** à coût quasi nul.
**Q2.15 — B** *(§2.3)*. **Risque de base** : instrument et exposition **imparfaitement corrélés**.
**Q2.16 — C** *(§2.3)*. *Natural hedge* = **compensation opérationnelle** (recettes et coûts en même devise).
**Q2.17 — A** *(§2.2)*. **Conserver** le cœur, **transférer** le périphérique.
**Q2.18 — B** *(§2.4)*. Le mapping **décompose en facteurs** pour agréger/couvrir.
**Q2.19 — C** *(§2.3)*. La couverture **réduit** une exposition ; la spéculation **prend** du risque pour un gain.
**Q2.20 — B** *(§2.3)*. Une couverture imparfaite peut **créer des pertes de base** voire une **position spéculative**.
**Q2.21 — C** *(§2.4)*. *Originate-to-distribute* **affaiblit la sélection/suivi** des emprunteurs.
**Q2.22 — B** *(§2.2)*. Bénéfice : **résultats plus stables** (planification, investissement).
**Q2.23 — C** *(§2.4)*. Le transfert peut **opacifier et concentrer** le risque (**AIG**).
**Q2.24 — B** *(§2.3)*. Garder l'exposition sans la compenser = **rétention** de risque.
**Q2.25 — B** *(§2.3)*. Les particuliers s'assurent car **averses au risque et non diversifiés**.
**Q2.26 — C** *(§2.2)*. Couvrir si **bénéfice attendu > coût** de la couverture.
**Q2.27 — C** *(§2.3)*. Le seul « *free lunch* » = la **diversification**.
**Q2.28 — B** *(§2.3)*. **Antisélection** : les profils les **plus risqués** s'assurent davantage.
**Q2.29 — C** *(§2.3)*. **Aléa moral** : l'assuré prend **plus de risque** une fois couvert.
**Q2.30 — B** *(§2.3)*. Vendre des futures = **figer le prix** (couverture **symétrique**, perte de l'*upside*).
**Q2.31 — C** *(§2.2)*. Les appels de marge créent un **risque de liquidité** (timing de trésorerie).
**Q2.32 — C** *(§2.4)*. Le risque agrégé du système n'est **pas supprimé**, seulement **redistribué** (voire concentré).

## Chapitre 3 · Governance

**Q3.1 — C** *(§3.2)*. Le conseil **fixe/approuve l'appétit** et supervise le cadre.
**Q3.2 — B** *(§3.2)*. Le **CRO** rapporte au **conseil / comité des risques** (indépendance).
**Q3.3 — A** *(§3.3)*. **1\textsuperscript{re} ligne** = les **métiers** qui **possèdent** le risque.
**Q3.4 — C** *(§3.3)*. **2\textsuperscript{e} ligne** = **risque & conformité**.
**Q3.5 — D** *(§3.3)*. **3\textsuperscript{e} ligne** = **audit interne** (assurance indépendante).
**Q3.6 — B** *(§3.3)*. L'audit interne **évalue** les contrôles ; il **ne gère pas** le risque.
**Q3.7 — C** *(§3.2)*. Le **comité d'audit** supervise reporting financier et contrôle interne.
**Q3.8 — D** *(§3.4)*. Rémunération : **différée, ajustée du risque, pluriannuelle, avec clawback**.
**Q3.9 — B** *(§3.4)*. Le *clawback* **récupère** des rémunérations versées après pertes/faute.
**Q3.10 — C** *(§3.5)*. *Risk shifting / asset substitution* : les **actionnaires** (près de la détresse) prennent **trop de risque** aux dépens des créanciers.
**Q3.11 — B** *(§3.4)*. Culture forte : **ton du sommet, responsabilité, escalade, challenge**.
**Q3.12 — A** *(§3.1)*. Gouvernance = **système de règles** dirigeant et contrôlant la firme.
**Q3.13 — C** *(§3.2)*. Le **comité des risques** supervise cadre et appétit.
**Q3.14 — B** *(§3.2)*. L'indépendance du CRO **empêche les métiers de court-circuiter** les contrôles.
**Q3.15 — C** *(§3.3)*. La **conformité** est une fonction de **2\textsuperscript{e} ligne**.
**Q3.16 — A** *(§3.3)*. La **prise de risque quotidienne et les contrôles intégrés** = **1\textsuperscript{re} ligne**.
**Q3.17 — B** *(§3.5)*. Conflit d'agence classique : **actionnaires vs créanciers/déposants**.
**Q3.18 — C** *(§3.4)*. Les bonus court terme excessifs **encouragent la prise de risque** et le court-termisme.
**Q3.19 — B** *(§3.4)*. *Tone at the top* = la **direction** fixe les standards éthiques/risque.
**Q3.20 — C** *(§3.2)*. Séparer **CEO et président du conseil** renforce l'**indépendance**.
**Q3.21 — D** *(§3.5)*. Parties prenantes : **actionnaires, créanciers, régulateurs, salariés, clients**.
**Q3.22 — B** *(§3.3)*. L'audit interne est **indépendant des activités auditées** (rapporte au comité d'audit).
**Q3.23 — C** *(§3.2)*. L'appétit est **approuvé par le conseil** puis **décliné en limites**.
**Q3.24 — B** *(§3.1)*. Un conseil qui **valide sans challenger** = **gouvernance faible**.
**Q3.25 — C** *(§3.4)*. Bonus différés/ajustés alignent sur la **performance pluriannuelle ajustée du risque**.
**Q3.26 — B** *(§3.3)*. Les trois lignes **clarifient les rôles** (éviter trous/chevauchements).
**Q3.27 — C** *(§3.5)*. Management vs actionnaires = **problème principal-agent**.
**Q3.28 — D** *(§3.2)*. Responsabilité ultime : **conseil et direction générale**.
**Q3.29 — C** *(§3.2)*. Comité des **risques** = profil **a priori** ; comité d'**audit** = contrôle **a posteriori**.
**Q3.30 — B** *(§3.4)*. Le **malus** réduit la **rémunération différée non acquise** avant versement.
**Q3.31 — C** *(§3.2)*. Le conseil doit être **indépendant et compétent** pour **challenger** la direction.
**Q3.32 — C** *(§3.1)*. But ultime : **aligner la prise de risque** sur l'**appétit** et l'**intérêt des parties prenantes**.

## Chapitre 4 · Credit Risk Transfer

**Q4.1 — B** *(§4.1)*. Transférer le crédit pour **libérer du capital** et **gérer la concentration**.
**Q4.2 — A** *(§4.2)*. L'**acheteur de protection** paie la prime et est **indemnisé** lors de l'événement de crédit.
**Q4.3 — C** *(§4.2)*. Le **vendeur** reçoit la prime et **paie au défaut** (≈ long crédit, comme un assureur).
**Q4.4 — B** *(§4.2)*. *Credit triangle* : $s\approx\lambda(1-R)$.
**Q4.5 — C** *(§4.2)*. $\lambda\approx s/(1-R)=0.024/0.60=4.0\%$.
**Q4.6 — B** *(§4.2)*. CDS ***naked*** = protection **sans détenir** la dette (spéculatif).
**Q4.7 — C** *(§4.2)*. $s\approx\lambda(1-R)=0.02\times0.70=1.4\%=140$ bps.
**Q4.8 — D** *(§4.3)*. La titrisation émet des **tranches** de séniorité via un **SPV**.
**Q4.9 — A** *(§4.3)*. La tranche **equity** absorbe les **premières** pertes (risque/rendement max).
**Q4.10 — C** *(§4.3)*. La tranche **senior** est **payée en premier** / perd en dernier (risque min).
**Q4.11 — B** *(§4.3)*. Fraction détruite $=[\min(L,D)-A]^{+}/(D-A)$.
**Q4.12 — C** *(§4.3)*. $[\min(5,7)-3]^{+}/(7-3)=2/4=50\%$.
**Q4.13 — D** *(§4.3)*. $[\min(10,7)-3]/4=4/4=100\%$.
**Q4.14 — A** *(§4.3)*. $[\min(2,7)-3]^{+}=\max(-1,0)=0\Rightarrow 0\%$.
**Q4.15 — B** *(§4.4)*. **$\rho\uparrow$** **pénalise le senior**, **favorise l'equity**.
**Q4.16 — C** *(§4.4)*. L'**EL du portefeuille** est **invariante** à $\rho$ (seule la forme change).
**Q4.17 — B** *(§4.4)*. Plus de corrélation $\Rightarrow$ distribution **bimodale / à queue épaisse**.
**Q4.18 — C** *(§4.5)*. La copule gaussienne **sous-estime la dépendance de queue**.
**Q4.19 — B** *(§4.5)*. **AIG** : **CDS vendus** + **déclencheurs de collatéral** $\to$ crise de liquidité (*wrong-way*).
**Q4.20 — A** *(§4.5)*. *Originate-to-distribute* **affaiblit la sélection** des emprunteurs.
**Q4.21 — C** *(§4.3)*. Un CDO **synthétique** s'appuie sur des **CDS**, pas des obligations *cash*.
**Q4.22 — B** *(§4.3)*. *First-to-default* paie au **premier** nom du panier qui fait défaut.
**Q4.23 — C** *(§4.3)*. Une **CLN** intègre un **CDS** : l'investisseur **porte le risque de crédit** (surcroît de rendement).
**Q4.24 — A** *(§4.3)*. Le *tranching* crée des titres de **qualités de crédit différentes** d'un même pool.
**Q4.25 — B** *(§4.2)*. La **base CDS-obligation** = écart entre **spread CDS** et **spread obligataire**.
**Q4.26 — C** *(§4.2)*. Si $R=0$, $s\approx\lambda$ (puisque $1-R=1$).
**Q4.27 — B** *(§4.5)*. Les **monolines** garantissaient du crédit structuré puis ont été **dégradés**.
**Q4.28 — C** *(§4.2)*. Acheter la protection **et** détenir l'obligation = position de crédit **couverte**.
**Q4.29 — B** *(§4.2)*. *Wrong-way risk* : l'exposition **monte** quand la contrepartie se **dégrade** (corrélation).
**Q4.30 — C** *(§4.3)*. La **mezzanine** se situe **entre equity et senior** dans la priorité des pertes.
**Q4.31 — B** *(§4.2)*. $\lambda$ = **probabilité instantanée conditionnelle** de défaut par unité de temps.
**Q4.32 — D** *(§4.1)*. La titrisation **transfère le risque** et **libère du capital réglementaire**.

## Chapitre 5 · MPT & CAPM

**Q5.1 — D** *(§5.1)*. $0.6(10\%)+0.4(5\%)=8.0\%$.
**Q5.2 — B** *(§5.1)*. $\sigma_P^2=0.16(0.09)+0.36(0.04)+2(0.4)(0.6)(0.5)(0.30)(0.20)=0.0432$, $\sigma_P=\sqrt{0.0432}\approx20.8\%$.
**Q5.3 — A** *(§5.1)*. $\operatorname{Cov}=0.4(0.25)(0.15)=0.0150$.
**Q5.4 — B** *(§5.1)*. $\rho=0.018/(0.30\cdot0.20)=0.30$.
**Q5.5 — B** *(§5.1)*. $\sigma_P^2=\mathbf{w}^\top\Sigma\mathbf{w}$ ; $\mathbf{w}^\top\boldsymbol\mu$ est le rendement.
**Q5.6 — A** *(§5.1)*. $\rho=-1$ avec poids/volatilités égaux $\Rightarrow$ risque **nul** ($\sigma_P=0$).
**Q5.7 — C** *(§5.1)*. $\sigma_P^2=0.25(0.04)+0.25(0.04)=0.02$, $\sigma_P=\sqrt{0.02}=14.1\%$.
**Q5.8 — B** *(§5.1)*. $0.5(8)+0.3(12)+0.2(20)=11.6\%$.
**Q5.9 — B** *(§5.2)*. La diversification supprime le risque **idiosyncratique** seulement.
**Q5.10 — C** *(§5.2)*. À la limite, la variance tend vers la **covariance moyenne** (risque systématique).
**Q5.11 — C** *(§5.2)*. Seul le risque **systématique** est rémunéré.
**Q5.12 — B** *(§5.2)*. $\beta^2\sigma_m^2$ = part **systématique** ; $\sigma_\varepsilon^2$ = idiosyncratique.
**Q5.13 — B** *(§5.3)*. Frontière efficiente = rendement **max** par niveau de risque, **au-dessus du MVP**.
**Q5.14 — A** *(§5.3)*. $w_1^{\text{MVP}}=(\sigma_2^2-\sigma_{12})/(\sigma_1^2+\sigma_2^2-2\sigma_{12})$.
**Q5.15 — C** *(§5.3)*. $\sigma_{12}=0$ : $w_1=0.04/(0.01+0.04)=0.80$.
**Q5.16 — C** *(§5.3)*. $w_1=\sigma_2/(\sigma_1+\sigma_2)=15/25=0.60$.
**Q5.17 — B** *(§5.3)*. Pente de la CML = **ratio de Sharpe du marché** (prix de marché du risque).
**Q5.18 — C** *(§5.3)*. $E=2\%+\frac{10\%-2\%}{20\%}(15\%)=2\%+6\%=8\%$.
**Q5.19 — C** *(§5.3)*. $w_m=\sigma/\sigma_m=15/20=75\%$.
**Q5.20 — B** *(§5.3)*. Séparation en deux fonds : **marché + actif sans risque** en proportions variables.
**Q5.21 — C** *(§5.4)*. $\beta=0.024/0.016=1.50$.
**Q5.22 — C** *(§5.4)*. $E=2\%+1.5(8\%-2\%)=2\%+9\%=11\%$.
**Q5.23 — C** *(§5.4)*. $\beta=\rho\,\sigma_i/\sigma_m=0.6(25/15)=1.00$.
**Q5.24 — B** *(§5.4)*. $\beta=0\Rightarrow$ rendement exigé $=R_f$.
**Q5.25 — B** *(§5.4)*. Titre **défensif** : $\beta<1$.
**Q5.26 — B** *(§5.4)*. $0.5(1.2)+0.3(0.8)+0.2(1.5)=1.14$.
**Q5.27 — B** *(§5.4)*. Au-dessus de la SML $\Rightarrow$ **sous-évalué** ($\alpha>0$).
**Q5.28 — C** *(§5.4)*. $11\%=2\%+1.5\,\mathrm{MRP}\Rightarrow \mathrm{MRP}=6\%$.
**Q5.29 — B** *(§5.4)*. La SML porte le rendement contre **$\beta$**.
**Q5.30 — B** *(§5.4)*. Exigé $=3\%+1.2(6\%)=10.2\%$ ; prévu $12\%>10.2\%\Rightarrow$ **sous-évalué**, $\alpha=+1.8\%$.
**Q5.31 — B** *(§5.4)*. **CML** = $\sigma$ (efficients) ; **SML** = $\beta$ (tout actif).
**Q5.32 — B** *(§5.4)*. Pour un portefeuille efficient ($\rho_{P,m}=1$), $\beta=\sigma_P/\sigma_m$.
**Q5.33 — B** *(§5.4)*. Dans le modèle de marché, $R^2=\rho_{i,m}^2$.
**Q5.34 — B** *(§5.5)*. **Roll** : portefeuille de marché **inobservable** $\Rightarrow$ CAPM non testable.
**Q5.35 — B** *(§5.6)*. $S=(10\%-2\%)/20\%=0.40$.
**Q5.36 — B** *(§5.6)*. $T=(10\%-2\%)/1.6=5.0\%=0.050$.
**Q5.37 — C** *(§5.6)*. $\alpha=13\%-[3\%+1.2(6\%)]=2.8\%$.
**Q5.38 — B** *(§5.6)*. $\mathrm{IR}=(11\%-8\%)/5\%=0.60$.
**Q5.39 — B** *(§5.6)*. Le **Treynor** utilise le risque **systématique** $\beta$.
**Q5.40 — B** *(§5.6)*. $M^2>R_m$ ssi le **Sharpe** du portefeuille dépasse celui du marché.

## Chapitre 6 · APT & Multifactor Models

**Q6.1 — B** *(§6.1)*. Le CAPM est un modèle **à facteur unique** (le marché).
**Q6.2 — C** *(§6.1)*. L'APT repose sur l'**absence d'arbitrage**.
**Q6.3 — B** *(§6.1)*. L'APT **n'exige pas** le **portefeuille de marché** (inobservable).
**Q6.4 — C** *(§6.1)*. Faiblesse : l'APT **ne spécifie pas** les facteurs (ni leur nombre).
**Q6.5 — B** *(§6.2)*. **SMB** = prime de **taille**.
**Q6.6 — C** *(§6.2)*. **HML** = prime de **valeur** (fort *book-to-market*).
**Q6.7 — B** *(§6.2)*. Carhart ajoute le **momentum** (WML/UMD).
**Q6.8 — D** *(§6.1)*. Le CAPM = APT à **un seul facteur** (le marché).
**Q6.9 — C** *(§6.1)*. $E(R)=3\%+1.0(4\%)+0.5(2\%)=8\%$.
**Q6.10 — C** *(§6.2)*. Facteurs **statistiques** = extraits par **ACP/PCA**, sans sens économique a priori.
**Q6.11 — C** *(§6.2)*. Facteurs **macro** : **PIB, inflation, spreads**.
**Q6.12 — C** *(§6.2)*. Facteurs **fondamentaux** : **taille, valeur, momentum**.
**Q6.13 — D** *(§6.1)*. $\varepsilon$ = **bruit idiosyncratique** (diversifiable).
**Q6.14 — B** *(§6.2)*. La prime de valeur = **value sur growth**.
**Q6.15 — C** *(§6.2)*. **Momentum** : les gagnants récents **continuent** de sur-performer.
**Q6.16 — B** *(§6.2)*. Fama-French 3F : **marché, SMB, HML**.
**Q6.17 — B** *(§6.2)*. FF 5F ajoute **rentabilité et investissement**.
**Q6.18 — B** *(§6.1)*. Mêmes expositions mais rendements différents $\Rightarrow$ **arbitrage** (impossible en équilibre).
**Q6.19 — C** *(§6.1)*. L'APT admet **plusieurs facteurs systématiques** rémunérés.
**Q6.20 — B** *(§6.1)*. Le risque **idiosyncratique** est **diversifié** dans un grand portefeuille.
**Q6.21 — B** *(§6.2)*. *Smart beta* / factoriel reposent sur les **modèles multifactoriels**.
**Q6.22 — B** *(§6.1)*. APT à un facteur (le marché) = **CAPM**.
**Q6.23 — C** *(§6.1)*. $E(R)=2\%+1.0(5\%)+0.5(3\%)-0.5(2\%)=2+5+1.5-1=7.5\%$.
**Q6.24 — B** *(§6.1)*. $RP_j$ = **prime de rendement** par unité d'exposition au facteur $j$.
**Q6.25 — B** *(§6.1)*. Avantage : **hypothèses plus faibles/robustes** (absence d'arbitrage).
**Q6.26 — C** *(§6.2)*. **UMD** = facteur **momentum**.
**Q6.27 — B** *(§6.2)*. Primes taille/valeur = preuves **contre** le CAPM à un facteur.
**Q6.28 — C** *(§6.2)*. Modèle **macro** = **variables économiques observables**.
**Q6.29 — B** *(§6.2)*. Fort $\beta_{HML}$ $\Rightarrow$ comportement de **value**.
**Q6.30 — C** *(§6.1)*. Le nombre de facteurs de l'APT **n'est pas spécifié** par la théorie.

## Chapitre 7 · BCBS 239

**Q7.1 — B** *(§7.1)*. BCBS 239 répond à l'**incapacité d'agréger** rapidement les expositions en crise.
**Q7.2 — C** *(§7.1)*. Il vise d'abord les **G-SIB**.
**Q7.3 — A** *(§7.2)*. Qualités : **accuracy, completeness, timeliness, adaptability**.
**Q7.4 — D** *(§7.2)*. Les systèmes doivent tenir **surtout en période de stress**.
**Q7.5 — B** *(§7.2)*. Responsabilité ultime : **conseil et direction générale**.
**Q7.6 — C** *(§7.2)*. **Adaptabilité** = requêtes ***ad hoc*** et agrégations de stress **à la demande**.
**Q7.7 — D** *(§7.2)*. **Clarté/utilité/distribution** = principes de **reporting**.
**Q7.8 — B** *(§7.2)*. **4 thèmes**.
**Q7.9 — A** *(§7.2)*. Thèmes : **gouvernance/infrastructure, agrégation, reporting, surveillance**.
**Q7.10 — C** *(§7.2)*. **Accuracy** : données **fiables**, **automatisées** pour limiter les erreurs.
**Q7.11 — B** *(§7.2)*. **Completeness** : **toutes les expositions matérielles** (lignes, entités, types).
**Q7.12 — C** *(§7.2)*. **Timeliness** : produire l'information **à temps**, surtout en crise.
**Q7.13 — B** *(§7.2)*. P1 : agrégation sous la responsabilité du **conseil/direction** et du cadre de gouvernance.
**Q7.14 — C** *(§7.2)*. P2 : une **infrastructure IT** soutenant l'agrégation **y compris en stress**.
**Q7.15 — B** *(§7.1)*. Émis par le **Comité de Bâle**.
**Q7.16 — D** *(§7.2)*. Les 3 derniers principes : **revue, mesures correctives, coopération *home/host***.
**Q7.17 — B** *(§7.2)*. **Comprehensiveness** = couvrir **tous les risques matériels**.
**Q7.18 — C** *(§7.2)*. **Distribution** : aux **bons destinataires**, en **confidentialité**.
**Q7.19 — B** *(§7.1)*. « On ne gère que ce qu'on voit » $\to$ focus **qualité des données et reporting**.
**Q7.20 — C** *(§7.2)*. La **fréquence** doit **augmenter en stress**.
**Q7.21 — B** *(§7.1)*. **BCBS 239** = document du Comité de Bâle n°239.
**Q7.22 — C** *(§7.2)*. Les systèmes **fragmentés/manuels** sont ce que BCBS 239 veut **corriger**.
**Q7.23 — C** *(§7.2)*. La **clarté des rapports** est un principe de **reporting**, pas d'agrégation.
**Q7.24 — B** *(§7.2)*. Surveillance : **revoir la conformité** et **exiger des corrections**.
**Q7.25 — C** *(§7.2)*. *Home/host* = **coordination transfrontalière** entre superviseurs.
**Q7.26 — C** *(§7.2)*. Bénéfice : des **décisions de risque plus rapides et meilleures** (normal et stress).
**Q7.27 — B** *(§7.2)*. Ne pas produire vite une exposition groupe viole **timeliness/completeness**.
**Q7.28 — C** *(§7.2)*. La **traçabilité automatisée** soutient l'**exactitude/intégrité**.
**Q7.29 — B** *(§7.2)*. La conformité est évaluée par les **superviseurs**.
**Q7.30 — D** *(§7.2)*. But : **renforcer l'agrégation des données de risque et le reporting**.

## Chapitre 8 · Enterprise Risk Management

**Q8.1 — C** *(§8.1)*. L'ERM = gestion **intégrée et transversale**, alignée sur stratégie et appétit.
**Q8.2 — B** *(§8.2)*. L'ERM capte **diversification et concentration** à l'échelle de la firme.
**Q8.3 — B** *(§8.2)*. **Capital économique** = estimation **interne** pour les **UL** à un niveau de confiance.
**Q8.4 — C** *(§8.2)*. Capital « portefeuille » **inférieur** à la somme des silos (**diversification**).
**Q8.5 — B** *(§8.2)*. $\mathrm{RAROC}$ = rendement ajusté du risque / **capital économique**.
**Q8.6 — C** *(§8.2)*. Valeur créée si **RAROC > coût des fonds propres** (*hurdle rate*).
**Q8.7 — A** *(§8.3)*. Référentiels ERM : **COSO ERM** et **ISO 31000**.
**Q8.8 — B** *(§8.3)*. Risques émergents : **climat** et **cyber**.
**Q8.9 — C** *(§8.3)*. Le cadre d'appétit **décline** l'appétit du conseil en **tolérances et limites**.
**Q8.10 — B** *(§8.2)*. Le capital économique est **interne** ($\ne$ réglementaire, imposé).
**Q8.11 — D** *(§8.1)*. L'ERM remplace les **silos** par une **vue intégrée**.
**Q8.12 — C** *(§8.2)*. Le **capital économique** est l'unité commune traduisant tous les risques.
**Q8.13 — B** *(§8.3)*. **COSO ERM** : orienté **gouvernance et objectifs** d'entreprise.
**Q8.14 — C** *(§8.3)*. Risque climatique = **physique** et **de transition**.
**Q8.15 — B** *(§8.3)*. L'IA/ML apporte de **nouveaux risques** (modèle, explicabilité, données, biais).
**Q8.16 — C** *(§8.2)*. Deux petites expositions cumulées peuvent former une **concentration** importante.
**Q8.17 — B** *(§8.2)*. Le RAROC permet de **comparer** des activités et d'**allouer le capital**.
**Q8.18 — C** *(§8.2)*. Le *hurdle rate* = **coût des fonds propres**.
**Q8.19 — B** *(§8.1)*. L'ERM aligne la prise de risque sur la **stratégie et l'appétit du conseil**.
**Q8.20 — C** *(§8.3)*. **ISO 31000** = norme **générique** de management du risque.
**Q8.21 — B** *(§8.2)*. Bénéfice de diversification : **tous les risques ne se réalisent pas ensemble**.
**Q8.22 — C** *(§8.2)*. Le capital économique couvre les **pertes inattendues (UL)**.
**Q8.23 — B** *(§8.1)*. Un **reporting unifié** est une marque de l'ERM.
**Q8.24 — C** *(§8.3)*. Résilience opérationnelle $\to$ **cyber** et **dépendance aux tiers**.
**Q8.25 — B** *(§8.2)*. Activité destructrice de valeur : RAROC **sous le coût du capital**.
**Q8.26 — C** *(§8.2)*. Le capital **réglementaire** est **imposé par le superviseur**.
**Q8.27 — B** *(§8.2)*. La vue « portefeuille » reconnaît les **corrélations** entre types de risque.
**Q8.28 — D** *(§8.3)*. Aussi : **géopolitique, tiers/chaîne d'appro., actifs numériques**.
**Q8.29 — B** *(§8.2)*. L'ERM réduit le capital total grâce à la **diversification entre risques**.
**Q8.30 — C** *(§8.1)*. Définition COSO : **gérer le risque dans l'appétit** pour une **assurance raisonnable** sur les objectifs.

## Chapitre 9 · Learning from Financial Disasters

**Q9.1 — B** *(§9)*. **S&L** = risque de **taux** (*mismatch* de duration actif-passif).
**Q9.2 — C** *(§9)*. **LTCM** = **modèle + levier + corrélations $\to$ 1**.
**Q9.3 — B** *(§9)*. **Barings** = **opérationnel** (pas de séparation front/back office).
**Q9.4 — C** *(§9)*. **Metallgesellschaft** = **base/rollover + liquidité** (appels de marge sur la couverture courte).
**Q9.5 — B** *(§9)*. **Société Générale** = **opérationnel** (positions non autorisées masquées).
**Q9.6 — A** *(§9)*. **Madoff** = **fraude (Ponzi)**, opacité, due diligence défaillante.
**Q9.7 — B** *(§9)*. **Northern Rock** = **liquidité de financement** (gros court terme pour actifs longs).
**Q9.8 — A** *(§9)*. **Bankers Trust/P&G** = ***suitability*/complexité** + réputationnel/juridique.
**Q9.9 — C** *(§9)*. **Orange County** = pari **à fort levier financé par repo** sur les taux.
**Q9.10 — B** *(§9)*. **Bangladesh/SWIFT** = **cyber/opérationnel**.
**Q9.11 — B** *(§9)*. **Volkswagen** = **réputation** (manquement éthique/opérationnel).
**Q9.12 — D** *(§9)*. Leçon des *rogue traders* : **séparation des tâches** + contrôles internes.
**Q9.13 — B** *(§9)*. **London Whale** = **VaR modifiée** sous-estimant le risque + contrôles faibles.
**Q9.14 — C** *(§9)*. **Continental Illinois** = **ruée** sur un financement de gros volatil.
**Q9.15 — B** *(§9)*. **Lehman** = **levier extrême** + actifs illiquides financés **très court** (repo).
**Q9.16 — C** *(§9)*. Leçon S&L = ***mismatch* de duration** (risque de taux du *banking book*, ALM).
**Q9.17 — B** *(§9)*. Le **levier** de LTCM a rendu de **petits mouvements** catastrophiques.
**Q9.18 — C** *(§9)*. En fuite vers la qualité, les corrélations **convergent vers 1** (la diversification échoue).
**Q9.19 — B** *(§9)*. Leeson cachait ses pertes dans un **compte d'erreurs** (« 88888 »).
**Q9.20 — C** *(§9)*. Le *stack-and-roll* a échoué par **base/rollover + timing de trésorerie** (marges).
**Q9.21 — B** *(§9)*. Kerviel exploitait sa connaissance du **back office** et des contrôles.
**Q9.22 — C** *(§9)*. Les rendements de Madoff étaient **trop réguliers/lisses** (invraisemblables).
**Q9.23 — B** *(§9)*. Northern Rock = **première ruée bancaire au Royaume-Uni depuis plus d'un siècle**.
**Q9.24 — C** *(§9)*. P&G : **swaps complexes à fort levier** mal compris.
**Q9.25 — B** *(§9)*. Orange County pariait sur des **taux bas** (qui ont **monté**).
**Q9.26 — C** *(§9)*. Le casse du Bangladesh a utilisé de **faux ordres SWIFT**.
**Q9.27 — B** *(§9)*. Volkswagen : **fraude aux tests d'émissions**.
**Q9.28 — D** *(§9)*. Thème commun : **défaillance des contrôles internes / séparation des tâches**.
**Q9.29 — B** *(§9)*. La « Baleine de Londres » = **JPMorgan** (unité CIO).
**Q9.30 — C** *(§9)*. Risque de **modèle** = pertes dues à des **modèles faux/mal calibrés** ou hypothèses brisées.
**Q9.31 — B** *(§9)*. Gains sur contrats longs **latents/futurs** ; pertes sur futures **en cash immédiat**.
**Q9.32 — C** *(§9)*. *Funding liquidity* : se prémunir par un **financement diversifié/stable** et des **coussins**.
**Q9.33 — D** *(§9)*. La perte est sur les marchés mais la **cause est opérationnelle/de gouvernance**.
**Q9.34 — C** *(§9)*. Leçon : **contrôles, gouvernance et humilité** face aux modèles comptent autant que la vue de marché.

## Chapitre 10 · The Great Financial Crisis

**Q10.1 — B** *(§10.1)*. Hypothèse fausse : **prix immobiliers non baissiers ensemble** et défauts **peu corrélés**.
**Q10.2 — B** *(§10.1)*. *Originate-to-distribute* a **affaibli la sélection** (prêts revendus).
**Q10.3 — C** *(§10.1)*. Agences : **conflits d'intérêts (issuer-pays)** + **modèles défaillants** $\Rightarrow$ sur-notation.
**Q10.4 — B** *(§10.2)*. **Spirale de liquidité** : baisses + retrait du financement $\Rightarrow$ **ventes forcées** $\Rightarrow$ nouvelles baisses.
**Q10.5 — C** *(§10.2)*. Point de bascule : **faillite de Lehman** (sept. 2008).
**Q10.6 — B** *(§10.1)*. **AIG** = concentration **sous-capitalisée** de risque de crédit via **CDS vendus**.
**Q10.7 — B** *(§10.3)*. **LCR/NSFR** introduits par **Bâle III**.
**Q10.8 — A** *(§10.2)*. *Breaking the buck* = **NAV sous 1 \$**, déclenchant une **ruée**.
**Q10.9 — C** *(§10.1)*. *Shadow banking* = **levier élevé** financé par **repo / papier commercial** court.
**Q10.10 — D** *(§10.3)*. Dodd-Frank = **FSOC, stress tests, règle Volcker, compensation centrale**.
**Q10.11 — B** *(§10.1)*. Les *teaser rates* supposaient un **refinancement** porté par la hausse des prix.
**Q10.12 — C** *(§10.1)*. Le *global savings glut* a nourri des **taux bas** et une **course au rendement**.
**Q10.13 — B** *(§10.1)*. Chaîne : **prêts $\to$ MBS $\to$ CDO ($\to$ CDO$^2$)**.
**Q10.14 — C** *(§10.1)*. Des tranches senior subprime ont été notées **AAA** (à tort).
**Q10.15 — B** *(§10.2)*. **Bear Stearns** : **racheté par JPMorgan** avec le soutien de la Fed.
**Q10.16 — C** *(§10.2)*. **Fannie/Freddie** : **mises sous tutelle** (conservatorship), sept. 2008.
**Q10.17 — D** *(§10.3)*. La crise a révélé des défaillances de **capital, liquidité, levier, gouvernance, incitations, transparence**.
**Q10.18 — B** *(§10.3)*. Le **ratio de levier** de Bâle III est **non pondéré du risque**.
**Q10.19 — C** *(§10.3)*. La **règle Volcker** restreint le **trading pour compte propre** des banques.
**Q10.20 — B** *(§10.3)*. La **compensation centrale (CCP)** réduit le **risque de contrepartie** et l'opacité.
**Q10.21 — C** *(§10.3)*. Approche **macroprudentielle** : le **système** et la **procyclicité**.
**Q10.22 — B** *(§10.2)*. *Fire sales* : vendre pour **se désendetter**, ce qui **déprime les prix**.
**Q10.23 — C** *(§10.2)*. Contagion par les **interconnexions** (chacun contrepartie de l'autre).
**Q10.24 — B** *(§10.2)*. **TARP** : **injections de capital et garanties** pour stabiliser les banques.
**Q10.25 — C** *(§10.3)*. *Skin in the game* : l'initiateur **retient** une part du risque titrisé.
**Q10.26 — B** *(§10.1)*. **Repo et papier commercial** = **financement de gros court terme**.
**Q10.27 — C** *(§10.2)*. Crise **systémique** car les défaillances **menaçaient tout le système**.
**Q10.28 — B** *(§10.3)*. Le **CVA** (Bâle III) couvre le **risque de contrepartie** sur dérivés.
**Q10.29 — C** *(§10.2)*. Après le *breaking the buck*, les *money market funds* ont subi une **ruée**.
**Q10.30 — D** *(§10.2)*. Séquence : **pertes $\to$ gel du financement $\to$ ventes forcées $\to$ contagion $\to$ systémique**.
**Q10.31 — B** *(§10.1)*. Cause centrale : **levier excessif** avec **trop peu de capital**.
**Q10.32 — C** *(§10.2)*. Point de bascule à retenir : **la faillite de Lehman**.

## Chapitre 11 · GARP Code of Conduct

**Q11.1 — B** *(§11.1)*. Intégrité : fournir une évaluation **honnête et étayée**, avec **jugement indépendant**.
**Q11.2 — C** *(§11.1)*. Tout conflit d'intérêts (même mineur) doit être **pleinement divulgué**.
**Q11.3 — B** *(§11.1)*. **Protéger** l'information confidentielle (ni divulguée ni utilisée), sauf **obligation légale**.
**Q11.4 — C** *(§11.1)*. Exagérer compétences/résultats = **dénaturation** (interdite).
**Q11.5 — B** *(§11.1)*. **Se conformer** aux lois et réglementations applicables.
**Q11.6 — D** *(§11.1)*. **Intégrité** : faire **primer** l'intérêt des clients/employeurs/du public sur l'intérêt personnel.
**Q11.7 — B** *(§11.1)*. Sanctions jusqu'au **retrait du titre FRM**.
**Q11.8 — C** *(§11.1)*. Le Code s'applique à **tous les membres et candidats**, mondialement.
**Q11.9 — B** *(§11.1)*. Maintenir/développer sa compétence = **responsabilités fondamentales**.
**Q11.10 — C** *(§11.1)*. Distinguer **faits et opinions** ; conclusions fondées sur une **analyse diligente**.
**Q11.11 — C** *(§11.1)*. Exercer un **jugement professionnel indépendant**.
**Q11.12 — B** *(§11.1)*. Aider à une **fraude** est **interdit** (intégrité professionnelle).
**Q11.13 — C** *(§11.1)*. Utiliser une **information privilégiée** pour trader est **interdit** (confidentialité, intégrité, loi).
**Q11.14 — D** *(§11.1)*. En cas de doute, choisir l'option **la plus transparente et prudente**.
**Q11.15 — B** *(§11.1)*. Un conflit inévitable doit être **pleinement divulgué**.
**Q11.16 — C** *(§11.1)*. But du Code : **protéger l'intégrité de la profession** et la **confiance du public**.
**Q11.17 — B** *(§11.1)*. Dénaturer la fiabilité d'un modèle = **misrepresentation** (interdite).
**Q11.18 — C** *(§11.1)*. L'information client reste **confidentielle** (exceptions légales).
**Q11.19 — B** *(§11.1)*. Compétence : **diligence raisonnable** et **maintien des connaissances**.
**Q11.20 — D** *(§11.1)*. Procédure : **enquête et sanctions** (avertissement, suspension, retrait).
**Q11.21 — B** *(§11.1)*. Bonnes pratiques : **méthodes reconnues** et **analyse diligente**.
**Q11.22 — C** *(§11.1)*. Faire primer son **intérêt personnel** sur les clients = **violation**.
**Q11.23 — B** *(§11.1)*. Le respect du Code est une **condition permanente** de la certification.
**Q11.24 — C** *(§11.1)*. Découvrir un manquement $\Rightarrow$ **agir avec intégrité**, **ne pas le dissimuler**.
**Q11.25 — B** *(§11.1)*. Exception à la confidentialité : la **divulgation légalement requise**.
**Q11.26 — C** *(§11.1)*. Falsifier un CV = **dénaturation** (interdite).
**Q11.27 — B** *(§11.1)*. Distinguer clairement **faits et opinions**.
**Q11.28 — D** *(§11.1)*. Même un conflit **apparent** doit être **divulgué**.
**Q11.29 — B** *(§11.1)*. Le Code lie **candidats comme FRM certifiés**.
**Q11.30 — C** *(§11.1)*. Les questions du Code sont des **mises en situation** (identifier la bonne conduite).

---

## Note finale

Cette synthèse couvre **l'intégralité des concepts** du book *Foundations of Risk Management* et propose **354 QCM** au format examen (**30 à 50 par chapitre**), avec un **corrigé commenté en français renvoyant aux sections** de la synthèse (§). *Les questions sont originales : les annales officielles du GARP n'étant pas publiées, aucune n'est attribuée à une session passée.*

**Méthode pour viser le 100 %.** (1) Traite chaque bloc **sans le corrigé** ($\approx$ 1 min 30/question), puis reprends chaque erreur via son **renvoi §**. (2) Mémorise les **formules** (EL ; *credit triangle* ; fonction de perte des tranches ; CAPM/SML ; CML ; ratios de Sharpe/Treynor/Jensen/IR/Sortino/$M^2$ ; APT ; RAROC) et la table **« incident $\to$ type de risque »** (§9). (3) Maîtrise les **pièges** récurrents : EL $\to$ provisions / UL $\to$ capital ; **CML** ($\sigma$, efficients) vs **SML** ($\beta$, tout actif) ; $\rho\uparrow$ pénalise le *senior* ; *funding* vs *market liquidity* ; CRO indépendant ; capital économique (interne) vs réglementaire. (4) Confronte toujours aux ***Learning Objectives* officiels GARP**. Les chapitres **5 et 6** (quantitatifs) et les **mesures de performance** rapportent le plus de points sûrs : assure-les en priorité.

Des **approfondissements dédiés** (démonstrations + études de cas) existent déjà pour les chapitres **4** et **5**, et peuvent être produits pour tout autre chapitre ; un **pack de 100 QCM** par chapitre est également réalisable sur demande.

*Synthèse pédagogique originale — FRM 2026, Part I, Foundations of Risk Management. Équations en LaTeX, notation GARP. À utiliser en complément des manuels officiels.*
