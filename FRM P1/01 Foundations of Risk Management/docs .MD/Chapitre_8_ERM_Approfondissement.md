# Chapitre 8 — Enterprise Risk Management (ERM & Future Trends)

### Gestion des risques d'entreprise et tendances futures

**FRM Part I · Foundations — Approfondissement complet**

*Cours structuré · 50 études de cas · 150 QCM commentés. Questions originales de style examen ; GARP ne publiant aucune annale, aucune question n'est attribuée à une session passée.*

# Partie A — Cours approfondi

## 8.1 De la gestion en silos à l'ERM

### 8.1.1 Le modèle traditionnel en silos

Dans le modèle traditionnel, chaque risque est géré séparément par la fonction ou l'unité qui le porte : la salle de marché suit son risque de marché, la direction des engagements son risque de crédit, la conformité son risque opérationnel et réglementaire. Chaque « silo » dispose de ses propres outils, de ses propres limites et de son propre reporting, sans vision consolidée. Ce cloisonnement présente trois faiblesses majeures. D'abord, il ignore les **interactions entre risques** : un choc de marché peut déclencher des défauts de crédit, qui eux-mêmes tendent les besoins de liquidité — enchaînement qu'aucun silo ne capte à lui seul. Ensuite, il empêche toute **agrégation** cohérente : impossible de répondre à la question « quelle est l'exposition totale du groupe à une contrepartie, tous portefeuilles confondus ? ». Enfin, il produit des **redondances et des angles morts** : deux unités peuvent couvrir le même risque en double pendant qu'un risque transversal n'est suivi par personne.

### 8.1.2 Définition de l'ERM

L'**Enterprise Risk Management (ERM)** est une approche **holistique et intégrée** de la gestion des risques à l'échelle de l'entreprise entière. Plutôt que de traiter chaque risque isolément, l'ERM considère le **portefeuille global de risques** de la firme, en tenant compte de leurs corrélations, de leurs concentrations et de leur contribution à la solvabilité d'ensemble. L'ERM ne supprime pas les fonctions spécialisées : il les coiffe d'une gouvernance transversale, d'un langage commun du risque et d'une agrégation qui remonte jusqu'au conseil d'administration. Son objectif n'est pas de minimiser le risque, mais de l'**optimiser** au regard de la stratégie et de l'appétit pour le risque retenus.

### 8.1.3 Vue holistique et interactions

La valeur ajoutée centrale de l'ERM tient à la prise en compte des **interdépendances**. Le risque agrégé d'un ensemble d'activités n'est jamais la simple somme des risques pris isolément : tant que les risques ne sont pas parfaitement corrélés, une partie se compense (diversification), tandis que d'autres se renforcent en période de stress (corrélations qui convergent vers 1 dans les crises). L'ERM cherche à mesurer cet effet net, à identifier les concentrations cachées et à surveiller les risques émergents qui ne relèvent d'aucun silo établi (cyber, climat, modèle).

> **⚑ Point d'examen —** L'ERM se distingue du modèle en silos par une **vue agrégée et intégrée** tenant compte des interactions entre risques. Son but est d'**optimiser** le profil rendement/risque au regard de la stratégie — **pas** de minimiser mécaniquement le risque, ni de remplacer les fonctions spécialisées.

## 8.2 Les motivations de l'ERM

### 8.2.1 Réduction des coûts de détresse financière

Une firme confrontée à des pertes extrêmes subit des coûts de **détresse financière** : renchérissement du financement, perte de clients et de personnel clé, ventes forcées d'actifs, atteinte à la réputation. Ces coûts sont convexes — ils croissent plus vite que proportionnellement à mesure que la firme s'approche de l'insolvabilité. En lissant la distribution des résultats et en écrêtant les pertes de queue, l'ERM réduit la **probabilité et le coût espéré** de la détresse, ce qui accroît la valeur de la firme même dans un monde où les actionnaires sont diversifiés.

### 8.2.2 Le bénéfice de diversification

C'est le socle quantitatif de l'ERM. Considérons deux activités dont les pertes ont pour écarts-types σ₁ et σ₂ et pour corrélation ρ. La variance de la perte agrégée vaut :

> σ(agg)² = σ₁² + σ₂² + 2ρ σ₁σ₂

d'où l'écart-type agrégé σ(agg) = √(σ₁² + σ₂² + 2ρ σ₁σ₂). Or la somme des risques pris isolément est σ₁ + σ₂. Le **bénéfice de diversification** se définit comme :

> BD = (σ₁ + σ₂) − σ(agg) ≥ 0    dès que  ρ < 1.

Le bénéfice est **nul** si ρ = 1 (risques parfaitement corrélés, aucun gain), **maximal** si ρ = -1, et **strictement positif** dans le cas général ρ < 1. Généralisé à n risques, l'écart-type agrégé s'écrit σ(agg) = √(Σᵢ Σⱼ ρᵢⱼ σᵢσⱼ) : c'est cette sous-additivité qui justifie qu'une vue portefeuille consomme **moins de capital** que la somme des vues isolées.

### 8.2.3 Allocation du capital et avantage informationnel

En reliant chaque activité à sa **contribution au risque agrégé** plutôt qu'à son risque autonome, l'ERM permet une allocation du capital économique fondée sur la réalité de la diversification. Une activité faiblement corrélée au reste du portefeuille consomme peu de capital marginal et peut être développée ; une activité qui concentre le risque en consomme davantage. L'ERM procure aussi un **avantage informationnel** : la direction dispose d'une image consolidée en temps utile, ce qui améliore la qualité et la rapidité des décisions stratégiques.

### 8.2.4 Attentes réglementaires et des parties prenantes

Régulateurs, agences de notation, investisseurs et conseils d'administration attendent désormais un dispositif ERM structuré. Un cadre robuste soutient la **notation de crédit** de la firme, facilite le dialogue prudentiel (les stress tests réglementaires supposent une capacité d'agrégation), et rassure les parties prenantes sur la maîtrise du profil de risque. L'ERM répond ainsi à une double logique : création de valeur interne et légitimité externe.

> **⚑ Point d'examen —** Les motivations de l'ERM : réduire les **coûts de détresse financière**, exploiter le **bénéfice de diversification** (risque agrégé sous-additif dès que ρ < 1), **allouer le capital** selon la contribution marginale au risque, obtenir un **avantage informationnel**, et répondre aux **attentes réglementaires et des parties prenantes**. Le bénéfice de diversification est nul si ρ = 1.

## 8.3 Le cadre ERM : les composantes

Un programme ERM opérationnel s'articule autour de sept composantes complémentaires, popularisées notamment par les travaux de James Lam. Elles couvrent l'ensemble de la chaîne, de la gouvernance jusqu'à la donnée.

> **Les sept composantes d'un programme ERM**
>
> **1. Gouvernance d'entreprise** — le conseil et la direction définissent l'appétit pour le risque, les responsabilités et la surveillance d'ensemble.  

**2. Gestion opérationnelle (*line management**)* — intégration du risque dans les décisions métier : tarification, octroi, développement d'activités.  

**3. Gestion de portefeuille** — vue agrégée des risques, fixation de limites, exploitation de la diversification à l'échelle de la firme.  

**4. Transfert de risque** — réduction des expositions indésirables ou trop coûteuses en capital via assurance, couverture ou titrisation.  

**5. Analytique du risque (*risk analytics**)* — quantification et mesure : VaR, capital économique, stress tests, RAROC.  

**6. Données et technologie** — infrastructure d'agrégation, qualité et traçabilité des données (lien direct avec BCBS 239).  

**7. Gestion des parties prenantes** — communication du profil de risque aux régulateurs, agences de notation, investisseurs et instances internes.

### 8.3.1 Cadres de référence : COSO ERM et ISO 31000

Deux référentiels structurent la pratique. Le **COSO ERM** (2017, *Enterprise Risk Management — Integrating with Strategy and Performance*) relie explicitement le risque à la **stratégie et à la performance**, autour de cinq composantes : gouvernance et culture ; stratégie et fixation des objectifs ; performance ; revue et révision ; information, communication et reporting. L'**ISO 31000** propose un cadre plus générique et international, fondé sur des **principes** (création de valeur, intégration, approche structurée), un **cadre organisationnel** et un **processus** itératif (identification, analyse, évaluation, traitement, suivi). Les deux convergent sur l'idée que le risque se gère de manière intégrée et alignée sur les objectifs, non comme une fonction de contrôle isolée.

> **⚑ Point d'examen —** Les sept composantes de Lam : gouvernance, gestion opérationnelle, gestion de portefeuille, transfert de risque, analytique, données & technologie, parties prenantes. Ne pas confondre **COSO ERM** (centré stratégie/performance, cinq composantes) et **ISO 31000** (cadre générique principes/cadre/processus).

## 8.4 Gouvernance et mise en œuvre

### 8.4.1 Rôle du conseil d'administration

Le conseil est l'organe ultime de la gouvernance du risque. Il lui revient d'**approuver l'appétit pour le risque**, de valider la stratégie de risque, de s'assurer que la direction dispose des moyens de la mettre en œuvre, et de superviser via un **comité des risques** dédié. Le conseil ne gère pas le risque au quotidien : il fixe le cap et contrôle.

### 8.4.2 Rôle de la direction et des lignes métier

La direction générale **met en œuvre** la stratégie de risque : elle traduit l'appétit en politiques et en limites, alloue les ressources, et rend compte au conseil. Les lignes métier portent la responsabilité première des risques qu'elles génèrent — la gestion du risque est l'affaire de tous, pas seulement de la fonction risque.

### 8.4.3 Les trois lignes de défense

Le modèle des **trois lignes de défense** clarifie les responsabilités. La **première ligne** — les métiers — détient et gère les risques au quotidien. La **deuxième ligne** — les fonctions risque et conformité — définit le cadre, fixe les normes, surveille et challenge la première ligne. La **troisième ligne** — l'audit interne — fournit une assurance indépendante sur l'efficacité des deux premières. L'indépendance croît de la première vers la troisième ligne ; la confusion des rôles entre elles est une faiblesse récurrente de gouvernance.

### 8.4.4 Bonnes pratiques et défis de mise en œuvre

Les facteurs de succès sont un **soutien fort du sommet** (*tone from the top*), une culture de risque partagée, des responsabilités claires, une infrastructure de données adéquate et un alignement des incitations. Les défis sont symétriques : résistance des silos existants, difficulté d'agrégation de données hétérogènes, quantification malaisée de certains risques (opérationnel, réputationnel, climatique), coût et durée de la transformation, et risque que l'ERM se réduise à un exercice de conformité formel sans influence réelle sur les décisions.

> **⚑ Point d'examen —** Trois lignes de défense : **(1)** métiers qui détiennent le risque, **(2)** fonctions risque/conformité qui cadrent et surveillent, **(3)** audit interne qui donne une assurance indépendante. Le conseil **approuve l'appétit** et supervise ; la direction **met en œuvre**. La gestion du risque incombe à tous, en premier lieu aux métiers.

## 8.5 Le Chief Risk Officer (CRO)

### 8.5.1 Rôle et responsabilités

Le **Chief Risk Officer (CRO)** dirige la fonction risque de l'entreprise. Il conçoit et déploie la stratégie ERM, supervise l'ensemble des risques (marché, crédit, opérationnel, liquidité, et transversaux), développe les méthodologies de mesure et de limites, veille à la qualité du reporting de risque et alerte sur les expositions et les risques émergents. Le CRO est le garant de la **cohérence transversale** du dispositif.

### 8.5.2 Positionnement et interactions

L'efficacité du CRO repose sur son **indépendance** et son accès direct aux instances de décision. Il rapporte typiquement au **directeur général (CEO)** et dispose d'une **ligne directe vers le comité des risques du conseil**, ce qui lui permet d'exprimer un point de vue sans être subordonné aux intérêts d'une ligne métier. Il interagit avec le **CFO** (capital, planification financière), avec les **responsables métier** (limites, décisions d'exposition) et avec l'**audit interne** (assurance). Cette position à la fois centrale et indépendante distingue le CRO d'un simple responsable de contrôle.

> **⚑ Point d'examen —** Le CRO conçoit et pilote l'ERM, supervise **tous** les risques et garantit la cohérence transversale. Clés d'examen : **indépendance**, reporting au **CEO** et **accès direct au comité des risques du conseil**. Le CRO n'est pas subordonné aux lignes métier dont il challenge les expositions.

## 8.6 L'appétit pour le risque (Risk Appetite Framework)

### 8.6.1 Une hiérarchie de notions

Le **cadre d'appétit pour le risque (RAF)** organise quatre notions emboîtées, qu'il faut soigneusement distinguer :

- La **capacité de risque (*risk capacity*)** est le montant **maximal** de risque que la firme peut absorber compte tenu de son capital, de sa liquidité et de ses contraintes réglementaires. C'est une borne objective.
- L'**appétit pour le risque (*risk appetite*)** est le niveau et le type de risque que la firme **choisit** d'assumer pour atteindre ses objectifs stratégiques. Il est **inférieur** à la capacité, laissant une marge de sécurité.
- La **tolérance au risque (*risk tolerance*)** décline l'appétit en **variations acceptables** autour d'objectifs, par catégorie de risque ou par activité.
- Les **limites de risque (*risk limits*)** traduisent l'appétit et la tolérance en **seuils opérationnels concrets** (limites de VaR, de concentration, de perte) suivis au quotidien.

L'ordre logique est donc : limites ≤ tolérance ≤ appétit ≤ capacité.

### 8.6.2 Le risk appetite statement

Le **risk appetite statement** est le document formel, approuvé par le conseil, qui exprime l'appétit de manière à la fois **qualitative** (types de risques recherchés ou proscrits, comportements attendus) et **quantitative** (cibles de solvabilité, plafonds de perte, notation visée). Il constitue la référence contre laquelle la prise de risque effective est mesurée.

### 8.6.3 Cascade et lien avec la stratégie

Un RAF efficace **cascade** l'appétit du sommet vers les lignes métier : l'énoncé global se décompose en tolérances par activité, puis en limites opérationnelles, de sorte que chaque décision locale reste cohérente avec l'appétit global. Surtout, l'appétit pour le risque est **indissociable de la stratégie** : il en fixe les bornes (quelles activités, quelle croissance, quel levier sont acceptables) et alimente la **planification stratégique**. Une stratégie ambitieuse suppose un appétit — et un capital — cohérents ; à l'inverse, un appétit trop étroit bride la création de valeur.

### 8.6.4 La culture de risque

La **culture de risque** est l'ensemble des valeurs, attitudes et comportements partagés qui déterminent la façon dont le risque est réellement pris et géré au sein de la firme. Elle repose sur le **ton donné par le sommet**, sur des **incitations alignées** (une rémunération qui récompense le rendement ajusté du risque, non le seul rendement brut), sur la liberté de remonter les mauvaises nouvelles, et sur la responsabilisation à tous les niveaux. Une culture défaillante — où les alertes sont ignorées, où les incitations poussent à la prise de risque excessive — vide de sa substance le meilleur dispositif formel.

> **⚑ Point d'examen —** Hiérarchie à mémoriser : **limites ≤ tolérance ≤ appétit ≤ capacité**. La **capacité** est la borne maximale objective ; l'**appétit** est le risque choisi (inférieur, avec marge). L'appétit est **lié à la stratégie** et alimente la planification. La **culture** (ton du sommet, incitations alignées) conditionne l'efficacité réelle du dispositif.

## 8.7 Capital économique et mesures ajustées du risque

### 8.7.1 Pertes attendues, inattendues et capital économique

La distribution des pertes d'un portefeuille se décompose en deux blocs. La **perte attendue (*Expected Loss*, EL)** est la perte moyenne anticipée ; elle se couvre par le **provisionnement** et se répercute dans la **tarification**. La **perte inattendue (*Unexpected Loss*, UL)** est l'écart entre une perte extrême et la moyenne ; c'est elle que le **capital** doit absorber. Le **capital économique (*Economic Capital*, EC)** se définit comme le capital nécessaire pour couvrir les pertes inattendues à un **niveau de confiance** donné α (par exemple 99,9 % sur un an) :

> EC = VaR(α) − EL

où VaR(α) est le quantile de perte au seuil α. Le capital économique n'est donc pas la perte totale possible, mais la seule composante **inattendue** au-delà de la perte moyenne déjà provisionnée.

### 8.7.2 Le bénéfice de diversification en capital

À l'échelle de l'entreprise, le capital économique agrégé est **inférieur** à la somme des capitaux « autonomes » (*standalone*) des activités, exactement pour la raison de sous-additivité vue en §8.2.2. En agrégeant les composantes via leurs corrélations, on obtient une approximation usuelle :

> EC(agg) = √(Σᵢ Σⱼ ρᵢⱼ ECᵢ ECⱼ)

et le **bénéfice de diversification** se mesure par :

> BD = Σᵢ ECᵢ(standalone) − EC(agg) ≥ 0,     ratio = (EC(agg)) ÷ (Σᵢ ECᵢ(standalone)) < 1.

Ce ratio de diversification quantifie l'économie de capital procurée par l'ERM. Il justifie une **allocation** du capital fondée sur la contribution **marginale** de chaque activité au risque agrégé, et non sur son risque isolé.

### 8.7.3 RAROC et création de valeur

Le **RAROC (*Risk-Adjusted Return on Capital*)** rapporte un résultat ajusté du risque au capital économique consommé :

> RAROC = (Revenus − Coûts − EL + rendement du capital) ÷ (Capital économique)

La règle de décision est simple : une activité **crée de la valeur** si son RAROC dépasse le **coût du capital** (le *hurdle rate* exigé par les actionnaires). Le RAROC permet de comparer des activités de natures très différentes sur une base homogène — chaque euro de résultat est pondéré par le risque et le capital qu'il mobilise — et de piloter l'allocation du capital vers les activités les plus rentables au regard du risque.

> **⚑ Point d'examen —** EC = VaR(α) − EL : le capital couvre la perte **inattendue**, pas la perte attendue (déjà provisionnée). Le capital agrégé est **sous-additif** (EC(agg) < Σ ECᵢ(standalone)). Décision RAROC : **accepter si RAROC > coût du capital**. Allocation selon la contribution **marginale** au risque.

## 8.8 Analyse de scénarios et stress testing

### 8.8.1 Analyse de sensibilité vs analyse de scénarios

L'**analyse de sensibilité** fait varier **un seul facteur** à la fois (un taux, un *spread*, un prix) pour isoler son effet, toutes choses égales par ailleurs. L'**analyse de scénarios** fait varier **plusieurs facteurs simultanément** de manière cohérente, en reproduisant un enchaînement plausible d'événements (une récession combine chute des actions, hausse des défauts, tension de liquidité et écartement des *spreads*). La sensibilité éclaire les expositions marginales ; le scénario capte les **interactions** et les effets de second tour — ce qui en fait un outil naturel de l'ERM.

### 8.8.2 Scénarios historiques et hypothétiques

Deux familles se complètent. Les **scénarios historiques** rejouent un épisode passé (krach de 1987, crise de 2008, choc Covid de 2020) : ancrés dans des données réelles et donc crédibles, mais limités par l'hypothèse que « le futur ressemblera au passé ». Les **scénarios hypothétiques** construisent des enchaînements plausibles mais inédits (cyber-attaque systémique, choc climatique brutal, éclatement d'une union monétaire) : indispensables pour capter les risques émergents, mais plus subjectifs dans leur calibrage. Un dispositif robuste combine les deux.

### 8.8.3 Stress testing : l'extrême mais plausible

Le **stress testing** applique des scénarios **extrêmes mais plausibles** pour évaluer la résistance de la firme dans des conditions sévères. Il complète les mesures probabilistes de type VaR en explorant précisément la **queue** de la distribution, là où la VaR est muette. Les stress tests alimentent la planification du capital, la gestion de crise et le dialogue prudentiel ; ils sont au cœur des exercices réglementaires (EBA, Fed CCAR).

### 8.8.4 Le reverse stress testing

Le **reverse stress testing** inverse la logique habituelle. Au lieu de partir d'un scénario pour en déduire un impact, il **part du résultat redouté** — l'insolvabilité, l'épuisement du capital ou de la liquidité, l'échec du modèle d'affaires — et **remonte** aux scénarios et combinaisons de chocs capables d'y conduire. Sa vertu est de révéler les **vulnérabilités cachées** et les hypothèses implicites de viabilité que les stress tests directs, contraints par des scénarios pré-définis, peuvent manquer.

### 8.8.5 Avantages, limites et comparaison avec la VaR

L'analyse de scénarios est **prospective** (forward-looking), capte les **événements de queue** et les interactions, et ne dépend pas de l'hypothèse de normalité. Ses limites tiennent à la **subjectivité** du choix des scénarios, à la difficulté d'y attacher des **probabilités**, et à la forte **dépendance aux hypothèses** retenues. Face à la **VaR** — qui résume le risque en un quantile fondé sur des données historiques et suppose souvent une distribution bien élevée — le scénario offre une lecture complémentaire : là où la VaR répond « combien puis-je perdre dans X % des cas en temps normal ? », le scénario répond « que se passe-t-il si *tel* enchaînement extrême se réalise ? ». Les deux approches ne se substituent pas : elles se complètent.

### 8.8.6 Usages dans l'ERM

Au sein d'un programme ERM, l'analyse de scénarios sert à **tester l'appétit pour le risque** (le portefeuille survit-il au scénario adverse dans les limites fixées ?), à **planifier le capital et la liquidité**, à **préparer la gestion de crise** (plans de continuité, actions de remédiation) et à **challenger les modèles** en confrontant leurs sorties à des situations extrêmes.

> **⚑ Point d'examen —** **Sensibilité** = un facteur ; **scénario** = plusieurs facteurs cohérents. **Stress test** = extrême mais plausible, explore la queue là où la VaR est muette. **Reverse stress testing** = partir du résultat redouté (faillite) et remonter aux scénarios qui y mènent. Scénario vs VaR : complémentaires, non substituables ; le scénario est prospectif et sans hypothèse de normalité.

## 8.9 Tendances futures de la gestion des risques

### 8.9.1 FinTech et désintermédiation

La **FinTech** reconfigure l'intermédiation financière : paiements, prêt entre particuliers, robo-advisors, néobanques. Elle abaisse les coûts et élargit l'accès, mais déplace le risque vers des acteurs moins régulés, crée de nouvelles dépendances technologiques et brouille le périmètre de surveillance prudentielle.

### 8.9.2 Intelligence artificielle, machine learning et big data

L'**IA et le machine learning** transforment le *credit scoring*, la détection de fraude, le trading algorithmique et la surveillance de la conformité, en exploitant le **big data** (sources non structurées, données alternatives). Les gains d'efficacité s'accompagnent de risques nouveaux : **opacité** des modèles (« boîtes noires »), **biais** hérités des données d'entraînement, **risque de modèle** accru, et dépendance à la **qualité des données** — ce qui replace la gouvernance des données (BCBS 239) au premier plan.

### 8.9.3 Cyber-risque

Le **cyber-risque** est devenu une menace de premier ordre, à la fois opérationnelle et potentiellement **systémique** : une attaque sur une infrastructure critique ou un prestataire mutualisé peut se propager à tout le système. Sa gestion mêle sécurité informatique, continuité d'activité, transfert (cyber-assurance) et coopération sectorielle.

### 8.9.4 Risque climatique et ESG

Le **risque climatique** se décline en risque **physique** (dommages liés aux événements climatiques) et risque de **transition** (dévalorisation d'actifs liée au passage à une économie bas-carbone, évolutions réglementaires et technologiques). Intégré au champ plus large **ESG**, il se caractérise par des **horizons longs**, une forte **incertitude** et des données encore immatures, ce qui en fait un défi majeur pour la modélisation et le *stress testing* prudentiel (exercices climatiques des banques centrales).

### 8.9.5 Cryptoactifs, DeFi et RegTech

Les **cryptoactifs** et la **finance décentralisée (DeFi)** introduisent une volatilité élevée, des risques de conservation, de contrepartie et de blanchiment, ainsi qu'un cadre réglementaire encore en construction (MiCA en Europe). En miroir, la **RegTech** automatise la conformité (KYC, reporting réglementaire, surveillance des transactions), promettant efficacité et réduction des coûts, au prix d'une dépendance accrue aux systèmes.

### 8.9.6 Implications pour la gestion des risques

Ces tendances partagent un dénominateur commun : elles accroissent la **vitesse**, l'**interconnexion** et la **complexité** des risques, tout en amplifiant le **risque de modèle** et la dépendance à la donnée. Elles appellent une fonction risque plus **agile**, dotée de compétences technologiques, capable d'agréger et de surveiller des risques qui traversent les frontières traditionnelles — précisément la promesse d'un ERM mature.

> **⚑ Point d'examen —** Tendances futures : **FinTech** (désintermédiation), **IA/ML/big data** (efficacité mais opacité, biais, risque de modèle), **cyber** (menace systémique), **climat/ESG** (risques physique et de transition, horizons longs), **crypto/DeFi** et **RegTech**. Fil conducteur : plus de vitesse, d'interconnexion et de risque de modèle → besoin d'un ERM agile et centré données.

# Partie B — 50 études de cas

## B.1 Des silos à l'ERM (cas 1 à 8)

**Cas 1 — L'exposition invisible.** Une banque découvre après coup qu'une même contrepartie était financée par trois desks distincts, chacun sous sa limite, mais dont la somme dépassait largement tout seuil raisonnable. *Enseignement* : le modèle en silos empêche l'agrégation des expositions ; l'ERM aurait révélé la concentration. (§8.1.1)

**Cas 2 — La cascade non anticipée.** Un choc de marché provoque des appels de marge, qui tarissent la liquidité, qui force des ventes d'actifs, qui aggravent les pertes de marché. Aucune fonction isolée n'avait modélisé l'enchaînement. *Enseignement* : l'ERM capte les **interactions** entre risques que les silos ignorent. (§8.1.3)

**Cas 3 — Deux couvertures pour rien.** Deux filiales couvrent séparément le même risque de change, doublant le coût de couverture pour une exposition nette faible au niveau groupe. *Enseignement* : la vue portefeuille de l'ERM élimine les redondances. (§8.1.1)

**Cas 4 — Le risque de personne.** Un risque cyber transversal n'est suivi ni par la salle de marché, ni par le crédit, ni par les opérations — chacun le croyant du ressort d'un autre. *Enseignement* : les silos créent des angles morts sur les risques émergents. (§8.1.1)

**Cas 5 — Minimiser n'est pas optimiser.** Un dirigeant propose de supprimer toute activité risquée pour « réduire le risque ». Le conseil rappelle que la firme est rémunérée pour prendre des risques maîtrisés. *Enseignement* : l'ERM vise à **optimiser**, non à minimiser le risque. (§8.1.2)

**Cas 6 — Le langage commun.** Faute de taxonomie partagée, « exposition » ne signifie pas la même chose au crédit et au marché, rendant toute consolidation impossible. *Enseignement* : l'ERM suppose un langage et des données communs (lien BCBS 239). (§8.1.2)

**Cas 7 — La corrélation qui monte.** En temps normal, deux portefeuilles semblent diversifiés ; en crise, leurs pertes surviennent ensemble. *Enseignement* : les corrélations convergent vers 1 dans les crises — l'ERM doit modéliser ce phénomène. (§8.1.3)

**Cas 8 — Du contrôle à la stratégie.** Une firme traite le risque comme une simple fonction de contrôle *a posteriori*. Un concurrent l'intègre à sa stratégie et arbitre mieux ses activités. *Enseignement* : l'ERM relie le risque à la stratégie et à la performance. (§8.1.2)

## B.2 Motivations et diversification (cas 9 à 16)

**Cas 9 — Le coût convexe de la détresse.** À l'approche de l'insolvabilité, une entreprise voit son coût de financement bondir et ses meilleurs clients partir. *Enseignement* : les coûts de détresse sont convexes ; écrêter les pertes de queue crée de la valeur. (§8.2.1)

**Cas 10 — Diversification chiffrée.** Deux activités ont σ₁ = 100 et σ₂ = 100 (M€). Isolées, elles somment à 200. Avec ρ = 0,3, l'agrégat vaut √(100² + 100² + 2(0,3)(100)(100)) = √(26 000) ≈ 161. *Enseignement* : bénéfice de diversification ≈ 39 M€, car ρ < 1. (§8.2.2)

**Cas 11 — Corrélation parfaite, aucun gain.** Mêmes activités avec ρ = 1 : l'agrégat vaut exactement √((100+100)²) = 200. *Enseignement* : si ρ = 1, le bénéfice de diversification est **nul**. (§8.2.2)

**Cas 12 — La couverture parfaite.** Avec ρ = -1, l'agrégat vaut |100 − 100| = 0. *Enseignement* : la diversification est maximale (risque annulé) quand ρ = -1. (§8.2.2)

**Cas 13 — Capital marginal, pas autonome.** Une nouvelle activité, faiblement corrélée au reste, consomme peu de capital **marginal** malgré un risque autonome élevé. *Enseignement* : l'ERM alloue le capital selon la contribution marginale au risque agrégé. (§8.2.3)

**Cas 14 — L'avantage informationnel.** Disposant d'une vue consolidée en temps réel, une direction réagit à un retournement avant ses concurrents encore prisonniers de reportings cloisonnés. *Enseignement* : l'ERM procure un avantage informationnel décisionnel. (§8.2.3)

**Cas 15 — La notation en jeu.** Une agence relève la note d'une firme après la mise en place d'un dispositif ERM crédible, abaissant son coût de financement. *Enseignement* : l'ERM répond aux attentes des parties prenantes et soutient la notation. (§8.2.4)

**Cas 16 — Lissage des résultats.** En réduisant la volatilité de ses résultats, une entreprise rassure investisseurs et créanciers et stabilise son accès au marché. *Enseignement* : la stabilité des résultats est une motivation directe de l'ERM. (§8.2.1)

## B.3 Composantes, frameworks et gouvernance (cas 17 à 26)

**Cas 17 — Le maillon données.** Un programme ERM ambitieux échoue faute d'infrastructure de données capable d'agréger des systèmes hétérogènes. *Enseignement* : « données et technologie » est une composante à part entière (lien BCBS 239). (§8.3)

**Cas 18 — Transférer plutôt que porter.** Une firme identifie un risque coûteux en capital et le cède via réassurance plutôt que de le conserver. *Enseignement* : le transfert de risque est une composante de l'ERM, à arbitrer contre son coût. (§8.3)

**Cas 19 — COSO ou ISO ?** Un comité hésite entre référentiels. Il retient COSO ERM pour son lien explicite à la stratégie et à la performance. *Enseignement* : COSO ERM (stratégie/performance, cinq composantes) diffère d'ISO 31000 (cadre générique). (§8.3.1)

**Cas 20 — L'analytique sans les métiers.** Une équipe quantitative produit des mesures sophistiquées que les lignes métier n'utilisent jamais. *Enseignement* : sans « line management » (intégration opérationnelle), l'analytique reste lettre morte. (§8.3)

**Cas 21 — Le conseil qui gère.** Un conseil s'immisce dans les décisions d'octroi quotidiennes au lieu de fixer l'appétit et de superviser. *Enseignement* : le conseil approuve l'appétit et contrôle ; il ne gère pas au quotidien. (§8.4.1)

**Cas 22 — Confusion des lignes.** La fonction risque (2ᵉ ligne) prend elle-même des positions, brouillant son rôle de challenge indépendant. *Enseignement* : les trois lignes de défense doivent rester distinctes. (§8.4.3)

**Cas 23 — L'audit qui cadre.** Un audit interne (3ᵉ ligne) se met à définir les limites de risque, empiétant sur la 2ᵉ ligne et perdant son indépendance d'assurance. *Enseignement* : la 3ᵉ ligne donne une assurance, elle ne conçoit pas le cadre. (§8.4.3)

**Cas 24 — Le ton du sommet.** Un dirigeant qui contourne publiquement les limites de risque signale que les règles sont optionnelles. *Enseignement* : le *tone from the top* est déterminant pour la mise en œuvre de l'ERM. (§8.4.4)

**Cas 25 — L'ERM de façade.** Un dispositif ERM existe sur le papier mais n'influence aucune décision réelle. *Enseignement* : le principal défaut de mise en œuvre est l'ERM réduit à un exercice de conformité formel. (§8.4.4)

**Cas 26 — Le risque de tous.** Un incident opérationnel révèle que les métiers considéraient le risque comme « l'affaire de la fonction risque ». *Enseignement* : la responsabilité première du risque incombe aux métiers (1ʳᵉ ligne). (§8.4.2)

## B.4 CRO et appétit pour le risque (cas 27 à 34)

**Cas 27 — Le CRO subordonné.** Un CRO rapporte hiérarchiquement au responsable de la salle de marché dont il doit challenger les positions. *Enseignement* : le CRO doit être indépendant et disposer d'un accès direct au conseil. (§8.5.2)

**Cas 28 — La ligne vers le conseil.** Un CRO alerte le comité des risques du conseil d'une dérive que la direction générale minimisait. *Enseignement* : l'accès direct du CRO au comité des risques est une garantie clé de gouvernance. (§8.5.2)

**Cas 29 — Capacité confondue avec appétit.** Une firme calibre ses limites sur son capital **maximal** disponible, sans marge de sécurité. *Enseignement* : l'appétit doit être **inférieur** à la capacité ; les confondre supprime le coussin de sécurité. (§8.6.1)

**Cas 30 — La hiérarchie respectée.** Une banque vérifie que ses limites opérationnelles restent en deçà des tolérances, elles-mêmes sous l'appétit, lui-même sous la capacité. *Enseignement* : limites ≤ tolérance ≤ appétit ≤ capacité. (§8.6.1)

**Cas 31 — L'énoncé qui engage.** Un *risk appetite statement* approuvé par le conseil fixe une notation cible et un plafond de perte annuelle, servant de référence de pilotage. *Enseignement* : l'énoncé d'appétit combine cibles qualitatives et quantitatives. (§8.6.2)

**Cas 32 — La cascade rompue.** L'appétit global est défini, mais jamais décliné en limites par activité ; les métiers naviguent à vue. *Enseignement* : un RAF doit cascader l'appétit jusqu'aux limites opérationnelles. (§8.6.3)

**Cas 33 — Appétit et stratégie.** Une firme veut doubler de taille sans revoir son appétit ni son capital ; l'incohérence menace sa solvabilité. *Enseignement* : appétit et stratégie sont indissociables et doivent être calibrés ensemble. (§8.6.3)

**Cas 34 — Les incitations perverses.** Une rémunération indexée sur le rendement brut pousse les équipes à maximiser le risque. *Enseignement* : une culture de risque saine aligne les incitations sur le rendement **ajusté du risque**. (§8.6.4)

## B.5 Scénarios et stress testing (cas 35 à 42)

**Cas 35 — Un facteur ne suffit pas.** Une analyse de sensibilité sur les seuls taux manque l'effet combiné taux + défauts + liquidité d'une récession. *Enseignement* : l'analyse de scénarios fait varier plusieurs facteurs de façon cohérente. (§8.8.1)

**Cas 36 — Rejouer 2008.** Une firme teste son portefeuille sur le scénario historique de la crise de 2008. *Enseignement* : les scénarios historiques sont crédibles mais supposent que le futur ressemble au passé. (§8.8.2)

**Cas 37 — L'inédit plausible.** Craignant un choc sans précédent, un risk manager construit un scénario hypothétique de cyber-attaque systémique. *Enseignement* : les scénarios hypothétiques captent les risques émergents que l'histoire n'a pas encore produits. (§8.8.2)

**Cas 38 — La queue que la VaR ignore.** Un stress test révèle une perte potentielle bien supérieure à la VaR à 99 %. *Enseignement* : le stress testing explore la queue de distribution là où la VaR est muette. (§8.8.3)

**Cas 39 — Partir de la faillite.** Plutôt que d'appliquer des scénarios pré-définis, une banque se demande « quels chocs nous rendraient insolvables ? » et remonte aux causes. *Enseignement* : c'est la démarche du *reverse stress testing*. (§8.8.4)

**Cas 40 — La vulnérabilité cachée.** Le reverse stress testing révèle qu'une combinaison modérée mais simultanée de chocs — que nul scénario direct n'avait envisagée — suffirait à épuiser le capital. *Enseignement* : le reverse stress testing débusque les hypothèses implicites de viabilité. (§8.8.4)

**Cas 41 — Complément, non substitut.** Un comité oppose VaR et scénarios ; le CRO rappelle qu'ils répondent à des questions différentes. *Enseignement* : VaR (quantile probabiliste) et scénarios (impact conditionnel) sont complémentaires. (§8.8.5)

**Cas 42 — Tester l'appétit.** Un scénario adverse est appliqué pour vérifier que le portefeuille reste dans les limites de l'appétit défini. *Enseignement* : l'analyse de scénarios sert à tester l'appétit et à planifier le capital. (§8.8.6)

## B.6 Tendances futures et cas chiffrés (cas 43 à 50)

**Cas 43 — La boîte noire.** Un modèle de *credit scoring* par apprentissage automatique refuse des crédits sans explication auditable. *Enseignement* : l'IA accroît l'efficacité mais soulève opacité et risque de modèle. (§8.9.2)

**Cas 44 — Le biais hérité.** Un modèle entraîné sur des données historiques reproduit un biais discriminatoire. *Enseignement* : le machine learning hérite des biais de ses données d'entraînement — vigilance data et gouvernance. (§8.9.2)

**Cas 45 — Le prestataire mutualisé.** Une attaque sur un fournisseur cloud partagé menace simultanément plusieurs institutions. *Enseignement* : le cyber-risque peut devenir systémique par les dépendances communes. (§8.9.3)

**Cas 46 — Physique et transition.** Un assureur évalue à la fois les dommages climatiques directs (physique) et la dévalorisation de ses actifs carbonés (transition). *Enseignement* : le risque climatique combine risque physique et risque de transition. (§8.9.4)

**Cas 47 — Bénéfice de diversification en capital.** Trois activités ont des capitaux autonomes de 60, 50 et 40 (M€), somme = 150. Après agrégation par corrélations, EC(agg) = 120. *Enseignement* : BD = 150 − 120 = **30** M€ ; ratio de diversification = 120/150 = **0,80**. (§8.7.2)

**Cas 48 — Capital économique.** Un portefeuille a une VaR à 99,9 % d'un an de 250 M€ et une perte attendue de 40 M€. *Enseignement* : EC = 250 − 40 = **210** M€ ; le capital couvre la perte **inattendue**, pas l'attendue déjà provisionnée. (§8.7.1)

**Cas 49 — Décision RAROC.** Une activité dégage un résultat ajusté du risque de 18 M€ pour un capital économique de 120 M€, soit un RAROC de 18/120 = **15 %**. Le coût du capital exigé est de 12 %. *Enseignement* : RAROC (15 %) > coût du capital (12 %) → l'activité **crée de la valeur**. (§8.7.3)

**Cas 50 — RAROC sous le seuil.** Une autre activité affiche un RAROC de 9 % pour un *hurdle rate* de 12 %. *Enseignement* : RAROC (9 %) < coût du capital (12 %) → l'activité **détruit de la valeur** et doit être réduite, repricée ou cédée. (§8.7.3)

# Partie C — 150 questions à choix multiple

*Questions originales de style examen. GARP ne publiant aucune annale, aucune n'est tirée d'une session passée. Réponses commentées en Partie D, avec renvois aux sections (§) et aux cas.*

## C.1 Des silos à l'ERM (Q1 à Q20)

**Q1.** L'ERM se distingue principalement du modèle en silos par —

- **(A)** une réduction systématique de tous les risques à zéro
- **(B)** une vue agrégée et intégrée tenant compte des interactions entre risques
- **(C)** la suppression des fonctions de risque spécialisées
- **(D)** l'externalisation de la gestion des risques
**Q2.** L'objectif central de l'ERM est de —

- **(A)** minimiser mécaniquement le risque total
- **(B)** éliminer entièrement le risque de crédit
- **(C)** optimiser le profil rendement/risque au regard de la stratégie
- **(D)** maximiser le levier financier
**Q3.** Une faiblesse majeure du modèle en silos est —

- **(A)** l'incapacité à agréger les expositions transversales
- **(B)** un excès de coordination entre unités
- **(C)** une centralisation excessive des décisions
- **(D)** un coût de capital structurellement trop faible
**Q4.** Dans le modèle en silos, un risque transversal émergent (ex. cyber) tend à être —

- **(A)** surveillé en double par toutes les unités
- **(B)** parfaitement couvert par la première ligne
- **(C)** un angle mort dont personne n'a la charge
- **(D)** automatiquement transféré à un assureur
**Q5.** L'ERM considère le risque de la firme comme —

- **(A)** la somme arithmétique des risques individuels
- **(B)** exclusivement du risque de marché
- **(C)** une variable indépendante de la stratégie
- **(D)** un portefeuille dont les composantes interagissent
**Q6.** En période de crise, les corrélations entre risques tendent à —

- **(A)** diminuer vers zéro
- **(B)** rester parfaitement stables
- **(C)** converger vers 1, réduisant la diversification
- **(D)** devenir systématiquement négatives
**Q7.** L'ERM relie la gestion des risques à —

- **(A)** la stratégie et la performance de la firme
- **(B)** la seule conformité réglementaire
- **(C)** la comptabilité analytique uniquement
- **(D)** la gestion de trésorerie exclusivement
**Q8.** Le cloisonnement en silos produit typiquement —

- **(A)** une allocation optimale du capital
- **(B)** une agrégation parfaite des expositions
- **(C)** une culture de risque parfaitement homogène
- **(D)** des redondances et des angles morts
**Q9.** Une condition préalable à l'agrégation des risques dans l'ERM est —

- **(A)** l'abandon de toute quantification
- **(B)** une taxonomie et des données communes
- **(C)** la suppression du reporting interne
- **(D)** l'indépendance totale des entités
**Q10.** Affirmer que l'ERM « minimise le risque » est —

- **(A)** exact dans tous les cas
- **(B)** vrai uniquement pour les banques
- **(C)** inexact : l'ERM vise à optimiser, pas à minimiser
- **(D)** vrai uniquement en période de crise
**Q11.** La prise en compte des interactions entre risques permet à l'ERM de —

- **(A)** ignorer les corrélations
- **(B)** détecter concentrations et effets de second tour
- **(C)** supprimer tout besoin de capital
- **(D)** garantir l'absence de pertes
**Q12.** Le risque agrégé d'un ensemble d'activités est —

- **(A)** toujours égal à la somme des risques isolés
- **(B)** toujours strictement supérieur à la somme
- **(C)** inférieur ou égal à la somme selon les corrélations
- **(D)** indépendant des corrélations
**Q13.** Dans une organisation en silos, mesurer l'exposition totale du groupe à une contrepartie est —

- **(A)** immédiat et automatique
- **(B)** sans intérêt opérationnel
- **(C)** du ressort du seul audit externe
- **(D)** difficile voire impossible
**Q14.** L'ERM ne supprime pas les fonctions spécialisées mais —

- **(A)** les remplace par un logiciel unique
- **(B)** les coiffe d'une gouvernance transversale et d'un langage commun
- **(C)** les rend totalement indépendantes
- **(D)** les externalise à des tiers
**Q15.** Une vue holistique des risques est surtout utile parce qu'elle —

- **(A)** révèle les interdépendances invisibles en silos
- **(B)** réduit le nombre d'employés
- **(C)** supprime le risque opérationnel
- **(D)** augmente le levier
**Q16.** La diversification à l'échelle de l'entreprise implique que le risque total —

- **(A)** croît toujours avec le nombre d'activités
- **(B)** devient nul
- **(C)** bénéficie de compensations tant que ρ < 1
- **(D)** égale la VaR d'une seule activité
**Q17.** Le passage du silo à l'ERM est avant tout —

- **(A)** un changement de logiciel comptable
- **(B)** une transformation de gouvernance et de culture
- **(C)** une simple réduction d'effectifs
- **(D)** une décision purement réglementaire
**Q18.** Un risque de concentration caché est mieux détecté par —

- **(A)** chaque silo pris indépendamment
- **(B)** l'audit externe seul
- **(C)** la comptabilité générale
- **(D)** une agrégation ERM à l'échelle de la firme
**Q19.** L'ERM traite le risque comme —

- **(A)** une contrainte purement défensive
- **(B)** un levier stratégique à optimiser
- **(C)** une simple variable comptable
- **(D)** un problème de la seule salle de marché
**Q20.** Quelle proposition décrit le mieux l'ERM ?

- **(A)** Gérer chaque risque dans son unité, séparément
- **(B)** Transférer tous les risques à des tiers
- **(C)** Gérer le portefeuille de risques de façon intégrée, en tenant compte des corrélations
- **(D)** Maximiser le rendement sans considération de risque
## C.2 Motivations et diversification (Q21 à Q40)

**Q21.** Les coûts de détresse financière sont dits « convexes » car ils —

- **(A)** décroissent à l'approche de l'insolvabilité
- **(B)** croissent plus vite que proportionnellement près de l'insolvabilité
- **(C)** restent strictement constants
- **(D)** varient de façon parfaitement linéaire
**Q22.** Écrêter les pertes de queue crée de la valeur car cela —

- **(A)** augmente la volatilité des résultats
- **(B)** supprime la charge d'impôt
- **(C)** réduit la probabilité et le coût espéré de la détresse
- **(D)** élimine tout risque de marché
**Q23.** La variance de la perte agrégée de deux risques vaut —

- **(A)** σ₁² + σ₂²
- **(B)** σ₁² + σ₂² + 2ρσ₁σ₂
- **(C)** σ₁ + σ₂
- **(D)** ρσ₁σ₂
**Q24.** Le bénéfice de diversification est nul lorsque —

- **(A)** ρ = 0
- **(B)** ρ = -1
- **(C)** ρ = 1
- **(D)** ρ = 0,5
**Q25.** Le bénéfice de diversification est maximal lorsque —

- **(A)** ρ = 1
- **(B)** ρ = -1
- **(C)** ρ = 0
- **(D)** les deux risques sont identiques
**Q26.** Avec σ₁ = σ₂ = 100 et ρ = 0, l'écart-type agrégé vaut environ —

- **(A)** 200
- **(B)** 141
- **(C)** 100
- **(D)** 0
**Q27.** La sous-additivité du risque implique qu'une vue portefeuille consomme —

- **(A)** plus de capital que la somme des vues isolées
- **(B)** moins de capital que la somme des vues isolées
- **(C)** exactement le même capital
- **(D)** aucun capital
**Q28.** L'allocation du capital dans l'ERM doit se fonder sur —

- **(A)** le risque autonome de chaque activité
- **(B)** le chiffre d'affaires de chaque activité
- **(C)** la contribution marginale au risque agrégé
- **(D)** le nombre d'employés
**Q29.** Une activité faiblement corrélée au reste du portefeuille —

- **(A)** consomme beaucoup de capital marginal
- **(B)** consomme peu de capital marginal
- **(C)** doit systématiquement être supprimée
- **(D)** augmente le risque agrégé plus que son risque propre
**Q30.** L'« avantage informationnel » de l'ERM désigne —

- **(A)** l'accès à des informations privilégiées de marché
- **(B)** une image consolidée du risque améliorant les décisions
- **(C)** un avantage fiscal
- **(D)** une réduction mécanique des effectifs
**Q31.** Un dispositif ERM crédible peut soutenir —

- **(A)** la dégradation de la qualité des données
- **(B)** la notation de crédit de la firme
- **(C)** l'augmentation illimitée du levier
- **(D)** l'abandon du reporting réglementaire
**Q32.** La stabilité des résultats est une motivation de l'ERM car elle —

- **(A)** augmente la volatilité
- **(B)** annule le capital réglementaire
- **(C)** rassure investisseurs et créanciers
- **(D)** supprime le risque de crédit
**Q33.** Généralisé à n risques, l'écart-type agrégé s'écrit —

- **(A)** Σᵢ σᵢ
- **(B)** √(Σᵢ Σⱼ ρᵢⱼσᵢσⱼ)
- **(C)** Σᵢ σᵢ²
- **(D)** maxᵢ σᵢ
**Q34.** Les attentes réglementaires en matière d'ERM se manifestent notamment via —

- **(A)** l'interdiction des stress tests
- **(B)** la suppression de la fonction CRO
- **(C)** les stress tests supposant une capacité d'agrégation
- **(D)** l'abandon du capital économique
**Q35.** Dans un monde où les actionnaires sont diversifiés, l'ERM —

- **(A)** devient totalement inutile
- **(B)** conserve de la valeur via la réduction des coûts de détresse
- **(C)** impose de maximiser le risque
- **(D)** supprime le risque idiosyncratique
**Q36.** Avec σ₁ = 60, σ₂ = 80 et ρ = 1, l'agrégat vaut —

- **(A)** 100
- **(B)** 140
- **(C)** 20
- **(D)** 0
**Q37.** Avec σ₁ = 60, σ₂ = 80 et ρ = -1, l'agrégat vaut —

- **(A)** 140
- **(B)** 20
- **(C)** 100
- **(D)** 0
**Q38.** Le bénéfice de diversification se définit comme —

- **(A)** σ(agg) − (σ₁ + σ₂)
- **(B)** (σ₁ + σ₂) − σ(agg)
- **(C)** σ₁ × σ₂
- **(D)** la valeur de ρ
**Q39.** Une activité fortement corrélée au portefeuille (concentrant le risque) devrait —

- **(A)** consommer peu de capital
- **(B)** consommer davantage de capital marginal
- **(C)** être développée sans limite
- **(D)** être exemptée de toute limite
**Q40.** La motivation « réduction des coûts de détresse » repose sur le fait que ces coûts —

- **(A)** sont parfaitement symétriques aux gains
- **(B)** sont convexes et pénalisent lourdement les pertes extrêmes
- **(C)** n'existent pas pour les banques
- **(D)** sont fixes quel que soit le niveau de perte
## C.3 Composantes et cadres de référence (Q41 à Q60)

**Q41.** Parmi les composantes d'un programme ERM (Lam) figure —

- **(A)** la gestion de portefeuille
- **(B)** la maximisation du dividende
- **(C)** la suppression de l'audit interne
- **(D)** l'externalisation totale du risque
**Q42.** La composante « données et technologie » de l'ERM est directement liée à —

- **(A)** la politique de dividende
- **(B)** la fixation des taux directeurs
- **(C)** BCBS 239 (agrégation et qualité des données)
- **(D)** la comptabilité fiscale
**Q43.** La composante « transfert de risque » consiste à —

- **(A)** conserver l'intégralité des risques
- **(B)** céder les risques indésirables ou coûteux en capital
- **(C)** ignorer les risques identifiés
- **(D)** augmenter délibérément l'exposition
**Q44.** La composante « line management » désigne —

- **(A)** l'audit interne
- **(B)** la fonction juridique
- **(C)** la gestion de trésorerie
- **(D)** l'intégration du risque dans les décisions métier
**Q45.** Le référentiel COSO ERM (2017) met l'accent sur —

- **(A)** la seule conformité comptable
- **(B)** l'intégration du risque à la stratégie et à la performance
- **(C)** l'abandon de la gouvernance
- **(D)** le trading haute fréquence
**Q46.** L'ISO 31000 se caractérise par —

- **(A)** un cadre générique (principes, cadre, processus)
- **(B)** un cadre réservé aux seules banques américaines
- **(C)** l'interdiction du stress testing
- **(D)** l'absence de tout principe directeur
**Q47.** La composante « risk analytics » recouvre notamment —

- **(A)** la gestion de la paie
- **(B)** la communication presse
- **(C)** la VaR, le capital économique et les stress tests
- **(D)** la gestion immobilière
**Q48.** La composante « stakeholder management » vise à —

- **(A)** ignorer les régulateurs
- **(B)** communiquer le profil de risque aux parties prenantes
- **(C)** supprimer le reporting externe
- **(D)** maximiser la volatilité des résultats
**Q49.** Combien de composantes structurent le programme ERM selon Lam ?

- **(A)** Trois
- **(B)** Cinq
- **(C)** Douze
- **(D)** Sept
**Q50.** COSO ERM comporte cinq composantes, dont —

- **(A)** gouvernance et culture
- **(B)** politique monétaire
- **(C)** tarification de détail
- **(D)** gestion de la paie
**Q51.** Sans intégration opérationnelle (line management), l'analytique de risque —

- **(A)** suffit à elle seule
- **(B)** remplace la gouvernance
- **(C)** reste inutilisée par les métiers
- **(D)** élimine le besoin de données
**Q52.** Le choix entre COSO ERM et ISO 31000 dépend surtout —

- **(A)** du cours de bourse
- **(B)** du besoin de lien explicite à la stratégie (COSO) vs cadre générique (ISO)
- **(C)** de la météo
- **(D)** du nombre d'agences bancaires
**Q53.** La composante « portfolio management » de l'ERM permet —

- **(A)** d'ignorer les limites
- **(B)** de supprimer le capital
- **(C)** de maximiser la concentration
- **(D)** une vue agrégée et l'exploitation de la diversification
**Q54.** Le processus ISO 31000 est —

- **(A)** ponctuel et unique
- **(B)** itératif (identification, analyse, évaluation, traitement, suivi)
- **(C)** limité au risque de marché
- **(D)** dépourvu de suivi
**Q55.** La gouvernance d'entreprise, comme composante ERM, relève en premier lieu —

- **(A)** de la salle de marché
- **(B)** de l'audit externe
- **(C)** du conseil et de la direction
- **(D)** des clients de la firme
**Q56.** Un programme ERM échouant faute d'agréger des systèmes hétérogènes illustre une défaillance de —

- **(A)** la composante données et technologie
- **(B)** la politique de dividende
- **(C)** la communication presse
- **(D)** la fiscalité
**Q57.** Le transfert de risque s'arbitre contre —

- **(A)** rien, il est toujours gratuit
- **(B)** son coût (prime, base de couverture)
- **(C)** le seul cours de l'action
- **(D)** la masse salariale
**Q58.** COSO ERM et ISO 31000 convergent sur l'idée que le risque —

- **(A)** se gère en silos
- **(B)** doit être ignoré
- **(C)** relève de la seule conformité
- **(D)** se gère de manière intégrée et alignée sur les objectifs
**Q59.** La composante « risk analytics » se distingue de « data and technology » en ce qu'elle —

- **(A)** fournit l'infrastructure de données
- **(B)** quantifie et mesure le risque à partir des données
- **(C)** gère les ressources humaines
- **(D)** communique aux régulateurs
**Q60.** Lequel de ces éléments N'EST PAS une composante ERM de Lam ?

- **(A)** Gouvernance d'entreprise
- **(B)** Gestion de portefeuille
- **(C)** Maximisation du levier à court terme
- **(D)** Transfert de risque
## C.4 Gouvernance, trois lignes de défense et CRO (Q61 à Q85)

**Q61.** Le rôle premier du conseil d'administration en matière de risque est de —

- **(A)** gérer les positions au quotidien
- **(B)** approuver l'appétit pour le risque et superviser
- **(C)** exécuter les transactions de couverture
- **(D)** tenir la comptabilité
**Q62.** La direction générale, en matière de risque, a pour rôle de —

- **(A)** mettre en œuvre la stratégie de risque et rendre compte au conseil
- **(B)** définir seule l'appétit sans le conseil
- **(C)** réaliser l'audit interne
- **(D)** remplacer le comité des risques
**Q63.** Dans le modèle des trois lignes de défense, la première ligne est —

- **(A)** l'audit interne
- **(B)** la fonction conformité
- **(C)** les métiers qui détiennent et gèrent les risques
- **(D)** le régulateur
**Q64.** La deuxième ligne de défense —

- **(A)** prend les positions de marché
- **(B)** définit le cadre, fixe les normes, surveille et challenge la première ligne
- **(C)** réalise l'assurance indépendante finale
- **(D)** approuve les comptes annuels
**Q65.** La troisième ligne de défense est assurée par —

- **(A)** les métiers
- **(B)** la fonction risque
- **(C)** la conformité
- **(D)** l'audit interne, qui fournit une assurance indépendante
**Q66.** Dans le modèle des trois lignes, l'indépendance —

- **(A)** décroît de la première vers la troisième ligne
- **(B)** croît de la première vers la troisième ligne
- **(C)** est identique pour les trois lignes
- **(D)** n'existe qu'en première ligne
**Q67.** La responsabilité première des risques générés par une activité incombe —

- **(A)** aux métiers qui les génèrent (première ligne)
- **(B)** exclusivement à la fonction risque
- **(C)** à l'audit externe
- **(D)** au régulateur
**Q68.** Le Chief Risk Officer (CRO) a pour mission principale de —

- **(A)** maximiser le résultat de trading
- **(B)** tenir la comptabilité
- **(C)** concevoir et piloter la stratégie ERM
- **(D)** gérer la communication commerciale
**Q69.** Le périmètre de supervision du CRO couvre —

- **(A)** le seul risque de marché
- **(B)** l'ensemble des risques, y compris transversaux
- **(C)** le seul risque de crédit
- **(D)** uniquement le risque opérationnel
**Q70.** Deux garanties clés du bon positionnement du CRO sont —

- **(A)** sa subordination à la salle de marché et sa rémunération variable
- **(B)** son rattachement au service commercial
- **(C)** son rôle d'exécution des transactions
- **(D)** son indépendance et son accès direct au conseil
**Q71.** Le CRO rapporte typiquement au —

- **(A)** directeur général (CEO)
- **(B)** responsable de la salle de marché
- **(C)** directeur commercial
- **(D)** trésorier
**Q72.** L'accès direct du CRO au comité des risques du conseil vise à —

- **(A)** contourner la comptabilité
- **(B)** lui permettre d'alerter sans être subordonné aux lignes métier
- **(C)** augmenter son bonus
- **(D)** remplacer l'audit interne
**Q73.** Le CRO se distingue d'un simple responsable de contrôle car il —

- **(A)** exécute les transactions
- **(B)** dépend hiérarchiquement des métiers
- **(C)** est indépendant et challenge les expositions des métiers
- **(D)** ne rend compte à personne
**Q74.** Le facteur culturel déterminant pour la mise en œuvre de l'ERM est —

- **(A)** le montant des dividendes
- **(B)** le ton donné par le sommet (tone from the top)
- **(C)** la localisation du siège
- **(D)** la taille du bilan
**Q75.** Le principal écueil de mise en œuvre de l'ERM est —

- **(A)** un excès de quantification
- **(B)** une centralisation trop poussée
- **(C)** une culture trop prudente
- **(D)** un ERM réduit à un exercice de conformité formel
**Q76.** Concernant le conseil d'administration, il est exact que —

- **(A)** il fixe l'appétit et supervise, sans gérer au quotidien
- **(B)** il exécute les couvertures de marché
- **(C)** il remplace la fonction risque
- **(D)** il tient les livres comptables
**Q77.** La confusion des rôles entre les trois lignes de défense constitue —

- **(A)** une bonne pratique de flexibilité
- **(B)** une faiblesse récurrente de gouvernance
- **(C)** une exigence réglementaire
- **(D)** un gage d'indépendance
**Q78.** Parmi les facteurs de succès de l'ERM figure —

- **(A)** l'absence de responsabilités définies
- **(B)** des incitations indexées sur le rendement brut
- **(C)** un soutien fort du sommet et des incitations alignées
- **(D)** l'isolement de la fonction risque
**Q79.** Parmi les défis de mise en œuvre de l'ERM figure —

- **(A)** l'excès de coopération entre entités
- **(B)** la résistance des silos et la difficulté d'agréger des données hétérogènes
- **(C)** la simplicité excessive des risques
- **(D)** l'absence de régulateurs
**Q80.** Le CRO interagit avec le CFO principalement sur —

- **(A)** le capital et la planification financière
- **(B)** la publicité produit
- **(C)** le recrutement commercial
- **(D)** la maintenance immobilière
**Q81.** L'audit interne (troisième ligne) doit —

- **(A)** définir les limites de risque
- **(B)** fournir une assurance indépendante, sans concevoir le cadre
- **(C)** prendre des positions de marché
- **(D)** fixer l'appétit pour le risque
**Q82.** La relation entre deuxième et première ligne se caractérise par —

- **(A)** une subordination de la deuxième à la première
- **(B)** une fusion des deux fonctions
- **(C)** un rôle de cadrage et de challenge de la première par la deuxième
- **(D)** une absence d'interaction
**Q83.** Dans un dispositif ERM sain, la gestion du risque est —

- **(A)** réservée à la fonction risque
- **(B)** l'affaire de tous, en premier lieu des métiers
- **(C)** externalisée à l'audit
- **(D)** du ressort du seul régulateur
**Q84.** Le comité des risques du conseil est —

- **(A)** un desk de trading
- **(B)** une fonction de back-office
- **(C)** une entité commerciale
- **(D)** un organe de supervision du risque au niveau du conseil
**Q85.** Le CRO est le garant de —

- **(A)** la maximisation du chiffre d'affaires
- **(B)** la cohérence transversale du dispositif de risque
- **(C)** la seule conformité fiscale
- **(D)** la politique de communication
## C.5 Appétit pour le risque et culture (Q86 à Q105)

**Q86.** La hiérarchie correcte du cadre d'appétit pour le risque est —

- **(A)** appétit ≤ limites ≤ tolérance ≤ capacité
- **(B)** capacité ≤ appétit ≤ tolérance ≤ limites
- **(C)** limites ≤ tolérance ≤ appétit ≤ capacité
- **(D)** tolérance ≤ limites ≤ capacité ≤ appétit
**Q87.** La capacité de risque (risk capacity) désigne —

- **(A)** le montant maximal de risque absorbable compte tenu du capital et des contraintes
- **(B)** le risque que la firme choisit d'assumer
- **(C)** une limite opérationnelle quotidienne
- **(D)** une variation acceptable autour d'un objectif
**Q88.** L'appétit pour le risque (risk appetite) est —

- **(A)** toujours égal à la capacité de risque
- **(B)** le niveau de risque choisi pour atteindre les objectifs, inférieur à la capacité
- **(C)** le risque maximal absorbable
- **(D)** une mesure comptable du capital
**Q89.** La tolérance au risque (risk tolerance) correspond à —

- **(A)** la capacité maximale
- **(B)** l'énoncé stratégique global
- **(C)** le capital réglementaire
- **(D)** les variations acceptables autour d'objectifs, par catégorie ou activité
**Q90.** Les limites de risque (risk limits) sont —

- **(A)** des concepts purement théoriques
- **(B)** des seuils opérationnels concrets suivis au quotidien
- **(C)** identiques à la capacité de risque
- **(D)** définies uniquement par le régulateur
**Q91.** Le risk appetite statement est —

- **(A)** un document commercial
- **(B)** une simple note interne informelle
- **(C)** un document formel approuvé par le conseil, qualitatif et quantitatif
- **(D)** un rapport comptable annuel
**Q92.** Que l'appétit soit inférieur à la capacité permet —

- **(A)** de laisser une marge de sécurité
- **(B)** d'annuler tout risque
- **(C)** de maximiser le levier
- **(D)** de supprimer les limites
**Q93.** « Cascader » l'appétit signifie —

- **(A)** l'ignorer au niveau des métiers
- **(B)** le décliner en tolérances par activité puis en limites opérationnelles
- **(C)** le fixer une seule fois sans suivi
- **(D)** le confier à l'audit externe
**Q94.** La relation entre appétit pour le risque et stratégie est —

- **(A)** inexistante
- **(B)** une simple coïncidence
- **(C)** gérée uniquement par la trésorerie
- **(D)** indissociable : l'appétit borne la stratégie et alimente sa planification
**Q95.** L'appétit pour le risque alimente —

- **(A)** la publicité produit
- **(B)** la planification stratégique de la firme
- **(C)** la seule comptabilité fiscale
- **(D)** la gestion des locaux
**Q96.** La culture de risque désigne —

- **(A)** le montant du capital réglementaire
- **(B)** la structure juridique de la firme
- **(C)** les valeurs, attitudes et comportements partagés face au risque
- **(D)** le nombre de desks de trading
**Q97.** Un pilier de la culture de risque est —

- **(A)** le ton donné par le sommet
- **(B)** l'anonymat des décisions
- **(C)** l'absence de responsabilité
- **(D)** la maximisation du bonus brut
**Q98.** Une culture de risque saine suppose des incitations —

- **(A)** indexées sur le rendement brut
- **(B)** alignées sur le rendement ajusté du risque
- **(C)** déconnectées de toute performance
- **(D)** fondées sur le seul volume d'activité
**Q99.** Confondre capacité et appétit de risque revient à —

- **(A)** renforcer la marge de sécurité
- **(B)** réduire le levier
- **(C)** supprimer le coussin de sécurité de la firme
- **(D)** améliorer la notation
**Q100.** Un bon risk appetite statement combine —

- **(A)** uniquement des cibles qualitatives
- **(B)** des cibles qualitatives et quantitatives
- **(C)** uniquement des données comptables
- **(D)** aucune cible mesurable
**Q101.** Une cascade d'appétit rompue conduit à —

- **(A)** une meilleure agrégation
- **(B)** une baisse du capital requis
- **(C)** une culture plus forte
- **(D)** des métiers naviguant à vue, sans limites cohérentes
**Q102.** Un appétit pour le risque trop étroit —

- **(A)** maximise toujours la valeur
- **(B)** bride la création de valeur
- **(C)** est sans effet sur la stratégie
- **(D)** augmente automatiquement le rendement
**Q103.** Une culture de risque défaillante —

- **(A)** vide de sa substance le meilleur dispositif formel
- **(B)** renforce l'efficacité des limites
- **(C)** est sans conséquence
- **(D)** améliore la remontée des mauvaises nouvelles
**Q104.** Une rémunération indexée sur le rendement brut crée —

- **(A)** un alignement parfait des intérêts
- **(B)** une réduction du risque
- **(C)** une incitation à une prise de risque excessive
- **(D)** une amélioration de la culture
**Q105.** Quel classement respecte le cadre d'appétit pour le risque ?

- **(A)** Capacité < appétit
- **(B)** Limites ≤ tolérance ≤ appétit ≤ capacité
- **(C)** Appétit > capacité
- **(D)** Tolérance > capacité
## C.6 Capital économique et mesures ajustées du risque (Q106 à Q120)

**Q106.** La perte attendue (Expected Loss) se couvre par —

- **(A)** le provisionnement et la tarification
- **(B)** le seul capital économique
- **(C)** une augmentation du levier
- **(D)** le transfert obligatoire à un tiers
**Q107.** La perte inattendue (Unexpected Loss) est —

- **(A)** la perte moyenne anticipée
- **(B)** l'écart entre une perte extrême et la moyenne, absorbé par le capital
- **(C)** toujours nulle
- **(D)** couverte par le provisionnement
**Q108.** Le capital économique se définit comme —

- **(A)** la perte totale possible
- **(B)** la perte attendue seule
- **(C)** VaR(α) − EL
- **(D)** le chiffre d'affaires annuel
**Q109.** Le capital économique couvre —

- **(A)** la perte attendue déjà provisionnée
- **(B)** la perte inattendue au-delà de la moyenne
- **(C)** l'ensemble des coûts d'exploitation
- **(D)** les dividendes
**Q110.** Avec une VaR à 99,9% d'un an de 250 M€ et une perte attendue de 40 M€, le capital économique vaut —

- **(A)** 290 M€
- **(B)** 250 M€
- **(C)** 40 M€
- **(D)** 210 M€
**Q111.** Le capital économique agrégé d'une entreprise est —

- **(A)** supérieur à la somme des capitaux autonomes
- **(B)** inférieur à la somme des capitaux autonomes (sous-additivité)
- **(C)** égal à la somme des capitaux autonomes
- **(D)** indépendant des corrélations
**Q112.** L'approximation usuelle du capital économique agrégé est —

- **(A)** Σᵢ ECᵢ
- **(B)** maxᵢ ECᵢ
- **(C)** √(Σᵢ Σⱼ ρᵢⱼ ECᵢ ECⱼ)
- **(D)** Σᵢ ECᵢ²
**Q113.** Le bénéfice de diversification en capital se mesure par —

- **(A)** Σᵢ ECᵢ(standalone) − EC(agg)
- **(B)** EC(agg) − Σᵢ ECᵢ(standalone)
- **(C)** EC(agg) × Σᵢ ECᵢ(standalone)
- **(D)** la VaR seule
**Q114.** Le ratio de diversification EC(agg) / Σᵢ ECᵢ(standalone) est —

- **(A)** toujours supérieur à 1
- **(B)** inférieur à 1
- **(C)** toujours égal à 1
- **(D)** négatif
**Q115.** Le RAROC rapporte —

- **(A)** le chiffre d'affaires au nombre d'employés
- **(B)** le dividende au cours de bourse
- **(C)** un résultat ajusté du risque au capital économique consommé
- **(D)** la perte attendue à la perte inattendue
**Q116.** La règle de décision RAROC est : une activité crée de la valeur si —

- **(A)** son RAROC est nul
- **(B)** son RAROC dépasse le coût du capital (hurdle rate)
- **(C)** son RAROC est négatif
- **(D)** son chiffre d'affaires augmente
**Q117.** Le RAROC permet de —

- **(A)** comparer des activités hétérogènes sur une base homogène ajustée du risque
- **(B)** ignorer le capital consommé
- **(C)** maximiser le levier sans limite
- **(D)** supprimer la mesure du risque
**Q118.** Une activité au RAROC de 15% avec un coût du capital de 12% —

- **(A)** détruit de la valeur
- **(B)** crée de la valeur
- **(C)** est neutre en valeur
- **(D)** doit être cédée immédiatement
**Q119.** Une activité au RAROC de 9% avec un hurdle rate de 12% —

- **(A)** crée de la valeur
- **(B)** est neutre
- **(C)** détruit de la valeur et doit être réduite, repricée ou cédée
- **(D)** doit être développée en priorité
**Q120.** L'allocation du capital fondée sur le RAROC oriente le capital selon —

- **(A)** le seul chiffre d'affaires
- **(B)** la contribution au risque et le rendement ajusté
- **(C)** l'ancienneté des activités
- **(D)** la taille des équipes
## C.7 Analyse de scénarios et stress testing (Q121 à Q140)

**Q121.** L'analyse de sensibilité se distingue de l'analyse de scénarios en ce qu'elle —

- **(A)** fait varier tous les facteurs à la fois
- **(B)** fait varier un seul facteur à la fois
- **(C)** ignore les facteurs de risque
- **(D)** ne s'applique qu'au risque de crédit
**Q122.** L'analyse de scénarios consiste à —

- **(A)** isoler l'effet d'un facteur unique
- **(B)** supprimer les corrélations
- **(C)** faire varier plusieurs facteurs simultanément de façon cohérente
- **(D)** calculer une moyenne historique
**Q123.** Un scénario historique —

- **(A)** rejoue un épisode réellement survenu (1987, 2008, 2020)
- **(B)** invente un enchaînement jamais observé
- **(C)** ignore les données passées
- **(D)** est purement théorique
**Q124.** La limite principale des scénarios historiques est —

- **(A)** leur absence de fondement empirique
- **(B)** l'hypothèse que le futur ressemblera au passé
- **(C)** leur incapacité à utiliser des données réelles
- **(D)** leur coût prohibitif
**Q125.** Un scénario hypothétique —

- **(A)** se limite à reproduire le passé
- **(B)** ne peut pas être quantifié
- **(C)** construit un enchaînement plausible mais inédit
- **(D)** est interdit par la réglementation
**Q126.** L'intérêt des scénarios hypothétiques est de —

- **(A)** réduire la subjectivité à zéro
- **(B)** capter les risques émergents que l'histoire n'a pas encore produits
- **(C)** éviter toute modélisation
- **(D)** supprimer le besoin de données
**Q127.** Le stress testing applique des scénarios —

- **(A)** parfaitement normaux et fréquents
- **(B)** impossibles et absurdes
- **(C)** sans lien avec la firme
- **(D)** extrêmes mais plausibles
**Q128.** Le stress testing complète la VaR car il —

- **(A)** reproduit exactement la VaR
- **(B)** explore la queue de distribution là où la VaR est muette
- **(C)** remplace toute mesure probabiliste
- **(D)** ignore les événements extrêmes
**Q129.** Le reverse stress testing consiste à —

- **(A)** partir d'un scénario pour en déduire un impact
- **(B)** calculer une VaR inversée
- **(C)** partir du résultat redouté (insolvabilité) et remonter aux scénarios
- **(D)** ignorer les scénarios extrêmes
**Q130.** Le reverse stress testing part du résultat redouté puis —

- **(A)** s'arrête là
- **(B)** remonte aux scénarios et combinaisons de chocs capables d'y conduire
- **(C)** calcule la perte attendue
- **(D)** fixe le dividende
**Q131.** La vertu du reverse stress testing est de —

- **(A)** confirmer les scénarios déjà connus
- **(B)** réduire le nombre de scénarios
- **(C)** supprimer le stress testing direct
- **(D)** révéler des vulnérabilités et hypothèses de viabilité cachées
**Q132.** L'analyse de scénarios est qualifiée de —

- **(A)** prospective (forward-looking)
- **(B)** purement rétrospective
- **(C)** comptable
- **(D)** réglementaire uniquement
**Q133.** Une limite de l'analyse de scénarios est —

- **(A)** son objectivité totale
- **(B)** la subjectivité du choix des scénarios
- **(C)** son indépendance vis-à-vis des hypothèses
- **(D)** son incapacité à être prospective
**Q134.** Une autre limite de l'analyse de scénarios est —

- **(A)** la trop grande précision des probabilités
- **(B)** l'excès de données disponibles
- **(C)** la difficulté d'attacher des probabilités aux scénarios
- **(D)** l'absence de toute hypothèse
**Q135.** Face aux scénarios, la VaR se caractérise par —

- **(A)** l'absence de toute hypothèse
- **(B)** un résumé en un quantile fondé sur des données historiques, supposant souvent la normalité
- **(C)** une capture systématique des événements extrêmes
- **(D)** une nature purement prospective
**Q136.** La relation entre VaR et analyse de scénarios est —

- **(A)** la VaR remplace les scénarios
- **(B)** les scénarios remplacent la VaR
- **(C)** elles sont incompatibles
- **(D)** elles sont complémentaires, répondant à des questions différentes
**Q137.** Un avantage de l'analyse de scénarios sur la VaR est qu'elle —

- **(A)** est plus simple à calculer
- **(B)** ne dépend pas de l'hypothèse de normalité
- **(C)** utilise moins de données
- **(D)** fournit une probabilité exacte
**Q138.** Dans l'ERM, l'analyse de scénarios sert notamment à —

- **(A)** tester si le portefeuille reste dans les limites de l'appétit
- **(B)** fixer les dividendes
- **(C)** recruter les équipes
- **(D)** gérer les locaux
**Q139.** L'analyse de scénarios contribue aussi à —

- **(A)** la publicité produit
- **(B)** la planification du capital et de la liquidité
- **(C)** la comptabilité fiscale seule
- **(D)** la maintenance informatique
**Q140.** Parmi les usages ERM de l'analyse de scénarios figure —

- **(A)** la suppression du reporting
- **(B)** l'augmentation du levier
- **(C)** la préparation de la gestion de crise et le challenge des modèles
- **(D)** l'abandon des limites
## C.8 Tendances futures (Q141 à Q150)

**Q141.** La FinTech agit principalement sur l'intermédiation financière par —

- **(A)** la désintermédiation (paiements, prêt P2P, néobanques)
- **(B)** la suppression de la monnaie
- **(C)** l'interdiction du crédit
- **(D)** la centralisation bancaire renforcée
**Q142.** Les principaux risques associés à l'IA et au machine learning sont —

- **(A)** l'excès de transparence
- **(B)** l'opacité, les biais et le risque de modèle
- **(C)** la disparition du risque de données
- **(D)** l'absence totale de risque
**Q143.** Un modèle de machine learning peut reproduire un biais discriminatoire car —

- **(A)** il est toujours neutre
- **(B)** il ignore les données
- **(C)** il hérite des biais présents dans ses données d'entraînement
- **(D)** il supprime les corrélations
**Q144.** Le cyber-risque est préoccupant notamment parce qu'il peut être —

- **(A)** sans conséquence
- **(B)** systémique via des dépendances communes (prestataires mutualisés)
- **(C)** limité à une seule firme par nature
- **(D)** entièrement éliminé par l'assurance
**Q145.** Le risque climatique se décline en —

- **(A)** risque de taux et risque de change
- **(B)** risque de liquidité seul
- **(C)** risque opérationnel seul
- **(D)** risque physique et risque de transition
**Q146.** Les cryptoactifs et la DeFi introduisent —

- **(A)** une stabilité parfaite
- **(B)** une volatilité élevée, des risques de conservation et un cadre réglementaire en construction
- **(C)** l'absence de tout risque de contrepartie
- **(D)** une réglementation déjà pleinement aboutie
**Q147.** La RegTech vise à —

- **(A)** automatiser la conformité (KYC, reporting, surveillance des transactions)
- **(B)** supprimer la réglementation
- **(C)** augmenter la fraude
- **(D)** remplacer le conseil d'administration
**Q148.** Le recours à l'IA et au big data replace au premier plan —

- **(A)** la politique de dividende
- **(B)** la gouvernance et la qualité des données (BCBS 239)
- **(C)** la gestion immobilière
- **(D)** la publicité
**Q149.** Le dénominateur commun des tendances futures est —

- **(A)** la baisse de la vitesse et de l'interconnexion
- **(B)** la disparition du risque de modèle
- **(C)** plus de vitesse, d'interconnexion et de risque de modèle
- **(D)** la simplification des risques
**Q150.** Ces tendances appellent une fonction risque —

- **(A)** figée et cloisonnée
- **(B)** plus agile, dotée de compétences technologiques et centrée sur la donnée
- **(C)** réduite à la conformité formelle
- **(D)** sans capacité d'agrégation
# Partie D — Corrigé commenté

*Chaque réponse renvoie aux sections (§) du cours et aux études de cas correspondantes.*

## D.1 Corrigé — Q1 à Q40 (silos, ERM, motivations, diversification)

**Q1 — B** *(§8.1.2 · cas 1)* L'ERM apporte une **vue agrégée et intégrée** tenant compte des interactions ; il ne supprime pas les fonctions spécialisées et ne réduit pas les risques à zéro.

**Q2 — C** *(§8.1.2 · cas 5)* L'ERM vise à **optimiser** le couple rendement/risque au regard de la stratégie, non à minimiser mécaniquement le risque.

**Q3 — A** *(§8.1.1 · cas 1)* Le cloisonnement empêche toute **agrégation** : impossible de sommer les expositions transversales d'un même groupe.

**Q4 — C** *(§8.1.1 · cas 4)* Un risque transversal émergent (cyber) devient un **angle mort**, chaque silo le croyant du ressort d'un autre.

**Q5 — D** *(§8.1.3)* L'ERM traite les risques comme un **portefeuille dont les composantes interagissent** (corrélations, effets de second tour).

**Q6 — C** *(§8.1.3 · cas 7)* En crise, les corrélations **convergent vers 1**, ce qui réduit la diversification observée en temps normal.

**Q7 — A** *(§8.1.2 · cas 8)* L'ERM relie explicitement le risque à la **stratégie et à la performance** (logique COSO ERM).

**Q8 — D** *(§8.1.1 · cas 3)* Les silos produisent des **redondances** (couvertures en double) et des **angles morts** (risques non suivis).

**Q9 — B** *(§8.1.2 · cas 6)* L'agrégation suppose une **taxonomie et des données communes** — lien direct avec BCBS 239.

**Q10 — C** *(§8.1.2)* Affirmation **inexacte** : l'ERM optimise le profil de risque, il ne le minimise pas.

**Q11 — B** *(§8.1.3)* La prise en compte des interactions permet de détecter **concentrations** et **effets de second tour** invisibles en silos.

**Q12 — C** *(§8.2.2)* Par sous-additivité, le risque agrégé est **inférieur ou égal** à la somme des risques isolés selon les corrélations.

**Q13 — D** *(§8.1.1 · cas 1)* En silos, mesurer l'exposition totale d'un groupe à une contrepartie est **difficile voire impossible**.

**Q14 — B** *(§8.1.2)* L'ERM **coiffe** les fonctions spécialisées d'une gouvernance transversale et d'un langage commun, sans les supprimer.

**Q15 — A** *(§8.1.3)* La vue holistique révèle les **interdépendances** que les silos ne peuvent pas voir.

**Q16 — C** *(§8.2.2)* La diversification produit des compensations tant que les risques ne sont pas parfaitement corrélés (ρ < 1).

**Q17 — B** *(§8.1.2 · cas 8)* Le passage à l'ERM est d'abord une **transformation de gouvernance et de culture**, non un simple outil.

**Q18 — D** *(§8.1.3 · cas 1)* Une **agrégation à l'échelle de la firme** détecte les concentrations cachées mieux que chaque silo isolé.

**Q19 — B** *(§8.1.2)* L'ERM traite le risque comme un **levier stratégique à optimiser**, pas seulement comme une contrainte.

**Q20 — C** *(§8.1.2)* Meilleure définition : gérer le **portefeuille de risques de façon intégrée**, en tenant compte des corrélations.

**Q21 — B** *(§8.2.1 · cas 9)* Les coûts de détresse sont **convexes** : ils croissent plus vite que proportionnellement près de l'insolvabilité.

**Q22 — C** *(§8.2.1)* Écrêter la queue **réduit la probabilité et le coût espéré** de la détresse, d'où création de valeur.

**Q23 — B** *(§8.2.2)* Variance de la perte agrégée : σ₁² + σ₂² + 2ρσ₁σ₂.

**Q24 — C** *(§8.2.2 · cas 11)* Si ρ = 1 (risques parfaitement corrélés), le bénéfice de diversification est **nul**.

**Q25 — B** *(§8.2.2 · cas 12)* Le bénéfice est **maximal** lorsque ρ = -1 (les risques se compensent).

**Q26 — B** *(§8.2.2 · cas 10)* √(100² + 100²) = √(20 000) ≈ 141 ; le bénéfice vaut 200 − 141 ≈ 59.

**Q27 — B** *(§8.2.2)* La **sous-additivité** implique qu'une vue portefeuille consomme **moins** de capital que la somme des vues isolées.

**Q28 — C** *(§8.2.3 · cas 13)* L'allocation se fonde sur la **contribution marginale** au risque agrégé, non sur le risque autonome.

**Q29 — B** *(§8.2.3 · cas 13)* Une activité faiblement corrélée consomme **peu de capital marginal** malgré un risque propre élevé.

**Q30 — B** *(§8.2.3 · cas 14)* L'avantage informationnel est une **image consolidée** du risque qui améliore et accélère les décisions.

**Q31 — B** *(§8.2.4 · cas 15)* Un dispositif ERM crédible soutient la **notation de crédit** et abaisse le coût de financement.

**Q32 — C** *(§8.2.1 · cas 16)* La stabilité des résultats **rassure investisseurs et créanciers** et stabilise l'accès au marché.

**Q33 — B** *(§8.2.2)* Généralisation à n risques : √(Σᵢ Σⱼ ρᵢⱼσᵢσⱼ).

**Q34 — C** *(§8.2.4)* Les **stress tests** réglementaires supposent une capacité d'agrégation, manifestant l'attente prudentielle.

**Q35 — B** *(§8.2.1)* Même avec des actionnaires diversifiés, l'ERM **crée de la valeur** via la réduction des coûts de détresse.

**Q36 — B** *(§8.2.2)* ρ = 1 : agrégat = σ₁ + σ₂ = 60 + 80 = 140.

**Q37 — B** *(§8.2.2)* ρ = -1 : agrégat = |σ₁ − σ₂| = |60 − 80| = 20.

**Q38 — B** *(§8.2.2)* Bénéfice de diversification = (σ₁ + σ₂) − σ(agg) ≥ 0.

**Q39 — B** *(§8.2.3)* Une activité concentrant le risque (forte corrélation) consomme **davantage** de capital marginal.

**Q40 — B** *(§8.2.1)* Les coûts de détresse sont **convexes** : ils pénalisent lourdement les pertes extrêmes.

## D.2 Corrigé — Q41 à Q85 (composantes, cadres, gouvernance, CRO)

**Q41 — A** *(§8.3 · cas 20)* La **gestion de portefeuille** est l'une des sept composantes de Lam (vue agrégée, limites).

**Q42 — C** *(§8.3 · cas 17)* « Données et technologie » renvoie à l'infrastructure d'agrégation et de qualité — **BCBS 239**.

**Q43 — B** *(§8.3 · cas 18)* Le transfert de risque consiste à **céder** les expositions indésirables ou coûteuses en capital.

**Q44 — D** *(§8.3)* « Line management » = **intégration du risque dans les décisions métier** (tarification, octroi).

**Q45 — B** *(§8.3.1 · cas 19)* COSO ERM (2017) relie le risque à la **stratégie et à la performance**.

**Q46 — A** *(§8.3.1)* ISO 31000 est un **cadre générique** : principes, cadre organisationnel, processus itératif.

**Q47 — C** *(§8.3)* « Risk analytics » recouvre **VaR, capital économique, stress tests, RAROC**.

**Q48 — B** *(§8.3)* « Stakeholder management » = **communiquer le profil de risque** aux parties prenantes.

**Q49 — D** *(§8.3)* Lam identifie **sept** composantes d'un programme ERM.

**Q50 — A** *(§8.3.1)* Parmi les cinq composantes de COSO ERM figure **gouvernance et culture**.

**Q51 — C** *(§8.3 · cas 20)* Sans « line management », l'analytique sophistiquée **reste inutilisée** par les métiers.

**Q52 — B** *(§8.3.1 · cas 19)* Le choix dépend du besoin de **lien à la stratégie** (COSO) ou d'un **cadre générique** (ISO).

**Q53 — D** *(§8.3)* « Portfolio management » offre une **vue agrégée** et exploite la diversification.

**Q54 — B** *(§8.3.1)* Le processus ISO 31000 est **itératif** : identification, analyse, évaluation, traitement, suivi.

**Q55 — C** *(§8.4.1)* La gouvernance d'entreprise relève d'abord du **conseil et de la direction**.

**Q56 — A** *(§8.3 · cas 17)* Un échec d'agrégation de systèmes hétérogènes est une défaillance de **données et technologie**.

**Q57 — B** *(§8.3 · cas 18)* Le transfert de risque s'arbitre contre **son coût** (prime, base de couverture).

**Q58 — D** *(§8.3.1)* COSO et ISO convergent : le risque se gère **de manière intégrée** et alignée sur les objectifs.

**Q59 — B** *(§8.3)* « Risk analytics » **quantifie** le risque à partir des données ; « data and technology » fournit l'infrastructure.

**Q60 — C** *(§8.3)* La **maximisation du levier à court terme** n'est pas une composante ERM de Lam.

**Q61 — B** *(§8.4.1 · cas 21)* Le conseil **approuve l'appétit** et **supervise** ; il ne gère pas les positions au quotidien.

**Q62 — A** *(§8.4.2)* La direction générale **met en œuvre** la stratégie de risque et rend compte au conseil.

**Q63 — C** *(§8.4.3)* Première ligne = **les métiers**, qui détiennent et gèrent les risques.

**Q64 — B** *(§8.4.3)* Deuxième ligne (risque/conformité) **définit le cadre, fixe les normes, surveille et challenge**.

**Q65 — D** *(§8.4.3)* Troisième ligne = **audit interne**, assurance indépendante sur les deux premières.

**Q66 — B** *(§8.4.3)* L'indépendance **croît** de la première vers la troisième ligne.

**Q67 — A** *(§8.4.2 · cas 26)* La responsabilité première du risque incombe aux **métiers** (première ligne).

**Q68 — C** *(§8.5.1)* Le CRO **conçoit et pilote la stratégie ERM**.

**Q69 — B** *(§8.5.1)* Le CRO supervise **l'ensemble des risques**, y compris transversaux.

**Q70 — D** *(§8.5.2 · cas 27)* Garanties clés : **indépendance** et **accès direct au conseil**.

**Q71 — A** *(§8.5.2)* Le CRO rapporte typiquement au **directeur général (CEO)**.

**Q72 — B** *(§8.5.2 · cas 28)* L'accès direct au comité des risques permet d'**alerter sans subordination** aux lignes métier.

**Q73 — C** *(§8.5.2)* Le CRO est **indépendant** et challenge les expositions des métiers, à la différence d'un simple contrôleur.

**Q74 — B** *(§8.4.4 · cas 24)* Le **ton donné par le sommet** (tone from the top) est déterminant pour la mise en œuvre.

**Q75 — D** *(§8.4.4 · cas 25)* Le principal écueil est un **ERM réduit à un exercice de conformité formel**.

**Q76 — A** *(§8.4.1)* Le conseil **fixe l'appétit et supervise**, sans gérer au quotidien.

**Q77 — B** *(§8.4.3 · cas 22)* La **confusion des rôles** entre lignes est une faiblesse récurrente de gouvernance.

**Q78 — C** *(§8.4.4)* Facteur de succès : **soutien fort du sommet** et **incitations alignées**.

**Q79 — B** *(§8.4.4)* Défi : **résistance des silos** et difficulté d'**agréger des données hétérogènes**.

**Q80 — A** *(§8.5.2)* Le CRO interagit avec le CFO sur le **capital et la planification financière**.

**Q81 — B** *(§8.4.3 · cas 23)* La troisième ligne **fournit une assurance** ; elle ne conçoit pas le cadre ni ne fixe l'appétit.

**Q82 — C** *(§8.4.3)* La deuxième ligne **cadre et challenge** la première ; elles restent distinctes.

**Q83 — B** *(§8.4.2 · cas 26)* La gestion du risque est **l'affaire de tous**, en premier lieu des métiers.

**Q84 — D** *(§8.4.1)* Le comité des risques du conseil est un **organe de supervision** du risque au niveau du conseil.

**Q85 — B** *(§8.5.1)* Le CRO garantit la **cohérence transversale** du dispositif de risque.

## D.3 Corrigé — Q86 à Q120 (appétit, culture, capital économique, RAROC)

**Q86 — C** *(§8.6.1 · cas 30)* Hiérarchie correcte : **limites ≤ tolérance ≤ appétit ≤ capacité**.

**Q87 — A** *(§8.6.1)* La **capacité** est le montant **maximal** de risque absorbable compte tenu du capital et des contraintes.

**Q88 — B** *(§8.6.1)* L'**appétit** est le risque **choisi** pour atteindre les objectifs, inférieur à la capacité.

**Q89 — D** *(§8.6.1)* La **tolérance** décline l'appétit en **variations acceptables** par catégorie ou activité.

**Q90 — B** *(§8.6.1)* Les **limites** sont des **seuils opérationnels concrets** suivis au quotidien.

**Q91 — C** *(§8.6.2 · cas 31)* Le risk appetite statement est un **document formel** approuvé par le conseil, qualitatif et quantitatif.

**Q92 — A** *(§8.6.1 · cas 29)* Un appétit inférieur à la capacité laisse une **marge de sécurité**.

**Q93 — B** *(§8.6.3 · cas 32)* « Cascader » = **décliner** l'appétit en tolérances puis en limites opérationnelles.

**Q94 — D** *(§8.6.3 · cas 33)* Appétit et stratégie sont **indissociables** : l'appétit borne la stratégie et alimente sa planification.

**Q95 — B** *(§8.6.3)* L'appétit pour le risque **alimente la planification stratégique**.

**Q96 — C** *(§8.6.4)* La culture de risque = **valeurs, attitudes et comportements partagés** face au risque.

**Q97 — A** *(§8.6.4 · cas 24)* Un pilier de la culture est le **ton donné par le sommet**.

**Q98 — B** *(§8.6.4 · cas 34)* Une culture saine suppose des incitations **alignées sur le rendement ajusté du risque**.

**Q99 — C** *(§8.6.1 · cas 29)* Confondre capacité et appétit **supprime le coussin de sécurité** de la firme.

**Q100 — B** *(§8.6.2)* Un bon énoncé d'appétit combine **cibles qualitatives et quantitatives**.

**Q101 — D** *(§8.6.3 · cas 32)* Une cascade rompue conduit à des **métiers naviguant à vue**, sans limites cohérentes.

**Q102 — B** *(§8.6.3)* Un appétit **trop étroit bride la création de valeur**.

**Q103 — A** *(§8.6.4)* Une culture défaillante **vide de sa substance** le meilleur dispositif formel.

**Q104 — C** *(§8.6.4 · cas 34)* Une rémunération sur rendement brut crée une **incitation à la prise de risque excessive**.

**Q105 — B** *(§8.6.1)* Classement correct : **limites ≤ tolérance ≤ appétit ≤ capacité**.

**Q106 — A** *(§8.7.1)* La perte attendue (EL) se couvre par le **provisionnement et la tarification**.

**Q107 — B** *(§8.7.1)* La perte inattendue (UL) est l'**écart entre une perte extrême et la moyenne**, absorbé par le capital.

**Q108 — C** *(§8.7.1 · cas 48)* Capital économique : EC = VaR(α) − EL.

**Q109 — B** *(§8.7.1)* Le capital couvre la perte **inattendue**, pas la perte attendue déjà provisionnée.

**Q110 — D** *(§8.7.1 · cas 48)* EC = 250 − 40 = **210** M€.

**Q111 — B** *(§8.7.2)* Le capital agrégé est **sous-additif** : inférieur à la somme des capitaux autonomes.

**Q112 — C** *(§8.7.2)* Approximation : EC(agg) = √(Σᵢ Σⱼ ρᵢⱼ ECᵢ ECⱼ).

**Q113 — A** *(§8.7.2 · cas 47)* Bénéfice de diversification : Σᵢ ECᵢ(standalone) − EC(agg).

**Q114 — B** *(§8.7.2 · cas 47)* Le ratio de diversification est **inférieur à 1** (ex. 120/150 = 0,80).

**Q115 — C** *(§8.7.3)* Le RAROC rapporte un **résultat ajusté du risque** au **capital économique** consommé.

**Q116 — B** *(§8.7.3 · cas 49)* Règle : une activité crée de la valeur si son **RAROC dépasse le coût du capital**.

**Q117 — A** *(§8.7.3)* Le RAROC permet de **comparer des activités hétérogènes** sur une base homogène ajustée du risque.

**Q118 — B** *(§8.7.3 · cas 49)* RAROC 15% > coût du capital 12% → l'activité **crée de la valeur**.

**Q119 — C** *(§8.7.3 · cas 50)* RAROC 9% < hurdle 12% → l'activité **détruit de la valeur** et doit être réduite, repricée ou cédée.

**Q120 — B** *(§8.7.3)* Le RAROC oriente le capital selon la **contribution au risque et le rendement ajusté**.

## D.4 Corrigé — Q121 à Q150 (scénarios, stress testing, tendances futures)

**Q121 — B** *(§8.8.1 · cas 35)* L'analyse de sensibilité fait varier **un seul facteur** à la fois.

**Q122 — C** *(§8.8.1)* L'analyse de scénarios fait varier **plusieurs facteurs simultanément** de façon cohérente.

**Q123 — A** *(§8.8.2 · cas 36)* Un scénario historique **rejoue un épisode réellement survenu** (1987, 2008, 2020).

**Q124 — B** *(§8.8.2)* Limite des scénarios historiques : l'hypothèse que le **futur ressemblera au passé**.

**Q125 — C** *(§8.8.2 · cas 37)* Un scénario hypothétique construit un enchaînement **plausible mais inédit**.

**Q126 — B** *(§8.8.2)* Les scénarios hypothétiques captent les **risques émergents** que l'histoire n'a pas produits.

**Q127 — D** *(§8.8.3 · cas 38)* Le stress testing applique des scénarios **extrêmes mais plausibles**.

**Q128 — B** *(§8.8.3 · cas 38)* Le stress testing **explore la queue** de distribution là où la VaR est muette.

**Q129 — C** *(§8.8.4 · cas 39)* Le reverse stress testing **part du résultat redouté** (insolvabilité) et remonte aux scénarios.

**Q130 — B** *(§8.8.4)* Il **remonte aux scénarios et combinaisons de chocs** capables de produire le résultat redouté.

**Q131 — D** *(§8.8.4 · cas 40)* Sa vertu : **révéler vulnérabilités et hypothèses de viabilité cachées**.

**Q132 — A** *(§8.8.5)* L'analyse de scénarios est **prospective** (forward-looking).

**Q133 — B** *(§8.8.5)* Limite : la **subjectivité** du choix des scénarios.

**Q134 — C** *(§8.8.5)* Autre limite : la **difficulté d'attacher des probabilités** aux scénarios.

**Q135 — B** *(§8.8.5)* La VaR résume le risque en un **quantile** fondé sur des données historiques, supposant souvent la normalité.

**Q136 — D** *(§8.8.5 · cas 41)* VaR et scénarios sont **complémentaires**, répondant à des questions différentes.

**Q137 — B** *(§8.8.5)* L'analyse de scénarios **ne dépend pas de l'hypothèse de normalité**.

**Q138 — A** *(§8.8.6 · cas 42)* Usage ERM : **tester si le portefeuille reste dans les limites de l'appétit**.

**Q139 — B** *(§8.8.6)* Usage : **planification du capital et de la liquidité**.

**Q140 — C** *(§8.8.6)* Usages : **préparation de la gestion de crise** et **challenge des modèles**.

**Q141 — A** *(§8.9.1)* La FinTech agit par **désintermédiation** (paiements, prêt P2P, néobanques).

**Q142 — B** *(§8.9.2 · cas 43)* Risques de l'IA/ML : **opacité, biais et risque de modèle**.

**Q143 — C** *(§8.9.2 · cas 44)* Un modèle ML **hérite des biais** présents dans ses données d'entraînement.

**Q144 — B** *(§8.9.3 · cas 45)* Le cyber-risque peut être **systémique** via des dépendances communes (prestataires mutualisés).

**Q145 — D** *(§8.9.4 · cas 46)* Le risque climatique se décline en risque **physique** et risque de **transition**.

**Q146 — B** *(§8.9.5)* Crypto/DeFi : **volatilité élevée**, risques de conservation, **cadre réglementaire en construction**.

**Q147 — A** *(§8.9.5)* La RegTech **automatise la conformité** (KYC, reporting, surveillance des transactions).

**Q148 — B** *(§8.9.2)* L'IA et le big data replacent au premier plan la **gouvernance et la qualité des données** (BCBS 239).

**Q149 — C** *(§8.9.6)* Dénominateur commun : **plus de vitesse, d'interconnexion et de risque de modèle**.

**Q150 — B** *(§8.9.6)* Ces tendances appellent une fonction risque **plus agile, technologique et centrée sur la donnée**.

---

**Fin du Chapitre 8 — Enterprise Risk Management & Future Trends · Cours · 50 cas · 150 QCM commentés**

