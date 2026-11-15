# Chapitre 7 — Data Aggregation & Risk Reporting (BCBS 239)

### Agrégation des données de risque et reporting

**FRM Part I · Foundations — Approfondissement complet**

*Cours structuré · 50 études de cas · 150 QCM commentés. Questions originales de style examen ; GARP ne publiant aucune annale, aucune question n'est attribuée à une session passée.*





# Partie A — Cours approfondi

## 7.1 Genèse : l'échec d'agrégation de 2007-2009

La norme **BCBS 239** — de son nom officiel *Principles for effective risk data aggregation and risk reporting* — est publiée par le **Comité de Bâle sur le contrôle bancaire** (Basel Committee on Banking Supervision) en **janvier 2013**. Elle constitue le document n°**239** de la série du Comité, d'où son appellation usuelle.

Son origine est directement enracinée dans la **crise financière de 2007-2009**. Lors de l'effondrement, de nombreuses banques d'importance systémique se sont révélées **incapables d'agréger rapidement leurs expositions au risque** à l'échelle du groupe. Interrogées par leurs superviseurs sur leur exposition consolidée à une contrepartie défaillante (Lehman Brothers en tête), plusieurs grandes institutions ont mis **plusieurs jours, voire plusieurs semaines**, à produire un chiffre fiable — parfois jamais un chiffre unique et réconcilié.


> **Le constat fondateur**
>
> La crise a démontré qu'**une banque ne peut gérer que ce qu'elle peut voir**. Des systèmes d'information **fragmentés**, un recours massif à des **traitements manuels** (feuilles de calcul, rapprochements ad hoc), l'absence de **source de référence unique** (*golden source*) et des **taxonomies de données incohérentes** entre entités ont empêché la direction et les régulateurs de mesurer le risque en temps utile.


Les causes techniques récurrentes identifiées :

- **Silos de données** : chaque ligne métier, filiale ou géographie conservait ses propres référentiels, sans identifiants communs de contrepartie, d'instrument ou de produit.
- **Réconciliations manuelles** : l'agrégation reposait sur des extractions retravaillées à la main, sources d'erreurs et impossibles à produire sous stress.
- **Absence de traçabilité (*data lineage*)** : impossible de remonter d'un chiffre agrégé jusqu'à la donnée élémentaire et à sa transformation.
- **Architecture non résiliente** : les systèmes conçus pour le reporting réglementaire de fin de période s'effondraient face à une demande *ad hoc* urgente.

La réponse de Bâle n'est donc **pas une exigence de capital** : c'est une exigence de **capacité opérationnelle et de gouvernance des données**. L'idée centrale est qu'une bonne gestion du risque présuppose une **infrastructure d'information** capable de délivrer, vite et juste, la bonne mesure à la bonne personne — y compris (et surtout) en période de tension.


> **⚑ Point d'examen —** BCBS 239 répond à **l'incapacité des banques à agréger rapidement leurs expositions** pendant la crise — **pas** à un excès de capital, ni à une sur-diversification, ni à un excès d'audit interne. C'est une norme de **qualité des données et de reporting**, non une norme de fonds propres ou de liquidité.


## 7.2 Champ d'application, proportionnalité et calendrier

### 7.2.1 À qui s'applique la norme

BCBS 239 vise **en premier lieu les banques d'importance systémique mondiale** (*Global Systemically Important Banks*, **G-SIB**). Le Comité **recommande fortement** que les superviseurs nationaux appliquent également les principes aux **banques d'importance systémique domestique** (*Domestic SIB*, **D-SIB**), avec un calendrier laissé à leur discrétion (typiquement trois ans après désignation).

Ne sont donc **pas** la cible première : les petites banques locales, les assureurs (relevant de Solvabilité II en Europe), les fonds spéculatifs. La logique est **systémique** : on impose la discipline la plus lourde là où la défaillance d'agrégation menace la stabilité financière globale.

### 7.2.2 Principe de proportionnalité

Les principes s'appliquent **selon la nature, la taille et la complexité** de l'établissement. Une institution plus grande, plus complexe et plus interconnectée doit satisfaire des attentes plus strictes. La proportionnalité ne dispense jamais des exigences fondamentales de qualité de données ; elle module l'exigence de moyens.

### 7.2.3 Calendrier

Les G-SIB désignées en 2011-2012 devaient se **conformer pleinement pour janvier 2016**, soit **trois ans** après la publication de la norme. Les revues de suivi du Comité (*Progress reports*, publiés régulièrement à partir de 2013) ont montré une conformité **lente et partielle** : au terme de l'échéance, **aucune G-SIB n'était pleinement conforme à l'ensemble des 14 principes**, la gouvernance et l'architecture informatique (thème I) restant les plus difficiles à satisfaire. Cette lenteur est elle-même un point de connaissance : elle illustre l'ampleur de la dette technique héritée des silos.


> **⚑ Point d'examen —** Cible **prioritaire** : les **G-SIB** ; extension **recommandée** aux **D-SIB**. Conformité attendue **trois ans** après publication (**janvier 2016** pour les G-SIB). Application **proportionnée** à la taille et à la complexité. Retenir que la conformité observée est restée **lente et incomplète**.


## 7.3 Architecture d'ensemble : 14 principes, 4 thèmes

BCBS 239 énonce **14 principes** regroupés en **quatre thèmes**. Cette structure est le squelette à mémoriser absolument.


> **Les 4 thèmes et les 14 principes**
>
> **Thème I — Gouvernance et infrastructure globale** (*overarching governance and infrastructure*)  

   P1 · Gouvernance     P2 · Architecture des données et infrastructure informatique  

**Thème II — Capacités d'agrégation des données de risque**  

   P3 · Exactitude et intégrité    P4 · Exhaustivité    P5 · Rapidité    P6 · Adaptabilité  

**Thème III — Pratiques de reporting des risques**  

   P7 · Exactitude    P8 · Exhaustivité (couverture)    P9 · Clarté et utilité    P10 · Fréquence    P11 · Distribution  

**Thème IV — Surveillance prudentielle, outils et coopération**  

   P12 · Revue    P13 · Mesures correctives et prudentielles    P14 · Coopération *home/host*


Deux distinctions structurantes doivent être parfaitement maîtrisées :

**Distinction 1 — Agrégation (thème II) vs. Reporting (thème III).** Le thème II concerne la **fabrication** de la donnée agrégée : est-elle exacte, complète, produite à temps, et adaptable à des demandes nouvelles ? Le thème III concerne la **restitution** : le rapport est-il exact, couvre-t-il tous les risques matériels, est-il clair et utile, produit à la bonne fréquence, et distribué aux bons destinataires ? Une même qualité (l'exactitude) apparaît dans les deux thèmes mais s'applique à des objets différents — la donnée d'un côté, le rapport de l'autre.

**Distinction 2 — Les quatre qualités d'agrégation.** Le cœur opérationnel de la norme tient en **quatre mots** (thème II, P3-P6) :

- **Accuracy / Exactitude et intégrité** (P3) : les données agrégées doivent être **fiables et exactes**, produites de manière **automatisée** autant que possible pour limiter l'erreur humaine, avec des **contrôles de rapprochement** et une traçabilité (*data lineage*).
- **Completeness / Exhaustivité** (P4) : capturer **toutes les expositions matérielles** — toutes les lignes métier, toutes les entités juridiques, tous les types de risque, toutes les zones géographiques — sans angle mort.
- **Timeliness / Rapidité** (P5) : produire l'information **à temps**, avec une capacité renforcée en **période de stress** où l'urgence croît.
- **Adaptability / Adaptabilité** (P6) : répondre à des **requêtes *ad hoc*** et générer des **agrégations de scénarios de stress à la demande**, y compris sur des découpages non prévus à l'avance.


> **⚑ Point d'examen —** Les **quatre qualités d'agrégation** (P3-P6) : **accuracy, completeness, timeliness, adaptability**. Piège classique : **la clarté du reporting n'en fait PAS partie** — elle relève du thème III (reporting). Autre piège : ces capacités doivent tenir **surtout en période de stress**, pas seulement en régime normal.


## 7.4 Thème I — Gouvernance et infrastructure (P1-P2)

### 7.4.1 Principe 1 — Gouvernance

Les dispositifs d'agrégation des données de risque et de reporting doivent être soumis à un **cadre de gouvernance robuste**, cohérent avec les autres principes du Comité en la matière. La **responsabilité ultime incombe au conseil d'administration et à la direction générale** — jamais au seul département informatique, ni à l'audit externe.

Implications concrètes :

- Le conseil et la direction **connaissent les limitations** de leurs capacités d'agrégation (portée, exhaustivité, exceptions, traitements manuels résiduels) et en tiennent compte dans leurs décisions.
- Les capacités d'agrégation sont considérées lors de toute **acquisition, cession ou lancement de nouveau produit** : on ne crée pas un nouveau silo.
- La qualité des données fait l'objet d'un **cadre de gouvernance des données** (*data governance*) avec des **propriétaires de données** (*data owners*) clairement désignés, des politiques de qualité et un dictionnaire/taxonomie communs.
- Le dispositif est **indépendant de tout événement particulier** : il fonctionne en continu, pas seulement à la demande d'un régulateur.

### 7.4.2 Principe 2 — Architecture des données et infrastructure informatique

La banque doit concevoir, construire et maintenir une **architecture de données et une infrastructure informatique** qui **soutiennent pleinement** les capacités d'agrégation et de reporting, **non seulement en temps normal mais aussi en période de stress ou de crise**, tout en respectant les autres principes.

Éléments clés :

- **Taxonomie et architecture intégrées** : identifiants uniques et cohérents pour les contreparties, les instruments et les comptes à l'échelle du groupe.
- **Golden source** : une source de référence unique par type de donnée, plutôt que des copies divergentes.
- **Data lineage automatisé** : capacité à remonter la chaîne de transformation d'une donnée, condition de l'exactitude et de l'auditabilité.
- **Continuité et résilience** : dispositifs de reprise (PRA/PCA) garantissant l'agrégation même en cas d'incident.


> **⚑ Point d'examen —** Thème I = **gouvernance (P1)** + **architecture/IT (P2)**. La **responsabilité ultime** est celle du **conseil et de la direction générale**. L'architecture doit tenir **y compris en stress**. C'est le thème resté le **plus difficile à satisfaire** dans les revues de suivi, car il attaque la dette technique des silos hérités.


## 7.5 Thème II — Capacités d'agrégation des données de risque (P3-P6)

C'est le **noyau technique** de la norme. Les quatre principes définissent les **quatre qualités** attendues de la donnée agrégée.

### 7.5.1 Principe 3 — Exactitude et intégrité (*Accuracy and Integrity*)

Les données de risque agrégées doivent être **exactes et fiables**. Attentes :

- Agrégation **essentiellement automatisée** pour **minimiser la probabilité d'erreur**.
- **Réconciliation** des données de risque avec les sources comptables et autres, afin de garantir la cohérence.
- **Data lineage** documenté ; contrôles de qualité aux points d'entrée et de transformation.
- Les **ajustements manuels** (*manual workarounds*) doivent être **explicités, documentés et exceptionnels** — leur multiplication est précisément le symptôme que la norme combat.
- Une **granularité suffisante** : la donnée doit être disponible au niveau élémentaire pour être ré-agrégée selon différents axes.

### 7.5.2 Principe 4 — Exhaustivité (*Completeness*)

La banque doit être capable de **capturer et d'agréger toutes les données de risque matérielles** du groupe. Cela couvre **toutes les catégories de risque** (crédit, marché, liquidité, opérationnel...), **toutes les lignes métier**, **toutes les entités juridiques**, **toutes les zones géographiques**.

Le test opérationnel : la banque peut-elle produire, **rapidement et de façon fiable**, son **exposition consolidée à une contrepartie unique** à travers tout le groupe ? L'incapacité à le faire est l'échec emblématique de 2008.

### 7.5.3 Principe 5 — Rapidité (*Timeliness*)

La banque doit générer des **données de risque agrégées à temps** (*in a timely manner*), tout en respectant exactitude et exhaustivité. La **rapidité requise dépend de la nature et de la volatilité du risque**, mais aussi du **caractère critique de la décision** : plus le risque est volatil et la décision urgente, plus la donnée doit être fraîche.

Exigence spécifique **en période de stress** : la banque doit pouvoir produire ses agrégats **plus vite** lorsque la situation l'exige. Un système qui ne délivre qu'un reporting mensuel figé échoue à ce principe.

### 7.5.4 Principe 6 — Adaptabilité (*Adaptability*)

La banque doit pouvoir **générer des données de risque agrégées pour répondre à un large éventail de demandes**, y compris :

- des **requêtes *ad hoc*** de gestion des risques (nouveaux découpages, nouvelles contreparties, nouveaux facteurs) ;
- des demandes du **superviseur** ;
- des **agrégations de scénarios de stress** définis à la demande ;
- des besoins liés à des **crises émergentes** ou à des changements réglementaires.

L'adaptabilité est la **flexibilité** du système : capacité à produire ce qui n'avait pas été anticipé, sans reconstruction lourde.


> **⚑ Point d'examen —** Ne pas confondre les quatre : **accuracy** = juste et automatisé (réconcilié) ; **completeness** = toutes les expositions matérielles, sans angle mort ; **timeliness** = à temps, plus vite en stress ; **adaptability** = requêtes *ad hoc* et stress à la demande. Une banque incapable de produire vite son exposition **groupe** à une contrepartie viole d'abord **completeness** et **timeliness**.


## 7.6 Thème III — Pratiques de reporting des risques (P7-P11)

Le reporting est la **restitution** de l'information agrégée aux décideurs. Cinq principes.

### 7.6.1 Principe 7 — Exactitude (*Accuracy*)

Les rapports de risque doivent **transmettre l'information de manière exacte et précise**. Ils sont **réconciliés et validés**. Le niveau d'exactitude requis est fonction de l'usage : les rapports **critiques** (décisions du conseil, gestion de crise) exigent le plus haut degré de fiabilité. Les attentes sur les données sous-jacentes rejoignent le P3.

### 7.6.2 Principe 8 — Exhaustivité / couverture (*Comprehensiveness*)

Les rapports doivent **couvrir tous les domaines de risque matériels** de l'organisation. La profondeur et la portée sont cohérentes avec la taille et la complexité des activités et le profil de risque. La couverture inclut, selon les cas, l'exposition et la concentration de crédit, la liquidité, le risque de marché, l'opérationnel, et une vue **prospective** (stress, appétit).

### 7.6.3 Principe 9 — Clarté et utilité (*Clarity and Usefulness*)

Les rapports doivent être **clairs, concis et utiles** à leurs destinataires. Ils communiquent l'information de façon **compréhensible**, calibrée pour le destinataire (le conseil n'a pas les mêmes besoins qu'un responsable de desk), et **support de la décision** : l'excès d'information tue l'information. Un bon rapport met en évidence les **risques émergents**, les dépassements de limites et les points d'attention.

### 7.6.4 Principe 10 — Fréquence (*Frequency*)

La **fréquence** des rapports est déterminée par leurs destinataires, la nature des risques et la rapidité avec laquelle ils évoluent. Point crucial : **la fréquence doit augmenter en période de stress** — un rapport mensuel en temps normal peut devenir quotidien, voire intra-journalier, en crise. Le système doit techniquement le permettre (lien avec P5/P6).

### 7.6.5 Principe 11 — Distribution (*Distribution*)

Les rapports doivent être **distribués aux bonnes parties prenantes**, dans les délais, tout en **préservant la confidentialité**. Ni rétention (priver le conseil d'une information), ni diffusion incontrôlée : la bonne information, au bon destinataire, au bon moment, de façon sécurisée.


> **⚑ Point d'examen —** Reporting (thème III) : **exactitude, couverture, clarté/utilité, fréquence, distribution**. Le distracteur type d'un QCM oppose une qualité de **donnée** (accuracy, completeness — thème II) à une qualité de **rapport** (clarté, distribution — thème III). **Clarté** et **distribution** sont des principes de **reporting**. La **fréquence augmente en stress**.


## 7.7 Thème IV — Surveillance prudentielle, outils et coopération (P12-P14)

### 7.7.1 Principe 12 — Revue (*Review*)

Les superviseurs **examinent et évaluent périodiquement** la conformité de la banque aux onze principes précédents. Ils testent notamment la capacité à produire des agrégations **sous stress** et **ad hoc**.

### 7.7.2 Principe 13 — Mesures correctives et outils prudentiels (*Remedial actions and supervisory measures*)

Les superviseurs disposent d'**outils** pour **exiger des mesures correctives** efficaces et en temps voulu en cas de déficiences. Ils peuvent, selon les cas, fixer des **délais de remédiation**, imposer des **contraintes** (limites d'activité, exigences additionnelles) tant que les faiblesses persistent.

### 7.7.3 Principe 14 — Coopération *home/host*

Les superviseurs **coopèrent au niveau transfrontalier** (autorité du pays d'origine — *home* — et des pays d'accueil — *host*) pour une surveillance cohérente des groupes internationaux. Cette coordination évite les angles morts entre juridictions.


> **⚑ Point d'examen —** Les **trois derniers principes** portent sur le **superviseur** : **revue (P12)**, **mesures correctives (P13)**, **coopération home/host (P14)**. La conformité BCBS 239 est évaluée par les **superviseurs** (ni actionnaires, ni clients, ni agences de notation). « Home/host » = coordination **transfrontalière** entre autorités, pas hébergement ni centre de données.


## 7.8 Mise en œuvre, data lineage et interactions

### 7.8.1 Ce que la conformité exige en pratique

Passer d'un existant en silos à une conformité BCBS 239 suppose typiquement :

- l'établissement d'une **taxonomie de données** et d'**identifiants uniques** à l'échelle du groupe ;
- la constitution de **golden sources** et la réduction des copies divergentes ;
- l'**automatisation** des chaînes d'agrégation et la **réduction des workarounds manuels** ;
- un **data lineage outillé** (traçabilité de bout en bout), condition de l'exactitude auditable ;
- un **cadre de gouvernance des données** avec propriétaires, politiques et contrôles de qualité ;
- des **tests réguliers sous stress et ad hoc** pour prouver l'adaptabilité et la rapidité.

### 7.8.2 Data lineage, golden source, taxonomie

Trois notions à distinguer nettement :

- **Golden source** : *la* source autorisée et unique d'une donnée de référence (contrepartie, instrument). Elle supprime la divergence entre systèmes.
- **Data lineage** : la **cartographie de la trajectoire** d'une donnée, de l'origine jusqu'au chiffre agrégé, incluant chaque transformation. Elle rend l'exactitude **traçable et auditable**.
- **Taxonomie de données** : le **vocabulaire commun** (dictionnaire, classifications, hiérarchies) garantissant que « exposition », « contrepartie » ou « défaut » signifient la même chose partout.

### 7.8.3 Interactions avec le reste du dispositif prudentiel

BCBS 239 est un **socle transversal** : sans agrégation fiable, les autres exercices sont fragilisés. Elle conditionne notamment la crédibilité des **stress tests** (dont la qualité dépend de la capacité à agréger sous scénario), la fiabilité du **reporting réglementaire**, l'efficacité du **cadre d'appétit pour le risque** (RAF) et la gestion des **grands risques / concentrations**. À l'inverse, elle ne fixe **aucune exigence de fonds propres, de liquidité, ni de norme comptable** : c'est une exigence de **capacité**, transversale à toutes les catégories de risque.

### 7.8.4 Repères quantitatifs de qualité des données

Bien que BCBS 239 soit une norme **qualitative**, sa mise en œuvre s'appuie sur des **indicateurs mesurables** (*data quality KPIs*) que le contrôle interne suit dans le temps. Quelques mesures usuelles, définies ici à titre pédagogique :


> Taux de complétude = (champs renseignés et valides) ÷ (champs attendus)



> Taux d'exactitude = 1 − (enregistrements en écart après réconciliation) ÷ (enregistrements totaux)



> Taux de couverture d'agrégation = (expositions matérielles capturées) ÷ (expositions matérielles totales)



> Part de traitement automatisé (STP) = (volumes traités sans intervention manuelle) ÷ (volumes totaux)



> Latence de reporting = t(disponibilité du rapport) − t(arrêté des données)


Ces ratios traduisent les qualités de la norme en cibles suivies : une **complétude** proche de 100 %, un **STP** élevé (donc peu de workarounds), une **latence** faible et **réductible en stress**. Ils servent aussi de base aux **tableaux de bord de gouvernance des données** présentés au conseil (bouclant avec P1).


> **⚑ Point d'examen —** Retenir la chaîne logique : **golden source** (unicité)  **taxonomie** (vocabulaire commun)  **automatisation** (moins de workarounds)  **data lineage** (traçabilité)  **exactitude auditable**. BCBS 239 ne touche **ni au capital, ni à la liquidité, ni à la comptabilité** : c'est une exigence de **capacité d'agrégation et de reporting**, transversale.




# Partie B — 50 études de cas

*Les cas ci-dessous illustrent l'application des principes BCBS 239. Les situations chiffrées sont des exemples pédagogiques construits pour l'entraînement ; les situations qualitatives synthétisent des enseignements publics de la crise et des revues de suivi du Comité, sans reproduire de données confidentielles d'aucune institution.*

## B.1 Genèse, champ d'application et calendrier (cas 1-10)

**Cas 1 — L'exposition introuvable à Lehman.** À l'automne 2008, plusieurs grandes banques sont incapables de chiffrer en une journée leur exposition consolidée à Lehman à travers desks, filiales et géographies. *Enseignement* : échec emblématique d'**exhaustivité (P4)** et de **rapidité (P5)** ; c'est ce cas type qui motive toute la norme.

**Cas 2 — Le chiffre qui change selon le système.** Une direction des risques reçoit trois estimations différentes de la même exposition selon la source interrogée (front, comptabilité, risque). *Enseignement* : absence de **golden source** et de **réconciliation** — cœur du **P3 (exactitude/intégrité)**.

**Cas 3 — Pourquoi les G-SIB d'abord.** Un superviseur explique qu'il concentre l'exigence sur les groupes systémiques mondiaux. *Enseignement* : la norme est **systémique** ; cible prioritaire = **G-SIB**, extension recommandée aux **D-SIB**.

**Cas 4 — L'assureur qui demande à être exclu.** Un assureur estime que BCBS 239 ne le vise pas. *Enseignement* : correct — la norme bâloise vise les **banques** (G-SIB/D-SIB) ; les assureurs relèvent d'autres cadres (Solvabilité II en Europe).

**Cas 5 — Trois ans pour se conformer.** Une G-SIB désignée en 2012 planifie sa conformité. *Enseignement* : échéance **janvier 2016**, soit **trois ans** après la publication de 2013.

**Cas 6 — Le *progress report* décevant.** Les revues de suivi du Comité montrent qu'aucune G-SIB n'est pleinement conforme à l'échéance. *Enseignement* : la **dette technique** des silos rend la conformité **lente** ; le thème I (gouvernance/IT) est le plus difficile.

**Cas 7 — La proportionnalité mal comprise.** Une banque moyenne invoque la proportionnalité pour négliger la qualité des données. *Enseignement* : la proportionnalité **module les moyens**, elle **ne dispense pas** des exigences fondamentales de qualité.

**Cas 8 — Nouvelle acquisition, nouveau silo.** Une banque intègre une cible sans harmoniser ses référentiels. *Enseignement* : viole l'esprit du **P1** — les capacités d'agrégation doivent être considérées lors de toute **acquisition**.

**Cas 9 — Le régulateur teste sous stress.** Un superviseur demande une agrégation *ad hoc* sur un périmètre non standard, en 48 h. *Enseignement* : test direct de l'**adaptabilité (P6)** et de la **rapidité (P5)**.

**Cas 10 — « Ce n'est pas une exigence de capital ».** Un dirigeant confond BCBS 239 avec une norme de fonds propres. *Enseignement* : BCBS 239 est une norme de **capacité (données + reporting)**, **sans** exigence de capital, de liquidité ni de comptabilité.

## B.2 Thème I — Gouvernance et infrastructure (cas 11-20)

**Cas 11 — Qui est responsable ?** Après un incident d'agrégation, la direction attribue la faute à l'IT. *Enseignement* : la **responsabilité ultime** est celle du **conseil et de la direction générale** (P1), pas du seul département informatique.

**Cas 12 — Le conseil ignore les limites.** Un conseil approuve une décision sur des chiffres dont il ignore qu'ils excluent une entité. *Enseignement* : le P1 impose que le conseil **connaisse les limitations** de ses capacités d'agrégation.

**Cas 13 — Data owners absents.** Personne n'est responsable de la qualité d'un référentiel clé. *Enseignement* : le cadre de **gouvernance des données** exige des **propriétaires de données** désignés.

**Cas 14 — Le dictionnaire manquant.** « Défaut » n'a pas la même définition dans deux filiales. *Enseignement* : nécessité d'une **taxonomie** commune (P2) ; sans vocabulaire partagé, pas d'agrégation fiable.

**Cas 15 — L'architecture qui craque en stress.** Un système conçu pour le reporting mensuel s'effondre face à une demande urgente. *Enseignement* : le P2 exige une infrastructure tenant **aussi en période de stress**.

**Cas 16 — Golden source vs. copies.** Cinq systèmes détiennent cinq versions du référentiel contreparties. *Enseignement* : établir une **golden source** unique supprime la divergence (P2/P3).

**Cas 17 — Le lineage introuvable.** Impossible de remonter d'un agrégat à la donnée source. *Enseignement* : absence de **data lineage** — condition de l'exactitude auditable (P2/P3).

**Cas 18 — PRA/PCA des données.** Un incident système empêche toute agrégation pendant 24 h. *Enseignement* : le P2 implique **résilience et continuité** de l'infrastructure d'agrégation.

**Cas 19 — Projet de remédiation pluriannuel.** Une banque lance un programme de convergence des référentiels sur trois ans. *Enseignement* : illustre l'ampleur de la **transformation** requise par le thème I.

**Cas 20 — Gouvernance indépendante des événements.** Une banque n'agrège correctement que lorsque le régulateur le demande. *Enseignement* : le dispositif doit fonctionner **en continu**, indépendamment de tout événement particulier.

## B.3 Thème II — Capacités d'agrégation (cas 21-30)

**Cas 21 — Trop de workarounds.** L'agrégation dépend de dizaines de feuilles de calcul retravaillées à la main. *Enseignement* : viole le **P3** ; les workarounds manuels doivent être **exceptionnels et documentés**, non la norme.

**Cas 22 — Réconciliation risque/compta.** Un écart persistant entre données de risque et comptabilité n'est pas expliqué. *Enseignement* : le **P3** exige la **réconciliation** avec les sources comptables.

**Cas 23 — L'entité oubliée.** Une filiale étrangère n'est pas incluse dans l'exposition groupe. *Enseignement* : viole l'**exhaustivité (P4)** — toutes les **entités juridiques** doivent être capturées.

**Cas 24 — Le type de risque manquant.** Le risque opérationnel n'est pas agrégé au niveau groupe. *Enseignement* : **P4** — **toutes les catégories de risque matérielles** doivent être couvertes.

**Cas 25 — Trop lent pour décider.** L'exposition n'est disponible que J+10, inutile pour piloter une crise. *Enseignement* : viole la **rapidité (P5)** ; la fraîcheur requise dépend de la **volatilité** et de la **criticité**.

**Cas 26 — Plus vite en stress.** En période calme un reporting hebdomadaire suffit ; en crise il faut du quotidien. *Enseignement* : le **P5** impose une capacité d'accélération **en stress**.

**Cas 27 — La requête ad hoc impossible.** La direction veut l'exposition à un secteur non prévu dans les axes standards ; le système ne sait pas la produire. *Enseignement* : viole l'**adaptabilité (P6)**.

**Cas 28 — Stress à la demande.** Le superviseur définit un scénario inédit et demande l'agrégation correspondante. *Enseignement* : **P6** — générer des **agrégations de stress à la demande**.

**Cas 29 — Granularité insuffisante.** Les données ne sont stockées qu'agrégées, empêchant toute ré-agrégation par un autre axe. *Enseignement* : le **P3/P6** suppose une **granularité élémentaire** suffisante.

**Cas 30 — Le test des quatre qualités.** Une banque veut auto-évaluer son agrégation. *Enseignement* : checklist — **exacte (P3)**, **complète (P4)**, **à temps (P5)**, **adaptable (P6)** ; les quatre simultanément.

## B.4 Thème III (reporting) et thème IV (surveillance) (cas 31-40)

**Cas 31 — Le rapport joliment faux.** Un rapport clair mais fondé sur des données non réconciliées. *Enseignement* : viole le **P7 (exactitude du reporting)** — un beau rapport faux reste faux.

**Cas 32 — La couverture partielle.** Le rapport au conseil omet le risque de liquidité. *Enseignement* : viole le **P8 (couverture)** — tous les domaines de risque **matériels**.

**Cas 33 — La noyade informationnelle.** Un rapport de 200 pages où l'essentiel est illisible. *Enseignement* : viole le **P9 (clarté/utilité)** — l'excès d'information nuit à la décision.

**Cas 34 — Le rapport calibré pour le conseil.** On distingue une synthèse pour le conseil et un détail pour les desks. *Enseignement* : bonne application du **P9** — adapter au **destinataire**.

**Cas 35 — Fréquence figée.** Le reporting reste mensuel même en pleine tempête de marché. *Enseignement* : viole le **P10** — la fréquence doit **augmenter en stress**.

**Cas 36 — Information retenue.** Un responsable ne transmet pas un dépassement de limite au conseil. *Enseignement* : viole le **P11 (distribution)** — pas de rétention ; la bonne info au bon destinataire.

**Cas 37 — Diffusion non maîtrisée.** Un rapport sensible circule hors du cercle autorisé. *Enseignement* : le **P11** exige aussi la **confidentialité** dans la distribution.

**Cas 38 — La revue prudentielle.** Le superviseur évalue périodiquement la conformité aux 11 premiers principes, y compris sous stress. *Enseignement* : **P12 (revue)**.

**Cas 39 — La mesure corrective.** Face à des déficiences, le superviseur impose un délai de remédiation et des contraintes temporaires. *Enseignement* : **P13 (mesures correctives et outils prudentiels)**.

**Cas 40 — Coordination home/host.** Autorités d'origine et d'accueil se coordonnent sur un groupe transfrontalier. *Enseignement* : **P14 (coopération home/host)** — coordination transfrontalière, pas hébergement.

## B.5 Cas chiffrés — indicateurs de qualité des données (cas 41-50)

**Cas 41 — Taux de complétude.** Sur 500 000 champs attendus, 486 000 sont renseignés et valides. Complétude = 486 000/500 000 = **97,2 %**. *Enseignement* : mesure directe du **P4** ; l'objectif est proche de 100 % sur les champs critiques.

**Cas 42 — Taux d'exactitude après réconciliation.** Sur 1 200 000 enregistrements, 3 600 restent en écart. Exactitude = 1 - 3 600/1 200 000 = 1 - 0,003 = **99,7 %**. *Enseignement* : proxy du **P3** ; les écarts résiduels doivent être **expliqués**.

**Cas 43 — Couverture d'agrégation.** Expositions matérielles capturées : 58,4 Md€ sur 60,0 Md€. Couverture = 58,4/60,0 = **97,3 %**. *Enseignement* : les 1,6 Md€ non capturés forment l'**angle mort** à traiter (P4).

**Cas 44 — Part de traitement automatisé (STP).** 820 000 volumes traités sans intervention sur 1 000 000. STP = 820 000/1 000 000 = **82 %** ; **18 %** restent en workaround manuel. *Enseignement* : plus le STP est bas, plus le risque d'erreur (P3) est élevé ; cible = maximiser le STP.

**Cas 45 — Latence de reporting en temps normal.** Arrêté des données à 18 h 00 J, rapport disponible à 09 h 00 J+1. Latence = **15 heures**. *Enseignement* : mesure du **P5/P10** ; à comparer à la latence exigée en stress.

**Cas 46 — Réduction de latence en stress.** En crise, la même chaîne doit livrer en 3 h au lieu de 15 h. Réduction requise = (15-3)/15 = **80 %**. *Enseignement* : le **P5** impose une capacité d'**accélération** ; sans automatisation, l'objectif est inatteignable.

**Cas 47 — Taux d'écart de réconciliation.** Risque : 1 000 M€ ; comptabilité : 1 004 M€. Écart relatif = 4/1 000 = **0,4 %**. *Enseignement* : au-delà d'un seuil de tolérance, l'écart déclenche investigation (P3/P7).

**Cas 48 — Concentration contrepartie enfin visible.** Après remédiation, la banque produit son exposition groupe à une contrepartie en **2 heures** (contre plusieurs jours auparavant). *Enseignement* : progrès conjoint sur **P4, P5, P6** ; c'est l'objectif fondateur atteint.

**Cas 49 — Score composite de qualité.** Complétude 97 %, exactitude 99 %, couverture 96 %, STP 85 % ; moyenne simple = (97+99+96+85)/4 = **94,25 %**. *Enseignement* : les KPI alimentent un **tableau de bord de gouvernance des données** présenté au conseil (boucle avec P1).

**Cas 50 — Coût du workaround manuel.** Une banque estime que chaque point de STP gagné réduit de 12 incidents/an les erreurs d'agrégation. Passer de 82 % à 90 % de STP ⇒ 8 × 12 = **96** incidents évités/an. *Enseignement* : quantifie le lien **automatisation  exactitude (P3)** et justifie l'investissement en infrastructure (P2).



# Partie C — 150 QCM

*Questions originales de style examen. GARP ne publiant aucune annale, aucune question n'est attribuée à une session passée. Corrigé commenté en Partie D, avec renvois aux sections (§) et aux cas.*

## C.1 Genèse et contexte (Q1-Q20)


**Q1.** BCBS 239 a été émise principalement en réponse à —

- **(A)** des banques détenant trop de capital
- **(B)** l'incapacité des banques à agréger rapidement leurs expositions au risque durant la crise
- **(C)** une sur-diversification des portefeuilles
- **(D)** un recours excessif à l'audit interne
**Q2.** Le nom officiel de BCBS 239 est —

- **(A)** Principles for effective risk data aggregation and risk reporting
- **(B)** Fundamental Review of the Trading Book
- **(C)** Principles for Sound Liquidity Risk Management
- **(D)** Internal Capital Adequacy Assessment Process
**Q3.** BCBS 239 a été publiée par —

- **(A)** la SEC
- **(B)** le Comité de Bâle sur le contrôle bancaire
- **(C)** le FMI
- **(D)** le FASB
**Q4.** L'année de publication de BCBS 239 est —

- **(A)** 2008
- **(B)** 2010
- **(C)** 2013
- **(D)** 2019
**Q5.** L'idée centrale sous-jacente à BCBS 239 est que —

- **(A)** le capital prime sur tout
- **(B)** une banque ne peut gérer que ce qu'elle peut voir
- **(C)** la diversification élimine le risque
- **(D)** l'audit externe suffit à la maîtrise des risques
**Q6.** Un facteur technique majeur de l'échec d'agrégation en 2008 était —

- **(A)** une automatisation excessive
- **(B)** des systèmes d'information fragmentés en silos
- **(C)** une taxonomie de données trop uniforme
- **(D)** une source de référence unique trop rigide
**Q7.** Le recours massif aux traitements manuels pendant la crise a —

- **(A)** amélioré la rapidité d'agrégation
- **(B)** réduit les erreurs
- **(C)** empêché une agrégation fiable et rapide
- **(D)** renforcé la traçabilité
**Q8.** Une « golden source » désigne —

- **(A)** une réserve d'or réglementaire
- **(B)** la source de référence unique et autorisée d'une donnée
- **(C)** un fonds propre de base
- **(D)** un rapport annuel
**Q9.** L'exemple emblématique motivant la norme est l'incapacité à chiffrer rapidement l'exposition consolidée à —

- **(A)** un indice boursier
- **(B)** une contrepartie défaillante comme Lehman Brothers
- **(C)** un souverain noté AAA
- **(D)** la banque centrale
**Q10.** BCBS 239 est avant tout une exigence de —

- **(A)** fonds propres
- **(B)** liquidité
- **(C)** capacité opérationnelle et de gouvernance des données
- **(D)** norme comptable
**Q11.** « BCBS 239 » fait référence à —

- **(A)** une règle de capital
- **(B)** le document n°239 du Comité de Bâle sur l'agrégation et le reporting des données de risque
- **(C)** un ratio de liquidité
- **(D)** une norme comptable
**Q12.** Des systèmes de données fragmentés et manuels sont —

- **(A)** encouragés par BCBS 239
- **(B)** sans rapport avec la norme
- **(C)** précisément ce que BCBS 239 vise à corriger
- **(D)** exigés par la norme
**Q13.** Le « data lineage » désigne —

- **(A)** la lignée managériale d'une banque
- **(B)** la traçabilité de bout en bout d'une donnée, de l'origine à l'agrégat
- **(C)** un ratio de levier
- **(D)** une politique de dividende
**Q14.** Une taxonomie de données commune sert à —

- **(A)** augmenter le capital
- **(B)** garantir un vocabulaire et des définitions partagés à l'échelle du groupe
- **(C)** réduire la fréquence des rapports
- **(D)** externaliser les données
**Q15.** L'objectif global de BCBS 239 est de —

- **(A)** relever les ratios de capital
- **(B)** réduire les coûts
- **(C)** standardiser la comptabilité
- **(D)** renforcer l'agrégation des données de risque et le reporting
**Q16.** La crise a révélé que de nombreuses banques mettaient, pour produire une exposition groupe fiable —

- **(A)** quelques secondes
- **(B)** plusieurs jours voire semaines
- **(C)** exactement une heure
- **(D)** aucun délai
**Q17.** L'absence d'identifiants uniques de contrepartie entre entités empêche surtout —

- **(A)** le calcul du capital
- **(B)** l'agrégation cohérente des expositions
- **(C)** la distribution de dividendes
- **(D)** la tenue de l'audit externe
**Q18.** Le principe « a bank can only manage what it can see » fonde l'accent de BCBS 239 sur —

- **(A)** le capital
- **(B)** la qualité des données et le reporting
- **(C)** le levier
- **(D)** les dividendes
**Q19.** BCBS 239 impose des exigences de nature —

- **(A)** principalement quantitative de fonds propres
- **(B)** principalement qualitative, portant sur les capacités et la gouvernance des données
- **(C)** comptable IFRS
- **(D)** fiscale
**Q20.** La norme s'applique de façon transversale à —

- **(A)** la seule catégorie du risque de marché
- **(B)** toutes les catégories de risque matérielles
- **(C)** au seul risque de crédit
- **(D)** au seul risque opérationnel
## C.2 Champ d'application, calendrier et proportionnalité (Q21-Q40)


**Q21.** BCBS 239 vise en premier lieu —

- **(A)** toutes les petites banques
- **(B)** les assureurs
- **(C)** les banques d'importance systémique mondiale (G-SIB)
- **(D)** les fonds spéculatifs
**Q22.** Le Comité recommande d'étendre les principes également aux —

- **(A)** banques d'importance systémique domestique (D-SIB)
- **(B)** compagnies d'assurance
- **(C)** fonds de pension
- **(D)** banques centrales
**Q23.** Les G-SIB devaient se conformer pleinement pour —

- **(A)** janvier 2014
- **(B)** janvier 2016
- **(C)** janvier 2020
- **(D)** aucune échéance
**Q24.** Le délai de conformité accordé aux G-SIB après publication était de —

- **(A)** un an
- **(B)** trois ans
- **(C)** dix ans
- **(D)** illimité
**Q25.** Le principe de proportionnalité signifie que les exigences s'appliquent selon —

- **(A)** la seule rentabilité
- **(B)** la nature, la taille et la complexité de l'établissement
- **(C)** le pays d'origine uniquement
- **(D)** l'ancienneté de la banque
**Q26.** Les assureurs européens relèvent principalement —

- **(A)** de BCBS 239
- **(B)** de Solvabilité II
- **(C)** de la FRTB
- **(D)** du LCR
**Q27.** Les revues de suivi (progress reports) du Comité ont montré que, à l'échéance —

- **(A)** toutes les G-SIB étaient pleinement conformes
- **(B)** aucune G-SIB n'était pleinement conforme à l'ensemble des principes
- **(C)** la norme avait été abrogée
- **(D)** seules les petites banques étaient concernées
**Q28.** Le thème resté le plus difficile à satisfaire dans les revues de suivi est —

- **(A)** la distribution des rapports
- **(B)** la gouvernance et l'architecture informatique
- **(C)** la fréquence des rapports
- **(D)** la coopération home/host
**Q29.** La proportionnalité —

- **(A)** dispense les petites banques de toute exigence de qualité
- **(B)** module les moyens sans supprimer les exigences fondamentales
- **(C)** s'applique uniquement aux G-SIB
- **(D)** concerne seulement le reporting
**Q30.** Une institution plus grande, complexe et interconnectée doit satisfaire des attentes —

- **(A)** plus faibles
- **(B)** identiques à toutes
- **(C)** plus strictes
- **(D)** nulles
**Q31.** La logique du champ d'application de BCBS 239 est —

- **(A)** fiscale
- **(B)** systémique : discipline la plus lourde là où la défaillance menace la stabilité
- **(C)** commerciale
- **(D)** comptable
**Q32.** Le calendrier d'application aux D-SIB est —

- **(A)** identique et simultané à celui des G-SIB
- **(B)** laissé à la discrétion des superviseurs nationaux
- **(C)** interdit
- **(D)** fixé à 2013
**Q33.** Les hedge funds sont —

- **(A)** la cible première de BCBS 239
- **(B)** hors du champ premier de la norme
- **(C)** soumis aux mêmes délais que les G-SIB
- **(D)** exemptés de toute réglementation
**Q34.** La lenteur de conformité observée illustre surtout —

- **(A)** un manque de volonté isolé
- **(B)** l'ampleur de la dette technique héritée des silos
- **(C)** une erreur de rédaction de la norme
- **(D)** l'absence d'utilité de la norme
**Q35.** Lors d'une acquisition, les capacités d'agrégation doivent être —

- **(A)** ignorées
- **(B)** prises en compte pour éviter la création d'un nouveau silo
- **(C)** réduites
- **(D)** externalisées
**Q36.** BCBS 239 est parfois désignée comme —

- **(A)** la norme sur l'agrégation des données de risque et le reporting
- **(B)** la norme sur le ratio de levier
- **(C)** la norme sur le risque de règlement
- **(D)** la norme sur la titrisation
**Q37.** Le respect de la proportionnalité par une banque moyenne —

- **(A)** l'autorise à négliger la qualité des données
- **(B)** n'exonère pas des exigences de qualité fondamentales
- **(C)** la place hors de tout contrôle
- **(D)** supprime le reporting
**Q38.** Le nombre total de principes énoncés par BCBS 239 est —

- **(A)** 7
- **(B)** 11
- **(C)** 14
- **(D)** 25
**Q39.** Ces principes sont regroupés en —

- **(A)** 2 thèmes
- **(B)** 4 thèmes
- **(C)** 11 thèmes
- **(D)** aucun regroupement
**Q40.** L'application aux D-SIB intervient typiquement, après désignation, dans un délai de l'ordre de —

- **(A)** trois mois
- **(B)** trois ans
- **(C)** trente ans
- **(D)** jamais
## C.3 Thème I — Gouvernance et infrastructure (Q41-Q60)


**Q41.** La responsabilité ultime de l'agrégation des données de risque incombe à —

- **(A)** au seul département informatique
- **(B)** au conseil d'administration et à la direction générale
- **(C)** à l'auditeur externe
- **(D)** aux desks de trading
**Q42.** Le Principe 1 de BCBS 239 concerne —

- **(A)** la gouvernance
- **(B)** la fréquence des rapports
- **(C)** la coopération transfrontalière
- **(D)** le ratio de liquidité
**Q43.** Le Principe 2 concerne —

- **(A)** la distribution des rapports
- **(B)** l'architecture des données et l'infrastructure informatique
- **(C)** la clarté des rapports
- **(D)** la revue prudentielle
**Q44.** Le Principe 2 exige des systèmes qui fonctionnent —

- **(A)** seulement en marchés calmes
- **(B)** entièrement manuellement
- **(C)** y compris en période de stress
- **(D)** seulement en fin d'exercice
**Q45.** Selon le Principe 1, le conseil et la direction doivent —

- **(A)** ignorer les limites de leurs systèmes
- **(B)** connaître les limitations de leurs capacités d'agrégation
- **(C)** déléguer toute responsabilité à l'IT
- **(D)** n'agréger qu'à la demande du régulateur
**Q46.** Un cadre de gouvernance des données efficace suppose —

- **(A)** aucune responsabilité désignée
- **(B)** des propriétaires de données (data owners) clairement identifiés
- **(C)** la suppression de la taxonomie
- **(D)** l'externalisation intégrale
**Q47.** Le dispositif d'agrégation doit fonctionner —

- **(A)** uniquement lors d'un événement de crise
- **(B)** en continu, indépendamment de tout événement particulier
- **(C)** seulement à la clôture annuelle
- **(D)** seulement pour les besoins fiscaux
**Q48.** Une acquisition mal intégrée qui crée un nouveau silo de données —

- **(A)** respecte le Principe 1
- **(B)** contrevient à l'esprit du Principe 1
- **(C)** est encouragée par la norme
- **(D)** relève du seul risque de marché
**Q49.** La « golden source » relève surtout des Principes —

- **(A)** 7 et 8
- **(B)** 2 et 3 (architecture et exactitude)
- **(C)** 12 et 13
- **(D)** aucun
**Q50.** La résilience et la continuité (PRA/PCA) de l'infrastructure d'agrégation relèvent du —

- **(A)** Principe 2
- **(B)** Principe 9
- **(C)** Principe 11
- **(D)** Principe 14
**Q51.** Le thème I regroupe les principes —

- **(A)** 1 et 2
- **(B)** 3 à 6
- **(C)** 7 à 11
- **(D)** 12 à 14
**Q52.** Attribuer un incident d'agrégation « à l'IT seul » est —

- **(A)** conforme au Principe 1
- **(B)** contraire au Principe 1, qui responsabilise le conseil et la direction
- **(C)** exigé par la norme
- **(D)** sans importance
**Q53.** Une taxonomie de données à l'échelle du groupe relève du —

- **(A)** Principe 2
- **(B)** Principe 10
- **(C)** Principe 13
- **(D)** Principe 14
**Q54.** Le conseil qui approuve une décision sur des chiffres excluant à son insu une entité viole —

- **(A)** le Principe 1 (connaissance des limites)
- **(B)** le Principe 10
- **(C)** le Principe 14
- **(D)** aucun principe
**Q55.** L'infrastructure informatique visée par le Principe 2 doit soutenir —

- **(A)** le seul reporting réglementaire annuel
- **(B)** pleinement l'agrégation et le reporting, y compris en stress
- **(C)** uniquement la comptabilité
- **(D)** uniquement le calcul du capital
**Q56.** Le thème I est jugé difficile car il attaque —

- **(A)** un problème purement cosmétique
- **(B)** la dette technique des systèmes hérités en silos
- **(C)** la seule présentation des rapports
- **(D)** la fréquence des rapports
**Q57.** Des définitions divergentes du « défaut » entre filiales relèvent d'un défaut de —

- **(A)** capital
- **(B)** taxonomie commune (Principe 2)
- **(C)** liquidité
- **(D)** levier
**Q58.** La gouvernance des données doit être —

- **(A)** déclenchée uniquement par le régulateur
- **(B)** permanente et intégrée au cadre de gouvernance de la banque
- **(C)** limitée aux périodes calmes
- **(D)** confiée aux clients
**Q59.** Un programme pluriannuel de convergence des référentiels illustre —

- **(A)** l'inutilité du thème I
- **(B)** l'ampleur de la transformation exigée par le thème I
- **(C)** une exigence de capital
- **(D)** une norme comptable
**Q60.** Les deux principes du thème I portent respectivement sur —

- **(A)** gouvernance et architecture/IT
- **(B)** exactitude et exhaustivité
- **(C)** clarté et distribution
- **(D)** revue et coopération
## C.4 Thème II — Capacités d'agrégation des données de risque (Q61-Q95)


**Q61.** Les quatre qualités d'agrégation exigées par BCBS 239 sont —

- **(A)** exactitude, exhaustivité, rapidité, adaptabilité
- **(B)** rentabilité, croissance, part de marché, taille
- **(C)** secret, exclusivité, brièveté, échelle
- **(D)** vitesse, coût, simplicité, échelle
**Q62.** Les systèmes d'agrégation doivent rester efficaces —

- **(A)** seulement en conditions normales
- **(B)** seulement pour le reporting réglementaire
- **(C)** seulement en fin d'exercice
- **(D)** en particulier en période de stress ou de crise
**Q63.** « Adaptabilité » (Principe 6) signifie la capacité à —

- **(A)** réduire le nombre de rapports
- **(B)** générer des requêtes ad hoc et des agrégations de scénarios de stress à la demande
- **(C)** externaliser la gestion des données
- **(D)** éviter la revue prudentielle
**Q64.** Le Principe 3 porte sur —

- **(A)** l'exactitude et l'intégrité
- **(B)** la fréquence
- **(C)** la distribution
- **(D)** la coopération home/host
**Q65.** Le Principe 4 porte sur —

- **(A)** l'exhaustivité (completeness)
- **(B)** la clarté
- **(C)** la revue
- **(D)** le capital
**Q66.** Le Principe 5 porte sur —

- **(A)** la rapidité (timeliness)
- **(B)** la distribution
- **(C)** la gouvernance
- **(D)** la coopération
**Q67.** Le Principe 6 porte sur —

- **(A)** l'adaptabilité
- **(B)** l'exactitude du reporting
- **(C)** la fréquence
- **(D)** la revue prudentielle
**Q68.** L'exactitude (Principe 3) suppose une agrégation —

- **(A)** entièrement manuelle
- **(B)** essentiellement automatisée pour minimiser l'erreur
- **(C)** sans aucun contrôle
- **(D)** externalisée
**Q69.** Le Principe 3 exige la réconciliation des données de risque avec —

- **(A)** les cours de bourse
- **(B)** les sources comptables et autres sources pertinentes
- **(C)** les prévisions marketing
- **(D)** les dividendes
**Q70.** Les ajustements manuels (manual workarounds) doivent être —

- **(A)** la règle générale
- **(B)** exceptionnels, explicités et documentés
- **(C)** interdits en toute circonstance
- **(D)** sans traçabilité
**Q71.** L'exhaustivité (Principe 4) impose de capturer —

- **(A)** le seul risque de marché
- **(B)** toutes les expositions matérielles : lignes métier, entités, types de risque, géographies
- **(C)** les seules expositions supérieures à 1 Md
- **(D)** les seules données comptables
**Q72.** Le test opérationnel du Principe 4 est la capacité à produire rapidement —

- **(A)** le cours de l'action
- **(B)** l'exposition consolidée du groupe à une contrepartie unique
- **(C)** le montant des dividendes
- **(D)** la valeur de marque
**Q73.** La rapidité requise (Principe 5) dépend notamment de —

- **(A)** la couleur des rapports
- **(B)** la nature et la volatilité du risque, et de la criticité de la décision
- **(C)** la taille du conseil
- **(D)** la date de création de la banque
**Q74.** En période de stress, la capacité de production des agrégats doit —

- **(A)** ralentir
- **(B)** rester inchangée
- **(C)** s'accélérer
- **(D)** être suspendue
**Q75.** Une banque incapable de produire vite son exposition groupe à une contrepartie viole d'abord —

- **(A)** la confidentialité
- **(B)** l'exhaustivité et la rapidité
- **(C)** le ratio de levier
- **(D)** la règle Volcker
**Q76.** Le data lineage automatisé soutient principalement —

- **(A)** le marketing
- **(B)** la planification fiscale
- **(C)** l'exactitude et l'intégrité des données de risque
- **(D)** la politique de dividende
**Q77.** Une granularité élémentaire suffisante est nécessaire pour —

- **(A)** empêcher toute agrégation
- **(B)** permettre la ré-agrégation selon différents axes
- **(C)** réduire l'exactitude
- **(D)** augmenter le capital
**Q78.** Répondre à une demande du superviseur sur un découpage non prévu relève de —

- **(A)** l'adaptabilité (Principe 6)
- **(B)** la fréquence (Principe 10)
- **(C)** la distribution (Principe 11)
- **(D)** la coopération (Principe 14)
**Q79.** Générer une agrégation pour un scénario de stress défini à la demande relève de —

- **(A)** l'exactitude
- **(B)** l'adaptabilité
- **(C)** la clarté
- **(D)** la revue
**Q80.** Laquelle N'EST PAS l'une des quatre qualités d'agrégation ? —

- **(A)** exactitude
- **(B)** exhaustivité
- **(C)** clarté des rapports
- **(D)** rapidité
**Q81.** Une agrégation dépendant de dizaines de feuilles de calcul manuelles viole surtout —

- **(A)** le Principe 3 (exactitude/intégrité)
- **(B)** le Principe 10
- **(C)** le Principe 14
- **(D)** aucun principe
**Q82.** Omettre une filiale étrangère de l'exposition groupe viole —

- **(A)** l'exactitude
- **(B)** l'exhaustivité (Principe 4)
- **(C)** la fréquence
- **(D)** la distribution
**Q83.** Ne pas agréger le risque opérationnel au niveau groupe viole —

- **(A)** le Principe 4 (toutes les catégories de risque)
- **(B)** le Principe 11
- **(C)** le Principe 13
- **(D)** aucun principe
**Q84.** Une exposition disponible seulement à J+10, inutile en crise, viole —

- **(A)** la rapidité (Principe 5)
- **(B)** la clarté
- **(C)** la distribution
- **(D)** la coopération
**Q85.** Ne stocker les données qu'à l'état agrégé (sans granularité) contrevient à —

- **(A)** la capacité de ré-agrégation (Principes 3/6)
- **(B)** la fréquence
- **(C)** la distribution
- **(D)** la revue
**Q86.** Un écart persistant et inexpliqué entre données de risque et comptabilité viole —

- **(A)** le Principe 3 (réconciliation)
- **(B)** le Principe 10
- **(C)** le Principe 14
- **(D)** aucun principe
**Q87.** Le noyau technique de BCBS 239 est constitué par le thème —

- **(A)** I
- **(B)** II (capacités d'agrégation)
- **(C)** III
- **(D)** IV
**Q88.** Le Principe 5 doit être concilié avec —

- **(A)** l'exactitude et l'exhaustivité
- **(B)** la seule rapidité, au détriment du reste
- **(C)** la confidentialité uniquement
- **(D)** le capital
**Q89.** L'adaptabilité inclut la réponse à —

- **(A)** des requêtes ad hoc, des demandes du superviseur et des crises émergentes
- **(B)** la seule clôture annuelle
- **(C)** aucune demande imprévue
- **(D)** la seule comptabilité
**Q90.** Pour l'auto-évaluation d'une banque, la checklist du thème II est —

- **(A)** exacte, complète, à temps, adaptable
- **(B)** rentable, liquide, capitalisée, cotée
- **(C)** brève, secrète, simple, peu coûteuse
- **(D)** annuelle, manuelle, locale, fixe
**Q91.** L'automatisation de l'agrégation vise principalement à —

- **(A)** augmenter les erreurs
- **(B)** minimiser la probabilité d'erreur humaine
- **(C)** ralentir la production
- **(D)** supprimer la traçabilité
**Q92.** « Completeness » se traduit le mieux par —

- **(A)** exhaustivité de la couverture des expositions matérielles
- **(B)** brièveté
- **(C)** clarté visuelle
- **(D)** confidentialité
**Q93.** « Timeliness » se traduit le mieux par —

- **(A)** production de l'information à temps, surtout en crise
- **(B)** exactitude arithmétique
- **(C)** distribution sécurisée
- **(D)** coopération transfrontalière
**Q94.** Le lien entre le Principe 2 (IT) et le Principe 3 (exactitude) tient à —

- **(A)** aucune relation
- **(B)** une architecture solide et un data lineage conditionnant l'exactitude auditable
- **(C)** la seule fréquence
- **(D)** la seule distribution
**Q95.** Produire l'exposition groupe à une contrepartie en 2 heures après remédiation traduit un progrès sur —

- **(A)** les seuls dividendes
- **(B)** l'exhaustivité, la rapidité et l'adaptabilité conjointement
- **(C)** la seule comptabilité
- **(D)** le seul capital
## C.5 Thème III — Pratiques de reporting (Q96-Q120)


**Q96.** Laquelle est un principe de reporting (et non d'agrégation) ? —

- **(A)** l'exactitude des données
- **(B)** l'exhaustivité des données
- **(C)** la clarté, l'utilité et la distribution des rapports
- **(D)** la conception de l'infrastructure IT
**Q97.** Le Principe 7 (reporting) porte sur —

- **(A)** l'exactitude des rapports
- **(B)** la gouvernance
- **(C)** l'architecture IT
- **(D)** la coopération home/host
**Q98.** Le Principe 8 (reporting) porte sur —

- **(A)** la couverture (comprehensiveness) de tous les risques matériels
- **(B)** la fréquence
- **(C)** la revue
- **(D)** le capital
**Q99.** Le Principe 9 porte sur —

- **(A)** la clarté et l'utilité des rapports
- **(B)** l'exhaustivité des données
- **(C)** la coopération
- **(D)** le levier
**Q100.** Le Principe 10 porte sur —

- **(A)** la fréquence des rapports
- **(B)** l'exactitude des données
- **(C)** la revue prudentielle
- **(D)** la taxonomie
**Q101.** Le Principe 11 porte sur —

- **(A)** la distribution des rapports
- **(B)** l'adaptabilité
- **(C)** la gouvernance
- **(D)** le capital
**Q102.** « Comprehensiveness » d'un rapport de risque signifie —

- **(A)** couvrir le seul risque de marché
- **(B)** couvrir tous les domaines de risque matériels
- **(C)** la brièveté maximale
- **(D)** une fréquence annuelle seulement
**Q103.** La « distribution » des rapports exige qu'ils —

- **(A)** soient rendus totalement publics
- **(B)** soient dissimulés au conseil
- **(C)** atteignent les bons destinataires tout en préservant la confidentialité
- **(D)** soient envoyés au seul service IT
**Q104.** La fréquence des rapports (Principe 10) devrait, en période de stress —

- **(A)** diminuer
- **(B)** rester annuelle
- **(C)** augmenter
- **(D)** être figée à jamais
**Q105.** Un rapport clair mais fondé sur des données non réconciliées viole —

- **(A)** le Principe 7 (exactitude du reporting)
- **(B)** le Principe 14
- **(C)** le Principe 2
- **(D)** aucun principe
**Q106.** Un rapport au conseil qui omet le risque de liquidité viole —

- **(A)** le Principe 8 (couverture)
- **(B)** le Principe 12
- **(C)** le Principe 6
- **(D)** aucun principe
**Q107.** Un rapport de 200 pages où l'essentiel est noyé viole —

- **(A)** le Principe 9 (clarté/utilité)
- **(B)** le Principe 3
- **(C)** le Principe 14
- **(D)** aucun principe
**Q108.** Distinguer une synthèse pour le conseil et un détail pour les desks illustre —

- **(A)** une violation du Principe 9
- **(B)** une bonne application du Principe 9 (adaptation au destinataire)
- **(C)** une violation du Principe 11
- **(D)** une exigence de capital
**Q109.** Maintenir un reporting mensuel figé en pleine tempête de marché viole —

- **(A)** le Principe 10 (fréquence)
- **(B)** le Principe 2
- **(C)** le Principe 14
- **(D)** aucun principe
**Q110.** Ne pas transmettre un dépassement de limite au conseil viole —

- **(A)** le Principe 11 (distribution, pas de rétention)
- **(B)** le Principe 5
- **(C)** le Principe 12
- **(D)** aucun principe
**Q111.** La diffusion d'un rapport sensible hors du cercle autorisé viole —

- **(A)** le Principe 11 (confidentialité dans la distribution)
- **(B)** le Principe 4
- **(C)** le Principe 6
- **(D)** aucun principe
**Q112.** Le niveau d'exactitude requis d'un rapport dépend de —

- **(A)** sa couleur
- **(B)** son usage : les rapports critiques exigent la plus haute fiabilité
- **(C)** sa longueur
- **(D)** la taille de la police
**Q113.** Un bon rapport de risque met en évidence —

- **(A)** uniquement les données historiques
- **(B)** les risques émergents, dépassements de limites et points d'attention
- **(C)** la seule performance commerciale
- **(D)** les dividendes futurs
**Q114.** Le thème III regroupe les principes —

- **(A)** 1 et 2
- **(B)** 3 à 6
- **(C)** 7 à 11
- **(D)** 12 à 14
**Q115.** La clarté du reporting (Principe 9) implique que —

- **(A)** plus il y a d'information, mieux c'est
- **(B)** l'excès d'information nuit à la décision ; le rapport doit être concis et utile
- **(C)** le rapport doit être illisible pour rester exhaustif
- **(D)** la fréquence prime sur le contenu
**Q116.** Le Principe 8 exige que la profondeur des rapports soit cohérente avec —

- **(A)** la seule rentabilité
- **(B)** la taille, la complexité et le profil de risque de la banque
- **(C)** le nombre d'actionnaires
- **(D)** la date de création
**Q117.** La fréquence des rapports est déterminée par —

- **(A)** les destinataires, la nature des risques et leur rapidité d'évolution
- **(B)** le seul calendrier fiscal
- **(C)** la couleur de la charte graphique
- **(D)** le cours de bourse
**Q118.** Un rapport exact mais illisible échoue au regard du —

- **(A)** Principe 9 (clarté/utilité)
- **(B)** Principe 3
- **(C)** Principe 14
- **(D)** aucun principe
**Q119.** La réconciliation et la validation des rapports relèvent du —

- **(A)** Principe 7
- **(B)** Principe 10
- **(C)** Principe 14
- **(D)** Principe 2 uniquement
**Q120.** Le thème III (reporting) porte, en une phrase, sur —

- **(A)** exactitude, couverture, clarté/utilité, fréquence, distribution des rapports
- **(B)** gouvernance et IT
- **(C)** revue et coopération
- **(D)** capital et liquidité
## C.6 Thème IV — Surveillance prudentielle (Q121-Q135)


**Q121.** Le Principe 12 concerne —

- **(A)** la revue prudentielle périodique de la conformité
- **(B)** la fréquence des rapports internes
- **(C)** la taxonomie
- **(D)** le capital
**Q122.** Le Principe 13 concerne —

- **(A)** les mesures correctives et les outils prudentiels
- **(B)** la clarté des rapports
- **(C)** l'exhaustivité des données
- **(D)** la gouvernance interne
**Q123.** Le Principe 14 concerne —

- **(A)** la coopération home/host entre superviseurs
- **(B)** la fréquence
- **(C)** l'architecture IT
- **(D)** le levier
**Q124.** La conformité à BCBS 239 est évaluée par —

- **(A)** les actionnaires
- **(B)** les superviseurs
- **(C)** les clients
- **(D)** les agences de notation
**Q125.** La revue prudentielle (Principe 12) teste notamment la capacité à produire des agrégations —

- **(A)** sous stress et ad hoc
- **(B)** uniquement annuelles
- **(C)** uniquement comptables
- **(D)** uniquement locales
**Q126.** Les outils du Principe 13 permettent au superviseur d'exiger —

- **(A)** des dividendes
- **(B)** des mesures correctives efficaces et en temps voulu
- **(C)** une hausse du cours de bourse
- **(D)** la publication des rapports
**Q127.** « Home/host » désigne —

- **(A)** le logement des employés
- **(B)** les centres de données
- **(C)** la coordination transfrontalière entre superviseurs d'origine et d'accueil
- **(D)** des arrangements d'externalisation
**Q128.** En cas de déficiences persistantes, le superviseur peut —

- **(A)** fixer un délai de remédiation et imposer des contraintes temporaires
- **(B)** trader à la place de la banque
- **(C)** fixer les dividendes
- **(D)** auditer les clients
**Q129.** La coopération home/host vise à éviter —

- **(A)** les angles morts de surveillance entre juridictions
- **(B)** la fréquence des rapports
- **(C)** la clarté des rapports
- **(D)** l'automatisation
**Q130.** La surveillance prudentielle (thème IV) implique des superviseurs qui —

- **(A)** tradent pour le compte des banques
- **(B)** examinent la conformité et exigent la remédiation
- **(C)** fixent les dividendes
- **(D)** auditent les clients
**Q131.** Les trois derniers des 14 principes portent sur —

- **(A)** le portefeuille de négociation
- **(B)** les ratios de capital
- **(C)** les ratios de liquidité
- **(D)** la revue, les mesures correctives et la coopération home/host
**Q132.** Le thème IV regroupe les principes —

- **(A)** 1 et 2
- **(B)** 3 à 6
- **(C)** 7 à 11
- **(D)** 12 à 14
**Q133.** Un superviseur qui impose des contraintes d'activité tant que les faiblesses persistent applique —

- **(A)** le Principe 13
- **(B)** le Principe 9
- **(C)** le Principe 4
- **(D)** le Principe 2
**Q134.** La revue de la conformité couvre —

- **(A)** les onze premiers principes
- **(B)** le seul Principe 1
- **(C)** aucun principe
- **(D)** le seul reporting
**Q135.** La coordination entre autorité d'origine et d'accueil pour un groupe transfrontalier illustre —

- **(A)** le Principe 14
- **(B)** le Principe 5
- **(C)** le Principe 9
- **(D)** le Principe 3
## C.7 Data lineage, interactions et indicateurs (Q136-Q150)


**Q136.** La chaîne logique de qualité des données est —

- **(A)** golden source, taxonomie, automatisation, data lineage, exactitude auditable
- **(B)** capital, liquidité, levier, comptabilité
- **(C)** dividende, cours, marketing, fiscalité
- **(D)** secret, brièveté, simplicité, coût
**Q137.** BCBS 239 ne fixe aucune exigence de —

- **(A)** fonds propres, de liquidité ni de norme comptable
- **(B)** qualité des données
- **(C)** reporting
- **(D)** gouvernance des données
**Q138.** BCBS 239 conditionne notamment la crédibilité des —

- **(A)** stress tests, du reporting réglementaire et du cadre d'appétit pour le risque
- **(B)** campagnes marketing
- **(C)** politiques de dividende
- **(D)** plans de rémunération
**Q139.** Le taux de complétude se calcule comme —

- **(A)** champs renseignés valides / champs attendus
- **(B)** profit / chiffre d'affaires
- **(C)** capital / actifs
- **(D)** dividende / bénéfice
**Q140.** Sur 500 000 champs attendus, 486 000 valides : la complétude est —

- **(A)** 92,0%
- **(B)** 95,0%
- **(C)** 97,2%
- **(D)** 99,7%
**Q141.** Le taux de traitement automatisé (STP) mesure —

- **(A)** la part des volumes traités sans intervention manuelle
- **(B)** le rendement des actifs
- **(C)** le ratio de levier
- **(D)** la marge nette
**Q142.** Un STP de 82% implique une part de workarounds manuels de —

- **(A)** 82%
- **(B)** 18%
- **(C)** 100%
- **(D)** 0%
**Q143.** La latence de reporting se définit comme —

- **(A)** l'écart entre l'arrêté des données et la disponibilité du rapport
- **(B)** le nombre de pages du rapport
- **(C)** le coût du rapport
- **(D)** la fréquence annuelle
**Q144.** Passer d'une latence de 15 h à 3 h en stress représente une réduction de —

- **(A)** 20%
- **(B)** 50%
- **(C)** 80%
- **(D)** 100%
**Q145.** Risque 1 000 M, comptabilité 1 004 M : l'écart relatif de réconciliation est —

- **(A)** 0,4%
- **(B)** 4,0%
- **(C)** 40%
- **(D)** 0,04%
**Q146.** Les indicateurs de qualité (complétude, exactitude, couverture, STP) alimentent —

- **(A)** un tableau de bord de gouvernance des données présenté au conseil
- **(B)** le calcul des dividendes
- **(C)** la valorisation boursière
- **(D)** la politique fiscale
**Q147.** Un STP faible accroît surtout le risque relatif à —

- **(A)** l'exactitude (Principe 3)
- **(B)** la coopération home/host
- **(C)** la fréquence
- **(D)** la distribution
**Q148.** Une couverture d'agrégation de 97,3% (58,4 sur 60,0 Md) signifie —

- **(A)** qu'aucune exposition n'est capturée
- **(B)** qu'environ 1,6 Md d'expositions matérielles restent un angle mort
- **(C)** que le capital est insuffisant
- **(D)** que la liquidité est excédentaire
**Q149.** Investir dans l'infrastructure (Principe 2) pour élever le STP se justifie car cela —

- **(A)** augmente les erreurs
- **(B)** réduit les erreurs d'agrégation en diminuant les workarounds manuels
- **(C)** ralentit la production
- **(D)** supprime la gouvernance
**Q150.** En une phrase, BCBS 239 est —

- **(A)** une exigence transversale de capacité d'agrégation des données de risque et de reporting, portée par la gouvernance
- **(B)** une norme de fonds propres
- **(C)** une norme de liquidité
- **(D)** une norme comptable IFRS
# Partie D — Corrigé commenté

*Chaque réponse renvoie à la section (§) et, le cas échéant, au cas correspondant.*

## D.1 Corrigé Q1-Q40

**Q1 — B** *(§7.1 · cas 1)* L'incapacité d'agréger rapidement les expositions en crise est l'origine directe de la norme.

**Q2 — A** *(§7.1)* Titre officiel : *Principles for effective risk data aggregation and risk reporting*.

**Q3 — B** *(§7.1)* Émise par le Comité de Bâle sur le contrôle bancaire (BCBS), document n°239.

**Q4 — C** *(§7.1)* Publiée en janvier 2013.

**Q5 — B** *(§7.1)* « On ne gère que ce que l'on voit » : l'information conditionne la maîtrise du risque.

**Q6 — B** *(§7.1)* Les systèmes fragmentés en silos ont empêché toute vue consolidée.

**Q7 — C** *(§7.1)* Les workarounds manuels rendent l'agrégation lente et faillible, surtout sous stress.

**Q8 — B** *(§7.8.2)* La *golden source* est la source de référence unique et autorisée d'une donnée.

**Q9 — B** *(§7.1 · cas 1)* L'exposition consolidée à Lehman est l'exemple fondateur.

**Q10 — C** *(§7.1)* C'est une exigence de capacité opérationnelle et de gouvernance des données, non de capital.

**Q11 — B** *(§7.1)* Document Bâle n°239 sur l'agrégation et le reporting des données de risque.

**Q12 — C** *(§7.1)* Ces systèmes sont précisément ce que la norme vise à corriger.

**Q13 — B** *(§7.8.2)* Le *data lineage* trace la donnée de l'origine à l'agrégat, transformations comprises.

**Q14 — B** *(§7.8.2)* La taxonomie garantit un vocabulaire et des définitions partagés dans tout le groupe.

**Q15 — D** *(§7.1)* Objectif global : renforcer l'agrégation des données de risque et le reporting.

**Q16 — B** *(§7.1)* Plusieurs jours, voire semaines, pour un chiffre consolidé fiable.

**Q17 — B** *(§7.1)* Sans identifiants uniques, l'agrégation cohérente des expositions devient impossible.

**Q18 — B** *(§7.1)* L'accent est mis sur la qualité des données et le reporting.

**Q19 — B** *(§7.1)* Exigence essentiellement qualitative : capacités et gouvernance des données.

**Q20 — B** *(§7.8.3)* La norme est transversale à toutes les catégories de risque matérielles.

**Q21 — C** *(§7.2.1 · cas 3)* Cible première : les G-SIB.

**Q22 — A** *(§7.2.1)* Extension recommandée aux D-SIB.

**Q23 — B** *(§7.2.3 · cas 5)* Conformité pleine attendue pour janvier 2016.

**Q24 — B** *(§7.2.3)* Trois ans après la publication de 2013.

**Q25 — B** *(§7.2.2)* Proportionnalité : selon la nature, la taille et la complexité.

**Q26 — B** *(§7.2.1 · cas 4)* Les assureurs européens relèvent de Solvabilité II.

**Q27 — B** *(§7.2.3 · cas 6)* À l'échéance, aucune G-SIB n'était pleinement conforme.

**Q28 — B** *(§7.2.3)* La gouvernance et l'architecture IT (thème I) sont restées les plus difficiles.

**Q29 — B** *(§7.2.2 · cas 7)* Elle module les moyens sans supprimer les exigences fondamentales.

**Q30 — C** *(§7.2.2)* Plus grande et complexe, l'institution doit satisfaire des attentes plus strictes.

**Q31 — B** *(§7.2.1)* Logique systémique : discipline la plus lourde là où la défaillance menace la stabilité.

**Q32 — B** *(§7.2.1)* Le calendrier D-SIB est laissé à la discrétion des superviseurs nationaux.

**Q33 — B** *(§7.2.1)* Les hedge funds sont hors du champ premier.

**Q34 — B** *(§7.2.3 · cas 6)* La lenteur reflète la dette technique héritée des silos.

**Q35 — B** *(§7.4.1 · cas 8)* Les capacités d'agrégation doivent être considérées lors des acquisitions.

**Q36 — A** *(§7.1)* Norme sur l'agrégation des données de risque et le reporting.

**Q37 — B** *(§7.2.2 · cas 7)* La proportionnalité n'exonère pas des exigences de qualité.

**Q38 — C** *(§7.3)* 14 principes.

**Q39 — B** *(§7.3)* Regroupés en 4 thèmes.

**Q40 — B** *(§7.2.1)* Application D-SIB typiquement trois ans après désignation.



## D.2 Corrigé Q41-Q75

**Q41 — B** *(§7.4.1 · cas 11)* Responsabilité ultime : conseil et direction générale.

**Q42 — A** *(§7.4.1)* Principe 1 = gouvernance.

**Q43 — B** *(§7.4.2)* Principe 2 = architecture des données et infrastructure IT.

**Q44 — C** *(§7.4.2)* Les systèmes doivent tenir y compris en période de stress.

**Q45 — B** *(§7.4.1 · cas 12)* Le conseil doit connaître les limitations de ses capacités d'agrégation.

**Q46 — B** *(§7.4.1 · cas 13)* Des propriétaires de données (*data owners*) doivent être désignés.

**Q47 — B** *(§7.4.1 · cas 20)* Le dispositif fonctionne en continu, indépendamment de tout événement.

**Q48 — B** *(§7.4.1 · cas 8)* Créer un nouveau silo contrevient à l'esprit du Principe 1.

**Q49 — B** *(§7.8.2)* La *golden source* relève de l'architecture (P2) et de l'exactitude (P3).

**Q50 — A** *(§7.4.2 · cas 18)* Résilience et continuité de l'infrastructure = Principe 2.

**Q51 — A** *(§7.3)* Thème I = principes 1 et 2.

**Q52 — B** *(§7.4.1 · cas 11)* Attribuer la faute « à l'IT seul » est contraire au Principe 1.

**Q53 — A** *(§7.4.2 · cas 14)* La taxonomie de groupe relève du Principe 2.

**Q54 — A** *(§7.4.1 · cas 12)* Décider sur des chiffres incomplets à son insu viole le Principe 1.

**Q55 — B** *(§7.4.2)* L'IT doit soutenir pleinement l'agrégation et le reporting, y compris en stress.

**Q56 — B** *(§7.4.2 · cas 19)* Le thème I attaque la dette technique des systèmes hérités.

**Q57 — B** *(§7.4.2 · cas 14)* Définitions divergentes du « défaut » = défaut de taxonomie (P2).

**Q58 — B** *(§7.4.1)* La gouvernance des données est permanente et intégrée.

**Q59 — B** *(§7.4.2 · cas 19)* Un programme pluriannuel illustre l'ampleur de la transformation.

**Q60 — A** *(§7.4)* Thème I : gouvernance et architecture/IT.

**Q61 — A** *(§7.5)* Quatre qualités : exactitude, exhaustivité, rapidité, adaptabilité.

**Q62 — D** *(§7.5)* Les systèmes doivent tenir surtout en période de stress.

**Q63 — B** *(§7.5.4)* Adaptabilité = requêtes *ad hoc* et stress à la demande.

**Q64 — A** *(§7.5.1)* Principe 3 = exactitude et intégrité.

**Q65 — A** *(§7.5.2)* Principe 4 = exhaustivité.

**Q66 — A** *(§7.5.3)* Principe 5 = rapidité.

**Q67 — A** *(§7.5.4)* Principe 6 = adaptabilité.

**Q68 — B** *(§7.5.1 · cas 21)* L'exactitude suppose une agrégation essentiellement automatisée.

**Q69 — B** *(§7.5.1 · cas 22)* Réconciliation avec les sources comptables et autres.

**Q70 — B** *(§7.5.1 · cas 21)* Les workarounds manuels doivent être exceptionnels et documentés.

**Q71 — B** *(§7.5.2 · cas 23)* Exhaustivité : toutes les expositions matérielles, sans angle mort.

**Q72 — B** *(§7.5.2 · cas 1)* Test : produire vite l'exposition groupe à une contrepartie unique.

**Q73 — B** *(§7.5.3 · cas 25)* La rapidité dépend de la volatilité du risque et de la criticité de la décision.

**Q74 — C** *(§7.5.3 · cas 26)* En stress, la production des agrégats doit s'accélérer.

**Q75 — B** *(§7.5.2 · cas 1)* Cette incapacité viole d'abord exhaustivité et rapidité.



## D.3 Corrigé Q76-Q120

**Q76 — C** *(§7.8.2 · cas 17)* Le *data lineage* soutient l'exactitude et l'intégrité des données.

**Q77 — B** *(§7.5.1 · cas 29)* Une granularité élémentaire permet la ré-agrégation par différents axes.

**Q78 — A** *(§7.5.4 · cas 9)* Répondre à un découpage non prévu du superviseur relève de l'adaptabilité (P6).

**Q79 — B** *(§7.5.4 · cas 28)* Agréger un scénario de stress à la demande = adaptabilité.

**Q80 — C** *(§7.5)* La clarté des rapports relève du reporting (thème III), pas de l'agrégation.

**Q81 — A** *(§7.5.1 · cas 21)* Une agrégation par feuilles manuelles viole le Principe 3.

**Q82 — B** *(§7.5.2 · cas 23)* Omettre une filiale viole l'exhaustivité (P4).

**Q83 — A** *(§7.5.2 · cas 24)* Ne pas agréger l'opérationnel viole le P4 (toutes catégories).

**Q84 — A** *(§7.5.3 · cas 25)* Une exposition à J+10 inutilisable en crise viole la rapidité (P5).

**Q85 — A** *(§7.5.1 · cas 29)* Stocker seulement l'agrégat empêche la ré-agrégation (P3/P6).

**Q86 — A** *(§7.5.1 · cas 22)* Un écart inexpliqué risque/compta viole le P3 (réconciliation).

**Q87 — B** *(§7.5)* Le noyau technique est le thème II.

**Q88 — A** *(§7.5.3)* La rapidité se concilie avec exactitude et exhaustivité.

**Q89 — A** *(§7.5.4)* L'adaptabilité couvre requêtes *ad hoc*, superviseur et crises émergentes.

**Q90 — A** *(§7.5 · cas 30)* Checklist thème II : exacte, complète, à temps, adaptable.

**Q91 — B** *(§7.5.1)* L'automatisation minimise la probabilité d'erreur humaine.

**Q92 — A** *(§7.5.2)* *Completeness* = exhaustivité de la couverture des expositions matérielles.

**Q93 — A** *(§7.5.3)* *Timeliness* = information produite à temps, surtout en crise.

**Q94 — B** *(§7.4.2 · §7.8.2)* Architecture et lineage conditionnent l'exactitude auditable.

**Q95 — B** *(§7.5 · cas 48)* Le progrès porte conjointement sur exhaustivité, rapidité, adaptabilité.

**Q96 — C** *(§7.6)* Clarté, utilité et distribution sont des principes de reporting.

**Q97 — A** *(§7.6.1)* Principe 7 = exactitude des rapports.

**Q98 — A** *(§7.6.2)* Principe 8 = couverture de tous les risques matériels.

**Q99 — A** *(§7.6.3)* Principe 9 = clarté et utilité.

**Q100 — A** *(§7.6.4)* Principe 10 = fréquence.

**Q101 — A** *(§7.6.5)* Principe 11 = distribution.

**Q102 — B** *(§7.6.2)* *Comprehensiveness* = couvrir tous les domaines de risque matériels.

**Q103 — C** *(§7.6.5 · cas 37)* Distribution : bons destinataires, confidentialité préservée.

**Q104 — C** *(§7.6.4 · cas 35)* En stress, la fréquence des rapports doit augmenter.

**Q105 — A** *(§7.6.1 · cas 31)* Un rapport clair mais faux viole le P7.

**Q106 — A** *(§7.6.2 · cas 32)* Omettre la liquidité viole la couverture (P8).

**Q107 — A** *(§7.6.3 · cas 33)* Noyer l'essentiel viole la clarté (P9).

**Q108 — B** *(§7.6.3 · cas 34)* Adapter au destinataire est une bonne application du P9.

**Q109 — A** *(§7.6.4 · cas 35)* Un reporting figé en tempête viole la fréquence (P10).

**Q110 — A** *(§7.6.5 · cas 36)* Retenir un dépassement de limite viole la distribution (P11).

**Q111 — A** *(§7.6.5 · cas 37)* Diffuser hors du cercle autorisé viole la confidentialité (P11).

**Q112 — B** *(§7.6.1)* L'exactitude requise dépend de l'usage ; les rapports critiques exigent le maximum.

**Q113 — B** *(§7.6.3)* Un bon rapport met en évidence risques émergents et dépassements.

**Q114 — C** *(§7.3)* Thème III = principes 7 à 11.

**Q115 — B** *(§7.6.3 · cas 33)* L'excès d'information nuit ; le rapport doit être concis et utile.

**Q116 — B** *(§7.6.2)* La profondeur est cohérente avec taille, complexité et profil de risque.

**Q117 — A** *(§7.6.4)* La fréquence dépend des destinataires, de la nature et de la vitesse d'évolution.

**Q118 — A** *(§7.6.3)* Un rapport exact mais illisible échoue au P9.

**Q119 — A** *(§7.6.1)* Réconciliation et validation des rapports = P7.

**Q120 — A** *(§7.6)* Thème III : exactitude, couverture, clarté/utilité, fréquence, distribution.



## D.4 Corrigé Q121-Q150

**Q121 — A** *(§7.7.1)* Principe 12 = revue prudentielle périodique.

**Q122 — A** *(§7.7.2)* Principe 13 = mesures correctives et outils prudentiels.

**Q123 — A** *(§7.7.3)* Principe 14 = coopération home/host.

**Q124 — B** *(§7.7 · cas 38)* La conformité est évaluée par les superviseurs.

**Q125 — A** *(§7.7.1 · cas 38)* La revue teste les agrégations sous stress et *ad hoc*.

**Q126 — B** *(§7.7.2 · cas 39)* Les outils permettent d'exiger des mesures correctives efficaces et en temps voulu.

**Q127 — C** *(§7.7.3 · cas 40)* Home/host = coordination transfrontalière entre superviseurs.

**Q128 — A** *(§7.7.2 · cas 39)* Le superviseur peut fixer un délai et imposer des contraintes temporaires.

**Q129 — A** *(§7.7.3 · cas 40)* La coopération évite les angles morts entre juridictions.

**Q130 — B** *(§7.7)* Les superviseurs examinent la conformité et exigent la remédiation.

**Q131 — D** *(§7.7)* Les trois derniers principes : revue, mesures correctives, coopération home/host.

**Q132 — D** *(§7.3)* Thème IV = principes 12 à 14.

**Q133 — A** *(§7.7.2 · cas 39)* Imposer des contraintes tant que les faiblesses persistent = P13.

**Q134 — A** *(§7.7.1)* La revue couvre les onze premiers principes.

**Q135 — A** *(§7.7.3 · cas 40)* Coordonner origine et accueil sur un groupe transfrontalier = P14.

**Q136 — A** *(§7.8.2)* Chaîne : golden source, taxonomie, automatisation, lineage, exactitude auditable.

**Q137 — A** *(§7.8.3)* BCBS 239 ne fixe ni capital, ni liquidité, ni norme comptable.

**Q138 — A** *(§7.8.3)* Elle conditionne stress tests, reporting réglementaire et cadre d'appétit.

**Q139 — A** *(§7.8.4 · cas 41)* Complétude = champs valides / champs attendus.

**Q140 — C** *(§7.8.4 · cas 41)* 486 000 / 500 000 = 97,2 %.

**Q141 — A** *(§7.8.4 · cas 44)* Le STP = part des volumes traités sans intervention manuelle.

**Q142 — B** *(§7.8.4 · cas 44)* STP 82 % ⇒ 18 % de workarounds manuels.

**Q143 — A** *(§7.8.4 · cas 45)* Latence = écart entre arrêté des données et disponibilité du rapport.

**Q144 — C** *(§7.8.4 · cas 46)* (15-3)/15 = 80 % de réduction.

**Q145 — A** *(§7.8.4 · cas 47)* 4 / 1 000 = 0,4 %.

**Q146 — A** *(§7.8.4 · cas 49)* Les KPI alimentent un tableau de bord de gouvernance présenté au conseil.

**Q147 — A** *(§7.8.4 · cas 44)* Un STP faible accroît le risque d'exactitude (P3).

**Q148 — B** *(§7.8.4 · cas 43)* Environ 1,6 Md d'expositions matérielles restent un angle mort.

**Q149 — B** *(§7.8.4 · cas 50)* Élever le STP réduit les erreurs en diminuant les workarounds manuels.

**Q150 — A** *(§7.8 · §7.3)* Exigence transversale de capacité d'agrégation et de reporting, portée par la gouvernance.





---

**Fin du Chapitre 7 — BCBS 239 · Cours · 50 cas · 150 QCM commentés**

