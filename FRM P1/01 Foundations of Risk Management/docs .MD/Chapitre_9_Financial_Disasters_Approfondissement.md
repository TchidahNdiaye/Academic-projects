# Chapitre 9 — Learning from Financial Disasters

### Les leçons des grandes catastrophes financières

**FRM Part I · Foundations — Approfondissement complet**

*Cours · 50 études de cas · 12 exercices corrigés · conseils méthodologiques · 150 QCM commentés. Questions originales de style examen ; GARP ne publiant aucune annale, aucune question n'est attribuée à une session passée.*

# Partie A — Cours approfondi

## 9.1 Pourquoi et comment étudier les catastrophes financières

### 9.1.1 L'intérêt pédagogique des désastres

Les grandes catastrophes financières constituent le laboratoire empirique de la gestion des risques : elles montrent, grandeur nature, comment un risque mal identifié, mal mesuré ou mal gouverné se transforme en perte catastrophique, voire en faillite. Le programme FRM ne demande pas de mémoriser des anecdotes, mais d'**extraire de chaque cas la défaillance de gestion des risques** qu'il illustre et la **leçon transposable**. Un même mécanisme — un défaut de séparation des tâches, une hypothèse de modèle fausse, un financement trop court — se retrouve dans des désastres séparés par des décennies et des continents.

### 9.1.2 Une grille d'analyse commune

Chaque cas se lit selon la même grille en quatre temps : (1) le **contexte et la stratégie** de la firme ; (2) le **mécanisme du risque** qui s'est matérialisé ; (3) les **défaillances de contrôle et de gouvernance** qui ont permis à la perte de croître ; (4) la **leçon** et la catégorie de risque dominante. Cette grille, développée en Partie D, permet de comparer des cas hétérogènes et d'éviter le piège de l'examen consistant à confondre les cas ou à leur attribuer la mauvaise leçon.

### 9.1.3 Les catégories de risque illustrées

Le chapitre organise les désastres par **type de risque dominant** : risque de taux d'intérêt (Savings & Loan), liquidité de financement (Lehman, Continental Illinois, Northern Rock), couvertures mal maîtrisées (Metallgesellschaft, Amaranth, Ashanti), risque de modèle (LTCM, London Whale, Niederhoffer), rogue trading et reporting trompeur (Barings, Société Générale, AIB, UBS, Daiwa), ingénierie financière et produits complexes (Bankers Trust, Orange County, Sachsen LB), risque de réputation (Volkswagen), gouvernance d'entreprise (Enron, WorldCom, Madoff) et cyber-risque (attaque SWIFT). Beaucoup de cas relèvent de plusieurs catégories à la fois — c'est précisément ce qui les rend instructifs.

> **⚑ Point d'examen —** À l'examen, chaque cas doit être rattaché à sa **catégorie de risque dominante** et à sa **leçon**. On n'attend pas des dates précises mais la **compréhension du mécanisme** : quel risque s'est matérialisé, quelle défaillance de contrôle l'a laissé croître, quelle leçon en tirer. Un même mécanisme (séparation des tâches, liquidité, levier, hypothèse de modèle) traverse plusieurs cas.

## 9.2 Risque de taux d'intérêt : la crise des Savings & Loan

### 9.2.1 Le modèle et le mismatch

Les caisses d'épargne américaines (*Savings & Loan*, ou *thrifts*) finançaient des **prêts hypothécaires à taux fixe et à long terme** avec des **dépôts à court terme**. Ce modèle produit un **mismatch de duration** structurel : l'actif a une duration longue, le passif une duration courte. Tant que la courbe des taux est stable et pentue, la marge d'intérêt est positive. Mais elle expose la firme à une **hausse des taux courts**.

### 9.2.2 Le déclencheur et l'aggravation

À la fin des années 1970 et au début des années 1980, la lutte de la Réserve fédérale (sous Paul Volcker) contre l'inflation propulse les taux courts à des niveaux très élevés. Le **coût des dépôts dépasse le rendement des prêts hypothécaires anciens à taux fixe** : la marge devient négative et la valeur nette des caisses s'effondre. La crise est aggravée par la **déréglementation** (élargissement des activités autorisées sans supervision adéquate) et par un puissant **aléa moral** : garanties par l'assurance des dépôts, de nombreuses caisses insolvables se lancent dans une fuite en avant (*gamble for resurrection*), prenant des risques extrêmes dans l'espoir de se refaire. Le coût final pour le contribuable américain se chiffre en centaines de milliards de dollars.

### 9.2.3 Les leçons

Le cas S&L est le cas d'école du **risque de taux d'intérêt** et du **risque de transformation** (asset-liability mismatch) : une institution qui emprunte court et prête long à taux fixe est vulnérable à une hausse des taux. Il illustre aussi le rôle destructeur de l'**aléa moral** créé par des garanties publiques mal conçues et l'importance d'une **supervision** à la hauteur des libertés accordées.

> **⚑ Point d'examen —** S&L = cas canonique du **risque de taux** et de l'**asset-liability mismatch** (emprunter court, prêter long à taux fixe). Déclencheur : **hausse des taux** (choc Volcker). Aggravation : **aléa moral** lié à la garantie des dépôts + déréglementation sans supervision. Leçon : gérer le **duration gap** et aligner la supervision sur les risques autorisés.

## 9.3 Risque de liquidité de financement

Le **risque de liquidité de financement** (*funding liquidity risk*) est l'incapacité à honorer ses engagements de trésorerie à leur échéance, faute de pouvoir se refinancer. Il faut le distinguer du **risque de liquidité de marché** (impossibilité de vendre un actif sans forte décote). Trois faillites en donnent des variantes complémentaires.

### 9.3.1 Lehman Brothers (2008)

Banque d'investissement à **levier très élevé** (de l'ordre de 30 pour 1), Lehman dépendait massivement d'un **financement de très court terme** (marché du *repo* renouvelé au jour le jour) pour porter un stock d'actifs immobiliers et de subprime **illiquides**. Elle recourait par ailleurs au « **Repo 105** », un artifice comptable déplaçant temporairement des actifs hors bilan aux dates d'arrêté pour **masquer son levier réel**. Lorsque la confiance s'évapore, les contreparties refusent de renouveler le financement : la banque ne peut plus se refinancer et fait faillite le 15 septembre 2008 — la plus grande faillite de l'histoire des États-Unis. Leçon : **levier excessif + dépendance au financement court terme + actifs illiquides** est une combinaison fatale ; l'opacité comptable ne fait que retarder l'échéance.

### 9.3.2 Continental Illinois (1984)

Septième banque américaine, Continental Illinois s'appuyait sur des **financements de gros** (interbancaire, dépôts institutionnels volatils) faute d'une base de **dépôts de détail** stables. Elle avait accumulé des **prêts énergie de mauvaise qualité** rachetés à la Penn Square Bank. Quand les pertes sur ces prêts sont connues, les créanciers de gros retirent leurs fonds en masse (un *run* de gros). Le sauvetage fédéral qui s'ensuit donne naissance à l'expression « **too big to fail** ». Leçon : la **concentration du financement sur des sources de gros volatiles** est dangereuse ; les dépôts de détail assurés sont plus stables.

### 9.3.3 Northern Rock (2007)

Cette banque britannique avait poussé à l'extrême le modèle « *originate-to-distribute* » : elle finançait ses prêts hypothécaires non par des dépôts, mais par la **titrisation** et le **marché de gros**. Quand le marché de la titrisation **gèle** à l'été 2007, elle ne peut plus refinancer son stock de prêts. L'annonce d'un soutien d'urgence de la Banque d'Angleterre déclenche le **premier bank run britannique depuis plus d'un siècle**, puis la nationalisation. Leçon : un modèle d'affaires reposant sur l'accès **permanent** aux marchés de financement est fragile ; la liquidité peut disparaître brutalement.

> **Funding liquidity vs market liquidity**
>
> Le **risque de liquidité de financement** est l'incapacité à lever des fonds pour honorer ses échéances (renouveler un *repo*, faire face à des retraits). Le **risque de liquidité de marché** est l'incapacité à céder un actif rapidement sans forte décote. Les deux se renforcent en crise : un besoin de financement force des ventes d'actifs dans un marché illiquide, à des prix dégradés.

> **⚑ Point d'examen —** Trois variantes du **funding liquidity risk** : **Lehman** (levier + repo court terme + actifs illiquides + Repo 105), **Continental Illinois** (dépendance aux financements de gros → origine de « too big to fail »), **Northern Rock** (financement par titrisation/marché de gros → premier bank run britannique en un siècle). Fil commun : dépendance à un financement court/instable pour porter des actifs longs ou illiquides.

## 9.4 Stratégies de couverture mal maîtrisées

Une couverture destinée à réduire le risque peut, mal conçue, en **créer un nouveau** — souvent un risque de trésorerie ou de base. Trois cas l'illustrent.

### 9.4.1 Metallgesellschaft (1993)

La filiale américaine MGRM avait vendu à ses clients des **contrats de livraison de pétrole à prix fixe et à long terme** (jusqu'à dix ans). Pour se couvrir, elle achetait des **contrats à terme (futures) de court terme** qu'elle **roulait** à chaque échéance : c'est la stratégie « **stack-and-roll** ». Le problème est double. D'abord, un **mismatch de flux de trésorerie** : la baisse des prix du pétrole provoque des **appels de marge immédiats** sur les futures (pertes en cash tout de suite), alors que les gains compensatoires sur les contrats long terme ne se matérialiseront que des années plus tard. Ensuite, le passage du marché en **contango** rend chaque *roll* coûteux (pertes de rollover). Confrontée à une hémorragie de trésorerie, la maison mère **liquide les positions au pire moment**, transformant une perte latente gérable en perte réalisée d'environ 1,3 milliard de dollars. Leçon : une couverture peut être **économiquement saine mais fatale en trésorerie** ; il faut gérer le **funding liquidity risk** de la couverture elle-même, ainsi que le risque de base et de rollover.

### 9.4.2 Amaranth Advisors (2006)

Ce hedge fund avait pris des positions **massives et concentrées** sur les **spreads de gaz naturel** (paris directionnels sur l'écart entre contrats de différents mois), via son trader Brian Hunter. Les positions étaient si grandes qu'elles représentaient une fraction dominante de l'*open interest* : le fonds **était** le marché. Quand les spreads évoluent à contre-sens, il est impossible de déboucler sans effondrer les prix contre soi-même. Le fonds perd environ **6,5 milliards de dollars en une semaine** et s'effondre. Leçon : **risque de concentration** et **risque de liquidité de marché** — une position dont la taille dépasse la capacité d'absorption du marché ne peut être dénouée sans pertes catastrophiques.

### 9.4.3 Ashanti Goldfields (1999)

Ce producteur d'or ghanéen s'était couvert contre une **baisse** du prix de l'or par des positions optionnelles complexes (dont des ventes d'options). Or le prix de l'or **bondit soudainement** en septembre 1999 (à la suite de l'accord des banques centrales européennes limitant leurs ventes d'or, dit *Washington Agreement*). Les positions vendues génèrent d'énormes **appels de marge** que le producteur ne peut honorer, le mettant au bord de la faillite. Leçon : une couverture calibrée sur un seul scénario (baisse) peut devenir **destructrice dans le scénario inverse** ; les positions optionnelles vendues exposent à des appels de marge illimités.

> **⚑ Point d'examen —** Couvertures mal maîtrisées : **Metallgesellschaft** (stack-and-roll → mismatch de trésorerie et pertes de rollover en contango, débouclage au pire moment), **Amaranth** (concentration extrême sur spreads de gaz → risque de liquidité de marché, « être le marché »), **Ashanti** (couverture vendue prise à contre-pied par une hausse de l'or → appels de marge). Leçon centrale : une couverture peut créer un **risque de liquidité/trésorerie** plus dangereux que le risque couvert.

## 9.5 Risque de modèle

Le **risque de modèle** est le risque de pertes résultant de décisions fondées sur des modèles **erronés, mal calibrés ou mal utilisés**. Il se manifeste par des hypothèses fausses, ou par la manipulation délibérée d'un modèle.

### 9.5.1 Long-Term Capital Management (1998)

LTCM, fondé par John Meriwether et associant deux futurs lauréats du prix Nobel (Merton et Scholes), pratiquait l'**arbitrage de convergence** : parier que des écarts de prix (*spreads*) entre titres proches se resserreraient. Chaque écart étant minuscule, le fonds recourait à un **levier gigantesque** (de l'ordre de 25 pour 1 au bilan, bien davantage en notionnel via les dérivés). Les modèles supposaient une **volatilité et des corrélations stables** et des marchés **liquides**. Le **défaut russe** d'août 1998 déclenche une **fuite vers la qualité** mondiale : au lieu de converger, les spreads **divergent**, les corrélations entre positions supposées indépendantes **convergent vers 1**, et la liquidité s'évapore. Le levier amplifie les pertes jusqu'à menacer le système financier ; la Réserve fédérale orchestre un sauvetage par un consortium de banques (environ 3,6 milliards de dollars). Leçon : **risque de modèle** (hypothèses de corrélation, de volatilité et de liquidité qui se brisent en crise) combiné à un **levier excessif** ; ce qui est diversifié en temps normal cesse de l'être dans une crise.

### 9.5.2 La « Baleine de Londres » (JPMorgan, 2012)

Au *Chief Investment Office* de JPMorgan à Londres, le trader Bruno Iksil (« la Baleine de Londres ») accumule des positions colossales sur des **indices de dérivés de crédit (CDS)**, présentées comme une couverture mais devenues un **pari directionnel**. Pour dissimuler la montée du risque, le **modèle interne de VaR est modifié** de manière à en **sous-estimer** l'ampleur, et les positions sont valorisées de façon avantageuse. La perte atteint environ **6,2 milliards de dollars**. Leçon : **risque de modèle** au sens de **manipulation du modèle de risque** (changer le modèle pour cacher l'exposition), doublée d'une défaillance de contrôle et de valorisation.

### 9.5.3 Niederhoffer (1997)

Le gérant Victor Niederhoffer vendait des **options de vente (puts) hors de la monnaie à découvert** (*naked puts*) sur l'indice S&P, une stratégie qui encaisse de petites primes régulières mais expose à une **perte massive en cas de krach**. La chute brutale du marché en octobre 1997 déclenche des **appels de marge** qui **anéantissent le fonds**. Leçon : **risque de queue** mal appréhendé — vendre des options à découvert revient à ramasser des pièces devant un rouleau compresseur ; le modèle sous-estime les événements extrêmes.

> **⚑ Point d'examen —** Risque de modèle : **LTCM** (hypothèses de corrélation/liquidité qui se brisent + levier extrême ; corrélations → 1 en crise), **London Whale** (VaR **manipulée** pour masquer une position de CDS), **Niederhoffer** (naked puts → risque de queue, ruine lors d'un krach). Leçon : un modèle n'est pas la réalité ; attention aux hypothèses de normalité, de corrélation et de liquidité, et à la manipulation des mesures de risque.

## 9.6 Rogue trading et reporting trompeur

Le *rogue trading* — un opérateur prenant des positions non autorisées et les dissimulant — n'est pas d'abord un problème de marché mais de **contrôle interne**. Le facteur récurrent est le **défaut de séparation des tâches** entre le *front office* (prise de position) et le *back office* (enregistrement, confirmation, réconciliation).

### 9.6.1 Barings Bank (1995)

À Singapour, le trader Nick Leeson **cumulait les fonctions de front et de back office**, ce qui lui permettait à la fois de prendre des positions et de les enregistrer. Il accumule des positions non autorisées sur les **futures Nikkei**, dissimulées dans un compte d'erreur (le fameux compte « **88888** »). Le **séisme de Kobe** fait chuter le Nikkei ; les pertes atteignent environ **827 millions de livres** et provoquent la **faillite de la plus ancienne banque marchande britannique**. Leçon emblématique : **la séparation front/back office est non négociable** ; sans elle, un seul individu peut prendre et masquer des risques mortels.

### 9.6.2 Société Générale — Jérôme Kerviel (2008)

Ancien du back office, Kerviel connaissait les mécanismes de contrôle et savait les contourner par des **transactions fictives** compensant en apparence ses positions. Il accumule des positions directionnelles non autorisées d'un notionnel d'environ **50 milliards d'euros**. Le débouclage forcé, en pleine chute des marchés de janvier 2008, cristallise une perte d'environ **4,9 milliards d'euros**. Leçon : la **connaissance des contrôles** par un initié rend leur robustesse d'autant plus cruciale ; les limites doivent être surveillées en **brut**, pas seulement en net.

### 9.6.3 Autres cas de rogue trading

Trois cas complètent le tableau et confirment le même diagnostic. **Allied Irish Banks / John Rusnak (2002)** : au sein de la filiale américaine Allfirst, des pertes de change d'environ **691 millions de dollars** dissimulées par de **faux contrats d'options**. **UBS / Kweku Adoboli (2011)** : environ **2,3 milliards de dollars** de pertes sur des positions non autorisées au *desk* ETF de Londres. **Daiwa Bank / Toshihide Iguchi (1995)** : environ **1,1 milliard de dollars** de pertes obligataires dissimulées pendant **onze ans**, là encore grâce au **cumul front/back office**. Dans chaque cas, la **réconciliation indépendante** et la **séparation des tâches** auraient détecté la fraude beaucoup plus tôt.

> **⚑ Point d'examen —** Rogue trading = défaillance de **contrôle interne**, pas de marché. Fil rouge : **absence de séparation front/back office** (Barings, Daiwa) ou **contournement des contrôles par un initié** (SocGen, AIB, UBS). Remèdes : **séparation des tâches**, **réconciliation indépendante**, surveillance des limites en **brut**, prise de congés obligatoire. Cas repère : **Barings/Leeson** (compte 88888, futures Nikkei, séisme de Kobe).

## 9.7 Ingénierie financière et produits complexes

Les produits dérivés complexes peuvent transférer des risques mal compris par l'acheteur — ou par le vendeur. La complexité masque le risque et crée des asymétries d'information.

### 9.7.1 Bankers Trust (1994)

Bankers Trust vendait des **dérivés sur mesure très complexes** à des entreprises industrielles, notamment **Gibson Greetings** et **Procter & Gamble**, qui subirent des pertes considérables lorsque les taux évoluèrent défavorablement. Des enregistrements internes révélèrent que des employés avaient **exploité la mauvaise compréhension** des produits par leurs clients. Au-delà des pertes financières, l'affaire infligea à la banque un **dommage réputationnel** durable et posa la question de l'**adéquation** (*suitability*) des produits vendus. Leçon : vendre de la complexité que le client ne comprend pas est un **risque de conduite et de réputation** majeur ; l'asymétrie d'information se paie.

### 9.7.2 Orange County (1994)

Le trésorier du comté d'Orange (Californie), Robert Citron, avait investi le portefeuille public dans des **prises en pension inversées (*reverse repos*) à effet de levier**, pariant sur une **baisse des taux**. La **hausse des taux de 1994** inflige des pertes d'environ **1,7 milliard de dollars** et conduit le comté à la **faillite**. Leçon : **levier** + **pari de taux** + **produits complexes** dans une entité publique dépourvue de **gouvernance du risque** adéquate ; un trésorier sans contre-pouvoir peut ruiner une collectivité.

### 9.7.3 Sachsen Landesbank (2007)

Cette banque régionale allemande avait logé, dans des **véhicules hors bilan** (conduits/SIV), des investissements en **subprime américain** financés à **court terme** par du papier commercial adossé à des actifs (*ABCP*). Le **gel du marché ABCP** en 2007 rend ces véhicules incapables de se refinancer, forçant la banque mère à les soutenir, ce qui précipite son **absorption**. Leçon : les **expositions hors bilan** et le **mismatch de liquidité** (actifs longs subprime financés à très court terme) transforment un problème « lointain » en risque existentiel pour la maison mère.

> **⚑ Point d'examen —** Produits complexes : **Bankers Trust** (dérivés vendus à Gibson Greetings et P&G → risque de conduite/réputation, asymétrie d'information), **Orange County** (reverse repos à levier + pari de taux → faillite d'une collectivité, gouvernance absente), **Sachsen LB** (conduits hors bilan subprime financés en ABCP → mismatch de liquidité). Leçon : la complexité masque le risque ; hors-bilan et suitability sont des angles morts classiques.

## 9.8 Risque de réputation : Volkswagen

### 9.8.1 Le « dieselgate »

En septembre 2015, l'agence américaine de l'environnement (EPA) révèle que Volkswagen avait installé un **logiciel truqueur** (*defeat device*) détectant les conditions de test d'émissions et réduisant artificiellement les rejets de ses moteurs diesel, qui dépassaient en réalité largement les normes en conditions réelles. Les conséquences sont massives : **amendes et transactions** de dizaines de milliards de dollars, **rappels** de millions de véhicules, **effondrement du cours** de l'action et **atteinte durable** à la marque.

### 9.8.2 Les leçons

Le **risque de réputation** est le risque qu'un comportement — ici une fraude délibérée — détruise la confiance des clients, des régulateurs et des investisseurs, avec des pertes financières bien supérieures au gain espéré de la fraude. Il découle souvent d'un **risque de conduite** (*conduct risk*) et d'une **défaillance de gouvernance et de culture** : une organisation où tricher paraît acceptable. Leçon : la réputation met des années à se construire et des heures à se détruire ; la conformité et l'éthique ne sont pas des coûts mais des protections du capital réputationnel.

> **⚑ Point d'examen —** Volkswagen (« dieselgate ») = cas canonique du **risque de réputation**, issu d'un **risque de conduite** (defeat device truquant les tests d'émissions) et d'une **défaillance de culture/gouvernance**. Leçon : les pertes réputationnelles dépassent de loin le gain de la fraude ; la culture d'entreprise est un déterminant de risque de premier ordre.

## 9.9 Gouvernance d'entreprise : Enron, WorldCom, Madoff

### 9.9.1 Enron (2001)

Enron utilisait des **entités ad hoc** (*special purpose entities*, SPE) pour **sortir de la dette du bilan** et gonfler ses résultats, combinées à une **comptabilité en valeur de marché** (*mark-to-market*) agressive sur des contrats de très long terme et à de graves **conflits d'intérêts** (le directeur financier Andrew Fastow dirigeait certaines des entités). La révélation de ces montages provoque la **faillite** en décembre 2001 et entraîne la disparition de son auditeur, **Arthur Andersen**. Leçon : **gouvernance défaillante**, **opacité comptable** (hors-bilan) et **conflits d'intérêts** ; l'affaire a directement inspiré la loi **Sarbanes-Oxley**.

### 9.9.2 WorldCom (2002)

WorldCom a commis une **fraude comptable** d'environ **11 milliards de dollars** en **capitalisant des charges d'exploitation courantes** (les inscrivant à l'actif au lieu de les passer en charges), gonflant artificiellement son résultat. C'est la plus grande faillite de son époque. Leçon : **fraude comptable** et défaillance des **contrôles internes** et de l'**audit** ; la simplicité de la manipulation en rend la détection d'autant plus impérative.

### 9.9.3 Bernard Madoff (2008)

Madoff a orchestré le plus grand **schéma de Ponzi** de l'histoire (de l'ordre de **65 milliards de dollars** de valeurs fictives), rémunérant d'anciens investisseurs avec l'argent des nouveaux. Les signaux d'alerte (*red flags*) étaient nombreux : des **rendements anormalement réguliers** et positifs, une **stratégie inexplicable**, un **auditeur minuscule** et l'**auto-conservation** des actifs (absence de dépositaire indépendant). La faillite de la **due diligence** des investisseurs et des régulateurs a permis à la fraude de durer des décennies. Leçon : **gouvernance et due diligence** ; des rendements « trop beaux pour être vrais » et l'absence de **tiers indépendants** (dépositaire, auditeur crédible) sont des signaux d'alarme.

> **⚑ Point d'examen —** Gouvernance : **Enron** (SPE hors bilan + mark-to-market agressif + conflits d'intérêts → Sarbanes-Oxley), **WorldCom** (capitalisation frauduleuse de charges, ~11 Md$), **Madoff** (Ponzi, red flags ignorés : rendements trop réguliers, pas de dépositaire indépendant). Leçon : transparence, contrôles, audit crédible et **tiers indépendants** ; méfiance envers ce qui est « trop beau pour être vrai ».

## 9.10 Cyber-risque : l'attaque SWIFT

### 9.10.1 Le braquage de la Bangladesh Bank (2016)

En 2016, des attaquants s'introduisent dans les systèmes de la banque centrale du Bangladesh, y installent un **logiciel malveillant**, puis émettent des **ordres de paiement SWIFT frauduleux** pour transférer près d'un milliard de dollars des réserves détenues à la Réserve fédérale de New York. La plupart des ordres sont bloqués — en partie grâce à une **faute d'orthographe** dans un ordre qui éveille les soupçons — mais environ **81 millions de dollars** sont effectivement détournés vers des casinos aux Philippines et largement perdus. L'attaque exploitait des **contrôles de sécurité insuffisants** côté client, le réseau SWIFT lui-même n'ayant pas été compromis.

### 9.10.2 Les leçons

Le **cyber-risque** est une composante majeure du **risque opérationnel** : il peut infliger des pertes directes et, via des infrastructures partagées, prendre une dimension **systémique**. Le cas SWIFT montre que la sécurité d'un réseau ne vaut que par celle de son **maillon le plus faible** (un participant mal protégé), et que la **détection**, la **ségrégation des systèmes** et la **surveillance des transactions** sont essentielles. Leçon : investir dans la cyber-résilience et traiter les tiers et participants comme des vecteurs de risque.

> **⚑ Point d'examen —** Attaque SWIFT (Bangladesh Bank, 2016) = **cyber-risque** relevant du **risque opérationnel**, potentiellement **systémique** via une infrastructure partagée. Le réseau SWIFT n'a pas été compromis : la faille était côté **participant** (contrôles insuffisants). Leçon : sécurité = maillon le plus faible ; détection, ségrégation et surveillance des transactions sont critiques.

## 9.11 Synthèse : les leçons transversales

Par-delà leur diversité, ces désastres font remonter un petit nombre de **causes récurrentes**. Le **levier excessif** transforme une perte gérable en faillite (Lehman, LTCM, Orange County). La **dépendance à un financement court ou instable** pour porter des actifs longs ou illiquides tue par la liquidité (Lehman, Continental Illinois, Northern Rock, Metallgesellschaft, Sachsen). La **concentration** — de position, de contrepartie, de source de financement — supprime les marges de sécurité (Amaranth, Continental Illinois). Les **hypothèses de modèle** fausses (corrélations, volatilité, liquidité, queues de distribution) se brisent précisément quand on en a besoin (LTCM, Niederhoffer, London Whale). Les **défaillances de contrôle interne**, au premier rang desquelles l'absence de **séparation des tâches**, laissent un individu prendre et masquer des risques mortels (Barings, Daiwa, SocGen, AIB, UBS). Enfin, la **gouvernance et la culture** — conflits d'intérêts, opacité, incitations perverses, tolérance à la fraude — sont le terreau commun des pires effondrements (Enron, WorldCom, Madoff, Volkswagen). La gestion des risques n'est pas d'abord une affaire de formules, mais de **structures, de contrôles et de culture**.

> **⚑ Point d'examen —** Six causes transversales à retenir : **(1)** levier excessif, **(2)** dépendance au financement court/instable, **(3)** concentration, **(4)** hypothèses de modèle fausses, **(5)** défaillance de contrôle interne (séparation des tâches), **(6)** gouvernance et culture déficientes. La plupart des cas combinent plusieurs de ces causes — c'est ce qui les rend catastrophiques.

# Partie B — 50 études de cas

## B.1 Risque de taux et ALM (cas 1 à 6)

**Cas 1 — Le duration gap.** Une caisse d'épargne détient des prêts hypothécaires à taux fixe (duration 7 ans) financés par des dépôts à vue (duration 0,5 an). Les taux montent de 200 points de base. *Enseignement* : le mismatch de duration fait chuter la valeur nette ; c'est le mécanisme du désastre S&L. (§9.2.1)

**Cas 2 — Emprunter court, prêter long.** Une banque affiche une marge d'intérêt confortable en période de courbe pentue et stable, sans couvrir son risque de taux. *Enseignement* : la marge masque une exposition ; une hausse des taux courts peut l'annuler (leçon S&L). (§9.2.2)

**Cas 3 — L'aléa moral de la garantie.** Une institution insolvable, dont les dépôts sont garantis par l'État, se lance dans des placements très risqués pour « se refaire ». *Enseignement* : la garantie publique crée un aléa moral (*gamble for resurrection*) ; d'où le besoin de supervision. (§9.2.2)

**Cas 4 — Déréglementation sans supervision.** De nouvelles activités sont autorisées à des caisses d'épargne sans renforcement du contrôle prudentiel. *Enseignement* : élargir les libertés sans aligner la supervision amplifie le risque systémique (leçon S&L). (§9.2.2)

**Cas 5 — La couverture par swap.** Une banque exposée au risque de taux (actif long, passif court) reçoit du fixe et paie du variable via un swap. *Enseignement* : un swap de taux peut réduire le duration gap ; l'absence d'une telle couverture fut fatale aux S&L. (§9.2.3)

**Cas 6 — Le choc Volcker.** Une hausse rapide et durable des taux directeurs renchérit le refinancement d'un stock de prêts anciens à taux fixe. *Enseignement* : le déclencheur du désastre S&L fut macroéconomique ; le risque était structurel et non couvert. (§9.2.2)

## B.2 Liquidité de financement (cas 7 à 14)

**Cas 7 — Le levier 30 pour 1.** Une banque d'investissement porte des actifs illiquides avec un levier de 30:1 et un financement repo au jour le jour. *Enseignement* : cette combinaison (levier + financement court + actifs illiquides) est celle de Lehman ; la perte de confiance suffit à provoquer la faillite. (§9.3.1)

**Cas 8 — Le window dressing.** Une firme déplace temporairement des actifs hors bilan aux dates d'arrêté pour afficher un levier réduit. *Enseignement* : le « Repo 105 » de Lehman masque le risque sans le réduire ; l'opacité comptable ne fait que retarder l'échéance. (§9.3.1)

**Cas 9 — Le run de gros.** Une banque financée par des dépôts institutionnels volatils voit ses créanciers de gros fuir à l'annonce de pertes. *Enseignement* : la concentration sur des financements de gros est instable (Continental Illinois → « too big to fail »). (§9.3.2)

**Cas 10 — Détail contre gros.** Deux banques ont le même actif ; l'une se finance par dépôts de détail assurés, l'autre par le marché interbancaire. *Enseignement* : les dépôts de détail sont plus stables ; la structure du passif détermine la résilience à la liquidité. (§9.3.2)

**Cas 11 — Le gel de la titrisation.** Une banque « originate-to-distribute » ne peut plus refinancer ses prêts quand le marché de la titrisation se ferme. *Enseignement* : dépendre de l'accès permanent aux marchés de financement est fragile (Northern Rock). (§9.3.3)

**Cas 12 — Le premier bank run en un siècle.** L'annonce d'un soutien d'urgence de la banque centrale déclenche la panique des déposants au lieu de les rassurer. *Enseignement* : la communication de crise est délicate ; la confiance, une fois rompue, s'auto-entretient (Northern Rock). (§9.3.3)

**Cas 13 — Funding vs market liquidity.** Une firme doit honorer des appels de marge mais ne peut vendre ses actifs qu'à forte décote. *Enseignement* : les deux liquidités se renforcent en crise ; un besoin de financement force des ventes à prix cassés. (§9.3)

**Cas 14 — La spirale de liquidité.** Des ventes forcées font baisser les prix, dégradant la valeur du collatéral, ce qui déclenche de nouveaux appels de marge. *Enseignement* : la boucle prix-collatéral-marge amplifie la crise (mécanisme commun à Lehman et LTCM). (§9.3)

## B.3 Couvertures mal maîtrisées (cas 15 à 22)

**Cas 15 — Le stack-and-roll.** Une firme couvre des engagements de livraison à 10 ans par des futures courts roulés en continu. *Enseignement* : le mismatch d'échéances crée un risque de trésorerie et de rollover (Metallgesellschaft). (§9.4.1)

**Cas 16 — Le contango qui coûte.** Chaque roulement de futures se fait à un prix plus élevé que celui qui expire. *Enseignement* : en contango, le *roll* génère des pertes récurrentes qui érodent la couverture (Metallgesellschaft). (§9.4.1)

**Cas 17 — L'appel de marge immédiat.** Les futures de couverture génèrent des pertes en cash aujourd'hui, alors que les gains sur le sous-jacent ne se matérialiseront que dans des années. *Enseignement* : une couverture économiquement saine peut être fatale en trésorerie. (§9.4.1)

**Cas 18 — Le débouclage au pire moment.** Faute de trésorerie, une firme liquide ses positions de couverture au creux du marché. *Enseignement* : transformer une perte latente en perte réalisée détruit la logique de couverture (Metallgesellschaft). (§9.4.1)

**Cas 19 — Être le marché.** Un fonds détient une part dominante de l'*open interest* sur un contrat. *Enseignement* : une position trop grande ne peut être dénouée sans effondrer les prix contre soi (Amaranth). (§9.4.2)

**Cas 20 — La concentration mortelle.** Tout le risque d'un fonds est logé dans un seul pari directionnel sur des spreads de gaz. *Enseignement* : l'absence de diversification supprime toute marge de sécurité (Amaranth). (§9.4.2)

**Cas 21 — La couverture à sens unique.** Un producteur se couvre par des ventes d'options calibrées sur une baisse du sous-jacent. *Enseignement* : une hausse inattendue déclenche des appels de marge illimités (Ashanti). (§9.4.3)

**Cas 22 — Le choc du Washington Agreement.** Un accord des banques centrales fait bondir le prix de l'or, prenant à contre-pied les positions vendues d'un producteur. *Enseignement* : les positions optionnelles vendues exposent à un risque asymétrique (Ashanti). (§9.4.3)

## B.4 Risque de modèle (cas 23 à 30)

**Cas 23 — L'arbitrage de convergence.** Un fonds parie que des spreads étroits vont se resserrer, avec un levier de 25:1. *Enseignement* : de minuscules écarts exigent un levier énorme qui amplifie les pertes si les spreads divergent (LTCM). (§9.5.1)

**Cas 24 — Les corrélations qui convergent.** En temps normal, les positions d'un fonds semblent indépendantes ; en crise, elles perdent toutes ensemble. *Enseignement* : les corrélations tendent vers 1 en crise ; la diversification supposée s'évapore (LTCM). (§9.5.1)

**Cas 25 — La fuite vers la qualité.** Un défaut souverain déclenche une ruée mondiale vers les actifs sûrs, faisant diverger les spreads au lieu de les resserrer. *Enseignement* : les modèles supposant la liquidité et la convergence se brisent dans les crises (LTCM). (§9.5.1)

**Cas 26 — La VaR modifiée.** Un desk change la méthodologie de son modèle de VaR de façon à afficher un risque plus faible. *Enseignement* : manipuler le modèle de risque masque l'exposition sans la réduire (London Whale). (§9.5.2)

**Cas 27 — La couverture devenue pari.** Une position présentée comme couverture de crédit grossit jusqu'à devenir un pari directionnel massif. *Enseignement* : une « couverture » qui prend une taille propre change de nature et de risque (London Whale). (§9.5.2)

**Cas 28 — Les naked puts.** Un gérant vend des options de vente à découvert pour encaisser des primes régulières. *Enseignement* : la stratégie ramasse de petits gains mais expose à une ruine lors d'un krach (Niederhoffer). (§9.5.3)

**Cas 29 — Le risque de queue oublié.** Un modèle calibré sur des marchés calmes ignore les mouvements extrêmes. *Enseignement* : sous-estimer les queues de distribution est une erreur de modèle classique (Niederhoffer, LTCM). (§9.5.3)

**Cas 30 — Le prix Nobel ne protège pas.** Un fonds dirigé par des sommités académiques échoue par excès de confiance dans ses modèles. *Enseignement* : la sophistication mathématique ne dispense pas d'humilité sur les hypothèses (LTCM). (§9.5.1)

## B.5 Rogue trading et contrôle interne (cas 31 à 38)

**Cas 31 — Le cumul front/back.** Un trader enregistre lui-même les opérations qu'il exécute. *Enseignement* : l'absence de séparation des tâches permet de prendre et de masquer des positions (Barings, Daiwa). (§9.6.1)

**Cas 32 — Le compte d'erreur.** Des pertes non autorisées sont dissimulées dans un compte technique peu surveillé. *Enseignement* : les comptes d'attente/erreur doivent être réconciliés indépendamment (compte 88888, Barings). (§9.6.1)

**Cas 33 — Le séisme révélateur.** Un choc externe (Kobe) fait exploser des positions cachées jusque-là gérables. *Enseignement* : la dissimulation ne supprime pas le risque ; un choc le révèle brutalement (Barings). (§9.6.1)

**Cas 34 — L'initié qui contourne.** Un ancien du back office connaît les contrôles et crée des transactions fictives pour les neutraliser. *Enseignement* : la connaissance des contrôles par un initié en exige la robustesse et l'indépendance (Kerviel, SocGen). (§9.6.2)

**Cas 35 — Le notionnel caché.** Des limites respectées en net masquent une exposition brute colossale. *Enseignement* : surveiller les positions en brut, pas seulement en net (SocGen). (§9.6.2)

**Cas 36 — Les fausses options.** Des pertes de change sont masquées par des contrats d'options fictifs. *Enseignement* : la réconciliation indépendante avec les contreparties aurait révélé la fraude (Rusnak, AIB). (§9.6.3)

**Cas 37 — Onze ans de dissimulation.** Un trader cache des pertes obligataires pendant plus d'une décennie. *Enseignement* : sans séparation des tâches ni congés obligatoires, une fraude peut durer des années (Iguchi, Daiwa). (§9.6.3)

**Cas 38 — Les congés obligatoires.** Un contrôle simple — imposer des congés et faire tourner les postes — révèle des positions qu'un seul individu maintenait. *Enseignement* : les congés obligatoires et la rotation sont des contrôles anti-fraude efficaces. (§9.6.3)

## B.6 Produits complexes et hors-bilan (cas 39 à 42)

**Cas 39 — La complexité vendue.** Une banque vend à une entreprise un dérivé sur mesure dont le client ne mesure pas le risque. *Enseignement* : exploiter l'asymétrie d'information crée un risque de conduite et de réputation (Bankers Trust). (§9.7.1)

**Cas 40 — Le trésorier à découvert.** Une collectivité investit sa trésorerie dans des reverse repos à levier en pariant sur la baisse des taux. *Enseignement* : levier + pari de taux + gouvernance absente = faillite publique (Orange County). (§9.7.2)

**Cas 41 — Le conduit hors bilan.** Une banque loge du subprime dans un véhicule hors bilan financé par de l'ABCP à court terme. *Enseignement* : le mismatch de liquidité hors bilan devient un risque existentiel pour la maison mère (Sachsen LB). (§9.7.3)

**Cas 42 — Le risque « lointain » qui revient.** Une exposition jugée périphérique et hors bilan contamine soudain le bilan principal. *Enseignement* : les expositions hors bilan sont un angle mort ; elles doivent être consolidées dans la vue des risques. (§9.7.3)

## B.7 Réputation et conduite (cas 43 à 44)

**Cas 43 — Le logiciel truqueur.** Un constructeur fausse délibérément les tests d'émissions de ses véhicules. *Enseignement* : le risque de conduite se mue en risque de réputation, avec des pertes bien supérieures au gain de la fraude (Volkswagen). (§9.8.1)

**Cas 44 — Des heures pour détruire des années.** Une marque bâtie sur des décennies s'effondre en quelques jours après une révélation. *Enseignement* : le capital réputationnel est fragile ; l'éthique et la conformité le protègent (Volkswagen). (§9.8.2)

## B.8 Gouvernance et fraude (cas 45 à 48)

**Cas 45 — La dette hors bilan.** Une firme sort sa dette du bilan via des entités ad hoc pour embellir ses comptes. *Enseignement* : l'opacité comptable et les conflits d'intérêts sont au cœur d'Enron ; d'où Sarbanes-Oxley. (§9.9.1)

**Cas 46 — Charges capitalisées.** Une entreprise inscrit à l'actif des charges d'exploitation courantes pour gonfler son résultat. *Enseignement* : une fraude comptable simple exige des contrôles internes et un audit robustes (WorldCom). (§9.9.2)

**Cas 47 — Les rendements trop réguliers.** Un fonds affiche des performances positives et lisses, quasi sans volatilité, année après année. *Enseignement* : des rendements « trop beaux pour être vrais » sont un signal d'alarme (Madoff). (§9.9.3)

**Cas 48 — L'absence de tiers indépendant.** Un gérant conserve lui-même les actifs et recourt à un auditeur minuscule. *Enseignement* : l'absence de dépositaire et d'auditeur crédibles est un red flag majeur ; la due diligence doit l'exiger (Madoff). (§9.9.3)

## B.9 Cyber-risque (cas 49 à 50)

**Cas 49 — Le maillon faible.** Des attaquants pénètrent le système d'un participant mal protégé pour émettre des ordres de paiement frauduleux sur un réseau par ailleurs sûr. *Enseignement* : la sécurité vaut celle du maillon le plus faible (attaque SWIFT, Bangladesh Bank). (§9.10.1)

**Cas 50 — La faute de frappe salvatrice.** Une erreur d'orthographe dans un ordre frauduleux éveille les soupçons et bloque la majeure partie du vol. *Enseignement* : la détection et la surveillance des transactions sont essentielles ; le cyber-risque relève du risque opérationnel (SWIFT). (§9.10.2)

# Partie C — Exercices d'application corrigés

*Douze exercices chiffrés reliant chaque grand désastre à un calcul de gestion des risques. Les montants sont stylisés à des fins pédagogiques ; ils reproduisent les ordres de grandeur et surtout les mécanismes.*

## Exercice 1 — Duration gap et insolvabilité (Savings & Loan)

**Énoncé.** Une caisse d'épargne présente le bilan suivant : actifs A = 1000 (duration D(A) = 7 ans), passifs L = 900 (duration D(L) = 1 an), fonds propres E = 100. Les taux augmentent de Δ y = +2% (200 points de base). Calculez l'impact sur les fonds propres et concluez.

**Solution.** La sensibilité de la valeur d'un poste à une variation de taux est Δ V ≈ -D · V · Δ y.

> Δ A ≈ -D(A) · A · Δ y = -7 × 1000 × 0,02 = -140

> Δ L ≈ -D(L) · L · Δ y = -1 × 900 × 0,02 = -18

> Δ E = Δ A − Δ L = -140 − (-18) = -122

Les fonds propres passent de 100 à 100 − 122 = -22 : la caisse devient **insolvable**. On retrouve ce résultat via le *duration gap* :

> DGAP = D(A) − (L) ÷ (A) D(L) = 7 − (900) ÷ (1000)× 1 = 6,1  ans,     Δ E ≈ -DGAP· A · Δ y = -6,1 × 1000 × 0,02 = -122.

**Leçon (§9.2).** Un actif long financé court à taux fixe rend la valeur nette hypersensible à une hausse des taux : c'est le mécanisme exact de la crise des S&L.

## Exercice 2 — Pertes de rollover en contango (Metallgesellschaft)

**Énoncé.** Une firme couvre une exposition de 10 millions de barils via des futures à un mois, roulés chaque mois. Le marché est en **contango** : à chaque roulement, le nouveau contrat coûte 0,40 $ de plus par baril que celui qui expire. Quelle est la perte de rollover cumulée sur 6 mois, indépendamment du mouvement de prix du sous-jacent ?

**Solution.** Chaque roulement coûte, par baril, le montant du contango. La perte par roll est :

> Perte(roll) = 0,40 × 10 000 000 = 4 000 000 $ = 4 M$

Sur 6 roulements :

> Perte cumulée = 4 M$ × 6 = 24 M$

**Leçon (§9.4.1).** En contango, le *stack-and-roll* génère une hémorragie récurrente, sans lien avec la justesse de la couverture sur le fond : le risque de rollover et de base est un coût réel qui érode la position.

## Exercice 3 — Levier et marge d'erreur (LTCM)

**Énoncé.** Un fonds dispose de 5 Md$ de capital et porte 125 Md$ d'actifs au bilan. Quel est son levier ? Quelle baisse en pourcentage de la valeur des actifs suffit à effacer tout le capital ?

**Solution.** Le levier bilanciel est :

> Levier = (Actifs) ÷ (Capital) = (125) ÷ (5) = 25:1

Le capital est effacé lorsque la perte égale 5 Md$, soit une baisse relative des actifs de :

> (5) ÷ (125) = 0,04 = 4%

Autrement dit, avec un levier L, une perte de 1/L sur les actifs ruine la firme : ici 1/25 = 4%.
**Leçon (§9.5.1).** Le levier comprime la marge d'erreur : un mouvement de marché de seulement 4% anéantit un fonds à 25:1. Combiné à des corrélations qui convergent vers 1 en crise, le levier fut fatal à LTCM.

## Exercice 4 — Appel de marge sur position vendue (Ashanti)

**Énoncé.** Un producteur d'or s'est couvert par des positions équivalant à une **exposition vendue** sur 2 millions d'onces. Le prix de l'or bondit de 255 à 305 $/oz. Estimez la perte de valeur de marché à régler en trésorerie (approximation linéaire) et expliquez le problème.

**Solution.** La variation de prix est Δ P = 305 − 255 = 50 $/oz. Pour une position vendue, une hausse du sous-jacent est une perte :

> Perte MtM ≈ Δ P × quantité = 50 × 2 000 000 = 100 M$

Cet appel de marge de 100 M$ doit être **honoré en cash immédiatement**, alors que le producteur ne vendra son or physique (dont la valeur a monté) que plus tard.
**Leçon (§9.4.3).** Une couverture calibrée pour une baisse devient destructrice si le sous-jacent monte : les positions vendues exposent à des appels de marge que la trésorerie courante ne peut absorber.

## Exercice 5 — Manipulation du modèle de VaR (London Whale)

**Énoncé.** Une position affiche une VaR de 130 M$ sous le modèle en vigueur. Un nouveau modèle, sans aucun changement de position, affiche une VaR de 65 M$. La limite de VaR du desk est de 100 M$. Analysez.

**Solution.** La réduction affichée est :

> (130 − 65) ÷ (130) = 50%

alors que le **risque économique est strictement inchangé**. Effet sur la conformité aux limites :

> Ancien modèle :  130 > 100 ⇒ dépassement ;     Nouveau modèle :  65 < 100 ⇒ « conforme ».

Le simple changement de modèle fait « rentrer » la position dans la limite.
**Leçon (§9.5.2).** Manipuler le modèle de risque masque l'exposition sans la réduire. La gouvernance des modèles (validation indépendante, contrôle des changements) est une défense essentielle.

## Exercice 6 — Trou de trésorerie d'une couverture (Metallgesellschaft)

**Énoncé.** Après une baisse des prix, les contrats de livraison long terme de la firme dégagent un gain latent de +80 M$ (elle vend cher à prix fixe), encaissable au fil des livraisons sur 10 ans. Les futures de couverture (longs) enregistrent en miroir une perte de -80 M$, réglée **immédiatement** en appels de marge. Quel est le résultat économique net et le besoin de financement immédiat ?

**Solution.** Résultat économique net :

> +80 − 80 = 0 M$ (la couverture fonctionne sur le fond)

Mais le profil de trésorerie est asymétrique :

> Immédiat :  -80 M$ (marge)     Étalé sur 10 ans :  +80 M$ (livraisons)

> ⇒ Besoin de financement immédiat = 80 M$.

**Leçon (§9.4.1).** Une couverture économiquement neutre peut ouvrir un trou de trésorerie considérable. Il faut modéliser et financer le *funding liquidity risk* de la couverture, sous peine de devoir déboucler au pire moment.

## Exercice 7 — Levier par reverse repos et sensibilité aux taux (Orange County)

**Énoncé.** Un trésorier public dispose d'un portefeuille propre de 7,5 Md$. Via des reverse repos, il emprunte 12,5 Md$ et porte une exposition totale de 20 Md$, de duration 7 ans. Les taux montent de Δ y = +1,5%. Calculez le levier, la perte, et son poids relatif au capital propre.

**Solution.** Levier sur le portefeuille propre :

> Levier = (20) ÷ (7,5) ≈ 2,67:1

Perte sur l'exposition totale :

> Perte ≈ -D · V · Δ y = -7 × 20 × 0,015 = -2,1 Md$

Poids relatif au capital propre :

> (2,1) ÷ (7,5) = 28%

**Leçon (§9.7.2).** Le levier amplifie la sensibilité aux taux : une hausse modérée (150 bp) inflige une perte égale à plus du quart du portefeuille propre. Sans gouvernance du risque, une collectivité peut ainsi faire faillite.

## Exercice 8 — Dépendance au financement de gros (Northern Rock)

**Énoncé.** Une banque finance un total de 100 Md£ : 25 Md£ de dépôts de détail et 75 Md£ de financement de gros et titrisation. Calculez le ratio de dépendance au financement de gros et le besoin de liquidité si ce marché gèle intégralement.

**Solution.**

> Ratio de financement de gros = (75) ÷ (100) = 75%

Si le marché de gros gèle, la part non renouvelable est :

> Besoin de liquidité = 75 Md£

soit les trois quarts du financement total, à remplacer d'urgence.
**Leçon (§9.3.3).** Une structure de passif reposant à 75% sur des marchés de financement rend la banque extrêmement vulnérable à leur fermeture. Les dépôts de détail assurés, plus stables, offrent une bien meilleure résilience.

## Exercice 9 — Reconstitution d'une perte de rogue trading (Société Générale)

**Énoncé.** Un trader a bâti, à l'insu des contrôles, une position directionnelle **longue** de 50 Md€ de notionnel sur indices actions. Pendant le débouclage forcé, le marché recule de 10%. Estimez la perte.

**Solution.**

> Perte ≈ 10% × 50 Md€ = 5 Md€

soit l'ordre de grandeur de la perte réelle (~4,9 Md€), la différence tenant au profil de débouclage.
**Leçon (§9.6.2).** Un notionnel **brut** dissimulé de 50 Md€ suffit à transformer une baisse ordinaire de 10% en une perte de 5 Md€. D'où l'impératif de surveiller les limites en brut et non seulement en net.

## Exercice 10 — Bank run et coussin de liquidité

**Énoncé.** Une banque détient 40 Md$ de dépôts et 8 Md$ d'actifs liquides de haute qualité. Un run provoque le retrait de 30% des dépôts en une semaine. La banque peut-elle y faire face ?

**Solution.** Sortie de dépôts :

> 0,30 × 40 = 12 Md$

Actifs liquides disponibles = 8 Md$. Déficit :

> 12 − 8 = 4 Md$

La banque doit combler 4 Md$ par des **ventes forcées** (à décote) ou un **soutien de banque centrale**.
**Leçon (§9.3).** Un coussin de 8 Md$ ne couvre pas une sortie de 12 Md$. La liquidité de financement et la liquidité de marché se combinent : les ventes forcées dans un marché stressé aggravent les pertes (spirale de liquidité).

## Exercice 11 — Naked puts et risque de queue (Niederhoffer)

**Énoncé.** Un gérant vend 10 000 options de vente (puts) sur indice, strike 900, prime encaissée 5 points, multiplicateur 100 $/point. Un krach fait tomber l'indice à 800. Comparez la prime encaissée à la perte.

**Solution.** Prime totale encaissée :

> 10 000 × 5 × 100 = 5 M$

Au strike 900 avec l'indice à 800, la valeur intrinsèque du put est 900 − 800 = 100 points. Perte nette par put = (100 − 5)× 100 = 9 500 $. Perte totale :

> 10 000 × 9 500 = 95 M$

Rapport perte/prime :

> (95) ÷ (5) = 19×

**Leçon (§9.5.3).** La vente d'options à découvert encaisse de petites primes régulières mais expose à une perte de queue démesurée : ici 19 fois la prime. Le modèle qui ignore les krachs sous-estime gravement ce risque.

## Exercice 12 — Convergence trade à levier qui diverge (LTCM)

**Énoncé.** Un fonds parie sur la **convergence** d'un spread obligataire de 20 bp vers zéro. La sensibilité combinée de la position est telle qu'un mouvement de 1 bp génère 5 M$ de résultat. En crise, le spread **diverge** de 20 à 50 bp. Calculez la perte et comparez-la à un capital alloué de 100 M$.

**Solution.** Mouvement adverse :

> Δ = 50 − 20 = +30 bp (divergence)

Perte :

> 30 bp × 5 M$/bp = 150 M$

Rapportée au capital alloué :

> (150) ÷ (100) = 150% ⇒ le capital du trade est effacé, avec appels de marge supplémentaires.

**Leçon (§9.5.1).** Parier sur la convergence en supposant que les spreads ne peuvent que se resserrer ignore le scénario de divergence en crise. Avec levier, la perte (150 M$) dépasse le capital (100 M$) et force un débouclage à perte — le cœur de l'effondrement de LTCM.

# Partie D — Conseils méthodologiques

## D.1 La grille d'analyse en quatre temps

Pour tout cas, l'examen attend moins des dates que la **maîtrise du mécanisme**. Appliquez systématiquement la même grille, qui structure aussi bien la révision que la réponse aux QCM :

1. **Contexte et stratégie** — quel était le modèle d'affaires ou la position ? (une caisse d'épargne prêtant long à taux fixe ; un fonds en arbitrage de convergence à fort levier ; un trader cumulant front et back office).
2. **Mécanisme du risque** — quel risque s'est matérialisé et par quel canal ? (hausse des taux ; divergence des spreads ; appels de marge ; gel du financement).
3. **Défaillances de contrôle et de gouvernance** — pourquoi la perte a-t-elle pu croître au lieu d'être arrêtée ? (absence de séparation des tâches ; limites surveillées en net ; modèle manipulé ; conflits d'intérêts ; opacité comptable).
4. **Leçon et catégorie** — quelle règle de gestion des risques en tirer, et à quelle catégorie rattacher le cas ?

> **Le réflexe « mécanisme, pas anecdote »**
>
> Face à un cas, ne récitez pas une chronologie : identifiez le **risque dominant**, la **défaillance de contrôle** qui l'a laissé croître et la **leçon**. Deux cas très différents (Barings et Daiwa) partagent la même leçon (séparation des tâches) ; un même cas (Metallgesellschaft) illustre plusieurs risques (liquidité de financement, base, rollover). C'est cette lecture qui est testée.

## D.2 Rattacher chaque cas à sa catégorie

Le tableau suivant condense l'essentiel. Sachez, pour chaque catégorie, citer le **cas repère** et la **leçon clé**.

| **Catégorie de risque** | **Cas repères** | **Leçon clé** |
| --- | --- | --- |
| Risque de taux | Savings & Loan (années 1980) | Gérer le *duration gap* : emprunter court et prêter long à taux fixe expose à une hausse des taux. |
| Liquidité de financement | Lehman, Continental Illinois, Northern Rock | Ne pas porter des actifs longs ou illiquides avec un financement court ou instable. |
| Couvertures mal maîtrisées | Metallgesellschaft, Amaranth, Ashanti | Une couverture peut créer un risque de trésorerie/liquidité pire que le risque couvert. |
| Risque de modèle | LTCM, London Whale, Niederhoffer | Attention aux hypothèses (corrélation, liquidité, queues) et à la manipulation des modèles. |
| Rogue trading | Barings, Société Générale, AIB, UBS, Daiwa | Séparation des tâches et réconciliation indépendante ; limites surveillées en brut. |
| Produits complexes | Bankers Trust, Orange County, Sachsen LB | Complexité, expositions hors bilan et adéquation (*suitability*) sont des angles morts. |
| Risque de réputation | Volkswagen (dieselgate) | La conduite et la culture protègent un capital réputationnel fragile. |
| Gouvernance d'entreprise | Enron, WorldCom, Madoff | Transparence, contrôles, audit crédible et tiers indépendants. |
| Cyber-risque | Attaque SWIFT (Bangladesh Bank) | La sécurité vaut celle du maillon le plus faible ; détecter et surveiller. |

## D.3 Les pièges classiques à l'examen

Plusieurs erreurs récurrentes coûtent des points. **Confondre les deux liquidités** : Metallgesellschaft et Lehman relèvent de la liquidité de *financement* (trésorerie), tandis qu'Amaranth relève surtout de la liquidité de *marché* (impossibilité de déboucler). **Attribuer la mauvaise leçon** : Barings n'est pas d'abord une erreur de marché mais un défaut de *séparation des tâches* ; LTCM n'est pas qu'un excès de levier mais un *risque de modèle*. **Croire qu'une couverture est sans risque** : le stack-and-roll a coulé Metallgesellschaft alors que la couverture était économiquement correcte. **Négliger le hors-bilan** : Enron et Sachsen LB rappellent que le danger se cache souvent en dehors du bilan. **Oublier la culture** : Volkswagen et Madoff montrent que la gouvernance et l'éthique sont des déterminants de risque de premier ordre, non des sujets accessoires.

## D.4 Mémoriser efficacement : cas repères et thèmes transversaux

Plutôt que d'apprendre les cas un à un, **regroupez-les par catégorie** et retenez un **cas repère** par catégorie (le plus emblématique), puis rattachez les autres par différence. Mémorisez ensuite les **six thèmes transversaux** qui reviennent partout : levier excessif, financement court/instable, concentration, hypothèses de modèle fausses, défaillance de contrôle interne, gouvernance et culture déficientes. Un cas se « reconstruit » alors à partir de ces briques : *Lehman = levier + financement court + actifs illiquides* ; *LTCM = levier + modèle + liquidité* ; *Barings = contrôle interne (séparation des tâches)*. Cette approche par briques est bien plus robuste sous la pression de l'examen que la mémorisation d'anecdotes isolées.

> **⚑ Point d'examen —** Méthode de révision : **(1)** un cas repère par catégorie ; **(2)** les autres cas rattachés par différence ; **(3)** les six thèmes transversaux comme briques de reconstruction. À l'examen, lisez l'énoncé en cherchant **le risque dominant** et **la défaillance de contrôle** — la bonne réponse en découle presque toujours.

## D.5 Méthode sur les QCM de type « cas »

Sur une question de cas, procédez en trois temps. D'abord, **identifiez la catégorie** évoquée par l'énoncé (mots-clés : « financement au jour le jour » → liquidité ; « cumul front/back » → contrôle interne ; « corrélations » → modèle). Ensuite, **reformulez la leçon** attendue avant de lire les options, pour ne pas être influencé par des distracteurs plausibles. Enfin, **éliminez** les réponses qui décrivent un vrai principe mais **hors sujet** pour ce cas précis (un piège fréquent : proposer une leçon correcte en soi mais rattachée au mauvais désastre). En cas de doute entre deux options, préférez celle qui nomme le **mécanisme dominant** du cas plutôt qu'un facteur secondaire.

# Partie E — 150 questions à choix multiple

*Questions originales de style examen. GARP ne publiant aucune annale, aucune n'est tirée d'une session passée. Réponses commentées en Partie F, avec renvois aux sections (§), cas et exercices.*

## E.1 Méthodologie de l'analyse des désastres (Q1 à Q8)

**Q1.** L'objectif d'étudier les catastrophes financières dans le cursus FRM est —

- **(A)** de mémoriser les dates et les montants exacts
- **(B)** de dramatiser l'histoire financière
- **(C)** d'extraire de chaque cas la défaillance de gestion des risques et la leçon transposable
- **(D)** de désigner des coupables
**Q2.** Ce que l'examen attend principalement d'un cas est —

- **(A)** la chronologie détaillée
- **(B)** la compréhension du mécanisme du risque et de la défaillance de contrôle
- **(C)** le nom de tous les dirigeants
- **(D)** le cours de bourse au jour près
**Q3.** La grille d'analyse recommandée comporte quatre temps :

- **(A)** contexte/stratégie, mécanisme du risque, défaillances de contrôle, leçon
- **(B)** achat, vente, profit, perte
- **(C)** passé, présent, futur, conclusion
- **(D)** marché, crédit, opérationnel, liquidité
**Q4.** Qu'un même mécanisme traverse plusieurs cas signifie que —

- **(A)** les cas sont sans rapport entre eux
- **(B)** seul le hasard explique les faillites
- **(C)** chaque cas est unique et non généralisable
- **(D)** une même défaillance (ex. séparation des tâches) se retrouve dans des désastres distincts
**Q5.** Le fait que beaucoup de cas relèvent de plusieurs catégories de risque —

- **(A)** rend l'analyse impossible
- **(B)** est précisément ce qui les rend instructifs
- **(C)** prouve qu'il n'y a pas de leçon
- **(D)** est une erreur de classification
**Q6.** Parmi les causes transversales récurrentes des désastres figure —

- **(A)** un excès de prudence
- **(B)** une diversification excessive
- **(C)** le levier excessif et la défaillance de contrôle interne
- **(D)** une réglementation trop stricte
**Q7.** La gestion des risques, à la lumière des désastres, est d'abord —

- **(A)** une affaire de formules mathématiques
- **(B)** une question de chance
- **(C)** un problème purement comptable
- **(D)** une affaire de structures, de contrôles et de culture
**Q8.** Un piège classique à l'examen consiste à —

- **(A)** lire attentivement l'énoncé
- **(B)** attribuer la mauvaise leçon ou confondre deux cas
- **(C)** identifier le risque dominant
- **(D)** relier un cas à sa catégorie
## E.2 Risque de taux : Savings & Loan (Q9 à Q20)

**Q9.** Le modèle d'affaires des caisses d'épargne (S&L) reposait sur —

- **(A)** des prêts hypothécaires à taux fixe et long terme financés par des dépôts à court terme
- **(B)** des prêts courts financés par des obligations longues
- **(C)** uniquement des fonds propres
- **(D)** des placements en actions
**Q10.** La catégorie de risque illustrée par la crise des S&L est —

- **(A)** le risque de réputation
- **(B)** le risque de taux d'intérêt
- **(C)** le risque cyber
- **(D)** le risque de fraude
**Q11.** Le mismatch au cœur du désastre S&L est —

- **(A)** une duration du passif supérieure à celle de l'actif
- **(B)** une absence totale de duration
- **(C)** une duration de l'actif supérieure à celle du passif
- **(D)** une égalité parfaite des durations
**Q12.** Le déclencheur de la crise des S&L fut —

- **(A)** une baisse des taux
- **(B)** une forte hausse des taux (lutte de la Fed contre l'inflation)
- **(C)** une cyber-attaque
- **(D)** une fraude comptable
**Q13.** L'effet de la hausse des taux sur les S&L fut —

- **(A)** une amélioration de la marge
- **(B)** aucun effet
- **(C)** une baisse du coût des dépôts
- **(D)** un coût des dépôts dépassant le rendement des prêts, d'où une marge négative
**Q14.** L'aléa moral dans la crise des S&L venait de —

- **(A)** la garantie des dépôts, incitant des caisses insolvables à parier gros (gamble for resurrection)
- **(B)** l'absence de toute garantie
- **(C)** une supervision trop stricte
- **(D)** l'interdiction de prêter
**Q15.** La crise des S&L a été aggravée par —

- **(A)** un renforcement de la supervision
- **(B)** une déréglementation sans supervision adéquate
- **(C)** une interdiction des dépôts
- **(D)** une baisse des taux
**Q16.** Le duration gap se calcule comme —

- **(A)** D(A) + D(L)
- **(B)** D(A) × D(L)
- **(C)** D(A) − (L) ÷ (A) D(L)
- **(D)** (D(A)) ÷ (D(L))
**Q17.** Pour réduire son risque de taux, une caisse aurait pu —

- **(A)** augmenter la duration de ses actifs
- **(B)** entrer dans un swap où elle reçoit du fixe et paie du variable
- **(C)** acheter davantage de prêts à taux fixe
- **(D)** supprimer ses fonds propres
**Q18.** La leçon principale du cas S&L est —

- **(A)** d'éviter toute activité de prêt
- **(B)** de maximiser le levier
- **(C)** d'ignorer la courbe des taux
- **(D)** de gérer le duration gap et l'asset-liability mismatch
**Q19.** « Emprunter court et prêter long » caractérise —

- **(A)** un arbitrage sans risque
- **(B)** le risque de transformation (de taux)
- **(C)** une couverture parfaite
- **(D)** une stratégie neutre au risque
**Q20.** Le coût final de la crise des S&L pour le contribuable américain s'est chiffré en —

- **(A)** quelques millions de dollars
- **(B)** zéro (autofinancé)
- **(C)** centaines de milliards de dollars
- **(D)** quelques milliers de dollars
## E.3 Liquidité de financement : Lehman, Continental Illinois, Northern Rock (Q21 à Q40)

**Q21.** Le risque de liquidité de financement (funding liquidity risk) est —

- **(A)** le risque de perte de change
- **(B)** l'incapacité à se refinancer pour honorer ses échéances de trésorerie
- **(C)** le risque de hausse des taux
- **(D)** le risque de fraude interne
**Q22.** Le risque de liquidité de marché se distingue en ce qu'il est —

- **(A)** l'incapacité à céder un actif sans forte décote
- **(B)** identique au risque de financement
- **(C)** un risque de taux
- **(D)** un risque de réputation
**Q23.** Le levier de Lehman Brothers était de l'ordre de —

- **(A)** 2 pour 1
- **(B)** 5 pour 1
- **(C)** 30 pour 1
- **(D)** 100 pour 1
**Q24.** Lehman dépendait principalement, pour son financement, de —

- **(A)** dépôts de détail assurés
- **(B)** financements repo de très court terme
- **(C)** émissions d'actions
- **(D)** subventions publiques
**Q25.** Le « Repo 105 » de Lehman servait à —

- **(A)** lever des fonds propres
- **(B)** couvrir le risque de taux
- **(C)** diversifier les actifs
- **(D)** masquer temporairement le levier aux dates d'arrêté comptable
**Q26.** La faillite de Lehman en septembre 2008 fut —

- **(A)** sans conséquence
- **(B)** la plus grande faillite de l'histoire des États-Unis
- **(C)** rapidement annulée
- **(D)** purement européenne
**Q27.** La combinaison fatale chez Lehman associait —

- **(A)** levier excessif, financement court terme et actifs illiquides
- **(B)** faible levier et actifs liquides
- **(C)** financement long terme et fonds propres élevés
- **(D)** absence de dette
**Q28.** Continental Illinois dépendait excessivement de —

- **(A)** dépôts de détail stables
- **(B)** financements de gros volatils
- **(C)** fonds propres
- **(D)** or physique
**Q29.** Le sauvetage de Continental Illinois a donné naissance à l'expression —

- **(A)** « trop petit pour survivre »
- **(B)** « buy and hold »
- **(C)** « too big to fail »
- **(D)** « originate to distribute »
**Q30.** Les pertes de Continental Illinois provenaient surtout de —

- **(A)** pertes de change
- **(B)** prêts énergie de mauvaise qualité (rachetés à Penn Square)
- **(C)** une cyber-attaque
- **(D)** une fraude comptable
**Q31.** Par rapport aux financements de gros, les dépôts de détail assurés sont —

- **(A)** plus volatils
- **(B)** plus coûteux systématiquement
- **(C)** interdits
- **(D)** plus stables
**Q32.** Le modèle de Northern Rock reposait sur —

- **(A)** des dépôts de détail majoritaires
- **(B)** la titrisation et le financement de gros (originate-to-distribute)
- **(C)** des fonds propres élevés
- **(D)** l'absence de prêts hypothécaires
**Q33.** Le déclencheur de la chute de Northern Rock fut —

- **(A)** le gel du marché de la titrisation à l'été 2007
- **(B)** une baisse des taux
- **(C)** une fraude d'un trader
- **(D)** une cyber-attaque
**Q34.** L'événement marquant lié à Northern Rock fut —

- **(A)** une fusion réussie
- **(B)** une introduction en bourse
- **(C)** le premier bank run britannique depuis plus d'un siècle
- **(D)** un rachat d'actions
**Q35.** L'issue de la crise de Northern Rock fut —

- **(A)** une reprise spontanée
- **(B)** la nationalisation
- **(C)** une expansion internationale
- **(D)** aucune conséquence
**Q36.** La spirale de liquidité désigne —

- **(A)** une hausse des fonds propres
- **(B)** une baisse du levier
- **(C)** une amélioration de la liquidité
- **(D)** des ventes forcées qui baissent les prix, dégradent le collatéral et déclenchent de nouveaux appels de marge
**Q37.** En crise, les liquidités de financement et de marché —

- **(A)** s'annulent
- **(B)** se renforcent mutuellement
- **(C)** sont sans lien
- **(D)** disparaissent séparément
**Q38.** La leçon commune à Lehman, Continental Illinois et Northern Rock est —

- **(A)** de ne pas porter des actifs longs ou illiquides avec un financement court ou instable
- **(B)** d'augmenter le levier
- **(C)** de supprimer les dépôts de détail
- **(D)** d'ignorer la structure du passif
**Q39.** Un « run de gros » se caractérise par —

- **(A)** une ruée des petits déposants uniquement
- **(B)** la fuite des créanciers institutionnels et de gros
- **(C)** une hausse des dépôts
- **(D)** une intervention réglementaire
**Q40.** Dans le cas Northern Rock, l'annonce d'un soutien d'urgence a —

- **(A)** immédiatement rassuré les déposants
- **(B)** fait monter le cours
- **(C)** déclenché la panique des déposants
- **(D)** été sans effet
## E.4 Couvertures mal maîtrisées : Metallgesellschaft, Amaranth, Ashanti (Q41 à Q58)

**Q41.** La stratégie de couverture de Metallgesellschaft (MGRM) était —

- **(A)** l'achat d'obligations d'État
- **(B)** le « stack-and-roll » (futures courts roulés en continu)
- **(C)** la vente à découvert d'actions
- **(D)** l'absence de couverture
**Q42.** Le problème central de la couverture de Metallgesellschaft était —

- **(A)** un excès de fonds propres
- **(B)** une couverture trop faible
- **(C)** un mismatch de trésorerie : marge immédiate sur futures, gains différés sur contrats longs
- **(D)** l'absence de contrepartie
**Q43.** En situation de contango, le roulement des futures génère —

- **(A)** des pertes de rollover récurrentes
- **(B)** des gains automatiques
- **(C)** aucun effet
- **(D)** une couverture parfaite
**Q44.** Face à l'hémorragie de trésorerie, Metallgesellschaft a —

- **(A)** augmenté ses positions
- **(B)** liquidé ses positions au pire moment, réalisant les pertes
- **(C)** ignoré les appels de marge
- **(D)** émis des actions
**Q45.** La leçon principale de Metallgesellschaft est qu'une couverture —

- **(A)** est toujours sans risque
- **(B)** doit être maximisée
- **(C)** élimine tout besoin de trésorerie
- **(D)** peut créer un risque de liquidité de financement plus dangereux que le risque couvert
**Q46.** Le risque de liquidité en cause chez Metallgesellschaft était surtout —

- **(A)** un risque de réputation
- **(B)** la liquidité de financement (trésorerie)
- **(C)** un risque de crédit
- **(D)** un risque cyber
**Q47.** Les positions d'Amaranth étaient concentrées sur —

- **(A)** les spreads de gaz naturel
- **(B)** les obligations souveraines
- **(C)** les actions technologiques
- **(D)** les devises émergentes
**Q48.** Qu'Amaranth « était le marché » signifie qu'il —

- **(A)** fixait les taux directeurs
- **(B)** détenait une part négligeable
- **(C)** détenait une part dominante de l'open interest, rendant tout débouclage destructeur
- **(D)** ne détenait aucune position
**Q49.** La perte d'Amaranth s'est élevée à environ —

- **(A)** 50 millions de dollars
- **(B)** 6,5 milliards de dollars en une semaine
- **(C)** aucune perte
- **(D)** 100 milliards de dollars
**Q50.** Le risque dominant du cas Amaranth est —

- **(A)** le risque de taux
- **(B)** le risque de réputation
- **(C)** le risque cyber
- **(D)** la concentration et la liquidité de marché
**Q51.** Ashanti Goldfields s'était couvert par —

- **(A)** l'achat d'actions minières
- **(B)** des positions optionnelles vendues calibrées sur une baisse de l'or
- **(C)** des dépôts bancaires
- **(D)** aucune couverture
**Q52.** Le déclencheur du désastre Ashanti fut —

- **(A)** une hausse soudaine du prix de l'or (Washington Agreement)
- **(B)** une baisse de l'or
- **(C)** une fraude interne
- **(D)** une cyber-attaque
**Q53.** La conséquence pour Ashanti fut —

- **(A)** un profit exceptionnel
- **(B)** aucun effet
- **(C)** des appels de marge massifs et une quasi-faillite
- **(D)** une acquisition
**Q54.** Les positions optionnelles vendues exposent le vendeur à —

- **(A)** un gain garanti
- **(B)** des appels de marge potentiellement illimités
- **(C)** aucun risque
- **(D)** une perte plafonnée à la prime
**Q55.** Le point commun entre Metallgesellschaft et Ashanti est —

- **(A)** une fraude comptable
- **(B)** une cyber-attaque
- **(C)** un défaut souverain
- **(D)** des appels de marge en cash lors d'un mouvement adverse
**Q56.** Une couverture mal conçue peut —

- **(A)** supprimer tout risque
- **(B)** créer un risque nouveau (trésorerie, base, rollover)
- **(C)** garantir un profit
- **(D)** être toujours neutre
**Q57.** Le risque de base désigne —

- **(A)** l'écart de comportement entre l'instrument de couverture et le sous-jacent couvert
- **(B)** le risque de fraude
- **(C)** le risque de taux
- **(D)** le risque de réputation
**Q58.** La leçon centrale des couvertures mal maîtrisées est de —

- **(A)** ne jamais se couvrir
- **(B)** maximiser l'effet de levier
- **(C)** gérer la liquidité et la trésorerie de la couverture elle-même
- **(D)** ignorer le risque de base
## E.5 Risque de modèle : LTCM, London Whale, Niederhoffer (Q59 à Q78)

**Q59.** Le risque de modèle est le risque de pertes dues à —

- **(A)** une hausse des taux uniquement
- **(B)** des modèles erronés, mal calibrés ou mal utilisés
- **(C)** une fraude comptable
- **(D)** une cyber-attaque
**Q60.** La stratégie de LTCM était —

- **(A)** l'arbitrage de convergence sur des spreads
- **(B)** l'investissement en actions de croissance
- **(C)** le prêt hypothécaire
- **(D)** la conservation d'or
**Q61.** Le levier bilanciel de LTCM était de l'ordre de —

- **(A)** 2 pour 1
- **(B)** 10 pour 1
- **(C)** 25 pour 1
- **(D)** aucun levier
**Q62.** Les modèles de LTCM supposaient —

- **(A)** des marchés illiquides
- **(B)** des corrélations et une volatilité stables, des marchés liquides
- **(C)** une hausse continue des spreads
- **(D)** l'absence de levier
**Q63.** Le déclencheur de l'effondrement de LTCM fut —

- **(A)** une baisse des taux
- **(B)** une fraude interne
- **(C)** une cyber-attaque
- **(D)** le défaut russe de 1998 et la fuite vers la qualité
**Q64.** Pendant la crise de 1998, chez LTCM —

- **(A)** les spreads ont convergé comme prévu
- **(B)** les spreads ont divergé et les corrélations ont convergé vers 1
- **(C)** rien ne s'est passé
- **(D)** le levier a disparu
**Q65.** LTCM a finalement été —

- **(A)** sauvé par un consortium de banques orchestré par la Fed
- **(B)** liquidé sans conséquence
- **(C)** nationalisé
- **(D)** racheté par un particulier
**Q66.** La leçon de LTCM combine —

- **(A)** fraude comptable et cyber-risque
- **(B)** excès de prudence et sous-levier
- **(C)** risque de modèle et levier excessif ; la diversification s'évapore en crise
- **(D)** risque de réputation uniquement
**Q67.** La « Baleine de Londres » opérait au sein de —

- **(A)** la salle des marchés de Barings
- **(B)** le Chief Investment Office de JPMorgan à Londres
- **(C)** le trésor d'Orange County
- **(D)** la banque centrale du Bangladesh
**Q68.** Les positions de la Baleine de Londres portaient sur —

- **(A)** des actions minières
- **(B)** des devises
- **(C)** des matières premières agricoles
- **(D)** des indices de dérivés de crédit (CDS)
**Q69.** Pour masquer la montée du risque, la Baleine de Londres a —

- **(A)** augmenté les fonds propres
- **(B)** modifié le modèle de VaR pour en sous-estimer l'ampleur
- **(C)** vendu des actifs liquides
- **(D)** réduit ses positions
**Q70.** La perte de la Baleine de Londres a atteint environ —

- **(A)** 6,2 milliards de dollars
- **(B)** 50 millions de dollars
- **(C)** aucune perte
- **(D)** 500 milliards de dollars
**Q71.** La position de la Baleine de Londres, présentée comme couverture, était devenue —

- **(A)** un placement sans risque
- **(B)** un dépôt bancaire
- **(C)** un pari directionnel
- **(D)** une émission d'actions
**Q72.** La leçon de la Baleine de Londres porte sur —

- **(A)** le risque de taux
- **(B)** la gouvernance des modèles (validation et contrôle des changements)
- **(C)** le risque de change
- **(D)** la liquidité de détail
**Q73.** La stratégie de Niederhoffer consistait à —

- **(A)** acheter des obligations d'État
- **(B)** diversifier largement
- **(C)** investir en immobilier
- **(D)** vendre des options de vente (puts) à découvert
**Q74.** Le désastre Niederhoffer fut déclenché par —

- **(A)** une hausse des taux
- **(B)** le krach boursier d'octobre 1997
- **(C)** une cyber-attaque
- **(D)** une fraude comptable
**Q75.** Le résultat pour le fonds de Niederhoffer fut —

- **(A)** son anéantissement par des appels de marge
- **(B)** un profit record
- **(C)** aucune conséquence
- **(D)** une introduction en bourse
**Q76.** Vendre des options à découvert (naked puts) expose surtout à —

- **(A)** un risque de taux
- **(B)** un risque de change
- **(C)** un risque de queue (perte massive lors d'un krach)
- **(D)** aucun risque
**Q77.** L'image « ramasser des pièces devant un rouleau compresseur » illustre —

- **(A)** un arbitrage sans risque
- **(B)** de petites primes régulières exposant à une perte de queue démesurée
- **(C)** une couverture parfaite
- **(D)** une diversification optimale
**Q78.** La leçon transversale du risque de modèle est que —

- **(A)** les modèles sont infaillibles
- **(B)** le levier est sans danger
- **(C)** la diversification est garantie
- **(D)** un modèle n'est pas la réalité : attention aux hypothèses et à leur manipulation
## E.6 Rogue trading et contrôle interne : Barings, Société Générale, AIB, UBS, Daiwa (Q79 à Q103)

**Q79.** Le rogue trading est avant tout un problème de —

- **(A)** risque de marché
- **(B)** contrôle interne
- **(C)** risque de taux
- **(D)** fraude comptable des dirigeants
**Q80.** Le facteur récurrent dans les affaires de rogue trading est —

- **(A)** le défaut de séparation entre front et back office
- **(B)** un excès de fonds propres
- **(C)** une réglementation trop stricte
- **(D)** une couverture trop importante
**Q81.** Chez Barings, Nick Leeson cumulait —

- **(A)** les fonctions d'audit et de conformité
- **(B)** les rôles de client et de banquier
- **(C)** les fonctions de front et de back office
- **(D)** les postes de PDG et de trésorier
**Q82.** Les positions non autorisées de Leeson portaient sur —

- **(A)** des futures sur l'indice Nikkei
- **(B)** des obligations souveraines américaines
- **(C)** des devises africaines
- **(D)** des actions technologiques
**Q83.** Leeson dissimulait ses pertes dans —

- **(A)** un compte offshore aux îles Caïmans
- **(B)** le compte d'erreur « 88888 »
- **(C)** un fonds de pension
- **(D)** un coffre physique
**Q84.** Le choc externe qui a révélé les pertes de Barings fut —

- **(A)** une hausse des taux américains
- **(B)** un défaut souverain
- **(C)** le séisme de Kobe
- **(D)** une cyber-attaque
**Q85.** La perte de Barings s'est élevée à environ —

- **(A)** 50 millions de livres
- **(B)** 10 milliards de livres
- **(C)** 2 millions de livres
- **(D)** 827 millions de livres
**Q86.** La conséquence pour Barings fut —

- **(A)** une amende symbolique
- **(B)** la faillite de la plus ancienne banque marchande britannique
- **(C)** une fusion réussie
- **(D)** aucune conséquence
**Q87.** La leçon emblématique de Barings est que —

- **(A)** la séparation des tâches (front/back) est non négociable
- **(B)** il faut augmenter le levier
- **(C)** les futures doivent être interdits
- **(D)** la diversification est inutile
**Q88.** Jérôme Kerviel, à la Société Générale, était un ancien du —

- **(A)** service marketing
- **(B)** département juridique
- **(C)** back office, dont il connaissait les contrôles
- **(D)** service informatique
**Q89.** Kerviel masquait ses positions par —

- **(A)** des comptes à l'étranger
- **(B)** des transactions fictives compensant en apparence ses positions
- **(C)** la destruction de documents
- **(D)** un chiffrement informatique
**Q90.** Le notionnel des positions non autorisées de Kerviel atteignait environ —

- **(A)** 500 millions d'euros
- **(B)** 5 milliards d'euros
- **(C)** 50 milliards d'euros
- **(D)** 500 milliards d'euros
**Q91.** La perte finale de la Société Générale fut d'environ —

- **(A)** 500 millions d'euros
- **(B)** 50 millions d'euros
- **(C)** 20 milliards d'euros
- **(D)** 4,9 milliards d'euros
**Q92.** La leçon du cas Kerviel sur les limites est de —

- **(A)** ne surveiller que les positions nettes
- **(B)** surveiller les positions en brut, pas seulement en net
- **(C)** supprimer les limites
- **(D)** augmenter les limites
**Q93.** La perte de la Société Générale a été cristallisée par —

- **(A)** le débouclage forcé des positions en pleine baisse des marchés
- **(B)** une fraude comptable
- **(C)** une cyber-attaque
- **(D)** un défaut souverain
**Q94.** John Rusnak, chez Allied Irish Banks (Allfirst), a subi des pertes de —

- **(A)** crédit immobilier
- **(B)** taux d'intérêt
- **(C)** change (FX)
- **(D)** matières premières
**Q95.** Rusnak dissimulait ses pertes par —

- **(A)** des comptes offshore
- **(B)** de faux contrats d'options
- **(C)** la corruption d'auditeurs
- **(D)** un logiciel malveillant
**Q96.** La perte du cas Rusnak (AIB) s'élevait à environ —

- **(A)** 50 millions de dollars
- **(B)** 5 milliards de dollars
- **(C)** 691 millions de dollars
- **(D)** 10 milliards de dollars
**Q97.** Kweku Adoboli, chez UBS, opérait au desk —

- **(A)** des matières premières
- **(B)** du crédit souverain
- **(C)** des changes
- **(D)** des ETF à Londres
**Q98.** La perte du cas Adoboli (UBS) atteignait environ —

- **(A)** 200 millions de dollars
- **(B)** 2,3 milliards de dollars
- **(C)** 20 milliards de dollars
- **(D)** aucune perte
**Q99.** Toshihide Iguchi, chez Daiwa, a dissimulé ses pertes pendant environ —

- **(A)** quelques semaines
- **(B)** deux ans
- **(C)** onze ans
- **(D)** trente ans
**Q100.** La perte du cas Iguchi (Daiwa) s'élevait à environ —

- **(A)** 1,1 milliard de dollars
- **(B)** 50 millions de dollars
- **(C)** 100 milliards de dollars
- **(D)** aucune perte
**Q101.** La cause structurelle commune à Barings et Daiwa était —

- **(A)** une fraude comptable des dirigeants
- **(B)** une cyber-attaque
- **(C)** un défaut souverain
- **(D)** le cumul des fonctions front et back office
**Q102.** Le contrôle qui aurait détecté ces fraudes plus tôt est —

- **(A)** une hausse des fonds propres
- **(B)** la réconciliation indépendante des opérations
- **(C)** une baisse des limites de marché
- **(D)** l'achat d'assurance
**Q103.** Un contrôle anti-fraude simple mais efficace consiste à —

- **(A)** augmenter les bonus
- **(B)** supprimer les congés
- **(C)** imposer des congés obligatoires et faire tourner les postes
- **(D)** centraliser tous les pouvoirs sur un trader
## E.7 Ingénierie financière et produits complexes : Bankers Trust, Orange County, Sachsen LB (Q104 à Q118)

**Q104.** Les clients emblématiques lésés par Bankers Trust furent —

- **(A)** Gibson Greetings et Procter & Gamble
- **(B)** deux banques centrales
- **(C)** des particuliers uniquement
- **(D)** des fonds souverains
**Q105.** Les enregistrements internes de Bankers Trust ont révélé —

- **(A)** une fraude comptable
- **(B)** une cyber-attaque
- **(C)** l'exploitation de la mauvaise compréhension des produits par les clients
- **(D)** un délit d'initié
**Q106.** Le risque dominant du cas Bankers Trust est le risque de —

- **(A)** taux d'intérêt
- **(B)** conduite et de réputation
- **(C)** liquidité
- **(D)** change
**Q107.** La notion clé illustrée par Bankers Trust est —

- **(A)** le duration gap
- **(B)** le levier
- **(C)** la titrisation
- **(D)** l'adéquation des produits vendus (suitability)
**Q108.** Le trésorier d'Orange County avait investi le portefeuille public en —

- **(A)** prises en pension inversées (reverse repos) à effet de levier
- **(B)** actions de croissance
- **(C)** or physique
- **(D)** dépôts à terme
**Q109.** Le pari implicite du portefeuille d'Orange County était —

- **(A)** une hausse des actions
- **(B)** une baisse des taux d'intérêt
- **(C)** une hausse de l'or
- **(D)** une baisse du dollar
**Q110.** Le déclencheur du désastre d'Orange County fut —

- **(A)** une fraude interne
- **(B)** une cyber-attaque
- **(C)** la hausse des taux d'intérêt de 1994
- **(D)** une baisse des taux
**Q111.** La conséquence pour Orange County fut —

- **(A)** un profit exceptionnel
- **(B)** aucune conséquence
- **(C)** une simple réprimande
- **(D)** une perte d'environ 1,7 milliard de dollars et la faillite du comté
**Q112.** La défaillance clé du cas Orange County était —

- **(A)** un excès de fonds propres
- **(B)** l'absence de gouvernance du risque (trésorier sans contre-pouvoir)
- **(C)** une réglementation trop stricte
- **(D)** une couverture excessive
**Q113.** Sachsen Landesbank avait logé ses expositions subprime dans —

- **(A)** des conduits/SIV hors bilan
- **(B)** son bilan principal exclusivement
- **(C)** un fonds de pension
- **(D)** des comptes de dépôt
**Q114.** Les actifs logés dans les véhicules de Sachsen LB étaient —

- **(A)** des obligations d'État allemandes
- **(B)** de l'or
- **(C)** du subprime américain
- **(D)** des actions européennes
**Q115.** Ces véhicules hors bilan étaient financés par —

- **(A)** des fonds propres
- **(B)** du papier commercial adossé à des actifs (ABCP) à court terme
- **(C)** des dépôts de détail
- **(D)** des obligations à 30 ans
**Q116.** Le déclencheur du désastre Sachsen LB fut —

- **(A)** le gel du marché ABCP en 2007
- **(B)** une baisse des taux
- **(C)** une fraude d'un trader
- **(D)** une hausse de l'or
**Q117.** La conséquence pour Sachsen LB fut —

- **(A)** un profit record
- **(B)** une introduction en bourse
- **(C)** aucune conséquence
- **(D)** le soutien forcé de la maison mère, précipitant son absorption
**Q118.** La leçon des cas de produits complexes est que —

- **(A)** la complexité élimine le risque
- **(B)** le hors-bilan est sans danger
- **(C)** complexité, expositions hors bilan et suitability sont des angles morts classiques
- **(D)** les collectivités ne peuvent pas faire faillite
## E.8 Risque de réputation : Volkswagen (Q119 à Q126)

**Q119.** La fraude de Volkswagen (« dieselgate ») reposait sur —

- **(A)** un logiciel truqueur (defeat device)
- **(B)** une fraude comptable
- **(C)** un délit d'initié
- **(D)** une cyber-attaque
**Q120.** Le dispositif de Volkswagen faussait —

- **(A)** les comptes annuels
- **(B)** les cours de bourse
- **(C)** les tests d'émissions des moteurs diesel
- **(D)** les taux d'intérêt
**Q121.** L'affaire Volkswagen a éclaté en —

- **(A)** 2008
- **(B)** 2015 (révélation par l'agence américaine EPA)
- **(C)** 2020
- **(D)** 1998
**Q122.** Le risque dominant du cas Volkswagen est le risque de —

- **(A)** liquidité
- **(B)** taux
- **(C)** crédit
- **(D)** réputation
**Q123.** Le risque sous-jacent à la réputation, chez Volkswagen, était le risque de —

- **(A)** marché
- **(B)** conduite (conduct risk)
- **(C)** change
- **(D)** modèle
**Q124.** Le déterminant profond du désastre Volkswagen relève de —

- **(A)** la liquidité de financement
- **(B)** la duration
- **(C)** la culture et de la gouvernance de l'entreprise
- **(D)** la titrisation
**Q125.** La leçon du cas Volkswagen sur le capital réputationnel est qu'il —

- **(A)** est indestructible
- **(B)** n'a aucune valeur
- **(C)** se reconstitue instantanément
- **(D)** est fragile : des années à bâtir, des heures à détruire
**Q126.** Les conséquences pour Volkswagen ont inclus —

- **(A)** des amendes massives, des rappels et l'effondrement du cours
- **(B)** un profit exceptionnel
- **(C)** aucune conséquence financière
- **(D)** une hausse durable de la marque
## E.9 Gouvernance d'entreprise : Enron, WorldCom, Madoff (Q127 à Q142)

**Q127.** Enron dissimulait sa dette au moyen de —

- **(A)** comptes offshore personnels
- **(B)** entités ad hoc (SPE) hors bilan
- **(C)** faux contrats d'options
- **(D)** un logiciel malveillant
**Q128.** La méthode comptable agressive d'Enron était —

- **(A)** la comptabilisation en valeur de marché (mark-to-market) de contrats longs
- **(B)** l'amortissement linéaire
- **(C)** la comptabilité de caisse
- **(D)** la méthode LIFO
**Q129.** Le conflit d'intérêts majeur chez Enron impliquait —

- **(A)** un auditeur externe indépendant
- **(B)** un régulateur
- **(C)** le directeur financier dirigeant certaines des entités
- **(D)** un actionnaire minoritaire
**Q130.** L'affaire Enron a entraîné —

- **(A)** une simple amende
- **(B)** la chute de son auditeur, Arthur Andersen
- **(C)** aucune conséquence pour les tiers
- **(D)** une fusion réussie
**Q131.** L'affaire Enron a directement inspiré —

- **(A)** les accords de Bâle II
- **(B)** la directive Solvabilité II
- **(C)** le règlement MiFID
- **(D)** la loi Sarbanes-Oxley
**Q132.** Le risque dominant du cas Enron est le risque de —

- **(A)** gouvernance (opacité, conflits d'intérêts)
- **(B)** taux d'intérêt
- **(C)** liquidité de marché
- **(D)** change
**Q133.** La fraude de WorldCom consistait à —

- **(A)** vendre des dérivés complexes
- **(B)** manipuler des devises
- **(C)** capitaliser des charges d'exploitation courantes (les inscrire à l'actif)
- **(D)** dissimuler des positions de trading
**Q134.** La fraude comptable de WorldCom portait sur environ —

- **(A)** 100 millions de dollars
- **(B)** 11 milliards de dollars
- **(C)** 500 milliards de dollars
- **(D)** 1 million de dollars
**Q135.** La leçon du cas WorldCom est la nécessité de —

- **(A)** maximiser le levier
- **(B)** supprimer l'audit interne
- **(C)** ignorer les contrôles
- **(D)** contrôles internes et d'un audit robustes
**Q136.** Le schéma de Bernard Madoff était —

- **(A)** un schéma de Ponzi
- **(B)** un arbitrage de convergence
- **(C)** une couverture stack-and-roll
- **(D)** une titrisation
**Q137.** L'ampleur de la fraude Madoff atteignait de l'ordre de —

- **(A)** 500 millions de dollars
- **(B)** 50 millions de dollars
- **(C)** 65 milliards de dollars (valeurs fictives)
- **(D)** 1 milliard de dollars
**Q138.** Un signal d'alarme (red flag) majeur chez Madoff était —

- **(A)** une forte volatilité des rendements
- **(B)** des rendements anormalement réguliers et positifs
- **(C)** des pertes fréquentes
- **(D)** une transparence totale
**Q139.** Un autre red flag structurel du cas Madoff était —

- **(A)** un dépositaire indépendant réputé
- **(B)** un audit par un grand cabinet
- **(C)** une régulation stricte
- **(D)** l'absence de dépositaire indépendant (auto-conservation des actifs)
**Q140.** Le cas Madoff était aussi signalé par —

- **(A)** un auditeur minuscule au regard des montants gérés
- **(B)** une cotation en bourse
- **(C)** des rapports publics détaillés
- **(D)** un conseil d'administration indépendant
**Q141.** La défaillance qui a permis à la fraude Madoff de durer était —

- **(A)** un excès de régulation
- **(B)** une transparence excessive
- **(C)** la faillite de la due diligence des investisseurs et des régulateurs
- **(D)** un audit trop rigoureux
**Q142.** La leçon transversale de la gouvernance est —

- **(A)** d'ignorer les tiers
- **(B)** transparence, contrôles, audit crédible et tiers indépendants ; méfiance envers le « trop beau pour être vrai »
- **(C)** de supprimer les auditeurs
- **(D)** de maximiser l'opacité
## E.10 Cyber-risque : l'attaque SWIFT (Q143 à Q150)

**Q143.** La cible de l'attaque SWIFT de 2016 était —

- **(A)** la banque centrale du Bangladesh
- **(B)** une banque d'investissement américaine
- **(C)** une bourse européenne
- **(D)** un fonds souverain
**Q144.** L'attaque a été menée par —

- **(A)** une simple erreur humaine
- **(B)** une panne technique
- **(C)** un logiciel malveillant émettant des ordres de paiement SWIFT frauduleux
- **(D)** un défaut de liquidité
**Q145.** Le montant effectivement détourné était d'environ —

- **(A)** 1 milliard de dollars
- **(B)** 81 millions de dollars
- **(C)** aucun montant
- **(D)** 10 milliards de dollars
**Q146.** Ce qui a permis de bloquer la majeure partie des transferts fut —

- **(A)** une intervention policière immédiate
- **(B)** un pare-feu du réseau SWIFT
- **(C)** une alerte de la banque centrale
- **(D)** une faute d'orthographe dans un ordre, éveillant les soupçons
**Q147.** Concernant le réseau SWIFT lui-même, l'attaque a montré qu'il —

- **(A)** avait été entièrement compromis
- **(B)** n'a pas été compromis : la faille était côté participant (contrôles insuffisants)
- **(C)** était obsolète
- **(D)** avait été fermé
**Q148.** La catégorie de risque illustrée par l'attaque SWIFT est le risque —

- **(A)** opérationnel (cyber)
- **(B)** de taux d'intérêt
- **(C)** de change
- **(D)** de réputation uniquement
**Q149.** L'attaque SWIFT illustre aussi une dimension potentiellement —

- **(A)** purement locale
- **(B)** négligeable
- **(C)** systémique, via une infrastructure partagée
- **(D)** sans conséquence
**Q150.** La leçon principale de l'attaque SWIFT est que —

- **(A)** le cyber-risque n'existe pas
- **(B)** les banques centrales sont invulnérables
- **(C)** la technologie seule suffit
- **(D)** la sécurité vaut celle du maillon le plus faible ; détection et surveillance sont critiques
# Partie F — Corrigé commenté

*Chaque réponse renvoie aux sections (§) du cours, aux études de cas et aux exercices d'application correspondants.*

## F.1 Corrigé — Q1 à Q40 (méthodologie, risque de taux, liquidité de financement)

**Q1 — C** *(§9.1.1)* Étudier les désastres sert à **extraire la défaillance de gestion des risques et la leçon transposable**, non à mémoriser des dates.

**Q2 — B** *(§9.1.2)* L'examen teste la **compréhension du mécanisme** et de la défaillance de contrôle, pas la chronologie.

**Q3 — A** *(§9.1.2 · Partie D)* La grille en quatre temps : **contexte/stratégie, mécanisme, défaillances de contrôle, leçon**.

**Q4 — D** *(§9.1.1)* Un même mécanisme (ex. **séparation des tâches**) se retrouve dans des désastres distincts (Barings, Daiwa).

**Q5 — B** *(§9.1.3)* Le chevauchement de catégories est **ce qui rend les cas instructifs** : les causes se combinent.

**Q6 — C** *(§9.11)* Parmi les causes transversales figurent le **levier excessif** et la **défaillance de contrôle interne**.

**Q7 — D** *(§9.11)* La gestion des risques est d'abord affaire de **structures, contrôles et culture**, pas seulement de formules.

**Q8 — B** *(§D.3)* Piège classique : **attribuer la mauvaise leçon** ou confondre deux cas.

**Q9 — A** *(§9.2.1 · cas 1)* Modèle S&L : **prêts hypothécaires longs à taux fixe** financés par des **dépôts courts**.

**Q10 — B** *(§9.2 · cas 1)* Cas canonique du **risque de taux d'intérêt**.

**Q11 — C** *(§9.2.1 · ex. 1)* Le mismatch : **duration de l'actif supérieure à celle du passif** (DGAP > 0).

**Q12 — B** *(§9.2.2 · cas 6)* Déclencheur : **forte hausse des taux** (désinflation Volcker).

**Q13 — D** *(§9.2.2)* Effet : le **coût des dépôts dépasse le rendement des prêts anciens**, d'où une marge négative.

**Q14 — A** *(§9.2.2 · cas 3)* Aléa moral : la **garantie des dépôts** pousse des caisses insolvables au *gamble for resurrection*.

**Q15 — B** *(§9.2.2 · cas 4)* Aggravation par la **déréglementation sans supervision** adéquate.

**Q16 — C** *(§9.2.3 · ex. 1)* Duration gap : DGAP = D(A) − (L) ÷ (A) D(L).

**Q17 — B** *(§9.2.3 · cas 5)* Un **swap recevant du fixe et payant du variable** réduit le duration gap.

**Q18 — D** *(§9.2.3)* Leçon : **gérer le duration gap et l'asset-liability mismatch**.

**Q19 — B** *(§9.2 · cas 2)* « Emprunter court, prêter long » = **risque de transformation** (de taux).

**Q20 — C** *(§9.2.2)* Coût final : **centaines de milliards de dollars** pour le contribuable américain.

**Q21 — B** *(§9.3)* Le *funding liquidity risk* est l'**incapacité à se refinancer** pour honorer ses échéances.

**Q22 — A** *(§9.3 · definition)* Le *market liquidity risk* est l'**incapacité à céder un actif sans forte décote**.

**Q23 — C** *(§9.3.1 · cas 7)* Levier de Lehman de l'ordre de **30 pour 1**.

**Q24 — B** *(§9.3.1 · cas 7)* Financement dominé par le **repo de très court terme**.

**Q25 — D** *(§9.3.1 · cas 8)* Le « Repo 105 » servait à **masquer le levier** aux dates d'arrêté comptable.

**Q26 — B** *(§9.3.1)* La faillite de Lehman fut **la plus grande de l'histoire des États-Unis**.

**Q27 — A** *(§9.3.1 · cas 7)* Combinaison fatale : **levier + financement court + actifs illiquides**.

**Q28 — B** *(§9.3.2 · cas 9)* Continental Illinois dépendait de **financements de gros volatils**.

**Q29 — C** *(§9.3.2 · cas 9)* Son sauvetage a donné naissance à « **too big to fail** ».

**Q30 — B** *(§9.3.2)* Pertes issues de **prêts énergie de mauvaise qualité** (rachetés à Penn Square).

**Q31 — D** *(§9.3.2 · cas 10)* Les **dépôts de détail assurés sont plus stables** que les financements de gros.

**Q32 — B** *(§9.3.3 · cas 11)* Northern Rock : **titrisation et financement de gros** (*originate-to-distribute*).

**Q33 — A** *(§9.3.3 · cas 11)* Déclencheur : **gel du marché de la titrisation** (été 2007).

**Q34 — C** *(§9.3.3 · cas 12)* **Premier bank run britannique depuis plus d'un siècle**.

**Q35 — B** *(§9.3.3)* Issue : **nationalisation**.

**Q36 — D** *(§9.3 · cas 14)* Spirale de liquidité : **ventes forcées** → baisse des prix → collatéral dégradé → nouveaux appels de marge.

**Q37 — B** *(§9.3 · cas 13)* En crise, les deux liquidités **se renforcent mutuellement**.

**Q38 — A** *(§9.3)* Leçon commune : **ne pas porter des actifs longs ou illiquides avec un financement court ou instable**.

**Q39 — B** *(§9.3.2 · cas 9)* Run de gros = **fuite des créanciers institutionnels et de gros**.

**Q40 — C** *(§9.3.3 · cas 12)* L'annonce d'un soutien d'urgence a **déclenché la panique** des déposants.

## F.2 Corrigé — Q41 à Q78 (couvertures mal maîtrisées, risque de modèle)

**Q41 — B** *(§9.4.1 · cas 15)* Metallgesellschaft : couverture par **stack-and-roll** (futures courts roulés).

**Q42 — C** *(§9.4.1 · ex. 6)* Problème central : **mismatch de trésorerie** (marge immédiate contre gains différés).

**Q43 — A** *(§9.4.1 · cas 16 · ex. 2)* En **contango**, le *roll* génère des **pertes de rollover** récurrentes.

**Q44 — B** *(§9.4.1 · cas 18)* La firme a **liquidé au pire moment**, réalisant les pertes.

**Q45 — D** *(§9.4.1)* Leçon : une couverture peut créer un **risque de liquidité de financement** plus dangereux que le risque couvert.

**Q46 — B** *(§9.4.1 · ex. 6)* Risque en cause : **liquidité de financement** (trésorerie).

**Q47 — A** *(§9.4.2 · cas 19)* Amaranth : positions concentrées sur les **spreads de gaz naturel**.

**Q48 — C** *(§9.4.2 · cas 19)* « Être le marché » = **part dominante de l'open interest**, rendant tout débouclage destructeur.

**Q49 — B** *(§9.4.2 · cas 20)* Perte d'environ **6,5 milliards de dollars en une semaine**.

**Q50 — D** *(§9.4.2)* Risque dominant : **concentration et liquidité de marché**.

**Q51 — B** *(§9.4.3 · cas 21)* Ashanti : **positions optionnelles vendues** calibrées sur une baisse de l'or.

**Q52 — A** *(§9.4.3 · cas 22)* Déclencheur : **hausse soudaine de l'or** (Washington Agreement).

**Q53 — C** *(§9.4.3 · ex. 4)* Conséquence : **appels de marge massifs et quasi-faillite**.

**Q54 — B** *(§9.4.3 · ex. 4)* Les positions vendues exposent à des **appels de marge potentiellement illimités**.

**Q55 — D** *(§9.4 · ex. 4)* Point commun Metallgesellschaft/Ashanti : **appels de marge en cash** lors d'un mouvement adverse.

**Q56 — B** *(§9.4 · cas 21)* Une couverture mal conçue peut **créer un risque nouveau** (trésorerie, base, rollover).

**Q57 — A** *(§9.4.1)* Risque de base = **écart de comportement entre l'instrument de couverture et le sous-jacent**.

**Q58 — C** *(§9.4)* Leçon centrale : **gérer la liquidité et la trésorerie de la couverture elle-même**.

**Q59 — B** *(§9.5)* Risque de modèle = pertes dues à des **modèles erronés, mal calibrés ou mal utilisés**.

**Q60 — A** *(§9.5.1 · cas 23)* LTCM : **arbitrage de convergence** sur des spreads.

**Q61 — C** *(§9.5.1 · ex. 3)* Levier bilanciel d'environ **25 pour 1**.

**Q62 — B** *(§9.5.1)* Hypothèses : **corrélations et volatilité stables, marchés liquides**.

**Q63 — D** *(§9.5.1 · cas 25)* Déclencheur : **défaut russe de 1998** et fuite vers la qualité.

**Q64 — B** *(§9.5.1 · cas 24 · ex. 12)* Les spreads ont **divergé** et les corrélations ont **convergé vers 1**.

**Q65 — A** *(§9.5.1)* LTCM **sauvé par un consortium de banques** orchestré par la Fed.

**Q66 — C** *(§9.5.1)* Leçon : **risque de modèle et levier excessif** ; la diversification s'évapore en crise.

**Q67 — B** *(§9.5.2)* London Whale : au **Chief Investment Office de JPMorgan à Londres**.

**Q68 — D** *(§9.5.2)* Positions sur des **indices de dérivés de crédit (CDS)**.

**Q69 — B** *(§9.5.2 · ex. 5)* Pour masquer le risque, **modification du modèle de VaR** afin d'en sous-estimer l'ampleur.

**Q70 — A** *(§9.5.2)* Perte d'environ **6,2 milliards de dollars**.

**Q71 — C** *(§9.5.2 · cas 27)* La « couverture » était devenue un **pari directionnel**.

**Q72 — B** *(§9.5.2)* Leçon : **gouvernance des modèles** (validation indépendante et contrôle des changements).

**Q73 — D** *(§9.5.3 · cas 28)* Niederhoffer : **vente d'options de vente (puts) à découvert**.

**Q74 — B** *(§9.5.3)* Déclencheur : **krach boursier d'octobre 1997**.

**Q75 — A** *(§9.5.3 · ex. 11)* Résultat : fonds **anéanti par les appels de marge**.

**Q76 — C** *(§9.5.3 · cas 29)* Les *naked puts* exposent à un **risque de queue** (perte massive lors d'un krach).

**Q77 — B** *(§9.5.3 · ex. 11)* Image : **petites primes régulières** exposant à une perte de queue démesurée.

**Q78 — D** *(§9.5)* Leçon : **un modèle n'est pas la réalité** ; attention aux hypothèses et à leur manipulation.

## F.3 Corrigé — Q79 à Q118 (rogue trading, produits complexes)

**Q79 — B** *(§9.6)* Le rogue trading est avant tout un problème de **contrôle interne**.

**Q80 — A** *(§9.6)* Facteur récurrent : **défaut de séparation front/back office**.

**Q81 — C** *(§9.6.1 · cas 31)* Leeson **cumulait les fonctions de front et de back office**.

**Q82 — A** *(§9.6.1)* Positions non autorisées sur des **futures Nikkei**.

**Q83 — B** *(§9.6.1 · cas 32)* Pertes cachées dans le **compte d'erreur « 88888 »**.

**Q84 — C** *(§9.6.1 · cas 33)* Choc révélateur : **séisme de Kobe**.

**Q85 — D** *(§9.6.1)* Perte d'environ **827 millions de livres**.

**Q86 — B** *(§9.6.1)* Conséquence : **faillite de la plus ancienne banque marchande britannique**.

**Q87 — A** *(§9.6.1)* Leçon : **la séparation des tâches (front/back) est non négociable**.

**Q88 — C** *(§9.6.2 · cas 34)* Kerviel, ancien du **back office**, en connaissait les contrôles.

**Q89 — B** *(§9.6.2)* Masquage par des **transactions fictives**.

**Q90 — C** *(§9.6.2 · ex. 9)* Notionnel d'environ **50 milliards d'euros**.

**Q91 — D** *(§9.6.2 · ex. 9)* Perte d'environ **4,9 milliards d'euros**.

**Q92 — B** *(§9.6.2 · cas 35)* Leçon : **surveiller les positions en brut**, pas seulement en net.

**Q93 — A** *(§9.6.2)* Perte cristallisée par le **débouclage forcé en pleine baisse**.

**Q94 — C** *(§9.6.3 · cas 36)* Rusnak (AIB) : pertes de **change (FX)**.

**Q95 — B** *(§9.6.3)* Dissimulation par de **faux contrats d'options**.

**Q96 — C** *(§9.6.3)* Perte d'environ **691 millions de dollars**.

**Q97 — D** *(§9.6.3)* Adoboli (UBS) : desk **ETF à Londres**.

**Q98 — B** *(§9.6.3)* Perte d'environ **2,3 milliards de dollars**.

**Q99 — C** *(§9.6.3 · cas 37)* Iguchi (Daiwa) : dissimulation pendant **onze ans**.

**Q100 — A** *(§9.6.3)* Perte d'environ **1,1 milliard de dollars**.

**Q101 — D** *(§9.6.3 · cas 31)* Cause commune Barings/Daiwa : **cumul front/back office**.

**Q102 — B** *(§9.6.3 · cas 36)* Contrôle détecteur : **réconciliation indépendante** des opérations.

**Q103 — C** *(§9.6.3 · cas 38)* Contrôle anti-fraude : **congés obligatoires et rotation des postes**.

**Q104 — A** *(§9.7.1 · cas 39)* Bankers Trust : clients lésés **Gibson Greetings et Procter & Gamble**.

**Q105 — C** *(§9.7.1)* Les enregistrements ont révélé l'**exploitation de la mauvaise compréhension** des clients.

**Q106 — B** *(§9.7.1)* Risque dominant : **conduite et réputation**.

**Q107 — D** *(§9.7.1)* Notion clé : **adéquation (suitability)** des produits vendus.

**Q108 — A** *(§9.7.2 · cas 40)* Orange County : **reverse repos à effet de levier**.

**Q109 — B** *(§9.7.2 · ex. 7)* Pari implicite : **baisse des taux d'intérêt**.

**Q110 — C** *(§9.7.2)* Déclencheur : **hausse des taux de 1994**.

**Q111 — D** *(§9.7.2 · ex. 7)* Conséquence : perte d'environ **1,7 milliard de dollars** et **faillite du comté**.

**Q112 — B** *(§9.7.2 · cas 40)* Défaillance clé : **absence de gouvernance du risque** (trésorier sans contre-pouvoir).

**Q113 — A** *(§9.7.3 · cas 41)* Sachsen LB : expositions logées dans des **conduits/SIV hors bilan**.

**Q114 — C** *(§9.7.3)* Actifs : **subprime américain**.

**Q115 — B** *(§9.7.3)* Financement par du **papier commercial adossé à des actifs (ABCP)** à court terme.

**Q116 — A** *(§9.7.3)* Déclencheur : **gel du marché ABCP** en 2007.

**Q117 — D** *(§9.7.3 · cas 42)* Conséquence : **soutien forcé de la maison mère**, précipitant son absorption.

**Q118 — C** *(§9.7)* Leçon : **complexité, expositions hors bilan et suitability** sont des angles morts classiques.

## F.4 Corrigé — Q119 à Q150 (réputation, gouvernance, cyber-risque)

**Q119 — A** *(§9.8.1 · cas 43)* Volkswagen : **logiciel truqueur (defeat device)**.

**Q120 — C** *(§9.8.1)* Le dispositif faussait les **tests d'émissions des moteurs diesel**.

**Q121 — B** *(§9.8.1)* Affaire éclatée en **2015** (révélation par l'agence américaine EPA).

**Q122 — D** *(§9.8 · cas 43)* Risque dominant : **réputation**.

**Q123 — B** *(§9.8.2)* Risque sous-jacent : **conduite (conduct risk)**.

**Q124 — C** *(§9.8.2 · cas 44)* Déterminant profond : **culture et gouvernance** de l'entreprise.

**Q125 — D** *(§9.8.2)* Le capital réputationnel est **fragile** : des années à bâtir, des heures à détruire.

**Q126 — A** *(§9.8.1)* Conséquences : **amendes massives, rappels et effondrement du cours**.

**Q127 — B** *(§9.9.1 · cas 45)* Enron : dette dissimulée via des **entités ad hoc (SPE) hors bilan**.

**Q128 — A** *(§9.9.1)* Méthode comptable agressive : **mark-to-market** de contrats de long terme.

**Q129 — C** *(§9.9.1)* Conflit d'intérêts : le **directeur financier dirigeait certaines des entités**.

**Q130 — B** *(§9.9.1)* L'affaire a entraîné la **chute de son auditeur, Arthur Andersen**.

**Q131 — D** *(§9.9.1)* Enron a directement inspiré la **loi Sarbanes-Oxley**.

**Q132 — A** *(§9.9.1 · cas 45)* Risque dominant : **gouvernance** (opacité, conflits d'intérêts).

**Q133 — C** *(§9.9.2 · cas 46)* WorldCom : **capitalisation de charges d'exploitation** (inscrites à l'actif).

**Q134 — B** *(§9.9.2)* Fraude d'environ **11 milliards de dollars**.

**Q135 — D** *(§9.9.2)* Leçon : nécessité de **contrôles internes et d'un audit robustes**.

**Q136 — A** *(§9.9.3 · cas 47)* Madoff : **schéma de Ponzi**.

**Q137 — C** *(§9.9.3)* Ampleur d'environ **65 milliards de dollars** (valeurs fictives).

**Q138 — B** *(§9.9.3 · cas 47)* Red flag : **rendements anormalement réguliers et positifs**.

**Q139 — D** *(§9.9.3 · cas 48)* Red flag structurel : **absence de dépositaire indépendant** (auto-conservation).

**Q140 — A** *(§9.9.3 · cas 48)* Signalé aussi par un **auditeur minuscule** au regard des montants gérés.

**Q141 — C** *(§9.9.3)* Défaillance : **faillite de la due diligence** des investisseurs et des régulateurs.

**Q142 — B** *(§9.9)* Leçon : **transparence, contrôles, audit crédible et tiers indépendants** ; méfiance envers le « trop beau pour être vrai ».

**Q143 — A** *(§9.10.1 · cas 49)* Cible : **banque centrale du Bangladesh**.

**Q144 — C** *(§9.10.1)* Attaque par un **logiciel malveillant émettant des ordres SWIFT frauduleux**.

**Q145 — B** *(§9.10.1)* Montant effectivement détourné : environ **81 millions de dollars**.

**Q146 — D** *(§9.10.1 · cas 50)* Blocage de la majeure partie grâce à une **faute d'orthographe** éveillant les soupçons.

**Q147 — B** *(§9.10.1)* Le réseau SWIFT **n'a pas été compromis** : la faille était côté participant.

**Q148 — A** *(§9.10.2)* Catégorie : **risque opérationnel (cyber)**.

**Q149 — C** *(§9.10.2)* Dimension potentiellement **systémique** via une infrastructure partagée.

**Q150 — D** *(§9.10.2)* Leçon : **la sécurité vaut celle du maillon le plus faible** ; détection et surveillance sont critiques.

---

**Fin du Chapitre 9 — Learning from Financial Disasters · Cours · 50 cas · 12 exercices corrigés · conseils méthodologiques · 150 QCM commentés**

