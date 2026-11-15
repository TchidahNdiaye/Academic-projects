# Chapitre 1 — The Building Blocks of Risk Management

### Foundations of Risk Management — Les fondations de la gestion des risques

**FRM Part I · Foundations — Approfondissement complet**

*Cours · 50 études de cas · 12 exercices corrigés · conseils méthodologiques · 150 QCM commentés. Questions originales de style examen ; GARP ne publiant aucune annale, aucune question n'est attribuée à une session passée.*

# Partie A — Cours approfondi

Le chapitre 1 ouvre le programme FRM. Il ne contient presque aucune formule complexe, mais il pose le **vocabulaire** et les **concepts fondateurs** sur lesquels reposent les dix books. En sous-estimer la subtilité est l'erreur classique du candidat quantitatif : les questions d'examen y jouent sur des nuances de définition. Ce chapitre répond à trois questions — *qu'est-ce que le risque ? comment le classe-t-on ? comment le mesure-t-on et le gère-t-on ?* — en s'appuyant sur les grandes catastrophes financières pour ancrer chaque notion.

## 1.1 Qu'est-ce que le risque ?

Dans le langage courant, « risque » évoque un danger, une perte possible. En finance, la notion est plus large et plus précise : le risque est l'**incertitude quant aux résultats futurs**. Cette incertitude peut jouer dans les deux sens — un résultat meilleur (*upside*) comme pire (*downside*) que prévu. Ce qui définit le risque, ce n'est donc pas la perte elle-même, mais la **dispersion** des issues possibles autour de leur valeur attendue.

> **Risque**
>
> Le **risque** est la **variabilité des résultats futurs** autour d'une espérance. Il combine deux éléments : l'**exposition** (le montant en jeu) et l'**incertitude** (la probabilité et l'ampleur des écarts). Un résultat certain, même mauvais, n'est pas un risque : c'est un coût. Le risque naît de ce qui n'est *pas* connu d'avance.

Cette définition a une conséquence directe : **prendre du risque n'est pas une erreur**. Une entreprise financière existe précisément pour assumer des risques que d'autres ne veulent pas porter, en échange d'une rémunération. Le but de la gestion des risques n'est pas de les supprimer — ce serait renoncer au rendement — mais de les **choisir consciemment**, de les **dimensionner** et de les **rémunérer** correctement.

> **Risque vs incertitude (Knight)**
>
> L'économiste Frank Knight distingue le **risque**, où les probabilités des issues sont *connues ou estimables* (on peut le modéliser, comme un lancer de dé), de l'**incertitude** (*Knightian uncertainty*), où ces probabilités sont *inconnues* (événements inédits, ruptures structurelles). La finance quantitative traite bien le risque mesurable ; les crises naissent souvent de l'incertitude non modélisable.

## 1.2 Le processus de gestion des risques

Gérer le risque n'est pas un acte ponctuel mais un **cycle continu** en quatre étapes, qui se répète en boucle à mesure que l'environnement évolue.

> **Le cycle de gestion des risques**
>
> **(1) Identifier** les risques auxquels l'institution est exposée. **(2) Mesurer / évaluer** leur ampleur (probabilité et impact). **(3) Gérer / atténuer** : décider de les éviter, retenir, réduire ou transférer. **(4) Surveiller** en continu et ajuster. L'omission d'une seule étape — souvent la surveillance — est à l'origine de nombreux désastres.

La première étape, l'**identification**, est la plus sous-estimée : on ne peut gérer que ce que l'on a su nommer. Les risques les plus dangereux sont souvent ceux qui n'apparaissent dans aucun tableau de bord. La **mesure** traduit le risque en grandeurs quantitatives (écart-type, VaR, perte attendue) — mais un chiffre faux est plus dangereux qu'une absence de chiffre, car il inspire une fausse confiance. La **gestion** choisit la réponse ; la **surveillance** vérifie que les décisions restent valides dans un monde qui change.

> **⚑ Point d'examen —** Mémorise les quatre étapes dans l'ordre : **identifier, mesurer, gérer, surveiller**. Les questions d'examen testent souvent le fait que le processus est *itératif* (une boucle, pas une ligne) et que l'**identification** précède toute mesure. Un risque non identifié ne peut être ni mesuré ni géré.

## 1.3 La typologie des risques financiers

Classer les risques est indispensable pour les gouverner : chaque catégorie appelle des outils de mesure et d'atténuation différents. Le curriculum distingue plusieurs grandes familles.

> **Les grandes catégories de risque**
>
> **Risque de marché** : pertes dues aux variations de prix (taux, actions, change, matières premières). **Risque de crédit** : pertes dues au défaut ou à la dégradation d'une contrepartie. **Risque de liquidité** : incapacité à financer ses positions ou à les liquider sans perte excessive. **Risque opérationnel** : pertes dues à des défaillances de processus, de personnes, de systèmes, ou à des événements externes.

À ces quatre familles « mesurables » s'ajoutent des risques plus diffus. Le **risque business** (ou commercial) découle de l'activité elle-même : incertitude sur les volumes, les marges, la demande. Le **risque stratégique** provient de mauvaises décisions de direction ou de changements majeurs de l'environnement concurrentiel. Le **risque de réputation** est l'atteinte à l'image et à la confiance, souvent conséquence d'un autre risque mal géré. Le **risque légal et réglementaire** naît des lois, litiges et sanctions.

> **Risque idiosyncratique vs systématique**
>
> Le risque **idiosyncratique** (spécifique) est propre à une entité ; il se **diversifie** en combinant de nombreuses positions. Le risque **systématique** (de marché) affecte l'ensemble du système et ne peut être diversifié : c'est le seul qui soit *rémunéré* à l'équilibre (voir chapitre 5, CAPM). Distinguer les deux est central pour toute la théorie du portefeuille.

Ces catégories ne sont pas étanches : un même événement en active souvent plusieurs. La faillite de Lehman Brothers en 2008 fut à la fois un choc de crédit, de liquidité et de réputation, avec des ramifications opérationnelles et systémiques. Savoir qu'un risque **se transforme** en un autre est une idée que l'examen teste régulièrement.

## 1.4 Risques connus et inconnus

Tous les risques ne sont pas également « visibles ». Une typologie célèbre distingue trois niveaux de connaissance, utile pour comprendre pourquoi les crises surprennent même les institutions les mieux outillées.

> **Known knowns, known unknowns, unknown unknowns**
>
> Les **known knowns** sont les risques identifiés et mesurés (on sait qu'on ne sait pas... et on l'a quantifié). Les **known unknowns** sont les risques identifiés mais mal mesurés (on sait qu'ils existent sans en connaître l'ampleur). Les **unknown unknowns** sont les risques non identifiés — les plus dangereux, car aucune couverture ne les vise. Les grandes crises viennent souvent de cette dernière catégorie.

Cette grille éclaire la limite fondamentale de la modélisation : les modèles ne traitent bien que les *known knowns*. Ils supposent que le futur ressemblera au passé (mêmes lois de probabilité, mêmes corrélations). Or les crises sont précisément des moments où ces hypothèses se brisent : les corrélations bondissent vers 1, la liquidité s'évapore, des événements « impossibles » se réalisent. C'est pourquoi la gestion des risques ne peut se réduire à des chiffres : elle exige du **jugement**, des **scénarios de stress** et une **humilité** face à l'incertitude.

> **⚑ Point d'examen —** Retiens que les **unknown unknowns** sont les plus dangereux et échappent par nature aux modèles. L'examen relie souvent cette idée aux **limites de la VaR** et à la nécessité du **stress testing** et du **jugement** : un modèle calibré sur des données normales sous-estime structurellement les événements extrêmes.

## 1.5 Risque, rendement et pertes attendues

Le lien entre risque et rendement est le principe organisateur de toute la finance : **on n'obtient un rendement supérieur qu'en acceptant un risque supérieur**. Un placement sans risque (bon du Trésor) rapporte peu ; un placement risqué doit offrir une **prime de risque** pour attirer les investisseurs. La gestion des risques ne cherche donc pas le risque minimal, mais le **meilleur rendement pour un niveau de risque donné**.

Pour piloter le capital, on décompose la perte potentielle en deux composantes, dont la distinction est **fondamentale** et se retrouvera dans les books crédit et opérationnel.

> **Perte attendue vs perte inattendue**
>
> La **perte attendue** (*expected loss*, EL) est la perte *moyenne* anticipée sur un horizon : c'est un coût prévisible, que l'on **provisionne** et que l'on intègre dans la tarification. La **perte inattendue** (*unexpected loss*, UL) est la *variabilité* autour de cette moyenne — les mauvaises surprises. Elle ne se provisionne pas : elle s'absorbe par le **capital économique**, coussin dimensionné pour survivre aux scénarios défavorables.

Cette distinction structure la logique prudentielle : le **prix** d'un produit couvre la perte attendue, le **capital** couvre la perte inattendue, et le **stress testing** explore ce qui dépasse même le capital (le risque extrême, ou *tail risk*). En crédit, la perte attendue s'écrit comme le produit de trois facteurs, formule à connaître par cœur car elle traverse tout le programme.

> EL = PD × LGD × EAD

où PD est la probabilité de défaut, LGD la perte en cas de défaut (*loss given default*, soit 1 moins le taux de recouvrement) et EAD l'exposition au moment du défaut (*exposure at default*).

> **⚑ Point d'examen —** La distinction **EL / UL** est un pilier conceptuel : EL se *provisionne* (coût), UL s'absorbe par le *capital*. Retiens EL = PD × LGD × EAD et la logique « prix pour l'attendu, capital pour l'inattendu, stress pour l'extrême ». C'est l'ossature de toute la mesure prudentielle du risque.

## 1.6 Les briques de la mesure du risque

Mesurer le risque, c'est le traduire en nombres exploitables. Plusieurs « briques » élémentaires y concourent, de la plus simple à la plus élaborée. Chacune sera approfondie dans les books suivants ; ici, l'objectif est d'en saisir l'articulation.

> **Les instruments de mesure élémentaires**
>
> L'**écart-type** (σ) mesure la dispersion des rendements — la brique de base de la volatilité. La **covariance** et la **corrélation** mesurent le lien entre deux positions, clé de la diversification. La **VaR** (Value-at-Risk) résume en un seul chiffre la perte maximale à un niveau de confiance et un horizon donnés. Le **stress testing** et l'**analyse de scénarios** évaluent les pertes dans des conditions extrêmes, là où les mesures statistiques échouent.

Ces briques s'emboîtent. L'écart-type suppose une distribution ; la corrélation permet d'agréger plusieurs risques en tenant compte de la diversification ; la VaR condense le tout en une perte-seuil ; le stress testing corrige le péché originel de ces mesures — leur dépendance à des données historiques qui ne préparent pas aux ruptures. La **VaR** cristallise cette tension : puissante pour résumer, elle est **aveugle à ce qui se passe au-delà du seuil** (d'où l'Expected Shortfall, book 4) et suppose des corrélations stables (fausses en crise).

> σ(p)² = w₁²σ₁² + w₂²σ₂² + 2 w₁ w₂ ρ(12)σ₁σ₂     ρ(12) = (Cov(R₁, R₂)) ÷ (σ₁σ₂)

La formule de la variance d'un portefeuille montre la **magie de la diversification** : tant que la corrélation ρ(12) est inférieure à 1, le risque du portefeuille est *inférieur* à la moyenne pondérée des risques individuels. C'est le seul « repas gratuit » de la finance — et il disparaît précisément quand les corrélations montent, en crise.

> **Value-at-Risk (intuition)**
>
> La **VaR** à un niveau de confiance α et un horizon h est la perte qui ne sera dépassée qu'avec une probabilité 1 − α. Exemple : une VaR à 99 % sur 1 jour de 10 M€ signifie qu'il y a 1 % de chance de perdre plus de 10 M€ en une journée. Elle *ne dit rien* de l'ampleur des pertes au-delà de ce seuil — sa limite majeure.

## 1.7 Pourquoi gérer le risque crée de la valeur

En théorie des marchés parfaits (Modigliani-Miller), la gestion du risque au niveau de la firme serait **neutre** : des actionnaires diversifiés pourraient gérer eux-mêmes leur exposition, sans que l'entreprise ait à le faire. Si, dans la réalité, les entreprises consacrent tant de ressources à gérer leurs risques, c'est que les marchés sont **imparfaits** — et ces imperfections sont autant de raisons créatrices de valeur.

> **Pourquoi la gestion du risque crée de la valeur**
>
> Elle réduit les **coûts de détresse financière** (une entreprise proche de la faillite perd des clients, des employés, subit des conditions de crédit dégradées). Elle diminue les **coûts d'agence** et l'incertitude des créanciers. Elle **lisse les flux de trésorerie**, préservant la capacité d'**investissement** et évitant de renoncer à des projets rentables faute de liquidités. Elle peut enfin réduire la **charge fiscale** espérée lorsque l'impôt est convexe.

L'argument est donc pragmatique, non idéologique : couvrir un risque a un **coût** (primes, marges, temps), et n'a de sens que si le bénéfice — réduction des coûts de détresse, stabilité, capacité d'investir — le dépasse. C'est pourquoi **couvrir intégralement n'est pas toujours optimal** : une entreprise dont le risque est le cœur de métier (une compagnie pétrolière sur le prix du pétrole) peut choisir d'en conserver une part, jugée rémunératrice. La décision de couverture est un **arbitrage coûts/bénéfices**, pas un réflexe.

> **⚑ Point d'examen —** Sais énoncer les canaux de création de valeur : **coûts de détresse**, coûts d'agence, **lissage des flux**, capacité d'investissement, fiscalité. Retiens le contrepoint Modigliani-Miller (en marché parfait, la couverture au niveau firme est neutre) et l'idée que **couvrir à 100 % n'est pas toujours optimal** — c'est un arbitrage. Ces nuances sont fréquemment testées.

## 1.8 Les outils d'atténuation du risque

Une fois un risque identifié et mesuré, quatre réponses génériques s'offrent à l'institution. Elles ne s'excluent pas : on les combine selon le coût, l'appétit et la nature du risque.

> **Les quatre traitements du risque**
>
> **Éviter** : ne pas prendre le risque (renoncer à une activité). **Retenir** (accepter) : conserver le risque et le couvrir par du capital, lorsqu'il est rémunérateur ou trop coûteux à transférer. **Atténuer / réduire** : diminuer l'exposition par la diversification, les limites ou la couverture (*hedging*). **Transférer** : céder le risque à un tiers via l'assurance, les dérivés ou la titrisation.

La **diversification** réduit le risque idiosyncratique en combinant des positions imparfaitement corrélées — sans coût direct, mais sans effet sur le risque systématique. La **couverture** (*hedging*) neutralise une exposition par une position opposée (un future, une option) : efficace, mais génératrice de **risque de base** (la couverture et le sous-jacent ne bougent jamais exactement de concert) et parfois de **risque de liquidité** (les appels de marge). L'**assurance** transfère un risque contre une prime. Le **transfert** enfin (titrisation, dérivés de crédit) déplace le risque vers ceux qui veulent le porter — un mécanisme sain en principe, mais dont l'abus, on le verra, fut au cœur de la crise de 2008.

> **Risque de base (basis risk)**
>
> Le **risque de base** est l'imperfection d'une couverture : l'écart entre la variation de l'instrument de couverture et celle de la position couverte. Couvrir du kérosène par des futures de pétrole brut laisse un risque de base (les deux prix ne sont pas parfaitement corrélés). Une couverture réduit le risque directionnel mais *crée* un risque de base résiduel.

## 1.9 Appétit, tolérance et limites de risque

Prendre du risque « consciemment » suppose de définir *combien* de risque l'institution est prête à assumer. C'est le rôle de l'appétit pour le risque, décliné en dispositifs de plus en plus opérationnels.

> **Appétit, tolérance et limites**
>
> L'**appétit pour le risque** (*risk appetite*) est la *quantité globale* de risque qu'une institution choisit d'assumer pour atteindre ses objectifs — une orientation stratégique fixée par le conseil. La **tolérance au risque** précise les écarts acceptables autour de cet appétit par type de risque. Les **limites de risque** en sont la traduction *opérationnelle* et chiffrée (plafonds de VaR, de notionnel, de concentration) imposées aux desks.

La chaîne va du général au particulier : l'appétit oriente, la tolérance encadre, les limites contraignent au jour le jour. Un dispositif cohérent garantit qu'aucune décision individuelle n'expose l'institution au-delà de ce que sa direction a consciemment accepté. Beaucoup de désastres — on va le voir — naissent d'un **découplage** entre l'appétit affiché et les risques réellement pris sur le terrain, faute de limites effectives ou de surveillance.

## 1.10 Les leçons des grandes catastrophes financières

Rien n'ancre mieux ces concepts que les catastrophes qui ont résulté de leur violation. Chaque grande faillite illustre l'oubli d'une brique fondamentale — et l'examen aime demander « quel risque illustre tel cas ? ».

**Barings (1995)** — l'effondrement d'une banque britannique tricentenaire tient à un seul trader, Nick Leeson, à Singapour, cumulant les fonctions de *front* et de *back office*. Sans **séparation des tâches**, il masqua des pertes croissantes dans un compte d'erreurs jusqu'à engloutir les fonds propres. *Leçon* : **risque opérationnel** — défaillance de contrôle interne et de gouvernance, plus que risque de marché.

**Metallgesellschaft (1993)** — la filiale américaine de ce groupe allemand avait vendu des contrats pétroliers à long terme, couverts par des futures à court terme roulés. Le raisonnement de couverture était défendable, mais le décalage d'échéances déclencha des **appels de marge** massifs quand les prix baissèrent : un risque de couverture bien conçu se mua en **risque de liquidité** fatal. *Leçon* : une couverture peut **transformer** un risque en un autre.

**Long-Term Capital Management (1998)** — un fonds dirigé par des prix Nobel, exploitant des écarts de valorisation minuscules avec un **levier colossal**. Ses modèles supposaient des corrélations stables ; la crise russe les fit toutes converger vers 1, et les pertes, amplifiées par le levier, menacèrent le système entier. *Leçon* : **risque de modèle** et de levier — les *unknown unknowns* que les modèles ne voient pas.

**La crise de 2007-2009** — la titrisation de crédits *subprime*, le modèle « *originate-to-distribute* », l'opacité des CDO et la sous-estimation des corrélations de défaut se combinèrent en une **contagion systémique**. *Leçon* : le transfert du risque, sain en principe, devient dangereux quand il déresponsabilise et masque les expositions (approfondi au chapitre 10).

> **⚑ Point d'examen —** Associe chaque désastre à son risque *dominant* : Barings → opérationnel (séparation des tâches) ; Metallgesellschaft → liquidité (couverture mal dimensionnée) ; LTCM → modèle et levier ; 2008 → systémique/crédit. C'est une question récurrente. Retiens aussi le fil conducteur : chaque faillite viole une **brique** vue dans ce chapitre (surveillance, séparation, humilité face aux corrélations).

# Partie B — 100 études de cas

Ces cas appliquent les concepts du chapitre à des situations concrètes. Chacun renvoie à la section de cours correspondante. Ils sont pensés pour ancrer les réflexes que les questions d'examen mobilisent.

## B.1 Nature du risque et incertitude (cas 1 à 12)

**Cas 1 — La perte certaine.** Une entreprise sait qu'elle paiera une amende fixe le mois prochain ; un dirigeant qualifie cela de « risque ». *Enseignement* : une perte **certaine** est un *coût*, pas un risque ; le risque naît de l'**incertitude** sur les issues. (§1.1)

**Cas 2 — L'upside oublié.** Un analyste ne considère comme « risque » que les scénarios de perte. *Enseignement* : le risque est la **dispersion** des résultats, à la hausse comme à la baisse ; ignorer l'*upside* déforme la mesure. (§1.1)

**Cas 3 — Le risque rémunéré.** Un comité veut réduire tous les risques à zéro. *Enseignement* : supprimer le risque, c'est renoncer au **rendement** ; l'objectif est de *choisir* et *dimensionner* le risque, non de l'éliminer. (§1.1)

**Cas 4 — Le dé et la rupture.** Un modèle traite un marché nouveau comme un phénomène aux probabilités connues. *Enseignement* : distinction de Knight — le **risque** a des probabilités estimables, l'**incertitude** non ; confondre les deux fabrique une fausse précision. (§1.1)

**Cas 5 — L'exposition sans incertitude.** Un trésorier détient une créance dont le paiement est garanti par l'État à échéance connue. *Enseignement* : forte exposition mais faible incertitude ; le risque combine **exposition et incertitude**, pas l'une sans l'autre. (§1.1)

**Cas 6 — La prime de risque.** Deux obligations offrent 2 % et 7 % ; un client s'étonne de l'écart. *Enseignement* : le surcroît de rendement rémunère un surcroît de **risque** (crédit, liquidité) ; il n'y a pas de rendement élevé sans risque élevé. (§1.1)

**Cas 7 — Le cœur de métier.** Une compagnie aérienne débat de couvrir 100 % de son exposition au kérosène. *Enseignement* : conserver une part de risque *lié au métier* peut être un choix rationnel ; couvrir n'est pas un impératif absolu. (§1.1)

**Cas 8 — La volatilité comme mesure.** Un gérant décrit le risque d'un fonds par son seul rendement moyen. *Enseignement* : la moyenne ne dit rien du risque ; il faut une mesure de **dispersion** (écart-type) pour le caractériser. (§1.1)

**Cas 9 — Le pari asymétrique.** Une stratégie gagne souvent un peu et perd rarement beaucoup. *Enseignement* : la dispersion seule ne suffit pas ; l'**asymétrie** du profil de risque compte, un point développé au book quantitatif. (§1.1)

**Cas 10 — Risque et objectif.** Deux investisseurs — un jeune épargnant et un retraité — jugent différemment le même portefeuille. *Enseignement* : le risque *acceptable* dépend des **objectifs** et de l'horizon ; il n'est pas une propriété absolue de l'actif. (§1.1)

**Cas 11 — L'incertitude irréductible.** Un dirigeant exige un modèle qui prédise « exactement » les pertes futures. *Enseignement* : aucun modèle n'annule l'**incertitude** ; il l'encadre. Promettre l'exactitude est un signal d'alerte. (§1.1)

**Cas 12 — Le risque comme opportunité.** Une banque refuse par principe tout actif volatil. *Enseignement* : la volatilité est aussi source de **rendement** ; une aversion excessive détruit de la valeur autant qu'une prise de risque inconsidérée. (§1.1)

## B.2 Le processus de gestion des risques (cas 13 à 24)

**Cas 13 — Le risque hors radar.** Une perte majeure provient d'une exposition qui ne figurait dans aucun rapport. *Enseignement* : on ne gère que ce qu'on a su **identifier** ; l'identification est la première et la plus critique des étapes. (§1.2)

**Cas 14 — La surveillance oubliée.** Un dispositif de couverture, jamais réévalué, devient inadapté après un changement de marché. *Enseignement* : le cycle est **itératif** ; l'omission de la **surveillance** est une cause fréquente de désastre. (§1.2)

**Cas 15 — Le faux chiffre rassurant.** Un modèle de VaR mal calibré affiche un risque très faible ; le desk augmente ses positions. *Enseignement* : un **chiffre faux** est plus dangereux qu'une absence de chiffre — il inspire une fausse confiance. (§1.2)

**Cas 16 — Mesurer sans gérer.** Une équipe produit des rapports de risque détaillés mais ne prend aucune décision. *Enseignement* : mesurer n'est pas **gérer** ; l'étape de décision (éviter/retenir/atténuer/transférer) doit suivre la mesure. (§1.2)

**Cas 17 — L'ordre des étapes.** Un projet veut mesurer un risque avant de l'avoir défini précisément. *Enseignement* : l'**identification** précède la mesure ; mesurer un risque mal défini produit un résultat trompeur. (§1.2)

**Cas 18 — La boucle rompue.** Après une réorganisation, personne ne réévalue les risques hérités. *Enseignement* : le processus doit **boucler** en continu ; un cycle interrompu laisse des risques dériver sans contrôle. (§1.2)

**Cas 19 — L'identification participative.** Les risques opérationnels réels sont connus des équipes de terrain, pas du comité. *Enseignement* : l'identification gagne à **remonter du terrain** ; les meilleurs signaux viennent souvent de la base. (§1.2)

**Cas 20 — Le tableau de bord trompeur.** Un indicateur unique masque une accumulation de petits risques corrélés. *Enseignement* : la **mesure** doit capturer les *interactions* ; agréger sans tenir compte des corrélations sous-estime le risque. (§1.2)

**Cas 21 — La réponse disproportionnée.** Face à un risque mineur, une firme engage une couverture coûteuse. *Enseignement* : la **gestion** est un arbitrage coût/bénéfice ; la réponse doit être proportionnée à l'enjeu. (§1.2)

**Cas 22 — Le risque accepté implicitement.** Une exposition non couverte l'est « par défaut », sans décision explicite. *Enseignement* : **retenir** un risque doit être un *choix conscient*, pas une omission ; l'inaction est une décision. (§1.2)

**Cas 23 — La réévaluation salvatrice.** Une revue trimestrielle détecte une dérive avant qu'elle ne devienne critique. *Enseignement* : la **surveillance** périodique transforme un désastre potentiel en ajustement mineur. (§1.2)

**Cas 24 — Le processus sans culture.** Un cycle formellement complet échoue car personne ne le prend au sérieux. *Enseignement* : le processus n'a de valeur que porté par une **culture du risque** ; la forme sans la substance ne protège pas. (§1.2)

## B.3 Typologie des risques (cas 25 à 42)

**Cas 25 — Le choc de taux.** Un portefeuille obligataire perd de la valeur quand les taux montent. *Enseignement* : **risque de marché** (composante taux) ; la valeur d'une obligation varie inversement aux taux. (§1.3)

**Cas 26 — Le défaut de contrepartie.** Un emprunteur cesse de rembourser. *Enseignement* : **risque de crédit** ; la perte dépend de PD, LGD et EAD. (§1.3)

**Cas 27 — L'actif invendable.** Une firme ne peut liquider une position sans effondrer son prix. *Enseignement* : **risque de liquidité** (de marché) ; l'actif est détenu, mais pas mobilisable sans perte excessive. (§1.3)

**Cas 28 — L'erreur de règlement.** Un paiement est envoyé deux fois à cause d'une défaillance de système. *Enseignement* : **risque opérationnel** ; défaillance de processus/systèmes, sans lien avec le marché. (§1.3)

**Cas 29 — La demande qui s'effondre.** Les revenus d'une activité chutent car le marché se contracte. *Enseignement* : **risque business** ; il découle de l'activité elle-même (volumes, marges). (§1.3)

**Cas 30 — Le virage manqué.** Une entreprise rate une transition technologique majeure. *Enseignement* : **risque stratégique** ; mauvaise décision de direction ou rupture d'environnement. (§1.3)

**Cas 31 — Le scandale médiatique.** Une pratique controversée fait fuir les clients. *Enseignement* : **risque de réputation** ; souvent la *conséquence* d'un autre risque mal géré. (§1.3)

**Cas 32 — La sanction réglementaire.** Un manquement à la conformité entraîne une lourde amende. *Enseignement* : **risque légal/réglementaire** ; lois, litiges et sanctions. (§1.3)

**Cas 33 — La diversification efficace.** Combiner de nombreux prêts peu corrélés réduit le risque global. *Enseignement* : le risque **idiosyncratique** se diversifie ; c'est le fondement de la mutualisation. (§1.3)

**Cas 34 — Le risque non diversifiable.** Une récession générale frappe toutes les positions à la fois. *Enseignement* : le risque **systématique** ne se diversifie pas ; c'est le seul rémunéré à l'équilibre (CAPM). (§1.3)

**Cas 35 — La cascade de risques.** Un défaut de crédit déclenche un problème de liquidité puis une atteinte à la réputation. *Enseignement* : un risque se **transforme** en un autre ; les catégories ne sont pas étanches. (§1.3)

**Cas 36 — Le change non couvert.** Une filiale étrangère voit ses profits fondre avec la devise. *Enseignement* : **risque de marché** (composante change) ; une exposition en devise est une position de risque. (§1.3)

**Cas 37 — La fraude interne.** Un employé détourne des fonds. *Enseignement* : **risque opérationnel** (fraude interne), l'un des sept types d'événements de Bâle. (§1.3)

**Cas 38 — La migration de notation.** Une contrepartie passe de A à BBB sans faire défaut. *Enseignement* : **risque de crédit** par *migration* (dégradation), distinct du défaut pur. (§1.3)

**Cas 39 — Le risque de financement.** Une banque ne parvient plus à renouveler ses financements courts. *Enseignement* : **risque de liquidité** (de financement), distinct de la liquidité de marché. (§1.3)

**Cas 40 — La concentration cachée.** Plusieurs positions semblent distinctes mais dépendent du même facteur. *Enseignement* : la **concentration** annule la diversification apparente ; il faut regarder les *facteurs* communs. (§1.3)

**Cas 41 — Le risque systémique.** La chute d'un acteur menace tout le système par interconnexion. *Enseignement* : **risque systémique** ; l'interconnexion transforme un risque local en menace globale (cf. 2008). (§1.3)

**Cas 42 — La catégorie ambiguë.** Une cyberattaque provoque à la fois pertes opérationnelles et atteinte à la réputation. *Enseignement* : un événement active **plusieurs catégories** ; la classification vise l'action, pas la pureté théorique. (§1.3)

## B.4 Risques connus, inconnus et limites des modèles (cas 43 à 52)

**Cas 43 — Le known known.** Une banque a identifié et quantifié son exposition de taux. *Enseignement* : **known known** — risque identifié *et* mesuré ; le cas idéal, gérable par les outils standards. (§1.4)

**Cas 44 — Le known unknown.** Une firme sait qu'elle est exposée à un risque politique sans pouvoir le chiffrer. *Enseignement* : **known unknown** — identifié mais mal mesuré ; à traiter par scénarios et jugement. (§1.4)

**Cas 45 — L'unknown unknown.** Une crise surgit d'un mécanisme que personne n'avait envisagé. *Enseignement* : **unknown unknown** — le plus dangereux, car aucune couverture ne le vise ; source typique des grandes crises. (§1.4)

**Cas 46 — Le modèle aveugle.** Un modèle calibré sur dix ans de données calmes ignore un régime de crise. *Enseignement* : les modèles ne traitent que les **known knowns** ; ils supposent que le futur ressemble au passé. (§1.4)

**Cas 47 — Les corrélations qui bondissent.** En crise, des actifs jugés indépendants chutent ensemble. *Enseignement* : les corrélations **montent vers 1** en stress, annulant la diversification au pire moment. (§1.4)

**Cas 48 — La queue sous-estimée.** Les pertes extrêmes réelles dépassent de loin celles prévues par une loi normale. *Enseignement* : les distributions réelles ont des **queues épaisses** ; les modèles gaussiens sous-estiment l'extrême. (§1.4)

**Cas 49 — Le stress test révélateur.** Un scénario hypothétique dévoile une vulnérabilité invisible aux mesures statistiques. *Enseignement* : le **stress testing** complète les modèles là où l'historique est muet. (§1.4)

**Cas 50 — L'excès de confiance.** Une VaR précise à la décimale masque une incertitude structurelle. *Enseignement* : la **fausse précision** est un piège ; un chiffre net ne garantit pas la justesse. (§1.4)

**Cas 51 — Le jugement irremplaçable.** Un gérant expérimenté réduit ses positions malgré des modèles rassurants, avant une crise. *Enseignement* : le **jugement** humain complète — parfois corrige — les modèles face aux *unknown unknowns*. (§1.4)

**Cas 52 — Le cygne noir.** Un événement « impossible » selon les modèles se produit et cause des pertes massives. *Enseignement* : les événements extrêmes rares mais dévastateurs échappent aux modèles ; d'où l'humilité et le stress. (§1.4)

## B.5 Risque, rendement et pertes attendues/inattendues (cas 53 à 64)

**Cas 53 — L'arbitrage risque-rendement.** Un client veut un rendement élevé sans aucun risque. *Enseignement* : impossible ; le rendement supérieur exige un risque supérieur — principe organisateur de la finance. (§1.5)

**Cas 54 — La provision pour perte attendue.** Une banque intègre un coût du risque moyen dans le prix de ses prêts. *Enseignement* : la **perte attendue** se *provisionne* et se *tarife* ; c'est un coût prévisible. (§1.5)

**Cas 55 — Le capital pour perte inattendue.** Une banque dimensionne ses fonds propres pour absorber les mauvaises surprises. *Enseignement* : la **perte inattendue** s'absorbe par le **capital**, pas par les provisions. (§1.5)

**Cas 56 — Le calcul EL.** PD = 2 %, LGD = 40 %, EAD = 1 M€. *Enseignement* : EL = 0,02 × 0,40 × 1 = 8 000 € ; formule à automatiser. (§1.5)

**Cas 57 — Le recouvrement.** Une créance en défaut est recouvrée à 70 %. *Enseignement* : LGD = 1 − taux de recouvrement = 30 % ; la LGD mesure la perte *nette* du recouvrement. (§1.5)

**Cas 58 — La tarification insuffisante.** Un prêt est vendu sans couvrir son coût du risque. *Enseignement* : ne pas intégrer l'**EL** dans le prix conduit à des pertes structurelles ; le prix doit couvrir l'attendu. (§1.5)

**Cas 59 — Le stress au-delà du capital.** Un scénario extrême dépasse même le capital économique. *Enseignement* : le **tail risk** excède l'UL ; le stress testing explore cette zone que le capital ne couvre pas. (§1.5)

**Cas 60 — La prime de risque justifiée.** Un investisseur exige un surcroît de rendement pour un actif volatil. *Enseignement* : la **prime de risque** rémunère l'incertitude assumée ; elle est le prix du risque. (§1.5)

**Cas 61 — L'EAD qui varie.** L'exposition d'une ligne de crédit dépend du tirage au moment du défaut. *Enseignement* : l'**EAD** n'est pas toujours l'encours nominal ; les engagements non tirés compliquent son estimation. (§1.5)

**Cas 62 — Le meilleur rendement à risque donné.** Deux portefeuilles ont le même risque mais des rendements différents. *Enseignement* : on privilégie le **meilleur rendement pour un risque donné** — logique de la frontière efficiente. (§1.5)

**Cas 63 — La perte moyenne trompeuse.** Une activité affiche une faible perte attendue mais une très forte variabilité. *Enseignement* : l'**EL** seule ne suffit pas ; l'**UL** (variabilité) détermine le besoin en capital. (§1.5)

**Cas 64 — Prix, capital, stress.** Une banque articule tarification, fonds propres et scénarios. *Enseignement* : l'ossature prudentielle — **prix** pour l'attendu, **capital** pour l'inattendu, **stress** pour l'extrême. (§1.5)

## B.6 La mesure du risque (cas 65 à 78)

**Cas 65 — L'écart-type comme volatilité.** Un gérant compare deux fonds par la dispersion de leurs rendements. *Enseignement* : l'**écart-type** est la brique de base de la volatilité ; il quantifie la dispersion. (§1.6)

**Cas 66 — Le pouvoir de la corrélation.** Deux actifs à corrélation faible réduisent fortement le risque combiné. *Enseignement* : la **corrélation** pilote la diversification ; plus elle est basse, plus le gain est grand. (§1.6)

**Cas 67 — Le repas gratuit.** Combiner des actifs imparfaitement corrélés abaisse le risque sans sacrifier le rendement moyen. *Enseignement* : la **diversification** est le seul « repas gratuit » ; elle opère tant que ρ < 1. (§1.6)

**Cas 68 — La corrélation qui disparaît.** En crise, la diversification cesse de protéger. *Enseignement* : le bénéfice de diversification **s'évapore** quand les corrélations montent — limite structurelle. (§1.6)

**Cas 69 — La VaR à 99 %.** Une VaR 1 jour de 10 M€ à 99 %. *Enseignement* : 1 % de chance de perdre *plus* de 10 M€ en un jour ; la VaR est une perte-seuil, pas un maximum. (§1.6)

**Cas 70 — L'angle mort de la VaR.** Deux portefeuilles ont la même VaR mais des pertes très différentes au-delà. *Enseignement* : la VaR est **aveugle au-delà du seuil** ; d'où l'Expected Shortfall (book 4). (§1.6)

**Cas 71 — Le calcul de variance.** Deux actifs, σ₁ = σ₂ = 20 %, ρ = 0, poids égaux. *Enseignement* : σ(p) = √(0,5²(0,2²) × 2) ≈ 14,1 % ; la diversification réduit le risque sous 20 %. (§1.6)

**Cas 72 — La corrélation parfaite.** Deux actifs à ρ = 1 ne procurent aucun bénéfice de diversification. *Enseignement* : à ρ = 1, le risque du portefeuille égale la moyenne pondérée ; nul gain. (§1.6)

**Cas 73 — La corrélation négative.** Deux actifs à ρ = -1 peuvent, bien dosés, annuler le risque. *Enseignement* : ρ = -1 permet une **couverture parfaite** ; c'est le principe du hedging. (§1.6)

**Cas 74 — Le scénario historique.** Une banque rejoue la crise de 2008 sur son portefeuille actuel. *Enseignement* : l'**analyse de scénarios** historiques teste la résistance à des chocs déjà observés. (§1.6)

**Cas 75 — Le scénario hypothétique.** Une firme imagine un choc inédit non présent dans l'historique. *Enseignement* : le **stress hypothétique** explore les *unknown unknowns* au-delà des données passées. (§1.6)

**Cas 76 — L'horizon de la VaR.** Une VaR 1 jour est convertie en VaR 10 jours. *Enseignement* : sous hypothèses, la VaR se met à l'échelle en **racine du temps** (√(h)) ; l'horizon change le chiffre. (§1.6)

**Cas 77 — L'agrégation naïve.** Additionner les VaR de plusieurs desks surestime le risque total. *Enseignement* : la **diversification** rend la VaR agrégée *inférieure* à la somme des VaR (sauf ρ = 1). (§1.6)

**Cas 78 — Le facteur de risque commun.** Plusieurs positions dépendent d'un même facteur sous-jacent. *Enseignement* : identifier les **facteurs de risque** communs est la clé d'une mesure et d'une agrégation correctes. (§1.6)

## B.7 Création de valeur et couverture (cas 79 à 88)

**Cas 79 — Les coûts de détresse.** Une entreprise proche de la faillite perd clients et talents. *Enseignement* : réduire la probabilité de détresse **crée de la valeur** ; c'est un canal majeur de la gestion du risque. (§1.7)

**Cas 80 — Le lissage des flux.** Une firme couvre ses revenus pour éviter des à-coups de trésorerie. *Enseignement* : le **lissage des cash-flows** préserve la capacité d'investissement et la stabilité. (§1.7)

**Cas 81 — L'argument Modigliani-Miller.** Un académicien objecte qu'en marché parfait, couvrir au niveau firme est inutile. *Enseignement* : en **marché parfait**, la couverture est neutre ; sa valeur vient des *imperfections*. (§1.7)

**Cas 82 — L'actionnaire diversifié.** Un investisseur peut gérer lui-même son exposition via son portefeuille. *Enseignement* : c'est l'argument *contre* la couverture au niveau firme en marché parfait ; il tombe dès qu'il y a frictions. (§1.7)

**Cas 83 — L'investissement préservé.** Une entreprise évite de renoncer à un projet rentable grâce à une trésorerie stable. *Enseignement* : la couverture protège la **capacité d'investissement** — création de valeur indirecte. (§1.7)

**Cas 84 — La couverture coûteuse.** Une firme dépense plus en primes que le risque couvert ne le justifie. *Enseignement* : couvrir a un **coût** ; la décision est un arbitrage coût/bénéfice, pas un automatisme. (§1.7)

**Cas 85 — Le risque de base.** Une compagnie couvre du kérosène avec des futures de brut. *Enseignement* : la couverture imparfaite laisse un **risque de base** ; les deux prix ne bougent pas exactement de concert. (§1.7)

**Cas 86 — La couverture qui crée un risque.** Un hedge génère des appels de marge imprévus. *Enseignement* : couvrir peut **transformer** un risque de marché en risque de *liquidité* (cf. Metallgesellschaft). (§1.7)

**Cas 87 — La sur-couverture.** Une firme couvre au-delà de son exposition réelle et se retrouve spéculative. *Enseignement* : couvrir plus que l'exposition *crée* une position directionnelle ; la couverture doit rester adossée. (§1.7)

**Cas 88 — La fiscalité convexe.** Une entreprise réduit sa charge fiscale espérée en lissant ses résultats. *Enseignement* : sous impôt **convexe**, le lissage des résultats abaisse l'impôt moyen — canal fiscal de création de valeur. (§1.7)

## B.8 Atténuation, appétit et gouvernance du risque (cas 89 à 100)

**Cas 89 — Éviter le risque.** Une banque renonce à une activité jugée trop risquée. *Enseignement* : **éviter** est l'un des quatre traitements ; parfois le plus sage, mais au prix du rendement associé. (§1.8)

**Cas 90 — Retenir et capitaliser.** Une firme conserve un risque rémunérateur en l'adossant à du capital. *Enseignement* : **retenir** est légitime quand le risque est rentable ou trop coûteux à transférer. (§1.8)

**Cas 91 — Atténuer par la limite.** Un desk se voit imposer un plafond de position. *Enseignement* : les **limites** atténuent le risque en bornant l'exposition ; traduction opérationnelle de l'appétit. (§1.8)

**Cas 92 — Transférer par l'assurance.** Une entreprise assure un risque contre une prime. *Enseignement* : **transférer** déplace le risque vers un tiers ; l'assurance en est la forme classique. (§1.8)

**Cas 93 — Transférer par la titrisation.** Une banque cède un portefeuille de prêts au marché. *Enseignement* : la **titrisation** transfère le risque de crédit ; saine en principe, dangereuse si elle déresponsabilise (2008). (§1.8)

**Cas 94 — Diversifier plutôt que couvrir.** Une firme réduit son risque idiosyncratique en multipliant les positions. *Enseignement* : la **diversification** atténue le risque spécifique *sans coût direct*, mais pas le systématique. (§1.8)

**Cas 95 — L'appétit défini par le conseil.** Le board fixe la quantité globale de risque acceptable. *Enseignement* : l'**appétit pour le risque** est une orientation stratégique du **conseil**, sommet du dispositif. (§1.8)

**Cas 96 — La limite déclinée.** L'appétit global est traduit en plafonds de VaR par desk. *Enseignement* : la chaîne **appétit → tolérance → limites** descend du stratégique à l'opérationnel. (§1.9)

**Cas 97 — Le découplage fatal.** Les risques pris sur le terrain dépassent l'appétit affiché, faute de limites effectives. *Enseignement* : un **découplage** entre appétit et prises réelles est à l'origine de nombreux désastres. (§1.9)

**Cas 98 — Barings et la séparation des tâches.** Un trader cumule front et back office et masque ses pertes. *Enseignement* : **risque opérationnel** ; l'absence de **séparation des tâches** rend la fraude possible. (§1.10)

**Cas 99 — LTCM et le levier.** Des modèles supposant des corrélations stables sont ruinés par un choc, amplifié par le levier. *Enseignement* : **risque de modèle et de levier** ; les corrélations convergent en crise. (§1.10)

**Cas 100 — La leçon transversale.** Chaque grande faillite viole une brique fondamentale du chapitre. *Enseignement* : identification, surveillance, séparation, humilité face aux corrélations — les désastres **sanctionnent** l'oubli d'un principe de base. (§1.10)

# Partie C — Méthodologie et informations complémentaires

## C.1 Comment aborder les questions du chapitre 1

Ce chapitre étant **qualitatif**, les questions se jouent sur des **nuances de définition** et des **associations** (quel risque illustre quel cas). Trois réflexes : lire chaque énoncé **mot à mot** (un adjectif change la réponse) ; se méfier des formulations **absolues** (« toujours », « jamais » sont souvent faux) ; privilégier la réponse la plus **nuancée et rigoureuse**. Les rares calculs (EL, variance, VaR simple) doivent être **automatiques** pour ne pas perdre de temps.

## C.2 Tableau de synthèse

| **Notion** | **À retenir** | **Piège à éviter** |
| --- | --- | --- |
| Risque | Variabilité des issues (upside & downside) | Le réduire à la seule perte. |
| Risque vs incertitude | Probabilités connues (Knight) vs inconnues | Modéliser l'incertitude comme du risque. |
| Processus | Identifier, mesurer, gérer, surveiller (boucle) | Oublier la surveillance ; mesurer avant d'identifier. |
| Catégories | Marché, crédit, liquidité, opérationnel, business, stratégique, réputation | Croire les catégories étanches. |
| Known/unknown | Unknown unknowns = les plus dangereux | Croire les modèles exhaustifs. |
| EL vs UL | EL se provisionne, UL s'absorbe par le capital | Confondre provision et capital. |
| Diversification | Réduit l'idiosyncratique si ρ<1 | L'attendre en crise (ρ→ 1). |
| VaR | Perte-seuil à un niveau et un horizon | La lire comme une perte maximale. |
| Valeur de la couverture | Coûts de détresse, lissage, investissement | Couvrir à 100 % par principe. |
| Appétit/limites | Appétit → tolérance → limites | Découpler l'appétit des prises réelles. |

## C.3 Glossaire bilingue des termes clés

Le curriculum FRM est en anglais ; maîtriser la terminologie évite les pièges de traduction.

| **Terme anglais** | **Sens** |
| --- | --- |
| Expected / Unexpected Loss | Perte attendue (provisionnée) / inattendue (capital). |
| Risk appetite / tolerance / limits | Appétit / tolérance / limites de risque. |
| Hedging / basis risk | Couverture / risque de base (imperfection de couverture). |
| Systematic / idiosyncratic risk | Risque systématique (non diversifiable) / idiosyncratique (diversifiable). |
| Known unknowns / unknown unknowns | Risques identifiés mais non chiffrés / non identifiés. |
| Value-at-Risk (VaR) | Perte-seuil à un niveau de confiance et un horizon. |
| Tail risk | Risque extrême, au-delà des mesures usuelles. |
| PD / LGD / EAD | Probabilité de défaut / perte en cas de défaut / exposition au défaut. |

## C.4 Pourquoi ce chapitre est fondamental pour la suite

Chaque notion posée ici **irrigue** le reste du programme, et sa maîtrise conditionne la compréhension des books ultérieurs. La distinction **EL / UL** structure le risque de crédit (book 2 Partie I et book 2 Partie II) et le capital réglementaire. Les **catégories de risque** organisent l'ensemble du cursus, chaque book approfondissant une famille. La **VaR** et le **stress testing**, esquissés ici, sont le cœur du book 4 (Partie I) et du book 1 (Partie II). La **diversification** et le couple **systématique/idiosyncratique** fondent la théorie du portefeuille (chapitres 5-6). Les **outils d'atténuation** (couverture, transfert) annoncent les dérivés (book 3) et la titrisation (chapitre 4, book 2 Partie II). Enfin, l'**appétit** et la **gouvernance** du risque sont développés au chapitre 3. En somme, ce chapitre est le **socle conceptuel** : le maîtriser, c'est se donner une grille de lecture pour tout le FRM.

> **⚑ Point d'examen —** Ne « survole » pas ce chapitre sous prétexte qu'il est peu calculatoire. Sa maîtrise **conceptuelle** paie dans les dix books : les questions les plus subtiles de l'examen reposent sur ces définitions fondatrices. Un candidat qui confond EL et UL, ou systématique et idiosyncratique, se pénalise sur *tout* le programme.

# Partie D — 100 questions à choix multiple

Questions originales de style examen, réparties par thème. Le corrigé commenté figure en Partie E, avec renvoi à la section de cours ou au cas pertinent.

## D.1 Nature du risque et incertitude (Q1 à Q14)

**Q1.** En finance, le risque désigne —

- **(A)** la variabilité des résultats futurs
- **(B)** une perte certaine
- **(C)** l'absence de rendement
- **(D)** une provision comptable
**Q2.** Une perte certaine, connue à l'avance, est —

- **(A)** un risque de marché
- **(B)** un coût, non un risque
- **(C)** un risque opérationnel
- **(D)** une perte inattendue
**Q3.** Le risque combine deux éléments —

- **(A)** rendement et liquidité
- **(B)** provision et capital
- **(C)** exposition et incertitude
- **(D)** PD et LGD
**Q4.** Selon Knight, le risque se distingue de l'incertitude par —

- **(A)** son ampleur
- **(B)** sa catégorie
- **(C)** son horizon
- **(D)** le fait que ses probabilités sont estimables
**Q5.** Pour une institution financière, prendre du risque —

- **(A)** est sa raison d'être, contre rémunération
- **(B)** est toujours une erreur
- **(C)** doit être totalement évité
- **(D)** n'a aucun lien avec le rendement
**Q6.** Le but de la gestion des risques est —

- **(A)** de supprimer tout risque
- **(B)** de choisir et dimensionner le risque
- **(C)** de maximiser la volatilité
- **(D)** d'éliminer le rendement
**Q7.** L'*upside* d'un risque désigne —

- **(A)** la perte maximale
- **(B)** le capital requis
- **(C)** un résultat meilleur que prévu
- **(D)** la probabilité de défaut
**Q8.** Un rendement élevé s'accompagne généralement —

- **(A)** d'un risque faible
- **(B)** d'aucun risque
- **(C)** d'une perte certaine
- **(D)** d'un risque élevé
**Q9.** L'incertitude au sens de Knight concerne des probabilités —

- **(A)** inconnues ou non estimables
- **(B)** toujours égales à 0,5
- **(C)** parfaitement connues
- **(D)** nulles
**Q10.** Le risque acceptable pour un investisseur dépend —

- **(A)** uniquement de l'actif
- **(B)** de ses objectifs et de son horizon
- **(C)** du seul rendement moyen
- **(D)** de facteurs sans rapport
**Q11.** Décrire un fonds par son seul rendement moyen —

- **(A)** suffit à mesurer son risque
- **(B)** est la meilleure pratique
- **(C)** ignore la dispersion, donc le risque
- **(D)** mesure la corrélation
**Q12.** Conserver une part de risque lié au cœur de métier —

- **(A)** est toujours interdit
- **(B)** est toujours une faute
- **(C)** supprime tout rendement
- **(D)** peut être un choix rationnel
**Q13.** Promettre un modèle prédisant « exactement » les pertes futures —

- **(A)** est un signal d'alerte (fausse précision)
- **(B)** est la norme du secteur
- **(C)** élimine l'incertitude
- **(D)** garantit la justesse
**Q14.** Une aversion excessive au risque —

- **(A)** maximise toujours la valeur
- **(B)** peut détruire de la valeur
- **(C)** est sans conséquence
- **(D)** supprime tous les coûts
## D.2 Le processus de gestion des risques (Q15 à Q26)

**Q15.** Les quatre étapes du processus de gestion des risques sont —

- **(A)** acheter, vendre, couvrir, clôturer
- **(B)** prévoir, espérer, subir, oublier
- **(C)** identifier, mesurer, gérer, surveiller
- **(D)** provisionner, capitaliser, stresser, publier
**Q16.** La première étape, la plus sous-estimée, est —

- **(A)** la mesure
- **(B)** la couverture
- **(C)** la publication
- **(D)** l'identification
**Q17.** Le processus de gestion des risques est —

- **(A)** itératif (une boucle continue)
- **(B)** linéaire et ponctuel
- **(C)** réalisé une fois pour toutes
- **(D)** dépourvu de surveillance
**Q18.** Un chiffre de risque faux —

- **(A)** vaut toujours mieux qu'aucun chiffre
- **(B)** est plus dangereux qu'une absence de chiffre
- **(C)** n'a aucun effet
- **(D)** garantit la prudence
**Q19.** Mesurer un risque sans prendre de décision —

- **(A)** suffit à le gérer
- **(B)** est l'objectif final
- **(C)** n'est pas le gérer
- **(D)** transfère le risque
**Q20.** On ne peut mesurer un risque que si on l'a d'abord —

- **(A)** couvert
- **(B)** provisionné
- **(C)** publié
- **(D)** identifié
**Q21.** La surveillance sert à —

- **(A)** vérifier que les décisions restent valides dans un monde qui change
- **(B)** supprimer l'identification
- **(C)** fixer l'appétit
- **(D)** calculer la probabilité de défaut
**Q22.** Retenir un risque sans décision explicite —

- **(A)** est impossible
- **(B)** reste une décision (l'inaction en est une)
- **(C)** élimine le risque
- **(D)** le transfère automatiquement
**Q23.** L'identification des risques opérationnels gagne à —

- **(A)** rester au sommet de la hiérarchie
- **(B)** ignorer le terrain
- **(C)** remonter du terrain
- **(D)** être laissée au hasard
**Q24.** Un tableau de bord réduit à un indicateur unique peut —

- **(A)** capturer toutes les interactions
- **(B)** remplacer la gouvernance
- **(C)** éliminer le risque
- **(D)** masquer une accumulation de risques corrélés
**Q25.** La réponse à un risque doit être —

- **(A)** proportionnée à l'enjeu
- **(B)** toujours maximale
- **(C)** toujours nulle
- **(D)** indépendante du coût
**Q26.** Un processus formellement complet mais sans culture du risque —

- **(A)** protège toujours
- **(B)** peut échouer
- **(C)** est optimal
- **(D)** supprime les pertes
## D.3 Typologie des risques (Q27 à Q44)

**Q27.** Une perte due à une hausse des taux relève du risque —

- **(A)** opérationnel
- **(B)** de crédit
- **(C)** de marché
- **(D)** de réputation
**Q28.** Le défaut d'une contrepartie relève du risque —

- **(A)** de marché
- **(B)** de liquidité
- **(C)** opérationnel
- **(D)** de crédit
**Q29.** Ne pas pouvoir liquider un actif sans perte excessive est un risque —

- **(A)** de liquidité
- **(B)** de crédit
- **(C)** stratégique
- **(D)** de réputation
**Q30.** Une défaillance de système ou de processus est un risque —

- **(A)** de marché
- **(B)** opérationnel
- **(C)** de crédit
- **(D)** business
**Q31.** Une chute des revenus due à la contraction du marché est un risque —

- **(A)** opérationnel
- **(B)** de crédit
- **(C)** business
- **(D)** de liquidité
**Q32.** Rater une transition technologique majeure est un risque —

- **(A)** de marché
- **(B)** de crédit
- **(C)** opérationnel
- **(D)** stratégique
**Q33.** Le risque de réputation est —

- **(A)** souvent la conséquence d'un autre risque mal géré
- **(B)** toujours indépendant des autres risques
- **(C)** une forme de risque de marché
- **(D)** sans impact réel
**Q34.** Une amende pour non-conformité relève du risque —

- **(A)** de marché
- **(B)** légal et réglementaire
- **(C)** idiosyncratique
- **(D)** systématique
**Q35.** Le risque idiosyncratique —

- **(A)** ne peut être diversifié
- **(B)** est toujours rémunéré
- **(C)** se diversifie en combinant des positions
- **(D)** est le risque de marché
**Q36.** Le risque systématique —

- **(A)** se diversifie facilement
- **(B)** est propre à une seule entité
- **(C)** n'est jamais rémunéré
- **(D)** ne se diversifie pas et est rémunéré
**Q37.** Qu'un risque de crédit déclenche un problème de liquidité illustre —

- **(A)** qu'un risque se transforme en un autre
- **(B)** l'étanchéité des catégories
- **(C)** une erreur de mesure
- **(D)** l'absence de risque
**Q38.** Une exposition en devise non couverte est un risque —

- **(A)** opérationnel
- **(B)** de marché (change)
- **(C)** de crédit
- **(D)** de réputation
**Q39.** Le détournement de fonds par un employé est un risque —

- **(A)** de marché
- **(B)** de crédit
- **(C)** opérationnel (fraude interne)
- **(D)** stratégique
**Q40.** Le passage d'une notation de A à BBB sans défaut est un risque de crédit par —

- **(A)** liquidité
- **(B)** fraude
- **(C)** choc de marché
- **(D)** migration
**Q41.** Ne plus pouvoir renouveler ses financements courts est un risque de liquidité —

- **(A)** de financement
- **(B)** de marché
- **(C)** opérationnel
- **(D)** de crédit
**Q42.** La chute d'un acteur menaçant tout le système par interconnexion est un risque —

- **(A)** purement idiosyncratique
- **(B)** systémique
- **(C)** opérationnel isolé
- **(D)** business
**Q43.** Une concentration cachée sur un même facteur —

- **(A)** renforce la diversification
- **(B)** est sans effet
- **(C)** annule la diversification apparente
- **(D)** supprime le risque
**Q44.** Une cyberattaque causant pertes et atteinte à l'image —

- **(A)** relève d'une seule catégorie
- **(B)** n'est pas un risque
- **(C)** est un pur risque de marché
- **(D)** active plusieurs catégories à la fois
## D.4 Risques connus, inconnus et limites des modèles (Q45 à Q54)

**Q45.** Un risque identifié et mesuré est un —

- **(A)** known known
- **(B)** known unknown
- **(C)** unknown unknown
- **(D)** cygne noir
**Q46.** Un risque identifié mais non chiffré est un —

- **(A)** known known
- **(B)** known unknown
- **(C)** unknown unknown
- **(D)** risque nul
**Q47.** Les risques les plus dangereux sont les —

- **(A)** known knowns
- **(B)** known unknowns
- **(C)** unknown unknowns
- **(D)** risques déjà provisionnés
**Q48.** Les modèles quantitatifs traitent surtout bien —

- **(A)** les unknown unknowns
- **(B)** l'incertitude de Knight
- **(C)** les cygnes noirs
- **(D)** les known knowns
**Q49.** En période de crise, les corrélations —

- **(A)** montent vers 1, annulant la diversification
- **(B)** tombent à zéro
- **(C)** restent parfaitement stables
- **(D)** deviennent négatives partout
**Q50.** Les distributions réelles de rendements ont —

- **(A)** des queues plus fines que la normale
- **(B)** des queues épaisses, sous-estimées par la loi normale
- **(C)** aucune queue
- **(D)** une variance nulle
**Q51.** Le stress testing sert à —

- **(A)** remplacer l'identification
- **(B)** calculer la probabilité de défaut
- **(C)** révéler des vulnérabilités invisibles aux mesures statistiques
- **(D)** supprimer le risque
**Q52.** Une VaR affichée à la décimale près —

- **(A)** garantit la justesse
- **(B)** élimine l'incertitude
- **(C)** est toujours exacte
- **(D)** peut masquer une incertitude structurelle
**Q53.** Face aux unknown unknowns, le jugement humain —

- **(A)** complète, voire corrige, les modèles
- **(B)** est inutile
- **(C)** doit être systématiquement ignoré
- **(D)** remplace toute mesure
**Q54.** Un événement « impossible » selon les modèles qui se réalise est —

- **(A)** un known known
- **(B)** un cygne noir (événement extrême)
- **(C)** une provision
- **(D)** une corrélation
## D.5 Risque, rendement et pertes (Q55 à Q68)

**Q55.** Un rendement élevé sans aucun risque —

- **(A)** est courant sur les marchés
- **(B)** est garanti par l'État
- **(C)** est impossible
- **(D)** définit la prime de risque
**Q56.** La perte attendue (EL) —

- **(A)** s'absorbe par le capital
- **(B)** est ignorée dans le prix
- **(C)** constitue le tail risk
- **(D)** se provisionne et se tarife
**Q57.** La perte inattendue (UL) —

- **(A)** s'absorbe par le capital économique
- **(B)** se provisionne
- **(C)** est le coût moyen
- **(D)** est toujours nulle
**Q58.** Avec PD = 2 %, LGD = 40 %, EAD = 1 M€, la perte attendue vaut —

- **(A)** 20 000 €
- **(B)** 8 000 €
- **(C)** 40 000 €
- **(D)** 2 000 €
**Q59.** Si le taux de recouvrement est de 70 %, la LGD est —

- **(A)** 70 %
- **(B)** 100 %
- **(C)** 30 %
- **(D)** 0 %
**Q60.** Ne pas intégrer l'EL dans le prix d'un prêt conduit à —

- **(A)** un profit garanti
- **(B)** une couverture parfaite
- **(C)** une réduction du capital
- **(D)** des pertes structurelles
**Q61.** Le tail risk désigne —

- **(A)** le risque extrême, au-delà même du capital
- **(B)** la perte moyenne
- **(C)** la provision
- **(D)** le rendement sans risque
**Q62.** La prime de risque —

- **(A)** est un cadeau sans contrepartie
- **(B)** rémunère l'incertitude assumée
- **(C)** réduit le rendement
- **(D)** est toujours nulle
**Q63.** L'EAD d'une ligne de crédit —

- **(A)** est toujours l'encours nominal fixe
- **(B)** est nulle
- **(C)** dépend du tirage au moment du défaut
- **(D)** égale la LGD
**Q64.** Entre deux portefeuilles de même risque, on privilégie —

- **(A)** le plus volatil
- **(B)** le moins rentable
- **(C)** l'un au hasard
- **(D)** celui au meilleur rendement
**Q65.** Une faible EL mais une forte variabilité implique —

- **(A)** un besoin en capital élevé (UL)
- **(B)** aucun besoin en capital
- **(C)** une perte certaine
- **(D)** une PD nulle
**Q66.** L'ossature prudentielle associe —

- **(A)** le prix à l'UL
- **(B)** le prix à l'attendu, le capital à l'inattendu, le stress à l'extrême
- **(C)** le capital à l'EL
- **(D)** le stress à la moyenne
**Q67.** La LGD mesure la perte —

- **(A)** brute, avant recouvrement
- **(B)** de marché
- **(C)** nette du recouvrement
- **(D)** opérationnelle
**Q68.** La formule de la perte attendue est —

- **(A)** PD + LGD + EAD
- **(B)** PD − LGD
- **(C)** LGD × EAD seulement
- **(D)** PD × LGD × EAD
## D.6 La mesure du risque (Q69 à Q82)

**Q69.** L'écart-type mesure —

- **(A)** la dispersion des rendements
- **(B)** la moyenne
- **(C)** la corrélation
- **(D)** la probabilité de défaut
**Q70.** La corrélation pilote —

- **(A)** la probabilité de défaut
- **(B)** le bénéfice de diversification
- **(C)** l'exposition au défaut
- **(D)** la maturité
**Q71.** La diversification opère tant que la corrélation est —

- **(A)** égale à 1
- **(B)** négative uniquement
- **(C)** inférieure à 1
- **(D)** nulle uniquement
**Q72.** En période de crise, le bénéfice de diversification —

- **(A)** augmente
- **(B)** reste stable
- **(C)** devient infini
- **(D)** s'évapore quand les corrélations montent
**Q73.** Une VaR 1 jour de 10 M€ à 99 % signifie —

- **(A)** 1 % de chance de perdre plus de 10 M€ en un jour
- **(B)** une perte maximale de 10 M€
- **(C)** une perte certaine de 10 M€
- **(D)** 99 % de chance de tout perdre
**Q74.** La principale limite de la VaR est —

- **(A)** sa simplicité
- **(B)** son aveuglement à ce qui se passe au-delà du seuil
- **(C)** son coût de calcul
- **(D)** son horizon
**Q75.** Deux actifs de volatilité 20 %, corrélation nulle, à poids égaux, donnent une volatilité de portefeuille d'environ —

- **(A)** 20 %
- **(B)** 28 %
- **(C)** 14,1 %
- **(D)** 0 %
**Q76.** Deux actifs à corrélation de 1 procurent —

- **(A)** une couverture parfaite
- **(B)** un fort gain de diversification
- **(C)** un risque nul
- **(D)** aucun bénéfice de diversification
**Q77.** Deux actifs à corrélation de −1, bien dosés, permettent —

- **(A)** une couverture parfaite (risque annulable)
- **(B)** un risque maximal
- **(C)** aucun effet
- **(D)** une perte certaine
**Q78.** Rejouer la crise de 2008 sur le portefeuille actuel est —

- **(A)** un stress hypothétique
- **(B)** une analyse de scénario historique
- **(C)** un calcul de probabilité de défaut
- **(D)** une provision
**Q79.** Un scénario de choc inédit, absent de l'historique, est —

- **(A)** une VaR paramétrique
- **(B)** une analyse historique
- **(C)** un stress hypothétique
- **(D)** une corrélation
**Q80.** Sous hypothèses, la VaR se met à l'échelle temporelle en —

- **(A)** proportion linéaire du temps
- **(B)** carré du temps
- **(C)** logarithme du temps
- **(D)** racine du temps
**Q81.** Additionner les VaR de plusieurs desks —

- **(A)** surestime le risque total (diversification ignorée)
- **(B)** le sous-estime toujours
- **(C)** donne un résultat exact
- **(D)** donne zéro
**Q82.** Identifier les facteurs de risque communs est essentiel pour —

- **(A)** supprimer le risque
- **(B)** une mesure et une agrégation correctes
- **(C)** fixer la probabilité de défaut
- **(D)** publier les comptes
## D.7 Création de valeur et couverture (Q83 à Q92)

**Q83.** Réduire la probabilité de détresse financière —

- **(A)** détruit de la valeur
- **(B)** est neutre
- **(C)** crée de la valeur
- **(D)** augmente la probabilité de défaut
**Q84.** Le lissage des cash-flows —

- **(A)** empêche l'investissement
- **(B)** est sans effet
- **(C)** augmente le risque
- **(D)** préserve la capacité d'investissement
**Q85.** En marché parfait (Modigliani-Miller), couvrir au niveau de la firme est —

- **(A)** neutre
- **(B)** toujours créateur de valeur
- **(C)** interdit
- **(D)** obligatoire
**Q86.** L'argument de l'actionnaire diversifié plaide —

- **(A)** pour la couverture au niveau firme
- **(B)** contre la couverture au niveau firme en marché parfait
- **(C)** pour un levier maximal
- **(D)** pour la spéculation
**Q87.** Puisque couvrir a un coût, la couverture —

- **(A)** doit toujours être totale
- **(B)** est toujours gratuite
- **(C)** relève d'un arbitrage coût/bénéfice
- **(D)** est sans intérêt
**Q88.** Couvrir du kérosène avec des futures de brut laisse —

- **(A)** aucun risque résiduel
- **(B)** un risque de crédit
- **(C)** un risque strictement nul
- **(D)** un risque de base
**Q89.** Une couverture peut transformer un risque de marché en —

- **(A)** risque de liquidité (appels de marge)
- **(B)** risque nul
- **(C)** profit garanti
- **(D)** risque de réputation
**Q90.** Couvrir au-delà de son exposition réelle —

- **(A)** reste une simple couverture
- **(B)** crée une position spéculative
- **(C)** supprime le risque
- **(D)** est recommandé
**Q91.** Sous impôt convexe, lisser les résultats —

- **(A)** augmente l'impôt
- **(B)** est sans effet
- **(C)** abaisse l'impôt moyen espéré
- **(D)** supprime l'impôt
**Q92.** La diversification, contrairement à la couverture —

- **(A)** a un coût direct élevé
- **(B)** élimine le risque systématique
- **(C)** crée un risque de base
- **(D)** réduit l'idiosyncratique sans coût direct
## D.8 Atténuation, appétit et gouvernance (Q93 à Q100)

**Q93.** Renoncer à une activité jugée trop risquée relève de —

- **(A)** éviter
- **(B)** retenir
- **(C)** transférer
- **(D)** diversifier
**Q94.** Conserver un risque rémunérateur adossé à du capital relève de —

- **(A)** éviter
- **(B)** retenir
- **(C)** transférer
- **(D)** l'assurance
**Q95.** Imposer un plafond de position à un desk relève de —

- **(A)** transférer
- **(B)** éviter
- **(C)** atténuer par la limite
- **(D)** l'assurance
**Q96.** Céder un risque à un tiers contre une prime relève de —

- **(A)** éviter
- **(B)** retenir
- **(C)** diversifier
- **(D)** transférer
**Q97.** L'appétit pour le risque est fixé par —

- **(A)** le conseil (orientation stratégique)
- **(B)** chaque trader individuellement
- **(C)** le régulateur seul
- **(D)** le back office
**Q98.** La chaîne de déclinaison du risque va —

- **(A)** des limites vers l'appétit
- **(B)** de l'appétit aux limites, via la tolérance
- **(C)** du terrain au hasard
- **(D)** sans structure définie
**Q99.** Un découplage entre appétit affiché et prises réelles —

- **(A)** est sans danger
- **(B)** améliore la performance
- **(C)** est à l'origine de nombreux désastres
- **(D)** supprime le risque
**Q100.** Le cas Barings illustre avant tout un risque —

- **(A)** de marché
- **(B)** de crédit
- **(C)** systémique
- **(D)** opérationnel (défaut de séparation des tâches)
# Partie E — Corrigé commenté

Chaque réponse renvoie à la section de cours et, le cas échéant, au cas ou au calcul correspondant.

## E.1 Corrigé — Q1 à Q14 (nature du risque et incertitude)

**Q1 — A** *§1.1* Le risque est la **variabilité des résultats futurs**, à la hausse comme à la baisse.

**Q2 — B** *§1.1 · cas 1* Une perte certaine est un **coût**, pas un risque : le risque naît de l'incertitude.

**Q3 — C** *§1.1* Le risque combine **exposition** et **incertitude**.

**Q4 — D** *§1.1* Chez Knight, le risque a des probabilités **estimables** ; l'incertitude non.

**Q5 — A** *§1.1* Une institution financière existe pour **assumer des risques** contre rémunération.

**Q6 — B** *§1.1* Gérer le risque, c'est le **choisir et le dimensionner**, non le supprimer.

**Q7 — C** *§1.1* L'*upside* est un résultat **meilleur que prévu**.

**Q8 — D** *§1.5* Un rendement élevé s'accompagne d'un **risque élevé**.

**Q9 — A** *§1.1* L'incertitude de Knight : probabilités **inconnues ou non estimables**.

**Q10 — B** *§1.1 · cas 10* Le risque acceptable dépend des **objectifs et de l'horizon**.

**Q11 — C** *§1.1* La moyenne seule **ignore la dispersion**, donc le risque.

**Q12 — D** *§1.1* Conserver une part du risque du cœur de métier peut être un **choix rationnel**.

**Q13 — A** *§1.2* Promettre l'exactitude est un signal de **fausse précision**.

**Q14 — B** *§1.1* Une aversion excessive peut **détruire de la valeur**.

## E.2 Corrigé — Q15 à Q26 (le processus de gestion des risques)

**Q15 — C** *§1.2* Les quatre étapes : **identifier, mesurer, gérer, surveiller**.

**Q16 — D** *§1.2* La première étape, la plus sous-estimée, est l'**identification**.

**Q17 — A** *§1.2* Le processus est **itératif** (une boucle continue).

**Q18 — B** *§1.2 · cas 15* Un chiffre faux est **plus dangereux** qu'une absence de chiffre.

**Q19 — C** *§1.2* Mesurer **n'est pas gérer** : la décision doit suivre.

**Q20 — D** *§1.2* On ne mesure que ce qu'on a d'abord **identifié**.

**Q21 — A** *§1.2* La surveillance vérifie que les décisions **restent valides**.

**Q22 — B** *§1.2 · cas 22* L'inaction **reste une décision**.

**Q23 — C** *§1.2* L'identification gagne à **remonter du terrain**.

**Q24 — D** *§1.2* Un indicateur unique peut **masquer des risques corrélés**.

**Q25 — A** *§1.2* La réponse doit être **proportionnée** à l'enjeu.

**Q26 — B** *§1.2* Sans culture du risque, un processus **peut échouer**.

## E.3 Corrigé — Q27 à Q44 (typologie des risques)

**Q27 — C** *§1.3* Une hausse des taux relève du risque **de marché**.

**Q28 — D** *§1.3* Le défaut d'une contrepartie relève du risque **de crédit**.

**Q29 — A** *§1.3* Un actif illiquide relève du risque **de liquidité**.

**Q30 — B** *§1.3* Une défaillance de système relève du risque **opérationnel**.

**Q31 — C** *§1.3* La contraction du marché relève du risque **business**.

**Q32 — D** *§1.3* Une transition ratée relève du risque **stratégique**.

**Q33 — A** *§1.3* La réputation est souvent la **conséquence** d'un autre risque.

**Q34 — B** *§1.3* Une amende relève du risque **légal et réglementaire**.

**Q35 — C** *§1.3* Le risque idiosyncratique **se diversifie**.

**Q36 — D** *§1.3* Le risque systématique **ne se diversifie pas** et est rémunéré.

**Q37 — A** *§1.3* Un risque se **transforme en un autre** ; les catégories ne sont pas étanches.

**Q38 — B** *§1.3* Une devise non couverte relève du risque de marché (**change**).

**Q39 — C** *§1.3* Un détournement relève du risque opérationnel (**fraude interne**).

**Q40 — D** *§1.3* Un passage de A à BBB sans défaut est un risque de **migration**.

**Q41 — A** *§1.3* Des financements non renouvelés : liquidité **de financement**.

**Q42 — B** *§1.3* L'interconnexion relève du risque **systémique**.

**Q43 — C** *§1.3* La concentration **annule la diversification** apparente.

**Q44 — D** *§1.3* Une cyberattaque active **plusieurs catégories** à la fois.

## E.4 Corrigé — Q45 à Q54 (risques connus, inconnus, limites des modèles)

**Q45 — A** *§1.4* Identifié et mesuré : **known known**.

**Q46 — B** *§1.4* Identifié mais non chiffré : **known unknown**.

**Q47 — C** *§1.4* Les plus dangereux sont les **unknown unknowns**.

**Q48 — D** *§1.4* Les modèles traitent bien les **known knowns**.

**Q49 — A** *§1.4* En crise, les corrélations **montent vers 1**.

**Q50 — B** *§1.4* Les rendements réels ont des **queues épaisses**.

**Q51 — C** *§1.4* Le stress révèle des vulnérabilités **invisibles aux mesures** statistiques.

**Q52 — D** *§1.4* Une VaR très précise peut masquer une **incertitude structurelle**.

**Q53 — A** *§1.4* Le jugement **complète, voire corrige** les modèles.

**Q54 — B** *§1.4* Un événement « impossible » réalisé est un **cygne noir**.

## E.5 Corrigé — Q55 à Q68 (risque, rendement, pertes attendues/inattendues)

**Q55 — C** *§1.5* Un rendement élevé sans aucun risque est **impossible**.

**Q56 — D** *§1.5* La perte attendue se **provisionne et se tarife**.

**Q57 — A** *§1.5* La perte inattendue s'absorbe par le **capital économique**.

**Q58 — B** *§1.5 · cas 56* EL = 0,02 × 0,40 × 1 M€ = **8 000** €.

**Q59 — C** *§1.5* LGD = 1 − 0,70 = **30 %**.

**Q60 — D** *§1.5* Ne pas tarifer l'EL conduit à des **pertes structurelles**.

**Q61 — A** *§1.5* Le tail risk est le risque extrême **au-delà même du capital**.

**Q62 — B** *§1.5* La prime de risque rémunère l'**incertitude assumée**.

**Q63 — C** *§1.5* L'EAD dépend du **tirage au moment du défaut**.

**Q64 — D** *§1.5* À risque donné, on privilégie le **meilleur rendement**.

**Q65 — A** *§1.5* Une forte variabilité implique un besoin en **capital élevé** (UL).

**Q66 — B** *§1.5* Prix pour l'**attendu**, capital pour l'inattendu, stress pour l'extrême.

**Q67 — C** *§1.5* La LGD est la perte **nette du recouvrement**.

**Q68 — D** *§1.5* EL = **PD × LGD × EAD**.

## E.6 Corrigé — Q69 à Q82 (la mesure du risque)

**Q69 — A** *§1.6* L'écart-type mesure la **dispersion des rendements**.

**Q70 — B** *§1.6* La corrélation pilote le **bénéfice de diversification**.

**Q71 — C** *§1.6* La diversification opère tant que ρ **< 1**.

**Q72 — D** *§1.6* En crise, le bénéfice **s'évapore** (corrélations → 1).

**Q73 — A** *§1.6 · cas 69* 1 % de chance de perdre **plus de 10 M€** en un jour.

**Q74 — B** *§1.6* La VaR est **aveugle au-delà du seuil**.

**Q75 — C** *§1.6 · cas 71* σ(p) = √(0,5² × 0,2² × 2) ≈ **14,1 %**.

**Q76 — D** *§1.6* À ρ = 1 : **aucun bénéfice** de diversification.

**Q77 — A** *§1.6* À ρ = -1 : **couverture parfaite** possible.

**Q78 — B** *§1.6* Rejouer 2008 est une analyse de **scénario historique**.

**Q79 — C** *§1.6* Un choc inédit relève du **stress hypothétique**.

**Q80 — D** *§1.6* La VaR se met à l'échelle en **racine du temps**.

**Q81 — A** *§1.6* Sommer les VaR **surestime** le risque total.

**Q82 — B** *§1.6* Les facteurs communs sont clés pour une **agrégation correcte**.

## E.7 Corrigé — Q83 à Q92 (création de valeur et couverture)

**Q83 — C** *§1.7* Réduire la détresse financière **crée de la valeur**.

**Q84 — D** *§1.7* Le lissage préserve la **capacité d'investissement**.

**Q85 — A** *§1.7* En marché parfait (M-M), la couverture est **neutre**.

**Q86 — B** *§1.7* L'actionnaire diversifié plaide **contre la couverture** au niveau firme.

**Q87 — C** *§1.7* Couvrir relève d'un **arbitrage** coût/bénéfice.

**Q88 — D** *§1.8* Couvrir le kérosène par du brut laisse un **risque de base**.

**Q89 — A** *§1.7 · cas 86* La couverture peut créer des **appels de marge** (risque de liquidité).

**Q90 — B** *§1.7* Sur-couvrir crée une position **spéculative**.

**Q91 — C** *§1.7* Sous impôt convexe, lisser **abaisse l'impôt** espéré.

**Q92 — D** *§1.8* La diversification réduit l'idiosyncratique **sans coût direct**.

## E.8 Corrigé — Q93 à Q100 (atténuation, appétit et gouvernance)

**Q93 — A** *§1.8* Renoncer à une activité relève d'**éviter**.

**Q94 — B** *§1.8* Conserver un risque capitalisé relève de **retenir**.

**Q95 — C** *§1.8* Un plafond de position relève d'**atténuer par la limite**.

**Q96 — D** *§1.8* Céder contre prime relève de **transférer**.

**Q97 — A** *§1.8* L'appétit pour le risque est fixé par le **conseil**.

**Q98 — B** *§1.9* La chaîne va de l'appétit aux limites, **via la tolérance**.

**Q99 — C** *§1.9* Un découplage appétit/prises est à l'origine de **nombreux désastres**.

**Q100 — D** *§1.10* Barings illustre un risque opérationnel (**séparation des tâches**).

---

**Fin du Chapitre 1 — The Building Blocks of Risk Management · Cours · 50 cas · 12 exercices corrigés · conseils méthodologiques · 150 QCM commentés**

