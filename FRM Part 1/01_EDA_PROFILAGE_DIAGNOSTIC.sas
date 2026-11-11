/******************************************************************************
 Programme : 01_EDA_PROFILAGE_DIAGNOSTIC.sas
 Objet     : Explorer le dataset consolide des arretes M et M-1, profiler les 15 CDE et diagnostiquer leur qualite (dimensions strictement utiles), avec comparaison M / M-1 dans chaque tableau.
 Entrees   : out.extraction_DWR_&periode. et out.extraction_DWR_&periode1. (tables consolidees, cle contrat = I_CNTRT_SIEGE-C_CNTRT_TYPE-N_CNTRT)
 Sorties   : UN classeur Excel pour Power BI : &dir_out./PBI_01_Qualite_&periode..xlsx (un onglet par table PUB_* / DIM_*, dont Stage 3) ; tables SAS : out.dq_resultats_&periode., out.dq_res_port_&periode., out.dq_scores_cde_&periode., out.dq_anomalies_&periode., out.dq_registre_&periode.
 Contenu   : S0 parametres | S1 exploration EDA_1 a EDA_8 | S2 diagnostic DQ_1 a DQ_6 | S3 publication | S4 Stage 3 | S5 classeur Excel
******************************************************************************/

%let t_start = %sysfunc(datetime());
/*===========================================================================
  S0. PARAMETRES COMMUNS AUX TROIS PROGRAMMES (a modifier a chaque arrete)
      Ces parametres sont dans 00_PARAMETRES.sas (un seul fichier pour les trois programmes).
      Aucun fichier referentiel n'est lu : tout est ecrit dans les programmes.
===========================================================================*/
/* Parametres communs (arretes, seuils, poids, domaines, formats, couleurs, table de travail work.base) :
   tout est dans 00_PARAMETRES.sas, a modifier une seule fois pour les trois programmes. */
%let dir_prog = /chemin/vers/programmes;          /* dossier contenant 00_PARAMETRES.sas */
%include "&dir_prog./00_PARAMETRES.sas";

/*===========================================================================
  S1.0 Preparation : presence des variables, catalogue des regles et des CDE
===========================================================================*/
/* S1.1 Les variables attendues sont-elles presentes dans les deux tables ? ----- */
data exp_var;
  infile datalines dlm='|' truncover;
  length variable $32 usage $48;
  input variable $ usage $;
datalines;
I_CNTRT_SIEGE|Cle contrat
C_CNTRT_TYPE|Cle contrat ; CDE 6 et 8
N_CNTRT|Cle contrat
I_DOSS_UNIQ|CDE 5 identifiant dossier
I_DOSSR_BCR_SIEGE|CDE 5 composant de la cle dossier
D_DOSSR_BCR|CDE 5 composant de la cle dossier
N_DOSSR_BCR|CDE 5 composant de la cle dossier
D_DOSSR_CREAT|Date de creation du dossier
B_TIERS_RETAIL|CDE 1 perimetre Retail / Corporate
M_CNTRT_DECLS_BILAN|CDE 2 declassement bilan (source f)
M_ENCOURS_DECLSMT|CDE 2 declassement bilan (source e)
M_CNTRT_DECLS_HBILAN|CDE 3 declassement hors bilan
ID_RMPM|CDE 4 identification personne
C_CNTRT_MODLE|CDE 8 modele bilan
C_CNTRT_MODLE_HB|CDE 6 modele hors bilan
final_model|CDE 8 modele retenu
C_NOMNC_ENGGM|CDE 7 identification engagement
Default_date|CDE 9 date de defaut
D_DEFAU|Date de defaut (autre source)
D_1ER_EVT_DEFAU|Date du premier evenement de defaut
TIME_IN_DEF1|CDE 9 duree en annees entamees
TIME_IN_DEF2|CDE 9 duree en mois
C_CNTRT_NIV_RSK|CDE 10 niveau de risque (1=EDX, 2=CER)
EDX_CER|CDE 10 statut EDX / CER derive
B_CNTRT_WRITE_OFF|CDE 11 passage en perte
FLAG_WO|CDE 11 write-off (indicateur secondaire)
Flag_GARAN|CDE 12 presence de garantie
C_GAR_ORIG|CDE 12 origine de la garantie
I_GAR|CDE 12 identifiant de la garantie
Flag_Crelog|CDE 13 garantie Credit Logement
M_BIL_ENCOURS_PERIODE|CDE 14 exposition bilan (BCR)
expo_dwr|CDE 14 exposition bilan (DWR) ; page Stage 3
nature_risque|Page Stage 3 : DOUTEUX / COMPROMIS
PROV_BRUTE|Page Stage 3 : provision
M_LGDD|Page Stage 3 : LGDD
TYPROV|Page Stage 3 : perimetre statistique (S)
SEG_DECL|CDE 15 segmentation Part / PRO
CAV_Type|Categorie de compte a vue
STAT_PERIM|Indicateur de perimetre statistique
M_PROVS_SOLDE|Provision comptable (solde)
D_ARRETE|Date d arrete
D_PHOTO|Date de photo
N_PERD|Periode
;
run;
proc sql;
  create table chk_var as
  select e.variable, e.usage,
         (select count(*) from dictionary.columns
           where libname = upcase(scan("&ds1.", 1, '.')) and memname = upcase(scan("&ds1.", 2, '.'))
             and upcase(name) = upcase(e.variable)) as in_m1,
         (select count(*) from dictionary.columns
           where libname = upcase(scan("&ds.", 1, '.')) and memname = upcase(scan("&ds.", 2, '.'))
             and upcase(name) = upcase(e.variable)) as in_m0
  from exp_var as e;
  select count(*) into :n_absent trimmed from chk_var where in_m1 = 0 or in_m0 = 0;
quit;
%put NOTE: &n_absent. variable(s) attendue(s) absente(s) de l une des deux tables (voir chk_var);

/* S2.1 Catalogue des regles et des CDE (ecrits ici : aucun referentiel a charger) */
data dim_regle;
  infile datalines dlm='|' dsd truncover;
  length RULE_ID $6 LIBELLE $100 CDE_ID $8 SEVERITE $10 DIMENSION $20 CAUSE_RACINE $20 METHODE_IMPACT $20 DRIVER_ID $4 OWNER $30;
  input RULE_ID $ LIBELLE $ CDE_ID $ SEVERITE $ DIMENSION $ CAUSE_RACINE $ METHODE_IMPACT $ DRIVER_ID $ OWNER $;
datalines;
R01|B_TIERS_RETAIL renseigne|CDE-01|Bloquant|Exhaustivite|SOURCE_DATA|NOT_QUANTIFIABLE||Data Owner Credit
R02|Valeur dans le domaine {R, C}|CDE-01|Bloquant|Validite|SOURCE_DATA|NOT_QUANTIFIABLE||Data Owner Credit
R05|Encours declassement bilan renseigne|CDE-02|Bloquant|Exhaustivite|SOURCE_DATA|ESTIMATED|D6|Data Owner Credit
R06|Numerique et >= 0|CDE-02|Bloquant|Validite|SOURCE_DATA|ESTIMATED|D6|Data Owner Credit
N09|Declassement source f = source e (tol. 1 pct)|CDE-02|Majeur|Coherence|SOURCE_DATA|ESTIMATED|D6|Data Owner Credit
R10|Renseigne (0 accepte)|CDE-03|Majeur|Exhaustivite|SOURCE_DATA|NOT_QUANTIFIABLE||Data Owner Credit
R11|>= 0|CDE-03|Bloquant|Validite|SOURCE_DATA|NOT_QUANTIFIABLE||Data Owner Credit
R14|Identifiant personne renseigne|CDE-04|Bloquant|Exhaustivite|SOURCE_DATA|NOT_QUANTIFIABLE||Data Owner Credit
R18|Les 3 composants de la cle dossier renseignes|CDE-05|Bloquant|Exhaustivite|SOURCE_DATA|NOT_QUANTIFIABLE||Data Owner Credit
N01|Identifiant dossier coherent avec ses composants|CDE-05|Majeur|Coherence|TRANSFORMATION|NOT_QUANTIFIABLE||Data Owner Credit
N02|Une ligne par contrat et par arrete|CDE-05|Bloquant|Unicite|TRANSFORMATION|NOT_QUANTIFIABLE||Data Engineering
R22|Contrat ALT porte au moins un modele|CDE-06|Majeur|Validite|MAPPING|NOT_QUANTIFIABLE||Model Owner LGD
R24|Code engagement renseigne|CDE-07|Majeur|Exhaustivite|SOURCE_DATA|NOT_QUANTIFIABLE||Data Owner Credit
R27|Type de contrat renseigne|CDE-08|Bloquant|Exhaustivite|SOURCE_DATA|NOT_QUANTIFIABLE||Data Owner Credit
R28|Type de contrat mappe a un modele|CDE-08|Majeur|Validite|MAPPING|NOT_QUANTIFIABLE||Model Owner LGD
N08|Modele bilan = modele retenu|CDE-08|Majeur|Coherence|MAPPING|NOT_QUANTIFIABLE||Model Owner LGD
R30|Date de defaut renseignee (hors 1901)|CDE-09|Bloquant|Exhaustivite|SOURCE_DATA|CALCULATED|D1|Data Owner Credit
R31|Date de defaut <= arrete|CDE-09|Bloquant|Validite|SOURCE_DATA|CALCULATED|D1|Data Owner Credit
R34|Duree moteur = duree recalculee (+/- 0,5 mois)|CDE-09|Majeur|Exactitude|TRANSFORMATION|CALCULATED|D1|Data Owner Credit
R35|Duree entre 0 et la borne haute plausible|CDE-09|Mineur|Validite|TRANSFORMATION|CALCULATED|D1|Data Owner Credit
R36|Niveau de risque renseigne|CDE-10|Majeur|Exhaustivite|SOURCE_DATA|CALCULATED|D2|Data Owner Credit
R37|Valeur dans {1=EDX, 2=CER}|CDE-10|Majeur|Validite|SOURCE_DATA|CALCULATED|D2|Data Owner Credit
R39|Indicateur dans {N, O}|CDE-11|Majeur|Validite|SOURCE_DATA|CALCULATED|D3|Data Owner Recouvrement
R40|Contrat passe en perte : encours bilan nul|CDE-11|Bloquant|Coherence|TRANSFORMATION|CALCULATED|D3|Data Owner Recouvrement
N11|Flag garantie renseigne|CDE-12|Majeur|Exhaustivite|SOURCE_DATA|CALCULATED|D4|Data Owner Suretes
R44|Flag dans {0,1}|CDE-12|Majeur|Validite|SOURCE_DATA|CALCULATED|D4|Data Owner Suretes
R48|Flag dans {0,1}|CDE-13|Bloquant|Validite|SOURCE_DATA|CALCULATED|D5|Data Owner Suretes
R50|Caution CL => garantie presente|CDE-13|Majeur|Coherence|SOURCE_DATA|CALCULATED|D5|Data Owner Suretes
R51|Encours de periode renseigne|CDE-14|Bloquant|Exhaustivite|SOURCE_DATA|ESTIMATED||Data Owner Credit
R52|Encours >= 0|CDE-14|Bloquant|Validite|SOURCE_DATA|ESTIMATED||Data Owner Credit
R53|Encours BCR = exposition DWR (tol. 1 pct)|CDE-14|Bloquant|Exactitude|SOURCE_DATA|ESTIMATED||Data Owner Credit
R55|Segment renseigne (modele CAV)|CDE-15|Bloquant|Exhaustivite|SOURCE_DATA|CALCULATED|D7|Data Owner Credit
R56|Segment dans {Part, PRO}|CDE-15|Bloquant|Validite|SOURCE_DATA|CALCULATED|D7|Data Owner Credit
N10|D_PHOTO = D_ARRETE et N_PERD = periode traitee||Bloquant|Actualite|TECHNICAL|NOT_QUANTIFIABLE||DQ Office
;
run;
data dim_cde;
  infile datalines dlm='|' dsd truncover;
  length CDE_ID $8 LIBELLE $60 LIBELLE_COURT $20 FAMILLE $20 NATURE $16;
  input CDE_ID $ LIBELLE $ LIBELLE_COURT $ FAMILLE $ NATURE $;
  cde_no = input(substr(CDE_ID, 5, 2), 2.);
datalines;
CDE-01|Perimetre Retail / Corporate|Retail/Corp.|Choix du modele|Code
CDE-02|Encours au declassement bilan|Declass. bilan|Assiette|Montant
CDE-03|Encours au declassement hors bilan|Declass. HB|Assiette|Montant
CDE-04|Identification personne|Personne|Rattachement|Identifiant
CDE-05|Identification dossier BCR|Dossier BCR|Rattachement|Identifiant
CDE-06|Identification Loan IQ (modalite ALT)|Loan IQ|Choix du modele|Code
CDE-07|Identification engagement|Engagement|Choix du modele|Code
CDE-08|Identification contrat (modele)|Contrat/modele|Choix du modele|Code
CDE-09|Duree en defaut|Duree defaut|Niveau de LGD|Date / duree
CDE-10|EDX / CER|EDX/CER|Niveau de LGD|Code
CDE-11|Passage en perte|Write-off|Assiette|Indicateur
CDE-12|Presence de garantie|Garantie|Niveau de LGD|Indicateur
CDE-13|Garantie Credit Logement|Credit Logement|Niveau de LGD|Indicateur
CDE-14|Exposition bilan|Expo bilan|Assiette|Montant
CDE-15|Segmentation Part / PRO|Part/PRO|Choix du modele|Code
;
run;
data dim_dimension;
  infile datalines dlm='|' dsd truncover;
  length DIMENSION $20 LIBELLE $40 EXIGENCE $60;
  input DIMENSION $ LIBELLE $ EXIGENCE $;
datalines;
Exhaustivite|Exhaustivite (completude)|BCBS 239 principe 4
Validite|Validite (domaine de valeurs)|BCBS 239 principe 3 (integrite)
Exactitude|Exactitude (recalcul ou seconde source)|BCBS 239 principe 3
Coherence|Coherence entre variables ou sources|BCBS 239 principe 3 (integrite)
Unicite|Unicite de la cle (grain)|BCBS 239 principe 3 (integrite)
Actualite|Actualite (date de photo = arrete)|BCBS 239 principe 5
;
run;



/*===========================================================================
  Index d'insertion et onglet Parametres
===========================================================================*/

title1 j=l "^S={background=cx0A2F3F color=white fontsize=15pt fontweight=bold} Parametres du diagnostic et choix des dimensions";
title3 j=l "^S={color=cxC00000 fontweight=bold} &banner.";
ods text = "^S={background=cxE1EFF5} Les seuils de statut des regles sont une PROPOSITION a faire valider (Validation des modeles, Data Owners). Les dimensions evaluees sont le strict minimum par nature de donnee ; une dimension absente de la matrice n est pas evaluee volontairement.";
data p_seuil;
  length FAMILLE $60; format VERT ORANGE percent8.1;
  FAMILLE = 'Regles, CDE et dimensions (toutes severites)'; VERT = &v_seuil.; ORANGE = &o_seuil.; output;
run;
title4 j=l "Seuils de statut";
proc print data=p_seuil noobs; run;
title4 j=l "Tolerances";
data p_tol;
  length PARAMETRE $40 VALEUR 8 USAGE $70;
  PARAMETRE = 'tol_decl'; VALEUR = &tol_decl.; USAGE = 'Ecart relatif maximal entre deux sources de montant'; output;
  PARAMETRE = 'tol_duree'; VALEUR = &tol_duree.; USAGE = 'Ecart maximal de duree en defaut (mois)'; output;
  PARAMETRE = 'duree_max'; VALEUR = &duree_max.; USAGE = 'Borne haute plausible de la duree (mois)'; output;
  PARAMETRE = 'nb_min_cell'; VALEUR = &nb_min_cell.; USAGE = 'Effectif minimal pour afficher un taux dans la matrice'; output;
run;
proc print data=p_tol noobs; run;
title4 j=l "Matrice des donnees critiques et des dimensions evaluees (strict minimum)";
proc print data=mat_cde noobs label; label CDE_ID='CDE' DONNEE='Donnee' NATURE='Nature' DIMENSIONS_EVALUEES='Dimensions evaluees' REGLES='Regles'; run;
title4 j=l "Dimensions et rapprochement reglementaire indicatif";
proc print data=dim_dimension noobs; run;
title4 j=l "Variables attendues dans les tables consolidees (1 = presente)";
proc print data=chk_var noobs label; label variable='Variable' usage='Usage' in_m1='Table M-1' in_m0='Table M'; run;
title1;

/*===========================================================================
  S1. EXPLORATION (EDA) DES DEUX ARRETES - etape 2 du CDC (zone d'atterrissage,
      controle d'entree, profilage). Chaque analyse est faite sur M-1 ET sur M,
      puis comparee (colonnes M-1, M, variation) dans le meme onglet Excel.
===========================================================================*/

/* S1.2 Table brute des deux arretes pour le profilage (sans variable derivee) */
data eda_raw;
  set &ds1.(in=in_m1) &ds.(in=in_m0);
  arrete = ifn(in_m1, &periode1., &periode.);
run;

/* S1.3 Volumetrie et grain : combien de lignes, de contrats, de dossiers ? ------
   Hypothese a verifier : 1 ligne = 1 contrat (I_CNTRT_SIEGE - C_CNTRT_TYPE - N_CNTRT)
   et plusieurs contrats par dossier (I_DOSS_UNIQ).                                  */
proc sql;
  create table e1_vol as
  select slot,
         count(*)                         as nb_lignes          label='Lignes',
         count(distinct contrat_id)       as nb_contrats        label='Contrats distincts',
         count(distinct I_DOSS_UNIQ)      as nb_dossiers        label='Dossiers distincts',
         sum(missing(contrat_id))         as nb_cle_incomplete  label='Cles contrat incompletes',
         sum(M_BIL_ENCOURS_PERIODE)       as encours_bilan      label='Encours bilan total (EUR)',
         sum(M_BIL_ENCOURS_PERIODE < 0)   as encours_negatifs   label='Encours bilan negatifs',
         sum(M_BIL_ENCOURS_PERIODE = 0)   as encours_nuls       label='Encours bilan nuls'
  from base group by slot;
  create table e1_dbl as                                  /* cles contrat en doublon */
  select slot, contrat_id, count(*) as nb_lignes
  from base where contrat_id ne '' group by slot, contrat_id having count(*) > 1;
  create table e1_dbl_n as select slot, count(*) as nb_cles_doublon from e1_dbl group by slot;
quit;
proc transpose data=e1_vol out=e1_t(rename=(_label_=indicateur)) name=code;
  id slot;
  var nb_lignes nb_contrats nb_dossiers nb_cle_incomplete encours_bilan encours_negatifs encours_nuls;
run;
data e1_ind;
  set e1_t;
  length lecture $70;
  variation = M0 - M1;
  if M1 not in (., 0) then variation_pct = variation / abs(M1);
  if code = 'nb_cle_incomplete' then lecture = 'Doit etre nul : une cle incomplete ne peut pas etre rapprochee';
  else if code = 'nb_contrats' then lecture = 'Grain vise : un contrat par ligne et par arrete';
  else if code = 'encours_bilan' then lecture = 'Denominateur des taux en encours';
  else if code = 'encours_negatifs' then lecture = 'A comparer a la regle R52 (encours >= 0)';
  format M1 M0 variation comma20.0 variation_pct percent8.1;
  keep indicateur M1 M0 variation variation_pct lecture;
run;
/* Nombre de contrats par dossier (classes) */
proc sql;
  create table e1_d1 as
  select slot, I_DOSS_UNIQ, count(*) as n from base where not missing(I_DOSS_UNIQ) group by slot, I_DOSS_UNIQ;
quit;
data e1_d2;
  set e1_d1;
  length classe $12;
  if n = 1 then do; classe = '1'; ord = 1; end;
  else if n = 2 then do; classe = '2'; ord = 2; end;
  else if n = 3 then do; classe = '3'; ord = 3; end;
  else if n <= 9 then do; classe = '4 a 9'; ord = 4; end;
  else do; classe = '10 et plus'; ord = 5; end;
run;
proc sql;
  create table e1_doss as
  select classe, ord,
         sum(slot = 'M1') as dossiers_m1 label='Dossiers M-1',
         sum(slot = 'M0') as dossiers_m  label='Dossiers M'
  from e1_d2 group by classe, ord order by ord;
  %let dbl_m1 = 0; %let dbl_m0 = 0;
  select nb_cles_doublon into :dbl_m1 trimmed from e1_dbl_n where slot = 'M1';
  select nb_cles_doublon into :dbl_m0 trimmed from e1_dbl_n where slot = 'M0';
  select M1, M0 into :c_m1 trimmed, :c_m0 trimmed from e1_ind where indicateur = 'Contrats distincts';
  select M1, M0 into :cl_m1 trimmed, :cl_m0 trimmed from e1_ind where indicateur = 'Cles contrat incompletes';
quit;
data _null_;
  length k $1500;
  k = catx(' ', 'Constat :', strip(put(&c_m0., frn.)), 'contrats distincts a M contre', strip(put(&c_m1., frn.)), 'a M-1',
           '(', strip(put((&c_m0. - &c_m1.) / &c_m1., frp.)), ').',
           strip(put(&dbl_m0., frn.)), 'cles contrat en doublon a M (', strip(put(&dbl_m1., frn.)), 'a M-1) et',
           strip(put(&cl_m0., frn.)), 'cles incompletes a M.',
           'Decision : un doublon ou une cle incomplete doit etre explique avant tout calcul de taux en encours (regle N02).');
  call symputx('k_e1', k);
run;

title1 j=l "^S={background=cx0A2F3F color=white fontsize=15pt fontweight=bold} Exploration 1 : volumetrie et grain, M-1 contre M";
title3 j=l "^S={color=cxC00000 fontweight=bold} &banner.";
ods text = "^S={background=cxE1EFF5} &k_e1.";
title4 j=l "Volumetrie des deux arretes";
proc print data=e1_ind noobs label;
  label indicateur='Indicateur' M1='M-1' M0='M' variation='Variation' variation_pct='Variation %' lecture='Lecture';
run;
title4 j=l "Nombre de contrats par dossier (classes)";
proc print data=e1_doss noobs label; var classe dossiers_m1 dossiers_m; format dossiers_m1 dossiers_m comma12.; label classe='Contrats par dossier'; run;
title4 j=l "Population : contrats presents aux deux arretes, entrees et sorties";
proc sql;
  create table e1_mvt as
  select case when a.contrat_id is null then 'Entrees (absents a M-1)'
              when b.contrat_id is null then 'Sorties (absents a M)'
              else 'Communs aux deux arretes' end as mouvement length=30,
         count(*) as nb_contrats,
         sum(a.w) as encours_m1 format=comma20.0,
         sum(b.w) as encours_m  format=comma20.0
  from (select contrat_id, w from base where slot = 'M1' and contrat_id ne '') as a
       full join (select contrat_id, w from base where slot = 'M0' and contrat_id ne '') as b
       on a.contrat_id = b.contrat_id
  group by calculated mouvement;
quit;
proc print data=e1_mvt noobs label; label mouvement='Mouvement' nb_contrats='Contrats' encours_m1='Encours M-1' encours_m='Encours M'; format nb_contrats comma12.; run;
title4;
title1 j=l "G1a. Contrats en defaut par portefeuille, M-1 contre M";
proc sql;
  create table g1a as select portefeuille, role, count(*) as n from base group by portefeuille, role;
quit;
proc sgplot data=g1a dattrmap=attr_role;
  vbar portefeuille / response=n group=role groupdisplay=cluster attrid=role datalabel;
  yaxis grid label="Contrats"; xaxis display=(nolabel);
run;
title1;

/* S1.4 Valeurs manquantes : quelles variables sont vides, et a quel point ? -------
   Dimension "Exhaustivite". Une ligne par variable et par arrete, puis comparaison. */
data eda_manq(keep=arrete variable type nb_total nb_manquant taux_renseigne);
  set eda_raw;
  by arrete notsorted;
  array num_ {*} _numeric_;
  array chr_ {*} _character_;
  array nm_  {3000} _temporary_;                 /* compteurs de manquants numeriques */
  array cm_  {3000} _temporary_;                 /* compteurs de manquants caracteres */
  length variable $64 type $1;
  if first.arrete then do;
    do k = 1 to 3000; nm_{k} = 0; cm_{k} = 0; end;
    nb_total = 0;
  end;
  nb_total + 1;
  do k = 1 to dim(num_); if missing(num_{k}) then nm_{k} + 1; end;
  do k = 1 to dim(chr_); if missing(chr_{k}) then cm_{k} + 1; end;
  if last.arrete then do;
    do k = 1 to dim(num_);
      variable = vname(num_{k}); type = 'N'; nb_manquant = nm_{k};
      taux_renseigne = 1 - nb_manquant / nb_total; output;
    end;
    do k = 1 to dim(chr_);
      variable = vname(chr_{k}); type = 'C'; nb_manquant = cm_{k};
      taux_renseigne = 1 - nb_manquant / nb_total; output;
    end;
  end;
run;
proc sql;
  create table e2 as
  select coalesce(m.variable, p.variable) as variable length=40 label='Variable',
         coalesce(m.type, p.type) as type label='Type',
         u.usage as usage length=48 label='Usage dans le dispositif',
         p.nb_manquant as manq_m1 label='Manquants M-1', m.nb_manquant as manq_m label='Manquants M',
         p.taux_renseigne as taux_m1 label='Taux M-1' format=percent8.2, m.taux_renseigne as taux_m label='Taux M' format=percent8.2,
         (m.taux_renseigne - p.taux_renseigne) as ecart_pts label='Ecart M - M-1 (pts)' format=percentn8.2
  from (select * from eda_manq where arrete = &periode.) as m
       full join (select * from eda_manq where arrete = &periode1.) as p on m.variable = p.variable
       left join exp_var as u on upcase(coalesce(m.variable, p.variable)) = upcase(u.variable)
  where upcase(coalesce(m.variable, p.variable)) ne 'ARRETE'
  order by calculated taux_m;
  select count(*), sum(taux_m < 1), sum(ecart_pts < -0.005) into :e2_n trimmed, :e2_inc trimmed, :e2_deg trimmed from e2;
  %let e2_v1 = (aucune); %let e2_t1 = 1;
  select variable, taux_m into :e2_v1 trimmed, :e2_t1 trimmed from e2 where usage ne '' and taux_m is not null having taux_m = min(taux_m);
quit;
data _null_;
  length k $1500;
  k = catx(' ', 'Constat :', strip(put(&e2_n., frn.)), 'variables profilees ;', strip(put(&e2_inc., frn.)), 'ne sont pas renseignees a 100 % a M.',
           'La variable de dispositif la moins renseignee est', "&e2_v1.", '(', strip(put(&e2_t1., frp.)), ').',
           strip(put(&e2_deg., frn.)), 'variables perdent plus de 0,5 point de renseignement entre M-1 et M.',
           'Decision : faire confirmer par le Data Owner les vides attendus (par exemple SEG_DECL hors comptes a vue) avant de les exclure des regles.');
  call symputx('k_e2', k);
run;

title1 j=l "^S={background=cx0A2F3F color=white fontsize=15pt fontweight=bold} Exploration 2 : valeurs manquantes par variable, M-1 contre M";
title3 j=l "^S={color=cxC00000 fontweight=bold} &banner.";
ods text = "^S={background=cxE1EFF5} &k_e2.";
proc report data=e2 nowd;
  columns variable type usage manq_m1 manq_m taux_m1 taux_m ecart_pts;
  define variable / display; define type / display; define usage / display;
  define manq_m1 / display format=comma12.; define manq_m / display format=comma12.;
  define taux_m1 / display; define taux_m / display; define ecart_pts / display;
  compute taux_m;
    if taux_m < &o_seuil. then call define(_col_, 'style', 'style=[background=cxF4CCCC]');
    else if taux_m < &v_seuil. then call define(_col_, 'style', 'style=[background=cxFCE5CD]');
    else call define(_col_, 'style', 'style=[background=cxD9EAD3]');
  endcomp;
run;
/* G2. Taux de renseignement des 25 variables les moins renseignees, M-1 et M */
proc sql;
  create table g2_top as select variable from e2 where taux_m is not null and usage ne '' order by taux_m;
quit;
data g2_top; set g2_top(obs=25); run;
proc sql;
  create table g2 as
  select m.variable, m.taux_renseigne,
         case when m.arrete = &periode. then 'M' else 'M-1' end as role length=4
  from eda_manq as m inner join g2_top as t on m.variable = t.variable;
quit;
title1 j=l "G2. Taux de renseignement des variables du dispositif les moins renseignees";
proc sgplot data=g2 dattrmap=attr_role;
  hbar variable / response=taux_renseigne group=role groupdisplay=cluster attrid=role categoryorder=respasc;
  refline &v_seuil. / axis=x lineattrs=(color=cx1BAF7A pattern=dash) label="95 %";
  refline &o_seuil. / axis=x lineattrs=(color=cxEDA100 pattern=dash) label="90 %";
  xaxis min=0 max=1 grid label="Taux de renseignement" valuesformat=percent8.; yaxis display=(nolabel) fitpolicy=none;
run;
title1;

/* S1.5 Distributions numeriques : valeurs extremes, negatifs, zeros --------------
   Dimension "Validite" (encours negatifs ? durees aberrantes ?).                    */
ods exclude all;
ods output summary = e3;
proc means data=base n nmiss min p1 p5 median p95 p99 max mean std sum stackodsoutput;
  class slot;
  var M_BIL_ENCOURS_PERIODE M_CNTRT_DECLS_BILAN M_CNTRT_DECLS_HBILAN expo_dwr M_ENCOURS_DECLSMT M_PROVS_SOLDE
      TIME_IN_DEF1 TIME_IN_DEF2 duree_recalc_mois;
run;
ods exclude none;
proc sql;
  create table e3_neg as
  select slot,
         sum(M_BIL_ENCOURS_PERIODE < 0) as encours_neg label='Encours bilan < 0',
         sum(M_CNTRT_DECLS_BILAN < 0)   as declb_neg   label='Declassement bilan < 0',
         sum(M_CNTRT_DECLS_HBILAN < 0)  as declhb_neg  label='Declassement HB < 0',
         sum(expo_dwr < 0)              as expo_dwr_neg label='Exposition DWR < 0',
         sum(TIME_IN_DEF2 < 0)          as duree_neg   label='Duree < 0',
         sum(TIME_IN_DEF2 > &duree_max.) as duree_sup_max label='Duree > borne haute'
  from base group by slot;
quit;

title1 j=l "^S={background=cx0A2F3F color=white fontsize=15pt fontweight=bold} Exploration 3 : distributions des montants et des durees";
title3 j=l "^S={color=cxC00000 fontweight=bold} &banner.";
ods text = "^S={background=cxE1EFF5} Lecture : pour chaque variable, la ligne M-1 est suivie de la ligne M (colonne slot : M1 puis M0). Comparer les percentiles P1 et P99, les minimums negatifs et les sommes : un decalage des extremes entre arretes signale une rupture de source plutot qu un mouvement de portefeuille.";
proc print data=e3 noobs label; run;
title4 j=l "Valeurs negatives et au-dela de la borne haute, par arrete";
proc print data=e3_neg noobs label; format _numeric_ comma12.; run;
title4;
data g_encours; set base; if M_BIL_ENCOURS_PERIODE > 0 then log10_encours = log10(M_BIL_ENCOURS_PERIODE); run;
title1 j=l "G3a. Distribution de l encours bilan (log10), M-1 contre M";
proc sgplot data=g_encours dattrmap=attr_role;
  histogram log10_encours / group=role attrid=role transparency=0.45 nbins=40;
  xaxis label="log10(M_BIL_ENCOURS_PERIODE)"; yaxis grid;
run;
title1 j=l "G3b. Encours par type de contrat (log10), M-1 contre M";
proc sgplot data=g_encours dattrmap=attr_role;
  vbox log10_encours / category=C_CNTRT_TYPE group=role attrid=role;
  yaxis label="log10(encours)" grid; xaxis display=(nolabel);
run;
title1;

/* S1.6 Variables categorielles : quelles modalites existent vraiment ? ----------
   Sert a FIXER les domaines autorises (R02, R37, R39, R44, R48, R56).               */
data mod_long(keep=slot variable modalite w);
  set base;
  length variable $32 modalite $60;
  variable = 'B_TIERS_RETAIL';   modalite = vvalue(B_TIERS_RETAIL);   output;
  variable = 'C_CNTRT_TYPE';     modalite = vvalue(C_CNTRT_TYPE);     output;
  variable = 'C_CNTRT_NIV_RSK';  modalite = vvalue(C_CNTRT_NIV_RSK);  output;
  variable = 'EDX_CER';          modalite = vvalue(EDX_CER);          output;
  variable = 'B_CNTRT_WRITE_OFF'; modalite = vvalue(B_CNTRT_WRITE_OFF); output;
  variable = 'FLAG_WO';          modalite = vvalue(FLAG_WO);          output;
  variable = 'C_CNTRT_MODLE';    modalite = vvalue(C_CNTRT_MODLE);    output;
  variable = 'C_CNTRT_MODLE_HB'; modalite = vvalue(C_CNTRT_MODLE_HB); output;
  variable = 'final_model';      modalite = vvalue(final_model);      output;
  variable = 'C_NOMNC_ENGGM';    modalite = vvalue(C_NOMNC_ENGGM);    output;
  variable = 'C_GAR_ORIG';       modalite = vvalue(C_GAR_ORIG);       output;
  variable = 'I_GAR';            modalite = vvalue(I_GAR);            output;
  variable = 'Flag_GARAN';       modalite = vvalue(Flag_GARAN);       output;
  variable = 'Flag_Crelog';      modalite = vvalue(Flag_Crelog);      output;
  variable = 'SEG_DECL';         modalite = vvalue(SEG_DECL);         output;
  variable = 'CAV_Type';         modalite = vvalue(CAV_Type);         output;
  variable = 'STAT_PERIM';       modalite = vvalue(STAT_PERIM);       output;
run;
proc sql;
  create table e4_a as
  select variable, case when strip(modalite) = '' then '(manquant)' else strip(modalite) end as modalite length=60,
         sum(slot = 'M1') as n_m1, sum(slot = 'M0') as n_m,
         sum(case when slot = 'M1' then w else 0 end) as enc_m1,
         sum(case when slot = 'M0' then w else 0 end) as enc_m
  from mod_long group by variable, calculated modalite;
  create table e4_tot as
  select variable, sum(n_m1) as t_m1, sum(n_m) as t_m from e4_a group by variable;
  create table e4_b as
  select a.variable, a.modalite, a.n_m1, a.n_m, a.enc_m1, a.enc_m,
         a.n_m1 / t.t_m1 as pct_m1 format=percent8.2, a.n_m / t.t_m as pct_m format=percent8.2,
         (a.n_m / t.t_m) - (a.n_m1 / t.t_m1) as ecart_pts format=percentn8.2
  from e4_a as a inner join e4_tot as t on a.variable = t.variable
  order by a.variable, a.n_m desc;
quit;
data e4;                                    /* 30 modalites au plus par variable */
  set e4_b;
  by variable;
  if first.variable then rang = 0;
  rang + 1;
  if rang <= 30;
run;
/* modalites apparues ou disparues entre M-1 et M */
proc sql noprint;
  select count(*) into :e4_new trimmed from e4_b where n_m1 = 0 and n_m > 0;
  select count(*) into :e4_gone trimmed from e4_b where n_m1 > 0 and n_m = 0;
quit;
data _null_;
  length k $1200;
  k = catx(' ', 'Constat :', strip(put(&e4_new., frn.)), 'modalite(s) apparaissent a M et', strip(put(&e4_gone., frn.)), 'disparaissent par rapport a M-1.',
           'Une modalite nouvelle sur un code de reference signale un changement de source ou de codification.',
           'Decision : arreter les domaines de valeurs (regles R02, R37, R39, R44, R48, R56) avec les Data Owners.');
  call symputx('k_e4', k);
run;

title1 j=l "^S={background=cx0A2F3F color=white fontsize=15pt fontweight=bold} Exploration 4 : modalites des variables categorielles";
title3 j=l "^S={color=cxC00000 fontweight=bold} &banner.";
ods text = "^S={background=cxE1EFF5} &k_e4.";
proc print data=e4 noobs label;
  var variable modalite n_m1 n_m pct_m1 pct_m ecart_pts enc_m1 enc_m;
  label variable='Variable' modalite='Modalite' n_m1='Contrats M-1' n_m='Contrats M' pct_m1='% M-1' pct_m='% M' ecart_pts='Ecart (pts)' enc_m1='Encours M-1' enc_m='Encours M';
  format n_m1 n_m enc_m1 enc_m comma16.0;
run;
title4 j=l "Liste complete des variables dont une modalite est apparue ou a disparu";
proc print data=e4_b noobs label; where (n_m1 = 0 and n_m > 0) or (n_m1 > 0 and n_m = 0); var variable modalite n_m1 n_m; run;
title4;

/* S1.7 Dates : plages, sentinelle 1901-01-01, dates futures ----------------------
   Dimensions "Validite" et "Coherence".                                             */
proc sql;
  create table e5 as
  select slot, 'Default_date' as variable length=24, min(Default_date) as min_d format=date9., max(Default_date) as max_d format=date9.,
         sum(Default_date = '01JAN1901'd) as nb_1901, sum(Default_date > D_ARRETE) as nb_apres_arrete, sum(missing(Default_date)) as nb_manquants, count(*) as nb_lignes
    from base group by slot
  union all
  select slot, 'D_DEFAU', min(D_DEFAU), max(D_DEFAU), sum(D_DEFAU = '01JAN1901'd), sum(D_DEFAU > D_ARRETE), sum(missing(D_DEFAU)), count(*) from base group by slot
  union all
  select slot, 'D_DOSSR_CREAT', min(D_DOSSR_CREAT), max(D_DOSSR_CREAT), sum(D_DOSSR_CREAT = '01JAN1901'd), sum(D_DOSSR_CREAT > D_ARRETE), sum(missing(D_DOSSR_CREAT)), count(*) from base group by slot
  union all
  select slot, 'D_1ER_EVT_DEFAU', min(D_1ER_EVT_DEFAU), max(D_1ER_EVT_DEFAU), sum(D_1ER_EVT_DEFAU = '01JAN1901'd), sum(D_1ER_EVT_DEFAU > D_ARRETE), sum(missing(D_1ER_EVT_DEFAU)), count(*) from base group by slot
  union all
  select slot, 'D_PHOTO', min(D_PHOTO), max(D_PHOTO), sum(D_PHOTO = '01JAN1901'd), sum(D_PHOTO > D_ARRETE), sum(missing(D_PHOTO)), count(*) from base group by slot
  union all
  select slot, 'D_ARRETE', min(D_ARRETE), max(D_ARRETE), 0, 0, sum(missing(D_ARRETE)), count(*) from base group by slot
  order by 2, 1;
  create table e5_arr as                     /* une seule date d arrete par table ? */
  select slot, N_PERD, D_ARRETE, D_PHOTO, count(*) as nb_lignes from base group by slot, N_PERD, D_ARRETE, D_PHOTO;
  select sum(nb_1901), sum(nb_apres_arrete) into :e5_1901 trimmed, :e5_fut trimmed from e5 where slot = 'M0';
  select count(*) into :e5_ndates trimmed from e5_arr where slot = 'M0';
quit;
data _null_;
  length k $1200;
  k = catx(' ', 'Constat : a M,', strip(put(&e5_1901., frn.)), 'valeurs sentinelles 1901-01-01 et', strip(put(&e5_fut., frn.)),
           'dates posterieures a l arrete sur les variables de date suivies ;', strip(put(&e5_ndates., frn.)),
           'combinaison(s) N_PERD / D_ARRETE / D_PHOTO dans la table de M (attendu : 1).',
           'Decision : la sentinelle 1901 doit etre traitee comme un manquant dans les calculs de duree (regles R30, R31, R34).');
  call symputx('k_e5', k);
run;

title1 j=l "^S={background=cx0A2F3F color=white fontsize=15pt fontweight=bold} Exploration 5 : plages de dates, sentinelle 1901, dates futures";
title3 j=l "^S={color=cxC00000 fontweight=bold} &banner.";
ods text = "^S={background=cxE1EFF5} &k_e5.";
proc print data=e5 noobs label;
  label slot='Arrete (M1=M-1, M0=M)' variable='Variable' min_d='Min' max_d='Max' nb_1901='Sentinelle 1901' nb_apres_arrete='Apres arrete' nb_manquants='Manquants' nb_lignes='Lignes';
  format nb_1901 nb_apres_arrete nb_manquants nb_lignes comma12.;
run;
title4 j=l "Dates d arrete et de photo presentes dans chaque table";
proc print data=e5_arr noobs; format D_ARRETE D_PHOTO date9. nb_lignes comma12.; run;
title4;
data g_dates;
  set base;
  length cat_date $16;
  if missing(Default_date) then cat_date = "Manquant";
  else if Default_date = '01JAN1901'd then cat_date = "Sentinelle 1901";
  else cat_date = "Date valide";
  if cat_date = "Date valide" then annee = year(Default_date);
run;
title1 j=l "G4a. Qualite de Default_date (contrats), M-1 contre M";
proc sgplot data=g_dates dattrmap=attr_role;
  vbar cat_date / group=role groupdisplay=cluster attrid=role stat=freq datalabel;
  yaxis grid label="Contrats"; xaxis display=(nolabel);
run;
title1 j=l "G4b. Contrats par annee de defaut (dates valides), M-1 contre M";
proc sgplot data=g_dates(where=(cat_date = "Date valide")) dattrmap=attr_role;
  vbar annee / group=role groupdisplay=cluster attrid=role stat=freq;
  xaxis label="Annee de defaut"; yaxis grid label="Contrats";
run;
title1;

/* S1.8 Ecarts entre sources redondantes (dimension "Coherence") ------------------
   Ces ecarts montrent quelles regles de coherence ont du sens (N09, R34, R53).      */
data eda_ecarts;
  set base;
  ecart_enc_declb = M_BIL_ENCOURS_PERIODE - M_CNTRT_DECLS_BILAN;      /* encours vs declassement      */
  ecart_enc_dwr   = M_BIL_ENCOURS_PERIODE - expo_dwr;                 /* BCR vs DWR                   */
  ecart_declb_e   = M_CNTRT_DECLS_BILAN - M_ENCOURS_DECLSMT;          /* source f vs source e         */
  ecart_duree     = TIME_IN_DEF1 * 12 - TIME_IN_DEF2;                 /* annees entamees vs mois      */
  ecart_duree_recalc = TIME_IN_DEF2 - duree_recalc_mois;              /* duree moteur vs recalculee   */
  sig_bcr_dwr = (not missing(expo_dwr) and not missing(M_BIL_ENCOURS_PERIODE) and abs(ecart_enc_dwr) > &tol_decl. * max(abs(expo_dwr), 1));
  sig_f_e     = (not missing(M_ENCOURS_DECLSMT) and not missing(M_CNTRT_DECLS_BILAN) and abs(ecart_declb_e) > &tol_decl. * max(abs(M_CNTRT_DECLS_BILAN), 1));
  sig_duree   = (not missing(ecart_duree_recalc) and abs(ecart_duree_recalc) > &tol_duree.);
run;
ods exclude all;
ods output summary = e6;
proc means data=eda_ecarts n nmiss min p1 median p99 max mean stackodsoutput;
  class slot;
  var ecart_enc_declb ecart_enc_dwr ecart_declb_e ecart_duree ecart_duree_recalc;
run;
ods exclude none;
proc sql;
  create table e6_sig as
  select slot, count(*) as nb_lignes,
         sum(sig_bcr_dwr) as ecart_bcr_dwr label='Ecart BCR / DWR > tolerance',
         sum(sig_f_e)     as ecart_f_e     label='Ecart source f / e > tolerance',
         sum(sig_duree)   as ecart_duree_sup label='Ecart de duree > tolerance'
  from eda_ecarts group by slot;
  select ecart_bcr_dwr, ecart_f_e, ecart_duree_sup into :s_bcr trimmed, :s_fe trimmed, :s_dur trimmed from e6_sig where slot = 'M0';
  select ecart_bcr_dwr, ecart_f_e, ecart_duree_sup into :s_bcr1 trimmed, :s_fe1 trimmed, :s_dur1 trimmed from e6_sig where slot = 'M1';
quit;
data _null_;
  length k $1200;
  k = catx(' ', 'Constat : a M,', strip(put(&s_bcr., frn.)), 'contrats ont un ecart BCR / DWR superieur a 1 %', '(', strip(put(&s_bcr1., frn.)), 'a M-1),',
           strip(put(&s_fe., frn.)), 'un ecart entre les sources f et e du declassement', '(', strip(put(&s_fe1., frn.)), 'a M-1) et',
           strip(put(&s_dur., frn.)), 'un ecart de duree superieur a la tolerance', '(', strip(put(&s_dur1., frn.)), 'a M-1).',
           'Decision : ces volumes fixent la portee des regles R53, N09 et R34 ; les tolerances sont a valider avec les Data Owners.');
  call symputx('k_e6', k);
run;

title1 j=l "^S={background=cx0A2F3F color=white fontsize=15pt fontweight=bold} Exploration 6 : ecarts entre sources redondantes";
title3 j=l "^S={color=cxC00000 fontweight=bold} &banner.";
ods text = "^S={background=cxE1EFF5} &k_e6.";
proc print data=e6 noobs label; run;
title4 j=l "Contrats avec ecart significatif, par arrete";
proc print data=e6_sig noobs label; format nb_lignes ecart_bcr_dwr ecart_f_e ecart_duree_sup comma12.; run;
title4;
/* G5. Declassement bilan : source f contre source e (points hors diagonale = ecarts), echantillon de 20 000 points */
data g5;
  set eda_ecarts(where=(not missing(M_ENCOURS_DECLSMT) and not missing(M_CNTRT_DECLS_BILAN)));
  if sig_f_e = 1 or ranuni(20260930) < 0.02;
run;
data g5; set g5(obs=20000); run;
title1 j=l "G5. Declassement bilan : source f contre source e (hors diagonale = ecart)";
proc sgpanel data=g5;
  panelby role / columns=2 novarname;
  scatter x=M_ENCOURS_DECLSMT y=M_CNTRT_DECLS_BILAN / markerattrs=(symbol=circlefilled size=4 color=cx0E5A78) transparency=0.6;
  lineparm x=0 y=0 slope=1 / lineattrs=(color=red pattern=dash);
  colaxis label="M_ENCOURS_DECLSMT" grid; rowaxis label="M_CNTRT_DECLS_BILAN" grid;
run;
title1;

/* S1.9 Croisements : comprendre les liens entre variables ------------------------ */
proc sql;
  create table e7_a as                      /* type de contrat x modele retenu */
  select C_CNTRT_TYPE, portefeuille, sum(slot = 'M1') as n_m1, sum(slot = 'M0') as n_m,
         sum(case when slot = 'M0' then w else 0 end) as enc_m
  from base group by C_CNTRT_TYPE, portefeuille order by C_CNTRT_TYPE, n_m desc;
  create table e7_a2 as                     /* un type de contrat qui mene a plusieurs modeles = routage instable */
  select C_CNTRT_TYPE, count(distinct portefeuille) as nb_modeles, sum(n_m) as n_m
  from e7_a where n_m > 0 group by C_CNTRT_TYPE having count(distinct portefeuille) > 1;
  create table e7_b as                      /* niveau de risque x EDX_CER derive */
  select vvalue(C_CNTRT_NIV_RSK) as niv_rsk length=8, vvalue(EDX_CER) as edx_cer length=8,
         sum(slot = 'M1') as n_m1, sum(slot = 'M0') as n_m
  from base group by calculated niv_rsk, calculated edx_cer;
  create table e7_c as                      /* passage en perte x encours */
  select slot, B_CNTRT_WRITE_OFF, count(*) as nb, sum(M_BIL_ENCOURS_PERIODE) as encours format=comma18.0
  from base group by slot, B_CNTRT_WRITE_OFF;
  create table e7_d as                      /* contrats Loan IQ (ALT) : modele et encours */
  select slot, portefeuille, count(*) as nb, sum(M_BIL_ENCOURS_PERIODE) as encours format=comma18.0
  from base where C_CNTRT_TYPE in (&lst_loaniq.) group by slot, portefeuille;
  create table e7_dos as                    /* encours par dossier */
  select slot, I_DOSS_UNIQ, count(*) as nb_contrats, sum(w) as encours
  from base where not missing(I_DOSS_UNIQ) group by slot, I_DOSS_UNIQ;
quit;
proc sort data=e7_dos; by slot descending encours; run;
data e7_top;                                 /* 15 plus gros dossiers par arrete */
  set e7_dos; by slot;
  if first.slot then rang = 0; rang + 1;
  if rang <= 15;
  format encours comma18.0;
run;
proc sql;
  create table e7_dos2 as
  select d.slot, d.encours, t.tot
  from e7_dos as d inner join (select slot, sum(encours) as tot from e7_dos group by slot) as t on d.slot = t.slot
  order by d.slot, d.encours desc;
quit;
data g_pareto2;                              /* courbe de Pareto de l encours par dossier */
  set e7_dos2; by slot;
  length role $4;
  if first.slot then do; cum = 0; rang = 0; end;
  cum + encours; rang + 1;
  cum_pct = cum / tot;
  role = ifc(slot = 'M1', 'M-1', 'M');
  if mod(rang, 50) = 1 or last.slot;              /* un point sur 50 suffit au trace */
run;
proc sql noprint;
  select count(*), sum(nb_modeles) into :e7_instab trimmed, :e7_dummy trimmed from e7_a2;
quit;
data _null_;
  length k $1200;
  k = catx(' ', 'Constat :', strip(put(&e7_instab., frn.)), 'type(s) de contrat menent a plusieurs modeles retenus a M (routage non deterministe, voir liste ci-dessous).',
           'Les 15 plus gros dossiers et la courbe de Pareto montrent la concentration de l encours : un taux en encours est tire par ces dossiers.',
           'Decision : prevoir un seuil de materialite en euros pour prioriser les anomalies.');
  call symputx('k_e7', k);
run;

title1 j=l "^S={background=cx0A2F3F color=white fontsize=15pt fontweight=bold} Exploration 7 : croisements et concentration";
title3 j=l "^S={color=cxC00000 fontweight=bold} &banner.";
ods text = "^S={background=cxE1EFF5} &k_e7.";
title4 j=l "Types de contrat menant a plusieurs modeles retenus (a M)";
proc print data=e7_a2 noobs; format n_m comma12.; run;
title4 j=l "Type de contrat x modele retenu";
proc print data=e7_a noobs label; label C_CNTRT_TYPE='Type' portefeuille='Modele retenu' n_m1='Contrats M-1' n_m='Contrats M' enc_m='Encours M'; format n_m1 n_m enc_m comma16.0; run;
title4 j=l "Niveau de risque (1=EDX, 2=CER) x EDX_CER derive";
proc print data=e7_b noobs; run;
title4 j=l "Passage en perte x encours";
proc print data=e7_c noobs; format nb comma12.; run;
title4 j=l "Contrats Loan IQ (ALT) : modele et encours";
proc print data=e7_d noobs; format nb comma12.; run;
title4 j=l "15 plus gros dossiers par encours (par arrete)";
proc print data=e7_top noobs; var slot rang I_DOSS_UNIQ nb_contrats encours; format nb_contrats comma12.; run;
title4;
title1 j=l "G6. Concentration de l encours par dossier (Pareto), M-1 contre M";
proc sgplot data=g_pareto2 dattrmap=attr_role;
  series x=rang y=cum_pct / group=role attrid=role lineattrs=(thickness=2);
  refline 0.8 / axis=y lineattrs=(pattern=dash color=gray) label="80 % de l encours";
  xaxis label="Rang du dossier (1 point sur 50, tries par encours decroissant)"; yaxis label="Part cumulee de l encours" valuesformat=percent8. grid;
run;
title1 j=l "G7. Duree en defaut (TIME_IN_DEF2) par niveau de risque (1=EDX, 2=CER), M-1 contre M";
proc sgplot data=base dattrmap=attr_role;
  vbox TIME_IN_DEF2 / category=NIV_RSK_C group=role attrid=role;
  yaxis label="TIME_IN_DEF2 (mois) - unite a confirmer" grid; xaxis display=(nolabel);
run;
title1;

/* S1.10 Synthese des constats de l'exploration */
data e8;
  length onglet $24 constat $1500;
  onglet = 'EDA_1_Volumetrie_grain'; constat = "&k_e1."; output;
  onglet = 'EDA_2_Manquants';        constat = "&k_e2."; output;
  onglet = 'EDA_4_Modalites';        constat = "&k_e4."; output;
  onglet = 'EDA_5_Dates';            constat = "&k_e5."; output;
  onglet = 'EDA_6_Ecarts_sources';   constat = "&k_e6."; output;
  onglet = 'EDA_7_Croisements';      constat = "&k_e7."; output;
run;

title1 j=l "^S={background=cx0A2F3F color=white fontsize=15pt fontweight=bold} Exploration 8 : constats et decisions a prendre avec les Data Owners";
title3 j=l "^S={color=cxC00000 fontweight=bold} &banner.";
ods text = "^S={background=cxE1EFF5} Synthese des constats de l exploration, a partager avec les Data Owners avant de figer les domaines de valeurs, les bornes et les tolerances.";
proc print data=e8 noobs label; label onglet='Onglet' constat='Constat'; run;
title1;

/*===========================================================================
  S2. DIAGNOSTIC DQ DES 15 CDE - etape 3 du CDC (regles, anomalies, scores)
      Principe : seules les dimensions utiles a chaque type de donnee sont
      evaluees (identifiant : exhaustivite, unicite, coherence des composants ;
      montant : exhaustivite, validite, exactitude contre une seconde source ;
      date / duree : exhaustivite, validite, exactitude par recalcul ;
      code / indicateur : validite du domaine, coherence si dependance).
      Rapprochement indicatif : BCBS 239 (principes 3 exactitude et integrite,
      4 exhaustivite, 5 actualite) et exigences de qualite des donnees de
      l EBA pour l estimation du risque de credit.
      Chaque regle est evaluee sur M-1 ET sur M, par portefeuille.
      Taux : en nombre (n_ok / n_pop) et en encours (a_ok / a_pop), poids =
      M_BIL_ENCOURS_PERIODE.
===========================================================================*/
proc sql;
  create table res
    (arrete num, portefeuille char(8), cde_no num, cde char(40), variable char(60), regle_id char(6),
     dimension char(20), severite char(10), description char(100),
     n_pop num, n_ok num, a_pop num, a_ok num);
  create table anom                                  /* toutes les anomalies (contrat x regle) */
    (arrete num, regle_id char(6), portefeuille char(8), cle_ano char(72), doss char(40), encours num);
quit;

/*============================================================================
  CDE 1 : Perimetre Retail / Corporate
============================================================================*/
/* R01 | CDE 1 Perimetre Retail / Corporate | variable : B_TIERS_RETAIL
   DIMENSION : EXHAUSTIVITE | severite : Bloquant
   Regle     : B_TIERS_RETAIL renseigne
   Population: 1=1  ->  conforme si : not missing(B_TIERS_RETAIL) */
proc sql;
  insert into res
  select arrete, portefeuille, 1, "Perimetre Retail / Corporate", "B_TIERS_RETAIL", "R01", "Exhaustivite", "Bloquant", "B_TIERS_RETAIL renseigne",
         count(*),
         sum(case when (not missing(B_TIERS_RETAIL)) then 1 else 0 end),
         sum(w),
         sum(case when (not missing(B_TIERS_RETAIL)) then w else 0 end)
  from base
  where (1=1)
  group by arrete, portefeuille;
  insert into anom
  select arrete, "R01", portefeuille, cle_ano, strip(I_DOSS_UNIQ), M_BIL_ENCOURS_PERIODE
  from base
  where (1=1) and not (not missing(B_TIERS_RETAIL));
quit;
/* R02 | CDE 1 Perimetre Retail / Corporate | variable : B_TIERS_RETAIL
   DIMENSION : VALIDITE | severite : Bloquant
   Regle     : Valeur dans le domaine {R, C}
   Population: not missing(B_TIERS_RETAIL)  ->  conforme si : B_TIERS_RETAIL in (&dom_tiers.) */
proc sql;
  insert into res
  select arrete, portefeuille, 1, "Perimetre Retail / Corporate", "B_TIERS_RETAIL", "R02", "Validite", "Bloquant", "Valeur dans le domaine {R, C}",
         count(*),
         sum(case when (B_TIERS_RETAIL in (&dom_tiers.)) then 1 else 0 end),
         sum(w),
         sum(case when (B_TIERS_RETAIL in (&dom_tiers.)) then w else 0 end)
  from base
  where (not missing(B_TIERS_RETAIL))
  group by arrete, portefeuille;
  insert into anom
  select arrete, "R02", portefeuille, cle_ano, strip(I_DOSS_UNIQ), M_BIL_ENCOURS_PERIODE
  from base
  where (not missing(B_TIERS_RETAIL)) and not (B_TIERS_RETAIL in (&dom_tiers.));
quit;

/*============================================================================
  CDE 2 : Encours au declassement bilan
============================================================================*/
/* R05 | CDE 2 Encours au declassement bilan | variable : M_CNTRT_DECLS_BILAN
   DIMENSION : EXHAUSTIVITE | severite : Bloquant
   Regle     : Encours declassement bilan renseigne
   Population: 1=1  ->  conforme si : not missing(M_CNTRT_DECLS_BILAN) */
proc sql;
  insert into res
  select arrete, portefeuille, 2, "Encours au declassement bilan", "M_CNTRT_DECLS_BILAN", "R05", "Exhaustivite", "Bloquant", "Encours declassement bilan renseigne",
         count(*),
         sum(case when (not missing(M_CNTRT_DECLS_BILAN)) then 1 else 0 end),
         sum(w),
         sum(case when (not missing(M_CNTRT_DECLS_BILAN)) then w else 0 end)
  from base
  where (1=1)
  group by arrete, portefeuille;
  insert into anom
  select arrete, "R05", portefeuille, cle_ano, strip(I_DOSS_UNIQ), M_BIL_ENCOURS_PERIODE
  from base
  where (1=1) and not (not missing(M_CNTRT_DECLS_BILAN));
quit;
/* R06 | CDE 2 Encours au declassement bilan | variable : M_CNTRT_DECLS_BILAN
   DIMENSION : VALIDITE | severite : Bloquant
   Regle     : Numerique et >= 0
   Population: not missing(M_CNTRT_DECLS_BILAN)  ->  conforme si : M_CNTRT_DECLS_BILAN >= 0 */
proc sql;
  insert into res
  select arrete, portefeuille, 2, "Encours au declassement bilan", "M_CNTRT_DECLS_BILAN", "R06", "Validite", "Bloquant", "Numerique et >= 0",
         count(*),
         sum(case when (M_CNTRT_DECLS_BILAN >= 0) then 1 else 0 end),
         sum(w),
         sum(case when (M_CNTRT_DECLS_BILAN >= 0) then w else 0 end)
  from base
  where (not missing(M_CNTRT_DECLS_BILAN))
  group by arrete, portefeuille;
  insert into anom
  select arrete, "R06", portefeuille, cle_ano, strip(I_DOSS_UNIQ), M_BIL_ENCOURS_PERIODE
  from base
  where (not missing(M_CNTRT_DECLS_BILAN)) and not (M_CNTRT_DECLS_BILAN >= 0);
quit;
/* N09 | CDE 2 Encours au declassement bilan | variable : M_CNTRT_DECLS_BILAN / M_ENCOURS_DECLSMT
   DIMENSION : COHERENCE | severite : Majeur
   Regle     : Declassement source f = source e (tol. 1 pct)
   Population: not missing(M_ENCOURS_DECLSMT) and not missing(M_CNTRT_DECLS_BILAN)  ->  conforme si : abs(M_CNTRT_DECLS_BILAN - M_ENCOURS_DECLSMT) <= &tol_decl.*max(abs(M_CNTRT_DECLS_BILAN),1) */
proc sql;
  insert into res
  select arrete, portefeuille, 2, "Encours au declassement bilan", "M_CNTRT_DECLS_BILAN / M_ENCOURS_DECLSMT", "N09", "Coherence", "Majeur", "Declassement source f = source e (tol. 1 pct)",
         count(*),
         sum(case when (abs(M_CNTRT_DECLS_BILAN - M_ENCOURS_DECLSMT) <= &tol_decl.*max(abs(M_CNTRT_DECLS_BILAN),1)) then 1 else 0 end),
         sum(w),
         sum(case when (abs(M_CNTRT_DECLS_BILAN - M_ENCOURS_DECLSMT) <= &tol_decl.*max(abs(M_CNTRT_DECLS_BILAN),1)) then w else 0 end)
  from base
  where (not missing(M_ENCOURS_DECLSMT) and not missing(M_CNTRT_DECLS_BILAN))
  group by arrete, portefeuille;
  insert into anom
  select arrete, "N09", portefeuille, cle_ano, strip(I_DOSS_UNIQ), M_BIL_ENCOURS_PERIODE
  from base
  where (not missing(M_ENCOURS_DECLSMT) and not missing(M_CNTRT_DECLS_BILAN)) and not (abs(M_CNTRT_DECLS_BILAN - M_ENCOURS_DECLSMT) <= &tol_decl.*max(abs(M_CNTRT_DECLS_BILAN),1));
quit;

/*============================================================================
  CDE 3 : Encours au declassement hors bilan
============================================================================*/
/* R10 | CDE 3 Encours au declassement hors bilan | variable : M_CNTRT_DECLS_HBILAN
   DIMENSION : EXHAUSTIVITE | severite : Majeur
   Regle     : Renseigne (0 accepte)
   Population: 1=1  ->  conforme si : not missing(M_CNTRT_DECLS_HBILAN) */
proc sql;
  insert into res
  select arrete, portefeuille, 3, "Encours au declassement hors bilan", "M_CNTRT_DECLS_HBILAN", "R10", "Exhaustivite", "Majeur", "Renseigne (0 accepte)",
         count(*),
         sum(case when (not missing(M_CNTRT_DECLS_HBILAN)) then 1 else 0 end),
         sum(w),
         sum(case when (not missing(M_CNTRT_DECLS_HBILAN)) then w else 0 end)
  from base
  where (1=1)
  group by arrete, portefeuille;
  insert into anom
  select arrete, "R10", portefeuille, cle_ano, strip(I_DOSS_UNIQ), M_BIL_ENCOURS_PERIODE
  from base
  where (1=1) and not (not missing(M_CNTRT_DECLS_HBILAN));
quit;
/* R11 | CDE 3 Encours au declassement hors bilan | variable : M_CNTRT_DECLS_HBILAN
   DIMENSION : VALIDITE | severite : Bloquant
   Regle     : >= 0
   Population: not missing(M_CNTRT_DECLS_HBILAN)  ->  conforme si : M_CNTRT_DECLS_HBILAN >= 0 */
proc sql;
  insert into res
  select arrete, portefeuille, 3, "Encours au declassement hors bilan", "M_CNTRT_DECLS_HBILAN", "R11", "Validite", "Bloquant", ">= 0",
         count(*),
         sum(case when (M_CNTRT_DECLS_HBILAN >= 0) then 1 else 0 end),
         sum(w),
         sum(case when (M_CNTRT_DECLS_HBILAN >= 0) then w else 0 end)
  from base
  where (not missing(M_CNTRT_DECLS_HBILAN))
  group by arrete, portefeuille;
  insert into anom
  select arrete, "R11", portefeuille, cle_ano, strip(I_DOSS_UNIQ), M_BIL_ENCOURS_PERIODE
  from base
  where (not missing(M_CNTRT_DECLS_HBILAN)) and not (M_CNTRT_DECLS_HBILAN >= 0);
quit;

/*============================================================================
  CDE 4 : Identification personne
============================================================================*/
/* R14 | CDE 4 Identification personne | variable : ID_RMPM
   DIMENSION : EXHAUSTIVITE | severite : Bloquant
   Regle     : Identifiant personne renseigne
   Population: 1=1  ->  conforme si : not missing(ID_RMPM) */
proc sql;
  insert into res
  select arrete, portefeuille, 4, "Identification personne", "ID_RMPM", "R14", "Exhaustivite", "Bloquant", "Identifiant personne renseigne",
         count(*),
         sum(case when (not missing(ID_RMPM)) then 1 else 0 end),
         sum(w),
         sum(case when (not missing(ID_RMPM)) then w else 0 end)
  from base
  where (1=1)
  group by arrete, portefeuille;
  insert into anom
  select arrete, "R14", portefeuille, cle_ano, strip(I_DOSS_UNIQ), M_BIL_ENCOURS_PERIODE
  from base
  where (1=1) and not (not missing(ID_RMPM));
quit;

/*============================================================================
  CDE 5 : Identification dossier BCR
============================================================================*/
/* R18 | CDE 5 Identification dossier BCR | variable : I_DOSSR_BCR_SIEGE, D_DOSSR_BCR, N_DOSSR_BCR
   DIMENSION : EXHAUSTIVITE | severite : Bloquant
   Regle     : Les 3 composants de la cle dossier renseignes
   Population: 1=1  ->  conforme si : not missing(I_DOSSR_BCR_SIEGE) and not missing(D_DOSSR_BCR) and not missing(N_DOSSR_BCR) */
proc sql;
  insert into res
  select arrete, portefeuille, 5, "Identification dossier BCR", "I_DOSSR_BCR_SIEGE, D_DOSSR_BCR, N_DOSSR_BCR", "R18", "Exhaustivite", "Bloquant", "Les 3 composants de la cle dossier renseignes",
         count(*),
         sum(case when (not missing(I_DOSSR_BCR_SIEGE) and not missing(D_DOSSR_BCR) and not missing(N_DOSSR_BCR)) then 1 else 0 end),
         sum(w),
         sum(case when (not missing(I_DOSSR_BCR_SIEGE) and not missing(D_DOSSR_BCR) and not missing(N_DOSSR_BCR)) then w else 0 end)
  from base
  where (1=1)
  group by arrete, portefeuille;
  insert into anom
  select arrete, "R18", portefeuille, cle_ano, strip(I_DOSS_UNIQ), M_BIL_ENCOURS_PERIODE
  from base
  where (1=1) and not (not missing(I_DOSSR_BCR_SIEGE) and not missing(D_DOSSR_BCR) and not missing(N_DOSSR_BCR));
quit;
/* N01 | CDE 5 Identification dossier BCR | variable : I_DOSS_UNIQ
   DIMENSION : COHERENCE | severite : Majeur
   Regle     : Identifiant dossier coherent avec ses composants
   Population: not missing(I_DOSS_UNIQ) and not missing(N_DOSSR_BCR)  ->  conforme si : index(strip(I_DOSS_UNIQ), strip(put(N_DOSSR_BCR,best20. -L))) > 0 */
proc sql;
  insert into res
  select arrete, portefeuille, 5, "Identification dossier BCR", "I_DOSS_UNIQ", "N01", "Coherence", "Majeur", "Identifiant dossier coherent avec ses composants",
         count(*),
         sum(case when (index(strip(I_DOSS_UNIQ), strip(put(N_DOSSR_BCR,best20. -L))) > 0) then 1 else 0 end),
         sum(w),
         sum(case when (index(strip(I_DOSS_UNIQ), strip(put(N_DOSSR_BCR,best20. -L))) > 0) then w else 0 end)
  from base
  where (not missing(I_DOSS_UNIQ) and not missing(N_DOSSR_BCR))
  group by arrete, portefeuille;
  insert into anom
  select arrete, "N01", portefeuille, cle_ano, strip(I_DOSS_UNIQ), M_BIL_ENCOURS_PERIODE
  from base
  where (not missing(I_DOSS_UNIQ) and not missing(N_DOSSR_BCR)) and not (index(strip(I_DOSS_UNIQ), strip(put(N_DOSSR_BCR,best20. -L))) > 0);
quit;
/* N02 | CDE 5 Identification dossier BCR | variable : contrat_id
   DIMENSION : UNICITE | severite : Bloquant
   Regle     : Une ligne par contrat et par arrete
   Population: contrat_id ne vide  ->  conforme si : contrat_id ne vide */
proc sql;
  insert into res
  select arrete, portefeuille, 5, "Identification dossier BCR", "contrat_id", "N02", "Unicite", "Bloquant", "Une ligne par contrat et par arrete",
         count(*), count(distinct contrat_id), ., .
  from base where contrat_id ne '' group by arrete, portefeuille;
  /* anomalies de N02 : toutes les lignes d'une cle contrat presente plusieurs fois */
  insert into anom
  select b.arrete, "N02", b.portefeuille, b.cle_ano, strip(b.I_DOSS_UNIQ), b.M_BIL_ENCOURS_PERIODE
  from base as b
       inner join (select arrete, contrat_id from base where contrat_id ne '' group by arrete, contrat_id having count(*) > 1) as d
       on b.arrete = d.arrete and b.contrat_id = d.contrat_id;
quit;

/*============================================================================
  CDE 6 : Identification Loan IQ (modalite ALT)
============================================================================*/
/* R22 | CDE 6 Identification Loan IQ (modalite ALT) | variable : C_CNTRT_TYPE
   DIMENSION : VALIDITE | severite : Majeur
   Regle     : Contrat ALT porte au moins un modele
   Population: C_CNTRT_TYPE in (&lst_loaniq.)  ->  conforme si : not missing(C_CNTRT_MODLE) or not missing(C_CNTRT_MODLE_HB) */
proc sql;
  insert into res
  select arrete, portefeuille, 6, "Identification Loan IQ (modalite ALT)", "C_CNTRT_TYPE", "R22", "Validite", "Majeur", "Contrat ALT porte au moins un modele",
         count(*),
         sum(case when (not missing(C_CNTRT_MODLE) or not missing(C_CNTRT_MODLE_HB)) then 1 else 0 end),
         sum(w),
         sum(case when (not missing(C_CNTRT_MODLE) or not missing(C_CNTRT_MODLE_HB)) then w else 0 end)
  from base
  where (C_CNTRT_TYPE in (&lst_loaniq.))
  group by arrete, portefeuille;
  insert into anom
  select arrete, "R22", portefeuille, cle_ano, strip(I_DOSS_UNIQ), M_BIL_ENCOURS_PERIODE
  from base
  where (C_CNTRT_TYPE in (&lst_loaniq.)) and not (not missing(C_CNTRT_MODLE) or not missing(C_CNTRT_MODLE_HB));
quit;

/*============================================================================
  CDE 7 : Identification engagement
============================================================================*/
/* R24 | CDE 7 Identification engagement | variable : C_NOMNC_ENGGM
   DIMENSION : EXHAUSTIVITE | severite : Majeur
   Regle     : Code engagement renseigne
   Population: 1=1  ->  conforme si : not missing(C_NOMNC_ENGGM) */
proc sql;
  insert into res
  select arrete, portefeuille, 7, "Identification engagement", "C_NOMNC_ENGGM", "R24", "Exhaustivite", "Majeur", "Code engagement renseigne",
         count(*),
         sum(case when (not missing(C_NOMNC_ENGGM)) then 1 else 0 end),
         sum(w),
         sum(case when (not missing(C_NOMNC_ENGGM)) then w else 0 end)
  from base
  where (1=1)
  group by arrete, portefeuille;
  insert into anom
  select arrete, "R24", portefeuille, cle_ano, strip(I_DOSS_UNIQ), M_BIL_ENCOURS_PERIODE
  from base
  where (1=1) and not (not missing(C_NOMNC_ENGGM));
quit;

/*============================================================================
  CDE 8 : Identification contrat (modele)
============================================================================*/
/* R27 | CDE 8 Identification contrat (modele) | variable : C_CNTRT_TYPE
   DIMENSION : EXHAUSTIVITE | severite : Bloquant
   Regle     : Type de contrat renseigne
   Population: 1=1  ->  conforme si : not missing(C_CNTRT_TYPE) */
proc sql;
  insert into res
  select arrete, portefeuille, 8, "Identification contrat (modele)", "C_CNTRT_TYPE", "R27", "Exhaustivite", "Bloquant", "Type de contrat renseigne",
         count(*),
         sum(case when (not missing(C_CNTRT_TYPE)) then 1 else 0 end),
         sum(w),
         sum(case when (not missing(C_CNTRT_TYPE)) then w else 0 end)
  from base
  where (1=1)
  group by arrete, portefeuille;
  insert into anom
  select arrete, "R27", portefeuille, cle_ano, strip(I_DOSS_UNIQ), M_BIL_ENCOURS_PERIODE
  from base
  where (1=1) and not (not missing(C_CNTRT_TYPE));
quit;
/* R28 | CDE 8 Identification contrat (modele) | variable : final_model
   DIMENSION : VALIDITE | severite : Majeur
   Regle     : Type de contrat mappe a un modele
   Population: not missing(C_CNTRT_TYPE)  ->  conforme si : not missing(final_model) */
proc sql;
  insert into res
  select arrete, portefeuille, 8, "Identification contrat (modele)", "final_model", "R28", "Validite", "Majeur", "Type de contrat mappe a un modele",
         count(*),
         sum(case when (not missing(final_model)) then 1 else 0 end),
         sum(w),
         sum(case when (not missing(final_model)) then w else 0 end)
  from base
  where (not missing(C_CNTRT_TYPE))
  group by arrete, portefeuille;
  insert into anom
  select arrete, "R28", portefeuille, cle_ano, strip(I_DOSS_UNIQ), M_BIL_ENCOURS_PERIODE
  from base
  where (not missing(C_CNTRT_TYPE)) and not (not missing(final_model));
quit;
/* N08 | CDE 8 Identification contrat (modele) | variable : C_CNTRT_MODLE / final_model
   DIMENSION : COHERENCE | severite : Majeur
   Regle     : Modele bilan = modele retenu
   Population: not missing(C_CNTRT_MODLE) and not missing(final_model)  ->  conforme si : C_CNTRT_MODLE = final_model */
proc sql;
  insert into res
  select arrete, portefeuille, 8, "Identification contrat (modele)", "C_CNTRT_MODLE / final_model", "N08", "Coherence", "Majeur", "Modele bilan = modele retenu",
         count(*),
         sum(case when (C_CNTRT_MODLE = final_model) then 1 else 0 end),
         sum(w),
         sum(case when (C_CNTRT_MODLE = final_model) then w else 0 end)
  from base
  where (not missing(C_CNTRT_MODLE) and not missing(final_model))
  group by arrete, portefeuille;
  insert into anom
  select arrete, "N08", portefeuille, cle_ano, strip(I_DOSS_UNIQ), M_BIL_ENCOURS_PERIODE
  from base
  where (not missing(C_CNTRT_MODLE) and not missing(final_model)) and not (C_CNTRT_MODLE = final_model);
quit;

/*============================================================================
  CDE 9 : Duree en defaut
============================================================================*/
/* R30 | CDE 9 Duree en defaut | variable : Default_date
   DIMENSION : EXHAUSTIVITE | severite : Bloquant
   Regle     : Date de defaut renseignee (hors 1901)
   Population: 1=1  ->  conforme si : not missing(Default_date) and Default_date ne '01JAN1901'd */
proc sql;
  insert into res
  select arrete, portefeuille, 9, "Duree en defaut", "Default_date", "R30", "Exhaustivite", "Bloquant", "Date de defaut renseignee (hors 1901)",
         count(*),
         sum(case when (not missing(Default_date) and Default_date ne '01JAN1901'd) then 1 else 0 end),
         sum(w),
         sum(case when (not missing(Default_date) and Default_date ne '01JAN1901'd) then w else 0 end)
  from base
  where (1=1)
  group by arrete, portefeuille;
  insert into anom
  select arrete, "R30", portefeuille, cle_ano, strip(I_DOSS_UNIQ), M_BIL_ENCOURS_PERIODE
  from base
  where (1=1) and not (not missing(Default_date) and Default_date ne '01JAN1901'd);
quit;
/* R31 | CDE 9 Duree en defaut | variable : Default_date
   DIMENSION : VALIDITE | severite : Bloquant
   Regle     : Date de defaut <= arrete
   Population: not missing(Default_date) and Default_date ne '01JAN1901'd  ->  conforme si : Default_date <= D_ARRETE */
proc sql;
  insert into res
  select arrete, portefeuille, 9, "Duree en defaut", "Default_date", "R31", "Validite", "Bloquant", "Date de defaut <= arrete",
         count(*),
         sum(case when (Default_date <= D_ARRETE) then 1 else 0 end),
         sum(w),
         sum(case when (Default_date <= D_ARRETE) then w else 0 end)
  from base
  where (not missing(Default_date) and Default_date ne '01JAN1901'd)
  group by arrete, portefeuille;
  insert into anom
  select arrete, "R31", portefeuille, cle_ano, strip(I_DOSS_UNIQ), M_BIL_ENCOURS_PERIODE
  from base
  where (not missing(Default_date) and Default_date ne '01JAN1901'd) and not (Default_date <= D_ARRETE);
quit;
/* R34 | CDE 9 Duree en defaut | variable : TIME_IN_DEF2
   DIMENSION : EXACTITUDE | severite : Majeur
   Regle     : Duree moteur = duree recalculee (+/- 0,5 mois)
   Population: not missing(duree_recalc_mois) and not missing(TIME_IN_DEF2)  ->  conforme si : abs(TIME_IN_DEF2 - duree_recalc_mois) <= &tol_duree. */
proc sql;
  insert into res
  select arrete, portefeuille, 9, "Duree en defaut", "TIME_IN_DEF2", "R34", "Exactitude", "Majeur", "Duree moteur = duree recalculee (+/- 0,5 mois)",
         count(*),
         sum(case when (abs(TIME_IN_DEF2 - duree_recalc_mois) <= &tol_duree.) then 1 else 0 end),
         sum(w),
         sum(case when (abs(TIME_IN_DEF2 - duree_recalc_mois) <= &tol_duree.) then w else 0 end)
  from base
  where (not missing(duree_recalc_mois) and not missing(TIME_IN_DEF2))
  group by arrete, portefeuille;
  insert into anom
  select arrete, "R34", portefeuille, cle_ano, strip(I_DOSS_UNIQ), M_BIL_ENCOURS_PERIODE
  from base
  where (not missing(duree_recalc_mois) and not missing(TIME_IN_DEF2)) and not (abs(TIME_IN_DEF2 - duree_recalc_mois) <= &tol_duree.);
quit;
/* R35 | CDE 9 Duree en defaut | variable : TIME_IN_DEF2
   DIMENSION : VALIDITE | severite : Mineur
   Regle     : Duree entre 0 et la borne haute plausible
   Population: not missing(TIME_IN_DEF2)  ->  conforme si : TIME_IN_DEF2 >= 0 and TIME_IN_DEF2 <= &duree_max. */
proc sql;
  insert into res
  select arrete, portefeuille, 9, "Duree en defaut", "TIME_IN_DEF2", "R35", "Validite", "Mineur", "Duree entre 0 et la borne haute plausible",
         count(*),
         sum(case when (TIME_IN_DEF2 >= 0 and TIME_IN_DEF2 <= &duree_max.) then 1 else 0 end),
         sum(w),
         sum(case when (TIME_IN_DEF2 >= 0 and TIME_IN_DEF2 <= &duree_max.) then w else 0 end)
  from base
  where (not missing(TIME_IN_DEF2))
  group by arrete, portefeuille;
  insert into anom
  select arrete, "R35", portefeuille, cle_ano, strip(I_DOSS_UNIQ), M_BIL_ENCOURS_PERIODE
  from base
  where (not missing(TIME_IN_DEF2)) and not (TIME_IN_DEF2 >= 0 and TIME_IN_DEF2 <= &duree_max.);
quit;

/*============================================================================
  CDE 10 : EDX / CER
============================================================================*/
/* R36 | CDE 10 EDX / CER | variable : C_CNTRT_NIV_RSK
   DIMENSION : EXHAUSTIVITE | severite : Majeur
   Regle     : Niveau de risque renseigne
   Population: 1=1  ->  conforme si : not missing(C_CNTRT_NIV_RSK) */
proc sql;
  insert into res
  select arrete, portefeuille, 10, "EDX / CER", "C_CNTRT_NIV_RSK", "R36", "Exhaustivite", "Majeur", "Niveau de risque renseigne",
         count(*),
         sum(case when (not missing(C_CNTRT_NIV_RSK)) then 1 else 0 end),
         sum(w),
         sum(case when (not missing(C_CNTRT_NIV_RSK)) then w else 0 end)
  from base
  where (1=1)
  group by arrete, portefeuille;
  insert into anom
  select arrete, "R36", portefeuille, cle_ano, strip(I_DOSS_UNIQ), M_BIL_ENCOURS_PERIODE
  from base
  where (1=1) and not (not missing(C_CNTRT_NIV_RSK));
quit;
/* R37 | CDE 10 EDX / CER | variable : C_CNTRT_NIV_RSK
   DIMENSION : VALIDITE | severite : Majeur
   Regle     : Valeur dans {1=EDX, 2=CER}
   Population: not missing(C_CNTRT_NIV_RSK)  ->  conforme si : NIV_RSK_C in ('1','2') */
proc sql;
  insert into res
  select arrete, portefeuille, 10, "EDX / CER", "C_CNTRT_NIV_RSK", "R37", "Validite", "Majeur", "Valeur dans {1=EDX, 2=CER}",
         count(*),
         sum(case when (NIV_RSK_C in ('1','2')) then 1 else 0 end),
         sum(w),
         sum(case when (NIV_RSK_C in ('1','2')) then w else 0 end)
  from base
  where (not missing(C_CNTRT_NIV_RSK))
  group by arrete, portefeuille;
  insert into anom
  select arrete, "R37", portefeuille, cle_ano, strip(I_DOSS_UNIQ), M_BIL_ENCOURS_PERIODE
  from base
  where (not missing(C_CNTRT_NIV_RSK)) and not (NIV_RSK_C in ('1','2'));
quit;

/*============================================================================
  CDE 11 : Passage en perte
============================================================================*/
/* R39 | CDE 11 Passage en perte | variable : B_CNTRT_WRITE_OFF
   DIMENSION : VALIDITE | severite : Majeur
   Regle     : Indicateur dans {N, O}
   Population: 1=1  ->  conforme si : B_CNTRT_WRITE_OFF in ('N','O') */
proc sql;
  insert into res
  select arrete, portefeuille, 11, "Passage en perte", "B_CNTRT_WRITE_OFF", "R39", "Validite", "Majeur", "Indicateur dans {N, O}",
         count(*),
         sum(case when (B_CNTRT_WRITE_OFF in ('N','O')) then 1 else 0 end),
         sum(w),
         sum(case when (B_CNTRT_WRITE_OFF in ('N','O')) then w else 0 end)
  from base
  where (1=1)
  group by arrete, portefeuille;
  insert into anom
  select arrete, "R39", portefeuille, cle_ano, strip(I_DOSS_UNIQ), M_BIL_ENCOURS_PERIODE
  from base
  where (1=1) and not (B_CNTRT_WRITE_OFF in ('N','O'));
quit;
/* R40 | CDE 11 Passage en perte | variable : B_CNTRT_WRITE_OFF / M_BIL_ENCOURS_PERIODE
   DIMENSION : COHERENCE | severite : Bloquant
   Regle     : Contrat passe en perte : encours bilan nul
   Population: B_CNTRT_WRITE_OFF = 'O'  ->  conforme si : coalesce(M_BIL_ENCOURS_PERIODE,0) = 0 */
proc sql;
  insert into res
  select arrete, portefeuille, 11, "Passage en perte", "B_CNTRT_WRITE_OFF / M_BIL_ENCOURS_PERIODE", "R40", "Coherence", "Bloquant", "Contrat passe en perte : encours bilan nul",
         count(*),
         sum(case when (coalesce(M_BIL_ENCOURS_PERIODE,0) = 0) then 1 else 0 end),
         sum(w),
         sum(case when (coalesce(M_BIL_ENCOURS_PERIODE,0) = 0) then w else 0 end)
  from base
  where (B_CNTRT_WRITE_OFF = 'O')
  group by arrete, portefeuille;
  insert into anom
  select arrete, "R40", portefeuille, cle_ano, strip(I_DOSS_UNIQ), M_BIL_ENCOURS_PERIODE
  from base
  where (B_CNTRT_WRITE_OFF = 'O') and not (coalesce(M_BIL_ENCOURS_PERIODE,0) = 0);
quit;

/*============================================================================
  CDE 12 : Presence de garantie
============================================================================*/
/* N11 | CDE 12 Presence de garantie | variable : Flag_GARAN
   DIMENSION : EXHAUSTIVITE | severite : Majeur
   Regle     : Flag garantie renseigne
   Population: 1=1  ->  conforme si : not missing(Flag_GARAN) */
proc sql;
  insert into res
  select arrete, portefeuille, 12, "Presence de garantie", "Flag_GARAN", "N11", "Exhaustivite", "Majeur", "Flag garantie renseigne",
         count(*),
         sum(case when (not missing(Flag_GARAN)) then 1 else 0 end),
         sum(w),
         sum(case when (not missing(Flag_GARAN)) then w else 0 end)
  from base
  where (1=1)
  group by arrete, portefeuille;
  insert into anom
  select arrete, "N11", portefeuille, cle_ano, strip(I_DOSS_UNIQ), M_BIL_ENCOURS_PERIODE
  from base
  where (1=1) and not (not missing(Flag_GARAN));
quit;
/* R44 | CDE 12 Presence de garantie | variable : Flag_GARAN
   DIMENSION : VALIDITE | severite : Majeur
   Regle     : Flag dans {0,1}
   Population: not missing(Flag_GARAN)  ->  conforme si : Flag_GARAN in ('0','1') */
proc sql;
  insert into res
  select arrete, portefeuille, 12, "Presence de garantie", "Flag_GARAN", "R44", "Validite", "Majeur", "Flag dans {0,1}",
         count(*),
         sum(case when (Flag_GARAN in ('0','1')) then 1 else 0 end),
         sum(w),
         sum(case when (Flag_GARAN in ('0','1')) then w else 0 end)
  from base
  where (not missing(Flag_GARAN))
  group by arrete, portefeuille;
  insert into anom
  select arrete, "R44", portefeuille, cle_ano, strip(I_DOSS_UNIQ), M_BIL_ENCOURS_PERIODE
  from base
  where (not missing(Flag_GARAN)) and not (Flag_GARAN in ('0','1'));
quit;

/*============================================================================
  CDE 13 : Garantie Credit Logement
============================================================================*/
/* R48 | CDE 13 Garantie Credit Logement | variable : Flag_Crelog
   DIMENSION : VALIDITE | severite : Bloquant
   Regle     : Flag dans {0,1}
   Population: not missing(Flag_Crelog)  ->  conforme si : Flag_Crelog in ('0','1') */
proc sql;
  insert into res
  select arrete, portefeuille, 13, "Garantie Credit Logement", "Flag_Crelog", "R48", "Validite", "Bloquant", "Flag dans {0,1}",
         count(*),
         sum(case when (Flag_Crelog in ('0','1')) then 1 else 0 end),
         sum(w),
         sum(case when (Flag_Crelog in ('0','1')) then w else 0 end)
  from base
  where (not missing(Flag_Crelog))
  group by arrete, portefeuille;
  insert into anom
  select arrete, "R48", portefeuille, cle_ano, strip(I_DOSS_UNIQ), M_BIL_ENCOURS_PERIODE
  from base
  where (not missing(Flag_Crelog)) and not (Flag_Crelog in ('0','1'));
quit;
/* R50 | CDE 13 Garantie Credit Logement | variable : Flag_Crelog / Flag_GARAN
   DIMENSION : COHERENCE | severite : Majeur
   Regle     : Caution CL => garantie presente
   Population: Flag_Crelog = '1'  ->  conforme si : Flag_GARAN = '1' */
proc sql;
  insert into res
  select arrete, portefeuille, 13, "Garantie Credit Logement", "Flag_Crelog / Flag_GARAN", "R50", "Coherence", "Majeur", "Caution CL => garantie presente",
         count(*),
         sum(case when (Flag_GARAN = '1') then 1 else 0 end),
         sum(w),
         sum(case when (Flag_GARAN = '1') then w else 0 end)
  from base
  where (Flag_Crelog = '1')
  group by arrete, portefeuille;
  insert into anom
  select arrete, "R50", portefeuille, cle_ano, strip(I_DOSS_UNIQ), M_BIL_ENCOURS_PERIODE
  from base
  where (Flag_Crelog = '1') and not (Flag_GARAN = '1');
quit;

/*============================================================================
  CDE 14 : Exposition bilan
============================================================================*/
/* R51 | CDE 14 Exposition bilan | variable : M_BIL_ENCOURS_PERIODE
   DIMENSION : EXHAUSTIVITE | severite : Bloquant
   Regle     : Encours de periode renseigne
   Population: 1=1  ->  conforme si : not missing(M_BIL_ENCOURS_PERIODE) */
proc sql;
  insert into res
  select arrete, portefeuille, 14, "Exposition bilan", "M_BIL_ENCOURS_PERIODE", "R51", "Exhaustivite", "Bloquant", "Encours de periode renseigne",
         count(*),
         sum(case when (not missing(M_BIL_ENCOURS_PERIODE)) then 1 else 0 end),
         sum(w),
         sum(case when (not missing(M_BIL_ENCOURS_PERIODE)) then w else 0 end)
  from base
  where (1=1)
  group by arrete, portefeuille;
  insert into anom
  select arrete, "R51", portefeuille, cle_ano, strip(I_DOSS_UNIQ), M_BIL_ENCOURS_PERIODE
  from base
  where (1=1) and not (not missing(M_BIL_ENCOURS_PERIODE));
quit;
/* R52 | CDE 14 Exposition bilan | variable : M_BIL_ENCOURS_PERIODE
   DIMENSION : VALIDITE | severite : Bloquant
   Regle     : Encours >= 0
   Population: not missing(M_BIL_ENCOURS_PERIODE)  ->  conforme si : M_BIL_ENCOURS_PERIODE >= 0 */
proc sql;
  insert into res
  select arrete, portefeuille, 14, "Exposition bilan", "M_BIL_ENCOURS_PERIODE", "R52", "Validite", "Bloquant", "Encours >= 0",
         count(*),
         sum(case when (M_BIL_ENCOURS_PERIODE >= 0) then 1 else 0 end),
         sum(w),
         sum(case when (M_BIL_ENCOURS_PERIODE >= 0) then w else 0 end)
  from base
  where (not missing(M_BIL_ENCOURS_PERIODE))
  group by arrete, portefeuille;
  insert into anom
  select arrete, "R52", portefeuille, cle_ano, strip(I_DOSS_UNIQ), M_BIL_ENCOURS_PERIODE
  from base
  where (not missing(M_BIL_ENCOURS_PERIODE)) and not (M_BIL_ENCOURS_PERIODE >= 0);
quit;
/* R53 | CDE 14 Exposition bilan | variable : M_BIL_ENCOURS_PERIODE / expo_dwr
   DIMENSION : EXACTITUDE | severite : Bloquant
   Regle     : Encours BCR = exposition DWR (tol. 1 pct)
   Population: not missing(expo_dwr) and not missing(M_BIL_ENCOURS_PERIODE)  ->  conforme si : abs(M_BIL_ENCOURS_PERIODE - expo_dwr) <= &tol_decl.*max(abs(expo_dwr),1) */
proc sql;
  insert into res
  select arrete, portefeuille, 14, "Exposition bilan", "M_BIL_ENCOURS_PERIODE / expo_dwr", "R53", "Exactitude", "Bloquant", "Encours BCR = exposition DWR (tol. 1 pct)",
         count(*),
         sum(case when (abs(M_BIL_ENCOURS_PERIODE - expo_dwr) <= &tol_decl.*max(abs(expo_dwr),1)) then 1 else 0 end),
         sum(w),
         sum(case when (abs(M_BIL_ENCOURS_PERIODE - expo_dwr) <= &tol_decl.*max(abs(expo_dwr),1)) then w else 0 end)
  from base
  where (not missing(expo_dwr) and not missing(M_BIL_ENCOURS_PERIODE))
  group by arrete, portefeuille;
  insert into anom
  select arrete, "R53", portefeuille, cle_ano, strip(I_DOSS_UNIQ), M_BIL_ENCOURS_PERIODE
  from base
  where (not missing(expo_dwr) and not missing(M_BIL_ENCOURS_PERIODE)) and not (abs(M_BIL_ENCOURS_PERIODE - expo_dwr) <= &tol_decl.*max(abs(expo_dwr),1));
quit;

/*============================================================================
  CDE 15 : Segmentation Part / PRO
============================================================================*/
/* R55 | CDE 15 Segmentation Part / PRO | variable : SEG_DECL
   DIMENSION : EXHAUSTIVITE | severite : Bloquant
   Regle     : Segment renseigne (modele CAV)
   Population: portefeuille = 'CAV'  ->  conforme si : not missing(SEG_DECL) */
proc sql;
  insert into res
  select arrete, portefeuille, 15, "Segmentation Part / PRO", "SEG_DECL", "R55", "Exhaustivite", "Bloquant", "Segment renseigne (modele CAV)",
         count(*),
         sum(case when (not missing(SEG_DECL)) then 1 else 0 end),
         sum(w),
         sum(case when (not missing(SEG_DECL)) then w else 0 end)
  from base
  where (portefeuille = 'CAV')
  group by arrete, portefeuille;
  insert into anom
  select arrete, "R55", portefeuille, cle_ano, strip(I_DOSS_UNIQ), M_BIL_ENCOURS_PERIODE
  from base
  where (portefeuille = 'CAV') and not (not missing(SEG_DECL));
quit;
/* R56 | CDE 15 Segmentation Part / PRO | variable : SEG_DECL
   DIMENSION : VALIDITE | severite : Bloquant
   Regle     : Segment dans {Part, PRO}
   Population: not missing(SEG_DECL)  ->  conforme si : SEG_DECL in ('Part','PRO') */
proc sql;
  insert into res
  select arrete, portefeuille, 15, "Segmentation Part / PRO", "SEG_DECL", "R56", "Validite", "Bloquant", "Segment dans {Part, PRO}",
         count(*),
         sum(case when (SEG_DECL in ('Part','PRO')) then 1 else 0 end),
         sum(w),
         sum(case when (SEG_DECL in ('Part','PRO')) then w else 0 end)
  from base
  where (not missing(SEG_DECL))
  group by arrete, portefeuille;
  insert into anom
  select arrete, "R56", portefeuille, cle_ano, strip(I_DOSS_UNIQ), M_BIL_ENCOURS_PERIODE
  from base
  where (not missing(SEG_DECL)) and not (SEG_DECL in ('Part','PRO'));
quit;

/*============================================================================
  CDE 0 : Transverse
============================================================================*/
/* N10 | TRANSVERSE | variables : D_PHOTO / D_ARRETE / N_PERD
   DIMENSION : ACTUALITE | severite : Bloquant
   Regle     : D_PHOTO = D_ARRETE et N_PERD = periode traitee
   Population: 1=1  ->  conforme si : D_PHOTO = D_ARRETE and N_PERD = arrete */
proc sql;
  insert into res
  select arrete, portefeuille, 0, "Transverse", "D_PHOTO / D_ARRETE / N_PERD", "N10", "Actualite", "Bloquant", "D_PHOTO = D_ARRETE et N_PERD = periode traitee",
         count(*),
         sum(case when (D_PHOTO = D_ARRETE and N_PERD = arrete) then 1 else 0 end),
         sum(w),
         sum(case when (D_PHOTO = D_ARRETE and N_PERD = arrete) then w else 0 end)
  from base
  where (1=1)
  group by arrete, portefeuille;
  insert into anom
  select arrete, "N10", portefeuille, cle_ano, strip(I_DOSS_UNIQ), M_BIL_ENCOURS_PERIODE
  from base
  where (1=1) and not (D_PHOTO = D_ARRETE and N_PERD = arrete);
quit;

/* S2.2 Taux, seuils et statut RAG de chaque regle ----------------------------------
     Toutes severites : vert >= &v_seuil. ; orange >= &o_seuil. ; rouge sinon. La severite ne joue que sur le poids (&p_bloq. / &p_maj. / &p_min.).
   Niveau portefeuille (out.dq_res_port) puis niveau tous portefeuilles (out.dq_resultats). */
proc sql;
  create table res_tot as
  select arrete, cde_no, cde, variable, regle_id, dimension, severite, description,
         sum(n_pop) as n_pop, sum(n_ok) as n_ok, sum(a_pop) as a_pop, sum(a_ok) as a_ok
  from res
  group by arrete, cde_no, cde, variable, regle_id, dimension, severite, description;
quit;
data out.dq_resultats_&periode.;
  set res_tot;
  length statut $8 role $4 slot $2;
  n_ko = n_pop - n_ok;
  a_ko = sum(a_pop, 0) - sum(a_ok, 0);
  if n_pop > 0 then taux_nb = n_ok / n_pop;
  if a_pop > 0 then taux_enc = a_ok / a_pop; else taux_enc = taux_nb;          /* unicite : pas de poids */
  seuil_vert = &v_seuil.; seuil_orange = &o_seuil.;
  if severite = 'Bloquant' then poids_pi = &p_bloq.; else if severite = 'Majeur' then poids_pi = &p_maj.; else poids_pi = &p_min.;
  if n_pop = 0 or missing(taux_enc) then statut = 'ND';
  else if taux_enc >= seuil_vert then statut = 'VERT';
  else if taux_enc >= seuil_orange then statut = 'ORANGE';
  else statut = 'ROUGE';
  if arrete = &periode. then do; role = 'M'; slot = 'M0'; end; else do; role = 'M-1'; slot = 'M1'; end;
  version_regles = 'v2.0';
  format taux_nb taux_enc seuil_vert seuil_orange percent9.3;
run;
data out.dq_res_port_&periode.;
  set res;
  n_ko = n_pop - n_ok; a_ko = sum(a_pop, 0) - sum(a_ok, 0);
run;

/* S2.3 Scores par CDE, par dimension et global (score = somme(pi x taux) / somme(pi)) */
proc sql;
  create table sc_cde as
  select r.arrete, r.cde_no, c.CDE_ID, c.LIBELLE, c.LIBELLE_COURT, c.FAMILLE,
         sum(r.poids_pi * r.taux_enc) / sum(r.poids_pi) as score format=percent9.3
  from out.dq_resultats_&periode. as r inner join dim_cde as c on r.cde_no = c.cde_no
  where r.statut ne 'ND' group by r.arrete, r.cde_no, c.CDE_ID, c.LIBELLE, c.LIBELLE_COURT, c.FAMILLE;
  create table sc_dim as
  select arrete, dimension, sum(poids_pi * taux_enc) / sum(poids_pi) as score format=percent9.3
  from out.dq_resultats_&periode. where statut ne 'ND' group by arrete, dimension;
  create table sc_glob as
  select arrete, mean(score) as score format=percent9.3
  from sc_cde group by arrete;
quit;
data out.dq_scores_cde_&periode.;
  set sc_cde;
  length feu $8 role $4;
  if score >= &v_seuil. then feu = 'VERT'; else if score >= &o_seuil. then feu = 'ORANGE'; else feu = 'ROUGE';
  role = ifc(arrete = &periode., 'M', 'M-1');
run;

/* S2.4 Anomalies : nouvelle, persistante, resolue ou sortie du perimetre --------------
   Une anomalie est un contrat x regle. Persistante : deja presente a M-1.
   Resolue : presente a M-1, le contrat existe encore a M et la regle est satisfaite.
   Sortie  : presente a M-1, le contrat n existe plus a M (ce n est pas une correction). */
proc sql;
  create table ids_m as select distinct contrat_id from base where slot = 'M0' and contrat_id ne '';
  create table anom_st as
  select coalesce(m.regle_id, p.regle_id) as regle_id length=6,
         coalesce(m.cle_ano, p.cle_ano) as cle_ano length=72,
         coalesce(m.portefeuille, p.portefeuille) as portefeuille length=8,
         coalesce(m.doss, p.doss) as doss length=40,
         coalesce(m.encours, p.encours) as encours,
         case when m.cle_ano is not null and p.cle_ano is not null then 'PERSISTANTE'
              when m.cle_ano is not null then 'NOUVELLE'
              when p.cle_ano in (select contrat_id from ids_m) then 'RESOLUE'
              else 'SORTIE' end as statut length=12
  from (select * from anom where arrete = &periode.)  as m
       full join (select * from anom where arrete = &periode1.) as p
       on m.regle_id = p.regle_id and m.cle_ano = p.cle_ano;
  create table anom_cat as
  select a.*, r.CDE_ID, r.SEVERITE, r.CAUSE_RACINE, r.DIMENSION, r.DRIVER_ID, r.METHODE_IMPACT
  from anom_st as a left join dim_regle as r on a.regle_id = r.RULE_ID;
  create table anom_n as                       /* comptage par regle et statut */
  select regle_id, statut, count(*) as n, sum(encours) as ead from anom_cat group by regle_id, statut;
quit;

/* S2.5 Tables de lecture du diagnostic ------------------------------------------- */
proc sql;
  create table dq1 as
  select m.regle_id, m.cde, m.variable, m.dimension, m.severite, m.description,
         m.n_pop as n_pop_m, p.n_pop as n_pop_m1,
         p.taux_enc as taux_m1 format=percent9.3, m.taux_enc as taux_m format=percent9.3,
         (m.taux_enc - p.taux_enc) as ecart_pts format=percentn9.3,
         p.statut as statut_m1, m.statut as statut_m, m.a_ko as enc_ko_m format=comma18.0, p.a_ko as enc_ko_m1 format=comma18.0,
         case when m.statut = 'ND' or p.statut = 'ND' then 'n/a'
              when (m.taux_enc - p.taux_enc) <= -0.001 then 'Degrade'
              when (m.taux_enc - p.taux_enc) >= 0.001 then 'Ameliore' else 'Stable' end as evolution length=10
  from (select * from out.dq_resultats_&periode. where arrete = &periode.) as m
       left join (select * from out.dq_resultats_&periode. where arrete = &periode1.) as p on m.regle_id = p.regle_id
  order by m.cde_no, m.regle_id;
  select count(*), sum(statut_m = 'ROUGE'), sum(statut_m = 'ORANGE'), sum(statut_m = 'VERT'), sum(evolution = 'Degrade')
    into :q_n trimmed, :q_r trimmed, :q_o trimmed, :q_v trimmed, :q_deg trimmed from dq1;
  select regle_id, taux_m into :q_pire trimmed, :q_pire_t trimmed from dq1 where statut_m ne 'ND' having taux_m = min(taux_m);
quit;
data _null_;
  length k $1500;
  k = catx(' ', 'Constat : sur', strip(put(&q_n., frn.)), 'regles evaluees a M,', strip(put(&q_v., frn.)), 'sont au vert,', strip(put(&q_o., frn.)), 'a l orange et',
           strip(put(&q_r., frn.)), 'au rouge.', strip(put(&q_deg., frn.)), 'regles se degradent d au moins 0,1 point par rapport a M-1.',
           'La regle la plus faible est', "&q_pire.", '(', strip(put(&q_pire_t., frp.)), 'en encours).',
           'Decision : traiter d abord les regles bloquantes au rouge et les regles qui se degradent.');
  call symputx('k_q1', k);
run;

title1 j=l "^S={background=cx0A2F3F color=white fontsize=15pt fontweight=bold} Diagnostic 1 : resultat de chaque regle, M-1 contre M";
title3 j=l "^S={color=cxC00000 fontweight=bold} &banner.";
ods text = "^S={background=cxE1EFF5} &k_q1.";
proc report data=dq1 nowd;
  columns regle_id cde variable dimension severite description n_pop_m taux_m1 taux_m ecart_pts statut_m1 statut_m evolution enc_ko_m;
  define regle_id / 'Regle'; define cde / 'Donnee (CDE)'; define variable / 'Variable'; define dimension / 'Dimension'; define severite / 'Severite';
  define description / 'Controle'; define n_pop_m / 'Population M' format=comma12.;
  define taux_m1 / 'Taux en encours M-1'; define taux_m / 'Taux en encours M'; define ecart_pts / 'Ecart (pts)';
  define statut_m1 / 'Statut M-1'; define statut_m / 'Statut M'; define evolution / 'Evolution'; define enc_ko_m / 'Encours en anomalie M (EUR)';
  compute statut_m;
    if statut_m = 'ROUGE' then call define(_col_, 'style', 'style=[background=cxF4CCCC]');
    else if statut_m = 'ORANGE' then call define(_col_, 'style', 'style=[background=cxFCE5CD]');
    else if statut_m = 'VERT' then call define(_col_, 'style', 'style=[background=cxD9EAD3]');
  endcomp;
  compute statut_m1;
    if statut_m1 = 'ROUGE' then call define(_col_, 'style', 'style=[background=cxF4CCCC]');
    else if statut_m1 = 'ORANGE' then call define(_col_, 'style', 'style=[background=cxFCE5CD]');
    else if statut_m1 = 'VERT' then call define(_col_, 'style', 'style=[background=cxD9EAD3]');
  endcomp;
run;
/* G8. Statut des regles par severite, M-1 contre M */
title1 j=l "G8. Statut des regles par severite, M-1 contre M";
proc sgpanel data=out.dq_resultats_&periode.(where=(statut ne 'ND')) dattrmap=attr_feu;
  panelby role / columns=2 novarname;
  vbar severite / group=statut groupdisplay=stack stat=freq attrid=feu datalabel;
  rowaxis grid label="Nombre de regles"; colaxis display=(nolabel);
run;
/* G9. Top 15 des regles par encours en anomalie a M, avec M-1 */
proc sql;
  create table g9_top as select regle_id from dq1 where enc_ko_m > 0 order by enc_ko_m desc;
quit;
data g9_top; set g9_top(obs=15); run;
proc sql;
  create table g9 as
  select r.regle_id, r.role, r.a_ko from out.dq_resultats_&periode. as r inner join g9_top as t on r.regle_id = t.regle_id;
quit;
title1 j=l "G9. Top 15 des regles par encours en anomalie (EUR), M-1 contre M";
proc sgplot data=g9 dattrmap=attr_role;
  hbar regle_id / response=a_ko group=role groupdisplay=cluster attrid=role categoryorder=respdesc;
  xaxis grid label="Encours en anomalie (EUR)" valuesformat=comma18.; yaxis display=(nolabel);
run;
title1;

proc sql;
  create table dq2 as
  select m.CDE_ID, m.LIBELLE, m.FAMILLE, p.score as score_m1 format=percent9.3, m.score as score_m format=percent9.3,
         (m.score - p.score) as ecart_pts format=percentn9.3, p.feu as feu_m1, m.feu as feu_m
  from (select * from out.dq_scores_cde_&periode. where arrete = &periode.) as m
       left join (select * from out.dq_scores_cde_&periode. where arrete = &periode1.) as p on m.CDE_ID = p.CDE_ID
  order by m.CDE_ID;
  create table dq2_dim as
  select m.dimension, p.score as score_m1 format=percent9.3, m.score as score_m format=percent9.3, (m.score - p.score) as ecart_pts format=percentn9.3
  from (select * from sc_dim where arrete = &periode.) as m left join (select * from sc_dim where arrete = &periode1.) as p on m.dimension = p.dimension;
  select score into :g_m trimmed from sc_glob where arrete = &periode.;
  select score into :g_m1 trimmed from sc_glob where arrete = &periode1.;
  select count(*), sum(feu_m = 'ROUGE'), sum(feu_m = 'ORANGE') into :s_n trimmed, :s_r trimmed, :s_o trimmed from dq2;
  select LIBELLE, score_m into :s_min trimmed, :s_min_t trimmed from dq2 having score_m = min(score_m);
quit;
data _null_;
  length k $1500;
  k = catx(' ', 'Constat : le score DQ global est de', strip(put(&g_m., frp2.)), 'a M contre', strip(put(&g_m1., frp2.)), 'a M-1',
           '(moyenne simple des scores CDE).',
           'Sur', strip(put(&s_n., frn.)), 'donnees critiques,', strip(put(&s_r., frn.)), 'sont au rouge et', strip(put(&s_o., frn.)), 'a l orange (vert >= 95 %, orange >= 90 %).',
           'La plus faible est', "&s_min.", '(', strip(put(&s_min_t., frp2.)), ').',
           'Decision : valider les seuils de feu et les poids de severite avant de communiquer le score.');
  call symputx('k_q2', k);
run;

title1 j=l "^S={background=cx0A2F3F color=white fontsize=15pt fontweight=bold} Diagnostic 2 : score par donnee, par dimension et global, M-1 contre M";
title3 j=l "^S={color=cxC00000 fontweight=bold} &banner.";
ods text = "^S={background=cxE1EFF5} &k_q2.";
title4 j=l "Score par donnee critique";
proc report data=dq2 nowd;
  columns CDE_ID LIBELLE FAMILLE score_m1 score_m ecart_pts feu_m1 feu_m;
  define CDE_ID / 'CDE'; define LIBELLE / 'Libelle'; define FAMILLE / 'Famille';
  define score_m1 / 'Score M-1'; define score_m / 'Score M'; define ecart_pts / 'Ecart (pts)'; define feu_m1 / 'Feu M-1'; define feu_m / 'Feu M';
  compute feu_m;
    if feu_m = 'ROUGE' then call define(_col_, 'style', 'style=[background=cxF4CCCC]');
    else if feu_m = 'ORANGE' then call define(_col_, 'style', 'style=[background=cxFCE5CD]');
    else call define(_col_, 'style', 'style=[background=cxD9EAD3]');
  endcomp;
run;
title4 j=l "Score par dimension (regles evaluees uniquement)";
proc print data=dq2_dim noobs label; label dimension='Dimension' score_m1='Score M-1' score_m='Score M' ecart_pts='Ecart (pts)'; run;
title4 j=l "Score global";
proc print data=sc_glob noobs label; label arrete='Arrete' score='Moyenne des scores CDE'; run;
title4;
title1 j=l "G10. Score DQ par donnee critique, M-1 contre M";
proc sgplot data=out.dq_scores_cde_&periode. dattrmap=attr_role;
  hbar LIBELLE / response=score group=role groupdisplay=cluster attrid=role categoryorder=respasc;
  refline &v_seuil. / axis=x lineattrs=(color=cx1BAF7A pattern=dash) label="95 %";
  refline &o_seuil. / axis=x lineattrs=(color=cxEDA100 pattern=dash) label="90 %";
  xaxis min=0.9 max=1 grid valuesformat=percent8.1; yaxis display=(nolabel) fitpolicy=none;
run;
title1 j=l "G11. Score par dimension, M-1 contre M";
proc sgplot data=sc_dim dattrmap=attr_role;
  format arrete best12.;
  vbar dimension / response=score group=arrete groupdisplay=cluster datalabel;
  yaxis min=0.9 max=1 grid valuesformat=percent8.1; xaxis display=(nolabel);
run;
title1;

/* S2.6 Matrice CDE x dimension : quelles dimensions sont evaluees, et ou sont les ecarts ? */
proc sql;
  create table mx as
  select r.arrete, r.slot, r.role, c.LIBELLE_COURT as cde length=24, c.cde_no, r.dimension,
         sum(r.n_ok) / sum(r.n_pop) as taux_nb, sum(r.a_ok) / sum(r.a_pop) as taux_enc, sum(r.n_pop) as n_pop
  from out.dq_resultats_&periode. as r inner join dim_cde as c on r.cde_no = c.cde_no
  where r.n_pop > 0 group by r.arrete, r.slot, r.role, c.LIBELLE_COURT, c.cde_no, r.dimension;
quit;
data mx;
  set mx;
  taux = coalesce(taux_enc, taux_nb);               /* unicite : taux en nombre */
  if n_pop < &nb_min_cell. then taux = .;           /* effectif trop faible : non affiche */
  format taux percent8.2;
run;
proc sort data=mx; by cde_no descending slot; run;     /* M-1 (M1) avant M (M0) */

title1 j=l "^S={background=cx0A2F3F color=white fontsize=15pt fontweight=bold} Diagnostic 3 : matrice donnee x dimension, M-1 contre M";
title3 j=l "^S={color=cxC00000 fontweight=bold} &banner.";
ods text = "^S={background=cxE1EFF5} Lecture : chaque case est le taux en encours de la dimension pour la donnee ; une case vide signifie que la dimension n est pas evaluee pour cette donnee (choix du strict minimum) ou que l effectif est inferieur a &nb_min_cell. contrats.";
proc tabulate data=mx missing;
  class cde dimension slot / order=data;
  var taux;
  table cde='Donnee', dimension=''*slot=''*taux=''*mean=''*f=percent8.2 / misstext=' ';
  format slot $rolef.;
run;
title1 j=l "G12. Taux par donnee et dimension, M-1 (gauche) et M (droite)";
proc sgpanel data=mx(where=(not missing(taux)));
  panelby role / columns=2 novarname;
  heatmapparm x=dimension y=cde colorresponse=taux / colormodel=(cxB03A2E cxFBE7BE cx2E7D4F) outline;
  text x=dimension y=cde text=taux / textattrs=(size=7) strip;
  rowaxis display=(nolabel) reverse fitpolicy=none; colaxis display=(nolabel);
  gradlegend / title="Taux";
run;
title1;

/* S2.7 Anomalies a transmettre aux data stewards (plus gros encours d'abord, par regle) */
proc sort data=anom_cat out=anom_s(where=(statut in ('NOUVELLE','PERSISTANTE'))); by regle_id descending encours; run;
data dq4;
  set anom_s; by regle_id;
  if first.regle_id then rang = 0; rang + 1;
  if rang <= &max_anom.;
  format encours comma18.0;
run;
proc sql;
  select count(*) into :a_nv trimmed from anom_cat where statut = 'NOUVELLE';
  select count(*) into :a_pe trimmed from anom_cat where statut = 'PERSISTANTE';
  select count(*) into :a_re trimmed from anom_cat where statut = 'RESOLUE';
  select count(*) into :a_so trimmed from anom_cat where statut = 'SORTIE';
quit;
data _null_;
  length k $1200;
  k = catx(' ', 'Constat :', strip(put(&a_nv., frn.)), 'anomalies nouvelles,', strip(put(&a_pe., frn.)), 'persistantes,', strip(put(&a_re., frn.)), 'resolues et',
           strip(put(&a_so., frn.)), 'sorties du perimetre entre M-1 et M. Le tableau liste les &max_anom. plus gros encours par regle ; la liste complete est dans out.dq_anomalies.',
           'Une sortie du perimetre n est pas une correction.');
  call symputx('k_q4', k);
run;
data out.dq_anomalies_&periode.; set anom_cat; run;

title1 j=l "^S={background=cx0A2F3F color=white fontsize=15pt fontweight=bold} Diagnostic 4 : contrats en anomalie a M";
title3 j=l "^S={color=cxC00000 fontweight=bold} &banner.";
ods text = "^S={background=cxE1EFF5} &k_q4.";
proc print data=dq4 noobs label;
  var regle_id CDE_ID SEVERITE portefeuille cle_ano doss encours statut;
  label regle_id='Regle' CDE_ID='CDE' SEVERITE='Severite' portefeuille='Portefeuille' cle_ano='Contrat' doss='Dossier' encours='Encours bilan' statut='Statut vs M-1';
run;
title1;

/* S2.8 Registre initial des anomalies : une ligne par regle orange ou rouge a M ----
   Les colonnes cause probable, action et echeance sont a remplir avec les data owners. */
proc sql noprint; select sum(w) into :enc_tot trimmed from base where slot = 'M0'; quit;
proc sql;
  create table reg0 as
  select q.*, r.OWNER as owner_catalogue
  from dq1 as q left join dim_regle as r on q.regle_id = r.RULE_ID
  where q.statut_m in ('ORANGE', 'ROUGE');
quit;
data out.dq_registre_&periode.;
  length cause_probable $200 proprietaire $40 action $200 echeance 8 statut_plan $12;
  set reg0;
  encours_ko_meur = enc_ko_m / 1000000;                   /* encours concerne en M EUR  */
  part_encours_ko = enc_ko_m / &enc_tot.;                 /* part du stock concerne     */
  cause_probable = ''; action = ''; echeance = .; statut_plan = 'A QUALIFIER';
  proprietaire = owner_catalogue;
  format echeance yymmdd10. part_encours_ko percent8.3 encours_ko_meur comma12.2;
run;
proc sort data=out.dq_registre_&periode.; by statut_m descending encours_ko_meur; run;

title1 j=l "^S={background=cx0A2F3F color=white fontsize=15pt fontweight=bold} Diagnostic 5 : registre initial des anomalies (regles orange ou rouge)";
title3 j=l "^S={color=cxC00000 fontweight=bold} &banner.";
ods text = "^S={background=cxE1EFF5} Registre a completer avec les Data Owners : cause probable, action corrective, echeance. La priorite suit l encours concerne.";
proc print data=out.dq_registre_&periode. noobs label;
  var regle_id cde dimension severite statut_m taux_m encours_ko_meur part_encours_ko cause_probable proprietaire action echeance statut_plan;
  label regle_id='Regle' cde='Donnee' dimension='Dimension' severite='Severite' statut_m='Statut M' taux_m='Taux M' encours_ko_meur='Encours concerne (MEUR)'
        part_encours_ko='Part du stock' cause_probable='Cause probable' proprietaire='Proprietaire' action='Action' echeance='Echeance' statut_plan='Statut du plan';
run;
title1;

/* S2.9 Evolution entre les deux arretes : mouvements d'anomalies et regles qui se degradent */
proc sql;
  create table dq6 as
  select a.regle_id, r.LIBELLE, a.severite,
         sum(statut = 'NOUVELLE') as nouvelles label='Nouvelles', sum(statut = 'PERSISTANTE') as persistantes label='Persistantes',
         sum(statut = 'RESOLUE') as resolues label='Resolues', sum(statut = 'SORTIE') as sorties label='Sorties du perimetre'
  from anom_cat as a left join dim_regle as r on a.regle_id = r.RULE_ID
  group by a.regle_id, r.LIBELLE, a.severite order by calculated nouvelles desc;
  create table dq6_long as
  select regle_id, 'Nouvelles' as statut length=14, nouvelles as n from dq6
  union all select regle_id, 'Persistantes', persistantes from dq6
  union all select regle_id, 'Resolues', resolues from dq6
  union all select regle_id, 'Sorties', sorties from dq6;
  create table dq6_top as select regle_id from dq6 order by (nouvelles + persistantes) desc;
quit;
data dq6_top; set dq6_top(obs=15); run;
proc sql;
  create table dq6_g as select l.* from dq6_long as l inner join dq6_top as t on l.regle_id = t.regle_id;
  create table dq6_deg as
  select regle_id, description, taux_m1, taux_m, ecart_pts, statut_m1, statut_m from dq1 where evolution = 'Degrade' order by ecart_pts;
quit;

title1 j=l "^S={background=cx0A2F3F color=white fontsize=15pt fontweight=bold} Diagnostic 6 : evolution entre M-1 et M (anomalies et stabilite)";
title3 j=l "^S={color=cxC00000 fontweight=bold} &banner.";
ods text = "^S={background=cxE1EFF5} Lecture : une anomalie nouvelle signale une degradation ou un changement de source ; une anomalie persistante est un stock non traite ; une anomalie resolue est une vraie correction ; une sortie du perimetre n est pas une correction.";
proc print data=dq6 noobs label; format nouvelles persistantes resolues sorties comma12.; run;
title4 j=l "Regles qui se degradent d au moins 0,1 point";
proc print data=dq6_deg noobs label; run;
title4;
title1 j=l "G13. Anomalies par regle et par statut (15 regles les plus touchees)";
proc sgplot data=dq6_g;
  styleattrs datacolors=(cxE34948 cxEDA100 cx1BAF7A cxA0A7B8);
  vbar regle_id / response=n group=statut groupdisplay=stack;
  yaxis grid label="Anomalies" valuesformat=comma12.; xaxis display=(nolabel);
run;
title1;

title; footnote;

/*===========================================================================
  S3. PUBLICATION POUR POWER BI (etape 6 du CDC, partie des etapes 2 et 3)
      Une table PUB_* ou DIM_* = un onglet du classeur Excel (S5). Valeur absente = cellule vide.
      Aucune donnee de contrat : uniquement des comptages et des sommes agreges.
===========================================================================*/
proc sql;
  create table n_port as
  select arrete, portefeuille, count(*) as n_contrats, sum(w) as ead from base group by arrete, portefeuille;

  /* Resultat de chaque regle par arrete et portefeuille (toutes les regles du catalogue).
     Regle sans aucun resultat sur l arrete = NON TESTEE (variable absente ou population vide partout) ;
     N_INDETERMINE n est pas calcule : valeur absente = non publie. */
  create table rule_run as select distinct arrete, regle_id from out.dq_resultats_&periode.;
  create table pub.PUB_RULE_RESULT as
  select n.arrete as N_PERD, n.portefeuille as PORTEFEUILLE, d.RULE_ID, d.CDE_ID as CDE_ID length=8, d.SEVERITE,
         n.n_contrats as N_CONTRATS,
         case when x.regle_id is null then 0 else coalesce(r.n_pop, 0) end as N_TESTE,
         case when x.regle_id is null then . else coalesce(r.n_pop, 0) - coalesce(r.n_ok, 0) end as N_ECHEC,
         . as N_INDETERMINE,
         case when x.regle_id is null then 0 else n.n_contrats - coalesce(r.n_pop, 0) end as N_NON_APPLICABLE,
         case when x.regle_id is null then n.n_contrats else 0 end as N_NON_TESTE,
         case when x.regle_id is null then . else coalesce(r.a_pop, 0) - coalesce(r.a_ok, 0) end as EAD_ECHEC
  from n_port as n cross join dim_regle as d
       left join rule_run as x on x.arrete = n.arrete and x.regle_id = d.RULE_ID
       left join out.dq_res_port_&periode. as r on r.arrete = n.arrete and r.portefeuille = n.portefeuille and r.regle_id = d.RULE_ID
  order by N_PERD, PORTEFEUILLE, RULE_ID;

  /* Comptages de conformite par donnee critique (somme des controles de la donnee) */
  create table pub.PUB_CDE_SCORE as
  select r.arrete as N_PERD, r.portefeuille as PORTEFEUILLE, c.CDE_ID, sum(r.n_pop) as N_APPLICABLE, sum(r.n_ok) as N_CONFORME,
         sum(coalesce(r.a_pop, 0)) as EAD_APPLICABLE, sum(coalesce(r.a_pop, 0) - coalesce(r.a_ok, 0)) as EAD_NON_CONFORME
  from out.dq_res_port_&periode. as r inner join dim_cde as c on r.cde_no = c.cde_no
  group by r.arrete, r.portefeuille, c.CDE_ID order by N_PERD, PORTEFEUILLE, CDE_ID;

  /* Meme comptage par dimension de qualite */
  create table pub.PUB_DIM_SCORE as
  select r.arrete as N_PERD, r.portefeuille as PORTEFEUILLE, c.CDE_ID, r.dimension as DIMENSION, sum(r.n_pop) as N_APPLICABLE, sum(r.n_ok) as N_CONFORME,
         sum(coalesce(r.a_pop, 0)) as EAD_APPLICABLE, sum(coalesce(r.a_pop, 0) - coalesce(r.a_ok, 0)) as EAD_NON_CONFORME
  from out.dq_res_port_&periode. as r inner join dim_cde as c on r.cde_no = c.cde_no
  group by r.arrete, r.portefeuille, c.CDE_ID, r.dimension order by N_PERD, PORTEFEUILLE, CDE_ID, DIMENSION;

  /* Anomalies agregees : cause racine, statut vs M-1, severite */
  create table pub.PUB_ISSUE_AGG as
  select &periode. as N_PERD, portefeuille as PORTEFEUILLE, CAUSE_RACINE, statut as STATUT, SEVERITE, count(*) as N_ANOMALIES, sum(coalesce(encours, 0)) as EAD
  from anom_cat group by portefeuille, CAUSE_RACINE, statut, SEVERITE order by PORTEFEUILLE, CAUSE_RACINE, STATUT;

  /* Profil des variables : taux de renseignement par arrete */
  create table pub.PUB_PROFIL_VAR as
  select m.arrete as N_PERD, m.variable as VARIABLE, m.type as TYPE, coalesce(u.usage, '') as USAGE, m.nb_total as N_LIGNES, m.nb_manquant as N_MANQUANT,
         m.taux_renseigne as TAUX_RENSEIGNE
  from eda_manq as m left join exp_var as u on upcase(m.variable) = upcase(u.variable)
  where upcase(m.variable) ne 'ARRETE' order by N_PERD, VARIABLE;

  /* Volumetrie et grain */
  create table pub.PUB_VOLUMETRIE as
  select case when v.slot = 'M0' then &periode. else &periode1. end as N_PERD, v.nb_lignes as N_LIGNES, v.nb_contrats as N_CONTRATS, v.nb_dossiers as N_DOSSIERS,
         v.nb_cle_incomplete as N_CLE_INCOMPLETE, coalesce(d.nb_cles_doublon, 0) as N_CLES_DOUBLON, v.encours_bilan as ENCOURS_BILAN
  from e1_vol as v left join e1_dbl_n as d on v.slot = d.slot order by N_PERD;

  /* Contrats critiques : au moins un echec d'une regle bloquante */
  create table crit_ids as
  select distinct a.arrete, a.portefeuille, a.cle_ano, coalesce(a.encours, 0) as encours
  from anom as a inner join dim_regle as r on a.regle_id = r.RULE_ID where r.SEVERITE = 'Bloquant';
  create table crit as
  select arrete, portefeuille, count(*) as n_crit, sum(encours) as ead_crit from crit_ids group by arrete, portefeuille;
  select count(*) into :n_prevus trimmed from dim_regle;
  select count(*) into :n_nonvert trimmed from out.dq_resultats_&periode. where arrete = &periode. and statut in ('ORANGE', 'ROUGE');
  select count(*) into :n_grain_rouge trimmed from out.dq_resultats_&periode. where arrete = &periode. and regle_id = 'N02' and statut = 'ROUGE';
quit;
data _null_;
  length g $24;
  if &n_absent. > 0 or &n_grain_rouge. > 0 then g = 'REJECTED';
  else if &n_nonvert. > 0 then g = 'ACCEPTED_WITH_WARNINGS';
  else g = 'ACCEPTED';
  call symputx('gate', g);
run;
proc sql;
  /* Synthese du run par portefeuille (porte d'entree, couverture des controles, contrats critiques) */
  create table pub.PUB_RUN_SUMMARY as
  select n.arrete as N_PERD, n.portefeuille as PORTEFEUILLE, "&run_id." as RUN_ID length=24, "&gate." as GATE_ENTREE length=24,
         n.n_contrats as N_CONTRATS, n.ead as EAD_TOTAL, &n_prevus. as N_CONTROLES_PREVUS,
         (select count(distinct regle_id) from out.dq_res_port_&periode. as r where r.arrete = n.arrete and r.portefeuille = n.portefeuille and r.n_pop > 0) as N_CONTROLES_EXEC,
         coalesce(c.n_crit, 0) as N_CONTRATS_CRITIQUES, coalesce(c.ead_crit, 0) as EAD_CRITIQUE,
         case when "&gate." = 'REJECTED' then 'N' else 'Y' end as REFRESH_READY length=1
  from n_port as n left join crit as c on n.arrete = c.arrete and n.portefeuille = c.portefeuille
  order by N_PERD, PORTEFEUILLE;
quit;

/* Dimensions (ecrites dans le programme) */
data pub.DIM_ARRETE;
  length N_PERD 8 AS_OF_DATE 8 ROLE $4 LIBELLE $24;
  N_PERD = &periode1.; AS_OF_DATE = intnx('month', mdy(mod(&periode1., 100), 1, int(&periode1. / 100)), 0, 'end'); ROLE = 'M-1'; LIBELLE = put(AS_OF_DATE, monname9.) || ' ' || put(year(AS_OF_DATE), 4.); output;
  N_PERD = &periode.;  AS_OF_DATE = intnx('month', mdy(mod(&periode., 100), 1, int(&periode. / 100)), 0, 'end'); ROLE = 'M';   LIBELLE = put(AS_OF_DATE, monname9.) || ' ' || put(year(AS_OF_DATE), 4.); output;
  format AS_OF_DATE yymmdd10.;
run;
data pub.DIM_PORTEFEUILLE;
  infile datalines dlm='|' dsd truncover;
  length PORTEFEUILLE $8 LIBELLE $40 GROUPE $20 ORDRE 8;
  input PORTEFEUILLE $ LIBELLE $ GROUPE $ ORDRE;
datalines;
CORP|Corporate|Statistique|1
IMMO|Immobilier|Statistique|2
CONSO|Consommation|Statistique|3
REVOL|Revolving|Statistique|4
CI|Credit d investissement|Statistique|5
CAV|Comptes a vue|Statistique|6
PGE|PGE (taux forfaitaire)|Forfaitaire|7
INDIV|Individuel|Hors statistique|8
EXCL|Exclu du perimetre|Hors statistique|9
NONAFF|Non affecte (exception)|Exception|10
;
run;
data pub.DIM_FEU;
  infile datalines dlm='|' dsd truncover;
  length FEU $8 ORDRE 8 COULEUR $8;
  input FEU $ ORDRE COULEUR $;
datalines;
VERT|1|#2E9E6F
ORANGE|2|#E8A33D
ROUGE|3|#D64545
ND|4|#A0A7B8
;
run;
data pub.DIM_CDE;     set dim_cde(drop=cde_no NATURE); run;
data pub.DIM_REGLE;                                  /* catalogue des regles (les seuils de feu sont dans 00_PARAMETRES.sas) */
  set dim_regle;
run;
data pub.DIM_DIMENSION; set dim_dimension; run;

/*===========================================================================
  S4. STAGE 3 IFRS 9 - STOCKS, PONTS ET LGDD PONDEREE (page S3 du dashboard)
      Code lineaire (sans macro-programme). Source : work.base (arretes M et M-1).
      Cle de rapprochement M / M-1 : contrat_id (I_CNTRT_SIEGE-C_CNTRT_TYPE-N_CNTRT).
      Tables produites : PUB_S3_STOCKS, PUB_S3_PONTS, PUB_S3_RECONCILIATION, PUB_S3_EXCEPTIONS.
      Perimetre : TOUT le Stage 3 (filtre s3_filtre), decoupe en STAT (statistique) et SPEC (specifique).
      Les modeles (CAV, CCO, CDI, COR, CRN, IMO) ne concernent que le perimetre statistique.
      A valider : TYPROV = 'S' definit le perimetre statistique, tout autre TYPROV = specifique ;
      expo_dwr et PROV_BRUTE sont en EUR et deja en valeurs absolues ; TIME_IN_DEF2 en mois.
      Tables produites en plus : PUB_S3_TID (tranches de time in default), DIM_S3_PERIMETRE, DIM_S3_MODELE, DIM_S3_RISQUE.
===========================================================================*/
%let s3_filtre  = %str(1=1);                               /* Stage 3 total : restreindre ici si la base contient d'autres stages */
%let s3_stat    = %str(TYPROV = 'S');                      /* condition du perimetre statistique ; le reste du Stage 3 = specifique */
%let s3_modeles = %str('CAV','CCO','CDI','COR','CRN','IMO');

/* S4.1 Normalisation et separation des exceptions */
data s3_pop0 s3_exc0;
  set base;
  where (&s3_filtre.);
  length MODEL_STD $20 NATURE_RISQUE_STD $30 MOTIF_EXCEPTION $120 PERIMETRE $4 TRANCHE $16;
  if (&s3_stat.) then PERIMETRE = 'STAT'; else PERIMETRE = 'SPEC';
  MODEL_STD = upcase(strip(final_model));
  if PERIMETRE = 'SPEC' then MODEL_STD = 'SPECIFIQUE';        /* pas de modele statistique : aucune exception de modele */
  else if missing(MODEL_STD) then MODEL_STD = 'NON_AFFECTE';
  else if MODEL_STD not in (&s3_modeles.) then MODEL_STD = 'HORS_PERIMETRE';
  /* Tranche de time in default (TIME_IN_DEF2 en mois) : <=1 an, [1;2[ ... [9;10[, >10 ans */
  if missing(TIME_IN_DEF2) then do; TRANCHE = 'Non renseigne'; ORDRE_TR = 12; end;
  else if TIME_IN_DEF2 <= 12 then do; TRANCHE = '<= 1 an'; ORDRE_TR = 1; end;
  else if TIME_IN_DEF2 >= 120 then do; TRANCHE = '> 10 ans'; ORDRE_TR = 11; end;
  else do; ORDRE_TR = floor(TIME_IN_DEF2 / 12) + 1; TRANCHE = '[' || strip(put(floor(TIME_IN_DEF2 / 12), 2.)) || ' ; ' || strip(put(floor(TIME_IN_DEF2 / 12) + 1, 2.)) || '[ ans'; end;
  if missing(strip(nature_risque)) then NATURE_RISQUE_STD = 'NON_RENSEIGNE';
  else if upcase(strip(nature_risque)) = 'DOUTEUX' then NATURE_RISQUE_STD = 'DOUTEUX';
  else if upcase(strip(nature_risque)) = 'COMPROMIS' then NATURE_RISQUE_STD = 'COMPROMIS';
  else NATURE_RISQUE_STD = 'AUTRE_A_CONTROLER';
  EAD_EUR = expo_dwr;                                      /* valeurs sources deja absolues */
  PROVISION_EUR = PROV_BRUTE;
  EAD_LGDD = 0; LGDD_X_EAD = 0;                            /* LGDD ponderee : numerateur et denominateur */
  if nmiss(M_LGDD, expo_dwr) = 0 then do; EAD_LGDD = expo_dwr; LGDD_X_EAD = M_LGDD * expo_dwr; end;
  MOTIF_EXCEPTION = '';
  if missing(contrat_id) or substr(contrat_id, 1, 7) = 'SANSCLE' then MOTIF_EXCEPTION = catx('|', MOTIF_EXCEPTION, 'CONTRAT_ID_MANQUANT');
  if PERIMETRE = 'STAT' and MODEL_STD in ('HORS_PERIMETRE', 'NON_AFFECTE') then MOTIF_EXCEPTION = catx('|', MOTIF_EXCEPTION, 'MODELE_HORS_PERIMETRE');
  if NATURE_RISQUE_STD = 'AUTRE_A_CONTROLER' then MOTIF_EXCEPTION = catx('|', MOTIF_EXCEPTION, 'NATURE_RISQUE_A_CONTROLER');
  if MOTIF_EXCEPTION ne '' then output s3_exc0; else output s3_pop0;
  keep arrete role contrat_id PERIMETRE MODEL_STD NATURE_RISQUE_STD TRANCHE ORDRE_TR EAD_EUR PROVISION_EUR EAD_LGDD LGDD_X_EAD MOTIF_EXCEPTION;
run;

/* S4.2 Unicite du contrat par arrete : le doublon n'est pas supprime en silence,
        la premiere ligne est conservee, les suivantes vont dans les exceptions (motif CONTRAT_ID_DOUBLON). */
proc sort data=s3_pop0 out=s3_pop nodupkey dupout=s3_dup; by arrete contrat_id; run;
data s3_dup; set s3_dup; MOTIF_EXCEPTION = 'CONTRAT_ID_DOUBLON'; run;
proc sql noprint;
  select count(*) into :n_s3_dup trimmed from s3_dup;
  select count(*) into :n_s3_exc trimmed from s3_exc0;
quit;
%put NOTE: Stage 3 - &n_s3_dup. doublon(s) de contrat_id, &n_s3_exc. exception(s) de perimetre;

/* S4.3 Stocks : arrete x perimetre x modele x nature de risque */
proc sql;
  create table pub.PUB_S3_STOCKS as
  select arrete as N_PERD, role as ROLE length=4, PERIMETRE, MODEL_STD as MODEL length=20, NATURE_RISQUE_STD as NATURE_RISQUE length=30,
         count(*) as NB_CONTRATS, sum(EAD_EUR) as EAD_EUR, sum(PROVISION_EUR) as PROVISION_EUR,
         sum(EAD_LGDD) as EAD_AVEC_LGDD, sum(LGDD_X_EAD) as LGDD_X_EAD,
         sum(case when missing(EAD_EUR) then 1 else 0 end) as NB_EAD_MANQ,
         sum(case when missing(PROVISION_EUR) then 1 else 0 end) as NB_PROVISION_MANQ
  from s3_pop
  group by arrete, role, PERIMETRE, MODEL_STD, NATURE_RISQUE_STD
  order by N_PERD, PERIMETRE, MODEL, NATURE_RISQUE;
quit;

/* S4.4 Appariement M-1 / M sur contrat_id puis cube des transitions (evite 28 x 700 000 lignes) */
data s3_m;  set s3_pop(where=(arrete = &periode.));  rename PERIMETRE = PERIM_M MODEL_STD = MODEL_M  NATURE_RISQUE_STD = RISQUE_M  EAD_EUR = EAD_M  PROVISION_EUR = PROV_M  EAD_LGDD = EADL_M  LGDD_X_EAD = LX_M;  keep contrat_id PERIMETRE MODEL_STD NATURE_RISQUE_STD EAD_EUR PROVISION_EUR EAD_LGDD LGDD_X_EAD; run;
data s3_m1; set s3_pop(where=(arrete = &periode1.)); rename PERIMETRE = PERIM_M1 MODEL_STD = MODEL_M1 NATURE_RISQUE_STD = RISQUE_M1 EAD_EUR = EAD_M1 PROVISION_EUR = PROV_M1 EAD_LGDD = EADL_M1 LGDD_X_EAD = LX_M1; keep contrat_id PERIMETRE MODEL_STD NATURE_RISQUE_STD EAD_EUR PROVISION_EUR EAD_LGDD LGDD_X_EAD; run;
proc sort data=s3_m;  by contrat_id; run;
proc sort data=s3_m1; by contrat_id; run;
data s3_pair;
  merge s3_m(in=in_m) s3_m1(in=in_m1);
  by contrat_id;
  PRESENT_M = in_m; PRESENT_M1 = in_m1;
run;
proc sql;
  create table s3_cube as
  select PERIM_M1, PERIM_M, MODEL_M1, MODEL_M, RISQUE_M1, RISQUE_M, PRESENT_M1, PRESENT_M, count(*) as NB,
         sum(coalesce(EAD_M1, 0)) as EAD_M1_S, sum(coalesce(EAD_M, 0)) as EAD_M_S,
         sum(coalesce(PROV_M1, 0)) as PROV_M1_S, sum(coalesce(PROV_M, 0)) as PROV_M_S,
         sum(coalesce(EADL_M1, 0)) as EADL_M1_S, sum(coalesce(EADL_M, 0)) as EADL_M_S,
         sum(coalesce(LX_M1, 0)) as LX_M1_S, sum(coalesce(LX_M, 0)) as LX_M_S
  from s3_pair
  group by PERIM_M1, PERIM_M, MODEL_M1, MODEL_M, RISQUE_M1, RISQUE_M, PRESENT_M1, PRESENT_M;
quit;

/* S4.5 Expansion des scopes : perimetre (TOUT, STAT, SPEC) x portefeuille (modele) x nature de risque.
        Les modeles (hors TOUS) n'existent que dans le statistique : ils sont disponibles avec TOUT et STAT (memes valeurs, car
        seuls les contrats statistiques portent un modele) et absents avec SPEC. 3 x 4 scopes TOUS + 2 x 6 modeles x 4 natures de risque. */
data s3_scope;
  set s3_cube;
  length SCOPE_PERIMETRE $4 SCOPE_MODELE $20 SCOPE_RISQUE $30;
  array a_per[3] $4  _temporary_ ('TOUT','STAT','SPEC');
  array a_mod[7] $20 _temporary_ ('TOUS','CAV','CCO','CDI','COR','CRN','IMO');
  array a_rsk[4] $30 _temporary_ ('TOUS','DOUTEUX','COMPROMIS','NON_RENSEIGNE');
  do p = 1 to 3;
    do i = 1 to 7;
      do j = 1 to 4;
        SCOPE_PERIMETRE = a_per[p]; SCOPE_MODELE = a_mod[i]; SCOPE_RISQUE = a_rsk[j];
        if SCOPE_MODELE ne 'TOUS' and SCOPE_PERIMETRE = 'SPEC' then continue;        /* pas de modele dans le perimetre specifique */
        IN_M  = (PRESENT_M = 1  and (SCOPE_PERIMETRE = 'TOUT' or PERIM_M  = SCOPE_PERIMETRE) and (SCOPE_MODELE = 'TOUS' or MODEL_M  = SCOPE_MODELE) and (SCOPE_RISQUE = 'TOUS' or RISQUE_M  = SCOPE_RISQUE));
        IN_M1 = (PRESENT_M1 = 1 and (SCOPE_PERIMETRE = 'TOUT' or PERIM_M1 = SCOPE_PERIMETRE) and (SCOPE_MODELE = 'TOUS' or MODEL_M1 = SCOPE_MODELE) and (SCOPE_RISQUE = 'TOUS' or RISQUE_M1 = SCOPE_RISQUE));
        if IN_M or IN_M1 then output;
      end;
    end;
  end;
  drop p i j;
run;

/* S4.6 Ponts : stock M-1, sorties, entrees, variation des communs, stock M (5 etapes par scope).
        Mesures : EAD, provision, et numerateur / denominateur de la LGDD ponderee
        (LGDD ponderee = LGDD_X_EAD / EAD_AVEC_LGDD, a calculer par mesure DAX : ratio non additif). */
proc sql;
  create table pub.PUB_S3_PONTS as
  select SCOPE_PERIMETRE, SCOPE_MODELE, SCOPE_RISQUE, 1 as ORDRE_ETAPE, 'Stock M-1' as ETAPE length=24,
         sum(case when IN_M1 then NB else 0 end) as NB_CONTRATS,
         sum(case when IN_M1 then EAD_M1_S else 0 end) as EAD_PONT_EUR,
         sum(case when IN_M1 then PROV_M1_S else 0 end) as PROVISION_PONT_EUR,
         sum(case when IN_M1 then EADL_M1_S else 0 end) as EAD_AVEC_LGDD,
         sum(case when IN_M1 then LX_M1_S else 0 end) as LGDD_X_EAD
  from s3_scope group by SCOPE_PERIMETRE, SCOPE_MODELE, SCOPE_RISQUE
  union all
  select SCOPE_PERIMETRE, SCOPE_MODELE, SCOPE_RISQUE, 2, 'Sorties perimetre',
         sum(case when IN_M1 and not IN_M then NB else 0 end),
         -sum(case when IN_M1 and not IN_M then EAD_M1_S else 0 end),
         -sum(case when IN_M1 and not IN_M then PROV_M1_S else 0 end),
         -sum(case when IN_M1 and not IN_M then EADL_M1_S else 0 end),
         -sum(case when IN_M1 and not IN_M then LX_M1_S else 0 end)
  from s3_scope group by SCOPE_PERIMETRE, SCOPE_MODELE, SCOPE_RISQUE
  union all
  select SCOPE_PERIMETRE, SCOPE_MODELE, SCOPE_RISQUE, 3, 'Entrees perimetre',
         sum(case when IN_M and not IN_M1 then NB else 0 end),
         sum(case when IN_M and not IN_M1 then EAD_M_S else 0 end),
         sum(case when IN_M and not IN_M1 then PROV_M_S else 0 end),
         sum(case when IN_M and not IN_M1 then EADL_M_S else 0 end),
         sum(case when IN_M and not IN_M1 then LX_M_S else 0 end)
  from s3_scope group by SCOPE_PERIMETRE, SCOPE_MODELE, SCOPE_RISQUE
  union all
  select SCOPE_PERIMETRE, SCOPE_MODELE, SCOPE_RISQUE, 4, 'Variation communs',
         sum(case when IN_M and IN_M1 then NB else 0 end),
         sum(case when IN_M and IN_M1 then EAD_M_S - EAD_M1_S else 0 end),
         sum(case when IN_M and IN_M1 then PROV_M_S - PROV_M1_S else 0 end),
         sum(case when IN_M and IN_M1 then EADL_M_S - EADL_M1_S else 0 end),
         sum(case when IN_M and IN_M1 then LX_M_S - LX_M1_S else 0 end)
  from s3_scope group by SCOPE_PERIMETRE, SCOPE_MODELE, SCOPE_RISQUE
  union all
  select SCOPE_PERIMETRE, SCOPE_MODELE, SCOPE_RISQUE, 5, 'Stock M',
         sum(case when IN_M then NB else 0 end),
         sum(case when IN_M then EAD_M_S else 0 end),
         sum(case when IN_M then PROV_M_S else 0 end),
         sum(case when IN_M then EADL_M_S else 0 end),
         sum(case when IN_M then LX_M_S else 0 end)
  from s3_scope group by SCOPE_PERIMETRE, SCOPE_MODELE, SCOPE_RISQUE
  order by SCOPE_PERIMETRE, SCOPE_MODELE, SCOPE_RISQUE, ORDRE_ETAPE;

  /* Reconciliation : stock M-1 + sorties + entrees + variation - stock M, attendu = 0 */
  create table pub.PUB_S3_RECONCILIATION as
  select SCOPE_PERIMETRE, SCOPE_MODELE, SCOPE_RISQUE,
         sum(case when ORDRE_ETAPE in (1,2,3,4) then EAD_PONT_EUR else 0 end) - sum(case when ORDRE_ETAPE = 5 then EAD_PONT_EUR else 0 end) as ECART_EAD_EUR,
         sum(case when ORDRE_ETAPE in (1,2,3,4) then PROVISION_PONT_EUR else 0 end) - sum(case when ORDRE_ETAPE = 5 then PROVISION_PONT_EUR else 0 end) as ECART_PROVISION_EUR
  from pub.PUB_S3_PONTS group by SCOPE_PERIMETRE, SCOPE_MODELE, SCOPE_RISQUE;

  /* Exceptions agregees (aucun identifiant de contrat publie) */
  create table pub.PUB_S3_EXCEPTIONS as
  select arrete as N_PERD, PERIMETRE, MODEL_STD as MODEL length=20, NATURE_RISQUE_STD as NATURE_RISQUE length=30, MOTIF_EXCEPTION, count(*) as NB_LIGNES
  from (select arrete, PERIMETRE, MODEL_STD, NATURE_RISQUE_STD, MOTIF_EXCEPTION from s3_exc0
        outer union corresponding
        select arrete, PERIMETRE, MODEL_STD, NATURE_RISQUE_STD, MOTIF_EXCEPTION from s3_dup)
  group by arrete, PERIMETRE, MODEL_STD, NATURE_RISQUE_STD, MOTIF_EXCEPTION;
  select max(abs(ECART_EAD_EUR)) + max(abs(ECART_PROVISION_EUR)) into :s3_ecart trimmed from pub.PUB_S3_RECONCILIATION;
quit;
%put NOTE: Stage 3 - ecart maximal de reconciliation = &s3_ecart. (attendu 0) ; revoir PUB_S3_EXCEPTIONS avant publication;

/* S4.7 Tranches de time in default (tableau de la page S3) : contrats, exposition et provision par tranche,
        pour chaque arrete (M / M-1), perimetre (TOUT, STAT, SPEC), portefeuille (TOUS + 6 modeles) et nature de risque (TOUS + 3). */
proc sql;
  create table s3_tid0 as
  select arrete, role, PERIMETRE, MODEL_STD as MODELE, NATURE_RISQUE_STD as RISQUE, ORDRE_TR, TRANCHE,
         count(*) as NB, sum(EAD_EUR) as EAD, sum(PROVISION_EUR) as PROV
  from s3_pop group by arrete, role, PERIMETRE, MODEL_STD, NATURE_RISQUE_STD, ORDRE_TR, TRANCHE;
quit;
data s3_tid1;
  set s3_tid0;
  length SCOPE_PERIMETRE $4 SCOPE_MODELE $20 SCOPE_RISQUE $30;
  do p = 1 to 2;
    do r = 1 to 2;
      do m = 1 to 2;
        if m = 2 and PERIMETRE ne 'STAT' then continue;          /* un modele n'existe que pour un contrat statistique */
        if p = 1 then SCOPE_PERIMETRE = 'TOUT'; else SCOPE_PERIMETRE = PERIMETRE;
        if r = 1 then SCOPE_RISQUE = 'TOUS'; else SCOPE_RISQUE = RISQUE;
        if m = 1 then SCOPE_MODELE = 'TOUS'; else SCOPE_MODELE = MODELE;
        output;
      end;
    end;
  end;
  drop p r m;
run;
proc sql;
  create table pub.PUB_S3_TID as
  select arrete as N_PERD, role as ROLE length=4, SCOPE_PERIMETRE, SCOPE_MODELE, SCOPE_RISQUE, ORDRE_TR as ORDRE_TRANCHE, TRANCHE,
         sum(NB) as NB_CONTRATS, sum(EAD) as EAD_EUR, sum(PROV) as PROVISION_EUR
  from s3_tid1
  group by arrete, role, SCOPE_PERIMETRE, SCOPE_MODELE, SCOPE_RISQUE, ORDRE_TR, TRANCHE
  order by N_PERD, SCOPE_PERIMETRE, SCOPE_MODELE, SCOPE_RISQUE, ORDRE_TRANCHE;
quit;

/* S4.8 Dimensions des filtres de la page S3 : perimetre, portefeuille (modele) et nature de risque, partagees par PUB_S3_PONTS et PUB_S3_TID */
data pub.DIM_S3_PERIMETRE;
  infile datalines dlm='|' truncover;
  length SCOPE_PERIMETRE $4 LIBELLE $20 ORDRE 8;
  input SCOPE_PERIMETRE $ LIBELLE $ ORDRE;
datalines;
TOUT|Tout le Stage 3|1
STAT|Statistique|2
SPEC|Specifique|3
;
run;
data pub.DIM_S3_MODELE;
  infile datalines dlm='|' truncover;
  length SCOPE_MODELE $20 LIBELLE $20 ORDRE 8;
  input SCOPE_MODELE $ LIBELLE $ ORDRE;
datalines;
TOUS|Tous|1
CAV|CAV|2
CCO|CCO|3
CDI|CDI|4
COR|COR|5
CRN|CRN|6
IMO|IMO|7
;
run;
data pub.DIM_S3_RISQUE;
  infile datalines dlm='|' truncover;
  length SCOPE_RISQUE $30 LIBELLE $30 ORDRE 8;
  input SCOPE_RISQUE $ LIBELLE $ ORDRE;
datalines;
TOUS|Toutes natures|1
DOUTEUX|Douteux|2
COMPROMIS|Compromis|3
NON_RENSEIGNE|Non renseigne|4
;
run;


/*===========================================================================
  S5. CLASSEUR EXCEL POWER BI DU PROGRAMME 01 : un seul fichier, un onglet par table
      Fichier : &dir_out./PBI_01_Qualite_&periode..xlsx
      Onglets : PUB_* et DIM_* (a charger dans Power BI), PUB_MANIFEST_E23 et PUB_RUN_LOG_E23 (controle).
      Aucune donnee de contrat : uniquement des comptages et des sommes agreges.
===========================================================================*/
data lst_a;
  infile datalines dlm='|' truncover;
  length TABLE_NAME $32 VAGUE 8;
  input TABLE_NAME $ VAGUE;
datalines;
PUB_RUN_SUMMARY|1
PUB_CDE_SCORE|1
PUB_DIM_SCORE|1
PUB_RULE_RESULT|1
PUB_ISSUE_AGG|1
PUB_PROFIL_VAR|1
PUB_VOLUMETRIE|1
PUB_S3_STOCKS|1
PUB_S3_PONTS|1
PUB_S3_RECONCILIATION|1
PUB_S3_EXCEPTIONS|1
PUB_S3_TID|1
DIM_ARRETE|0
DIM_PORTEFEUILLE|0
DIM_CDE|0
DIM_REGLE|0
DIM_DIMENSION|0
DIM_FEU|0
DIM_S3_PERIMETRE|0
DIM_S3_MODELE|0
DIM_S3_RISQUE|0
;
run;

/* Manifeste : nombre de lignes et de colonnes de chaque onglet (remplace l empreinte des CSV) */
proc sql;
  create table pub.PUB_MANIFEST_E23 as
  select &periode. as N_PERD, "&run_id." as PUBLICATION_ID length=24, l.TABLE_NAME length=32, l.VAGUE,
         'V3.0' as SCHEMA_VERSION length=8, t.nobs as N_LIGNES, t.nvar as N_COLONNES,
         case when "&gate." = 'REJECTED' or &n_s3_dup. > 0 then 'N' else 'Y' end as REFRESH_READY length=1
  from lst_a as l left join dictionary.tables as t on t.libname = 'PUB' and upcase(t.memname) = upcase(l.TABLE_NAME)
  order by VAGUE desc, TABLE_NAME;
quit;
data pub.PUB_RUN_LOG_E23;
  length N_PERD 8 RUN_ID $24 STEP_ID $24 DUREE_S 8 STATUT $8 MESSAGE $200;
  N_PERD = &periode.; RUN_ID = "&run_id."; STEP_ID = 'ETAPES_2_3'; DUREE_S = round(datetime() - &t_start., 1);
  STATUT = ifc(&n_absent. > 0 or &n_s3_dup. > 0, 'ERREUR', 'OK');
  MESSAGE = catx(' ', 'Porte d entree', "&gate.", ';', put(&n_absent., 4. -L), 'variable(s) attendue(s) absente(s) ;', put(&n_s3_dup., 8. -L), 'doublon(s) contrat Stage 3');
run;
data lst_x; set lst_a end=fin; output; if fin then do; TABLE_NAME = 'PUB_MANIFEST_E23'; VAGUE = 0; output; TABLE_NAME = 'PUB_RUN_LOG_E23'; output; end; run;

/* Ecriture : on supprime l ancien classeur puis un onglet par table (nom d onglet = nom de table) */
filename xlout "&dir_out./PBI_01_Qualite_&periode..xlsx";
data _null_; rc = fdelete('xlout'); run;
filename xlout clear;
libname xl xlsx "&dir_out./PBI_01_Qualite_&periode..xlsx";
data _null_;
  set lst_x;
  call execute(cats('data xl.', TABLE_NAME, '; set pub.', TABLE_NAME, '; run;'));
run;
libname xl clear;

/* FIN : lire le classeur Excel, trancher avec les Data Owners (domaines, bornes, tolerances),
   puis lancer le programme 02 (routage et HLC). */
