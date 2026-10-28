/******************************************************************************
 Programme : DQ_06_PUBLICATION_REPORTING.sas      (programme 6 sur 6 - dispositif DQ LGD / provisions)
 Etape CDC : Etape 6 - Publication ; prepare l etape 7 (Power BI) et la presentation des analyses
 Version   : v1.2
 Objet     : Tables agregees PUB_* et DIM_* et export CSV pour Power BI (manifeste a empreinte chainee), puis rapport Excel EDA / diagnostic avec tableaux, constats et graphiques.
 Entrees   : dq.* (programmes 01 a 05), stg.*, ref.*
 Sorties   : pub.*, CSV UTF-8 dans &root./csv, &root./rapport/DQ_EDA_Diagnostic_&am..xlsx (19 onglets, 20 graphiques)
 Contenu   : S0 parametres | S1 publication Power BI | S2 rapport Excel (sous-sections R1 a R17)
 Style     : lineaire, sans macro programme. Seules les variables macro (%let),
             quelques call execute sont utilises. Chaque section est precedee de
             son commentaire.
 Ordre     : lancer les programmes dans l ordre 01 a 06 (un programme peut etre
             relance seul : il recree ses tables, sans doublon d historique).
******************************************************************************/
/*---------------------------------------------------------------------------
  SECTION 0 : PARAMETRES COMMUNS (identique dans les 6 programmes)
  A modifier a chaque arrete : root, am, am1, run_seq. Modifier les memes
  lignes dans les 6 programmes, ou lancer les 6 programmes dans la meme session.
---------------------------------------------------------------------------*/
/* S0.1 Parametres a modifier a chaque arrete ---------------------------------*/
%let root = /sasdata/dq_lgd;      /* racine de l'arborescence DQ (a adapter)   */
%let am   = 202609;               /* arrete M   au format AAAAMM               */
%let am1  = 202608;               /* arrete M-1 au format AAAAMM               */
%let run_seq = 01;                /* numero de relance dans le mois            */

/* S0.2 Parametres derives (ne pas modifier) ---------------------------------*/
%let prog           = &root./programmes;   /* repertoire des programmes */
%let run_id         = RUN_&am._&run_seq.;
%let code_version   = V1.2;
%let schema_version = V1.2;

options dlcreatedir nofmterr validvarname=v7 missing='.';

/* Dates d'arrete = dernier jour du mois, stockees en jours SAS */
data _null_;
  m  = intnx('month', mdy(mod(&am.,100),  1, floor(&am./100)),  0, 'end');
  m1 = intnx('month', mdy(mod(&am1.,100), 1, floor(&am1./100)), 0, 'end');
  call symputx('asof_m',  m);
  call symputx('asof_m1', m1);
run;

/* S0.3 Bibliotheques ---------------------------------------------------------
   ent = paquets deposes (lecture seule)   ref = referentiels approuves
   raw = copie brute des paquets          stg = tables harmonisees
   dq  = diagnostics, anomalies, KPI      pub = tables publiees pour Power BI */
libname ent "&root./entrees";
libname ref "&root./ref";
libname raw "&root./raw";
libname stg "&root./stg";
libname dq  "&root./dq";
libname pub "&root./pub";
filename csvout "&root./csv";            /* repertoire des CSV Power BI */


/* S0.5 Seuils approuves en variables macro s_<SEUIL_ID> (ref.REF_SEUIL) */
data _null_;
  set ref.REF_SEUIL;
  call symputx('s_' || strip(SEUIL_ID), VALEUR);
run;


/*======================================================================*/
/* SECTION 1 : PUBLICATION POWER BI */
/******************************************************************************
 Section 1.1 (ex-programme DQ_06_PUBLICATION_POWERBI)
 Etape     : 6 - Publication des tables pour Power BI (CSV)
 Objet     : Assembler les tables PUB_* (faits agreges, une seule fois par arrete), les dimensions
             DIM_*, le registre des decisions, la certification et le manifeste, puis les exporter en CSV.
 Principes : - aucune donnee de contrat : uniquement des agregats additifs (sommes et comptages) ;
             - les taux (conformite, concordance) sont calcules dans Power BI a partir des comptages ;
             - toutes les tables de faits portent N_PERD (entier AAAAMM) et PORTEFEUILLE ;
             - une relance du meme arrete remplace les fichiers (idempotent).
 Format CSV: UTF-8, separateur virgule, point decimal, dates AAAA-MM-JJ, valeur nulle = champ vide,
             identifiants en texte. L'ordre des colonnes est celui du dictionnaire (voir LISEZMOI).
 Vagues    : 1 = qualite des donnees | 2 = HLC | 3 = provision et impact | 4 = gouvernance
 Entrees   : dq.AGG_*, dq.ECL_EFFECT_BRIDGE, dq.RUN_LOG, ref.REF_*
 Sorties   : bibliotheque pub (tables PUB_* et DIM_*) et fichiers CSV dans &root./csv
 Version   : v1.2
******************************************************************************/
%let t0 = %sysfunc(datetime());   /* heure de debut de la section */

/* Echeances de traitement des decisions (convention de pilotage, a adapter) */
%let ech_bloq  = 30NOV2026;      /* decision bloquante */
%let ech_autre = 15DEC2026;      /* autre decision ouverte */

/* 1. Vague 1 : qualite des donnees --------------------------------------------------------------- */
proc sql;
  /* nombre de controles executes (au moins un contrat teste) par portefeuille et arrete */
  create table work.exec as
  select N_PERD, PORTEFEUILLE, count(distinct RULE_ID) as N_CONTROLES_EXEC
  from dq.AGG_RULE_RESULT where N_TESTE > 0
  group by N_PERD, PORTEFEUILLE;

  create table pub.PUB_RUN_SUMMARY as
  select s.N_PERD, s.PORTEFEUILLE, "&run_id." as RUN_ID length=20, g.STATUT as GATE_ENTREE length=24,
         s.N_CONTRATS, s.EAD_TOTAL, 27 as N_CONTROLES_PREVUS,
         coalesce(x.N_CONTROLES_EXEC, 0) as N_CONTROLES_EXEC,
         s.N_CONTRATS_CRITIQUES, s.EAD_CRITIQUE, 'Y' as REFRESH_READY length=1
  from dq.AGG_RUN_SUMMARY as s
       left join dq.DQ_INPUT_QUALITY_GATE as g on s.N_PERD = g.N_PERD
       left join work.exec as x on s.N_PERD = x.N_PERD and s.PORTEFEUILLE = x.PORTEFEUILLE
  order by s.N_PERD, s.PORTEFEUILLE;

  create table pub.PUB_CDE_SCORE as
  select N_PERD, PORTEFEUILLE, CDE_ID, N_APPLICABLE, N_CONFORME, EAD_APPLICABLE, EAD_NON_CONFORME
  from dq.AGG_CDE_SCORE order by N_PERD, PORTEFEUILLE, CDE_ID;

  create table pub.PUB_RULE_RESULT as
  select N_PERD, PORTEFEUILLE, RULE_ID, CDE_ID, SEVERITE, N_CONTRATS, N_TESTE, N_ECHEC,
         N_INDETERMINE, N_NON_APPLICABLE, N_NON_TESTE, EAD_ECHEC
  from dq.AGG_RULE_RESULT order by N_PERD, PORTEFEUILLE, RULE_ID;

  create table pub.PUB_ISSUE_AGG as
  select N_PERD, PORTEFEUILLE, CAUSE_RACINE, STATUT, SEVERITE, N_ANOMALIES, EAD
  from dq.AGG_ISSUE order by N_PERD, PORTEFEUILLE, CAUSE_RACINE, STATUT, SEVERITE;

  create table pub.PUB_SCOPE as
  select N_PERD, PORTEFEUILLE, ROUTE, CLASSE_POP, N_CONTRATS, EAD
  from dq.AGG_SCOPE order by N_PERD, PORTEFEUILLE, ROUTE, CLASSE_POP;

/* 2. Vague 2 : concordance HLC et drivers ------------------------------------------------------ */
  create table pub.PUB_HLC_COMPARE as
  select N_PERD, PORTEFEUILLE, MATCH_STATUS, MISMATCH_CAUSE, N_CONTRATS, EAD, ECL_PROD
  from dq.AGG_HLC_COMPARE order by N_PERD, PORTEFEUILLE, MATCH_STATUS, MISMATCH_CAUSE;

  create table pub.PUB_HLC_DRIVER_DIFF as
  select N_PERD, PORTEFEUILLE, DRIVER_ID, HLC_PROD, HLC_RECALC, N_CONTRATS, EAD, ECL_ECART
  from dq.AGG_HLC_DRIVER_DIFF order by N_PERD, PORTEFEUILLE, DRIVER_ID, HLC_PROD, HLC_RECALC;

/* 3. Vague 3 : provision, bridge et impact --------------------------------------------------- */
  create table pub.PUB_ECL_AGG as
  select N_PERD, PORTEFEUILLE, N_CONTRATS, EAD_BIL, EAD_HB, ECL, ECL_PROD, ECL_CORRIGE
  from dq.AGG_ECL order by N_PERD, PORTEFEUILLE;

  create table pub.PUB_ECL_EFFECT as
  select N_PERD, PORTEFEUILLE, EFFET_CODE, MONTANT
  from dq.ECL_EFFECT_BRIDGE order by PORTEFEUILLE, EFFET_CODE;

  create table pub.PUB_IMPACT_AGG as
  select N_PERD, PORTEFEUILLE, STATUT_IMPACT, N_ANOMALIES, EAD, IMPACT_ECL
  from dq.AGG_IMPACT order by N_PERD, PORTEFEUILLE, STATUT_IMPACT;
quit;

/* 4. Vague 4 : gouvernance ------------------------------------------------------------------------
   Statut de la decision : lu dans le texte du statut du cahier des charges (mots ASCII).
   Decisions bloquantes : liste du cahier des charges V4 (Q14 Q15 Q25 Q31 Q32 Q45 Q46 Q47 Q49 Q50
   Q51 Q52 Q53 Q57). Tant qu'une decision bloquante n'est pas fermee, l'arrete n'est pas certifiable. */
data pub.PUB_DECISION;
  length N_PERD 8 DECISION_ID $10 SUJET $76 PROPRIETAIRE $30 STATUT $12 BLOQUANT $1 ECHEANCE 8;
  set ref.REF_DECISION;
  N_PERD = &am.;
  if index(STATUT_CDC, 'Ferm') = 1 then STATUT = 'FERMEE';
  else if index(STATUT_CDC, 'Suppr') = 1 then STATUT = 'SUPPRIMEE';
  else if index(STATUT_CDC, 'Part') = 1 then STATUT = 'PARTIELLE';
  else if index(STATUT_CDC, 'confirmer') > 0 then STATUT = 'A_CONFIRMER';
  else STATUT = 'OUVERTE';
  BLOQUANT = ifc(DECISION_ID in ('Q14','Q15','Q25','Q31','Q32','Q45','Q46','Q47','Q49','Q50',
                                  'Q51','Q52','Q53','Q57'), 'Y', 'N');
  if DECISION_ID in ('Q45','Q49','Q50','Q57') then PROPRIETAIRE = 'Data Owner Credit';
  else if DECISION_ID in ('Q51','Q52','Q53') then PROPRIETAIRE = 'Finance';
  else PROPRIETAIRE = 'Model Owner LGD';
  ECHEANCE = .;
  if BLOQUANT = 'Y' then ECHEANCE = "&ech_bloq."d;
  else if STATUT in ('OUVERTE','PARTIELLE','A_CONFIRMER') then ECHEANCE = "&ech_autre."d;
  format ECHEANCE yymmdd10.;
  keep N_PERD DECISION_ID SUJET PROPRIETAIRE STATUT BLOQUANT ECHEANCE;
run;

/* Certification : une ligne par motif de non-certification */
data pub.PUB_CERTIFICATION;
  length N_PERD 8 STATUT $14 MOTIF_ID $4 MOTIF $200;
  set pub.PUB_DECISION;
  where BLOQUANT = 'Y' and STATUT ne 'FERMEE';
  N_PERD = &am.; STATUT = 'NOT_CERTIFIED';
  MOTIF_ID = cats('M', put(_n_, z2.));
  MOTIF = catx(' ', DECISION_ID, 'ouverte (bloquante) :', SUJET);
  output;
  keep N_PERD STATUT MOTIF_ID MOTIF;
run;

proc sql noprint;
  select count(*) into :n_motifs trimmed from pub.PUB_CERTIFICATION;
quit;

data work.cert0;
  length N_PERD 8 STATUT $14 MOTIF_ID $4 MOTIF $200;
  N_PERD = &am.; STATUT = 'CERTIFIED'; MOTIF_ID = 'M00';
  MOTIF = 'Aucune decision bloquante ouverte';
  if &n_motifs. = 0 then output;
run;
proc append base=pub.PUB_CERTIFICATION data=work.cert0 force; run;

/* Journal des etapes du run (les etapes deja terminees) */
proc sql;
  create table pub.PUB_RUN_LOG as
  select N_PERD, RUN_ID, STEP_ID, DUREE_S, STATUT, MESSAGE
  from dq.RUN_LOG where RUN_ID = "&run_id." order by STEP_ID;
quit;

/* Audit des parametres appliques : LGDD, facteur FL, taux de tirage, taux PGE, seuils */
data pub.PUB_PARAM_AUDIT;
  length N_PERD 8 PARAMETER_ID $40 VALEUR 8 VALID_FROM VALID_TO 8 VERSION $28 APPROBATION $28;
  format VALID_FROM VALID_TO yymmdd10.;
  set ref.REF_LGDD (in=a rename=(VERSION=V1 APPROBATION=A1))
      ref.REF_TAUX_TIRAGE (in=b rename=(VERSION=V2 APPROBATION=A2))
      ref.REF_PGE_TAUX (in=c rename=(VERSION=V3 APPROBATION=A3))
      ref.REF_SEUIL (in=d rename=(STATUT=A4));
  if a then do;
    N_PERD = year(VALID_TO) * 100 + month(VALID_TO); VERSION = V1; APPROBATION = A1;
    PARAMETER_ID = cats('LGDD_', MODELE, '_H', HLC); VALEUR = LGDD; output;
    PARAMETER_ID = cats('FL_', MODELE, '_H', HLC);   VALEUR = FL;   output;
  end;
  else if b then do;
    N_PERD = year(VALID_TO) * 100 + month(VALID_TO); VERSION = V2; APPROBATION = A2;
    PARAMETER_ID = cats('TT_', MODELE, '_E', TYPRIS); VALEUR = TAUX_TIRAGE; output;
  end;
  else if c then do;
    N_PERD = &am.; VERSION = V3; APPROBATION = A3;
    PARAMETER_ID = cats('PGE_', C_NOMNC_ENGGM, '_E', TYPRIS); VALEUR = TAUX_FORFAIT; output;
  end;
  else if d then do;
    N_PERD = &am.; VERSION = 'SEUILS_2026'; APPROBATION = A4;
    VALID_FROM = mdy(1, 1, 2026); VALID_TO = mdy(12, 31, 9999);
    PARAMETER_ID = cats('SEUIL_', SEUIL_ID); output;
  end;
  keep N_PERD PARAMETER_ID VALEUR VALID_FROM VALID_TO VERSION APPROBATION;
run;

/* 5. Dimensions ----------------------------------------------------------------------------------- */
data pub.DIM_ARRETE;
  length N_PERD 8 AS_OF_DATE 8 ROLE $4 LIBELLE $20;
  format AS_OF_DATE yymmdd10.;
  array mois[12] $10 _temporary_ ('Janvier','Fevrier','Mars','Avril','Mai','Juin','Juillet',
                                  'Aout','Septembre','Octobre','Novembre','Decembre');
  N_PERD = &am1.; AS_OF_DATE = &asof_m1.; ROLE = 'M-1';
  LIBELLE = catx(' ', mois[mod(&am1., 100)], put(floor(&am1. / 100), 4.)); output;
  N_PERD = &am.;  AS_OF_DATE = &asof_m.;  ROLE = 'M';
  LIBELLE = catx(' ', mois[mod(&am., 100)], put(floor(&am. / 100), 4.)); output;
run;

proc sql;
  create table pub.DIM_PORTEFEUILLE as
  select PORTEFEUILLE, LIBELLE, GROUPE, ORDRE from ref.REF_PORTEFEUILLE order by ORDRE;
  create table pub.DIM_CDE as
  select CDE_ID, LIBELLE, LIBELLE_COURT, FAMILLE, POIDS, SEUIL_VERT, SEUIL_ORANGE from ref.REF_CDE;
  create table pub.DIM_REGLE as
  select RULE_ID, LIBELLE, CDE_ID, SEVERITE from ref.REF_REGLE;
  create table pub.DIM_FEU as select FEU, ORDRE, COULEUR from ref.REF_FEU;
  create table pub.DIM_MVT as select CODE, LIBELLE, ORDRE from ref.REF_MVT;
  create table pub.DIM_DRIVER as select DRIVER_ID, LIBELLE from ref.REF_DRIVER;
  create table pub.DIM_EFFET as select EFFET_CODE, LIBELLE, ORDRE, GROUPE from ref.REF_EFFET;
quit;

/* 6. Format numerique : precision complete, sans notation scientifique -------------------------
   (les dates gardent leur format AAAA-MM-JJ, defini a la creation des tables ci-dessus) */
proc sql;
  create table work.cols as
  select memname, name
  from dictionary.columns
  where libname = 'PUB' and type = 'num' and format = '' and (memname like 'PUB%' or memname like 'DIM%')
  order by memname, varnum;
quit;

data _null_;
  set work.cols;
  by memname;
  if first.memname then call execute(cats('proc datasets lib=pub nolist; modify ', memname, '; format'));
  call execute(cats(' ', name));
  if last.memname then call execute(' best15.; quit;');
run;

/* 7. Liste des tables a exporter, avec leur vague ---------------------------------------------- */
data work.lst;
  length TABLE_NAME $32 VAGUE 8;
  input TABLE_NAME $ VAGUE;
  datalines;
PUB_RUN_SUMMARY 1
PUB_CDE_SCORE 1
PUB_RULE_RESULT 1
PUB_ISSUE_AGG 1
PUB_SCOPE 1
PUB_HLC_COMPARE 2
PUB_HLC_DRIVER_DIFF 2
PUB_ECL_AGG 3
PUB_ECL_EFFECT 3
PUB_IMPACT_AGG 3
PUB_CERTIFICATION 4
PUB_DECISION 4
PUB_RUN_LOG 4
PUB_PARAM_AUDIT 4
DIM_ARRETE 0
DIM_PORTEFEUILLE 0
DIM_CDE 0
DIM_REGLE 0
DIM_FEU 0
DIM_MVT 0
DIM_DRIVER 0
DIM_EFFET 0
;
run;

/* 8. Export CSV en UTF-8 : un fichier par table -----------------------------------------------
   Le champ numerique manquant est ecrit vide (option MISSING temporairement a blanc). */
options missing=' ';
data _null_;
  set work.lst;
  call execute(cats('filename csvf "', "&root.", '/csv/', TABLE_NAME, '.csv" encoding="utf-8";'));
  call execute(cats('proc export data=pub.', TABLE_NAME, ' outfile=csvf dbms=csv replace; run;'));
  call execute('filename csvf clear;');
run;
options missing='.';

/* 9. Manifeste : nombre de lignes et empreinte (MD5 chaine ligne a ligne) de chaque fichier ----
   Empreinte = h(0) = vide ; h(i) = MD5 en minuscules de (h(i-1) puis la ligne i), en-tete inclus.
   Power BI (ou le controle) peut recalculer cette empreinte pour detecter un fichier modifie. */
data pub.PUB_MANIFEST;
  length N_PERD 8 PUBLICATION_ID $24 TABLE_NAME $32 VAGUE 8 SCHEMA_VERSION $8
         N_LIGNES 8 EMPREINTE $32 REFRESH_READY $1 f $300 h $32;
  set work.lst;
  N_PERD = &am.; PUBLICATION_ID = "&run_id."; SCHEMA_VERSION = "&schema_version."; REFRESH_READY = 'Y';
  f = cats("&root.", '/csv/', TABLE_NAME, '.csv');
  h = ''; n = 0; eof = 0;
  do until (eof);
    infile dummy filevar=f end=eof encoding='utf-8' lrecl=32767 truncover;
    input;
    n + 1;
    h = lowcase(put(md5(cats(h, _infile_)), $hex32.));
  end;
  N_LIGNES = n - 1;                                   /* sans l'en-tete */
  EMPREINTE = h;
  keep N_PERD PUBLICATION_ID TABLE_NAME VAGUE SCHEMA_VERSION N_LIGNES EMPREINTE REFRESH_READY;
run;

filename csvf "&root./csv/PUB_MANIFEST.csv" encoding="utf-8";
options missing=' ';
proc export data=pub.PUB_MANIFEST outfile=csvf dbms=csv replace; run;
options missing='.';
filename csvf clear;

%let step_id  = DQ_06_PUBLICATION;
%let step_msg = Tables PUB et CSV Power BI publies;
proc sql;
  delete from dq.RUN_LOG where RUN_ID = "&run_id." and STEP_ID = "&step_id.";
quit;
data work.log1;
  length RUN_ID $20 STEP_ID $40 STATUT $8 MESSAGE $200;
  N_PERD = &am.; RUN_ID = "&run_id."; STEP_ID = "&step_id.";
  DEBUT = &t0.; FIN = datetime(); DUREE_S = round(FIN - DEBUT, 1);
  STATUT = 'OK'; MESSAGE = "&step_msg.";
  format DEBUT FIN datetime19.;
run;
proc append base=dq.RUN_LOG data=work.log1 force; run;


/*======================================================================*/
/* SECTION 2 : RAPPORT EXCEL EDA ET DIAGNOSTIC */


%let t0 = %sysfunc(datetime());

/*---------------------------------------------------------------------------
  S0.6 PARAMETRES PROPRES AU RAPPORT
---------------------------------------------------------------------------*/
libname rap "&root./rapport";                              /* cree le dossier si absent */
%let rapport = &root./rapport/DQ_EDA_Diagnostic_&am..xlsx;
%let banner  = DONNEES DE TEST FICTIVES - arrete du 30/09/2026 - exemple de sortie SAS;

/* Seuils de feu des regles (PROPOSITION a faire valider avec la Validation des modeles) */
%let vert_cb   = 0.995;   /* regles Critique et Bloquant : vert si conformite >= */
%let orange_cb = 0.98;    /*                               orange si >=          */
%let vert_mm   = 0.99;    /* regles Majeur et Mineur                             */
%let orange_mm = 0.97;

/* Formats d affichage : espace pour les milliers, virgule decimale, pour le texte des constats */
proc format;
  picture frn  (round) low-<0 = '000 000 000 009' (prefix='-')  0-high = '000 000 000 009';
  picture frp  (round) low-<0 = '099,9 %' (prefix='-' mult=1000) 0-high = '099,9 %' (mult=1000);
  picture frp2 (round) low-<0 = '099,99 %' (prefix='-' mult=10000) 0-high = '099,99 %' (mult=10000);
  picture frm1 (round) low-<0 = '000 000 009,9' (prefix='-' mult=10) 0-high = '000 000 009,9' (mult=10);
  picture frm2 (round) low-<0 = '000 000 009,99' (prefix='-' mult=100) 0-high = '000 000 009,99' (mult=100);
  value pcna . = 'n/a' other = [percent8.1];
run;

/* Style Excel : en-tetes de tableau bleu petrole, police Arial */
proc template;
  define style styles.dqxl;
    parent = styles.excel;
    class header / background=cx0E5A78 color=white fontfamily=Arial fontsize=9pt fontweight=bold;
    class data   / fontfamily=Arial fontsize=9.5pt;
    class systemtitle / fontfamily=Arial fontsize=9pt fontweight=medium color=cx555555;
  end;
run;

/* Couleurs des feux, utilisees par tous les graphiques (ATTRMAP) */
data work.attr_feu;
  length ID $8 VALUE $8 FILLCOLOR $10;
  ID = 'feu';
  VALUE = 'Vert';   FILLCOLOR = 'cx1BAF7A'; output;
  VALUE = 'Orange'; FILLCOLOR = 'cxEDA100'; output;
  VALUE = 'Rouge';  FILLCOLOR = 'cxE34948'; output;
  VALUE = 'ND';     FILLCOLOR = 'cxA0A7B8'; output;
run;

/* Table de travail de l arrete M : un contrat par ligne, avec routage, drivers, production et HLC */
proc sql;
  create table work.cm as
  select c.*, d.ROUTE, d.PORTEFEUILLE, d.TID_ANNEES, d.EAD,
         h.ENCOURS_PROD, h.DECLAS_PROD, h.TID_PROD, s.MATCH_STATUS
  from stg.STG_CREDIT_SNAPSHOT as c
       inner join dq.DRV_CONTRAT as d on c.N_PERD = d.N_PERD and c.ID_CONTRAT = d.ID_CONTRAT
       left join stg.STG_PRODUCTION_HLC as h on c.N_PERD = h.N_PERD and c.ID_CONTRAT = h.ID_CONTRAT
       left join dq.HLC_SEGMENT_COMPARE as s on c.N_PERD = s.N_PERD and c.ID_CONTRAT = s.ID_CONTRAT
  where c.N_PERD = &am. and c.KEY_OCC = 1;
quit;

options nodate nonumber;
ods escapechar = '^';
ods graphics on / width=780px height=420px imagefmt=png;

/* Ouverture du classeur : un onglet par section, ODS EXCEL ecrit titres, textes, tables et graphiques */
ods excel file="&rapport." style=styles.dqxl
    options(sheet_interval="none" embedded_titles="yes" flow="tables" gridlines="off");

/*===========================================================================
  Lisez-moi et Parametres
===========================================================================*/
ods excel options(sheet_interval="now" sheet_name="Lisez-moi" absolute_column_width="26,46,30,40,52");
title1 j=l "^S={background=cx0A2F3F color=white fontsize=15pt fontweight=bold} Premier constat sur un arrete : exploration et diagnostic DQ";
title2 j=l "Sorties du programme DQ_06_PUBLICATION_REPORTING.sas section 2 (graphiques G1 a G20 produits par PROC SGPLOT)";
title3 j=l "^S={color=cxC00000 fontweight=bold} &banner.";
ods text = "^S={background=cxE1EFF5} Ce classeur est produit par SAS : un onglet par question, avec un encadre constat, les tables et les graphiques. Les chiffres viennent du run &run_id. (arrete &am. contre &am1.).";

data work.lisez;
  length ONGLET $26 QUESTION $90 SECTION $12 SOURCE $70 USAGE $70;
  infile datalines dlm='|' dsd truncover;
  input ONGLET $ QUESTION $ SECTION $ SOURCE $ USAGE $;
datalines;
Paramètres|Quels seuils et quelles ponderations ?|S0|macro-variables, ref.REF_SEUIL, ref.REF_CDE|Seuils de feu proposes, a faire valider
EDA_1_Volumétrie_grain|Combien de lignes, de contrats, de dossiers ? Le grain est-il respecte ?|R1|stg.STG_CREDIT_SNAPSHOT, dq.DQ_KEY_PROFILE|Verifier les doublons de cle avant tout taux en encours
EDA_2_Manquants|Quelles variables sont vides, et est-ce attendu ?|R2|dq.DQ_PROFILE_CDE|Trie du plus vide au plus renseigne
EDA_3_Numériques|Quelles distributions, quels negatifs, quels extremes ?|R3|PROC MEANS sur stg et dq.DRV_CONTRAT|Fixer les bornes plausibles
EDA_4_Modalités|Quelles valeurs prennent les variables categorielles ?|R4|stg, stg.STG_COLLATERAL_LINK|Faire valider les listes de codes
EDA_5_Dates|Quelles plages, quelles dates futures ?|R5|stg.STG_CREDIT_SNAPSHOT|Qualifier les dates posterieures a l arrete
EDA_6_Écarts_sources|Les sources redondantes concordent-elles ?|R6|stg + stg.STG_PRODUCTION_HLC|Seuils a recalibrer apres trois arretes
EDA_7_Croisements|Comment se repartissent comptes, portefeuilles et dossiers ?|R7|dq.DRV_CONTRAT|Prevoir un seuil de materialite en euros
EDA_8_Constats|Quelles decisions sortent de l exploration ?|R8|synthese EDA_1 a EDA_7|A partager avec les Data Owners
DQ_1_Résultats|Quel est le resultat de chacune des 27 regles ?|R9|dq.AGG_RULE_RESULT, ref.REF_REGLE|Les regles rouges se traitent en premier
DQ_2_Scores|Quel est le score de chaque CDE, famille, global ?|R10|dq.DQ_KPI_CDE, dq.DQ_KPI_RUN|Score = somme(poids x taux) / somme(poids)
DQ_3_Matrice|Ou se concentrent les non-conformites ?|R11|dq.AGG_CDE_SCORE|n/a : non applicable ou effectif < 30
DQ_4_Anomalies|Quels sont les plus gros contrats en anomalie ?|R12|dq.DQ_ISSUE|Liste pour les data stewards
DQ_5_Registre|Quelles regles qualifier, avec quelle piste ?|R13|dq.AGG_RULE_RESULT, dq.DQ_RCA_CANDIDATE|Completer proprietaire, action, echeance
HLC_1_Concordance|La HLC de production est-elle celle attendue ?|R14|dq.AGG_HLC_*, dq.HLC_TRANSITION|Les causes DATA se traitent a la source
ECL_1_Provision|Quelle provision, quel impact des anomalies ?|R15|dq.AGG_ECL, dq.AGG_IMPACT|Impact estime, calcule ou non quantifiable
ECL_2_Bridge|Qu est-ce qui explique la variation M-1 vers M ?|R16|dq.ECL_EFFECT_BRIDGE|Le residuel doit rester nul
DEC_1_Décisions|Quelles decisions bloquent la certification ?|R17|pub.PUB_DECISION|Non certifiable tant qu une bloquante est ouverte
;
run;
proc print data=work.lisez noobs label;
  label ONGLET='Onglet' QUESTION='Question posee' SECTION='Section' SOURCE='Table SAS source' USAGE='Comment l utiliser';
run;

ods excel options(sheet_interval="now" sheet_name="Paramètres" absolute_column_width="34,16,16,18,60");
title1 j=l "^S={background=cx0A2F3F color=white fontsize=15pt fontweight=bold} Parametres du diagnostic";
title2 j=l "S0 : macro-variables et ref.REF_SEUIL / ref.REF_CDE";
title3 j=l "^S={color=cxC00000 fontweight=bold} &banner.";
ods text = "^S={background=cxE1EFF5} Les seuils de feu des regles sont une proposition a faire valider avec la Validation des modeles. Les seuils des CDE et les poids viennent de REF_CDE ; les seuils metier viennent de REF_SEUIL.";
data work.p_feu;
  length FAMILLE $24;
  FAMILLE = 'Critique et Bloquant'; VERT = &vert_cb.; ORANGE = &orange_cb.; output;
  FAMILLE = 'Majeur et Mineur';     VERT = &vert_mm.; ORANGE = &orange_mm.; output;
  FAMILLE = 'CDE (tous)';           VERT = 0.99;      ORANGE = 0.97;        output;
  format VERT ORANGE percent8.1;
run;
title1 j=l "Seuils de feu des regles (proposition)";
proc print data=work.p_feu noobs; run;
title1 j=l "Seuils metier (ref.REF_SEUIL)";
proc print data=ref.REF_SEUIL noobs; run;
title1 j=l "Ponderations et seuils des CDE (ref.REF_CDE)";
proc print data=ref.REF_CDE noobs; var CDE_ID LIBELLE FAMILLE POIDS SEUIL_VERT SEUIL_ORANGE; format SEUIL_VERT SEUIL_ORANGE percent8.; run;
title1;

/*===========================================================================
  R1. EDA_1 - Volumetrie et grain
  Question : combien de lignes, de contrats, de dossiers ? Le grain est-il respecte ?
===========================================================================*/
proc sql noprint;
  select N_LIGNES, N_CLES_DISTINCTES, N_DOUBLONS into :e1_lignes trimmed, :e1_contrats trimmed, :e1_dbl trimmed
    from dq.DQ_KEY_PROFILE where N_PERD = &am. and PAQUET = 'CREDIT';
  select N_ORPHELINS into :e1_orph trimmed from dq.DQ_KEY_PROFILE where N_PERD = &am. and PAQUET = 'GARANTIES';
  select count(distinct DOSSIER_ID), sum(MTENGB) into :e1_doss trimmed, :e1_enc trimmed from work.cm;
quit;

data _null_;
  length k $2000;
  k = catx(' ', 'Constat : le paquet CREDIT compte', strip(put(&e1_lignes.,frn.)), 'lignes pour', strip(put(&e1_contrats.,frn.)), 'contrats distincts.',
           strip(put(&e1_dbl.,frn.)), 'cles contrat apparaissent plusieurs fois, alors que le grain attendu est un contrat par ligne et par arrete.',
           'Un dossier regroupe en moyenne', strip(put(&e1_contrats./&e1_doss.,frm2.)), 'contrat.',
           'Les liens de garantie comptent', strip(put(&e1_orph.,frn.)), 'orphelins (garantie sans contrat).',
           'Decision : identifier l origine des doublons avant de calculer des taux en encours ; ils sont portes par la regle DQ-003.');
  call symputx('k_e1', k);
  call symputx('n_dup', &e1_dbl.);
run;

ods excel options(sheet_interval="now" sheet_name="EDA_1_Volumétrie_grain" absolute_column_width="44,18,18,18,50");
title1 j=l "^S={background=cx0A2F3F color=white fontsize=15pt fontweight=bold} Exploration 1 : Volumetrie et grain";
title2 j=l "R1 : stg.STG_CREDIT_SNAPSHOT, dq.DQ_KEY_PROFILE";
title3 j=l "^S={color=cxC00000 fontweight=bold} &banner.";
ods text = "^S={background=cxE1EFF5} &k_e1.";

data work.e1_ind;
  length INDICATEUR $60 LECTURE $60;
  INDICATEUR = 'Lignes dans le paquet CREDIT';        VALEUR = &e1_lignes.;   LECTURE = 'Grain vise : un contrat par ligne'; output;
  INDICATEUR = 'Contrats distincts (ID_CONTRAT)';     VALEUR = &e1_contrats.; LECTURE = ''; output;
  INDICATEUR = 'Cles contrat en doublon';             VALEUR = &e1_dbl.;      LECTURE = 'Signalees par DUP_KEY et KEY_OCC, jamais supprimees'; output;
  INDICATEUR = 'Dossiers distincts (DOSSIER_ID)';     VALEUR = &e1_doss.;     LECTURE = 'CODSGDOS - DATCRE - NUM_DOSS'; output;
  INDICATEUR = 'Contrats par dossier (moyenne)';      VALEUR = &e1_contrats./&e1_doss.; LECTURE = ''; output;
  INDICATEUR = 'Encours bilan total (EUR)';           VALEUR = &e1_enc.;      LECTURE = 'Somme de MTENGB : denominateur des taux en encours'; output;
  format VALEUR comma18.2;
run;
title1 j=l "Volumetrie du paquet CREDIT, arrete &am.";
proc print data=work.e1_ind noobs; var INDICATEUR VALEUR LECTURE; run;

title1 j=l "Cles par paquet (dq.DQ_KEY_PROFILE)";
proc print data=dq.DQ_KEY_PROFILE noobs; where N_PERD = &am.; var PAQUET N_LIGNES N_CLES_DISTINCTES N_DOUBLONS N_ORPHELINS; format N_: comma12.; run;

proc sql;
  create table work.e1_d1 as select DOSSIER_ID, count(*) as N from work.cm where DOSSIER_ID ne '' group by DOSSIER_ID;
quit;
data work.e1_d2;
  set work.e1_d1;
  length CLASSE $12;
  if N = 1 then do; CLASSE = '1'; ORD = 1; end;
  else if N = 2 then do; CLASSE = '2'; ORD = 2; end;
  else if N = 3 then do; CLASSE = '3'; ORD = 3; end;
  else if N = 4 then do; CLASSE = '4'; ORD = 4; end;
  else if N <= 9 then do; CLASSE = '5 a 9'; ORD = 5; end;
  else do; CLASSE = '10 et plus'; ORD = 6; end;
run;
proc sql;
  create table work.e1_doss as
  select CLASSE, count(*) as NB_DOSSIERS, sum(N) as NB_CONTRATS, count(*) / (select count(*) from work.e1_d2) as PCT_DOSSIERS format=percent8.1
  from work.e1_d2 group by CLASSE, ORD order by ORD;
quit;
title1 j=l "Nombre de contrats par dossier";
proc print data=work.e1_doss noobs; format NB_DOSSIERS NB_CONTRATS comma12.; run;

proc sql;
  create table work.g1 as select PORTEFEUILLE, count(*) as N from work.cm group by PORTEFEUILLE;
quit;
title1 j=l "G1. Contrats en defaut par portefeuille (arrete &am.)";
proc sgplot data=work.g1;
  hbar PORTEFEUILLE / response=N categoryorder=respdesc datalabel fillattrs=(color=cx2A78D6);
  xaxis label="Contrats" grid; yaxis display=(nolabel);
run;
title1;

/*===========================================================================
  R2. EDA_2 - Valeurs manquantes par variable des 15 CDE
===========================================================================*/
data work.e2;
  set dq.DQ_PROFILE_CDE;
  where N_PERD = &am.;
  length STATUT $12 COMMENTAIRE $70;
  TAUX_REN = 1 - N_MANQUANT / N_LIGNES;
  if N_MANQUANT = 0 then STATUT = 'Complete';
  else if N_MANQUANT = N_LIGNES then STATUT = 'Vide';
  else if VARIABLE in ('SEG_DECL','M_ENCRS_HB','D_PR_EVT') then STATUT = 'Attendu';
  else STATUT = 'A qualifier';
  if VARIABLE = 'SEG_DECL' then COMMENTAIRE = 'Vide attendu hors comptes a vue';
  else if VARIABLE = 'M_ENCRS_HB' then COMMENTAIRE = 'Vide attendu hors produits a hors bilan ; source a confirmer (Q50)';
  else if VARIABLE = 'D_PR_EVT' then COMMENTAIRE = 'Vide attendu si aucun evenement precontentieux';
  LIBELLE = catx(' ', CDE_ID, VARIABLE);
run;
proc sort data=work.e2; by TAUX_REN CDE_ID; run;

proc sql noprint;
  select count(*) into :e2_n trimmed from work.e2;
  select count(*) into :e2_att trimmed from work.e2 where STATUT = 'Attendu';
  select count(*), min(N_MANQUANT), max(N_MANQUANT) into :e2_q trimmed, :e2_qmin trimmed, :e2_qmax trimmed from work.e2 where STATUT = 'A qualifier';
  select N_MANQUANT into :n_seg_vide trimmed from work.e2 where VARIABLE = 'SEG_DECL';
  select N_MANQUANT into :n_hb_vide trimmed from work.e2 where VARIABLE = 'M_ENCRS_HB';
quit;
data _null_;
  length k $2000;
  k = catx(' ', 'Constat :', strip(put(&e2_n.,frn.)), 'variables profilees sur les 15 CDE. Aucune n est entierement vide.',
           strip(put(&e2_att.,frn.)), 'variables ont un vide attendu', cats('(SEG_DECL ', strip(put(&n_seg_vide.,frn.)), ', M_ENCRS_HB ', strip(put(&n_hb_vide.,frn.)), ') ;'),
           strip(put(&e2_q.,frn.)), 'variables ont un vide a qualifier, de', strip(put(&e2_qmin.,frn.)), 'a', strip(put(&e2_qmax.,frn.)), 'lignes.',
           'Le tableau est trie du plus vide au plus renseigne. Decision : faire confirmer par le Data Owner les vides attendus avant de les exclure des regles.');
  call symputx('k_e2', k);
run;

ods excel options(sheet_interval="now" sheet_name="EDA_2_Manquants" absolute_column_width="10,12,18,8,12,14,14,14,14,58");
title1 j=l "^S={background=cx0A2F3F color=white fontsize=15pt fontweight=bold} Exploration 2 : Valeurs manquantes par variable";
title2 j=l "R2 : dq.DQ_PROFILE_CDE (une ligne par variable des 15 CDE)";
title3 j=l "^S={color=cxC00000 fontweight=bold} &banner.";
ods text = "^S={background=cxE1EFF5} &k_e2.";
proc report data=work.e2 nowd;
  columns CDE_ID TABLE_SOURCE VARIABLE TYPE_VAR N_LIGNES N_MANQUANT TAUX_REN N_DISTINCT STATUT COMMENTAIRE;
  define CDE_ID / 'Donnee (CDE)'; define TABLE_SOURCE / 'Paquet'; define VARIABLE / 'Variable'; define TYPE_VAR / 'Type';
  define N_LIGNES / 'Nb lignes' format=comma12.; define N_MANQUANT / 'Nb manquants' format=comma12.;
  define TAUX_REN / 'Taux renseigne' format=percent8.1; define N_DISTINCT / 'Valeurs distinctes' format=comma12.;
  define STATUT / 'Statut'; define COMMENTAIRE / 'Commentaire';
  compute STATUT;
    if STATUT = 'Vide' then call define(_col_, 'style', 'style=[background=cxF4CCCC]');
    else if STATUT = 'A qualifier' then call define(_col_, 'style', 'style=[background=cxFCE5CD]');
    else if STATUT = 'Attendu' then call define(_col_, 'style', 'style=[background=cxE1EFF5]');
    else call define(_col_, 'style', 'style=[background=cxD9EAD3]');
  endcomp;
run;
title1 j=l "G2. Taux de renseignement par variable des 15 CDE";
proc sgplot data=work.e2;
  hbar LIBELLE / response=TAUX_REN categoryorder=respasc fillattrs=(color=cx2A78D6);
  refline 0.995 / axis=x lineattrs=(color=cx1BAF7A pattern=dash) label="99,5 %";
  refline 0.98 / axis=x lineattrs=(color=cxEDA100 pattern=dash) label="98 %";
  xaxis min=0 max=1 grid label="Taux de renseignement" valuesformat=percent8.; yaxis display=(nolabel) fitpolicy=none;
run;
title1;

/*===========================================================================
  R3. EDA_3 - Distribution des variables numeriques
===========================================================================*/
proc means data=work.cm stackodsoutput n nmiss min p1 p5 median p95 p99 max mean sum noprint;
  var MTENGB M_ENCRS_HB DECLAS_B DECLAS_H TID_ANNEES EAD;
  ods output summary = work.e3_m;
run;
proc sql;
  create table work.e3_neg as
  select 'MTENGB' as Variable length=32, sum(MTENGB < 0) as N_NEG, min(MTENGB) as MINI from work.cm
  union all select 'M_ENCRS_HB', sum(M_ENCRS_HB < 0), min(M_ENCRS_HB) from work.cm
  union all select 'DECLAS_B', sum(DECLAS_B < 0), min(DECLAS_B) from work.cm
  union all select 'DECLAS_H', sum(DECLAS_H < 0), min(DECLAS_H) from work.cm
  union all select 'TID_ANNEES', sum(TID_ANNEES < 0), min(TID_ANNEES) from work.cm
  union all select 'EAD', sum(EAD < 0), min(EAD) from work.cm;
  create table work.e3 as
  select m.*, n.N_NEG from work.e3_m as m left join work.e3_neg as n on m.Variable = n.Variable;
  select N_NEG, Min into :n_neg trimmed, :mn_neg trimmed from work.e3 where Variable = 'MTENGB';
  select Max, Median into :mx_enc trimmed, :md_enc trimmed from work.e3 where Variable = 'MTENGB';
  select N_NEG into :n_neg_hb trimmed from work.e3 where Variable = 'M_ENCRS_HB';
  select Max into :mx_tid trimmed from work.e3 where Variable = 'TID_ANNEES';
quit;
data _null_;
  length k $2000;
  k = catx(' ', 'Constat : l encours bilan prend des valeurs negatives (minimum', strip(put(&mn_neg.,frn.)), 'EUR,', strip(put(&n_neg.,frn.)), 'contrats) et atteint', strip(put(&mx_enc.,frn.)),
           'EUR sur un contrat, alors que la mediane est de', strip(put(&md_enc.,frn.)), 'EUR. L encours hors bilan est negatif pour', strip(put(&n_neg_hb.,frn.)),
           'contrats. La duree en defaut va jusqu a', strip(put(&mx_tid.,frn.)), 'ans. Decision : conserver le brut, signaler les negatifs (regle DQ-013), et fixer avec le Recouvrement la borne haute plausible de la duree en defaut.');
  call symputx('k_e3', k);
run;
ods excel options(sheet_interval="now" sheet_name="EDA_3_Numériques" absolute_column_width="14,34,10,10,14,12,12,12,14,14,16,14,18,12");
title1 j=l "^S={background=cx0A2F3F color=white fontsize=15pt fontweight=bold} Exploration 3 : Distribution des variables numeriques";
title2 j=l "R3 : PROC MEANS (n, nmiss, min, p1, p5, mediane, p95, p99, max, moyenne, somme)";
title3 j=l "^S={color=cxC00000 fontweight=bold} &banner.";
ods text = "^S={background=cxE1EFF5} &k_e3.";
proc print data=work.e3 noobs;
  var Variable N NMiss Min P1 P5 Median P95 P99 Max Mean Sum N_NEG;
  format N NMiss N_NEG comma12. Min P1 P5 Median P95 P99 Max Mean Sum comma18.1;
run;
data work.g3;
  set work.cm;
  if MTENGB > 0 then LOG10_MTENGB = log10(MTENGB);
run;
title1 j=l "G3a. Distribution de l encours bilan (log10)";
proc sgplot data=work.g3;
  histogram LOG10_MTENGB / fillattrs=(color=cx1BAF7A) nbins=40;
  xaxis label="log10(MTENGB)"; yaxis label="Contrats" grid;
run;
title1 j=l "G3b. Encours bilan par portefeuille (log10)";
proc sgplot data=work.g3(where=(PORTEFEUILLE in ('CORP','IMMO','CONSO','REVOL','CI','CAV')));
  vbox LOG10_MTENGB / category=PORTEFEUILLE fillattrs=(color=cxA9C8EF);
  yaxis label="log10(MTENGB)" grid;
run;
title1;

/*===========================================================================
  R4. EDA_4 - Modalites des variables categorielles
===========================================================================*/
proc sql;
  create table work.e4 as
  select 'TYP_CPTE' as VARIABLE length=20, coalescec(TYP_CPTE,'(vide)') as MODALITE length=30, count(*) as N, count(*) / (select count(*) from work.cm) as PCT format=percent8.1, sum(MTENGB) as ENCOURS format=comma18.
    from work.cm group by calculated MODALITE
  union all
  select 'C_ENTITE', coalescec(C_ENTITE,'(vide)') as MODALITE, count(*), count(*) / (select count(*) from work.cm), sum(MTENGB) from work.cm group by calculated MODALITE
  union all
  select 'TYPRIS', coalescec(TYPRIS,'(vide)') as MODALITE, count(*), count(*) / (select count(*) from work.cm), sum(MTENGB) from work.cm group by calculated MODALITE
  union all
  select 'SEG_DECL', coalescec(SEG_DECL,'(vide)') as MODALITE, count(*), count(*) / (select count(*) from work.cm), sum(MTENGB) from work.cm group by calculated MODALITE
  union all
  select 'C_NOMNC_ENGGM', coalescec(C_NOMNC_ENGGM,'(vide)') as MODALITE, count(*), count(*) / (select count(*) from work.cm), sum(MTENGB) from work.cm group by calculated MODALITE
  union all
  select 'C_DEVISE', coalescec(C_DEVISE,'(vide)') as MODALITE, count(*), count(*) / (select count(*) from work.cm), sum(MTENGB) from work.cm group by calculated MODALITE;
quit;
proc sort data=work.e4; by VARIABLE descending N; run;

proc sql;
  create table work.e4_gar as
  select coalescec(g.C_NOMNC_GARNT,'(vide)') as CODE length=12, count(*) as N,
         case when g.C_NOMNC_GARNT = '' then 'Vide'
              when e.C_NOMNC_GARNT is not null then 'Oui' else 'Non' end as ELIGIBLE length=6
  from stg.STG_COLLATERAL_LINK as g left join ref.REF_GARANTIE_ELIG as e on g.C_NOMNC_GARNT = e.C_NOMNC_GARNT
  where g.N_PERD = &am.
  group by calculated CODE, calculated ELIGIBLE
  order by N desc;
  select sum(N) into :n_gar_nonelig trimmed from work.e4_gar where ELIGIBLE = 'Non';
  select sum(N) into :n_gar_vide trimmed from work.e4_gar where ELIGIBLE = 'Vide';
  select count(*) into :n_zz99 trimmed from work.cm where C_ENTITE = 'ZZ99';
  select sum(TYPRIS = '1'), sum(TYPRIS = '2'), sum(TYPRIS in ('0','3')), sum(C_DEVISE ne 'EUR') into :n_edx trimmed, :n_cer trimmed, :n_rk trimmed, :n_dev trimmed from work.cm;
quit;
data work.e4_gar2;                       /* codes hors liste et 10 codes eligibles les plus frequents */
  set work.e4_gar;
  if ELIGIBLE = 'Oui' then do; RG + 1; if RG > 10 then delete; end;
  drop RG;
run;
data _null_;
  length k $2000;
  k = catx(' ', 'Constat :', strip(put(&n_edx.,frn.)), 'contrats ont TYPRIS = 1 (EDX) et', strip(put(&n_cer.,frn.)), 'ont TYPRIS = 2 (CER) ;', strip(put(&n_rk.,frn.)),
           'contrats portent un code hors domaine (0 ou 3). L entite ZZ99 apparait sur', strip(put(&n_zz99.,frn.)), 'contrats, et', strip(put(&n_dev.,frn.)),
           'contrats ne sont pas en euro ou n ont pas de devise. Les codes de garantie sont listes en bas de l onglet :', strip(put(&n_gar_nonelig.,frn.)),
           'liens portent un code absent de la liste d eligibilite et', strip(put(&n_gar_vide.,frn.)), 'sont vides. Decision : faire valider la liste d eligibilite par le Data Owner (Q15).');
  call symputx('k_e4', k);
run;
ods excel options(sheet_interval="now" sheet_name="EDA_4_Modalités" absolute_column_width="22,28,14,14,18,40");
title1 j=l "^S={background=cx0A2F3F color=white fontsize=15pt fontweight=bold} Exploration 4 : Modalites des variables categorielles";
title2 j=l "R4 : PROC SQL (nombre et encours bilan) ; liens de garantie en bas";
title3 j=l "^S={color=cxC00000 fontweight=bold} &banner.";
ods text = "^S={background=cxE1EFF5} &k_e4.";
proc print data=work.e4 noobs; format N comma12.; run;
title1 j=l "Codes de garantie : codes hors liste et 10 codes les plus frequents (stg.STG_COLLATERAL_LINK)";
proc report data=work.e4_gar2 nowd;
  columns CODE N ELIGIBLE;
  define CODE / 'Code'; define N / 'Nb liens' format=comma12.; define ELIGIBLE / 'Eligible (REF_GARANTIE_ELIG)';
  compute ELIGIBLE;
    if ELIGIBLE = 'Oui' then call define(_col_, 'style', 'style=[background=cxD9EAD3]');
    else call define(_col_, 'style', 'style=[background=cxF4CCCC]');
  endcomp;
run;
proc sql; create table work.g4 as select coalescec(TYP_CPTE,'(vide)') as TYP_CPTE, sum(MTENGB)/1e6 as ENC_M from work.cm group by calculated TYP_CPTE; quit;
title1 j=l "G4. Encours bilan par type de compte (M EUR)";
proc sgplot data=work.g4;
  hbar TYP_CPTE / response=ENC_M categoryorder=respdesc fillattrs=(color=cxEB6834);
  xaxis label="M EUR" grid; yaxis display=(nolabel);
run;
title1;

/*===========================================================================
  R5. EDA_5 - Dates : plages, sentinelles, dates futures
===========================================================================*/
proc sql;
  create table work.e5 as
  select 'DATCRE' as VARIABLE length=12, 'Creation du dossier' as LIBELLE length=30, sum(missing(DATCRE)) as N_MANQUANT, min(DATCRE) as MINI format=yymmdd10., max(DATCRE) as MAXI format=yymmdd10.,
         sum(DATCRE ne . and DATCRE < '01JAN1950'd) as N_SENTINELLE, sum(DATCRE > &asof_m.) as N_FUTUR from work.cm
  union all
  select 'DATDECLA', 'Declassement', sum(missing(DATDECLA)), min(DATDECLA), max(DATDECLA), sum(DATDECLA ne . and DATDECLA < '01JAN1950'd), sum(DATDECLA > &asof_m.) from work.cm
  union all
  select 'D_PR_EVT', 'Premier evenement', sum(missing(D_PR_EVT)), min(D_PR_EVT), max(D_PR_EVT), sum(D_PR_EVT ne . and D_PR_EVT < '01JAN1950'd), sum(D_PR_EVT > &asof_m.) from work.cm;
  select N_FUTUR into :n_futur_def trimmed from work.e5 where VARIABLE = 'DATDECLA';
  select N_FUTUR into :n_futur_cre trimmed from work.e5 where VARIABLE = 'DATCRE';
  select N_MANQUANT into :n_evt_vide trimmed from work.e5 where VARIABLE = 'D_PR_EVT';
  select sum(N_SENTINELLE) into :n_sent trimmed from work.e5;
  create table work.e5_an as select year(DATDECLA) as ANNEE, count(*) as N from work.cm where DATDECLA ne . group by calculated ANNEE order by ANNEE;
quit;
data _null_;
  length k $2000;
  k = catx(' ', 'Constat :', ifc(&n_sent. = 0, 'aucune valeur sentinelle (avant 1950) dans les trois dates.', catx(' ', strip(put(&n_sent.,frn.)), 'valeurs sentinelles (avant 1950).')),
           strip(put(&n_futur_def.,frn.)), 'dates de declassement et', strip(put(&n_futur_cre.,frn.)), 'dates de creation de dossier sont posterieures a l arrete du 30/09/2026.',
           strip(put(&n_evt_vide.,frn.)), 'contrats n ont pas de date de premier evenement (vide attendu pour une partie).',
           'Ces dates sont a rapprocher des regles DQ-016 (duree en defaut) et DQ-024 (variations atypiques). Decision : demander au Data Owner si une date future est un decalage de calendrier ou une erreur de saisie.');
  call symputx('k_e5', k);
run;
ods excel options(sheet_interval="now" sheet_name="EDA_5_Dates" absolute_column_width="16,30,14,14,14,16,16");
title1 j=l "^S={background=cx0A2F3F color=white fontsize=15pt fontweight=bold} Exploration 5 : Dates, sentinelles, dates futures";
title2 j=l "R5 : stg.STG_CREDIT_SNAPSHOT";
title3 j=l "^S={color=cxC00000 fontweight=bold} &banner.";
ods text = "^S={background=cxE1EFF5} &k_e5.";
proc print data=work.e5 noobs; format N_: comma12.; run;
title1 j=l "Contrats par annee de declassement";
proc print data=work.e5_an noobs; format N comma12.; run;
title1 j=l "G5. Contrats par annee de declassement";
proc sgplot data=work.e5_an;
  vbar ANNEE / response=N fillattrs=(color=cx2A78D6);
  xaxis label="Annee de DATDECLA"; yaxis label="Contrats" grid;
run;
title1;

/*===========================================================================
  R6. EDA_6 - Ecarts entre sources redondantes
===========================================================================*/
proc sql;
  create table work.e6 as
  select 'Encours bilan : MTENGB contre ENCOURS_PROD (production)' as COMPARAISON length=70,
         count(*) as N_COMPARES, sum(abs(MTENGB - ENCOURS_PROD) / abs(ENCOURS_PROD) > 0.01) as N_ECART, 'Ecart relatif > 1 %' as CRITERE length=30
    from work.cm where MTENGB ne . and ENCOURS_PROD not in (., 0)
  union all
  select 'Declassement bilan : DECLAS_B contre DECLAS_PROD', count(*), sum(abs(DECLAS_B - DECLAS_PROD) / abs(DECLAS_PROD) > 0.01), 'Ecart relatif > 1 %'
    from work.cm where DECLAS_B ne . and DECLAS_PROD not in (., 0)
  union all
  select 'Duree en defaut : TID recalcule contre TID_PROD', count(*), sum(TID_PROD ne TID_ANNEES), 'Annees differentes' from work.cm where TID_PROD ne .
  union all
  select 'HLC attendue contre HLC de production (portefeuilles statistiques)', count(*), sum(MATCH_STATUS = 'MISMATCH'), 'HLC differente'
    from work.cm where MATCH_STATUS in ('MATCH','MISMATCH');
  select N_COMPARES, N_ECART into :e6_n1 trimmed, :e6_e1 trimmed from work.e6 where COMPARAISON like 'Encours%';
  select N_ECART into :e6_e2 trimmed from work.e6 where COMPARAISON like 'Declassement%';
  select N_ECART into :e6_e3 trimmed from work.e6 where COMPARAISON like 'Duree%';
  select N_ECART into :e6_e4 trimmed from work.e6 where COMPARAISON like 'HLC%';
quit;
%let n_ecart_enc = &e6_e1.;
data _null_;
  length k $2000;
  k = catx(' ', 'Constat :', strip(put(&e6_e1.,frn.)), 'contrats', cats('(', strip(put(&e6_e1./&e6_n1.,frp.)), ')'), 'ont un encours extrait different de plus de 1 % de l encours de production, ce qui peut alimenter la regle DQ-013.',
           'Le declassement bilan differe pour', strip(put(&e6_e2.,frn.)), 'contrats, et la duree en defaut pour', strip(put(&e6_e3.,frn.)), 'contrats.',
           'La HLC de production differe de la HLC attendue pour', strip(put(&e6_e4.,frn.)), 'contrats (voir HLC_1). Decision : ces seuils sont retenus tels quels pour le diagnostic ; ils seront recalibres avec les Data Owners apres trois arretes.');
  call symputx('k_e6', k);
run;
ods excel options(sheet_interval="now" sheet_name="EDA_6_Écarts_sources" absolute_column_width="58,16,16,28,12");
title1 j=l "^S={background=cx0A2F3F color=white fontsize=15pt fontweight=bold} Exploration 6 : Ecarts entre sources redondantes";
title2 j=l "R6 : stg.STG_CREDIT_SNAPSHOT joint a stg.STG_PRODUCTION_HLC et dq.DRV_CONTRAT";
title3 j=l "^S={color=cxC00000 fontweight=bold} &banner.";
ods text = "^S={background=cxE1EFF5} &k_e6.";
data work.e6b; set work.e6; PART_ECART = N_ECART / N_COMPARES; format PART_ECART percent8.2 N_COMPARES N_ECART comma12.; run;
proc print data=work.e6b noobs; var COMPARAISON N_COMPARES N_ECART PART_ECART CRITERE; run;
data work.g6; set work.cm; where MTENGB > 0 and ENCOURS_PROD > 0; if ranuni(12345) < 0.2; run;
title1 j=l "G6. Encours bilan : extraction contre production (echantillon de 20 %)";
proc sgplot data=work.g6;
  scatter x=ENCOURS_PROD y=MTENGB / markerattrs=(symbol=circlefilled size=4 color=cx1BAF7A) transparency=0.6;
  lineparm x=1 y=1 slope=1 / lineattrs=(color=cxE34948 pattern=dash);
  xaxis type=log label="ENCOURS_PROD (production)" grid; yaxis type=log label="MTENGB (extraction)" grid;
run;
title1;

/*===========================================================================
  R7. EDA_7 - Croisements et concentration
===========================================================================*/
proc sql;
  create table work.e7_dos as
  select DOSSIER_ID, count(*) as N_CTR, sum(MTENGB) as ENC from work.cm where DOSSIER_ID ne '' group by DOSSIER_ID order by calculated ENC desc;
  select sum(ENC) into :e7_tot trimmed from work.e7_dos;
quit;
data work.e7_top work.e7_par2;            /* courbe de Pareto : part cumulee de l encours */
  set work.e7_dos nobs=nb;
  retain CUM 0 P80;
  CUM + max(ENC, 0);
  RANG_PCT = _n_ / nb;
  CUM_PCT = CUM / &e7_tot.;
  if _n_ <= 20 then do; RANG = _n_; PCT = ENC / &e7_tot.; output work.e7_top; end;
  if CUM_PCT >= 0.8 and missing(P80) then do; P80 = RANG_PCT; call symputx('p80', P80); end;
  output work.e7_par2;
  keep DOSSIER_ID N_CTR ENC RANG RANG_PCT CUM_PCT PCT P80;
run;
data work.e7_top; set work.e7_top; CUM20 = sum(CUM20, PCT); retain CUM20; run;
proc sql noprint; select max(CUM20) into :cum20 trimmed from work.e7_top; quit;
data _null_;
  length k $2000;
  k = catx(' ', 'Constat : les 20 premiers dossiers pesent', strip(put(&cum20.,frp.)), 'de l encours bilan, et', strip(put(&p80.,frp.)), 'des dossiers suffisent a porter 80 % de l encours.',
           'Un taux en encours peut donc basculer a cause d un seul de ces dossiers. Decision : prevoir un seuil de materialite en euros pour qualifier les anomalies.');
  call symputx('k_e7', k);
run;
ods excel options(sheet_interval="now" sheet_name="EDA_7_Croisements" absolute_column_width="16,12,12,12,12,12,12,12,16");
title1 j=l "^S={background=cx0A2F3F color=white fontsize=15pt fontweight=bold} Exploration 7 : Croisements et concentration";
title2 j=l "R7 : dq.DRV_CONTRAT (type de compte x portefeuille) et top dossiers";
title3 j=l "^S={color=cxC00000 fontweight=bold} &banner.";
ods text = "^S={background=cxE1EFF5} &k_e7.";
title1 j=l "Type de compte x portefeuille (nombre de contrats)";
proc report data=work.cm nowd;
  columns TYP_CPTE PORTEFEUILLE, N;
  define TYP_CPTE / group 'Type de compte';
  define PORTEFEUILLE / across ' ';
run;
title1 j=l "Vingt plus gros dossiers par encours bilan";
proc print data=work.e7_top noobs; var RANG DOSSIER_ID N_CTR ENC PCT CUM20; format ENC comma18. PCT CUM20 percent8.2; label CUM20='Cumule'; run;
title1 j=l "G7. Concentration de l encours par dossier (Pareto)";
proc sgplot data=work.e7_par2;
  series x=RANG_PCT y=CUM_PCT / lineattrs=(color=cx2A78D6 thickness=2);
  refline 0.8 / axis=y lineattrs=(pattern=dash color=gray) label="80 % de l encours";
  xaxis label="Part des dossiers (tries par encours decroissant)" valuesformat=percent8. grid;
  yaxis label="Part cumulee de l encours" valuesformat=percent8. grid;
run;
title1;

/*===========================================================================
  R8. EDA_8 - Constats et decisions (synthese des onglets EDA_1 a EDA_7)
===========================================================================*/
proc sql noprint;
  select OWNER into :o_003 trimmed from ref.REF_REGLE where RULE_ID = 'DQ-003';
  select OWNER into :o_014 trimmed from ref.REF_REGLE where RULE_ID = 'DQ-014';
  select OWNER into :o_010 trimmed from ref.REF_REGLE where RULE_ID = 'DQ-010';
  select OWNER into :o_013 trimmed from ref.REF_REGLE where RULE_ID = 'DQ-013';
  select OWNER into :o_016 trimmed from ref.REF_REGLE where RULE_ID = 'DQ-016';
  select OWNER into :o_018 trimmed from ref.REF_REGLE where RULE_ID = 'DQ-018';
  select OWNER into :o_006 trimmed from ref.REF_REGLE where RULE_ID = 'DQ-006';
quit;
data work.e8;
  length CONSTAT $120 PREUVE $8 REGLE $8 DECISION $70 PROPRIETAIRE $30;
  N = 0;
  CONSTAT = catx(' ', strip(put(&n_dup.,frn.)), 'cles contrat en doublon dans le paquet CREDIT');
    PREUVE='EDA_1'; REGLE='DQ-003'; PRIORITE=1; DECISION='Identifier l origine des doublons (jointure source)'; PROPRIETAIRE="&o_003."; ECHEANCE='30NOV2026'd; N+1; output;
  CONSTAT = catx(' ', 'Exposition hors bilan vide pour', strip(put(&n_hb_vide.,frn.)), 'contrats ; source a confirmer');
    PREUVE='EDA_2'; REGLE='DQ-014'; PRIORITE=1; DECISION='Confirmer la colonne source du hors bilan (Q50)'; PROPRIETAIRE="&o_014."; ECHEANCE='30NOV2026'd; N+1; output;
  CONSTAT = catx(' ', 'Segmentation CAV vide pour', strip(put(&n_seg_vide.,frn.)), 'contrats');
    PREUVE='EDA_2'; REGLE='DQ-010'; PRIORITE=2; DECISION='Confirmer que le vide est attendu hors comptes a vue'; PROPRIETAIRE="&o_010."; ECHEANCE='15DEC2026'd; N+1; output;
  CONSTAT = catx(' ', 'Encours bilan negatif pour', strip(put(&n_neg.,frn.)), 'contrats (minimum', strip(put(&mn_neg.,frn.)), 'EUR)');
    PREUVE='EDA_3'; REGLE='DQ-013'; PRIORITE=2; DECISION='Garder le brut, signaler, qualifier avec le Data Owner'; PROPRIETAIRE="&o_013."; ECHEANCE='15DEC2026'd; N+1; output;
  CONSTAT = catx(' ', strip(put(&n_futur_def.,frn.)), 'dates de declassement posterieures a l arrete');
    PREUVE='EDA_5'; REGLE='DQ-016'; PRIORITE=1; DECISION='Dire si c est un decalage de calendrier ou une erreur'; PROPRIETAIRE="&o_016."; ECHEANCE='30NOV2026'd; N+1; output;
  CONSTAT = catx(' ', strip(put(&n_gar_nonelig.,frn.)), 'liens de garantie avec un code hors liste d eligibilite');
    PREUVE='EDA_4'; REGLE='DQ-018'; PRIORITE=1; DECISION='Valider la liste d eligibilite (Q15)'; PROPRIETAIRE="&o_018."; ECHEANCE='30NOV2026'd; N+1; output;
  CONSTAT = catx(' ', 'Entite ZZ99 sur', strip(put(&n_zz99.,frn.)), 'contrats');
    PREUVE='EDA_4'; REGLE='DQ-006'; PRIORITE=2; DECISION='Ajouter ou rejeter l entite dans le referentiel'; PROPRIETAIRE="&o_006."; ECHEANCE='15DEC2026'd; N+1; output;
  CONSTAT = catx(' ', strip(put(&n_ecart_enc.,frn.)), 'contrats avec un encours extrait eloigne de plus de 1 % de la production');
    PREUVE='EDA_6'; REGLE='DQ-013'; PRIORITE=2; DECISION='Recalibrer le seuil apres trois arretes'; PROPRIETAIRE="&o_013."; ECHEANCE='31JAN2027'd; N+1; output;
  CONSTAT = catx(' ', 'Vingt premiers dossiers =', strip(put(&cum20.,frp.)), 'de l encours bilan');
    PREUVE='EDA_7'; REGLE='-'; PRIORITE=3; DECISION='Definir un seuil de materialite en euros'; PROPRIETAIRE='Data Owner Credit'; ECHEANCE='31JAN2027'd; N+1; output;
  format ECHEANCE yymmdd10.;
run;
ods excel options(sheet_interval="now" sheet_name="EDA_8_Constats" absolute_column_width="6,60,18,16,10,52,22,14");
title1 j=l "^S={background=cx0A2F3F color=white fontsize=15pt fontweight=bold} Exploration 8 : Constats et decisions";
title2 j=l "R8 : synthese des onglets EDA_1 a EDA_7, a partager avec les Data Owners avant le diagnostic";
title3 j=l "^S={color=cxC00000 fontweight=bold} &banner.";
ods text = "^S={background=cxE1EFF5} Cette liste transforme l exploration en decisions. Chaque constat renvoie a sa preuve (onglet), a la regle concernee et a la decision attendue. Les priorites 1 bloquent le calcul d une regle ou d une donnee. Proprietaires et echeances viennent du catalogue de regles.";
proc print data=work.e8 noobs label;
  var N CONSTAT PREUVE REGLE PRIORITE DECISION PROPRIETAIRE ECHEANCE;
  label N='N' CONSTAT='Constat' PREUVE='Preuve' REGLE='Regle touchee' PRIORITE='Priorite' DECISION='Decision attendue' PROPRIETAIRE='Proprietaire' ECHEANCE='Echeance';
run;
title1;

/*===========================================================================
  R9. DQ_1 - Resultat de chaque regle (27 regles, arrete M)
  Feu : conformite = 1 - echecs / testes ; seuils differents selon la severite.
===========================================================================*/
proc sql;
  create table work.d1 as
  select a.RULE_ID, r.LIBELLE, r.CDE_ID, r.SEVERITE, r.CAUSE_RACINE, r.OWNER,
         sum(a.N_TESTE) as N_TESTE, sum(a.N_NON_APPLICABLE) as N_NA, sum(a.N_INDETERMINE) as N_IND,
         sum(a.N_ECHEC) as N_ECHEC, sum(a.EAD_ECHEC) as EAD_ECHEC
  from dq.AGG_RULE_RESULT as a left join ref.REF_REGLE as r on a.RULE_ID = r.RULE_ID
  where a.N_PERD = &am.
  group by a.RULE_ID, r.LIBELLE, r.CDE_ID, r.SEVERITE, r.CAUSE_RACINE, r.OWNER
  order by a.RULE_ID;
quit;
data work.d1;
  set work.d1;
  length FEU $8;
  if N_TESTE > 0 then CONF = 1 - N_ECHEC / N_TESTE; else CONF = .;
  if N_TESTE = 0 then FEU = 'ND';
  else if SEVERITE in ('Critique','Bloquant') then FEU = ifc(CONF >= &vert_cb., 'Vert', ifc(CONF >= &orange_cb., 'Orange', 'Rouge'));
  else FEU = ifc(CONF >= &vert_mm., 'Vert', ifc(CONF >= &orange_mm., 'Orange', 'Rouge'));
  format CONF percent8.2 N_TESTE N_NA N_IND N_ECHEC comma12. EAD_ECHEC comma18.;
run;
proc sql noprint;
  select count(*) into :d1_calc trimmed from work.d1 where FEU ne 'ND';
  select sum(FEU='Vert'), sum(FEU='Orange'), sum(FEU='Rouge'), sum(FEU='ND') into :d1_v trimmed, :d1_o trimmed, :d1_r trimmed, :d1_nd trimmed from work.d1;
  select RULE_ID, 1 - CONF into :d1_wr trimmed, :d1_wt trimmed from work.d1 where FEU ne 'ND' having CONF = min(CONF);
  select RULE_ID, N_ECHEC into :d1_vr trimmed, :d1_vn trimmed from work.d1 having N_ECHEC = max(N_ECHEC);
  select RULE_ID into :d1_rouges separated by ', ' from work.d1 where FEU = 'Rouge';
quit;
data _null_;
  length k $2000;
  k = catx(' ', 'Constat :', strip(put(&d1_calc.,frn.)), 'regles sont calculees sur 27, dont', strip(put(&d1_v.,frn.)), 'au vert,', strip(put(&d1_o.,frn.)), 'a l orange et', strip(put(&d1_r.,frn.)),
           cats('au rouge (', "&d1_rouges.", ').'), strip(put(&d1_nd.,frn.)), 'regles restent non testees (DQ-023 et DQ-027 : aucune source fournie).',
           'La regle la plus en echec en taux est', "&d1_wr.", cats('(', strip(put(&d1_wt.,frp.)), ' d echec) ;'), 'la plus en echec en volume est', "&d1_vr.", cats('(', strip(put(&d1_vn.,frn.)), ' contrats).'),
           'Les taux sont calcules par SAS a partir des colonnes n_teste et n_echec.');
  call symputx('k_d1', k);
run;
ods excel options(sheet_interval="now" sheet_name="DQ_1_Résultats" absolute_column_width="10,38,9,11,13,13,13,13,13,13,14,16");
title1 j=l "^S={background=cx0A2F3F color=white fontsize=15pt fontweight=bold} Diagnostic 1 : Resultat de chaque regle";
title2 j=l "R9 : dq.AGG_RULE_RESULT cumule sur les portefeuilles, joint a ref.REF_REGLE";
title3 j=l "^S={color=cxC00000 fontweight=bold} &banner.";
ods text = "^S={background=cxE1EFF5} &k_d1.";
proc report data=work.d1 nowd;
  columns RULE_ID LIBELLE CDE_ID SEVERITE N_TESTE N_NA N_IND N_ECHEC CONF EAD_ECHEC FEU CAUSE_RACINE;
  define RULE_ID / 'Regle'; define LIBELLE / 'Libelle'; define CDE_ID / 'CDE'; define SEVERITE / 'Severite';
  define N_TESTE / 'Contrats testes'; define N_NA / 'Non applicable'; define N_IND / 'Indetermines'; define N_ECHEC / 'Contrats en echec';
  define CONF / 'Taux de conformite'; define EAD_ECHEC / 'Exposition en echec (EUR)'; define FEU / 'Feu'; define CAUSE_RACINE / 'Cause racine (catalogue)';
  compute FEU;
    if FEU = 'Vert' then call define(_col_, 'style', 'style=[background=cxD9EAD3]');
    else if FEU = 'Orange' then call define(_col_, 'style', 'style=[background=cxFCE5CD]');
    else if FEU = 'Rouge' then call define(_col_, 'style', 'style=[background=cxF4CCCC]');
    else call define(_col_, 'style', 'style=[background=cxE7E6E6]');
  endcomp;
run;
proc sql;
  create table work.g8a as select FEU, count(*) as N from work.d1 group by FEU;
quit;
title1 j=l "G8a. Regles par feu";
proc sgplot data=work.g8a dattrmap=work.attr_feu;
  vbar FEU / response=N group=FEU attrid=feu datalabel;
  xaxis discreteorder=data display=(nolabel); yaxis label="Regles" grid;
run;
title1 j=l "G8b. Taux de conformite par regle (%)";
proc sort data=work.d1 out=work.g8b; by CONF; run;
proc sgplot data=work.g8b(where=(FEU ne 'ND')) dattrmap=work.attr_feu;
  hbar RULE_ID / response=CONF group=FEU attrid=feu categoryorder=respasc;
  xaxis min=0.9 max=1 grid valuesformat=percent8.; yaxis display=(nolabel);
run;
proc sql;
  create table work.g9 as
  select RULE_ID, PORTEFEUILLE, 100 * sum(N_ECHEC) / sum(N_TESTE) as TX format=4.1
  from dq.AGG_RULE_RESULT
  where N_PERD = &am. and PORTEFEUILLE in ('CORP','IMMO','CONSO','REVOL','CI','CAV','PGE')
  group by RULE_ID, PORTEFEUILLE having sum(N_TESTE) > 0;
quit;
title1 j=l "G9. Taux d echec (%) par regle et portefeuille";
proc sgplot data=work.g9;
  heatmapparm x=PORTEFEUILLE y=RULE_ID colorresponse=TX / colormodel=(cxDBE8F8 cx6FA2E3 cx143A6B);
  text x=PORTEFEUILLE y=RULE_ID text=TX / textattrs=(size=7) strip;
  xaxis display=(nolabel); yaxis display=(nolabel) reverse; gradlegend / title="% d echec";
run;
title1;

/*===========================================================================
  R10. DQ_2 - Scores par donnee (CDE), par famille et global
  Score = somme(poids x taux) / somme(poids). Une donnee non notee est ND, jamais 100 %.
===========================================================================*/
proc sql;
  create table work.d2 as
  select c.CDE_ID, c.LIBELLE, c.FAMILLE, c.POIDS, c.SEUIL_VERT, c.SEUIL_ORANGE,
         m.N_APPLICABLE, m.N_CONFORME, p.TAUX_CONFORMITE as TAUX_M1, m.TAUX_CONFORMITE as TAUX_M,
         (m.TAUX_CONFORMITE - p.TAUX_CONFORMITE) * 100 as ECART_PT
  from ref.REF_CDE as c
       left join dq.DQ_KPI_CDE as m on c.CDE_ID = m.CDE_ID and m.N_PERD = &am.
       left join dq.DQ_KPI_CDE as p on c.CDE_ID = p.CDE_ID and p.N_PERD = &am1.
  order by c.CDE_ID;
quit;
data work.d2;
  set work.d2;
  length FEU $8;
  if TAUX_M = . then FEU = 'ND';
  else if TAUX_M >= SEUIL_VERT then FEU = 'Vert';
  else if TAUX_M >= SEUIL_ORANGE then FEU = 'Orange';
  else FEU = 'Rouge';
  format TAUX_M1 TAUX_M percent8.2 ECART_PT 8.2 N_APPLICABLE N_CONFORME comma12.;
run;
proc sql;
  create table work.d2_fam as
  select FAMILLE, sum(POIDS * TAUX_M) / sum(POIDS) as SCORE_M format=percent8.2 from work.d2 where TAUX_M ne . group by FAMILLE order by FAMILLE;
  select SCORE_DQ into :sc_m trimmed from dq.DQ_KPI_RUN where N_PERD = &am.;
  select SCORE_DQ into :sc_m1 trimmed from dq.DQ_KPI_RUN where N_PERD = &am1.;
  select sum(FEU='Vert'), sum(FEU='Orange'), sum(FEU='Rouge') into :c_v trimmed, :c_o trimmed, :c_r trimmed from work.d2;
  select LIBELLE, TAUX_M into :w1 trimmed, :w1t trimmed from work.d2 having TAUX_M = min(TAUX_M);
  select LIBELLE, ECART_PT into :dn trimmed, :dnv trimmed from work.d2 having ECART_PT = min(ECART_PT);
  select LIBELLE, ECART_PT into :up trimmed, :upv trimmed from work.d2 having ECART_PT = max(ECART_PT);
  select FAMILLE, SCORE_M into :ff trimmed, :ffv trimmed from work.d2_fam having SCORE_M = min(SCORE_M);
quit;
data _null_;
  length k $2000;
  k = catx(' ', 'Constat : le score DQ pondere est de', strip(put(&sc_m.,frp2.)), 'a M, contre', strip(put(&sc_m1.,frp2.)), 'a M-1.', strip(put(&c_v.,frn.)), 'CDE sont au vert,', strip(put(&c_o.,frn.)),
           'a l orange et', strip(put(&c_r.,frn.)), 'au rouge. Le plus faible est', "&w1.", cats('(', strip(put(&w1t.,frp.)), ').'), 'La plus forte baisse est celle de', "&dn.", cats('(', strip(put(&dnv.,frm2.)), ' point)'),
           'et la plus forte hausse celle de', "&up.", cats('(+', strip(put(&upv.,frm2.)), ' point).'), 'La famille la plus faible est', "&ff.", cats('(', strip(put(&ffv.,frp.)), ').'),
           'Les seuils 99 % et 97 % sont une proposition a calibrer.');
  call symputx('k_d2', k);
run;
ods excel options(sheet_interval="now" sheet_name="DQ_2_Scores" absolute_column_width="10,36,16,8,14,14,14,14,12,10");
title1 j=l "^S={background=cx0A2F3F color=white fontsize=15pt fontweight=bold} Diagnostic 2 : Scores par donnee, par famille et global";
title2 j=l "R10 : dq.DQ_KPI_CDE, dq.DQ_KPI_RUN - score = somme(poids x taux) / somme(poids)";
title3 j=l "^S={color=cxC00000 fontweight=bold} &banner.";
ods text = "^S={background=cxE1EFF5} &k_d2.";
proc report data=work.d2 nowd;
  columns CDE_ID LIBELLE FAMILLE POIDS N_APPLICABLE N_CONFORME TAUX_M1 TAUX_M ECART_PT FEU;
  define CDE_ID / 'CDE'; define LIBELLE / 'Libelle'; define FAMILLE / 'Famille'; define POIDS / 'Poids'; define N_APPLICABLE / 'Applicables'; define N_CONFORME / 'Conformes';
  define TAUX_M1 / 'Taux M-1'; define TAUX_M / 'Taux M'; define ECART_PT / 'Ecart (pt)'; define FEU / 'Feu';
  compute FEU;
    if FEU = 'Vert' then call define(_col_, 'style', 'style=[background=cxD9EAD3]');
    else if FEU = 'Orange' then call define(_col_, 'style', 'style=[background=cxFCE5CD]');
    else if FEU = 'Rouge' then call define(_col_, 'style', 'style=[background=cxF4CCCC]');
    else call define(_col_, 'style', 'style=[background=cxE7E6E6]');
  endcomp;
run;
title1 j=l "Score par famille";
proc print data=work.d2_fam noobs; run;
title1 j=l "Score global du run (dq.DQ_KPI_RUN)";
proc print data=dq.DQ_KPI_RUN noobs; format SCORE_DQ percent8.2; run;
data work.g10;
  set work.d2;
  length PERIODE $12 LIB $40;
  LIB = catx(' ', CDE_ID, substr(LIBELLE, 1, 28));
  PERIODE = "M-1 (&am1.)"; TAUX = TAUX_M1; output;
  PERIODE = "M (&am.)";     TAUX = TAUX_M;  output;
  keep LIB PERIODE TAUX;
run;
title1 j=l "G10. Conformite des 15 CDE, M-1 contre M";
proc sgplot data=work.g10;
  hbar LIB / response=TAUX group=PERIODE groupdisplay=cluster;
  refline 0.99 / axis=x lineattrs=(color=cx1BAF7A pattern=dash);
  refline 0.97 / axis=x lineattrs=(color=cxEDA100 pattern=dash);
  xaxis min=0.93 max=1 grid valuesformat=percent8.; yaxis display=(nolabel) reverse;
run;
title1;

/*===========================================================================
  R11. DQ_3 - Matrice CDE x portefeuille (effectif minimum 30 contrats)
===========================================================================*/
proc sql;
  create table work.d3 as
  select CDE_ID, PORTEFEUILLE, sum(N_APPLICABLE) as N_APP, sum(N_CONFORME) as N_CONF,
         case when sum(N_APPLICABLE) >= 30 then sum(N_CONFORME) / sum(N_APPLICABLE) else . end as TAUX
  from dq.AGG_CDE_SCORE
  where N_PERD = &am. and PORTEFEUILLE in ('CORP','IMMO','CONSO','REVOL','CI','CAV','PGE')
  group by CDE_ID, PORTEFEUILLE;
  select CDE_ID, PORTEFEUILLE, TAUX into :m1c trimmed, :m1p trimmed, :m1t trimmed from work.d3 where TAUX ne . having TAUX = min(TAUX);
quit;
proc sort data=work.d3; by CDE_ID; run;
proc transpose data=work.d3 out=work.d3t(drop=_name_); by CDE_ID; id PORTEFEUILLE; var TAUX; run;
data _null_;
  length k $2000;
  k = catx(' ', 'Lecture : chaque cellule donne le taux de conformite d un CDE sur un portefeuille ; une cellule grise n/a signifie que le CDE ne s applique pas a ce portefeuille ou que l effectif applicable est inferieur a 30 contrats.',
           'La cellule la plus faible est', "&m1c.", 'sur', "&m1p.", cats('(', strip(put(&m1t.,frp.)), ').'), 'Les taux sont lus dans dq.AGG_CDE_SCORE, arrete &am..');
  call symputx('k_d3', k);
run;
ods excel options(sheet_interval="now" sheet_name="DQ_3_Matrice" absolute_column_width="10,12,12,12,12,12,12,12");
title1 j=l "^S={background=cx0A2F3F color=white fontsize=15pt fontweight=bold} Diagnostic 3 : Matrice CDE x portefeuille";
title2 j=l "R11 : dq.AGG_CDE_SCORE (arrete &am.)";
title3 j=l "^S={color=cxC00000 fontweight=bold} &banner.";
ods text = "^S={background=cxE1EFF5} &k_d3.";
proc report data=work.d3t nowd;
  columns CDE_ID CORP IMMO CONSO REVOL CI CAV PGE;
  define CDE_ID / 'CDE';
  define CORP / format=pcna.; define IMMO / format=pcna.; define CONSO / format=pcna.; define REVOL / format=pcna.;
  define CI / format=pcna.; define CAV / format=pcna.; define PGE / format=pcna.;
  compute CORP;
    if CORP = . then call define(_col_, 'style', 'style=[background=cxE7E6E6]'); else if CORP >= 0.99 then call define(_col_, 'style', 'style=[background=cxD9EAD3]');
    else if CORP >= 0.97 then call define(_col_, 'style', 'style=[background=cxFCE5CD]'); else call define(_col_, 'style', 'style=[background=cxF4CCCC]');
  endcomp;
  compute IMMO;
    if IMMO = . then call define(_col_, 'style', 'style=[background=cxE7E6E6]'); else if IMMO >= 0.99 then call define(_col_, 'style', 'style=[background=cxD9EAD3]');
    else if IMMO >= 0.97 then call define(_col_, 'style', 'style=[background=cxFCE5CD]'); else call define(_col_, 'style', 'style=[background=cxF4CCCC]');
  endcomp;
  compute CONSO;
    if CONSO = . then call define(_col_, 'style', 'style=[background=cxE7E6E6]'); else if CONSO >= 0.99 then call define(_col_, 'style', 'style=[background=cxD9EAD3]');
    else if CONSO >= 0.97 then call define(_col_, 'style', 'style=[background=cxFCE5CD]'); else call define(_col_, 'style', 'style=[background=cxF4CCCC]');
  endcomp;
  compute REVOL;
    if REVOL = . then call define(_col_, 'style', 'style=[background=cxE7E6E6]'); else if REVOL >= 0.99 then call define(_col_, 'style', 'style=[background=cxD9EAD3]');
    else if REVOL >= 0.97 then call define(_col_, 'style', 'style=[background=cxFCE5CD]'); else call define(_col_, 'style', 'style=[background=cxF4CCCC]');
  endcomp;
  compute CI;
    if CI = . then call define(_col_, 'style', 'style=[background=cxE7E6E6]'); else if CI >= 0.99 then call define(_col_, 'style', 'style=[background=cxD9EAD3]');
    else if CI >= 0.97 then call define(_col_, 'style', 'style=[background=cxFCE5CD]'); else call define(_col_, 'style', 'style=[background=cxF4CCCC]');
  endcomp;
  compute CAV;
    if CAV = . then call define(_col_, 'style', 'style=[background=cxE7E6E6]'); else if CAV >= 0.99 then call define(_col_, 'style', 'style=[background=cxD9EAD3]');
    else if CAV >= 0.97 then call define(_col_, 'style', 'style=[background=cxFCE5CD]'); else call define(_col_, 'style', 'style=[background=cxF4CCCC]');
  endcomp;
  compute PGE;
    if PGE = . then call define(_col_, 'style', 'style=[background=cxE7E6E6]'); else if PGE >= 0.99 then call define(_col_, 'style', 'style=[background=cxD9EAD3]');
    else if PGE >= 0.97 then call define(_col_, 'style', 'style=[background=cxFCE5CD]'); else call define(_col_, 'style', 'style=[background=cxF4CCCC]');
  endcomp;
run;
title1 j=l "G11. Conformite (%) par CDE et portefeuille";
proc sgplot data=work.d3;
  heatmapparm x=PORTEFEUILLE y=CDE_ID colorresponse=TAUX / colormodel=(cxE34948 cxEDA100 cxF7F6F2 cx1BAF7A);
  xaxis display=(nolabel); yaxis display=(nolabel) reverse; gradlegend / title="Conformite";
run;
title1;

/*===========================================================================
  R12. DQ_4 - Contrats en anomalie : 8 plus gros par regle a l orange ou au rouge
===========================================================================*/
proc sql;
  create table work.d4a as
  select * from dq.DQ_ISSUE
  where N_PERD = &am. and RULE_ID in (select RULE_ID from work.d1 where FEU in ('Orange','Rouge'))
  order by RULE_ID, EAD desc;
quit;
data work.d4;
  set work.d4a;
  by RULE_ID;
  if first.RULE_ID then RG = 0;
  RG + 1;
  if RG <= 8;
  format EAD comma18.;
run;
proc sql noprint;
  select count(*), sum(STATUT='NOUVELLE'), sum(STATUT='PERSISTANTE'), count(distinct RULE_ID) into :d4_n trimmed, :d4_new trimmed, :d4_per trimmed, :d4_nr trimmed
    from dq.DQ_ISSUE where N_PERD = &am.;
quit;
data _null_;
  length k $2000;
  k = catx(' ', 'Cette liste est destinee aux data stewards : elle donne, pour chaque regle a l orange ou au rouge, les 8 contrats les plus lourds en exposition.',
           'Au total', strip(put(&d4_n.,frn.)), 'anomalies a M, dont', strip(put(&d4_new.,frn.)), 'nouvelles et', strip(put(&d4_per.,frn.)), 'persistantes.',
           'Les valeurs constatees se retrouvent en joignant dq.DQ_RULE_FLAG par ID_CONTRAT.');
  call symputx('k_d4', k);
run;
ods excel options(sheet_interval="now" sheet_name="DQ_4_Anomalies" absolute_column_width="10,16,14,16,12,16,22");
title1 j=l "^S={background=cx0A2F3F color=white fontsize=15pt fontweight=bold} Diagnostic 4 : Contrats en anomalie (extrait)";
title2 j=l "R12 : dq.DQ_ISSUE (8 plus gros contrats par regle a l orange ou au rouge)";
title3 j=l "^S={color=cxC00000 fontweight=bold} &banner.";
ods text = "^S={background=cxE1EFF5} &k_d4.";
proc print data=work.d4 noobs label;
  var RULE_ID ID_CONTRAT PORTEFEUILLE EAD SEVERITE STATUT CAUSE_RACINE;
  label RULE_ID='Regle' ID_CONTRAT='Contrat' PORTEFEUILLE='Portefeuille' EAD='Exposition (EUR)' SEVERITE='Severite' STATUT='Statut' CAUSE_RACINE='Cause racine';
run;
proc sql;
  create table work.g12 as
  select RULE_ID, STATUT, count(*) as N from dq.DQ_ISSUE where N_PERD = &am. group by RULE_ID, STATUT;
  create table work.g12t as select RULE_ID, sum(N) as T from work.g12 group by RULE_ID order by calculated T desc;
quit;
data work.g12t; set work.g12t(obs=12); run;
proc sql; create table work.g12b as select a.* from work.g12 as a inner join work.g12t as b on a.RULE_ID = b.RULE_ID; quit;
title1 j=l "G12. Anomalies par regle et statut (12 regles les plus en echec)";
proc sgplot data=work.g12b;
  hbar RULE_ID / response=N group=STATUT groupdisplay=stack categoryorder=respdesc;
  xaxis label="Anomalies (contrat x regle)" grid; yaxis display=(nolabel);
run;
title1;

/*===========================================================================
  R13. DQ_5 - Registre initial des anomalies (regles orange ou rouge)
===========================================================================*/
proc sort data=dq.DQ_RCA_CANDIDATE out=work.rca; where N_PERD = &am.; by RULE_ID descending LIFT; run;
data work.rca1; set work.rca; by RULE_ID; if first.RULE_ID; length PISTE $80; PISTE = catx(' ', DIMENSION, '=', VALEUR, cats('(sur-representation x ', strip(put(LIFT, 8.1)), ')')); keep RULE_ID PISTE; run;
proc sql;
  create table work.d5 as
  select a.RULE_ID, a.LIBELLE, a.CDE_ID, a.SEVERITE, a.CONF, a.N_ECHEC, a.EAD_ECHEC, a.FEU, a.CAUSE_RACINE, r.PISTE, a.OWNER,
         ' ' as ACTION length=40, ' ' as ECHEANCE length=12
  from work.d1 as a left join work.rca1 as r on a.RULE_ID = r.RULE_ID
  where a.FEU in ('Orange','Rouge')
  order by a.FEU desc, a.EAD_ECHEC desc;
  select count(*), sum(FEU='Rouge') into :d5_n trimmed, :d5_r trimmed from work.d5;
quit;
data _null_;
  length k $2000;
  k = catx(' ', 'Constat :', strip(put(&d5_n.,frn.)), 'regles sont a qualifier, dont', strip(put(&d5_r.,frn.)), 'au rouge. Les colonnes piste sont des hypotheses a verifier, pas des conclusions :',
           'elles indiquent la valeur la plus sur-representee parmi les echecs. Chaque ligne appelle un proprietaire, une action et une echeance valides par le Data Owner (colonnes a completer).');
  call symputx('k_d5', k);
run;
ods excel options(sheet_interval="now" sheet_name="DQ_5_Registre" absolute_column_width="10,34,9,10,12,13,15,10,18,36,20,24,12");
title1 j=l "^S={background=cx0A2F3F color=white fontsize=15pt fontweight=bold} Diagnostic 5 : Registre initial des anomalies";
title2 j=l "R13 : une ligne par regle orange ou rouge ; colonnes de qualification a completer avec les Data Owners";
title3 j=l "^S={color=cxC00000 fontweight=bold} &banner.";
ods text = "^S={background=cxE1EFF5} &k_d5.";
proc report data=work.d5 nowd;
  columns RULE_ID LIBELLE CDE_ID SEVERITE CONF N_ECHEC EAD_ECHEC FEU CAUSE_RACINE PISTE OWNER ACTION ECHEANCE;
  define RULE_ID / 'Regle'; define LIBELLE / 'Libelle'; define CDE_ID / 'CDE'; define SEVERITE / 'Severite'; define CONF / 'Taux de conformite' format=percent8.2;
  define N_ECHEC / 'Contrats en echec' format=comma12.; define EAD_ECHEC / 'Exposition (EUR)' format=comma18.; define FEU / 'Feu'; define CAUSE_RACINE / 'Cause racine (catalogue)';
  define PISTE / 'Piste (valeur sur-representee)'; define OWNER / 'Proprietaire'; define ACTION / 'Action'; define ECHEANCE / 'Echeance';
  compute FEU;
    if FEU = 'Rouge' then call define(_col_, 'style', 'style=[background=cxF4CCCC]'); else call define(_col_, 'style', 'style=[background=cxFCE5CD]');
  endcomp;
run;
title1 j=l "G13. Exposition concernee par regle a l orange ou au rouge (M EUR)";
data work.g13; set work.d5; EAD_M = EAD_ECHEC / 1e6; run;
proc sgplot data=work.g13 dattrmap=work.attr_feu;
  hbar RULE_ID / response=EAD_M group=FEU attrid=feu categoryorder=respdesc;
  xaxis label="M EUR" grid; yaxis display=(nolabel);
run;
title1;

/*===========================================================================
  R14. HLC_1 - Concordance de la HLC de production avec la HLC attendue
===========================================================================*/
proc sql;
  create table work.h1 as
  select PORTEFEUILLE,
         sum(case when MATCH_STATUS = 'MATCH' then N_CONTRATS else 0 end) as N_MATCH,
         sum(case when MATCH_STATUS = 'MISMATCH' then N_CONTRATS else 0 end) as N_MISMATCH,
         sum(case when MATCH_STATUS = 'UNDETERMINED' then N_CONTRATS else 0 end) as N_UNDET,
         sum(N_CONTRATS) as N_TOTAL
  from dq.AGG_HLC_COMPARE
  where N_PERD = &am. and PORTEFEUILLE in ('CORP','IMMO','CONSO','REVOL','CI','CAV') and MATCH_STATUS in ('MATCH','MISMATCH','UNDETERMINED')
  group by PORTEFEUILLE;
quit;
data work.h1;
  set work.h1;
  TAUX = N_MATCH / N_TOTAL;
  ORD = findw('CORP IMMO CONSO REVOL CI CAV', strip(PORTEFEUILLE));
  format N_MATCH N_MISMATCH N_UNDET N_TOTAL comma12. TAUX percent8.1;
run;
proc sort data=work.h1; by ORD; run;
proc sql;
  create table work.h1_c as
  select MATCH_STATUS, MISMATCH_CAUSE, sum(N_CONTRATS) as N format=comma12., sum(EAD) as EAD format=comma18.
  from dq.AGG_HLC_COMPARE where N_PERD = &am. and MATCH_STATUS in ('MISMATCH','UNDETERMINED')
  group by MATCH_STATUS, MISMATCH_CAUSE;
  create table work.h1_d as
  select d.DRIVER_ID, r.LIBELLE, sum(d.N_CONTRATS) as N format=comma12., sum(d.EAD) as EAD format=comma18., sum(d.ECL_ECART) as ECL_ECART format=comma18.
  from dq.AGG_HLC_DRIVER_DIFF as d left join ref.REF_DRIVER as r on d.DRIVER_ID = r.DRIVER_ID
  where d.N_PERD = &am.
  group by d.DRIVER_ID, r.LIBELLE order by d.DRIVER_ID;
  select sum(N_TOTAL), sum(N_MATCH), sum(N_MISMATCH), sum(N_UNDET) into :h_tot trimmed, :h_m trimmed, :h_x trimmed, :h_u trimmed from work.h1;
  select sum(N) into :c_data trimmed from work.h1_c where MISMATCH_CAUSE = 'DATA';
  select sum(N) into :c_dec trimmed from work.h1_c where MISMATCH_CAUSE = 'DECISION';
  select sum(N) into :c_map trimmed from work.h1_c where MISMATCH_CAUSE = 'MAPPING';
  select sum(N) into :c_ver trimmed from work.h1_c where MISMATCH_CAUSE = 'RULE_VERSION_ISSUE';
  select LIBELLE, ECL_ECART into :dr_e trimmed, :dr_ev trimmed from work.h1_d having ECL_ECART = max(ECL_ECART);
  select LIBELLE, N into :dr_n trimmed, :dr_nv trimmed from work.h1_d having N = max(N);
quit;
data _null_;
  length k $2000;
  k = catx(' ', 'Constat : sur', strip(put(&h_tot.,frn.)), 'contrats des six portefeuilles statistiques,', strip(put(&h_m.,frn.)), 'concordent', cats('(', strip(put(&h_m./&h_tot.,frp.)), '),'),
           strip(put(&h_x.,frn.)), 'divergent et', strip(put(&h_u.,frn.)), 'restent indetermines. Les causes des ecarts sont la donnee', cats('(', strip(put(&c_data.,frn.)), '),'),
           'la decision non tranchee', cats('(', strip(put(&c_dec.,frn.)), '),'), 'le mapping', cats('(', strip(put(&c_map.,frn.)), ')'), 'et la version de regle', cats('(', strip(put(&c_ver.,frn.)), ').'),
           'Au niveau des drivers,', "&dr_e.", 'pese le plus sur la provision', cats('(', strip(put(&dr_ev./1000,frn.)), ' k EUR)'), 'et', "&dr_n.", 'sur le nombre de contrats', cats('(', strip(put(&dr_nv.,frn.)), ').'));
  call symputx('k_h1', k);
run;
ods excel options(sheet_interval="now" sheet_name="HLC_1_Concordance" absolute_column_width="26,16,16,18,16,16,16");
title1 j=l "^S={background=cx0A2F3F color=white fontsize=15pt fontweight=bold} Concordance 1 : HLC de production contre HLC attendue";
title2 j=l "R14 : dq.AGG_HLC_COMPARE, dq.AGG_HLC_DRIVER_DIFF, dq.HLC_TRANSITION";
title3 j=l "^S={color=cxC00000 fontweight=bold} &banner.";
ods text = "^S={background=cxE1EFF5} &k_h1.";
proc print data=work.h1 noobs label; var PORTEFEUILLE N_MATCH N_MISMATCH N_UNDET N_TOTAL TAUX;
  label N_MATCH='MATCH' N_MISMATCH='MISMATCH' N_UNDET='UNDETERMINED' N_TOTAL='Total' TAUX='Taux de concordance'; run;
title1 j=l "Causes des ecarts et des indetermines";
proc print data=work.h1_c noobs; run;
title1 j=l "Ecarts par driver";
proc print data=work.h1_d noobs; run;

data work.attr_hlc;
  length ID $8 VALUE $14 FILLCOLOR $10;
  ID = 'hlc'; VALUE = 'MATCH';        FILLCOLOR = 'cx1BAF7A'; output;
  VALUE = 'MISMATCH';     FILLCOLOR = 'cxE34948'; output;
  VALUE = 'UNDETERMINED'; FILLCOLOR = 'cxEDA100'; output;
run;
data work.g14;
  set work.h1;
  length STATUT $14;
  STATUT = 'MATCH';        PCT = N_MATCH / N_TOTAL;    output;
  STATUT = 'MISMATCH';     PCT = N_MISMATCH / N_TOTAL; output;
  STATUT = 'UNDETERMINED'; PCT = N_UNDET / N_TOTAL;    output;
run;
title1 j=l "G14. HLC de production contre HLC attendue (M) - axe tronque a 90 %";
proc sgplot data=work.g14 dattrmap=work.attr_hlc;
  hbar PORTEFEUILLE / response=PCT group=STATUT attrid=hlc groupdisplay=stack;
  xaxis min=0.9 max=1 grid valuesformat=percent8.; yaxis display=(nolabel) reverse;
run;
title1 j=l "G15a. Ecarts de driver (contrats)";
proc sgplot data=work.h1_d; vbar DRIVER_ID / response=N fillattrs=(color=cx2A78D6) datalabel; xaxis display=(nolabel); yaxis grid label="Contrats"; run;
title1 j=l "G15b. Ecart de provision par driver (EUR)";
proc sgplot data=work.h1_d; vbar DRIVER_ID / response=ECL_ECART fillattrs=(color=cxEB6834); xaxis display=(nolabel); yaxis grid label="EUR"; run;
title1 j=l "G16a. Transitions de HLC attendue M-1 vers M : IMMO";
proc sgplot data=dq.HLC_TRANSITION(where=(PORTEFEUILLE_M = 'IMMO'));
  heatmapparm x=HLC_M y=HLC_M1 colorresponse=N_CONTRATS / colormodel=(cxDBE8F8 cx2A78D6 cx143A6B);
  text x=HLC_M y=HLC_M1 text=N_CONTRATS / textattrs=(size=8);
  xaxis type=discrete label="HLC a M"; yaxis type=discrete reverse label="HLC a M-1"; gradlegend / title="Contrats";
run;
title1 j=l "G16b. Transitions de HLC attendue M-1 vers M : CI";
proc sgplot data=dq.HLC_TRANSITION(where=(PORTEFEUILLE_M = 'CI'));
  heatmapparm x=HLC_M y=HLC_M1 colorresponse=N_CONTRATS / colormodel=(cxDBE8F8 cx2A78D6 cx143A6B);
  text x=HLC_M y=HLC_M1 text=N_CONTRATS / textattrs=(size=8);
  xaxis type=discrete label="HLC a M"; yaxis type=discrete reverse label="HLC a M-1"; gradlegend / title="Contrats";
run;
title1;

/*===========================================================================
  R15. ECL_1 - Provision : production, recalcul, correction, impact des anomalies
===========================================================================*/
proc sql;
  create table work.l1 as
  select PORTEFEUILLE, N_CONTRATS, ECL_PROD, ECL as ECL_RECALC, ECL_CORRIGE,
         ECL_PROD - ECL as D_PROD_RECALC, ECL_CORRIGE - ECL_PROD as D_CORR_PROD
  from dq.AGG_ECL
  where N_PERD = &am. and PORTEFEUILLE in ('CORP','IMMO','CONSO','REVOL','CI','CAV','PGE');
  select sum(ECL_PROD), sum(ECL_RECALC), sum(ECL_CORRIGE) into :t_prod trimmed, :t_rec trimmed, :t_cor trimmed from work.l1;
  create table work.l1_i as
  select STATUT_IMPACT, sum(N_ANOMALIES) as N_ANOMALIES format=comma12., sum(EAD) as EAD format=comma18., sum(IMPACT_ECL) as IMPACT_ECL format=comma18.
  from dq.AGG_IMPACT where N_PERD = &am. group by STATUT_IMPACT;
  select sum(IMPACT_ECL) into :imp_tot trimmed from work.l1_i;
  select N_ANOMALIES, EAD into :nq_n trimmed, :nq_e trimmed from work.l1_i where STATUT_IMPACT = 'NOT_QUANTIFIABLE';
quit;
data _null_;
  length k $2000;
  k = catx(' ', 'Constat : a M, la provision de production vaut', strip(put(&t_prod./1e6,frm1.)), 'M EUR, la provision recalculee', strip(put(&t_rec./1e6,frm1.)), 'M EUR et la provision corrigee', strip(put(&t_cor./1e6,frm1.)), 'M EUR.',
           'L ecart production - recalcul est de', strip(put((&t_prod.-&t_rec.)/1e6,frm2.)), 'M EUR', cats('(', strip(put((&t_prod.-&t_rec.)/&t_prod.,frp2.)), ').'),
           'Corriger les anomalies ferait varier la provision de', strip(put(&imp_tot./1000,frn.)), 'k EUR sur les anomalies chiffrees ;', strip(put(&nq_n.,frn.)), 'anomalies ne sont pas quantifiables',
           cats('(', strip(put(&nq_e./1e6,frm1.)), ' M EUR d exposition),'), 'donc l impact reel est plus incertain que ce total.');
  call symputx('k_l1', k);
run;
ods excel options(sheet_interval="now" sheet_name="ECL_1_Provision" absolute_column_width="22,16,18,18,18,18,18");
title1 j=l "^S={background=cx0A2F3F color=white fontsize=15pt fontweight=bold} Provision 1 : Production, recalcul, correction, impact des anomalies";
title2 j=l "R15 : dq.AGG_ECL, dq.AGG_IMPACT";
title3 j=l "^S={color=cxC00000 fontweight=bold} &banner.";
ods text = "^S={background=cxE1EFF5} &k_l1.";
proc report data=work.l1 nowd;
  columns PORTEFEUILLE N_CONTRATS ECL_PROD ECL_RECALC ECL_CORRIGE D_PROD_RECALC D_CORR_PROD;
  define PORTEFEUILLE / group 'Portefeuille'; define N_CONTRATS / analysis sum 'Contrats' format=comma12.;
  define ECL_PROD / analysis sum 'Provision production (EUR)' format=comma18.; define ECL_RECALC / analysis sum 'Provision recalculee (EUR)' format=comma18.;
  define ECL_CORRIGE / analysis sum 'Provision corrigee (EUR)' format=comma18.; define D_PROD_RECALC / analysis sum 'Production - recalcul (EUR)' format=comma18.;
  define D_CORR_PROD / analysis sum 'Corrigee - production (EUR)' format=comma18.;
  rbreak after / summarize style=[fontweight=bold];
run;
title1 j=l "Impact des anomalies sur la provision (une fois par contrat : estime > calcule > non quantifiable)";
proc print data=work.l1_i noobs; run;
data work.g17;
  set work.l1;
  length SCENARIO $14;
  SCENARIO = 'Production';  VAL = ECL_PROD / 1e6;    output;
  SCENARIO = 'Recalculee';  VAL = ECL_RECALC / 1e6;  output;
  SCENARIO = 'Corrigee';    VAL = ECL_CORRIGE / 1e6; output;
run;
title1 j=l "G17. Provision FLL_PROV : production, recalculee, corrigee (M EUR)";
proc sgplot data=work.g17;
  vbar PORTEFEUILLE / response=VAL group=SCENARIO groupdisplay=cluster;
  xaxis values=('CORP' 'IMMO' 'CONSO' 'REVOL' 'CI' 'CAV' 'PGE') display=(nolabel); yaxis label="M EUR" grid;
run;
title1 j=l "G18a. Anomalies par statut d impact";
proc sgplot data=work.l1_i; vbar STATUT_IMPACT / response=N_ANOMALIES fillattrs=(color=cx2A78D6) datalabel; xaxis display=(nolabel); yaxis grid label="Anomalies"; run;
proc sql;
  create table work.g18 as select PORTEFEUILLE, sum(IMPACT_ECL) / 1000 as IMPACT_K from dq.AGG_IMPACT
  where N_PERD = &am. and PORTEFEUILLE in ('CORP','IMMO','CONSO','REVOL','CI','CAV','PGE') group by PORTEFEUILLE;
quit;
title1 j=l "G18b. Impact corrige - production (k EUR)";
proc sgplot data=work.g18;
  vbar PORTEFEUILLE / response=IMPACT_K fillattrs=(color=cxEB6834);
  xaxis values=('CORP' 'IMMO' 'CONSO' 'REVOL' 'CI' 'CAV' 'PGE') display=(nolabel); yaxis label="k EUR" grid;
run;
title1;

/*===========================================================================
  R16. ECL_2 - Bridge de provision M-1 vers M (somme des portefeuilles)
===========================================================================*/
proc sql;
  create table work.l2 as
  select b.EFFET_CODE, e.LIBELLE, e.ORDRE, sum(b.MONTANT) as MONTANT format=comma18.
  from dq.ECL_EFFECT_BRIDGE as b left join ref.REF_EFFET as e on b.EFFET_CODE = e.EFFET_CODE
  group by b.EFFET_CODE, e.LIBELLE, e.ORDRE order by e.ORDRE;
  select MONTANT into :b_open trimmed from work.l2 where EFFET_CODE = 'OPEN';
  select sum(MONTANT) into :b_close trimmed from work.l2;
  select sum(MONTANT) into :b_pop trimmed from work.l2 where EFFET_CODE in ('P01','P02','P03');
  select MONTANT into :b_p01 trimmed from work.l2 where EFFET_CODE = 'P01';
  select MONTANT into :b_p02 trimmed from work.l2 where EFFET_CODE = 'P02';
  select MONTANT into :b_p03 trimmed from work.l2 where EFFET_CODE = 'P03';
  select MONTANT into :b_e04 trimmed from work.l2 where EFFET_CODE = 'E04';
  select MONTANT into :b_e06 trimmed from work.l2 where EFFET_CODE = 'E06';
  select MONTANT into :b_e07 trimmed from work.l2 where EFFET_CODE = 'E07';
quit;
data _null_;
  length k $2000;
  k = catx(' ', 'Constat : la provision recalculee passe de', strip(put(&b_open./1e6,frm1.)), 'M EUR a M-1 a', strip(put(&b_close./1e6,frm1.)), 'M EUR a M, soit', strip(put((&b_close.-&b_open.)/1e6,frm2.)), 'M EUR.',
           'Les populations expliquent', strip(put(&b_pop./1e6,frm2.)), 'M EUR', cats('(entrees ', strip(put(&b_p01./1e6,frm2.)), ', sorties ', strip(put(&b_p02./1e6,frm2.)), ', changements de route ', strip(put(&b_p03./1e6,frm2.)), ').'),
           'Parmi les effets sur les contrats communs, les parametres', cats('(', strip(put(&b_e06./1e6,frm2.)), ' M EUR)'), 'et la duree en defaut et le statut', cats('(', strip(put(&b_e04./1e6,frm2.)), ' M EUR)'), 'dominent.',
           'L effet residuel est de', strip(put(&b_e07.,frm2.)), 'EUR : il est nul si les sept effets expliquent tout l ecart.');
  call symputx('k_l2', k);
run;
ods excel options(sheet_interval="now" sheet_name="ECL_2_Bridge" absolute_column_width="14,44,20,50");
title1 j=l "^S={background=cx0A2F3F color=white fontsize=15pt fontweight=bold} Provision 2 : Passage de M-1 a M";
title2 j=l "R16 : dq.ECL_EFFECT_BRIDGE (somme des portefeuilles)";
title3 j=l "^S={color=cxC00000 fontweight=bold} &banner.";
ods text = "^S={background=cxE1EFF5} &k_l2.";
proc print data=work.l2 noobs label; var EFFET_CODE LIBELLE MONTANT; label EFFET_CODE='Code' LIBELLE='Effet' MONTANT='Montant (EUR)'; run;
data work.g19;                            /* cascade : une barre flottante par effet */
  set work.l2 end=fin;
  length LAB $14 SENS $8;
  retain CUM 0;
  LAB = EFFET_CODE;
  if EFFET_CODE = 'OPEN' then do; LAB = 'M-1'; SENS = 'Total'; BAS = 0; HAUT = MONTANT / 1e6; CUM = MONTANT; output; end;
  else do;
    SENS = ifc(MONTANT >= 0, 'Hausse', 'Baisse');
    BAS = min(CUM, CUM + MONTANT) / 1e6; HAUT = max(CUM, CUM + MONTANT) / 1e6; CUM + MONTANT; output;
  end;
  if fin then do; LAB = 'M'; SENS = 'Total'; BAS = 0; HAUT = CUM / 1e6; output; end;
run;
data work.attr_wf;
  length ID $8 VALUE $8 FILLCOLOR $10;
  ID = 'wf'; VALUE = 'Total';  FILLCOLOR = 'cx2A78D6'; output;
  VALUE = 'Hausse'; FILLCOLOR = 'cxEB6834'; output;
  VALUE = 'Baisse'; FILLCOLOR = 'cx1BAF7A'; output;
run;
title1 j=l "G19. Passage de la provision recalculee de M-1 a M (M EUR, axe tronque)";
proc sgplot data=work.g19 dattrmap=work.attr_wf;
  highlow x=LAB low=BAS high=HAUT / group=SENS attrid=wf type=bar barwidth=0.7;
  xaxis discreteorder=data display=(nolabel); yaxis min=246 label="M EUR" grid;
run;
title1;

/*===========================================================================
  R17. DEC_1 - Decisions et certification
===========================================================================*/
proc sql noprint;
  select count(*), sum(BLOQUANT = 'Y'), sum(BLOQUANT = 'Y' and STATUT ne 'FERMEE') into :x_n trimmed, :x_b trimmed, :x_o trimmed from pub.PUB_DECISION where N_PERD = &am.;
  select count(*) into :x_m trimmed from pub.PUB_CERTIFICATION where N_PERD = &am.;
  select max(STATUT) into :x_st trimmed from pub.PUB_CERTIFICATION where N_PERD = &am.;
quit;
data _null_;
  length k $2000;
  k = catx(' ', 'Constat :', strip(put(&x_n.,frn.)), 'decisions sont suivies, dont', strip(put(&x_b.,frn.)), 'bloquantes ;', strip(put(&x_o.,frn.)), 'bloquantes restent non fermees.',
           'Tant qu une decision bloquante n est pas fermee, l arrete n est pas certifiable : statut', "&x_st.,", 'avec', strip(put(&x_m.,frn.)), 'motifs. Les proprietaires et echeances sont ceux du jeu de test.');
  call symputx('k_x1', k);
run;
ods excel options(sheet_interval="now" sheet_name="DEC_1_Décisions" absolute_column_width="8,40,22,14,12,14");
title1 j=l "^S={background=cx0A2F3F color=white fontsize=15pt fontweight=bold} Gouvernance 1 : Decisions et certification";
title2 j=l "R17 : pub.PUB_DECISION et pub.PUB_CERTIFICATION";
title3 j=l "^S={color=cxC00000 fontweight=bold} &banner.";
ods text = "^S={background=cxE1EFF5} &k_x1.";
title1 j=l "Decisions bloquantes";
proc report data=pub.PUB_DECISION nowd;
  where N_PERD = &am. and BLOQUANT = 'Y';
  columns DECISION_ID SUJET PROPRIETAIRE STATUT BLOQUANT ECHEANCE;
  define DECISION_ID / 'N'; define SUJET / 'Sujet'; define PROPRIETAIRE / 'Proprietaire'; define STATUT / 'Statut'; define BLOQUANT / 'Bloquante';
  define ECHEANCE / 'Echeance' format=yymmdd10.;
  compute STATUT;
    if STATUT = 'FERMEE' then call define(_col_, 'style', 'style=[background=cxD9EAD3]');
    else if STATUT = 'PARTIELLE' then call define(_col_, 'style', 'style=[background=cxFFF2CC]');
    else call define(_col_, 'style', 'style=[background=cxFCE5CD]');
  endcomp;
run;
title1 j=l "Motifs de non-certification";
proc print data=pub.PUB_CERTIFICATION noobs; where N_PERD = &am.; var MOTIF_ID MOTIF; run;
title1 j=l "G20. Decisions par statut";
proc sgplot data=pub.PUB_DECISION(where=(N_PERD = &am.));
  vbar BLOQUANT / group=STATUT groupdisplay=stack;
  xaxis display=(nolabel); yaxis label="Decisions" grid;
run;
title1;

/*===========================================================================
  Fermeture du classeur
===========================================================================*/
ods excel close;
ods graphics off;
title;
%put NOTE: DQ_06 section 2 - rapport ecrit dans &rapport.;

%let step_id  = DQ_05_RAPPORT;
%let step_msg = Rapport Excel EDA et diagnostic ecrit;

proc sql;
  delete from dq.RUN_LOG where RUN_ID = "&run_id." and STEP_ID = "&step_id.";
quit;
data work.log1;
  length RUN_ID $20 STEP_ID $40 STATUT $8 MESSAGE $200;
  N_PERD = &am.; RUN_ID = "&run_id."; STEP_ID = "&step_id.";
  DEBUT = &t0.; FIN = datetime(); DUREE_S = round(FIN - DEBUT, 1);
  STATUT = 'OK'; MESSAGE = "&step_msg.";
  format DEBUT FIN datetime19.;
run;
proc append base=dq.RUN_LOG data=work.log1 force; run;
