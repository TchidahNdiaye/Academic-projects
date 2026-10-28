/******************************************************************************
 Programme : DQ_01_SOURCES_PAQUETS.sas   str    (programme 1 sur 6 - dispositif DQ LGD / provisions)
 Etape CDC : Etape 1 - Sources et paquets mensuels
 Version   : v1.2
 Objet     : Configuration et lancement, parametres et referentiels, collecte des paquets mensuels M et M-1 (RAW puis STG), manifeste.
 Entrees   : paquets CSV (M et M-1) + manifestes + referentiels REF_*.csv
 Sorties   : raw.*, stg.*, ref.*, dq.DQ_INPUT_MANIFEST, dq.RUN_LOG
 Contenu   : S0 configuration et lancement | S1 parametres et referentiels | S2 collecte RAW/STG et snapshots
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

/* S0.4 Tables d'historique : creees vides si elles n'existent pas (permet les relances) */
data _null_;
  if not exist('dq.RUN_LOG') then call execute(
    'data dq.RUN_LOG; length N_PERD 8 RUN_ID $20 STEP_ID $40 DEBUT 8 FIN 8 DUREE_S 8 STATUT $8 MESSAGE $200;
     format DEBUT FIN datetime19.; stop; run;');
  if not exist('dq.DQ_ISSUE_HIST') then call execute(
    'data dq.DQ_ISSUE_HIST; length RUN_ID $20 N_PERD 8 ID_CONTRAT $20 RULE_ID $8 PORTEFEUILLE $8 EAD 8 STATUT $12; stop; run;');
run;



/*======================================================================*/
/* SECTION 1 : REFERENTIELS */
/******************************************************************************
 Section 1.1 (ex-programme DQ_00_PARAMETRES_REFERENTIELS)
 Etape     : 1 - Sources et paquets (referentiels et parametres)
 Objet     : Charger les referentiels et parametres APPROUVES (fichiers CSV
             deposes dans &root./referentiels) dans les tables ref.REF_*, puis
             preparer les parametres valides a chaque arrete (M-1 et M).
 Regle     : AUCUNE valeur metier n'est codee en dur dans les autres programmes.
             LGDD, facteur forward-looking, taux de tirage, taux PGE, seuils,
             listes (34 codes de garantie, SEG_DECL, mapping produits,
             exclusions) sont lus ici. Toute valeur est versionnee (VERSION,
             VALID_FROM, VALID_TO, APPROBATION).
 Entrees   : &root./referentiels/REF_*.csv (UTF-8, une ligne d'en-tete)
 Sorties   : ref.REF_* ; dq.PARAM_LGDD, dq.PARAM_TT, dq.PARAM_PGE (valides a
             l'arrete) ; dq.DQ_REF_CHECK (controles des referentiels)
 Controles : couverture des 48 HLC par arrete, taux de tirage complets pour
             Corporate / CI / Revolving / CAV, taux PGE disponibles (sinon
             avertissement : un taux manquant n'est jamais remplace par zero).
 Decisions : Q31 (LGDD), Q32 / Q50 (taux de tirage), Q51 (facteur
             forward-looking), Q52 (taux PGE). Les valeurs du jeu de test sont
             FICTIVES : elles doivent etre remplacees par les valeurs approuvees.
 Version   : v1.2
******************************************************************************/
%let t0 = %sysfunc(datetime());   /* heure de debut de la section */

/* 1. Chargement des referentiels ---------------------------------------------
   Un DATA step par fichier. Les noms de colonnes sont ceux des fichiers CSV. */
/* REF_PORTEFEUILLE : 10 lignes dans le jeu de test */
data ref.REF_PORTEFEUILLE;
  infile "&root./referentiels/REF_PORTEFEUILLE.csv" dsd firstobs=2 truncover lrecl=32767 encoding="utf-8";
  length PORTEFEUILLE $16 LIBELLE $50 GROUPE $36 ORDRE 8;
  input PORTEFEUILLE :$16. LIBELLE :$50. GROUPE :$36. ORDRE :best32.;
run;

/* REF_CDE : 15 lignes dans le jeu de test */
data ref.REF_CDE;
  infile "&root./referentiels/REF_CDE.csv" dsd firstobs=2 truncover lrecl=32767 encoding="utf-8";
  length CDE_ID $16 LIBELLE $72 LIBELLE_COURT $34 FAMILLE $34 POIDS 8 SEUIL_VERT 8
    SEUIL_ORANGE 8 SOURCE $80;
  input CDE_ID :$16. LIBELLE :$72. LIBELLE_COURT :$34. FAMILLE :$34. POIDS :best32.
    SEUIL_VERT :best32. SEUIL_ORANGE :best32. SOURCE :$80.;
run;

/* REF_REGLE : 27 lignes dans le jeu de test */
data ref.REF_REGLE;
  infile "&root./referentiels/REF_REGLE.csv" dsd firstobs=2 truncover lrecl=32767 encoding="utf-8";
  length RULE_ID $16 LIBELLE $52 CDE_ID $16 SEVERITE $20 NIVEAU $18 CAUSE_RACINE $34
    METHODE_IMPACT $36 OWNER $50;
  input RULE_ID :$16. LIBELLE :$52. CDE_ID :$16. SEVERITE :$20. NIVEAU :$18. CAUSE_RACINE :$34.
    METHODE_IMPACT :$36. OWNER :$50.;
run;

/* REF_MAPPING_PRODUIT : 10 lignes dans le jeu de test */
data ref.REF_MAPPING_PRODUIT;
  infile "&root./referentiels/REF_MAPPING_PRODUIT.csv" dsd firstobs=2 truncover lrecl=32767 encoding="utf-8";
  length TYP_CPTE $10 C_NOMNC_ENGGM $10 SOUS_PORTEFEUILLE $14;
  input TYP_CPTE :$10. C_NOMNC_ENGGM :$10. SOUS_PORTEFEUILLE :$14.;
run;

/* REF_LOANIQ : 2 lignes dans le jeu de test */
data ref.REF_LOANIQ;
  infile "&root./referentiels/REF_LOANIQ.csv" dsd firstobs=2 truncover lrecl=32767 encoding="utf-8";
  length TYP_CPTE $10 MOTIF $18;
  input TYP_CPTE :$10. MOTIF :$18.;
run;

/* REF_CRELOG : 1 lignes dans le jeu de test */
data ref.REF_CRELOG;
  infile "&root./referentiels/REF_CRELOG.csv" dsd firstobs=2 truncover lrecl=32767 encoding="utf-8";
  length I_PERSN_GARANT $38 LIBELLE $34;
  input I_PERSN_GARANT :$38. LIBELLE :$34.;
run;

/* REF_GARANTIE_ELIG : 34 lignes dans le jeu de test */
data ref.REF_GARANTIE_ELIG;
  infile "&root./referentiels/REF_GARANTIE_ELIG.csv" dsd firstobs=2 truncover lrecl=32767 encoding="utf-8";
  length C_NOMNC_GARNT $10 VALID_FROM 8 VALID_TO 8;
  input C_NOMNC_GARNT :$10. VALID_FROM :yymmdd10. VALID_TO :yymmdd10.;
  format VALID_FROM VALID_TO yymmdd10.;
run;

/* REF_SEG_DECL : 30 lignes dans le jeu de test */
data ref.REF_SEG_DECL;
  infile "&root./referentiels/REF_SEG_DECL.csv" dsd firstobs=2 truncover lrecl=32767 encoding="utf-8";
  length SEG_DECL $8 CATEGORIE_CAV $14;
  input SEG_DECL :$8. CATEGORIE_CAV :$14.;
run;

/* REF_ENTITE : 8 lignes dans le jeu de test */
data ref.REF_ENTITE;
  infile "&root./referentiels/REF_ENTITE.csv" dsd firstobs=2 truncover lrecl=32767 encoding="utf-8";
  length C_ENTITE $12 TYPE $8 LIBELLE $56;
  input C_ENTITE :$12. TYPE :$8. LIBELLE :$56.;
run;

/* REF_EXCLUSION : 7 lignes dans le jeu de test */
data ref.REF_EXCLUSION;
  infile "&root./referentiels/REF_EXCLUSION.csv" dsd firstobs=2 truncover lrecl=32767 encoding="utf-8";
  length VARIABLE $20 VALEUR $14 MOTIF $22;
  input VARIABLE :$20. VALEUR :$14. MOTIF :$22.;
run;

/* REF_LGDD : 96 lignes dans le jeu de test */
data ref.REF_LGDD;
  infile "&root./referentiels/REF_LGDD.csv" dsd firstobs=2 truncover lrecl=32767 encoding="utf-8";
  length MODELE $14 HLC 8 LGDD 8 FL 8 VALID_FROM 8 VALID_TO 8 VERSION $28 APPROBATION $28;
  input MODELE :$14. HLC :best32. LGDD :best32. FL :best32. VALID_FROM :yymmdd10.
    VALID_TO :yymmdd10. VERSION :$28. APPROBATION :$28.;
  format VALID_FROM VALID_TO yymmdd10.;
run;

/* REF_TAUX_TIRAGE : 16 lignes dans le jeu de test */
data ref.REF_TAUX_TIRAGE;
  infile "&root./referentiels/REF_TAUX_TIRAGE.csv" dsd firstobs=2 truncover lrecl=32767 encoding="utf-8";
  length MODELE $14 TYPRIS $8 TAUX_TIRAGE 8 VALID_FROM 8 VALID_TO 8 VERSION $24 APPROBATION $28;
  input MODELE :$14. TYPRIS :$8. TAUX_TIRAGE :best32. VALID_FROM :yymmdd10. VALID_TO :yymmdd10.
    VERSION :$24. APPROBATION :$28.;
  format VALID_FROM VALID_TO yymmdd10.;
run;

/* REF_PGE_TAUX : 3 lignes dans le jeu de test */
data ref.REF_PGE_TAUX;
  infile "&root./referentiels/REF_PGE_TAUX.csv" dsd firstobs=2 truncover lrecl=32767 encoding="utf-8";
  length C_NOMNC_ENGGM $10 TYPRIS $8 TAUX_FORFAIT 8 VALID_FROM 8 VALID_TO 8 VERSION $26
    APPROBATION $22;
  input C_NOMNC_ENGGM :$10. TYPRIS :$8. TAUX_FORFAIT :best32. VALID_FROM :yymmdd10.
    VALID_TO :yymmdd10. VERSION :$26. APPROBATION :$22.;
  format VALID_FROM VALID_TO yymmdd10.;
run;

/* REF_SEUIL : 11 lignes dans le jeu de test */
data ref.REF_SEUIL;
  infile "&root./referentiels/REF_SEUIL.csv" dsd firstobs=2 truncover lrecl=32767 encoding="utf-8";
  length SEUIL_ID $46 VALEUR 8 LIBELLE $146 STATUT $46;
  input SEUIL_ID :$46. VALEUR :best32. LIBELLE :$146. STATUT :$46.;
run;

/* REF_EFFET : 11 lignes dans le jeu de test */
data ref.REF_EFFET;
  infile "&root./referentiels/REF_EFFET.csv" dsd firstobs=2 truncover lrecl=32767 encoding="utf-8";
  length EFFET_CODE $12 LIBELLE $80 ORDRE 8 GROUPE $36;
  input EFFET_CODE :$12. LIBELLE :$80. ORDRE :best32. GROUPE :$36.;
run;

/* REF_DRIVER : 7 lignes dans le jeu de test */
data ref.REF_DRIVER;
  infile "&root./referentiels/REF_DRIVER.csv" dsd firstobs=2 truncover lrecl=32767 encoding="utf-8";
  length DRIVER_ID $8 LIBELLE $36;
  input DRIVER_ID :$8. LIBELLE :$36.;
run;

/* REF_FEU : 4 lignes dans le jeu de test */
data ref.REF_FEU;
  infile "&root./referentiels/REF_FEU.csv" dsd firstobs=2 truncover lrecl=32767 encoding="utf-8";
  length FEU $16 ORDRE 8 COULEUR $18;
  input FEU :$16. ORDRE :best32. COULEUR :$18.;
run;

/* REF_MVT : 3 lignes dans le jeu de test */
data ref.REF_MVT;
  infile "&root./referentiels/REF_MVT.csv" dsd firstobs=2 truncover lrecl=32767 encoding="utf-8";
  length CODE $12 LIBELLE $34 ORDRE 8;
  input CODE :$12. LIBELLE :$34. ORDRE :best32.;
run;

/* REF_SCHEMA : 53 lignes dans le jeu de test */
data ref.REF_SCHEMA;
  infile "&root./referentiels/REF_SCHEMA.csv" dsd firstobs=2 truncover lrecl=32767 encoding="utf-8";
  length PAQUET $22 FICHIER_RACINE $36 COLONNE $40 ORDRE 8 TYPE_ATTENDU $8 OBLIGATOIRE $8 CLE $8;
  input PAQUET :$22. FICHIER_RACINE :$36. COLONNE :$40. ORDRE :best32. TYPE_ATTENDU :$8.
    OBLIGATOIRE :$8. CLE :$8.;
run;

/* REF_CDE_VARIABLE : 25 lignes dans le jeu de test */
data ref.REF_CDE_VARIABLE;
  infile "&root./referentiels/REF_CDE_VARIABLE.csv" dsd firstobs=2 truncover lrecl=32767 encoding="utf-8";
  length CDE_ID $16 TABLE_SOURCE $16 VARIABLE $32 TYPE_VAR $8;
  input CDE_ID :$16. TABLE_SOURCE :$16. VARIABLE :$32. TYPE_VAR :$8.;
run;

/* REF_LINEAGE : 150 lignes dans le jeu de test */
data ref.REF_LINEAGE;
  infile "&root./referentiels/REF_LINEAGE.csv" dsd firstobs=2 truncover lrecl=32767 encoding="utf-8";
  length CDE_ID $16 HOP 8 COUCHE $26 OBJET $158 TRANSFORMATION $218 PROGRAMME $54 STATUT $26
    DECISIONS $40;
  input CDE_ID :$16. HOP :best32. COUCHE :$26. OBJET :$158. TRANSFORMATION :$218.
    PROGRAMME :$54. STATUT :$26. DECISIONS :$40.;
run;

/* REF_DECISION : 57 lignes dans le jeu de test */
data ref.REF_DECISION;
  infile "&root./referentiels/REF_DECISION.csv" dsd firstobs=2 truncover lrecl=32767 encoding="utf-8";
  length DECISION_ID $10 SUJET $76 STATUT_CDC $42 DETAIL $212;
  input DECISION_ID :$10. SUJET :$76. STATUT_CDC :$42. DETAIL :$212.;
run;

/* 2. Parametres valides a chaque arrete ---------------------------------------
   On croise chaque table de parametres avec les deux arretes du run et on ne
   garde que la ligne dont la plage de validite contient la date d'arrete. */
data work.arretes;
  N_PERD = &am1.; ASOF = &asof_m1.; output;
  N_PERD = &am.;  ASOF = &asof_m.;  output;
run;

proc sql;
  create table dq.PARAM_LGDD as
  select a.N_PERD, p.MODELE, p.HLC, p.LGDD, p.FL, p.VERSION
  from work.arretes as a, ref.REF_LGDD as p
  where a.ASOF between p.VALID_FROM and p.VALID_TO;

  create table dq.PARAM_TT as
  select a.N_PERD, p.MODELE, p.TYPRIS, p.TAUX_TIRAGE, p.VERSION
  from work.arretes as a, ref.REF_TAUX_TIRAGE as p
  where a.ASOF between p.VALID_FROM and p.VALID_TO;

  create table dq.PARAM_PGE as
  select a.N_PERD, p.C_NOMNC_ENGGM, p.TYPRIS, p.TAUX_FORFAIT, p.VERSION
  from work.arretes as a, ref.REF_PGE_TAUX as p
  where a.ASOF between p.VALID_FROM and p.VALID_TO;
quit;

/* 3. Controles des referentiels -------------------------------------------------
   Chaque controle donne VALEUR (observe), ATTENDU et STATUT (PASS / FAIL / WARN). */
proc sql;
  create table work.chk1 as
  select N_PERD, 'LGDD_48_HLC' as CONTROLE length=40,
         count(*) as VALEUR, 48 as ATTENDU,
         case when count(*) = 48 then 'PASS' else 'FAIL' end as STATUT length=8
  from dq.PARAM_LGDD
  where LGDD is not null and FL is not null
  group by N_PERD;

  create table work.chk2 as
  select N_PERD, 'TAUX_TIRAGE_8_CASES' as CONTROLE length=40,
         count(*) as VALEUR, 8 as ATTENDU,
         case when count(*) = 8 then 'PASS' else 'FAIL' end as STATUT length=8
  from dq.PARAM_TT
  where TAUX_TIRAGE is not null
  group by N_PERD;

  create table work.chk3 as
  select N_PERD, 'TAUX_PGE_4_CASES' as CONTROLE length=40,
         count(*) as VALEUR, 4 as ATTENDU,
         case when count(*) = 4 then 'PASS' else 'WARN' end as STATUT length=8
  from dq.PARAM_PGE
  where TAUX_FORFAIT is not null
  group by N_PERD;
quit;

data dq.DQ_REF_CHECK;
  length RUN_ID $20;
  set work.chk1 work.chk2 work.chk3;
  RUN_ID = "&run_id.";
run;

/* Un referentiel LGDD ou taux de tirage incomplet arrete le run (rien n'est invente). */
data _null_;
  set dq.DQ_REF_CHECK;
  if STATUT = 'FAIL' then do;
    put 'ERROR: referentiel incomplet : ' CONTROLE= N_PERD= VALEUR= ATTENDU=;
    abort cancel;
  end;
  if STATUT = 'WARN' then put 'WARNING: referentiel incomplet : ' CONTROLE= N_PERD= VALEUR= ATTENDU=;
run;

/* 4. Journal ------------------------------------------------------------------ */
%let step_id  = DQ_00_PARAMETRES;
%let step_msg = Referentiels charges et valides;
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

/* S0.5 Seuils approuves en variables macro s_<SEUIL_ID> (ref.REF_SEUIL) */
data _null_;
  set ref.REF_SEUIL;
  call symputx('s_' || strip(SEUIL_ID), VALEUR);
run;


/* SECTION 2 : COLLECTE */
/******************************************************************************
 Section 2.1 (ex-programme DQ_01_COLLECTE_SNAPSHOT)
 Etape     : 1 - Sources et paquets mensuels
 Objet     : Lire les cinq paquets de chaque arrete (M-1 et M) en copie brute
             (RAW_*), puis les harmoniser (STG_*) sans rien corriger en silence.
             Construire le manifeste d'entree et l'en-tete du run.
 Paquets   : CREDIT (contrats en defaut), GARANTIES (liens contrat-garantie),
             EVT_PERTE (evenements de passage en perte, RE9EFSAA),
             PROD_HLC (HLC et drivers de production, vBCR_HISTO_HLC_HIS),
             PROD_ECL (provision de production FLL_PROV).
 Entrees   : &root./entrees/<FICHIER>_AAAAMM.csv ; MANIFEST_AAAAMM.csv
 Sorties   : raw.RAW_MANIFEST, raw.RAW_<PAQUET>
             stg.STG_CREDIT_SNAPSHOT (contrat x arrete), stg.STG_COLLATERAL_LINK
             (contrat x garantie), stg.STG_WO_EVENT (contrat x evenement),
             stg.STG_PRODUCTION_HLC, stg.STG_PRODUCTION_ECL
             dq.DQ_INPUT_MANIFEST (fichier x run), dq.DQ_RUN_HEADER (run)
 Regles    : - le manifeste est lu avant le contenu ;
             - l'en-tete d'un fichier doit etre exactement celui du contrat
               d'interface (sinon fichier rejete, aucune ligne lue) ;
             - la valeur brute est conservee ; l'harmonisation se limite a
               majuscules, suppression des blancs et blanc -> manquant ;
             - une cle dupliquee n'est pas supprimee : KEY_OCC (rang) et DUP_KEY
               (1 si la cle apparait plusieurs fois) la signalent ;
             - les tables a relation un-a-plusieurs (garanties, evenements) sont
               conservees a part : aucune jointure multiplicative.
 Version   : v1.2
******************************************************************************/
%let t0 = %sysfunc(datetime());   /* heure de debut de la section */

/* 1. Manifeste des paquets deposes ------------------------------------------
   Une ligne par fichier : nombre de lignes attendu, somme de controle, empreinte. */
data raw.RAW_MANIFEST;
  length RUN_ID $20 SOURCE_FILE $200 N_PERD 8 PAQUET $12 FICHIER $60 NB_LIGNES 8
         SOMME_CONTROLE 8 EMPREINTE $32 VERSION_SCHEMA $8;
  retain RUN_ID "&run_id.";
  array per[2] _temporary_ (&am1. &am.);
  do i = 1 to 2;
    SOURCE_FILE = cats("&root./entrees/MANIFEST_", per[i], ".csv");
    if fileexist(SOURCE_FILE) then do;
      infile dummy filevar=SOURCE_FILE dsd truncover end=eof lrecl=1024;
      input;                                          /* ligne d'en-tete */
      do while (not eof);
        input N_PERD :best32. PAQUET :$12. FICHIER :$60. NB_LIGNES :best32.
              SOMME_CONTROLE :best32. EMPREINTE :$32. VERSION_SCHEMA :$8.;
        output;
      end;
    end;
  end;
  stop;                                  /* idiome FILEVAR : arret explicite */
  drop i;
run;

/* 2. Lecture brute des paquets ------------------------------------------------
   Idiome identique pour chaque paquet : pour M-1 puis M, ouvrir le fichier,
   verifier l'en-tete, lire les lignes. HEADER_OK = 0 : fichier ignore.
   Un jeu work.HDR_<PAQUET> memorise, par fichier, l'en-tete et le nombre de lignes. */

/* ---- Paquet CREDIT : fichier CREDIT_AAAAMM.csv ---- */
data raw.RAW_CREDIT (drop=i eof HEADER_OK N_LU FILE_OK) work.HDR_CREDIT (keep=SOURCE_FILE FILE_OK HEADER_OK N_LU);
  length RUN_ID $20 SOURCE_FILE $200 LIGNE_NO 8 ID_CONTRAT $20 CODSGDOS $8 NUM_DOSS $12 I_PRSNE_RP $20 C_ENTITE $8 C_DEVISE $4 TYP_CPTE $8 C_NOMNC_ENGGM $8 SEG_DECL $8 TYPRIS $4;
  retain RUN_ID "&run_id.";
  array per[2] _temporary_ (&am1. &am.);
  do i = 1 to 2;
    SOURCE_FILE = cats("&root./entrees/CREDIT_", per[i], ".csv");
    HEADER_OK = 0; N_LU = 0; LIGNE_NO = 0;
    FILE_OK = fileexist(SOURCE_FILE);
    if FILE_OK then do;
      infile dummy filevar=SOURCE_FILE dsd truncover end=eof lrecl=4096;
      input;                                          /* ligne d'en-tete */
      HEADER_OK = (compress(_infile_, '0D'x) = "ARRETE,ID_CONTRAT,CODSGDOS,DATCRE,NUM_DOSS,I_PRSNE_RP,C_ENTITE,C_DEVISE,TYP_CPTE,B_TIERS_RETAIL,C_NOMNC_ENGGM,SEG_DECL,TYPRIS,DATDECLA,D_PR_EVT,DECLAS_B,DECLAS_H,MTENGB,M_ENCRS_HB");
      do while (not eof);
        input ARRETE :yymmdd10. ID_CONTRAT :$20. CODSGDOS :$8. DATCRE :yymmdd10.
              NUM_DOSS :$12. I_PRSNE_RP :$20. C_ENTITE :$8. C_DEVISE :$4. TYP_CPTE :$8.
              B_TIERS_RETAIL :best32. C_NOMNC_ENGGM :$8. SEG_DECL :$8. TYPRIS :$4.
              DATDECLA :yymmdd10. D_PR_EVT :yymmdd10. DECLAS_B :best32. DECLAS_H :best32.
              MTENGB :best32. M_ENCRS_HB :best32.;
        LIGNE_NO + 1; N_LU + 1;
        if HEADER_OK then output raw.RAW_CREDIT;
      end;
    end;
    output work.HDR_CREDIT;
  end;
  stop;                                  /* idiome FILEVAR : arret explicite */
run;

/* ---- Paquet GARANTIES : fichier GARANTIES_AAAAMM.csv ---- */
data raw.RAW_GARANTIES (drop=i eof HEADER_OK N_LU FILE_OK) work.HDR_GARANTIES (keep=SOURCE_FILE FILE_OK HEADER_OK N_LU);
  length RUN_ID $20 SOURCE_FILE $200 LIGNE_NO 8 ID_CONTRAT $20 C_NOMNC_GARNT $8 I_PERSN_GARANT $20;
  retain RUN_ID "&run_id.";
  array per[2] _temporary_ (&am1. &am.);
  do i = 1 to 2;
    SOURCE_FILE = cats("&root./entrees/GARANTIES_", per[i], ".csv");
    HEADER_OK = 0; N_LU = 0; LIGNE_NO = 0;
    FILE_OK = fileexist(SOURCE_FILE);
    if FILE_OK then do;
      infile dummy filevar=SOURCE_FILE dsd truncover end=eof lrecl=4096;
      input;                                          /* ligne d'en-tete */
      HEADER_OK = (compress(_infile_, '0D'x) = "ARRETE,ID_CONTRAT,C_NOMNC_GARNT,I_PERSN_GARANT,M_VALO_GARANT,D_VALO,D_ECHEANCE");
      do while (not eof);
        input ARRETE :yymmdd10. ID_CONTRAT :$20. C_NOMNC_GARNT :$8. I_PERSN_GARANT :$20.
              M_VALO_GARANT :best32. D_VALO :yymmdd10. D_ECHEANCE :yymmdd10.;
        LIGNE_NO + 1; N_LU + 1;
        if HEADER_OK then output raw.RAW_GARANTIES;
      end;
    end;
    output work.HDR_GARANTIES;
  end;
  stop;                                  /* idiome FILEVAR : arret explicite */
run;

/* ---- Paquet EVT_PERTE : fichier EVENEMENTS_PERTE_AAAAMM.csv ---- */
data raw.RAW_EVT_PERTE (drop=i eof HEADER_OK N_LU FILE_OK) work.HDR_EVT_PERTE (keep=SOURCE_FILE FILE_OK HEADER_OK N_LU);
  length RUN_ID $20 SOURCE_FILE $200 LIGNE_NO 8 ID_CONTRAT $20 TYP_OUP $2 TYP_MVT $4;
  retain RUN_ID "&run_id.";
  array per[2] _temporary_ (&am1. &am.);
  do i = 1 to 2;
    SOURCE_FILE = cats("&root./entrees/EVENEMENTS_PERTE_", per[i], ".csv");
    HEADER_OK = 0; N_LU = 0; LIGNE_NO = 0;
    FILE_OK = fileexist(SOURCE_FILE);
    if FILE_OK then do;
      infile dummy filevar=SOURCE_FILE dsd truncover end=eof lrecl=4096;
      input;                                          /* ligne d'en-tete */
      HEADER_OK = (compress(_infile_, '0D'x) = "ARRETE,ID_CONTRAT,D_EVT,MTFLUX,TYP_OUP,TYP_MVT,B_ANNUL");
      do while (not eof);
        input ARRETE :yymmdd10. ID_CONTRAT :$20. D_EVT :yymmdd10. MTFLUX :best32.
              TYP_OUP :$2. TYP_MVT :$4. B_ANNUL :best32.;
        LIGNE_NO + 1; N_LU + 1;
        if HEADER_OK then output raw.RAW_EVT_PERTE;
      end;
    end;
    output work.HDR_EVT_PERTE;
  end;
  stop;                                  /* idiome FILEVAR : arret explicite */
run;

/* ---- Paquet PROD_HLC : fichier PRODUCTION_HLC_AAAAMM.csv ---- */
data raw.RAW_PROD_HLC (drop=i eof HEADER_OK N_LU FILE_OK) work.HDR_PROD_HLC (keep=SOURCE_FILE FILE_OK HEADER_OK N_LU);
  length RUN_ID $20 SOURCE_FILE $200 LIGNE_NO 8 ID_CONTRAT $20 MODELE_PROD $8 NIV_RISQUE_PROD $4 CAT_CAV_PROD $8 VERSION_REGLE_PROD $12;
  retain RUN_ID "&run_id.";
  array per[2] _temporary_ (&am1. &am.);
  do i = 1 to 2;
    SOURCE_FILE = cats("&root./entrees/PRODUCTION_HLC_", per[i], ".csv");
    HEADER_OK = 0; N_LU = 0; LIGNE_NO = 0;
    FILE_OK = fileexist(SOURCE_FILE);
    if FILE_OK then do;
      infile dummy filevar=SOURCE_FILE dsd truncover end=eof lrecl=4096;
      input;                                          /* ligne d'en-tete */
      HEADER_OK = (compress(_infile_, '0D'x) = "ARRETE,ID_CONTRAT,HLC_PROD,MODELE_PROD,TID_PROD,NIV_RISQUE_PROD,TOP_APUR_PROD,GARANTIE_PROD,CRELOG_PROD,ENCOURS_PROD,DECLAS_PROD,CAT_CAV_PROD,VERSION_REGLE_PROD");
      do while (not eof);
        input ARRETE :yymmdd10. ID_CONTRAT :$20. HLC_PROD :best32. MODELE_PROD :$8.
              TID_PROD :best32. NIV_RISQUE_PROD :$4. TOP_APUR_PROD :best32.
              GARANTIE_PROD :best32. CRELOG_PROD :best32. ENCOURS_PROD :best32.
              DECLAS_PROD :best32. CAT_CAV_PROD :$8. VERSION_REGLE_PROD :$12.;
        LIGNE_NO + 1; N_LU + 1;
        if HEADER_OK then output raw.RAW_PROD_HLC;
      end;
    end;
    output work.HDR_PROD_HLC;
  end;
  stop;                                  /* idiome FILEVAR : arret explicite */
run;

/* ---- Paquet PROD_ECL : fichier PRODUCTION_ECL_AAAAMM.csv ---- */
data raw.RAW_PROD_ECL (drop=i eof HEADER_OK N_LU FILE_OK) work.HDR_PROD_ECL (keep=SOURCE_FILE FILE_OK HEADER_OK N_LU);
  length RUN_ID $20 SOURCE_FILE $200 LIGNE_NO 8 ID_CONTRAT $20 VERSION_MOTEUR $20;
  retain RUN_ID "&run_id.";
  array per[2] _temporary_ (&am1. &am.);
  do i = 1 to 2;
    SOURCE_FILE = cats("&root./entrees/PRODUCTION_ECL_", per[i], ".csv");
    HEADER_OK = 0; N_LU = 0; LIGNE_NO = 0;
    FILE_OK = fileexist(SOURCE_FILE);
    if FILE_OK then do;
      infile dummy filevar=SOURCE_FILE dsd truncover end=eof lrecl=4096;
      input;                                          /* ligne d'en-tete */
      HEADER_OK = (compress(_infile_, '0D'x) = "ARRETE,ID_CONTRAT,FLL_PROV,LGDD_PROD,TAUX_TIRAGE_PROD,FL_PROD,VERSION_MOTEUR");
      do while (not eof);
        input ARRETE :yymmdd10. ID_CONTRAT :$20. FLL_PROV :best32. LGDD_PROD :best32.
              TAUX_TIRAGE_PROD :best32. FL_PROD :best32. VERSION_MOTEUR :$20.;
        LIGNE_NO + 1; N_LU + 1;
        if HEADER_OK then output raw.RAW_PROD_ECL;
      end;
    end;
    output work.HDR_PROD_ECL;
  end;
  stop;                                  /* idiome FILEVAR : arret explicite */
run;

/* 3. Harmonisation : tables STG_* ----------------------------------------------
   Regles : majuscules, suppression des blancs de bord, blanc -> manquant (deja
   le cas pour les numeriques et les dates). Aucune autre correction.
   N_PERD = annee*100 + mois de l'arrete ; cle contrat = ARRETE x ID_CONTRAT. */

/* 3.1 Contrats en defaut. On trie pour ranger les doublons de cle. */
proc sort data=raw.RAW_CREDIT out=work.credit1; by ARRETE ID_CONTRAT LIGNE_NO; run;

data stg.STG_CREDIT_SNAPSHOT;
  set work.credit1;
  by ARRETE ID_CONTRAT;
  N_PERD = year(ARRETE) * 100 + month(ARRETE);
  ID_CONTRAT    = upcase(strip(ID_CONTRAT));
  CODSGDOS      = upcase(strip(CODSGDOS));
  NUM_DOSS      = upcase(strip(NUM_DOSS));
  I_PRSNE_RP    = upcase(strip(I_PRSNE_RP));
  C_ENTITE      = upcase(strip(C_ENTITE));
  C_DEVISE      = upcase(strip(C_DEVISE));
  TYP_CPTE      = upcase(strip(TYP_CPTE));
  C_NOMNC_ENGGM = upcase(strip(C_NOMNC_ENGGM));
  SEG_DECL      = upcase(strip(SEG_DECL));
  TYPRIS        = upcase(strip(TYPRIS));
  /* rang et indicateur de doublon de cle (arrete x contrat) */
  if first.ID_CONTRAT then KEY_OCC = 0;
  KEY_OCC + 1;
  DUP_KEY = (not (first.ID_CONTRAT and last.ID_CONTRAT));
  /* identifiant du dossier BCR : CODSGDOS - DATCRE (AAAAMMJJ) - NUM_DOSS */
  length DOSSIER_ID $40;
  if CODSGDOS ne '' and DATCRE ne . and NUM_DOSS ne '' then
    DOSSIER_ID = catx('-', CODSGDOS, put(DATCRE, yymmddn8.), NUM_DOSS);
  format ARRETE DATCRE DATDECLA D_PR_EVT yymmdd10.;
run;

/* 3.2 Liens contrat-garantie : conserves au grain lien (jamais agreges ici). */
data stg.STG_COLLATERAL_LINK;
  set raw.RAW_GARANTIES;
  N_PERD = year(ARRETE) * 100 + month(ARRETE);
  ID_CONTRAT     = upcase(strip(ID_CONTRAT));
  C_NOMNC_GARNT  = upcase(strip(C_NOMNC_GARNT));     /* vide = pas de garantie (EdB) */
  I_PERSN_GARANT = strip(I_PERSN_GARANT);
  format ARRETE D_VALO D_ECHEANCE yymmdd10.;
run;

/* 3.3 Evenements de perte : conserves au grain evenement. */
data stg.STG_WO_EVENT;
  set raw.RAW_EVT_PERTE;
  N_PERD = year(ARRETE) * 100 + month(ARRETE);
  ID_CONTRAT = upcase(strip(ID_CONTRAT));
  TYP_OUP    = upcase(strip(TYP_OUP));
  TYP_MVT    = strip(TYP_MVT);                       /* texte : '03' garde son zero */
  format ARRETE D_EVT yymmdd10.;
run;

/* 3.4 Production : HLC et drivers stockes, provision FLL_PROV. */
data stg.STG_PRODUCTION_HLC;
  set raw.RAW_PROD_HLC;
  N_PERD = year(ARRETE) * 100 + month(ARRETE);
  ID_CONTRAT      = upcase(strip(ID_CONTRAT));
  MODELE_PROD     = upcase(strip(MODELE_PROD));
  NIV_RISQUE_PROD = strip(NIV_RISQUE_PROD);
  CAT_CAV_PROD    = upcase(strip(CAT_CAV_PROD));
  format ARRETE yymmdd10.;
run;

data stg.STG_PRODUCTION_ECL;
  set raw.RAW_PROD_ECL;
  N_PERD = year(ARRETE) * 100 + month(ARRETE);
  ID_CONTRAT = upcase(strip(ID_CONTRAT));
  format ARRETE yymmdd10.;
run;

/* 4. Manifeste d'entree : comparaison du manifeste depose et de la lecture ------
   STATUT : ACCEPTED | REJECTED_ABSENT (fichier introuvable) |
            REJECTED_SCHEMA (en-tete different du contrat) |
            REJECTED_VOLUME (lignes lues differentes du manifeste). */
data work.hdr_all;
  length PAQUET $12;
  set work.HDR_CREDIT (in=a) work.HDR_GARANTIES (in=b) work.HDR_EVT_PERTE (in=c)
      work.HDR_PROD_HLC (in=d) work.HDR_PROD_ECL (in=e);
  if a then PAQUET = 'CREDIT';
  if b then PAQUET = 'GARANTIES';
  if c then PAQUET = 'EVT_PERTE';
  if d then PAQUET = 'PROD_HLC';
  if e then PAQUET = 'PROD_ECL';
  /* periode lue dans le nom du fichier : les six derniers chiffres avant .csv */
  N_PERD = input(substr(SOURCE_FILE, length(SOURCE_FILE) - 9, 6), best6.);
run;

proc sql;
  create table dq.DQ_INPUT_MANIFEST as
  select "&run_id." as RUN_ID length=20,
         m.N_PERD, m.PAQUET, m.FICHIER, m.NB_LIGNES as NB_LIGNES_MANIFESTE,
         h.N_LU as NB_LIGNES_LUES, m.SOMME_CONTROLE, m.EMPREINTE, m.VERSION_SCHEMA,
         h.HEADER_OK,
         case when h.PAQUET is null or h.FILE_OK = 0 then 'REJECTED_ABSENT'
              when h.HEADER_OK = 0 then 'REJECTED_SCHEMA'
              when h.N_LU ne m.NB_LIGNES then 'REJECTED_VOLUME'
              else 'ACCEPTED' end as STATUT length=16
  from raw.RAW_MANIFEST as m
       left join work.hdr_all as h
       on m.N_PERD = h.N_PERD and m.PAQUET = h.PAQUET;
quit;

/* 5. En-tete du run ------------------------------------------------------------ */
data dq.DQ_RUN_HEADER;
  length RUN_ID $20 CODE_VERSION $8 SCHEMA_VERSION $8 PARAMETER_SET_ID $40;
  RUN_ID = "&run_id.";
  N_PERD_M = &am.; N_PERD_M1 = &am1.;
  ASOF_M = &asof_m.; ASOF_M1 = &asof_m1.;
  CODE_VERSION = "&code_version."; SCHEMA_VERSION = "&schema_version.";
  PARAMETER_SET_ID = cats('PS_', &am.);
  DATE_LANCEMENT = datetime();
  format ASOF_M ASOF_M1 yymmdd10. DATE_LANCEMENT datetime19.;
run;

%let step_id  = DQ_01_COLLECTE;
%let step_msg = Paquets lus et harmonises (M-1 et M);
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
