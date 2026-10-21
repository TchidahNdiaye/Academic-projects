/******************************************************************************
 Programme : DQ_01_Exploration_dataset_consolide.sas
 Objet     : ANALYSE EXPLORATOIRE du dataset consolide, sur UN arrete.
             A lancer AVANT le diagnostic DQ (script 02) : on comprend le
             dataset (structure, grain, valeurs, ecarts entre sources) avant de
             fixer les domaines de valeurs et les seuils des regles.
 Entree    : out.extraction_DWR_&periode.
 Sortie    : tables WORK.eda_* + rapport Excel (ODS) pour partage avec les
             data owners.
 Style     : code lineaire, PAS de macro (hors %let de parametres).
             Chaque bloc = une question que l'on se pose sur les donnees.
******************************************************************************/

/*---------------------------------------------------------------------------
  0. PARAMETRES  (seules lignes a modifier)
---------------------------------------------------------------------------*/
%let periode = 202609;                         /* arrete traite (AAAAMM)        */
%let ds      = out.extraction_DWR_&periode.;   /* table consolidee              */
%let rapport = /chemin/vers/EDA_&periode..xlsx;/* rapport Excel                 */

ods excel file="&rapport." options(sheet_interval="proc" embedded_titles="yes");
options nodate nonumber;

/*---------------------------------------------------------------------------
  1. STRUCTURE : quelles variables, quels types, quelles longueurs ?
     -> permet de verifier que le mapping du lignage est bien dans la table
---------------------------------------------------------------------------*/
title "1. Structure de la table consolidee";
proc contents data=&ds. varnum;
run;

/*---------------------------------------------------------------------------
  2. VOLUMETRIE ET GRAIN : combien de lignes, de contrats, de dossiers ?
     Hypothese a verifier : 1 ligne = 1 contrat (I_CNTRT_SIEGE-C_CNTRT_TYPE-N_CNTRT)
     et plusieurs contrats par dossier (I_DOSS_UNIQ).
---------------------------------------------------------------------------*/
data work.eda_base;
  set &ds.;
  length contrat_id $60;
  /* cle contrat construite SANS masquer les composants manquants */
  if nmiss(I_CNTRT_SIEGE, N_CNTRT) = 0 and not missing(C_CNTRT_TYPE) then
    contrat_id = catx('-', put(I_CNTRT_SIEGE, best20. -L), C_CNTRT_TYPE, put(N_CNTRT, best20. -L));
run;

title "2a. Volumetrie";
proc sql;
  select count(*)                          as nb_lignes,
         count(distinct contrat_id)        as nb_contrats_distincts,
         count(distinct I_DOSS_UNIQ)       as nb_dossiers_distincts,
         sum(missing(contrat_id))          as nb_cle_contrat_incomplete,
         sum(M_BIL_ENCOURS_PERIODE)        as encours_bilan_total format=comma20.
  from work.eda_base;
quit;

title "2b. Doublons de cle contrat (le grain est-il respecte ?)";
proc sql;
  create table work.eda_doublons as
  select contrat_id, count(*) as nb_lignes
  from work.eda_base
  where contrat_id ne ''
  group by contrat_id
  having count(*) > 1;
  select count(*) as nb_cles_en_doublon from work.eda_doublons;
quit;

title "2c. Nombre de contrats par dossier";
proc sql;
  create table work.eda_contrats_par_dossier as
  select I_DOSS_UNIQ, count(*) as nb_contrats from work.eda_base group by I_DOSS_UNIQ;
quit;
proc freq data=work.eda_contrats_par_dossier;
  tables nb_contrats / missing;
run;

/*---------------------------------------------------------------------------
  3. VALEURS MANQUANTES : quelles variables sont vides, et a quel point ?
     (dimension "Exhaustivite")
---------------------------------------------------------------------------*/
/* Table unique "une ligne par variable" : type, nb de manquants, taux de
   renseignement. C'est la table a reprendre dans le rapport (onglet Manquants). */
proc sql noprint;
  select count(*) into :nb_lignes trimmed from work.eda_base;
quit;

data work.eda_manquants(keep=variable type nb_total nb_manquant taux_renseigne);
  set work.eda_base;
  array num_ {*} _numeric_;
  array chr_ {*} _character_;
  array nm_  {500} _temporary_ (500*0);       /* compteurs de manquants numeriques  */
  array cm_  {500} _temporary_ (500*0);       /* compteurs de manquants caracteres  */
  length variable $64 type $1;
  do k = 1 to dim(num_); if missing(num_{k}) then nm_{k} + 1; end;
  do k = 1 to dim(chr_); if missing(chr_{k}) then cm_{k} + 1; end;
  if _n_ = &nb_lignes. then do;                /* derniere ligne : on ecrit le bilan */
    nb_total = &nb_lignes.;
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

/* Valeurs distinctes de chaque variable (sortie ODS de PROC FREQ) */
ods output nlevels=work.eda_distinct;
proc freq data=work.eda_base nlevels;
  tables _all_ / noprint;
run;

title "3. Manquants par variable (tries du plus vide au plus renseigne)";
proc sort data=work.eda_manquants; by taux_renseigne; run;
proc print data=work.eda_manquants noobs; format taux_renseigne percent8.2; run;

/*---------------------------------------------------------------------------
  4. DISTRIBUTIONS NUMERIQUES : valeurs extremes, negatifs, zeros
     (dimension "Validite" : encours negatifs ? durees aberrantes ?)
---------------------------------------------------------------------------*/
title "4a. Distribution des montants et durees";
proc means data=work.eda_base n nmiss min p1 p5 median p95 p99 max mean std sum;
  var M_BIL_ENCOURS_PERIODE M_CNTRT_DECLS_BILAN M_CNTRT_DECLS_HBILAN expo_dwr
      M_ENCOURS_DECLSMT M_PROVS_SOLDE TIME_IN_DEF1 TIME_IN_DEF2;
run;

title "4b. Combien de negatifs et de zeros ?";
proc sql;
  select sum(M_BIL_ENCOURS_PERIODE < 0) as encours_negatifs,
         sum(M_BIL_ENCOURS_PERIODE = 0) as encours_nuls,
         sum(M_CNTRT_DECLS_BILAN < 0)   as declb_negatifs,
         sum(M_CNTRT_DECLS_HBILAN < 0)  as declhb_negatifs,
         sum(expo_dwr < 0)              as expo_dwr_negatives
  from work.eda_base;
quit;

/*---------------------------------------------------------------------------
  5. VARIABLES CATEGORIELLES : quelles modalites existent vraiment ?
     -> sert a FIXER les domaines autorises (R37, R39, R44, R48...)
     Pour Flag_GARAN / Flag_Crelog : lister les modalites de C_GAR_ORIG et I_GAR.
---------------------------------------------------------------------------*/
title "5. Modalites des variables categorielles (avec manquants)";
proc freq data=work.eda_base order=freq;
  tables C_CNTRT_TYPE C_CNTRT_NIV_RSK EDX_CER B_CNTRT_WRITE_OFF FLAG_WO
         C_CNTRT_MODLE C_CNTRT_MODLE_HB final_model
         C_GAR_ORIG I_GAR Flag_GARAN Flag_Crelog CAV_Type STAT_PERIM
         / missing;
run;

/*---------------------------------------------------------------------------
  6. DATES : plages, sentinelle 1901-01-01, dates futures
     (dimensions "Validite" et "Coherence")
---------------------------------------------------------------------------*/
title "6. Dates : min, max et valeurs suspectes";
proc sql;
  select 'Default_date' as variable, min(Default_date) as min_d format=date9., max(Default_date) as max_d format=date9.,
         sum(Default_date = '01JAN1901'd) as nb_1901, sum(Default_date > D_ARRETE) as nb_apres_arrete, nmiss(Default_date) as nb_manquants from work.eda_base
  union all
  select 'D_DEFAU', min(D_DEFAU), max(D_DEFAU), sum(D_DEFAU = '01JAN1901'd), sum(D_DEFAU > D_ARRETE), nmiss(D_DEFAU) from work.eda_base
  union all
  select 'D_DOSSR_CREAT', min(D_DOSSR_CREAT), max(D_DOSSR_CREAT), sum(D_DOSSR_CREAT = '01JAN1901'd), sum(D_DOSSR_CREAT > D_ARRETE), nmiss(D_DOSSR_CREAT) from work.eda_base
  union all
  select 'D_1ER_EVT_DEFAU', min(D_1ER_EVT_DEFAU), max(D_1ER_EVT_DEFAU), sum(D_1ER_EVT_DEFAU = '01JAN1901'd), sum(D_1ER_EVT_DEFAU > D_ARRETE), nmiss(D_1ER_EVT_DEFAU) from work.eda_base
  union all
  select 'D_PHOTO', min(D_PHOTO), max(D_PHOTO), sum(D_PHOTO = '01JAN1901'd), sum(D_PHOTO > D_ARRETE), nmiss(D_PHOTO) from work.eda_base
  union all
  select 'D_ARRETE', min(D_ARRETE), max(D_ARRETE), 0, 0, nmiss(D_ARRETE) from work.eda_base;
quit;

title "6b. Une seule date d'arrete dans la table ?";
proc freq data=work.eda_base;
  tables D_ARRETE D_PHOTO N_PERD / missing;
run;

/*---------------------------------------------------------------------------
  7. ECARTS ENTRE SOURCES REDONDANTES (dimension "Coherence")
     Ces ecarts montrent quelles regles de coherence auront du sens.
---------------------------------------------------------------------------*/
data work.eda_ecarts;
  set work.eda_base;
  ecart_enc_declb = M_BIL_ENCOURS_PERIODE - M_CNTRT_DECLS_BILAN;       /* encours vs declassement   */
  ecart_enc_dwr   = M_BIL_ENCOURS_PERIODE - expo_dwr;                  /* BCR vs DWR                */
  ecart_declb_e   = M_CNTRT_DECLS_BILAN   - M_ENCOURS_DECLSMT;         /* f vs e                    */
  ecart_duree     = TIME_IN_DEF1 * 12 - TIME_IN_DEF2;                  /* annees entamees vs mois   */
  if not missing(Default_date) and Default_date ne '01JAN1901'd then
    duree_recalc_mois = (D_ARRETE - Default_date) / 30.4375;
  ecart_duree_recalc = TIME_IN_DEF2 - duree_recalc_mois;
run;

title "7a. Distribution des ecarts entre sources";
proc means data=work.eda_ecarts n nmiss min p1 median p99 max mean;
  var ecart_enc_declb ecart_enc_dwr ecart_declb_e ecart_duree ecart_duree_recalc;
run;

title "7b. Nombre de contrats avec ecart significatif";
proc sql;
  select sum(abs(ecart_enc_dwr) > 0.01*max(abs(expo_dwr),1)) as ecart_bcr_dwr,
         sum(abs(ecart_declb_e) > 0.01*max(abs(M_CNTRT_DECLS_BILAN),1)) as ecart_f_e,
         sum(abs(ecart_duree_recalc) > 0.5) as ecart_duree_sup_demi_mois
  from work.eda_ecarts;
quit;

/*---------------------------------------------------------------------------
  8. CROISEMENTS : comprendre les liens entre variables
---------------------------------------------------------------------------*/
title "8a. Type de contrat x modele retenu";
proc freq data=work.eda_base; tables C_CNTRT_TYPE * final_model / missing norow nocol nopercent; run;

title "8b. Niveau de risque (1=EDX, 2=CER) x EDX_CER derive";
proc freq data=work.eda_base; tables C_CNTRT_NIV_RSK * EDX_CER / missing norow nocol nopercent; run;

title "8c. Passage en perte x encours";
proc means data=work.eda_base n sum mean; class B_CNTRT_WRITE_OFF; var M_BIL_ENCOURS_PERIODE; run;

title "8d. Contrats Loan IQ (ALT) : modele et encours";
proc means data=work.eda_base n sum; where C_CNTRT_TYPE = 'ALT'; class final_model; var M_BIL_ENCOURS_PERIODE; run;

/*---------------------------------------------------------------------------
  9. CONCENTRATION DES ENCOURS : qui pese dans les taux ponderes ?
     Un taux en encours est tire par les plus gros dossiers.
---------------------------------------------------------------------------*/
title "9. Top 20 dossiers par encours bilan";
proc sql outobs=20;
  select I_DOSS_UNIQ, count(*) as nb_contrats, sum(M_BIL_ENCOURS_PERIODE) as encours format=comma20.
  from work.eda_base group by I_DOSS_UNIQ order by calculated encours desc;
quit;

title;
ods excel close;

/* FIN : lire le rapport, trancher avec les data owners (domaines, bornes,
   modalites de garantie), puis lancer DQ_02_Diagnostic_par_CDE.sas */
