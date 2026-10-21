/******************************************************************************
 Programme : DQ_02_Diagnostic_par_CDE.sas
 Objet     : DIAGNOSTIC DQ des 15 donnees critiques (CDE) sur UN arrete.
             Phase 1, activite 5 (profilage) et activite 10 (classement des
             anomalies par donnee et par dimension) de la note de cadrage.
 Prerequis : avoir lu le rapport de DQ_01_Exploration (domaines, bornes).
 Principe  : code LINEAIRE, sans macro. Un bloc = un controle.
             Chaque bloc indique : la CDE, la variable, la DIMENSION evaluee,
             l'identifiant de regle (note, section 7), la severite.
 Taux      : en nombre (n_ok / n_pop) et en encours (a_ok / a_pop), le poids
             etant M_BIL_ENCOURS_PERIODE (note, section 4.2).
 Dimensions: Exhaustivite | Validite | Exactitude | Coherence | Unicite |
             Integrite | Actualite | Stabilite | Tracabilite
             (voir la matrice CDE x dimension en fin de programme).
 Convention: sans accents dans le code pour eviter les soucis d'encodage.
 Blocs mis en commentaire = CDE non couvertes par la table consolidee
             (1, 4, 7, 15) ou garanties (12, 13) : a activer quand la variable
             existe. Les enlever sans variable fait echouer le bloc.
******************************************************************************/

/*---------------------------------------------------------------------------
  0. PARAMETRES
---------------------------------------------------------------------------*/
%let periode   = 202609;                          /* arrete AAAAMM             */
%let ds        = out.extraction_DWR_&periode.;    /* table consolidee          */
%let kappa     = 0.05;                            /* tolerance R08             */
%let tol_duree = 0.5;                             /* tolerance R34 (mois)      */
%let duree_max = 240;                             /* borne haute duree (mois)  */
%let dir_csv   = /chemin/vers/export_csv;         /* sortie CSV pour Power BI  */
%let max_anom  = 200;                             /* anomalies gardees par regle */

/*---------------------------------------------------------------------------
  1. TABLE DE TRAVAIL : poids, cle contrat, duree recalculee
---------------------------------------------------------------------------*/
data work.base;
  set &ds.;
  length contrat_id $60 NIV_RSK_C $8;
  /* niveau de risque en caractere, quel que soit son type d'origine (1=EDX, 2=CER) */
  NIV_RSK_C = strip(vvalue(C_CNTRT_NIV_RSK));
  /* poids = encours bilan (negatifs ramenes a 0 pour ponderer seulement) */
  w = max(0, coalesce(M_BIL_ENCOURS_PERIODE, 0));
  /* cle contrat */
  if nmiss(I_CNTRT_SIEGE, N_CNTRT) = 0 and not missing(C_CNTRT_TYPE) then
    contrat_id = catx('-', put(I_CNTRT_SIEGE, best20. -L), C_CNTRT_TYPE, put(N_CNTRT, best20. -L));
  /* duree recalculee en mois entre date de defaut et arrete (R34) */
  if not missing(Default_date) and Default_date ne '01JAN1901'd then
    duree_recalc_mois = (D_ARRETE - Default_date) / 30.4375;
run;

/* Table des resultats : une ligne par regle */
proc sql;
  create table work.res
    (arrete num, cde_no num, cde char(40), variable char(60), regle_id char(6),
     dimension char(20), severite char(10), description char(100),
     n_pop num, n_ok num, a_pop num, a_ok num);
  /* Anomalies : les &max_anom. plus gros contrats en anomalie, par regle */
  create table work.anom
    (arrete num, regle_id char(6), contrat_id char(60), I_DOSS_UNIQ char(40), encours num);
quit;


/*============================================================================
  CDE 1 : Perimetre Retail/Corporate
============================================================================*/
/* CDE non couverte actuellement : blocs inactifs, a activer quand la variable est disponible. */

/* R01 | CDE 1 Perimetre Retail/Corporate | variable : B_TIERS_RETAIL
   DIMENSION : EXHAUSTIVITE | severite : Bloquant
   Regle     : B_TIERS_RETAIL renseigne
   Population: 1=1  ->  conforme si : not missing(B_TIERS_RETAIL) */
/* BLOC INACTIF - variable absente de la table consolidee :
proc sql;
  insert into work.res
  select &periode., 1, "Perimetre Retail/Corporate", "B_TIERS_RETAIL", "R01", "Exhaustivite", "Bloquant", "B_TIERS_RETAIL renseigne",
         count(*),
         sum(case when (not missing(B_TIERS_RETAIL)) then 1 else 0 end),
         sum(w),
         sum(case when (not missing(B_TIERS_RETAIL)) then w else 0 end)
  from work.base
  where (1=1);
quit;
proc sql outobs=&max_anom.;
  insert into work.anom
  select &periode., "R01", contrat_id, strip(I_DOSS_UNIQ), M_BIL_ENCOURS_PERIODE
  from work.base
  where (1=1) and not (not missing(B_TIERS_RETAIL))
  order by M_BIL_ENCOURS_PERIODE desc;
quit;
*/


/* R02 | CDE 1 Perimetre Retail/Corporate | variable : B_TIERS_RETAIL
   DIMENSION : VALIDITE | severite : Bloquant
   Regle     : Valeur dans le domaine (a fixer apres EDA)
   Population: not missing(B_TIERS_RETAIL)  ->  conforme si : B_TIERS_RETAIL in ('R','C') */
/* BLOC INACTIF - variable absente de la table consolidee :
proc sql;
  insert into work.res
  select &periode., 1, "Perimetre Retail/Corporate", "B_TIERS_RETAIL", "R02", "Validite", "Bloquant", "Valeur dans le domaine (a fixer apres EDA)",
         count(*),
         sum(case when (B_TIERS_RETAIL in ('R','C')) then 1 else 0 end),
         sum(w),
         sum(case when (B_TIERS_RETAIL in ('R','C')) then w else 0 end)
  from work.base
  where (not missing(B_TIERS_RETAIL));
quit;
proc sql outobs=&max_anom.;
  insert into work.anom
  select &periode., "R02", contrat_id, strip(I_DOSS_UNIQ), M_BIL_ENCOURS_PERIODE
  from work.base
  where (not missing(B_TIERS_RETAIL)) and not (B_TIERS_RETAIL in ('R','C'))
  order by M_BIL_ENCOURS_PERIODE desc;
quit;
*/


/*============================================================================
  CDE 2 : Encours au declassement bilan
============================================================================*/

/* R05 | CDE 2 Encours au declassement bilan | variable : M_CNTRT_DECLS_BILAN
   DIMENSION : EXHAUSTIVITE | severite : Bloquant
   Regle     : Encours declassement bilan renseigne
   Population: 1=1  ->  conforme si : not missing(M_CNTRT_DECLS_BILAN) */
proc sql;
  insert into work.res
  select &periode., 2, "Encours au declassement bilan", "M_CNTRT_DECLS_BILAN", "R05", "Exhaustivite", "Bloquant", "Encours declassement bilan renseigne",
         count(*),
         sum(case when (not missing(M_CNTRT_DECLS_BILAN)) then 1 else 0 end),
         sum(w),
         sum(case when (not missing(M_CNTRT_DECLS_BILAN)) then w else 0 end)
  from work.base
  where (1=1);
quit;
/* Anomalies de R05 : contrats non conformes, les plus gros d'abord.
   OUTOBS limite le nombre de lignes inserees. */
proc sql outobs=&max_anom.;
  insert into work.anom
  select &periode., "R05", contrat_id, strip(I_DOSS_UNIQ), M_BIL_ENCOURS_PERIODE
  from work.base
  where (1=1) and not (not missing(M_CNTRT_DECLS_BILAN))
  order by M_BIL_ENCOURS_PERIODE desc;
quit;


/* R06 | CDE 2 Encours au declassement bilan | variable : M_CNTRT_DECLS_BILAN
   DIMENSION : VALIDITE | severite : Bloquant
   Regle     : Numerique et >= 0
   Population: not missing(M_CNTRT_DECLS_BILAN)  ->  conforme si : M_CNTRT_DECLS_BILAN >= 0 */
proc sql;
  insert into work.res
  select &periode., 2, "Encours au declassement bilan", "M_CNTRT_DECLS_BILAN", "R06", "Validite", "Bloquant", "Numerique et >= 0",
         count(*),
         sum(case when (M_CNTRT_DECLS_BILAN >= 0) then 1 else 0 end),
         sum(w),
         sum(case when (M_CNTRT_DECLS_BILAN >= 0) then w else 0 end)
  from work.base
  where (not missing(M_CNTRT_DECLS_BILAN));
quit;
/* Anomalies de R06 : contrats non conformes, les plus gros d'abord.
   OUTOBS limite le nombre de lignes inserees. */
proc sql outobs=&max_anom.;
  insert into work.anom
  select &periode., "R06", contrat_id, strip(I_DOSS_UNIQ), M_BIL_ENCOURS_PERIODE
  from work.base
  where (not missing(M_CNTRT_DECLS_BILAN)) and not (M_CNTRT_DECLS_BILAN >= 0)
  order by M_BIL_ENCOURS_PERIODE desc;
quit;


/* R08 | CDE 2 Encours au declassement bilan | variable : M_CNTRT_DECLS_BILAN / M_BIL_ENCOURS_PERIODE
   DIMENSION : COHERENCE | severite : Majeur
   Regle     : Encours de periode <= (1+kappa) x declassement
   Population: M_CNTRT_DECLS_BILAN > 0  ->  conforme si : M_BIL_ENCOURS_PERIODE <= (1+&kappa.)*M_CNTRT_DECLS_BILAN */
proc sql;
  insert into work.res
  select &periode., 2, "Encours au declassement bilan", "M_CNTRT_DECLS_BILAN / M_BIL_ENCOURS_PERIODE", "R08", "Coherence", "Majeur", "Encours de periode <= (1+kappa) x declassement",
         count(*),
         sum(case when (M_BIL_ENCOURS_PERIODE <= (1+&kappa.)*M_CNTRT_DECLS_BILAN) then 1 else 0 end),
         sum(w),
         sum(case when (M_BIL_ENCOURS_PERIODE <= (1+&kappa.)*M_CNTRT_DECLS_BILAN) then w else 0 end)
  from work.base
  where (M_CNTRT_DECLS_BILAN > 0);
quit;
/* Anomalies de R08 : contrats non conformes, les plus gros d'abord.
   OUTOBS limite le nombre de lignes inserees. */
proc sql outobs=&max_anom.;
  insert into work.anom
  select &periode., "R08", contrat_id, strip(I_DOSS_UNIQ), M_BIL_ENCOURS_PERIODE
  from work.base
  where (M_CNTRT_DECLS_BILAN > 0) and not (M_BIL_ENCOURS_PERIODE <= (1+&kappa.)*M_CNTRT_DECLS_BILAN)
  order by M_BIL_ENCOURS_PERIODE desc;
quit;


/* N09 | CDE 2 Encours au declassement bilan | variable : M_CNTRT_DECLS_BILAN / M_ENCOURS_DECLSMT
   DIMENSION : COHERENCE | severite : Majeur
   Regle     : Declassement source f = source e (tol. 1 pct)
   Population: not missing(M_ENCOURS_DECLSMT) and not missing(M_CNTRT_DECLS_BILAN)  ->  conforme si : abs(M_CNTRT_DECLS_BILAN - M_ENCOURS_DECLSMT) <= 0.01*max(abs(M_CNTRT_DECLS_BILAN),1) */
proc sql;
  insert into work.res
  select &periode., 2, "Encours au declassement bilan", "M_CNTRT_DECLS_BILAN / M_ENCOURS_DECLSMT", "N09", "Coherence", "Majeur", "Declassement source f = source e (tol. 1 pct)",
         count(*),
         sum(case when (abs(M_CNTRT_DECLS_BILAN - M_ENCOURS_DECLSMT) <= 0.01*max(abs(M_CNTRT_DECLS_BILAN),1)) then 1 else 0 end),
         sum(w),
         sum(case when (abs(M_CNTRT_DECLS_BILAN - M_ENCOURS_DECLSMT) <= 0.01*max(abs(M_CNTRT_DECLS_BILAN),1)) then w else 0 end)
  from work.base
  where (not missing(M_ENCOURS_DECLSMT) and not missing(M_CNTRT_DECLS_BILAN));
quit;
/* Anomalies de N09 : contrats non conformes, les plus gros d'abord.
   OUTOBS limite le nombre de lignes inserees. */
proc sql outobs=&max_anom.;
  insert into work.anom
  select &periode., "N09", contrat_id, strip(I_DOSS_UNIQ), M_BIL_ENCOURS_PERIODE
  from work.base
  where (not missing(M_ENCOURS_DECLSMT) and not missing(M_CNTRT_DECLS_BILAN)) and not (abs(M_CNTRT_DECLS_BILAN - M_ENCOURS_DECLSMT) <= 0.01*max(abs(M_CNTRT_DECLS_BILAN),1))
  order by M_BIL_ENCOURS_PERIODE desc;
quit;


/*============================================================================
  CDE 3 : Encours au declassement hors bilan
============================================================================*/

/* R10 | CDE 3 Encours au declassement hors bilan | variable : M_CNTRT_DECLS_HBILAN
   DIMENSION : EXHAUSTIVITE | severite : Majeur
   Regle     : Renseigne (0 accepte)
   Population: 1=1  ->  conforme si : not missing(M_CNTRT_DECLS_HBILAN) */
proc sql;
  insert into work.res
  select &periode., 3, "Encours au declassement hors bilan", "M_CNTRT_DECLS_HBILAN", "R10", "Exhaustivite", "Majeur", "Renseigne (0 accepte)",
         count(*),
         sum(case when (not missing(M_CNTRT_DECLS_HBILAN)) then 1 else 0 end),
         sum(w),
         sum(case when (not missing(M_CNTRT_DECLS_HBILAN)) then w else 0 end)
  from work.base
  where (1=1);
quit;
/* Anomalies de R10 : contrats non conformes, les plus gros d'abord.
   OUTOBS limite le nombre de lignes inserees. */
proc sql outobs=&max_anom.;
  insert into work.anom
  select &periode., "R10", contrat_id, strip(I_DOSS_UNIQ), M_BIL_ENCOURS_PERIODE
  from work.base
  where (1=1) and not (not missing(M_CNTRT_DECLS_HBILAN))
  order by M_BIL_ENCOURS_PERIODE desc;
quit;


/* R11 | CDE 3 Encours au declassement hors bilan | variable : M_CNTRT_DECLS_HBILAN
   DIMENSION : VALIDITE | severite : Bloquant
   Regle     : >= 0
   Population: not missing(M_CNTRT_DECLS_HBILAN)  ->  conforme si : M_CNTRT_DECLS_HBILAN >= 0 */
proc sql;
  insert into work.res
  select &periode., 3, "Encours au declassement hors bilan", "M_CNTRT_DECLS_HBILAN", "R11", "Validite", "Bloquant", ">= 0",
         count(*),
         sum(case when (M_CNTRT_DECLS_HBILAN >= 0) then 1 else 0 end),
         sum(w),
         sum(case when (M_CNTRT_DECLS_HBILAN >= 0) then w else 0 end)
  from work.base
  where (not missing(M_CNTRT_DECLS_HBILAN));
quit;
/* Anomalies de R11 : contrats non conformes, les plus gros d'abord.
   OUTOBS limite le nombre de lignes inserees. */
proc sql outobs=&max_anom.;
  insert into work.anom
  select &periode., "R11", contrat_id, strip(I_DOSS_UNIQ), M_BIL_ENCOURS_PERIODE
  from work.base
  where (not missing(M_CNTRT_DECLS_HBILAN)) and not (M_CNTRT_DECLS_HBILAN >= 0)
  order by M_BIL_ENCOURS_PERIODE desc;
quit;


/* R12 | CDE 3 Encours au declassement hors bilan | variable : M_CNTRT_DECLS_HBILAN / C_CNTRT_MODLE_HB
   DIMENSION : COHERENCE | severite : Mineur
   Regle     : Hors bilan = 0 si pas de modele hors bilan
   Population: missing(C_CNTRT_MODLE_HB)  ->  conforme si : coalesce(M_CNTRT_DECLS_HBILAN,0) = 0 */
proc sql;
  insert into work.res
  select &periode., 3, "Encours au declassement hors bilan", "M_CNTRT_DECLS_HBILAN / C_CNTRT_MODLE_HB", "R12", "Coherence", "Mineur", "Hors bilan = 0 si pas de modele hors bilan",
         count(*),
         sum(case when (coalesce(M_CNTRT_DECLS_HBILAN,0) = 0) then 1 else 0 end),
         sum(w),
         sum(case when (coalesce(M_CNTRT_DECLS_HBILAN,0) = 0) then w else 0 end)
  from work.base
  where (missing(C_CNTRT_MODLE_HB));
quit;
/* Anomalies de R12 : contrats non conformes, les plus gros d'abord.
   OUTOBS limite le nombre de lignes inserees. */
proc sql outobs=&max_anom.;
  insert into work.anom
  select &periode., "R12", contrat_id, strip(I_DOSS_UNIQ), M_BIL_ENCOURS_PERIODE
  from work.base
  where (missing(C_CNTRT_MODLE_HB)) and not (coalesce(M_CNTRT_DECLS_HBILAN,0) = 0)
  order by M_BIL_ENCOURS_PERIODE desc;
quit;


/*============================================================================
  CDE 4 : Identification personne
============================================================================*/
/* CDE non couverte actuellement : blocs inactifs, a activer quand la variable est disponible. */

/* R14 | CDE 4 Identification personne | variable : ID_RMPM
   DIMENSION : EXHAUSTIVITE | severite : Bloquant
   Regle     : Identifiant personne renseigne
   Population: 1=1  ->  conforme si : not missing(ID_RMPM) */
/* BLOC INACTIF - variable absente de la table consolidee :
proc sql;
  insert into work.res
  select &periode., 4, "Identification personne", "ID_RMPM", "R14", "Exhaustivite", "Bloquant", "Identifiant personne renseigne",
         count(*),
         sum(case when (not missing(ID_RMPM)) then 1 else 0 end),
         sum(w),
         sum(case when (not missing(ID_RMPM)) then w else 0 end)
  from work.base
  where (1=1);
quit;
proc sql outobs=&max_anom.;
  insert into work.anom
  select &periode., "R14", contrat_id, strip(I_DOSS_UNIQ), M_BIL_ENCOURS_PERIODE
  from work.base
  where (1=1) and not (not missing(ID_RMPM))
  order by M_BIL_ENCOURS_PERIODE desc;
quit;
*/


/*============================================================================
  CDE 5 : Identification dossier BCR
============================================================================*/

/* R18 | CDE 5 Identification dossier BCR | variable : I_DOSSR_BCR_SIEGE, D_DOSSR_BCR, N_DOSSR_BCR
   DIMENSION : EXHAUSTIVITE | severite : Bloquant
   Regle     : Les 3 composants de la cle dossier renseignes
   Population: 1=1  ->  conforme si : not missing(I_DOSSR_BCR_SIEGE) and not missing(D_DOSSR_BCR) and not missing(N_DOSSR_BCR) */
proc sql;
  insert into work.res
  select &periode., 5, "Identification dossier BCR", "I_DOSSR_BCR_SIEGE, D_DOSSR_BCR, N_DOSSR_BCR", "R18", "Exhaustivite", "Bloquant", "Les 3 composants de la cle dossier renseignes",
         count(*),
         sum(case when (not missing(I_DOSSR_BCR_SIEGE) and not missing(D_DOSSR_BCR) and not missing(N_DOSSR_BCR)) then 1 else 0 end),
         sum(w),
         sum(case when (not missing(I_DOSSR_BCR_SIEGE) and not missing(D_DOSSR_BCR) and not missing(N_DOSSR_BCR)) then w else 0 end)
  from work.base
  where (1=1);
quit;
/* Anomalies de R18 : contrats non conformes, les plus gros d'abord.
   OUTOBS limite le nombre de lignes inserees. */
proc sql outobs=&max_anom.;
  insert into work.anom
  select &periode., "R18", contrat_id, strip(I_DOSS_UNIQ), M_BIL_ENCOURS_PERIODE
  from work.base
  where (1=1) and not (not missing(I_DOSSR_BCR_SIEGE) and not missing(D_DOSSR_BCR) and not missing(N_DOSSR_BCR))
  order by M_BIL_ENCOURS_PERIODE desc;
quit;


/* R20 | CDE 5 Identification dossier BCR | variable : D_DOSSR_CREAT
   DIMENSION : VALIDITE | severite : Majeur
   Regle     : Date de creation valide (hors 1901) et <= arrete
   Population: not missing(D_DOSSR_CREAT)  ->  conforme si : D_DOSSR_CREAT ne '01JAN1901'd and D_DOSSR_CREAT <= D_ARRETE */
proc sql;
  insert into work.res
  select &periode., 5, "Identification dossier BCR", "D_DOSSR_CREAT", "R20", "Validite", "Majeur", "Date de creation valide (hors 1901) et <= arrete",
         count(*),
         sum(case when (D_DOSSR_CREAT ne '01JAN1901'd and D_DOSSR_CREAT <= D_ARRETE) then 1 else 0 end),
         sum(w),
         sum(case when (D_DOSSR_CREAT ne '01JAN1901'd and D_DOSSR_CREAT <= D_ARRETE) then w else 0 end)
  from work.base
  where (not missing(D_DOSSR_CREAT));
quit;
/* Anomalies de R20 : contrats non conformes, les plus gros d'abord.
   OUTOBS limite le nombre de lignes inserees. */
proc sql outobs=&max_anom.;
  insert into work.anom
  select &periode., "R20", contrat_id, strip(I_DOSS_UNIQ), M_BIL_ENCOURS_PERIODE
  from work.base
  where (not missing(D_DOSSR_CREAT)) and not (D_DOSSR_CREAT ne '01JAN1901'd and D_DOSSR_CREAT <= D_ARRETE)
  order by M_BIL_ENCOURS_PERIODE desc;
quit;


/* N01 | CDE 5 Identification dossier BCR | variable : I_DOSS_UNIQ
   DIMENSION : COHERENCE | severite : Majeur
   Regle     : Identifiant dossier coherent avec ses composants
   Population: not missing(I_DOSS_UNIQ) and not missing(N_DOSSR_BCR)  ->  conforme si : index(strip(I_DOSS_UNIQ), strip(put(N_DOSSR_BCR,best20. -L))) > 0 */
proc sql;
  insert into work.res
  select &periode., 5, "Identification dossier BCR", "I_DOSS_UNIQ", "N01", "Coherence", "Majeur", "Identifiant dossier coherent avec ses composants",
         count(*),
         sum(case when (index(strip(I_DOSS_UNIQ), strip(put(N_DOSSR_BCR,best20. -L))) > 0) then 1 else 0 end),
         sum(w),
         sum(case when (index(strip(I_DOSS_UNIQ), strip(put(N_DOSSR_BCR,best20. -L))) > 0) then w else 0 end)
  from work.base
  where (not missing(I_DOSS_UNIQ) and not missing(N_DOSSR_BCR));
quit;
/* Anomalies de N01 : contrats non conformes, les plus gros d'abord.
   OUTOBS limite le nombre de lignes inserees. */
proc sql outobs=&max_anom.;
  insert into work.anom
  select &periode., "N01", contrat_id, strip(I_DOSS_UNIQ), M_BIL_ENCOURS_PERIODE
  from work.base
  where (not missing(I_DOSS_UNIQ) and not missing(N_DOSSR_BCR)) and not (index(strip(I_DOSS_UNIQ), strip(put(N_DOSSR_BCR,best20. -L))) > 0)
  order by M_BIL_ENCOURS_PERIODE desc;
quit;


/*============================================================================
  CDE 6 : Identification Loan IQ (modalite ALT)
============================================================================*/

/* R22 | CDE 6 Identification Loan IQ (modalite ALT) | variable : C_CNTRT_TYPE
   DIMENSION : VALIDITE | severite : Majeur
   Regle     : Contrat ALT porte au moins un modele
   Population: C_CNTRT_TYPE = 'ALT'  ->  conforme si : not missing(C_CNTRT_MODLE) or not missing(C_CNTRT_MODLE_HB) */
proc sql;
  insert into work.res
  select &periode., 6, "Identification Loan IQ (modalite ALT)", "C_CNTRT_TYPE", "R22", "Validite", "Majeur", "Contrat ALT porte au moins un modele",
         count(*),
         sum(case when (not missing(C_CNTRT_MODLE) or not missing(C_CNTRT_MODLE_HB)) then 1 else 0 end),
         sum(w),
         sum(case when (not missing(C_CNTRT_MODLE) or not missing(C_CNTRT_MODLE_HB)) then w else 0 end)
  from work.base
  where (C_CNTRT_TYPE = 'ALT');
quit;
/* Anomalies de R22 : contrats non conformes, les plus gros d'abord.
   OUTOBS limite le nombre de lignes inserees. */
proc sql outobs=&max_anom.;
  insert into work.anom
  select &periode., "R22", contrat_id, strip(I_DOSS_UNIQ), M_BIL_ENCOURS_PERIODE
  from work.base
  where (C_CNTRT_TYPE = 'ALT') and not (not missing(C_CNTRT_MODLE) or not missing(C_CNTRT_MODLE_HB))
  order by M_BIL_ENCOURS_PERIODE desc;
quit;


/*============================================================================
  CDE 7 : Identification engagement
============================================================================*/
/* CDE non couverte actuellement : blocs inactifs, a activer quand la variable est disponible. */

/* R24 | CDE 7 Identification engagement | variable : C_NOMNC_ENGGM
   DIMENSION : EXHAUSTIVITE | severite : Majeur
   Regle     : Code engagement renseigne
   Population: 1=1  ->  conforme si : not missing(C_NOMNC_ENGGM) */
/* BLOC INACTIF - variable absente de la table consolidee :
proc sql;
  insert into work.res
  select &periode., 7, "Identification engagement", "C_NOMNC_ENGGM", "R24", "Exhaustivite", "Majeur", "Code engagement renseigne",
         count(*),
         sum(case when (not missing(C_NOMNC_ENGGM)) then 1 else 0 end),
         sum(w),
         sum(case when (not missing(C_NOMNC_ENGGM)) then w else 0 end)
  from work.base
  where (1=1);
quit;
proc sql outobs=&max_anom.;
  insert into work.anom
  select &periode., "R24", contrat_id, strip(I_DOSS_UNIQ), M_BIL_ENCOURS_PERIODE
  from work.base
  where (1=1) and not (not missing(C_NOMNC_ENGGM))
  order by M_BIL_ENCOURS_PERIODE desc;
quit;
*/


/*============================================================================
  CDE 8 : Identification contrat (modele)
============================================================================*/

/* R27 | CDE 8 Identification contrat (modele) | variable : C_CNTRT_TYPE
   DIMENSION : EXHAUSTIVITE | severite : Bloquant
   Regle     : Type de contrat renseigne
   Population: 1=1  ->  conforme si : not missing(C_CNTRT_TYPE) */
proc sql;
  insert into work.res
  select &periode., 8, "Identification contrat (modele)", "C_CNTRT_TYPE", "R27", "Exhaustivite", "Bloquant", "Type de contrat renseigne",
         count(*),
         sum(case when (not missing(C_CNTRT_TYPE)) then 1 else 0 end),
         sum(w),
         sum(case when (not missing(C_CNTRT_TYPE)) then w else 0 end)
  from work.base
  where (1=1);
quit;
/* Anomalies de R27 : contrats non conformes, les plus gros d'abord.
   OUTOBS limite le nombre de lignes inserees. */
proc sql outobs=&max_anom.;
  insert into work.anom
  select &periode., "R27", contrat_id, strip(I_DOSS_UNIQ), M_BIL_ENCOURS_PERIODE
  from work.base
  where (1=1) and not (not missing(C_CNTRT_TYPE))
  order by M_BIL_ENCOURS_PERIODE desc;
quit;


/* R28 | CDE 8 Identification contrat (modele) | variable : final_model
   DIMENSION : VALIDITE | severite : Majeur
   Regle     : Type de contrat mappe a un modele
   Population: not missing(C_CNTRT_TYPE)  ->  conforme si : not missing(final_model) */
proc sql;
  insert into work.res
  select &periode., 8, "Identification contrat (modele)", "final_model", "R28", "Validite", "Majeur", "Type de contrat mappe a un modele",
         count(*),
         sum(case when (not missing(final_model)) then 1 else 0 end),
         sum(w),
         sum(case when (not missing(final_model)) then w else 0 end)
  from work.base
  where (not missing(C_CNTRT_TYPE));
quit;
/* Anomalies de R28 : contrats non conformes, les plus gros d'abord.
   OUTOBS limite le nombre de lignes inserees. */
proc sql outobs=&max_anom.;
  insert into work.anom
  select &periode., "R28", contrat_id, strip(I_DOSS_UNIQ), M_BIL_ENCOURS_PERIODE
  from work.base
  where (not missing(C_CNTRT_TYPE)) and not (not missing(final_model))
  order by M_BIL_ENCOURS_PERIODE desc;
quit;


/* N08 | CDE 8 Identification contrat (modele) | variable : C_CNTRT_MODLE / final_model
   DIMENSION : COHERENCE | severite : Majeur
   Regle     : Modele bilan = modele retenu
   Population: not missing(C_CNTRT_MODLE) and not missing(final_model)  ->  conforme si : C_CNTRT_MODLE = final_model */
proc sql;
  insert into work.res
  select &periode., 8, "Identification contrat (modele)", "C_CNTRT_MODLE / final_model", "N08", "Coherence", "Majeur", "Modele bilan = modele retenu",
         count(*),
         sum(case when (C_CNTRT_MODLE = final_model) then 1 else 0 end),
         sum(w),
         sum(case when (C_CNTRT_MODLE = final_model) then w else 0 end)
  from work.base
  where (not missing(C_CNTRT_MODLE) and not missing(final_model));
quit;
/* Anomalies de N08 : contrats non conformes, les plus gros d'abord.
   OUTOBS limite le nombre de lignes inserees. */
proc sql outobs=&max_anom.;
  insert into work.anom
  select &periode., "N08", contrat_id, strip(I_DOSS_UNIQ), M_BIL_ENCOURS_PERIODE
  from work.base
  where (not missing(C_CNTRT_MODLE) and not missing(final_model)) and not (C_CNTRT_MODLE = final_model)
  order by M_BIL_ENCOURS_PERIODE desc;
quit;


/*============================================================================
  CDE 9 : Duree en defaut
============================================================================*/

/* R30 | CDE 9 Duree en defaut | variable : Default_date
   DIMENSION : EXHAUSTIVITE | severite : Bloquant
   Regle     : Date de defaut renseignee (hors 1901)
   Population: 1=1  ->  conforme si : not missing(Default_date) and Default_date ne '01JAN1901'd */
proc sql;
  insert into work.res
  select &periode., 9, "Duree en defaut", "Default_date", "R30", "Exhaustivite", "Bloquant", "Date de defaut renseignee (hors 1901)",
         count(*),
         sum(case when (not missing(Default_date) and Default_date ne '01JAN1901'd) then 1 else 0 end),
         sum(w),
         sum(case when (not missing(Default_date) and Default_date ne '01JAN1901'd) then w else 0 end)
  from work.base
  where (1=1);
quit;
/* Anomalies de R30 : contrats non conformes, les plus gros d'abord.
   OUTOBS limite le nombre de lignes inserees. */
proc sql outobs=&max_anom.;
  insert into work.anom
  select &periode., "R30", contrat_id, strip(I_DOSS_UNIQ), M_BIL_ENCOURS_PERIODE
  from work.base
  where (1=1) and not (not missing(Default_date) and Default_date ne '01JAN1901'd)
  order by M_BIL_ENCOURS_PERIODE desc;
quit;


/* R31 | CDE 9 Duree en defaut | variable : Default_date
   DIMENSION : VALIDITE | severite : Bloquant
   Regle     : Date de defaut <= arrete
   Population: not missing(Default_date) and Default_date ne '01JAN1901'd  ->  conforme si : Default_date <= D_ARRETE */
proc sql;
  insert into work.res
  select &periode., 9, "Duree en defaut", "Default_date", "R31", "Validite", "Bloquant", "Date de defaut <= arrete",
         count(*),
         sum(case when (Default_date <= D_ARRETE) then 1 else 0 end),
         sum(w),
         sum(case when (Default_date <= D_ARRETE) then w else 0 end)
  from work.base
  where (not missing(Default_date) and Default_date ne '01JAN1901'd);
quit;
/* Anomalies de R31 : contrats non conformes, les plus gros d'abord.
   OUTOBS limite le nombre de lignes inserees. */
proc sql outobs=&max_anom.;
  insert into work.anom
  select &periode., "R31", contrat_id, strip(I_DOSS_UNIQ), M_BIL_ENCOURS_PERIODE
  from work.base
  where (not missing(Default_date) and Default_date ne '01JAN1901'd) and not (Default_date <= D_ARRETE)
  order by M_BIL_ENCOURS_PERIODE desc;
quit;


/* R32 | CDE 9 Duree en defaut | variable : Default_date / D_DOSSR_CREAT
   DIMENSION : COHERENCE | severite : Majeur
   Regle     : Date de defaut >= date de creation du dossier
   Population: not missing(Default_date) and not missing(D_DOSSR_CREAT) and Default_date ne '01JAN1901'd and D_DOSSR_CREAT ne '01JAN1901'd  ->  conforme si : Default_date >= D_DOSSR_CREAT */
proc sql;
  insert into work.res
  select &periode., 9, "Duree en defaut", "Default_date / D_DOSSR_CREAT", "R32", "Coherence", "Majeur", "Date de defaut >= date de creation du dossier",
         count(*),
         sum(case when (Default_date >= D_DOSSR_CREAT) then 1 else 0 end),
         sum(w),
         sum(case when (Default_date >= D_DOSSR_CREAT) then w else 0 end)
  from work.base
  where (not missing(Default_date) and not missing(D_DOSSR_CREAT) and Default_date ne '01JAN1901'd and D_DOSSR_CREAT ne '01JAN1901'd);
quit;
/* Anomalies de R32 : contrats non conformes, les plus gros d'abord.
   OUTOBS limite le nombre de lignes inserees. */
proc sql outobs=&max_anom.;
  insert into work.anom
  select &periode., "R32", contrat_id, strip(I_DOSS_UNIQ), M_BIL_ENCOURS_PERIODE
  from work.base
  where (not missing(Default_date) and not missing(D_DOSSR_CREAT) and Default_date ne '01JAN1901'd and D_DOSSR_CREAT ne '01JAN1901'd) and not (Default_date >= D_DOSSR_CREAT)
  order by M_BIL_ENCOURS_PERIODE desc;
quit;


/* R34 | CDE 9 Duree en defaut | variable : TIME_IN_DEF2
   DIMENSION : EXACTITUDE | severite : Majeur
   Regle     : Duree moteur = duree recalculee (+/- 0,5 mois)
   Population: not missing(duree_recalc_mois) and not missing(TIME_IN_DEF2)  ->  conforme si : abs(TIME_IN_DEF2 - duree_recalc_mois) <= &tol_duree. */
proc sql;
  insert into work.res
  select &periode., 9, "Duree en defaut", "TIME_IN_DEF2", "R34", "Exactitude", "Majeur", "Duree moteur = duree recalculee (+/- 0,5 mois)",
         count(*),
         sum(case when (abs(TIME_IN_DEF2 - duree_recalc_mois) <= &tol_duree.) then 1 else 0 end),
         sum(w),
         sum(case when (abs(TIME_IN_DEF2 - duree_recalc_mois) <= &tol_duree.) then w else 0 end)
  from work.base
  where (not missing(duree_recalc_mois) and not missing(TIME_IN_DEF2));
quit;
/* Anomalies de R34 : contrats non conformes, les plus gros d'abord.
   OUTOBS limite le nombre de lignes inserees. */
proc sql outobs=&max_anom.;
  insert into work.anom
  select &periode., "R34", contrat_id, strip(I_DOSS_UNIQ), M_BIL_ENCOURS_PERIODE
  from work.base
  where (not missing(duree_recalc_mois) and not missing(TIME_IN_DEF2)) and not (abs(TIME_IN_DEF2 - duree_recalc_mois) <= &tol_duree.)
  order by M_BIL_ENCOURS_PERIODE desc;
quit;


/* R35 | CDE 9 Duree en defaut | variable : TIME_IN_DEF2
   DIMENSION : VALIDITE | severite : Mineur
   Regle     : Duree entre 0 et la borne haute plausible
   Population: not missing(TIME_IN_DEF2)  ->  conforme si : TIME_IN_DEF2 >= 0 and TIME_IN_DEF2 <= &duree_max. */
proc sql;
  insert into work.res
  select &periode., 9, "Duree en defaut", "TIME_IN_DEF2", "R35", "Validite", "Mineur", "Duree entre 0 et la borne haute plausible",
         count(*),
         sum(case when (TIME_IN_DEF2 >= 0 and TIME_IN_DEF2 <= &duree_max.) then 1 else 0 end),
         sum(w),
         sum(case when (TIME_IN_DEF2 >= 0 and TIME_IN_DEF2 <= &duree_max.) then w else 0 end)
  from work.base
  where (not missing(TIME_IN_DEF2));
quit;
/* Anomalies de R35 : contrats non conformes, les plus gros d'abord.
   OUTOBS limite le nombre de lignes inserees. */
proc sql outobs=&max_anom.;
  insert into work.anom
  select &periode., "R35", contrat_id, strip(I_DOSS_UNIQ), M_BIL_ENCOURS_PERIODE
  from work.base
  where (not missing(TIME_IN_DEF2)) and not (TIME_IN_DEF2 >= 0 and TIME_IN_DEF2 <= &duree_max.)
  order by M_BIL_ENCOURS_PERIODE desc;
quit;


/* N07 | CDE 9 Duree en defaut | variable : TIME_IN_DEF1 / TIME_IN_DEF2
   DIMENSION : COHERENCE | severite : Majeur
   Regle     : Version 1 (annees entamees) compatible avec version 2 (mois)
   Population: not missing(TIME_IN_DEF1) and not missing(TIME_IN_DEF2)  ->  conforme si : abs(TIME_IN_DEF1*12 - TIME_IN_DEF2) <= 12 */
proc sql;
  insert into work.res
  select &periode., 9, "Duree en defaut", "TIME_IN_DEF1 / TIME_IN_DEF2", "N07", "Coherence", "Majeur", "Version 1 (annees entamees) compatible avec version 2 (mois)",
         count(*),
         sum(case when (abs(TIME_IN_DEF1*12 - TIME_IN_DEF2) <= 12) then 1 else 0 end),
         sum(w),
         sum(case when (abs(TIME_IN_DEF1*12 - TIME_IN_DEF2) <= 12) then w else 0 end)
  from work.base
  where (not missing(TIME_IN_DEF1) and not missing(TIME_IN_DEF2));
quit;
/* Anomalies de N07 : contrats non conformes, les plus gros d'abord.
   OUTOBS limite le nombre de lignes inserees. */
proc sql outobs=&max_anom.;
  insert into work.anom
  select &periode., "N07", contrat_id, strip(I_DOSS_UNIQ), M_BIL_ENCOURS_PERIODE
  from work.base
  where (not missing(TIME_IN_DEF1) and not missing(TIME_IN_DEF2)) and not (abs(TIME_IN_DEF1*12 - TIME_IN_DEF2) <= 12)
  order by M_BIL_ENCOURS_PERIODE desc;
quit;


/*============================================================================
  CDE 10 : EDX / CER
============================================================================*/

/* R36 | CDE 10 EDX / CER | variable : C_CNTRT_NIV_RSK
   DIMENSION : EXHAUSTIVITE | severite : Majeur
   Regle     : Niveau de risque renseigne
   Population: 1=1  ->  conforme si : not missing(C_CNTRT_NIV_RSK) */
proc sql;
  insert into work.res
  select &periode., 10, "EDX / CER", "C_CNTRT_NIV_RSK", "R36", "Exhaustivite", "Majeur", "Niveau de risque renseigne",
         count(*),
         sum(case when (not missing(C_CNTRT_NIV_RSK)) then 1 else 0 end),
         sum(w),
         sum(case when (not missing(C_CNTRT_NIV_RSK)) then w else 0 end)
  from work.base
  where (1=1);
quit;
/* Anomalies de R36 : contrats non conformes, les plus gros d'abord.
   OUTOBS limite le nombre de lignes inserees. */
proc sql outobs=&max_anom.;
  insert into work.anom
  select &periode., "R36", contrat_id, strip(I_DOSS_UNIQ), M_BIL_ENCOURS_PERIODE
  from work.base
  where (1=1) and not (not missing(C_CNTRT_NIV_RSK))
  order by M_BIL_ENCOURS_PERIODE desc;
quit;


/* R37 | CDE 10 EDX / CER | variable : C_CNTRT_NIV_RSK
   DIMENSION : VALIDITE | severite : Majeur
   Regle     : Valeur dans {1=EDX, 2=CER}
   Population: not missing(C_CNTRT_NIV_RSK)  ->  conforme si : NIV_RSK_C in ('1','2') */
proc sql;
  insert into work.res
  select &periode., 10, "EDX / CER", "C_CNTRT_NIV_RSK", "R37", "Validite", "Majeur", "Valeur dans {1=EDX, 2=CER}",
         count(*),
         sum(case when (NIV_RSK_C in ('1','2')) then 1 else 0 end),
         sum(w),
         sum(case when (NIV_RSK_C in ('1','2')) then w else 0 end)
  from work.base
  where (not missing(C_CNTRT_NIV_RSK));
quit;
/* Anomalies de R37 : contrats non conformes, les plus gros d'abord.
   OUTOBS limite le nombre de lignes inserees. */
proc sql outobs=&max_anom.;
  insert into work.anom
  select &periode., "R37", contrat_id, strip(I_DOSS_UNIQ), M_BIL_ENCOURS_PERIODE
  from work.base
  where (not missing(C_CNTRT_NIV_RSK)) and not (NIV_RSK_C in ('1','2'))
  order by M_BIL_ENCOURS_PERIODE desc;
quit;


/*============================================================================
  CDE 11 : Passage en perte
============================================================================*/

/* R39 | CDE 11 Passage en perte | variable : B_CNTRT_WRITE_OFF
   DIMENSION : VALIDITE | severite : Majeur
   Regle     : Indicateur dans {N, O}
   Population: 1=1  ->  conforme si : B_CNTRT_WRITE_OFF in ('N','O') */
proc sql;
  insert into work.res
  select &periode., 11, "Passage en perte", "B_CNTRT_WRITE_OFF", "R39", "Validite", "Majeur", "Indicateur dans {N, O}",
         count(*),
         sum(case when (B_CNTRT_WRITE_OFF in ('N','O')) then 1 else 0 end),
         sum(w),
         sum(case when (B_CNTRT_WRITE_OFF in ('N','O')) then w else 0 end)
  from work.base
  where (1=1);
quit;
/* Anomalies de R39 : contrats non conformes, les plus gros d'abord.
   OUTOBS limite le nombre de lignes inserees. */
proc sql outobs=&max_anom.;
  insert into work.anom
  select &periode., "R39", contrat_id, strip(I_DOSS_UNIQ), M_BIL_ENCOURS_PERIODE
  from work.base
  where (1=1) and not (B_CNTRT_WRITE_OFF in ('N','O'))
  order by M_BIL_ENCOURS_PERIODE desc;
quit;


/* R40 | CDE 11 Passage en perte | variable : B_CNTRT_WRITE_OFF / M_BIL_ENCOURS_PERIODE
   DIMENSION : COHERENCE | severite : Bloquant
   Regle     : Contrat passe en perte : encours bilan nul
   Population: B_CNTRT_WRITE_OFF = 'O'  ->  conforme si : coalesce(M_BIL_ENCOURS_PERIODE,0) = 0 */
proc sql;
  insert into work.res
  select &periode., 11, "Passage en perte", "B_CNTRT_WRITE_OFF / M_BIL_ENCOURS_PERIODE", "R40", "Coherence", "Bloquant", "Contrat passe en perte : encours bilan nul",
         count(*),
         sum(case when (coalesce(M_BIL_ENCOURS_PERIODE,0) = 0) then 1 else 0 end),
         sum(w),
         sum(case when (coalesce(M_BIL_ENCOURS_PERIODE,0) = 0) then w else 0 end)
  from work.base
  where (B_CNTRT_WRITE_OFF = 'O');
quit;
/* Anomalies de R40 : contrats non conformes, les plus gros d'abord.
   OUTOBS limite le nombre de lignes inserees. */
proc sql outobs=&max_anom.;
  insert into work.anom
  select &periode., "R40", contrat_id, strip(I_DOSS_UNIQ), M_BIL_ENCOURS_PERIODE
  from work.base
  where (B_CNTRT_WRITE_OFF = 'O') and not (coalesce(M_BIL_ENCOURS_PERIODE,0) = 0)
  order by M_BIL_ENCOURS_PERIODE desc;
quit;


/*============================================================================
  CDE 12 : Presence de garantie
============================================================================*/
/* CDE non couverte actuellement : blocs inactifs, a activer quand la variable est disponible. */

/* N11 | CDE 12 Presence de garantie | variable : Flag_GARAN
   DIMENSION : EXHAUSTIVITE | severite : Majeur
   Regle     : Flag garantie renseigne (a construire depuis C_GAR_ORIG / I_GAR)
   Population: 1=1  ->  conforme si : not missing(Flag_GARAN) */
/* BLOC INACTIF - variable absente de la table consolidee :
proc sql;
  insert into work.res
  select &periode., 12, "Presence de garantie", "Flag_GARAN", "N11", "Exhaustivite", "Majeur", "Flag garantie renseigne (a construire depuis C_GAR_ORIG / I_GAR)",
         count(*),
         sum(case when (not missing(Flag_GARAN)) then 1 else 0 end),
         sum(w),
         sum(case when (not missing(Flag_GARAN)) then w else 0 end)
  from work.base
  where (1=1);
quit;
proc sql outobs=&max_anom.;
  insert into work.anom
  select &periode., "N11", contrat_id, strip(I_DOSS_UNIQ), M_BIL_ENCOURS_PERIODE
  from work.base
  where (1=1) and not (not missing(Flag_GARAN))
  order by M_BIL_ENCOURS_PERIODE desc;
quit;
*/


/* R44 | CDE 12 Presence de garantie | variable : Flag_GARAN
   DIMENSION : VALIDITE | severite : Majeur
   Regle     : Flag dans {0,1}
   Population: not missing(Flag_GARAN)  ->  conforme si : Flag_GARAN in ('0','1') */
/* BLOC INACTIF - variable absente de la table consolidee :
proc sql;
  insert into work.res
  select &periode., 12, "Presence de garantie", "Flag_GARAN", "R44", "Validite", "Majeur", "Flag dans {0,1}",
         count(*),
         sum(case when (Flag_GARAN in ('0','1')) then 1 else 0 end),
         sum(w),
         sum(case when (Flag_GARAN in ('0','1')) then w else 0 end)
  from work.base
  where (not missing(Flag_GARAN));
quit;
proc sql outobs=&max_anom.;
  insert into work.anom
  select &periode., "R44", contrat_id, strip(I_DOSS_UNIQ), M_BIL_ENCOURS_PERIODE
  from work.base
  where (not missing(Flag_GARAN)) and not (Flag_GARAN in ('0','1'))
  order by M_BIL_ENCOURS_PERIODE desc;
quit;
*/


/*============================================================================
  CDE 13 : Garantie Credit Logement
============================================================================*/
/* CDE non couverte actuellement : blocs inactifs, a activer quand la variable est disponible. */

/* R48 | CDE 13 Garantie Credit Logement | variable : Flag_Crelog
   DIMENSION : VALIDITE | severite : Bloquant
   Regle     : Flag dans {0,1}
   Population: not missing(Flag_Crelog)  ->  conforme si : Flag_Crelog in ('0','1') */
/* BLOC INACTIF - variable absente de la table consolidee :
proc sql;
  insert into work.res
  select &periode., 13, "Garantie Credit Logement", "Flag_Crelog", "R48", "Validite", "Bloquant", "Flag dans {0,1}",
         count(*),
         sum(case when (Flag_Crelog in ('0','1')) then 1 else 0 end),
         sum(w),
         sum(case when (Flag_Crelog in ('0','1')) then w else 0 end)
  from work.base
  where (not missing(Flag_Crelog));
quit;
proc sql outobs=&max_anom.;
  insert into work.anom
  select &periode., "R48", contrat_id, strip(I_DOSS_UNIQ), M_BIL_ENCOURS_PERIODE
  from work.base
  where (not missing(Flag_Crelog)) and not (Flag_Crelog in ('0','1'))
  order by M_BIL_ENCOURS_PERIODE desc;
quit;
*/


/* R50 | CDE 13 Garantie Credit Logement | variable : Flag_Crelog / Flag_GARAN
   DIMENSION : COHERENCE | severite : Majeur
   Regle     : Caution CL => garantie presente
   Population: Flag_Crelog = '1'  ->  conforme si : Flag_GARAN = '1' */
/* BLOC INACTIF - variable absente de la table consolidee :
proc sql;
  insert into work.res
  select &periode., 13, "Garantie Credit Logement", "Flag_Crelog / Flag_GARAN", "R50", "Coherence", "Majeur", "Caution CL => garantie presente",
         count(*),
         sum(case when (Flag_GARAN = '1') then 1 else 0 end),
         sum(w),
         sum(case when (Flag_GARAN = '1') then w else 0 end)
  from work.base
  where (Flag_Crelog = '1');
quit;
proc sql outobs=&max_anom.;
  insert into work.anom
  select &periode., "R50", contrat_id, strip(I_DOSS_UNIQ), M_BIL_ENCOURS_PERIODE
  from work.base
  where (Flag_Crelog = '1') and not (Flag_GARAN = '1')
  order by M_BIL_ENCOURS_PERIODE desc;
quit;
*/


/*============================================================================
  CDE 14 : Exposition bilan
============================================================================*/

/* R51 | CDE 14 Exposition bilan | variable : M_BIL_ENCOURS_PERIODE
   DIMENSION : EXHAUSTIVITE | severite : Bloquant
   Regle     : Encours de periode renseigne
   Population: 1=1  ->  conforme si : not missing(M_BIL_ENCOURS_PERIODE) */
proc sql;
  insert into work.res
  select &periode., 14, "Exposition bilan", "M_BIL_ENCOURS_PERIODE", "R51", "Exhaustivite", "Bloquant", "Encours de periode renseigne",
         count(*),
         sum(case when (not missing(M_BIL_ENCOURS_PERIODE)) then 1 else 0 end),
         sum(w),
         sum(case when (not missing(M_BIL_ENCOURS_PERIODE)) then w else 0 end)
  from work.base
  where (1=1);
quit;
/* Anomalies de R51 : contrats non conformes, les plus gros d'abord.
   OUTOBS limite le nombre de lignes inserees. */
proc sql outobs=&max_anom.;
  insert into work.anom
  select &periode., "R51", contrat_id, strip(I_DOSS_UNIQ), M_BIL_ENCOURS_PERIODE
  from work.base
  where (1=1) and not (not missing(M_BIL_ENCOURS_PERIODE))
  order by M_BIL_ENCOURS_PERIODE desc;
quit;


/* R52 | CDE 14 Exposition bilan | variable : M_BIL_ENCOURS_PERIODE
   DIMENSION : VALIDITE | severite : Bloquant
   Regle     : Encours >= 0
   Population: not missing(M_BIL_ENCOURS_PERIODE)  ->  conforme si : M_BIL_ENCOURS_PERIODE >= 0 */
proc sql;
  insert into work.res
  select &periode., 14, "Exposition bilan", "M_BIL_ENCOURS_PERIODE", "R52", "Validite", "Bloquant", "Encours >= 0",
         count(*),
         sum(case when (M_BIL_ENCOURS_PERIODE >= 0) then 1 else 0 end),
         sum(w),
         sum(case when (M_BIL_ENCOURS_PERIODE >= 0) then w else 0 end)
  from work.base
  where (not missing(M_BIL_ENCOURS_PERIODE));
quit;
/* Anomalies de R52 : contrats non conformes, les plus gros d'abord.
   OUTOBS limite le nombre de lignes inserees. */
proc sql outobs=&max_anom.;
  insert into work.anom
  select &periode., "R52", contrat_id, strip(I_DOSS_UNIQ), M_BIL_ENCOURS_PERIODE
  from work.base
  where (not missing(M_BIL_ENCOURS_PERIODE)) and not (M_BIL_ENCOURS_PERIODE >= 0)
  order by M_BIL_ENCOURS_PERIODE desc;
quit;


/* R53 | CDE 14 Exposition bilan | variable : M_BIL_ENCOURS_PERIODE / expo_dwr
   DIMENSION : EXACTITUDE | severite : Bloquant
   Regle     : Encours BCR = exposition DWR (tol. 1 pct)
   Population: not missing(expo_dwr) and not missing(M_BIL_ENCOURS_PERIODE)  ->  conforme si : abs(M_BIL_ENCOURS_PERIODE - expo_dwr) <= 0.01*max(abs(expo_dwr),1) */
proc sql;
  insert into work.res
  select &periode., 14, "Exposition bilan", "M_BIL_ENCOURS_PERIODE / expo_dwr", "R53", "Exactitude", "Bloquant", "Encours BCR = exposition DWR (tol. 1 pct)",
         count(*),
         sum(case when (abs(M_BIL_ENCOURS_PERIODE - expo_dwr) <= 0.01*max(abs(expo_dwr),1)) then 1 else 0 end),
         sum(w),
         sum(case when (abs(M_BIL_ENCOURS_PERIODE - expo_dwr) <= 0.01*max(abs(expo_dwr),1)) then w else 0 end)
  from work.base
  where (not missing(expo_dwr) and not missing(M_BIL_ENCOURS_PERIODE));
quit;
/* Anomalies de R53 : contrats non conformes, les plus gros d'abord.
   OUTOBS limite le nombre de lignes inserees. */
proc sql outobs=&max_anom.;
  insert into work.anom
  select &periode., "R53", contrat_id, strip(I_DOSS_UNIQ), M_BIL_ENCOURS_PERIODE
  from work.base
  where (not missing(expo_dwr) and not missing(M_BIL_ENCOURS_PERIODE)) and not (abs(M_BIL_ENCOURS_PERIODE - expo_dwr) <= 0.01*max(abs(expo_dwr),1))
  order by M_BIL_ENCOURS_PERIODE desc;
quit;


/* N06 | CDE 14 Exposition bilan | variable : expo_dwr
   DIMENSION : VALIDITE | severite : Majeur
   Regle     : Exposition DWR brute >= 0 (avant ecretage)
   Population: not missing(expo_dwr)  ->  conforme si : expo_dwr >= 0 */
proc sql;
  insert into work.res
  select &periode., 14, "Exposition bilan", "expo_dwr", "N06", "Validite", "Majeur", "Exposition DWR brute >= 0 (avant ecretage)",
         count(*),
         sum(case when (expo_dwr >= 0) then 1 else 0 end),
         sum(w),
         sum(case when (expo_dwr >= 0) then w else 0 end)
  from work.base
  where (not missing(expo_dwr));
quit;
/* Anomalies de N06 : contrats non conformes, les plus gros d'abord.
   OUTOBS limite le nombre de lignes inserees. */
proc sql outobs=&max_anom.;
  insert into work.anom
  select &periode., "N06", contrat_id, strip(I_DOSS_UNIQ), M_BIL_ENCOURS_PERIODE
  from work.base
  where (not missing(expo_dwr)) and not (expo_dwr >= 0)
  order by M_BIL_ENCOURS_PERIODE desc;
quit;


/*============================================================================
  CDE 15 : Segmentation Part/PRO
============================================================================*/
/* CDE non couverte actuellement : blocs inactifs, a activer quand la variable est disponible. */

/* R55 | CDE 15 Segmentation Part/PRO | variable : SEG_DECL
   DIMENSION : EXHAUSTIVITE | severite : Bloquant
   Regle     : Segment renseigne
   Population: 1=1  ->  conforme si : not missing(SEG_DECL) */
/* BLOC INACTIF - variable absente de la table consolidee :
proc sql;
  insert into work.res
  select &periode., 15, "Segmentation Part/PRO", "SEG_DECL", "R55", "Exhaustivite", "Bloquant", "Segment renseigne",
         count(*),
         sum(case when (not missing(SEG_DECL)) then 1 else 0 end),
         sum(w),
         sum(case when (not missing(SEG_DECL)) then w else 0 end)
  from work.base
  where (1=1);
quit;
proc sql outobs=&max_anom.;
  insert into work.anom
  select &periode., "R55", contrat_id, strip(I_DOSS_UNIQ), M_BIL_ENCOURS_PERIODE
  from work.base
  where (1=1) and not (not missing(SEG_DECL))
  order by M_BIL_ENCOURS_PERIODE desc;
quit;
*/


/* R56 | CDE 15 Segmentation Part/PRO | variable : SEG_DECL
   DIMENSION : VALIDITE | severite : Bloquant
   Regle     : Segment dans {Part, PRO}
   Population: not missing(SEG_DECL)  ->  conforme si : SEG_DECL in ('Part','PRO') */
/* BLOC INACTIF - variable absente de la table consolidee :
proc sql;
  insert into work.res
  select &periode., 15, "Segmentation Part/PRO", "SEG_DECL", "R56", "Validite", "Bloquant", "Segment dans {Part, PRO}",
         count(*),
         sum(case when (SEG_DECL in ('Part','PRO')) then 1 else 0 end),
         sum(w),
         sum(case when (SEG_DECL in ('Part','PRO')) then w else 0 end)
  from work.base
  where (not missing(SEG_DECL));
quit;
proc sql outobs=&max_anom.;
  insert into work.anom
  select &periode., "R56", contrat_id, strip(I_DOSS_UNIQ), M_BIL_ENCOURS_PERIODE
  from work.base
  where (not missing(SEG_DECL)) and not (SEG_DECL in ('Part','PRO'))
  order by M_BIL_ENCOURS_PERIODE desc;
quit;
*/


/*============================================================================
  CDE 5 (suite) : UNICITE de la cle contrat (grain du dataset)
  Formule note 4.3 : nb de cles distinctes / nb d'enregistrements
============================================================================*/
proc sql;
  insert into work.res
  select &periode., 5, "Identification dossier BCR", "contrat_id", "N02", "Unicite", "Bloquant",
         "Une ligne par contrat et par arrete",
         count(*), count(distinct contrat_id), ., .
  from work.base
  where contrat_id ne '';
quit;

/*============================================================================
  TRANSVERSE : ACTUALITE (date de photo = date d'arrete = periode traitee)
============================================================================*/
proc sql;
  insert into work.res
  select &periode., 0, "Transverse", "D_PHOTO / D_ARRETE / N_PERD", "N10", "Actualite", "Bloquant",
         "D_PHOTO = D_ARRETE et N_PERD = periode traitee",
         count(*), sum(case when D_PHOTO = D_ARRETE and N_PERD = &periode. then 1 else 0 end),
         sum(w), sum(case when D_PHOTO = D_ARRETE and N_PERD = &periode. then w else 0 end)
  from work.base;
quit;

/*---------------------------------------------------------------------------
  2. TAUX, SEUILS ET STATUT RAG (note section 7)
     Bloquant : vert >= 99,9 % ; orange >= 99,5 %
     Majeur   : vert >= 99,5 % ; orange >= 98 %
     Mineur   : vert >= 98 %   ; orange >= 95 %
---------------------------------------------------------------------------*/
data out.dq_resultats_&periode.;
  set work.res;
  n_ko = n_pop - n_ok;
  a_ko = a_pop - a_ok;
  if n_pop > 0 then taux_nb = n_ok / n_pop;
  if a_pop > 0 then taux_enc = a_ok / a_pop; else taux_enc = taux_nb;   /* unicite : pas de poids */
  if severite = 'Bloquant' then do; seuil_vert = 0.999; seuil_orange = 0.995; poids_pi = 3; end;
  else if severite = 'Majeur' then do; seuil_vert = 0.995; seuil_orange = 0.980; poids_pi = 2; end;
  else do; seuil_vert = 0.980; seuil_orange = 0.950; poids_pi = 1; end;
  length statut $8;
  if n_pop = 0 or missing(taux_enc) then statut = 'ND';
  else if taux_enc >= seuil_vert then statut = 'VERT';
  else if taux_enc >= seuil_orange then statut = 'ORANGE';
  else statut = 'ROUGE';
  d_arrete = intnx('month', mdy(mod(&periode.,100), 1, int(&periode./100)), 0, 'end');
  arrete = &periode.;
  version_regles = 'v1.0';
  format d_arrete yymmdd10. taux_nb taux_enc seuil_vert seuil_orange percent9.3;
run;

/*---------------------------------------------------------------------------
  3. SCORES : par CDE, par dimension, global (note 4.4)
     Score = somme(pi x taux) / somme(pi) ; global = moyenne des scores CDE.
---------------------------------------------------------------------------*/
proc sql;
  create table work.score_cde as
  select &periode. as arrete, 'CDE' as niveau length=10, put(cde_no,z2.) as code length=40,
         sum(poids_pi*taux_enc)/sum(poids_pi) as score
  from out.dq_resultats_&periode. where statut ne 'ND' and cde_no > 0 group by cde_no;
  create table work.score_dim as
  select &periode. as arrete, 'DIMENSION' as niveau length=10, dimension as code length=40,
         sum(poids_pi*taux_enc)/sum(poids_pi) as score
  from out.dq_resultats_&periode. where statut ne 'ND' group by dimension;
  create table work.score_glob as
  select &periode. as arrete, 'GLOBAL' as niveau length=10, 'GLOBAL' as code length=40,
         mean(score) as score from work.score_cde;
quit;
data out.dq_scores_&periode.; set work.score_cde work.score_dim work.score_glob; format score percent9.3; run;

/*---------------------------------------------------------------------------
  4. MATRICE CDE x DIMENSION : quelles dimensions sont evaluees, par CDE ?
     (cases vides = dimension non evaluee a ce stade)
---------------------------------------------------------------------------*/
title "Taux en encours par CDE et par dimension";
proc tabulate data=out.dq_resultats_&periode. missing;
  class cde dimension;
  var taux_enc;
  table cde, dimension * taux_enc=' ' * mean=' ' * f=percent8.2;
run;
title "Regles en orange ou rouge";
proc print data=out.dq_resultats_&periode. noobs;
  where statut in ('ORANGE','ROUGE');
  var regle_id cde variable dimension severite n_pop n_ko taux_nb taux_enc statut;
run;
title;

/*---------------------------------------------------------------------------
  5. ANOMALIES ET REGISTRE INITIAL (phase 1, activite 10)
     Registre = une ligne par regle orange ou rouge. Les colonnes cause probable,
     proprietaire, action et echeance sont a remplir avec les data owners.
---------------------------------------------------------------------------*/
data out.dq_anomalies_&periode.; set work.anom; run;

proc sql noprint;
  select sum(w) into :encours_total trimmed from work.base;
quit;

data out.dq_registre_&periode.;
  length cause_probable $200 proprietaire $40 action $200 echeance 8 statut_plan $12;
  set out.dq_resultats_&periode.(where=(statut in ('ORANGE','ROUGE')));
  encours_ko_meur   = a_ko / 1000000;                 /* encours concerne en M EUR  */
  part_encours_ko   = a_ko / &encours_total.;         /* part du stock en defaut    */
  cause_probable = ''; proprietaire = ''; action = ''; echeance = .; statut_plan = 'A QUALIFIER';
  format echeance yymmdd10. part_encours_ko percent8.3;
run;
proc sort data=out.dq_registre_&periode.; by descending statut descending encours_ko_meur; run;

/*---------------------------------------------------------------------------
  6. EXPORT CSV (entree Power BI)
---------------------------------------------------------------------------*/
proc export data=out.dq_resultats_&periode. outfile="&dir_csv./dq_resultats_&periode..csv" dbms=csv replace; delimiter=';'; run;
proc export data=out.dq_scores_&periode.    outfile="&dir_csv./dq_scores_&periode..csv"    dbms=csv replace; delimiter=';'; run;
proc export data=out.dq_anomalies_&periode. outfile="&dir_csv./dq_anomalies_&periode..csv" dbms=csv replace; delimiter=';'; run;
proc export data=out.dq_registre_&periode.  outfile="&dir_csv./dq_registre_&periode..csv"  dbms=csv replace; delimiter=';'; run;
