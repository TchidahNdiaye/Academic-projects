/******************************************************************************
 Programme : 00_PARAMETRES.sas  -  PARAMETRAGE UNIQUE DES TROIS PROGRAMMES
 Usage     : inclus par 01, 02 et 03 par %include "&dir_prog./00_PARAMETRES.sas";
             c'est le SEUL fichier a modifier a chaque arrete (S0.1) et pour les seuils,
             poids, domaines et couleurs (S0.2 a S0.5). S0.6 construit la table de travail work.base.
 Contenu   : S0.1 arretes | S0.2 seuils, poids, domaines | S0.3 bibliotheque | S0.4 formats | S0.5 couleurs | S0.6 table de travail
******************************************************************************/
/* S0.1 Arretes et tables consolidees (une table par arrete, memes colonnes) */
%let periode  = 202609;                          /* arrete M   (AAAAMM)                   */
%let periode1 = 202608;                          /* arrete M-1 (AAAAMM)                   */
%let ds       = out.extraction_DWR_&periode.;    /* table consolidee de l'arrete M        */
%let ds1      = out.extraction_DWR_&periode1.;   /* table consolidee de l'arrete M-1      */
%let dir_out  = /chemin/vers/sorties_DQ;         /* racine des sorties ; sous-dossier a creer : pub ; le classeur Excel est ecrit a la racine */
%let run_id   = RUN_&periode._01;                /* identifiant du run                    */

/* S0.2 Seuils et tolerances des controles */
%let kappa       = 0.05;       /* tolerance encours / declassement                       */
%let tol_decl    = 0.01;       /* tolerance relative entre deux sources de montant (1 %) */
%let tol_duree   = 0.5;        /* tolerance duree recalculee (mois)                      */
%let duree_max   = 240;        /* borne haute plausible de la duree en defaut (mois)     */
%let seuil_indiv = 1000000;    /* seuil d'individualisation (EUR) : encours au declassement */
%let seuil_mat   = 0.001;      /* seuil de materialite : part de l'encours total         */
%let max_anom    = 50;         /* anomalies listees par regle dans l'Excel               */
%let nb_min_cell = 30;         /* effectif minimum pour afficher un taux dans une matrice */
/* PARAMETRAGE UNIQUE DES FEUX ET DES POIDS (valable pour tous les traitements) --------
   Un seul couple de seuils pour : regles (toutes severites), donnees critiques (CDE),
   dimensions, concordance HLC, couverture et reconciliation. Vert >= v_seuil ;
   orange >= o_seuil ; rouge en dessous.                                                  */
%let v_seuil = 0.95;  %let o_seuil = 0.90;
/* Poids de severite dans le calcul des scores (un seul jeu ; mettre 1 / 1 / 1 pour neutraliser) */
%let p_bloq = 3;  %let p_maj = 2;  %let p_min = 1;
/* Domaines a trancher avec les Data Owners (une seule ligne a modifier) */
%let val_retail = %str('R');                /* B_TIERS_RETAIL : Retail (le CDC dit 1 = Retail, sinon Corporate -> a confirmer) */
%let val_corp   = %str('C');                /* B_TIERS_RETAIL : Corporate */
%let dom_tiers  = %str(&val_retail.,&val_corp.);
%let lst_loaniq = %str('ALT','ATL','IMP');  /* Loan IQ : ALT dans le code, ATL / IMP dans le CDC -> a trancher */

/* S0.3 Bibliotheque de publication et options d'affichage */
libname pub "&dir_out./pub";                     /* tables PUB_* et DIM_* (une par onglet du classeur Excel) */
options nodate nonumber missing=' ' validvarname=v7;
ods escapechar = '^';
ods graphics on / width=780px height=420px imagefmt=png;
%let banner = Arretes &periode1. (M-1) et &periode. (M) - sortie SAS - a faire valider avec les Data Owners;

/* S0.4 Formats d'affichage (milliers avec espace, virgule decimale) */
proc format;
  picture frn  (round) low-<0 = '000 000 000 009' (prefix='-')  0-high = '000 000 000 009';
  picture frp  (round) low-<0 = '099,9 %' (prefix='-' mult=1000) 0-high = '099,9 %' (mult=1000);
  picture frp2 (round) low-<0 = '099,99 %' (prefix='-' mult=10000) 0-high = '099,99 %' (mult=10000);
  picture frm1 (round) low-<0 = '000 000 009,9' (prefix='-' mult=10) 0-high = '000 000 009,9' (mult=10);
  picture frm2 (round) low-<0 = '000 000 009,99' (prefix='-' mult=100) 0-high = '000 000 009,99' (mult=100);
  value $rolef 'M1' = 'M-1' 'M0' = 'M';
  value $mdlmap 'COR'='CORP' 'IMO'='IMMO' 'CCO'='CONSO' 'CRN'='REVOL' 'CDI'='CI' 'CAV'='CAV' other=_same_;   /* codes production -> codes des programmes (a confirmer par le Model Owner) */
run;

/* S0.5 Couleurs des graphiques */
data attr_feu;                               /* couleurs des feux */
  length ID $8 VALUE $8 FILLCOLOR $10;
  ID = 'feu';
  VALUE = 'VERT';   FILLCOLOR = 'cx1BAF7A'; output;
  VALUE = 'ORANGE'; FILLCOLOR = 'cxEDA100'; output;
  VALUE = 'ROUGE';  FILLCOLOR = 'cxE34948'; output;
  VALUE = 'ND';     FILLCOLOR = 'cxA0A7B8'; output;
run;
data attr_role;                              /* couleurs des deux arretes : M-1 clair, M fonce */
  length ID $8 VALUE $8 FILLCOLOR $10 LINECOLOR $10;
  ID = 'role';
  VALUE = 'M-1'; FILLCOLOR = 'cx9DB7D9'; LINECOLOR = 'cx9DB7D9'; output;
  VALUE = 'M';   FILLCOLOR = 'cx0E5A78'; LINECOLOR = 'cx0E5A78'; output;
run;

/* S0.6 Table de travail : les deux arretes empiles, une ligne par contrat et par arrete.
   Les variables derivees servent a tous les controles : poids, cle contrat, portefeuille,
   niveau de risque en caractere, duree recalculee a partir de la date de defaut.         */
data base;
  set &ds1.(in=in_m1) &ds.(in=in_m0);
  length contrat_id $60 cle_ano $72 NIV_RSK_C $8 role $4 slot $2 portefeuille $8;
  if in_m1 then do; arrete = &periode1.; role = 'M-1'; slot = 'M1'; end;
  else          do; arrete = &periode.;  role = 'M';   slot = 'M0'; end;
  NIV_RSK_C = strip(vvalue(C_CNTRT_NIV_RSK));                    /* 1 = EDX, 2 = CER, quel que soit le type d'origine */
  w = max(0, coalesce(M_BIL_ENCOURS_PERIODE, 0));                /* poids = encours bilan (negatifs ramenes a 0)      */
  if nmiss(I_CNTRT_SIEGE, N_CNTRT) = 0 and not missing(C_CNTRT_TYPE) then
    contrat_id = catx('-', put(I_CNTRT_SIEGE, best20. -L), C_CNTRT_TYPE, put(N_CNTRT, best20. -L));
  if missing(contrat_id) then cle_ano = catx('#', 'SANSCLE', slot, put(_n_, 12. -L));   /* cle d'anomalie : jamais vide */
  else cle_ano = contrat_id;
  portefeuille = put(upcase(strip(final_model)), $mdlmap.);      /* portefeuille = modele retenu (codes harmonises)   */
  if portefeuille = '' then portefeuille = 'NONAFF';
  if not missing(Default_date) and Default_date ne '01JAN1901'd then do;
    duree_recalc_mois = (D_ARRETE - Default_date) / 30.4375;     /* duree recalculee en mois                          */
    tid_recalc = ceil((D_ARRETE - Default_date) / 365.25);       /* annees entamees recalculees                       */
  end;
run;


