/******************************************************************************
 Programme : DQ_ETAPE_5_MOTEUR_FINANCIER.sas
 Etape CDC : Etape 5 - moteur financier : provision recalculee, impact des anomalies, bridge M-1 vers M
 Version   : v2.0
 Objet     : Recalculer la provision FLL_PROV = FL_HLC x [LGDD_HLC x Expo_bilan + LGDD_HLC x Taux_tirage x Expo_HB] selon trois scenarios (production, recalcule, corrige), chiffrer l impact des anomalies, expliquer la variation M-1 vers M et preparer la decision de certification.
 Entrees   : out.dq_hlc_base_&periode. (etape 4), out.dq_anomalies_&periode. et out.dq_resultats_&periode. (etapes 2-3) ; parametres LGDD, FL et taux de tirage ecrits dans la section S0.8 (a remplir)
 Sorties   : Excel : &dir_out./excel/DQ_Etape5_&periode..xlsx (7 onglets ECL_1 a ECL_6, DEC_1) ; CSV Power BI : PUB_ECL_AGG, PUB_ECL_EFFECT, PUB_IMPACT_AGG, PUB_IMPACT_REGLE, PUB_HLC_DRIVER_DIFF (remplace celui de l etape 4), PUB_PARAM_AUDIT, PUB_COUVERTURE, PUB_RECONCILIATION, PUB_SENSIBILITE, PUB_DECISION, PUB_CERTIFICATION, DIM_EFFET, DIM_MVT
 Contenu   : S0 parametres | S1 scenarios ECL | S2 impact des anomalies | S3 bridge | S4 couverture, reconciliation, sensibilite | S5 gouvernance | S6 Excel | S7 publication
 Style     : code lineaire, PAS de macro programme (seulement des %let de parametres
             et quelques call execute). Un bloc = une question ou un controle.
             Pas d accent dans le code (encodage). Aucun fichier referentiel.
 Remarque  : programme non execute sur le dataset reel : au premier lancement,
             verifier les noms de variables (tableau de la section S1.1) et la
             correspondance des codes (section S0 / mapping).
******************************************************************************/

%let t_start = %sysfunc(datetime());
/*===========================================================================
  S0. PARAMETRES COMMUNS AUX TROIS PROGRAMMES (a modifier a chaque arrete)
      Ce bloc est identique dans les programmes des etapes 2-3, 4 et 5.
      Aucun fichier referentiel n'est lu : tout est ecrit dans le programme.
===========================================================================*/
/* S0.1 Arretes et tables consolidees (une table par arrete, memes colonnes) */
%let periode  = 202609;                          /* arrete M   (AAAAMM)                   */
%let periode1 = 202608;                          /* arrete M-1 (AAAAMM)                   */
%let ds       = out.extraction_DWR_&periode.;    /* table consolidee de l'arrete M        */
%let ds1      = out.extraction_DWR_&periode1.;   /* table consolidee de l'arrete M-1      */
%let dir_out  = /chemin/vers/sorties_DQ;         /* racine des sorties ; sous-dossiers a creer : excel, csv, pub */
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
/* Seuils de statut des regles (vert / orange) selon la severite */
%let v_bloq = 0.999;  %let o_bloq = 0.995;
%let v_maj  = 0.995;  %let o_maj  = 0.980;
%let v_min  = 0.980;  %let o_min  = 0.950;
/* Seuils de feu d'une donnee critique (CDE) */
%let v_cde  = 0.99;   %let o_cde  = 0.97;

/* S0.3 Bibliotheque de publication et options d'affichage */
libname pub "&dir_out./pub";                     /* tables PUB_* et DIM_* (une par fichier CSV) */
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
run;

/* S0.5 Style Excel et couleurs des graphiques */
proc template;
  define style styles.dqxl;
    parent = styles.excel;
    class header / background=cx0E5A78 color=white fontfamily=Arial fontsize=9pt fontweight=bold;
    class data   / fontfamily=Arial fontsize=9.5pt;
    class systemtitle / fontfamily=Arial fontsize=9pt fontweight=medium color=cx555555;
  end;
run;
data work.attr_feu;                               /* couleurs des feux */
  length ID $8 VALUE $8 FILLCOLOR $10;
  ID = 'feu';
  VALUE = 'VERT';   FILLCOLOR = 'cx1BAF7A'; output;
  VALUE = 'ORANGE'; FILLCOLOR = 'cxEDA100'; output;
  VALUE = 'ROUGE';  FILLCOLOR = 'cxE34948'; output;
  VALUE = 'ND';     FILLCOLOR = 'cxA0A7B8'; output;
run;
data work.attr_role;                              /* couleurs des deux arretes : M-1 clair, M fonce */
  length ID $8 VALUE $8 FILLCOLOR $10 LINECOLOR $10;
  ID = 'role';
  VALUE = 'M-1'; FILLCOLOR = 'cx9DB7D9'; LINECOLOR = 'cx9DB7D9'; output;
  VALUE = 'M';   FILLCOLOR = 'cx0E5A78'; LINECOLOR = 'cx0E5A78'; output;
run;

/* S0.6 Table de travail : les deux arretes empiles, une ligne par contrat et par arrete.
   Les variables derivees servent a tous les controles : poids, cle contrat, portefeuille,
   niveau de risque en caractere, duree recalculee a partir de la date de defaut.         */
data work.base;
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
  portefeuille = upcase(strip(final_model));                     /* portefeuille = modele retenu                      */
  if portefeuille = '' then portefeuille = 'NONAFF';
  if not missing(Default_date) and Default_date ne '01JAN1901'd then do;
    duree_recalc_mois = (D_ARRETE - Default_date) / 30.4375;     /* duree recalculee en mois                          */
    tid_recalc = ceil((D_ARRETE - Default_date) / 365.25);       /* annees entamees recalculees                       */
  end;
run;

/* S0.7 PARAMETRES PROPRES A L ETAPE 5 -------------------------------------------------- */
%let lst_stat = 'CORP','IMMO','CONSO','REVOL','CI','CAV';     /* identique a l etape 4 */
%let r_ci  = 0.35;                                             /* identique a l etape 4 */
%let r_cav = 1.051;
%let tol_prov_abs = 1;          /* EUR : ecart tolere par contrat entre provision recalculee et provision de production */
%let tol_prov_rel = 0.001;      /* ou 0,1 % de la provision de production                                          */
%let seuil_couv   = 0.98;       /* part minimale de l encours statistique avec provision calculable                  */
%let seuil_recon  = 0.99;       /* part minimale de contrats reconcilies                                             */
%let seuil_concord = 0.95;      /* concordance HLC minimale pour certifier                                           */
%let tol_residu   = 1;          /* EUR : residu E07 admis                                                            */
%let version_param = LGDD_A_RENSEIGNER;                         /* version des parametres, pour l audit                   */
%let approbation   = A_RENSEIGNER;                              /* reference d approbation des parametres                 */

/* S0.8 PARAMETRES FINANCIERS (a remplir : LGDD, FL, taux de tirage) -------------------------
   Une ligne par portefeuille et par HLC ; deux colonnes de valeurs par grandeur : arrete M-1 puis arrete M.
   Valeurs en decimal (0,35 pour 35 pct). Une valeur manquante (.) rend la provision NON calculable
   pour les contrats concernes : elle n est jamais remplacee par zero. */
data work.par_in;
  infile datalines dlm='|' dsd truncover;
  length PORTEFEUILLE $8 HLC 8 LGDD_M1 8 FL_M1 8 LGDD_M 8 FL_M 8;
  input PORTEFEUILLE $ HLC LGDD_M1 FL_M1 LGDD_M FL_M;
datalines;
CORP|1|.|.|.|.
CORP|2|.|.|.|.
CORP|3|.|.|.|.
CORP|4|.|.|.|.
CORP|5|.|.|.|.
CORP|6|.|.|.|.
CORP|7|.|.|.|.
CORP|8|.|.|.|.
CORP|9|.|.|.|.
IMMO|1|.|.|.|.
IMMO|2|.|.|.|.
IMMO|3|.|.|.|.
IMMO|4|.|.|.|.
IMMO|5|.|.|.|.
IMMO|6|.|.|.|.
IMMO|7|.|.|.|.
IMMO|8|.|.|.|.
IMMO|9|.|.|.|.
CONSO|1|.|.|.|.
CONSO|2|.|.|.|.
CONSO|3|.|.|.|.
CONSO|4|.|.|.|.
CONSO|5|.|.|.|.
CONSO|6|.|.|.|.
CONSO|7|.|.|.|.
CONSO|8|.|.|.|.
CONSO|9|.|.|.|.
REVOL|1|.|.|.|.
REVOL|2|.|.|.|.
REVOL|3|.|.|.|.
REVOL|4|.|.|.|.
REVOL|5|.|.|.|.
REVOL|6|.|.|.|.
REVOL|7|.|.|.|.
CI|1|.|.|.|.
CI|2|.|.|.|.
CI|3|.|.|.|.
CI|4|.|.|.|.
CI|5|.|.|.|.
CI|6|.|.|.|.
CI|7|.|.|.|.
CI|8|.|.|.|.
CAV|1|.|.|.|.
CAV|2|.|.|.|.
CAV|3|.|.|.|.
CAV|4|.|.|.|.
CAV|5|.|.|.|.
CAV|6|.|.|.|.
;
run;
data work.par_td_in;
  infile datalines dlm='|' dsd truncover;
  length PORTEFEUILLE $8 TIRAGE_M1 8 TIRAGE_M 8;
  input PORTEFEUILLE $ TIRAGE_M1 TIRAGE_M;
datalines;
CORP|.|.
IMMO|.|.
CONSO|.|.
REVOL|.|.
CI|.|.
CAV|.|.
;
run;
data work.par;                                   /* une ligne par arrete, portefeuille et HLC */
  set work.par_in;
  N_PERD = &periode1.; LGDD = LGDD_M1; FL = FL_M1; output;
  N_PERD = &periode.;  LGDD = LGDD_M;  FL = FL_M;  output;
  keep N_PERD PORTEFEUILLE HLC LGDD FL;
run;
data work.par_td;
  set work.par_td_in;
  N_PERD = &periode1.; TIRAGE = TIRAGE_M1; output;
  N_PERD = &periode.;  TIRAGE = TIRAGE_M;  output;
  keep N_PERD PORTEFEUILLE TIRAGE;
run;
proc sql noprint;
  select count(*) into :n_par_manq trimmed from work.par where missing(LGDD) or missing(FL);
  select count(*) into :n_td_manq trimmed from work.par_td where missing(TIRAGE);
quit;
%put NOTE: &n_par_manq. ligne(s) LGDD ou FL manquante(s) et &n_td_manq. taux de tirage manquant(s) : les provisions correspondantes ne seront pas calculees.;

/* Dimension des effets du bridge */
data work.dim_effet;
  infile datalines dlm='|' dsd truncover;
  length EFFET_CODE $4 LIBELLE $50 ORDRE 8 GROUPE $20;
  input EFFET_CODE $ LIBELLE $ ORDRE GROUPE $;
datalines;
OPEN|Provision M-1|1|Ouverture
P01|Entrees dans le perimetre|2|Population
P02|Sorties du perimetre|3|Population
P03|Changements de route ou de portefeuille|4|Population
E01|Exposition bilan|5|Contrats communs
E02|Exposition hors bilan (taux de tirage)|6|Contrats communs
E03|Garantie|7|Contrats communs
E04|Duree en defaut et statut|8|Contrats communs
E05|HLC / segment (reliquat de l arbre)|9|Contrats communs
E06|Parametres|10|Contrats communs
E07|Residuel|11|Contrats communs
;
run;
data work.dim_mvt;
  infile datalines dlm='|' dsd truncover;
  length CODE $4 LIBELLE $30 ORDRE 8;
  input CODE $ LIBELLE $ ORDRE;
datalines;
OPEN|Ouverture (M-1)|1
OUT|Sorties|2
IN|Entrees|3
;
run;

/*===========================================================================
  S1. SCENARIOS DE PROVISION
      PROD    provision de production (M_PROVS_SOLDE), telle que publiee
      RECALC  formule rejouee avec la HLC de la production (controle du moteur)
      CORRIGE formule avec la HLC attendue et les drivers recalcules (etape 4)
      Sont aussi calcules : ATT (HLC de l arbre, drivers de la production), un scenario
      par driver D1 a D7, et HLC +1 / HLC -1 (sensibilite).
      Perimetre : voie statistique (six arbres). PGE et voie individuelle sont hors calcul.
===========================================================================*/
data work.c0;
  set out.dq_hlc_base_&periode.;
  where route_att = 'STAT' and portefeuille in (&lst_stat.);
  ex_b = max(0, coalesce(w, 0));
  ex_hb = max(0, coalesce(ead, 0) - ex_b);
  if portefeuille in ('CORP', 'IMMO', 'CONSO') then maxcl = 9; else if portefeuille = 'REVOL' then maxcl = 7;
  else if portefeuille = 'CI' then maxcl = 8; else maxcl = 6;
run;
data work.cl;                                      /* une ligne par contrat et par scenario */
  set work.c0;
  length scen $4;
  array s{7} s_d1-s_d7; array h{7} h_d1-h_d7;
  keep arrete slot cle_ano portefeuille scen hlc ex_b ex_hb;
  scen = 'PROD'; hlc = hlc_prod; output;
  scen = 'ATT';  hlc = hlc_att;  output;
  scen = 'COR';  hlc = coalesce(hlc_all, hlc_prod); output;
  do k = 1 to 7;
    if s{k} = 1 and not missing(h{k}) then do; scen = cats('D', k); hlc = h{k}; output; end;
  end;
  if not missing(hlc_prod) then do;
    scen = 'PLUS';  hlc = min(hlc_prod + 1, maxcl); output;
    scen = 'MOIN';  hlc = max(hlc_prod - 1, 1);     output;
  end;
run;
proc sql;
  create table work.cl_e as
  select c.*, p.LGDD, p.FL, t.TIRAGE,
         case when p.LGDD is not null and p.FL is not null and t.TIRAGE is not null and c.hlc is not null
              then p.FL * p.LGDD * (c.ex_b + t.TIRAGE * c.ex_hb) end as ecl
  from work.cl as c
       left join work.par as p on c.arrete = p.N_PERD and c.portefeuille = p.PORTEFEUILLE and c.hlc = p.HLC
       left join work.par_td as t on c.arrete = t.N_PERD and c.portefeuille = t.PORTEFEUILLE;
  create table work.ce as
  select arrete, slot, cle_ano,
         max(case when scen = 'PROD' then ecl end) as ecl_rec, max(case when scen = 'ATT' then ecl end) as ecl_att,
         max(case when scen = 'COR' then ecl end) as ecl_cor, max(case when scen = 'PLUS' then ecl end) as ecl_plus,
         max(case when scen = 'MOIN' then ecl end) as ecl_moin,
         max(case when scen = 'D1' then ecl end) as ecl_d1, max(case when scen = 'D2' then ecl end) as ecl_d2,
         max(case when scen = 'D3' then ecl end) as ecl_d3, max(case when scen = 'D4' then ecl end) as ecl_d4,
         max(case when scen = 'D5' then ecl end) as ecl_d5, max(case when scen = 'D6' then ecl end) as ecl_d6,
         max(case when scen = 'D7' then ecl end) as ecl_d7
  from work.cl_e group by arrete, slot, cle_ano;
  create table work.cc as
  select c.*, e.ecl_rec, e.ecl_att, e.ecl_cor, e.ecl_plus, e.ecl_moin, e.ecl_d1, e.ecl_d2, e.ecl_d3, e.ecl_d4, e.ecl_d5, e.ecl_d6, e.ecl_d7
  from work.c0 as c left join work.ce as e on c.arrete = e.arrete and c.slot = e.slot and c.cle_ano = e.cle_ano;
quit;
data work.cc;
  set work.cc;
  calc_ok = (not missing(ecl_rec) and not missing(ecl_cor));      /* provisions recalculee et corrigee toutes deux calculables */
  ecart_prod = ecl_rec - ecl_prod;
  tol_c = max(&tol_prov_abs., &tol_prov_rel. * abs(coalesce(ecl_prod, 0)));
  recon_ok = (not missing(ecl_rec) and not missing(ecl_prod) and abs(ecart_prod) <= tol_c);
run;
/* Table lue par les tableaux de sortie */
proc sql;
  create table work.e1 as
  select role, portefeuille, count(*) as n_contrats, sum(calc_ok) as n_calc, sum(case when calc_ok then ex_b else 0 end) as ead_bil format=comma18.,
         sum(case when calc_ok then ex_hb else 0 end) as ead_hb format=comma18.,
         sum(case when calc_ok then ecl_prod else 0 end) as ecl_prod format=comma18., sum(case when calc_ok then ecl_rec else 0 end) as ecl format=comma18.,
         sum(case when calc_ok then ecl_cor else 0 end) as ecl_corrige format=comma18.
  from work.cc group by role, portefeuille order by portefeuille, role desc;
  create table work.e1t as
  select role, sum(n_contrats) as n_contrats, sum(n_calc) as n_calc, sum(ead_bil) as ead_bil format=comma18., sum(ead_hb) as ead_hb format=comma18.,
         sum(ecl_prod) as ecl_prod format=comma18., sum(ecl) as ecl format=comma18., sum(ecl_corrige) as ecl_corrige format=comma18.
  from work.e1 group by role;
  select coalesce(max(case when role = 'M' then ecl end), 0), coalesce(max(case when role = 'M' then ecl_corrige end), 0), coalesce(max(case when role = 'M' then n_calc end), 0),
         coalesce(max(case when role = 'M' then n_contrats end), 0)
    into :t_rec trimmed, :t_cor trimmed, :t_ncalc trimmed, :t_nctr trimmed from work.e1t;
quit;

/*===========================================================================
  S2. IMPACT DES ANOMALIES SUR LA PROVISION
      Chaque anomalie (contrat x regle) liee a un driver est chiffree :
        CALCULATED       le driver est recalcule : impact = provision(HLC avec driver recalcule) - provision(HLC de l arbre)
        ESTIMATED        pas de source de recalcul sur ce contrat : taux d impact moyen du meme portefeuille et driver x encours
        NOT_QUANTIFIABLE regle sans driver, ou provision non calculable
      Les impacts de regles differentes sur un meme contrat ne s additionnent pas (un contrat peut porter
      plusieurs anomalies) : l impact additif par contrat est donne par l ecart corrige - recalcule.
===========================================================================*/
data work.an1;
  set out.dq_anomalies_&periode.;
  length slot $2;
  if statut in ('NOUVELLE', 'PERSISTANTE') then do; slot = 'M0'; output; end;
  if statut in ('PERSISTANTE', 'RESOLUE', 'SORTIE') then do; slot = 'M1'; output; end;
run;
proc sql;
  create table work.an2 as
  select a.regle_id, a.DRIVER_ID, a.CAUSE_RACINE, a.statut as statut_anom, a.slot, c.arrete, c.role, c.portefeuille, c.cle_ano, c.ex_b + c.ex_hb as ead,
         c.ecl_att, c.ecl_d1, c.ecl_d2, c.ecl_d3, c.ecl_d4, c.ecl_d5, c.ecl_d6, c.ecl_d7, c.s_d1, c.s_d2, c.s_d3, c.s_d4, c.s_d5, c.s_d6, c.s_d7,
         c.c_d1, c.c_d2, c.c_d3, c.c_d4, c.c_d5, c.c_d6, c.c_d7
  from work.an1 as a inner join work.cc as c on a.cle_ano = c.cle_ano and a.slot = c.slot;
quit;
data work.an3;
  set work.an2;
  array e{7} ecl_d1-ecl_d7; array s{7} s_d1-s_d7; array c{7} c_d1-c_d7;
  length statut_impact $16;
  impact_ecl = .; statut_impact = 'NOT_QUANTIFIABLE';
  if DRIVER_ID ne '' then do;
    k = input(substr(DRIVER_ID, 2), 1.);
    if c{k} = 1 and not missing(ecl_att) then do;
      if s{k} = 1 and not missing(e{k}) then do; impact_ecl = e{k} - ecl_att; statut_impact = 'CALCULATED'; end;
      else if s{k} ne 1 then do; impact_ecl = 0; statut_impact = 'CALCULATED'; end;
    end;
    else if c{k} ne 1 then statut_impact = 'ESTIMATED';
  end;
run;
proc sql;
  create table work.an_rate as
  select arrete, portefeuille, DRIVER_ID, sum(impact_ecl) / sum(ead) as taux_impact
  from work.an3 where statut_impact = 'CALCULATED' and ead > 0 group by arrete, portefeuille, DRIVER_ID;
  create table work.an4 as
  select a.*, case when a.statut_impact = 'ESTIMATED' and r.taux_impact is not null then a.ead * r.taux_impact else a.impact_ecl end as impact_final,
         case when a.statut_impact = 'ESTIMATED' and r.taux_impact is null then 'NOT_QUANTIFIABLE' else a.statut_impact end as statut_final length=16
  from work.an3 as a left join work.an_rate as r on a.arrete = r.arrete and a.portefeuille = r.portefeuille and a.DRIVER_ID = r.DRIVER_ID;
  create table work.i1 as                                  /* par portefeuille et statut d impact */
  select arrete as N_PERD, role, portefeuille as PORTEFEUILLE, statut_final as STATUT_IMPACT, count(*) as N_ANOMALIES, sum(ead) as EAD, sum(impact_final) as IMPACT_ECL
  from work.an4 group by arrete, role, portefeuille, statut_final;
  create table work.i2 as                                  /* par regle */
  select arrete as N_PERD, role, regle_id as RULE_ID, DRIVER_ID, statut_final as STATUT_IMPACT, count(*) as N_ANOMALIES, sum(ead) as EAD, sum(impact_final) as IMPACT_ECL,
         sum(abs(impact_final)) as IMPACT_ABS
  from work.an4 group by arrete, role, regle_id, DRIVER_ID, statut_final;
  create table work.i3 as                                  /* par driver : effet de chaque correction prise seule */
  select role, driver_id, count(*) as n_contrats, sum(impact) as impact_ecl, sum(ead) as ead
  from (select role, 'D1' as driver_id, ecl_d1 - ecl_att as impact, ex_b + ex_hb as ead from work.cc where s_d1 = 1 and ecl_d1 is not null and ecl_att is not null
        union all select role, 'D2', ecl_d2 - ecl_att, ex_b + ex_hb from work.cc where s_d2 = 1 and ecl_d2 is not null and ecl_att is not null
        union all select role, 'D3', ecl_d3 - ecl_att, ex_b + ex_hb from work.cc where s_d3 = 1 and ecl_d3 is not null and ecl_att is not null
        union all select role, 'D4', ecl_d4 - ecl_att, ex_b + ex_hb from work.cc where s_d4 = 1 and ecl_d4 is not null and ecl_att is not null
        union all select role, 'D5', ecl_d5 - ecl_att, ex_b + ex_hb from work.cc where s_d5 = 1 and ecl_d5 is not null and ecl_att is not null
        union all select role, 'D6', ecl_d6 - ecl_att, ex_b + ex_hb from work.cc where s_d6 = 1 and ecl_d6 is not null and ecl_att is not null
        union all select role, 'D7', ecl_d7 - ecl_att, ex_b + ex_hb from work.cc where s_d7 = 1 and ecl_d7 is not null and ecl_att is not null)
  group by role, driver_id;
  create table work.i4 as                                  /* par cause d ecart HLC : ecart corrige - recalcule */
  select role, mismatch_cause as cause, count(*) as n_contrats, sum(ecl_cor - ecl_rec) as impact_ecl, sum(ex_b + ex_hb) as ead
  from work.cc where calc_ok and match_status = 'MISMATCH' group by role, mismatch_cause;
  select coalesce(sum(case when role = 'M' then impact_ecl end), 0) into :t_imp trimmed from work.i1 where statut_impact in ('CALCULATED', 'ESTIMATED');
quit;

/*===========================================================================
  S3. BRIDGE M-1 VERS M (provision recalculee, formule avec HLC de la production)
      Population : voie statistique, provision calculable. Cle contrat obligatoire.
      OPEN  provision de M-1 ; P01 entrees ; P02 sorties ; P03 changements de route / portefeuille
      E01 a E06 : sur les contrats communs restes dans le meme portefeuille, on change UN facteur
      a la fois dans l ordre bilan, hors bilan, garantie, duree et statut, HLC (reliquat de l arbre),
      parametres. Les etats garantie et duree rejouent l arbre ; la HLC rejouee n est retenue que
      si elle change le resultat de l arbre. E07 = residuel (doit valoir zero).
===========================================================================*/
proc sql;
  create table work.pres as select distinct cle_ano, slot from out.dq_hlc_base_&periode.;
  create table work.br_x as
  select coalesce(a.cle_ano, b.cle_ano) as cle_ano length=72,
         a.portefeuille as ptf1, b.portefeuille as ptf0, a.ecl_rec as ecl1, b.ecl_rec as ecl0,
         a.ex_b as w1, a.ex_hb as hb1, b.ex_b as w0, b.ex_hb as hb0, a.hlc_prod as hlc1, b.hlc_prod as hlc0,
         a.b_tid as tid1, a.b_typris as typ1, a.b_wo_any as wo1, a.b_wo_part as wop1, a.b_gar as gar1, a.b_crelog as cl1, a.b_cat as cat1, a.r_bil as rbil1, a.b_den as den1,
         b.b_tid as tid0, b.b_typris as typ0, b.b_wo_any as wo0, b.b_wo_part as wop0, b.b_gar as gar0, b.b_crelog as cl0
  from (select * from work.cc where slot = 'M1' and cle_ano not like 'SANSCLE%' and ecl_rec is not null) as a
       full join (select * from work.cc where slot = 'M0' and cle_ano not like 'SANSCLE%' and ecl_rec is not null) as b on a.cle_ano = b.cle_ano;
  create table work.br as
  select x.*, (p1.cle_ano is not null) as pres1, (p0.cle_ano is not null) as pres0
  from work.br_x as x
       left join (select cle_ano from work.pres where slot = 'M1') as p1 on x.cle_ano = p1.cle_ano
       left join (select cle_ano from work.pres where slot = 'M0') as p0 on x.cle_ano = p0.cle_ano;
  select count(*) into :n_sanscle trimmed from work.cc where cle_ano like 'SANSCLE%';
quit;
data work.br2;
  set work.br;
  length mvt $4;
  e1 = (not missing(ecl1)); e0 = (not missing(ecl0));
  if e1 and e0 and ptf1 = ptf0 then mvt = 'STAY';
  else if e1 and e0 then mvt = 'P03';
  else if e1 then do; if pres0 > 0 then mvt = 'P03'; else mvt = 'P02'; end;
  else if e0 then do; if pres1 > 0 then mvt = 'P03'; else mvt = 'P01'; end;
run;
/* S3.1 Rejeu de l arbre pour les contrats communs : T0 (drivers de M-1), T3 (garantie de M), T4 (garantie, duree et statut de M) */
data work.br_tree_in;
  set work.br2(where=(mvt = 'STAY'));
  length vari $2 t_modele $8 t_typris $4 t_cat $8;
  t_modele = ptf1; t_cat = cat1; r_bil = rbil1; r_den = den1;
  vari = 'T0'; t_tid = tid1; t_typris = typ1; t_wo_any = wo1; t_wo_part = wop1; t_gar = gar1; t_crelog = cl1; output;
  vari = 'T3'; t_tid = tid1; t_typris = typ1; t_wo_any = wo1; t_wo_part = wop1; t_gar = gar0; t_crelog = cl0; output;
  vari = 'T4'; t_tid = tid0; t_typris = typ0; t_wo_any = wo0; t_wo_part = wop0; t_gar = gar0; t_crelog = cl0; output;
  keep cle_ano vari t_modele t_tid t_typris t_wo_any t_wo_part t_gar t_crelog t_cat r_bil r_den;
run;
data work.br_tree_out;
  set work.br_tree_in;
  length r_cas $4 t_rcas $4 t_chemin $40 t_flag $4;
  r_val = .;
  if r_den = . or r_den = 0 or r_bil = . then r_cas = 'NULL';
  else if r_den < 0 then do;                    /* declassement crediteur */
    if r_bil < 0 then r_cas = 'LOW';            /*   bilan crediteur : HLC la moins risquee */
    else if r_bil = 0 then r_cas = 'ZERO';      /*   bilan nul : cas a arbitrer (Q47) */
    else r_cas = 'NULL';                        /*   bilan debiteur : regle du declassement nul */
  end;
  else do;                                      /* declassement debiteur */
    if r_bil < 0 then r_cas = 'LOW';            /*   ratio negatif : HLC la moins risquee */
    else if r_bil = 0 then do; r_cas = 'ZERO'; r_val = 0; end;
    else do; r_cas = 'CALC'; r_val = r_bil / r_den; end;
  end;
  t_rcas = r_cas; t_r = r_val;
  t_hlc = .; t_chemin = 'INDETERMINE'; t_flag = '';
  if t_tid ne . and t_typris in ('1','2') then do;
    t_edx = (t_typris = '1');

    if t_modele = 'CORP' then do;
      if t_edx and not t_wo_any then do;
        t_hlc = 1 + (t_tid > 2) + (t_tid > 4);                         /* 1 / 2 / 3 */
        t_chemin = 'EDX_SANS_WO';
      end;
      else if t_tid <= 1 then do; t_hlc = 4; t_chemin = 'CER_OU_WO>TID<=1'; end;
      else if t_gar and not t_wo_part then do; t_hlc = 5; t_chemin = 'CER_OU_WO>GARANTIE'; end;
      else do;
        t_hlc = 6 + (t_tid > 2) + (t_tid > 3) + (t_tid >= 6);          /* 6 / 7 / 8 / 9 */
        t_chemin = 'CER_OU_WO>SANS_GARANTIE';
      end;
    end;

    else if t_modele = 'IMMO' then do;
      if t_edx and not t_wo_any then do;
        if t_tid <= 1 then do; t_hlc = 2 - t_crelog; t_chemin = 'EDX_SANS_WO>TID<=1'; end;   /* Credit Logement : 1 */
        else do; t_hlc = 3 + (t_tid > 2); t_chemin = 'EDX_SANS_WO'; end;                     /* 3 / 4 */
      end;
      else do;
        t_hlc = 5 + (t_tid > 2) + (t_tid > 3) + (t_tid > 5) + (t_tid > 7);                  /* 5 a 9 */
        t_chemin = 'CER_OU_WO';
        if t_tid = 2 then t_flag = 'Q20';                              /* borne a 2 ans a confirmer */
      end;
    end;

    else if t_modele = 'CONSO' then do;
      if t_edx and not t_wo_part then do;
        t_hlc = 1 + (t_tid > 1) + (t_tid > 2) + (t_tid > 4) + (t_tid > 7);                  /* 1 a 5 */
        t_chemin = 'EDX_SANS_WOP';
        if t_hlc in (3, 4) then t_flag = 'Q22';                        /* libelles ambigus */
        else if t_hlc = 5 then t_flag = 'Q23';
      end;
      else do;
        t_hlc = 6 + (t_tid > 2) + (t_tid > 3) + (t_tid > 4);                                /* 6 a 9 */
        t_chemin = 'CER_OU_WOP';
      end;
    end;

    else if t_modele = 'REVOL' then do;
      if t_edx and not t_wo_part then do;
        t_hlc = 1 + (t_tid > 1) + (t_tid > 2); t_chemin = 'EDX_SANS_WOP';                   /* 1 / 2 / 3 */
      end;
      else do;
        t_hlc = 4 + (t_tid > 1) + (t_tid > 3) + (t_tid > 4); t_chemin = 'CER_OU_WOP';       /* 4 a 7 */
      end;
    end;

    else if t_modele = 'CI' then do;
      if t_edx and not t_wo_part then do;
        if t_tid <= 3 then do; t_hlc = 2 - t_gar; t_chemin = 'EDX_SANS_WOP>TID<=3'; end;     /* garantie : 1 */
        else do; t_hlc = 3; t_chemin = 'EDX_SANS_WOP>TID>3'; end;
      end;
      else if t_gar and not t_wo_part then do; t_hlc = 4; t_chemin = 'CER_OU_WOP>GARANTIE'; end;
      else do;
        t_h678 = 6 + (t_tid > 6) + (t_tid > 9);                        /* 6 / 7 / 8 selon le TID */
        if t_rcas = 'LOW' then do; t_hlc = 5; t_chemin = 'CER_OU_WOP>R_MIN'; end;
        else if t_rcas = 'ZERO' then do; t_hlc = 5; t_chemin = 'CER_OU_WOP>R_ZERO'; t_flag = 'Q47'; end;
        else if t_rcas = 'NULL' then do; t_hlc = t_h678; t_chemin = 'CER_OU_WOP>DECL_NUL'; end;
        else do;
          if t_r <= &r_ci. then t_hlc = 5; else t_hlc = t_h678;
          t_chemin = 'CER_OU_WOP>RATIO';
        end;
      end;
    end;

    else if t_modele = 'CAV' then do;
      if t_tid <= 1 then do;
        if t_edx and not t_wo_part then do; t_hlc = 1; t_chemin = 'TID<=1>EDX'; end;
        else if t_cat = 'PRIVE' then do; t_hlc = 2; t_chemin = 'TID<=1>CER_PRIVE'; end;
        else if t_cat = 'PRO' then do; t_hlc = 3; t_chemin = 'TID<=1>CER_PRO'; end;
        else if t_cat = 'PRO_VIDE' then do; t_hlc = 3; t_chemin = 'TID<=1>CER_PRO'; t_flag = 'Q49'; end;
        else t_chemin = 'INDETERMINE_CAT';                             /* categorie inconnue : pas de HLC */
      end;
      else if t_tid > 6 then do; t_hlc = 6; t_chemin = 'TID>6'; end;
      else if t_rcas = 'LOW' then do; t_hlc = 4; t_chemin = 'TID1_6>R_MIN'; end;
      else if t_rcas = 'ZERO' then do; t_hlc = 4; t_chemin = 'TID1_6>R_ZERO'; t_flag = 'Q47'; end;
      else if t_rcas = 'NULL' then do; t_hlc = 5; t_chemin = 'TID1_6>DECL_NUL'; end;
      else do;
        if t_r < &r_cav. then t_hlc = 4; else t_hlc = 5;
        t_chemin = 'TID1_6>RATIO';
      end;
    end;

    else t_chemin = 'MODELE_INCONNU';
  end;
  hlc_t = t_hlc;
  keep cle_ano vari hlc_t;
run;
proc sql;
  create table work.br_t as
  select cle_ano, max(case when vari = 'T0' then hlc_t end) as ht0, max(case when vari = 'T3' then hlc_t end) as ht3, max(case when vari = 'T4' then hlc_t end) as ht4
  from work.br_tree_out group by cle_ano;
  create table work.br3 as
  select b.*, t.ht0, t.ht3, t.ht4 from work.br2 as b left join work.br_t as t on b.cle_ano = t.cle_ano;
quit;
/* S3.2 HLC de chaque etat : la HLC rejouee remplace la HLC de production seulement si elle change le resultat de l arbre */
data work.br4;
  set work.br3(where=(mvt = 'STAY'));
  tr0 = ht0; tr3 = coalesce(ht3, tr0); tr4 = coalesce(ht4, tr3);
  h0 = hlc1;
  h3 = ifn(not missing(tr3) and not missing(tr0) and tr3 ne tr0, tr3, h0);
  h4 = ifn(not missing(tr4) and not missing(tr3) and tr4 ne tr3, tr4, h3);
  h5 = hlc0;
run;
data work.brl;                                   /* sept etats par contrat */
  set work.br4;
  length etat $2;
  keep cle_ano ptf1 etat hlc ex_b ex_hb nper;
  ptf1 = ptf1;
  etat = 'S0'; hlc = h0; ex_b = w1; ex_hb = hb1; nper = &periode1.; output;
  etat = 'S1'; hlc = h0; ex_b = w0; ex_hb = hb1; nper = &periode1.; output;
  etat = 'S2'; hlc = h0; ex_b = w0; ex_hb = hb0; nper = &periode1.; output;
  etat = 'S3'; hlc = h3; ex_b = w0; ex_hb = hb0; nper = &periode1.; output;
  etat = 'S4'; hlc = h4; ex_b = w0; ex_hb = hb0; nper = &periode1.; output;
  etat = 'S5'; hlc = h5; ex_b = w0; ex_hb = hb0; nper = &periode1.; output;
  etat = 'S6'; hlc = h5; ex_b = w0; ex_hb = hb0; nper = &periode.;  output;
run;
proc sql;
  create table work.brl_e as
  select c.cle_ano, c.ptf1, c.etat,
         case when p.LGDD is not null and p.FL is not null and t.TIRAGE is not null and c.hlc is not null
              then p.FL * p.LGDD * (c.ex_b + t.TIRAGE * c.ex_hb) end as ecl
  from work.brl as c
       left join work.par as p on c.nper = p.N_PERD and c.ptf1 = p.PORTEFEUILLE and c.hlc = p.HLC
       left join work.par_td as t on c.nper = t.N_PERD and c.ptf1 = t.PORTEFEUILLE;
  create table work.brw as
  select cle_ano, ptf1, max(case when etat = 'S0' then ecl end) as s0, max(case when etat = 'S1' then ecl end) as s1, max(case when etat = 'S2' then ecl end) as s2,
         max(case when etat = 'S3' then ecl end) as s3, max(case when etat = 'S4' then ecl end) as s4, max(case when etat = 'S5' then ecl end) as s5,
         max(case when etat = 'S6' then ecl end) as s6
  from work.brl_e group by cle_ano, ptf1;
quit;
/* S3.3 Effets par portefeuille (montants en EUR) : lignes longues ptf x effet */
data work.eff_long;
  length PORTEFEUILLE $8 EFFET_CODE $4;
  set work.brw(in=a) work.br2(in=b where=(mvt in ('P01', 'P02', 'P03')));
  if a then do;
    PORTEFEUILLE = ptf1;
    EFFET_CODE = 'E01'; MONTANT = s1 - s0; output;
    EFFET_CODE = 'E02'; MONTANT = s2 - s1; output;
    EFFET_CODE = 'E03'; MONTANT = s3 - s2; output;
    EFFET_CODE = 'E04'; MONTANT = s4 - s3; output;
    EFFET_CODE = 'E05'; MONTANT = s5 - s4; output;
    EFFET_CODE = 'E06'; MONTANT = s6 - s5; output;
  end;
  else do;
    if mvt = 'P01' then do; PORTEFEUILLE = ptf0; EFFET_CODE = 'P01'; MONTANT = ecl0; output; end;
    else if mvt = 'P02' then do; PORTEFEUILLE = ptf1; EFFET_CODE = 'P02'; MONTANT = -ecl1; output; end;
    else if mvt = 'P03' then do;
      if e1 then do; PORTEFEUILLE = ptf1; EFFET_CODE = 'P03'; MONTANT = -ecl1; output; end;
      if e0 then do; PORTEFEUILLE = ptf0; EFFET_CODE = 'P03'; MONTANT = ecl0; output; end;
    end;
  end;
  keep PORTEFEUILLE EFFET_CODE MONTANT;
run;
proc sql;
  create table work.ptf_l as select distinct portefeuille as PORTEFEUILLE from work.c0;
  create table work.eff_s as select PORTEFEUILLE, EFFET_CODE, sum(MONTANT) as MONTANT from work.eff_long group by PORTEFEUILLE, EFFET_CODE;
  create table work.ouv as                                           /* ouverture et cloture : somme des provisions recalculees de chaque arrete */
  select ptf1 as PORTEFEUILLE, sum(ecl1) as ouv_v from work.br2 where e1 group by ptf1;
  create table work.clo as
  select ptf0 as PORTEFEUILLE, sum(ecl0) as clo_v from work.br2 where e0 group by ptf0;
  create table work.bridge as
  select p.PORTEFEUILLE, d.EFFET_CODE, d.LIBELLE, d.ORDRE, d.GROUPE,
         case when d.EFFET_CODE = 'OPEN' then coalesce(o.ouv_v, 0)
              when d.EFFET_CODE = 'E07' then coalesce(c.CLOSE, 0) - coalesce(o.ouv_v, 0) - coalesce((select sum(MONTANT) from work.eff_s as s where s.PORTEFEUILLE = p.PORTEFEUILLE), 0)
              else coalesce((select sum(MONTANT) from work.eff_s as s where s.PORTEFEUILLE = p.PORTEFEUILLE and s.EFFET_CODE = d.EFFET_CODE), 0) end as MONTANT format=comma18.2
  from work.ptf_l as p cross join work.dim_effet as d left join work.ouv as o on p.PORTEFEUILLE = o.PORTEFEUILLE left join work.clo as c on p.PORTEFEUILLE = c.PORTEFEUILLE
  order by p.PORTEFEUILLE, d.ORDRE;
  create table work.bridge_g as
  select EFFET_CODE, LIBELLE, ORDRE, GROUPE, sum(MONTANT) as MONTANT format=comma18.2 from work.bridge group by EFFET_CODE, LIBELLE, ORDRE, GROUPE order by ORDRE;
  create table work.bridge_recon as
  select b.PORTEFEUILLE, sum(case when b.EFFET_CODE ne 'E07' then b.MONTANT else 0 end) as bridge_hors_residu format=comma18.2,
         max(coalesce(c.clo_v, 0)) as provision_m format=comma18.2,
         max(coalesce(c.clo_v, 0)) - sum(case when b.EFFET_CODE ne 'E07' then b.MONTANT else 0 end) as residuel format=comma18.2
  from work.bridge as b left join work.clo as c on b.PORTEFEUILLE = c.PORTEFEUILLE group by b.PORTEFEUILLE;
  select coalesce(sum(MONTANT), 0) into :t_e07 trimmed from work.bridge where EFFET_CODE = 'E07';
  select coalesce(sum(MONTANT), 0) into :t_open trimmed from work.bridge where EFFET_CODE = 'OPEN';
  select coalesce(sum(clo_v), 0) into :t_close trimmed from work.clo;
quit;

/*===========================================================================
  S4. COUVERTURE, RECONCILIATION AVEC LA PRODUCTION, SENSIBILITE, VARIATIONS ATYPIQUES
===========================================================================*/
/* S4.1 Couverture : part de l encours dont la provision est calculable, par voie et par portefeuille */
proc sql;
  create table work.k1 as
  select arrete as N_PERD, role, route_att as ROUTE, portefeuille as PORTEFEUILLE, count(*) as N_CONTRATS, sum(ead) as EAD
  from out.dq_hlc_base_&periode. group by arrete, role, route_att, portefeuille;
  create table work.k2 as
  select role, portefeuille, hlc_prod, count(*) as n_contrats, sum(not missing(ecl_rec)) as n_calc, sum(ex_b + ex_hb) as ead,
         sum(case when missing(ecl_rec) then ex_b + ex_hb else 0 end) as ead_non_calc format=comma18.
  from work.cc group by role, portefeuille, hlc_prod;
  create table work.k3 as                                                    /* couverture par portefeuille et arrete */
  select role, portefeuille, count(*) as n_contrats, sum(not missing(ecl_rec)) / count(*) as part_calc format=percent8.2,
         sum(case when not missing(ecl_rec) then ex_b + ex_hb else 0 end) / max(1, sum(ex_b + ex_hb)) as part_ead format=percent8.2,
         sum(missing(hlc_prod)) as n_sans_hlc
  from work.cc group by role, portefeuille;
  create table work.k4 as                                                    /* parametres manquants et encours concerne */
  select c.arrete as N_PERD, c.portefeuille as PORTEFEUILLE, c.hlc_prod as HLC, count(*) as N_CONTRATS, sum(c.ex_b + c.ex_hb) as EAD
  from work.cc as c left join work.par as p on c.arrete = p.N_PERD and c.portefeuille = p.PORTEFEUILLE and c.hlc_prod = p.HLC
  where not missing(c.hlc_prod) and (p.LGDD is null or p.FL is null) group by c.arrete, c.portefeuille, c.hlc_prod;
  select coalesce(min(case when role = 'M' then part_ead end), 0) into :min_couv trimmed from work.k3;
  select coalesce(sum(case when role = 'M' then ex_b + ex_hb end), 0), coalesce(sum(case when role = 'M' and not missing(ecl_rec) then ex_b + ex_hb end), 0)
    into :ead_stat trimmed, :ead_calc trimmed from work.cc;
quit;
%let couv_glob = %sysevalf(&ead_calc. / %sysfunc(max(&ead_stat., 1)));
/* S4.2 Reconciliation avec la provision de production */
proc sql;
  create table work.r1 as
  select role, portefeuille, count(*) as n_contrats, sum(recon_ok) as n_ok, sum(recon_ok) / count(*) as part_ok format=percent8.2,
         sum(ecl_prod) as ecl_prod format=comma18., sum(ecl_rec) as ecl_rec format=comma18., sum(ecl_rec - ecl_prod) as ecart format=comma18.,
         (sum(ecl_rec) - sum(ecl_prod)) / max(1, abs(sum(ecl_prod))) as ecart_rel format=percent9.4
  from work.cc where not missing(ecl_rec) and not missing(ecl_prod) group by role, portefeuille order by portefeuille, role desc;
  create table work.r2 as
  select cle_ano, portefeuille, hlc_prod, ecl_prod, ecl_rec, ecart_prod, ex_b + ex_hb as ead from work.cc where slot = 'M0' and not missing(ecart_prod) and not recon_ok
  order by abs(ecart_prod) desc;
  select coalesce(sum(case when role = 'M' then recon_ok end), 0) / max(1, sum(case when role = 'M' then 1 else 0 end)) into :part_recon trimmed
  from work.cc where not missing(ecl_rec) and not missing(ecl_prod);
quit;
data work.r2; set work.r2(obs=20); run;
/* S4.3 Sensibilite : HLC +1 et HLC -1 sur la HLC de production */
proc sql;
  create table work.s1 as
  select role, portefeuille, sum(ecl_rec) as ecl_rec format=comma18., sum(ecl_plus) as ecl_plus format=comma18., sum(ecl_moin) as ecl_moin format=comma18.,
         sum(ecl_plus - ecl_rec) as delta_plus format=comma18., sum(ecl_moin - ecl_rec) as delta_moins format=comma18.,
         sum(ecl_plus - ecl_rec) / max(1, abs(sum(ecl_rec))) as rel_plus format=percent8.2, sum(ecl_moin - ecl_rec) / max(1, abs(sum(ecl_rec))) as rel_moins format=percent8.2
  from work.cc where not missing(ecl_rec) and not missing(ecl_plus) and not missing(ecl_moin) group by role, portefeuille order by portefeuille, role desc;
  create table work.s2 as                                                   /* variations les plus fortes sur les contrats communs */
  select cle_ano, ptf1 as portefeuille, ecl1, ecl0, ecl0 - ecl1 as variation format=comma18.2, (ecl0 - ecl1) / max(1, abs(ecl1)) as variation_rel format=percent9.2,
         hlc1, hlc0
  from work.br2 where mvt = 'STAY' order by abs(calculated variation) desc;
quit;
data work.s2; set work.s2(obs=20); run;

/*===========================================================================
  S5. GOUVERNANCE : decisions a prendre et statut de certification
      Les decisions sont listees dans ce programme (aucun fichier a charger). Mettre a jour STATUT,
      BLOQUANT et ECHEANCE a chaque arrete. Une decision bloquante non fermee empeche la certification.
===========================================================================*/
data work.dec;
  infile datalines dlm='|' dsd truncover;
  length DECISION_ID $6 SUJET $80 PROPRIETAIRE $30 STATUT $12 BLOQUANT $1 ECHEANCE 8;
  informat ECHEANCE yymmdd10.; format ECHEANCE yymmdd10.;
  input DECISION_ID $ SUJET $ PROPRIETAIRE $ STATUT $ BLOQUANT $ ECHEANCE;
  N_PERD = &periode.;
datalines;
Q01|Grain de calcul|Model Owner LGD|PARTIELLE|N|2026-12-15
Q02|Stage 3 / NPL|Model Owner LGD|SUPPRIMEE|N|
Q03|Exclusions|Model Owner LGD|PARTIELLE|N|2026-12-15
Q04|Seuil de 1 M|Model Owner LGD|FERMEE|N|
Q05|Loan IQ|Model Owner LGD|FERMEE|N|
Q06|PGE|Model Owner LGD|PARTIELLE|N|2026-12-15
Q07|Mapping produit|Model Owner LGD|PARTIELLE|N|2026-12-15
Q08|Corporate / Retail|Model Owner LGD|FERMEE|N|
Q09|Source du TID|Model Owner LGD|FERMEE|N|
Q10|Conversion du TID|Model Owner LGD|FERMEE|N|
Q11|Cure et redefaut|Model Owner LGD|OUVERTE|N|2026-12-15
Q12|EDX / CER|Model Owner LGD|FERMEE|N|
Q13|Write-off|Model Owner LGD|FERMEE|N|
Q14|Write-off partiel|Model Owner LGD|OUVERTE|Y|2026-11-30
Q15|Garantie presente ou eligible|Model Owner LGD|PARTIELLE|Y|2026-11-30
Q16|Liste des garanties|Model Owner LGD|FERMEE|N|
Q17|Credit Logement|Model Owner LGD|FERMEE|N|
Q18|Corporate HLC 5|Model Owner LGD|FERMEE|N|
Q19|Corporate a 6 ans|Model Owner LGD|FERMEE|N|
Q20|Immobilier a 2 ans|Model Owner LGD|A_CONFIRMER|N|2026-12-15
Q21|Immobilier Crelog|Model Owner LGD|FERMEE|N|
Q22|Conso HLC 3 et 4|Model Owner LGD|A_CONFIRMER|N|2026-12-15
Q23|Conso HLC 5|Model Owner LGD|A_CONFIRMER|N|2026-12-15
Q24|CI branche parent|Model Owner LGD|FERMEE|N|
Q25|CI garantie|Model Owner LGD|OUVERTE|Y|2026-11-30
Q26|CI ratio non calculable|Model Owner LGD|FERMEE|N|
Q27|CAV 1,051|Model Owner LGD|FERMEE|N|
Q28|CAV apres un an|Model Owner LGD|FERMEE|N|
Q29|CAV Prive / Pro|Model Owner LGD|FERMEE|N|
Q30|Ratios et signes|Model Owner LGD|FERMEE|N|
Q31|LGDD|Model Owner LGD|OUVERTE|Y|2026-11-30
Q32|Taux de tirage|Model Owner LGD|FERMEE|Y|2026-11-30
Q33|Facteur forward-looking|Model Owner LGD|PARTIELLE|N|2026-12-15
Q34|Formule de provision|Model Owner LGD|FERMEE|N|
Q35|Provision de production|Model Owner LGD|PARTIELLE|N|2026-12-15
Q36|Bridge de Cout du Risque|Model Owner LGD|SUPPRIMEE|N|
Q37|Materialite|Model Owner LGD|OUVERTE|N|2026-12-15
Q38|Attribution des effets|Model Owner LGD|OUVERTE|N|2026-12-15
Q39|Scenario corrige|Model Owner LGD|OUVERTE|N|2026-12-15
Q40|Interface de depot|Model Owner LGD|OUVERTE|N|2026-12-15
Q41|Securite detaillee|Model Owner LGD|OUVERTE|N|2026-12-15
Q42|SLA et volumes|Model Owner LGD|OUVERTE|N|2026-12-15
Q43|Run et ownership|Model Owner LGD|OUVERTE|N|2026-12-15
Q44|Regle de certification|Model Owner LGD|OUVERTE|N|2026-12-15
Q45|Noms de l encours au declassement|Data Owner Credit|OUVERTE|Y|2026-11-30
Q46|Formule du ratio R|Model Owner LGD|OUVERTE|Y|2026-11-30
Q47|Zero dans la table du ratio|Model Owner LGD|OUVERTE|Y|2026-11-30
Q48|EDX / CER et top apurement manquants|Model Owner LGD|OUVERTE|N|2026-12-15
Q49|Categorie CAV|Data Owner Credit|OUVERTE|Y|2026-11-30
Q50|Exposition hors bilan|Data Owner Credit|OUVERTE|Y|2026-11-30
Q51|Facteur forward-looking|Finance|OUVERTE|Y|2026-11-30
Q52|Taux PGE|Finance|OUVERTE|Y|2026-11-30
Q53|Net Credit Logement|Finance|OUVERTE|Y|2026-11-30
Q54|Fichier de mapping|Model Owner LGD|OUVERTE|N|2026-12-15
Q55|Signe de MTENGB|Model Owner LGD|OUVERTE|N|2026-12-15
Q56|datdef|Model Owner LGD|OUVERTE|N|2026-12-15
Q57|Variables d exclusion|Data Owner Credit|OUVERTE|Y|2026-11-30
;
run;
proc sql noprint;
  select count(*) into :n_bloq_dq trimmed from out.dq_resultats_&periode. where role = 'M' and severite = 'Bloquant' and statut = 'ROUGE';
  select count(*) into :n_conc trimmed from out.dq_hlc_base_&periode. where slot = 'M0' and route_att = 'STAT' and match_status in ('MATCH', 'MISMATCH');
  select count(*) into :n_match trimmed from out.dq_hlc_base_&periode. where slot = 'M0' and route_att = 'STAT' and match_status = 'MATCH';
quit;
%let taux_concord = %sysevalf(&n_match. / %sysfunc(max(&n_conc., 1)));
data work.cert0;
  length N_PERD 8 MOTIF $200 ordre 8;
  N_PERD = &periode.;
  set work.dec(where=(BLOQUANT = 'Y' and STATUT in ('OUVERTE', 'PARTIELLE', 'A_CONFIRMER'))) end=fin;
  MOTIF = catx(' ', DECISION_ID, 'ouverte (bloquante) :', SUJET); ordre = _n_; output;
  keep N_PERD MOTIF ordre;
run;
data work.cert1;
  length N_PERD 8 MOTIF $200 ordre 8;
  N_PERD = &periode.;
  if &n_bloq_dq. > 0 then do; MOTIF = catx(' ', &n_bloq_dq., 'regle(s) bloquante(s) de qualite des donnees en ROUGE a M'); ordre = 1001; output; end;
  if &couv_glob. < &seuil_couv. then do; MOTIF = catx(' ', 'Couverture de la provision recalculee', put(&couv_glob., percent8.2), 'inferieure a', put(&seuil_couv., percent8.1)); ordre = 1002; output; end;
  if &part_recon. < &seuil_recon. then do; MOTIF = catx(' ', 'Reconciliation avec la production', put(&part_recon., percent8.2), 'inferieure a', put(&seuil_recon., percent8.1)); ordre = 1003; output; end;
  if abs(&t_e07.) > &tol_residu. then do; MOTIF = catx(' ', 'Residuel du bridge non nul :', put(&t_e07., comma18.2), 'EUR'); ordre = 1004; output; end;
  if &taux_concord. < &seuil_concord. then do; MOTIF = catx(' ', 'Concordance HLC', put(&taux_concord., percent8.2), 'inferieure a', put(&seuil_concord., percent8.1)); ordre = 1005; output; end;
  if &n_par_manq. + &n_td_manq. > 0 then do; MOTIF = catx(' ', &n_par_manq. + &n_td_manq., 'parametre(s) financier(s) manquant(s)'); ordre = 1006; output; end;
run;
data work.cert;
  length STATUT $16 MOTIF_ID $4;
  set work.cert0 work.cert1;
  retain k 0;
  k + 1; MOTIF_ID = cats('M', put(k, z2.)); STATUT = 'NOT_CERTIFIED';
  keep N_PERD STATUT MOTIF_ID MOTIF;
run;
proc sql noprint; select count(*) into :n_motifs trimmed from work.cert; quit;
data work.cert_f;
  length N_PERD 8 STATUT $16 MOTIF_ID $4 MOTIF $200;
  if &n_motifs. = 0 then do; N_PERD = &periode.; STATUT = 'CERTIFIED'; MOTIF_ID = 'M00'; MOTIF = 'Aucun motif de non certification'; output; end;
  else do until (eof); set work.cert end=eof; output; end;
  stop;
run;

/*===========================================================================
  S6. CLASSEUR EXCEL ETAPE 5 : onglets ECL_1 a ECL_6 et DEC_1 a inserer apres HLC_6
      (positions 23 a 29 du fichier de visualisation), M-1 et M cote a cote.
===========================================================================*/
ods excel file="&dir_out./excel/DQ_Etape5_&periode..xlsx" style=styles.dqxl
    options(sheet_interval="none" embedded_titles="yes" flow="tables" gridlines="off");
ods graphics on / width=900px height=480px;

ods excel options(sheet_interval="now" sheet_name="ECL_1_Provision" absolute_column_width="12,10,12,16,16,18,18,18");
title1 j=l "^S={background=cx0A2F3F color=white fontsize=15pt fontweight=bold} ECL_1 - Provision : production, recalculee, corrigee";
title2 j=l "Onglet ECL_1_Provision : a inserer apres l onglet HLC_6_Transitions du fichier de visualisation (position 23) - source : S1 scenarios";
title3 j=l "^S={color=cxC00000 fontweight=bold} &banner.";

%let c_e1 = Sur &t_ncalc. contrats statistiques calculables a M (sur &t_nctr.), la provision recalculee avec la HLC de la production vaut %sysfunc(putn(&t_rec., comma18.)) EUR et la provision corrigee %sysfunc(putn(&t_cor., comma18.)) EUR. Les parametres financiers (section S0.8) sont a renseigner : une valeur manquante rend la provision non calculable, elle n est jamais remplacee par zero.;
ods text = "^S={background=cxE1EFF5} &c_e1.";

proc report data=work.e1 nowd;
  column portefeuille role n_contrats n_calc ead_bil ead_hb ecl_prod ecl ecl_corrige;
  define portefeuille / group 'Portefeuille'; define role / display 'Arrete';
  define n_contrats / 'Contrats' format=comma12.; define n_calc / 'Calculables' format=comma12.;
  define ead_bil / 'EAD bilan'; define ead_hb / 'EAD hors bilan'; define ecl_prod / 'Provision production';
  define ecl / 'Provision recalculee'; define ecl_corrige / 'Provision corrigee';
  rbreak after / summarize style=[fontweight=bold];
run;
proc sql;
  create table work.e1g as
  select role, portefeuille, 'Production' as scenario length=12, ecl_prod as montant from work.e1
  union all select role, portefeuille, 'Recalculee', ecl from work.e1
  union all select role, portefeuille, 'Corrigee', ecl_corrige from work.e1;
quit;
title1 j=l "G1. Provision par scenario et portefeuille (EUR), M-1 contre M";
proc sgpanel data=work.e1g;
  panelby role / columns=2 novarname;
  vbar portefeuille / response=montant group=scenario groupdisplay=cluster;
  rowaxis grid label="EUR" valuesformat=comma18.; colaxis display=(nolabel);
run;
title1;

ods excel options(sheet_interval="now" sheet_name="ECL_2_Bridge" absolute_column_width="10,44,18,18,18,18,18,18,18");
title1 j=l "^S={background=cx0A2F3F color=white fontsize=15pt fontweight=bold} ECL_2 - Bridge de la provision recalculee de M-1 a M";
title2 j=l "Onglet ECL_2_Bridge : a inserer apres l onglet ECL_1_Provision du fichier de visualisation (position 24) - source : S3 bridge";
title3 j=l "^S={color=cxC00000 fontweight=bold} &banner.";

%let c_e2 = Ouverture %sysfunc(putn(&t_open., comma18.)) EUR, cloture %sysfunc(putn(&t_close., comma18.)) EUR, residuel E07 %sysfunc(putn(&t_e07., comma18.2)) EUR. Un residuel non nul indique des contrats dont un etat intermediaire n est pas calculable (HLC rejouee sans parametre). &n_sanscle. ligne(s) sans cle contrat sont exclues du bridge.;
ods text = "^S={background=cxE1EFF5} &c_e2.";

proc report data=work.bridge_g nowd;
  column ordre EFFET_CODE LIBELLE GROUPE MONTANT;
  define ordre / noprint order; define EFFET_CODE / 'Code'; define LIBELLE / 'Effet'; define GROUPE / 'Groupe'; define MONTANT / 'Montant (EUR)' format=comma18.2;
run;
data work.wf;                                    /* cascade : barres flottantes */
  set work.bridge_g end=fin;
  length signe $8;
  retain cum 0;
  if EFFET_CODE = 'OPEN' then do; bas = 0; haut = MONTANT; cum = MONTANT; signe = 'Total'; end;
  else do; bas = min(cum, cum + MONTANT); haut = max(cum, cum + MONTANT); cum = cum + MONTANT; signe = ifc(MONTANT >= 0, 'Hausse', 'Baisse'); end;
  output;
  if fin then do; EFFET_CODE = 'CLOSE'; LIBELLE = 'Provision M'; ORDRE = 99; bas = 0; haut = cum; signe = 'Total'; output; end;
run;
data work.attr_wf;
  length ID $4 VALUE $8 FILLCOLOR $10;
  ID = 'wf';
  VALUE = 'Total'; FILLCOLOR = 'cx0E5A78'; output;
  VALUE = 'Hausse'; FILLCOLOR = 'cxE34948'; output;
  VALUE = 'Baisse'; FILLCOLOR = 'cx1BAF7A'; output;
run;
title1 j=l "G2. Cascade de la provision recalculee, M-1 vers M (EUR)";
proc sgplot data=work.wf dattrmap=work.attr_wf noautolegend;
  highlow x=EFFET_CODE low=bas high=haut / type=bar group=signe attrid=wf barwidth=0.7;
  xaxis discreteorder=data display=(nolabel); yaxis grid label="EUR" valuesformat=comma18.;
run;
title1 j=l "Detail par portefeuille (EUR)";
proc transpose data=work.bridge out=work.bw(drop=_name_) prefix=c_; by PORTEFEUILLE; id EFFET_CODE; var MONTANT; run;
proc report data=work.bw nowd;
  column PORTEFEUILLE c_OPEN c_P01 c_P02 c_P03 c_E01 c_E02 c_E03 c_E04 c_E05 c_E06 c_E07;
  define PORTEFEUILLE / 'Portefeuille'; define c_OPEN / 'Ouverture M-1' format=comma18.; define c_P01 / 'Entrees' format=comma18.; define c_P02 / 'Sorties' format=comma18.;
  define c_P03 / 'Changements' format=comma18.; define c_E01 / 'Bilan' format=comma18.; define c_E02 / 'Hors bilan' format=comma18.; define c_E03 / 'Garantie' format=comma18.;
  define c_E04 / 'Duree, statut' format=comma18.; define c_E05 / 'HLC reliquat' format=comma18.; define c_E06 / 'Parametres' format=comma18.; define c_E07 / 'Residuel' format=comma18.;
run;
title1 j=l "Controle : ouverture + effets = provision recalculee de M";
proc report data=work.bridge_recon nowd; column PORTEFEUILLE bridge_hors_residu provision_m residuel; run;
title1;

ods excel options(sheet_interval="now" sheet_name="ECL_3_Impact_anomalies" absolute_column_width="22,14,14,16,16,18,18");
title1 j=l "^S={background=cx0A2F3F color=white fontsize=15pt fontweight=bold} ECL_3 - Impact des anomalies sur la provision";
title2 j=l "Onglet ECL_3_Impact_anomalies : a inserer apres l onglet ECL_2_Bridge du fichier de visualisation (position 25) - source : S2 impact";
title3 j=l "^S={color=cxC00000 fontweight=bold} &banner.";

%let c_e3 = Impact chiffre des anomalies a M : %sysfunc(putn(&t_imp., comma18.)) EUR (calcule et estime). Un impact negatif signifie que la correction diminue la provision. Les anomalies sans driver sont NOT_QUANTIFIABLE : elles sont comptees mais non chiffrees. Les impacts par regle ne sont pas additifs entre eux.;
ods text = "^S={background=cxE1EFF5} &c_e3.";

title1 j=l "Effet de chaque driver pris seul (provision avec driver recalcule moins provision de l arbre)";
proc report data=work.i3 nowd; column driver_id role n_contrats ead impact_ecl;
  define driver_id / group 'Driver'; define role / display 'Arrete'; define n_contrats / format=comma12. 'Contrats'; define ead / format=comma18. 'EAD'; define impact_ecl / format=comma18.2 'Impact (EUR)'; run;
title1 j=l "Ecart provision corrigee moins recalculee, par cause d ecart de HLC (additif par contrat)";
proc report data=work.i4 nowd; column cause role n_contrats ead impact_ecl;
  define cause / group 'Cause'; define role / display 'Arrete'; define n_contrats / format=comma12. 'Contrats'; define ead / format=comma18. 'EAD'; define impact_ecl / format=comma18.2 'Impact (EUR)'; run;
title1 j=l "Impact par portefeuille et statut de chiffrage";
proc report data=work.i1 nowd; column PORTEFEUILLE STATUT_IMPACT role N_ANOMALIES EAD IMPACT_ECL;
  define PORTEFEUILLE / group; define STATUT_IMPACT / group 'Statut'; define role / display 'Arrete'; define N_ANOMALIES / format=comma12. 'Anomalies'; define EAD / format=comma18.; define IMPACT_ECL / format=comma18.2 'Impact (EUR)'; run;
proc sql;
  create table work.i2m as select RULE_ID, DRIVER_ID, STATUT_IMPACT, N_ANOMALIES, EAD, IMPACT_ECL, IMPACT_ABS from work.i2 where role = 'M' order by IMPACT_ABS desc;
quit;
data work.i2m; set work.i2m(obs=20); run;
title1 j=l "Les 20 regles au plus fort impact a M";
proc report data=work.i2m nowd; column RULE_ID DRIVER_ID STATUT_IMPACT N_ANOMALIES EAD IMPACT_ECL;
  define RULE_ID / 'Regle'; define DRIVER_ID / 'Driver'; define STATUT_IMPACT / 'Statut'; define N_ANOMALIES / format=comma12. 'Anomalies'; define EAD / format=comma18.; define IMPACT_ECL / format=comma18.2 'Impact (EUR)'; run;
title1 j=l "G3. Impact par regle a M (EUR)";
proc sgplot data=work.i2m;
  hbar RULE_ID / response=IMPACT_ECL group=STATUT_IMPACT categoryorder=respdesc;
  xaxis grid label="Impact (EUR)" valuesformat=comma18.; yaxis display=(nolabel);
run;
title1;

ods excel options(sheet_interval="now" sheet_name="ECL_4_Couverture" absolute_column_width="12,10,12,14,14,14,16");
title1 j=l "^S={background=cx0A2F3F color=white fontsize=15pt fontweight=bold} ECL_4 - Couverture du calcul de provision";
title2 j=l "Onglet ECL_4_Couverture : a inserer apres l onglet ECL_3_Impact_anomalies du fichier de visualisation (position 26) - source : S4.1 couverture";
title3 j=l "^S={color=cxC00000 fontweight=bold} &banner.";

%let c_e4 = Part de l encours statistique dont la provision est calculable a M : %sysfunc(putn(&couv_glob., percent8.2)). Les encours hors voie statistique (PGE, individuel, exclus) ne sont pas recalcules ici. Les couples portefeuille x HLC sans LGDD ou FL renseigne sont listes dans le dernier tableau.;
ods text = "^S={background=cxE1EFF5} &c_e4.";

title1 j=l "Couverture par portefeuille";
proc report data=work.k3 nowd; column portefeuille role n_contrats part_calc part_ead n_sans_hlc;
  define portefeuille / group; define role / display 'Arrete'; define n_contrats / format=comma12. 'Contrats'; define part_calc / 'Part des contrats calculables'; define part_ead / 'Part de l encours calculable'; define n_sans_hlc / format=comma12. 'Sans HLC de production';
  compute part_ead; if not missing(part_ead) and part_ead < &seuil_couv. then call define(_col_, 'style', 'style=[background=cxF4CCCC]'); endcomp;
run;
title1 j=l "Population par voie de traitement (tous contrats)";
proc report data=work.k1 nowd; column ROUTE role PORTEFEUILLE N_CONTRATS EAD;
  define ROUTE / group; define role / display 'Arrete'; define PORTEFEUILLE / group; define N_CONTRATS / format=comma12.; define EAD / format=comma18.; run;
title1 j=l "Parametres manquants : couples portefeuille x HLC avec encours";
proc report data=work.k4 nowd; column N_PERD PORTEFEUILLE HLC N_CONTRATS EAD; define EAD / format=comma18.; run;
title1 j=l "G4. Part de l encours calculable, par portefeuille";
proc sgplot data=work.k3 dattrmap=work.attr_role;
  vbar portefeuille / response=part_ead group=role groupdisplay=cluster attrid=role;
  refline &seuil_couv. / axis=y lineattrs=(color=red pattern=dash);
  yaxis grid min=0 max=1 label="Part de l encours" valuesformat=percent8.; xaxis display=(nolabel);
run;
title1;

ods excel options(sheet_interval="now" sheet_name="ECL_5_Reconciliation" absolute_column_width="12,10,12,12,12,18,18,18,12");
title1 j=l "^S={background=cx0A2F3F color=white fontsize=15pt fontweight=bold} ECL_5 - Reconciliation avec la provision de production";
title2 j=l "Onglet ECL_5_Reconciliation : a inserer apres l onglet ECL_4_Couverture du fichier de visualisation (position 27) - source : S4.2 reconciliation";
title3 j=l "^S={color=cxC00000 fontweight=bold} &banner.";

%let c_e5 = Part des contrats dont la provision recalculee est egale a celle de la production a la tolerance pres (&tol_prov_abs. EUR ou %sysfunc(putn(&tol_prov_rel., percent8.2)) de la provision) : %sysfunc(putn(&part_recon., percent8.2)). Un ecart persistant signale un moteur de calcul ou des parametres differents de ceux de la production.;
ods text = "^S={background=cxE1EFF5} &c_e5.";

proc report data=work.r1 nowd; column portefeuille role n_contrats n_ok part_ok ecl_prod ecl_rec ecart ecart_rel;
  define portefeuille / group; define role / display 'Arrete'; define n_contrats / format=comma12. 'Contrats'; define n_ok / format=comma12. 'Reconcilies'; define part_ok / 'Part reconciliee';
  define ecl_prod / 'Provision production'; define ecl_rec / 'Provision recalculee'; define ecart / 'Ecart'; define ecart_rel / 'Ecart relatif';
  compute part_ok; if not missing(part_ok) and part_ok < &seuil_recon. then call define(_col_, 'style', 'style=[background=cxF4CCCC]'); endcomp;
run;
title1 j=l "Les 20 plus gros ecarts de contrat a M";
proc report data=work.r2 nowd; column cle_ano portefeuille hlc_prod ecl_prod ecl_rec ecart_prod ead;
  define cle_ano / 'Contrat'; define ecl_prod / format=comma18.2 'Production'; define ecl_rec / format=comma18.2 'Recalculee'; define ecart_prod / format=comma18.2 'Ecart'; define ead / format=comma18.; run;
title1 j=l "G5. Provision de production contre recalculee, par portefeuille (EUR)";
proc sgplot data=work.r1(where=(role = 'M'));
  vbar portefeuille / response=ecl_prod fillattrs=(color=cx9DB7D9) legendlabel="Production";
  vbar portefeuille / response=ecl_rec fillattrs=(color=cx0E5A78) barwidth=0.5 legendlabel="Recalculee";
  yaxis grid label="EUR" valuesformat=comma18.; xaxis display=(nolabel);
run;
title1;

ods excel options(sheet_interval="now" sheet_name="ECL_6_Sensibilite" absolute_column_width="12,10,16,16,16,16,16,12,12");
title1 j=l "^S={background=cx0A2F3F color=white fontsize=15pt fontweight=bold} ECL_6 - Sensibilite a la HLC et variations atypiques";
title2 j=l "Onglet ECL_6_Sensibilite : a inserer apres l onglet ECL_5_Reconciliation du fichier de visualisation (position 28) - source : S4.3";
title3 j=l "^S={color=cxC00000 fontweight=bold} &banner.";

%let c_e6 = Chaque contrat est decale d une classe HLC vers le haut puis vers le bas : l effet mesure l importance d une erreur de classification sur la provision. Les 20 plus fortes variations de contrats communs sont a expliquer avec les Data Owners.;
ods text = "^S={background=cxE1EFF5} &c_e6.";

proc report data=work.s1 nowd; column portefeuille role ecl_rec delta_plus rel_plus delta_moins rel_moins;
  define portefeuille / group; define role / display 'Arrete'; define ecl_rec / format=comma18. 'Provision recalculee'; define delta_plus / format=comma18. 'HLC + 1'; define rel_plus / 'Relatif +1';
  define delta_moins / format=comma18. 'HLC - 1'; define rel_moins / 'Relatif -1'; run;
title1 j=l "Les 20 plus fortes variations de provision entre M-1 et M (contrats communs)";
proc report data=work.s2 nowd; column cle_ano portefeuille hlc1 hlc0 ecl1 ecl0 variation variation_rel;
  define cle_ano / 'Contrat'; define hlc1 / 'HLC M-1'; define hlc0 / 'HLC M'; define ecl1 / format=comma18.2 'Provision M-1'; define ecl0 / format=comma18.2 'Provision M'; define variation / 'Variation'; define variation_rel / 'Variation relative'; run;
title1 j=l "G6. Effet d un decalage de HLC sur la provision a M (EUR)";
proc sgplot data=work.s1(where=(role = 'M'));
  vbar portefeuille / response=delta_plus fillattrs=(color=cxE34948) legendlabel="HLC +1";
  vbar portefeuille / response=delta_moins fillattrs=(color=cx1BAF7A) legendlabel="HLC -1";
  yaxis grid label="EUR" valuesformat=comma18.; xaxis display=(nolabel);
run;
title1;

ods excel options(sheet_interval="now" sheet_name="DEC_1_Decisions" absolute_column_width="10,60,28,14,12,14");
title1 j=l "^S={background=cx0A2F3F color=white fontsize=15pt fontweight=bold} DEC_1 - Decisions a prendre et statut de certification";
title2 j=l "Onglet DEC_1_Decisions : a inserer apres l onglet ECL_6_Sensibilite du fichier de visualisation (position 29) - source : S5 gouvernance";
title3 j=l "^S={color=cxC00000 fontweight=bold} &banner.";

%let c_d1 = Statut de certification a M : %sysfunc(ifc(&n_motifs. = 0, CERTIFIED, NOT_CERTIFIED)) (&n_motifs. motif(s)). Les decisions bloquantes ouvertes, les regles de qualite bloquantes en ROUGE, la couverture, la reconciliation, le residuel du bridge et la concordance HLC conditionnent la certification.;
ods text = "^S={background=cxE1EFF5} &c_d1.";

title1 j=l "Motifs de non certification";
proc report data=work.cert_f nowd; column MOTIF_ID STATUT MOTIF; define MOTIF / 'Motif' style(column)=[cellwidth=8in]; run;
title1 j=l "Decisions";
proc report data=work.dec nowd; column DECISION_ID SUJET PROPRIETAIRE STATUT BLOQUANT ECHEANCE;
  define DECISION_ID / 'Decision'; define SUJET / 'Sujet'; define PROPRIETAIRE / 'Proprietaire'; define STATUT / 'Statut'; define BLOQUANT / 'Bloquant'; define ECHEANCE / 'Echeance';
  compute STATUT; if STATUT = 'OUVERTE' then call define(_col_, 'style', 'style=[background=cxFCE5CD]'); else if STATUT = 'FERMEE' then call define(_col_, 'style', 'style=[background=cxD9EAD3]'); endcomp;
run;
title1 j=l "G7. Decisions par statut et caractere bloquant";
proc sgplot data=work.dec;
  vbar STATUT / group=BLOQUANT groupdisplay=stack stat=freq datalabel;
  yaxis grid label="Decisions"; xaxis display=(nolabel);
run;
title1;
ods excel close;
ods graphics off;
title; footnote;

/*===========================================================================
  S7. PUBLICATION POWER BI : memes noms de tables et de colonnes que les maquettes.
      PUB_HLC_DRIVER_DIFF est republie avec ECL_ECART renseigne : il remplace le fichier de l etape 4.
===========================================================================*/
proc sql;
  create table pub.PUB_ECL_AGG as
  select arrete as N_PERD, portefeuille as PORTEFEUILLE length=8, count(*) as N_CONTRATS, sum(ex_b) as EAD_BIL, sum(ex_hb) as EAD_HB,
         sum(ecl_rec) as ECL, sum(ecl_prod) as ECL_PROD, sum(ecl_cor) as ECL_CORRIGE
  from work.cc where calc_ok group by arrete, portefeuille;
  create table pub.PUB_ECL_EFFECT as
  select &periode. as N_PERD, PORTEFEUILLE length=8, EFFET_CODE length=4, MONTANT from work.bridge;
  create table pub.PUB_IMPACT_AGG as
  select N_PERD, PORTEFEUILLE, STATUT_IMPACT, N_ANOMALIES, EAD, IMPACT_ECL from work.i1;
  create table pub.PUB_IMPACT_REGLE as
  select N_PERD, RULE_ID, DRIVER_ID, STATUT_IMPACT, N_ANOMALIES, EAD, IMPACT_ECL from work.i2;
  create table pub.PUB_COUVERTURE as
  select &periode. as N_PERD, role as ROLE, portefeuille as PORTEFEUILLE length=8, n_contrats as N_CONTRATS, part_calc as PART_CONTRATS_CALCULABLES, part_ead as PART_EAD_CALCULABLE, n_sans_hlc as N_SANS_HLC
  from work.k3;
  create table pub.PUB_RECONCILIATION as
  select &periode. as N_PERD, role as ROLE, portefeuille as PORTEFEUILLE length=8, n_contrats as N_CONTRATS, n_ok as N_RECONCILIES, ecl_prod as ECL_PROD, ecl_rec as ECL_RECALC, ecart as ECART
  from work.r1;
  create table pub.PUB_SENSIBILITE as
  select &periode. as N_PERD, role as ROLE, portefeuille as PORTEFEUILLE length=8, ecl_rec as ECL_RECALC, delta_plus as DELTA_HLC_PLUS1, delta_moins as DELTA_HLC_MOINS1
  from work.s1;
quit;
/* Republication de PUB_HLC_DRIVER_DIFF avec l ecart de provision */
data work.drv2;
  set work.cc(where=(not missing(hlc_att)));
  array s{7} s_d1-s_d7; array h{7} h_d1-h_d7; array e{7} ecl_d1-ecl_d7;
  length DRIVER_ID $4;
  do k = 1 to 7;
    if s{k} = 1 and not missing(h{k}) and h{k} ne hlc_att then do;
      DRIVER_ID = cats('D', k); HLC_RECALC = h{k}; ECL_DIFF = ifn(not missing(e{k}) and not missing(ecl_att), e{k} - ecl_att, .); EAD_C = ex_b + ex_hb; output;
    end;
  end;
  keep arrete portefeuille DRIVER_ID hlc_att HLC_RECALC ECL_DIFF EAD_C;
run;
proc sql;
  create table pub.PUB_HLC_DRIVER_DIFF as
  select arrete as N_PERD, portefeuille as PORTEFEUILLE length=8, DRIVER_ID, hlc_att as HLC_PROD, HLC_RECALC, count(*) as N_CONTRATS, sum(EAD_C) as EAD, sum(ECL_DIFF) as ECL_ECART
  from work.drv2 group by arrete, portefeuille, DRIVER_ID, hlc_att, HLC_RECALC;
quit;
/* Audit des parametres : une ligne par valeur de parametre et par arrete */
data pub.PUB_PARAM_AUDIT;
  set work.par(in=a) work.par_td(in=b);
  length PARAMETER_ID $32 VERSION $24 APPROBATION $24;
  keep N_PERD PARAMETER_ID VALEUR VALID_FROM VALID_TO VERSION APPROBATION;
  format VALID_FROM VALID_TO yymmdd10.;
  VALID_FROM = input(cats(put(N_PERD, 6.), '01'), yymmdd8.);
  VALID_TO = intnx('month', VALID_FROM, 0, 'end');
  VERSION = "&version_param."; APPROBATION = "&approbation.";
  if a then do;
    PARAMETER_ID = cats('LGDD_', PORTEFEUILLE, '_H', HLC); VALEUR = LGDD; output;
    PARAMETER_ID = cats('FL_', PORTEFEUILLE, '_H', HLC);   VALEUR = FL;   output;
  end;
  else do; PARAMETER_ID = cats('TIRAGE_', PORTEFEUILLE); VALEUR = TIRAGE; output; end;
run;
data pub.PUB_DECISION; set work.dec(keep=N_PERD DECISION_ID SUJET PROPRIETAIRE STATUT BLOQUANT ECHEANCE); format ECHEANCE yymmdd10.; run;
data pub.PUB_CERTIFICATION; set work.cert_f; run;
data pub.DIM_EFFET; set work.dim_effet(keep=EFFET_CODE LIBELLE ORDRE GROUPE); run;
data pub.DIM_MVT; set work.dim_mvt; run;

data work.lst_c;
  infile datalines dlm='|' truncover;
  length TABLE_NAME $32 VAGUE 8;
  input TABLE_NAME $ VAGUE;
datalines;
PUB_ECL_AGG|3
PUB_ECL_EFFECT|3
PUB_IMPACT_AGG|3
PUB_IMPACT_REGLE|3
PUB_HLC_DRIVER_DIFF|3
PUB_PARAM_AUDIT|3
PUB_COUVERTURE|3
PUB_RECONCILIATION|3
PUB_SENSIBILITE|3
PUB_DECISION|4
PUB_CERTIFICATION|4
DIM_EFFET|0
DIM_MVT|0
;
run;
/* Consolidation : toutes les tables PUB_* et DIM_* de ce programme sont empilees dans UN seul CSV,
   PUB_FACT_E5.csv, au format long : FACT_TYPE (table d origine), N_PERD, cles K1 a K8 (texte),
   MESURE et VALEUR. Le nom d origine de chaque cle est dans PUB_CONTROLE_E5.csv (lignes SCHEMA).
   Dans Power BI : filtrer sur FACT_TYPE, renommer K1 a K8 d apres le schema (Power Query). */
data work.lst_fact; length TABLE_NAME $32 VAGUE 8; TABLE_NAME = 'PUB_FACT_E5'; VAGUE = 9; run;
proc sql;
  create table work.f_cols0 as
  select upcase(memname) as memname length=32, name length=32, type, varnum
  from dictionary.columns where libname = 'PUB' and upcase(memname) in (select upcase(TABLE_NAME) from work.lst_c);
quit;
data work.f_cols1;
  set work.f_cols0;
  length role $8;
  if upcase(name) = 'N_PERD' then do; role = 'PERIODE'; ordre = 0; end;
  else if type = 'char' or prxmatch('/^(HLC|ORDRE|BLOQUANT|VALID_|ECHEANCE)/i', strip(name)) then do; role = 'CLE'; ordre = 1; end;
  else do; role = 'MESURE'; ordre = 2; end;
run;
proc sort data=work.f_cols1; by memname ordre varnum; run;
data work.f_cols;
  set work.f_cols1;
  by memname;
  retain kpos 0;
  if first.memname then kpos = 0;
  if role = 'CLE' then kpos + 1;
run;
data _null_;
  set work.f_cols end=fin;
  by memname;
  length liste $4000;
  retain liste '' nmes 0;
  if first.memname then do;
    nmes = 0;
    call execute(cats('data work.ft_', _n_, '; set pub.', memname, ';'));
    call execute('length FACT_TYPE $32 K1-K8 $60 MESURE_F $40; keep FACT_TYPE N_PERD_F K1-K8 MESURE_F VALEUR_F;');
    call execute(cats('FACT_TYPE = "', memname, '"; N_PERD_F = .; VALEUR_F = .; MESURE_F = "";'));
    liste = catx(' ', liste, cats('work.ft_', _n_));
  end;
  if role = 'PERIODE' then call execute(cats('N_PERD_F = ', name, ';'));
  else if role = 'CLE' then call execute(cats('K', kpos, ' = strip(vvalue(', name, '));'));
  else do; nmes + 1; call execute(cats('MESURE_F = "', name, '"; VALEUR_F = ', name, '; output;')); end;
  if last.memname then do;
    if nmes = 0 then call execute('output;');
    call execute('run;');
  end;
  if fin then call execute(cats('data pub.PUB_FACT_E5; set ', liste, '; rename N_PERD_F = N_PERD MESURE_F = MESURE VALEUR_F = VALEUR; run;'));
run;
/* Schema : nom d origine des cles K1 a K8 et des mesures de chaque FACT_TYPE */
data work.schema;
  length TYPE $8 A $40 B $40 C $40 D $60 E $8 N 8;
  set work.f_cols;
  TYPE = 'SCHEMA'; A = memname; B = role; C = name; D = ''; E = ''; N = ifn(role = 'CLE', kpos, .);
  keep TYPE A B C D E N;
run;
proc export data=pub.PUB_FACT_E5 outfile="&dir_out./csv/PUB_FACT_E5.csv" dbms=csv replace; run;
data pub.PUB_MANIFEST_E5;
  length N_PERD 8 PUBLICATION_ID $24 TABLE_NAME $32 VAGUE 8 SCHEMA_VERSION $8 N_LIGNES 8 EMPREINTE $32 REFRESH_READY $1 f $300 h $32 n 8 eof 8;
  set work.lst_fact;
  N_PERD = &periode.; PUBLICATION_ID = "&run_id."; SCHEMA_VERSION = 'V2.0'; REFRESH_READY = ifc(&n_motifs. = 0, 'Y', 'N');
  f = cats("&dir_out./csv/", TABLE_NAME, '.csv'); h = ''; n = 0; eof = 0;
  do until (eof);
    infile dummy filevar=f end=eof encoding='utf-8' lrecl=32767 truncover;
    input;
    n + 1;
    h = lowcase(put(md5(cats(h, _infile_)), $hex32.));
  end;
  N_LIGNES = n - 1; EMPREINTE = h;
  keep N_PERD PUBLICATION_ID TABLE_NAME VAGUE SCHEMA_VERSION N_LIGNES EMPREINTE REFRESH_READY;
run;
data pub.PUB_RUN_LOG_E5;
  length N_PERD 8 RUN_ID $24 STEP_ID $24 DUREE_S 8 STATUT $8 MESSAGE $120;
  N_PERD = &periode.; RUN_ID = "&run_id."; STEP_ID = 'ETAPE_5'; DUREE_S = round(datetime() - &t_start., 1);
  STATUT = ifc(&n_par_manq. + &n_td_manq. > 0, 'ALERTE', 'OK');
  MESSAGE = catx(' ', 'Certification', ifc(&n_motifs. = 0, 'CERTIFIED', 'NOT_CERTIFIED'), ';', &n_motifs., 'motif(s) ;', &n_par_manq. + &n_td_manq., 'parametre(s) manquant(s)');
run;

/* Fichier de controle unique : empreinte du fichier de faits, schema des cles, journal d execution */
data pub.PUB_CONTROLE_E5;
  length TYPE $8 A $40 B $40 C $40 D $200 E $32 N 8;
  set work.schema(in=s)
      pub.PUB_MANIFEST_E5(in=m keep=N_PERD PUBLICATION_ID TABLE_NAME SCHEMA_VERSION N_LIGNES EMPREINTE REFRESH_READY)
      pub.PUB_RUN_LOG_E5(in=r keep=N_PERD RUN_ID STEP_ID DUREE_S STATUT MESSAGE);
  if m then do; TYPE = 'MANIFEST'; A = TABLE_NAME; B = PUBLICATION_ID; C = SCHEMA_VERSION; D = REFRESH_READY; E = EMPREINTE; N = N_LIGNES; end;
  if r then do; TYPE = 'RUN_LOG'; A = STEP_ID; B = RUN_ID; C = STATUT; D = MESSAGE; E = ''; N = DUREE_S; end;
  keep TYPE A B C D E N;
run;
proc export data=pub.PUB_CONTROLE_E5 outfile="&dir_out./csv/PUB_CONTROLE_E5.csv" dbms=csv replace; run;
/* FIN : renseigner les parametres (S0.8), relancer, puis charger les CSV dans Power BI. */
