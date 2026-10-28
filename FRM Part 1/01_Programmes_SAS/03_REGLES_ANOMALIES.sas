/******************************************************************************
 Programme : DQ_03_REGLES_ANOMALIES.sas      (programme 3 sur 6 - dispositif DQ LGD / provisions)
 Etape CDC : Etape 3 - Moteur DQ : regles, anomalies, causes
 Version   : v1.2
 Objet     : Routage et drivers de chaque contrat, 27 regles DQ au contrat, anomalies, causes racines, KPI par CDE et agregats.
 Entrees   : stg.*, ref.*
 Sorties   : dq.DRV_CONTRAT, dq.DQ_RULE_FLAG, dq.DQ_ISSUE, dq.DQ_CONTRAT_CRITIQUE, dq.DQ_KPI_CDE, dq.AGG_*
 Contenu   : S0 parametres | S1 routage et drivers | S2 regles | S3 anomalies et KPI
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
/* SECTION 1 : ROUTAGE ET DRIVERS */
/******************************************************************************
 Section 1.1 (ex-programme DQ_03a_ROUTAGE_DRIVERS)
 Etape     : 3 - Moteur DQ : routage des perimetres et calcul des drivers
 Objet     : Pour CHAQUE contrat en defaut (y compris exclus et individuels),
             appliquer dans l'ordre les six aiguillages de l'EdB, puis calculer
             les drivers independamment du code de production.
 Ordre de decision (EdB, cahier des charges 3.1) :
             1 Exclusions (entite, devise)          -> route EXCLU
             2 Loan IQ au niveau de la personne      -> route LOANIQ  (individuel)
             3 Seuil 1 M EUR au RP (repli dossier)   -> route SEUIL   (individuel)
             4 PGE (D24, D25)                        -> route PGE     (taux forfaitaire)
             5 Retail / Corporate (B_TIERS_RETAIL=1)
             6 Sous-portefeuille Retail via mapping  -> route STAT ou NONAFF
 Drivers   : DATDEF, TID, write-off, garantie, Credit Logement, ratio R, categorie CAV.
 Entrees   : stg.STG_CREDIT_SNAPSHOT, stg.STG_COLLATERAL_LINK, stg.STG_WO_EVENT,
             ref.REF_EXCLUSION, ref.REF_MAPPING_PRODUIT, ref.REF_LOANIQ, ref.REF_CRELOG,
             ref.REF_PGE_TAUX, ref.REF_GARANTIE_ELIG, ref.REF_SEG_DECL, ref.REF_SEUIL
 Sorties   : dq.DRV_CONTRAT (contrat x arrete : route, portefeuille, drivers,
             classe de population) ; dq.EVT_CONTRAT (indicateurs par contrat sur les
             evenements, utilises par DQ-015)
 Regles    : une donnee manquante ne vaut ni zero, ni absence de garantie, ni
             sortie du perimetre : elle produit un statut explicite (voir DQ_03b).
 Hypotheses a confirmer : Q14 (write-off partiel = montant < 95 % de l'encours),
             Q46/Q47 (denominateur de R), Q49 (valeur vide de SEG_DECL = Pro).
 Version   : v1.2
******************************************************************************/
%let t0 = %sysfunc(datetime());   /* heure de debut de la section */

/* Devise de reference : la ligne "<>EUR" de ref.REF_EXCLUSION donne EUR */
data _null_;
  set ref.REF_EXCLUSION;
  where VARIABLE = 'C_DEVISE';
  call symputx('devise_ref', substr(VALEUR, 3));
run;

/* 1. Rattachement des listes de reference (jointures un-a-un : pas de multiplication) */
proc sql;
  create table work.c1 as
  select a.*,
         case when e.VALEUR is not null then 1 else 0 end as ENT_EXCL,
         coalesce(m.SOUS_PORTEFEUILLE, '') as SOUS_PTF length=14,
         case when q.TYP_CPTE is not null then 1 else 0 end as LOANIQ_CTR,
         case when p.C_NOMNC_ENGGM is not null then 1 else 0 end as PGE_FLAG,
         coalesce(s.CATEGORIE_CAV, 'INCONNU') as CAT_BRUTE length=8
  from stg.STG_CREDIT_SNAPSHOT (where=(KEY_OCC = 1)) as a
       left join (select VALEUR from ref.REF_EXCLUSION where VARIABLE = 'C_ENTITE') as e
         on a.C_ENTITE = e.VALEUR
       left join ref.REF_MAPPING_PRODUIT as m
         on a.TYP_CPTE = m.TYP_CPTE and a.C_NOMNC_ENGGM = m.C_NOMNC_ENGGM
       left join ref.REF_LOANIQ as q
         on a.TYP_CPTE = q.TYP_CPTE
       left join (select distinct C_NOMNC_ENGGM from ref.REF_PGE_TAUX) as p
         on a.C_NOMNC_ENGGM = p.C_NOMNC_ENGGM
       left join ref.REF_SEG_DECL as s
         on a.SEG_DECL = s.SEG_DECL;
quit;

/* 2. Identifiant de rattachement, exclusion, encours pour le seuil, ratio R -------- */
data work.c2;
  length ID_PERSONNE $60 MOTIF_EXCL $8 CAT_CAV $8 r_cas $4;
  set work.c1;

  /* Personne : RP ; repli sur le dossier BCR ; dernier recours le contrat */
  if I_PRSNE_RP ne '' then ID_PERSONNE = I_PRSNE_RP;
  else if DOSSIER_ID ne '' then ID_PERSONNE = cats('DOS:', DOSSIER_ID);
  else ID_PERSONNE = cats('CTR:', ID_CONTRAT);
  RP_REPLI = (I_PRSNE_RP = '');

  /* Exclusions : entite exclue ou devise differente de la devise de reference */
  EXCL_FLAG = (ENT_EXCL = 1 or (C_DEVISE ne '' and C_DEVISE ne "&devise_ref."));
  if ENT_EXCL = 1 then MOTIF_EXCL = 'ENTITE';
  else if EXCL_FLAG then MOTIF_EXCL = 'DEVISE';

  /* Encours au declassement (bilan + hors bilan). Si le declassement bilan est
     nul ou absent, on retient l'engagement de l'arrete (EdB 3.8) */
  if DECLAS_B ne . and DECLAS_B > 0 then b_ret = DECLAS_B;
  else b_ret = max(MTENGB, 0);                       /* max ignore le manquant */
  ENCOURS_SEUIL_CTR = sum(b_ret, DECLAS_H);          /* sum ignore le manquant */

  /* Duree en defaut : plus ancienne des trois dates, en jours puis en annees entieres */
  DATDEF     = min(DATCRE, DATDECLA, D_PR_EVT);
  DATDEF_MAX = max(DATCRE, DATDECLA, D_PR_EVT);
  if DATDEF ne . then do;
    TID_JOURS  = ARRETE - DATDEF;
    TID_ANNEES = ceil(max(TID_JOURS, 0) / 365.25);
  end;

  /* Categorie CAV : liste Prive / Pro ; une valeur vide de la liste Pro est signalee (Q49) */
  CAT_CAV = CAT_BRUTE;
  if SEG_DECL = '' and CAT_BRUTE = 'PRO' then CAT_CAV = 'PRO_VIDE';

  /* Ratio R : exposition bilan / encours au declassement, table des quatre cas */
  if DECLAS_B = . then R_DENOM = .;
  else if &s_R_DENOM_BH. = 1 then R_DENOM = sum(DECLAS_B, DECLAS_H);
  else R_DENOM = DECLAS_B;
  r_bil = MTENGB; r_den = R_DENOM;
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
  R_CAS = r_cas; R_VALEUR = r_val;

  /* Exposition retenue pour les agregats : parties debitrices bilan et hors bilan */
  EAD = sum(max(MTENGB, 0), max(M_ENCRS_HB, 0));
  drop ENT_EXCL CAT_BRUTE b_ret r_cas r_val r_bil r_den;
run;

/* 3. Niveau de la personne : Loan IQ et encours agrege au RP ---------------------
   LOANIQ_PERSONNE : une seule ligne Loan IQ suffit a sortir toute la personne.
   ENCOURS_SEUIL_RP : somme des encours des contrats non exclus de la personne. */
proc sql;
  create table work.rp as
  select N_PERD, ID_PERSONNE,
         max(LOANIQ_CTR) as LOANIQ_PERSONNE,
         sum(case when EXCL_FLAG = 0 then ENCOURS_SEUIL_CTR else 0 end) as ENCOURS_SEUIL_RP
  from work.c2
  group by N_PERD, ID_PERSONNE;

  create table work.c3 as
  select a.*, b.LOANIQ_PERSONNE, b.ENCOURS_SEUIL_RP
  from work.c2 as a left join work.rp as b
       on a.N_PERD = b.N_PERD and a.ID_PERSONNE = b.ID_PERSONNE;
quit;

/* 4. Routage : les six aiguillages dans l'ordre ---------------------------------- */
data work.c4;
  length ROUTE $8 PORTEFEUILLE $8 SEUIL_NIVEAU $8;
  set work.c3;
  SEUIL_NIVEAU = ifc(RP_REPLI = 1, 'DOSSIER', 'RP');
  if EXCL_FLAG = 1 then do; ROUTE = 'EXCLU'; PORTEFEUILLE = 'EXCL'; end;
  else if LOANIQ_PERSONNE = 1 then do; ROUTE = 'LOANIQ'; PORTEFEUILLE = 'INDIV'; end;
  else if ENCOURS_SEUIL_RP > &s_SEUIL_RP. then do; ROUTE = 'SEUIL'; PORTEFEUILLE = 'INDIV'; end;
  else if PGE_FLAG = 1 then do; ROUTE = 'PGE'; PORTEFEUILLE = 'PGE'; end;
  else if B_TIERS_RETAIL = 1 then do;
    if SOUS_PTF ne '' then do; ROUTE = 'STAT'; PORTEFEUILLE = SOUS_PTF; end;
    else do; ROUTE = 'NONAFF'; PORTEFEUILLE = 'NONAFF'; end;       /* Retail non mappe : exception */
  end;
  else do; ROUTE = 'STAT'; PORTEFEUILLE = 'CORP'; end;           /* Corporate (y compris valeur vide) */
run;

/* 5. Evenements de perte : write-off ------------------------------------------------
   Evenement de perte (EdB) : MTFLUX non nul, TYP_OUP = E, TYP_MVT dans 03, 04, 14.
   On ignore les annulations, les evenements futurs et les doublons exacts. */
proc sort data=stg.STG_WO_EVENT out=work.evt1;
  by N_PERD ID_CONTRAT D_EVT MTFLUX TYP_OUP TYP_MVT;
run;

data work.evt2;
  set work.evt1;
  by N_PERD ID_CONTRAT D_EVT MTFLUX TYP_OUP TYP_MVT;
  DUP_EVT = (not first.TYP_MVT);                         /* meme evenement deja vu */
  EVT_VALIDE = (MTFLUX ne . and MTFLUX ne 0 and TYP_OUP = 'E'
                and TYP_MVT in ('03','04','14') and B_ANNUL = 0 and D_EVT <= ARRETE);
run;

proc sql;
  create table work.wo as
  select N_PERD, ID_CONTRAT, sum(abs(MTFLUX)) as WO_MONTANT
  from work.evt2
  where EVT_VALIDE = 1 and DUP_EVT = 0
  group by N_PERD, ID_CONTRAT;
quit;

/* 6. Garanties : drapeaux au lien, puis agregation au contrat --------------------- */
proc sql;
  create table work.gar1 as
  select g.N_PERD, g.ID_CONTRAT, g.ARRETE, g.D_ECHEANCE, g.M_VALO_GARANT, g.D_VALO,
         g.C_NOMNC_GARNT, g.I_PERSN_GARANT,
         (e.C_NOMNC_GARNT is not null) as CODE_ELIG,
         (c.I_PERSN_GARANT is not null) as IS_CRELOG
  from stg.STG_COLLATERAL_LINK as g
       left join ref.REF_GARANTIE_ELIG as e
         on g.C_NOMNC_GARNT = e.C_NOMNC_GARNT and g.ARRETE between e.VALID_FROM and e.VALID_TO
       left join ref.REF_CRELOG as c
         on g.I_PERSN_GARANT = c.I_PERSN_GARANT;
quit;

data work.gar2;
  set work.gar1;
  ACTIVE         = (D_ECHEANCE = . or D_ECHEANCE >= ARRETE);     /* non echue a l'arrete */
  LIEN_ELIG_ACTIF = (CODE_ELIG = 1 and ACTIVE = 1);
  NONELIG        = (C_NOMNC_GARNT ne '' and CODE_ELIG = 0);       /* code hors des 34 */
  ECHUE_ELIG     = (CODE_ELIG = 1 and ACTIVE = 0);
  CRELOG_OK      = (IS_CRELOG = 1 and LIEN_ELIG_ACTIF = 1);
  CRELOG_VALO_MANQ = (CRELOG_OK = 1 and (M_VALO_GARANT = . or D_VALO = .));
run;

proc sql;
  create table work.gar3 as
  select N_PERD, ID_CONTRAT,
         count(*) as N_LIENS,
         max(LIEN_ELIG_ACTIF) as GAR_ELIG,
         max(NONELIG) as GAR_NONELIG,
         max(ECHUE_ELIG) as GAR_ECHUE,
         sum(IS_CRELOG) as N_CRELOG,
         max(CRELOG_OK) as CRELOG_VALIDE,
         max(CRELOG_VALO_MANQ) as CRELOG_VALO_MANQ
  from work.gar2
  group by N_PERD, ID_CONTRAT;
quit;

/* 7. Assemblage des drivers ---------------------------------------------------------- */
proc sql;
  create table work.c5 as
  select a.*, w.WO_MONTANT,
         coalesce(g.N_LIENS, 0) as N_LIENS, coalesce(g.GAR_ELIG, 0) as GAR_ELIG,
         coalesce(g.GAR_NONELIG, 0) as GAR_NONELIG, coalesce(g.GAR_ECHUE, 0) as GAR_ECHUE,
         coalesce(g.N_CRELOG, 0) as N_CRELOG, coalesce(g.CRELOG_VALIDE, 0) as CRELOG_VALIDE,
         coalesce(g.CRELOG_VALO_MANQ, 0) as CRELOG_VALO_MANQ
  from work.c4 as a
       left join work.wo as w on a.N_PERD = w.N_PERD and a.ID_CONTRAT = w.ID_CONTRAT
       left join work.gar3 as g on a.N_PERD = g.N_PERD and a.ID_CONTRAT = g.ID_CONTRAT;
quit;

data work.c6;
  set work.c5;
  WO_ANY     = (WO_MONTANT ne .);
  WO_PARTIEL = (WO_ANY = 1 and WO_MONTANT < 0.95 * ENCOURS_SEUIL_CTR);   /* hypothese Q14 */
  WO_TOTAL   = (WO_ANY = 1 and WO_PARTIEL = 0);
  /* Corporate et Immobilier lisent "write-off" ; les autres modeles "write-off partiel" */
  if PORTEFEUILLE in ('CORP','IMMO') then WO_DRIVER = WO_ANY; else WO_DRIVER = WO_PARTIEL;
run;

/* 8. Classe de population M-1 / M et changement de route ---------------------------- */
proc sql;
  create table work.pop as
  select a.N_PERD, a.ID_CONTRAT, a.ROUTE,
         b.ROUTE as ROUTE_AUTRE length=8
  from work.c6 as a
       left join work.c6 as b
         on a.ID_CONTRAT = b.ID_CONTRAT and b.N_PERD = case when a.N_PERD = &am. then &am1. else &am. end;
quit;

proc sql;
  create table dq.DRV_CONTRAT as
  select a.*,
         case when p.ROUTE_AUTRE is null then (case when a.N_PERD = &am. then 'IN' else 'OUT' end)
              when p.ROUTE_AUTRE ne a.ROUTE then 'SCOPE_CHANGE'
              else 'COMMON' end as CLASSE_POP length=12
  from work.c6 as a
       left join work.pop as p on a.N_PERD = p.N_PERD and a.ID_CONTRAT = p.ID_CONTRAT
  order by a.N_PERD, a.ID_CONTRAT;
quit;

/* 9. Indicateurs evenements par contrat (pour DQ-015) --------------------------------- */
proc sql;
  create table dq.EVT_CONTRAT as
  select e.N_PERD, e.ID_CONTRAT,
         count(*) as N_EVT,
         max(case when (e.TYP_OUP in ('E','R') and e.TYP_MVT in ('01','03','04','14')) then 0 else 1 end) as EVT_KO_DOM,
         max(e.DUP_EVT) as EVT_DUP,
         max(case when (e.D_EVT ne . and d.DATDEF ne . and e.D_EVT < d.DATDEF and e.TYP_OUP = 'E') then 1 else 0 end) as EVT_AVANT_DEF
  from work.evt2 as e
       inner join dq.DRV_CONTRAT as d on e.N_PERD = d.N_PERD and e.ID_CONTRAT = d.ID_CONTRAT
  group by e.N_PERD, e.ID_CONTRAT;
quit;

%let step_id  = DQ_03a_ROUTAGE_DRIVERS;
%let step_msg = Routage et drivers calcules;
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
/* SECTION 2 : REGLES */
/******************************************************************************
 Section 2.1 (ex-programme DQ_03b_DIAGNOSTIC_REGLES)
 Etape     : 3 - Moteur DQ : execution des regles au contrat
 Objet     : Evaluer les 27 controles du catalogue sur toutes les creances (y
             compris exclues et individuelles). Un controle renvoie, pour chaque
             contrat, un statut sur UN caractere :
               P = PASS  F = FAIL  A = NOT_APPLICABLE  T = NOT_TESTED  U = UNDETERMINED
             NOT_APPLICABLE et NOT_TESTED ne valent jamais PASS.
 Entrees   : dq.DRV_CONTRAT, dq.EVT_CONTRAT, stg.STG_PRODUCTION_HLC, stg.STG_PRODUCTION_ECL,
             dq.PARAM_LGDD, dq.PARAM_PGE, ref.REF_ENTITE, ref.REF_SEUIL, ref.REF_LINEAGE,
             dq.DQ_RECON_SOURCE, dq.DQ_SCHEMA_RESULT
 Sorties   : dq.DQ_RULE_FLAG (contrat x arrete : R001-R027, sous-tests V04-V13 de DQ-024,
             statut des 15 CDE : C = conforme, N = non conforme, A = non applicable)
 Controles de niveau RUN (R001, R002, R025, R026) : leur resultat est propage a tous les contrats
             afin que la couverture des controles soit uniforme.
 Non teste : R022 (provision de production) est evalue a l'etape 5a ; R023 (overrides : aucune
             source fournie) et R027 (non-regression : jeu de test non fourni) restent T.
 Stabilite M / M-1 : DQ-024 et ses sous-tests ne sont evaluables qu'a l'arrete M (T a M-1).
 ATTENTION : en SAS un numerique manquant est PLUS PETIT que tout nombre. Toute comparaison
             "x < 0" est donc ecrite "x ne . and x < 0".
 Version   : v1.2
******************************************************************************/
%let t0 = %sysfunc(datetime());   /* heure de debut de la section */

/* 1. Resultats de niveau run (reconciliation, schema, lignage, grain) -------------- */
proc sql noprint;
  select count(*) into :n_recon_fail trimmed from dq.DQ_RECON_SOURCE where STATUT = 'FAIL';
  select count(*) into :n_schema_ko  trimmed from dq.DQ_SCHEMA_RESULT where STATUT in ('MANQUANTE','ORDRE_DIFFERENT');
  select count(distinct CDE_ID) into :n_cde_lineage trimmed from ref.REF_LINEAGE;
  select count(*) - count(distinct catx('|', put(N_PERD, 6.), ID_CONTRAT)) into :n_dup_grain trimmed from dq.DRV_CONTRAT;
quit;

/* 2. Rattachement des sources de comparaison (jointures un-a-un) --------------------- */
proc sql;
  create table work.b1 as
  select a.*,
         (e.C_ENTITE is not null) as ENTITE_CONNUE,
         h.MODELE_PROD, h.HLC_PROD, h.TID_PROD, h.NIV_RISQUE_PROD, h.TOP_APUR_PROD, h.GARANTIE_PROD,
         h.CRELOG_PROD, h.ENCOURS_PROD, h.DECLAS_PROD, h.CAT_CAV_PROD, h.VERSION_REGLE_PROD,
         (h.MODELE_PROD ne '') as PROD_HLC_PRESENT,
         p.FLL_PROV, p.LGDD_PROD, p.FL_PROD, p.TAUX_TIRAGE_PROD,
         (p.FLL_PROV ne .) as PROD_ECL_PRESENT,
         coalesce(v.N_EVT, 0) as N_EVT, coalesce(v.EVT_KO_DOM, 0) as EVT_KO_DOM,
         coalesce(v.EVT_DUP, 0) as EVT_DUP, coalesce(v.EVT_AVANT_DEF, 0) as EVT_AVANT_DEF,
         lg.LGDD as LGDD_REF, lg.FL as FL_REF,
         pg.TAUX_FORFAIT as TAUX_PGE_REF
  from dq.DRV_CONTRAT as a
       left join ref.REF_ENTITE as e on a.C_ENTITE = e.C_ENTITE
       left join stg.STG_PRODUCTION_HLC as h on a.N_PERD = h.N_PERD and a.ID_CONTRAT = h.ID_CONTRAT
       left join stg.STG_PRODUCTION_ECL as p on a.N_PERD = p.N_PERD and a.ID_CONTRAT = p.ID_CONTRAT
       left join dq.EVT_CONTRAT as v on a.N_PERD = v.N_PERD and a.ID_CONTRAT = v.ID_CONTRAT
       left join dq.PARAM_LGDD as lg on a.N_PERD = lg.N_PERD and h.MODELE_PROD = lg.MODELE and h.HLC_PROD = lg.HLC
       left join dq.PARAM_PGE as pg on a.N_PERD = pg.N_PERD and a.C_NOMNC_ENGGM = pg.C_NOMNC_ENGGM and a.TYPRIS = pg.TYPRIS;

  /* valeurs de l'arrete precedent pour les contrats presents aux deux arretes (DQ-024) */
  create table work.b2 as
  select a.*,
         p.MTENGB as MTENGB_M1, p.DECLAS_B as DECLAS_B_M1, p.TYPRIS as TYPRIS_M1,
         p.B_TIERS_RETAIL as BTR_M1, p.TID_JOURS as TIDJ_M1
  from work.b1 as a
       left join dq.DRV_CONTRAT as p
         on a.N_PERD = &am. and p.N_PERD = &am1. and a.ID_CONTRAT = p.ID_CONTRAT;
quit;

/* 3. Evaluation des controles ----------------------------------------------------------- */
data dq.DQ_RULE_FLAG;
  set work.b2;
  array rr[27] $1 R001-R027;
  length V04 V07 V09 V12 V13 $1 p_cas $4 r_cas $4;
  length CDE01-CDE15 $1 s $12;
  do i = 1 to 27; rr[i] = 'A'; end;

  /* Indicateurs communs */
  IS_CAV = (SOUS_PTF = 'CAV');
  HB_PRODUIT = (B_TIERS_RETAIL = 0 or SOUS_PTF in ('REVOL','CAV','CI'));  /* produits portant du hors bilan */

  /* DQ-001 couverture de population et DQ-002 schema : resultat du run */
  R001 = ifc(&n_recon_fail. > 0, 'F', 'P');
  R002 = ifc(&n_schema_ko. > 0, 'F', 'P');

  /* DQ-003 unicite des cles : cle dupliquee ou composant du dossier BCR manquant (CDE-02) */
  R003 = 'P'; if DUP_KEY = 1 or DOSSIER_ID = '' then R003 = 'F';

  /* DQ-004 identifiant personne : RP manquant (repli sur le dossier trace) (CDE-01) */
  R004 = 'P'; if I_PRSNE_RP = '' then R004 = 'F';

  /* DQ-005 Loan IQ au RP : propagation impossible si le RP est absent (CDE-03) */
  if LOANIQ_CTR = 1 then do; R005 = 'P'; if I_PRSNE_RP = '' then R005 = 'F'; end;

  /* DQ-006 exclusions : entite ou devise manquante ou inconnue (CDE-03) */
  R006 = 'P'; if C_ENTITE = '' or ENTITE_CONNUE = 0 or C_DEVISE = '' then R006 = 'F';

  /* DQ-007 Corporate / Retail : valeur absente ou hors {0,1}, ou incoherence avec SEG_DECL (CDE-04) */
  R007 = 'P';
  if B_TIERS_RETAIL = . or B_TIERS_RETAIL not in (0, 1) then R007 = 'F';
  else if B_TIERS_RETAIL = 0 and CAT_CAV = 'PRIVE' and IS_CAV then R007 = 'F';   /* Prive implique Retail */

  /* DQ-008 mapping produit : un Retail non PGE doit avoir un sous-portefeuille unique (CDE-03, 05) */
  if B_TIERS_RETAIL = 1 and PGE_FLAG = 0 then do; R008 = 'P'; if SOUS_PTF = '' then R008 = 'F'; end;

  /* DQ-009 detection PGE : le taux forfaitaire (nature x EDX/CER) doit exister (CDE-05) */
  if PGE_FLAG = 1 then do; R009 = 'P'; if TAUX_PGE_REF = . then R009 = 'F'; end;

  /* DQ-010 categorie CAV : SEG_DECL dans les listes Prive ou Pro ; vide = indetermine (Q49) (CDE-06) */
  if PORTEFEUILLE = 'CAV' or IS_CAV then do;
    R010 = 'P';
    if CAT_CAV = 'INCONNU' then R010 = 'F';
    else if CAT_CAV = 'PRO_VIDE' then R010 = 'U';
  end;

  /* DQ-011 declassement bilan : nul (ou absent) alors que le bilan est positif ; signe (CDE-07) */
  R011 = 'P';
  if (DECLAS_B = . or DECLAS_B = 0) and MTENGB ne . and MTENGB > 0 then R011 = 'F';
  else if DECLAS_B ne . and DECLAS_B < 0 and IS_CAV = 0 then R011 = 'F';

  /* DQ-012 declassement hors bilan : applicable aux produits portant du hors bilan (CDE-08) */
  if HB_PRODUIT then do; R012 = 'P'; if DECLAS_H = . then R012 = 'F'; end;

  /* DQ-013 exposition bilan : absente, de signe inattendu, ou differente de la production (CDE-09) */
  R013 = 'P';
  if MTENGB = . then R013 = 'F';
  else if MTENGB < 0 and IS_CAV = 0 then R013 = 'F';
  else if PROD_HLC_PRESENT and ENCOURS_PROD ne . and abs(MTENGB - ENCOURS_PROD) > &s_TOL_MONTANT. then R013 = 'F';

  /* DQ-014 exposition hors bilan : absente ou negative (variable a confirmer, Q50) (CDE-10) */
  if HB_PRODUIT then do;
    R014 = 'P';
    if M_ENCRS_HB = . then R014 = 'F';
    else if M_ENCRS_HB < 0 then R014 = 'F';
  end;

  /* DQ-015 passage en perte : domaine, doublons, date dans l'episode de defaut, flag de production (CDE-11) */
  if N_EVT > 0 or (PROD_HLC_PRESENT and TOP_APUR_PROD = 1) then do;
    R015 = 'P';
    if EVT_KO_DOM = 1 or EVT_DUP = 1 or EVT_AVANT_DEF = 1 then R015 = 'F';
    else if PROD_HLC_PRESENT and coalesce(TOP_APUR_PROD, 0) ne WO_DRIVER then R015 = 'F';
  end;

  /* DQ-016 date de defaut et TID : date absente ou future, dates trop espacees, TID de production different (CDE-12) */
  R016 = 'P';
  if DATDEF = . then R016 = 'F';
  else if DATDEF > ARRETE then R016 = 'F';
  else if DATDEF_MAX - DATDEF > 365 then R016 = 'F';
  else if PROD_HLC_PRESENT and TID_PROD ne . and TID_PROD ne TID_ANNEES then R016 = 'F';

  /* DQ-017 EDX / CER : domaine {1, 2} (CDE-13) */
  R017 = 'P'; if TYPRIS not in ('1','2') then R017 = 'F';

  /* DQ-018 garantie : code hors des 34 ou garantie echue encore declaree active en production (CDE-14) */
  if PORTEFEUILLE in ('CORP','CI') or N_LIENS > 0 then do;
    R018 = 'P';
    if GAR_NONELIG = 1 then R018 = 'F';
    else if GAR_ECHUE = 1 and GAR_ELIG = 0 and GARANTIE_PROD = 1 then R018 = 'F';
  end;

  /* DQ-019 Credit Logement : Immobilier seulement, unicite, valorisation admissible (CDE-15) */
  if N_CRELOG > 0 or SOUS_PTF = 'IMMO' then do;
    R019 = 'P';
    if (N_CRELOG > 0 and SOUS_PTF ne 'IMMO') or N_CRELOG > 1 or CRELOG_VALO_MANQ = 1 then R019 = 'F';
  end;

  /* DQ-020 ratio R (CI et CAV statistiques) : recalcul contre le ratio de production (donnee derivee) */
  if ROUTE = 'STAT' and PORTEFEUILLE in ('CI','CAV') then do;
    r_bil = ENCOURS_PROD; r_den = DECLAS_PROD;
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
    p_cas = r_cas; p_val = r_val;
    R020 = 'P';
    if R_CAS = 'ZERO' then R020 = 'U';
    if PROD_HLC_PRESENT then do;
      if p_cas ne R_CAS then R020 = 'F';
      else if p_cas = 'CALC' and abs(p_val - R_VALEUR) > 0.001 then R020 = 'F';
    end;
  end;

  /* DQ-021 parametres : LGDD et facteur appliques par la production contre le referentiel */
  if PROD_ECL_PRESENT and HLC_PROD ne . then do;
    R021 = 'P';
    if LGDD_REF = . or FL_REF = . then R021 = 'F';
    else if abs(LGDD_REF - LGDD_PROD) > 1e-9 or abs(FL_REF - FL_PROD) > 1e-9 then R021 = 'F';
  end;

  /* DQ-022 : evalue a l'etape 5a. DQ-023 et DQ-027 : non testes (pas de source). */
  R022 = 'T'; R023 = 'T'; R027 = 'T';

  /* DQ-024 variations atypiques M / M-1 : evaluables a M, contrats presents aux deux arretes */
  V04 = 'T'; V07 = 'T'; V09 = 'T'; V12 = 'T'; V13 = 'T'; R024 = 'T';
  if N_PERD = &am. then do;
    V04 = 'A'; V07 = 'A'; V09 = 'A'; V12 = 'A'; V13 = 'A'; R024 = 'A';
    if CLASSE_POP ne 'IN' then do;
      V04 = 'P'; V07 = 'P'; V09 = 'P'; V12 = 'P'; V13 = 'P'; R024 = 'P';
      if MTENGB ne . and MTENGB_M1 ne . and MTENGB_M1 ne 0 and abs(MTENGB / MTENGB_M1 - 1) > &s_VAR_MTENGB_MAX. then V09 = 'F';
      if DECLAS_B ne . and DECLAS_B_M1 ne . and abs(DECLAS_B - DECLAS_B_M1) > &s_TOL_MONTANT. then V07 = 'F';
      if TYPRIS_M1 = '2' and TYPRIS = '1' then V13 = 'F';                      /* retour CER -> EDX */
      if B_TIERS_RETAIL ne . and BTR_M1 ne . and B_TIERS_RETAIL ne BTR_M1 then V04 = 'F';
      if TID_JOURS ne . and TIDJ_M1 ne . and (TID_JOURS - TIDJ_M1 < 28 or TID_JOURS - TIDJ_M1 > 32) then V12 = 'F';
      if V04 = 'F' or V07 = 'F' or V09 = 'F' or V12 = 'F' or V13 = 'F' then R024 = 'F';
    end;
  end;

  /* DQ-025 lignage : les 15 CDE ont un lignage documente. DQ-026 double comptage : grain unique. */
  R025 = ifc(&n_cde_lineage. = 15, 'P', 'F');
  R026 = ifc(&n_dup_grain. = 0, 'P', 'F');

  /* Statut des 15 donnees critiques : C conforme, N non conforme (F ou U), A non applicable.
     findc(s, 'PFU') > 0 : au moins un controle applicable ; findc(s, 'FU') > 0 : au moins un echec. */
  s = cats(R004);           if findc(s, 'PFU') then CDE01 = ifc(findc(s, 'FU') > 0, 'N', 'C'); else CDE01 = 'A';
  s = cats(R003);           if findc(s, 'PFU') then CDE02 = ifc(findc(s, 'FU') > 0, 'N', 'C'); else CDE02 = 'A';
  s = cats(R005, R006, R008); if findc(s, 'PFU') then CDE03 = ifc(findc(s, 'FU') > 0, 'N', 'C'); else CDE03 = 'A';
  s = cats(R007, V04);      if findc(s, 'PFU') then CDE04 = ifc(findc(s, 'FU') > 0, 'N', 'C'); else CDE04 = 'A';
  s = cats(R008, R009);     if findc(s, 'PFU') then CDE05 = ifc(findc(s, 'FU') > 0, 'N', 'C'); else CDE05 = 'A';
  s = cats(R010);           if findc(s, 'PFU') then CDE06 = ifc(findc(s, 'FU') > 0, 'N', 'C'); else CDE06 = 'A';
  s = cats(R011, V07);      if findc(s, 'PFU') then CDE07 = ifc(findc(s, 'FU') > 0, 'N', 'C'); else CDE07 = 'A';
  s = cats(R012);           if findc(s, 'PFU') then CDE08 = ifc(findc(s, 'FU') > 0, 'N', 'C'); else CDE08 = 'A';
  s = cats(R013, V09);      if findc(s, 'PFU') then CDE09 = ifc(findc(s, 'FU') > 0, 'N', 'C'); else CDE09 = 'A';
  s = cats(R014);           if findc(s, 'PFU') then CDE10 = ifc(findc(s, 'FU') > 0, 'N', 'C'); else CDE10 = 'A';
  s = cats(R015);           if findc(s, 'PFU') then CDE11 = ifc(findc(s, 'FU') > 0, 'N', 'C'); else CDE11 = 'A';
  s = cats(R016, V12);      if findc(s, 'PFU') then CDE12 = ifc(findc(s, 'FU') > 0, 'N', 'C'); else CDE12 = 'A';
  s = cats(R017, V13);      if findc(s, 'PFU') then CDE13 = ifc(findc(s, 'FU') > 0, 'N', 'C'); else CDE13 = 'A';
  s = cats(R018);           if findc(s, 'PFU') then CDE14 = ifc(findc(s, 'FU') > 0, 'N', 'C'); else CDE14 = 'A';
  s = cats(R019);           if findc(s, 'PFU') then CDE15 = ifc(findc(s, 'FU') > 0, 'N', 'C'); else CDE15 = 'A';

  keep N_PERD ID_CONTRAT PORTEFEUILLE ROUTE EAD R001-R027 V04 V07 V09 V12 V13 CDE01-CDE15;
run;

%let step_id  = DQ_03b_DIAGNOSTIC_REGLES;
%let step_msg = 27 controles evalues au contrat;
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
/* SECTION 3 : ANOMALIES ET KPI */
/******************************************************************************
 Section 3.1 (ex-programme DQ_03c_ANOMALIES_KPI)
 Etape     : 3 - Moteur DQ : anomalies, statuts, KPI et agregats de la vague 1
 Objet     : 1) transformer les echecs en anomalies (une ligne par contrat x regle) ;
             2) qualifier chaque anomalie : NOUVELLE, PERSISTANTE, RESOLUE, SORTIE ;
             3) calculer les KPI par donnee critique et le score DQ ;
             4) proposer des pistes de cause racine ;
             5) preparer les agregats additifs lus par l'etape 6 (vague 1 du tableau de bord).
 Entrees   : dq.DQ_RULE_FLAG, dq.DRV_CONTRAT, ref.REF_REGLE, ref.REF_CDE
 Sorties   : dq.DQ_ISSUE, dq.DQ_ISSUE_HIST (historique, sans doublon si relance),
             dq.DQ_CONTRAT_CRITIQUE, dq.DQ_KPI_CDE, dq.DQ_KPI_RUN, dq.DQ_RCA_CANDIDATE,
             dq.AGG_RUN_SUMMARY, dq.AGG_CDE_SCORE, dq.AGG_RULE_RESULT, dq.AGG_ISSUE, dq.AGG_SCOPE
 Definitions :
   - anomalie  : contrat x regle au statut F (les indetermines U sont comptes a part) ;
   - statut a l'arrete M : PERSISTANTE si la meme anomalie existait a M-1, sinon NOUVELLE ;
   - statut a l'arrete M-1 : PERSISTANTE si elle subsiste a M ; SORTIE si le contrat a quitte
     la population ; RESOLUE sinon ;
   - contrat critique : au moins un echec d'une regle de severite Critique ou Bloquant
     (DQ-022 exclu : evaluee a l'etape 5) ;
   - taux de conformite d'une CDE = contrats conformes / contrats applicables (U compte non conforme) ;
   - score DQ = moyenne ponderee (POIDS de ref.REF_CDE) des taux de conformite des CDE applicables.
 La severite (gravite de l'erreur) et la materialite (exposition) sont deux notions distinctes :
 l'exposition est conservee a cote de chaque anomalie.
 Version   : v1.2
******************************************************************************/
%let t0 = %sysfunc(datetime());   /* heure de debut de la section */

/* 1. Anomalies : une ligne par contrat x regle en echec ---------------------------------- */
data work.iss0;
  set dq.DQ_RULE_FLAG;
  array rr[27] $1 R001-R027;
  length RULE_ID $8;
  do i = 1 to 27;
    if rr[i] = 'F' and i ne 22 then do;            /* DQ-022 : etape 5a */
      RULE_ID = cats('DQ-', put(i, z3.));
      output;
    end;
  end;
  keep N_PERD ID_CONTRAT PORTEFEUILLE EAD RULE_ID;
run;

/* 2. Statut de chaque anomalie par comparaison des deux arretes ------------------------------ */
proc sql;
  create table dq.DQ_ISSUE as
  select a.N_PERD, a.ID_CONTRAT, a.PORTEFEUILLE, a.RULE_ID, a.EAD,
         r.SEVERITE, r.CAUSE_RACINE, r.METHODE_IMPACT,
         case when a.N_PERD = &am. then
                (case when b.ID_CONTRAT is not null then 'PERSISTANTE' else 'NOUVELLE' end)
              else
                (case when b.ID_CONTRAT is not null then 'PERSISTANTE'
                      when d.ID_CONTRAT is null then 'SORTIE'
                      else 'RESOLUE' end)
         end as STATUT length=12
  from work.iss0 as a
       left join ref.REF_REGLE as r on a.RULE_ID = r.RULE_ID
       left join work.iss0 as b
         on a.ID_CONTRAT = b.ID_CONTRAT and a.RULE_ID = b.RULE_ID
            and b.N_PERD = case when a.N_PERD = &am. then &am1. else &am. end
       left join (select ID_CONTRAT from dq.DRV_CONTRAT where N_PERD = &am.) as d
         on a.ID_CONTRAT = d.ID_CONTRAT
  order by a.N_PERD, a.ID_CONTRAT, a.RULE_ID;
quit;

/* Historique : on remplace les lignes du meme run (relance sans doublon) */
proc sql;
  delete from dq.DQ_ISSUE_HIST where RUN_ID = "&run_id.";
quit;
data work.hist1;
  length RUN_ID $20;
  set dq.DQ_ISSUE (keep=N_PERD ID_CONTRAT RULE_ID PORTEFEUILLE EAD STATUT);
  RUN_ID = "&run_id.";
run;
proc append base=dq.DQ_ISSUE_HIST data=work.hist1 force; run;

/* 3. Contrats critiques ---------------------------------------------------------------------- */
proc sql;
  create table work.crit as
  select N_PERD, ID_CONTRAT, count(*) as NB_FAIL_CRITIQUE
  from dq.DQ_ISSUE
  where SEVERITE in ('Critique', 'Bloquant')
  group by N_PERD, ID_CONTRAT;

  create table dq.DQ_CONTRAT_CRITIQUE as
  select d.N_PERD, d.ID_CONTRAT, d.PORTEFEUILLE, d.EAD,
         coalesce(c.NB_FAIL_CRITIQUE, 0) as NB_FAIL_CRITIQUE,
         (c.NB_FAIL_CRITIQUE > 0) as CRITIQUE
  from dq.DRV_CONTRAT as d
       left join work.crit as c on d.N_PERD = c.N_PERD and d.ID_CONTRAT = c.ID_CONTRAT;
quit;

/* 4. Score par donnee critique : mise au format long des 15 statuts CDE --------------------- */
data work.cde_long;
  set dq.DQ_RULE_FLAG;
  array cc[15] $1 CDE01-CDE15;
  length CDE_ID $8 ST $1;
  do k = 1 to 15;
    CDE_ID = cats('CDE-', put(k, z2.));
    ST = cc[k];
    output;
  end;
  keep N_PERD PORTEFEUILLE CDE_ID ST EAD;
run;

proc sql;
  create table dq.AGG_CDE_SCORE as
  select N_PERD, PORTEFEUILLE, CDE_ID,
         sum(ST ne 'A') as N_APPLICABLE,
         sum(ST = 'C') as N_CONFORME,
         sum(case when ST ne 'A' then EAD else 0 end) as EAD_APPLICABLE,
         sum(case when ST = 'N' then EAD else 0 end) as EAD_NON_CONFORME
  from work.cde_long
  group by N_PERD, PORTEFEUILLE, CDE_ID;

  create table work.kpi1 as
  select N_PERD, CDE_ID, sum(N_APPLICABLE) as N_APPLICABLE, sum(N_CONFORME) as N_CONFORME,
         calculated N_CONFORME / max(calculated N_APPLICABLE, 1) as TAUX_CONFORMITE
  from dq.AGG_CDE_SCORE
  group by N_PERD, CDE_ID;
quit;

/* Les tables ref et dq doivent rester sans alias de colonne ambigus : jointure explicite */
proc sql;
  create table dq.DQ_KPI_CDE as
  select k.N_PERD, k.CDE_ID, k.N_APPLICABLE, k.N_CONFORME,
         k.N_CONFORME / max(k.N_APPLICABLE, 1) as TAUX_CONFORMITE
  from work.kpi1 as k
  order by k.N_PERD, k.CDE_ID;

  /* Score DQ = somme(poids x taux) / somme(poids), sur les CDE applicables */
  create table dq.DQ_KPI_RUN as
  select k.N_PERD,
         sum(c.POIDS * k.TAUX_CONFORMITE) / sum(c.POIDS) as SCORE_DQ
  from dq.DQ_KPI_CDE as k inner join ref.REF_CDE as c on k.CDE_ID = c.CDE_ID
  where k.N_APPLICABLE > 0
  group by k.N_PERD;
quit;

/* 5. Resultats des regles : mise au format long des 27 statuts ------------------------------ */
data work.rule_long;
  set dq.DQ_RULE_FLAG;
  array rr[27] $1 R001-R027;
  length RULE_ID $8 ST $1;
  do i = 1 to 27;
    if i ne 22 then do;                             /* DQ-022 : etape 5a */
      RULE_ID = cats('DQ-', put(i, z3.));
      ST = rr[i];
      output;
    end;
  end;
  keep N_PERD PORTEFEUILLE RULE_ID ST EAD;
run;

proc sql;
  create table dq.AGG_RULE_RESULT as
  select a.N_PERD, a.PORTEFEUILLE, a.RULE_ID, r.CDE_ID, r.SEVERITE,
         count(*) as N_CONTRATS,
         sum(a.ST in ('P','F','U')) as N_TESTE,
         sum(a.ST = 'F') as N_ECHEC,
         sum(a.ST = 'U') as N_INDETERMINE,
         sum(a.ST = 'A') as N_NON_APPLICABLE,
         sum(a.ST = 'T') as N_NON_TESTE,
         sum(case when a.ST = 'F' then a.EAD else 0 end) as EAD_ECHEC
  from work.rule_long as a left join ref.REF_REGLE as r on a.RULE_ID = r.RULE_ID
  group by a.N_PERD, a.PORTEFEUILLE, a.RULE_ID, r.CDE_ID, r.SEVERITE;
quit;

/* 6. Agregats : synthese du run, anomalies, perimetre ----------------------------------------- */
proc sql;
  create table dq.AGG_RUN_SUMMARY as
  select c.N_PERD, c.PORTEFEUILLE,
         count(*) as N_CONTRATS,
         sum(c.EAD) as EAD_TOTAL,
         sum(c.CRITIQUE) as N_CONTRATS_CRITIQUES,
         sum(case when c.CRITIQUE = 1 then c.EAD else 0 end) as EAD_CRITIQUE
  from dq.DQ_CONTRAT_CRITIQUE as c
  group by c.N_PERD, c.PORTEFEUILLE;

  create table dq.AGG_ISSUE as
  select N_PERD, PORTEFEUILLE, CAUSE_RACINE, STATUT, SEVERITE,
         count(*) as N_ANOMALIES, sum(EAD) as EAD
  from dq.DQ_ISSUE
  group by N_PERD, PORTEFEUILLE, CAUSE_RACINE, STATUT, SEVERITE;

  create table dq.AGG_SCOPE as
  select N_PERD, PORTEFEUILLE, ROUTE, CLASSE_POP, count(*) as N_CONTRATS, sum(EAD) as EAD
  from dq.DRV_CONTRAT
  group by N_PERD, PORTEFEUILLE, ROUTE, CLASSE_POP;
quit;

/* 7. Pistes de cause racine (arrete M) ----------------------------------------------------------
   Pour chaque regle, on cherche la valeur d'une dimension (portefeuille, entite, type de compte,
   code du dossier) ou les echecs sont le plus concentres : lift = part des echecs / part de la
   population. Une piste est retenue si au moins 20 echecs portent sur la valeur. */
data work.dim_pop;
  set dq.DRV_CONTRAT (where=(N_PERD = &am.) keep=N_PERD ID_CONTRAT PORTEFEUILLE C_ENTITE TYP_CPTE CODSGDOS);
  length DIMENSION $16 VALEUR $20;
  DIMENSION = 'PORTEFEUILLE'; VALEUR = PORTEFEUILLE; output;
  DIMENSION = 'C_ENTITE';     VALEUR = C_ENTITE;     output;
  DIMENSION = 'TYP_CPTE';     VALEUR = TYP_CPTE;     output;
  DIMENSION = 'CODSGDOS';     VALEUR = CODSGDOS;     output;
  keep ID_CONTRAT DIMENSION VALEUR;
run;

proc sql noprint;
  select count(*) into :n_pop_m trimmed from dq.DRV_CONTRAT where N_PERD = &am.;
quit;

proc sql;
  create table work.rca1 as
  select i.RULE_ID, p.DIMENSION, p.VALEUR, count(*) as N_ECHECS_VALEUR
  from dq.DQ_ISSUE as i inner join work.dim_pop as p on i.ID_CONTRAT = p.ID_CONTRAT
  where i.N_PERD = &am.
  group by i.RULE_ID, p.DIMENSION, p.VALEUR
  having count(*) >= 20;

  create table work.rca2 as
  select RULE_ID, count(*) as N_ECHECS_REGLE from dq.DQ_ISSUE where N_PERD = &am. group by RULE_ID;

  create table work.rca3 as
  select DIMENSION, VALEUR, count(*) as N_POP_VALEUR from work.dim_pop group by DIMENSION, VALEUR;

  create table work.rca4 as
  select a.RULE_ID, a.DIMENSION, a.VALEUR, a.N_ECHECS_VALEUR, b.N_ECHECS_REGLE,
         (a.N_ECHECS_VALEUR / b.N_ECHECS_REGLE) / (c.N_POP_VALEUR / &n_pop_m.) as LIFT
  from work.rca1 as a
       inner join work.rca2 as b on a.RULE_ID = b.RULE_ID
       inner join work.rca3 as c on a.DIMENSION = c.DIMENSION and a.VALEUR = c.VALEUR;
quit;

proc sort data=work.rca4; by RULE_ID descending LIFT; run;

data dq.DQ_RCA_CANDIDATE;
  length N_PERD 8;
  set work.rca4;
  by RULE_ID;
  if first.RULE_ID;                                  /* meilleure piste par regle */
  N_PERD = &am.;
run;

%let step_id  = DQ_03c_ANOMALIES_KPI;
%let step_msg = Anomalies, KPI et agregats vague 1;
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
