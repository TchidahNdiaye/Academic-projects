/******************************************************************************
 Programme : DQ_02_ENTREE_PROFILAGE.sas      (programme 2 sur 6 - dispositif DQ LGD / provisions)
 Etape CDC : Etape 2 - Zone d atterrissage, controle d entree et profilage
 Version   : v1.2
 Objet     : Porte d entree (schema, volumes, cles, rapprochements) et profilage des 15 CDE.
 Entrees   : stg.*, ref.*
 Sorties   : dq.DQ_INPUT_QUALITY_GATE, dq.DQ_SCHEMA_RESULT, dq.DQ_RECON_SOURCE, dq.DQ_KEY_PROFILE, dq.DQ_PROFILE_CDE
 Contenu   : S0 parametres | S1 controle d entree | S2 profilage des 15 CDE
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
/* SECTION 1 : CONTROLE D ENTREE */
/******************************************************************************
 Section 1.1 (ex-programme DQ_02a_CONTROLE_ENTREE)
 Etape     : 2 - Zone d'atterrissage, controle d'entree
 Objet     : Detecter les anomalies structurelles des l'entree et decider si
             les etapes suivantes peuvent s'executer (porte d'entree).
 Controles : 1) schema : colonnes attendues presentes, dans l'ordre ;
             2) cles : doublons, cles manquantes, liens orphelins (garanties et
                evenements sans contrat) ;
             3) reconciliation : lignes et sommes lues contre le manifeste ;
             4) volume : variation du nombre de contrats M / M-1.
 Entrees   : ref.REF_SCHEMA, ref.REF_SEUIL, dq.DQ_INPUT_MANIFEST,
             stg.STG_*, raw.RAW_CREDIT, raw.RAW_PROD_ECL, entetes des fichiers
 Sorties   : dq.DQ_SCHEMA_RESULT, dq.DQ_KEY_PROFILE, dq.DQ_RECON_SOURCE,
             dq.DQ_INPUT_QUALITY_GATE (ACCEPTED, ACCEPTED_WITH_WARNINGS, REJECTED)
 Regle     : un seuil n'est pas code en dur : GATE_WARN_VOL et GATE_REJECT_VOL
             viennent de ref.REF_SEUIL (a calibrer avec l'owner apres une baseline).
 Arret     : si la porte d'entree de l'arrete M est REJECTED, le programme
             s'arrete (abort cancel) et les etapes 3 a 6 ne sont pas lancees.
 Cas DQ    : DQ-001 (couverture), DQ-002 (schema), DQ-003 (unicite des cles).
 Version   : v1.2
******************************************************************************/
%let t0 = %sysfunc(datetime());   /* heure de debut de la section */

data work.arretes;
  N_PERD = &am1.; output;
  N_PERD = &am.;  output;
run;

/* 1. Schema : lire l'en-tete de chaque fichier et le comparer au contrat ----- */
proc sql;
  create table work.fichiers as
  select distinct s.PAQUET, s.FICHIER_RACINE, a.N_PERD
  from ref.REF_SCHEMA as s, work.arretes as a;
quit;

/* Une ligne par colonne trouvee dans l'en-tete du fichier (premiere ligne) */
data work.trouve;
  set work.fichiers;
  length SOURCE_FILE $200 ENTETE $2000 COLONNE $32;
  SOURCE_FILE = cats("&root./entrees/", FICHIER_RACINE, "_", N_PERD, ".csv");
  if fileexist(SOURCE_FILE) then do;
    infile dummy filevar=SOURCE_FILE truncover lrecl=4096;
    input ENTETE $char2000.;
    ENTETE = compress(ENTETE, '0D'x);
    do ORDRE_LU = 1 to countw(ENTETE, ',');
      COLONNE = upcase(scan(ENTETE, ORDRE_LU, ','));
      output;
    end;
  end;
  keep PAQUET N_PERD COLONNE ORDRE_LU;
run;

/* Colonnes attendues x arretes, croisees avec les colonnes trouvees */
proc sql;
  create table work.attendu as
  select s.PAQUET, a.N_PERD, s.COLONNE, s.ORDRE as ORDRE_ATTENDU
  from ref.REF_SCHEMA as s, work.arretes as a;

  create table dq.DQ_SCHEMA_RESULT as
  select coalesce(e.N_PERD, f.N_PERD)   as N_PERD,
         coalesce(e.PAQUET, f.PAQUET)   as PAQUET length=12,
         coalesce(e.COLONNE, f.COLONNE) as COLONNE length=32,
         e.ORDRE_ATTENDU, f.ORDRE_LU,
         case when e.COLONNE is null then 'EN_PLUS'
              when f.COLONNE is null then 'MANQUANTE'
              when e.ORDRE_ATTENDU ne f.ORDRE_LU then 'ORDRE_DIFFERENT'
              else 'PRESENT' end as STATUT length=16
  from work.attendu as e
       full join work.trouve as f
       on e.N_PERD = f.N_PERD and e.PAQUET = f.PAQUET and e.COLONNE = f.COLONNE
  order by N_PERD, PAQUET, ORDRE_ATTENDU;
quit;

/* 2. Cles : doublons, cles manquantes, orphelins -------------------------------- */
proc sql;
  /* contrats : cle ARRETE x ID_CONTRAT */
  create table work.k1 as
  select N_PERD, 'CREDIT' as PAQUET length=12,
         count(*) as N_LIGNES,
         count(distinct ID_CONTRAT) as N_CLES_DISTINCTES,
         count(*) - count(distinct ID_CONTRAT) as N_DOUBLONS,
         sum(ID_CONTRAT = '') as N_CLES_MANQUANTES,
         0 as N_ORPHELINS
  from stg.STG_CREDIT_SNAPSHOT
  group by N_PERD;

  /* garanties : cle contrat x code x garant ; orphelin = contrat absent du snapshot */
  create table work.k2 as
  select g.N_PERD, 'GARANTIES' as PAQUET length=12,
         count(*) as N_LIGNES,
         count(distinct catx('|', g.ID_CONTRAT, g.C_NOMNC_GARNT, g.I_PERSN_GARANT)) as N_CLES_DISTINCTES,
         count(*) - count(distinct catx('|', g.ID_CONTRAT, g.C_NOMNC_GARNT, g.I_PERSN_GARANT)) as N_DOUBLONS,
         sum(g.ID_CONTRAT = '') as N_CLES_MANQUANTES,
         sum(c.ID_CONTRAT is null) as N_ORPHELINS
  from stg.STG_COLLATERAL_LINK as g
       left join (select distinct N_PERD, ID_CONTRAT from stg.STG_CREDIT_SNAPSHOT) as c
       on g.N_PERD = c.N_PERD and g.ID_CONTRAT = c.ID_CONTRAT
  group by g.N_PERD;

  /* evenements : cle contrat x date x montant x type */
  create table work.k3 as
  select e.N_PERD, 'EVT_PERTE' as PAQUET length=12,
         count(*) as N_LIGNES,
         count(distinct catx('|', e.ID_CONTRAT, put(e.D_EVT, yymmdd10.), put(e.MTFLUX, best16.), e.TYP_OUP, e.TYP_MVT)) as N_CLES_DISTINCTES,
         count(*) - count(distinct catx('|', e.ID_CONTRAT, put(e.D_EVT, yymmdd10.), put(e.MTFLUX, best16.), e.TYP_OUP, e.TYP_MVT)) as N_DOUBLONS,
         sum(e.ID_CONTRAT = '') as N_CLES_MANQUANTES,
         sum(c.ID_CONTRAT is null) as N_ORPHELINS
  from stg.STG_WO_EVENT as e
       left join (select distinct N_PERD, ID_CONTRAT from stg.STG_CREDIT_SNAPSHOT) as c
       on e.N_PERD = c.N_PERD and e.ID_CONTRAT = c.ID_CONTRAT
  group by e.N_PERD;
quit;

data dq.DQ_KEY_PROFILE;
  set work.k1 work.k2 work.k3;
run;

/* 3. Reconciliation avec le manifeste ------------------------------------------ */
proc sql;
  /* 3.1 nombre de lignes lues contre manifeste, pour les cinq paquets */
  create table work.r1 as
  select N_PERD, PAQUET, 'NB_LIGNES' as INDICATEUR length=20,
         NB_LIGNES_MANIFESTE as VALEUR_MANIFESTE, NB_LIGNES_LUES as VALEUR_LUE,
         NB_LIGNES_LUES - NB_LIGNES_MANIFESTE as ECART,
         case when NB_LIGNES_LUES = NB_LIGNES_MANIFESTE then 'PASS' else 'FAIL' end as STATUT length=8
  from dq.DQ_INPUT_MANIFEST;

  /* 3.2 somme de MTENGB (paquet CREDIT) contre la somme de controle du manifeste */
  create table work.r2 as
  select m.N_PERD, 'CREDIT' as PAQUET length=12, 'SOMME_MTENGB' as INDICATEUR length=20,
         m.SOMME_CONTROLE as VALEUR_MANIFESTE, s.SOMME as VALEUR_LUE,
         s.SOMME - m.SOMME_CONTROLE as ECART,
         case when abs(s.SOMME - m.SOMME_CONTROLE) <= &s_TOL_MONTANT. then 'PASS' else 'FAIL' end as STATUT length=8
  from dq.DQ_INPUT_MANIFEST as m,
       (select year(ARRETE)*100 + month(ARRETE) as N_PERD, sum(MTENGB) as SOMME from raw.RAW_CREDIT group by calculated N_PERD) as s
  where m.PAQUET = 'CREDIT' and m.N_PERD = s.N_PERD;

  /* 3.3 somme de FLL_PROV (paquet PROD_ECL) contre la somme de controle */
  create table work.r3 as
  select m.N_PERD, 'PROD_ECL' as PAQUET length=12, 'SOMME_FLL_PROV' as INDICATEUR length=20,
         m.SOMME_CONTROLE as VALEUR_MANIFESTE, s.SOMME as VALEUR_LUE,
         s.SOMME - m.SOMME_CONTROLE as ECART,
         case when abs(s.SOMME - m.SOMME_CONTROLE) <= &s_TOL_MONTANT. then 'PASS' else 'FAIL' end as STATUT length=8
  from dq.DQ_INPUT_MANIFEST as m,
       (select year(ARRETE)*100 + month(ARRETE) as N_PERD, sum(FLL_PROV) as SOMME from raw.RAW_PROD_ECL group by calculated N_PERD) as s
  where m.PAQUET = 'PROD_ECL' and m.N_PERD = s.N_PERD;
quit;

data dq.DQ_RECON_SOURCE;
  set work.r1 work.r2 work.r3;
run;

/* 4. Porte d'entree ----------------------------------------------------------------
   Une ligne par arrete. Rejet : fichier absent / schema / volume / reconciliation en
   echec, ou variation de volume au-dela du seuil de rejet. Avertissement : doublons,
   orphelins, ou variation de volume au-dela du seuil d'avertissement. */
proc sql;
  create table work.g1 as
  select a.N_PERD,
         (select count(*) from dq.DQ_INPUT_MANIFEST m where m.N_PERD = a.N_PERD and m.STATUT ne 'ACCEPTED') as N_FICHIERS_REJETES,
         (select count(*) from dq.DQ_SCHEMA_RESULT s where s.N_PERD = a.N_PERD and s.STATUT in ('MANQUANTE','ORDRE_DIFFERENT')) as N_COLONNES_KO,
         (select count(*) from dq.DQ_RECON_SOURCE r where r.N_PERD = a.N_PERD and r.STATUT = 'FAIL') as N_RECON_FAIL,
         (select sum(N_DOUBLONS) from dq.DQ_KEY_PROFILE k where k.N_PERD = a.N_PERD) as N_DOUBLONS,
         (select sum(N_ORPHELINS) from dq.DQ_KEY_PROFILE k where k.N_PERD = a.N_PERD) as N_ORPHELINS,
         (select N_CLES_DISTINCTES from dq.DQ_KEY_PROFILE k where k.N_PERD = a.N_PERD and k.PAQUET = 'CREDIT') as N_CONTRATS
  from work.arretes as a;
quit;

/* variation de volume : M contre M-1 (non calculable pour M-1) */
proc sql noprint;
  select N_CONTRATS into :n_m1 trimmed from work.g1 where N_PERD = &am1.;
quit;

data dq.DQ_INPUT_QUALITY_GATE;
  length RUN_ID $20 STATUT $24 MOTIF $200;
  set work.g1;
  RUN_ID = "&run_id.";
  if N_PERD = &am. then VAR_VOLUME = N_CONTRATS / &n_m1. - 1;
  if N_FICHIERS_REJETES > 0 or N_COLONNES_KO > 0 or N_RECON_FAIL > 0
     or (VAR_VOLUME ne . and abs(VAR_VOLUME) >= &s_GATE_REJECT_VOL.) then STATUT = 'REJECTED';
  else if N_DOUBLONS > 0 or N_ORPHELINS > 0
     or (VAR_VOLUME ne . and abs(VAR_VOLUME) >= &s_GATE_WARN_VOL.) then STATUT = 'ACCEPTED_WITH_WARNINGS';
  else STATUT = 'ACCEPTED';
  MOTIF = catx(' ; ',
               ifc(N_FICHIERS_REJETES > 0, 'Fichier rejete', ''),
               ifc(N_COLONNES_KO > 0, 'Schema non conforme', ''),
               ifc(N_RECON_FAIL > 0, 'Reconciliation en echec', ''),
               ifc(N_DOUBLONS > 0, 'Doublons de cle', ''),
               ifc(N_ORPHELINS > 0, 'Liens orphelins', ''),
               ifc(VAR_VOLUME ne ., catx(' ', 'Variation de volume', strip(put(VAR_VOLUME * 100, 5.1)), '%'), ''));
run;

/* Arret du run si la porte d'entree de l'arrete M est REJECTED */
data _null_;
  set dq.DQ_INPUT_QUALITY_GATE;
  where N_PERD = &am.;
  if STATUT = 'REJECTED' then do;
    put 'ERROR: porte d''entree REJECTED pour ' N_PERD= MOTIF=;
    abort cancel;
  end;
run;

%let step_id  = DQ_02a_CONTROLE_ENTREE;
%let step_msg = Porte entree evaluee;
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
/* SECTION 2 : PROFILAGE DES 15 CDE */
/******************************************************************************
 Section 2.1 (ex-programme DQ_02b_PROFILAGE_CDE)
 Etape     : 2 - Profilage des 15 donnees critiques (CDE)
 Objet     : Produire, pour chaque variable des 15 CDE et chaque arrete, le profil :
             lignes, manquants (vide ou point), zeros, valeurs distinctes,
             minimum, maximum, somme, valeur la plus frequente.
 Principe  : on met les variables au format LONG (une ligne par variable et par
             valeur) puis on agrege avec PROC SQL. Cela evite d'ecrire un bloc
             de code par variable.
 Entrees   : stg.STG_CREDIT_SNAPSHOT (KEY_OCC = 1), stg.STG_COLLATERAL_LINK,
             stg.STG_WO_EVENT, ref.REF_CDE_VARIABLE
 Sortie    : dq.DQ_PROFILE_CDE (arrete x variable)
 Distinctions : manquant (vide ou .), zero (valeur 0), distinct, hors domaine
             (le domaine est controle a l'etape 3). Le profilage ne juge pas.
 Version   : v1.2
******************************************************************************/
%let t0 = %sysfunc(datetime());   /* heure de debut de la section */

/* 1. Mise au format long -------------------------------------------------------
   Trois tables sources, trois types : caractere (C), numerique (N), date (D). */

/* 1.1 Contrats : variables caracteres */
data work.long_credit_c;
  set stg.STG_CREDIT_SNAPSHOT (where=(KEY_OCC = 1));
  length TABLE_SOURCE $8 VARIABLE $32 VAL_C $60;
  array c[*] I_PRSNE_RP CODSGDOS NUM_DOSS TYP_CPTE C_ENTITE C_DEVISE C_NOMNC_ENGGM SEG_DECL TYPRIS;
  TABLE_SOURCE = 'CREDIT';
  do i = 1 to dim(c);
    VARIABLE = vname(c[i]);
    VAL_C = c[i];
    output;
  end;
  keep N_PERD TABLE_SOURCE VARIABLE VAL_C;
run;

/* 1.2 Contrats : variables numeriques et dates */
data work.long_credit_n;
  set stg.STG_CREDIT_SNAPSHOT (where=(KEY_OCC = 1));
  length TABLE_SOURCE $8 VARIABLE $32 TYPE_VAR $1;
  array n[*] B_TIERS_RETAIL DECLAS_B DECLAS_H MTENGB M_ENCRS_HB;
  array d[*] DATCRE DATDECLA D_PR_EVT;
  TABLE_SOURCE = 'CREDIT';
  TYPE_VAR = 'N';
  do i = 1 to dim(n); VARIABLE = vname(n[i]); VAL_N = n[i]; output; end;
  TYPE_VAR = 'D';
  do i = 1 to dim(d); VARIABLE = vname(d[i]); VAL_N = d[i]; output; end;
  keep N_PERD TABLE_SOURCE VARIABLE TYPE_VAR VAL_N;
run;

/* 1.3 Garanties (grain lien) */
data work.long_gar_c;
  set stg.STG_COLLATERAL_LINK;
  length TABLE_SOURCE $8 VARIABLE $32 VAL_C $60;
  TABLE_SOURCE = 'GAR';
  VARIABLE = 'C_NOMNC_GARNT'; VAL_C = C_NOMNC_GARNT; output;
  VARIABLE = 'I_PERSN_GARANT'; VAL_C = I_PERSN_GARANT; output;
  keep N_PERD TABLE_SOURCE VARIABLE VAL_C;
run;

data work.long_gar_n;
  set stg.STG_COLLATERAL_LINK;
  length TABLE_SOURCE $8 VARIABLE $32 TYPE_VAR $1;
  TABLE_SOURCE = 'GAR';
  TYPE_VAR = 'N'; VARIABLE = 'M_VALO_GARANT'; VAL_N = M_VALO_GARANT; output;
  TYPE_VAR = 'D'; VARIABLE = 'D_VALO'; VAL_N = D_VALO; output;
  VARIABLE = 'D_ECHEANCE'; VAL_N = D_ECHEANCE; output;
  keep N_PERD TABLE_SOURCE VARIABLE TYPE_VAR VAL_N;
run;

/* 1.4 Evenements de perte (grain evenement) */
data work.long_evt_c;
  set stg.STG_WO_EVENT;
  length TABLE_SOURCE $8 VARIABLE $32 VAL_C $60;
  TABLE_SOURCE = 'EVT';
  VARIABLE = 'TYP_OUP'; VAL_C = TYP_OUP; output;
  VARIABLE = 'TYP_MVT'; VAL_C = TYP_MVT; output;
  keep N_PERD TABLE_SOURCE VARIABLE VAL_C;
run;

data work.long_evt_n;
  set stg.STG_WO_EVENT;
  length TABLE_SOURCE $8 VARIABLE $32 TYPE_VAR $1;
  TABLE_SOURCE = 'EVT'; TYPE_VAR = 'N'; VARIABLE = 'MTFLUX'; VAL_N = MTFLUX; output;
  keep N_PERD TABLE_SOURCE VARIABLE TYPE_VAR VAL_N;
run;

data work.long_c; set work.long_credit_c work.long_gar_c work.long_evt_c; run;
data work.long_n; set work.long_credit_n work.long_gar_n work.long_evt_n; run;

/* 2. Agregation : profil des variables caracteres ------------------------------ */
proc sql;
  create table work.prof_c as
  select N_PERD, TABLE_SOURCE, VARIABLE, 'C' as TYPE_VAR length=1,
         count(*) as N_LIGNES,
         sum(VAL_C = '') as N_MANQUANT,
         0 as N_ZERO,
         count(distinct VAL_C) as N_DISTINCT,          /* le vide n'est pas compte */
         '' as VAL_MIN length=12, '' as VAL_MAX length=12,
         . as SOMME
  from work.long_c
  group by N_PERD, TABLE_SOURCE, VARIABLE;

  /* valeur la plus frequente (hors vide) et sa frequence */
  create table work.freq_c as
  select N_PERD, TABLE_SOURCE, VARIABLE, VAL_C, count(*) as FREQ
  from work.long_c
  where VAL_C ne ''
  group by N_PERD, TABLE_SOURCE, VARIABLE, VAL_C;
quit;

proc sort data=work.freq_c; by N_PERD TABLE_SOURCE VARIABLE descending FREQ VAL_C; run;

data work.top_c;
  set work.freq_c;
  by N_PERD TABLE_SOURCE VARIABLE;
  if first.VARIABLE;
  rename VAL_C = TOP_VALEUR FREQ = TOP_FREQ;
run;

/* 3. Agregation : profil des variables numeriques et dates -------------------- */
proc sql;
  create table work.prof_n as
  select N_PERD, TABLE_SOURCE, VARIABLE, TYPE_VAR,
         count(*) as N_LIGNES,
         sum(VAL_N is missing) as N_MANQUANT,
         sum(VAL_N = 0) as N_ZERO,
         count(distinct VAL_N) as N_DISTINCT,
         min(VAL_N) as MIN_N, max(VAL_N) as MAX_N,
         case when TYPE_VAR = 'N' then sum(VAL_N) else . end as SOMME
  from work.long_n
  group by N_PERD, TABLE_SOURCE, VARIABLE, TYPE_VAR;
quit;

/* Les dates sont ecrites en AAAA-MM-JJ, les nombres en texte (2 decimales) */
data work.prof_n2;
  length VAL_MIN VAL_MAX $12;
  set work.prof_n;
  if TYPE_VAR = 'D' then do;
    if MIN_N ne . then VAL_MIN = put(MIN_N, yymmdd10.);
    if MAX_N ne . then VAL_MAX = put(MAX_N, yymmdd10.);
  end;
  else do;
    if MIN_N ne . then VAL_MIN = strip(put(round(MIN_N, 0.01), best12.));
    if MAX_N ne . then VAL_MAX = strip(put(round(MAX_N, 0.01), best12.));
  end;
  drop MIN_N MAX_N;
run;

/* 4. Assemblage et rattachement a la donnee critique ---------------------------- */
data work.prof_all;
  length TOP_VALEUR $60 TOP_FREQ 8;
  set work.prof_c work.prof_n2;
run;

proc sql;
  create table dq.DQ_PROFILE_CDE as
  select p.N_PERD, v.CDE_ID, p.TABLE_SOURCE, p.VARIABLE, p.TYPE_VAR,
         p.N_LIGNES, p.N_MANQUANT, p.N_ZERO, p.N_DISTINCT, p.VAL_MIN, p.VAL_MAX, p.SOMME,
         t.TOP_VALEUR, t.TOP_FREQ
  from work.prof_all as p
       inner join ref.REF_CDE_VARIABLE as v
         on p.TABLE_SOURCE = v.TABLE_SOURCE and p.VARIABLE = v.VARIABLE
       left join work.top_c as t
         on p.N_PERD = t.N_PERD and p.TABLE_SOURCE = t.TABLE_SOURCE and p.VARIABLE = t.VARIABLE
  order by p.N_PERD, v.CDE_ID, p.VARIABLE;
quit;

%let step_id  = DQ_02b_PROFILAGE_CDE;
%let step_msg = Profil des 15 CDE produit;
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
