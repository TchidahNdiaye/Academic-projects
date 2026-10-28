/******************************************************************************
 Programme : DQ_05_MOTEUR_FINANCIER.sas      (programme 5 sur 6 - dispositif DQ LGD / provisions)
 Etape CDC : Etape 5 - Moteur financier : provision recalculee, impact, variation M / M-1
 Version   : v1.2
 Objet     : Provision recalculee et corrigee (trois scenarios), impact des anomalies, bridge de population et d effets M-1 vers M.
 Entrees   : stg.*, dq.DRV_CONTRAT, dq.DQ_ISSUE, dq.HLC_*, ref.*
 Sorties   : dq.ECL_SCENARIO_RESULT, dq.ECL_EFFECT_BRIDGE, dq.FINANCIAL_RECON, dq.POPULATION_BRIDGE, dq.AGG_ECL, dq.AGG_IMPACT
 Contenu   : S0 parametres | S1 provision et impact des anomalies | S2 bridge M-1 vers M
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
/* SECTION 1 : PROVISION ET IMPACT */
/******************************************************************************
 Section 1.1 (ex-programme DQ_05a_IMPACT_ECL)
 Etape     : 5 - Impact financier des anomalies : trois scenarios de provision
 Objet     : Recalculer la provision FLL_PROV de chaque contrat du perimetre (statistique et PGE)
             dans trois scenarios, a parametres identiques (dq.PARAM_*) :
               PROD     provision de la production (stg.STG_PRODUCTION_ECL)
               RECALC   HLC attendue (4a) et donnees telles quelles
               CORRIGE  HLC attendue apres correction des anomalies corrigeables :
                        - declassement bilan nul ou absent : repli sur l'engagement (EdB 3.8) ;
                        - exposition hors bilan manquante : estimation (declassement H x part du bilan) ;
                        - exposition bilan negative hors CAV : valeur absolue.
             IMPACT = CORRIGE - PROD, affecte UNE seule fois par contrat (jamais additionne par anomalie).
 Regle DQ-022 : provision de production contre recalcul. F si l'ecart depasse
             max(TOL_PROV_ABS ; TOL_PROV_REL x |provision|) ou si la provision est absente ;
             U si le recalcul n'est pas calculable ; A hors perimetre.
 Statut de l'impact (METHODE_IMPACT de ref.REF_REGLE) :
             CALCULATED (correction exacte), ESTIMATED (hypothese), NOT_QUANTIFIABLE (impact vide).
 Entrees   : dq.HLC_BASE, dq.DQ_ISSUE, dq.DQ_RULE_FLAG, dq.PARAM_*, ref.REF_SEUIL, ref.REF_REGLE
 Sorties   : dq.ECL_SCENARIO_RESULT, dq.DQ_RULE_FLAG (R022 mis a jour), dq.DQ_ISSUE (+ DQ-022),
             dq.AGG_RULE_RESULT (+ DQ-022), dq.AGG_ISSUE (recalcule), dq.AGG_ECL, dq.AGG_IMPACT,
             dq.FINANCIAL_RECON
 Version   : v1.2
******************************************************************************/
%let t0 = %sysfunc(datetime());   /* heure de debut de la section */

/* 1. Provision dans les scenarios RECALC et CORRIGE ---------------------------------------- */
data dq.ECL_SCENARIO_RESULT;
  length t_modele $8 t_typris $4 t_rcas $4 t_cat $8 t_chemin $40 t_flag $4 r_cas $4;
  set dq.HLC_BASE (keep=N_PERD ID_CONTRAT ROUTE PORTEFEUILLE C_NOMNC_ENGGM TYPRIS HLC_ATT
                        MTENGB M_ENCRS_HB DECLAS_B DECLAS_H EAD ECL_PROD
                        TID_ANNEES WO_ANY WO_PARTIEL GAR_ELIG CRELOG_VALIDE CAT_CAV);
  length h_per h_hlc h_lgdd h_fl h_tt h_taux 8 h_mod $14 h_typ $8 h_ent $10;
  if _n_ = 1 then do;
    declare hash hl(dataset: "dq.PARAM_LGDD(rename=(N_PERD=h_per MODELE=h_mod HLC=h_hlc LGDD=h_lgdd FL=h_fl))");
    hl.defineKey('h_per', 'h_mod', 'h_hlc');
    hl.defineData('h_lgdd', 'h_fl');
    hl.defineDone();
    declare hash ht(dataset: "dq.PARAM_TT(rename=(N_PERD=h_per MODELE=h_mod TYPRIS=h_typ TAUX_TIRAGE=h_tt))");
    ht.defineKey('h_per', 'h_mod', 'h_typ');
    ht.defineData('h_tt');
    ht.defineDone();
    declare hash hp(dataset: "dq.PARAM_PGE(rename=(N_PERD=h_per C_NOMNC_ENGGM=h_ent TYPRIS=h_typ TAUX_FORFAIT=h_taux))");
    hp.defineKey('h_per', 'h_ent', 'h_typ');
    hp.defineData('h_taux');
    hp.defineDone();
  end;
  ECL_RECALC = .; ECL_CORRIGE = .;

  if ROUTE in ('STAT','PGE') then do;
    /* scenario RECALC */
    e_per = N_PERD; e_route = ROUTE; e_modele = PORTEFEUILLE; e_hlc = HLC_ATT;
    e_bil = MTENGB; e_hb = M_ENCRS_HB; e_typris = TYPRIS; e_enggm = C_NOMNC_ENGGM;
    e_ecl = .;
    if e_bil ne . then do;
      e_bp = max(e_bil, 0);
      e_hp = max(coalesce(e_hb, 0), 0);
      if e_route = 'PGE' then do;
        h_per = e_per; h_ent = e_enggm; h_typ = e_typris; h_taux = .;
        if hp.find() = 0 then e_ecl = h_taux * e_bp;
      end;
      else if e_route = 'STAT' and e_hlc ne . then do;
        h_per = e_per; h_mod = e_modele; h_hlc = e_hlc; h_lgdd = .; h_fl = .;
        if hl.find() = 0 then do;
          h_typ = e_typris; h_tt = 0;
          if ht.find() ne 0 then h_tt = 0;                  /* taux de tirage absent : 0 (comme la production) */
          e_ecl = h_fl * (h_lgdd * e_bp + h_lgdd * h_tt * e_hp);
        end;
      end;
    end;
    ECL_RECALC = e_ecl;

    /* scenario CORRIGE : corrections successives sur des copies des donnees */
    bil = MTENGB; hb = M_ENCRS_HB; h2 = HLC_ATT;

    /* (a) declassement bilan nul ou absent : le denominateur de R devient l'engagement */
    if (DECLAS_B = . or DECLAS_B = 0) and bil ne . and bil > 0 and ROUTE = 'STAT' then do;
      r_bil = bil; r_den = sum(bil, DECLAS_H);
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
      t_modele = PORTEFEUILLE; t_tid = TID_ANNEES; t_typris = TYPRIS;
      t_wo_any = WO_ANY; t_wo_part = WO_PARTIEL; t_gar = GAR_ELIG; t_crelog = CRELOG_VALIDE;
      t_rcas = r_cas; t_r = r_val; t_cat = CAT_CAV;
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
              if t_r <= &s_R_CI. then t_hlc = 5; else t_hlc = t_h678;
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
            if t_r < &s_R_CAV. then t_hlc = 4; else t_hlc = 5;
            t_chemin = 'TID1_6>RATIO';
          end;
        end;

        else t_chemin = 'MODELE_INCONNU';
      end;
      h2 = t_hlc;
    end;

    /* (b) exposition bilan negative hors CAV : valeur absolue */
    if bil ne . and bil < 0 and PORTEFEUILLE ne 'CAV' then bil = abs(bil);

    /* (c) exposition hors bilan manquante : estimation (hypothese) */
    if hb = . and PORTEFEUILLE in ('REVOL','CAV','CI','CORP') then do;
      if DECLAS_H = . then hb = 0;
      else do;
        if DECLAS_B = . or DECLAS_B = 0 then part = 1;
        else if bil = . then part = 0;
        else part = min(1, abs(bil) / DECLAS_B);
        hb = DECLAS_H * part;
      end;
    end;

    e_hlc = h2; e_bil = bil; e_hb = hb;
    e_ecl = .;
    if e_bil ne . then do;
      e_bp = max(e_bil, 0);
      e_hp = max(coalesce(e_hb, 0), 0);
      if e_route = 'PGE' then do;
        h_per = e_per; h_ent = e_enggm; h_typ = e_typris; h_taux = .;
        if hp.find() = 0 then e_ecl = h_taux * e_bp;
      end;
      else if e_route = 'STAT' and e_hlc ne . then do;
        h_per = e_per; h_mod = e_modele; h_hlc = e_hlc; h_lgdd = .; h_fl = .;
        if hl.find() = 0 then do;
          h_typ = e_typris; h_tt = 0;
          if ht.find() ne 0 then h_tt = 0;                  /* taux de tirage absent : 0 (comme la production) */
          e_ecl = h_fl * (h_lgdd * e_bp + h_lgdd * h_tt * e_hp);
        end;
      end;
    end;
    ECL_CORRIGE = e_ecl;
  end;
  keep N_PERD ID_CONTRAT ROUTE PORTEFEUILLE MTENGB M_ENCRS_HB EAD ECL_PROD ECL_RECALC ECL_CORRIGE;
run;

/* 2. DQ-022 : provision de production contre recalcul --------------------------------------- */
data work.r22;
  set dq.ECL_SCENARIO_RESULT (keep=N_PERD ID_CONTRAT ECL_PROD ECL_RECALC);
  length R022 $1;
  tol = max(&s_TOL_PROV_ABS., &s_TOL_PROV_REL. * abs(ECL_PROD));      /* abs(manquant) = manquant */
  if ECL_PROD ne . or ECL_RECALC ne . then do;
    R022 = 'P';
    if ECL_PROD ne . and ECL_RECALC = . then R022 = 'U';
    else if ECL_PROD ne . and ECL_RECALC ne . and abs(ECL_PROD - ECL_RECALC) > tol then R022 = 'F';
    else if ECL_PROD = . and ECL_RECALC ne . then R022 = 'F';       /* provision absente de la production */
  end;
  else R022 = 'A';
  keep N_PERD ID_CONTRAT R022;
run;

proc sort data=dq.DQ_RULE_FLAG; by N_PERD ID_CONTRAT; run;
proc sort data=work.r22; by N_PERD ID_CONTRAT; run;
data dq.DQ_RULE_FLAG;
  merge dq.DQ_RULE_FLAG (drop=R022) work.r22;
  by N_PERD ID_CONTRAT;
run;

/* 3. Anomalies DQ-022 : meme qualification (NOUVELLE...) que les autres regles ------------ */
proc sql;
  delete from dq.DQ_ISSUE where RULE_ID = 'DQ-022';
  delete from dq.AGG_RULE_RESULT where RULE_ID = 'DQ-022';

  create table work.f22 as
  select f.N_PERD, f.ID_CONTRAT, f.PORTEFEUILLE, f.EAD, 'DQ-022' as RULE_ID length=8
  from dq.DQ_RULE_FLAG as f where f.R022 = 'F';

  create table work.i22 as
  select a.N_PERD, a.ID_CONTRAT, a.PORTEFEUILLE, a.RULE_ID, a.EAD,
         r.SEVERITE, r.CAUSE_RACINE, r.METHODE_IMPACT,
         case when a.N_PERD = &am. then
                (case when b.ID_CONTRAT is not null then 'PERSISTANTE' else 'NOUVELLE' end)
              else
                (case when b.ID_CONTRAT is not null then 'PERSISTANTE'
                      when d.ID_CONTRAT is null then 'SORTIE'
                      else 'RESOLUE' end)
         end as STATUT length=12
  from work.f22 as a
       left join ref.REF_REGLE as r on a.RULE_ID = r.RULE_ID
       left join work.f22 as b
         on a.ID_CONTRAT = b.ID_CONTRAT
            and b.N_PERD = case when a.N_PERD = &am. then &am1. else &am. end
       left join (select ID_CONTRAT from dq.DRV_CONTRAT where N_PERD = &am.) as d
         on a.ID_CONTRAT = d.ID_CONTRAT;
quit;

data dq.DQ_ISSUE;
  set dq.DQ_ISSUE work.i22;
run;
proc sort data=dq.DQ_ISSUE; by N_PERD ID_CONTRAT RULE_ID; run;

/* Historique : on remplace les lignes DQ-022 du run */
proc sql;
  delete from dq.DQ_ISSUE_HIST where RUN_ID = "&run_id." and RULE_ID = 'DQ-022';
quit;
data work.hist22;
  length RUN_ID $20;
  set work.i22 (keep=N_PERD ID_CONTRAT RULE_ID PORTEFEUILLE EAD STATUT);
  RUN_ID = "&run_id.";
run;
proc append base=dq.DQ_ISSUE_HIST data=work.hist22 force; run;

/* Agregats des anomalies recalcules (inclut DQ-022) et resultat de la regle DQ-022 */
proc sql;
  create table dq.AGG_ISSUE as
  select N_PERD, PORTEFEUILLE, CAUSE_RACINE, STATUT, SEVERITE,
         count(*) as N_ANOMALIES, sum(EAD) as EAD
  from dq.DQ_ISSUE
  group by N_PERD, PORTEFEUILLE, CAUSE_RACINE, STATUT, SEVERITE;

  create table work.rr22 as
  select f.N_PERD, f.PORTEFEUILLE, 'DQ-022' as RULE_ID length=8, r.CDE_ID, r.SEVERITE,
         count(*) as N_CONTRATS,
         sum(f.R022 in ('P','F','U')) as N_TESTE,
         sum(f.R022 = 'F') as N_ECHEC,
         sum(f.R022 = 'U') as N_INDETERMINE,
         sum(f.R022 = 'A') as N_NON_APPLICABLE,
         sum(f.R022 = 'T') as N_NON_TESTE,
         sum(case when f.R022 = 'F' then f.EAD else 0 end) as EAD_ECHEC
  from dq.DQ_RULE_FLAG as f, ref.REF_REGLE as r
  where r.RULE_ID = 'DQ-022'
  group by f.N_PERD, f.PORTEFEUILLE, r.CDE_ID, r.SEVERITE;
quit;
proc append base=dq.AGG_RULE_RESULT data=work.rr22 force; run;

/* 4. Agregats de provision et reconciliation financiere --------------------------------------- */
proc sql;
  create table dq.AGG_ECL as
  select N_PERD, PORTEFEUILLE,
         count(*) as N_CONTRATS,
         sum(case when MTENGB > 0 then MTENGB else 0 end) as EAD_BIL,
         sum(case when M_ENCRS_HB > 0 then M_ENCRS_HB else 0 end) as EAD_HB,
         sum(ECL_RECALC) as ECL,
         sum(ECL_PROD) as ECL_PROD,
         sum(ECL_CORRIGE) as ECL_CORRIGE
  from dq.ECL_SCENARIO_RESULT
  where ROUTE in ('STAT','PGE')
  group by N_PERD, PORTEFEUILLE
  order by N_PERD, PORTEFEUILLE;

  create table dq.FINANCIAL_RECON as
  select N_PERD, PORTEFEUILLE,
         sum(ECL_PROD) as ECL_PROD, sum(ECL_RECALC) as ECL_RECALC, sum(ECL_CORRIGE) as ECL_CORRIGE,
         sum(ECL_PROD) - sum(ECL_RECALC) as ECART
  from dq.ECL_SCENARIO_RESULT
  where ROUTE in ('STAT','PGE')
  group by N_PERD, PORTEFEUILLE
  order by N_PERD, PORTEFEUILLE;
quit;

/* 5. Impact par methode : un montant par contrat, porte par la methode la plus forte ----------
   Priorite : ESTIMATED (2) > CALCULATED (1) > NOT_QUANTIFIABLE (0, impact vide).
   Pour un contrat, l'impact CORRIGE - PROD est porte par la premiere anomalie de la meilleure
   methode ; les autres anomalies du contrat comptent dans N_ANOMALIES mais sans montant. */
proc sql;
  create table work.imp1 as
  select i.N_PERD, i.ID_CONTRAT, i.PORTEFEUILLE, i.RULE_ID, i.METHODE_IMPACT, i.EAD,
         case i.METHODE_IMPACT when 'ESTIMATED' then 2 when 'CALCULATED' then 1 else 0 end as PRIO,
         e.ECL_CORRIGE - e.ECL_PROD as IMPACT_CTR
  from dq.DQ_ISSUE as i
       left join dq.ECL_SCENARIO_RESULT as e on i.N_PERD = e.N_PERD and i.ID_CONTRAT = e.ID_CONTRAT
  order by i.N_PERD, i.ID_CONTRAT, i.RULE_ID;

  create table work.imp2 as
  select N_PERD, ID_CONTRAT, max(PRIO) as PBEST from work.imp1 group by N_PERD, ID_CONTRAT;
quit;

data work.imp3;
  merge work.imp1 work.imp2;
  by N_PERD ID_CONTRAT;
  retain porte_fait;
  if first.ID_CONTRAT then porte_fait = 0;
  IMPACT_ECL = .;
  if PRIO = PBEST and PRIO > 0 and porte_fait = 0 then do;
    IMPACT_ECL = IMPACT_CTR;
    porte_fait = 1;
  end;
run;

proc sql;
  create table work.imp4 as
  select N_PERD, PORTEFEUILLE, METHODE_IMPACT as STATUT_IMPACT,
         count(*) as N_ANOMALIES, sum(IMPACT_ECL) as IMPACT_ECL
  from work.imp3
  group by N_PERD, PORTEFEUILLE, METHODE_IMPACT;

  create table work.imp5 as
  select N_PERD, PORTEFEUILLE, METHODE_IMPACT as STATUT_IMPACT, sum(EAD) as EAD
  from (select distinct N_PERD, ID_CONTRAT, PORTEFEUILLE, METHODE_IMPACT, EAD from work.imp3)
  group by N_PERD, PORTEFEUILLE, METHODE_IMPACT;

  create table dq.AGG_IMPACT as
  select a.N_PERD, a.PORTEFEUILLE, a.STATUT_IMPACT, a.N_ANOMALIES, b.EAD, a.IMPACT_ECL
  from work.imp4 as a inner join work.imp5 as b
       on a.N_PERD = b.N_PERD and a.PORTEFEUILLE = b.PORTEFEUILLE and a.STATUT_IMPACT = b.STATUT_IMPACT
  order by a.N_PERD, a.PORTEFEUILLE, a.STATUT_IMPACT;
quit;

%let step_id  = DQ_05a_IMPACT_ECL;
%let step_msg = Provisions PROD RECALC CORRIGE et impact;
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
/* SECTION 2 : BRIDGE M-1 VERS M */
/******************************************************************************
 Section 2.1 (ex-programme DQ_05b_BRIDGE_MM1)
 Etape     : 5 - Passage de la provision de M-1 a M (bridge) : populations et effets
 Objet     : Expliquer la variation de la provision recalculee entre M-1 et M.
             Populations (une ligne par portefeuille) :
               OPEN  provision recalculee de M-1 (perimetre statistique et PGE)
               P01   entrees : contrats presents a M seulement
               P02   sorties : contrats presents a M-1 seulement (montant negatif)
               P03   changements de perimetre ou de portefeuille entre M-1 et M
             Effets sur les contrats communs restes dans le meme portefeuille (E01 a E07), obtenus en
             changeant UN facteur a la fois dans cet ordre, et en recalculant la provision a chaque etat :
               etat 0  M-1 (donnees, HLC, parametres de M-1)
               etat 1  + exposition bilan de M          -> E01 effet bilan
               etat 2  + exposition hors bilan de M     -> E02 effet hors bilan
               etat 3  + garanties de M (HLC rejouee)   -> E03 effet garantie
               etat 4  + duree et statut de M (HLC rejouee, EDX/CER de M) -> E04 effet TID / statut
               etat 5  + HLC attendue de M              -> E05 effet changement de HLC (reliquat de l'arbre)
               etat 6  + parametres de M (LGDD, FL, taux) -> E06 effet parametres
             E07 = residu : variation reelle moins somme des effets explicites (non explique).
             Controle : OPEN + P01 + P02 + P03 + E01..E07 = provision recalculee de M, par portefeuille.
 Entrees   : dq.HLC_BASE, dq.ECL_SCENARIO_RESULT, dq.PARAM_*
 Sorties   : dq.POPULATION_BRIDGE, dq.ECL_EFFECT_BRIDGE (= AGG_ECL_EFFECT), dq.BRIDGE_RECON
 Version   : v1.2
******************************************************************************/
%let t0 = %sysfunc(datetime());   /* heure de debut de la section */

/* 1. Un enregistrement par contrat avec les valeurs de M-1 (_0) et de M (_1) ---------------- */
proc sql;
  create table work.ba as
  select h.ID_CONTRAT, h.ROUTE as ROUTE_0, h.PORTEFEUILLE as PTF_0, h.HLC_ATT as HLC_0,
         h.TID_ANNEES as TID_0, h.TYPRIS as TYP_0, h.WO_ANY as WOA_0, h.WO_PARTIEL as WOP_0,
         h.GAR_ELIG as GAR_0, h.CRELOG_VALIDE as CRL_0, h.R_CAS as RCAS_0, h.R_VALEUR as RVAL_0,
         h.CAT_CAV as CAT_0, h.MTENGB as BIL_0, h.M_ENCRS_HB as HB_0, h.C_NOMNC_ENGGM as ENG_0,
         e.ECL_RECALC as ECL_0
  from dq.HLC_BASE as h inner join dq.ECL_SCENARIO_RESULT as e
       on h.N_PERD = e.N_PERD and h.ID_CONTRAT = e.ID_CONTRAT
  where h.N_PERD = &am1.
  order by h.ID_CONTRAT;

  create table work.bb as
  select h.ID_CONTRAT, h.ROUTE as ROUTE_1, h.PORTEFEUILLE as PTF_1, h.HLC_ATT as HLC_1,
         h.TID_ANNEES as TID_1, h.TYPRIS as TYP_1, h.WO_ANY as WOA_1, h.WO_PARTIEL as WOP_1,
         h.GAR_ELIG as GAR_1, h.CRELOG_VALIDE as CRL_1, h.R_CAS as RCAS_1, h.R_VALEUR as RVAL_1,
         h.CAT_CAV as CAT_1, h.MTENGB as BIL_1, h.M_ENCRS_HB as HB_1, h.C_NOMNC_ENGGM as ENG_1,
         e.ECL_RECALC as ECL_1
  from dq.HLC_BASE as h inner join dq.ECL_SCENARIO_RESULT as e
       on h.N_PERD = e.N_PERD and h.ID_CONTRAT = e.ID_CONTRAT
  where h.N_PERD = &am.
  order by h.ID_CONTRAT;
quit;

/* 2. Etats 0 a 6 et lignes du bridge (format long : une ligne par contrat x effet) ----------- */
data work.rows;
  length t_modele $8 t_typris $4 t_rcas $4 t_cat $8 t_chemin $40 t_flag $4
         e_route $4 e_modele $8 e_typris $4 e_enggm $10 PORTEFEUILLE $8 EFFET_CODE $12 POPULATION $12;
  array v[7] v1-v7;
  merge work.ba (in=ina) work.bb (in=inb);
  by ID_CONTRAT;
  length h_per h_hlc h_lgdd h_fl h_tt h_taux 8 h_mod $14 h_typ $8 h_ent $10;
  if _n_ = 1 then do;
    declare hash hl(dataset: "dq.PARAM_LGDD(rename=(N_PERD=h_per MODELE=h_mod HLC=h_hlc LGDD=h_lgdd FL=h_fl))");
    hl.defineKey('h_per', 'h_mod', 'h_hlc');
    hl.defineData('h_lgdd', 'h_fl');
    hl.defineDone();
    declare hash ht(dataset: "dq.PARAM_TT(rename=(N_PERD=h_per MODELE=h_mod TYPRIS=h_typ TAUX_TIRAGE=h_tt))");
    ht.defineKey('h_per', 'h_mod', 'h_typ');
    ht.defineData('h_tt');
    ht.defineDone();
    declare hash hp(dataset: "dq.PARAM_PGE(rename=(N_PERD=h_per C_NOMNC_ENGGM=h_ent TYPRIS=h_typ TAUX_FORFAIT=h_taux))");
    hp.defineKey('h_per', 'h_ent', 'h_typ');
    hp.defineData('h_taux');
    hp.defineDone();
  end;

  s0 = (ina and ROUTE_0 in ('STAT','PGE'));
  s1 = (inb and ROUTE_1 in ('STAT','PGE'));
  z0 = sum(ECL_0, 0); z1 = sum(ECL_1, 0);

  /* Ouverture : toute la provision recalculee de M-1 dans le perimetre */
  if s0 then do;
    PORTEFEUILLE = PTF_0; EFFET_CODE = 'OPEN'; MONTANT = z0; POPULATION = 'OPEN'; output;
  end;

  if ina and not inb then do;                         /* sortie */
    if s0 then do; PORTEFEUILLE = PTF_0; EFFET_CODE = 'P02'; MONTANT = -z0; POPULATION = 'P02'; output; end;
  end;
  else if inb and not ina then do;                    /* entree */
    if s1 then do; PORTEFEUILLE = PTF_1; EFFET_CODE = 'P01'; MONTANT = z1; POPULATION = 'P01'; output; end;
  end;
  else if (s0 ne s1) or (s0 and s1 and (PTF_0 ne PTF_1 or ROUTE_0 ne ROUTE_1)) then do;
    /* changement de perimetre ou de portefeuille : on sort l'ancien, on entre le nouveau */
    if s0 then do; PORTEFEUILLE = PTF_0; EFFET_CODE = 'P03'; MONTANT = -z0; POPULATION = 'P03'; output; end;
    if s1 then do; PORTEFEUILLE = PTF_1; EFFET_CODE = 'P03'; MONTANT = z1;  POPULATION = 'P03'; output; end;
  end;
  else if s0 and s1 then do;                          /* contrat commun, meme portefeuille et meme route */
    PORTEFEUILLE = PTF_1; POPULATION = 'COMMUN';
    e_route = ROUTE_0; e_modele = PTF_1;

    /* HLC rejouee avec les garanties de M (etat 3) et avec la duree/statut de M (etat 4) ;
       le ratio R et la categorie CAV restent ceux de M-1 */
    hg = .; hd = .;
    if ROUTE_0 = 'STAT' then do;
      t_modele = PTF_1; t_tid = TID_0; t_typris = TYP_0; t_wo_any = WOA_0; t_wo_part = WOP_0;
      t_gar = GAR_1; t_crelog = CRL_1; t_rcas = RCAS_0; t_r = RVAL_0; t_cat = CAT_0;
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
              if t_r <= &s_R_CI. then t_hlc = 5; else t_hlc = t_h678;
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
            if t_r < &s_R_CAV. then t_hlc = 4; else t_hlc = 5;
            t_chemin = 'TID1_6>RATIO';
          end;
        end;

        else t_chemin = 'MODELE_INCONNU';
      end;
      hg = t_hlc;
      t_modele = PTF_1; t_tid = TID_1; t_typris = TYP_1; t_wo_any = WOA_1; t_wo_part = WOP_1;
      t_gar = GAR_1; t_crelog = CRL_1; t_rcas = RCAS_0; t_r = RVAL_0; t_cat = CAT_0;
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
              if t_r <= &s_R_CI. then t_hlc = 5; else t_hlc = t_h678;
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
            if t_r < &s_R_CAV. then t_hlc = 4; else t_hlc = 5;
            t_chemin = 'TID1_6>RATIO';
          end;
        end;

        else t_chemin = 'MODELE_INCONNU';
      end;
      hd = t_hlc;
    end;

    do k = 0 to 6;
      select (k);
        when (0) do; e_hlc = HLC_0;                  e_bil = BIL_0; e_hb = HB_0; e_typris = TYP_0; e_enggm = ENG_0; e_per = &am1.; end;
        when (1) do; e_hlc = HLC_0;                  e_bil = BIL_1; e_hb = HB_0; e_typris = TYP_0; e_enggm = ENG_0; e_per = &am1.; end;
        when (2) do; e_hlc = HLC_0;                  e_bil = BIL_1; e_hb = HB_1; e_typris = TYP_0; e_enggm = ENG_0; e_per = &am1.; end;
        when (3) do; e_hlc = ifn(hg ne ., hg, HLC_0); e_bil = BIL_1; e_hb = HB_1; e_typris = TYP_0; e_enggm = ENG_0; e_per = &am1.; end;
        when (4) do; e_hlc = ifn(hd ne ., hd, HLC_0); e_bil = BIL_1; e_hb = HB_1; e_typris = TYP_1; e_enggm = ENG_1; e_per = &am1.; end;
        when (5) do; e_hlc = HLC_1;                  e_bil = BIL_1; e_hb = HB_1; e_typris = TYP_1; e_enggm = ENG_1; e_per = &am1.; end;
        otherwise do; e_hlc = HLC_1;                 e_bil = BIL_1; e_hb = HB_1; e_typris = TYP_1; e_enggm = ENG_1; e_per = &am.;  end;
      end;
      e_ecl = .;
      if e_bil ne . then do;
        e_bp = max(e_bil, 0);
        e_hp = max(coalesce(e_hb, 0), 0);
        if e_route = 'PGE' then do;
          h_per = e_per; h_ent = e_enggm; h_typ = e_typris; h_taux = .;
          if hp.find() = 0 then e_ecl = h_taux * e_bp;
        end;
        else if e_route = 'STAT' and e_hlc ne . then do;
          h_per = e_per; h_mod = e_modele; h_hlc = e_hlc; h_lgdd = .; h_fl = .;
          if hl.find() = 0 then do;
            h_typ = e_typris; h_tt = 0;
            if ht.find() ne 0 then h_tt = 0;                  /* taux de tirage absent : 0 (comme la production) */
            e_ecl = h_fl * (h_lgdd * e_bp + h_lgdd * h_tt * e_hp);
          end;
        end;
      end;
      v[k + 1] = sum(e_ecl, 0);                       /* non calculable : 0, la difference restera dans le residu */
    end;

    EFFET_CODE = 'E01'; MONTANT = v2 - v1; output;
    EFFET_CODE = 'E02'; MONTANT = v3 - v2; output;
    EFFET_CODE = 'E03'; MONTANT = v4 - v3; output;
    EFFET_CODE = 'E04'; MONTANT = v5 - v4; output;
    EFFET_CODE = 'E05'; MONTANT = v6 - v5; output;
    EFFET_CODE = 'E06'; MONTANT = v7 - v6; output;
    EFFET_CODE = 'E07'; MONTANT = (z1 - z0) - (v7 - v1); output;     /* residu */
  end;
  keep ID_CONTRAT PORTEFEUILLE EFFET_CODE POPULATION MONTANT;
run;

/* 3. Agregats --------------------------------------------------------------------------------- */
proc sql;
  create table dq.ECL_EFFECT_BRIDGE as
  select &am. as N_PERD, PORTEFEUILLE, EFFET_CODE, sum(MONTANT) as MONTANT
  from work.rows
  group by PORTEFEUILLE, EFFET_CODE
  order by PORTEFEUILLE, EFFET_CODE;

  create table dq.POPULATION_BRIDGE as
  select &am. as N_PERD, PORTEFEUILLE, POPULATION, count(distinct ID_CONTRAT) as N_CONTRATS
  from work.rows
  where EFFET_CODE in ('OPEN','P01','P02','P03') or POPULATION = 'COMMUN'
  group by PORTEFEUILLE, POPULATION
  order by PORTEFEUILLE, POPULATION;

  /* Controle : ouverture + variations = provision recalculee de M, par portefeuille */
  create table work.close1 as
  select PORTEFEUILLE, sum(MONTANT) as ECL_BRIDGE from work.rows group by PORTEFEUILLE;

  create table work.close2 as
  select PORTEFEUILLE, sum(ECL_RECALC) as ECL_M
  from dq.ECL_SCENARIO_RESULT
  where N_PERD = &am. and ROUTE in ('STAT','PGE')
  group by PORTEFEUILLE;

  create table dq.BRIDGE_RECON as
  select coalesce(a.PORTEFEUILLE, b.PORTEFEUILLE) as PORTEFEUILLE, &am. as N_PERD,
         a.ECL_BRIDGE, b.ECL_M, a.ECL_BRIDGE - b.ECL_M as ECART,
         case when abs(sum(a.ECL_BRIDGE, -b.ECL_M)) <= &s_TOL_MONTANT. then 'PASS' else 'FAIL' end as STATUT length=8
  from work.close1 as a full join work.close2 as b on a.PORTEFEUILLE = b.PORTEFEUILLE;
quit;

%let step_id  = DQ_05b_BRIDGE_MM1;
%let step_msg = Bridge de provision M-1 vers M;
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
