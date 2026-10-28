/******************************************************************************
 Programme : DQ_04_COMPARATEUR_HLC.sas      (programme 4 sur 6 - dispositif DQ LGD / provisions)
 Etape CDC : Etape 4 - Comparateur HLC et segment, au niveau des drivers
 Version   : v1.2
 Objet     : HLC attendue contre HLC de production, comparaison driver par driver, transitions, indetermines.
 Entrees   : stg.*, dq.DRV_CONTRAT, ref.*
 Sorties   : dq.HLC_BRANCH_TRACE, dq.HLC_KPI, dq.HLC_TRANSITION, dq.HLC_DRIVER_COMPARE, dq.HLC_SEGMENT_COMPARE, dq.AGG_HLC_*
 Contenu   : S0 parametres | S1 HLC attendue | S2 comparaison des drivers
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
/* SECTION 1 : HLC ATTENDUE */
/******************************************************************************
 Section 1.1 (ex-programme DQ_04a_CONTROLE_HLC)
 Etape     : 4 - Controle de la classe de LGD (HLC) : recalcul et comparaison
 Objet     : 1) rapprocher chaque contrat des fichiers de production (HLC et provision) ;
             2) recalculer la HLC attendue avec les arbres de l'EdB (DQ_INC_ARBRES_HLC) ;
             3) comparer a la HLC de production et attribuer un statut ;
             4) tracer la branche suivie et les transitions de HLC entre M-1 et M.
 Entrees   : dq.DRV_CONTRAT (etape 3a), stg.STG_PRODUCTION_HLC, stg.STG_PRODUCTION_ECL
 Sorties   : dq.HLC_BASE (contrat x arrete : drivers + HLC attendue + production),
             dq.HLC_SEGMENT_COMPARE (statut provisoire ; la cause finale est fixee en 4b),
             dq.HLC_BRANCH_TRACE (volumes par branche), dq.HLC_TRANSITION (HLC M-1 -> M)
 Statut de comparaison (MATCH_STATUS) :
   MATCH           HLC de production = HLC attendue
   MISMATCH        HLC differente, ou contrat route hors du perimetre statistique mais present en
                   production (Loan IQ, seuil), ou modele de production different du portefeuille
   UNDETERMINED    la HLC attendue ne peut pas etre calculee (donnee absente ou non affecte), ou
                   le contrat est absent des fichiers de production
   NOT_APPLICABLE  contrat hors perimetre statistique, correctement absent de la production
 Cause (MISMATCH_CAUSE) : MAPPING, RULE_VERSION_ISSUE, ABSENT_PRODUCTION, UNDETERMINED, NOT_APPLICABLE,
   NONE ; la valeur provisoire __DRV__ (ecart de HLC sur un meme modele) devient DATA ou DECISION
   en 4b selon qu'un driver differe ou non.
 Une HLC attendue manquante n'est jamais remplacee par une valeur par defaut.
 Version   : v1.2
******************************************************************************/
%let t0 = %sysfunc(datetime());   /* heure de debut de la section */

/* Version de la regle HLC implementee par ce programme (comparee a VERSION_REGLE_PROD) */
%let ver_att = HLC_V1.1;

/* 1. Rapprochement avec la production (jointures un-a-un sur N_PERD + ID_CONTRAT) -------- */
proc sql;
  create table work.h1 as
  select a.*,
         h.MODELE_PROD, h.HLC_PROD, h.TID_PROD, h.NIV_RISQUE_PROD, h.TOP_APUR_PROD, h.GARANTIE_PROD,
         h.CRELOG_PROD, h.ENCOURS_PROD, h.DECLAS_PROD, h.CAT_CAV_PROD, h.VERSION_REGLE_PROD,
         (h.MODELE_PROD ne '' or h.HLC_PROD is not missing) as PROD_HLC_PRESENT,
         p.FLL_PROV as ECL_PROD
  from dq.DRV_CONTRAT as a
       left join stg.STG_PRODUCTION_HLC as h on a.N_PERD = h.N_PERD and a.ID_CONTRAT = h.ID_CONTRAT
       left join stg.STG_PRODUCTION_ECL as p on a.N_PERD = p.N_PERD and a.ID_CONTRAT = p.ID_CONTRAT;
quit;

/* 2. HLC attendue (arbres de l'EdB) puis statut de comparaison ------------------------------ */
data dq.HLC_BASE;
  length t_modele $8 t_typris $4 t_rcas $4 t_cat $8 t_chemin $40 t_flag $4
         CHEMIN $60 RULE_FLAG $4 MATCH_STATUS $16 MISMATCH_CAUSE $20;
  set work.h1;
  HLC_ATT = .; CHEMIN = ''; RULE_FLAG = '';

  /* Seuls les contrats du perimetre statistique ont une HLC attendue */
  if ROUTE = 'STAT' and PORTEFEUILLE in ('CORP','IMMO','CONSO','REVOL','CI','CAV') then do;
    t_modele = PORTEFEUILLE; t_tid = TID_ANNEES; t_typris = TYPRIS;
    t_wo_any = WO_ANY; t_wo_part = WO_PARTIEL;
    t_gar = GAR_ELIG; t_crelog = CRELOG_VALIDE;
    t_rcas = R_CAS; t_r = R_VALEUR; t_cat = CAT_CAV;
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
    HLC_ATT = t_hlc;
    CHEMIN = catx('>', PORTEFEUILLE, t_chemin);
    RULE_FLAG = t_flag;                       /* regle non approuvee (Q20 Q22 Q23 Q47 Q49) */
  end;

  /* Statut : les tests sont evalues dans l'ordre, le premier vrai l'emporte */
  if ROUTE ne 'STAT' then do;
    if PROD_HLC_PRESENT = 1 and ROUTE in ('LOANIQ','SEUIL') then do;
      MATCH_STATUS = 'MISMATCH'; MISMATCH_CAUSE = 'MAPPING';        /* devait etre individuel */
    end;
    else do; MATCH_STATUS = 'NOT_APPLICABLE'; MISMATCH_CAUSE = 'NOT_APPLICABLE'; end;
  end;
  else if PORTEFEUILLE not in ('CORP','IMMO','CONSO','REVOL','CI','CAV') then do;
    MATCH_STATUS = 'UNDETERMINED'; MISMATCH_CAUSE = 'UNDETERMINED';  /* Retail non affecte */
  end;
  else if PROD_HLC_PRESENT = 0 then do;
    MATCH_STATUS = 'UNDETERMINED'; MISMATCH_CAUSE = 'ABSENT_PRODUCTION';
  end;
  else if HLC_ATT = . then do;
    MATCH_STATUS = 'UNDETERMINED'; MISMATCH_CAUSE = 'UNDETERMINED';
  end;
  else if MODELE_PROD ne PORTEFEUILLE then do;
    MATCH_STATUS = 'MISMATCH'; MISMATCH_CAUSE = 'MAPPING';
  end;
  else if HLC_PROD = HLC_ATT then do;
    MATCH_STATUS = 'MATCH'; MISMATCH_CAUSE = 'NONE';
  end;
  else if VERSION_REGLE_PROD ne "&ver_att." then do;
    MATCH_STATUS = 'MISMATCH'; MISMATCH_CAUSE = 'RULE_VERSION_ISSUE';
  end;
  else do;
    MATCH_STATUS = 'MISMATCH'; MISMATCH_CAUSE = '__DRV__';           /* precise en 4b */
  end;
  drop t_: ;
run;

/* Version provisoire de la table de comparaison (completee par 4b) */
proc sql;
  create table dq.HLC_SEGMENT_COMPARE as
  select N_PERD, ID_CONTRAT, ROUTE, PORTEFEUILLE, HLC_ATT, CHEMIN, RULE_FLAG,
         MODELE_PROD, HLC_PROD, VERSION_REGLE_PROD, MATCH_STATUS, MISMATCH_CAUSE,
         '' as DRIVERS_ECART length=40, EAD
  from dq.HLC_BASE
  order by N_PERD, ID_CONTRAT;
quit;

/* 3. Trace des branches : combien de contrats suivent chaque branche de l'arbre ------------ */
proc sql;
  create table dq.HLC_BRANCH_TRACE as
  select N_PERD, PORTEFEUILLE, CHEMIN, HLC_ATT, count(*) as N_CONTRATS, sum(EAD) as EAD
  from dq.HLC_BASE
  where ROUTE = 'STAT'
  group by N_PERD, PORTEFEUILLE, CHEMIN, HLC_ATT
  order by N_PERD, PORTEFEUILLE, CHEMIN, HLC_ATT;

/* 4. Transitions de HLC attendue entre M-1 et M (contrats presents aux deux arretes) ----- */
  create table dq.HLC_TRANSITION as
  select b.PORTEFEUILLE as PORTEFEUILLE_M, a.HLC_ATT as HLC_M1, b.HLC_ATT as HLC_M,
         count(*) as N_CONTRATS
  from dq.HLC_BASE as a inner join dq.HLC_BASE as b on a.ID_CONTRAT = b.ID_CONTRAT
  where a.N_PERD = &am1. and b.N_PERD = &am.
  group by b.PORTEFEUILLE, a.HLC_ATT, b.HLC_ATT
  order by PORTEFEUILLE_M, HLC_M1, HLC_M;
quit;

%let step_id  = DQ_04a_CONTROLE_HLC;
%let step_msg = HLC attendue et comparaison a la production;
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
/* SECTION 2 : COMPARAISON DES DRIVERS */
/******************************************************************************
 Section 2.1 (ex-programme DQ_04b_COMPARAISON_DRIVERS)
 Etape     : 4 - Comparaison driver par driver et attribution de la cause finale
 Objet     : Pour chaque contrat statistique present en production, comparer les sept drivers
             recalcules aux valeurs de production, puis expliquer chaque ecart de HLC :
               DATA      au moins un driver differe : la donnee d'entree est en cause ;
               DECISION  tous les drivers sont egaux mais la HLC differe : l'arbre ou une regle
                         non tranchee est en cause (a arbitrer par le proprietaire du modele).
 Drivers   : D1 duree (TID) | D2 EDX/CER | D3 passage en perte | D4 garantie (CORP, CI)
             D5 Credit Logement (IMMO) | D6 ratio R (CI, CAV) | D7 categorie CAV
 Entrees   : dq.HLC_BASE (4a), dq.PARAM_LGDD / PARAM_TT (parametres de l'arrete)
 Sorties   : dq.HLC_DRIVER_COMPARE (contrat x driver), dq.HLC_SEGMENT_COMPARE (cause finale),
             dq.HLC_KPI, dq.AGG_HLC_COMPARE, dq.AGG_HLC_DRIVER_DIFF (avec ecart de provision)
 Ecart de provision (ECL_ECART) : provision avec la HLC de production moins provision avec la HLC
             attendue, a donnees identiques ; manquant si l'une des deux n'est pas calculable.
 Version   : v1.2
******************************************************************************/
%let t0 = %sysfunc(datetime());   /* heure de debut de la section */

/* 1. Comparaison des drivers : une ligne par contrat x driver applicable --------------------- */
data dq.HLC_DRIVER_COMPARE;
  length DRIVER_ID $4 VALEUR_RECALC VALEUR_PROD $24 EGAL 8 r_cas p_cas $4 cat_rec $8;
  set dq.HLC_BASE (where=(ROUTE = 'STAT' and PROD_HLC_PRESENT = 1 and MODELE_PROD = PORTEFEUILLE
                          and PORTEFEUILLE in ('CORP','IMMO','CONSO','REVOL','CI','CAV')));

  /* D1 - duree en defaut en annees entieres */
  DRIVER_ID = 'D1';
  VALEUR_RECALC = ifc(TID_ANNEES = ., '', strip(put(TID_ANNEES, best12.)));
  VALEUR_PROD   = ifc(TID_PROD = .,   '', strip(put(TID_PROD, best12.)));
  if TID_ANNEES ne . then EGAL = (TID_ANNEES = TID_PROD); else EGAL = .;
  output;

  /* D2 - EDX / CER */
  DRIVER_ID = 'D2';
  VALEUR_RECALC = TYPRIS; VALEUR_PROD = NIV_RISQUE_PROD;
  EGAL = (TYPRIS = NIV_RISQUE_PROD);
  output;

  /* D3 - passage en perte (write-off ; partiel pour les modeles autres que CORP et IMMO) */
  DRIVER_ID = 'D3';
  VALEUR_RECALC = ifc(WO_DRIVER = ., '', strip(put(WO_DRIVER, best12.)));
  VALEUR_PROD   = ifc(TOP_APUR_PROD = ., '', strip(put(TOP_APUR_PROD, best12.)));
  EGAL = (WO_DRIVER = TOP_APUR_PROD);
  output;

  /* D4 - garantie eligible et active (Corporate, CI) */
  if PORTEFEUILLE in ('CORP','CI') then do;
    DRIVER_ID = 'D4';
    VALEUR_RECALC = ifc(GAR_ELIG = ., '', strip(put(GAR_ELIG, best12.)));
    VALEUR_PROD   = ifc(GARANTIE_PROD = ., '', strip(put(GARANTIE_PROD, best12.)));
    EGAL = (GAR_ELIG = GARANTIE_PROD);
    output;
  end;

  /* D5 - garantie Credit Logement valide (Immobilier) */
  if PORTEFEUILLE = 'IMMO' then do;
    DRIVER_ID = 'D5';
    VALEUR_RECALC = ifc(CRELOG_VALIDE = ., '', strip(put(CRELOG_VALIDE, best12.)));
    VALEUR_PROD   = ifc(CRELOG_PROD = ., '', strip(put(CRELOG_PROD, best12.)));
    EGAL = (CRELOG_VALIDE = CRELOG_PROD);
    output;
  end;

  /* D6 - ratio R (CI, CAV) : cas + valeur ; le ratio de production est recalcule a partir
         de ses propres composantes (encours et declassement de production) */
  if PORTEFEUILLE in ('CI','CAV') then do;
    DRIVER_ID = 'D6';
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
    VALEUR_RECALC = catx(':', R_CAS, ifc(R_VALEUR = ., '', strip(put(round(R_VALEUR, 0.001), best12.))));
    VALEUR_PROD   = catx(':', p_cas, ifc(p_val = ., '', strip(put(round(p_val, 0.001), best12.))));
    EGAL = (R_CAS = p_cas);
    if EGAL = 1 and R_CAS = 'CALC' then EGAL = (abs(R_VALEUR - p_val) <= 0.001);
    output;
  end;

  /* D7 - categorie Prive / Pro (CAV) : une valeur vide du segment est traitee comme Pro (Q49) */
  if PORTEFEUILLE = 'CAV' then do;
    DRIVER_ID = 'D7';
    cat_rec = ifc(CAT_CAV = 'PRO_VIDE', 'PRO', CAT_CAV);
    VALEUR_RECALC = cat_rec; VALEUR_PROD = CAT_CAV_PROD;
    EGAL = (cat_rec = CAT_CAV_PROD);
    output;
  end;

  keep N_PERD ID_CONTRAT PORTEFEUILLE HLC_ATT HLC_PROD EAD DRIVER_ID VALEUR_RECALC VALEUR_PROD EGAL;
run;

/* 2. Liste des drivers en ecart par contrat (D1,D3...) puis cause finale --------------------- */
proc sort data=dq.HLC_DRIVER_COMPARE (where=(EGAL = 0)) out=work.bad;
  by N_PERD ID_CONTRAT DRIVER_ID;
run;

data work.ecart;
  length DRIVERS_ECART $40;
  set work.bad;
  by N_PERD ID_CONTRAT;
  retain DRIVERS_ECART;
  if first.ID_CONTRAT then DRIVERS_ECART = '';
  DRIVERS_ECART = catx(',', DRIVERS_ECART, DRIVER_ID);
  if last.ID_CONTRAT then output;
  keep N_PERD ID_CONTRAT DRIVERS_ECART;
run;

proc sql;
  create table dq.HLC_SEGMENT_COMPARE as
  select a.N_PERD, a.ID_CONTRAT, a.ROUTE, a.PORTEFEUILLE, a.HLC_ATT, a.CHEMIN, a.RULE_FLAG,
         a.MODELE_PROD, a.HLC_PROD, a.VERSION_REGLE_PROD, a.MATCH_STATUS,
         case when a.MISMATCH_CAUSE = '__DRV__' then
                (case when e.DRIVERS_ECART ne '' then 'DATA' else 'DECISION' end)
              else a.MISMATCH_CAUSE end as MISMATCH_CAUSE length=20,
         coalesce(e.DRIVERS_ECART, '') as DRIVERS_ECART length=40,
         a.EAD
  from dq.HLC_BASE as a left join work.ecart as e
       on a.N_PERD = e.N_PERD and a.ID_CONTRAT = e.ID_CONTRAT
  order by a.N_PERD, a.ID_CONTRAT;

  /* KPI de concordance : parmi les contrats comparables (MATCH + MISMATCH) */
  create table dq.HLC_KPI as
  select N_PERD, PORTEFEUILLE,
         sum(MATCH_STATUS in ('MATCH','MISMATCH')) as N_COMPARABLES,
         sum(MATCH_STATUS = 'MATCH') as N_MATCH,
         sum(MATCH_STATUS = 'MATCH') / max(sum(MATCH_STATUS in ('MATCH','MISMATCH')), 1) as TAUX_CONCORDANCE,
         sum(MATCH_STATUS = 'UNDETERMINED') as N_INDETERMINE
  from dq.HLC_SEGMENT_COMPARE
  group by N_PERD, PORTEFEUILLE;
quit;

/* 3. Ecart de provision par ligne en ecart (HLC de production contre HLC attendue) --------- */
proc sql;
  create table work.bad2 as
  select d.N_PERD, d.ID_CONTRAT, d.PORTEFEUILLE, d.DRIVER_ID, d.HLC_PROD, d.HLC_ATT, d.EAD,
         b.MTENGB, b.M_ENCRS_HB, b.TYPRIS
  from dq.HLC_DRIVER_COMPARE as d
       inner join dq.HLC_BASE as b on d.N_PERD = b.N_PERD and d.ID_CONTRAT = b.ID_CONTRAT
  where d.EGAL = 0;
quit;

data work.bad3;
  set work.bad2;
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
  e_per = N_PERD; e_route = 'STAT'; e_modele = PORTEFEUILLE; e_bil = MTENGB; e_hb = M_ENCRS_HB;
  e_typris = TYPRIS; e_enggm = '';
  e_hlc = HLC_PROD;
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
  ecl_prod_hlc = e_ecl;
  e_hlc = HLC_ATT;
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
  ECL_ECART = ecl_prod_hlc - e_ecl;               /* manquant si l'un des deux est manquant */
run;

proc sql;
  create table dq.AGG_HLC_DRIVER_DIFF as
  select N_PERD, PORTEFEUILLE, DRIVER_ID, HLC_PROD, HLC_ATT as HLC_RECALC,
         count(*) as N_CONTRATS, sum(EAD) as EAD, sum(ECL_ECART) as ECL_ECART
  from work.bad3
  group by N_PERD, PORTEFEUILLE, DRIVER_ID, HLC_PROD, HLC_ATT
  order by N_PERD, PORTEFEUILLE, DRIVER_ID, HLC_PROD, HLC_RECALC;

  create table dq.AGG_HLC_COMPARE as
  select N_PERD, PORTEFEUILLE, MATCH_STATUS, MISMATCH_CAUSE,
         count(*) as N_CONTRATS, sum(a.EAD) as EAD, coalesce(sum(b.ECL_PROD), 0) as ECL_PROD
  from dq.HLC_SEGMENT_COMPARE as a
       inner join (select N_PERD as P2, ID_CONTRAT as I2, ECL_PROD from dq.HLC_BASE) as b
         on a.N_PERD = b.P2 and a.ID_CONTRAT = b.I2
  group by N_PERD, PORTEFEUILLE, MATCH_STATUS, MISMATCH_CAUSE
  order by N_PERD, PORTEFEUILLE, MATCH_STATUS, MISMATCH_CAUSE;
quit;

%let step_id  = DQ_04b_COMPARAISON_DRIVERS;
%let step_msg = Drivers compares et causes finales;
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
