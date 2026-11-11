/******************************************************************************
 Programme : 02_ROUTAGE_HLC.sas
 Objet     : Evaluer la qualite du routage (segment) et de chaque arbre de decision LGD : branche suivie, HLC attendue contre HLC de production, indetermines, ecart driver par driver, transitions M-1 vers M.
 Entrees   : out.extraction_DWR_&periode. et out.extraction_DWR_&periode1. (memes tables consolidees que l etape 2-3)
 Sorties   : UN classeur Excel pour Power BI : &dir_out./PBI_02_Routage_HLC_&periode..xlsx (un onglet par table) : PUB_E4_SYNTHESE, PUB_HLC_STOCK, PUB_SCOPE, PUB_HLC_COMPARE, (PUB_HLC_DRIVER_DIFF est publiee par le programme 03), PUB_DRIVER_ECART, PUB_HLC_BRANCH, PUB_HLC_INDETERMINE, PUB_HLC_TRANSITION, DIM_DRIVER ; table SAS : out.dq_hlc_base_&periode. (lue par l etape 5)
 Contenu   : S0 parametres et correspondance | S1 drivers | S2 routage attendu | S3 arbres HLC | S4 comparaison | S5 libelles | S6 publication | S7 classeur Excel
******************************************************************************/

%let t_start = %sysfunc(datetime());
/*===========================================================================
  S0. PARAMETRES COMMUNS AUX TROIS PROGRAMMES (a modifier a chaque arrete)
      Ces parametres sont dans 00_PARAMETRES.sas (un seul fichier pour les trois programmes).
      Aucun fichier referentiel n'est lu : tout est ecrit dans les programmes.
===========================================================================*/
/* Parametres communs (arretes, seuils, poids, domaines, formats, couleurs, table de travail work.base) :
   tout est dans 00_PARAMETRES.sas, a modifier une seule fois pour les trois programmes. */
%let dir_prog = /chemin/vers/programmes;          /* dossier contenant 00_PARAMETRES.sas */
%include "&dir_prog./00_PARAMETRES.sas";

/* S0.7 CORRESPONDANCE DES VARIABLES ET DES CODES PROPRES A L ETAPE 4 -------------------
   A verifier au premier lancement : ces noms et listes de codes ne figurent pas dans les
   scripts d exploration. Tout est modifiable ici, rien n est lu dans un fichier.          */
%let v_hlc_prod = HLC_PROD;               /* classe HLC calculee par le moteur de production (1 a 9)  */
%let v_prov_prod = M_PROVS_SOLDE;         /* provision de production (utilisee a l etape 5)             */
%let v_expo_hb = M_CNTRT_DECLS_HBILAN;    /* exposition hors bilan retenue pour l EAD (a confirmer)     */
/* Valeurs de final_model (modele retenu par la production) selon la voie de traitement */
%let lst_stat  = 'CORP','IMMO','CONSO','REVOL','CI','CAV';   /* voie statistique : six arbres            */
%let lst_pge   = 'PGE';                                       /* voie forfaitaire PGE                     */
%let lst_indiv = 'INDIV','LOANIQ';                            /* voie individuelle                        */
%let lst_excl  = 'EXCL';                                      /* hors perimetre                           */
/* Detection du PGE : codes de C_NOMNC_ENGGM (A COMPLETER) */
%let lst_pge_eng = 'D24','D25';       /* PGE : C_NOMNC_ENGGM in {D24, D25} (CDC 3.1) */
/* Seuils du ratio R (tranches EdB) */
%let r_ci  = 0.35;     /* credit d investissement : R <= seuil => HLC 5 */
%let r_cav = 1.051;    /* comptes a vue : R < seuil => HLC 4            */
/* Tolerance de comparaison de deux denominateurs de R */
%let tol_den = 0.01;

/*===========================================================================
  S1. DRIVERS : valeur detenue par la production (b_) et valeur recalculee
      a partir d une autre source du dataset (a_). Sept drivers D1 a D7.
      Chaque affectation est un choix de mapping a valider (cf. Q14, Q47, Q49).
===========================================================================*/
data b0;
  set base;
  length route_prod $8 route_att $8 port_att $8 b_typris $4 a_typris $4 b_cat $8 a_cat $8 edx_c $8 seg_c $8 cav_c $8 motif $28;
  hlc_prod = &v_hlc_prod.;
  ecl_prod = &v_prov_prod.;
  ead = w + max(0, coalesce(&v_expo_hb., 0));
  /* voie de traitement de la production, deduite du modele retenu */
  if portefeuille in (&lst_stat.)  then route_prod = 'STAT';
  else if portefeuille in (&lst_pge.)   then route_prod = 'PGE';
  else if portefeuille in (&lst_indiv.) then route_prod = 'INDIV';
  else if portefeuille in (&lst_excl.)  then route_prod = 'EXCLU';
  else route_prod = 'NONAFF';
  /* D1 duree en defaut : annees entamees (production) contre annees recalculees depuis la date de defaut */
  b_tid = TIME_IN_DEF1;                   a_tid = tid_recalc;
  /* D2 statut EDX / CER : niveau de risque (1 = EDX, 2 = CER) contre statut derive EDX_CER */
  b_typris = NIV_RSK_C;
  edx_c = upcase(strip(vvalue(EDX_CER)));
  if edx_c in ('EDX', '1') then a_typris = '1'; else if edx_c in ('CER', '2') then a_typris = '2'; else a_typris = '';
  /* D3 write-off : indicateur de passage en perte contre indicateur secondaire FLAG_WO.
     Q14 ouverte : le write-off partiel n est pas defini, il est traite ici comme le write-off. */
  b_wo_any = (B_CNTRT_WRITE_OFF = 'O');   a_wo_any = (strip(vvalue(FLAG_WO)) in ('1', 'O', 'Y'));
  b_wo_part = b_wo_any;                   a_wo_part = a_wo_any;
  /* D4 garantie : indicateur contre presence d un identifiant et d une origine de garantie */
  b_gar = (Flag_GARAN = '1');             a_gar = (not missing(I_GAR) and not missing(C_GAR_ORIG));
  /* D5 Credit Logement : indicateur contre origine de la garantie (codes a completer) */
  b_crelog = (Flag_Crelog = '1');         a_crelog = (upcase(strip(C_GAR_ORIG)) in ('CL', 'CREDIT LOGEMENT', 'CREDITLOGEMENT'));
  /* D6 ratio R : exposition bilan / declassement ; denominateur source f (production) contre source e */
  r_bil = M_BIL_ENCOURS_PERIODE;
  b_den = sum(M_CNTRT_DECLS_BILAN, M_CNTRT_DECLS_HBILAN);
  a_den = sum(M_ENCOURS_DECLSMT, M_CNTRT_DECLS_HBILAN);
  /* D7 categorie des comptes a vue : CAV_Type (production) contre segment declare SEG_DECL */
  cav_c = upcase(strip(vvalue(CAV_Type)));  seg_c = upcase(strip(vvalue(SEG_DECL)));
  if cav_c in ('PRIVE', 'PART', 'P') then b_cat = 'PRIVE'; else if cav_c in ('PRO', 'PRO_VIDE') then b_cat = 'PRO'; else b_cat = 'INCONNU';
  if seg_c in ('PART', 'PRIVE') then a_cat = 'PRIVE'; else if seg_c = 'PRO' then a_cat = 'PRO'; else a_cat = 'INCONNU';
  /* ecarts de driver : 1 si la valeur recalculee existe et differe de celle de la production */
  e_d1 = (not missing(a_tid) and a_tid ne b_tid);
  e_d2 = (a_typris ne '' and a_typris ne b_typris);
  e_d3 = (a_wo_any ne b_wo_any);
  e_d4 = (a_gar ne b_gar);
  e_d5 = (a_crelog ne b_crelog);
  e_d6 = (not missing(a_den) and not missing(b_den) and abs(a_den - b_den) > &tol_den. * max(abs(b_den), 1));
  e_d7 = (a_cat ne 'INCONNU' and a_cat ne b_cat);
  c_d1 = (not missing(a_tid)); c_d2 = (a_typris ne ''); c_d3 = 1; c_d4 = 1; c_d5 = 1;
  c_d6 = (not missing(a_den) and not missing(b_den)); c_d7 = (a_cat ne 'INCONNU');
run;

/*===========================================================================
  S2. ROUTAGE ATTENDU : exclusions (amont) -> Loan IQ -> seuil 1 MEUR au RP ->
      PGE -> Corporate / Retail -> sous-portefeuille. Comparaison a la voie de
      production (final_model). Un seul ordre, le premier test vrai l emporte.
===========================================================================*/
/* S2.1 Encours au declassement agrege par RP (identifiant personne) : seuil d individualisation */
proc sql;
  create table rp_decl as
  select arrete, ID_RMPM, sum(M_CNTRT_DECLS_BILAN, 0) + sum(M_CNTRT_DECLS_HBILAN, 0) as decl_rp,
         max(C_CNTRT_TYPE in (&lst_loaniq.)) as rp_loaniq          /* propagation Loan IQ au RP (CDC) */
  from b0 where not missing(ID_RMPM) group by arrete, ID_RMPM;
  /* Repli : RP absent -> seuil estime au niveau du dossier BCR (I_DOSS_UNIQ), avec indicateur de repli */
  create table doss_decl as
  select arrete, I_DOSS_UNIQ, sum(M_CNTRT_DECLS_BILAN, 0) + sum(M_CNTRT_DECLS_HBILAN, 0) as decl_doss
  from b0 where not missing(I_DOSS_UNIQ) group by arrete, I_DOSS_UNIQ;
quit;
/* S2.2 Mapping produit -> modele observe : modele dominant de chaque type de contrat Retail
   (sur les deux arretes) ; un contrat Retail dont le modele differe du modele dominant de son type est suspect. */
proc sql;
  create table map_obs0 as
  select C_CNTRT_TYPE, portefeuille, count(*) as n
  from b0 where B_TIERS_RETAIL = &val_retail. and portefeuille in (&lst_stat.) and not missing(C_CNTRT_TYPE)
  group by C_CNTRT_TYPE, portefeuille order by C_CNTRT_TYPE, n desc;
quit;
data map_obs;                          /* modele dominant par type de contrat */
  set map_obs0; by C_CNTRT_TYPE;
  if first.C_CNTRT_TYPE;
  rename portefeuille = modele_dominant n = n_dominant;
run;
/* Mapping explicite eventuel : a remplir pour imposer un modele a un type de contrat (laisser vide sinon) */
data map_exp;
  infile datalines dlm='|' truncover;
  length C_CNTRT_TYPE $8 modele_impose $8;
  input C_CNTRT_TYPE $ modele_impose $;
datalines;
;
run;
proc sql;
  create table b1 as
  select b.*, r.decl_rp, r.rp_loaniq, d.decl_doss, o.modele_dominant, e.modele_impose
  from b0 as b
       left join rp_decl as r on b.arrete = r.arrete and b.ID_RMPM = r.ID_RMPM
       left join doss_decl as d on b.arrete = d.arrete and b.I_DOSS_UNIQ = d.I_DOSS_UNIQ
       left join map_obs as o on b.C_CNTRT_TYPE = o.C_CNTRT_TYPE
       left join map_exp as e on b.C_CNTRT_TYPE = e.C_CNTRT_TYPE;
quit;
data b2;
  set b1;
  length classe_pop $12;
  decl_eff = decl_rp;                                       /* seuil au RP ; repli sur le dossier BCR si RP absent */
  repli_dossier = (missing(decl_rp) and not missing(decl_doss));
  if repli_dossier then decl_eff = decl_doss;
  /* voie attendue */
  if portefeuille in (&lst_excl.) then route_att = 'EXCLU';                               /* exclusions appliquees en amont */
  else if C_CNTRT_TYPE in (&lst_loaniq.) or rp_loaniq = 1 then route_att = 'LOANIQ';
  else if decl_eff > &seuil_indiv. then route_att = 'SEUIL';
  else if C_NOMNC_ENGGM in (&lst_pge_eng.) then route_att = 'PGE';
  else route_att = 'STAT';
  /* sous-portefeuille attendu (voie STAT) */
  port_att = '';
  if route_att = 'STAT' then do;
    if B_TIERS_RETAIL = &val_corp. then port_att = 'CORP';
    else if B_TIERS_RETAIL = &val_retail. then port_att = coalescec(modele_impose, modele_dominant);
  end;
run;

/*===========================================================================
  S3. ARBRES HLC : HLC attendue avec les six arbres de l EdB (48 classes).
      On empile un scenario par contrat : BASE (drivers de la production),
      D1 a D7 (un seul driver remplace par sa valeur recalculee, seulement s il
      differe) et ALL (tous les drivers recalcules = scenario corrige).
      L arbre est ecrit une seule fois ; les arbres Corporate, Conso, Revolving,
      CI et CAV sont "Draft" dans l EdB (drapeaux Q20 Q22 Q23 Q47 Q49).
===========================================================================*/
data tree_in;
  set b2(where=(route_att = 'STAT' and port_att in (&lst_stat.)));
  length scen $4 t_modele $8 t_typris $4 t_cat $8;
  /* scenario BASE */
  scen = 'BASE'; t_modele = port_att; t_tid = b_tid; t_typris = b_typris; t_wo_any = b_wo_any; t_wo_part = b_wo_part; t_gar = b_gar; t_crelog = b_crelog; t_cat = b_cat; r_den = b_den; output;
  if e_d1 then do; scen = 'D1'; t_modele = port_att; t_tid = b_tid; t_typris = b_typris; t_wo_any = b_wo_any; t_wo_part = b_wo_part; t_gar = b_gar; t_crelog = b_crelog; t_cat = b_cat; r_den = b_den; t_tid = a_tid; output; end;
  if e_d2 then do; scen = 'D2'; t_modele = port_att; t_tid = b_tid; t_typris = b_typris; t_wo_any = b_wo_any; t_wo_part = b_wo_part; t_gar = b_gar; t_crelog = b_crelog; t_cat = b_cat; r_den = b_den; t_typris = a_typris; output; end;
  if e_d3 then do; scen = 'D3'; t_modele = port_att; t_tid = b_tid; t_typris = b_typris; t_wo_any = b_wo_any; t_wo_part = b_wo_part; t_gar = b_gar; t_crelog = b_crelog; t_cat = b_cat; r_den = b_den; t_wo_any = a_wo_any; t_wo_part = a_wo_part; output; end;
  if e_d4 then do; scen = 'D4'; t_modele = port_att; t_tid = b_tid; t_typris = b_typris; t_wo_any = b_wo_any; t_wo_part = b_wo_part; t_gar = b_gar; t_crelog = b_crelog; t_cat = b_cat; r_den = b_den; t_gar = a_gar; output; end;
  if e_d5 then do; scen = 'D5'; t_modele = port_att; t_tid = b_tid; t_typris = b_typris; t_wo_any = b_wo_any; t_wo_part = b_wo_part; t_gar = b_gar; t_crelog = b_crelog; t_cat = b_cat; r_den = b_den; t_crelog = a_crelog; output; end;
  if e_d6 then do; scen = 'D6'; t_modele = port_att; t_tid = b_tid; t_typris = b_typris; t_wo_any = b_wo_any; t_wo_part = b_wo_part; t_gar = b_gar; t_crelog = b_crelog; t_cat = b_cat; r_den = b_den; r_den = a_den; output; end;
  if e_d7 then do; scen = 'D7'; t_modele = port_att; t_tid = b_tid; t_typris = b_typris; t_wo_any = b_wo_any; t_wo_part = b_wo_part; t_gar = b_gar; t_crelog = b_crelog; t_cat = b_cat; r_den = b_den; t_cat = a_cat; output; end;
  if e_d1 or e_d2 or e_d3 or e_d4 or e_d5 or e_d6 or e_d7 then do;                       /* scenario ALL : tous les drivers recalcules */
    scen = 'ALL'; t_modele = port_att; t_tid = coalesce(a_tid, b_tid); t_typris = coalescec(a_typris, b_typris);
    t_wo_any = a_wo_any; t_wo_part = a_wo_part; t_gar = a_gar; t_crelog = a_crelog;
    t_cat = ifc(a_cat = 'INCONNU', b_cat, a_cat); r_den = coalesce(a_den, b_den); output;
  end;
  keep arrete slot cle_ano scen t_modele t_tid t_typris t_wo_any t_wo_part t_gar t_crelog t_cat r_bil r_den;
run;

data tree_out;
  set tree_in;
  length t_rcas $4 r_cas $4 t_chemin $40 t_flag $4 t_hlc 8;
  /* ratio R : quatre cas (LOW, NULL, CALC, ZERO) */
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
  /* arbre de decision */
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
  hlc = t_hlc;
  length motif $28;
  motif = '';
  if hlc = . then do;                                           /* motif d indetermination */
    if t_tid = . then motif = 'DUREE_DEFAUT_MANQUANTE';
    else if t_typris not in ('1', '2') then motif = 'NIVEAU_RISQUE_INVALIDE';
    else if t_chemin = 'INDETERMINE_CAT' then motif = 'CATEGORIE_CAV_INCONNUE';
    else if t_chemin = 'MODELE_INCONNU' then motif = 'MODELE_INCONNU';
    else motif = 'AUTRE';
  end;
  keep arrete slot cle_ano scen hlc t_chemin t_flag motif;
run;
proc sql;
  create table hlc_s as
  select arrete, slot, cle_ano,
         max(case when scen = 'BASE' then hlc end) as hlc_att, max(case when scen = 'BASE' then t_chemin end) as chemin length=40,
         max(case when scen = 'BASE' then t_flag end) as rule_flag length=4, max(case when scen = 'BASE' then motif end) as motif_ind length=28,
         max(case when scen = 'ALL' then hlc end) as h_all, max(scen = 'ALL') as s_all,
         max(case when scen = 'D1' then hlc end) as h_d1, max(scen = 'D1') as s_d1, max(case when scen = 'D2' then hlc end) as h_d2, max(scen = 'D2') as s_d2,
         max(case when scen = 'D3' then hlc end) as h_d3, max(scen = 'D3') as s_d3, max(case when scen = 'D4' then hlc end) as h_d4, max(scen = 'D4') as s_d4,
         max(case when scen = 'D5' then hlc end) as h_d5, max(scen = 'D5') as s_d5, max(case when scen = 'D6' then hlc end) as h_d6, max(scen = 'D6') as s_d6,
         max(case when scen = 'D7' then hlc end) as h_d7, max(scen = 'D7') as s_d7
  from tree_out group by arrete, slot, cle_ano;
  create table hlc_c0 as
  select b.*, h.hlc_att, h.chemin, h.rule_flag, h.motif_ind, h.h_all, h.s_all, h.h_d1, h.s_d1, h.h_d2, h.s_d2, h.h_d3, h.s_d3, h.h_d4, h.s_d4,
         h.h_d5, h.s_d5, h.h_d6, h.s_d6, h.h_d7, h.s_d7
  from b2 as b left join hlc_s as h on b.arrete = h.arrete and b.slot = h.slot and b.cle_ano = h.cle_ano;
quit;

/*===========================================================================
  S4. COMPARAISON : statut de concordance, cause, hors-plage, classe de population
      MATCH          HLC de production = HLC attendue
      MISMATCH       HLC differente, ou voie / modele different de celui attendu
      UNDETERMINED   HLC attendue non calculable, ou HLC absente de la production
      NOT_APPLICABLE contrat hors voie statistique, correctement sans HLC
      Cause : MAPPING (segment), DATA (un driver differe et explique l ecart),
      DECISION (aucun driver ne differe : regle a arbitrer), ABSENT_PRODUCTION, UNDETERMINED.
===========================================================================*/
data hlc_c;
  set hlc_c0;
  length match_status $16 mismatch_cause $20 hors_plage $1;
  /* HLC avec drivers recalcules : si aucun ecart, c est la HLC attendue */
  hlc_all = ifn(s_all = 1, h_all, hlc_att);
  if s_d1 ne 1 then h_d1 = hlc_att; if s_d2 ne 1 then h_d2 = hlc_att; if s_d3 ne 1 then h_d3 = hlc_att; if s_d4 ne 1 then h_d4 = hlc_att;
  if s_d5 ne 1 then h_d5 = hlc_att; if s_d6 ne 1 then h_d6 = hlc_att; if s_d7 ne 1 then h_d7 = hlc_att;
  hors_plage = '0';                                               /* HLC de production hors de l arbre de son modele */
  if not missing(hlc_prod) then do;
    if portefeuille in ('CORP', 'IMMO', 'CONSO') and not (1 <= hlc_prod <= 9) then hors_plage = '1';
    else if portefeuille = 'REVOL' and not (1 <= hlc_prod <= 7) then hors_plage = '1';
    else if portefeuille = 'CI' and not (1 <= hlc_prod <= 8) then hors_plage = '1';
    else if portefeuille = 'CAV' and not (1 <= hlc_prod <= 6) then hors_plage = '1';
  end;
  /* statut : le premier test vrai l emporte */
  if route_att ne 'STAT' then do;
    if not missing(hlc_prod) and route_att in ('LOANIQ', 'SEUIL') then do; match_status = 'MISMATCH'; mismatch_cause = 'MAPPING'; end;   /* devait etre individuel */
    else do; match_status = 'NOT_APPLICABLE'; mismatch_cause = 'NOT_APPLICABLE'; end;
  end;
  else if port_att = '' then do; match_status = 'UNDETERMINED'; mismatch_cause = 'UNDETERMINED'; end;      /* segment non attribuable */
  else if route_prod ne 'STAT' or portefeuille ne port_att then do; match_status = 'MISMATCH'; mismatch_cause = 'MAPPING'; end;
  else if missing(hlc_prod) then do; match_status = 'UNDETERMINED'; mismatch_cause = 'ABSENT_PRODUCTION'; end;
  else if missing(hlc_att) then do; match_status = 'UNDETERMINED'; mismatch_cause = 'UNDETERMINED'; end;
  else if hlc_prod = hlc_att then do; match_status = 'MATCH'; mismatch_cause = 'NONE'; end;
  else do;
    match_status = 'MISMATCH';
    if (s_d1 = 1 and h_d1 ne hlc_att) or (s_d2 = 1 and h_d2 ne hlc_att) or (s_d3 = 1 and h_d3 ne hlc_att) or (s_d4 = 1 and h_d4 ne hlc_att)
       or (s_d5 = 1 and h_d5 ne hlc_att) or (s_d6 = 1 and h_d6 ne hlc_att) or (s_d7 = 1 and h_d7 ne hlc_att) then mismatch_cause = 'DATA';
    else mismatch_cause = 'DECISION';
  end;
  /* un contrat Retail dont le modele differe du modele dominant de son type : mapping suspect */
  map_suspect = (route_prod = 'STAT' and B_TIERS_RETAIL = &val_retail. and not missing(modele_dominant) and portefeuille ne modele_dominant);
run;

/* S4.2 Classe de population entre les deux arretes (communs, entrees, sorties, changements de voie) */
proc sql;
  create table pop as
  select coalesce(a.cle_ano, b.cle_ano) as cle_ano length=72,
         a.route_att as route_m1, b.route_att as route_m,
         case when a.cle_ano is null then 'IN' when b.cle_ano is null then 'OUT'
              when a.route_att ne b.route_att then 'SCOPE_CHANGE' else 'COMMON' end as classe_pop length=12
  from (select cle_ano, route_att from hlc_c where slot = 'M1') as a
       full join (select cle_ano, route_att from hlc_c where slot = 'M0') as b on a.cle_ano = b.cle_ano;
  create table hlc_cp as
  select c.*, p.classe_pop from hlc_c as c left join pop as p on c.cle_ano = p.cle_ano;
quit;
data out.dq_hlc_base_&periode.;                              /* lue par l etape 5 */
  set hlc_cp;
  keep arrete slot role cle_ano contrat_id portefeuille port_att route_att route_prod classe_pop hlc_prod hlc_att hlc_all chemin rule_flag motif_ind
       match_status mismatch_cause hors_plage map_suspect e_d1-e_d7 c_d1-c_d7 h_d1-h_d7 s_d1-s_d7 ead w ecl_prod
       b_tid b_typris b_wo_any b_wo_part b_gar b_crelog b_cat r_bil b_den;
run;

data dim_driver;
  infile datalines dlm='|' dsd truncover;
  length DRIVER_ID $4 LIBELLE $40 SOURCE_PROD $32 SOURCE_RECALC $32 DECISION_LIEE $12;
  input DRIVER_ID $ LIBELLE $ SOURCE_PROD $ SOURCE_RECALC $ DECISION_LIEE $;
datalines;
D1|Duree en defaut (annees entamees)|TIME_IN_DEF1|Default_date et D_ARRETE|Q20
D2|Statut EDX / CER|C_CNTRT_NIV_RSK|EDX_CER|
D3|Write-off (total et partiel)|B_CNTRT_WRITE_OFF|FLAG_WO|Q14
D4|Garantie eligible|Flag_GARAN|I_GAR et C_GAR_ORIG|
D5|Garantie Credit Logement|Flag_Crelog|C_GAR_ORIG|
D6|Ratio R (denominateur)|M_CNTRT_DECLS_BILAN|M_ENCOURS_DECLSMT|Q47
D7|Categorie des comptes a vue|CAV_Type|SEG_DECL|Q49
;
run;
/*===========================================================================
  S5. TABLEAUX D EXPLORATION ETAPE 4 (HLC_1 a HLC_6) : listings SAS, M-1 et M.
===========================================================================*/
data attr_ms;                                 /* couleurs des statuts de concordance */
  length ID $8 VALUE $16 FILLCOLOR $10;
  ID = 'ms';
  VALUE = 'MATCH';          FILLCOLOR = 'cx1BAF7A'; output;
  VALUE = 'MISMATCH';       FILLCOLOR = 'cxE34948'; output;
  VALUE = 'UNDETERMINED';   FILLCOLOR = 'cxEDA100'; output;
  VALUE = 'NOT_APPLICABLE'; FILLCOLOR = 'cxA0A7B8'; output;
run;
ods graphics on / width=900px height=480px;

title1 j=l "^S={background=cx0A2F3F color=white fontsize=15pt fontweight=bold} HLC_1 - Concordance HLC de production / HLC attendue";
title3 j=l "^S={color=cxC00000 fontweight=bold} &banner.";

proc sql;
  create table h1 as
  select role, portefeuille, count(*) as n_contrats, sum(match_status = 'MATCH') as n_match, sum(match_status = 'MISMATCH') as n_mismatch,
         sum(match_status = 'UNDETERMINED') as n_undet, sum(match_status = 'NOT_APPLICABLE') as n_na,
         sum(case when match_status = 'MATCH' then 1 else 0 end) / max(1, sum(match_status in ('MATCH', 'MISMATCH', 'UNDETERMINED'))) as taux_concordance format=percent8.2,
         sum(case when match_status = 'MISMATCH' then ead else 0 end) as ead_mismatch format=comma18.,
         sum(ead) as ead format=comma18.
  from out.dq_hlc_base_&periode. where portefeuille in (&lst_stat.) or route_att = 'STAT' group by role, portefeuille order by portefeuille, role desc;
  select max(case when role = 'M' then taux_concordance end), max(case when role = 'M-1' then taux_concordance end)
    into :c_m trimmed, :c_m1 trimmed from (select role, sum(n_match) / max(1, sum(n_match + n_mismatch + n_undet)) as taux_concordance from h1 group by role);
quit;
%let c_m = %sysfunc(putn(&c_m., percent8.2));  %let c_m1 = %sysfunc(putn(&c_m1., percent8.2));
%let c_h1 = Concordance globale : &c_m1. a M-1 et &c_m. a M. Un MISMATCH de cause MAPPING releve du routage, DATA d un driver different, DECISION d une regle d arbre a arbitrer.;
ods text = "^S={background=cxE1EFF5} &c_h1.";

proc report data=h1 nowd;
  column portefeuille role n_contrats n_match n_mismatch n_undet n_na taux_concordance ead_mismatch ead;
  define portefeuille / group 'Portefeuille'; define role / display 'Arrete';
  define n_contrats / 'Contrats' format=comma12.; define n_match / 'MATCH' format=comma12.; define n_mismatch / 'MISMATCH' format=comma12.;
  define n_undet / 'UNDETERMINED' format=comma12.; define n_na / 'NON APPLICABLE' format=comma12.;
  define taux_concordance / 'Taux de concordance'; define ead_mismatch / 'EAD en ecart'; define ead / 'EAD';
  compute taux_concordance;
    if taux_concordance < &o_seuil. then call define(_col_, 'style', 'style=[background=cxF4CCCC]');
    else if taux_concordance < &v_seuil. then call define(_col_, 'style', 'style=[background=cxFCE5CD]');
    else call define(_col_, 'style', 'style=[background=cxD9EAD3]');
  endcomp;
run;
proc sql;
  create table h1g as select role, portefeuille, match_status, count(*) as n from out.dq_hlc_base_&periode.
  where route_att = 'STAT' group by role, portefeuille, match_status;
quit;
title1 j=l "G1. Statuts de concordance par portefeuille, M-1 contre M";
proc sgpanel data=h1g dattrmap=attr_ms;
  panelby role / columns=2 novarname;
  vbar portefeuille / response=n group=match_status groupdisplay=stack attrid=ms;
  rowaxis grid label="Contrats"; colaxis display=(nolabel);
run;
title1;

title1 j=l "^S={background=cx0A2F3F color=white fontsize=15pt fontweight=bold} HLC_2 - Routage attendu / voie de production et segment";
title3 j=l "^S={color=cxC00000 fontweight=bold} &banner.";

proc sql;
  create table h2 as
  select role, route_att as voie_attendue, route_prod as voie_production, count(*) as n_contrats, sum(ead) as ead format=comma18.
  from out.dq_hlc_base_&periode. group by role, route_att, route_prod order by voie_attendue, voie_production, role desc;
  create table h2b as
  select role, portefeuille as modele_production, port_att as modele_attendu, count(*) as n_contrats, sum(ead) as ead format=comma18.
  from out.dq_hlc_base_&periode. where route_att = 'STAT' and portefeuille ne port_att group by role, portefeuille, port_att order by n_contrats desc;
  create table h2c as
  select role, count(*) as n_retail_stat, sum(map_suspect) as n_suspect, sum(map_suspect) / count(*) as part_suspect format=percent8.2
  from out.dq_hlc_base_&periode. where route_prod = 'STAT' and portefeuille ne 'CORP' group by role;
  select count(*) into :n_h2 trimmed from out.dq_hlc_base_&periode. where slot = 'M0' and route_att ne 'STAT' and route_att ne route_prod and route_att in ('LOANIQ', 'SEUIL') and not missing(hlc_prod);
quit;
%let c_h2 = &n_h2. contrat(s) de l arrete M devraient suivre la voie individuelle (Loan IQ ou seuil de &seuil_indiv. EUR) alors qu ils portent une HLC statistique. Les ecarts de modele (deuxieme tableau) et les contrats Retail hors du modele dominant de leur type (troisieme tableau) signalent un mapping produit a confirmer.;
ods text = "^S={background=cxE1EFF5} &c_h2.";

title1 j=l "Voie attendue contre voie de production";
proc report data=h2 nowd; column voie_attendue voie_production role n_contrats ead;
  define voie_attendue / group; define voie_production / group; define role / display 'Arrete'; define n_contrats / format=comma12. 'Contrats'; define ead / 'EAD'; run;
title1 j=l "Modele de production different du modele attendu";
proc report data=h2b nowd; column role modele_production modele_attendu n_contrats ead; run;
title1 j=l "Contrats Retail hors du modele dominant de leur type de contrat";
proc report data=h2c nowd; column role n_retail_stat n_suspect part_suspect; run;
title1 j=l "G2. Voie attendue contre voie de production (contrats)";
proc sgpanel data=h2 dattrmap=attr_role;
  panelby role / columns=2 novarname;
  hbar voie_attendue / response=n_contrats group=voie_production groupdisplay=stack;
  rowaxis display=(nolabel); colaxis grid label="Contrats";
run;
title1;

title1 j=l "^S={background=cx0A2F3F color=white fontsize=15pt fontweight=bold} HLC_3 - Branches des arbres : effectifs, EAD et concordance";
title3 j=l "^S={color=cxC00000 fontweight=bold} &banner.";

proc sql;
  create table h3 as
  select portefeuille, chemin, role, coalesce(rule_flag, '') as regle_non_approuvee length=4, count(*) as n_contrats, sum(ead) as ead format=comma18.,
         sum(match_status = 'MATCH') / max(1, sum(match_status in ('MATCH', 'MISMATCH'))) as taux_concordance format=percent8.2
  from out.dq_hlc_base_&periode. where route_att = 'STAT' and port_att in (&lst_stat.) group by portefeuille, chemin, role, rule_flag
  order by portefeuille, chemin, role desc;
  select count(distinct chemin) into :n_h3 trimmed from h3;
  select count(*) into :n_h3f trimmed from out.dq_hlc_base_&periode. where slot = 'M0' and rule_flag ne '';
quit;
%let c_h3 = &n_h3. branche(s) distincte(s) suivie(s). &n_h3f. contrat(s) de l arrete M passent par une regle de l EdB non approuvee (drapeaux Q20, Q22, Q23, Q47, Q49) : leur HLC attendue est provisoire.;
ods text = "^S={background=cxE1EFF5} &c_h3.";

proc report data=h3 nowd;
  column portefeuille chemin regle_non_approuvee role n_contrats ead taux_concordance;
  define portefeuille / group; define chemin / group 'Branche'; define regle_non_approuvee / group 'Regle non approuvee';
  define role / display 'Arrete'; define n_contrats / format=comma12. 'Contrats'; define ead / 'EAD'; define taux_concordance / 'Concordance';
  compute taux_concordance;
    if not missing(taux_concordance) and taux_concordance < &o_seuil. then call define(_col_, 'style', 'style=[background=cxF4CCCC]');
  endcomp;
run;
title1 j=l "G3. Contrats par branche, M-1 contre M";
proc sgplot data=h3 dattrmap=attr_role;
  hbar chemin / response=n_contrats group=role groupdisplay=cluster attrid=role categoryorder=respdesc;
  xaxis grid label="Contrats"; yaxis display=(nolabel) fitpolicy=none;
run;
title1;

title1 j=l "^S={background=cx0A2F3F color=white fontsize=15pt fontweight=bold} HLC_4 - Contrats a HLC indeterminee ou absente";
title3 j=l "^S={color=cxC00000 fontweight=bold} &banner.";

proc sql;
  create table h4 as
  select role, mismatch_cause as cause, coalesce(motif_ind, '') as motif length=28, count(*) as n_contrats, sum(ead) as ead format=comma18.
  from out.dq_hlc_base_&periode. where match_status = 'UNDETERMINED' group by role, mismatch_cause, motif_ind order by n_contrats desc;
  select coalesce(sum(n_contrats), 0) into :n_h4 trimmed from h4 where role = 'M';
quit;
%let c_h4 = &n_h4. contrat(s) indetermine(s) a l arrete M. ABSENT_PRODUCTION : la production n a pas de HLC. UNDETERMINED : la HLC attendue ne peut pas etre calculee (duree, niveau de risque, categorie CAV ou segment manquants). Aucune valeur par defaut n est imposee.;
ods text = "^S={background=cxE1EFF5} &c_h4.";

proc report data=h4 nowd; column role cause motif n_contrats ead; run;
title1 j=l "G4. Contrats indetermines par cause et motif";
proc sgplot data=h4 dattrmap=attr_role;
  hbar motif / response=n_contrats group=role groupdisplay=cluster attrid=role categoryorder=respdesc;
  xaxis grid label="Contrats"; yaxis display=(nolabel);
run;
title1;

title1 j=l "^S={background=cx0A2F3F color=white fontsize=15pt fontweight=bold} HLC_5 - Ecarts par driver D1 a D7 et effet sur la HLC";
title3 j=l "^S={color=cxC00000 fontweight=bold} &banner.";

data h5_long;
  set out.dq_hlc_base_&periode.(where=(route_att = 'STAT'));
  array e{7} e_d1-e_d7; array c{7} c_d1-c_d7; array s{7} s_d1-s_d7; array h{7} h_d1-h_d7;
  do k = 1 to 7;
    driver_id = cats('D', k);
    if c{k} = 1 then do;
      comparable = 1; ecart = (e{k} = 1); change_hlc = (s{k} = 1 and not missing(h{k}) and not missing(hlc_att) and h{k} ne hlc_att);
      output;
    end;
  end;
  keep role portefeuille driver_id comparable ecart change_hlc ead;
run;
proc sql;
  create table h5 as
  select l.role, l.driver_id, d.libelle, count(*) as n_comparables, sum(ecart) as n_ecarts, sum(ecart) / count(*) as taux_ecart format=percent8.2,
         sum(change_hlc) as n_change_hlc, sum(case when change_hlc = 1 then ead else 0 end) as ead_change format=comma18.
  from h5_long as l left join dim_driver as d on l.driver_id = d.driver_id
  group by l.role, l.driver_id, d.libelle order by l.driver_id, l.role desc;
quit;
%let c_h5 = Un driver est en ecart quand la valeur de la production differe de la valeur recalculee depuis une autre source du dataset. Seuls les ecarts qui changent la HLC (colonne Changent la HLC) ont un effet sur la provision, mesure a l etape 5.;
ods text = "^S={background=cxE1EFF5} &c_h5.";

proc report data=h5 nowd;
  column driver_id libelle role n_comparables n_ecarts taux_ecart n_change_hlc ead_change;
  define driver_id / group 'Driver'; define libelle / group 'Libelle'; define role / display 'Arrete';
  define n_comparables / format=comma12. 'Comparables'; define n_ecarts / format=comma12. 'Ecarts'; define taux_ecart / 'Taux d ecart';
  define n_change_hlc / format=comma12. 'Changent la HLC'; define ead_change / 'EAD concernee';
  compute taux_ecart;
    if taux_ecart > 0.05 then call define(_col_, 'style', 'style=[background=cxF4CCCC]');
    else if taux_ecart > 0.01 then call define(_col_, 'style', 'style=[background=cxFCE5CD]');
  endcomp;
run;
title1 j=l "G5. Taux d ecart par driver, M-1 contre M";
proc sgplot data=h5 dattrmap=attr_role;
  vbar driver_id / response=taux_ecart group=role groupdisplay=cluster attrid=role;
  yaxis grid label="Taux d ecart" valuesformat=percent8.1; xaxis display=(nolabel);
run;
title1;

title1 j=l "^S={background=cx0A2F3F color=white fontsize=15pt fontweight=bold} HLC_6 - Transitions de HLC entre M-1 et M (contrats communs)";
title3 j=l "^S={color=cxC00000 fontweight=bold} &banner.";

proc sql;
  create table h6a as
  select a.portefeuille, a.hlc_att as hlc_m1, b.hlc_att as hlc_m, count(*) as n_contrats, sum(b.ead) as ead format=comma18.
  from (select * from out.dq_hlc_base_&periode. where slot = 'M1' and classe_pop = 'COMMON') as a
       inner join (select * from out.dq_hlc_base_&periode. where slot = 'M0' and classe_pop = 'COMMON') as b on a.cle_ano = b.cle_ano
  where a.hlc_att is not null and b.hlc_att is not null group by a.portefeuille, a.hlc_att, b.hlc_att;
  create table h6b as
  select a.portefeuille, count(*) as n_communs,
         sum(a.hlc_prod ne b.hlc_prod) as n_chg_prod, sum(a.hlc_att ne b.hlc_att) as n_chg_att,
         sum(a.hlc_prod ne b.hlc_prod) / count(*) as part_chg_prod format=percent8.2, sum(a.hlc_att ne b.hlc_att) / count(*) as part_chg_att format=percent8.2
  from (select * from out.dq_hlc_base_&periode. where slot = 'M1' and classe_pop = 'COMMON') as a
       inner join (select * from out.dq_hlc_base_&periode. where slot = 'M0' and classe_pop = 'COMMON') as b on a.cle_ano = b.cle_ano
  group by a.portefeuille;
quit;
%let c_h6 = Seuls les contrats presents aux deux arretes avec une HLC attendue calculable sont suivis. Une transition de la HLC de production sans changement de la HLC attendue (ou l inverse) signale une instabilite des drivers entre les deux arretes.;
ods text = "^S={background=cxE1EFF5} &c_h6.";

title1 j=l "Part de contrats dont la HLC change entre M-1 et M";
proc report data=h6b nowd; column portefeuille n_communs n_chg_prod part_chg_prod n_chg_att part_chg_att; run;
title1 j=l "G6. Matrice de transition de la HLC attendue (contrats communs, tous portefeuilles)";
proc sql;
  create table h6c as select hlc_m1, hlc_m, sum(n_contrats) as n from h6a group by hlc_m1, hlc_m;
quit;
proc sgplot data=h6c;
  heatmapparm x=hlc_m y=hlc_m1 colorresponse=n / colormodel=(cxFFFFFF cx9DB7D9 cx0E5A78) outline;
  text x=hlc_m y=hlc_m1 text=n / textattrs=(size=8) strip;
  xaxis type=discrete label="HLC a M"; yaxis type=discrete reverse label="HLC a M-1";
run;
title1;
ods graphics off;
title; footnote;

/*===========================================================================
  S6. PUBLICATION POWER BI : tables PUB_* (un onglet chacune), avec les noms de colonnes
      des maquettes. N_PERD = arrete (202608 ou 202609).
===========================================================================*/
/* PUB_SCOPE : population par voie attendue et classe de population */
proc sql;
  create table pub.PUB_SCOPE as
  select arrete as N_PERD, route_att as ROUTE length=8, classe_pop as CLASSE_POP length=12, portefeuille as PORTEFEUILLE length=8,
         count(*) as N_CONTRATS, sum(ead) as EAD
  from out.dq_hlc_base_&periode. group by arrete, route_att, classe_pop, portefeuille;
  /* PUB_HLC_COMPARE : croisement HLC production x HLC attendue */
  create table pub.PUB_HLC_COMPARE as
  select arrete as N_PERD, portefeuille as PORTEFEUILLE length=8, hlc_prod as HLC_PROD, hlc_att as HLC_ATT, match_status as MATCH_STATUS length=16,
         mismatch_cause as MISMATCH_CAUSE length=20, count(*) as N_CONTRATS, sum(ead) as EAD
  from out.dq_hlc_base_&periode. group by arrete, portefeuille, hlc_prod, hlc_att, match_status, mismatch_cause;
  /* PUB_HLC_BRANCH : effectifs par branche d arbre */
  create table pub.PUB_HLC_BRANCH as
  select arrete as N_PERD, portefeuille as PORTEFEUILLE length=8, chemin as BRANCHE length=40, coalesce(rule_flag, '') as REGLE_NON_APPROUVEE length=4,
         count(*) as N_CONTRATS, sum(ead) as EAD, sum(match_status = 'MATCH') as N_MATCH, sum(match_status = 'MISMATCH') as N_MISMATCH
  from out.dq_hlc_base_&periode. where route_att = 'STAT' group by arrete, portefeuille, chemin, rule_flag;
  /* PUB_HLC_INDETERMINE : causes d indetermination */
  create table pub.PUB_HLC_INDETERMINE as
  select arrete as N_PERD, portefeuille as PORTEFEUILLE length=8, mismatch_cause as CAUSE length=20, coalesce(motif_ind, '') as MOTIF length=28,
         count(*) as N_CONTRATS, sum(ead) as EAD
  from out.dq_hlc_base_&periode. where match_status = 'UNDETERMINED' group by arrete, portefeuille, mismatch_cause, motif_ind;
  /* PUB_DRIVER_ECART : taux d ecart par driver (alimente la visualisation des drivers) */
  create table pub.PUB_DRIVER_ECART as
  select (case when role = 'M' then &periode. else &periode1. end) as N_PERD, driver_id as DRIVER_ID length=4,
         count(*) as N_COMPARABLES, sum(ecart) as N_ECARTS, sum(change_hlc) as N_CHANGE_HLC, sum(case when change_hlc = 1 then ead else 0 end) as EAD_CHANGE_HLC
  from h5_long group by role, driver_id;
  /* PUB_HLC_STABILITE : part de contrats dont la HLC change entre M-1 et M (production, attendue) */
  create table pub.PUB_HLC_STABILITE as
  select &periode. as N_PERD, portefeuille as PORTEFEUILLE length=8, n_communs as N_COMMUNS, n_chg_prod as N_CHG_PROD, n_chg_att as N_CHG_ATT from h6b;
  /* PUB_HLC_TRANSITION : matrice M-1 vers M de la HLC attendue */
  create table pub.PUB_HLC_TRANSITION as
  select &periode. as N_PERD, portefeuille as PORTEFEUILLE length=8, hlc_m1 as HLC_M1, hlc_m as HLC_M, n_contrats as N_CONTRATS, ead as EAD from h6a;
quit;
/* PUB_HLC_DRIVER_DIFF : contrats dont la HLC attendue change quand un seul driver est recalcule.
   HLC_PROD = HLC de l arbre avec les drivers de la production ; HLC_RECALC = HLC avec le driver recalcule.
   ECL_ECART reste vide ici : l etape 5 republie cette table avec la valeur. */
data drv0;
  set out.dq_hlc_base_&periode.(where=(route_att = 'STAT'));
  array s{7} s_d1-s_d7; array h{7} h_d1-h_d7;
  length DRIVER_ID $4;
  do k = 1 to 7;
    if s{k} = 1 and not missing(h{k}) and not missing(hlc_att) and h{k} ne hlc_att then do; DRIVER_ID = cats('D', k); HLC_RECALC = h{k}; output; end;
  end;
  keep arrete portefeuille DRIVER_ID hlc_att HLC_RECALC ead;
run;
proc sql;
  create table pub.PUB_HLC_DRIVER_DIFF as
  select arrete as N_PERD, portefeuille as PORTEFEUILLE length=8, DRIVER_ID, hlc_att as HLC_PROD, HLC_RECALC, count(*) as N_CONTRATS, sum(ead) as EAD, . as ECL_ECART
  from drv0 group by arrete, portefeuille, DRIVER_ID, hlc_att, HLC_RECALC;
quit;
/* PUB_E4_SYNTHESE : synthese de la vague 2 (page d ouverture HLC), par arrete et portefeuille.
   Concordance HLC, indetermines, ecarts de routage (voie de production differente de la voie attendue). */
proc sql;
  create table pub.PUB_E4_SYNTHESE as
  select arrete as N_PERD, portefeuille as PORTEFEUILLE length=8,
         count(*) as N_CONTRATS, sum(ead) as EAD,
         sum(match_status = 'MATCH') as N_MATCH, sum(match_status = 'MISMATCH') as N_MISMATCH,
         sum(match_status = 'UNDETERMINED') as N_INDETERMINE, sum(match_status = 'NOT_APPLICABLE') as N_NON_APPLICABLE,
         sum(case when match_status = 'MISMATCH' then ead else 0 end) as EAD_MISMATCH,
         sum(case when match_status = 'UNDETERMINED' then ead else 0 end) as EAD_INDETERMINE,
         sum(route_prod ne route_att and route_att ne 'EXCLU') as N_ECART_ROUTAGE,
         sum(case when route_prod ne route_att and route_att ne 'EXCLU' then ead else 0 end) as EAD_ECART_ROUTAGE
  from out.dq_hlc_base_&periode. group by arrete, portefeuille;
  /* PUB_HLC_STOCK : stock par HLC de production, par arrete et portefeuille (voie statistique) */
  create table pub.PUB_HLC_STOCK as
  select arrete as N_PERD, portefeuille as PORTEFEUILLE length=8, hlc_prod as HLC_PROD, count(*) as N_CONTRATS, sum(ead) as EAD
  from out.dq_hlc_base_&periode. where route_att = 'STAT' group by arrete, portefeuille, hlc_prod;
quit;
data pub.DIM_DRIVER; set dim_driver; run;
data pub.DIM_ROUTE;
  infile datalines dlm='|' dsd truncover;
  length ROUTE $8 LIBELLE $50 ORDRE 8;
  input ROUTE $ LIBELLE $ ORDRE;
datalines;
EXCLU|Exclu en amont|1
LOANIQ|Individuel Loan IQ|2
SEUIL|Individuel au dessus du seuil|3
PGE|Forfait PGE|4
STAT|Voie statistique (arbres HLC)|5
;
run;
data pub.DIM_MATCH;
  infile datalines dlm='|' dsd truncover;
  length MATCH_STATUS $16 LIBELLE $40 ORDRE 8 COULEUR $8;
  input MATCH_STATUS $ LIBELLE $ ORDRE COULEUR $;
datalines;
MATCH|Concordant|1|#2E9E6F
MISMATCH|Ecart|2|#D64545
UNDETERMINED|Indetermine|3|#E8A33D
NOT_APPLICABLE|Non applicable|4|#A0A7B8
;
run;

/*===========================================================================
  S7. CLASSEUR EXCEL POWER BI DU PROGRAMME 02 : un seul fichier, un onglet par table
      Fichier : &dir_out./PBI_02_Routage_HLC_&periode..xlsx
===========================================================================*/
data lst_b;
  infile datalines dlm='|' truncover;
  length TABLE_NAME $32 VAGUE 8;
  input TABLE_NAME $ VAGUE;
datalines;
PUB_E4_SYNTHESE|2
PUB_HLC_STOCK|2
PUB_SCOPE|2
PUB_HLC_COMPARE|2
PUB_HLC_BRANCH|2
PUB_HLC_INDETERMINE|2
PUB_DRIVER_ECART|2
PUB_HLC_TRANSITION|2
PUB_HLC_STABILITE|2
DIM_DRIVER|0
DIM_ROUTE|0
DIM_MATCH|0
;
run;
proc sql noprint;
  select count(*) into :n_mm trimmed from out.dq_hlc_base_&periode. where slot = 'M0' and match_status = 'MISMATCH';
  create table pub.PUB_MANIFEST_E4 as
  select &periode. as N_PERD, "&run_id." as PUBLICATION_ID length=24, l.TABLE_NAME length=32, l.VAGUE,
         'V3.0' as SCHEMA_VERSION length=8, t.nobs as N_LIGNES, t.nvar as N_COLONNES, 'Y' as REFRESH_READY length=1
  from lst_b as l left join dictionary.tables as t on t.libname = 'PUB' and upcase(t.memname) = upcase(l.TABLE_NAME)
  order by VAGUE desc, TABLE_NAME;
quit;
data pub.PUB_RUN_LOG_E4;
  length N_PERD 8 RUN_ID $24 STEP_ID $24 DUREE_S 8 STATUT $8 MESSAGE $200;
  N_PERD = &periode.; RUN_ID = "&run_id."; STEP_ID = 'ETAPE_4'; DUREE_S = round(datetime() - &t_start., 1); STATUT = 'OK';
  MESSAGE = catx(' ', put(&n_mm., 8. -L), 'contrat(s) en ecart de HLC a M');
run;
data lst_x; set lst_b end=fin; output; if fin then do; TABLE_NAME = 'PUB_MANIFEST_E4'; VAGUE = 0; output; TABLE_NAME = 'PUB_RUN_LOG_E4'; output; end; run;
filename xlout "&dir_out./PBI_02_Routage_HLC_&periode..xlsx";
data _null_; rc = fdelete('xlout'); run;
filename xlout clear;
libname xl xlsx "&dir_out./PBI_02_Routage_HLC_&periode..xlsx";
data _null_;
  set lst_x;
  call execute(cats('data xl.', TABLE_NAME, '; set pub.', TABLE_NAME, '; run;'));
run;
libname xl clear;
/* FIN : lire le classeur, arbitrer les causes DECISION avec la Validation des modeles,
   puis lancer le programme 03 (moteur financier). */
