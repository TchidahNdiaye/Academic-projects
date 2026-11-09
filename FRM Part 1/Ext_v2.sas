/*==========================================================================
  SORTIES :
    F_STAGE3_STOCKS : ARRETE x MODEL x NATURE_RISQUE_STD
    F_STAGE3_PONTS  : SCOPE_MODELE x SCOPE_RISQUE x ETAPE
    F_STAGE3_EXCEPTIONS_AGG : table d'audit facultative, non necessaire au PBI

  CLE DE RAPPROCHEMENT : CONTRACT_ID
  NATURE DU RISQUE : DOUTEUX / COMPROMIS / NON_RENSEIGNE (vide)
  MODELES : CAV, CCO, CDI, COR, CRN, IMO

  CHAMPS SOURCE identifie(s) dans les captures / confirmes :
    arrete, contract_id, final_model, nature_risque, expo_dwr,
    PROV_BRUTE, M_LGDD, TYPROV

  A VALIDER :
    - BASE contient les deux arretes et le perimetre Stage 3 attendu.
    - TYPROV='S' definit bien le perimetre statistique. Sinon modifier le filtre.
    - expo_dwr et PROV_BRUTE sont en EUR et deja en valeurs absolues.
    - contract_id est unique par arrete. Le programme bloque si doublons.

  OPTIMISATION 700k+ LIGNES :
    - Filtrage des deux arretes en amont.
    - CONTROLE de la cle avant rapprochement.
    - PROC SORT + DATA step MERGE sur contract_id.
    - Cube de transitions agrege par ancien/nouveau modele et ancien/nouveau
      risque avant de calculer les 28 scopes : evite de multiplier 700k lignes
      par 28 dans une table intermediaire.
==========================================================================*/


/* 1. Identification des anomalies (comptes F sans P) */
PROC SQL;
    CREATE TABLE work.anomalies_fusions AS
    SELECT 
        a.id_contrat,
        a.id_doss,
        a.cmpt_fusion,
        'Fusion rompue (P absent)' AS statut_anomalie
    FROM 
        work.ma_table_base a
    WHERE 
        a.cmpt_fusion = 'F'               /* On cible uniquement les comptes F */
        AND a.id_doss IS NOT NULL         /* On s'assure d'avoir un dossier */
        AND NOT EXISTS (                  /* Et on vérifie qu'il n'y a pas de P dans ce dossier */
            SELECT 1 
            FROM work.ma_table_base b 
            WHERE b.id_doss = a.id_doss 
              AND b.cmpt_fusion = 'P'
        );
QUIT;


/* PARAMETRES */
%let SRC         = BASE;
%let AR_M1       = 202608;
%let AR_M        = 202609;
%let FILTRE_STAT = %str(TYPROV = 'S'); /* mettre 1=1 si perimetre deja filtre */
%let MODELES     = %str('CAV','CCO','CDI','COR','CRN','IMO');
%let OUTFILE     = %str(C:\DATA\STG3_Extraction_&AR_M..xlsx);

%macro build_stage3;

  /* 1. Normalisation + separation des exceptions. */
  data work._S3_POP work._S3_EXCEPTIONS;
    set &SRC.;
    where arrete in (&AR_M1., &AR_M.) and (&FILTRE_STAT.);

    length MODEL_STD $20 NATURE_RISQUE_STD $30 MOTIF_EXCEPTION $120 ROLE $3;

    if arrete=&AR_M. then ROLE='M';
    else ROLE='M-1';

    MODEL_STD = upcase(strip(final_model));
    if missing(MODEL_STD) then MODEL_STD = 'NON_AFFECTE';
    if MODEL_STD not in (&MODELES.) then MODEL_STD = 'HORS_PERIMETRE';

    if missing(strip(nature_risque)) then NATURE_RISQUE_STD = 'NON_RENSEIGNE';
    else do;
      select (upcase(strip(nature_risque)));
        when ('DOUTEUX')   NATURE_RISQUE_STD = 'DOUTEUX';
        when ('COMPROMIS') NATURE_RISQUE_STD = 'COMPROMIS';
        otherwise          NATURE_RISQUE_STD = 'AUTRE_A_CONTROLER';
      end;
    end;

    /* Les valeurs sources EAD et provision sont deja absolues. */
    EAD_EUR       = expo_dwr;
    PROVISION_EUR = PROV_BRUTE;
    LGDD_SOURCE   = M_LGDD;

    FLAG_ID_MANQ = missing(contract_id);
    FLAG_MODEL_OK = (MODEL_STD in (&MODELES.));
    FLAG_RISK_OK  = (NATURE_RISQUE_STD in ('DOUTEUX','COMPROMIS','NON_RENSEIGNE'));

    MOTIF_EXCEPTION = '';
    if FLAG_ID_MANQ then MOTIF_EXCEPTION = catx('|', MOTIF_EXCEPTION, 'CONTRACT_ID_MANQUANT');
    if not FLAG_MODEL_OK then MOTIF_EXCEPTION = catx('|', MOTIF_EXCEPTION, 'MODELE_HORS_PERIMETRE');
    if not FLAG_RISK_OK then MOTIF_EXCEPTION = catx('|', MOTIF_EXCEPTION, 'NATURE_RISQUE_A_CONTROLER');

    if MOTIF_EXCEPTION ne '' then output work._S3_EXCEPTIONS;
    else output work._S3_POP;

    keep arrete ROLE contract_id final_model nature_risque TYPROV
         MODEL_STD NATURE_RISQUE_STD EAD_EUR PROVISION_EUR LGDD_SOURCE
         FLAG_ID_MANQ FLAG_MODEL_OK FLAG_RISK_OK MOTIF_EXCEPTION;
  run;

  /* 2. Unicite de CONTRACT_ID par arrete. Ne pas dedoublonner a l'aveugle. */
  proc sql;
    create table work._DUPLICATS as
    select arrete, contract_id, count(*) as N_OCC
    from work._S3_POP
    group by arrete, contract_id
    having calculated N_OCC > 1;
    select count(*) into :N_DUP trimmed from work._DUPLICATS;
  quit;

  %if %sysevalf(&N_DUP. > 0) %then %do;
    %put ERROR: &N_DUP. couples ARRETE/CONTRACT_ID ne sont pas uniques.;
    %put ERROR: Examiner WORK._DUPLICATS. Le rapprochement est interrompu.;
    %return;
  %end;

  /* 3. Controle descriptif des periodes, modeles et modalites risque. */
  proc freq data=work._S3_POP;
    tables arrete*MODEL_STD arrete*NATURE_RISQUE_STD / missing list;
  run;
  proc means data=work._S3_POP n nmiss min max sum;
    class arrete;
    var EAD_EUR PROVISION_EUR LGDD_SOURCE;
  run;

  /* 4. F_STAGE3_STOCKS : max. 2 x 6 x 3 = 36 lignes. */
  proc sql;
    create table work.F_STAGE3_STOCKS as
    select
      arrete as ARRETE,
      ROLE,
      MODEL_STD as MODEL length=20,
      NATURE_RISQUE_STD,
      count(distinct contract_id) as NB_CONTRATS,
      sum(EAD_EUR) as EAD_EUR,
      sum(PROVISION_EUR) as PROVISION_EUR,
      sum(case when missing(EAD_EUR) then 1 else 0 end) as NB_EAD_MANQ,
      sum(case when missing(PROVISION_EUR) then 1 else 0 end) as NB_PROVISION_MANQ,
      sum(case when missing(LGDD_SOURCE) then 1 else 0 end) as NB_LGDD_MANQ,
      sum(case when NATURE_RISQUE_STD='NON_RENSEIGNE' then 1 else 0 end)
        as NB_NATURE_RISQUE_NON_RENSEIGNE
    from work._S3_POP
    group by arrete, ROLE, MODEL_STD, NATURE_RISQUE_STD
    order by arrete, MODEL, NATURE_RISQUE_STD;
  quit;

  /* 5. Snapshots avec variables renommees pour un MERGE sans collision. */
  data work._S3_M;
    set work._S3_POP(where=(arrete=&AR_M.));
    MODEL_M=MODEL_STD;
    RISQUE_M=NATURE_RISQUE_STD;
    EAD_M=EAD_EUR;
    PROV_M=PROVISION_EUR;
    PRESENT_M=1;
    keep contract_id MODEL_M RISQUE_M EAD_M PROV_M PRESENT_M;
  run;

  data work._S3_M1;
    set work._S3_POP(where=(arrete=&AR_M1.));
    MODEL_M1=MODEL_STD;
    RISQUE_M1=NATURE_RISQUE_STD;
    EAD_M1=EAD_EUR;
    PROV_M1=PROVISION_EUR;
    PRESENT_M1=1;
    keep contract_id MODEL_M1 RISQUE_M1 EAD_M1 PROV_M1 PRESENT_M1;
  run;

  proc sort data=work._S3_M;  by contract_id; run;
  proc sort data=work._S3_M1; by contract_id; run;

  /* 6. FULL OUTER JOIN efficace : MERGE SAS sur CONTRACT_ID unique. */
  data work._S3_PAIR;
    merge work._S3_M(in=IN_M) work._S3_M1(in=IN_M1);
    by contract_id;
    PRESENT_M  = IN_M;
    PRESENT_M1 = IN_M1;
    if IN_M or IN_M1;
  run;

  /* 7. Cube de transitions : reduit le nombre de lignes avant expansion.
        Grain : ancien/nouveau modele x ancienne/nouvelle nature risque.
        Le volume du cube est tres inferieur au nombre de contrats. */
  proc sql;
    create table work._S3_TRANS_CUBE as
    select
      MODEL_M1, MODEL_M, RISQUE_M1, RISQUE_M,
      PRESENT_M1, PRESENT_M,
      count(*) as NB_CONTRATS_CUBE,
      sum(case when PRESENT_M1=1 then coalesce(EAD_M1,0) else 0 end) as EAD_M1_SUM,
      sum(case when PRESENT_M=1 then coalesce(EAD_M,0) else 0 end) as EAD_M_SUM,
      sum(case when PRESENT_M1=1 then coalesce(PROV_M1,0) else 0 end) as PROV_M1_SUM,
      sum(case when PRESENT_M=1 then coalesce(PROV_M,0) else 0 end) as PROV_M_SUM
    from work._S3_PAIR
    group by MODEL_M1, MODEL_M, RISQUE_M1, RISQUE_M, PRESENT_M1, PRESENT_M;
  quit;

  /* 8. Expansion des 28 scopes sur le cube agrege, pas sur les 700k contrats. */
  data work._S3_SCOPE_CUBE;
    set work._S3_TRANS_CUBE;
    length SCOPE_MODELE $20 SCOPE_RISQUE $30;
    length IN_SCOPE_M IN_SCOPE_M1 8;
    length STATUT_SCOPE $12;

    array _MODELES[7] $20 _temporary_
      ('TOUS','CAV','CCO','CDI','COR','CRN','IMO');
    array _RISQUES[4] $30 _temporary_
      ('TOUS','DOUTEUX','COMPROMIS','NON_RENSEIGNE');

    do _I=1 to dim(_MODELES);
      SCOPE_MODELE=_MODELES[_I];
      do _J=1 to dim(_RISQUES);
        SCOPE_RISQUE=_RISQUES[_J];
        IN_SCOPE_M = (PRESENT_M=1
          and (SCOPE_MODELE='TOUS' or MODEL_M=SCOPE_MODELE)
          and (SCOPE_RISQUE='TOUS' or RISQUE_M=SCOPE_RISQUE));
        IN_SCOPE_M1 = (PRESENT_M1=1
          and (SCOPE_MODELE='TOUS' or MODEL_M1=SCOPE_MODELE)
          and (SCOPE_RISQUE='TOUS' or RISQUE_M1=SCOPE_RISQUE));

        if IN_SCOPE_M=1 and IN_SCOPE_M1=1 then STATUT_SCOPE='COMMUN';
        else if IN_SCOPE_M=1 then STATUT_SCOPE='ENTREE';
        else if IN_SCOPE_M1=1 then STATUT_SCOPE='SORTIE';
        else STATUT_SCOPE='HORS_SCOPE';

        if IN_SCOPE_M=1 or IN_SCOPE_M1=1 then output;
      end;
    end;
    drop _I _J;
  run;

  /* 9. F_STAGE3_PONTS : jusqu'a 28 scopes x 5 etapes = 140 lignes.
        Les valeurs manquantes contribuent 0 au pont et sont signalees dans
        F_STAGE3_STOCKS ; ne pas certifier un pont si des montants manquent. */
  proc sql;
    create table work.F_STAGE3_PONTS as
    select SCOPE_MODELE, SCOPE_RISQUE,
      1 as ORDRE_ETAPE, 'Stock M-1' as ETAPE length=32,
      sum(case when IN_SCOPE_M1=1 then NB_CONTRATS_CUBE else 0 end) as NB_CONTRATS,
      sum(case when IN_SCOPE_M1=1 then EAD_M1_SUM else 0 end) as EAD_PONT_EUR,
      sum(case when IN_SCOPE_M1=1 then PROV_M1_SUM else 0 end) as PROVISION_PONT_EUR
    from work._S3_SCOPE_CUBE group by SCOPE_MODELE, SCOPE_RISQUE

    union all
    select SCOPE_MODELE, SCOPE_RISQUE,
      2, 'Sorties perimetre',
      sum(case when IN_SCOPE_M1=1 and IN_SCOPE_M=0 then NB_CONTRATS_CUBE else 0 end),
      -sum(case when IN_SCOPE_M1=1 and IN_SCOPE_M=0 then EAD_M1_SUM else 0 end),
      -sum(case when IN_SCOPE_M1=1 and IN_SCOPE_M=0 then PROV_M1_SUM else 0 end)
    from work._S3_SCOPE_CUBE group by SCOPE_MODELE, SCOPE_RISQUE

    union all
    select SCOPE_MODELE, SCOPE_RISQUE,
      3, 'Entrees perimetre',
      sum(case when IN_SCOPE_M=1 and IN_SCOPE_M1=0 then NB_CONTRATS_CUBE else 0 end),
      sum(case when IN_SCOPE_M=1 and IN_SCOPE_M1=0 then EAD_M_SUM else 0 end),
      sum(case when IN_SCOPE_M=1 and IN_SCOPE_M1=0 then PROV_M_SUM else 0 end)
    from work._S3_SCOPE_CUBE group by SCOPE_MODELE, SCOPE_RISQUE

    union all
    select SCOPE_MODELE, SCOPE_RISQUE,
      4, 'Variation communs',
      sum(case when IN_SCOPE_M=1 and IN_SCOPE_M1=1 then NB_CONTRATS_CUBE else 0 end),
      sum(case when IN_SCOPE_M=1 and IN_SCOPE_M1=1 then EAD_M_SUM-EAD_M1_SUM else 0 end),
      sum(case when IN_SCOPE_M=1 and IN_SCOPE_M1=1 then PROV_M_SUM-PROV_M1_SUM else 0 end)
    from work._S3_SCOPE_CUBE group by SCOPE_MODELE, SCOPE_RISQUE

    union all
    select SCOPE_MODELE, SCOPE_RISQUE,
      5, 'Stock M',
      sum(case when IN_SCOPE_M=1 then NB_CONTRATS_CUBE else 0 end),
      sum(case when IN_SCOPE_M=1 then EAD_M_SUM else 0 end),
      sum(case when IN_SCOPE_M=1 then PROV_M_SUM else 0 end)
    from work._S3_SCOPE_CUBE group by SCOPE_MODELE, SCOPE_RISQUE
    order by SCOPE_MODELE, SCOPE_RISQUE, ORDRE_ETAPE;
  quit;

  /* 10. Reconciliation : doit etre egal a 0 pour EAD et provision. */
  proc sql;
    create table work.STG3_RECONCILIATION as
    select SCOPE_MODELE, SCOPE_RISQUE,
      sum(case when ORDRE_ETAPE in (1,2,3,4) then EAD_PONT_EUR else 0 end)
        - sum(case when ORDRE_ETAPE=5 then EAD_PONT_EUR else 0 end) as ECART_EAD_EUR,
      sum(case when ORDRE_ETAPE in (1,2,3,4) then PROVISION_PONT_EUR else 0 end)
        - sum(case when ORDRE_ETAPE=5 then PROVISION_PONT_EUR else 0 end) as ECART_PROVISION_EUR
    from work.F_STAGE3_PONTS
    group by SCOPE_MODELE, SCOPE_RISQUE;
  quit;

  /* 11. Exceptions agregees : aucun CONTRACT_ID exporte. */
  proc sql;
    create table work.F_STAGE3_EXCEPTIONS_AGG as
    select arrete, MODEL_STD, NATURE_RISQUE_STD, MOTIF_EXCEPTION,
           count(*) as NB_LIGNES
    from work._S3_EXCEPTIONS
    group by arrete, MODEL_STD, NATURE_RISQUE_STD, MOTIF_EXCEPTION;
  quit;

  /* 12. Export dans un seul classeur Stage 3, avec 2 tables obligatoires. */
  libname STGOUT xlsx "&OUTFILE.";
  data STGOUT.STG3_STOCKS; set work.F_STAGE3_STOCKS; run;
  data STGOUT.STG3_PONTS; set work.F_STAGE3_PONTS; run;
  data STGOUT.STG3_EXCEPTIONS; set work.F_STAGE3_EXCEPTIONS_AGG; run;
  libname STGOUT clear;

  %put NOTE: Extraction terminee. Examiner WORK.STG3_RECONCILIATION avant publication.;
%mend build_stage3;

%build_stage3;

/* RECETTE AVANT PUBLICATION
   - ECART_EAD_EUR = 0 et ECART_PROVISION_EUR = 0 pour chaque scope.
   - Stocks M/M-1 concordants avec les totaux SAS sources.
   - NATURE_RISQUE_STD ne comporte que les trois modalites attendues.
   - Les exceptions MODELE_HORS_PERIMETRE / AUTRE_A_CONTROLER sont examinees.
   - Si EAD/provision manquantes > 0, le pont reste a controler : le calcul
     technique remplace les manquants par 0 uniquement pour la reconciliation.
   - "Sorties/Entrees perimetre" peut inclure des changements de modele ou de
     risque; ne pas les presenter comme sorties/entrees reglementaires Stage 3
     sans variable de statut Stage 3 couvrant aussi le portefeuille complet.
*/
