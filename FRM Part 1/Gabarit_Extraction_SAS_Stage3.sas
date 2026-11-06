/*=====================================================================
  GABARIT D'EXTRACTION STAGE 3 IFRS 9 - M ET M-1
  Objectif : produire les tables compactes alimentant une page Power BI.

  A VALIDER AVANT EXECUTION :
  - BASE : table SAS source contenant les deux arretes.
  - I_DOSS_UNIQ : candidat de cle facilite (unicite/stabilite a tester).
  - TYPE_RISQUE : remplacer par le champ officiel Douteux/Compromis.
  - Predicat Stage 3 : reprendre le filtre officiel du moteur metier.
  - Unite EAD/provision et signe de PROV_BRUTE.
  - Echelle de M_LGDD (0-1 ou 0-100) et ponderation autorisee.

  Ne pas inclure MODEL ou TYPE_RISQUE dans la cle de rapprochement.
=====================================================================*/

%let SRC    = BASE;
%let AR_M   = 202609;
%let AR_M1  = 202608;

/* 1. SNAPSHOTS NORMALISES : une ligne par cle facilite et par arrete. */
data work.STG3_M;
  set &SRC.;
  where arrete=&AR_M.
    /* and <PREDICAT_STAGE3_OFFICIEL> */;
  if missing(I_DOSS_UNIQ) then delete;
  length ID_FACILITE $100 MODEL $20 TYPE_RISQUE_STD $20 ROLE $3;
  ID_FACILITE = strip(I_DOSS_UNIQ);       /* candidat a valider */
  MODEL = strip(final_model);
  if missing(MODEL) then MODEL='Non affecte';
  TYPE_RISQUE_STD = upcase(strip(TYPE_RISQUE)); /* champ a remplacer */
  ARRETE_STD = arrete;
  ROLE = 'M';
  EAD_EUR = expo_dwr;
  PROVISION_EUR = PROV_BRUTE;             /* normaliser le signe si necessaire */
  LGDD = M_LGDD;
  HLC = final_HLC;
  keep ARRETE_STD ROLE ID_FACILITE MODEL TYPE_RISQUE_STD
       EAD_EUR PROVISION_EUR LGDD HLC I_EXT_KAC;
run;

data work.STG3_M1;
  set &SRC.;
  where arrete=&AR_M1.
    /* and <PREDICAT_STAGE3_OFFICIEL> */;
  if missing(I_DOSS_UNIQ) then delete;
  length ID_FACILITE $100 MODEL $20 TYPE_RISQUE_STD $20 ROLE $3;
  ID_FACILITE = strip(I_DOSS_UNIQ);
  MODEL = strip(final_model);
  if missing(MODEL) then MODEL='Non affecte';
  TYPE_RISQUE_STD = upcase(strip(TYPE_RISQUE));
  ARRETE_STD = arrete;
  ROLE = 'M-1';
  EAD_EUR = expo_dwr;
  PROVISION_EUR = PROV_BRUTE;
  LGDD = M_LGDD;
  HLC = final_HLC;
  keep ARRETE_STD ROLE ID_FACILITE MODEL TYPE_RISQUE_STD
       EAD_EUR PROVISION_EUR LGDD HLC I_EXT_KAC;
run;

/* 2. CONTROLE D'UNICITE. STOPPER LE TRAITEMENT SI DOUBLONS NON EXPLIQUES. */
proc sql;
  create table work.DOUBLONS_M as
  select ID_FACILITE, count(*) as N
  from work.STG3_M
  group by ID_FACILITE
  having calculated N > 1;

  create table work.DOUBLONS_M1 as
  select ID_FACILITE, count(*) as N
  from work.STG3_M1
  group by ID_FACILITE
  having calculated N > 1;
quit;

/* Examiner DOUBLONS_M et DOUBLONS_M1 avant de poursuivre.
   Ne pas supprimer les doublons avec NODUPKEY sans comprendre leur origine. */

/* 3. RAPPROCHEMENT M/M-1 SUR LA CLE FACILITE STABLE. */
proc sql;
  create table work.STG3_BRIDGE as
  select
      coalescec(M.ID_FACILITE, M1.ID_FACILITE) as ID_FACILITE length=100,
      case when not missing(M.ID_FACILITE) then 1 else 0 end as PRESENT_M,
      case when not missing(M1.ID_FACILITE) then 1 else 0 end as PRESENT_M1,
      M.MODEL as MODEL_M, M1.MODEL as MODEL_M1,
      M.TYPE_RISQUE_STD as TYPE_RISQUE_M,
      M1.TYPE_RISQUE_STD as TYPE_RISQUE_M1,
      M.EAD_EUR as EAD_M, M1.EAD_EUR as EAD_M1,
      M.PROVISION_EUR as PROV_M, M1.PROVISION_EUR as PROV_M1,
      M.LGDD as LGDD_M, M1.LGDD as LGDD_M1,
      M.HLC as HLC_M, M1.HLC as HLC_M1
  from work.STG3_M as M
  full join work.STG3_M1 as M1
    on M.ID_FACILITE=M1.ID_FACILITE;
quit;

data work.STG3_BRIDGE;
  set work.STG3_BRIDGE;
  length STATUT_PRESENCE $8;
  if PRESENT_M=1 and PRESENT_M1=1 then STATUT_PRESENCE='COMMUN';
  else if PRESENT_M=1 then STATUT_PRESENCE='ENTREE';
  else if PRESENT_M1=1 then STATUT_PRESENCE='SORTIE';

  if STATUT_PRESENCE='COMMUN' and not missing(EAD_M)
     and not missing(EAD_M1) then DELTA_EAD_COMMUN=EAD_M-EAD_M1;
  else DELTA_EAD_COMMUN=.;

  if STATUT_PRESENCE='COMMUN' and not missing(PROV_M)
     and not missing(PROV_M1) then DELTA_PROV_COMMUN=PROV_M-PROV_M1;
  else DELTA_PROV_COMMUN=.;

  FLAG_CHANGEMENT_MODELE = (PRESENT_M=1 and PRESENT_M1=1 and MODEL_M ne MODEL_M1);
  FLAG_CHANGEMENT_RISQUE = (PRESENT_M=1 and PRESENT_M1=1 and
                             TYPE_RISQUE_M ne TYPE_RISQUE_M1);
run;

/* 4. ETENDRE LE BRIDGE EN TROIS PERIMETRES : TOUT / DOUTEUX / COMPROMIS.
   Une migration de classe est une entree/sortie du sous-perimetre, mais reste
   une facilite commune au Stage 3 global. */
data work.STG3_SCOPE_LONG;
  set work.STG3_BRIDGE;
  length SCOPE_RISQUE $12 STATUT_SCOPE $8;
  array scopes[3] $12 _temporary_ ('TOUT','DOUTEUX','COMPROMIS');
  do _i=1 to dim(scopes);
    SCOPE_RISQUE=scopes[_i];
    if SCOPE_RISQUE='TOUT' then do;
      PRESENT_SCOPE_M=PRESENT_M;
      PRESENT_SCOPE_M1=PRESENT_M1;
    end;
    else do;
      PRESENT_SCOPE_M=(PRESENT_M=1 and TYPE_RISQUE_M=SCOPE_RISQUE);
      PRESENT_SCOPE_M1=(PRESENT_M1=1 and TYPE_RISQUE_M1=SCOPE_RISQUE);
    end;
    if PRESENT_SCOPE_M=1 and PRESENT_SCOPE_M1=1 then STATUT_SCOPE='COMMUN';
    else if PRESENT_SCOPE_M=1 then STATUT_SCOPE='ENTREE';
    else if PRESENT_SCOPE_M1=1 then STATUT_SCOPE='SORTIE';
    if PRESENT_SCOPE_M=1 or PRESENT_SCOPE_M1=1 then output;
  end;
  drop _i;
run;

/* 5. TABLE DU PONT : 3 perimetres x 5 etapes. Montants en EUR.
   Les montants manquants sont exclus des sommes, mais comptabilises ;
   le pont ne doit pas etre certifie si NB_EAD_MANQ / NB_PROV_MANQ > 0. */
proc sql;
  create table work.STG3_PONT as
  select SCOPE_RISQUE, 1 as ORDRE, 'Stock M-1' as ETAPE length=32,
    sum(PRESENT_SCOPE_M1) as NB_CONTRATS,
    sum(case when PRESENT_SCOPE_M1=1 and not missing(EAD_M1) then EAD_M1 else 0 end) as EAD_PONT_EUR,
    sum(case when PRESENT_SCOPE_M1=1 and not missing(PROV_M1) then PROV_M1 else 0 end) as PROV_PONT_EUR,
    sum(case when PRESENT_SCOPE_M1=1 and missing(EAD_M1) then 1 else 0 end) as NB_EAD_MANQ,
    sum(case when PRESENT_SCOPE_M1=1 and missing(PROV_M1) then 1 else 0 end) as NB_PROV_MANQ
  from work.STG3_SCOPE_LONG group by SCOPE_RISQUE

  union all
  select SCOPE_RISQUE, 2, 'Sorties',
    sum(case when STATUT_SCOPE='SORTIE' then 1 else 0 end),
    -sum(case when STATUT_SCOPE='SORTIE' and not missing(EAD_M1) then EAD_M1 else 0 end),
    -sum(case when STATUT_SCOPE='SORTIE' and not missing(PROV_M1) then PROV_M1 else 0 end),
    sum(case when STATUT_SCOPE='SORTIE' and missing(EAD_M1) then 1 else 0 end),
    sum(case when STATUT_SCOPE='SORTIE' and missing(PROV_M1) then 1 else 0 end)
  from work.STG3_SCOPE_LONG group by SCOPE_RISQUE

  union all
  select SCOPE_RISQUE, 3, 'Entrees',
    sum(case when STATUT_SCOPE='ENTREE' then 1 else 0 end),
    sum(case when STATUT_SCOPE='ENTREE' and not missing(EAD_M) then EAD_M else 0 end),
    sum(case when STATUT_SCOPE='ENTREE' and not missing(PROV_M) then PROV_M else 0 end),
    sum(case when STATUT_SCOPE='ENTREE' and missing(EAD_M) then 1 else 0 end),
    sum(case when STATUT_SCOPE='ENTREE' and missing(PROV_M) then 1 else 0 end)
  from work.STG3_SCOPE_LONG group by SCOPE_RISQUE

  union all
  select SCOPE_RISQUE, 4, 'Variation des communs',
    sum(case when STATUT_SCOPE='COMMUN' then 1 else 0 end),
    sum(case when STATUT_SCOPE='COMMUN' and not missing(EAD_M) and not missing(EAD_M1)
             then EAD_M-EAD_M1 else 0 end),
    sum(case when STATUT_SCOPE='COMMUN' and not missing(PROV_M) and not missing(PROV_M1)
             then PROV_M-PROV_M1 else 0 end),
    sum(case when STATUT_SCOPE='COMMUN' and (missing(EAD_M) or missing(EAD_M1)) then 1 else 0 end),
    sum(case when STATUT_SCOPE='COMMUN' and (missing(PROV_M) or missing(PROV_M1)) then 1 else 0 end)
  from work.STG3_SCOPE_LONG group by SCOPE_RISQUE

  union all
  select SCOPE_RISQUE, 5, 'Stock M',
    sum(PRESENT_SCOPE_M),
    sum(case when PRESENT_SCOPE_M=1 and not missing(EAD_M) then EAD_M else 0 end),
    sum(case when PRESENT_SCOPE_M=1 and not missing(PROV_M) then PROV_M else 0 end),
    sum(case when PRESENT_SCOPE_M=1 and missing(EAD_M) then 1 else 0 end),
    sum(case when PRESENT_SCOPE_M=1 and missing(PROV_M) then 1 else 0 end)
  from work.STG3_SCOPE_LONG group by SCOPE_RISQUE;
quit;

/* 6. TABLE AGREGEE PAR ARRETE / PERIMETRE / MODELE.
   La table source doit contenir les deux snapshots normalises. */
data work.STG3_SNAPSHOT_LONG;
  set work.STG3_M work.STG3_M1;
  length SCOPE_RISQUE $12;
  SCOPE_RISQUE='TOUT'; output;
  if TYPE_RISQUE_STD='DOUTEUX' then do;
    SCOPE_RISQUE='DOUTEUX'; output;
  end;
  else if TYPE_RISQUE_STD='COMPROMIS' then do;
    SCOPE_RISQUE='COMPROMIS'; output;
  end;
run;

proc sql;
  create table work.STG3_AGG_MODELE as
  select ARRETE_STD as ARRETE, ROLE, SCOPE_RISQUE, MODEL,
    count(distinct ID_FACILITE) as NB_CONTRATS,
    sum(EAD_EUR) as EAD_EUR,
    sum(PROVISION_EUR) as PROVISION_EUR,
    sum(case when not missing(LGDD) and not missing(EAD_EUR) and EAD_EUR>0
             then LGDD*EAD_EUR else 0 end) as LGDD_NUM,
    sum(case when not missing(LGDD) and not missing(EAD_EUR) and EAD_EUR>0
             then EAD_EUR else 0 end) as LGDD_DEN_EUR,
    sum(case when missing(EAD_EUR) then 1 else 0 end) as NB_EAD_MANQ,
    sum(case when missing(PROVISION_EUR) then 1 else 0 end) as NB_PROV_MANQ,
    sum(case when missing(LGDD) then 1 else 0 end) as NB_LGDD_MANQ
  from work.STG3_SNAPSHOT_LONG
  group by ARRETE_STD, ROLE, SCOPE_RISQUE, MODEL;
quit;

/* 7. TRANSITIONS MODELE / RISQUE POUR EXPLIQUER LES MIGRATIONS. */
proc sql;
  create table work.STG3_TRANSITIONS as
  select TYPE_RISQUE_M1, TYPE_RISQUE_M, MODEL_M1, MODEL_M,
    count(*) as NB_CONTRATS,
    sum(EAD_M1) as EAD_M1_EUR,
    sum(EAD_M) as EAD_M_EUR,
    sum(EAD_M-EAD_M1) as DELTA_EAD_EUR,
    sum(PROV_M1) as PROV_M1_EUR,
    sum(PROV_M) as PROV_M_EUR,
    sum(PROV_M-PROV_M1) as DELTA_PROV_EUR
  from work.STG3_BRIDGE
  where PRESENT_M=1 and PRESENT_M1=1
  group by TYPE_RISQUE_M1, TYPE_RISQUE_M, MODEL_M1, MODEL_M;
quit;

/* 8. RECONCILIATION A REALISER AVANT EXPORT :
   - stocks M et M-1 entre source et STG3_AGG_MODELE ;
   - Stock M-1 - Sorties + Entrees + Variation communs = Stock M ;
   - faire les controles sur EAD et provision pour les 3 perimetres ;
   - verifier NB_EAD_MANQ et NB_PROV_MANQ avant de certifier le pont.

   Exports conseilles : STG3_AGG_MODELE, STG3_PONT, STG3_TRANSITIONS,
   ainsi qu'une table STG3_DQ_CONTROLES construite avec le dictionnaire DQ.
   Garder STG3_BRIDGE en CSV/zone SAS pour audit, pas dans le classeur Excel
   de restitution si le volume est superieur a la limite d'une feuille.
*/
