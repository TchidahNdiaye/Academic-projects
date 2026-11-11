// Requêtes Power Query — coller chaque bloc dans une requête vide (éditeur avancé)
// p_Dossier = "C:\LGD\sorties\" ; p_Periode = "202609"

// ---- fnTable
(Classeur as text, Onglet as text) as table =>
let Src = Excel.Workbook(File.Contents(p_Dossier & Classeur & p_Periode & ".xlsx"), true, true),
    T = Src{[Item=Onglet, Kind="Sheet"]}[Data]
in T

// ---- PUB_RUN_SUMMARY
let
    Src = fnTable("PBI_01_Qualite_", "PUB_RUN_SUMMARY"),
    Typ = Table.TransformColumnTypes(Src, {{"N_PERD", type number}, {"PORTEFEUILLE", type text}, {"RUN_ID", type text}, {"GATE_ENTREE", type text}, {"N_CONTRATS", type number}, {"EAD_TOTAL", type number}, {"N_CONTROLES_PREVUS", type number}, {"N_CONTROLES_EXEC", type number}, {"N_CONTRATS_CRITIQUES", type number}, {"EAD_CRITIQUE", type number}, {"REFRESH_READY", type text}})
in Typ

// ---- PUB_CDE_SCORE
let
    Src = fnTable("PBI_01_Qualite_", "PUB_CDE_SCORE"),
    Typ = Table.TransformColumnTypes(Src, {{"N_PERD", type number}, {"PORTEFEUILLE", type text}, {"CDE_ID", type text}, {"N_APPLICABLE", type number}, {"N_CONFORME", type number}, {"EAD_APPLICABLE", type number}, {"EAD_NON_CONFORME", type number}})
in Typ

// ---- PUB_DIM_SCORE
let
    Src = fnTable("PBI_01_Qualite_", "PUB_DIM_SCORE"),
    Typ = Table.TransformColumnTypes(Src, {{"N_PERD", type number}, {"PORTEFEUILLE", type text}, {"CDE_ID", type text}, {"DIMENSION", type text}, {"N_APPLICABLE", type number}, {"N_CONFORME", type number}, {"EAD_APPLICABLE", type number}, {"EAD_NON_CONFORME", type number}})
in Typ

// ---- PUB_RULE_RESULT
let
    Src = fnTable("PBI_01_Qualite_", "PUB_RULE_RESULT"),
    Typ = Table.TransformColumnTypes(Src, {{"N_PERD", type number}, {"PORTEFEUILLE", type text}, {"RULE_ID", type text}, {"CDE_ID", type text}, {"SEVERITE", type text}, {"N_CONTRATS", type number}, {"N_TESTE", type number}, {"N_ECHEC", type number}, {"N_INDETERMINE", type number}, {"N_NON_APPLICABLE", type number}, {"N_NON_TESTE", type number}, {"EAD_ECHEC", type number}})
in Typ

// ---- PUB_ISSUE_AGG
let
    Src = fnTable("PBI_01_Qualite_", "PUB_ISSUE_AGG"),
    Typ = Table.TransformColumnTypes(Src, {{"N_PERD", type number}, {"PORTEFEUILLE", type text}, {"CAUSE_RACINE", type text}, {"STATUT", type text}, {"SEVERITE", type text}, {"N_ANOMALIES", type number}, {"EAD", type number}})
in Typ

// ---- PUB_PROFIL_VAR
let
    Src = fnTable("PBI_01_Qualite_", "PUB_PROFIL_VAR"),
    Typ = Table.TransformColumnTypes(Src, {{"N_PERD", type number}, {"VARIABLE", type text}, {"TYPE", type text}, {"USAGE", type text}, {"N_LIGNES", type number}, {"N_MANQUANT", type number}, {"TAUX_RENSEIGNE", type number}})
in Typ

// ---- PUB_VOLUMETRIE
let
    Src = fnTable("PBI_01_Qualite_", "PUB_VOLUMETRIE"),
    Typ = Table.TransformColumnTypes(Src, {{"N_PERD", type number}, {"N_LIGNES", type number}, {"N_CONTRATS", type number}, {"N_DOSSIERS", type number}, {"N_CLE_INCOMPLETE", type number}, {"N_CLES_DOUBLON", type number}, {"ENCOURS_BILAN", type number}})
in Typ

// ---- PUB_S3_STOCKS
let
    Src = fnTable("PBI_01_Qualite_", "PUB_S3_STOCKS"),
    Typ = Table.TransformColumnTypes(Src, {{"N_PERD", type number}, {"ROLE", type text}, {"PERIMETRE", type text}, {"MODEL", type text}, {"NATURE_RISQUE", type text}, {"NB_CONTRATS", type number}, {"EAD_EUR", type number}, {"PROVISION_EUR", type number}, {"EAD_AVEC_LGDD", type number}, {"LGDD_X_EAD", type number}, {"NB_EAD_MANQ", type number}, {"NB_PROVISION_MANQ", type number}})
in Typ

// ---- PUB_S3_PONTS
let
    Src = fnTable("PBI_01_Qualite_", "PUB_S3_PONTS"),
    Typ = Table.TransformColumnTypes(Src, {{"SCOPE_PERIMETRE", type text}, {"SCOPE_MODELE", type text}, {"SCOPE_RISQUE", type text}, {"ORDRE_ETAPE", type number}, {"ETAPE", type text}, {"NB_CONTRATS", type number}, {"EAD_PONT_EUR", type number}, {"PROVISION_PONT_EUR", type number}, {"EAD_AVEC_LGDD", type number}, {"LGDD_X_EAD", type number}})
in Typ

// ---- PUB_S3_RECONCILIATION
let
    Src = fnTable("PBI_01_Qualite_", "PUB_S3_RECONCILIATION"),
    Typ = Table.TransformColumnTypes(Src, {{"SCOPE_PERIMETRE", type text}, {"SCOPE_MODELE", type text}, {"SCOPE_RISQUE", type text}, {"ECART_EAD_EUR", type number}, {"ECART_PROVISION_EUR", type number}})
in Typ

// ---- PUB_S3_EXCEPTIONS
let
    Src = fnTable("PBI_01_Qualite_", "PUB_S3_EXCEPTIONS"),
    Typ = Table.TransformColumnTypes(Src, {{"N_PERD", type number}, {"PERIMETRE", type text}, {"MODEL", type text}, {"NATURE_RISQUE", type text}, {"MOTIF_EXCEPTION", type text}, {"NB_LIGNES", type number}})
in Typ

// ---- PUB_S3_TID
let
    Src = fnTable("PBI_01_Qualite_", "PUB_S3_TID"),
    Typ = Table.TransformColumnTypes(Src, {{"N_PERD", type number}, {"ROLE", type text}, {"SCOPE_PERIMETRE", type text}, {"SCOPE_MODELE", type text}, {"SCOPE_RISQUE", type text}, {"ORDRE_TRANCHE", type number}, {"TRANCHE", type text}, {"NB_CONTRATS", type number}, {"EAD_EUR", type number}, {"PROVISION_EUR", type number}})
in Typ

// ---- DIM_ARRETE
let
    Src = fnTable("PBI_01_Qualite_", "DIM_ARRETE"),
    Typ = Table.TransformColumnTypes(Src, {{"N_PERD", type number}, {"AS_OF_DATE", type date}, {"ROLE", type text}, {"LIBELLE", type text}})
in Typ

// ---- DIM_PORTEFEUILLE
let
    Src = fnTable("PBI_01_Qualite_", "DIM_PORTEFEUILLE"),
    Typ = Table.TransformColumnTypes(Src, {{"PORTEFEUILLE", type text}, {"LIBELLE", type text}, {"GROUPE", type text}, {"ORDRE", type number}})
in Typ

// ---- DIM_CDE
let
    Src = fnTable("PBI_01_Qualite_", "DIM_CDE"),
    Typ = Table.TransformColumnTypes(Src, {{"CDE_ID", type text}, {"LIBELLE", type text}, {"LIBELLE_COURT", type text}, {"FAMILLE", type text}})
in Typ

// ---- DIM_REGLE
let
    Src = fnTable("PBI_01_Qualite_", "DIM_REGLE"),
    Typ = Table.TransformColumnTypes(Src, {{"RULE_ID", type text}, {"LIBELLE", type text}, {"CDE_ID", type text}, {"SEVERITE", type text}, {"DIMENSION", type text}, {"CAUSE_RACINE", type text}, {"METHODE_IMPACT", type text}, {"DRIVER_ID", type text}, {"OWNER", type text}})
in Typ

// ---- DIM_DIMENSION
let
    Src = fnTable("PBI_01_Qualite_", "DIM_DIMENSION"),
    Typ = Table.TransformColumnTypes(Src, {{"DIMENSION", type text}, {"LIBELLE", type text}, {"EXIGENCE", type text}})
in Typ

// ---- DIM_S3_PERIMETRE
let
    Src = fnTable("PBI_01_Qualite_", "DIM_S3_PERIMETRE"),
    Typ = Table.TransformColumnTypes(Src, {{"SCOPE_PERIMETRE", type text}, {"LIBELLE", type text}, {"ORDRE", type number}})
in Typ

// ---- DIM_S3_MODELE
let
    Src = fnTable("PBI_01_Qualite_", "DIM_S3_MODELE"),
    Typ = Table.TransformColumnTypes(Src, {{"SCOPE_MODELE", type text}, {"LIBELLE", type text}, {"ORDRE", type number}})
in Typ

// ---- DIM_S3_RISQUE
let
    Src = fnTable("PBI_01_Qualite_", "DIM_S3_RISQUE"),
    Typ = Table.TransformColumnTypes(Src, {{"SCOPE_RISQUE", type text}, {"LIBELLE", type text}, {"ORDRE", type number}})
in Typ

// ---- DIM_FEU
let
    Src = fnTable("PBI_01_Qualite_", "DIM_FEU"),
    Typ = Table.TransformColumnTypes(Src, {{"FEU", type text}, {"ORDRE", type number}, {"COULEUR", type text}})
in Typ

// ---- PUB_MANIFEST_E23
let
    Src = fnTable("PBI_01_Qualite_", "PUB_MANIFEST_E23"),
    Typ = Table.TransformColumnTypes(Src, {{"N_PERD", type number}, {"PUBLICATION_ID", type text}, {"TABLE_NAME", type text}, {"VAGUE", type number}, {"SCHEMA_VERSION", type text}, {"N_LIGNES", type number}, {"N_COLONNES", type number}, {"REFRESH_READY", type text}})
in Typ

// ---- PUB_RUN_LOG_E23
let
    Src = fnTable("PBI_01_Qualite_", "PUB_RUN_LOG_E23"),
    Typ = Table.TransformColumnTypes(Src, {{"N_PERD", type number}, {"RUN_ID", type text}, {"STEP_ID", type text}, {"DUREE_S", type number}, {"STATUT", type text}, {"MESSAGE", type text}})
in Typ

// ---- PUB_E4_SYNTHESE
let
    Src = fnTable("PBI_02_Routage_HLC_", "PUB_E4_SYNTHESE"),
    Typ = Table.TransformColumnTypes(Src, {{"N_PERD", type number}, {"PORTEFEUILLE", type text}, {"N_CONTRATS", type number}, {"EAD", type number}, {"N_MATCH", type number}, {"N_MISMATCH", type number}, {"N_INDETERMINE", type number}, {"N_NON_APPLICABLE", type number}, {"EAD_MISMATCH", type number}, {"EAD_INDETERMINE", type number}, {"N_ECART_ROUTAGE", type number}, {"EAD_ECART_ROUTAGE", type number}})
in Typ

// ---- PUB_HLC_STOCK
let
    Src = fnTable("PBI_02_Routage_HLC_", "PUB_HLC_STOCK"),
    Typ = Table.TransformColumnTypes(Src, {{"N_PERD", type number}, {"PORTEFEUILLE", type text}, {"HLC_PROD", type text}, {"N_CONTRATS", type number}, {"EAD", type number}})
in Typ

// ---- PUB_SCOPE
let
    Src = fnTable("PBI_02_Routage_HLC_", "PUB_SCOPE"),
    Typ = Table.TransformColumnTypes(Src, {{"N_PERD", type number}, {"ROUTE", type text}, {"CLASSE_POP", type text}, {"PORTEFEUILLE", type text}, {"N_CONTRATS", type number}, {"EAD", type number}})
in Typ

// ---- PUB_HLC_COMPARE
let
    Src = fnTable("PBI_02_Routage_HLC_", "PUB_HLC_COMPARE"),
    Typ = Table.TransformColumnTypes(Src, {{"N_PERD", type number}, {"PORTEFEUILLE", type text}, {"HLC_PROD", type text}, {"HLC_ATT", type text}, {"MATCH_STATUS", type text}, {"MISMATCH_CAUSE", type text}, {"N_CONTRATS", type number}, {"EAD", type number}})
in Typ

// ---- PUB_HLC_BRANCH
let
    Src = fnTable("PBI_02_Routage_HLC_", "PUB_HLC_BRANCH"),
    Typ = Table.TransformColumnTypes(Src, {{"N_PERD", type number}, {"PORTEFEUILLE", type text}, {"BRANCHE", type text}, {"REGLE_NON_APPROUVEE", type text}, {"N_CONTRATS", type number}, {"EAD", type number}, {"N_MATCH", type number}, {"N_MISMATCH", type number}})
in Typ

// ---- PUB_HLC_INDETERMINE
let
    Src = fnTable("PBI_02_Routage_HLC_", "PUB_HLC_INDETERMINE"),
    Typ = Table.TransformColumnTypes(Src, {{"N_PERD", type number}, {"PORTEFEUILLE", type text}, {"CAUSE", type text}, {"MOTIF", type text}, {"N_CONTRATS", type number}, {"EAD", type number}})
in Typ

// ---- PUB_DRIVER_ECART
let
    Src = fnTable("PBI_02_Routage_HLC_", "PUB_DRIVER_ECART"),
    Typ = Table.TransformColumnTypes(Src, {{"N_PERD", type number}, {"DRIVER_ID", type text}, {"N_COMPARABLES", type number}, {"N_ECARTS", type number}, {"N_CHANGE_HLC", type number}, {"EAD_CHANGE_HLC", type number}})
in Typ

// ---- PUB_HLC_TRANSITION
let
    Src = fnTable("PBI_02_Routage_HLC_", "PUB_HLC_TRANSITION"),
    Typ = Table.TransformColumnTypes(Src, {{"N_PERD", type number}, {"PORTEFEUILLE", type text}, {"HLC_M1", type text}, {"HLC_M", type text}, {"N_CONTRATS", type number}, {"EAD", type number}})
in Typ

// ---- PUB_HLC_STABILITE
let
    Src = fnTable("PBI_02_Routage_HLC_", "PUB_HLC_STABILITE"),
    Typ = Table.TransformColumnTypes(Src, {{"N_PERD", type number}, {"PORTEFEUILLE", type text}, {"N_COMMUNS", type number}, {"N_CHG_PROD", type number}, {"N_CHG_ATT", type number}})
in Typ

// ---- DIM_DRIVER
let
    Src = fnTable("PBI_02_Routage_HLC_", "DIM_DRIVER"),
    Typ = Table.TransformColumnTypes(Src, {{"DRIVER_ID", type text}, {"LIBELLE", type text}, {"SOURCE_PROD", type text}, {"SOURCE_RECALC", type text}, {"DECISION_LIEE", type text}})
in Typ

// ---- DIM_ROUTE
let
    Src = fnTable("PBI_02_Routage_HLC_", "DIM_ROUTE"),
    Typ = Table.TransformColumnTypes(Src, {{"ROUTE", type text}, {"LIBELLE", type text}, {"ORDRE", type number}})
in Typ

// ---- DIM_MATCH
let
    Src = fnTable("PBI_02_Routage_HLC_", "DIM_MATCH"),
    Typ = Table.TransformColumnTypes(Src, {{"MATCH_STATUS", type text}, {"LIBELLE", type text}, {"ORDRE", type number}, {"COULEUR", type text}})
in Typ

// ---- PUB_MANIFEST_E4
let
    Src = fnTable("PBI_02_Routage_HLC_", "PUB_MANIFEST_E4"),
    Typ = Table.TransformColumnTypes(Src, {{"N_PERD", type number}, {"PUBLICATION_ID", type text}, {"TABLE_NAME", type text}, {"VAGUE", type number}, {"SCHEMA_VERSION", type text}, {"N_LIGNES", type number}, {"N_COLONNES", type number}, {"REFRESH_READY", type text}})
in Typ

// ---- PUB_RUN_LOG_E4
let
    Src = fnTable("PBI_02_Routage_HLC_", "PUB_RUN_LOG_E4"),
    Typ = Table.TransformColumnTypes(Src, {{"N_PERD", type number}, {"RUN_ID", type text}, {"STEP_ID", type text}, {"DUREE_S", type number}, {"STATUT", type text}, {"MESSAGE", type text}})
in Typ

// ---- PUB_E5_SYNTHESE
let
    Src = fnTable("PBI_03_Provisions_", "PUB_E5_SYNTHESE"),
    Typ = Table.TransformColumnTypes(Src, {{"N_PERD", type number}, {"PORTEFEUILLE", type text}, {"N_CONTRATS", type number}, {"N_CALCULABLES", type number}, {"EAD_BIL", type number}, {"EAD_HB", type number}, {"ECL_PROD", type number}, {"ECL_RECALC", type number}, {"ECL_CORRIGE", type number}, {"EAD_NON_CALCULABLE", type number}})
in Typ

// ---- PUB_ECL_AGG
let
    Src = fnTable("PBI_03_Provisions_", "PUB_ECL_AGG"),
    Typ = Table.TransformColumnTypes(Src, {{"N_PERD", type number}, {"PORTEFEUILLE", type text}, {"N_CONTRATS", type number}, {"EAD_BIL", type number}, {"EAD_HB", type number}, {"ECL", type number}, {"ECL_PROD", type number}, {"ECL_CORRIGE", type number}})
in Typ

// ---- PUB_ECL_EFFECT
let
    Src = fnTable("PBI_03_Provisions_", "PUB_ECL_EFFECT"),
    Typ = Table.TransformColumnTypes(Src, {{"N_PERD", type number}, {"PORTEFEUILLE", type text}, {"EFFET_CODE", type text}, {"MONTANT", type number}})
in Typ

// ---- PUB_IMPACT_AGG
let
    Src = fnTable("PBI_03_Provisions_", "PUB_IMPACT_AGG"),
    Typ = Table.TransformColumnTypes(Src, {{"N_PERD", type number}, {"PORTEFEUILLE", type text}, {"STATUT_IMPACT", type text}, {"N_ANOMALIES", type number}, {"EAD", type number}, {"IMPACT_ECL", type number}})
in Typ

// ---- PUB_IMPACT_REGLE
let
    Src = fnTable("PBI_03_Provisions_", "PUB_IMPACT_REGLE"),
    Typ = Table.TransformColumnTypes(Src, {{"N_PERD", type number}, {"RULE_ID", type text}, {"DRIVER_ID", type text}, {"STATUT_IMPACT", type text}, {"N_ANOMALIES", type number}, {"EAD", type number}, {"IMPACT_ECL", type number}})
in Typ

// ---- PUB_HLC_DRIVER_DIFF
let
    Src = fnTable("PBI_03_Provisions_", "PUB_HLC_DRIVER_DIFF"),
    Typ = Table.TransformColumnTypes(Src, {{"N_PERD", type number}, {"PORTEFEUILLE", type text}, {"DRIVER_ID", type text}, {"HLC_PROD", type text}, {"HLC_RECALC", type text}, {"N_CONTRATS", type number}, {"EAD", type number}, {"ECL_ECART", type number}})
in Typ

// ---- PUB_PARAM_AUDIT
let
    Src = fnTable("PBI_03_Provisions_", "PUB_PARAM_AUDIT"),
    Typ = Table.TransformColumnTypes(Src, {{"N_PERD", type number}, {"PARAMETER_ID", type text}, {"VALEUR", type number}, {"VALID_FROM", type date}, {"VALID_TO", type date}, {"VERSION", type text}, {"APPROBATION", type text}})
in Typ

// ---- PUB_COUVERTURE
let
    Src = fnTable("PBI_03_Provisions_", "PUB_COUVERTURE"),
    Typ = Table.TransformColumnTypes(Src, {{"N_PERD", type number}, {"ROLE", type text}, {"PORTEFEUILLE", type text}, {"N_CONTRATS", type number}, {"PART_CONTRATS_CALCULABLES", type number}, {"PART_EAD_CALCULABLE", type number}, {"N_SANS_HLC", type number}})
in Typ

// ---- PUB_RECONCILIATION
let
    Src = fnTable("PBI_03_Provisions_", "PUB_RECONCILIATION"),
    Typ = Table.TransformColumnTypes(Src, {{"N_PERD", type number}, {"ROLE", type text}, {"PORTEFEUILLE", type text}, {"N_CONTRATS", type number}, {"N_RECONCILIES", type number}, {"ECL_PROD", type number}, {"ECL_RECALC", type number}, {"ECART", type number}})
in Typ

// ---- PUB_SENSIBILITE
let
    Src = fnTable("PBI_03_Provisions_", "PUB_SENSIBILITE"),
    Typ = Table.TransformColumnTypes(Src, {{"N_PERD", type number}, {"ROLE", type text}, {"PORTEFEUILLE", type text}, {"ECL_RECALC", type number}, {"DELTA_HLC_PLUS1", type number}, {"DELTA_HLC_MOINS1", type number}})
in Typ

// ---- PUB_PARAM_MANQUANT
let
    Src = fnTable("PBI_03_Provisions_", "PUB_PARAM_MANQUANT"),
    Typ = Table.TransformColumnTypes(Src, {{"N_PERD", type number}, {"PORTEFEUILLE", type text}, {"HLC", type text}, {"N_CONTRATS", type number}, {"EAD", type number}})
in Typ

// ---- PUB_DECISION
let
    Src = fnTable("PBI_03_Provisions_", "PUB_DECISION"),
    Typ = Table.TransformColumnTypes(Src, {{"N_PERD", type number}, {"DECISION_ID", type text}, {"SUJET", type text}, {"PROPRIETAIRE", type text}, {"STATUT", type text}, {"BLOQUANT", type text}, {"ECHEANCE", type date}})
in Typ

// ---- PUB_CERTIFICATION
let
    Src = fnTable("PBI_03_Provisions_", "PUB_CERTIFICATION"),
    Typ = Table.TransformColumnTypes(Src, {{"N_PERD", type number}, {"STATUT", type text}, {"MOTIF_ID", type text}, {"MOTIF", type text}})
in Typ

// ---- DIM_EFFET
let
    Src = fnTable("PBI_03_Provisions_", "DIM_EFFET"),
    Typ = Table.TransformColumnTypes(Src, {{"EFFET_CODE", type text}, {"LIBELLE", type text}, {"ORDRE", type number}, {"GROUPE", type text}})
in Typ

// ---- DIM_MVT
let
    Src = fnTable("PBI_03_Provisions_", "DIM_MVT"),
    Typ = Table.TransformColumnTypes(Src, {{"CODE", type text}, {"LIBELLE", type text}, {"ORDRE", type number}})
in Typ

// ---- PUB_MANIFEST_E5
let
    Src = fnTable("PBI_03_Provisions_", "PUB_MANIFEST_E5"),
    Typ = Table.TransformColumnTypes(Src, {{"N_PERD", type number}, {"PUBLICATION_ID", type text}, {"TABLE_NAME", type text}, {"VAGUE", type number}, {"SCHEMA_VERSION", type text}, {"N_LIGNES", type number}, {"N_COLONNES", type number}, {"REFRESH_READY", type text}})
in Typ

// ---- PUB_RUN_LOG_E5
let
    Src = fnTable("PBI_03_Provisions_", "PUB_RUN_LOG_E5"),
    Typ = Table.TransformColumnTypes(Src, {{"N_PERD", type number}, {"RUN_ID", type text}, {"STEP_ID", type text}, {"DUREE_S", type number}, {"STATUT", type text}, {"MESSAGE", type text}})
in Typ

// ---- PUB_RUN_LOG
Table.Combine({PUB_RUN_LOG_E23, PUB_RUN_LOG_E4, PUB_RUN_LOG_E5})

// ---- PUB_MANIFEST
Table.Combine({PUB_MANIFEST_E23, PUB_MANIFEST_E4, PUB_MANIFEST_E5})
