############################################################
# MOTEUR R ALM SOLVABILITÉ MULTI-NIVEAUX Tchidah
# ---------------------------------------------------------
# Entrées "clean" :
#   base_actifs_ALM_clean.xlsx
#   base_passifs_ALM_clean.xlsx
#   Courbes_taux_ALM_clean.xlsx
#   Hypotheses_ALM_clean.xlsx
#
# Niveaux :
#   Niveau 1 : Groupe (Zone = "Groupe")
#   Niveau 2 : Filiales (France, Europe HF, AmLat)
#   Niveau 3 : Segments IFRS17 / IFRS9
#
# Chaque couche = bloc autonome produisant un data.frame,
# et tout est agrégé dans un Excel final.
############################################################

## 0. PACKAGES ----

# install.packages(c("readxl","dplyr","tidyr","purrr","ggplot2","openxlsx"))
library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(openxlsx)

options(scipen = 999)

############################################################
# 1. IMPORT & HARMONISATION DES DONNÉES
############################################################

# 1.0 Chemins des fichiers (adapter au besoin)
path_actifs   <- "base_actifs_ALM_clean.xlsx"
path_passifs  <- "base_passifs_ALM_clean.xlsx"
path_courbes  <- "Courbes_taux_ALM_clean.xlsx"
path_hypo     <- "Hypotheses_ALM_clean.xlsx"

# 1.1 Import brut
actifs_raw   <- read_excel(path_actifs)
passifs_raw  <- read_excel(path_passifs)
courbes_raw  <- read_excel(path_courbes)
hypo_raw     <- read_excel(path_hypo, sheet = "Hypotheses_ALM")

# 1.2 Harmonisation des Zones
map_zone <- c(
  "Groupe"            = "Groupe",
  "France"            = "France",
  "Europe_hors_France"= "Europe HF",
  "Europe HF"         = "Europe HF",
  "Amerique_Latine"   = "AmLat",
  "AmeriqueLatine"    = "AmLat",
  "AmLat"             = "AmLat"
)

actifs <- actifs_raw %>%
  mutate(
    Zone = recode(Zone, !!!map_zone),
    Actif_MEUR = Actif_MEUR
  )

passifs <- passifs_raw %>%
  mutate(
    Zone = recode(Zone, !!!map_zone),
    Passif_MEUR = Passif_MEUR
  )

# 1.3 Séparation hors-bilan (on les suit mais hors bilan éco)
passifs_hb <- passifs %>%
  filter(Categorie_risque == "Hors-bilan")

passifs_econ <- passifs %>%
  filter(Categorie_risque != "Hors-bilan")

actifs_hb <- actifs %>%
  filter(!is.na(Hors_bilan_type) & Hors_bilan_type != "")

actifs_econ <- actifs %>%
  filter(is.na(Hors_bilan_type) | Hors_bilan_type == "")

# 1.4 Paramètres issus des hypothèses
# Hypotheses_ALM_clean.xlsx doit contenir par ex. :
#  - CoC_rate (pour marge de risque)
#  - SCR_target_ratio
#  - SCR_min_ratio
#  - prob_breach_target (pour ORSA dividendes)
#  - vol_MNI_rel (volatilité relative MNI)
#  - paramètres ESG (a_hw, sigma_hw, etc.)
hypo_params <- hypo_raw %>%
  pivot_wider(names_from = Parametre, values_from = Valeur)

CoC_rate          <- as.numeric(hypo_params$CoC_rate %||% 0.06)
SCR_target_ratio  <- as.numeric(hypo_params$SCR_target_ratio %||% 1.45)
SCR_min_ratio     <- as.numeric(hypo_params$SCR_min_ratio %||% 1.00)
prob_breach_target<- as.numeric(hypo_params$prob_breach_target %||% 0.05)
vol_MNI_rel       <- as.numeric(hypo_params$vol_MNI_rel %||% 0.20)

# Paramètres ESG simples (peuvent être raffinés)
a_hw    <- as.numeric(hypo_params$a_hw    %||% 0.10)
b_hw    <- as.numeric(hypo_params$b_hw    %||% 0.02)
sigma_hw<- as.numeric(hypo_params$sigma_hw%||% 0.01)

mu_eq    <- as.numeric(hypo_params$mu_eq    %||% 0.06)
sigma_eq <- as.numeric(hypo_params$sigma_eq %||% 0.20)

mu_infl  <- as.numeric(hypo_params$mu_infl  %||% 0.02)
sigma_infl<-as.numeric(hypo_params$sigma_infl%||% 0.01)

mu_spread<- as.numeric(hypo_params$mu_spread %||% 0.00)
sigma_spread<-as.numeric(hypo_params$sigma_spread %||% 0.01)

############################################################
# 2. COURBES DE TAUX & FONCTIONS DE BASE
############################################################

courbe_fr <- courbes_raw %>%
  filter(Zone == "France", Devise == "EUR") %>%
  arrange(Maturity_Years)

get_rate <- function(curve, t) {
  if (t <= 0) return(0)
  approx(curve$Maturity_Years, curve$Rate, xout = t, rule = 2)$y
}

df_disc <- function(curve, t) {
  r <- get_rate(curve, t)
  exp(-r * t)
}

pv_cf <- function(cf_tbl, curve) {
  cf_tbl %>%
    mutate(DF = sapply(t, df_disc, curve = curve),
           PV = CF_MEUR * DF) %>%
    summarise(PV_total = sum(PV)) %>%
    pull(PV_total)
}

duration_mac <- function(cf_tbl, curve) {
  tmp <- cf_tbl %>%
    mutate(
      DF = sapply(t, df_disc, curve = curve),
      PV = CF_MEUR * DF
    )
  tot <- sum(tmp$PV)
  if (tot == 0) return(0)
  sum(tmp$t * tmp$PV) / tot
}

key_rate_durations <- function(cf_tbl, curve, key_mats = c(1,5,10,20,30), bump_bp = 0.0001){
  base_pv <- pv_cf(cf_tbl, curve)
  krd <- numeric(length(key_mats))
  for (i in seq_along(key_mats)) {
    m <- key_mats[i]
    curve_bumped <- curve
    curve_bumped$Rate <- curve$Rate + ifelse(curve$Maturity_Years >= m, bump_bp, 0)
    pv_bumped <- pv_cf(cf_tbl, curve_bumped)
    krd[i] <- (pv_bumped - base_pv) / (-base_pv * bump_bp)
  }
  tibble(Maturite_cle = key_mats, KRD = krd)
}

`%||%` <- function(x,y) ifelse(is.null(x) || length(x)==0, y, x)

############################################################
# 3. BILANS ÉCONOMIQUES – MULTI-MÉTHODES
############################################################

zones_filiales <- c("France","Europe HF","AmLat")

# 3.1 Fonction de bilan "market-consistent" (Solvency II standard)
compute_bilan_mc <- function(scope_zone,
                             actifs_tbl = actifs_econ,
                             passifs_tbl = passifs_econ){
  A <- actifs_tbl %>%
    filter(Zone == scope_zone) %>%
    summarise(Actifs_MC_MEUR = sum(Actif_MEUR), .groups="drop")
  
  P <- passifs_tbl %>%
    filter(Zone == scope_zone) %>%
    summarise(Passifs_MC_MEUR = sum(Passif_MEUR), .groups="drop")
  
  crossing(A, P) %>%
    mutate(FP_MC_MEUR = Actifs_MC_MEUR - Passifs_MC_MEUR,
           Scope = scope_zone) %>%
    select(Scope, everything())
}

# 3.2 Projections (déterministes) – Actifs & Passifs
#    (on réutilisera ces flux pour EV, CF matching, etc.)

proj_actif_ligne <- function(row, horizon = 60){
  cls <- row[["Classe_actif"]]
  nom <- row[["Actif_MEUR"]]
  mat <- row[["Maturite_moyenne"]]
  if (is.na(mat) || mat <= 0) mat <- 10
  
  t_vec <- 0:horizon
  cf <- numeric(length = horizon+1)
  
  r_c <- dplyr::case_when(
    cls %in% c("Obligations","Prets","TCN") ~ 0.025,
    cls %in% c("Actions","OPCVM")           ~ 0.06,
    cls == "Immobilier"                     ~ 0.035,
    TRUE                                    ~ 0.01
  )
  
  if (cls %in% c("Obligations","Prets","TCN")) {
    mat_int <- max(1, min(horizon, round(mat)))
    for (t in t_vec[-1]) {
      if (t < mat_int) cf[t+1] <- nom * r_c
      if (t == mat_int) cf[t+1] <- cf[t+1] + nom
    }
  } else if (cls %in% c("Actions","OPCVM")) {
    div <- 0.02
    cf[2:(horizon+1)] <- nom * div
  } else if (cls == "Immobilier") {
    rent <- 0.035
    cf[2:(horizon+1)] <- nom * rent
  }
  
  tibble(
    id_actif = row[["id_actif"]],
    Zone = row[["Zone"]],
    IFRS9_categorie = row[["IFRS9_categorie"]],
    Classe_actif = cls,
    t = t_vec,
    CF_MEUR = cf
  )
}

proj_passif_ligne <- function(row, horizon = 60){
  nom <- row[["Passif_MEUR"]]
  seg <- row[["Modele_IFRS17"]]
  cat <- row[["Categorie_risque"]]
  mat <- row[["Maturite"]]
  if (is.na(mat) || mat <= 0) mat <- 15
  
  t_vec <- 0:horizon
  cf <- numeric(length = horizon+1)
  mat_int <- max(1, min(horizon, round(mat)))
  
  if (seg %in% c("VFA","BBA/VFA")) {
    ann <- 0.30 * nom / mat_int
    for (t in 1:mat_int) cf[t+1] <- cf[t+1] + ann
    cf[mat_int+1] <- cf[mat_int+1] + 0.70 * nom
  } else if (seg %in% c("BBA")) {
    ann <- nom / mat_int
    for (t in 1:mat_int) cf[t+1] <- ann
  } else if (seg %in% c("PAA","PAA/BEL")) {
    mat_paa <- min(3, mat_int)
    ann <- nom / mat_paa
    for (t in 1:mat_paa) cf[t+1] <- ann
  } else if (seg %in% c("Sinistres","Sinistres_non_vie")) {
    mat_sin <- min(5, mat_int)
    weights <- rev(1:mat_sin)
    weights <- weights / sum(weights)
    cf[2:(mat_sin+1)] <- nom * weights
  } else {
    ann <- nom / mat_int
    for (t in 1:mat_int) cf[t+1] <- ann
  }
  
  tibble(
    id_passif = row[["Produit_detaille"]],
    Zone      = row[["Zone"]],
    Modele_IFRS17 = seg,
    Categorie_risque = cat,
    t = t_vec,
    CF_MEUR = cf
  )
}

# 3.3 Projection complet pour un scope (réutilisée partout)
proj_scope <- function(scope_zone,
                       actif_tbl = actifs_econ,
                       passif_tbl = passifs_econ,
                       horizon = 60){
  
  act_scope <- actif_tbl %>% filter(Zone == scope_zone)
  pas_scope <- passif_tbl %>% filter(Zone == scope_zone)
  
  cfA <- act_scope %>%
    split(.$id_actif) %>%
    map_df(~proj_actif_ligne(.x, horizon = horizon))
  
  cfA_tot <- cfA %>%
    group_by(t) %>%
    summarise(CF_MEUR = sum(CF_MEUR), .groups = "drop")
  
  cfP <- pas_scope %>%
    split(.$Produit_detaille) %>%
    map_df(~proj_passif_ligne(.x, horizon = horizon))
  
  cfP_tot <- cfP %>%
    group_by(t) %>%
    summarise(CF_MEUR = sum(CF_MEUR), .groups = "drop")
  
  NPV_A <- pv_cf(cfA_tot, courbe_fr)
  BEL0  <- pv_cf(cfP_tot, courbe_fr)
  
  mni_tbl <- cfA_tot %>%
    rename(CF_A = CF_MEUR) %>%
    full_join(cfP_tot %>% rename(CF_P = CF_MEUR),
              by = "t") %>%
    mutate(across(c(CF_A,CF_P), ~replace_na(.,0)),
           MNI_t = CF_A - CF_P,
           DF_t  = sapply(t, df_disc, curve = courbe_fr),
           PV_MNI_t = MNI_t * DF_t)
  
  MNI_total <- sum(mni_tbl$PV_MNI_t)
  
  DurA <- duration_mac(cfA_tot, courbe_fr)
  DurP <- duration_mac(cfP_tot, courbe_fr)
  KRD_A <- key_rate_durations(cfA_tot, courbe_fr)
  KRD_P <- key_rate_durations(cfP_tot, courbe_fr)
  
  list(
    scope = scope_zone,
    cfA = cfA,
    cfA_tot = cfA_tot,
    cfP = cfP,
    cfP_tot = cfP_tot,
    NPV_A = NPV_A,
    BEL0 = BEL0,
    mni = mni_tbl,
    MNI_total = MNI_total,
    DurA = DurA,
    DurP = DurP,
    KRD_A = KRD_A,
    KRD_P = KRD_P
  )
}

scopes <- c("Groupe", zones_filiales)
results_scopes <- lapply(scopes, proj_scope)

# 3.4 Bilan Market-Consistent consolidé & filiales
bilan_mc_scopes <- bind_rows(lapply(scopes, compute_bilan_mc))

# 3.5 Embedded Value (EV) – version simple :
# EV ~ FP_MC + MNI_total (valeur actualisée des flux futurs) 
embedded_value_scopes <- map_df(results_scopes, function(res){
  mc <- bilan_mc_scopes %>% filter(Scope == res$scope)
  tibble(
    Scope = res$scope,
    FP_MC_MEUR = mc$FP_MC_MEUR,
    MNI_PV_MEUR = res$MNI_total,
    EV_simple_MEUR = mc$FP_MC_MEUR + res$MNI_total
  )
})

# 3.6 Diagnostic "cash-flow matching" (écarts par bucket de maturité)
cf_gap_buckets <- map_df(results_scopes, function(res){
  cfA <- res$cfA_tot
  cfP <- res$cfP_tot
  cf_join <- cfA %>%
    rename(CF_A = CF_MEUR) %>%
    full_join(cfP %>% rename(CF_P = CF_MEUR),
              by = "t") %>%
    mutate(across(c(CF_A,CF_P), ~replace_na(.,0)),
           Gap_CF = CF_A - CF_P,
           Bucket = cut(t,
                        breaks = c(0,5,10,20,30,60),
                        labels = c("0-5","5-10","10-20","20-30","30-60"),
                        include.lowest = TRUE))
  cf_gap <- cf_join %>%
    group_by(Scope = res$scope, Bucket) %>%
    summarise(CF_A = sum(CF_A),
              CF_P = sum(CF_P),
              Gap_CF = sum(Gap_CF),
              .groups="drop")
  cf_gap
})

############################################################
# 4. SENSIBILITÉS & SCÉNARIOS ORSA NARRATIFS
############################################################

sensibilites_scope <- function(res_scope, bump_bp = 0.01){
  cfA_tot <- res_scope$cfA_tot
  cfP_tot <- res_scope$cfP_tot
  
  # Δ taux : bump courbe complète
  courbe_up <- courbe_fr
  courbe_up$Rate <- courbe_up$Rate + bump_bp
  NPV_A_up <- pv_cf(cfA_tot, courbe_up)
  BEL0_up  <- pv_cf(cfP_tot, courbe_up)
  
  # Δ spread : bump sur actifs uniquement (approx)
  courbe_spread <- courbe_fr
  courbe_spread$Rate <- courbe_spread$Rate + bump_bp
  NPV_A_spread_up <- pv_cf(cfA_tot, courbe_spread)
  
  # Δ lapse : +10% flux passifs
  cfP_lapse <- cfP_tot %>%
    mutate(CF_MEUR = CF_MEUR * 1.10)
  BEL0_lapse_up <- pv_cf(cfP_lapse, courbe_fr)
  
  # Δ mortality : +10% sur BBA/VFA
  cfP_mort <- res_scope$cfP %>%
    mutate(CF_MEUR = ifelse(Modele_IFRS17 %in% c("BBA","VFA","BBA/VFA"),
                            CF_MEUR*1.10,
                            CF_MEUR)) %>%
    group_by(t) %>%
    summarise(CF_MEUR = sum(CF_MEUR), .groups="drop")
  BEL0_mort_up <- pv_cf(cfP_mort, courbe_fr)
  
  tibble(
    Scope = res_scope$scope,
    dNPV_A_dtaux      = NPV_A_up - res_scope$NPV_A,
    dBEL0_dtaux       = BEL0_up  - res_scope$BEL0,
    dNPV_A_dspread    = NPV_A_spread_up - res_scope$NPV_A,
    dBEL0_dlapse      = BEL0_lapse_up - res_scope$BEL0,
    dBEL0_dmortality  = BEL0_mort_up  - res_scope$BEL0
  )
}

sens_scopes <- map_df(results_scopes, sensibilites_scope)

# 4.1 Scénarios narratifs ORSA (déterministes)
# Exemple : Taux+200, Spreads+150, Actions-25%
# On reste simple : on applique des multiplicateurs aux PV

scenarios_orsa <- tribble(
  ~Scenario,        ~shock_rate_bp, ~shock_spread_bp, ~shock_equity,
  "Base",                 0,              0,               0.00,
  "Taux+200",           200,              0,               0.00,
  "Spread+150",           0,            150,               0.00,
  "Equity-25",            0,              0,              -0.25,
  "Combo",              200,            150,              -0.25
)

# On calcule un proxy : 
#  - Δtaux / Δspread via sensibilités,
#  - choc equity sur la MNI via proportion d’actifs risqués (voir plus bas).

# Poids actions / immobilier par scope (approx)
poids_actions_scope <- actifs_econ %>%
  mutate(Classe_ALM = case_when(
    Classe_actif %in% c("Actions","OPCVM") ~ "Actions",
    Classe_actif == "Immobilier"          ~ "Immobilier",
    TRUE                                  ~ "Autres"
  )) %>%
  group_by(Zone, Classe_ALM) %>%
  summarise(Actif_MEUR = sum(Actif_MEUR), .groups="drop") %>%
  group_by(Zone) %>%
  mutate(Poids = Actif_MEUR / sum(Actif_MEUR)) %>%
  ungroup()

# La table ORSA_scenarios_scope utilisera ces proxys.
# (Bloc préparé, mais calcul détaillé des PV par scénario
# pourra être complété spécifiquement si besoin.)

############################################################
# 5. SURPLUS CORE vs TOTAL (RÈGLES D’EXCLUSION)
############################################################

# On définit "Techniques" = passifs & actifs marqués comme technique / dérivés
# Exemple : Categorie_risque %in% c("Technique","Hors-bilan") ou Classe_actif="Dérivés"
# A adapter si besoin selon la base.

passifs_core <- passifs_econ %>%
  filter(!Categorie_risque %in% c("Technique","Hors-bilan"))

passifs_tech <- passifs_econ %>%
  filter(Categorie_risque %in% c("Technique","Hors-bilan"))

actifs_core <- actifs_econ %>%
  filter(!Classe_actif %in% c("Derives","Dérivés"))

actifs_tech <- actifs_econ %>%
  filter(Classe_actif %in% c("Derives","Dérivés"))

compute_surplus <- function(actif_tbl, passif_tbl, label){
  bind_rows(lapply(scopes, function(z){
    A <- actif_tbl %>% filter(Zone == z) %>% summarise(A = sum(Actif_MEUR)) %>% pull(A)
    P <- passif_tbl %>% filter(Zone == z) %>% summarise(P = sum(Passif_MEUR)) %>% pull(P)
    tibble(
      Scope = z,
      Surface = label,
      Actifs_MEUR = A,
      Passifs_MEUR = P,
      Surplus_MEUR = A - P
    )
  }))
}

surplus_core  <- compute_surplus(actifs_core, passifs_core, "Core")
surplus_total <- compute_surplus(actifs_econ, passifs_econ, "Total")

surplus_all <- bind_rows(surplus_core, surplus_total)

############################################################
# 6. MARGE DE RISQUE (RISK MARGIN) – APPROCHE SIMPLE
############################################################

# Proxy : RM ≈ CoC * (SCR_nonhedge / r_eff)
# Ici : on prend RM_scope = CoC_rate * SCR_scope * facteur (par ex 5 ans moyen)
# On utilise SCR proxy = 20% BEL (à raffiner avec vrais SCR si dispo).

risk_margin_scopes <- map_df(results_scopes, function(res){
  SCR_proxy <- 0.20 * res$BEL0
  RM_proxy  <- CoC_rate * SCR_proxy * 5  # 5 ans de duration SCR moyen
  tibble(
    Scope = res$scope,
    BEL0_MEUR = res$BEL0,
    SCR_proxy_MEUR = SCR_proxy,
    RM_proxy_MEUR  = RM_proxy
  )
})

############################################################
# 7. ANALYSES PAR PRODUIT / SEGMENT IFRS17 / IFRS9
############################################################

# 7.1 Contributions à MNI, BEL, NPV par segment IFRS17 (passif)
# On agrège la MNI par Modele_IFRS17 / Categorie_risque pour Groupe

analyse_IFRS17_MNI <- results_scopes[[1]]$mni %>%
  # jointure avec les flux passifs par produit
  left_join(
    results_scopes[[1]]$cfP %>%
      select(id_passif, Modele_IFRS17, Categorie_risque, t) %>%
      distinct(),
    by = "t"
  ) %>%
  group_by(Modele_IFRS17, Categorie_risque) %>%
  summarise(
    MNI_PV_MEUR = sum(PV_MNI_t, na.rm = TRUE),
    .groups="drop"
  )

# 7.2 IFRS9 – Contributions à NPV actifs par catégorie
cfA_groupe <- results_scopes[[1]]$cfA %>%
  group_by(IFRS9_categorie, t) %>%
  summarise(CF_MEUR = sum(CF_MEUR), .groups="drop")

analyse_IFRS9_NPV <- cfA_groupe %>%
  group_by(IFRS9_categorie) %>%
  summarise(
    NPV_Actifs_MEUR = pv_cf(cur_data(), courbe_fr),
    .groups="drop"
  )

############################################################
# 8. MNI PAR GRANDES FAMILLES VIE / ÉPARGNE / NON-VIE
############################################################

# On suppose que base_passifs_ALM_clean contient une colonne Segment_ALM
# (ex: "Vie/Epargne", "Non-vie", "Financier", etc.)
# Sinon, adapter la classification ici.

mni_par_segment <- function(res_scope, passif_tbl){
  seg_info <- passif_tbl %>%
    filter(Zone == res_scope$scope) %>%
    select(Produit_detaille, Segment_ALM) %>%
    distinct()
  
  res_scope$mni %>%
    left_join(
      res_scope$cfP %>%
        select(id_passif, t, Produit_detaille) %>%
        distinct(),
      by = "t"
    ) %>%
    left_join(seg_info, by = "Produit_detaille") %>%
    group_by(Scope = res_scope$scope, Segment_ALM) %>%
    summarise(MNI_PV_MEUR = sum(PV_MNI_t, na.rm = TRUE),
              .groups="drop")
}

MNI_segments <- bind_rows(lapply(results_scopes, mni_par_segment, passif_tbl = passifs_econ))

############################################################
# 9. MAPPING VFA/BBA vs IFRS9 – ALIGNEMENT ALM
############################################################

# Cartographie des volumes VFA/BBA par type d’actif IFRS9 pour le Groupe

vfa_bba <- passifs_econ %>%
  filter(Zone == "Groupe",
         Modele_IFRS17 %in% c("VFA","BBA","BBA/VFA")) %>%
  group_by(Modele_IFRS17, Produit_detaille) %>%
  summarise(Passif_MEUR = sum(Passif_MEUR), .groups="drop")

# Approche simple : on relie les produits VFA/BBA aux actifs de même Zone "Groupe"
# en regardant la structure IFRS9 globale (proxy de l'adossement).

ifrs9_struct <- actifs_econ %>%
  filter(Zone == "Groupe") %>%
  group_by(IFRS9_categorie) %>%
  summarise(Actif_MEUR = sum(Actif_MEUR), .groups="drop") %>%
  mutate(Poids = Actif_MEUR / sum(Actif_MEUR))

mapping_VFA_IFRS9 <- vfa_bba %>%
  mutate(Total_passif = sum(Passif_MEUR),
         Proportion = Passif_MEUR / Total_passif) %>%
  crossing(ifrs9_struct) %>%
  mutate(Actif_affecte_MEUR = Proportion * Actif_MEUR)

############################################################
# 10. ESG STOCHASTIQUE (HULL–WHITE + EQUITY + INFLATION + SPREADS)
############################################################

simulate_ESG_HW <- function(n_scen = 1000, horizon = 5, dt = 1){
  t_steps <- 0:horizon
  n_t <- length(t_steps)
  
  r <- matrix(0, n_scen, n_t)
  S <- matrix(0, n_scen, n_t)
  I <- matrix(0, n_scen, n_t)  # inflation index
  s_spread <- matrix(0, n_scen, n_t)
  
  # initial values
  r[,1] <- get_rate(courbe_fr, 1)
  S[,1] <- 100
  I[,1] <- 100
  s_spread[,1] <- 0.01
  
  for (k in 2:n_t){
    z_r  <- rnorm(n_scen)
    z_eq <- rnorm(n_scen)
    z_infl <- rnorm(n_scen)
    z_sp  <- rnorm(n_scen)
    
    # Hull–White 1f
    r[,k] <- r[,k-1] + a_hw*(b_hw - r[,k-1])*dt + sigma_hw*sqrt(dt)*z_r
    
    # Equity
    S[,k] <- S[,k-1] * exp((mu_eq - 0.5*sigma_eq^2)*dt + sigma_eq*sqrt(dt)*z_eq)
    
    # Inflation index
    I[,k] <- I[,k-1] * exp((mu_infl - 0.5*sigma_infl^2)*dt + sigma_infl*sqrt(dt)*z_infl)
    
    # Spreads (process de type Brownien simple)
    s_spread[,k] <- s_spread[,k-1] + mu_spread*dt + sigma_spread*sqrt(dt)*z_sp
  }
  
  list(
    t = t_steps,
    r = r,
    S = S,
    I = I,
    s_spread = s_spread
  )
}

# Simulation ESG pour le scope Groupe
ESG_groupe <- simulate_ESG_HW(n_scen = 1000, horizon = 5, dt = 1)

# Poids d’actifs par classe (Groupe)
actifs_groupe <- actifs_econ %>% filter(Zone == "Groupe")
A0_total <- sum(actifs_groupe$Actif_MEUR)
actifs_groupe <- actifs_groupe %>%
  mutate(Poids_MV = Actif_MEUR / A0_total)

w_taux <- sum(actifs_groupe$Poids_MV[actifs_groupe$Classe_actif %in% c("Obligations","Prets","TCN")])
w_eq   <- sum(actifs_groupe$Poids_MV[actifs_groupe$Classe_actif %in% c("Actions","OPCVM")])
w_imm  <- sum(actifs_groupe$Poids_MV[actifs_groupe$Classe_actif == "Immobilier"])

# Construction d'un MNI stochastique sur 5 ans (proxy)
res_groupe <- results_scopes[[which(scopes=="Groupe")]]

n_scen  <- nrow(ESG_groupe$r)
horizon <- 5

R_eq    <- ESG_groupe$S[,2:(horizon+1)] / ESG_groupe$S[,1:horizon] - 1
Delta_r <- ESG_groupe$r[,2:(horizon+1)] - ESG_groupe$r[,1:horizon]
Delta_s <- ESG_groupe$s_spread[,2:(horizon+1)] - ESG_groupe$s_spread[,1:horizon]

facteur_spread <- 5

MNI_scen <- matrix(0, n_scen, horizon)
for (t in 1:horizon){
  contrib_eq   <- A0_total * w_eq  * R_eq[,t]
  contrib_im   <- A0_total * w_imm * (R_eq[,t] * 0.5)
  contrib_sp   <- -A0_total * w_taux * facteur_spread * Delta_s[,t]
  contrib_taux <- -(res_groupe$DurA - res_groupe$DurP) * A0_total * Delta_r[,t]
  MNI_scen[,t] <- contrib_eq + contrib_im + contrib_sp + contrib_taux
}

# Calibration simple de la volatilité MNI (si vol_MNI_rel fourni)
# On peut ajuster l'échelle globale :
if (!is.na(vol_MNI_rel) && vol_MNI_rel > 0){
  # on cible un ratio sigma_MNI / E[|MNI|] ~ vol_MNI_rel
  current_vol <- sd(as.vector(MNI_scen))
  current_mean_abs <- mean(abs(as.vector(MNI_scen)))
  target_vol <- vol_MNI_rel * current_mean_abs
  if (current_vol > 0){
    scale_factor <- target_vol / current_vol
    MNI_scen <- MNI_scen * scale_factor
  }
}

############################################################
# 11. POLITIQUE DE DIVIDENDES ORSA – β OPTIMAL
############################################################

# SCR proxy Groupe (utilisé pour ratio solvabilité)
SCR_MEUR <- 0.20 * res_groupe$BEL0
OF_base  <- bilan_mc_scopes %>% filter(Scope=="Groupe") %>% pull(FP_MC_MEUR)

simulate_div_path <- function(MNI_path,
                              OF_init, SCR, SCR_target, SCR_min,
                              beta, div_max_factor = 0.25){
  horizon <- length(MNI_path)
  OF <- numeric(horizon+1)
  DIV<- numeric(horizon+1)
  R  <- numeric(horizon+1)
  OF[1] <- OF_init
  R[1]  <- OF[1]/SCR
  div_max <- div_max_factor * OF_init
  for (t in 1:horizon){
    surplus_t <- max(0, OF[t] - SCR_target*SCR)
    d <- beta * surplus_t
    d <- min(d, div_max)
    if (OF[t]/SCR < SCR_min) d <- 0
    DIV[t+1] <- d
    OF[t+1]  <- OF[t] + MNI_path[t] - d
    R[t+1]   <- OF[t+1]/SCR
  }
  list(OF=OF, DIV=DIV, R=R)
}

eval_beta <- function(beta, MNI_scen, OF_init, SCR, SCR_target, SCR_min){
  n_scen  <- nrow(MNI_scen)
  horizon <- ncol(MNI_scen)
  total_div <- numeric(n_scen)
  min_ratio <- numeric(n_scen)
  for (i in 1:n_scen){
    res <- simulate_div_path(MNI_scen[i,], OF_init, SCR, SCR_target, SCR_min, beta)
    total_div[i] <- sum(res$DIV[-1])
    min_ratio[i] <- min(res$R)
  }
  prob_breach <- mean(min_ratio < SCR_min)
  tibble(
    beta = beta,
    prob_breach = prob_breach,
    div_mean_5y = mean(total_div),
    div_median_5y = median(total_div)
  )
}

beta_grid  <- seq(0, 0.9, by = 0.05)
res_beta_all <- map_df(beta_grid, ~eval_beta(.x, MNI_scen, OF_base,
                                             SCR_MEUR, SCR_target_ratio, SCR_min_ratio))

beta_opt <- res_beta_all %>%
  filter(prob_breach <= prob_breach_target) %>%
  arrange(desc(beta)) %>%
  slice(1) %>%
  pull(beta)

if (length(beta_opt) == 0) beta_opt <- 0

# Dividendes soutenables sur 5 ans (avec β optimal)
eval_opt <- eval_beta(beta_opt, MNI_scen, OF_base,
                      SCR_MEUR, SCR_target_ratio, SCR_min_ratio)

dividendes_soutenables <- tibble(
  beta_opt = beta_opt,
  div_mean_5y = eval_opt$div_mean_5y,
  div_median_5y = eval_opt$div_median_5y,
  prob_breach_beta_opt = eval_opt$prob_breach
)

############################################################
# 12. INDICATEURS DE CAPACITÉ DISTRIBUTIVE
############################################################

# 12.1 Ratio de solvabilité t0
solv_t0 <- tibble(
  Scope = "Groupe",
  FP_MC_MEUR = OF_base,
  SCR_proxy_MEUR = SCR_MEUR,
  Ratio_SCR_t0 = OF_base / SCR_MEUR
)

# 12.2 Organic Capital Generation (1 an) – proxy = E[MNI_1y] - ΔSCR
OCG_proxy <- mean(MNI_scen[,1])
delta_SCR_proxy <- 0  # simplification
OCG_tbl <- tibble(
  Scope = "Groupe",
  OCG_1y_MEUR = OCG_proxy,
  delta_SCR_proxy_MEUR = delta_SCR_proxy
)

# 12.3 Free Cash-Flow to Equity (FCFE) – proxy = MNI - ΔBS (simple)
FCFE_tbl <- tibble(
  Scope = "Groupe",
  FCFE_1y_MEUR = OCG_proxy  # dans cette version simple
)

# 12.4 Capital buffer vs capital cible
capital_buffer <- tibble(
  Scope = "Groupe",
  FP_MC_MEUR = OF_base,
  Capital_cible_MEUR = SCR_target_ratio * SCR_MEUR,
  Capital_buffer_MEUR = OF_base - SCR_target_ratio * SCR_MEUR
)

############################################################
# 13. SYNTHÈSES & EXPORT EXCEL
############################################################

# Synthèse NPV / BEL / MNI / Durations
synth_scopes <- map_df(results_scopes, function(res){
  tibble(
    Scope = res$scope,
    NPV_Actifs_MEUR = res$NPV_A,
    BEL0_MEUR       = res$BEL0,
    MNI_PV_MEUR     = res$MNI_total,
    DurA_ans        = res$DurA,
    DurP_ans        = res$DurP
  )
})

# Segments IFRS17 & IFRS9 (Groupe)
segments_ifrs17 <- passifs_econ %>%
  filter(Zone == "Groupe") %>%
  group_by(Modele_IFRS17, Categorie_risque) %>%
  summarise(Passif_MEUR = sum(Passif_MEUR), .groups="drop")

segments_ifrs9 <- actifs_econ %>%
  filter(Zone == "Groupe") %>%
  group_by(IFRS9_categorie, Classe_actif) %>%
  summarise(Actif_MEUR = sum(Actif_MEUR), .groups="drop")

# Création du classeur Excel final
wb <- createWorkbook()

addWorksheet(wb, "Bilan_MC")
writeData(wb, "Bilan_MC", bilan_mc_scopes)

addWorksheet(wb, "Embedded_Value")
writeData(wb, "Embedded_Value", embedded_value_scopes)

addWorksheet(wb, "CF_Gap_Buckets")
writeData(wb, "CF_Gap_Buckets", cf_gap_buckets)

addWorksheet(wb, "Synthese_Scopes")
writeData(wb, "Synthese_Scopes", synth_scopes)

addWorksheet(wb, "Segments_IFRS17")
writeData(wb, "Segments_IFRS17", segments_ifrs17)

addWorksheet(wb, "Segments_IFRS9")
writeData(wb, "Segments_IFRS9", segments_ifrs9)

addWorksheet(wb, "Sensibilites")
writeData(wb, "Sensibilites", sens_scopes)

addWorksheet(wb, "Surplus_Core_Total")
writeData(wb, "Surplus_Core_Total", surplus_all)

addWorksheet(wb, "Risk_Margin")
writeData(wb, "Risk_Margin", risk_margin_scopes)

addWorksheet(wb, "Analyse_IFRS17_MNI")
writeData(wb, "Analyse_IFRS17_MNI", analyse_IFRS17_MNI)

addWorksheet(wb, "Analyse_IFRS9_NPV")
writeData(wb, "Analyse_IFRS9_NPV", analyse_IFRS9_NPV)

addWorksheet(wb, "MNI_par_Segment")
writeData(wb, "MNI_par_Segment", MNI_segments)

addWorksheet(wb, "Mapping_VFA_IFRS9")
writeData(wb, "Mapping_VFA_IFRS9", mapping_VFA_IFRS9)

addWorksheet(wb, "ORSA_Beta_grid")
writeData(wb, "ORSA_Beta_grid", res_beta_all)

addWorksheet(wb, "Dividendes_soutenables")
writeData(wb, "Dividendes_soutenables", dividendes_soutenables)

addWorksheet(wb, "Capacite_Distributive")
writeData(wb, "Capacite_Distributive",
          bind_rows(solv_t0, OCG_tbl, FCFE_tbl, capital_buffer))

saveWorkbook(wb, "ALM_Multiniveaux_v2_Results.xlsx", overwrite = TRUE)

############################################################
# FIN DU MOTEUR R ALM SOLVABILITÉ MULTI-NIVEAUX v2
############################################################
