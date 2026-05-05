library(tibble)

# Load hallmark_t2g to validate pathway names against
load("data/hallmark_t2g.rda")

# Pathway names use the same format as hallmark_t2g$term (no "HALLMARK_" prefix)
hallmark_pathway_categories <- tribble(
    ~term                               , ~process_category    ,

    # Cellular component
    "APICAL_JUNCTION"                   , "cellular_component" ,
    "APICAL_SURFACE"                    , "cellular_component" ,
    "PEROXISOME"                        , "cellular_component" ,

    # Development
    "ADIPOGENESIS"                      , "development"        ,
    "ANGIOGENESIS"                      , "development"        ,
    "EPITHELIAL_MESENCHYMAL_TRANSITION" , "development"        ,
    "MYOGENESIS"                        , "development"        ,
    "SPERMATOGENESIS"                   , "development"        ,
    "PANCREAS_BETA_CELLS"               , "development"        ,

    # DNA damage
    "DNA_REPAIR"                        , "DNA_damage"         ,
    "UV_RESPONSE_DN"                    , "DNA_damage"         ,
    "UV_RESPONSE_UP"                    , "DNA_damage"         ,

    # Immune
    "ALLOGRAFT_REJECTION"               , "immune"             ,
    "COAGULATION"                       , "immune"             ,
    "COMPLEMENT"                        , "immune"             ,
    "INTERFERON_ALPHA_RESPONSE"         , "immune"             ,
    "INTERFERON_GAMMA_RESPONSE"         , "immune"             ,
    "IL6_JAK_STAT3_SIGNALING"           , "immune"             ,
    "INFLAMMATORY_RESPONSE"             , "immune"             ,

    # Metabolic
    "BILE_ACID_METABOLISM"              , "metabolic"          ,
    "CHOLESTEROL_HOMEOSTASIS"           , "metabolic"          ,
    "FATTY_ACID_METABOLISM"             , "metabolic"          ,
    "GLYCOLYSIS"                        , "metabolic"          ,
    "HEME_METABOLISM"                   , "metabolic"          ,
    "OXIDATIVE_PHOSPHORYLATION"         , "metabolic"          ,
    "XENOBIOTIC_METABOLISM"             , "metabolic"          ,

    # Pathway / stress
    "APOPTOSIS"                         , "pathway"            ,
    "HYPOXIA"                           , "pathway"            ,
    "PROTEIN_SECRETION"                 , "pathway"            ,
    "UNFOLDED_PROTEIN_RESPONSE"         , "pathway"            ,
    "REACTIVE_OXYGEN_SPECIES_PATHWAY"   , "pathway"            ,

    # Proliferation
    "E2F_TARGETS"                       , "proliferation"      ,
    "G2M_CHECKPOINT"                    , "proliferation"      ,
    "MYC_TARGETS_V1"                    , "proliferation"      ,
    "MYC_TARGETS_V2"                    , "proliferation"      ,
    "P53_PATHWAY"                       , "proliferation"      ,
    "MITOTIC_SPINDLE"                   , "proliferation"      ,

    # Signaling
    "ANDROGEN_RESPONSE"                 , "signaling"          ,
    "ESTROGEN_RESPONSE_EARLY"           , "signaling"          ,
    "ESTROGEN_RESPONSE_LATE"            , "signaling"          ,
    "IL2_STAT5_SIGNALING"               , "signaling"          ,
    "KRAS_SIGNALING_UP"                 , "signaling"          ,
    "KRAS_SIGNALING_DN"                 , "signaling"          ,
    "MTORC1_SIGNALING"                  , "signaling"          ,
    "NOTCH_SIGNALING"                   , "signaling"          ,
    "PI3K_AKT_MTOR_SIGNALING"           , "signaling"          ,
    "HEDGEHOG_SIGNALING"                , "signaling"          ,
    "TGF_BETA_SIGNALING"                , "signaling"          ,
    "TNFA_SIGNALING_VIA_NFKB"           , "signaling"          ,
    "WNT_BETA_CATENIN_SIGNALING"        , "signaling"
)

# Validate: all terms must exist in hallmark_t2g
t2g_terms <- unique(hallmark_t2g$term)
missing <- setdiff(hallmark_pathway_categories$term, t2g_terms)
if (length(missing) > 0) {
    stop(
        "Terms in hallmark_pathway_categories not found in hallmark_t2g:\n",
        paste(missing, collapse = "\n")
    )
}

# Capitalize first letter of each process category (e.g. "development" -> "Development")
hallmark_pathway_categories$process_category <- paste0(
    toupper(substring(hallmark_pathway_categories$process_category, 1, 1)),
    substring(hallmark_pathway_categories$process_category, 2)
)

# remove _ from process categories
hallmark_pathway_categories$process_category <- gsub(
    "_",
    " ",
    hallmark_pathway_categories$process_category
)

# Create human-readable pathway labels
hallmark_pathway_labels_short <- c(
    ADIPOGENESIS = "Adipogenesis",
    ALLOGRAFT_REJECTION = "Allograft rejection",
    ANDROGEN_RESPONSE = "Androgen response",
    ANGIOGENESIS = "Angiogenesis",
    APICAL_JUNCTION = "Apical junction",
    APICAL_SURFACE = "Apical surface",
    APOPTOSIS = "Apoptosis",
    BILE_ACID_METABOLISM = "Bile acid metabolism",
    CHOLESTEROL_HOMEOSTASIS = "Cholesterol homeostasis",
    COAGULATION = "Coagulation",
    COMPLEMENT = "Complement",
    DNA_REPAIR = "DNA repair",
    E2F_TARGETS = "E2F targets",
    EPITHELIAL_MESENCHYMAL_TRANSITION = "Epithelial-mesenchymal transition",
    ESTROGEN_RESPONSE_EARLY = "Estrogen response (early)",
    ESTROGEN_RESPONSE_LATE = "Estrogen response (late)",
    FATTY_ACID_METABOLISM = "Fatty acid metabolism",
    G2M_CHECKPOINT = "G2/M checkpoint",
    GLYCOLYSIS = "Glycolysis",
    HEDGEHOG_SIGNALING = "Hedgehog signaling",
    HEME_METABOLISM = "Heme metabolism",
    HYPOXIA = "Hypoxia",
    IL2_STAT5_SIGNALING = "IL-2/STAT5 signaling",
    IL6_JAK_STAT3_SIGNALING = "IL-6/JAK/STAT3 signaling",
    INFLAMMATORY_RESPONSE = "Inflammatory response",
    INTERFERON_ALPHA_RESPONSE = "Interferon-α response",
    INTERFERON_GAMMA_RESPONSE = "Interferon-γ response",
    KRAS_SIGNALING_DN = "KRAS signaling (down)",
    KRAS_SIGNALING_UP = "KRAS signaling (up)",
    MITOTIC_SPINDLE = "Mitotic spindle",
    MTORC1_SIGNALING = "mTORC1 signaling",
    MYC_TARGETS_V1 = "MYC targets (v1)",
    MYC_TARGETS_V2 = "MYC targets (v2)",
    MYOGENESIS = "Myogenesis",
    NOTCH_SIGNALING = "Notch signaling",
    OXIDATIVE_PHOSPHORYLATION = "Oxidative phosphorylation",
    P53_PATHWAY = "p53 pathway",
    PANCREAS_BETA_CELLS = "Pancreatic β cells",
    PEROXISOME = "Peroxisome",
    PI3K_AKT_MTOR_SIGNALING = "PI3K/AKT/mTOR signaling",
    PROTEIN_SECRETION = "Protein secretion",
    REACTIVE_OXYGEN_SPECIES_PATHWAY = "Reactive oxygen species pathway",
    SPERMATOGENESIS = "Spermatogenesis",
    TGF_BETA_SIGNALING = "TGF-β signaling",
    TNFA_SIGNALING_VIA_NFKB = "TNFα signaling via NF-κB",
    UNFOLDED_PROTEIN_RESPONSE = "Unfolded protein response",
    UV_RESPONSE_DN = "UV response (down)",
    UV_RESPONSE_UP = "UV response (up)",
    WNT_BETA_CATENIN_SIGNALING = "Wnt/β-catenin signaling",
    XENOBIOTIC_METABOLISM = "Xenobiotic metabolism"
)

# Create pathway labels with Greek symbols spelled out
hallmark_pathway_labels_long <- c(
    ADIPOGENESIS = "Adipogenesis",
    ALLOGRAFT_REJECTION = "Allograft rejection",
    ANDROGEN_RESPONSE = "Androgen response",
    ANGIOGENESIS = "Angiogenesis",
    APICAL_JUNCTION = "Apical junction",
    APICAL_SURFACE = "Apical surface",
    APOPTOSIS = "Apoptosis",
    BILE_ACID_METABOLISM = "Bile acid metabolism",
    CHOLESTEROL_HOMEOSTASIS = "Cholesterol homeostasis",
    COAGULATION = "Coagulation",
    COMPLEMENT = "Complement",
    DNA_REPAIR = "DNA repair",
    E2F_TARGETS = "E2F targets",
    EPITHELIAL_MESENCHYMAL_TRANSITION = "Epithelial–mesenchymal transition",
    ESTROGEN_RESPONSE_EARLY = "Estrogen response (early)",
    ESTROGEN_RESPONSE_LATE = "Estrogen response (late)",
    FATTY_ACID_METABOLISM = "Fatty acid metabolism",
    G2M_CHECKPOINT = "G2/M checkpoint",
    GLYCOLYSIS = "Glycolysis",
    HEDGEHOG_SIGNALING = "Hedgehog signaling",
    HEME_METABOLISM = "Heme metabolism",
    HYPOXIA = "Hypoxia",
    IL2_STAT5_SIGNALING = "Interleukin-2 STAT5 signaling",
    IL6_JAK_STAT3_SIGNALING = "Interleukin-6 JAK STAT3 signaling",
    INFLAMMATORY_RESPONSE = "Inflammatory response",
    INTERFERON_ALPHA_RESPONSE = "Interferon alpha response",
    INTERFERON_GAMMA_RESPONSE = "Interferon gamma response",
    KRAS_SIGNALING_DN = "KRAS signaling (downregulated)",
    KRAS_SIGNALING_UP = "KRAS signaling (upregulated)",
    MITOTIC_SPINDLE = "Mitotic spindle",
    MTORC1_SIGNALING = "mTOR complex 1 signaling",
    MYC_TARGETS_V1 = "MYC targets (version 1)",
    MYC_TARGETS_V2 = "MYC targets (version 2)",
    MYOGENESIS = "Myogenesis",
    NOTCH_SIGNALING = "Notch signaling",
    OXIDATIVE_PHOSPHORYLATION = "Oxidative phosphorylation",
    P53_PATHWAY = "p53 pathway",
    PANCREAS_BETA_CELLS = "Pancreatic beta cells",
    PEROXISOME = "Peroxisome",
    PI3K_AKT_MTOR_SIGNALING = "PI3K/AKT/mTOR signaling",
    PROTEIN_SECRETION = "Protein secretion",
    REACTIVE_OXYGEN_SPECIES_PATHWAY = "Reactive oxygen species pathway",
    SPERMATOGENESIS = "Spermatogenesis",
    TGF_BETA_SIGNALING = "Transforming growth factor (TGF) beta signaling",
    TNFA_SIGNALING_VIA_NFKB = "Tumor necrosis factor (TNF) alpha signaling via NF-kappa-B",
    UNFOLDED_PROTEIN_RESPONSE = "Unfolded protein response",
    UV_RESPONSE_DN = "Ultraviolet response (downregulated)",
    UV_RESPONSE_UP = "Ultraviolet response (upregulated)",
    WNT_BETA_CATENIN_SIGNALING = "Wnt beta catenin signaling",
    XENOBIOTIC_METABOLISM = "Xenobiotic metabolism"
)

# Reorder labels to match the pathway categories order
hallmark_pathway_labels_short <- hallmark_pathway_labels_short[
    hallmark_pathway_categories$term
]
hallmark_pathway_labels_long <- hallmark_pathway_labels_long[
    hallmark_pathway_categories$term
]

# Validate that labels are in the same order as the data
stopifnot(
    "Names of hallmark_pathway_labels_short must match hallmark_pathway_categories$term in order" = identical(
        names(hallmark_pathway_labels_short),
        hallmark_pathway_categories$term
    ),
    "Names of hallmark_pathway_labels_long must match hallmark_pathway_categories$term in order" = identical(
        names(hallmark_pathway_labels_long),
        hallmark_pathway_categories$term
    )
)

# Add labels to the main data object
hallmark_pathway_categories <- hallmark_pathway_categories |>
    dplyr::mutate(
        label_short = hallmark_pathway_labels_short[term],
        label_long = hallmark_pathway_labels_long[term]
    )

# Save the combined data object
usethis::use_data(
    hallmark_pathway_categories,
    overwrite = TRUE
)
