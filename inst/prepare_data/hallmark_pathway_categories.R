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


usethis::use_data(
    hallmark_pathway_categories,
    internal = TRUE,
    overwrite = TRUE
)
