library(tidyverse)
library(data.table)
library(lubridate)
library(skimr)

##### METHOD DEVELOPMENT SITES - Read in site data for methods development sites ####
western_sites_classes<-read_csv("master_site_class_xwalk_030723.csv") %>%
  filter(Region=="West") %>%
  mutate(State=str_sub(SiteCode, 1,2))

western_sites_classes %>%
  group_by(Region_detail, Class) %>%
  tally() %>%
  pivot_wider(names_from=Region_detail, values_from = n)


#Identify WESTERN databases
myDBs<-c("WMBR_1_1","WEx_SDAM_0","WMBR_2","WMV_1","FD003","FD004") 

#### MAIN - Read in main site data table and filter for GP sites ####
junk<- read_csv("https://sdamchecker.sccwrp.org/checker/download/main-all")# %>%
junk_west<-junk %>% filter(origin_database %in% myDBs) 
junk_east<-junk %>% filter(origin_database %in% c("NESE Baseline Revisits v2", "NESE Baseline v1", "NESE Validation v1"))
junk_gp<-junk %>% filter(origin_database %in% c("Great Plains Baseline Revisits v2_1_October","Great Plains SDAM v3","Great Plains Validation_v2")) #Missing GP revisits v3

main_df<- junk %>%
  filter(origin_database %in% myDBs) %>%
  transmute( # Align column names to original metric calculator script naming
    Download_date = Sys.time(),
    Database= origin_database,
    ParentGlobalID = globalid,
    SiteCode = sitecode,
    SiteName = sitename,
    Assessors = assessor,
    Recorder= recorder, QA=qa,
    CollectionDate= collectiondate,
    CreationDate= creationdate,
    Creator=creator,
    EditDate=editdate,
    Lat_field= lat, 
    Long_field= long,
    Weather=weathernow,
    PctCloudCover = case_when(is.na(cloudynow) == T ~ 0,T~cloudynow), # Replace missing cloud cover with zero
    # rain_yesno, #NESE
    Disturbances=disturbed,
    Disturbances_details=disturbed_note,
    Bankwidth_0= bankfullwidth0,
    Bankwidth_15= bankfullwidth15,
    Bankwidth_30= bankfullwidth30,
    # ReachLength_targ= reachlengthcalc,
    # ReachLength_actual= reachlength, #Missing from all Western Expansion?
    Slope=valleyslope,
    Landuse=p_landuse,#NESE
    # Landuse_s=s_landuse, #Now secondary landuse for NESE. Was primary for other datasets
    Lanuse_Notes=landuse_note,
    Riparian_yn= riparian, 
    ErosionDeposition_yn=erosion,
    FloodplainConnectivity_yn=floodplain,
    SurfaceFlow_pct=hi_reachlengthsurface,
    SurfaceSubsurfaceFlow_pct= case_when(
      is.na(hi_reachlengthsub) ==T ~ SurfaceFlow_pct, # Replace missing subsurface flow with surface flow
      T~hi_reachlengthsub),
    IsolatedPools_number= case_when(
      is.na(pools_observed) == T ~ 0, # Replace missing pool number with zero
      T~pools_observed),
    # MaxPoolDepth_calc = max_pool_depth_calc, # NESE
    # MaxPoolDepth = max_pool_depth,# NESE
    SeepsSprings_yn=hi_seepsspring,# NESE
    # SeepsSprings_inchannel = inchannel,# NESE
    # baseflowscore,# NESE
    # baseflow_notes,#NESE
    # LeafLitter_score = hi_leaflitter,# NESE
    # LeafLitter_notes = leafnotes,# NESE
    # ODL_score = hi_odl,# NESE
    # ODL_notes = odlnotes, # NESE
    WaterInChannel_score = case_when(is.na(hi_channelscore)~2*baseflowscore,
                                     T~hi_channelscore), # CALCULATE DUE TO MISSING FOR NESE (KSM)
    WaterInChannel_notes=  "Calculated as twice the value of NC indicator", # Not recorded for NESE
    HydricSoils_score=hydricsoils,
    HydricSoils_locations=locations, 
    HydricSoils_notes= hydricnotes,
    WoodyJams_number= case_when( # This will consolidate the two woody jams columns for NESE - needs to be fixed in consolidation script still
      is.na(woody_jams) & is.na(woodyjams1) ~0,
      is.na(woody_jams)  ~ woodyjams1,
      is.na(woodyjams1)  ~ woody_jams,
      T~0 # Replace missing wood jams count with zero
    ), 
    # WoodyJams_source=trees_shrubs,  # Not recorded for NESE
    WoodyJams_source=woody_material,  # Same as trees_shrubs in other databases
    # WoodyJams_notes=woodyjamsnotes,# Not recorded for NESE
    UplandRootedPlants_score= uplandrootedplants_score, # Both uplandrootedplants score and fiberousroots score ARE recorded for the NESE region (KSM)
    UplandRootedPlants_notes= uplandrootedplants_notes, 
    # FibrousRootedPlants_score= fibrous_rootscore, # Actually, fibrous roots for NESE in ununified database
    # FibrousRootedPlants_notes= fibrous_rootnotes, # Actually, fibrous roots for NESE in ununified database
    SedimentOnPlantsDebris_score= hi_debrisscore, 
    SedimentOnPlantsDebris_notes= debrisnotes,
    SoilMoisture1=locationonemoisture, 
    SoilMoisture2=locationtwomoisture,
    SoilMoisture3=locationthreemoisture,
    SoilTexture1=locationonetexture,
    SoilTexture2=locationtwotexture,
    SoilTexture3=locationthreetexture,
    # Continuity_score = gi_contbbscore, # NESE - Continuity  of channel and bank score
    # Continuity_notes = contbbnotes,# NESE
    # Depositional_score = gi_depbbscore,# NESE - Depositional bars and benches
    # Depositional_notes = depbbnotes,# NESE
    # AlluvialDep_score = gi_radscore,# NESE
    # AlluvialDep_notes = radnotes,# NESE
    # Headcut_score = gi_headcutscore,# NESE
    # Headcut_notes = headcutnotes,# NESE
    # GradeControl_score = gi_gcscore,# NESE
    # GradeControl_notes = gcnotes,# NESE
    # NaturalValley_score = gi_nvscore,# NESE
    # NaturalValley_notes = nvnotes, # NESE
    Sinuosity_score= si_sinuosityscore,
    Sinuousity_method= sinuositymethod,
    Sinuosiy_notes=sinuositycondition,
    # ActiveFloodplain_score = gi_afpscore,# NESE
    # ActiveFloodplain_notes = afpnotes,# NESE
    # fp_number,# NESE - number of locations assessed for bankfull width
    # fp_bankful,# NESE - Bankfull width, location 1
    # fp_floodprone,# NESE - Floodprone width, location 1
    # fp_greaterthan,# NESE - 2x bankfull width, location 1
    # fp_bankful2,# NESE
    # fp_floodprone2,#NESE
    # fp_greaterthan2,# NESE
    # fp_bankful3,# NESE
    # fp_floodprone3,# NESE
    # fp_greaterthan3,# NESE
    # ADDED THE CALCULATION OF ENTRENCHMENT SCORE (KSM)
    # fp_entrenchmentratio1= case_when(fp_greaterthan=="yes"~2.5,T~fp_floodprone/fp_bankful),
    # fp_entrenchmentratio2= case_when(fp_greaterthan2=="yes"~2.5,T~fp_floodprone2/fp_bankful2),
    # fp_entrenchmentratio3= case_when(fp_greaterthan3=="yes"~2.5,T~fp_floodprone/fp_bankful3),
    ChannelDimensions_method= "direct measurement",# Not recorded for NESE
    RifflePoolSeq_score= gi_sequencescore, 
    RifflePoolSeq_notes= seqnotes,
    SubstrateSorting_score=gi_substratesorting, 
    SubstrateSorting_notes=ginotes, 
    # Substrate_Bedrock = scsc_bedrock, #NESE
    # Substrate_Boulder = scsc_boulder,#NESE
    # Substrate_BoulderSlab = scsc_boulderslab,#NESE
    # Substrate_Cobble = scsc_cobble,#NESE
    # Substrate_Gravel = scsc_gravel,#NESE
    # Substrate_Sand = scsc_sand,#NESE
    # Substrate_Silt = scsc_silt,#NESE
    # Substrate_Clay = scsc_clay,#NESE
    # Substrate_Muck = scsc_muck,#NESE
    # Substrate_Leaf = scsc_leaf,#NESE
    # Substrate_Fine = scsc_fine,#NESE
    # Substrate_Artificial = scsc_artifical,#NESE
    # number_of_fish,#NESE
    all_mosqfish=mosquitofish,
    fishabund_note,#NESE
    Fish_score = abundancescorefish, # Not recorded for NESE
    #BMI_score= abundancescorebenthic,# Not recorded for NESE
    Algae_score= abundancealgae, 
    #Mosquitofish = mosquitofish, # Not recorded for NESE
    #Vertebrate_notes= observedabundancenote, # Not recorded for NESE
    IOFB_yn= observedfungi, 
    #Snakes_yn= observedsnakes, # Not recorded for NESE
    #Snakes_abundance= obsnakesabundance, # Not recorded for NESE
    #Turtles_yn= observedturtles, # Not recorded for NESE
    #Turtles_abundance=obturtlesabundance, # Not recorded for NESE
    #Amphibians_yn=observedamphibians, # Not recorded for NESE
    #Amphibians_abundance= obampiabundance, # Not recorded for NESE
    #FrogVocalizations_yn= observedvocalfrogs, # Not recorded for NESE
    #BiologicalIndicators_notes= observedabundancenote, # Not recorded for NESE
    AlgalCover_Live=streambedlive, 
    AlgalCover_Dead=streambeddead,
    AlgalCover_Upstream=streambeddeadmats,
    AlgalCover_notes= streambedalgaenotes, 
    dens_UU=u_upstream,
    dens_UL=u_left,
    dens_UR=u_right,
    dens_UD=u_downstream,
    dens_MU=m_upstream,
    dens_ML=m_left,
    dens_MR=m_right,
    dens_MD=m_downstream,
    dens_DU=l_upstream,
    dens_DL=l_left,
    dens_DR=l_right,
    dens_DD=l_downstream, 
    Moss_cover=bryophytemosses, 
    Liverwort_cover=bryophyteliverworts,
    Bryophyte_notes=bryophtyenotes,
    ironox_bfscore = case_when(IOFB_yn=="present"~1.5,IOFB_yn=="notdetected"~0, T~0), #NM scoring
    ironox_bfnotes,#NESE
    # fibrous_rootscore,#NESE
    # fibrous_rootnotes,#NESE
    DifferencesInVegetation_score=vegetationdifferencescore, 
    DifferencesInVegetation_notes=vegenotes, 
    NWPL_checklist= regionalindicators, 
    ai_fieldid, #NESE
    Additional_notes= additionalnotes,
    ChannelDimensions_score=gi_dimensionscore,
    # case_when(#NESE
    #   is.na(ancillarynote) & is.na(additionalnotes) ~ NA_character_,
    #   is.na(ancillarynote) ~additionalnotes, #Other regions have "ancillarynote" field instead of "additionalnotes"
    #   is.na(additionalnotes) ~ ancillarynote),
    hydrovegenote #NESE
  ) %>%
  rowwise() %>%
  # mutate(fp_entrenchmentratio_mean=mean(c(fp_entrenchmentratio1,fp_entrenchmentratio2,fp_entrenchmentratio3), na.rm=T),
  # fp_entrenchmentratio_mean=case_when(fp_entrenchmentratio_mean>2.5~2.5, T~fp_entrenchmentratio_mean),
  ####NEED TO FIX FOR NON NESE
  # ChannelDimensions_score=#case_when(fp_entrenchmentratio_mean<1.2~0,
  #         fp_entrenchmentratio_mean<2.5~1.5,
  #        fp_entrenchmentratio_mean>=2.5~3,
  # T~NA_real_)
  # ) %>%
  ungroup() %>%
  #Excluded sites
  filter(SiteCode!="NOT RECORDED") %>%
  filter(!SiteCode %in% c("NVAW1193",
                          "NVAW1190",
                          "COAW0781",
                          "CAAW0247")) 





###############################################

# More fixes for main dataset
main_combined <- main_df %>%
  mutate(
    Slope = case_when( #These samples are missing slope. Replace with mean from other visits
      ParentGlobalID == "fe4ad980-1c2f-4aba-94a5-aed695182b42" ~ 10, # Replace with 10; same value recorded for other two samples; NYNE9549_B
      ParentGlobalID == "930118c4-e434-4dfa-ad69-169b42965184" ~ 1,# Replace with 1; same value recorded for other two samples; NYNE9030_B
      #ParentGlobalID == "19652a45-8c2a-4f8c-a8db-351c69f0d016" ~ ?, # No other samples for this validation site. NYNE4129_V - 2021-05-26
      ParentGlobalID == "ba3b1308-4d70-4e0b-bc76-cb2619a90ec0" ~ (1+2)/2,# Replace with average of other two samples; FLSE9524_B
      ParentGlobalID == "d9e9fbc3-298a-44ba-b70e-80effc6d49f5" ~ 2,# Replace with average of other two samples; FLSE9523_B
      ParentGlobalID == "e945ad5e-aa36-49de-af18-76fae6c7845e" ~ (3+2)/2,# Replace with average of other two samples; FLSE9508_B 
      #ParentGlobalID == "1b890a44-be00-49cb-b7ac-bbfc6f15fb59" ~ ?, # No other samples for this validation site. ARNE125_V - 2021-06-30
      T ~ Slope   ),
    SeepsSprings_yn = case_when(
      SeepsSprings_yn == "notdectected" ~ "notdetected", #misspelling
      is.na(SeepsSprings_yn) ~ "notdetected", #Assume not detected if not reported
      SeepsSprings_yn == "na" ~ "notdetected", #Assume not detected if not reported 
      ParentGlobalID == "e945ad5e-aa36-49de-af18-76fae6c7845e" ~ "notdetected", # Missing for one sample - FLSE9508_B, 10/19/2021, revisits v2
      T~SeepsSprings_yn),
    SeepsSprings_inchannel = case_when(
      SeepsSprings_yn == "notdetected" ~ "no",
      is.na(SeepsSprings_inchannel) ~ "no",
      SeepsSprings_inchannel=="na"~"no",
      T~SeepsSprings_inchannel),
    Landuse = case_when(
      ParentGlobalID == "3bd11db2-be7a-4c8d-8002-6f0d63ab82b5" ~ "forested", # VANE8897_B, 2020-11-19 - all other samples say forested landuse
      T~ Landuse),
    HydricSoils_score = case_when(
      ParentGlobalID == "6c0625a5-e3b0-4fb5-8dca-1caaea8866aa" ~ NA_character_, # FLSE9551_B, 10/17/2021 - no notes or anything, see what field crews say
      T~ HydricSoils_score),
    HydricSoils_locations = case_when(
      HydricSoils_score == "not_assessed" ~ 0,
      T ~ HydricSoils_locations),
    #dens_UD = case_when(
    # ParentGlobalID == "f1605b27-9b15-4d78-800f-69bc04ec9c6b" ~ 17, # MENE4957_V, 12/15/2021 - all other densiometer measurments were 17 for this sample 
    #T~dens_UD),
    #SubstrateSorting_score = case_when(
    # ParentGlobalID == "" ~ , #
    #T~SubstrateSorting_score),
    #RifflePoolSeq_score = case_when(
    # ParentGlobalID == "" ~ , #
    #ParentGlobalID == "" ~ , #
    #T~RifflePoolSeq_score),
    #SedimentOnPlantsDebris_score = case_when(
    # ParentGlobalID == "" ~ ,#
    #ParentGlobalID == "" ~ , #
    #T~SedimentOnPlantsDebris_score),
    WoodyJams_number = case_when(
      is.na(WoodyJams_number) ==T ~ 0, # Replace missing (29 samples) wood jam count with zero
      T~WoodyJams_number)#,
    #ODL_score = case_when(
    # ParentGlobalID == "b0e96530-75a0-46d8-b6c1-00472781558a" ~ NA_real_, #SCSE8343_B, 2020-12-14 - No information about why it's missing
    #ParentGlobalID = "938c28ad-6ae2-475d-ba7f-01cee11116e2" ~ 0.5, # TNNE8765_B, 2021-04-04 - "0.5" present in the notes column, but this might just be duplicated from the next field over
    #ParentGlobalID = "6c0625a5-e3b0-4fb5-8dca-1caaea8866aa" ~ NA_real_, #FLSE9551_B, 2021-10-17 - No information available 
    #T ~ ODL_score),
    #LeafLitter_score = case_when(
    # ParentGlobalID == "" ~ , #
    #ParentGlobalID == "" ~ , #
    #T~ LeafLitter_score),
    #baseflowscore = case_when(
    # ParentGlobalID == "" ~ , #
    #T~ baseflowscore),
    #MaxPoolDepth = case_when(
    # ParentGlobalID == "" ~ , #
    #ParentGlobalID == "" ~ , #
    #ParentGlobalID == "" ~ , #
    #ParentGlobalID == "" ~ , #
    #T~MaxPoolDepth),
    #ReachLength_actual = case_when(
    # ParentGlobalID == "" ~ , #
    #T~ReachLength_actual),
    #Bankwidth_0 = case_when(
    # ParentGlobalID == "" ~ , #
    #T~ Bankwidth_0),
    #Bankwidth_15 = case_when(
    # ParentGlobalID == "" ~ , #
    #T~ Bankwidth_15),
    #Bankwidth_30 = case_when(
    # ParentGlobalID == "" ~ , #
    #T~ Bankwidth_30)
  )


#Missing entrenchment ratios estimated by examination of photos and consultation of field crews. Did not update Survey123 data.
#GASE9180_B
main_combined$ChannelDimensions_score[main_combined$ParentGlobalID=="751abafb-e40b-46c9-a336-56c69d2690e2"]<-3
main_combined$ChannelDimensions_method[main_combined$ParentGlobalID=="751abafb-e40b-46c9-a336-56c69d2690e2"]<-"Estimated from photos, other visits"
main_combined$fp_entrenchmentratio_mean[main_combined$ParentGlobalID=="751abafb-e40b-46c9-a336-56c69d2690e2"]<-2.5
#MDNE3514_B
main_combined$ChannelDimensions_score[main_combined$ParentGlobalID=="2f2cabe0-5bcc-483e-bd3e-ca44dce42cfc"]<-1.5
main_combined$ChannelDimensions_method[main_combined$ParentGlobalID=="2f2cabe0-5bcc-483e-bd3e-ca44dce42cfc"]<-"Estimated from photos, other visits"
main_combined$fp_entrenchmentratio_mean[main_combined$ParentGlobalID=="2f2cabe0-5bcc-483e-bd3e-ca44dce42cfc"]<-2
#NCSE9079_B
main_combined$ChannelDimensions_score[main_combined$ParentGlobalID=="3b4334ca-bf77-4545-922d-116da75e3e0b"]<-1.5
main_combined$ChannelDimensions_method[main_combined$ParentGlobalID=="3b4334ca-bf77-4545-922d-116da75e3e0b"]<-"Estimated from photos, other visits, and consultation with field crew"
main_combined$fp_entrenchmentratio_mean[main_combined$ParentGlobalID=="3b4334ca-bf77-4545-922d-116da75e3e0b"]<-2
#NHNE8905_V
main_combined$ChannelDimensions_score[main_combined$ParentGlobalID=="4f667856-c8d1-4ea3-aef2-7d79c8ef91a4"]<-3
main_combined$ChannelDimensions_method[main_combined$ParentGlobalID=="4f667856-c8d1-4ea3-aef2-7d79c8ef91a4"]<-"Estimated from photos and consultation with field crew"
main_combined$fp_entrenchmentratio_mean[main_combined$ParentGlobalID=="4f667856-c8d1-4ea3-aef2-7d79c8ef91a4"]<-2.5
#OHNE8437_B
main_combined$ChannelDimensions_score[main_combined$ParentGlobalID=="{9F61EDAB-4D5C-40D0-8382-EF39C19D1310}"]<-1.5
main_combined$ChannelDimensions_method[main_combined$ParentGlobalID=="{9F61EDAB-4D5C-40D0-8382-EF39C19D1310}"]<-"Estimated from photos, other visits"
main_combined$fp_entrenchmentratio_mean[main_combined$ParentGlobalID=="{9F61EDAB-4D5C-40D0-8382-EF39C19D1310}"]<-NA_real_
#OHNE9125_B
main_combined$ChannelDimensions_score[main_combined$ParentGlobalID=="b4ea0ed9-7aeb-470a-a8a9-558846a1052a"]<-1.5
main_combined$ChannelDimensions_method[main_combined$ParentGlobalID=="b4ea0ed9-7aeb-470a-a8a9-558846a1052a"]<-"Estimated from photos, other visits"
main_combined$fp_entrenchmentratio_mean[main_combined$ParentGlobalID=="b4ea0ed9-7aeb-470a-a8a9-558846a1052a"]<-2
#OHNE9126_B
main_combined$ChannelDimensions_score[main_combined$ParentGlobalID=="423ae7ca-8dad-4ff1-8a8c-c88472a4c7af"]<-1.5
main_combined$ChannelDimensions_method[main_combined$ParentGlobalID=="423ae7ca-8dad-4ff1-8a8c-c88472a4c7af"]<-"Estimated from photos, other visits"
main_combined$fp_entrenchmentratio_mean[main_combined$ParentGlobalID=="423ae7ca-8dad-4ff1-8a8c-c88472a4c7af"]<-2



#### XWALK to "true" determinations ####
names(main_combined)
nese_determinations<-read_csv("NESEbetaclass_10062022.csv") %>%
  select(SiteCode=Site.Code,
         Instrument_Single=`Instrum/Single`,
         Determination_final=Class)
xwalk<-read_csv("xwalk_nese_step0.csv") %>%
  filter(Usability=="OK") %>%
  left_join(nese_determinations) %>%
  mutate(Determination_final=case_when(Determination_final=="Perennial"~"P",
                                       Determination_final=="Intermittent"~"I",
                                       Determination_final=="Ephemeral"~"E",
                                       is.na(Determination_final)~"U",
                                       T~"U"))

xwalk %>%
  select(SiteCode, REGION, Determination_final) %>%
  unique() %>%
  group_by(REGION, Determination_final) %>% tally() %>%
  pivot_wider(names_from=Determination_final, values_from =n )

#####
#AMPHIBIANS
amphib_df<-read_csv("https://sdamchecker.sccwrp.org/checker/download/amphibians-all") %>%
  filter(origin_database %in% myDBs) %>%
  transmute(
    Download_date = Sys.time(),
    Database=origin_database,
    ParentGlobalID = parentglobalid,
    AmphibGlobalID=globalid,
    SiteCode = am_sitecode, 
    Amphib_Species=amphibians_taxon, 
    Amphib_LifeStage=amphibians_lifestage, 
    Amphib_Abundance=amphibians_abundance, 
    Amphib_Notes=amphibians_notes
  )
amphib_combined<-amphib_df 

#### HYDROVEG - Rename columns and filter for GP data in hydroveg dataset ####
hydroveg_df<- 
  read_csv("https://sdamchecker.sccwrp.org/checker/download/hydroveg-all") %>%
  filter(origin_database %in% myDBs) %>%
  transmute(
    Download_date = Sys.time(),
    Database=origin_database,
    ParentGlobalID = parentglobalid,
    VegGlobalID=globalid,
    SiteCode = hv_sitecode, 
    Plant_Species=hv_species, 
    Plant_Status=indicatorstatus, 
    Plant_flag=unusualdistro,
    InChannel = hv_inchannel, #NESE
    Plant_notes=hv_notes
  )

# Manual changes to vegetation data

veg_combined <- hydroveg_df %>%
  mutate(Plant_Species = str_trim(Plant_Species)) %>% # Remove carriage returns, leading white space and trailing white space
  mutate(Plant_Species = case_when(
    Plant_Species %in% c("NR","?") ~ "No plant",
    T~Plant_Species
  )) %>%
  mutate(Plant_Status = case_when(
    is.na(Plant_Status) == T ~ "nonhydrophyte",
    T~ Plant_Status),
    Plant_flag = case_when(
      is.na(Plant_flag) == T ~ "no",
      T~Plant_flag
    ))

#### AQUATIC INVERTEBRATES ####
ai_df<-
  read_csv("https://sdamchecker.sccwrp.org/checker/download/aquatic_invertebrates-all") %>%
  filter(origin_database %in% myDBs) %>%
  transmute(
    Download_date = Sys.time(),
    Database=origin_database,
    ParentGlobalID = parentglobalid,
    AIGlobalID=globalid,
    SiteCode = ai_sitecode, 
    AI_Taxon=taxon,
    AI_Lifestage=lifestage,
    AI_LiveDead=ai_living, 
    AI_Abundance=case_when(ai_abundance<0~NA_real_, T~ai_abundance), #Negative values are actually blanks
    AI_Notes= ai_notes) %>%
  filter(AI_Taxon != "Na" & AI_Taxon != "na" & !(is.na(AI_Taxon)))

# CLEANING BASED ON CURRENT AND PRIOR DATASETS

ai_combined <- ai_df %>%
  mutate(AI_Taxon = trimws(AI_Taxon,whitespace = "[ \t\r\n]")) %>% # Remove trailing and leading white spaces
  mutate(
    AI_Taxon2=case_when(#This is a "cleaned" name based on Taxon 1, but reflects no change to taxonomic resolution
      AI_Taxon == "Simulliidae" ~ "Simuliidae", 
      AI_Taxon %in% c("Small red mite","Mite","Red mite")~"Acariformes",
      AI_Taxon %in% c("Aeshniidae","Aeshindae",
                      "Odonata aeshnidae",
                      "aeshnidae", 
                      "Aeshinidae")~"Aeshnidae", 
      AI_Taxon %in% c("Arachniddae", # NESE
                      "Achnidae")~"Arachnida",
      AI_Taxon %in% c("Ascellidae","asselidae",
                      "asellidae")~"Asellidae",
      AI_Taxon %in% c("Scuds",
                      "amphipoda", "Amphopoda","Amphipod sp","Amphipod spp",
                      "Amphipod spp.","Amphipodae",
                      "Amohipoda")~"Amphipoda", 
      AI_Taxon %in% c("Odonata Antisoptera", 
                      "Anisoptera sp")~"Anisoptera",
      
      AI_Taxon %in% c("Ephemeroptera Ameletidae")~"Ameletidae", 
      AI_Taxon %in% c("Hemiptera belostomatida",
                      "Hemiptera belostomatidae")~"Belostomatidae", 
      AI_Taxon %in% c("Bastiscidae","Baetiscidae")~"Baetiscidae",
      AI_Taxon %in% c("belostomaridae")~"Belostomaridae",
      
      AI_Taxon %in% c("Caecidotea sp")~"Caecidotea",
      AI_Taxon %in% c("Canidae")~"Caenidae",
      AI_Taxon %in% c("Calopterygidae (Calopteryx)","calopterygidae")~"Calopteryx",
      #AI_Taxon %in% c("ceratapogonidae","Ceratopogonidae","Ceratoponidae")~"Ceratapogonidae", # THIS WAS SPELLED WRONG
      AI_Taxon %in% c("ceratapogonidae","Ceratapogonidae","Ceratoponidae")~"Ceratopogonidae", # Corrected the above spelling 9/26 (KSM)
      AI_Taxon %in% c("Copapod","Copapods")~"Copepoda",
      AI_Taxon %in% c("Beetle","Unknown Coleoptera", "Coleoptera + Coleoptera larvae","Coleoptera exuvia","Coleoptera larva","Coleoptera larva?","Coleoptera larvae",
                      "Part of Coleoptera larvae", "Coleoptera spp unknown","Coleoptera spp","Hemiptera and Coleoptera","Small black beetle- coleoptera",
                      "Small beetle with ridges", "Small ridged beetle- coleoptera","Small black beetle",
                      "Unknown beetle")~"Coleoptera",
      
      AI_Taxon %in% c("Bivalve spp?","Bivalves")~"Bivalve",
      AI_Taxon %in% c("Brachycentidae",
                      "Trichoptera brachycentridae",
                      "brachycentridae")~"Brachycentridae",
      AI_Taxon %in% c("Bulk", "Bulk 1", "Bulk 2","bulk sample","Bulk sample", "Bulk Sample","BULK SAMPLE","Bulk sample #1",
                      "Bulk sample #2","Bulk sample #3","Bulk sample #4","Bulk sample 1","Bulk Sample 1","Bulk sample 2", "Bulk sample1" )~"Bulk sample",
      
      AI_Taxon %in% c("Blood worm","Chironomid", "Diptera chironomidae","Diptera, Red midge","Red midge","Red midges",
                      "Diptera- midge","Diptera chironomidae","Diptera, Red midge","Diptera -midge","Midge larvae",
                      "Midge spp unknown","Midge larvae","Midge?","Midges","Small black worm/ midge","Tan midge", 
                      "Diptera spp","Diptera spp.","ChironomidaeIAUVIA","Chironimidae","Chironomidae (pupae)",
                      "Chironomiidae","Chrinonomidae",
                      "chironomidae")~"Chironomidae", # Added 9/26 KSM
      
      AI_Taxon %in% c("Trichoptera calamoceratidae" )~"Calamoceratidae",
      AI_Taxon %in% c("Odonata calopterygidae", "Calyopterigidae")~"Calopterygidae", 
      AI_Taxon %in% c("Coenegrionidae",
                      "Odonata Coenagriondae", "Zygoptera (Coenagrionidae)","Coenagrinidae","Coengarionidae")~"Coenagrionidae", 
      AI_Taxon %in% c("Corduligastridae",
                      "Odonata cordulegastridae",
                      "Cordulegasridae")~"Cordulegastridae", 
      AI_Taxon %in% c("Caddis fly","Caddis pupa", "Caddis pupae", "Caddisdly","Caddisflies","Caddisfly", "Pupa and trichoptera case","Possibly a caddisfly",
                      "Trichoptera and cases","Trichoptera case","Trichoptera cases","Trichoptera cases ?","Trichoptera cases and unknown larva",
                      "Trichoptera cases?3","Trichoptera casings","Trichoptera pupae","Tricoptera","Woody case","Large clear amber casing, spp unkn",
                      "trichoptera", "Trichoptera sp", "trichoptera sp (empty case)",
                      "Trichoptera sp (pupae in case)","Unknown caddis", 
                      "Unknown Caddis 1", "Unknown caddis 2",
                      "trichoptera case", 
                      "Dipseudopsidae", # Added 9/26 KSM - Likely not in SAFIT but reported for one NESE sample
                      "Unknown cased caddis", "Cased caddis")~"Trichoptera", 
      
      AI_Taxon %in% c( "Clam (corbela?)","Corbicula","Fingernail clam",
                       "Corbivula")~"Corbiculidae",
      AI_Taxon %in% c( "Crawfish","Crayfish","Lobster species")~"Cambaridae",
      AI_Taxon %in% c( "Crustacean arm","Crustacean",
                       "Anostraca")~"Crustacea", # Added 9/26 KSM
      AI_Taxon %in% c( "Cullicidae","Mosquitos",
                       "Cuclicidae")~"Culicidae",
      AI_Taxon %in% c( "Diptera -unknown spp","Dipterans","Fly larvae", "Unknown diptera",
                       "Unknown dipteran", "Unknown fly larva")~"Diptera",
      AI_Taxon %in% c( "Diptera dixidae")~"Dixidae",
      AI_Taxon %in% c( "Diptera simuliidae","Diptera: black fly larvae", "Simulidae", "Sinubidae",
                       "Simullidae")~"Simuliidae", 
      AI_Taxon %in% c( "Dobsonfly")~"Corydalidae",
      AI_Taxon %in% c( "Corydalidae - Orohermes","Corydalidae (Oreohermes-Dysmicohermes group)",
                       "Corydalidae (orohermes group)","Corydalidae orohermes",
                       "Orohermes")~"Corydalidae (Orohermes-Dysmicohermes group)", # Added 9/26 KSM 
      AI_Taxon %in% c( "Corydalidae (Corydalus)", "Corydalus sp.", 
                       "Corydalis")~"Corydalus", #NESE
      AI_Taxon %in% c( "Corydalidae (Nigronia)","Corydalidae nigronia")~"Nigronia", 
      
      AI_Taxon %in% c( "Ephemeroptera-Ephemerellidae",
                       "Emphemerellidae", "Ephemerelidae",
                       "Ephemeroptera ephemerellidae", 
                       "Ephemerrellidae")~"Ephemerellidae", 
      
      AI_Taxon %in% c( "Elmidae (adult)","Coleoptera elmidae")~"Elmidae", 
      AI_Taxon %in% c( "Ephermeridae")~"Ephemeridae", 
      
      AI_Taxon %in% c( "Ephemeroptera exuvia","Ephemeroptera spp","Ephemeroptera spp","Ephemeroptera spp unkn","Mayfly, non-baetidae 1","Mayfly, non-Baetidae 2",
                       "Ephemeroptera? Exuvia","Mayflies","Mayfly","Small mayfly with gills",
                       "Ephemeroptera sp", "ephemeroptera sp",
                       "Unknown mayfly")~"Ephemeroptera",
      
      AI_Taxon %in% c( "Ephemeroptera heptagiidae",
                       "Ephemeroptera heptageniidae")~"Heptageniidae", 
      AI_Taxon %in% c( "Fallceon and Baetis adonis","Fallceon and Baetis adonis and Tricorythodes",
                       "Ephemeroptera Baetidae", "baetidae","Baetiidae")~"Baetidae", 
      AI_Taxon %in% c( "Garridae","Water strider","Water striders",
                       "Hemiptera gerridae")~"Gerridae", 
      AI_Taxon %in% c( "Gastropoda shell","Gastropoda Shell","Gastropoda shells","Spiral snail","Snail","Snail 1",
                       "Snail 2","Snail/limpet?","Snails")~"Gastropoda", 
      AI_Taxon %in% c( "Glossosomelidae")~"Glossosomatidae", 
      AI_Taxon %in% c( "Glossosomatidae (Glossoma)")~"Glossoma", 
      AI_Taxon %in% c( "Gomphiidae", "Odonata Gomphidae","gomphidae")~"Gomphidae", 
      AI_Taxon %in% c( "Gryinidae","Coleoptera Gynnidae")~"Gyrinidae", 
      AI_Taxon %in% c( "Halipidae")~"Haliplidae", 
      AI_Taxon %in% c( "Tricoptera-glossosomatidae")~"Glossosomatidae",
      AI_Taxon %in% c( "Helicopsycidae")~"Helicopsychidae",
      AI_Taxon %in% c( "Hydropsyche occidentalis",
                       "Hydro physidae", "Hydrophychidae", "Hydrophycidae","Hydrophyschidae",
                       "Hydropsididae","Hydrosychidae", "Hyrdopsychidae", "Trichoptera Hydropyschidae",
                       "Trichoptera hydrosychidae")~"Hydropsychidae",
      AI_Taxon %in% c( "Hyallelidae")~"Hyalellidae", 
      AI_Taxon %in% c( "Hydrobiid")~"Hydrobiidae", 
      AI_Taxon %in% c( "Hemipteran")~"Hemiptera",
      AI_Taxon %in% c( "Heptegeniidae",
                       "Haptageniidae","Heptagenidae","Heptigeniidae")~"Heptageniidae",
      AI_Taxon %in% c( "Large dytiscidae","Predacious diving beetle","Small dytiscidae",
                       "Dyticidae", "Dytiscid", "Dytiscidae (adult)",
                       "Dytiscidae (larva)","Dytiscidae (larvae)",
                       "Coleoptera dystiscidae","Coleoptera Dytiscidae")~"Dytiscidae",
      AI_Taxon %in% c( "Leach",
                       "Hirudinae", "Hirundinea")~"Hirudinea", 
      AI_Taxon %in% c( "Isonychidae")~"Isonychiidae", 
      AI_Taxon %in% c( "Isopod","Isopods")~"Isopoda", 
      AI_Taxon %in% c( "Lepidodoptera","Lepidoptera (larvae)")~"Lepidoptera", 
      AI_Taxon %in% c( "Leptoceridae (Ceraclea)")~"Ceraclea", 
      AI_Taxon %in% c( "Leptophleniidae")~"Leptophlebiidae", 
      AI_Taxon %in% c( "Libuellidae")~"Libellulidae", 
      AI_Taxon %in% c( "Limnaeidae","lymaeidae","Lymnadae","Lymnaedae","Lymnaeid",
                       "Lymnaidae")~"Lymnaeidae", 
      AI_Taxon %in% c( "Limnphild","Lmniphild","Limephilidae","Lmniphilid","Lmniphillid","Limnephilidae/phryganeidae",
                       "Limnophilidae","Limnephillidae","Tricoptera limnephilidae")~"Limnephilidae",
      AI_Taxon %in% c("Mysida")~"Mysidae",
      AI_Taxon %in% c( "Limnephilidae (Onocosmoecus)")~"Onocosmoecus", 
      
      AI_Taxon %in% c( "Hydrophyilidae",
                       "Hydrophildae", "Hydrophilidae (adult)", "Hydrophilidae (larva)",
                       "Hydrophillidae","Hydrophylidae")~"Hydrophilidae", 
      AI_Taxon %in% c( "Hydrachmida","Hydrachnida","Hydracarina" )~"Hydrachnidia",
      
      AI_Taxon %in% c( "N/A","NA", "No bugs","No Bugs", "NO BUGS","No invertebrates","No taxon found","None","No invertebrates observed",
                       "None found.","None seen","None." ,"No invertebrates observed",
                       "No aquatic invertebrates observed", "No benthic macroinvertebrates",
                       "No benthic macroinvertebrates.","No aquatic invertebrates",
                       "No Invertebrates", "No inverts found", # NESE
                       "None observed", "none", "Unknown")~"No invertebrates observed", #NESE
      AI_Taxon %in% c( "Nemouriidae" )~"Nemouridae",
      AI_Taxon %in% c( "Nepidae (Ranatra)" )~"Ranatra", 
      AI_Taxon %in% c( "Notonectidae  spp",
                       "Nonectidae")~"Notonectidae", 
      AI_Taxon %in% c( "Nauchoridae",
                       "Naucordiae")~"Naucoridae",
      AI_Taxon %in% c(  "Odoceridae", "Odontocenidae")~"Odontoceridae", 
      AI_Taxon %in% c( "Odanata","Odanata spp","Odonata","Odonata exuvia","Odonata spp","Odonata spp unknown",
                       "Odonate","Odonates",
                       "Libellulidae/cordulidae", "Libellulidae/Corduliidae", "Odonata sp.",
                       "Odonata sp", "Unknown zygoptera")~"Odonata",
      AI_Taxon %in% c(  "Oligicheates","Oligochaete","Oligochete","Oligachaete",
                        "Oligiochaeta","Oligochaeta annelida",
                        "Annelida oligochaeta")~"Oligochaeta",
      AI_Taxon %in% c(  "Ostracod","Ostrapoda" )~"Ostracoda", 
      AI_Taxon %in% c( "Palaemonidae" )~"Palaemonidae", 
      AI_Taxon %in% c( "Parastacidae" )~"Parastacidae", 
      AI_Taxon %in% c( "Pelodidae", "Perlodid?" )~"Perlodidae",
      AI_Taxon %in% c( "Philopotemidae" )~"Philopotamidae",
      AI_Taxon %in% c( "Physid","Phyidae","Physdiae","plysidae" )~"Physidae",
      AI_Taxon %in% c( "Pisidae", "Pisiidae", "Pisidiidae","Sphaeridae", 
                       "Sphaerridae", 
                       "Pisidium")~"Sphaeriidae", # Added 9/26 KSM
      AI_Taxon %in% c("Planorbid")~"Planorbidae",
      AI_Taxon %in% c("Platyhelmenthes")~"Platyhelminthes",
      AI_Taxon %in% c( "Plecoptera exuvia", "Stonefly","Stoneflies","Stonefly?",
                       "Stoneflys" ,"Plecoptera spp")~"Plecoptera",
      AI_Taxon %in% c( "Plecoptera: Golden stonefly?","Pearlidae" ,
                       "Plecoptera perlidae")~"Perlidae",
      AI_Taxon %in% c( "Pleoroceridae","Pleuroceridae" )~"Pleuroceridae",
      AI_Taxon %in% c("polycentropodidae","Polycentropidae" )~"Polycentropodidae",
      AI_Taxon %in% c("Polycentropodidae (Nyctiophylax)" )~"Nyctiophylax",
      AI_Taxon %in% c("Polymitarcyidae" )~"Polymitarcyidae",
      AI_Taxon %in% c("Pleidae (Neoplea)" )~"Neoplea",
      AI_Taxon %in% c( "Psenidae","Psephidae","Psenphenidae")~"Psephenidae",
      AI_Taxon %in% c("Rhyalophilidae")~"Rhyacophilidae",
      AI_Taxon %in% c( "Siphlonuridae?" )~"Siphlonuridae",
      AI_Taxon %in% c( "Megaloptera sialidae" )~"Sialidae",
      AI_Taxon %in% c( "Sialidae (Sialis)" )~"Sialis",
      AI_Taxon %in% c( "Syrphidae" )~"Syrphidae",
      AI_Taxon %in% c( "Taberidae" )~"Tabanidae",
      AI_Taxon %in% c( "Tipuliidae","tipulidae" )~"Tipulidae",
      AI_Taxon %in% c( "Tubellaria","Turbellaria platyhelminthes" )~"Turbellaria",
      AI_Taxon %in% c( "Tricorythidae (Tricorythodes)" )~"Tricorythodes",
      AI_Taxon %in% c(  "Thienemannimyia group", "Thinemannimyia group" )~"Thienemannimyia group",
      AI_Taxon %in% c( "Tricorythidae" )~"Tricorythidae",
      AI_Taxon %in% c( "Water boatman","Water boatmen","Corixicae",
                       "Hemiptera corixidae","Coxidae")~"Corixidae", 
      AI_Taxon %in% c("Trichoptera Sericostomatidae" )~"Sericostomatidae",
      AI_Taxon %in% c( "Worms" ,"Annelidae", "Annelid", "Annelida")~"Annelida",
      AI_Taxon %in% c( "Insect exuviae unknown","Exuvia?","Unknown bugs" ,
                       "Unknown exuvia", "Unknown invert" ,"Unknown inverts" ,
                       "Unknown larvae" ,"Unknown pupa", "unknown spp" ,
                       "Unknown spp" ,"Pupa?","Unknown 1", "Unknown insect larvae",
                       "Unknown1", "Unknown2")~"Insecta",
      AI_Taxon %in% c("Vivaparidae" )~"Viviparidae",
      AI_Taxon %in% c("Antocha","Hexatoma","Limnophila", "Pseudolimnophila")~"Limoniidae", # Added 9/26 KSM
      AI_Taxon %in% c("Cecidomyiidae")~"Nematocera", # Added 9/26 KSM
      T~AI_Taxon),
    
    AI_Family= case_when( #This aggregates Taxon2 to best Family name, or higher if necessary. 
      # No sub-family names should remain
      # All caps if not to desired level
      AI_Taxon2 %in% c("Arachnida")~"Arachnida",
      AI_Taxon2 %in% c("Arthropoda")~"ARTHROPODA",
      AI_Taxon2 %in% c("Asellidae")~"Asellidae",
      AI_Taxon2 %in% c("Abedus","Belostomatidae")~"Belostomatidae",
      AI_Taxon2 %in% c("Acariformes","Sperchon",
                       "Hydrachnidia", "Hydrachnidae",
                       "Acari")~"Acariformes",  # ADDED 9/26 KSM
      AI_Taxon2 %in% c("Aeshnidae")~"Aeshnidae",
      AI_Taxon2 %in% c("Agabus")~"Dytiscidae",
      AI_Taxon2 %in% c("Ameletidae")~"Ameletidae",
      AI_Taxon2 %in% c("Hyalella","Amphipoda" )~"Amphipoda",
      AI_Taxon2 %in% c("Anisoptera", "Odonata","Zygoptera")~"ODONATA",
      AI_Taxon2 %in% c("Erpobdellidae","Hirudinea","Annelida","Oligochaeta" )~"Annelida",
      AI_Taxon2 %in% c("Argia","Ceonagrionidae", "Coenagrionidae")~"Coenagrionidae",
      AI_Taxon2 %in% c("Athericidae")~"Athericidae",
      AI_Taxon2 %in% c("Baetidae","Baetis adonis","Fallceon")~"Baetidae",
      AI_Taxon2 %in% c("Baetiscidae")~"Baetiscidae",
      AI_Taxon2 %in% c("Bivalve")~"BIVALVE",
      AI_Taxon2 %in% c("Blephariceridae")~"Blephariceridae",
      AI_Taxon2 %in% c("Brachycentridae")~"Brachycentridae",
      AI_Taxon2 %in% c("Bulk sample")~"BULK",
      AI_Taxon2 %in% c("Caenidae")~"Caenidae",
      AI_Taxon2 %in% c("Cambaridae")~"Cambaridae",
      AI_Taxon2 %in% c("Cheumatopsyche", "Hydropsychidae")~"Hydropsychidae", 
      AI_Taxon2 %in% c("Chironomidae", "Thienemannimyia group")~"Chironomidae",
      AI_Taxon2 %in% c("Coleoptera")~"COLEOPTERA",
      AI_Taxon2 %in% c("Corbiculidae")~"Corbiculidae",
      AI_Taxon2 %in% c("Cordulegastridae")~"Cordulegastridae",
      AI_Taxon2 %in% c("Corixidae")~"Corixidae",
      AI_Taxon2 %in% c("Crustacea" )~"CRUSTACEA",
      AI_Taxon2 %in% c("Culicidae" )~"Culicidae",
      AI_Taxon2 %in% c("Daphnia","Cladocera" )~"Cladocera",
      AI_Taxon2 %in% c("Diptera" )~"DIPTERA",
      AI_Taxon2 %in% c("Dixidae" )~"Dixidae",
      AI_Taxon2 %in% c("Dryopidae",
                       "Helichus")~"Dryopidae",
      AI_Taxon2 %in% c("Dugesia","Platyhelminthes",
                       "Turbellaria" )~"Turbellaria",
      AI_Taxon2 %in% c("Dytiscidae" )~"Dytiscidae",
      AI_Taxon2 %in% c("Elmidae" )~"Elmidae",
      AI_Taxon2 %in% c("Ephemerellidae" )~"Ephemerellidae",
      AI_Taxon2 %in% c("Ephemeroptera" )~"EPHEMEROPTERA",
      AI_Taxon2 %in% c("Gastropoda" )~"GASTROPODA",
      AI_Taxon2 %in% c("Gerridae" )~"Gerridae",
      AI_Taxon2 %in% c("Glossosomatidae" )~"Glossosomatidae",
      AI_Taxon2 %in% c("Gomphidae" )~"Gomphidae",
      AI_Taxon2 %in% c("Haliplidae" )~"Haliplidae",
      AI_Taxon2 %in% c("Helicopsychidae" )~"Helicopsychidae",
      AI_Taxon2 %in% c("Hemiptera",
                       "Heteroptera","Hydrometridae")~"HEMIPTERA", # Added 9/26 KSM
      AI_Taxon2 %in% c("Heptageniidae" )~"Heptageniidae",
      AI_Taxon2 %in% c("Hydrobiidae", "Potamopyrgus antipodarum" )~"Hydrobiidae",
      AI_Taxon2 %in% c("Hydrophilidae","Tropisternus" )~"Hydrophilidae",
      AI_Taxon2 %in% c("Isonychiidae" )~"Isonychiidae", 
      AI_Taxon2 %in% c("Insecta" )~"INSECTA",
      AI_Taxon2 %in% c("Isoperla denningi","Perlodidae",
                       "Isoperla")~"Perlodidae", # Added by KSM - NOTE, these taxa were originally reported as Isoperlidae
      AI_Taxon2 %in% c("Lepidoptera")~"LEPIDOPTERA",
      AI_Taxon2 %in% c("Lepidostoma","Lepidostomatidae" )~"Lepidostomatidae",
      AI_Taxon2 %in% c("Leptohyphidae", "Tricorythodes",
                       "Tricorythidae")~"Leptohyphidae",
      AI_Taxon2 %in% c("Leptophlebiidae" )~"Leptophlebiidae",
      AI_Taxon2 %in% c("Lestidae" )~"Lestidae",
      AI_Taxon2 %in% c("Limnephilidae",
                       "Onocosmoecus" )~"Limnephilidae", 
      AI_Taxon2 %in% c("Mysidacea" )~"Mysidacea",
      AI_Taxon2 %in% c("Muscidae" )~"Muscidae",
      AI_Taxon2 %in% c("Nepidae", "Ranatra" )~"Nepidae", 
      AI_Taxon2 %in% c("Naucoridae" )~"Naucoridae",
      AI_Taxon2 %in% c("Nemouridae" )~"Nemouridae",
      AI_Taxon2 %in% c("No invertebrates observed" )~"No invertebrates observed",
      AI_Taxon2 %in% c("Notonectidae" )~"Notonectidae",
      AI_Taxon2 %in% c("Oniscidae" )~"Oniscidae",
      AI_Taxon2 %in% c("Paltothemis lineatipes" )~"Libellulidae",
      AI_Taxon2 %in% c( "Parastacidae" )~"Parastacidae", 
      AI_Taxon2 %in% c("Perlidae" )~"Perlidae",
      AI_Taxon2 %in% c("Philopotamidae" )~"Philopotamidae",
      AI_Taxon2 %in% c("Physa","Physidae" )~"Physidae",
      AI_Taxon2 %in% c("Planariidae" )~"Planariidae",
      
      AI_Taxon2 %in% c("Plecoptera")~"PLECOPTERA", 
      AI_Taxon2 %in% c("Pleidae" ,"Neoplea")~"Pleidae",
      AI_Taxon2 %in% c("Polymitarcyidae" )~"Polymitarcyidae",
      AI_Taxon2 %in% c("Psephenidae" )~"Psephenidae",
      AI_Taxon2 %in% c("Rhyacophila rayneri","Rhyacophilidae" )~"Rhyacophilidae",
      AI_Taxon2 %in% c("Simuliidae","Simulium" )~"Simuliidae",
      AI_Taxon2 %in% c("Siphlonuridae" )~"Siphlonuridae",
      AI_Taxon2 %in% c("Stratiomyidae" )~"Stratiomyidae",
      AI_Taxon2 %in% c("Tabanidae" )~"Tabanidae",
      AI_Taxon2 %in% c("Taenionema" )~"Taeniopterygidae",
      AI_Taxon2 %in% c("Tipulidae", "Limoniinae",
                       "Tipula" )~"Tipulidae", # ADDED 9/26 KSM  
      AI_Taxon2 %in% c("Trichoptera",
                       "Molannidae")~"TRICHOPTERA",# ADDED 9/26 KSM  
      AI_Taxon2 %in% c("Unknown" )~"UNKNOWN",
      AI_Taxon2 %in% c("Veliidae" )~"Veliidae",
      AI_Taxon2 %in% c("Planorbidae" )~"Planorbidae",
      AI_Taxon2 %in% c("Beraeidae" )~"Beraeidae", #Not in SAFIT
      AI_Taxon2 %in% c("Ceratopogonidae" )~"Ceratopogonidae",
      AI_Taxon2 %in% c("Sphaeriidae" )~"Sphaeriidae",
      AI_Taxon2 %in% c("Odontoceridae" )~"Odontoceridae",
      AI_Taxon2 %in% c("Amphizoidae" )~"Amphizoidae",
      AI_Taxon2 %in% c("Leptoceridae" ,"Ceraclea")~"Leptoceridae",
      AI_Taxon2 %in% c("Gammaridae" )~"Gammaridae",
      AI_Taxon2 %in% c("Sericostomatidae" )~"Sericostomatidae",
      AI_Taxon2 %in% c("Daphniidae" )~"Daphniidae", #EXCLUDE??? Microcrustacean
      AI_Taxon2 %in% c("Psychodidae" )~"Psychodidae",
      AI_Taxon2 %in% c("Copepoda" )~"COPEPODA", #EXCLUDE??? Microcrustacean
      AI_Taxon2 %in% c("Ostracoda" )~"OSTRACODA", #EXCLUDE??? Microcrustacean
      AI_Taxon2 %in% c("Gyrinidae" )~"Gyrinidae",
      AI_Taxon2 %in% c("Decapoda","Palaemonidae")~"DECAPODA",
      AI_Taxon2 %in% c("Lymnaeidae" )~"Lymnaeidae",
      AI_Taxon2 %in% c("Chaoboridae" )~"Chaoboridae",
      AI_Taxon2 %in% c("Sialidae",
                       "Sialis" )~"Sialidae", 
      AI_Taxon2 %in% c("Carabidae" )~"Carabidae", ##Likely terrestrial?
      AI_Taxon2 %in% c("Megaloptera" )~"MEGALOPTERA",
      AI_Taxon2 %in% c("Tateidae" )~"Hydrobiidae", #Not in safit. Mud-snails, most likely
      AI_Taxon2 %in% c("Ephydridae" )~"Ephydridae", #Probably good but often in briny water
      AI_Taxon2 %in% c("Hebridae" )~"Hebridae", #Semi-aquatic
      AI_Taxon2 %in% c("Capniidae" )~"Capniidae",
      AI_Taxon2 %in% c("Rossianidae" )~"Rossianidae",
      AI_Taxon2 %in% c("Hydraenidae" )~"Hydraenidae",
      AI_Taxon2 %in% c("Ephemerellidae" )~"Ephemerellidae",
      AI_Taxon2 %in% c("Apataniidae" )~"Apataniidae",
      AI_Taxon2 %in% c("Bithyniidae" )~"Bithyniidae", #Not in SAFIT. Likely mis-identification for Hydrobiidae
      AI_Taxon2 %in% c("Uenoidae" )~"Uenoidae",
      AI_Taxon2 %in% c("Pomatiopsidae" )~"Pomatiopsidae", 
      AI_Taxon2 %in% c("Chloroperlidae" )~"Chloroperlidae",
      AI_Taxon2 %in% c("Hydroptilidae" )~"Hydroptilidae",
      AI_Taxon2 %in% c("Limoniidae" )~"Limoniidae",
      AI_Taxon2 %in% c("Hydroscaphidae" )~"Hydroscaphidae",
      AI_Taxon2 %in% c("Staphylinidae" )~"Staphylinidae", #Likely terrestrial?
      AI_Taxon2 %in% c("Georissidae" )~"Georissidae", #Semi-aquatic
      AI_Taxon2 %in% c("Mesoveliidae" )~"Mesoveliidae",#Semi-aquatic
      AI_Taxon2 %in% c("Saldidae" )~"Saldidae",#Semi-aquatic
      AI_Taxon2 %in% c("Goeridae" )~"Goeridae",
      AI_Taxon2 %in% c("Petaluridae" )~"Petaluridae",
      AI_Taxon2 %in% c("Calamoceratidae" )~"Calamoceratidae",
      AI_Taxon2 %in% c("Phryganeidae" )~"Phryganeidae",
      AI_Taxon2 %in% c("Polycentropodidae",
                       "Nyctiophylax" )~"Polycentropodidae", 
      AI_Taxon2 %in% c("Psychomyiidae" )~"Psychomyiidae",
      AI_Taxon2 %in% c("Ephemeridae", "Ephemera" )~"Ephemeridae",
      AI_Taxon2 %in% c("Eulichadidae" )~"Eulichadidae",
      AI_Taxon2 %in% c("Dolichopodidae" )~"Dolichopodidae",
      AI_Taxon2 %in% c("Scirtidae" )~"Scirtidae",
      AI_Taxon2 %in% c("Calopterygidae" )~"Calopterygidae",
      AI_Taxon2 %in% c("Valvatidae" )~"Valvatidae",
      AI_Taxon2 %in% c("Helophoridae" )~"Helophoridae",
      AI_Taxon2 %in% c("Pyralidae" )~"Pyralidae",
      AI_Taxon2 %in% c("Pteronarcyidae",
                       "Pteronarcys" )~"Pteronarcyidae", # Added 9/26 KSM
      AI_Taxon2 %in% c("Libellulidae" )~"Libellulidae",
      AI_Taxon2 %in% c("Astacoidea" )~"ASTACOIDEA",
      AI_Taxon2 %in% c("Viviparidae" )~"Viviparidae",
      AI_Taxon2 %in% c("Acroloxidae" )~"Ancylidae", #Not in SAFIT. Mis-identification? 2 observations in iNat
      AI_Taxon2 %in% c("Ptilodactylidae" )~"Ptilodactylidae",
      AI_Taxon2 %in% c("Cyphoderinae" )~"COLLEMBOLA",#Not in SAFIT, EXCLUDE?
      AI_Taxon2 %in% c("Empididae" )~"Empididae",
      AI_Taxon2 %in% c("Ecnomidae" )~"Ecnomidae", #Not in SAFIT, likely mis-identification
      AI_Taxon2 %in% c("Nematocera" )~"DIPTERA",
      AI_Taxon2 %in% c("Pelecorhynchidae" )~"Pelecorhynchidae",
      AI_Taxon2 %in% c("Palaemonidae" )~"Palaemonidae", 
      AI_Taxon2 %in% c("Ptychopteridae" )~"Ptychopteridae",
      AI_Taxon2 %in% c("Sciomyzidae" )~"Sciomyzidae",
      AI_Taxon2 %in% c("Tanyderidae" )~"Tanyderidae",
      AI_Taxon2 %in% c("Thaumaleidae" )~"Thaumaleidae",
      AI_Taxon2 %in% c("Talitridae" )~"Talitridae",
      AI_Taxon2 %in% c( "Thiaridae" )~"Thiaridae",
      AI_Taxon2 %in% c( "Tricorythidae" )~"Tricorythidae",
      
      AI_Taxon2 %in% c("Gelastocoridae" )~"Gelastocoridae", #Semi-aquatic
      AI_Taxon2 %in% c("Isopoda" )~"Isopoda",
      AI_Taxon2 %in% c("Pleuroceridae" )~"Pleuroceridae", #likely juga
      AI_Taxon2 %in% c("Corduliidae" )~"Corduliidae",
      AI_Taxon2 %in% c("Leuctridae" )~"Leuctridae",
      AI_Taxon2 %in% c("Peltoperlidae" )~"Peltoperlidae",
      AI_Taxon2 %in% c("Taeniopterygidae" )~"Taeniopterygidae",
      AI_Taxon2 %in% c("Hydrobiosidae" )~"Hydrobiosidae",
      AI_Taxon2 %in% c("Macroveliidae" )~"Macroveliidae", #Semi-aquatic
      AI_Taxon2 %in% c("Macromiidae" )~"Macromiidae", #Semi-aquatic
      AI_Taxon2 %in% c("Neuroptera" )~"NEUROPTERA",
      AI_Taxon2 %in% c("Unionida",
                       "Elliptio","Unionidae" )~"Unionidae",
      AI_Taxon2 %in% c("Bivalvia" )~"BIVALVE",
      AI_Taxon2 %in% c("Corydalus","Corydalus cornutus",
                       "Corydalidae (N-P)","Corydalidae (O-D group)", "Corydalidae","Neohermes",
                       "Corydalidae (Orohermes-Dysmicohermes group)" ,"Nigronia",
                       "Corydalidae (Neohermes-Protochauliodes group)")~"Corydalidae", 
      AI_Taxon2 %in% c("Hyalellidae" )~"Hyalellidae",
      AI_Taxon2 %in% c("Thienemannimyia" )~"Chironomidae",
      AI_Taxon2 %in% c("Noteridae" )~"Noteridae",
      AI_Taxon2 %in% c("Ancylidae" )~"Ancylidae",
      AI_Taxon2 %in% c("Pediciidae", #Not in SAFIT, but treated as Limoniinae
                       "Dicranota")~"Pediciidae", # Added 9/26 KSM
      AI_Taxon2 %in% c("Mysidae" )~"Mysidae",
      AI_Taxon2 %in% c("Mollusca" )~"MOLLUSCA",# Added 9/26 KSM
      AI_Taxon2 %in% c("Nematomorpha" )~"NEMATOMORPHA",# Added 9/26 KSM
      AI_Taxon2 %in% c("Nematoda" )~"NEMATODA",# Added 9/26 KSM
      T~"Exclude"),
    #Add order-level identifications
    AI_Order = case_when(AI_Family %in% c("Ameletidae", "Baetidae","Baetiscidae", "Caenidae", "Ephemerellidae","Ephemeridae", "EPHEMEROPTERA", 
                                          "Heptageniidae", "Isonychiidae", "Leptohyphidae", "Leptophlebiidae", 
                                          "Polymitarcyidae", "Siphlonuridae", "Tricorythidae")~"Ephemeroptera",
                         AI_Family %in% c("Capniidae", "Chloroperlidae", "Leuctridae", "Nemouridae","Peltoperlidae", 
                                          "Perlidae", "Perlodidae", "PLECOPTERA","Pteronarcyidae", "Taeniopterygidae")~"Plecoptera",
                         AI_Family %in% c("Brachycentridae", "Calamoceratidae", "Glossosomatidae", "Helicopsychidae", 
                                          "Hydropsychidae", "Hydroptilidae","Lepidostomatidae","Leptoceridae", "Limnephilidae", 
                                          "Odontoceridae",  "Philopotamidae", "Phryganeidae","Polycentropodidae","Hydrobiosidae",
                                          "Rhyacophilidae","Sericostomatidae", "TRICHOPTERA","Uenoidae","Psychomyiidae")~"Trichoptera",
                         AI_Family %in% c("Aeshnidae", "Calopterygidae", "Coenagrionidae", "Cordulegastridae","Corduliidae", 
                                          "Gomphidae", "Lestidae", "Libellulidae","Macromiidae", "ODONATA")~"Odonata",
                         AI_Family %in% c("Belostomatidae", "Corixidae", "Gerridae", "HEMIPTERA", "Naucoridae", 
                                          "Nepidae", "Notonectidae", "Pleidae", "Veliidae")~"Hemiptera",
                         AI_Family %in% c("COLEOPTERA", "Dryopidae", "Dytiscidae", "Elmidae","Gyrinidae", "Haliplidae", 
                                          "Hydrophilidae", "Psephenidae","Ptilodactylidae","Scirtidae")~"Coleoptera",
                         AI_Family %in% c("Athericidae", "Blephariceridae", "Ceratopogonidae", "Chaoboridae", 
                                          "Chironomidae", 
                                          "Culicidae",
                                          "DIPTERA", "Dixidae", "Dolichopodidae","Empididae","Ephydridae","Limoniidae",
                                          "Muscidae","Pediciidae","Psychodidae","Ptychopteridae", "Simuliidae", 
                                          "Stratiomyidae", "Tabanidae", "Tipulidae")~"Diptera",
                         AI_Family %in% c("Ancylidae", "GASTROPODA", "Hydrobiidae", "Physidae", "Planorbidae", 
                                          "Thiaridae","Lymnaeidae","Pleuroceridae","Valvatidae","Viviparidae")~"GASTROPODA" ,#Most are basomatomorpha
                         AI_Family %in% c("BIVALVE", "Corbiculidae", "Dreissenidae", "Margaritiferidae", "Sphaeriidae",
                                          "Unionidae")~"BIVALVIA",
                         AI_Family %in% c("Amphipoda", "Asellidae", "Cambaridae", "Cladocera","COPEPODA", "CRUSTACEA", 
                                          "DECAPODA","Isopoda", "Mysidacea", "Palaemonidae", "Parastacidae", "Astacidae")~"CRUSTACEA",
                         AI_Family %in% c("Annelida")~"Annelida",
                         AI_Family %in% c("Acariformes","Arachnida")~"Acariformes",
                         AI_Family %in% c("Platyheminthes","Planariidae","Turbellaria")~"Platyheminthes",
                         AI_Family %in% c("NEMATOMORPHA")~"NEMATOMORPHA",
                         AI_Family %in% c("NEMATODA")~"NEMATODA",
                         AI_Family %in% c("Corydalidae","Sialidae")~"Megaloptera",
                         AI_Family %in% c("LEPIDOPTERA")~"Lepidoptera",
                         T~"OtherOrder_OrderNotKnown"
                     
                         ),
    #This code assigns families to appropriate groups to assist with metric calculation
    AI_Ephemeroptera = AI_Family %in% c("Ameletidae", "Baetidae","Baetiscidae", "Caenidae", "Ephemerellidae","Ephemeridae", "EPHEMEROPTERA", 
                                        "Heptageniidae", "Isonychiidae", "Leptohyphidae", "Leptophlebiidae", 
                                        "Polymitarcyidae", "Siphlonuridae", "Tricorythidae"), # ADDED BY KSM DEC 22
    AI_Plecoptera = AI_Family %in% c("Capniidae", "Chloroperlidae", "Leuctridae", "Nemouridae","Peltoperlidae", 
                                     "Perlidae", "Perlodidae", "PLECOPTERA","Pteronarcyidae", "Taeniopterygidae"), 
    AI_Trichoptera = AI_Family %in% c("Brachycentridae", "Calamoceratidae", "Glossosomatidae", "Helicopsychidae", 
                                      "Hydropsychidae", "Hydroptilidae","Lepidostomatidae","Leptoceridae", "Limnephilidae", 
                                      "Odontoceridae",  "Philopotamidae", "Phryganeidae","Polycentropodidae",
                                      "Rhyacophilidae","Sericostomatidae", "TRICHOPTERA","Uenoidae","Hydrobiosidae","Psychomyiidae"),
    AI_EPT = AI_Ephemeroptera | AI_Plecoptera | AI_Trichoptera,
    AI_Odonata = AI_Family %in% c("Aeshnidae", "Calopterygidae", "Coenagrionidae", "Cordulegastridae","Corduliidae", 
                                  "Gomphidae", "Lestidae", "Libellulidae","Macromiidae", "ODONATA"),
    AI_Hemiptera = AI_Family %in% c("Belostomatidae", "Corixidae", "Gerridae", "HEMIPTERA", "Naucoridae", 
                                    "Nepidae", "Notonectidae", "Pleidae", "Veliidae"), # ADDED BY KSM DEC 22
    AI_Coleoptera = AI_Family %in% c("COLEOPTERA", "Dryopidae", "Dytiscidae", "Elmidae","Gyrinidae", "Haliplidae", 
                                     "Hydrophilidae", "Psephenidae","Ptilodactylidae","Scirtidae"),
    AI_Diptera = AI_Family %in% c("Athericidae", "Blephariceridae", "Ceratopogonidae", "Chaoboridae", 
                                  "Chironomidae", 
                                  "Culicidae",
                                  "DIPTERA", "Dixidae", "Dolichopodidae","Empididae","Ephydridae","Limoniidae",
                                  "Muscidae","Pediciidae","Psychodidae","Ptychopteridae", "Simuliidae", 
                                  "Stratiomyidae", "Tabanidae", "Tipulidae"),
    AI_Gastropod = AI_Family %in% c("Ancylidae", "GASTROPODA", "Hydrobiidae", "Physidae", "Planorbidae", 
                                    "Thiaridae","Lymnaeidae","Pleuroceridae","Valvatidae","Viviparidae"),# ADDED BY KSM DEC 22
    AI_Bivalve = AI_Family %in% c("BIVALVE", "Corbiculidae", "Dreissenidae", "Margaritiferidae", "Sphaeriidae",
                                  "Unionidae"),
    # AI_Clams = AI_Family %in% c("Corbiculidae",  "Sphaeriidae"),
    AI_Bivalve_Nonfingernail = AI_Family %in% c("BIVALVE", "Corbiculidae", "Dreissenidae", "Margaritiferidae", "Unionidae"),
    AI_Clams_Fingernail = AI_Family %in% c("Sphaeriidae"),
    AI_Mollusk = (AI_Gastropod | AI_Bivalve),
    AI_Crayfish = AI_Family %in% c("Cambaridae","DECAPODA", "Parastacidae", "Astacidae"),
    AI_Crustacean = AI_Family %in% c("Amphipoda", "Asellidae", "Cambaridae", "Cladocera","COPEPODA", "CRUSTACEA", 
                                     "DECAPODA","Isopoda", "Mysidacea", "Palaemonidae", "Parastacidae", "Astacidae"), 
    AI_Noninsect = (AI_Crustacean | AI_Gastropod | AI_Bivalve | AI_Family %in% c("Annelida","Acariformes",
                                                                                 "Platyheminthes","Planariidae",# ADDED BY KSM DEC 23
                                                                                 "MOLLUSCA","NEMATOMORPHA","NEMATODA",
                                                                                 "Turbellaria")),# Added 9/26 KSM
    AI_PerennialIndicator_PNW = AI_Family %in% c("Pleuroceidae","Ancylidae","Hydrobiidae","Margaritiferidae","Unionidae",
                                                 "Rhyacophilidae","Philopotamidae","Hydropsychidae","Glossosomatidae",
                                                 "Perlidae","Pteronarcyidae","Elmidae","Psephenidae",
                                                 "Gomphidae","Cordulegastridae","Calopterygidae","Corydalidae"),
    AI_PerennialIndicator_NC =
      AI_Family  %in% c("Baetidae", "Caenidae", "Ephemerellidae","Ephemeridae","Heptageniidae","Leptophlebiidae","Siphlonuridae",
                        "Peltoperlidae","Perlidae","Perlodidae",
                        "Hydropsychidae","Lepidostomatidae","Limnephilidae","Molannidae","Odontoceridae","Philopotamidae","Polycentropidae","Psychomyiidae","Rhyacophilidae",
                        "Corydalidae","Sialidae",
                        "Aeshnidae","Calopterygidae","Cordulegastridae","Gomphidae","Libellulidae",
                        "Ptychopteridae",
                        "Elmidae","Psephenidae",
                        "Unionidae","Ancylidae","Planorbidae","Pleuroceridae") |
      AI_Taxon2 =="Tipula" |
      (AI_Taxon2 =="Helichus" & AI_Lifestage=="insect adult" ), 
    AI_GOLD = (AI_Gastropod| (AI_Family %in% "Annelida") | AI_Diptera),
    AI_OCH = (AI_Odonata | AI_Coleoptera | AI_Hemiptera),
    AI_Tolerant = AI_Family %in% c("Annelida","Acariformes",
                                   "Platyheminthes","Planariidae",
                                   "NEMATOMORPHA","NEMATODA",
                                   "Turbellaria",
                                   #Dipterans:
                                   "Chironomidae","Culicidae","Psychodidae",
                                   #Crustaceans
                                   "Amphipoda", "Asellidae","Isopoda",
                                   #Snails
                                   "Physidae",
                                   #Odonates
                                   "Libellulidae","Coenagrionidae", 
                                   #Other insects where all taxa have TV > 8
                                   "Belostomatidae","Corixidae","Haliplidae"),
    AI_Tolerant_Alt = (AI_Noninsect  & !(AI_Bivalve|AI_Crayfish) |
                        AI_Family %in% c("Culicidae","Chironomidae"))
                                     )
ai_combined %>% select(AI_Tolerant, AI_Tolerant_Alt, AI_Family) %>% unique() %>% filter(AI_Tolerant_Alt) %>%write.table("clipboard", sep="\t", row.names=F)
ai_combined %>% filter(AI_Tolerant_Alt) %>%group_by(AI_Family) %>% tally() %>% arrange(n) %>%write.table("clipboard", sep="\t", row.names=F)
ai_combined %>% group_by(AI_Order) %>% tally()
ai_combined %>% filter(AI_Order=="OtherOrder_OrderNotKnown") %>% group_by(AI_Family) %>% tally()
# 
#   


# MORE CLEANUP FOR AI DATA BASED ON BUG HABITAT TYPE (TERRESTRIAL, SEMI-AQUATIC)
# This code may be used to drop terrestrial bugs and other taxa we want to exclude from analysis
excluded_bugs<-c(
  "Exclude","No invertebrates observed",
  #Terrestrial
  "Aphididae", "Aphrophoridae","Araneae","Arionidae","Arthropleidae","Braconidae",
  "Chloropidae","Chrysididae","Chrysopidae","Cicadellidae","Cicadidae","Diplopoda",
  "Diprionidae","Halictidae","Ithonidae","Limacidae","Lithobiidae","Lygaeidae","Meinertellidae",
  "Melyridae","Miridae","Mycetophilidae","Pentatomidae","Ptiliidae","Salpingidae","Sciaridae","Stylommatophora",
  "Zygentoma","Formicidae","Crabronidae","Thripidae","Coreidae","Vespidae","Noctuidae",
  "Crambidae", "Hymenoptera","Oxychilidae","Syrphidae",
  "Cyphoderinae",
  "Oniscidae", # ADDED BY KSM DEC 22
  "Oniscidea", # ADDED BY KSM DEC 22
  "Terrestrial beetle", "Terrestrial beetle sp","Terrestrial snail",# ADDED BY KSM DEC 22
  "Terrestrial sp", # Added 9/26 KSM
  #Saline
  "Canacidae", #Tidal flies
  # "Mysidae", #Mysid shrimp
  #Mosquitos
  "Culicidae"
)

#Opt to exclude semi-aquatic, surface dwellers, etc
semi_aquatic<-c(
  #Beetles
  "Carabidae", "Staphylinidae","Georissidae","Chrysomelidae", "Curculionidae",
  #Hemiptera
  "Veliidae", "Gerridae", "Mesoveliidae","Saldidae","Gelastocoridae","Macroveliidae",
  #Other
  "Talitridae"
)

# check_ai <- ai_combined %>%
#   filter(AI_Family == "DOTHIS") %>%
#   transmute(AI_Taxon, AI_Taxon2,Notes = paste("Changed taxon from ",AI_Taxon," (KSM)",sep = "")) %>%
#   unique() %>%
#   filter(!AI_Taxon2 %in% excluded_bugs) %>%
#   filter(!AI_Taxon2 %in% semi_aquatic)
# 
# 
# write_csv(check_ai, file = "NotForGit/clean_taxonomysamples.csv") 

# Remove terrestrial bugs, semi-aquatic species and other non-insect species from AI taxonomy dataset
ai_combined2<-ai_combined %>%
  filter(!AI_Family %in% excluded_bugs) %>%
  filter(!AI_Taxon2 %in% excluded_bugs) %>%
  filter(!AI_Family %in% semi_aquatic) %>%
  filter(!AI_Taxon2 %in% semi_aquatic)

####
# Logger data, if needed
# logger_df<-  read_csv("https://sdamchecker.sccwrp.org/checker/download/logger_raw") %>%
#   transmute(SiteCode = login_sitecode,
#             PendantID = pendantid,
#             LoggerNumber = login_loggernumber,
#             StartDate = login_start,
#             EndDate=login_end,
#             DateTime=datetime,
#             Temperature=temperature,
#             Temp_units=temperature_units,
#             Intensity=intensity,
#             Intensity_units=intensity_units,
#             TempC = case_when(Temp_units=="DegC"~Temperature,
#                               Temp_units=="DegF"~(Temperature-32)*(5/9),
#                               T~NA_real_),
#             login_email = login_email
#             )

#### Write data to NotForGit folder ####
write_csv(main_combined %>% inner_join(xwalk) %>% mutate(CalcTime=Sys.time()), file="NotForGit/1_Imported and cleaned data/main_step1.csv")
write_csv(veg_combined %>% inner_join(xwalk) %>% mutate(CalcTime=Sys.time()), file="NotForGit/1_Imported and cleaned data/veg_step1.csv")
write_csv(ai_combined2 %>% inner_join(xwalk) %>% mutate(CalcTime=Sys.time()), file="NotForGit/1_Imported and cleaned data/aquaticinvertebrates_step1.csv")
write_csv(amphib_combined %>% inner_join(xwalk) %>% mutate(CalcTime=Sys.time()), file="NotForGit/1_Imported and cleaned data/amphibians_step1.csv")
write_csv(xwalk %>% mutate(CalcTime=Sys.time()), file="NotForGit/1_Imported and cleaned data/xwalk_step1.csv")
#write_csv(loggers_meta_combined2 %>% mutate(CalcTime=Sys.time()), file="NotForGit/1_Imported and cleaned data/loggers_meta_combined_step1.csv")
#write_csv(logger_data %>% mutate(CalcTime=Sys.time()), file="NotForGit/1_Imported and cleaned data/loggers_readings_combined_step1.csv")
#
