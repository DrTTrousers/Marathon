
# A Capr cohort template is a function that returns a cohort
drugEraTemplate <- function(ingredientConceptId) {
  
  drugConceptSet <- Capr::cs(Capr::descendants(ingredientConceptId))
  
  Capr::cohort(
    entry = Capr::entry(Capr::drugEra(drugConceptSet, Capr::age(Capr::gte(16))),
                  observationWindow = Capr::continuousObservation(-365L, 0L)),
    exit = Capr::exit(Capr::drugExit(drugConceptSet, persistenceWindow = 30))
  )
}


con <- DatabaseConnector::connect(Eunomia::getEunomiaConnectionDetails())

library(dplyr, warn.conflicts = FALSE)

# create a cohort for every single ingredient
df <- DatabaseConnector::querySql(con, 
                      "Select * from main.concept where concept_class_id = 'Ingredient'") %>% 
  tibble() %>% 
  select(CONCEPT_ID, CONCEPT_NAME) %>% 
  mutate(CAPR_COHORT = purrr::map(CONCEPT_ID, drugEraTemplate)) %>% 
  mutate(COHORT_JSON = purrr::map_chr(CAPR_COHORT,Capr::as.json))


json_folder = paste0("~/JSONS")
json_files = list.files(json_folder)
for (json_file in json_files) {
  
  writeLines(json_file)
  
  ### Authorise call required for each cohort upload
  ROhdsiWebApi::authorizeWebApi(baseUrl =  baseUrl, 
                                authMethod = authMethod, 
                                webApiUsername = Sys.getenv('ATLAS_USER'), 
                                webApiPassword = Sys.getenv('ATLAS_PASSWORD'))
  
  ## read JSON file from the folder 
  cohortfromJSON = jsonlite::read_json(json_file)
  
  ## post cohort definition in baseURL 
  ROhdsiWebApi::postCohortDefinition(baseUrl = baseUrl, cohortDefinition =  cohortfromJSON, name = paste0(json_file)) 
  
}