### Define function to perform all required steps
#' Concept pumping function
#'
#' @param mappedValues      A dataframe of strings and concepts
#' @param strings           A vector of the value set names
#' @param project           A string to append for the concept set name in ATLAS
#' @param conn              A valid CDM database
#' @param schema            A valid CDM schema
#' @param mapToStandard     A flag for if the concepts require mapping to standard values
#' @param Descendants       A flag for if the concepts should include all descendant concepts when imported (modifiable in ATLAS)
#' @param Exclude           A flag for if the concepts should be excluded when imported (modifiable in ATLAS)
#' @param Atlas             The WebAPI url for the ATLAS instance
#' @param authMethod        The authentication method for the ATLAS instance
#' @param Username          The Username to upload the concept sets to ATLAS under
#' @param Password          The Password to authorise the user to upload to ATLAS
#'
#' @return
#' @export
#'
#' @examples
conceptPump <- function(mappedValues = export,
                        strings = VSAC_strings[1],
                        project = "[Test Concept Pump]",
                        conn = connection,
                        schema = "",
                        mapToStandard = FALSE,
                        Descendants = FALSE,
                        Exclude = FALSE,
                        Atlas = baseUrl,
                        authMethod = "db", 
                        Username = "ATLAS_USER", 
                        Password = "ATLAS_PASSWORD"){
  
  ### variables to control the concept mapping
  oneSet <- mappedValues %>% filter(id == strings) %>% pull(conceptId)
  
  ### String to name the concept set
  nameSet <- paste(project, strings)
  
  ### query scehma for concept details
  VSAC_1 <- Capr::getConceptIdDetails(conceptIds = oneSet,
                                      connection = connection,
                                      vocabularyDatabaseSchema = schema,
                                      mapToStandard = mapToStandard)
  
  ### customised mapping variables for include/exclude per concept
  l <- length(VSAC_1[[1]])
  VSAC_mapping <- Capr::createConceptMapping(n = l,
                                             includeDescendants = rep(Descendants, l),
                                             isExcluded = rep(Exclude, l))
  
  ### create the concept set
  VSAC_1 <- VSAC_1 %>% Capr::createConceptSetExpressionCustom(Name = nameSet, conceptMapping = VSAC_mapping)
  
  ### create a dummy cohort to define out the concept set properly
  
  VSAC_dummyCohort <- Capr::createPrimaryCriteria(Name = "ThisIsADummy",
                                                  ComponentList = list(Capr::createDrugExposure(conceptSetExpression = VSAC_1)),
                                                  Limit = "All")
  
  VSAC_defined <- Capr::createCohortDefinition(Name = "Fakecohort_Conceptsetextraction",
                                               PrimaryCriteria = VSAC_dummyCohort)
  
  VSAC_compiled <- Capr::compileCohortDefinition(VSAC_defined)
  
  ### Create the concept item from the JSON
  VSAC_Conceptset <- RJSONIO::fromJSON(VSAC_compiled)
  VSAC_Conceptset <- VSAC_Conceptset$ConceptSets[[1]]$expression
  
  ### Authorise call required for each concept upload
  ROhdsiWebApi::authorizeWebApi(baseUrl =  Atlas, 
                                authMethod = authMethod, 
                                webApiUsername = Username, 
                                webApiPassword = Password)
  
  ### The CirceR json produced earlier lacks the "columns" or list elements for mapping variables which are FALSE
  ### Modified version of postDefinition() which corrects this error
  postrequest <-  ROhdsiWebApi::postDefinition(baseUrl = Atlas,
                                               definition = VSAC_Conceptset,
                                               name = nameSet,
                                               category = "conceptSet") 
}