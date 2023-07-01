
### CDM Database connection details
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "snowflake",
                                                                connectionString = paste0(Sys.getenv("PA_DA_GERMANY_SERVER"),Sys.getenv("CI_WARE")),
                                                                pathToDriver = "~/drivers",
                                                                user = Sys.getenv("SNOWFLAKE_USER"),
                                                                password = Sys.getenv("SNOWFLAKE_PASSWORD"))
cdmSchema <- Sys.getenv("PA_DA_GERMANY_SCHEMA")

#' Sheet read - a helper function to ingest codes saved as excel workbooks and prepare them for mapping
#'
#' @param fname  An object of xlsx type that needs to be read in containing named sheets of codes to be mapped, including which vocabulary its supposed to be
#'
#' @return
#' @export
#'
#' @examples
multiplesheets <- function(fname){
  
  # getting info about all excel sheets
  sheets <- readxl::excel_sheets(fname)
  tibble <- lapply(sheets, function(x) readxl::read_excel(fname, sheet = x))
  data_frame <- lapply(tibble, as.data.frame)
  
  # assigning names to data frames
  names(data_frame) <- sheets
  
  # print data frame
  print(data_frame)
}



#' Title
#'
#' @param path          A file path to a csv, xlsx or R file that provides the source vocabulary codes to map. Appropriate method should be selected automatically
#' @param cleaning      Logical trigger to identify if the input cleaning heuristics are required. Defaults to FALSE if the input R file is used
#' @param relationships A string that defines which relationship within the hierarchy to use. Defaults to the most useful, "Maps to"
#' @param output        A string variable chosen from xlsx, conceptSet or cohort that defines what is produced by the function
#' @param label         A string to identify the ouptut files
#'
#' @return
#' @export
#'
#' @examples
translate <- function(path = "SomeInputofCodes",
                      cleaning = TRUE,
                      relationships = "Maps to",
                      output = "xlsx",
                      label = NULL
                      ){
  if(!file.exists(path)){
    stop("The provided input file does not exist, have you pointed to the correct path?")
  }
  
  if(grepl(".R", path, fixed = TRUE)){
   cleaning <- FALSE
    source(path)
  }
  
  if(grepl(".xlsx", path, fixed = TRUE)){
  toMap <-  multiplesheets(path)
  }
  if(grepl(".csv", path, fixed = TRUE)){
  toMap <-  read.csv(path)
  }

  toMap2 <- dplyr::bind_rows(toMap, .id = "value_set_name")
  
  if(cleaning){
    toMap2 <- toMap2 %>% dplyr::mutate(value_set_name = as.factor(value_set_name)) %>% 
      tidyr::separate("Code_Type", c("Vocabulary", "Junk"), sep = " ") %>% 
      dplyr::mutate(Vocabulary = gsub("-", "", .$Vocabulary)) %>% 
      dplyr::select(-"Junk")
  }
  
  strings <- levels(toMap2$value_set_name)
  
  mapping <- list()
  
  for (s in strings){
    vocabs <- levels(toMap2 %>% filter(value_set_name == s) %>% pull(Vocabulary) %>% forcats::fct_inorder())
    
      for (i in vocabs){
        oneSet <- toMap2 %>% filter(Vocabulary == i, value_set_name == s) %>% pull(code)
        
        connection <- DatabaseConnector::connect(connectionDetails)
        
        conceptQuery <- "SELECT * FROM @vocabularyDatabaseSchema.concept\n  WHERE concept_code LIKE ANY ('@conceptCode') AND vocabulary_id LIKE '%@vocabulary';"
        
        x<- DatabaseConnector::renderTranslateQuerySql(connection = connection,
                                                       sql = conceptQuery,
                                                       snakeCaseToCamelCase = TRUE,
                                                       vocabularyDatabaseSchema = cdmSchema,
                                                       conceptCode = paste(oneSet, collapse = "','"),
                                                       vocabulary = i) %>% dplyr::tibble()
        
        conceptIdToMap <- x$conceptId
        
        mappingQuery <- "SELECT b.* FROM @vocabularyDatabaseSchema.concept_relationship a
    JOIN  @vocabularyDatabaseSchema.concept b on b.concept_id = a.concept_id_2 AND a.relationship_id in ('@relationship')
    WHERE a.concept_id_1 in (@conceptId);"
        conceptDetails <- DatabaseConnector::renderTranslateQuerySql(connection = connection,
                                                                     sql = mappingQuery,
                                                                     snakeCaseToCamelCase = TRUE,
                                                                     vocabularyDatabaseSchema = cdmSchema,
                                                                     conceptId = conceptIdToMap,
                                                                     relationship = relationships) %>% dplyr::tibble()
        DatabaseConnector::dbDisconnect(connection)
        innerloop[[i]]<- conceptDetails
        
      }
    mapping[[s]] <- dplyr::bind_rows(innerloop, .id = "id")
    
  }
  
  mapping <- dplyr::bind_rows(mapping, .id = "id") %>% distinct(across(everything()))
  
  if(output == "xlsx" | output == "excel" | output == "xl" | output == "xls"){ 
    xlsx::write.xlsx(mapping, paste0(label, ".xlsx"))
  }
  if(output == "conceptSet" | output == "conceptset" | output == "concepts" | output == "set"){
    Marathon::conceptPump(mapping)
  }
}


