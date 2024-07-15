#---- Load packages and inputs ----
# v0.5

library(tools)
library(fabR)
library(madshapR)
library(Rmonize)
library(tidyverse)
library(janitor)
library(htmltools)

# download before after function
library(crayon)

avant_apres_harmo <- function(
    dossier = NULL, 
    harmonized_dossier = NULL, 
    split_by = NULL,
    summarize_output = TRUE,
    data_proc_elem = attributes(harmonized_dossier)$`Rmonize::Data Processing Elements`,
    dataschema = attributes(harmonized_dossier)$`Rmonize::DataSchema`,
    dataset = NULL,
    data_dict = NULL,
    harmonized_dataset = NULL,
    harmonized_data_dict = NULL,
    cols_dataset = names(dataset),
    col_harmonized_dataset = names(harmonized_dataset)
    
){
  
  # internal function to extract names of objects in dpe
  extract_var <- function(x){
    x <- x %>%
      str_replace_all('"',"`") %>%
      str_replace_all("'","`") %>%
      str_remove_all("`") 
    x = x[!is.na(x)]
    
    return(x)}
  
  
  if(!is.null(dossier) & !is.null(dataset)) 
    stop('no dossier nor dataset')
  if(!is.null(harmonized_dossier) & !is.null(harmonized_dataset)) 
    stop('no harmonized dossier nor harmonized dataset')
  
  if(!toString(split_by) %in%
     c("","dataschema_variable","input_dataset","Mlstr_harmo::rule_category"))
    stop(call. = FALSE, "
Possible values for `split_by`:\n 
'dataschema_variable','input_dataset' or 'Mlstr_harmo::rule_category' or NULL")
  
  
  
  
  # case if user provides dataset and harmo dataset
  if(!is.null(dataset) & !is.null(harmonized_dataset)){
    
    dataset <- 
      as_dataset(dataset,col_id = col_id(dataset)) %>%
      select(all_of(cols_dataset))
    
    if(is.null(data_dict)) {
      data_dict <- data_dict_extract(dataset)
      data_dict <- data_dict_match_dataset(dataset,data_dict)$data_dict
      dataset <- data_dict_apply(dataset,data_dict)
    }
    
    
    harmonized_dataset <- 
      as_dataset(harmonized_dataset,col_id = col_id(harmonized_dataset)) %>%
      select(all_of(col_harmonized_dataset))
    
    if(is.null(harmonized_data_dict)) {
      
      if(is.null(dataschema)){
        harmonized_data_dict <- data_dict_extract(harmonized_dataset)
      }else{
        harmonized_data_dict <- dataschema
      }
      
      harmonized_data_dict <- data_dict_match_dataset(harmonized_dataset,harmonized_data_dict)$data_dict
      harmonized_dataset <- data_dict_apply(harmonized_dataset,harmonized_data_dict)
    }
    
    dossier <- dossier_create(dataset)
    harmonized_dossier <- dossier_create(harmonized_dataset)
    
    if(is.null(data_proc_elem)){
      
      if(length(col_harmonized_dataset) != 1) 
        stop('col_harmonized_dataset must be unique')
      
      dataset_from  <- 
        dataset %>%
        select(all_of(col_dataset))
      
      # extract the dataset output
      dataset_to <- 
        harmonized_dataset %>%
        select(all_of(col_harmonized_dataset))
      
      if(sum(col_dataset %in% col_harmonized_dataset) > 1) {
        
        names(dataset_to) = paste0(col_dataset,'(output)')
        names(dataset_from) = paste0(col_harmonized_dataset,'(input)')
        
      }
      
      beforafter_all <- bind_cols(dataset_to,dataset_from)
      
      if(ncol(beforafter_all) == 0) return(tibble())
      
      beforafter_all <- 
        beforafter_all %>%
        distinct() %>%
        arrange(across(everything())) %>%
        mutate(
          `  ` = '||',
        ) %>%
        select(1,`  `,everything())
      
      return(beforafter_all) 
    }
    
    name_dataset <- unique(data_proc_elem$input_dataset)
    if(length(name_dataset) > 1) stop('names of datasets not unique in DPE')
    
    # extract the data_dict input (not used yet. for future dev)
    # if(!is.null(data_dict)) dataset <- data_dict_apply(dataset, data_dict)
    
    dossier = dossier_create(dataset)
    names(dossier) = name_dataset
    
    # extract the data_dict input (not used yet. for future dev)
    if(is.null(dataschema)) 
      dataschema = list(Variables = tibble(
        name = extract_var(data_proc_elem$dataschema_variable),
        label = name,
        valueType = 'text'
      )) %>% as_dataschema_mlstr()
    
    harmonized_dataset = data_dict_match_dataset(harmonized_dataset,dataschema, output = 'dataset')
    harmonized_dossier = dossier_create(harmonized_dataset)
    names(harmonized_dossier) = name_dataset
    # harmonized_dossier = as_harmonized_dossier(harmonized_dossier, dataschema,data_proc_elem)
    
    avant_apres <- 
      avant_apres_harmo(
        dossier = dossier, 
        harmonized_dossier = harmonized_dossier,
        split_by = split_by,
        data_proc_elem = data_proc_elem,
        dataschema = dataschema,
        summarize_output = summarize_output)
    
    return(avant_apres)
  }
  
  
  # case when user provides dossiers 
  # extract global objects: dataschema and dpe
  # dataschema <- 
  #   attributes(harmonized_dossier)$`Rmonize::DataSchema`
  
  # make sure the names of the datasets are properly written in the dpe
  data_proc_elem <- 
    data_proc_elem %>% 
    mutate(input_dataset = extract_var(input_dataset))
  
  # reduce complexity
  intersected_datasets <- 
    intersect(intersect(names(dossier), names(harmonized_dossier)), data_proc_elem$input_dataset)
  
  dossier <- dossier[intersected_datasets]
  harmonized_dossier <- harmonized_dossier[intersected_datasets]
  data_proc_elem <- data_proc_elem %>% filter(input_dataset %in% intersected_datasets)
  
  data_dicts <- dossier %>% lapply(data_dict_extract)
  harmonized_data_dicts <- harmonized_dossier %>% lapply(data_dict_extract)
  
  
  # # # split by study
  # dpe_lines <-
  #   data_proc_elem %>%
  #   group_split(pick(all_of(c("dataschema_variable","input_dataset")))) %>%
  #   set_names(paste0("Line : ",1:nrow(data_proc_elem)))
  # 
  # test <- 
  #   dpe_lines %>% lapply(nrow) %>% 
  #   lapply(function(x) x != 1) %>%
  #   unlist() %>% sum
  # 
  # if(test > 0) stop('dataschema_variable and input_dataset combination is not unique')
  # 
  # initialize
  beforafter_all = tibble(
    input_dataset = as.character(),
    dataschema_variable = as.character(),
    input_variables = as.character(),
    class_output = as.character(),
  )
  
  # for each dataset
  for(i in seq_len(nrow(data_proc_elem))){
    # stop()}
    
    # # initialize
    beforafter_i = tibble()
    # message(str_sub(paste0("\n",
    #                        "--before/after of : ",
    #                        crayon::bold(dataset_i)," -----------------------------------------------------"),1,81))
    
    # extract the involved dpe_dataset lines
    dpe_i <- data_proc_elem[i,] 
    
    # extract the dataset input
    dataset_i_from <- dossier[[dpe_i$input_dataset]]
    
    # extract the data_dict input
    data_dict_i_from <- data_dicts[[dpe_i$input_dataset]]
    
    # extract the dataset output
    dataset_i_to <- harmonized_dossier[[dpe_i$input_dataset]]
    
    # extract the data_dict output
    data_dict_i_to <- harmonized_data_dicts[[dpe_i$input_dataset]]
    
    # extract variables (input and output) 
    vars_from_i <-
      str_squish(unlist(strsplit(
        dpe_i$`input_variables`,split = ";"))) %>%
      extract_var %>% 
      set_names(paste0('input_value_',c(seq_len(length(.)))))
    
    var_to_i  <- 
      extract_var(dpe_i$`dataschema_variable`) %>%
      set_names('output_value')
    
    `Mlstr_harmo::rule_category` <- dpe_i$`Mlstr_harmo::rule_category`
    `Mlstr_harmo::algorithm` <- dpe_i$`Mlstr_harmo::algorithm`
    
    err = try({
      
      #   - generate subdataset (input and output)
      if(toString(vars_from_i) %in% '__BLANK__') {
        dataset_i_from  <- dataset_i_from %>% select(any_of(vars_from_i))
      }else{
        dataset_i_from  <- dataset_i_from %>% select(all_of(vars_from_i))
      }
      
      if(ncol(dataset_i_from) == 0){
        dataset_i_from <- 
          tibble(`input_value_1` = 
                   rep(extract_var(`Mlstr_harmo::algorithm`),
                       nrow(dataset_i_from)))}
      
      dataset_i_to <- 
        dataset_i_to %>% select(all_of(var_to_i)) %>%
        add_index() %>%
        arrange(pick(all_of("output_value"))) %>%
        mutate(across(everything(), as.character))
      
      dataset_i_from <- 
        dataset_i_from %>%
        slice(as_any_integer(dataset_i_to$index)) %>%
        mutate(across(everything(), as.character))
      
      dataset_i_to <- dataset_i_to %>% select(-index)
      
      #   - generate subdata dict (input and output)
      data_dict_i_from <- 
        data_dict_i_from %>%
        data_dict_filter(paste0('name %in% c("',paste0(c(vars_from_i),collapse = '","'),'")'))
      
      data_dict_i_to <- 
        data_dict_i_to %>%
        data_dict_filter(paste0('name %in% c("',paste0(c(var_to_i),collapse = '","'),'")'))
      
      #   - add class of each observation (either valid value, missing, 
      #     NA or categorical). Use preprocess function for that
      
      preprocess_from_i <-
        dataset_preprocess(
          distinct(dataset_i_from %>% setNames(vars_from_i)),
          data_dict_i_from) %>%
        mutate(cat_label = ifelse(!is.na(cat_label),paste0("[",cat_label,"]"),cat_label)) %>%
        unite("value_var_lab", c("value_var","cat_label"),sep = ' ',na.rm = TRUE,remove = FALSE) %>%
        mutate(valid_class = ifelse(valid_class == "1_Valid values"," {cat}",valid_class)) %>%
        mutate(valid_class = ifelse(valid_class == "2_Missing values"," {missing} {cat}",valid_class)) %>%
        mutate(valid_class = ifelse(valid_class == "3_Valid other values","",valid_class)) %>%
        mutate(valid_class = ifelse(valid_class == "4_NA values","NA {missing}",valid_class)) %>%
        unite("value_var_lab", c("value_var_lab","valid_class"),sep = '',na.rm = TRUE) %>%
        left_join(tibble(name = vars_from_i,name_input = names(vars_from_i)),by = c("name")) %>%
        select(name = "name_input","value_var","value_var_lab") %>%
        distinct %>% 
        group_split(pick(all_of('name'))) %>% as.list() %>%
        setNames(names(vars_from_i)) %>%
        lapply(function(x) x %>% 
                 rename_with(.cols = "value_var",.fn = ~ unique(x$name)) %>% 
                 rename_with(.cols = "value_var_lab",.fn = ~ paste0(unique(x$name),"_lab")) %>% 
                 select(-'name'))
      
      for(k in seq_along(preprocess_from_i)){
        # stop()}
        
        dataset_i_from <- 
          dataset_i_from %>% 
          left_join(preprocess_from_i[[k]],by = names(preprocess_from_i[k]))
        
      }
      
      preprocess_to_i <-
        dataset_preprocess(
          distinct(dataset_i_to %>% setNames(var_to_i)),
          data_dict_i_to) %>%
        mutate(cat_label = ifelse(!is.na(cat_label),paste0("[",cat_label,"]"),cat_label)) %>%
        unite("value_var_lab", c("value_var","cat_label"),sep = ' ',na.rm = TRUE,remove = FALSE) %>%
        mutate(valid_class = ifelse(valid_class == "1_Valid values"," {cat}",valid_class)) %>%
        mutate(valid_class = ifelse(valid_class == "2_Missing values"," {missing} {cat}",valid_class)) %>%
        mutate(valid_class = ifelse(valid_class == "3_Valid other values","",valid_class)) %>%
        mutate(valid_class = ifelse(valid_class == "4_NA values","NA {missing}",valid_class)) %>%
        unite("value_var_lab", c("value_var_lab","valid_class"),sep = '',na.rm = TRUE) %>%
        left_join(tibble(name = var_to_i,name_output = names(var_to_i)),by = c("name")) %>%
        select(name = "name_output","value_var","value_var_lab") %>%
        distinct %>% 
        rename(output_value = "value_var",
               output_value_lab = "value_var_lab") %>% 
        select(-"name")
      
      #   - combine them (output on the left, then input)      
      beforafter_i <- 
        dataset_i_to %>%
        left_join(preprocess_to_i,by = c('output_value')) %>%
        bind_cols(dataset_i_from) 
      
      # Unite columns dynamically based on suffixes
      for(k in names(vars_from_i)) {
        # stop()}
        beforafter_i <- 
          beforafter_i %>% 
          mutate(!! as_any_symbol(names(vars_from_i[k])) := !!as_any_symbol(paste0(names(vars_from_i[k]),"_lab"))) %>%
          select(-paste0(names(vars_from_i[k]),"_lab"))
      }
      
      beforafter_i <- 
        beforafter_i %>% mutate(output_value = output_value_lab) %>%
        select(-'output_value_lab')
      
      beforafter_i <- 
        beforafter_i %>% 
        mutate(
          input_dataset = dpe_i$input_dataset,
          dataschema_variable = var_to_i,
          `Mlstr_harmo::rule_category` = `Mlstr_harmo::rule_category`,
          `Mlstr_harmo::algorithm` = `Mlstr_harmo::algorithm`, 
          cut = '||',
          input_variables = 
            paste0(paste0(names(vars_from_i)," = ", vars_from_i),collapse = ' ; \n')
        ) %>%
        select(
          input_dataset,
          dataschema_variable,
          input_variables, 
          `Mlstr_harmo::rule_category`,
          `Mlstr_harmo::algorithm`,
          ` ` = cut,
          starts_with('output_value'),
          `  ` = cut,
          starts_with('input_value')) %>%
        distinct
      
      # summarise information for each case determined by the rule category
      if(`Mlstr_harmo::rule_category` == 'id_creation') {
        beforafter_i <- 
          beforafter_i %>% 
          mutate(
            output_value = paste0(var_to_i,'(s)'),
            input_value_1 = paste0(vars_from_i,'(s)')) %>%
          distinct
      }
      
      if(`Mlstr_harmo::rule_category` %in% c('paste','impossible','undetermined')) {
        beforafter_i <- 
          beforafter_i %>% 
          mutate(input_value_1 = '__BLANK__') %>%
          distinct
      }
      
      if(`Mlstr_harmo::rule_category` %in% c('recode','direct_mapping')) {
        
        beforafter_i <- 
          beforafter_i %>% 
          # select(` `:last_col()) %>%
          mutate(
            output_value = ifelse(str_detect(input_value_1,"\\{cat\\}|\\{missing\\}"),
                                  output_value,
                                  ifelse(output_value == input_value_1,'identical',output_value)
            ))
        
        if(summarize_output == TRUE){
          
          beforafter_i <- 
            beforafter_i %>% 
            group_by(pick(-c('input_value_1'))) %>% slice(c(1,n())) %>%
            distinct() %>%
            reframe(input_value_1 = paste0(input_value_1,collapse = " [...] ")) %>%
            mutate(input_value_1 = ifelse(str_detect(input_value_1,"\\{cat\\} \\[...\\] "),
                                          str_replace_all(input_value_1,"\\{cat\\} \\[...\\] ","{cat}{split}"),input_value_1)) %>%
            separate_longer_delim(input_value_1,delim = "{split}")
          
        }
      }
      
      beforafter_i <-
        beforafter_i %>%
        mutate(across(c("output_value",starts_with("input_value")), 
                      ~ str_remove_all(.," \\{cat\\}")))
      
    }, silent = TRUE)
    
    err <- ifelse((class(err)[1] == 'try-error'),'**failed**','')
    message(paste0(str_sub(paste0(
      str_trunc(paste0(
        "    ",i,"/",nrow(data_proc_elem)," : ",
        var_to_i," (",dpe_i$input_dataset,")"),width = 49,ellipsis = '[...]'),
      "                                       "),1,50)),bold(err))
    
    beforafter_all <- bind_rows(beforafter_all,distinct(beforafter_i))
  }
  
  # clean elements.
  #  - if one dataset, the column is deleted
  if(length(unique(beforafter_all$input_dataset)) == 1 ) 
    beforafter_all['input_dataset'] <- NULL 
  
  table_split <-
    beforafter_all %>% group_by(pick(all_of(split_by))) %>% ungroup() %>%
    group_split(pick(all_of(split_by))) %>%
    as.list() %>%
    lapply(function(x) remove_empty(x,'cols'))
  
  names(table_split) <- 
    if(toString(split_by) %in% "") "all" else sort(unique(pull(beforafter_all,all_of(split_by))))
  
  for(i in seq_along(table_split)){
    # stop()}
    
    # clean elements. if the input variable is unique, replace name of the 
    # column input_value by the actual name of the variable
    
    var_from_i <- 
      distinct(table_split[[i]]['input_variables']) %>%
      separate_longer_delim(input_variables, delim = " ; \n") %>%
      separate_wider_delim(input_variables, delim = " = ",names = c("input_variables","name")) %>%
      mutate(input_variables = ifelse(input_variables == "input_value_1","input_value",input_variables))
    
    names_from_i        <- c(var_from_i$input_variables)
    names(names_from_i) <- var_from_i$name
    
    if(toString(names(names_from_i)) == "__BLANK__") names(names_from_i) <- "input_value"
    
    #  - if one input value per input variable, rename the input_value_1 into input_value
    if(length(unique(names_from_i)) == 1){
      table_split[[i]] <-
        table_split[[i]] %>%
        rename_with(.cols = starts_with('input_value'),.fn = ~ "input_value") %>%
        mutate(input_variables = str_replace(input_variables,"input_value_1 =","input_value ="))
    } 
    
    #  - if one variables, the column is renamed/deleted
    if(length(unique(table_split[[i]]$input_variables)) == 1){
      table_split[[i]] <-
        table_split[[i]] %>%
        rename_with(.cols = starts_with('input_value'),.fn = ~ c(names(names_from_i))) %>%
        select(-input_variables)
    } 
    
    #  - if one output variable, the column is renamed/deleted
    if(length(unique(table_split[[i]]$dataschema_variable)) == 1){
      table_split[[i]] <- 
        table_split[[i]] %>%
        rename_with(.cols = starts_with('output_value'),.fn = ~ c(unique(table_split[[i]]$dataschema_variable))) %>%
        select(-dataschema_variable)
    } 
    
  }
  
  # if only one table for all processing, unlist it
  if(length(names(table_split)) == 1) table_split <- table_split[[1]]
  
  return(table_split)
}

# download objects
objects <- 
  list.files("output_documents/",pattern = ".rds",full.names = TRUE)

for(i in objects){
  # stop()}
  temp <- readRDS(i)
  
  if(str_detect(i,"checks-")){
    assign(value = temp,x = "checks")
  }else{
    assign(value = temp,x = basename(file_path_sans_ext(i)))    
  }
  
  rm(temp)
}

#---- Load datasets ----

# put dataset in a dossier
dossier <- lapply(str_subset(ls(),"data_clean"),get)
names(dossier) <- unique(DPE$input_dataset)

#---- Transformation of data ----
harmonized_dataset <- harmo_process(dossier,dataschema,DPE)

#---- if_warnings ----
harmo_warnings <- 
  attributes(harmonized_dataset)$`Rmonize::Data Processing Elements` %>%
  bind_rows(tibble(`Rmonize::warning_status`= as.character())) %>%
  filter(!is.na(`Rmonize::warning_status`)) 

if(nrow(harmo_warnings) == 0){
  
  checks$harmonization_warnings <- FALSE
  checks$harmonization_warnings_detail <- NULL
  
}else{
  
  checks$harmonization_warnings <- TRUE  
  checks$harmonization_warnings_detail <- 
    attributes(harmonized_dataset)$`Rmonize::Data Processing Elements`
  
}

#---- if_errors ----

harmo_errors <- 
  attributes(harmonized_dataset)$`Rmonize::Data Processing Elements` %>%
  bind_rows(tibble(`Rmonize::error_status`= as.character())) %>%
  filter(!is.na(`Rmonize::error_status`))

if(nrow(harmo_errors) == 0){
  
  checks$harmonization_errors <- FALSE
  checks$harmonization_errors_detail <- NULL
  
}else{
  
  checks$harmonization_errors <- TRUE  
  checks$harmonization_errors_detail <-     
    attributes(harmonized_dataset)$`Rmonize::Data Processing Elements`
  
}

saveRDS(checks, checks_path)

if(any(checks$harmonization_errors,checks$harmonization_warnings)){
  
  message(
    "
Harmonization contains errors/warnings that may be associated to errors in the 
process.

Please contact Maelstrom Research at ",email_contact," and 
send the file '",basename(checks_path),"' from the folder 'output_documents'.")  
  
}


# récupérer le dpe 

#---- before/after ----

if(!any(checks$harmonization_errors,checks$harmonization_warnings)){
  
  # before after report
  before_after_report <- 
    avant_apres_harmo(dossier,harmonized_dataset,split_by = "Mlstr_harmo::rule_category")
  
  before_after_report_path <- 
    paste0("output_documents/","before_after_report-",
           checks$harmo_group,'.xlsx')
  write_excel_allsheets(
    before_after_report,
    before_after_report_path)
  
  # harmonization summary report
  output_summary <- 
    harmonized_dossier_summarize(harmonized_dataset)
  output_summary_path <- 
    paste0("output_documents/","harmonization_summary-",
           checks$harmo_group,'.xlsx')
  write_excel_allsheets(
    output_summary,
    output_summary_path)
  
  # pooled harmonized dataset (harmonized dataset)
  pooled_harmonized_dataset <- 
    pooled_harmonized_dataset_create(harmonized_dataset)
  harmonized_data_path <- 
    paste0("output_dataset/","harmonized_data-",
           checks$harmo_group,'.xlsx')
  write_excel_allsheets(
    pooled_harmonized_dataset,
    harmonized_data_path)
  
  # harmonized data dict
  harmonized_data_dict <- 
    data_dict_extract(pooled_harmonized_dataset)
  harmonized_data_dict_path <- 
    paste0("output_documents/","harmonized_data_dict-",
           checks$harmo_group,'.xlsx')
  write_excel_allsheets(
    harmonized_data_dict,
    harmonized_data_dict_path)
  
  # visual report
  viz_path <- 
    paste0("output_documents/","visual_report-",checks$harmo_group)
  harmonized_dossier_visualize(
    harmonized_dossier = harmonized_dataset,
    bookdown_path = viz_path,
    harmonized_dossier_summary = output_summary)
  
}else{
  
  # harmonization assessment report
  output_summary <- 
    harmonized_dossier_evaluate(harmonized_dataset)
  output_summary_path <- 
    paste0("output_documents/","harmonization_summary-",
           checks$harmo_group,'.xlsx')
  write_excel_allsheets(
    output_summary,
    output_summary_path)
  
}

#---- clean objects ----

objects <- 
  list.files("output_documents/",pattern = ".rds",full.names = TRUE)[
    !list.files("output_documents/",pattern = ".rds",full.names = TRUE) %in%
      (list.files("output_documents/",pattern = ".rds",full.names = TRUE) %>%
         str_subset(pattern = "checks") %>%
         str_subset(pattern = "path",negate = TRUE))]

invisible(file.remove(objects))

rm(list = c("i","objects","dossier","harmo_errors","harmo_warnings",
            "checks_path","col_harmonized_dataset","cols_dataset",
            "checks_init","email_contact","output_summary"))

