# Debe instalar la libreria stringr
# install.packages("stringr")
getInputLogFile <- function() {
  return(read.csv(file.choose()));
}

setFolderToWork <- function(file_path) {
  setwd(dirname(file_path));
}

getInformationByDistrict <- function(input_log_file, district = 5 ) {
  filter_information_by_district = subset(input_log_file, district == 5);
  return(filter_information_by_district$crimedescr);
}

# information_to_clean must be an array with the crimes
# in district
cleanInformation <- function(information_to_clean) {
  print(information_to_clean);
  information_to_clean = gsub("-","",information_to_clean);
  information_to_clean = gsub("\\/","",information_to_clean);
  information_to_clean = gsub("\\$","",information_to_clean);
  information_to_clean = gsub("\\+","",information_to_clean);
  information_to_clean = gsub("\\.","",information_to_clean);
  # quita la informacion dentro de los parentesis
  # http://stackoverflow.com/questions/13529360/replace-text-within-parenthesis-in-r
  # It works like this:
  #  *? finds 0 or more spaces before (and after) the parentheses.
  # Since ( and ) are special symbols in a regex, you need to escape these, i.e. (\\(
  # The .*? is a wildcard find to find all characters, where the ? means to find in a 
  # non-greedy way. This is necessary because regex is greedy by default. In other words, 
  # by default the regex will start the match at the first opening parentheses and ends 
  # the match at the last closing parentheses.
  information_to_clean = gsub("*\\(.+?\\) *","",information_to_clean);
  information_to_clean = gsub("[0-9]","",information_to_clean);
  information_to_clean = gsub("\\)","",information_to_clean);
  information_to_clean = gsub(" PC ","",information_to_clean);
  information_to_clean = gsub("PC ","",information_to_clean);
  information_to_clean = gsub(" VC ","",information_to_clean);
  information_to_clean = gsub("VC ","",information_to_clean);
  information_to_clean = gsub(" I RPT ","",information_to_clean);
  information_to_clean = gsub("I RPT ","",information_to_clean);
  information_to_clean = gsub("I RPT","",information_to_clean);
  information_to_clean = gsub("VEHICLE","VEH",information_to_clean);
  # eliminando todo lo que tenga tamano de 2 caracteres, cuidado porque se va ID y WO
  information_to_clean = gsub("\\b[[:alpha:]]{2}\\b","",information_to_clean);
  # eliminando conector AND
  information_to_clean = gsub(" AND","",information_to_clean);
  # Removing leading/trailing whitespaces:
  information_to_clean = trimws(information_to_clean);
}


main = function() {
  # Get input log file, it must be a CSV
  input_log = getInputLogFile();
  # Set directory to save the output log
  folder_to_work = setFolderToWork(input_log);
  # Filter by district
  crime_in_district_unclean = getInformationByDistrict(input_log);
  crime_in_district = cleanInformation(crime_in_district_unclean);
}

main()