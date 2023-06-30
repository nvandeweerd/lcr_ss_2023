# Learner Corpus Research Summer School
# Workshop: Automatic annotation of learner corpus data
# July 4th, 2023, Louvain-la-Neuve

# Activity 2: Parsing

#### Step 1. Install required packages and load them ####
# If you do not already have the following packages on your computer, please
# run the following lines to install them
install.packages("dplyr") # For easy data manipulation
install.packages ("spacyr") # A package for dependency annotation
koRpus::install.koRpus.lang("en") # The utilities to process English data
# Load packages
library(dplyr); library(spacyr)

#### Step 2. Set up spaCy: First time ####
# Install spaCy on your computer
spacy_install()
# Note: you can also install spaCy in a virtual environment using `spacy_install_virtualenv()`
# Download the English language model
spacy_download_langmodel(model = "en_core_web_sm")

#### Step 3. Initiate spaCy: At the beginning of a session ####
# Install spaCy on your computer
spacy_initialize(model = "en_core_web_sm")


#### Step 4. Dependency parsing using spaCy ####

##### Load in the ICLE corpus data frame  #####
icle_corpus <- read.csv("data/icle_corpus.csv") %>%
  # spaCy needs the data frame to follow the TIF standard
  select(doc_id = file, text = text_clean)

##### Test spaCy on the first sentence of BRFF1065.txt)  #####
# Extract the first sentence
sentence <- str_split(icle_corpus$text[icle_corpus$doc_id == "BRFF1065.txt"], "(?<=\\.)[^\\.]")[[1]][1]
# Run spaCy on the first sentence to see the full parse
spacy_parse(sentence, dependency = TRUE,
                               entity = FALSE,
                               additional_attributes = c("head")) %>%
  # Need to convert the head column to characters so we can read it easily 
  mutate(head = sapply(head, as.character))


###### Question 1. What word is the final period dependent on? #####
###### Question 2. What type of dependency relationships are marked by ‘amod’ and ‘dobj’? ######


# Now we will apply spaCy to the entire text
BRFF1065 <- icle_corpus$text[icle_corpus$doc_id == "BRFF1065.txt"]

BRFF1065_parsed <- spacy_parse(BRFF1065, dependency = TRUE, additional_attributes = c("head")) %>%
  mutate(head = sapply(head, as.character))

# From here, it is easy to analyze this data frame like any other. 
# For example, we can extract all dependencies of interest 
BRFF1065_parsed %>%
  # Filter out only amods 
  filter(dep_rel == "amod") %>% 
  # Select only the token and the head 
  select(token, head)

###### Question 3. How many amod dependencies are there? ######
###### Question 4. What adjective modifies the word ‘difference’? ######

# We could also extract all direct object relations
BRFF1065_parsed %>%
  # Filter out only amods 
  filter(dep_rel == "dobj") %>% 
  # Select only the token and the head 
  select(token, head)

###### Question 5. What is the object of the verb 'control'? ######

# It's also very easy to apply spaCy to the entire corpus
# Note: the data frame must contain the columns 'doc_id' and 'text' 
# Now we will apply spaCy to the entire corpus
icle_parsed = spacy_parse(icle_corpus,
                          dependency = TRUE, 
                          entity = TRUE, 
                          additional_attributes = c("head")) %>%
  mutate(head = sapply(head, as.character))

###### Question 5. What is the most frequency dependency relation in the corpus? ######

icle_parsed %>%
  count(dep_rel) %>%
  arrange(desc(n))


# spacyr also has some convenience functions to extract specific structures
# For example, we can extract all noun phrases:
spacy_extract_nounphrases(icle)

# Note that this also includes the length of each noun phrase (in words) so 
# it is relatively straightforward to calculate the average length of noun phrases 
mean(spacy_extract_nounphrases(icle)$length) 

###### Question 6. What is the average length of noun phrases in the corpus? ######

# Another convenience function is named entity recognition
spacy_extract_entity(icle)


