# Learner Corpus Research Summer School
# Workshop: Automatic annotation of learner corpus data
# July 4th, 2023, Louvain-la-Neuve

# Activity 1: POS-tagging

#### Step 1. Install TreeTagger####
# In order to complete this activity, you need to first install TreeTagger
# on your personal computer. 
# Follow the instructions here: https://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/

#### Step 2. Install required packages and load them ####
# If you do not already have the following packages on your computer, please
# run the following lines to install them
install.packages("dplyr") # For easy data manipulation
install.packages ("stringr") # For easy manipulation of strings
install.packages ("koRpus") # A package for analyzing corpus data 
koRpus::install.koRpus.lang("en") # The utilities to process English data
# Load packages
library(dplyr); library(stringr); library(koRpus); library(koRpus.lang.en)

#### Step 3. Pre-processing ####

##### Load in the ICLE corpus files as a data frame #####
icle_files <- list.files("data/icle_texts/", full.names = TRUE)
icle_corpus <- data.frame(
  # Set the first column of data frame as the file names
  file = sapply(icle_files, basename),
  # Read in each of the text files using the scan() function
  # Paste the separate paragraphs in each text file together using the paste0() function
  text = sapply(icle_files, function(x) paste0(scan(x, what = character(), sep = "\n"), collapse = " "))
)

##### Inspect the texts ####
# Check whether the data frame is okay 
# Note: the glimpse() function is similar to str() in base R but the output
# is easier to read 
glimpse(icle_corpus)

# Print the text for the file BRFF1065.txt. 
icle_corpus$text[icle_corpus$file == "BRFF1065.txt"]

# Print the text for the file BRFF1069.txt. 
icle_corpus$text[icle_corpus$file == "BRFF1069.txt"]

###### Question 1. What do you notice about these texts? #####
# What pre-processing steps (if any) might be necessary before 
# using automatic annotation tools?

# Get a list of all of the characters in the corpus to check if there are
# special characters that need to be dealt with
sort(unique(unlist(str_split(icle_corpus$text, pattern = ""))))

###### Question 2. What 'special characters' do you notice? #####

##### Clean the texts ####
# Write a function to clean the texts
clean_texts <- function(text){
  text %>%
    # Remove the file header
    str_remove_all("<ICLE-BR-FF-\\d{4}\\.\\d>\\s+") %>%
    # Remove all instances of <R
    str_remove_all("<R") %>%
    # Remove all instances of <* and *
    str_remove_all("<\\*\\s+(\\*\\s+)?") %>%
    # Only one space between words
    str_replace_all("\\s+", " ") %>%
    # Remove white space at beginning and end of the text
    trimws()
}
# Test the function on the file BRFF1065.txt
clean_texts(icle_corpus$text[icle_corpus$file == "BRFF1065.txt"])
# Test the function on the file BRFF1069.txt
clean_texts(icle_corpus$text[icle_corpus$file == "BRFF1069.txt"])
# Make a new column in your data frame and apply the cleaning function to all
# the texts
icle_corpus <- icle_corpus %>%
  mutate(text_clean = clean_texts(text))
# Save the icle_corpus file for later
write.csv(icle_corpus, file = "data/icle_corpus.csv", row.names = FALSE)

# Check whether any special characters remain 
sort(unique(unlist(str_split(icle_corpus$text_clean, pattern = ""))))

##### POS-tag and lemmatize the texts ####

# First test the tagger on a single file (BRFF1065.txt)
BRFF1065 <- icle_corpus$text_clean[icle_corpus$file == "BRFF1065.txt"]

BRFF1065_tagged <- treetag(
  # The text or file location
  file = BRFF1065,
  # Set format to 'obj' if you want to tag an object that is already loaded into R
  # Set format to 'file' if you want to tag a text file on your computer
  format = "obj",
  treetagger="manual",
  lang="en",
  TT.options=list(
    # Change this to the location where TreeTagger is installed on your computer
    path="/Applications/tree-tagger",
    preset="en"
  )
)

# Note that the object that gets returned is not a data frame but a 'kRp' object
# Call glimpse to see the different things that are contained within the object
glimpse(BRFF1065_tagged)

# You can use special functions to access these things. 
# For example: 
describe(BRFF1065_tagged)

###### Question 3. How many 'words' and 'sentences' does this text contain? #####

###### Question 4. What is the average sentence length? #####

# To get the data frame with POS tags and lemmas, you can use taggedText() 
# For example, to get the data frame with the POS-tags: 
taggedText(BRFF1065_tagged)

# From here, it is easy to analyze this data frame like any other. 
# For example, we can count the number of each type of POS tag
BRFF1065_tagged %>%
  # Access the data frame with the POS tags
  taggedText() %>%
  # Count each unique POS tag
  count(tag) %>% 
  # Arrange from most to least frequent
  arrange(desc(n)) %>%
  # Only show the first ten 
  head(10)

# Or the frequency of the different lemmas
BRFF1065_tagged %>%
  taggedText() %>%
  count(lemma) %>% 
  arrange(desc(n)) %>%
  head(10)

###### Question 5. How many adjectives (JJ) are there?#####

###### Question 6. How many common nouns (NN, NNS) are there?#####

# Note: if you want a 'horizontal' version, you can combine the tokens and tags with
# the paste function 
BRFF1065_tagged_horizontal <- paste(taggedText(BRFF1065_tagged)$token, taggedText(BRFF1065_tagged)$tag,  sep = "_", collapse = " ")
BRFF1065_tagged_horizontal

# This may be useful for searching for specific combinations.
# For example: We can extract all adjective + noun collocations in the following way
str_extract_all(BRFF1065_tagged_horizontal, "[^(_ )]+_JJ [^_]+_NNS?")

# As you may have noticed, TreeTagger was not able to recognize some of the lemmas.
# We can find out which tokens were not able to be lemmatized.
BRFF1065_tagged %>%
  taggedText() %>%
  filter(lemma == "<unknown>") %>% 
  count(token) %>%
  arrange(desc(n)) 

###### Question 7. How should you best deal with these unknown lemmas?#####

# Now we will apply the tagger to the entire corpus
# Use mapply to apply the TreeTagger to each file in the corpus
icle_tagged <- mapply(function(x, y) treetag(x, 
                             format = "obj", 
                             treetagger = "manual",
                             lang = "en", 
                              TT.options=list(
                               path="/Applications/tree-tagger",
                               preset="en"
                               ), 
                             # The document id is located in the 'file' column
                             doc_id = y), 
              x = icle_corpus$text_clean, y = icle_corpus$file) %>%
  # Extract the data frame with the POS tags
  lapply(., taggedText) %>%
  # Merge the data frames from each file together
  bind_rows()

# Inspect the data frame
glimpse(icle_tagged)
  
# We can now query this corpus rather easily. 
# For example, to find the most frequent noun lemmas in the corpus:
icle_tagged %>%
  filter(tag %in% c("NN", "NNS")) %>%
  count(lemma) %>%
  arrange(desc(n)) %>% 
  head(10)

# Which texts contain unknown lemmas:
icle_tagged %>%
  filter(lemma == "<unknown>") %>%
  count(doc_id, token) %>%
  arrange(desc(n))

###### Question 8. What are the most frequent verb lemmas in the corpus?#####

