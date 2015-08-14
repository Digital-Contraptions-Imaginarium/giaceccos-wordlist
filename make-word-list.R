# This script uses the word lists prepared by Geoffrey Leech, Paul Rayson and Andrew Wilson for
# their "Word Frequencies in Written and Spoken English: based on the British National Corpus" book
# available at http://ucrel.lancs.ac.uk/bncfreq/flists.html . 

makeWordList <- function (inputFile, no_of_dice = 6) {
    
    require(stringi)

    # could not find a more elegant way than write this myself: makes a base 10 number (or a vector
    # thereof) in its base 6 representation made with dice (so zero is shown as 1, one as 2 etc.)
    base10toDiceSet <- function (x) {
        return(sapply(x, function (y) {
            temp <- ""
            while (y >= 6) {
                temp <- paste0(y %% 6 + 1, temp, collapse = "")
                y <- y %/% 6
            }
            return(stri_pad_left(paste0(y + 1, temp, collapse = ""), width = no_of_dice, pad = "1"))  
        }))    
    }
    
    temp <- read.table(pipe(paste0("unzip -qc ", inputFile, collapse = "")), header = FALSE, sep = "", quote = "", stringsAsFactors = FALSE)
    names(temp) <- c("word", "wordType", "partOfSpeech", "frequency", "range", "dispersion")
    
    # Drop all words whose grammatical word class is not one of the specified types
    temp <- temp[temp$wordType %in% c("Adj", "Conj", "DetP", "NoC", "Prep", "Verb"), ]
    
    # Drop all words whose length is < 4 
    temp <- temp[!(stri_length(temp$word) < 4), ]
    
    # Drop all words that include special characters (AKA, not \w)
    temp <- temp[grepl("^\\w*$", temp$word), ]
    
    # Drop all words that include any digit or the specified characters
    # TODO: not sure this is correct
    temp <- temp[!grepl("\\d|[_]", temp$word), ]
    
    # Convert all to uppercase
    temp$word <- toupper(temp$word)
    
    # Drop all words that include the repetition of the same letter at least two times... and yes this 
    # include words with double letters, in case people couldn't spell them!
    temp <- temp[!grepl("([A-Z])\\1{1,}", temp$word), ]
    
    # Drop duplicates (if any)
    temp <- temp[!duplicated(temp$word), ]
    
    # Score as a weighted average of rarity and length with weigts of 0.9 and 0.1 respectively
    maxLength <- max(stri_length(temp$word))
    maxFrequency <- max(temp$frequency)
    temp$score <- (maxLength - stri_length(temp$word)) / maxLength * .1 +  (maxFrequency - temp$frequency) / maxFrequency * .9
    
    # WISHLIST
    # - Support for blacklists
    # - Drop words that differ just by 1 letter whose phonetics could be ambiguous to some, e.g. 
    #   dye and die

    # Sort by score and keep the best 6 ^ NO_OF_DICE only 
    temp <- head(temp[order(-temp$score), ], no_of_dice ^ 6)
    
    # Sort alphabetically and return
    temp <- temp[order(temp$word), ]

    # re-number in base 6    
    temp$dice <- base10toDiceSet(0:(nrow(temp) - 1))

    return(temp)
}

write.table(makeWordList("./1_1_all_fullalpha.zip")[, c("dice", "word")], "./giaceccos-wordlist.asc.unsigned", quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")
