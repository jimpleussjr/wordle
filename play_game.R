source("wordle.R")

reset()  
prep_words()

# For the first round of words
get_words(get_filtered_words())

### write the word you entered in "letters"
### and the color of the results in "colors" where:
###      green = 'g', black = 'b', yellow = 'y'
run_attempt(letters_attempt = "wench", colors_attempt = "byggb")

# Rerun it for subsequent attempts. If you need to start over, just run reset()


