source("wordle_functions.R")

reset()  
prep_words()

# For the first round of words
get_words() 

### sum_pos_props = sum of all the probabilities that the letter will appear in its 
    ### particular position in the subset of feasible words
### sum_prop_words = sum of all the probabilities that the letters will appear anywhere
    ### in the subset of feasible words.
### repeat_ltrs = number of letters repeated in the word. Better to use 0 early on.

### write the word you entered in "letters"
### and the color of the results in "colors" where:
###      green = 'g', black = 'b', yellow = 'y'

run_attempt(letters_attempt = "clear", colors_attempt = "bbbby")
run_attempt(letters_attempt = "torrs", colors_attempt = "bbybb")
run_attempt(letters_attempt = "briny", colors_attempt = "bgbgb")
run_attempt(letters_attempt = "drunk", colors_attempt = "bgggb")

# Rerun it for subsequent attempts. If you need to start over, just run reset() and prep_words()


