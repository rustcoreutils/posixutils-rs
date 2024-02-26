
# cat: notes

## to-do

* Bug:  if stdout write_all() produces Err, the program will erroneously
output the filename as the culprit, rather than the string "stdout"
* Questionable behavior:  if write_all() produces Err, the program will
continue to the next file, rather than stopping.

