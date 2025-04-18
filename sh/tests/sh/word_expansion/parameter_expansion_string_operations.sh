VAR=ababcbc NULL=''

echo ${#VAR}
echo ${#NULL}
echo ${#UNSET}
echo ${VAR%}
echo ${VAR%b*}
echo ${VAR%%}
echo ${VAR%%b*}
echo ${VAR#}
echo ${VAR#*b}
echo ${VAR##}
echo ${VAR##*b}