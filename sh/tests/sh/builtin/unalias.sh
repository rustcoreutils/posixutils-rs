alias echo='echo one'
unalias echo
echo

alias echo='echo two'
alias :='echo three'
unalias -a
echo
:

alias echo='echo four'
unalias -- echo
echo

alias echo='echo five'
alias :='echo six'
unalias echo :
echo
: