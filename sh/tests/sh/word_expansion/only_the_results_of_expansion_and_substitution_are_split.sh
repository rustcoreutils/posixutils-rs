IFS=,
VAR='a,b,c'

echo x,y,${VAR},z,w
echo x,y,$(echo 'a,b,c'),z,w

