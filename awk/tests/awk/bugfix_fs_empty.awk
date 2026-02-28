BEGIN { FS="" }
{
    for (i=1; i<NF; i++) printf "%s ", $i;
    if (NF > 0) printf "%s", $NF;
    printf "\n"
}
