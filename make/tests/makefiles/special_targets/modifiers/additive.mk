.SILENT: rule1

rule1: rule2
	echo "Me too"

rule2:
	echo "I'm silent"

.SILENT: rule2
