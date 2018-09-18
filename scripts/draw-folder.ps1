Get-ChildItem $1 -Filter *.dot | Foreach-Object {neato -n -Tpng -O $_.Name}
