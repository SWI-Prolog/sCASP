cat << EOF
<html>
<body>
<ul>
  <li>SWI-Prolog $(scasp --version 2>&1)
  <li>Ciao $(scasp-ciao --version)
</ul>
EOF

swi()
{ /bin/time -f %U bash -c "scasp -n0 --unknown=fail $1 2>&1 > /dev/null" 2>&1
}

ciao()
{ /bin/time -f %U bash -c "scasp-ciao -n0 $1 2>&1 > /dev/null" 2>&1
}

cat << EOF
<table>
<tr><th>Program<th>SWI<th>Ciao</tr>
EOF
for f in $*; do
  echo "<tr><th>$f<td>$(swi $f)<td>$(ciao $f)</tr>"
done
cat << EOF
</table>
</body>
</html>
EOF
