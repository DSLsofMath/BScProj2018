# bird2code.awk
/^[^<]/ || /^$/ {print; next}

/^</ {
  print "\\begin{spec}"
  sub(/^< /,"")
  print
  rc = getline
  while(($0 ~ /^</) && (rc > 0)) {
    sub(/^< /,"")
    print
    rc = getline
  }
  print "\\end{spec}\n"
}