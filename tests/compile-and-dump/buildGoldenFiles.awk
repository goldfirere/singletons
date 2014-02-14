/INSERT/{while((getline line < $2) > 0 ){if(line !~ /INSERT/){print line}}close($2);next}1
