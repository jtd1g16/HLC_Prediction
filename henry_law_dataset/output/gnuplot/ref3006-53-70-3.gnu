# load "ref3006-53-70-3.gnu"
# chem = "1,2,5,6-dibenzantheracene DBA(a,h)"

set terminal postscript eps color
set title "ref = 3006; chem = 1,2,5,6-dibenzantheracene DBA(a,h); casrn = 53-70-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =    179.4622     * exp(  -11786.53    *(1/   298.    -1/T))

set label "" at    298.1500    ,    186.3364     point
set label "" at    308.1500    ,    45.36222     point
set label "" at    318.1500    ,    16.42193     point
set label "" at    328.1500    ,    4.536222     point
set label "" at    338.1500    ,    1.713372     point
set label "" at    298.1500    ,    179.4622     point ps 2 pt 6

plot [290:340] H(T)
