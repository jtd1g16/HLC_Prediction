# load "ref3006-215-58-7.gnu"
# chem = "1,2,3,4-dibenzantheracene DBA(a,c)"

set terminal postscript eps color
set title "ref = 3006; chem = 1,2,3,4-dibenzantheracene DBA(a,c); casrn = 215-58-7"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =    18.77257     * exp(  -8593.819    *(1/   298.    -1/T))

set label "" at    298.1500    ,    21.20380     point
set label "" at    308.1500    ,    7.718537     point
set label "" at    318.1500    ,    2.315561     point
set label "" at    328.1500    ,    1.210983     point
set label "" at    338.1500    ,   0.7718537     point
set label "" at    298.1500    ,    18.77257     point ps 2 pt 6

plot [290:340] H(T)
