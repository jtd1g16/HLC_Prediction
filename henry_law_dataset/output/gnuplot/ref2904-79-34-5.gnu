# load "ref2904-79-34-5.gnu"
# chem = "1,2,3-trichlorobenzene"

set terminal postscript eps color
set title "ref = 2904; chem = 1,2,3-trichlorobenzene; casrn = 79-34-5"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2302450E-01 * exp(  -6464.936    *(1/   298.    -1/T))

set label "" at    298.5500    ,   0.2877536E-01 point
set label "" at    308.1500    ,   0.8870559E-02 point
set label "" at    318.1500    ,   0.4785274E-02 point
set label "" at    328.1500    ,   0.3362536E-02 point
set label "" at    333.1500    ,   0.2734967E-02 point
set label "" at    298.1500    ,   0.2302450E-01 point ps 2 pt 6

plot [290:340] H(T)
