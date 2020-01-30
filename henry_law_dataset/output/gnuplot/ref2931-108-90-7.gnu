# load "ref2931-108-90-7.gnu"
# chem = "chlorobenzene"

set terminal postscript eps color
set title "ref = 2931; chem = chlorobenzene; casrn = 108-90-7"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.3028082E-02 * exp(  -1890.115    *(1/   298.    -1/T))

set label "" at    293.1500    ,   0.3570676E-02 point
set label "" at    303.1500    ,   0.2566796E-02 point
set label "" at    313.1500    ,   0.2112754E-02 point
set label "" at    323.1500    ,   0.1969484E-02 point
set label "" at    298.1500    ,   0.3028082E-02 point ps 2 pt 6

plot [290:330] H(T)
