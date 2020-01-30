# load "ref2489-129-00-0.gnu"
# chem = "pyrene"

set terminal postscript eps color
set title "ref = 2489; chem = pyrene; casrn = 129-00-0"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.8516718     * exp(  -6273.597    *(1/   298.    -1/T))

set label "" at    281.6900    ,    2.857143     point
set label "" at    283.5400    ,    2.631579     point
set label "" at    286.6500    ,    1.923077     point
set label "" at    287.6100    ,    1.818182     point
set label "" at    289.8500    ,    1.562500     point
set label "" at    291.2000    ,    1.408451     point
set label "" at    294.6800    ,    1.111111     point
set label "" at    298.7000    ,   0.8130081     point
set label "" at    300.5100    ,   0.7299270     point
set label "" at    302.8100    ,   0.6172840     point
set label "" at    305.4300    ,   0.5102041     point
set label "" at    298.1500    ,   0.8516718     point ps 2 pt 6

plot [280:310] H(T)
