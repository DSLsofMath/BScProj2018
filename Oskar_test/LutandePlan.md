
Ett lutande plan består av en låda som ligger på en triangel med en viss vinkel. Det finns egenskaper som massor, friktionskoecienter, hastigheter m.m. Alla dessa hänger ihop med olika ekvationer. Ett DSL innehåller alla dessa ekvationer. Och sedan så kan man manipulera ekvationerna för att få fram den sökta variabeln (ev. som funktion av något annat).
 
Enklaste fallet är ingen friktion och en låda som man bara släpper och som glider ner. Beräkna accelerationen längst med planet.

a = sin theta * g, borde gälla, men svårt att avgöra utan en bild. Hitta så enkla/direkta samband som möjligt mellan "intressanta" tal som vinkel och acceleariton och rörelseenergi. a = sin theta * g är att föredra framför vf^-vi^2 = 2*a*x.
 
    -- "tilted plane"
    data TP = Angle
            | Acceleration
    
    -- Operation
    data Op = Variable TP
            | Sin Op
            | Mul Op Op
            | GA -- Gravitantional acceleration
            | SinInv Op
            | Div Op Op
    
    -- Likhet mellan två operationer/uttryck
    data Equ = Equ Op Op
    
    -- Man har en massa olika samband av detta slag
    equality1 = Equ (Variable Acceleration) (Sin (Variable Angle) `Mul` GA)
    
Sambanden vill man kunna manipulera, precis som man manipulerar ekvationer vanligtvis. Istället för att ha någon smart funktion som gör det åt en, så kanske det räcker att säga att en likhet man fått fram från en annan gäller om den kan QuickCheckas. Den första likheten ses som en funktion som ger ett värde, som man stoppar in i den andra likheten och ser nu att den fortfarande gäller.
