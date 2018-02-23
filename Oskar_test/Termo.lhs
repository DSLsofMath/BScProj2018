
I en termodynamisk proccess så vandrar man runt. Man har ett läge i form av tryck och volym, om substansmängd är fix. Har även egenskaper som en- eller två-atomig gas.

> data State = State Double Double
> -- Första är tryck, andra är volym

> data System = System Double Int
> -- Första är substansmängd, andra är antal atomer i gasen

> data Energy = Energy Double Double
> -- Första är tillförd värme, andra är utfört arbete av gasen

> r :: Double
> r = 8.3145

> -- Temperature from state
> tfs :: System -> State -> Double
> tfs (System n na) (State p v) = p*v / (n*r)

> -- Internal energy from state
> iefs :: System -> State -> Double
> iefs sy@(System n na) st@(State p v) = (0.5 + na)*r*(tfs sy st)

Man förflyttar sig med en av fyra olika typer av processer

- Isobar "samma tryck"
- Isokor "samma volym"
- Isoterm "samma temperatur"
- Adiabat "inget värmeutbyte"

En cykel består av processer så att man är tillbaka där man började.


I en isobar övergång så behålls trycket. Man anger en önskad slutvolym.

För att göra en övergång behöver man veta info om systemet man gör det på. Man behöver veta vilket tillståndet det är i. Och till sist vilken volymn man önskar gå till.

> isobar :: System -> State -> Double -> (Energy, State)
> isobar (System n na) (State p v) vF = 
>   where
>     deltaV    = vF - v
>     work      = p * deltaV
>     deltaEint = 



Ide: bevisa carnot-verkninsgrad, kanske gör det med "bevis-representation" och visar parallellerna mellan representationerna.