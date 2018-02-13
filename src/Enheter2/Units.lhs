
 # Enheter

*Enheter* och *storheter* är grundläggande inom fysiken. En *storhet* har en *enhet* och ett *mätetal* (och även ett prefix). Bara storheter med samma enhet går att addera. Att lägga ihop exempelvis volym och sträcka skulle inte gå - vad skulle det vara för beräkning?

Det finns 7 slags storheter med varsin tillhörande SI-enhet:

- Längd (meter)
- Massa (kilogram)
- Tid (sekunder)
- Temperatur (kelvin)
- Substansmängd (mol)
- Elektrisk ström (ampere)
- Ljusstyrka (candela)

I kursen *Fysik för Ingenjörer* stöter man bara på de 5 första. Därför tas bara de med i denna text.

Vi kommer enbart använda oss av SI-enheter. Därför kommer exempelvis "meter" vara utbytbart med "längd". Därför har vi valt att kalla alla enheter med motsvarande namn på storheten istället.

Det domänspecifika språket för enheter kommer behandla enheter på både *typnivå* och *värdenivå*. Typnivå för att redan vid kompileringstillfället se att inga otillåtna operationer görs. Värdenivå för att kunna skriva ut enheter snyggt.

Implementationerna på typnivå och värdeninvå blir snarlika. Man hade kunna nöja sig med en implementation och använda sig av `Data.Proxy`, men det blir krånligt. Detta sätt är längre men lättare.

Låt oss börja med enheter på värdesnivå.