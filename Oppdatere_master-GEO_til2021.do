/*  Verktøyscript:
	OPPDATERE GEO-MASTERFIL FRA  ETT ÅR TIL NESTE 

*** KMD svarer i mail at de ikke kjenner til noen endringer i geo-inndelingen for 2021.
	Scriptet kopierer masterfil fra 2020, og retter navn etc.
	
	
***		MÅ FØLGES OPP:
***		"Fauske Fuossko" er riktig likevel - rette opp (ble feil i fjor). ok
***		Samiske navn vi ikke har snappet opp!
***		- Dielddanuorri – Tjeldsund .ok
***		- Hammerfest – Hámmerfeasta .ok
***		- Namsos – Nåavmesjenjaelmie .ok


***		Obs om Bydeler i Stavanger:
***		Eiganes og Våland får fjernet "kommunedel" fra den filen som brukes i flatfilproduksjon
***		- ellers blir det femsiders bydelsprofil (2020- ).
	
	Poenget med å scripte er å lettere kunne teste/endre, og å 
	dokumentere hva vi gjør. Scriptet må nybygges neste år.
	
	Scriptnavn: Oppdatere_master-GEO_til2021
	
	Lager Excelfil og txt-fil tilrettelagt for ny metode (fra H-2017) for oppdatering av Regiontabell 
	i khp.fhi.no.
	Se nedenfor.

	SSB standard for kommuneinndeling: (http://www.ssb.no/klass/klassifikasjoner/131/koder)
	
	FASIT for kommunenummer og -navn per 22.9.2020 (NB: HAR NOEN UFULLSTENDIGE NAVN): 
		F:\Prosjekter\Kommunehelsa\Masterfiler\2021\Kommuneliste_fra_KLASS_per_2020-09-22.csv
		Lastet ned fra SSB/KLASS.
	MÅ OPPDATERES - MANGE ENDRINGER SENERE PÅ HØSTEN.

	Oversikt over endringer fra tidligere år:
	* for 2014-profilene: * ingenting *	
	* for 2015-profiler:
	* - legge til bydeler for 4 byer, OG legge inn "Bydel" i Oslo-navnene. Kilde: Bydeler.dta i \2015.
	* - Nye navn for 1739 Røyrvik og 1939 Storfjord.
	* - Lage en fil til bruk i grafer, med kortversjon av navnene som value label.
	* for 2016-profiler:  * ingenting *
	* for 2018-profiler: Fire smnslå (Larvik, Holmestrand, Færder, Indre Fosen) 
	* og nye numre i hele 50 Trøndelag.
	* for 2019-profiler: 
	* - Avsluttet prod. av ASCII-versjoner av filene, lager bare Unicode. 
	* - Dermed redusert til ett script for oppdatering av geomasterfiler.
	* - Kopiert inn prod. av RegionMaster_ÅÅÅÅ.txt fra "Lage_Unicode-versjoner"-scriptet (se 2018).
	* - Lagt inn Helseregioner - hadde notert at de trengtes andre steder. Fjerne dem fra RegionMaster.
	* - Nytt navn: Nordreisa på samisk/kvensk.
	* For 2020-profiler: Masse. Kommuner: Leser inn filen fra KMD og flikker på den (manglende samisk, etc).
		Nye fylker: Hardkoder. Bydeler: nye i Stavanger. Helseregioner nye numre.
		MIDLERTIDIG (håper jeg): Luke vekk bokstav "eng" fra Porsáŋki. Den blir spm.tegn et sted på veien 
		fra Indikator.txt til profil.
	*/

/*	
Man må stå i rett mappe for å unngå feil. Bruk
kommandoen cd "F:\Prosjekter\Kommunehelsa\Masterfiler\..(rett årstall).."
*/

*===============================================================================
* Må kjøres i Stataversjoner som skriver Unicode, dvs. minst v.14.
assert `c(version)' >=14
set more off
pause on

*LEGG INN RETT ÅRSTALL!
cd "F:\Prosjekter\Kommunehelsa\Masterfiler\2021"

************************************************************************
*  TEST på at det er lagt inn rett årstall i filbanen. Vanligvis vil dette 
*  skriptet kjøres året før profilene produseres. Verifiser derfor at 
* 		[årstall i filbanen] = [inneværende år] + 1
************************************************************************
local aarstalliFilbanen = real(substr("`c(pwd)'", -4,4))
local innevaerendeAar = real(word("`c(current_date)'", 3))
assert `aarstalliFilbanen'==`innevaerendeAar' + 1 

local profilaar = `aarstalliFilbanen'
local fjoraar  =  `profilaar'-1
di `fjoraar'

/*-------------------------------------------------
*	Sjekking: Vs. navnelista for kommunene fra KLASS. Kommenter ut når sjekket.
*	Kan ikke bare merge på navn, pga vår masterfil har Oslo K og Oslo F - ikke entydig.

	import delimited "Kommuneliste_fra_KLASS_per_2020-09-22.csv", clear
	sort code			//Er kommunenummer
	rename code geo
	merge 1:1 geo using "../`fjoraar'/Stedsnavn_SSB_Unicode.dta"
	replace name = subinstr(name, "- ", "", .)
	gen diff = ""
	replace diff = "***" if name != Sted & length(Sted_kode) == 4
			* Funn: Må rette 1841 Fauske, KLASS har feil 1870 Sortland (mangler samisk), 
			* tegnsettproblemer med 5436 Porsanger og 5437 Karasjok fra KLASS.
	exit
*-------------------------------------------------*/

* Hente inn fjorårets masterfil. 
*use "../`fjoraar'/Stedsnavn_SSB_ASCII.dta", clear	
use "../`fjoraar'/Stedsnavn_SSB_Unicode.dta", clear	
*exit

* RETTELSER
replace Sted = subinstr(Sted,"Fuosko","Fuossko",1)	//KMD har det riktig! 
													//Stedet heter Fuossko, mens kommunenavnet er Fuosko suokhan.
replace Sted = "Dielddanuorri Tjeldsund" if strmatch(Sted, "Tjeldsund")
replace Sted = "Hammerfest Hámmerfeasta" if strmatch(Sted, "Hammerfest")
replace Sted = "Namsos Nåavmesjenjaelmie" if strmatch(Sted, "Namsos")

*Avslutning:
* SPES: Eiganes og Våland blir for langt, lager femsiders profiler.
* Fjerner "kommunedel" fra den aktuelle filen, men endrer ikke de andre filene.
replace Sted = subinstr(Sted, " kommunedel", "",.) if Sted_kode == "110303"

sort Sted_kode
notes drop _dta
note: Oppdatert `: di %tdCYND date(c(current_date),"DMY")' av skriptet som i hvert fall opprinnelig het Oppdatere_master-GEO...
label data "Sjekk notes"
save "Stedsnavn_SSB_Unicode.dta", replace

replace Sted = "Eiganes og Våland kommunedel" if Sted_kode == "110303"
*exit

*Excel- og txt-versjoner av fila - Dropper Sentrum og Marka i Oslo.
* Lagres som Unicode-txt-fil. Den lastes opp på SQL-Server hos SIIT,
* for oppdatering av Regiontabellen i utvalgsmekanismen på khp.fhi.no.
* Her må Hreg tas ut...
**************************************************************************************

preserve
	gen HarBydel =.
	replace HarBydel =1 if (geo==301 | geo==1103 | geo==4601 | geo==5001 )
	gen Aar=`profilaar'
	gen RegiontypeId="KOMMUNE" if length(Sted_kode)==4
	replace RegiontypeId="FYLKE" if length(Sted_kode)==2 & Sted_kode!="00"
	replace RegiontypeId="BYDEL" if length(Sted_kode)==6 
	sort Sted_kode 
	drop if Sted_kode=="030116" | Sted_kode=="030117"
	drop if (geo>80 & geo<90)		//Helseregioner
*pause Inni preserve	
	keep Sted_kode Aar Sted RegiontypeId HarBydel
	order Sted_kode Aar Sted RegiontypeId
	export excel using "Geografiliste_`profilaar'_utenSentrumMarka.xlsx", firstrow(variables) replace
	export delimited RegionMaster_`profilaar'.txt, delimiter(tab) nolabel replace
restore

*exit

*TILLEGG - EGEN FIL FOR BRUK I GRAFER ETC:
* Numerisk Geo bør ha kommune- (etc)navnene som value labels, og korte navn. 
* Det er nyttig ved bruk i f.eks. grafer.
**************************************************************************************
/*	Det følgende er en prosedyre som 
	 a) fjerner fylkesangivelser i kommunenavn (av typen "Os (Hordaland)" ). 
	 b) Fjerner "bydel".
	 c) fjerner de samiske navnene, så f.eks. Troms ikke heter "Troms Romsa".
	 d) løper gjennom filen og ekstraherer hvert navn, legger det i label, og dropper raden.
	 e) tilordner den nye labelen til numerisk geo.
	 
	 BEMERK: Disse navnene er uten samisk - de er ikke "helt offisielle", men er tilrettelagt for bruk
	 i grafer.
	*/	 
	tempfile mellomlager
	sort geo

	//Fylkesangivelser kommer i parentes. Let etter "navn<space>startparentes"
	replace Sted = regexs(1) ///
		if regexm(Sted, "([a-zA-Z0-9æøåÆØÅ]+)([ ][\(])")
	
	//Fjerne "bydel"
	replace Sted = subinstr(Sted, "Bydel ", "", .)
	replace Sted = subinstr(Sted, " bydel", "", .)
	replace Sted = subinstr(Sted, " kommunedel", "", .)
	
	//Språkversjoner
	replace Sted = "Røyrvik"	if strmatch(Sted, "*Røyrvik")
	replace Sted = "Storfjord"	if strmatch(Sted, "*Storfjord*")
	replace Sted = "Kåfjord"	if strmatch(Sted, "G?ivuotna*")
	replace Sted = "Kautokeino" if strmatch(Sted, "Guovdage*")
	replace Sted = "Porsanger"	if strmatch(Sted, "Porsanger*")
	replace Sted = "Karasjok"	if strmatch(Sted, "K?r??johka*")
	replace Sted = "Tana"		if strmatch(Sted, "Deatnu*")
	replace Sted = "Nesseby"	if strmatch(Sted, "Unj?rga*")
	replace Sted = "Snåsa"		if strmatch(Sted, "Snåase*")
	replace Sted = "Fauske"		if strmatch(Sted, "Fauske*")
	replace Sted = "Hamarøy"	if strmatch(Sted, "Hamarøy*")
	replace Sted = "Sortland"	if strmatch(Sted, "Sortland*")
	replace Sted = "Harstad"	if strmatch(Sted, "Harstad*")
	replace Sted = "Lavangen"	if strmatch(Sted, "*Lavangen")
	replace Sted = "Nordreisa" 	if strmatch(Sted, "Nordreisa*")
	replace Sted = "Hattfjelldal" 	if strmatch(Sted, "*Hattfjelldal")
	replace Sted = "Tjeldsund" 	if strmatch(Sted, "*Tjeldsund")
	replace Sted = "Hammerfest" if strmatch(Sted, "Hammerfest*")
	replace Sted = "Namsos" 	if strmatch(Sted, "Namsos*")

*exit
	save `mellomlager', replace

	****Generere value labels til numerisk geo: stripper ned filen til null...
	local antobs=_N

	forvalues i=1/`antobs' {  
		local tall	= geo	// Leser øverste rad
		local tekst	= Sted
		di "`tall'" " er " "`tekst'"
		
		label define geonavn `tall' "`tekst'", add
		drop in 1
	}
	* Labelen må mellomlagres, for "use" sletter labels i memory (selv uten option "clear"!)	
	label save geonavn using "label_geonavn.do", replace

	use `mellomlager', clear
	do label_geonavn.do			//Oppretter labelen ...
	label values geo geonavn	//...og bruker den.
	clonevar GEO=geo 					//For merging i indikatorfaktaark
	sort GEO
	
	note: Kortversjon av stedsnavn (uten "bydel", fylke eller samisk), og disse 
	note: er lagt som value labels på numerisk Geo.
	note: Til bruk særlig i grafer.
	save "Stedsnavn_SSB_TIL_GRAFER_Unicode.dta", replace
		
