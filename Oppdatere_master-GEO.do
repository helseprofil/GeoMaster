/*  Verktøyscript:
	OPPDATERE GEO-MASTERFIL FRA  ETT ÅR TIL NESTE - TIDLØST FILNAVN FOR GIT-REPO

*** Nytt H-23: Starter med oppdatert tabell tblGeo fra norgeo/orgdata-filen geo-koder.accdb.	
	
	
	
***		Obs om Bydeler i Stavanger:
***		Eiganes og Våland får fjernet "kommunedel" fra den filen som brukes i flatfilproduksjon
***		- ellers blir det femsiders bydelsprofil (2020- ).
	
	Poenget med å scripte er å lettere kunne teste/endre, og å 
	dokumentere hva vi gjør. Scriptet må nybygges neste år.
	
	Scriptnavn: Oppdatere_master-GEO.do - Under Git versjonskontroll (fra sep-21).
	
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
	  - legge til bydeler for 4 byer, OG legge inn "Bydel" i Oslo-navnene. Kilde: Bydeler.dta i \2015.
	  - Nye navn for 1739 Røyrvik og 1939 Storfjord.
	  - Lage en fil til bruk i grafer, med kortversjon av navnene som value label.
	* for 2016-profiler:  * ingenting *
	* for 2018-profiler: Fire smnslå (Larvik, Holmestrand, Færder, Indre Fosen) 
	    og nye numre i hele 50 Trøndelag.
	* for 2019-profiler: 
	  - Avsluttet prod. av ASCII-versjoner av filene, lager bare Unicode. 
	  - Dermed redusert til ett script for oppdatering av geomasterfiler.
	  - Kopiert inn prod. av RegionMaster_ÅÅÅÅ.txt fra "Lage_Unicode-versjoner"-scriptet (se 2018).
	  - Lagt inn Helseregioner - hadde notert at de trengtes andre steder. Fjerne dem fra RegionMaster.
	  - Nytt navn: Nordreisa på samisk/kvensk.
	* For 2020-profiler: Masse. Kommuner: Leser inn filen fra KMD og flikker på den (manglende samisk, etc).
		Nye fylker: Hardkoder. Bydeler: nye i Stavanger. Helseregioner nye numre.
		MIDLERTIDIG (håper jeg): Luke vekk bokstav "eng" fra Porsáŋki. Den blir spm.tegn et sted på veien 
		fra Indikator.txt til profil.
	* For OVP-2021: Lage egen versjon av RegionMaster, der fylkene er tatt ut (det lages ikke fylkesprofiler ennå)
	* For 2022:
	  - Nye samiske navn, Fant forskjeller fra norgeo-data per 18.9.21:
		Evenes Evenášši
		Hábmer - Hamarøy (rekkefølge)
		Nordland - Nordlánnda
		(porsanger med Eng - lukes ut pga trøbbel på vei til profilene)
		(Troms og Finnmark - Romsa ja Finnmárku - Tromssa ja Fi... - Blir for langt! DROPPER.)
		Trøndelag - Trööndelage
	* For 2023:
	  - Nye samiske navn: Trondheim Tråanten, Røros Rossen
	* For 2024:
	  - Splitting av flere fylker, med nye fylkesnummer.
	  - Derfor tatt utgangspunkt i oppdatert tblGeo fra orgdata-filen.
		
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
clear frames

*LEGG INN RETT ÅRSTALL!
local profilaar "2024"

* KJØRING:
************************************************************************
*  TEST på at det er lagt inn rett årstall i filbanen. Vanligvis vil dette 
*  skriptet kjøres året før profilene produseres. Verifiser derfor at 
* 		[årstall i filbanen] = [inneværende år] + 1
************************************************************************
local aarstalliFilbanen = real("`profilaar'") //`profilaar' 
local innevaerendeAar = real(word("`c(current_date)'", 3))
assert `aarstalliFilbanen'==`innevaerendeAar' + 1 

local fjoraar  =  `profilaar'-1
di `fjoraar'
* Sikre at mappestrukturen eksisterer
capture mkdir "F:\Forskningsprosjekter\PDB 2455 - Helseprofiler og til_\Masterfiler/`profilaar'"
cd "F:\Forskningsprosjekter\PDB 2455 - Helseprofiler og til_\Masterfiler/`profilaar'"

/*-------------------------------------------------
*	Sjekking: Vs. navnelista for kommunene fra KLASS. Kommenter ut når sjekket.
*	Kan ikke bare merge på navn, pga vår masterfil har Oslo K og Oslo F - ikke entydig.

*** BØR ERSTATTES MED NORGEO-DATA, dvs. bruke R-kommandoer.
*** Og da må det legges inn Git-kommandoer først, for å sikre at R jobber i riktig versjon.

	import delimited "Kommuneliste_fra_KLASS_per_2023-01-30.csv", clear
	sort code			//Er kommunenummer
	rename code geo
	merge 1:1 geo using "../`fjoraar'/Stedsnavn_SSB_Unicode.dta"	//fjorårets
*	merge 1:1 geo using "Stedsnavn_SSB_Unicode.dta"					//Årets fil (når den er laget!)
 	replace name = subinstr(name, "- ", "", .)
	gen diff = ""
	replace diff = "***" if name != Sted & length(Sted_kode) == 4
			* Funn: 
			* tegnsettproblemer med 5437 Karasjok fra KLASS, og med 1853 Evenes. 
			* Nye samiske navn TRD, Røros
	exit
*-------------------------------------------------*/

* Hente inn kildetabeller
/*	odbc load, exec(`"SELECT Fr.INDIKATOR, Fr.MODUS FROM FRISKVIK Fr 
	WHERE Fr.MODUS='F' "') ///
	dsn("MS Access Database; DBQ=F:\Prosjekter\Kommunehelsa\PRODUKSJON\STYRING\KHELSA.mdb;")   clear */

odbc load, exec(`"SELECT t.code, t.name, t.validTo, t.level FROM tblGeo t WHERE t.validTo = '`profilaar'' AND t.level <> 'grunnkrets' "') ///
dsn("MS Access Database; DBQ=F:\Forskningsprosjekter\PDB 2455 - Helseprofiler og til_\PRODUKSJON\STYRING\raw-khelse\geo-koder.accdb;")   clear
rename (code name validTo level) (Sted_kode Sted Aar RegiontypeId)

frame create hreg
frame change hreg
odbc load, exec(`"SELECT * FROM GeoKoder G WHERE G.GEOniv = 'H' AND G.TIL = 9999 "') ///
dsn("MS Access Database; DBQ=F:\Forskningsprosjekter\PDB 2455 - Helseprofiler og til_\PRODUKSJON\STYRING\KHELSA.mdb;")   clear

frame change default

* RETTELSER  - samisk 's med pil ned' mangler
replace Sted = "Evenes Evenášši" if strmatch(Sted, "Evenes*")
replace Sted = "Kárášjohka Karasjok" if strmatch(Sted, "*Karasjok")

* RYDDING
drop if substr(Sted_kode, -2, 2) == "99"

replace Sted = "Bydel " + Sted if length(Sted_kode) == 6 & substr(Sted_kode, 1, 4) == "0301"
replace Sted = Sted + " bydel" if length(Sted_kode) == 6 & inlist(substr(Sted_kode, 1, 4), "4601", "5001")
replace Sted = Sted + " kommunedel" if length(Sted_kode) == 6 & substr(Sted_kode, 1, 4) == "1103"

replace Sted = subinstr(Sted, " - ", " ",.)		// Ta bort bindestreker mellom språkversjoner

* Legge inn rad for Landet
insobs 1, before(1)
replace Sted_kode = "00" in 1
replace Sted = "Hele landet" in 1
replace Aar = "2024" in 1

replace RegiontypeId = strupper(RegiontypeId)
order Sted_kode Aar Sted RegiontypeId


********************************************************************************
* AVSLUTNING - LAGRE RESULTATFILER


	** Ser ut som ny rekkefølge er egnet, nå med nye grunnlagsdata...





*** 1) RegionMaster: for oppdatering av Regiontabellen i utvalgsmekanismen på khp.fhi.no.
*		- Dropper Sentrum og Marka i Oslo.
*		- Dropper Helseregioner.
*		- Lagres som Unicode-txt-fil, OG som Excelfil.
* 	Egen versjon for Oppvekstprofiler, uten fylkene. (Kun txt-versjon)
**************************************************************************************

preserve
	gen HarBydel =.
	replace HarBydel =1 if inlist(Sted_kode, "0301", "1103", "4601", "5001")
	sort Sted_kode 
	drop if Sted_kode == "030116" | Sted_kode == "030117"
*pause Inni preserve	
	export excel using "Geografiliste_`profilaar'_utenSentrumMarka.xlsx", firstrow(variables) replace
	export delimited RegionMaster_`profilaar'.txt, delimiter(tab) nolabel replace
	
	drop if RegiontypeId == "FYLKE"
	export delimited RegionMaster_OPPVEKST_`profilaar'.txt, delimiter(tab) nolabel replace
restore


*** 2) Komplett DTA-fil
* Hekte på Helseregioner fra naboframe
drop RegiontypeId Aar

frame change hreg
drop ID FRA TIL TYP GEOniv 
rename (GEO NAVN) (Sted_kode Sted)
order Sted_kode Sted

frame change default
frameappend hreg			// Stata 17: brukte package frameappend from http://fmwww.bc.edu/RePEc/bocode/f.
							// search i Help for frameappend, klikk lenke, "click here to install". Er tillatt på FHI.

* Lage numerisk GEO 
destring Sted_kode, generate(geo)
order Sted_kode Sted

* SPES (og gjelder inntil videre): Eiganes og Våland blir for langt, lager femsiders profiler.
* Fjerner "kommunedel" fra den aktuelle filen, men endrer ikke de andre filene.
replace Sted = subinstr(Sted, " kommunedel", "",.) if Sted_kode == "110303"

sort Sted_kode
notes drop _dta
note: Oppdatert `: di %tdCYND date(c(current_date),"DMY")' av skriptet som i hvert fall opprinnelig het Oppdatere_master-GEO...
label data "Sjekk notes"
save "Stedsnavn_SSB_Unicode.dta", replace

replace Sted = "Eiganes og Våland kommunedel" if Sted_kode == "110303"
*exit


*** 3) EGEN FIL FOR BRUK I GRAFER ETC:
* 		Numerisk Geo bør ha kommune- (etc)navnene som value labels, og korte navn. 
* 		Det er nyttig ved bruk i f.eks. grafer.
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
		if regexm(Sted, "([a-zA-Z0-9æøöåÆØÅ]+)([ ][\(])")
	
	//Fjerne "bydel"
	replace Sted = subinstr(Sted, "Bydel ", "", .)
	replace Sted = subinstr(Sted, " bydel", "", .)
	replace Sted = subinstr(Sted, " kommunedel", "", .)
	
	//Språkversjoner
	replace Sted = "Evenes" 	if strmatch(Sted, "Evenes*")
	replace Sted = "Fauske"		if strmatch(Sted, "Fauske*")
	replace Sted = "Finnmark"	if strmatch(Sted, "Finnmark*")
	replace Sted = "Hamarøy"	if strmatch(Sted, "*Hamarøy")
	replace Sted = "Hammerfest" if strmatch(Sted, "Hammerfest*")
	replace Sted = "Harstad"	if strmatch(Sted, "Harstad*")
	replace Sted = "Hattfjelldal" 	if strmatch(Sted, "*Hattfjelldal")
	replace Sted = "Karasjok"	if strmatch(Sted, "K?r??johka*")
	replace Sted = "Kautokeino" if strmatch(Sted, "Guovdage*")
	replace Sted = "Kåfjord"	if strmatch(Sted, "G?ivuotna*")
	replace Sted = "Lavangen"	if strmatch(Sted, "*Lavangen")
	replace Sted = "Namsos" 	if strmatch(Sted, "Namsos*")
	replace Sted = "Nesseby"	if strmatch(Sted, "Unj?rga*")
	replace Sted = "Nordland" 	if strmatch(Sted, "Nordland*")
	replace Sted = "Nordreisa" 	if strmatch(Sted, "Nordreisa*")
	replace Sted = "Porsanger"	if strmatch(Sted, "Porsanger*")
	replace Sted = "Røros"		if strmatch(Sted, "Røros*")
	replace Sted = "Røyrvik"	if strmatch(Sted, "*Røyrvik")
	replace Sted = "Snåsa"		if strmatch(Sted, "Snåase*")
	replace Sted = "Sortland"	if strmatch(Sted, "Sortland*")
	replace Sted = "Storfjord"	if strmatch(Sted, "*Storfjord*")
	replace Sted = "Tana"		if strmatch(Sted, "Deatnu*")
	replace Sted = "Tjeldsund" 	if strmatch(Sted, "*Tjeldsund")
	replace Sted = "Troms" 		if strmatch(Sted, "Troms *")
	replace Sted = "Trondheim" 	if strmatch(Sted, "Trondheim*")
	replace Sted = "Trøndelag" 	if strmatch(Sted, "Trøndelag*")

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
	export delimited "Stedsnavn_SSB_TIL_GRAFER_Unicode.csv", delimiter(";") nolabel replace
