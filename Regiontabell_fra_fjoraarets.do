/*	VERKTØY: FIKSE REGIONTABELL LIK FJORÅRETS
	
	Når det ikke er Geo-endringer, så vi har utsatt å oppdatere geo-koder.accdb, 
	får jeg ikke kjørt det vanlige scriptet for å lage geo-masterfiler.
	
	Tar den gamle regionmasteren og legger inn nytt årstall.
	Gi gammel fil nytt navn først, så ødelegger vi ikke noe.
*/

local profilaar "2025"	// Årstall som skal legges inn
local path "O:\Prosjekt\FHP\Masterfiler\2025"
local innfil1 "RegionMaster_2025_2024.txt"
local utfil1 "RegionMaster_2025.txt"

local innfil2 "RegionMaster_OPPVEKST_2025_2024.txt"
local utfil2 "RegionMaster_OPPVEKST_2025.txt"

*==========================================
foreach nr in 1 2 {

	* Bevare GEO som string - ledende nuller
	import delimited "`path'\\`innfil`nr''", delimiter("\t") stringcols(1) case(preserve) clear

	replace Aar = `profilaar'

	export delimited "`path'\\`utfil`nr''", delimiter(tab) nolabel replace
}