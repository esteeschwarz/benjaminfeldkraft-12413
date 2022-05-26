# Dieses Script liest eine CSV-Datendatei in GNU R ein.
# Beim Einlesen werden für alle Variablen Beschriftungen (comment) angelegt.
# Die Beschriftungen für Werte wird ebenfalls als Attribute (attr) abgelegt.

#ds_file = file.choose()
# setwd("./")
ds_file = srvd
ds = read.table(
  file=ds_file, encoding="UTF-8",
  header = FALSE, sep = "\t", quote = "\"",
  dec = ".", row.names = "CASE",
  col.names = c(
    "CASE","SERIAL","REF","QUESTNNR","MODE","STARTED","MT02","MT02_01","MT02_02",
    "MT03","MT03_01","MT03_02","MT03_03","MT03_04","MT03_05","MT04","MT04_01",
    "MT04_02","MT04_03","MT04_04","F401","F402","F403","F404","F405","F406","F407",
    "F408","F409","F410","F411","F412","F413","F414","F415","F416","F417","F418",
    "F419","F420","F421","F422","TIME001","TIME002","TIME003","TIME004","TIME005",
    "TIME006","TIME007","TIME008","TIME009","TIME010","TIME011","TIME012","TIME013",
    "TIME014","TIME015","TIME016","TIME017","TIME018","TIME019","TIME020","TIME021",
    "TIME022","TIME023","TIME024","TIME025","TIME_SUM","MAILSENT","LASTDATA",
    "FINISHED","Q_VIEWER","LASTPAGE","MAXPAGE","MISSING","MISSREL","TIME_RSI",
    "DEG_TIME"
  ),
  as.is = TRUE,
  colClasses = c(
    CASE="numeric", SERIAL="character", REF="character", QUESTNNR="character",
    MODE="factor", STARTED="POSIXct", MT02="numeric", MT02_01="logical",
    MT02_02="logical", MT03="numeric", MT03_01="logical", MT03_02="logical",
    MT03_03="logical", MT03_04="logical", MT03_05="logical", MT04="numeric",
    MT04_01="logical", MT04_02="logical", MT04_03="logical", MT04_04="logical",
    F401="numeric", F402="numeric", F403="numeric", F404="numeric",
    F405="numeric", F406="numeric", F407="numeric", F408="numeric",
    F409="numeric", F410="numeric", F411="numeric", F412="numeric",
    F413="numeric", F414="numeric", F415="numeric", F416="numeric",
    F417="numeric", F418="numeric", F419="numeric", F420="numeric",
    F421="numeric", F422="numeric", TIME001="integer", TIME002="integer",
    TIME003="integer", TIME004="integer", TIME005="integer", TIME006="integer",
    TIME007="integer", TIME008="integer", TIME009="integer", TIME010="integer",
    TIME011="integer", TIME012="integer", TIME013="integer", TIME014="integer",
    TIME015="integer", TIME016="integer", TIME017="integer", TIME018="integer",
    TIME019="integer", TIME020="integer", TIME021="integer", TIME022="integer",
    TIME023="integer", TIME024="integer", TIME025="integer", TIME_SUM="integer",
    MAILSENT="POSIXct", LASTDATA="POSIXct", FINISHED="logical",
    Q_VIEWER="logical", LASTPAGE="numeric", MAXPAGE="numeric",
    MISSING="numeric", MISSREL="numeric", TIME_RSI="numeric", DEG_TIME="numeric"
  ),
  skip = 1,
  check.names = TRUE, fill = TRUE,
  strip.white = FALSE, blank.lines.skip = TRUE,
  comment.char = "",
  na.strings = ""
)

rm(ds_file)

attr(ds, "project") = "hux2022"
attr(ds, "description") = "hux2022"
attr(ds, "date") = "2022-02-11 08:58:58"
attr(ds, "server") = "https://www.soscisurvey.de"

# Variable und Value Labels
ds$F401 = factor(ds$F401, levels=c("1","2","3","4","5","6","-9"), labels=c("Weil Oskar glaubt, dass Pauls Vorgesetzter seinem Wunsch zustimmen wird.","Weil Oskar glaubt, Paul sollte mit der Frage warten, bis sein Vorgesetzter weniger Stress hat.","Weil Oskar glaubt, dass Paul von lauten Tieren nicht verletzt werden wird.","Weil Vierbeiner ungefährlich werden, wenn sie zum Kläffen ihre ganze Energie verbrauchen.","Weil Oskar weiß, dass Paul den Joghurt aus dem Kühlschrank im Büro klaut.","Weil gerade zwei Autos gleichzeitig auf den Parkplatz gefahren sind.","[NA] nicht beantwortet"), ordered=FALSE)
ds$F402 = factor(ds$F402, levels=c("1","2","3","4","5","6","-9"), labels=c("Weil Lulu weiß, dass Katja ihren Mann ebenfalls betrogen hat.","Weil Lulu glaubt, dass Katja ihren Mann nicht noch einmal betrügen wird.","Weil Lulu nicht möchte, dass Katja die Scheiben kaputt macht.","Weil Katja sich einem zerbrechlichen Gebäude befindet.","Weil in der Ehe oft mit Gegenständen geworfen wird.","Weil bei der Hochzeit nur drei Leute eingeladen wurden.","[NA] nicht beantwortet"), ordered=FALSE)
ds$F403 = factor(ds$F403, levels=c("1","2","3","4","5","6","-9"), labels=c("Weil Anna glaubt, dass Tom sich mehr anstrengen sollte.","Weil Anna glaubt, dass Tom kein Talent hat.","Weil Anna glaubt, dass Tom undicht wird.","Weil Tom ein Fels ist.","Weil Anna glaubt, dass das Orchester im nächsten Jahr auf Tournee gehen wird.","Weil im Konzertsaal die Beleuchtung ausgefallen ist.","[NA] nicht beantwortet"), ordered=FALSE)
ds$F404 = factor(ds$F404, levels=c("1","2","3","4","5","6","-9"), labels=c("Weil Karla glaubt, dass Andreas das Geld nicht verdient hat.","Weil Karla möchte, dass Andreas nicht mehr arbeitet.","Weil Karla glaubt, dass Andreas ein Landwirt ist.","Weil Andreas riesige Erdäpfel anbaut.","Weil die Reparatur des Traktors sehr teuer war.","Weil im Hofladen Kirschen im Sonderangebot sind.","[NA] nicht beantwortet"), ordered=FALSE)
ds$F405 = factor(ds$F405, levels=c("1","2","3","4","5","6","-9"), labels=c("Weil Max und Moritz sich nicht an die Regeln halten, wenn die Eltern nicht da sind.","Weil Max und Moritz sich ordentlich benehmen, wenn die Eltern nicht da sind.","Weil Opa glaubt, dass die Tiere auf den Möbeln herumspringen.","Weil Opa beobachtet hat, dass kleine Tiere von großen gejagt werden.","Weil Opa gerne Süßigkeiten essen möchte.","Weil Opa noch nichts zum Abendbrot gegessen hat und hungrig ist.","[NA] nicht beantwortet"), ordered=FALSE)
ds$F406 = factor(ds$F406, levels=c("1","2","3","4","5","6","-9"), labels=c("Weil Paula glaubt, dass die Kolleginnen zusammenhalten und sich nicht gegenseitig bei ihrer Vorgesetzten verraten.","Weil Paula denkt, dass die Kolleginnen nicht zusammenhalten und sich gegenseitig verraten würden.","Weil Paula weiß, dass Vögel friedlich miteinander umgehen.","Weil Paula beobachtet hat, dass Tiere der selben Art versuchen sich beim Kampf nicht ernsthaft zu verletzen.","Weil Paula weiß, dass Sabine heute Spätschicht hat und noch lange in der Firma sein wird.","Weil Paula weiß, dass Sabine morgen Geburtstag hat.","[NA] nicht beantwortet"), ordered=FALSE)
ds$F407 = factor(ds$F407, levels=c("1","2","3","4","5","6","-9"), labels=c("Weil die Straße erst sicher gemacht wurde, nachdem der Unfall passiert ist.","Weil der Unfall nicht verhindert werden konnte.","Weil eine Wasserstelle geschlossen wird, sobald dort jemand stirbt.","Weil größere Löcher im Boden gefährlich sind.","Weil sonst alle ihren Müll im Wasser entsorgen.","Weil es draußen nicht besonders warm ist zu dieser Jahreszeit.","[NA] nicht beantwortet"), ordered=FALSE)
ds$F408 = factor(ds$F408, levels=c("1","2","3","4","5","6","-9"), labels=c("Weil Lotta findet, dass Anton selber Schuld ist, dass er gestürzt ist.","Weil Lotta denkt, dass sie Anton zum stürzen gebracht hat.","Weil Anton in sein eigenes Loch gefallen ist.","Weil Anton in das Loch gefallen ist, das er für Lotta gebuddelt hat.","Weil Lotta möchte, dass Anton mit ihr spielt.","Weil Lotta letzte Nacht schlecht geschlafen hat.","[NA] nicht beantwortet"), ordered=FALSE)
ds$F409 = factor(ds$F409, levels=c("1","2","3","4","5","6","-9"), labels=c("Weil auch *A* denkt, dass der Stadt die Millionen wichtiger sind, als unabhängig zu bleiben.","Weil *A* denkt, dass der Stadt ihre Unabhängigkeit wichtiger als irgendwelche Millionen ist.","Weil *A* der Meinung ist, dass man sich eben mit genug Mitteln durchaus ein solches Haus leisten kann.","Weil *A* denkt, dass ein Stadtgebäude nicht unbedingt so viel kostet.","Weil *A* denkt, dass der Unternehmer ein Kino aus dem Gebäude machen will.","Weil sie weiß, dass die Beleuchtung in den Gebäuden erst in der Dämmerung eingeschaltet wird.","[NA] nicht beantwortet"), ordered=FALSE)
ds$F410 = factor(ds$F410, levels=c("1","2","3","4","5","6","-9"), labels=c("Weil sie nicht erwarten konnte, gerade zufällig einen dicken Umschlag Scheine in der Tasche zu haben, als sie den Zauberberg im Fenster sieht?","Weil sie nicht schon wieder die ganzen schwer verdienten Scheine für Kleinigkeiten ausgeben will?","Weil sie in dem Laden sehr häufig zu viel Geld ausgibt und nicht erwarten kann, dass sich das ändert?","Weil in dem Laden die meiste Zeit nur Ratgeberliteratur verkauft wird?","Weil die Gäste im Cafe heute sehr geizig waren und sie außerdem genug Bücher hat?","Weil auch Tunnel in anderen Gegenden gegraben werden und sie nicht weiß, ob die Schweiz überhaupt zur EU gehört.","[NA] nicht beantwortet"), ordered=FALSE)
ds$F411 = factor(ds$F411, levels=c("1","2","3","4","5","6","-9"), labels=c("Weil Mike mit seiner Aussage zu verstehen gibt, älter werden zu wollen.","Weil Mike seine Gerichte nicht für zu gefährlich hält.","Weil Mike Angst hat, durch zu viel Essen zu groß für die enge Küche zu werden?","Weil Mike einmal gesagt hat, dass er weniger essen möchte und das schon eine Weile her ist.","Weil den Gästen die Eingangstür zum Restaurant zu klein ist.","Weil er mit der S-Bahn direkt von zu Hause zum Restaurant fahren kann.","[NA] nicht beantwortet"), ordered=FALSE)
ds$F412 = factor(ds$F412, levels=c("1","2","3","4","5","6","-9"), labels=c("Weil sie denkt, dass bis man ernten kann, noch eine Menge zu tun sein wird.","Weil sie denkt, dass sie sich jetzt erstmal ausruhen können.","Weil sie denkt, dass die Vögel in dieser Jahreszeit alles wegfressen werden.","Weil sie denkt, dass sie Vögel im letzten Winter kaum etwas zu fressen hatten.","Weil sie denkt, der Nachbar hat immer die grüneren Tomaten.","Weil sie denkt, dass der Badesee mitten im Wald liegt.","[NA] nicht beantwortet"), ordered=FALSE)
ds$F413 = factor(ds$F413, levels=c("1","2","3","4","5","6","-9"), labels=c("Weil *A* glaubte frei zu haben und nun doch arbeiten gehen muss.","Weil *A* frei hat und nicht arbeiten gehen muss.","Weil *A* denkt, dass ihr Sieg dahinschmilzt.","Weil *A* ihren Lottoschein in der Waschmaschine gewaschen hat.","Weil *A* Eis essen gehen möchte.","Weil *A* lieber Drachen fliegen lassen möchte.","[NA] nicht beantwortet"), ordered=FALSE)
ds$F414 = factor(ds$F414, levels=c("1","2","3","4","5","6","-9"), labels=c("Weil *B* meint, dass Attraktivität oft blenden (täuschen) kann.","Weil *B* findet, dass das äußere Erscheinungsbild den wahren Wert zeigt..","Weil das Bilnken (Schimmern) allein kein Charakteristkum (Alleinstellungsmerkmal) des Edelmetalls ist.","Weil B beobachtet hat, dass es Sachen gibt, die wie wertvolle Metalle aussehen, aber überhaupt nicht wertvoll sind.","Weil A schon länger glücklich verheiratet ist.","Weil *B* Lust auf ein Bad hat.","[NA] nicht beantwortet"), ordered=FALSE)
ds$F415 = factor(ds$F415, levels=c("1","2","3","4","5","6","-9"), labels=c("Weil *B* den Disput mit *A* beenden möchte.","Weil *B* weiter mit *A* streiten möchte.","Weil *B* intelligenter ist als *A*. (Weil *B* einen höheren IQ hat als *A*)","Weil *A* ein dummer Mensch ist.","Weil *B* mit *A* zu Abend essen möchte.","Weil es heute noch regnen wird.","[NA] nicht beantwortet"), ordered=FALSE)
ds$F416 = factor(ds$F416, levels=c("1","2","3","4","5","6","-9"), labels=c("Weil *B* möchte, dass *A* ihr Geld für schlechte Zeiten bewahrt. (weil *B* nicht möchte, dass *A* ihr Geld unnötig ausgibt)","Weil *B* möchte, dass *A* ihr Geld jetzt ausgibt.","Weil *B* möchte, dass *A* mit ihren Lebensmitteln haushälterisch umgeht.","Weil Backwaren in schlimmen Situationen helfen können.","Weil *B* mit *A* gemeinsam zum Konzert gehen möchte.","Weil der Blumenladen in eine andere Straße umgezogen ist.","[NA] nicht beantwortet"), ordered=FALSE)
ds$F417 = factor(ds$F417, levels=c("1","2","3","4","5","6","-9"), labels=c("Weil Lisa denkt, dass sie sich voreilig in den Reihen umgestellt haben und deswegen nun mehr Zeit brauchen.","Weil Lisa denkt, dass es sich gelohnt hat, sich in die andere Reihe zu stellen.","Weil Lisa denkt, dass man oft Falsches tut, wenn man nicht ausdauernd ist.","Weil Lisa denkt, dass Karin jetzt bestraft wird.","Weil Lisa denkt, dass Karin von einer Schlange gebissen wurde.","Weil Lisa denkt, dass sie Schokoeis einkaufen sollten.","[NA] nicht beantwortet"), ordered=FALSE)
ds$F418 = factor(ds$F418, levels=c("1","2","3","4","5","6","-9"), labels=c("Weil Maria glaubt, dass Susi besser auf ihre Wertsachen aufpassen müsste.","Weil Maria glaubt, dass Susi ihre teuren Gegenstände nicht zu schützen braucht.","Weil Maria glaubt, dass Schutzbeauftragte nötig sind.","Weil Maria glaubt, dass manche Dinge wertvoll sind.","Weil Maria glaubt, dass Susi ihr Fahrrad blau lackieren soll.","Weil Maria glaubt, dass es besser ist, Auto zu fahren.","[NA] nicht beantwortet"), ordered=FALSE)
ds$F419 = factor(ds$F419, levels=c("1","2","3","4","5","6","-9"), labels=c("Weil Luis glaubt, dass die Tomaten Lasagne eine liebe Idee war, die dennoch negative Auswirkungen hatte.","Weil Luis glaubt, dass das Nudelgericht eine sehr nette, hilfreiche Geste war.","Weil Luis glaubt, dass lieb gemeinte Handlungen häufig zu Tränen führen.","Weil Luis glaubt, dass Hilfsbereitschaft wichtig ist.","Weil Luis glaubt, dass Tomaten Nachtschattengewächse sind.","Weil Luis glaubt, dass italienische Gerichte lecker schmecken.","[NA] nicht beantwortet"), ordered=FALSE)
ds$F420 = factor(ds$F420, levels=c("1","2","3","4","5","6","-9"), labels=c("Weil Moritz denkt, dass Hans selbst seinen Diebstahl ausgleichen muss.","Weil Moritz denkt, dass er selbst in den Laden gehen muss, um den geklauten Artikel zurückzubringen.","Weil Moritz denkt, dass Hans den Wagen wieder aus dem Matsch fahren muss.","Weil Moritz denkt, dass Fahrzeuge schnell schmutzig werden.","Weil Moritz denkt, dass Elektrogeschäfte viel Strom verbrauchen.","Weil Moritz denkt, dass Hans sich einen Fernseher kaufen muss.","[NA] nicht beantwortet"), ordered=FALSE)
ds$F421 = factor(ds$F421, levels=c("1","2","3","4","5","6","-9"), labels=c("Weil Maja meint, dass Laura gesiegt hat, weil Laura überzeugt von ihren Leistungen ist.","Weil Maja meint, dass Laura auch ohne Selbstvertrauen gewonnen hätte.","Weil Maja meint, dass Laura mit ihren Gedanken Felsmassive verschieben kann.","Weil Maja meint, dass sich Felsmassive bewegen.","Weil Maja mein, dass Wettrennen öffentlich organisiert sind.","Weil Maja meint, dass Laufschuhe bequem sind.","[NA] nicht beantwortet"), ordered=FALSE)
ds$F422 = factor(ds$F422, levels=c("1","2","3","4","5","6","-9"), labels=c("Weil Sebastian der Meinung ist, dass die Ursache des Gerüchts die tatsächliche Auflösung des Geschäfts war.","Weil Sebastian der Meinung ist, dass die Gerüchte um die Auflösung des Geschäfts unbegründet waren.","Weil Sebastian der Meinung ist, dass es brennt, wenn dunkler Dampf zu sehen ist.","Weil Sebastian der Meinung ist, dass Warenhäuser brennen können.","Weil Sebastian der Meinung ist, dass es in Supermärkten Einkaufswagen gibt.","Weil Sebastian der Meinung ist, dass Kassierer freundliche Menschen sind.","[NA] nicht beantwortet"), ordered=FALSE)
attr(ds$MT02_01,"F") = "nicht gewählt"
attr(ds$MT02_01,"T") = "ausgewählt"
attr(ds$MT02_02,"F") = "nicht gewählt"
attr(ds$MT02_02,"T") = "ausgewählt"
attr(ds$MT03_01,"F") = "nicht gewählt"
attr(ds$MT03_01,"T") = "ausgewählt"
attr(ds$MT03_02,"F") = "nicht gewählt"
attr(ds$MT03_02,"T") = "ausgewählt"
attr(ds$MT03_03,"F") = "nicht gewählt"
attr(ds$MT03_03,"T") = "ausgewählt"
attr(ds$MT03_04,"F") = "nicht gewählt"
attr(ds$MT03_04,"T") = "ausgewählt"
attr(ds$MT03_05,"F") = "nicht gewählt"
attr(ds$MT03_05,"T") = "ausgewählt"
attr(ds$MT04_01,"F") = "nicht gewählt"
attr(ds$MT04_01,"T") = "ausgewählt"
attr(ds$MT04_02,"F") = "nicht gewählt"
attr(ds$MT04_02,"T") = "ausgewählt"
attr(ds$MT04_03,"F") = "nicht gewählt"
attr(ds$MT04_03,"T") = "ausgewählt"
attr(ds$MT04_04,"F") = "nicht gewählt"
attr(ds$MT04_04,"T") = "ausgewählt"
attr(ds$FINISHED,"F") = "abgebrochen"
attr(ds$FINISHED,"T") = "ausgefüllt"
attr(ds$Q_VIEWER,"F") = "Teilnehmer"
attr(ds$Q_VIEWER,"T") = "Durchklicker"
comment(ds$SERIAL) = "Seriennummer (sofern verwendet)"
comment(ds$REF) = "Referenz (sofern im Link angegeben)"
comment(ds$QUESTNNR) = "Fragebogen, der im Interview verwendet wurde"
comment(ds$MODE) = "Interview-Modus"
comment(ds$STARTED) = "Zeitpunkt zu dem das Interview begonnen hat (Europe/Berlin)"
comment(ds$MT02) = "meta: Ausweichoption (negativ) oder Anzahl ausgewählter Optionen"
comment(ds$MT02_01) = "meta: einverstanden mit?"
comment(ds$MT02_02) = "meta: etc."
comment(ds$MT03) = "meta_age: Ausweichoption (negativ) oder Anzahl ausgewählter Optionen"
comment(ds$MT03_01) = "meta_age: 18-25"
comment(ds$MT03_02) = "meta_age: 26-30"
comment(ds$MT03_03) = "meta_age: 31-40"
comment(ds$MT03_04) = "meta_age: 41-50"
comment(ds$MT03_05) = "meta_age: 51-70"
comment(ds$MT04) = "gender: Ausweichoption (negativ) oder Anzahl ausgewählter Optionen"
comment(ds$MT04_01) = "gender: m"
comment(ds$MT04_02) = "gender: w"
comment(ds$MT04_03) = "gender: d"
comment(ds$MT04_04) = "gender: k.a."
comment(ds$F401) = "item5.01"
comment(ds$F402) = "item5.02"
comment(ds$F403) = "item5.03"
comment(ds$F404) = "item5.04"
comment(ds$F405) = "item5.05"
comment(ds$F406) = "item5.06"
comment(ds$F407) = "item5.07"
comment(ds$F408) = "item5.08"
comment(ds$F409) = "item5.09"
comment(ds$F410) = "item5.1"
comment(ds$F411) = "item5.11"
comment(ds$F412) = "item5.12"
comment(ds$F413) = "item5.13"
comment(ds$F414) = "item5.14"
comment(ds$F415) = "item5.15"
comment(ds$F416) = "item5.16"
comment(ds$F417) = "item5.17"
comment(ds$F418) = "item5.18"
comment(ds$F419) = "item5.19"
comment(ds$F420) = "item5.2"
comment(ds$F421) = "item5.21"
comment(ds$F422) = "item5.22"
comment(ds$TIME001) = "Verweildauer Seite 1"
comment(ds$TIME002) = "Verweildauer Seite 2"
comment(ds$TIME003) = "Verweildauer Seite 3"
comment(ds$TIME004) = "Verweildauer Seite 4"
comment(ds$TIME005) = "Verweildauer Seite 5"
comment(ds$TIME006) = "Verweildauer Seite 6"
comment(ds$TIME007) = "Verweildauer Seite 7"
comment(ds$TIME008) = "Verweildauer Seite 8"
comment(ds$TIME009) = "Verweildauer Seite 9"
comment(ds$TIME010) = "Verweildauer Seite 10"
comment(ds$TIME011) = "Verweildauer Seite 11"
comment(ds$TIME012) = "Verweildauer Seite 12"
comment(ds$TIME013) = "Verweildauer Seite 13"
comment(ds$TIME014) = "Verweildauer Seite 14"
comment(ds$TIME015) = "Verweildauer Seite 15"
comment(ds$TIME016) = "Verweildauer Seite 16"
comment(ds$TIME017) = "Verweildauer Seite 17"
comment(ds$TIME018) = "Verweildauer Seite 18"
comment(ds$TIME019) = "Verweildauer Seite 19"
comment(ds$TIME020) = "Verweildauer Seite 20"
comment(ds$TIME021) = "Verweildauer Seite 21"
comment(ds$TIME022) = "Verweildauer Seite 22"
comment(ds$TIME023) = "Verweildauer Seite 23"
comment(ds$TIME024) = "Verweildauer Seite 24"
comment(ds$TIME025) = "Verweildauer Seite 25"
comment(ds$TIME_SUM) = "Verweildauer gesamt (ohne Ausreißer)"
comment(ds$MAILSENT) = "Versandzeitpunkt der Einladungsmail (nur für nicht-anonyme Adressaten)"
comment(ds$LASTDATA) = "Zeitpunkt als der Datensatz das letzte mal geändert wurde"
comment(ds$FINISHED) = "Wurde die Befragung abgeschlossen (letzte Seite erreicht)?"
comment(ds$Q_VIEWER) = "Hat der Teilnehmer den Fragebogen nur angesehen, ohne die Pflichtfragen zu beantworten?"
comment(ds$LASTPAGE) = "Seite, die der Teilnehmer zuletzt bearbeitet hat"
comment(ds$MAXPAGE) = "Letzte Seite, die im Fragebogen bearbeitet wurde"
comment(ds$MISSING) = "Anteil fehlender Antworten in Prozent"
comment(ds$MISSREL) = "Anteil fehlender Antworten (gewichtet nach Relevanz)"
comment(ds$TIME_RSI) = "Maluspunkte für schnelles Ausfüllen"
comment(ds$DEG_TIME) = "Maluspunkte für schnelles Ausfüllen"



# Assure that the comments are retained in subsets
as.data.frame.avector = as.data.frame.vector
`[.avector` <- function(x,i,...) {
  r <- NextMethod("[")
  mostattributes(r) <- attributes(x)
  r
}
ds_tmp = data.frame(
  lapply(ds, function(x) {
    structure( x, class = c("avector", class(x) ) )
  } )
)
mostattributes(ds_tmp) = attributes(ds)
ds = ds_tmp
rm(ds_tmp)

