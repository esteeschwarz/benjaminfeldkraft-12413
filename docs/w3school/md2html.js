import marked from "marked"

const mdOptions = {
  // whether to conform to original MD implementation
  pedantic: false,
  // Github Flavoured Markdown
  gfm: true,
  // tables extension
  tables: true,
  // smarter list behavior
  smartLists: true,
  // "smart" typographic punctuation for things like quotes and dashes
  smartypants: true,
  // sanitize HTML tags
  sanitize: true,
  // ... other options
}
let markdownText = "### general
- the corpus material converted to ANNIS readable format
- makes exploration via ANNIS user interface possible
- [ANNIS view (RUEG corpus):](https://korpling.org/annis3/#_c=UlVFRy1LTVItVFVSXzAuMQ)

hhh
### tech:
- textbase for conversion can be different formats like being exported by transcription software. > [list of systems](http://prowiki.ids-mannheim.de/bin/view/GAIS/TranskriptionseditorenListe)
- the format resulting is an xml-scheme looking like this:";

export function convertToHTML(markdownText) {
  marked.setOptions(mdOptions)
  return marked(markdownText)
}