import
{
    Renderer,
    Stave,
    StaveNote,
    Voice,
    Formatter,
    Tuplet,
    Beam,
    GraceNoteGroup,
    GraceNote,
    Articulation,
    Tremolo,
    Annotation,
    AnnotationVerticalJustify,
    Font,
    Modifier
}
from "vexflow";
import { purs_measures_to_json } from "./purs-bridge";

let renderer, context;

export function init(div: HTMLDivElement) {
    renderer = new Renderer(div, Renderer.Backends.SVG);
    renderer.resize(div.offsetHeight, div.offsetWidth);
    context = renderer.getContext();
}

let beams, tuplets; // global lists (convenient for below helpers)

const c = ["c/5"];
const xc = ["c/5/x"];

const right = () => new Annotation("R").setFont(Font.SANS_SERIF, 15, 'normal', 'bold').setVerticalJustification(AnnotationVerticalJustify.BOTTOM);
const left = () => new Annotation("L").setFont(Font.SANS_SERIF, 15, 'normal', 'bold').setVerticalJustification(AnnotationVerticalJustify.BOTTOM);
const accent = () => new Articulation("a>");
const marcato = () => new Articulation("a^");
const trem = n => new Tremolo(n);
const flam = () => new GraceNoteGroup([new GraceNote({ keys: c, duration: "8", slash: true })], true);
const grace_n = n => {
    let gns = [];

    for(let i = 0; i < n; i++) {
        gns.push(new GraceNote({ keys: c, duration: "16" }));
    }

    beams.push(new Beam(gns));

    return new GraceNoteGroup(gns, true);
};

const note = (duration: string, modifiers: Modifier[] = [], is_x = false): StaveNote => {
    let res = new StaveNote({ keys: (is_x ? xc : c), duration });
    modifiers.map(mod => res.addModifier(mod));
    return res;
};

// read measures from purescript object, create StaveNotes
function make_measures(purs_measures: any): StaveNote[][] {
    let json_measures = purs_measures_to_json(purs_measures);

    function mk_modifiers(n): Modifier[] {
        let res = [];

        if(n.num_grace_notes == 1)
            res.push(flam());
        else if(n.num_grace_notes >= 2)
            res.push(grace_n(n.num_grace_notes));

        if(n.num_tremolo)
            res.push(trem(n.num_tremolo));

        if(n.is_accent)
            res.push(accent());

        if(n.is_marcato)
            res.push(marcato());

        if(n.stick == "R")
            res.push(right());
        else
            res.push(left());

        return res;
    }

    function notes_from_drawable(d: any): StaveNote[] {
        if(d.is_rest) {
            return [note(d.value.duration + "r")];
        } else if(!d.is_tuplet) {
            return [note(d.value.duration, mk_modifiers(d.value.note), d.value.note.is_gock)];
        } else {
            return []; // TODO
        }
    }

    return json_measures.map(jm => jm.flatMap(d => notes_from_drawable(d)));
}

export function engrave(purs_measures: any): void {
    beams = [];
    tuplets = [];

    let measures = make_measures(purs_measures);

    /*
    let measures = [
        [
            note("16"),
            note("16"),
            note("16"),

            note("8"),
            note("8"),

            note("8r"),
            note("8", [accent(), grace1()]),

            note("8", [grace2()]),
            note("8r"),

            note("q", [trem3()]),
        ],
        [
            note("8", [trem2(), right()]),
            note("16", [left()]),
            note("16", [right()]),

            note("q", [left()], true),
            note("16", [right()]),
            note("16"),
            note("16"),
            note("16"),

            note("16"),
            note("16"),
            note("16"),
            note("16"),
        ],
        [
            note("q", [marcato()]),
            note("q"),
            note("q"),
            note("q"),
        ],
        [
            note("q", [marcato()]),
            note("q"),
            note("q"),
            note("q"),
        ],
        [
            note("q", [marcato()]),
            note("q"),
            note("q"),
            note("q"),
        ],
        [
            note("q", [marcato()]),
            note("q"),
            note("q"),
            note("q"),
        ],
    ]
    */


    /*
    tuplets = tuplets.concat([
        new Tuplet(measures[0].slice(0, 3), {num_notes: 3, notes_occupied: 2}),
        new Tuplet(measures[0].slice(0, 5), {num_notes: 3, notes_occupied: 2}),
    ]);
    */

    /*
    beams = beams.concat([
        new Beam(measures[0].slice(0, 5)),
        new Beam(measures[1].slice(0, 3)),
    ]);
    */

    let f = new Formatter();

    const DY = 120; // vertical distance between staves
    const MAX_X = 700; // max width of a line
    const START_X = 10;
    const START_Y = 40;
    const EXTRA_SPACE = 50;

    let voices = measures.map(m => new Voice({ num_beats: 4, beat_value: 4 }).setStrict(false).addTickables(m)); // TODO remove setStrict(false)
    let widths = voices.map(v => f.preCalculateMinTotalWidth([v]));

    const clef = "percussion";
    const time_sig = "4/4";

    // calculate stave positions

    let staves = [];

    let x = START_X;
    let y = START_Y;

    for(let i = 0; i < widths.length; i++) {
        let w = widths[i] + EXTRA_SPACE + 30 * +(x == START_X) + 30 * +(y == START_Y);
        let xp = x + w;
        let yp = y;

        if(i == widths.length - 1 || xp + widths[i + 1] + EXTRA_SPACE > MAX_X) {
            w = MAX_X - x;
            xp = START_X;
            yp = y + DY;
        }

        console.log("x", x, "w", w, "widths i", widths[i], "i + 1", widths[i + 1]);
        console.log("next offset", x + w + widths[i + 1] + EXTRA_SPACE);

        let s = new Stave(x, y, w);

        if(x == START_X) {
            s.addClef(clef);

            if(y == START_Y) {
                s.addTimeSignature(time_sig);
            }
        }

        staves.push(s);

        f.joinVoices([voices[i]]).formatToStave([voices[i]], staves[i]);

        x = xp;
        y = yp;
    }

    staves.map(s => s.setContext(context).draw());

    for(let i = 0; i < voices.length; i++) {
        voices[i].draw(context, staves[i]);
    }

    beams.map(b => b.setContext(context).draw());
    tuplets.map(t => t.setContext(context).draw());
}
