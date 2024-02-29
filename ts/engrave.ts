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
const grace1 = () => new GraceNoteGroup([new GraceNote({ keys: c, duration: "8", slash: true })], true);
const trem2 = () => new Tremolo(2);
const trem3 = () => new Tremolo(3);
const grace2 = () => {
    let gns = [
        new GraceNote({ keys: c, duration: "16" }),
        new GraceNote({ keys: c, duration: "16" })
    ];

    beams.push(new Beam(gns));

    return new GraceNoteGroup(gns, true);
};

const note = (duration: string, modifiers: Modifier[] = [], is_x = false): StaveNote => {
    let res = new StaveNote({ keys: (is_x ? xc : c), duration });
    modifiers.map(mod => res.addModifier(mod));
    return res;
};

// read measures from purescript object
function read_measures(input: any): StaveNote[][] {
    return [[ // TODO
        note("q", [marcato()]),
        note("q"),
        note("q"),
        note("q"),
    ]];
}

export function engrave(input: any): void {
    beams = [];
    tuplets = [];

    let measures = read_measures(input);

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

    let voices = measures.map(m => new Voice({ num_beats: 4, beat_value: 4 }).addTickables(m))
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
