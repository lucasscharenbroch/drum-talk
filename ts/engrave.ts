import
{
    Renderer,
    Stave,
    StaveNote,
    StaveTie,
    Voice,
    Formatter,
    Tuplet,
    Dot,
    Beam,
    GraceNoteGroup,
    GraceNote,
    Articulation,
    Tremolo,
    Annotation,
    AnnotationVerticalJustify,
    Font,
    Modifier,
    Fraction
} from "vexflow";
import {
    purs_measures_to_json,
    unpack_duration,
    duration_to_number
} from "./purs-bridge";

let renderer, context;

let MAX_X;

export function init(div: HTMLDivElement) {
    renderer = new Renderer(div, Renderer.Backends.SVG);
    renderer.resize(div.offsetWidth - 20, div.offsetHeight);
    MAX_X = div.offsetWidth - 20;
    context = renderer.getContext();
}

let beams, tuplets, ties; // global lists (convenient for below helpers)

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

const note = (duration: number, is_rest: boolean, modifiers: Modifier[] = [], is_x = false): StaveNote[] => {
    const _main_note = ds => new StaveNote({ keys: (is_x ? xc : c), duration: ds + (is_rest ? "r" : "")});
    const _dummy_note = ds => new StaveNote({ keys: (is_x ? xc : c), duration: ds + (is_rest ? "r" : "")});

    let duration_spec = unpack_duration(duration);

    let main_note = _main_note(duration_spec.mainDuration);

    for(let i = 0; i < duration_spec.numDots; i++)
        Dot.buildAndAttach([main_note]);

    let tied = duration_spec.tied.map(_dummy_note);

    let res = [main_note, ...tied];

    if(tied.length > 0 && !is_rest) {
        let t = new StaveTie({
            first_note: main_note,
            last_note: tied[tied.length - 1],
        });
        ties.push(t);
    }

    modifiers.map(mod => main_note.addModifier(mod));
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

    function notes_from_drawable(d: any, should_beam_tuplets: boolean = true): StaveNote[] {
        if(d.is_rest) {
            return note(d.value.duration, true);
        } else if(!d.is_tuplet) {
            return note(d.value.duration, false, mk_modifiers(d.value.note), d.value.note.is_gock);
        } else { // tuplet
            let notes = d.value.items.flatMap(dn => notes_from_drawable(dn, false));

            tuplets.push(new Tuplet(notes, { num_notes: d.value.num_notes, notes_occupied: d.value.notes_occupied}));

            if(should_beam_tuplets && d.value.items.every(x => duration_to_number(x.value.duration) < .25))
                beams.push(new Beam(notes));

            return notes;
        }
    }

    function beamify(beamed_notes: any[]): any[] {
        if(beamed_notes.length <= 1 || beamed_notes[0].isRest()) return beamed_notes;
        beams.push(new Beam(beamed_notes));
        return beamed_notes;
    }

    return json_measures.map(jm => jm.flatMap(bg => beamify(bg.flatMap(d => notes_from_drawable(d)))));
}

export function engrave(time_sig: string, purs_measures: any): void {
    context.clear();
    beams = [];
    tuplets = [];
    ties = [];

    let measures = make_measures(purs_measures);

    let f = new Formatter();

    const DY = 120; // vertical distance between staves
    const START_X = 10;
    const START_Y = 40;
    const EXTRA_SPACE = 50;
    const MUL = 1.75; // multiplier

    const calc_width = w => w * MUL + EXTRA_SPACE;

    let voices = measures.map(m => new Voice({ num_beats: 4, beat_value: 4 }).setStrict(false).addTickables(m)); // TODO remove setStrict(false)
    let widths = voices.map(v => f.preCalculateMinTotalWidth([v]));

    const clef = "percussion";

    // calculate stave positions

    let staves = [];

    let x = START_X;
    let y = START_Y;

    for(let i = 0; i < widths.length; i++) {
        let w = calc_width(widths[i]) + 30 * +(x == START_X) + 30 * +(y == START_Y);
        let xp = x + w;
        let yp = y;

        if(i == widths.length - 1 || xp + calc_width(widths[i + 1]) > MAX_X) {
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
    ties.map(t => t.setContext(context).draw());
}
