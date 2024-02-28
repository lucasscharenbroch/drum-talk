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

let beams, tuplets, voices, staves; // global lists

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

const note = (duration: string, modifiers: Modifier[] = []): StaveNote => {
    let res = new StaveNote({ keys: c, duration });
    modifiers.map(mod => res.addModifier(mod));
    return res;
};

const xnote = (duration: string, modifiers: Modifier[] = []): StaveNote => {
    let res = new StaveNote({ keys: xc, duration });
    modifiers.map(mod => res.addModifier(mod));
    return res;
};

export function draw_test() {
    beams = [];
    tuplets = [];
    staves = [];
    voices = [];

    // Create the notes
    const notes1 = [
        // A quarter-note C.
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
    ];

    const notes2 = [
        note("8", [trem2(), right()]),
        note("16", [left()]),
        note("16", [right()]),

        xnote("q", [left()]),
        note("q", [right()]),
        note("q"),
    ]

    const notes3 = [
        note("q", [marcato()]),
        note("q"),
        note("q"),
        note("q"),
    ]

    tuplets = tuplets.concat([
        new Tuplet(notes1.slice(0, 3), {num_notes: 3, notes_occupied: 2}),
        new Tuplet(notes1.slice(0, 5), {num_notes: 3, notes_occupied: 2}),
    ]);

    beams = beams.concat([
        new Beam(notes1.slice(0, 5)),
        new Beam(notes2.slice(0, 3)),
    ]);

    let f = new Formatter();

    // Create a voice in 4/4 and add above notes
    const voice1 = new Voice({ num_beats: 4, beat_value: 4 });
    const voice2 = new Voice({ num_beats: 4, beat_value: 4 });
    const voice3 = new Voice({ num_beats: 4, beat_value: 4 });

    voice1.addTickables(notes1);
    voice2.addTickables(notes2);
    voice3.addTickables(notes3);

    // Format and justify the notes to 400 pixels.
    let x = f.preCalculateMinTotalWidth([voice1]);
    let y = f.preCalculateMinTotalWidth([voice2]);
    let z = f.preCalculateMinTotalWidth([voice3]);

    // Create a stave of width 400 at position 10, 40 on the canvas.
    const stave1 = new Stave(10, 40, x + 200);
    const stave2 = new Stave(stave1.getWidth() + stave1.getX(), 40, y + 80);
    const stave3 = new Stave(10, 140, z + 200);

    f = f.formatToStave([voice1], stave1);
    f = f.formatToStave([voice2], stave2);
    f = f.formatToStave([voice3], stave3);

    // Add a clef and time signature.
    stave1.addClef("percussion").addTimeSignature("4/4");
    stave3.addClef("percussion");

    // Connect it to the rendering context and draw!
    stave1.setContext(context).draw();
    stave2.setContext(context).draw();
    stave3.setContext(context).draw();

    f.joinVoices([voice1]).formatToStave([voice1], stave1)
    f.joinVoices([voice2]).formatToStave([voice2], stave2)
    f.joinVoices([voice3]).formatToStave([voice3], stave3)

    // Render voice
    voice1.draw(context, stave1);
    voice2.draw(context, stave2);
    voice3.draw(context, stave3);

    beams.map(b => b.setContext(context).draw());
    tuplets.map(t => t.setContext(context).draw());
}
