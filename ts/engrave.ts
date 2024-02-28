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
    Font
}
from "vexflow";

let div, renderer, context;

export function init() {
    div = document.getElementById("output") as HTMLDivElement;
    renderer = new Renderer(div, Renderer.Backends.SVG);
    renderer.resize(1200, 500);
    context = renderer.getContext();
}

export function draw_test() {
    let fn = new GraceNote({ keys: ["c/5"], duration: "8", slash: true})
    let dn1 = new GraceNote({ keys: ["c/5"], duration: "16"})
    let dn2 = new GraceNote({ keys: ["c/5"], duration: "16"})

    const right = () => new Annotation("R").setFont(Font.SANS_SERIF, 15, 'normal', 'bold').setVerticalJustification(AnnotationVerticalJustify.BOTTOM);
    const left = () => new Annotation("L").setFont(Font.SANS_SERIF, 15, 'normal', 'bold').setVerticalJustification(AnnotationVerticalJustify.BOTTOM);
    const accent = () => new Articulation("a>");
    const marcato = () => new Articulation("a^");

    // Create the notes
    const notes1 = [
        // A quarter-note C.
        new StaveNote({ keys: ["c/5"], duration: "16" }),
        new StaveNote({ keys: ["c/5"], duration: "16" }),
        new StaveNote({ keys: ["c/5"], duration: "16" }),

        new StaveNote({ keys: ["c/5"], duration: "8" }),
        new StaveNote({ keys: ["c/5"], duration: "8" }),

        // A quarter-note D.
        new StaveNote({ keys: ["c/5"], duration: "8r" }),
        new StaveNote({ keys: ["c/5"], duration: "8" }),

        new StaveNote({ keys: ["c/5"], duration: "8" }),
        new StaveNote({ keys: ["c/5"], duration: "8r" }),

        // A C-Major chord.
        new StaveNote({ keys: ["c/5"], duration: "q" }),
    ];

    const notes2 = [
        new StaveNote({ keys: ["c/5"], duration: "8" }),
        new StaveNote({ keys: ["c/5"], duration: "16" }),
        new StaveNote({ keys: ["c/5"], duration: "16" }),

        new StaveNote({ keys: ["c/5/x"], duration: "q" }),
        new StaveNote({ keys: ["c/5"], duration: "q" }),
        new StaveNote({ keys: ["c/5"], duration: "q" }),
    ]

    const notes3 = [
        new StaveNote({ keys: ["c/5"], duration: "q" }),
        new StaveNote({ keys: ["c/5"], duration: "q" }),
        new StaveNote({ keys: ["c/5"], duration: "q" }),
        new StaveNote({ keys: ["c/5"], duration: "q" }),
    ]


    notes1[6].addModifier(accent());
    notes1[6].addModifier(new GraceNoteGroup([fn], true));
    notes1[7].addModifier(new GraceNoteGroup([dn1, dn2], true))
    notes1[9].addModifier(new Tremolo(3));

    notes2[0].addModifier(new Tremolo(2));
    notes2[0].addModifier(right());
    notes2[1].addModifier(left());
    notes2[2].addModifier(right());
    notes2[3].addModifier(left());
    notes2[4].addModifier(right());

    notes3[0].addModifier(marcato());

    let tuplets = [
    new Tuplet(notes1.slice(0, 3), {num_notes: 3, notes_occupied: 2}),
    new Tuplet(notes1.slice(0, 5), {num_notes: 3, notes_occupied: 2}),
    ];


    let beams = [
    new Beam(notes1.slice(0, 5)),
    new Beam([dn1, dn2]),
    new Beam(notes2.slice(0, 3)),
    ];

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
    stave3.addClef("percussion").addTimeSignature("4/4");

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
