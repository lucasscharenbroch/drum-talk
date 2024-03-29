import { compile as _compile, defaultSettings } from '../output/Main'
import { isRight } from '../output/Util'
import * as Parse from '../output/Parse'
import * as Drawable from '../output/Drawable';
import * as Note from '../output/Note';
import * as Util from '../output/Util'
import * as Rational from '../output/Data.Rational'

const toRational = Rational.toRational(Rational.toRationalInt);

export const default_settings = defaultSettings;
export const mk_signature = (x, y) => Parse.TimeSig.create(x)(y);
export const mk_duration = x => toRational(1)(x); // (duration is newtype)

export function compile(settings: any, s: string): {success: boolean, value: any} {
    let res = _compile(settings)(s);

    return {
        success: isRight(res),
        value: res.value0
    };
}

type drawable_obj = {is_rest: boolean, is_tuplet: boolean, value: d_note | d_rest | d_tuplet};

type d_rest = {duration: number};
type d_note = {duration: number, note: note};
type d_tuplet = {duration: number, items: drawable_obj[], num_notes: number, notes_occupied: number};

type note = {
    num_grace_notes: any,
    num_tremolo: number,
    is_gock: boolean,
    is_accent: boolean,
    is_marcato: boolean,
    stick: string
}

function drawable_note_to_json(drawable: any): drawable_obj {
    if(drawable instanceof Drawable.DrawableNote) {
        const roll_trems = duration_to_number(drawable.value1) >= 0.25 ? 3 : 2;

        return {
            is_rest: false,
            is_tuplet: false,
            value: {
                duration: drawable.value1,
                note: {
                    num_grace_notes: Util.natToNum(drawable.value0.numGraceNotes),
                    num_tremolo: drawable.value0.stroke instanceof Note.Double ? 1 :
                                 drawable.value0.stroke instanceof Note.LongRoll ? roll_trems : 0,
                    is_gock: drawable.value0.stroke instanceof Note.Gock,
                    is_accent: drawable.value0.articulation instanceof Note.Accent,
                    is_marcato: drawable.value0.articulation instanceof Note.Marcato,
                    stick: drawable.value0.stick instanceof Note.WeakRight ? "R" :
                           drawable.value0.stick instanceof Note.StrongRight ? "R" :
                           "L",
                }
            }
        };
    } else if(drawable instanceof Drawable.DrawableRest) {
        return {
            is_rest: true,
            is_tuplet: false,
            value: {
                duration: drawable.value0,
            }
        }
    } else {
        return {
            is_rest: false,
            is_tuplet: true,
            value: {
                duration: drawable.value1,
                items: drawable.value0.map(drawable_note_to_json),
                num_notes: drawable.value2,
                notes_occupied: drawable.value3
            }
        };
    }
}

export function purs_measures_to_json(drawable_measures: any): drawable_obj[][][] {
    return drawable_measures.map(m => m.map(b => b.map(drawable_note_to_json)));
}

export const unpack_duration = Util.unpackDuration;
export const duration_to_number = Util.durationToNumber;
