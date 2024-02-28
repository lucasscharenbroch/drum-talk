import {compile as _compile, defaultSettings} from '../output/Main'
import {isRight} from '../output/Util'

export function compile(s: string): {success: boolean, value: any} {
    let res = _compile(defaultSettings)(s);

    return {
        success: isRight(res),
        value: res.value0
    };
}