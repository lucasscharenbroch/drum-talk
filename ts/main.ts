import Main from './Main.svelte';
import {init, draw_test} from './engrave'
import {compile} from './compile'

console.log(compile("1 2 3 4"));
console.log(compile("_"));

const main = new Main({
    target: document.body,
});

export default main;
