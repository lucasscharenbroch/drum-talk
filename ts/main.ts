import Main from './Main.svelte';
import {xyz} from '../output/Main/index.js'
import {init, draw_test} from "./engrave"

console.log(xyz("abc"));

const main = new Main({
    target: document.body,
});

init();

draw_test();

export default main;
