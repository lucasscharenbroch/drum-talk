import Main from './Main.svelte';
import {xyz} from '../output/Main/index.js'

console.log(xyz("abc"));

const main = new Main({
    target: document.body,
});

export default main;
