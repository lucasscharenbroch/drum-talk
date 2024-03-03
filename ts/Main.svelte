<script lang="ts">
    import { onMount } from 'svelte';
    import { init, engrave } from './engrave'
    import { compile } from './purs-bridge'
    import { writable } from 'svelte/store';

    let text_input =
`(Ft Ft) 2 {-}a face --..
e 2 {'}A PtfF PtfF
[2] {'}e {'}3 {'}a {'}&
[1] {-}a fAce (_..) (z_x)
e a (-..-) (.-..) <16>(x x) a
(fac fac) fadd fadd --.f
!-!- -!!- !<8>=! !<8>=!
tpl 2e&a (ta ta ta ta ta) (tpl tpl)`;

    let output_div;
    let setup_complete = false;

    let err_mesg = writable("");

    function set_up_engraving() {
        init(output_div);
        setup_complete = true;
        do_compile(text_input);
    }

    function do_compile(text: string) {
        if(!setup_complete) return;

        let comp_out = compile(text);

        if(comp_out.success) {
            err_mesg.set("");
            engrave(comp_out.value);
        } else {
            err_mesg.set(comp_out.value);
        }
    }

    onMount(set_up_engraving);

    $: do_compile(text_input);
</script>

<h1>Drum-Talk</h1>
<main>
    <aside>
        <textarea bind:value={text_input}></textarea>
        <p class="red">{$err_mesg}</p>
    </aside>
    <div id="output" bind:this={output_div}></div>
</main>

<style>
    h1 {
        text-align: center;
    }

    main {
        display: flex;
        justify-content: space-evenly;
    }


    main > * {
        height: 80vh;
    }

    aside {
        width: 30vw;
    }

    #output {
        width: 60vw;
    }

    textarea {
        height: 80%;
        width: 100%;
    }

    .red {
        color: red;
    }
</style>
