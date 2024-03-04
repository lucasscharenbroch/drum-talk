<script lang="ts">
    import { onMount } from 'svelte';
    import { init, engrave } from './engrave'
    import { compile } from './purs-bridge'
    import { writable } from 'svelte/store';
    import Settings from './Settings.svelte'

    let text_input =
`(Ft Ft) 2 {-}a face --..
e 2 {'}A PtfF PtfF
[2] {'}e {'}3 {'}a {'}&
[1] {-}a fAce (_..) (z_x)
e a (-..-) (.-..) <16>(x x) a
(fac fac) fadd fadd --.f
!-!- -!!- !=! !=!
tpl 2e&a (ta ta ta ta ta) (tpl tpl)`;

    let output_div;
    let setup_complete = false;

    let err_mesg = writable("");

    let settings, time_signature;

    function sig_to_string(sig): string {
        return sig.value0 + "/" + sig.value1;
    }

    function set_up_engraving() {
        init(output_div);
        setup_complete = true;
        do_compile(settings, text_input);
    }

    function do_compile(_settings: any, text: string) {
        if(!setup_complete) return;

        let comp_out = compile(_settings, text);

        if(comp_out.success) {
            err_mesg.set("");
            engrave(sig_to_string(time_signature), comp_out.value);
        } else {
            err_mesg.set(comp_out.value);
        }
    }

    onMount(set_up_engraving);

    $: do_compile(settings, text_input);
</script>

<h1>Drum-Talk</h1>
<main>
    <aside>
        <textarea bind:value={text_input}></textarea>
        <p class="red">{$err_mesg}</p>
    </aside>
    <div id="output" bind:this={output_div}></div>
</main>
<Settings bind:settings bind:time_signature></Settings>

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
