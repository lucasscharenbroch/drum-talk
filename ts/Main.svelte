<script lang="ts">
    import { onMount } from 'svelte';
    import { init, draw_test } from './engrave'

    let text_input = "1 2 3 4";
    let output_div;

    function set_up_engraving() {
        init(output_div);
        draw_test();
    }

    onMount(set_up_engraving);

    $: console.log(text_input);
</script>

<h1>Drum-Talk</h1>
<main>
    <textarea bind:value={text_input}></textarea>
    <div bind:this={output_div}></div>
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
        width: 40vw;
        height: 80vh;
    }
</style>
