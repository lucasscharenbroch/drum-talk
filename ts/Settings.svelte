<script lang="ts">
    import SelectDuration from './SelectDuration.svelte';
    import { default_settings, mk_signature, mk_duration } from './purs-bridge'

    export let settings = default_settings;
    export let time_signature;
    let sig_num = "4";
    let sig_denom = "4";

    let def_short_dur = "16";
    let def_long_dur = "8";
    let def_group_dur = "4";

    const s_to_n = s => s == "" ? 1 : Number(s);

    $: settings.timeSig = time_signature;
    $: settings.defLong = mk_duration(s_to_n(def_long_dur));
    $: settings.defShort = mk_duration(s_to_n(def_short_dur));
    $: settings.defGroupDuration = mk_duration(s_to_n(def_group_dur));
    $: time_signature = mk_signature(s_to_n(sig_num), s_to_n(sig_denom));
</script>

<section id="settings">
    <div>
        <label for="sig-num">Time Signature: </label>
        <input id="sig-num" bind:value={sig_num} min="1" step="1">
        <SelectDuration bind:value={sig_denom}></SelectDuration>
    </div>

    <div>
        <label for="long-dur">Default Short Duration</label>
        <SelectDuration id="long-dur" bind:value={def_short_dur}></SelectDuration>
    </div>

    <div>
        <label for="short-dur">Default Long Duration</label>
        <SelectDuration id="short-dur" bind:value={def_long_dur}></SelectDuration>
    </div>

    <div>
        <label for="group-dur">Default Group Duration</label>
        <SelectDuration id="group-dur" bind:value={def_group_dur}></SelectDuration>
    </div>
</section>

<style>
    #settings {
        display: flex;
        justify-content: space-evenly;
    }
</style>
