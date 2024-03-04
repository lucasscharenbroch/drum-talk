<script lang="ts">
    import { default_settings, mk_sig } from './purs-bridge'

    export let settings = default_settings;
    export let time_signature;
    let sig_num = "4";
    let sig_denom = "4";

    const s_to_n = s => s == "" ? 1 : Number(s);

    $: settings.timeSig = time_signature;
    $: time_signature = mk_sig(s_to_n(sig_num), s_to_n(sig_denom));
</script>

<input bind:value={sig_num} min="1" step="1">

<select bind:value={sig_denom}>
  <option value="2">2</option>
  <option value="4">4</option>
  <option value="8">8</option>
</select>
