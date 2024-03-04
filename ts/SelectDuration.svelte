<script lang="ts">
    export let value;
    export let id = "";
</script>

<select bind:value={value} id={id}>
    <option value="1">1</option>
    <option value="2">2</option>
    <option value="4">4</option>
    <option value="8">8</option>
    <option value="16">16</option>
    <option value="32">32</option>
</select>