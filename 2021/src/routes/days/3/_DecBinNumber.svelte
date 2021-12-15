<script lang="ts">
	export let bit: Array<'1' | '0'>;
</script>

{#if bit.length}
	<code>{bit.join('')}</code>
	<sub class="mr-4">2</sub>
	<code>{parseInt(bit.join(''), 2)}</code>
	<sub>10</sub>
{/if}
