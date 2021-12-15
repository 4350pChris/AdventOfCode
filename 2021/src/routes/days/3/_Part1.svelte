<script lang="ts">
	import { invertBits, part1 } from '$lib/3';
	import DecBinNumber from './_DecBinNumber.svelte';

	export let input: string[];

	const gen = part1(input);

	let done = false;
	let mostCommon: Array<'1' | '0'> = [];
	let result: number;

	const handleClick = () => {
		if (!done) {
			const next = gen.next();
			done = next.done;
			if (!done) {
				mostCommon = next.value as Array<'1' | '0'>;
			} else {
				result = next.value as number;
			}
		}
	};
</script>

<button class="btn btn-outline" on:click={handleClick}>Next Most Common</button>
<div class="text-lg font-bold">
	<p>
		Gamma Rate
		<br />
		<DecBinNumber bit={mostCommon} />
	</p>
	<p>
		Epsilon
		<br />
		<DecBinNumber bit={invertBits(mostCommon)} />
	</p>
	{#if result}
		<p>
			Result: <code>{result}</code>
		</p>
	{/if}
</div>
<div class="columns-1 md:columns-3 lg:columns-5">
	{#each input as num}
		<code>
			{#each num as n, i}
				<span class="transition" class:text-red-500={mostCommon[i] === n}>{n}</span>
			{/each}
		</code>
	{/each}
</div>
