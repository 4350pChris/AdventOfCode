<script lang="ts">
	import { part2 } from '$lib/3';
	import DecBinNumber from './_DecBinNumber.svelte';

	export let input: string[];

	let oxCandidates: string[] = [...input];
	let coCandidates: string[] = [...input];
	let mostCommon: Array<'1' | '0'> = [];
	let leastCommon: Array<'1' | '0'> = [];
	let result = 0;
	let done = false;
	let gen = part2(input);

	const reset = () => {
		oxCandidates = [...input];
		coCandidates = [...input];
		mostCommon = [];
		leastCommon = [];
		result = 0;
		done = false;
		gen = part2(input);
	};

	const handleClick = () => {
		if (!done) {
			const next = gen.next();
			done = next.done;
			if (!done) {
				const val = next.value as {
					oxCandidates: string[];
					coCandidates: string[];
					mostCommon: Array<'1' | '0'>;
					leastCommon: Array<'1' | '0'>;
				};
				oxCandidates = val.oxCandidates;
				coCandidates = val.coCandidates;
				mostCommon = val.mostCommon;
				leastCommon = val.leastCommon;
			} else {
				result = next.value as number;
			}
		}
	};
</script>

<button class="btn btn-outline" class:btn-disabled={done} disabled={done} on:click={handleClick}
	>Filter Lists</button
>
<button class="btn btn-secondary" on:click={reset}>reset</button>
<div class="grid grid-cols-2 space-x-8">
	<div>
		<p class="text-lg font-bold mb-2">
			Oxygen (most common bit)
			<br />
			<DecBinNumber bit={mostCommon} />
		</p>
		<div class="inline-flex flex-wrap">
			{#each oxCandidates as ox}
				<code class="inline-block mr-2">{ox}</code>
			{/each}
		</div>
	</div>
	<div>
		<p class="text-lg font-bold mb-2">
			CO2 (least common bit)
			<br />
			<DecBinNumber bit={leastCommon} />
		</p>
		<div class="inline-flex flex-wrap">
			{#each coCandidates as co}
				<code class="inline-block mr-2">{co}</code>
			{/each}
		</div>
	</div>
</div>
