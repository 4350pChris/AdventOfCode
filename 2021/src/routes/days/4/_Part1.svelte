<script lang="ts">
	import { part1 } from '$lib/4';
	import type { Field } from '$lib/4';
	import FieldBox from './_FieldBox.svelte';

	export let numbers: number[];
	export let fields: Field[];

	let gen = part1(numbers, fields);
	let done = false;
	let result = 0;
	let drawn: number[] = [];
	let localFields: { field: Field; count: number }[] = [...fields].map((field) => ({
		field,
		count: 0
	}));

	const handleClick = () => {
		if (!done) {
			const res = gen.next();
			done = res.done;
			const [n, fs] = res.value as [number, { field: Field; count: number }[]];
			drawn = [...drawn, n];
			localFields = fs;
			if (done) {
				result = (res.value as [number, { field: Field; count: number }[], number])[2];
			}
		}
	};
</script>

{#if result}
	<div>Result: {result}</div>
{/if}
<button class="btn btn-outline" on:click={handleClick}>Draw next number</button>
<p>Numbers: {drawn}</p>
<div>
	{#each localFields.sort((a, b) => b.count - a.count) as { field, count }}
		<FieldBox {field} done={count === 5} />
	{/each}
</div>
