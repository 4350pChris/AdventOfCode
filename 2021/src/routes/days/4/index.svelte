<script lang="ts" context="module">
	import { load } from '$lib/loadInput';

	export { load };
</script>

<script lang="ts">
	import { makeFields, makeNumbers, part1, part2, type Field } from '$lib/4';
	import FieldBox from './_FieldBox.svelte';
	import { fade } from 'svelte/transition';
	import { onDestroy } from 'svelte';

	export let input: string;

	const [numbersLine, ...rest] = input.split('\n');
	const numbers = makeNumbers(numbersLine);
	const fields = makeFields(rest);

	let gen: ReturnType<typeof part1>;
	let activePart = 1;
	let done = false;
	let result = 0;
	let drawn: number[] = [];
	let localFields: { field: Field; count: number }[] = [];
	let runner: number;

	$: init(activePart);

	function init(active: number) {
		done = false;
		result = 0;
		drawn = [];
		localFields = fields.map((field) => ({
			field,
			count: 0
		}));
		gen = active === 1 ? part1(numbers, fields) : part2(numbers, fields);
		clearInterval(runner);
		runner = undefined;
	}

	onDestroy(stop);

	function run() {
		runner = window.setInterval(handleClick, 1000);
	}

	function stop() {
		if (runner) {
			clearInterval(runner);
			runner = undefined;
		}
	}

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

<h3 class="text-6xl">Binary Diagnostics</h3>
<div class="tabs mt-4">
	<button
		class="tab text-lg tab-bordered"
		class:tab-active={activePart === 1}
		on:click={() => (activePart = 1)}>Part 1</button
	>
	<button
		class="tab text-lg tab-bordered"
		class:tab-active={activePart === 2}
		on:click={() => (activePart = 2)}>Part 2</button
	>
</div>
<section class="flex flex-col gap-4 mt-4 min-w-full">
	{#if result}
		<div>Result: {result}</div>
	{/if}
	<div>
		<button
			class="btn btn-outline"
			class:btn-disabled={done}
			disabled={done}
			on:click={() => (runner ? stop() : run())}
		>
			{#if runner}
				Stop
			{:else}
				run
			{/if}
		</button>
		<button
			class="btn btn-outline"
			class:btn-disabled={done}
			disabled={done}
			on:click={handleClick}
		>
			Draw next number
		</button>
		<button class="btn btn-secondary" on:click={() => init(activePart)}>Reset</button>
	</div>
	<p>
		Numbers:
		{#each drawn as n}
			<code transition:fade>
				{n}
			</code>
		{/each}
	</p>
	<div class="flex flex-row flex-wrap justify-between">
		{#each localFields.sort( (a, b) => (activePart === 1 ? b.count - a.count : a.count - b.count) ) as { field, count }}
			<FieldBox {field} done={count === 5} />
		{/each}
	</div>
</section>
