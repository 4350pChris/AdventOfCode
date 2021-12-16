<script lang="ts" context="module">
	import { load } from '$lib/loadInput';

	export { load };
</script>

<script lang="ts">
	import { makeFields, makeNumbers } from '$lib/4';

	import Part1 from './_Part1.svelte';
	// import Part2 from './_Part2.svelte';

	export let input: string;

	const [numbersLine, ...rest] = input.split('\n');
	const numbers = makeNumbers(numbersLine);
	const fields = makeFields(rest);

	console.log(fields);

	let comp: any = Part1;
</script>

<h3 class="text-6xl">Binary Diagnostics</h3>
<div class="tabs mt-4">
	<button
		class="tab text-lg tab-bordered"
		class:tab-active={comp === Part1}
		on:click={() => (comp = Part1)}>Part 1</button
	>
	<!-- <button
		class="tab text-lg tab-bordered"
		class:tab-active={comp === Part2}
		on:click={() => (comp = Part2)}>Part 2</button
	> -->
</div>
<section class="space-y-4 mt-4 min-w-full">
	<svelte:component this={comp} {fields} {numbers} />
</section>
