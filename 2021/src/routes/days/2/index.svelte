<script lang="ts" context="module">
	import { load } from '$lib/loadInput';

	export { load };
</script>

<script lang="ts">
	import { part1, part2 } from '$lib/2';

	import SubmarineCanvas from './_SubmarineCanvas.svelte';
	export let input: string;
</script>

<h3 class="text-6xl">Dive!</h3>
<section class="space-y-4 mt-4">
	<h4 class="text-2xl font-bold">Part 1</h4>
	<SubmarineCanvas genFn={() => part1(input.split('\n'))} size={4000} xScale={0.8} />
</section>
<section class="space-y-4 mt-4">
	<h4 class="text-2xl font-bold">Part 2</h4>
	<SubmarineCanvas genFn={() => part2(input.split('\n'))} size={2000000} xScale={350} />
</section>
