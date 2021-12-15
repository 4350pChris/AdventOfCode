<script lang="ts" context="module">
	import { load } from '$lib/loadInput';

	export { load };
</script>

<script lang="ts">
	import { part1 } from '$lib/2';

	import SubmarineCanvas from './_SubmarineCanvas.svelte';
	export let input: string;
</script>

<h3 class="text-6xl">Dive!</h3>
<section class="space-y-4 mt-4">
	<h4 class="text-2xl font-bold">Part 1</h4>
	<SubmarineCanvas genFn={() => part1(input.split('\n'))} size={4000} />
</section>
