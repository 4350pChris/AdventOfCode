<script lang="ts" context="module">
	import { load } from '$lib/loadInput';

	export { load };
</script>

<script lang="ts">
	export let input: string;

	const numbers = input.split(',').map((n) => Number(n));

	function part1(numbers: number[]) {
		const sorted = numbers.sort((a, b) => a - b);
		const median = sorted[Math.floor(sorted.length / 2)];
		return sorted.reduce((sum, n) => sum + Math.abs(median - n), 0);
	}

	function part2(numbers: number[]) {
		const fuel = (x: number) => (x * (x + 1)) / 2;
		const sum = (numbers: number[]) => numbers.reduce((acc, n) => acc + n, 0);
		const mean = sum(numbers) / numbers.length;
		const lower = sum(numbers.map((n) => fuel(Math.abs(n - Math.floor(mean)))));
		const upper = sum(numbers.map((n) => fuel(Math.abs(n - Math.ceil(mean)))));
		return Math.min(lower, upper);
	}
</script>

<h3 class="text-6xl">The Treachery of Whales</h3>
<section class="flex flex-col gap-4 mt-4 min-w-full">
	<p class="text-2xl">No fany visualisation for today.</p>
	<h4 class="text-lg">Part 1</h4>
	<code>{part1(numbers)}</code>
	<h4 class="text-lg">Part 2</h4>
	<code>{part2(numbers)}</code>
</section>
