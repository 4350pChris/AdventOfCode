<script lang="ts" context="module">
	import { load } from '$lib/loadInput';

	export { load };
</script>

<script lang="ts">
	import { part1, part2 } from '$lib/1';
	export let input: string;

	const lines = input?.split('\n').map((n) => Number(n));
	const p1 = part1(lines);
	const p2 = part2(lines);
</script>

<h3 class="text-6xl">Sonar</h3>
{#each [p1, p2] as part, i}
	<section class="space-y-4 mt-4">
		<h4 class="text-2xl font-bold">Part {i + 1}</h4>
		<p class="font-bold">
			Total increases
			<code>{part.result}</code>
		</p>
		<p>Input</p>
		<code class="block px-4 py-2 bg-base-200 columns-2 md:columns-4 lg:columns-6">
			{#each part.lines as line, i}
				{line}
				{#if part.increases.includes(i)} Increase {/if}
				<br />
			{/each}
		</code>
	</section>
{/each}
