<script lang="ts" context="module">
	import { load } from '$lib/loadInput';

	export { load };
</script>

<script lang="ts">
	import { part1, part2, type Result, type Position, type Pipe } from '$lib/5';
	import { useRunner } from '$lib/runner';

	export let input: string;

	const lines = input.split('\n');
	let done = false;
	let result = 0;
	let activePart = 1;
	let gen: Result;
	let intersections: Position[] = [];
	let pipes: Pipe[] = [];

	const { run, stop, running } = useRunner(handleClick);

	$: init(activePart);

	function init(active: number) {
		done = false;
		result = 0;
		pipes = [];
		intersections = [];
		gen = active === 1 ? part1(lines) : part2(lines);
		stop();
	}

	function full() {
		const ps = [];
		let inter: Position[] = [];
		for (const res of gen) {
			if (typeof res === 'number') {
				result = res;
			} else {
				ps.push(res.pipe);
				inter = res.intersections;
			}
		}
		done = true;
		intersections = inter;
		pipes = ps;
	}

	function handleClick() {
		if (!done) {
			const res = gen.next();
			done = res.done;
			if (typeof res.value === 'number') {
				result = res.value;
			} else if (typeof res.value === 'object') {
				intersections = res.value.intersections;
				pipes = [...pipes, res.value.pipe];
			}
		}
	}
</script>

<h3 class="text-6xl">U tryna get the pipe?</h3>
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
	<div class="flex gap-4">
		<button
			class="btn btn-outline"
			class:btn-disabled={done}
			disabled={done}
			on:click={() => ($running ? stop() : run())}
		>
			{#if $running}
				Stop
			{:else}
				run
			{/if}
		</button>
		<button class="btn btn-secondary" on:click={() => init(activePart)}>Reset</button>
		<button class="btn btn-secondary" on:click={() => full()}>To end</button>
	</div>
	<div
		class="relative bg-contain bg-center"
		style="height: 1000px; width: 1000px; background-image: url('/yudodis.jpg');"
	>
		<button
			class="bottom-4 right-4 absolute btn h-24 w-24 aspect-square bg-cover text-gray-100"
			class:btn-disabled={done}
			style="background-image: url('/jrswish.png');"
			disabled={done}
			on:click={handleClick}
			title="lay pipe"
		/>
		<svg height="1000" width="1000" class="bg-gray-800/50">
			{#each pipes as [[x1, y1], [x2, y2]], i (i)}
				<line {x1} {x2} {y1} {y2} class="stroke-slate-100 stroke-2" />
			{/each}
		</svg>
		{#each intersections as [x, y], i (i)}
			<span
				class="absolute h-3 w-3 -translate-x-[50%] -translate-y-[50%] rounded-full bg-purple-400 opacity-75"
				style="top: {y}px; left: {x}px;"
			/>
		{/each}
	</div>
</section>
