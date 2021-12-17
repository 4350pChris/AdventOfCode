<script lang="ts" context="module">
	import { load } from '$lib/loadInput';

	export { load };
</script>

<script lang="ts">
	import { main, Result } from '$lib/6';
	import { useRunner } from '$lib/runner';

	export let input: string;

	const { run, running, stop } = useRunner(handleClick);

	let result = 0;
	let done = false;
	let activePart = 1;
	let gen: Result;

	$: init(activePart);

	function init(active) {
		result = 0;
		done = false;
		gen = main(
			input.split(',').map((n) => Number(n)),
			active === 1 ? 80 : 256
		);
	}

	function handleClick() {
		if (!done) {
			const res = gen.next();
			done = res.done;
			if (res.value) {
				result = res.value;
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
		<!-- <button class="btn btn-secondary" on:click={() => full()}>To end</button> -->
	</div>
</section>
