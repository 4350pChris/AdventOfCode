<script lang="ts">
	import type { Field } from '$lib/4';

	export let field: Field;
	export let done: boolean;
</script>

<div class={`card shadow-lg ${done ? 'ring-2 ring-red-300 animate-pulse' : ' '}`}>
	<div class="card-body flex-initial">
		<div class="max-w-max grid place-content-center grid-cols-5 grid-rows-5 gap-2">
			{#each field as row}
				{#each row as [n, isSet]}
					<code class:text-red-500={isSet}>{n}</code>
				{/each}
			{/each}
		</div>
	</div>
</div>
