<script lang="ts">
	import { activeDay } from '$lib/stores';

	const days = Array.from(Array(25)).map((_, i) => i + 1);
</script>

<div class="rounded-lg shadow bg-base-200 drawer drawer-mobile">
	<input id="my-drawer-2" type="checkbox" class="drawer-toggle" />
	<main
		class="flex-grow flex flex-col p-4 md:p-8 lg:p-16 items-center overflow-x-hidden bg-base-100 text-base-content drawer-content"
	>
		<label for="my-drawer-2" class="mb-4 btn btn-primary drawer-button lg:hidden">open menu</label>
		<slot />
	</main>
	<div class="drawer-side">
		<label for="my-drawer-2" class="drawer-overlay" />
		<aside
			class="flex flex-col justify-between border-r border-base-200 bg-base-100 text-base-content w-72"
		>
			<div class="sticky top-0 bg-base-100">
				<a class="block text-xl text-center py-4 w-full link-primary" href="/"
					>Advent Of Code 2021</a
				>
			</div>
			<nav>
				<ul class="menu p-4 overflow-y-auto bg-base-100 text-base-content">
					{#each days as day (day)}
						<li>
							<a href="/days/{day}" class:active={$activeDay === day}>Day {day}</a>
						</li>
					{/each}
				</ul>
			</nav>
		</aside>
	</div>
</div>

<style global>
	@tailwind base;
	@tailwind components;
	@tailwind utilities;
</style>
